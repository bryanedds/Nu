namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.NuConstants
module World = WorldPrims

[<AutoOpen>]
module EntityDispatcherModule =

    type EntityDispatcher () =

        abstract member Init : Entity * IXDispatcherContainer -> Entity
        default dispatcher.Init (entity, _) = entity

        abstract member Register : Address * World -> World
        default dispatcher.Register (_, world) = world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (_, world) = world

        abstract member GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (_, _) = 0.0f

[<AutoOpen>]
module Entity2dDispatcherModule =

    type [<AbstractClass>] Entity2dDispatcher () =
        inherit EntityDispatcher ()

        override dispatcher.Init (entity2d, dispatcherContainer) =
            let entity2d = base.Init (entity2d, dispatcherContainer)
            let entity2d = Entity2dFacet.init entity2d dispatcherContainer
            entity2d
                .SetPosition(Vector2.Zero)
                .SetDepth(0.0f)
                .SetSize(DefaultEntitySize)
                .SetRotation(0.0f)

        override dispatcher.GetPickingPriority (entity, _) =
            Entity2dFacet.getPickingPriority entity

        abstract member PropagatePhysics : Address * World -> World
        default dispatcher.PropagatePhysics (_, world) = world

        abstract member HandleBodyTransformMessage : Address * BodyTransformMessage * World -> World
        default dispatcher.HandleBodyTransformMessage (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * Matrix3 * Matrix3 * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _, _, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = DefaultEntitySize

        abstract member IsTransformRelative : Entity * World -> bool
        default dispatcher.IsTransformRelative (_, _) = true

[<AutoOpen>]
module GuiDispatcherModule =

    type [<AbstractClass>] GuiDispatcher () =
        inherit Entity2dDispatcher ()

        override dispatcher.Init (entity, dispatcherContainer) =
            let entity = base.Init (entity, dispatcherContainer)
            GuiFacet.init entity dispatcherContainer

[<AutoOpen>]
module SimpleBodyDispatcherModule =

    type [<AbstractClass>] SimpleBodyDispatcher (makeBodyShape) =
        inherit Entity2dDispatcher ()

        override dispatcher.Init (entity, dispatcherContainer) =
            let entity = base.Init (entity, dispatcherContainer)
            SimpleBodyFacet.init entity dispatcherContainer

        override dispatcher.Register (address, world) =
            SimpleBodyFacet.registerPhysics makeBodyShape address world

        override dispatcher.Unregister (address, world) =
            SimpleBodyFacet.unregisterPhysics address world
            
        override dispatcher.PropagatePhysics (address, world) =
            SimpleBodyFacet.propagatePhysics makeBodyShape address world

        override dispatcher.HandleBodyTransformMessage (address, message, world) =
            SimpleBodyFacet.handleBodyTransformMessage address message world

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with

        [<XField>] member this.IsDown with get () = this?IsDown () : bool
        member this.SetIsDown (value : bool) : Entity = this?IsDown <- value
        [<XField>] member this.UpSprite with get () = this?UpSprite () : Sprite
        member this.SetUpSprite (value : Sprite) : Entity = this?UpSprite <- value
        [<XField>] member this.DownSprite with get () = this?DownSprite () : Sprite
        member this.SetDownSprite (value : Sprite) : Entity = this?DownSprite <- value
        [<XField>] member this.ClickSound with get () = this?ClickSound () : Sound
        member this.SetClickSound (value : Sound) : Entity = this?ClickSound <- value

    type [<Sealed>] ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleButtonEventDownMouseLeft message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let button = World.getEntity message.Subscriber world
                let mousePositionButton = Entity.mouseToEntity mousePosition world button
                if button.Enabled && button.Visible then
                    if NuMath.isInBox3 mousePositionButton button.Position button.Size then
                        let button = button.SetIsDown true
                        let world = World.setEntity message.Subscriber button world
                        let world = World.publish (straddr "Down" message.Subscriber) message.Subscriber NoData world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."

        let handleButtonEventUpMouseLeft message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let button = World.getEntity message.Subscriber world
                let mousePositionButton = Entity.mouseToEntity mousePosition world button
                if button.Enabled && button.Visible then
                    let world =
                        let button = button.SetIsDown false
                        let world = World.setEntity message.Subscriber button world
                        World.publish (straddr "Up" message.Subscriber) message.Subscriber NoData world
                    if NuMath.isInBox3 mousePositionButton button.Position button.Size && button.IsDown then
                        let world = World.publish (straddr "Click" message.Subscriber) message.Subscriber NoData world
                        let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                        let world = { world with AudioMessages = sound :: world.AudioMessages }
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."

        override dispatcher.Init (button, dispatcherContainer) =
            let button = base.Init (button, dispatcherContainer)
            button
                .SetIsDown(false)
                .SetUpSprite({ SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetDownSprite({ SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetClickSound({ SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEvent address -<| CustomSub handleButtonEventDownMouseLeft |>
                World.observe UpMouseLeftEvent address -<| CustomSub handleButtonEventUpMouseLeft

        override dispatcher.GetRenderDescriptors (button, viewAbsolute, _, _) =
            if not button.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = button.Position * viewAbsolute
                              Size = button.Size
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = if button.IsDown then button.DownSprite else button.UpSprite
                              Color = Vector4.One }
                          Depth = button.Depth }]

        override dispatcher.GetQuickSize (button, world) =
            let sprite = button.UpSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with

        [<XField>] member this.LabelSprite with get () = this?LabelSprite () : Sprite
        member this.SetLabelSprite (value : Sprite) : Entity = this?LabelSprite <- value

    type [<Sealed>] LabelDispatcher () =
        inherit GuiDispatcher ()
            
        override dispatcher.Init (label, dispatcherContainer) =
            let label = base.Init (label, dispatcherContainer)
            label.SetLabelSprite { SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.GetRenderDescriptors (label, viewAbsolute, _, _) =
            if not label.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = label.Position * viewAbsolute
                              Size = label.Size
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = label.LabelSprite
                              Color = Vector4.One }
                          Depth = label.Depth }]

        override dispatcher.GetQuickSize (label, world) =
            let sprite = label.LabelSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module TextBoxDispatcherModule =

    type Entity with

        [<XField>] member this.BoxSprite with get () = this?BoxSprite () : Sprite
        member this.SetBoxSprite (value : Sprite) : Entity = this?BoxSprite <- value
        [<XField>] member this.Text with get () = this?Text () : string
        member this.SetText (value : string) : Entity = this?Text <- value
        [<XField>] member this.TextFont with get () = this?TextFont () : Font
        member this.SetTextFont (value : Font) : Entity = this?TextFont <- value
        [<XField>] member this.TextOffset with get () = this?TextOffset () : Vector2
        member this.SetTextOffset (value : Vector2) : Entity = this?TextOffset <- value
        [<XField>] member this.TextColor with get () = this?TextColor () : Vector4
        member this.SetTextColor (value : Vector4) : Entity = this?TextColor <- value

    type [<Sealed>] TextBoxDispatcher () =
        inherit GuiDispatcher ()
            
        override dispatcher.Init (textBox, dispatcherContainer) =
            let textBox = base.Init (textBox, dispatcherContainer)
            textBox
                .SetBoxSprite({ SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetText(String.Empty)
                .SetTextFont({ FontAssetName = "Font"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetTextOffset(Vector2.Zero)
                .SetTextColor(Vector4.One)

        override dispatcher.GetRenderDescriptors (textBox, viewAbsolute, _, _) =
            if not textBox.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = textBox.Position * viewAbsolute
                              Size = textBox.Size
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = textBox.BoxSprite
                              Color = Vector4.One }
                          Depth = textBox.Depth }
                 LayerableDescriptor <|
                    LayeredTextDescriptor
                        { Descriptor =
                            { Text = textBox.Text
                              Position = (textBox.Position + textBox.TextOffset) * viewAbsolute
                              Size = textBox.Size - textBox.TextOffset
                              Font = textBox.TextFont
                              Color = textBox.TextColor }
                          Depth = textBox.Depth }]

        override dispatcher.GetQuickSize (textBox, world) =
            let sprite = textBox.BoxSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with

        [<XField>] member this.IsOn with get () = this?IsOn () : bool
        member this.SetIsOn (value : bool) : Entity = this?IsOn <- value
        [<XField>] member this.IsPressed with get () = this?IsPressed () : bool
        member this.SetIsPressed (value : bool) : Entity = this?IsPressed <- value
        [<XField>] member this.OffSprite with get () = this?OffSprite () : Sprite
        member this.SetOffSprite (value : Sprite) : Entity = this?OffSprite <- value
        [<XField>] member this.OnSprite with get () = this?OnSprite () : Sprite
        member this.SetOnSprite (value : Sprite) : Entity = this?OnSprite <- value
        [<XField>] member this.ToggleSound with get () = this?ToggleSound () : Sound
        member this.SetToggleSound (value : Sound) : Entity = this?ToggleSound <- value

    type [<Sealed>] ToggleDispatcher () =
        inherit GuiDispatcher ()

        let handleToggleEventDownMouseLeft message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let toggle = World.getEntity message.Subscriber world
                let mousePositionToggle = Entity.mouseToEntity mousePosition world toggle
                if toggle.Enabled && toggle.Visible then
                    if NuMath.isInBox3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle = toggle.SetIsPressed true
                        let world = World.setEntity message.Subscriber toggle world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."
    
        let handleToggleEventUpMouseLeft message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let toggle = World.getEntity message.Subscriber world
                let mousePositionToggle = Entity.mouseToEntity mousePosition world toggle
                if toggle.Enabled && toggle.Visible && toggle.IsPressed then
                    let toggle = toggle.SetIsPressed false
                    if NuMath.isInBox3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle = toggle.SetIsOn <| not toggle.IsOn
                        let world = World.setEntity message.Subscriber toggle world
                        let messageType = if toggle.IsOn then "On" else "Off"
                        let world = World.publish (straddr messageType message.Subscriber) message.Subscriber NoData world
                        let sound = PlaySound { Volume = 1.0f; Sound = toggle.ToggleSound }
                        let world = { world with AudioMessages = sound :: world.AudioMessages }
                        (Handled, world)
                    else
                        let world = World.setEntity message.Subscriber toggle world
                        (Unhandled, world) // TODO: make sure message should actually be Unhandled!
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."
        
        override dispatcher.Init (toggle, dispatcherContainer) =
            let toggle = base.Init (toggle, dispatcherContainer)
            toggle
                .SetIsOn(false)
                .SetIsPressed(false)
                .SetOffSprite({ SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetOnSprite({ SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetToggleSound({ SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEvent address -<| CustomSub handleToggleEventDownMouseLeft |>
                World.observe UpMouseLeftEvent address -<| CustomSub handleToggleEventUpMouseLeft

        override dispatcher.GetRenderDescriptors (toggle, viewAbsolute, _, _) =
            if not toggle.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = toggle.Position * viewAbsolute
                              Size = toggle.Size
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite
                              Color = Vector4.One }
                          Depth = toggle.Depth }]

        override dispatcher.GetQuickSize (toggle, world) =
            let sprite = toggle.OffSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with

        [<XField>] member this.IsTouched with get () = this?IsTouched () : bool
        member this.SetIsTouched (value : bool) : Entity = this?IsTouched <- value

    type [<Sealed>] FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleFeelerEventDownMouseLeft message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) as mouseButtonData ->
                let feeler = World.getEntity message.Subscriber world
                let mousePositionFeeler = Entity.mouseToEntity mousePosition world feeler
                if feeler.Enabled && feeler.Visible then
                    if NuMath.isInBox3 mousePositionFeeler feeler.Position feeler.Size then
                        let feeler = feeler.SetIsTouched true
                        let world = World.setEntity message.Subscriber feeler world
                        let world = World.publish (straddr "Touch" message.Subscriber) message.Subscriber mouseButtonData world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."
    
        let handleFeelerEventUpMouseLeft message world =
            match message.Data with
            | MouseButtonData _ ->
                let feeler = World.getEntity message.Subscriber world
                if feeler.Enabled && feeler.Visible then
                    let feeler = feeler.SetIsTouched false
                    let world = World.setEntity message.Subscriber feeler world
                    let world = World.publish (straddr "Release" message.Subscriber) message.Subscriber NoData world
                    (Handled, world)
                else (Unhandled, world)
            | _ -> failwith <| "Expected MouseButtonData from event '" + addrToStr message.Event + "'."
        
        override dispatcher.Init (feeler, dispatcherContainer) =
            let feeler = base.Init (feeler, dispatcherContainer)
            feeler.SetIsTouched false

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEvent address -<| CustomSub handleFeelerEventDownMouseLeft |>
                World.observe UpMouseLeftEvent address -<| CustomSub handleFeelerEventUpMouseLeft

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        [<XField>] member this.Fill with get () = this?Fill () : single
        member this.SetFill (value : single) : Entity = this?Fill <- value
        [<XField>] member this.FillInset with get () = this?FillInset () : single
        member this.SetFillInset (value : single) : Entity = this?FillInset <- value
        [<XField>] member this.FillSprite with get () = this?FillSprite () : Sprite
        member this.SetFillSprite (value : Sprite) : Entity = this?FillSprite <- value
        [<XField>] member this.BorderSprite with get () = this?BorderSprite () : Sprite
        member this.SetBorderSprite (value : Sprite) : Entity = this?BorderSprite <- value

    type [<Sealed>] FillBarDispatcher () =
        inherit GuiDispatcher ()

        let getFillBarSpriteDims (fillBar : Entity) =
            let spriteInset = fillBar.Size * fillBar.FillInset * 0.5f
            let spritePosition = fillBar.Position + spriteInset
            let spriteWidth = (fillBar.Size.X - spriteInset.X * 2.0f) * fillBar.Fill
            let spriteHeight = fillBar.Size.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        override dispatcher.Init (fillBar, dispatcherContainer) =
            let fillBar = base.Init (fillBar, dispatcherContainer)
            fillBar
                .SetFill(0.0f)
                .SetFillInset(0.0f)
                .SetFillSprite({ SpriteAssetName = "Image9"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetBorderSprite({ SpriteAssetName = "Image10"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                
        override dispatcher.GetRenderDescriptors (fillBar, viewAbsolute, _, _) =
            if not fillBar.Visible then []
            else
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = fillBarSpritePosition * viewAbsolute
                              Size = fillBarSpriteSize
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = fillBar.FillSprite
                              Color = Vector4.One }
                          Depth = fillBar.Depth }
                    LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = fillBar.Position * viewAbsolute
                              Size = fillBar.Size
                              Rotation = 0.0f
                              OptInset = None
                              Sprite = fillBar.BorderSprite
                              Color = Vector4.One }
                          Depth = fillBar.Depth }]

        override dispatcher.GetQuickSize (fillBar, world) =
            let sprite = fillBar.BorderSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module BlockDispatcherModule =

    type [<Sealed>] BlockDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (block : Entity) -> BoxShape { Extent = block.Size * 0.5f; Center = Vector2.Zero })

        override dispatcher.Init (block, dispatcherContainer) =
            let block = base.Init (block, dispatcherContainer)
            let block = SimpleSpriteFacet.init block dispatcherContainer
            block.SetImageSprite { SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.GetRenderDescriptors (block, viewAbsolute, viewRelative, _) =
            SimpleSpriteFacet.getRenderDescriptors block viewAbsolute viewRelative

        override dispatcher.GetQuickSize (block, world) =
            SimpleSpriteFacet.getQuickSize block world

[<AutoOpen>]
module AvatarDispatcherModule =

    type [<Sealed>] AvatarDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (avatar : Entity) -> CircleShape { Radius = avatar.Size.X * 0.5f; Center = Vector2.Zero })

        override dispatcher.Init (avatar, dispatcherContainer) =
            let avatar = base.Init (avatar, dispatcherContainer)
            let avatar = SimpleSpriteFacet.init avatar dispatcherContainer
            avatar
                .SetFixedRotation(true)
                .SetLinearDamping(10.0f)
                .SetGravityScale(0.0f)
                .SetImageSprite({ SpriteAssetName = "Image7"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.GetRenderDescriptors (avatar, viewAbsolute, viewRelative, _) =
            SimpleSpriteFacet.getRenderDescriptors avatar viewAbsolute viewRelative

        override dispatcher.GetQuickSize (avatar, world) =
            SimpleSpriteFacet.getQuickSize avatar world

[<AutoOpen>]
module CharacterDispatcherModule =

    type [<Sealed>] CharacterDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (character : Entity) -> CapsuleShape { Height = character.Size.Y * 0.5f; Radius = character.Size.Y * 0.25f; Center = Vector2.Zero })

        override dispatcher.Init (character, dispatcherContainer) =
            let character = base.Init (character, dispatcherContainer)
            let character = SimpleSpriteFacet.init character dispatcherContainer
            character
                .SetFixedRotation(true)
                .SetLinearDamping(3.0f)
                .SetImageSprite({ SpriteAssetName = "Image6"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.GetRenderDescriptors (character, viewAbsolute, viewRelative, _) =
            SimpleSpriteFacet.getRenderDescriptors character viewAbsolute viewRelative

        override dispatcher.GetQuickSize (character, world) =
            SimpleSpriteFacet.getQuickSize character world

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with

        [<XField>] member this.TileMapAsset with get () = this?TileMapAsset () : TileMapAsset
        member this.SetTileMapAsset (value : TileMapAsset) : Entity = this?TileMapAsset <- value
        [<XField>] member this.Parallax with get () = this?Parallax () : single
        member this.SetParallax (value : single) : Entity = this?Parallax <- value

        static member makeTileMapData tileMapAsset world =
            let (_, _, map) = Metadata.getTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap
            let mapSize = (map.Width, map.Height)
            let tileSize = (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single <| fst tileSize, single <| snd tileSize)
            let tileMapSize = (fst mapSize * fst tileSize, snd mapSize * snd tileSize)
            let tileMapSizeF = Vector2 (single <| fst tileMapSize, single <| snd tileMapSize)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

        static member makeTileData (tileMap : Entity) tmd (tl : TmxLayer) tileIndex =
            let mapRun = fst tmd.MapSize
            let tileSetRun = fst tmd.TileSetSize
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * fst tmd.TileSize
            let gid2 = (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tileMap.Position
            let tilePosition = (
                int tileMapPosition.X + fst tmd.TileSize * i,
                int tileMapPosition.Y - snd tmd.TileSize * (j + 1)) // subtraction for right-handedness
            let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    type [<Sealed>] TileMapDispatcher () =
        inherit Entity2dDispatcher ()

        let getTilePhysicsId tmid (tli : int) (ti : int) =
            let bytes = Array.create<byte> 8 <| byte 0
            BitConverter.GetBytes(ti : int).CopyTo(bytes, 0)
            PhysicsId (tmid, Guid (tli, int16 0, int16 0, bytes))

        let registerTilePhysicsBox address (tm : Entity) tmd tli td ti world =
            let physicsId = getTilePhysicsId tm.Id tli ti
            let createBodyMessage =
                CreateBodyMessage
                    { EntityAddress = address
                      PhysicsId = physicsId
                      Position =
                        Vector2 (
                            single <| fst td.TilePosition + fst tmd.TileSize / 2,
                            single <| snd td.TilePosition + snd tmd.TileSize / 2 + snd tmd.TileMapSize)
                      Rotation = tm.Rotation
                      BodyProperties =
                        { Shape = BoxShape { Extent = Vector2 (single <| fst tmd.TileSize, single <| snd tmd.TileSize) * 0.5f; Center = Vector2.Zero }
                          BodyType = BodyType.Static
                          Density = tm.Density
                          Friction = tm.Friction
                          Restitution = tm.Restitution
                          FixedRotation = true
                          LinearDamping = 0.0f
                          AngularDamping = 0.0f
                          GravityScale = 0.0f
                          CollisionCategories = Physics.toCollisionCategories tm.CollisionCategories
                          CollisionMask = Physics.toCollisionCategories tm.CollisionMask
                          IsBullet = false
                          IsSensor = false }}
            { world with PhysicsMessages = createBodyMessage :: world.PhysicsMessages }

        let registerTilePhysicsPolygon address (tm : Entity) tmd tli td ti vertices world =
            let physicsId = getTilePhysicsId tm.Id tli ti
            let createBodyMessage =
                CreateBodyMessage
                    { EntityAddress = address
                      PhysicsId = physicsId
                      Position =
                        Vector2 (
                            single <| fst td.TilePosition + fst tmd.TileSize / 2,
                            single <| snd td.TilePosition + snd tmd.TileSize / 2 + snd tmd.TileMapSize)
                      Rotation = tm.Rotation
                      BodyProperties =
                        { Shape = PolygonShape { Vertices = vertices; Center = Vector2.Zero }
                          BodyType = BodyType.Static
                          Density = tm.Density
                          Friction = tm.Friction
                          Restitution = tm.Restitution
                          FixedRotation = true
                          LinearDamping = 0.0f
                          AngularDamping = 0.0f
                          GravityScale = 0.0f
                          CollisionCategories = Physics.toCollisionCategories tm.CollisionCategories
                          CollisionMask = Physics.toCollisionCategories tm.CollisionMask
                          IsBullet = false
                          IsSensor = false }}
            { world with PhysicsMessages = createBodyMessage :: world.PhysicsMessages }

        let registerTilePhysics tm tmd (tl : TmxLayer) tli address ti world _ =
            let td = Entity.makeTileData tm tmd tl ti
            match td.OptTileSetTile with
            | None -> world
            | Some tileSetTile ->
                let collisionProperty = ref Unchecked.defaultof<string>
                if not <| tileSetTile.Properties.TryGetValue (CollisionProperty, collisionProperty) then world
                else
                    let collisionExpr = string collisionProperty.Value
                    let collisionTerms = List.ofArray <| collisionExpr.Split '?'
                    let collisionTermsTrimmed = List.map (fun (term : string) -> term.Trim ()) collisionTerms
                    match collisionTermsTrimmed with
                    | [""]
                    | ["Box"] -> registerTilePhysicsBox address tm tmd tli td ti world
                    | ["Polygon"; verticesStr] ->
                        let vertexStrs = List.ofArray <| verticesStr.Split '|'
                        try let vertices = List.map (fun str -> (TypeDescriptor.GetConverter (typeof<Vector2>)).ConvertFromString str :?> Vector2) vertexStrs
                            let verticesOffset = List.map (fun vertex -> vertex - Vector2 0.5f) vertices
                            let verticesScaled = List.map (fun vertex -> Vector2.Multiply (vertex, tmd.TileSizeF)) verticesOffset
                            registerTilePhysicsPolygon address tm tmd tli td ti verticesScaled world
                        with :? NotSupportedException ->
                            trace <| "Could not parse collision polygon vertices '" + verticesStr + "'. Format is 'Polygon ? 0.0;0.0 | 0.0;1.0 | 1.0;1.0 | 1.0;0.0'"
                            world
                    | _ ->
                        trace <| "Invalid tile collision shape expression '" + collisionExpr + "'."
                        world

        let registerTileLayerPhysics address tileMap tileMapData tileLayerIndex world (tileLayer : TmxLayer) =
            if not <| tileLayer.Properties.ContainsKey CollisionProperty then world
            else
                Seq.foldi
                    (registerTilePhysics tileMap tileMapData tileLayer tileLayerIndex address)
                    world
                    tileLayer.Tiles

        let registerTileMapPhysics address world =
            let tileMap = World.getEntity address world
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (registerTileLayerPhysics address tileMap tileMapData)
                world
                tileMapData.Map.Layers

        let unregisterTileMapPhysics address world =
            let tileMap = World.getEntity address world
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                    if not <| tileLayer.Properties.ContainsKey CollisionProperty then world
                    else
                        Seq.foldi
                            (fun tileIndex world _ ->
                                let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex
                                match tileData.OptTileSetTile with
                                | None -> world
                                | Some tileSetTile ->
                                    if not <| tileSetTile.Properties.ContainsKey CollisionProperty then world
                                    else
                                        let physicsId = getTilePhysicsId tileMap.Id tileLayerIndex tileIndex
                                        let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
                                        { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages })
                            world
                            tileLayer.Tiles)
                world
                tileMapData.Map.Layers
        
        override dispatcher.Init (tileMap, dispatcherContainer) =
            let tileMap = base.Init (tileMap, dispatcherContainer)
            tileMap
                .SetDensity(NormalDensity)
                .SetFriction(0.0f)
                .SetRestitution(0.0f)
                .SetCollisionCategories("1")
                .SetCollisionMask("*")
                .SetTileMapAsset({ TileMapAssetName = "TileMap"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetParallax(0.0f)

        override dispatcher.Register (address, world) =
            registerTileMapPhysics address world

        override dispatcher.Unregister (address, world) =
            unregisterTileMapPhysics address world
            
        override dispatcher.PropagatePhysics (address, world) =
            world |>
                unregisterTileMapPhysics address |>
                registerTileMapPhysics address

        override dispatcher.GetRenderDescriptors (tileMap, _, viewRelative, world) =
            if not tileMap.Visible then []
            else
                let tileMapAsset = tileMap.TileMapAsset
                match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
                | None -> []
                | Some (_, sprites, map) ->
                    let layers = List.ofSeq map.Layers
                    let viewScale = Matrix3.getScaleMatrix viewRelative
                    let tileSourceSize = (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight) * viewScale
                    List.mapi
                        (fun i (layer : TmxLayer) ->
                            let depth = tileMap.Depth + single i * 2.0f
                            let parallaxTranslation = tileMap.Parallax * depth * Matrix3.getTranslation viewRelative
                            let layeredTileLayerDescriptor =
                                LayeredTileLayerDescriptor
                                    { Descriptor =
                                        { Position = (tileMap.Position + parallaxTranslation) * viewRelative
                                          Size = Vector2.Zero
                                          Rotation = tileMap.Rotation
                                          MapSize = (map.Width, map.Height)
                                          Tiles = layer.Tiles
                                          TileSourceSize = tileSourceSize
                                          TileSize = tileSize
                                          TileMapSize = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                                          TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                          TileSetSprite = List.head sprites } // MAGIC_VALUE: for same reason as above
                                      Depth = depth } // MAGIC_VALUE: assumption
                            LayerableDescriptor layeredTileLayerDescriptor)
                        layers

        override dispatcher.GetQuickSize (tileMap, world) =
            let tileMapAsset = tileMap.TileMapAsset
            match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
            | None -> failwith "Unexpected match failure in Nu.World.TileMapDispatcher.GetQuickSize."
            | Some (_, _, map) -> Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)

    type GroupDispatcher () =

        abstract member Init : Group * IXDispatcherContainer -> Group
        default dispatcher.Init (group, _) = group
        
        abstract member Register : Address * Entity list * World -> World
        default dispatcher.Register (address, entities, world) = World.addEntities address entities world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (address, world) = World.clearEntities address world

    type TransitionDispatcher () =
        class end

    type ScreenDispatcher () =

        abstract member Register : Address * GroupDescriptor list * World -> World
        default dispatcher.Register (address, groupDescriptors, world) = World.addGroups address groupDescriptors world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (address, world) = World.clearGroups address world

    type GameDispatcher () =
        
        abstract member Register : World -> World
        default dispatcher.Register world = world