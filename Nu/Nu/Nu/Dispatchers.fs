namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.NuConstants

[<AutoOpen>]
module Entity2dDispatcherModule =

    type [<AbstractClass>] Entity2dDispatcher () =
        inherit EntityDispatcher ()

        override dispatcher.Init (entity2d, dispatcherContainer) =
            let entity2d = base.Init (entity2d, dispatcherContainer)
            let entity2d = Entity2dFacet.init entity2d dispatcherContainer
            entity2d |>
                Entity.setPosition Vector2.Zero |>
                Entity.setDepth 0.0f |>
                Entity.setSize DefaultEntitySize |>
                Entity.setRotation 0.0f

        override dispatcher.GetPickingPriority (entity, _) =
            Entity2dFacet.getPickingPriority entity

        abstract member PropagatePhysics : Address * World -> World
        default dispatcher.PropagatePhysics (_, world) = world

        abstract member HandleBodyTransformMessage : Address * BodyTransformMessage * World -> World
        default dispatcher.HandleBodyTransformMessage (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _) = []

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

    type [<AbstractClass>] SimpleBodyDispatcher () =
        inherit Entity2dDispatcher ()

        abstract member GetBodyShape : Entity * World -> BodyShape
        default dispatcher.GetBodyShape (entity, _) =
            BoxShape { Extent = entity.Size * 0.5f; Center = Vector2.Zero }

        override dispatcher.Init (entity, dispatcherContainer) =
            let entity = base.Init (entity, dispatcherContainer)
            SimpleBodyFacet.init entity dispatcherContainer

        override dispatcher.Register (address, world) =
            let getBodyShape = (fun entity world -> dispatcher.GetBodyShape (entity, world))
            SimpleBodyFacet.registerPhysics getBodyShape address world

        override dispatcher.Unregister (address, world) =
            SimpleBodyFacet.unregisterPhysics address world
            
        override dispatcher.PropagatePhysics (address, world) =
            let getBodyShape = (fun entity world -> dispatcher.GetBodyShape (entity, world))
            SimpleBodyFacet.propagatePhysics getBodyShape address world

        override dispatcher.HandleBodyTransformMessage (address, message, world) =
            SimpleBodyFacet.handleBodyTransformMessage address message world

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with

        member entity.IsDown with get () = entity?IsDown () : bool
        static member setIsDown (value : bool) (entity : Entity) : Entity = entity?IsDown <- value
        member entity.UpSprite with get () = entity?UpSprite () : Sprite
        static member setUpSprite (value : Sprite) (entity : Entity) : Entity = entity?UpSprite <- value
        member entity.DownSprite with get () = entity?DownSprite () : Sprite
        static member setDownSprite (value : Sprite) (entity : Entity) : Entity = entity?DownSprite <- value
        member entity.ClickSound with get () = entity?ClickSound () : Sound
        static member setClickSound (value : Sound) (entity : Entity) : Entity = entity?ClickSound <- value

    type [<Sealed>] ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleButtonEventDownMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let button = World.getEntity event.Subscriber world
                let mousePositionButton = Entity.mouseToEntity mouseButtonData.Position world button
                if button.Enabled && button.Visible then
                    if NuMath.isPointInBounds3 mousePositionButton button.Position button.Size then
                        let button = Entity.setIsDown true button
                        let world = World.setEntity event.Subscriber button world
                        let world = World.publish4 (straddr "Down" event.Subscriber) event.Subscriber NoData world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        let handleButtonEventUpMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let button = World.getEntity event.Subscriber world
                let mousePositionButton = Entity.mouseToEntity mouseButtonData.Position world button
                if button.Enabled && button.Visible then
                    let world =
                        let button = Entity.setIsDown false button
                        let world = World.setEntity event.Subscriber button world
                        World.publish4 (straddr "Up" event.Subscriber) event.Subscriber NoData world
                    if NuMath.isPointInBounds3 mousePositionButton button.Position button.Size && button.IsDown then
                        let world = World.publish4 (straddr "Click" event.Subscriber) event.Subscriber NoData world
                        let world = World.playSound button.ClickSound 1.0f world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        override dispatcher.Init (button, dispatcherContainer) =
            let button = base.Init (button, dispatcherContainer)
            button |>
                Entity.setIsDown false |>
                Entity.setUpSprite { SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setDownSprite { SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setClickSound { SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEventName address (CustomSub handleButtonEventDownMouseLeft) |>
                World.observe UpMouseLeftEventName address (CustomSub handleButtonEventUpMouseLeft)

        override dispatcher.GetRenderDescriptors (button, _) =
            if button.Visible then
                [LayerableDescriptor
                    { Depth = button.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = button.Position
                              Size = button.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = if button.IsDown then button.DownSprite else button.UpSprite
                              Color = Vector4.One }}]
            else []

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

        member entity.LabelSprite with get () = entity?LabelSprite () : Sprite
        static member setLabelSprite (value : Sprite) (entity : Entity) : Entity = entity?LabelSprite <- value

    type [<Sealed>] LabelDispatcher () =
        inherit GuiDispatcher ()
            
        override dispatcher.Init (label, dispatcherContainer) =
            let label = base.Init (label, dispatcherContainer)
            Entity.setLabelSprite { SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } label

        override dispatcher.GetRenderDescriptors (label, _) =
            if label.Visible then
                [LayerableDescriptor
                    { Depth = label.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = label.Position
                              Size = label.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = label.LabelSprite
                              Color = Vector4.One }}]
            else []

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

        member entity.BoxSprite with get () = entity?BoxSprite () : Sprite
        static member setBoxSprite (value : Sprite) (entity : Entity) : Entity = entity?BoxSprite <- value
        member entity.Text with get () = entity?Text () : string
        static member setText (value : string) (entity : Entity) : Entity = entity?Text <- value
        member entity.TextFont with get () = entity?TextFont () : Font
        static member setTextFont (value : Font) (entity : Entity) : Entity = entity?TextFont <- value
        member entity.TextOffset with get () = entity?TextOffset () : Vector2
        static member setTextOffset (value : Vector2) (entity : Entity) : Entity = entity?TextOffset <- value
        member entity.TextColor with get () = entity?TextColor () : Vector4
        static member setTextColor (value : Vector4) (entity : Entity) : Entity = entity?TextColor <- value

    type [<Sealed>] TextBoxDispatcher () =
        inherit GuiDispatcher ()
            
        override dispatcher.Init (textBox, dispatcherContainer) =
            let textBox = base.Init (textBox, dispatcherContainer)
            textBox |>
                Entity.setBoxSprite { SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setText String.Empty |>
                Entity.setTextFont { FontAssetName = "Font"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setTextOffset Vector2.Zero |>
                Entity.setTextColor Vector4.One

        override dispatcher.GetRenderDescriptors (textBox, _) =
            if textBox.Visible then
                [LayerableDescriptor
                    { Depth = textBox.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = textBox.Position
                              Size = textBox.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = textBox.BoxSprite
                              Color = Vector4.One }}
                 LayerableDescriptor
                    { Depth = textBox.Depth
                      LayeredDescriptor =
                        TextDescriptor
                            { Text = textBox.Text
                              Position = (textBox.Position + textBox.TextOffset)
                              Size = textBox.Size - textBox.TextOffset
                              ViewType = Absolute
                              Font = textBox.TextFont
                              Color = textBox.TextColor }}]
            else []

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

        member entity.IsOn with get () = entity?IsOn () : bool
        static member setIsOn (value : bool) (entity : Entity) : Entity = entity?IsOn <- value
        member entity.IsPressed with get () = entity?IsPressed () : bool
        static member setIsPressed (value : bool) (entity : Entity) : Entity = entity?IsPressed <- value
        member entity.OffSprite with get () = entity?OffSprite () : Sprite
        static member setOffSprite (value : Sprite) (entity : Entity) : Entity = entity?OffSprite <- value
        member entity.OnSprite with get () = entity?OnSprite () : Sprite
        static member setOnSprite (value : Sprite) (entity : Entity) : Entity = entity?OnSprite <- value
        member entity.ToggleSound with get () = entity?ToggleSound () : Sound
        static member setToggleSound (value : Sound) (entity : Entity) : Entity = entity?ToggleSound <- value

    type [<Sealed>] ToggleDispatcher () =
        inherit GuiDispatcher ()

        let handleToggleEventDownMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let toggle = World.getEntity event.Subscriber world
                let mousePositionToggle = Entity.mouseToEntity mouseButtonData.Position world toggle
                if toggle.Enabled && toggle.Visible then
                    if NuMath.isPointInBounds3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle = Entity.setIsPressed true toggle
                        let world = World.setEntity event.Subscriber toggle world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)
    
        let handleToggleEventUpMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let toggle = World.getEntity event.Subscriber world
                let mousePositionToggle = Entity.mouseToEntity mouseButtonData.Position world toggle
                if toggle.Enabled && toggle.Visible && toggle.IsPressed then
                    let toggle = Entity.setIsPressed false toggle
                    let world = World.setEntity event.Subscriber toggle world
                    if NuMath.isPointInBounds3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle = Entity.setIsOn (not toggle.IsOn) toggle
                        let world = World.setEntity event.Subscriber toggle world
                        let eventType = if toggle.IsOn then "On" else "Off" // TODO: make these constants
                        let world = World.publish4 (straddr eventType event.Subscriber) event.Subscriber NoData world
                        let world = World.playSound toggle.ToggleSound 1.0f world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)
        
        override dispatcher.Init (toggle, dispatcherContainer) =
            let toggle = base.Init (toggle, dispatcherContainer)
            toggle |>
                Entity.setIsOn false |>
                Entity.setIsPressed false |>
                Entity.setOffSprite { SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setOnSprite { SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setToggleSound { SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEventName address (CustomSub handleToggleEventDownMouseLeft) |>
                World.observe UpMouseLeftEventName address (CustomSub handleToggleEventUpMouseLeft)

        override dispatcher.GetRenderDescriptors (toggle, _) =
            if toggle.Visible then
                [LayerableDescriptor
                    { Depth = toggle.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = toggle.Position
                              Size = toggle.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = if toggle.IsOn || toggle.IsPressed then toggle.OnSprite else toggle.OffSprite
                              Color = Vector4.One }}]
            else []

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

        member entity.IsTouched with get () = entity?IsTouched () : bool
        static member setIsTouched (value : bool) (entity : Entity) : Entity = entity?IsTouched <- value

    type [<Sealed>] FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleFeelerEventDownMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let feeler = World.getEntity event.Subscriber world
                let mousePositionFeeler = Entity.mouseToEntity mouseButtonData.Position world feeler
                if feeler.Enabled && feeler.Visible then
                    if NuMath.isPointInBounds3 mousePositionFeeler feeler.Position feeler.Size then
                        let feeler = Entity.setIsTouched true feeler
                        let world = World.setEntity event.Subscriber feeler world
                        let world = World.publish4 (straddr "Touch" event.Subscriber) event.Subscriber (MouseButtonData mouseButtonData) world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)
    
        let handleFeelerEventUpMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let feeler = World.getEntity event.Subscriber world
                if feeler.Enabled && feeler.Visible then
                    let feeler = Entity.setIsTouched false feeler
                    let world = World.setEntity event.Subscriber feeler world
                    let world = World.publish4 (straddr "Release" event.Subscriber) event.Subscriber NoData world
                    (Handled, world)
                else (Unhandled, world)
            else (Unhandled, world)
        
        override dispatcher.Init (feeler, dispatcherContainer) =
            let feeler = base.Init (feeler, dispatcherContainer)
            Entity.setIsTouched false feeler

        override dispatcher.Register (address, world) =
            world |>
                World.observe DownMouseLeftEventName address (CustomSub handleFeelerEventDownMouseLeft) |>
                World.observe UpMouseLeftEventName address (CustomSub handleFeelerEventUpMouseLeft)

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member entity.Fill with get () = entity?Fill () : single
        static member setFill (value : single) (entity : Entity) : Entity = entity?Fill <- value
        member entity.FillInset with get () = entity?FillInset () : single
        static member setFillInset (value : single) (entity : Entity) : Entity = entity?FillInset <- value
        member entity.FillSprite with get () = entity?FillSprite () : Sprite
        static member setFillSprite (value : Sprite) (entity : Entity) : Entity = entity?FillSprite <- value
        member entity.BorderSprite with get () = entity?BorderSprite () : Sprite
        static member setBorderSprite (value : Sprite) (entity : Entity) : Entity = entity?BorderSprite <- value

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
            fillBar |>
                Entity.setFill 0.0f |>
                Entity.setFillInset 0.0f |>
                Entity.setFillSprite { SpriteAssetName = "Image9"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setBorderSprite { SpriteAssetName = "Image10"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }
                
        override dispatcher.GetRenderDescriptors (fillBar, _) =
            if fillBar.Visible then
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar
                [LayerableDescriptor
                    { Depth = fillBar.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = fillBarSpritePosition
                              Size = fillBarSpriteSize
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = fillBar.FillSprite
                              Color = Vector4.One }}
                 LayerableDescriptor
                    { Depth = fillBar.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = fillBar.Position
                              Size = fillBar.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Sprite = fillBar.BorderSprite
                              Color = Vector4.One }}]
            else []

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
        inherit SimpleBodyDispatcher ()

        override dispatcher.Init (block, dispatcherContainer) =
            let block = base.Init (block, dispatcherContainer)
            let block = SimpleSpriteFacet.init block dispatcherContainer
            Entity.setImageSprite { SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } block

        override dispatcher.GetRenderDescriptors (block, world) =
            SimpleSpriteFacet.getRenderDescriptors block Relative world

        override dispatcher.GetQuickSize (block, world) =
            SimpleSpriteFacet.getQuickSize block world

        override dispatcher.GetBodyShape (block, _) =
            BoxShape { Extent = block.Size * 0.5f; Center = Vector2.Zero }

[<AutoOpen>]
module AvatarDispatcherModule =

    type [<Sealed>] AvatarDispatcher () =
        inherit SimpleBodyDispatcher ()

        override dispatcher.Init (avatar, dispatcherContainer) =
            let avatar = base.Init (avatar, dispatcherContainer)
            let avatar = SimpleSpriteFacet.init avatar dispatcherContainer
            avatar |>
                Entity.setFixedRotation true |>
                Entity.setLinearDamping 10.0f |>
                Entity.setGravityScale 0.0f |>
                Entity.setImageSprite { SpriteAssetName = "Image7"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.GetRenderDescriptors (avatar, world) =
            SimpleSpriteFacet.getRenderDescriptors avatar Relative world

        override dispatcher.GetQuickSize (avatar, world) =
            SimpleSpriteFacet.getQuickSize avatar world

        override dispatcher.GetBodyShape (avatar, _) =
            CircleShape { Radius = avatar.Size.X * 0.5f; Center = Vector2.Zero }

[<AutoOpen>]
module CharacterDispatcherModule =

    type [<Sealed>] CharacterDispatcher () =
        inherit SimpleBodyDispatcher ()

        override dispatcher.Init (character, dispatcherContainer) =
            let character = base.Init (character, dispatcherContainer)
            let character = SimpleSpriteFacet.init character dispatcherContainer
            character |>
                Entity.setFixedRotation true |>
                Entity.setLinearDamping 3.0f |>
                Entity.setImageSprite { SpriteAssetName = "Image6"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName }

        override dispatcher.GetRenderDescriptors (character, world) =
            SimpleSpriteFacet.getRenderDescriptors character Relative world

        override dispatcher.GetQuickSize (character, world) =
            SimpleSpriteFacet.getQuickSize character world

        override dispatcher.GetBodyShape (character, _) =
            CapsuleShape { Height = character.Size.Y * 0.5f; Radius = character.Size.Y * 0.25f; Center = Vector2.Zero }

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with

        member entity.TileMapAsset with get () = entity?TileMapAsset () : TileMapAsset
        static member setTileMapAsset (value : TileMapAsset) (entity : Entity) : Entity = entity?TileMapAsset <- value
        member entity.Parallax with get () = entity?Parallax () : single
        static member setParallax (value : single) (entity : Entity) : Entity = entity?Parallax <- value

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
            PhysicsId (tmid, intsToGuid tli ti)

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
                if tileSetTile.Properties.TryGetValue (CollisionProperty, collisionProperty) then
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
                else world

        let registerTileLayerPhysics address tileMap tileMapData tileLayerIndex world (tileLayer : TmxLayer) =
            if tileLayer.Properties.ContainsKey CollisionProperty then
                Seq.foldi
                    (registerTilePhysics tileMap tileMapData tileLayer tileLayerIndex address)
                    world
                    tileLayer.Tiles
            else world

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
                    if tileLayer.Properties.ContainsKey CollisionProperty then
                        Seq.foldi
                            (fun tileIndex world _ ->
                                let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex
                                match tileData.OptTileSetTile with
                                | None -> world
                                | Some tileSetTile ->
                                    if tileSetTile.Properties.ContainsKey CollisionProperty then
                                        let physicsId = getTilePhysicsId tileMap.Id tileLayerIndex tileIndex
                                        let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
                                        { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }
                                    else world)
                            world
                            tileLayer.Tiles
                    else world)
                world
                tileMapData.Map.Layers
        
        override dispatcher.Init (tileMap, dispatcherContainer) =
            let tileMap = base.Init (tileMap, dispatcherContainer)
            tileMap |>
                Entity.setDensity NormalDensity |>
                Entity.setFriction 0.0f |>
                Entity.setRestitution 0.0f |>
                Entity.setCollisionCategories "1" |>
                Entity.setCollisionMask "*" |>
                Entity.setTileMapAsset { TileMapAssetName = "TileMap"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setParallax 0.0f

        override dispatcher.Register (address, world) =
            registerTileMapPhysics address world

        override dispatcher.Unregister (address, world) =
            unregisterTileMapPhysics address world
            
        override dispatcher.PropagatePhysics (address, world) =
            world |>
                unregisterTileMapPhysics address |>
                registerTileMapPhysics address

        override dispatcher.GetRenderDescriptors (tileMap, world) =
            if not tileMap.Visible then []
            else
                let tileMapAsset = tileMap.TileMapAsset
                match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
                | None -> []
                | Some (_, sprites, map) ->
                    let layers = List.ofSeq map.Layers
                    let tileSourceSize = (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                    List.foldi
                        (fun i descriptors (layer : TmxLayer) ->
                            let depth = tileMap.Depth + single i * 2.0f
                            let parallaxTranslation = tileMap.Parallax * depth * -world.Camera.EyeCenter
                            let parallaxPosition = tileMap.Position + parallaxTranslation
                            let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                            if Camera.inView3 parallaxPosition size world.Camera then
                                let descriptor =
                                    LayerableDescriptor 
                                        { Depth = depth // MAGIC_VALUE: assumption
                                          LayeredDescriptor =
                                            TileLayerDescriptor
                                                { Position = parallaxPosition
                                                  Size = size
                                                  Rotation = tileMap.Rotation
                                                  ViewType = Relative
                                                  MapSize = (map.Width, map.Height)
                                                  Tiles = layer.Tiles
                                                  TileSourceSize = tileSourceSize
                                                  TileSize = tileSize
                                                  TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                                  TileSetSprite = List.head sprites }} // MAGIC_VALUE: for same reason as above
                                descriptor :: descriptors
                            else descriptors)
                        []
                        layers

        override dispatcher.GetQuickSize (tileMap, world) =
            let tileMapAsset = tileMap.TileMapAsset
            match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
            | None -> failwith "Unexpected match failure in Nu.World.TileMapDispatcher.GetQuickSize."
            | Some (_, _, map) -> Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)