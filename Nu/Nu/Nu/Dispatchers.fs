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
module DispatchersModule =

    type Entity with
        
        (* button xfields *)
        [<XField>] member this.IsDown with get () = this?IsDown () : bool
        member this.SetIsDown (value : bool) : Entity = this?IsDown <- value
        [<XField>] member this.UpSprite with get () = this?UpSprite () : Sprite
        member this.SetUpSprite (value : Sprite) : Entity = this?UpSprite <- value
        [<XField>] member this.DownSprite with get () = this?DownSprite () : Sprite
        member this.SetDownSprite (value : Sprite) : Entity = this?DownSprite <- value
        [<XField>] member this.ClickSound with get () = this?ClickSound () : Sound
        member this.SetClickSound (value : Sound) : Entity = this?ClickSound <- value

        (* label xfields *)
        [<XField>] member this.LabelSprite with get () = this?LabelSprite () : Sprite
        member this.SetLabelSprite (value : Sprite) : Entity = this?LabelSprite <- value

        (* text box xfields *)
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

        (* toggle xfields *)
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

        (* feeler xfields *)
        [<XField>] member this.IsTouched with get () = this?IsTouched () : bool
        member this.SetIsTouched (value : bool) : Entity = this?IsTouched <- value

        (* fill bar xfields *)
        [<XField>] member this.Fill with get () = this?Fill () : single
        member this.SetFill (value : single) : Entity = this?Fill <- value
        [<XField>] member this.FillInset with get () = this?FillInset () : single
        member this.SetFillInset (value : single) : Entity = this?FillInset <- value
        [<XField>] member this.FillSprite with get () = this?FillSprite () : Sprite
        member this.SetFillSprite (value : Sprite) : Entity = this?FillSprite <- value
        [<XField>] member this.BorderSprite with get () = this?BorderSprite () : Sprite
        member this.SetBorderSprite (value : Sprite) : Entity = this?BorderSprite <- value

        (* block xfields *)
        [<XField>] member this.PhysicsId with get () = this?PhysicsId () : PhysicsId
        member this.SetPhysicsId (value : PhysicsId) : Entity = this?PhysicsId <- value
        [<XField>] member this.Density with get () = this?Density () : single
        member this.SetDensity (value : single) : Entity = this?Density <- value
        [<XField>] member this.BodyType with get () = this?BodyType () : BodyType
        member this.SetBodyType (value : BodyType) : Entity = this?BodyType <- value
        [<XField>] member this.ImageSprite with get () = this?ImageSprite () : Sprite
        member this.SetImageSprite (value : Sprite) : Entity = this?ImageSprite <- value

        (* avatar xfields *)
        [<XField>] member this.LinearDamping with get () = this?LinearDamping () : single
        member this.SetLinearDamping (value : single) : Entity = this?LinearDamping <- value

        (* character xfields *)
        [<XField>] member this.Radius with get () = this?Radius () : single
        member this.SetRadius (value : single) : Entity = this?Radius <- value

        (* tile map xfields *)
        [<XField>] member this.PhysicsIds with get () = this?PhysicsIds () : PhysicsId list
        member this.SetPhysicsIds (value : PhysicsId list) : Entity = this?PhysicsIds <- value
        [<XField>] member this.TileMapAsset with get () = this?TileMapAsset () : TileMapAsset
        member this.SetTileMapAsset (value : TileMapAsset) : Entity = this?TileMapAsset <- value

    type EntityDispatcher () =

        abstract member Init : Entity * IXDispatcherContainer -> Entity
        default this.Init (entity, dispatcherContainer) = entity

        abstract member Register : Entity * Address * World -> Entity * World
        default this.Register (entity, address, world) = (entity, world)

        abstract member Unregister : Entity * Address * World -> World
        default this.Unregister (entity, address, world) = world

    type Entity2dDispatcher () =
        inherit EntityDispatcher ()
            
        override this.Init (entity2d, dispatcherContainer) =
            let entity2d' = base.Init (entity2d, dispatcherContainer)
            // perhaps a nice 'with' syntax macro would work better here -
            // http://fslang.uservoice.com/forums/245727-f-language/suggestions/5674940-implement-syntactic-macros
            entity2d'
                .SetPosition(Vector2.Zero)
                .SetDepth(0.0f)
                .SetSize(DefaultEntitySize)
                .SetRotation(0.0f)

        abstract member PropagatePhysics : Entity * Address * World -> World
        default this.PropagatePhysics (entity, address, world) = world

        abstract member ReregisterPhysicsHack : Entity * Address * World -> World
        default this.ReregisterPhysicsHack (entity, groupAddress, world) = world

        abstract member HandleBodyTransformMessage : Entity * BodyTransformMessage * Address * World -> World
        default this.HandleBodyTransformMessage (entity, message, address, world) = world

        abstract member GetRenderDescriptors : Entity * Matrix3 * Matrix3 * World -> RenderDescriptor list
        default this.GetRenderDescriptors (entity, viewAbsolute, viewRelative, world) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default this.GetQuickSize (entity, world) = DefaultEntitySize

        abstract member IsTransformRelative : Entity * World -> bool
        default this.IsTransformRelative (entity, world) = true

    type ButtonDispatcher () =
        inherit Entity2dDispatcher ()

        let handleButtonEventDownMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let button = get world <| World.worldEntity subscriber
                let mousePositionButton = Entity.mouseToEntity mousePosition world button
                if button.Enabled && button.Visible then
                    if NuMath.isInBox3 mousePositionButton button.Position button.Size then
                        let button' = button.SetIsDown true
                        let world' = set button' world <| World.worldEntity subscriber
                        let (keepRunning, world'') = World.publish (straddr "Down" subscriber) subscriber { Handled = false; Data = NoData } world'
                        (Message.handle message, keepRunning, world'')
                    else (message, true, world)
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")

        let handleButtonEventUpMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let button = get world <| World.worldEntity subscriber
                let mousePositionButton = Entity.mouseToEntity mousePosition world button
                if button.Enabled && button.Visible then
                    let (keepRunning, world') =
                        let button' = button.SetIsDown false
                        let world'' = set button' world <| World.worldEntity subscriber
                        World.publish (straddr "Up" subscriber) subscriber { Handled = false; Data = NoData } world''
                    if keepRunning && NuMath.isInBox3 mousePositionButton button.Position button.Size && button.IsDown then
                        let (keepRunning', world'') = World.publish (straddr "Click" subscriber) subscriber { Handled = false; Data = NoData } world'
                        let sound = PlaySound { Volume = 1.0f; Sound = button.ClickSound }
                        let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                        (Message.handle message, keepRunning', world'3)
                    else (message, keepRunning, world')
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")

        override dispatcher.Init (button, dispatcherContainer) =
            let button' = base.Init (button, dispatcherContainer)
            button'
                .SetIsDown(false)
                .SetUpSprite({ SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetDownSprite({ SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetClickSound({ SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (button, address, world) =
            let world' =
                world |>
                    World.subscribe NuConstants.DownMouseLeftEvent address -<| CustomSub handleButtonEventDownMouseLeft |>
                    World.subscribe UpMouseLeftEvent address -<| CustomSub handleButtonEventUpMouseLeft
            (button, world')

        override dispatcher.Unregister (button, address, world) =
            world |>
                World.unsubscribe DownMouseLeftEvent address |>
                World.unsubscribe UpMouseLeftEvent address

        override dispatcher.GetRenderDescriptors (button, viewAbsolute, viewRelative, world) =
            if not button.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = button.Position * viewAbsolute
                              Size = button.Size
                              Rotation = 0.0f
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

    type LabelDispatcher () =
        inherit Entity2dDispatcher ()
            
        override dispatcher.Init (label, dispatcherContainer) =
            let label' = base.Init (label, dispatcherContainer)
            label'.SetLabelSprite({ SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.GetRenderDescriptors (label, viewAbsolute, viewRelative, world) =
            if not label.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = label.Position * viewAbsolute
                              Size = label.Size
                              Rotation = 0.0f
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

    type TextBoxDispatcher () =
        inherit Entity2dDispatcher ()
            
        override dispatcher.Init (textBox, dispatcherContainer) =
            let textBox' = base.Init (textBox, dispatcherContainer)
            textBox'
                .SetBoxSprite({ SpriteAssetName = "Image4"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetText(String.Empty)
                .SetTextFont({ FontAssetName = "Font"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetTextOffset(Vector2.Zero)
                .SetTextColor(Vector4.One)

        override dispatcher.GetRenderDescriptors (textBox, viewAbsolute, viewRelative, world) =
            if not textBox.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = textBox.Position * viewAbsolute
                              Size = textBox.Size
                              Rotation = 0.0f
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

    type ToggleDispatcher () =
        inherit Entity2dDispatcher ()

        let handleToggleEventDownMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let toggle = get world <| World.worldEntity subscriber
                let mousePositionToggle = Entity.mouseToEntity mousePosition world toggle
                if toggle.Enabled && toggle.Visible then
                    if NuMath.isInBox3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle' = toggle.SetIsPressed true
                        let world' = set toggle' world <| World.worldEntity subscriber
                        (Message.handle message, true, world')
                    else (message, true, world)
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")
    
        let handleToggleEventUpMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) ->
                let toggle = get world <| World.worldEntity subscriber
                let mousePositionToggle = Entity.mouseToEntity mousePosition world toggle
                if toggle.Enabled && toggle.Visible && toggle.IsPressed then
                    let toggle' = toggle.SetIsPressed false
                    if NuMath.isInBox3 mousePositionToggle toggle'.Position toggle'.Size then
                        let toggle'' = toggle'.SetIsOn <| not toggle'.IsOn
                        let world' = set toggle'' world <| World.worldEntity subscriber
                        let messageType = if toggle''.IsOn then "On" else "Off"
                        let (keepRunning, world'') = World.publish (straddr messageType subscriber) subscriber { Handled = false; Data = NoData } world'
                        let sound = PlaySound { Volume = 1.0f; Sound = toggle''.ToggleSound }
                        let world'3 = { world'' with AudioMessages = sound :: world''.AudioMessages }
                        (Message.handle message, keepRunning, world'3)
                    else
                        let world' = set toggle' world <| World.worldEntity subscriber
                        (message, true, world')
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")
        
        override dispatcher.Init (toggle, dispatcherContainer) =
            let toggle' = base.Init (toggle, dispatcherContainer)
            toggle'
                .SetIsOn(false)
                .SetIsPressed(false)
                .SetOffSprite({ SpriteAssetName = "Image"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetOnSprite({ SpriteAssetName = "Image2"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetToggleSound({ SoundAssetName = "Sound"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (toggle, address, world) =
            let world' =
                world |>
                    World.subscribe DownMouseLeftEvent address -<| CustomSub handleToggleEventDownMouseLeft |>
                    World.subscribe UpMouseLeftEvent address -<| CustomSub handleToggleEventUpMouseLeft
            (toggle, world')

        override dispatcher.Unregister (toggle, address, world) =
            world |>
                World.unsubscribe DownMouseLeftEvent address |>
                World.unsubscribe UpMouseLeftEvent address

        override dispatcher.GetRenderDescriptors (toggle, viewAbsolute, viewRelative, world) =
            if not toggle.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = toggle.Position * viewAbsolute
                              Size = toggle.Size
                              Rotation = 0.0f
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

    type FeelerDispatcher () =
        inherit Entity2dDispatcher ()

        let handleFeelerEventDownMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData (mousePosition, _) as mouseButtonData ->
                let feeler = get world <| World.worldEntity subscriber
                let mousePositionFeeler = Entity.mouseToEntity mousePosition world feeler
                if feeler.Enabled && feeler.Visible then
                    if NuMath.isInBox3 mousePositionFeeler feeler.Position feeler.Size then
                        let feeler' = feeler.SetIsTouched true
                        let world' = set feeler' world <| World.worldEntity subscriber
                        let (keepRunning, world'') = World.publish (straddr "Touch" subscriber) subscriber { Handled = false; Data = mouseButtonData } world'
                        (Message.handle message, keepRunning, world'')
                    else (message, true, world)
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")
    
        let handleFeelerEventUpMouseLeft event publisher subscriber message world =
            match message.Data with
            | MouseButtonData _ ->
                let feeler = get world <| World.worldEntity subscriber
                if feeler.Enabled && feeler.Visible then
                    let feeler' = feeler.SetIsTouched false
                    let world' = set feeler' world <| World.worldEntity subscriber
                    let (keepRunning, world'') = World.publish (straddr "Release" subscriber) subscriber { Handled = false; Data = NoData } world'
                    (Message.handle message, keepRunning, world'')
                else (message, true, world)
            | _ -> failwith ("Expected MouseButtonData from event '" + addrToStr event + "'.")
        
        override dispatcher.Init (feeler, dispatcherContainer) =
            let feeler' = base.Init (feeler, dispatcherContainer)
            feeler'.SetIsTouched(false)

        override dispatcher.Register (feeler, address, world) =
            let world' =
                world |>
                    World.subscribe DownMouseLeftEvent address -<| CustomSub handleFeelerEventDownMouseLeft |>
                    World.subscribe UpMouseLeftEvent address -<| CustomSub handleFeelerEventUpMouseLeft
            (feeler, world')

        override dispatcher.Unregister (feeler, address, world) =
            world |>
                World.unsubscribe UpMouseLeftEvent address |>
                World.unsubscribe DownMouseLeftEvent address

        override dispatcher.GetQuickSize (feeler, world) =
            Vector2 64.0f

        override dispatcher.IsTransformRelative (_, _) =
            false

    type FillBarDispatcher () =
        inherit Entity2dDispatcher ()

        let getFillBarSpriteDims (fillBar : Entity) =
            let spriteInset = fillBar.Size * fillBar.FillInset * 0.5f
            let spritePosition = fillBar.Position + spriteInset
            let spriteWidth = (fillBar.Size.X - spriteInset.X * 2.0f) * fillBar.Fill
            let spriteHeight = fillBar.Size.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        override dispatcher.Init (fillBar, dispatcherContainer) =
            let fillBar' = base.Init (fillBar, dispatcherContainer)
            fillBar'
                .SetFill(0.0f)
                .SetFillInset(0.0f)
                .SetFillSprite({ SpriteAssetName = "Image9"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })
                .SetBorderSprite({ SpriteAssetName = "Image10"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.GetRenderDescriptors (fillBar, viewAbsolute, viewRelative, world) =
            if not fillBar.Visible then []
            else
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = fillBarSpritePosition * viewAbsolute
                              Size = fillBarSpriteSize
                              Rotation = 0.0f
                              Sprite = fillBar.FillSprite
                              Color = Vector4.One }
                          Depth = fillBar.Depth }
                    LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = fillBar.Position * viewAbsolute
                              Size = fillBar.Size
                              Rotation = 0.0f
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

    type BlockDispatcher () =
        inherit Entity2dDispatcher ()

        let registerBlockPhysics address (block : Entity) world =
            let block' = block.SetPhysicsId <| Physics.getId block.Id
            let createBodyMessage =
                { EntityAddress = address
                  PhysicsId = block'.PhysicsId
                  Shape = BoxShape
                    { Extent = block'.Size * 0.5f
                      Properties =
                        { Center = Vector2.Zero
                          Friction = 0.2f
                          Restitution = 0.0f
                          FixedRotation = false
                          LinearDamping = 5.0f
                          AngularDamping = 5.0f
                          GravityScale = 1.0f }}
                  Position = block'.Position + block'.Size * 0.5f
                  Rotation = block'.Rotation
                  Density = block'.Density
                  BodyType = block'.BodyType }
            let world' = { world with PhysicsMessages = CreateBodyMessage createBodyMessage :: world.PhysicsMessages }
            (block', world')

        let unregisterBlockPhysics (_ : Address) (block : Entity) world =
            let destroyBodyMessage = { DestroyBodyMessage.PhysicsId = block.PhysicsId }
            { world with PhysicsMessages = DestroyBodyMessage destroyBodyMessage :: world.PhysicsMessages }

        override dispatcher.Init (block, dispatcherContainer) =
            let block' = base.Init (block, dispatcherContainer)
            block'
                .SetPhysicsId(Physics.InvalidId)
                .SetDensity(NormalDensity)
                .SetBodyType(BodyType.Dynamic)
                .SetImageSprite({ SpriteAssetName = "Image3"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (block, address, world) =
            registerBlockPhysics address block world

        override dispatcher.Unregister (block, address, world) =
            unregisterBlockPhysics address block world
            
        override dispatcher.PropagatePhysics (block, address, world) =
            let (block', world') = world |> unregisterBlockPhysics address block |> registerBlockPhysics address block
            set block' world' <| World.worldEntity address

        override dispatcher.ReregisterPhysicsHack (block, groupAddress, world) =
            let address = addrstr groupAddress block.Name
            let world' = unregisterBlockPhysics address block world
            let (block', world'') = registerBlockPhysics address block world'
            set block' world'' <| World.worldEntity address

        override dispatcher.HandleBodyTransformMessage (block, message, address, world) =
            let block' =
                block
                    .SetPosition(message.Position - block.Size * 0.5f) // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                    .SetRotation(message.Rotation)
            set block' world <| World.worldEntity message.EntityAddress
            
        override dispatcher.GetRenderDescriptors (block, viewAbsolute, viewRelative, world) =
            if not block.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = block.Position * viewRelative
                              Size = block.Size * Matrix3.getScaleMatrix viewAbsolute
                              Rotation = block.Rotation
                              Sprite = block.ImageSprite
                              Color = Vector4.One }
                          Depth = block.Depth }]

        override dispatcher.GetQuickSize (block, world) =
            let sprite = block.ImageSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size
    
    type AvatarDispatcher () =
        inherit Entity2dDispatcher ()

        let registerAvatarPhysics address (avatar : Entity) world =
            let avatar' = avatar.SetPhysicsId <| Physics.getId avatar.Id
            let createBodyMessage =
                { EntityAddress = address
                  PhysicsId = avatar'.PhysicsId
                  Shape =
                    CircleShape
                        { Radius = avatar'.Size.X * 0.5f
                          Properties =
                            { Center = Vector2.Zero
                              Friction = 0.2f
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = avatar'.LinearDamping
                              AngularDamping = 0.0f
                              GravityScale = 1.0f }}
                  Position = avatar'.Position + avatar'.Size * 0.5f
                  Rotation = avatar'.Rotation
                  Density = avatar'.Density
                  BodyType = BodyType.Dynamic }
            let world' = { world with PhysicsMessages = CreateBodyMessage createBodyMessage :: world.PhysicsMessages }
            (avatar', world')

        let unregisterAvatarPhysics (avatar : Entity) world =
            let destroyBodyMessage = { DestroyBodyMessage.PhysicsId = avatar.PhysicsId }
            { world with PhysicsMessages = DestroyBodyMessage destroyBodyMessage :: world.PhysicsMessages }

        override dispatcher.Init (avatar, dispatcherContainer) =
            let avatar' = base.Init (avatar, dispatcherContainer)
            avatar'
                .SetPhysicsId(Physics.InvalidId)
                .SetLinearDamping(10.0f)
                .SetDensity(NormalDensity)
                .SetImageSprite({ SpriteAssetName = "Image7"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (avatar, address, world) =
            registerAvatarPhysics address avatar world

        override dispatcher.Unregister (avatar, address, world) =
            unregisterAvatarPhysics avatar world
            
        override dispatcher.PropagatePhysics (avatar, address, world) =
            let (avatar', world') = world |> unregisterAvatarPhysics avatar |> registerAvatarPhysics address avatar
            set avatar' world' <| World.worldEntity address

        override dispatcher.ReregisterPhysicsHack (avatar, groupAddress, world) =
            let address = addrstr groupAddress avatar.Name
            let world' = unregisterAvatarPhysics avatar world
            let (avatar', world'') = registerAvatarPhysics address avatar world'
            set avatar' world'' <| World.worldEntity address

        override dispatcher.HandleBodyTransformMessage (avatar, message, address, world) =
            let avatar' =
                avatar
                    .SetPosition(message.Position - avatar.Size * 0.5f) // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                    .SetRotation(message.Rotation)
            set avatar' world <| World.worldEntity message.EntityAddress

        override dispatcher.GetRenderDescriptors (avatar, viewAbsolute, viewRelative, world) =
            if not avatar.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = avatar.Position * viewRelative
                              Size = avatar.Size * Matrix3.getScaleMatrix viewAbsolute
                              Rotation = avatar.Rotation
                              Sprite = avatar.ImageSprite
                              Color = Vector4.One }
                          Depth = avatar.Depth }]

        override dispatcher.GetQuickSize (avatar, world) =
            let sprite = avatar.ImageSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> DefaultEntitySize
            | Some size -> size

    type CharacterDispatcher () =
        inherit Entity2dDispatcher ()

        let registerCharacterPhysics address (character : Entity) world =
            let character' = character.SetPhysicsId <| Physics.getId character.Id
            let createBodyMessage =
                { EntityAddress = address
                  PhysicsId = character'.PhysicsId
                  Shape =
                    CapsuleShape
                        { Height = character'.Size.Y * 0.5f
                          Radius = character'.Radius
                          Properties =
                            { Center = Vector2.Zero
                              Friction = 0.2f
                              Restitution = 0.0f
                              FixedRotation = true
                              LinearDamping = character'.LinearDamping
                              AngularDamping = 0.0f
                              GravityScale = 1.0f }}
                  Position = character'.Position + character'.Size * 0.5f
                  Rotation = character'.Rotation
                  Density = character'.Density
                  BodyType = BodyType.Dynamic }
            let world' = { world with PhysicsMessages = CreateBodyMessage createBodyMessage :: world.PhysicsMessages }
            (character', world')

        let unregisterCharacterPhysics (character : Entity) world =
            let destroyBodyMessage = { DestroyBodyMessage.PhysicsId = character.PhysicsId }
            { world with PhysicsMessages = DestroyBodyMessage destroyBodyMessage :: world.PhysicsMessages }

        override dispatcher.Init (character, dispatcherContainer) =
            let character' = base.Init (character, dispatcherContainer)
            character'
                .SetPhysicsId(Physics.InvalidId)
                .SetRadius(NuConstants.DefaultEntitySize.X * 0.25f)
                .SetLinearDamping(3.0f)
                .SetDensity(NuConstants.NormalDensity)
                .SetImageSprite({ SpriteAssetName = "Image6"; PackageName = NuConstants.DefaultPackageName; PackageFileName = NuConstants.AssetGraphFileName })

        override dispatcher.Register (character, address, world) =
            registerCharacterPhysics address character world

        override dispatcher.Unregister (character, address, world) =
            unregisterCharacterPhysics character world
            
        override dispatcher.PropagatePhysics (character, address, world) =
            let (character', world') = world |> unregisterCharacterPhysics character |> registerCharacterPhysics address character
            set character' world' <| World.worldEntity address

        override dispatcher.ReregisterPhysicsHack (character, groupAddress, world) =
            let address = addrstr groupAddress character.Name
            let world' = unregisterCharacterPhysics character world
            let (character', world'') = registerCharacterPhysics address character world'
            set character' world'' <| World.worldEntity address

        override dispatcher.HandleBodyTransformMessage (character, message, address, world) =
            let character' =
                character
                    .SetPosition(message.Position - character.Size * 0.5f) // TODO: see if this center-offsetting can be encapsulated within the Physics module!
                    .SetRotation(message.Rotation)
            set character' world <| World.worldEntity message.EntityAddress

        override dispatcher.GetRenderDescriptors (character, viewAbsolute, viewRelative, world) =
            if not character.Visible then []
            else
                [LayerableDescriptor <|
                    LayeredSpriteDescriptor
                        { Descriptor =
                            { Position = character.Position * viewRelative
                              Size = character.Size * Matrix3.getScaleMatrix viewAbsolute
                              Rotation = character.Rotation
                              Sprite = character.ImageSprite
                              Color = Vector4.One }
                          Depth = character.Depth }]

        override dispatcher.GetQuickSize (character, world) =
            let sprite = character.ImageSprite
            match Metadata.tryGetTextureSizeAsVector2 sprite.SpriteAssetName sprite.PackageName world.AssetMetadataMap with
            | None -> NuConstants.DefaultEntitySize
            | Some size -> size

    type TileMapDispatcher () =
        inherit Entity2dDispatcher ()

        let registerTilePhysicsBox address (tm : Entity) tmd td world =
            let physicsId = Physics.getId tm.Id
            let shapeProperties =
                { Center = Vector2.Zero
                  Friction = 0.2f
                  Restitution = 0.0f
                  FixedRotation = true
                  LinearDamping = 0.0f
                  AngularDamping = 0.0f
                  GravityScale = 1.0f }
            let createBodyMessage =
                { EntityAddress = address
                  PhysicsId = physicsId
                  Shape = BoxShape { Extent = Vector2 (single <| fst tmd.TileSize, single <| snd tmd.TileSize) * 0.5f; Properties = shapeProperties }
                  Position =
                    Vector2 (
                        single <| fst td.TilePosition + fst tmd.TileSize / 2,
                        single <| snd td.TilePosition + snd tmd.TileSize / 2 + snd tmd.TileMapSize)
                  Rotation = tm.Rotation
                  Density = tm.Density
                  BodyType = BodyType.Static }
            let world' = { world with PhysicsMessages = CreateBodyMessage createBodyMessage :: world.PhysicsMessages }
            (world', physicsId)

        let registerTilePhysicsPolygon address (tm : Entity) tmd td vertices world =
            let physicsId = Physics.getId tm.Id
            let shapeProperties =
                { Center = Vector2.Zero
                  Friction = 0.2f
                  Restitution = 0.0f
                  FixedRotation = true
                  LinearDamping = 0.0f
                  AngularDamping = 0.0f
                  GravityScale = 1.0f }
            let createBodyMessage =
                { EntityAddress = address
                  PhysicsId = physicsId
                  Shape = PolygonShape { Vertices = vertices; Properties = shapeProperties }
                  Position =
                    Vector2 (
                        single <| fst td.TilePosition + fst tmd.TileSize / 2,
                        single <| snd td.TilePosition + snd tmd.TileSize / 2 + snd tmd.TileMapSize)
                  Rotation = tm.Rotation
                  Density = tm.Density
                  BodyType = BodyType.Static }
            let world' = { world with PhysicsMessages = CreateBodyMessage createBodyMessage :: world.PhysicsMessages }
            (world', physicsId)

        let registerTilePhysics tm tmd tld address tileIndex (world, physicsIds) _ =
            let td = World.makeTileData tm tmd tld tileIndex
            match td.OptTileSetTile with
            | None -> (world, physicsIds)
            | Some tileSetTile ->
                let collisionProperty = ref Unchecked.defaultof<string>
                if not <| tileSetTile.Properties.TryGetValue (NuConstants.CollisionProperty, collisionProperty) then (world, physicsIds)
                else
                    let collisionExpr = string collisionProperty.Value
                    let collisionTerms = List.ofArray <| collisionExpr.Split '?'
                    let collisionTermsTrimmed = List.map (fun (term : string) -> term.Trim ()) collisionTerms
                    match collisionTermsTrimmed with
                    | [""]
                    | ["Box"] ->
                        let (world', physicsId) = registerTilePhysicsBox address tm tmd td world
                        (world', physicsId :: physicsIds)
                    | ["Polygon"; verticesStr] ->
                        let vertexStrs = List.ofArray <| verticesStr.Split '|'
                        try let vertices = List.map (fun str -> (TypeDescriptor.GetConverter (typeof<Vector2>)).ConvertFromString str :?> Vector2) vertexStrs
                            let verticesOffset = List.map (fun vertex -> vertex - Vector2 0.5f) vertices
                            let verticesScaled = List.map (fun vertex -> Vector2.Multiply (vertex, tmd.TileSizeF)) verticesOffset
                            let (world', physicsId) = registerTilePhysicsPolygon address tm tmd td verticesScaled world
                            (world', physicsId :: physicsIds)
                        with :? NotSupportedException as e ->
                            trace <| "Could not parse collision polygon vertices '" + verticesStr + "'. Format is 'Polygon ? 0.0;1.0 | 1.0;1.0 | 1.0;0.0'"
                            (world, physicsIds)
                    | _ ->
                        trace <| "Invalid tile collision shape expression '" + collisionExpr + "'."
                        (world, physicsIds)

        let registerTileLayerPhysics address tileMap tileMapData collisionLayer world =
            let tileLayerData = World.makeTileLayerData tileMap tileMapData collisionLayer
            if not <| tileLayerData.Layer.Properties.ContainsKey NuConstants.CollisionProperty then (world, [])
            else
                Seq.foldi
                    (registerTilePhysics tileMap tileMapData tileLayerData address)
                    (world, [])
                    tileLayerData.Tiles

        let registerTileMapPhysics address (tileMap : Entity) world =
            let tileMapData = World.makeTileMapData tileMap.TileMapAsset world
            let (world', physicsIds) =
                List.fold
                    (fun (world'', physicsIds) tileLayer ->
                        let (world'3, physicsIds') = registerTileLayerPhysics address tileMap tileMapData tileLayer world''
                        (world'3, physicsIds @ physicsIds'))
                    (world, [])
                    (List.ofSeq tileMapData.Map.Layers)
            let tileMap' = tileMap.SetPhysicsIds physicsIds
            (tileMap', world')

        let unregisterTilePhysics world physicsId =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }

        let unregisterTileMapPhysics (_ : Address) (tileMap : Entity) world =
            List.fold unregisterTilePhysics world <| tileMap.PhysicsIds
        
        override dispatcher.Init (tileMap, dispatcherContainer) =
            let tileMap' = base.Init (tileMap, dispatcherContainer)
            tileMap'
                .SetPhysicsIds([])
                .SetDensity(NormalDensity)
                .SetTileMapAsset({ TileMapAssetName = "TileMap"; PackageName = DefaultPackageName; PackageFileName = AssetGraphFileName })

        override dispatcher.Register (tileMap, address, world) =
            registerTileMapPhysics address tileMap world

        override dispatcher.Unregister (tileMap, address, world) =
            unregisterTileMapPhysics address tileMap world
            
        override dispatcher.PropagatePhysics (tileMap, address, world) =
            let (tileMap', world') = world |> unregisterTileMapPhysics address tileMap |> registerTileMapPhysics address tileMap
            set tileMap' world' <| World.worldEntity address

        override dispatcher.ReregisterPhysicsHack (tileMap, groupAddress, world) =
            let address = addrstr groupAddress tileMap.Name
            let world' = unregisterTileMapPhysics address tileMap world
            let (tileMap', world'') = registerTileMapPhysics address tileMap world'
            set tileMap' world'' <| World.worldEntity address

        override dispatcher.GetRenderDescriptors (tileMap, viewAbsolute, viewRelative, world) =
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
                            let layeredTileLayerDescriptor =
                                LayeredTileLayerDescriptor
                                    { Descriptor =
                                        { Position = tileMap.Position * viewRelative
                                          Size = Vector2.Zero
                                          Rotation = tileMap.Rotation
                                          MapSize = (map.Width, map.Height)
                                          Tiles = layer.Tiles
                                          TileSourceSize = tileSourceSize
                                          TileSize = tileSize
                                          TileMapSize = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                                          TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                          TileSetSprite = List.head sprites } // MAGIC_VALUE: for same reason as above
                                      Depth = tileMap.Depth + single i * 2.0f } // MAGIC_VALUE: assumption
                            LayerableDescriptor layeredTileLayerDescriptor)
                        layers

        override dispatcher.GetQuickSize (tileMap, world) =
            let tileMapAsset = tileMap.TileMapAsset
            match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
            | None -> failwith "Unexpected match failure in Nu.World.TileMapDispatcher.GetQuickSize."
            | Some (_, _, map) -> Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)

    type GroupDispatcher () =

        abstract member Init : Group * IXDispatcherContainer -> Group
        default this.Init (group, dispatcherContainer) = group
        
        abstract member Register : Group * Address * Entity list * World -> World
        default this.Register (_, address, entities, world) = World.addEntities address entities world

        abstract member Unregister : Group * Address * World -> World
        default this.Unregister (_, address, world) = World.removeEntities address world

    type TransitionDispatcher () =
        class end

    type ScreenDispatcher () =

        abstract member Register : Screen * Address * GroupDescriptor list * World -> World
        default this.Register (_, address, groupDescriptors, world) = World.addGroups address groupDescriptors world

        abstract member Unregister : Screen * Address * World -> World
        default this.Unregister (_, address, world) = World.removeGroups address world

    type GameDispatcher () =
        abstract member Register : Game * World -> World
        default this.Register (_, world) = world