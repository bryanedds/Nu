namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.NuConstants

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with

        member entity.MinorId = entity?MinorId () : Guid
        static member setMinorId (value : Guid) (entity : Entity) : Entity = entity?MinorId <- value
        member entity.BodyType = entity?BodyType () : BodyType
        static member setBodyType (value : BodyType) (entity : Entity) : Entity = entity?BodyType <- value
        member entity.Density = entity?Density () : single
        static member setDensity (value : single) (entity : Entity) : Entity = entity?Density <- value
        member entity.Friction = entity?Friction () : single
        static member setFriction (value : single) (entity : Entity) : Entity = entity?Friction <- value
        member entity.Restitution = entity?Restitution () : single
        static member setRestitution (value : single) (entity : Entity) : Entity = entity?Restitution <- value
        member entity.FixedRotation = entity?FixedRotation () : bool
        static member setFixedRotation (value : bool) (entity : Entity) : Entity = entity?FixedRotation <- value
        member entity.LinearDamping = entity?LinearDamping () : single
        static member setLinearDamping (value : single) (entity : Entity) : Entity = entity?LinearDamping <- value
        member entity.AngularDamping = entity?AngularDamping () : single
        static member setAngularDamping (value : single) (entity : Entity) : Entity = entity?AngularDamping <- value
        member entity.GravityScale = entity?GravityScale () : single
        static member setGravityScale (value : single) (entity : Entity) : Entity = entity?GravityScale <- value
        member entity.CollisionCategories = entity?CollisionCategories () : string
        static member setCollisionCategories (value : string) (entity : Entity) : Entity = entity?CollisionCategories <- value
        member entity.CollisionMask = entity?CollisionMask () : string
        static member setCollisionMask (value : string) (entity : Entity) : Entity = entity?CollisionMask <- value
        member entity.CollisionExpression = entity?CollisionExpression () : string
        static member setCollisionExpr (value : string) (entity : Entity) : Entity = entity?CollisionExpr <- value
        member entity.IsBullet = entity?IsBullet () : bool
        static member setIsBullet (value : bool) (entity : Entity) : Entity = entity?IsBullet <- value
        member entity.IsSensor = entity?IsSensor () : bool
        static member setIsSensor (value : bool) (entity : Entity) : Entity = entity?IsSensor <- value

        static member getPhysicsId (entity : Entity) =
            PhysicsId (entity.Id, entity.MinorId)

    type RigidBodyFacet () =
        inherit EntityFacet ()

        static let fieldDescriptors =
            [Entity.describeField Field?MinorId InvalidId
             Entity.describeField Field?BodyType Dynamic
             Entity.describeField Field?Density NormalDensity
             Entity.describeField Field?Friction 0.0f
             Entity.describeField Field?Restitution 0.0f
             Entity.describeField Field?FixedRotation false
             Entity.describeField Field?LinearDamping 1.0f
             Entity.describeField Field?AngularDamping 1.0f
             Entity.describeField Field?GravityScale 1.0f
             Entity.describeField Field?CollisionCategories "1"
             Entity.describeField Field?CollisionMask "*"
             Entity.describeField Field?CollisionExpression "Box"
             Entity.describeField Field?IsBullet false
             Entity.describeField Field?IsSensor false]

        static let getBodyShape (entity : Entity) =
            Physics.evalCollisionExpression (entity.Size : Vector2) entity.CollisionExpression

        static member FieldDescriptors =
            fieldDescriptors

        override facet.Attach entity =
            let entity = Entity.attachFields fieldDescriptors entity
            Entity.setMinorId (NuCore.makeId ()) entity

        override facet.Detach entity =
            Entity.detachFields fieldDescriptors entity

        override facet.RegisterPhysics (entity, address, world) =
            let bodyProperties = 
                { Shape = getBodyShape entity
                  BodyType = entity.BodyType
                  Density = entity.Density
                  Friction = entity.Friction
                  Restitution = entity.Restitution
                  FixedRotation = entity.FixedRotation
                  LinearDamping = entity.LinearDamping
                  AngularDamping = entity.AngularDamping
                  GravityScale = entity.GravityScale
                  CollisionCategories = Physics.toCollisionCategories entity.CollisionCategories
                  CollisionMask = Physics.toCollisionCategories entity.CollisionMask
                  IsBullet = entity.IsBullet
                  IsSensor = entity.IsSensor }
            let physicsId = Entity.getPhysicsId entity
            let position = entity.Position + entity.Size * 0.5f
            let rotation = entity.Rotation
            World.createBody address physicsId position rotation bodyProperties world

        override facet.UnregisterPhysics (entity, _, world) =
            World.destroyBody (Entity.getPhysicsId entity) world

        override facet.PropagatePhysics (entity, address, world) =
            let world = facet.UnregisterPhysics (entity, address, world)
            facet.RegisterPhysics (entity, address, world)

        override facet.HandleBodyTransformMessage (entity, address, message : BodyTransformMessage, world) =
            // OPTIMIZATION: entity is not changed (avoiding a change entity event) if position and rotation haven't changed.
            if entity.Position <> message.Position || entity.Rotation <> message.Rotation then
                let entity =
                    entity |>
                        // TODO: see if the following center-offsetting can be encapsulated within the Physics module!
                        Entity.setPosition (message.Position - entity.Size * 0.5f) |>
                        Entity.setRotation message.Rotation
                World.setEntity address entity world
            else world

[<AutoOpen>]
module SpriteFacetModule =

    type Entity with

        member entity.ViewType = entity?ViewType () : ViewType
        static member setViewType (value : ViewType) (entity : Entity) : Entity = entity?ViewType <- value
        member entity.SpriteImage = entity?SpriteImage () : Image
        static member setSpriteImage (value : Image) (entity : Entity) : Entity = entity?SpriteImage <- value

    type SpriteFacet () =
        inherit EntityFacet ()

        static let fieldDescriptors =
            [Entity.describeField Field?ViewType Relative
             Entity.describeField Field?SpriteImage { ImageAssetName = "Image3"; PackageName = "Default"}]

        static member FieldDescriptors =
            fieldDescriptors

        override facet.Attach entity =
            /// TODO: check for compatibility for facets
            Entity.attachFields fieldDescriptors entity

        override facet.Detach entity =
            Entity.detachFields fieldDescriptors entity

        override facet.GetRenderDescriptors (entity : Entity, world) =
            if entity.Visible && Camera.inView3 entity.Position entity.Size world.Camera then
                [LayerableDescriptor
                    { Depth = entity.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.Position
                              Size = entity.Size
                              Rotation = entity.Rotation
                              ViewType = entity.ViewType
                              OptInset = None
                              Image = entity.SpriteImage
                              Color = Vector4.One }}]
            else []

        override facet.GetQuickSize (entity : Entity, world) =
            let image = entity.SpriteImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with

        member entity.Stutter = entity?Stutter () : int
        static member setStutter (value : int) (entity : Entity) : Entity = entity?Stutter <- value
        member entity.TileCount = entity?TileCount () : int
        static member setTileCount (value : int) (entity : Entity) : Entity = entity?TileCount <- value
        member entity.TileRun = entity?TileRun () : int
        static member setTileRun (value : int) (entity : Entity) : Entity = entity?TileRun <- value
        member entity.TileSize = entity?TileSize () : Vector2
        static member setTileSize (value : Vector2) (entity : Entity) : Entity = entity?TileSize <- value
        member entity.AnimatedSpriteImage = entity?AnimatedSpriteImage () : Image
        static member setAnimatedSpriteImage (value : Image) (entity : Entity) : Entity = entity?AnimatedSpriteImage <- value

    type AnimatedSpriteFacet () =
        inherit EntityFacet ()

        static let fieldDescriptors =
            [Entity.describeField Field?Stutter 4
             Entity.describeField Field?TileCount 16 
             Entity.describeField Field?TileRun 4
             Entity.describeField Field?TileSize <| Vector2 (16.0f, 16.0f)
             Entity.describeField Field?ViewType Relative
             Entity.describeField Field?AnimatedSpriteImage { ImageAssetName = "Image7"; PackageName = "Default"}]

        static let getSpriteOptInset (entity : Entity) world =
            let tile = (int world.TickTime / entity.Stutter) % entity.TileCount
            let tileI = tile % entity.TileRun
            let tileJ = tile / entity.TileRun
            let tileX = single tileI * entity.TileSize.X
            let tileY = single tileJ * entity.TileSize.Y
            let inset = Vector4 (tileX, tileY, tileX + entity.TileSize.X, tileY + entity.TileSize.Y)
            Some inset

        static member FieldDescriptors =
            fieldDescriptors

        override facet.Attach entity =
            Entity.attachFields fieldDescriptors entity

        override facet.Detach entity =
            Entity.detachFields fieldDescriptors entity

        override facet.GetRenderDescriptors (entity : Entity, world) =
            if entity.Visible && Camera.inView3 entity.Position entity.Size world.Camera then
                [LayerableDescriptor
                    { Depth = entity.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.Position
                              Size = entity.Size
                              Rotation = entity.Rotation
                              ViewType = entity.ViewType
                              OptInset = getSpriteOptInset entity world
                              Image = entity.AnimatedSpriteImage
                              Color = Vector4.One }}]
            else []

        override facet.GetQuickSize (entity : Entity, _ : World) =
            entity.TileSize

[<AutoOpen>]
module EntityDispatcherModule =

    type Entity with
    
        static member private sortFstDesc (priority, _) (priority2, _) =
            if priority = priority2 then 0
            elif priority > priority2 then -1
            else 1

        static member mouseToEntity position world (entity : Entity) =
            let positionScreen = Camera.mouseToScreen position world.Camera
            let view = (if Entity.isTransformRelative entity world then Camera.getViewRelativeF else Camera.getViewAbsoluteF) world.Camera
            let positionEntity = positionScreen * view
            positionEntity

        static member setPositionSnapped snap position (entity : Entity) =
            let snapped = NuMath.snap2F snap position
            Entity.setPosition snapped entity

        static member getTransform (entity : Entity) =
            { Transform.Position = entity.Position
              Depth = entity.Depth
              Size = entity.Size
              Rotation = entity.Rotation }

        static member setTransform positionSnap rotationSnap transform (entity : Entity) =
            let transform = NuMath.snapTransform positionSnap rotationSnap transform
            entity |>
                Entity.setPosition transform.Position |>
                Entity.setDepth transform.Depth |>
                Entity.setSize transform.Size |>
                Entity.setRotation transform.Rotation

        static member pickingSort entities world =
            let priorities = List.map (fun (entity : Entity) -> Entity.getPickingPriority entity world) entities
            let prioritiesAndEntities = List.zip priorities entities
            let prioritiesAndEntitiesSorted = List.sortWith Entity.sortFstDesc prioritiesAndEntities
            List.map snd prioritiesAndEntitiesSorted

        static member tryPick position entities world =
            let entitiesSorted = Entity.pickingSort entities world
            List.tryFind
                (fun entity ->
                    let positionEntity = Entity.mouseToEntity position world entity
                    let transform = Entity.getTransform entity
                    let picked = NuMath.isPointInBounds3 positionEntity transform.Position transform.Size
                    picked)
                entitiesSorted

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
        
        member gui.Enabled = gui?Enabled () : bool
        static member setEnabled (value : bool) (gui : Entity) : Entity = gui?Enabled <- value

    type [<AbstractClass>] GuiDispatcher () =
        inherit EntityDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?Enabled true]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (gui, world) =
            let gui = base.AttachFields (gui, world)
            Entity.attachFields fieldDescriptors gui

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with

        member button.IsDown = button?IsDown () : bool
        static member setIsDown (value : bool) (button : Entity) : Entity = button?IsDown <- value
        member button.UpImage = button?UpImage () : Image
        static member setUpImage (value : Image) (button : Entity) : Entity = button?UpImage <- value
        member button.DownImage = button?DownImage () : Image
        static member setDownImage (value : Image) (button : Entity) : Entity = button?DownImage <- value
        member button.ClickSound = button?ClickSound () : Sound
        static member setClickSound (value : Sound) (button : Entity) : Entity = button?ClickSound <- value

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?IsDown false
             Entity.describeField Field?UpImage { ImageAssetName = "Image"; PackageName = DefaultPackageName }
             Entity.describeField Field?DownImage { ImageAssetName = "Image2"; PackageName = DefaultPackageName }
             Entity.describeField Field?ClickSound { SoundAssetName = "Sound"; PackageName = DefaultPackageName }]

        let handleButtonEventDownMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let button = World.getEntity event.Subscriber world
                let mousePositionButton = Entity.mouseToEntity mouseButtonData.Position world button
                if button.Enabled && button.Visible then
                    if NuMath.isPointInBounds3 mousePositionButton button.Position button.Size then
                        let button = Entity.setIsDown true button
                        let world = World.setEntity event.Subscriber button world
                        let world = World.publish4 (DownEventName + event.Subscriber) event.Subscriber NoData world
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
                        World.publish4 (UpEventName + event.Subscriber) event.Subscriber NoData world
                    if NuMath.isPointInBounds3 mousePositionButton button.Position button.Size && button.IsDown then
                        let world = World.publish4 (ClickEventName + event.Subscriber) event.Subscriber NoData world
                        let world = World.playSound button.ClickSound 1.0f world
                        (Handled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (button, world) =
            let button = base.AttachFields (button, world)
            Entity.attachFields fieldDescriptors button

        override dispatcher.Register (_, address, world) =
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
                              Image = if button.IsDown then button.DownImage else button.UpImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (button, world) =
            let image = button.UpImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with

        member label.LabelImage = label?LabelImage () : Image
        static member setLabelImage (value : Image) (label : Entity) : Entity = label?LabelImage <- value

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?LabelImage { ImageAssetName = "Image4"; PackageName = DefaultPackageName }]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (label, world) =
            let label = base.AttachFields (label, world)
            Entity.attachFields fieldDescriptors label

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
                              Image = label.LabelImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (label, world) =
            let image = label.LabelImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with

        member text.Text = text?Text () : string
        static member setText (value : string) (text : Entity) : Entity = text?Text <- value
        member text.TextFont = text?TextFont () : Font
        static member setTextFont (value : Font) (text : Entity) : Entity = text?TextFont <- value
        member text.TextOffset = text?TextOffset () : Vector2
        static member setTextOffset (value : Vector2) (text : Entity) : Entity = text?TextOffset <- value
        member text.TextColor = text?TextColor () : Vector4
        static member setTextColor (value : Vector4) (text : Entity) : Entity = text?TextColor <- value
        member text.BackgroundImage = text?BackgroundImage () : Image
        static member setBackgroundImage (value : Image) (text : Entity) : Entity = text?BackgroundImage <- value

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?Text String.Empty
             Entity.describeField Field?TextFont { FontAssetName = "Font"; PackageName = DefaultPackageName }
             Entity.describeField Field?TextOffset Vector2.Zero
             Entity.describeField Field?TextColor Vector4.One
             Entity.describeField Field?BackgroundImage { ImageAssetName = "Image4"; PackageName = DefaultPackageName }]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (text, world) =
            let text = base.AttachFields (text, world)
            Entity.attachFields fieldDescriptors text

        override dispatcher.GetRenderDescriptors (text, _) =
            if text.Visible then
                [LayerableDescriptor
                    { Depth = text.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = text.Position
                              Size = text.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Image = text.BackgroundImage
                              Color = Vector4.One }}
                 LayerableDescriptor
                    { Depth = text.Depth
                      LayeredDescriptor =
                        TextDescriptor
                            { Text = text.Text
                              Position = (text.Position + text.TextOffset)
                              Size = text.Size - text.TextOffset
                              ViewType = Absolute
                              Font = text.TextFont
                              Color = text.TextColor }}]
            else []

        override dispatcher.GetQuickSize (text, world) =
            let image = text.BackgroundImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with

        member toggle.IsOn = toggle?IsOn () : bool
        static member setIsOn (value : bool) (toggle : Entity) : Entity = toggle?IsOn <- value
        member toggle.IsPressed = toggle?IsPressed () : bool
        static member setIsPressed (value : bool) (toggle : Entity) : Entity = toggle?IsPressed <- value
        member toggle.OffImage = toggle?OffImage () : Image
        static member setOffImage (value : Image) (toggle : Entity) : Entity = toggle?OffImage <- value
        member toggle.OnImage = toggle?OnImage () : Image
        static member setOnImage (value : Image) (toggle : Entity) : Entity = toggle?OnImage <- value
        member toggle.ToggleSound = toggle?ToggleSound () : Sound
        static member setToggleSound (value : Sound) (toggle : Entity) : Entity = toggle?ToggleSound <- value

    type ToggleDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?IsOn false
             Entity.describeField Field?IsPressed false
             Entity.describeField Field?OffImage { ImageAssetName = "Image"; PackageName = DefaultPackageName }
             Entity.describeField Field?OnImage { ImageAssetName = "Image2"; PackageName = DefaultPackageName }
             Entity.describeField Field?ToggleSound { SoundAssetName = "Sound"; PackageName = DefaultPackageName }]

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
                    if NuMath.isPointInBounds3 mousePositionToggle toggle.Position toggle.Size then
                        let toggle = Entity.setIsOn (not toggle.IsOn) toggle
                        let world = World.setEntity event.Subscriber toggle world
                        let eventName = if toggle.IsOn then OnEventName else OffEventName
                        let world = World.publish4 (eventName + event.Subscriber) event.Subscriber NoData world
                        let world = World.playSound toggle.ToggleSound 1.0f world
                        (Handled, world)
                    else
                        let world = World.setEntity event.Subscriber toggle world
                        (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (toggle, world) =
            let toggle = base.AttachFields (toggle, world)
            Entity.attachFields fieldDescriptors toggle

        override dispatcher.Register (_, address, world) =
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
                              Image = if toggle.IsOn || toggle.IsPressed then toggle.OnImage else toggle.OffImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (toggle, world) =
            let image = toggle.OffImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with

        member feeler.IsTouched = feeler?IsTouched () : bool
        static member setIsTouched (value : bool) (feeler : Entity) : Entity = feeler?IsTouched <- value

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?IsTouched false]

        let handleFeelerEventDownMouseLeft event world =
            if World.isAddressSelected event.Subscriber world then
                let mouseButtonData = EventData.toMouseButtonData event.Data
                let feeler = World.getEntity event.Subscriber world
                let mousePositionFeeler = Entity.mouseToEntity mouseButtonData.Position world feeler
                if feeler.Enabled && feeler.Visible then
                    if NuMath.isPointInBounds3 mousePositionFeeler feeler.Position feeler.Size then
                        let feeler = Entity.setIsTouched true feeler
                        let world = World.setEntity event.Subscriber feeler world
                        let world = World.publish4 (TouchEventName + event.Subscriber) event.Subscriber (MouseButtonData mouseButtonData) world
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
                    let world = World.publish4 (ReleaseEventName + event.Subscriber) event.Subscriber NoData world
                    (Handled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (feeler, world) =
            let feeler = base.AttachFields (feeler, world)
            Entity.attachFields fieldDescriptors feeler

        override dispatcher.Register (_, address, world) =
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
    
        member fillBar.Fill = fillBar?Fill () : single
        static member setFill (value : single) (fillBar : Entity) : Entity = fillBar?Fill <- value
        member fillBar.FillInset = fillBar?FillInset () : single
        static member setFillInset (value : single) (fillBar : Entity) : Entity = fillBar?FillInset <- value
        member fillBar.FillImage = fillBar?FillImage () : Image
        static member setFillImage (value : Image) (fillBar : Entity) : Entity = fillBar?FillImage <- value
        member fillBar.BorderImage = fillBar?BorderImage () : Image
        static member setBorderImage (value : Image) (fillBar : Entity) : Entity = fillBar?BorderImage <- value

    type FillBarDispatcher () =
        inherit GuiDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?Fill 0.0f
             Entity.describeField Field?FillInset 0.0f
             Entity.describeField Field?FillImage { ImageAssetName = "Image9"; PackageName = DefaultPackageName }
             Entity.describeField Field?BorderImage { ImageAssetName = "Image10"; PackageName = DefaultPackageName }]

        let getFillBarSpriteDims (fillBar : Entity) =
            let spriteInset = fillBar.Size * fillBar.FillInset * 0.5f
            let spritePosition = fillBar.Position + spriteInset
            let spriteWidth = (fillBar.Size.X - spriteInset.X * 2.0f) * fillBar.Fill
            let spriteHeight = fillBar.Size.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (fillBar, world) =
            let fillBar = base.AttachFields (fillBar, world)
            Entity.attachFields fieldDescriptors fillBar

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
                              Image = fillBar.FillImage
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
                              Image = fillBar.BorderImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (fillBar, world) =
            let image = fillBar.BorderImage
            match Metadata.tryGetTextureSizeAsVector2 image.ImageAssetName image.PackageName world.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

        override dispatcher.IsTransformRelative (_, _) =
            false

[<AutoOpen>]
module RigidBodyDispatcherModule =

    type [<AbstractClass>] RigidBodyDispatcher () =
        inherit EntityDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?FacetNames [typeof<RigidBodyFacet>.Name]]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (rigidBody, world) =
            let rigidBody = base.AttachFields (rigidBody, world)
            Entity.attachFields fieldDescriptors rigidBody

[<AutoOpen>]
module RigidBodySpriteDispatcherModule =

    type [<AbstractClass>] RigidBodySpriteDispatcher () =
        inherit EntityDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?FacetNames [typeof<RigidBodyFacet>.Name; typeof<SpriteFacet>.Name]]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (rigidBody, world) =
            let rigidBody = base.AttachFields (rigidBody, world)
            Entity.attachFields fieldDescriptors rigidBody

[<AutoOpen>]
module RigidBodyAnimatedSpriteDispatcherModule =

    type [<AbstractClass>] RigidBodyAnimatedSpriteDispatcher () =
        inherit EntityDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?FacetNames [typeof<RigidBodyFacet>.Name; typeof<AnimatedSpriteFacet>.Name]]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (rigidBody, world) =
            let rigidBody = base.AttachFields (rigidBody, world)
            Entity.attachFields fieldDescriptors rigidBody

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit RigidBodySpriteDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?SpriteImage { ImageAssetName = "Image3"; PackageName = DefaultPackageName }]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (block, world) =
            let block = base.AttachFields (block, world)
            Entity.attachFields fieldDescriptors block

[<AutoOpen>]
module AvatarDispatcherModule =

    type AvatarDispatcher () =
        inherit RigidBodySpriteDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?FixedRotation true
             Entity.describeField Field?LinearDamping 10.0f
             Entity.describeField Field?GravityScale 0.0f
             Entity.describeField Field?CollisionExpression "Circle"
             Entity.describeField Field?SpriteImage { ImageAssetName = "Image7"; PackageName = DefaultPackageName }]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (avatar, world) =
            let avatar = base.AttachFields (avatar, world)
            Entity.attachFields fieldDescriptors avatar

[<AutoOpen>]
module CharacterDispatcherModule =

    type CharacterDispatcher () =
        inherit RigidBodySpriteDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?FixedRotation true
             Entity.describeField Field?LinearDamping 3.0f
             Entity.describeField Field?CollisionExpression "Capsule"
             Entity.describeField Field?SpriteImage { ImageAssetName = "Image6"; PackageName = DefaultPackageName }]

        static member FieldDescriptors =
            fieldDescriptors

        override dispatcher.AttachFields (character, world) =
            let character = base.AttachFields (character, world)
            Entity.attachFields fieldDescriptors character

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with

        member entity.TileMapAsset = entity?TileMapAsset () : TileMapAsset
        static member setTileMapAsset (value : TileMapAsset) (entity : Entity) : Entity = entity?TileMapAsset <- value
        member entity.Parallax = entity?Parallax () : single
        static member setParallax (value : single) (entity : Entity) : Entity = entity?Parallax <- value

        static member makeTileMapData tileMapAsset world =
            let (_, _, map) = Metadata.getTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap
            let mapSize = Vector2I (map.Width, map.Height)
            let tileSize = Vector2I (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
            let tileMapSize = Vector2I (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
            let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = Vector2I (optTileSetWidth.Value / tileSize.X, optTileSetHeight.Value / tileSize.Y)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

        static member makeTileData (tileMap : Entity) tmd (tl : TmxLayer) tileIndex =
            let mapRun = tmd.MapSize.X
            let tileSetRun = tmd.TileSetSize.X
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * tmd.TileSize.X
            let gid2 = Vector2I (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tileMap.Position
            let tilePosition =
                Vector2I (
                    int tileMapPosition.X + tmd.TileSize.X * i,
                    int tileMapPosition.Y - tmd.TileSize.Y * (j + 1)) // subtraction for right-handedness
            let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        static let fieldDescriptors =
            [Entity.describeField Field?Density NormalDensity
             Entity.describeField Field?Friction 0.0f
             Entity.describeField Field?Restitution 0.0f
             Entity.describeField Field?CollisionCategories "1"
             Entity.describeField Field?CollisionMask "*"
             Entity.describeField Field?TileMapAsset { TileMapAssetName = "TileMap"; PackageName = DefaultPackageName }
             Entity.describeField Field?Parallax 0.0f]

        let getTilePhysicsId tmid (tli : int) (ti : int) =
            PhysicsId (tmid, intsToGuid tli ti)

        let registerTilePhysicsShape address (tm : Entity) tmd tli td ti cexpr world =
            let tileShape = Physics.evalCollisionExpression (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            let physicsId = getTilePhysicsId tm.Id tli ti
            let createBodyMessage =
                CreateBodyMessage
                    { EntityAddress = address
                      PhysicsId = physicsId
                      Position =
                        Vector2 (
                            single <| td.TilePosition.X + tmd.TileSize.X / 2,
                            single <| td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y)
                      Rotation = tm.Rotation
                      BodyProperties =
                        { Shape = tileShape
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
            | Some tileSetTile ->
                let collisionProperty = ref Unchecked.defaultof<string>
                if tileSetTile.Properties.TryGetValue (CollisionProperty, collisionProperty) then
                    let collisionExpr = string collisionProperty.Value
                    registerTilePhysicsShape address tm tmd tli td ti collisionExpr world
                else world
            | None -> world

        let registerTileLayerPhysics address tileMap tileMapData tileLayerIndex world (tileLayer : TmxLayer) =
            if tileLayer.Properties.ContainsKey CollisionProperty then
                Seq.foldi
                    (registerTilePhysics tileMap tileMapData tileLayer tileLayerIndex address)
                    world
                    tileLayer.Tiles
            else world

        let registerTileMapPhysics (tileMap : Entity) address world =
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (registerTileLayerPhysics address tileMap tileMapData)
                world
                tileMapData.Map.Layers

        let unregisterTileMapPhysics (tileMap : Entity) world =
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                    if tileLayer.Properties.ContainsKey CollisionProperty then
                        Seq.foldi
                            (fun tileIndex world _ ->
                                let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex
                                match tileData.OptTileSetTile with
                                | Some tileSetTile ->
                                    if tileSetTile.Properties.ContainsKey CollisionProperty then
                                        let physicsId = getTilePhysicsId tileMap.Id tileLayerIndex tileIndex
                                        let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
                                        { world with PhysicsMessages = destroyBodyMessage :: world.PhysicsMessages }
                                    else world
                                | None -> world)
                            world
                            tileLayer.Tiles
                    else world)
                world
                tileMapData.Map.Layers

        static member FieldDescriptors =
            fieldDescriptors
        
        override dispatcher.AttachFields (tileMap, world) =
            let tileMap = base.AttachFields (tileMap, world)
            Entity.attachFields fieldDescriptors tileMap

        override dispatcher.Register (tileMap, address, world) =
            registerTileMapPhysics tileMap address world

        override dispatcher.Unregister (tileMap, _, world) =
            unregisterTileMapPhysics tileMap world
            
        override dispatcher.PropagatePhysics (tileMap, address, world) =
            world |>
                unregisterTileMapPhysics tileMap |>
                registerTileMapPhysics tileMap address

        override dispatcher.GetRenderDescriptors (tileMap, world) =
            if tileMap.Visible then
                let tileMapAsset = tileMap.TileMapAsset
                match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
                | Some (_, images, map) ->
                    let layers = List.ofSeq map.Layers
                    let tileSourceSize = Vector2I (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                    List.foldi
                        (fun i descriptors (layer : TmxLayer) ->
                            let depth = tileMap.Depth + single i * 2.0f // MAGIC_VALUE: assumption
                            let parallaxTranslation = tileMap.Parallax * depth * -world.Camera.EyeCenter
                            let parallaxPosition = tileMap.Position + parallaxTranslation
                            let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                            if Camera.inView3 parallaxPosition size world.Camera then
                                let descriptor =
                                    LayerableDescriptor 
                                        { Depth = depth
                                          LayeredDescriptor =
                                            TileLayerDescriptor
                                                { Position = parallaxPosition
                                                  Size = size
                                                  Rotation = tileMap.Rotation
                                                  ViewType = Relative
                                                  MapSize = Vector2I (map.Width, map.Height)
                                                  Tiles = layer.Tiles
                                                  TileSourceSize = tileSourceSize
                                                  TileSize = tileSize
                                                  TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                                  TileSetImage = List.head images }} // MAGIC_VALUE: for same reason as above
                                descriptor :: descriptors
                            else descriptors)
                        []
                        layers
                | None -> []
            else []

        override dispatcher.GetQuickSize (tileMap, world) =
            let tileMapAsset = tileMap.TileMapAsset
            match Metadata.tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap with
            | Some (_, _, map) -> Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)
            | None -> failwith "Unexpected match failure in Nu.World.TileMapDispatcher.GetQuickSize."