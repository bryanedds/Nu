namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with

        member entity.MinorId = entity?MinorId : Guid
        static member setMinorId (value : Guid) (entity : Entity) = entity?MinorId <- value
        member entity.BodyType = entity?BodyType : BodyType
        static member setBodyType (value : BodyType) (entity : Entity) = entity?BodyType <- value
        member entity.Density = entity?Density : single
        static member setDensity (value : single) (entity : Entity) = entity?Density <- value
        member entity.Friction = entity?Friction : single
        static member setFriction (value : single) (entity : Entity) = entity?Friction <- value
        member entity.Restitution = entity?Restitution : single
        static member setRestitution (value : single) (entity : Entity) = entity?Restitution <- value
        member entity.FixedRotation = entity?FixedRotation : bool
        static member setFixedRotation (value : bool) (entity : Entity) = entity?FixedRotation <- value
        member entity.LinearDamping = entity?LinearDamping : single
        static member setLinearDamping (value : single) (entity : Entity) = entity?LinearDamping <- value
        member entity.AngularDamping = entity?AngularDamping : single
        static member setAngularDamping (value : single) (entity : Entity) = entity?AngularDamping <- value
        member entity.GravityScale = entity?GravityScale : single
        static member setGravityScale (value : single) (entity : Entity) = entity?GravityScale <- value
        member entity.CollisionCategories = entity?CollisionCategories : string
        static member setCollisionCategories (value : string) (entity : Entity) = entity?CollisionCategories <- value
        member entity.CollisionMask = entity?CollisionMask : string
        static member setCollisionMask (value : string) (entity : Entity) = entity?CollisionMask <- value
        member entity.CollisionExpr = entity?CollisionExpr : string
        static member setCollisionExpr (value : string) (entity : Entity) = entity?CollisionExpr <- value
        member entity.IsBullet = entity?IsBullet : bool
        static member setIsBullet (value : bool) (entity : Entity) = entity?IsBullet <- value
        member entity.IsSensor = entity?IsSensor : bool
        static member setIsSensor (value : bool) (entity : Entity) = entity?IsSensor <- value
        member entity.PhysicsId = { SourceId = entity.Id; BodyId = entity.MinorId }

    type RigidBodyFacet () =
        inherit Facet ()

        static let getBodyShape (entity : Entity) =
            Physics.evalCollisionExpr (entity.Size : Vector2) entity.CollisionExpr

        static member FieldDefinitions =
            [variable? MinorId <| fun () -> Core.makeId ()
             define? BodyType Dynamic
             define? Density NormalDensity
             define? Friction 0.0f
             define? Restitution 0.0f
             define? FixedRotation false
             define? LinearDamping 1.0f
             define? AngularDamping 1.0f
             define? GravityScale 1.0f
             define? CollisionCategories "1"
             define? CollisionMask "*"
             define? CollisionExpr "Box"
             define? IsBullet false
             define? IsSensor false]

        override facet.RegisterPhysics (address, entity, world) =
            let bodyProperties = 
                { BodyId = entity.PhysicsId.BodyId
                  Position = entity.Position + entity.Size * 0.5f
                  Rotation = entity.Rotation
                  Shape = getBodyShape entity
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
            World.createBody address entity.Id bodyProperties world

        override facet.UnregisterPhysics (_, entity, world) =
            World.destroyBody entity.PhysicsId world

        override facet.PropagatePhysics (address, entity, world) =
            let world = facet.UnregisterPhysics (address, entity, world)
            facet.RegisterPhysics (address, entity, world)

[<AutoOpen>]
module SpriteFacetModule =

    type Entity with

        member entity.SpriteImage = entity?SpriteImage : Image
        static member setSpriteImage (value : Image) (entity : Entity) = entity?SpriteImage <- value

    type SpriteFacet () =
        inherit Facet ()

        static member FieldDefinitions =
            [define? SpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image3" }]

        override facet.GetRenderDescriptors (entity : Entity, world) =
            if entity.Visible && Camera.inView3 entity.ViewType entity.Position entity.Size world.Camera then
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
            let imageAssetTag = Image.toAssetTag entity.SpriteImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with

        member entity.Stutter = entity?Stutter : int
        static member setStutter (value : int) (entity : Entity) = entity?Stutter <- value
        member entity.TileCount = entity?TileCount : int
        static member setTileCount (value : int) (entity : Entity) = entity?TileCount <- value
        member entity.TileRun = entity?TileRun : int
        static member setTileRun (value : int) (entity : Entity) = entity?TileRun <- value
        member entity.TileSize = entity?TileSize : Vector2
        static member setTileSize (value : Vector2) (entity : Entity) = entity?TileSize <- value
        member entity.AnimatedSpriteImage = entity?AnimatedSpriteImage : Image // TODO: be nice to rename this to AnimationSheet?
        static member setAnimatedSpriteImage (value : Image) (entity : Entity) = entity?AnimatedSpriteImage <- value

    type AnimatedSpriteFacet () =
        inherit Facet ()

        static let getOptSpriteInset (entity : Entity) world =
            let tile = (int world.State.TickTime / entity.Stutter) % entity.TileCount
            let tileI = tile % entity.TileRun
            let tileJ = tile / entity.TileRun
            let tileX = single tileI * entity.TileSize.X
            let tileY = single tileJ * entity.TileSize.Y
            let inset = Vector4 (tileX, tileY, tileX + entity.TileSize.X, tileY + entity.TileSize.Y)
            Some inset

        static member FieldDefinitions =
            [define? Stutter 4
             define? TileCount 16 
             define? TileRun 4
             define? TileSize <| Vector2 (16.0f, 16.0f)
             define? AnimatedSpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image7" }]

        override facet.GetRenderDescriptors (entity : Entity, world) =
            if entity.Visible && Camera.inView3 entity.ViewType entity.Position entity.Size world.Camera then
                [LayerableDescriptor
                    { Depth = entity.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.Position
                              Size = entity.Size
                              Rotation = entity.Rotation
                              ViewType = entity.ViewType
                              OptInset = getOptSpriteInset entity world
                              Image = entity.AnimatedSpriteImage
                              Color = Vector4.One }}]
            else []

        override facet.GetQuickSize (entity : Entity, _ : World) =
            entity.TileSize

[<AutoOpen>]
module EntityDispatcherModule =

    type Entity with
    
        // OPTIMIZATION: priority annotated as single to decrease GC pressure.
        static member private sortFstDesc (priority : single, _) (priority2 : single, _) =
            if priority > priority2 then -1
            elif priority < priority2 then 1
            else 0

        static member setPositionSnapped snap position (entity : Entity) =
            let snapped = Math.snap2F snap position
            Entity.setPosition snapped entity

        static member getTransform (entity : Entity) =
            { Transform.Position = entity.Position
              Depth = entity.Depth
              Size = entity.Size
              Rotation = entity.Rotation }

        static member setTransform positionSnap rotationSnap transform (entity : Entity) =
            let transform = Math.snapTransform positionSnap rotationSnap transform
            entity |>
                Entity.setPosition transform.Position |>
                Entity.setDepth transform.Depth |>
                Entity.setSize transform.Size |>
                Entity.setRotation transform.Rotation

        static member pickingSort entities world =
            let prioritiesAndEntities = List.map (fun (entity : Entity) -> (Entity.getPickingPriority entity world, entity)) entities
            let prioritiesAndEntities = List.sortWith Entity.sortFstDesc prioritiesAndEntities
            List.map snd prioritiesAndEntities

        static member tryPick position entities world =
            let entitiesSorted = Entity.pickingSort entities world
            List.tryFind
                (fun entity ->
                    let positionWorld = Camera.mouseToWorld entity.ViewType position world.Camera
                    let transform = Entity.getTransform entity
                    let picked = Math.isPointInBounds3 positionWorld transform.Position transform.Size
                    picked)
                entitiesSorted

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
        
        member gui.Enabled = gui?Enabled : bool
        static member setEnabled (value : bool) (gui : Entity) = gui?Enabled <- value
        member gui.SwallowMouseLeft = gui?SwallowMouseLeft : bool
        static member setSwallowMouseLeft (value : bool) (gui : Entity) = gui?SwallowMouseLeft <- value

    type GuiDispatcher () =
        inherit EntityDispatcher ()

        static let handleMouseLeft event world =
            let (gui : Entity, mouseButtonData : MouseButtonData) = World.unwrapSD event world
            let mousePositionWorld = Camera.mouseToWorld gui.ViewType mouseButtonData.Position world.Camera
            let eventHandling = if gui.SwallowMouseLeft && Math.isPointInBounds3 mousePositionWorld gui.Position gui.Size then Resolve else Cascade
            (eventHandling, world)
        
        static member FieldDefinitions =
            [define? ViewType Absolute
             define? Enabled true
             define? SwallowMouseLeft true]

        override dispatcher.Register (address, gui, world) =
            let world =
                world |>
                World.monitor handleMouseLeft MouseLeftDownEventAddress address |>
                World.monitor handleMouseLeft MouseLeftUpEventAddress address
            (gui, world)

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with

        member button.Down = button?Down : bool
        static member setDown (value : bool) (button : Entity) = button?Down <- value
        member button.UpImage = button?UpImage : Image
        static member setUpImage (value : Image) (button : Entity) = button?UpImage <- value
        member button.DownImage = button?DownImage : Image
        static member setDownImage (value : Image) (button : Entity) = button?DownImage <- value
        member button.OptClickSound = button?OptClickSound : Sound option
        static member setOptClickSound (value : Sound option) (button : Entity) = button?OptClickSound <- value

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown event world =
            let (address, button : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && button.Enabled && button.Visible then
                let mousePositionWorld = Camera.mouseToWorld button.ViewType mouseButtonData.Position world.Camera
                if Math.isPointInBounds3 mousePositionWorld button.Position button.Size then
                    let button = Entity.setDown true button
                    let world = World.setEntity address button world
                    let world = World.publish4 () (DownEventAddress ->>- address) address world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp event world =
            let (address, button : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && button.Enabled && button.Visible then
                let mousePositionWorld = Camera.mouseToWorld button.ViewType mouseButtonData.Position world.Camera
                let world =
                    let button = Entity.setDown false button
                    let world = World.setEntity address button world
                    World.publish4 () (UpEventAddress ->>- address) address world
                if Math.isPointInBounds3 mousePositionWorld button.Position button.Size && button.Down then
                    let world = World.publish4 () (ClickEventAddress ->>- address) address world
                    let world =
                        match button.OptClickSound with
                        | Some clickSound -> World.playSound 1.0f clickSound world
                        | None -> world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Down false
             define? UpImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image" }
             define? DownImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image2" }
             define? OptClickSound <| Some { SoundPackageName = DefaultPackageName; SoundAssetName = "Sound" }
             define? SwallowMouseLeft false]

        override dispatcher.Register (address, button, world) =
            let world =
                world |>
                World.monitor handleMouseLeftDown MouseLeftDownEventAddress address |>
                World.monitor handleMouseLeftUp MouseLeftUpEventAddress address
            (button, world)

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
                              Image = if button.Down then button.DownImage else button.UpImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (button, world) =
            let imageAssetTag = Image.toAssetTag button.UpImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with

        member label.LabelImage = label?LabelImage : Image
        static member setLabelImage (value : Image) (label : Entity) = label?LabelImage <- value

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member FieldDefinitions =
            [define? LabelImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image4" }
             define? SwallowMouseLeft true]

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
            let imageAssetTag = Image.toAssetTag label.LabelImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with

        member text.Text : string = text?Text
        static member setText (value : string) (text : Entity) = text?Text <- value
        member text.TextFont = text?TextFont : Font
        static member setTextFont (value : Font) (text : Entity) = text?TextFont <- value
        member text.TextOffset = text?TextOffset : Vector2
        static member setTextOffset (value : Vector2) (text : Entity) = text?TextOffset <- value
        member text.TextColor = text?TextColor : Vector4
        static member setTextColor (value : Vector4) (text : Entity) = text?TextColor <- value
        member text.BackgroundImage = text?BackgroundImage : Image
        static member setBackgroundImage (value : Image) (text : Entity) = text?BackgroundImage <- value

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member FieldDefinitions =
            [define? Text String.Empty
             define? TextFont { FontPackageName = DefaultPackageName; FontAssetName = "Font" }
             define? TextOffset Vector2.Zero
             define? TextColor Vector4.One
             define? BackgroundImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image4" }
             define? SwallowMouseLeft true]

        override dispatcher.GetRenderDescriptors (text, _) =
            if text.Visible then
                [LayerableDescriptor
                    { Depth = text.Depth
                      LayeredDescriptor =
                        TextDescriptor
                            { Text = text.Text
                              Position = (text.Position + text.TextOffset)
                              Size = text.Size - text.TextOffset
                              ViewType = Absolute
                              Font = text.TextFont
                              Color = text.TextColor }}
                 LayerableDescriptor
                    { Depth = text.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = text.Position
                              Size = text.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Image = text.BackgroundImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (text, world) =
            let imageAssetTag = Image.toAssetTag text.BackgroundImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with

        member toggle.On = toggle?IsOn : bool
        static member setOn (value : bool) (toggle : Entity) = toggle?On <- value
        member toggle.Pressed = toggle?Pressed : bool
        static member setPressed (value : bool) (toggle : Entity) = toggle?Pressed <- value
        member toggle.OffImage = toggle?OffImage : Image
        static member setOffImage (value : Image) (toggle : Entity) = toggle?OffImage <- value
        member toggle.OnImage = toggle?OnImage : Image
        static member setOnImage (value : Image) (toggle : Entity) = toggle?OnImage <- value
        member toggle.OptToggleSound = toggle?OptToggleSound : Sound option
        static member setOptToggleSound (value : Sound option) (toggle : Entity) = toggle?OptToggleSound <- value

    type ToggleDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown event world =
            let (address, toggle : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && toggle.Enabled && toggle.Visible then
                let mousePositionWorld = Camera.mouseToWorld toggle.ViewType mouseButtonData.Position world.Camera
                if Math.isPointInBounds3 mousePositionWorld toggle.Position toggle.Size then
                    let toggle = Entity.setPressed true toggle
                    let world = World.setEntity address toggle world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp event world =
            let (address, toggle : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && toggle.Enabled && toggle.Visible && toggle.Pressed then
                let mousePositionWorld = Camera.mouseToWorld toggle.ViewType mouseButtonData.Position world.Camera
                let toggle = Entity.setPressed false toggle
                if Math.isPointInBounds3 mousePositionWorld toggle.Position toggle.Size then
                    let toggle = Entity.setOn (not toggle.On) toggle
                    let world = World.setEntity address toggle world
                    let eventAddress = if toggle.On then OnEventAddress else OffEventAddress
                    let world = World.publish4 () (eventAddress ->>- address) address world
                    let world =
                        match toggle.OptToggleSound with
                        | Some toggleSound -> World.playSound 1.0f toggleSound world
                        | None -> world
                    (Resolve, world)
                else
                    let world = World.setEntity address toggle world
                    (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? On false
             define? Pressed false
             define? OffImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image" }
             define? OnImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image2" }
             define? ToggleSound { SoundPackageName = DefaultPackageName; SoundAssetName = "Sound" }
             define? SwallowMouseLeft false]

        override dispatcher.Register (address, toggle, world) =
            let world =
                world |>
                World.monitor handleMouseLeftDown MouseLeftDownEventAddress address |>
                World.monitor handleMouseLeftUp MouseLeftUpEventAddress address
            (toggle, world)

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
                              Image = if toggle.On || toggle.Pressed then toggle.OnImage else toggle.OffImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (toggle, world) =
            let imageAssetTag = Image.toAssetTag toggle.OffImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with

        member feeler.Touched = feeler?Touched : bool
        static member setTouched (value : bool) (feeler : Entity) = feeler?Touched <- value

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown event world =
            let (address, feeler : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && feeler.Enabled && feeler.Visible then
                let mousePositionWorld = Camera.mouseToWorld feeler.ViewType mouseButtonData.Position world.Camera
                if Math.isPointInBounds3 mousePositionWorld feeler.Position feeler.Size then
                    let feeler = Entity.setTouched true feeler
                    let world = World.setEntity address feeler world
                    let world = World.publish4 mouseButtonData.Position (TouchEventAddress ->>- address) address world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)
    
        let handleMouseLeftUp event world =
            let (address, feeler : Entity, mouseButtonData : MouseButtonData) = World.unwrapASD event world
            if World.isAddressSelected address world && feeler.Enabled && feeler.Visible then
                let feeler = Entity.setTouched false feeler
                let world = World.setEntity address feeler world
                let world = World.publish4 mouseButtonData.Position (ReleaseEventAddress ->>- address) address world
                (Resolve, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Touched false
             define? SwallowMouseLeft false]

        override dispatcher.Register (address, feeler, world) =
            let world =
                world |>
                World.monitor handleMouseLeftDown MouseLeftDownEventAddress address |>
                World.monitor handleMouseLeftUp MouseLeftUpEventAddress address
            (feeler, world)

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member fillBar.Fill = fillBar?Fill : single
        static member setFill (value : single) (fillBar : Entity) = fillBar?Fill <- value
        member fillBar.FillInset = fillBar?FillInset : single
        static member setFillInset (value : single) (fillBar : Entity) = fillBar?FillInset <- value
        member fillBar.FillImage = fillBar?FillImage : Image
        static member setFillImage (value : Image) (fillBar : Entity) = fillBar?FillImage <- value
        member fillBar.BorderImage = fillBar?BorderImage : Image
        static member setBorderImage (value : Image) (fillBar : Entity) = fillBar?BorderImage <- value

    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        let getFillBarSpriteDims (fillBar : Entity) =
            let spriteInset = fillBar.Size * fillBar.FillInset * 0.5f
            let spritePosition = fillBar.Position + spriteInset
            let spriteWidth = (fillBar.Size.X - spriteInset.X * 2.0f) * fillBar.Fill
            let spriteHeight = fillBar.Size.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member FieldDefinitions =
            [define? Fill 0.0f
             define? FillInset 0.0f
             define? FillImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image9" }
             define? BorderImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image10" }
             define? SwallowMouseLeft true]

        override dispatcher.GetRenderDescriptors (fillBar, _) =
            if fillBar.Visible then
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar
                [LayerableDescriptor
                    { Depth = fillBar.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = fillBar.Position
                              Size = fillBar.Size
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Image = fillBar.BorderImage
                              Color = Vector4.One }}
                 LayerableDescriptor
                    { Depth = fillBar.Depth
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = fillBarSpritePosition
                              Size = fillBarSpriteSize
                              Rotation = 0.0f
                              ViewType = Absolute
                              OptInset = None
                              Image = fillBar.FillImage
                              Color = Vector4.One }}]
            else []

        override dispatcher.GetQuickSize (fillBar, world) =
            let imageAssetTag = Image.toAssetTag fillBar.BorderImage
            match Metadata.tryGetTextureSizeAsVector2 imageAssetTag world.State.AssetMetadataMap with
            | Some size -> size
            | None -> DefaultEntitySize

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? BodyType Static
             define? SpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image3" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module BoxDispatcherModule =

    type BoxDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? SpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image3" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module TopViewCharacterDispatcherModule =

    type TopViewCharacterDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? FixedRotation true
             define? LinearDamping 10.0f
             define? GravityScale 0.0f
             define? CollisionExpr "Circle"
             define? SpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image7" }]
        
        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module SideViewCharacterDispatcherModule =

    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? FixedRotation true
             define? LinearDamping 3.0f
             define? CollisionExpr "Capsule"
             define? SpriteImage { ImagePackageName = DefaultPackageName; ImageAssetName = "Image6" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with

        member entity.TileMapAsset = entity?TileMapAsset : TileMapAsset
        static member setTileMapAsset (value : TileMapAsset) (entity : Entity) = entity?TileMapAsset <- value
        member entity.Parallax = entity?Parallax : single
        static member setParallax (value : single) (entity : Entity) = entity?Parallax <- value

        static member makeTileMapData (tileMapAsset : TileMapAsset) world =
            let tileMapAssetTag = TileMapAsset.toAssetTag tileMapAsset
            let map = __c <| Metadata.getTileMapMetadata tileMapAssetTag world.State.AssetMetadataMap
            let mapSize = Vector2i (map.Width, map.Height)
            let tileSize = Vector2i (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
            let tileMapSize = Vector2i (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
            let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let optTileSetWidth = tileSet.Image.Width
            let optTileSetHeight = tileSet.Image.Height
            let tileSetSize = Vector2i (optTileSetWidth.Value / tileSize.X, optTileSetHeight.Value / tileSize.Y)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

        static member makeTileData (tileMap : Entity) tmd (tl : TmxLayer) tileIndex =
            let mapRun = tmd.MapSize.X
            let tileSetRun = tmd.TileSetSize.X
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * tmd.TileSize.X
            let gid2 = Vector2i (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tileMap.Position
            let tilePosition =
                Vector2i (
                    int tileMapPosition.X + tmd.TileSize.X * i,
                    int tileMapPosition.Y - tmd.TileSize.Y * (j + 1)) // subtraction for right-handedness
            let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        let getTileBodyProperties6 (tm : Entity) tmd tli td ti cexpr =
            let tileShape = Physics.evalCollisionExpr (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            { BodyId = intsToGuid tli ti
              Position =
                Vector2 (
                    single <| td.TilePosition.X + tmd.TileSize.X / 2,
                    single <| td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y)
              Rotation = tm.Rotation
              Shape = tileShape
              BodyType = BodyType.Static
              Density = NormalDensity
              Friction = tm.Friction
              Restitution = tm.Restitution
              FixedRotation = true
              LinearDamping = 0.0f
              AngularDamping = 0.0f
              GravityScale = 0.0f
              CollisionCategories = Physics.toCollisionCategories tm.CollisionCategories
              CollisionMask = Physics.toCollisionCategories tm.CollisionMask
              IsBullet = false
              IsSensor = false }

        let getTileBodyProperties tm tmd (tl : TmxLayer) tli ti =
            let td = Entity.makeTileData tm tmd tl ti
            match td.OptTileSetTile with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue CollisionProperty with
                | (true, collisionProperty) ->
                    let collisionExpr = acstring collisionProperty
                    let tileBodyProperties = getTileBodyProperties6 tm tmd tli td ti collisionExpr
                    Some tileBodyProperties
                | (false, _) -> None
            | None -> None

        let getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex (tileLayer : TmxLayer) =
            if tileLayer.Properties.ContainsKey CollisionProperty then
                Seq.foldi
                    (fun i bodyPropertyList _ ->
                        match getTileBodyProperties tileMap tileMapData tileLayer tileLayerIndex i with
                        | Some bodyProperties -> bodyProperties :: bodyPropertyList
                        | None -> bodyPropertyList)
                    []
                    tileLayer.Tiles
            else []
        
        let registerTileLayerPhysics address tileMap tileMapData tileLayerIndex world tileLayer =
            let bodyPropertyList = getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex tileLayer
            World.createBodies address tileMap.Id bodyPropertyList world

        let registerTileMapPhysics address (tileMap : Entity) world =
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (registerTileLayerPhysics address tileMap tileMapData)
                world
                tileMapData.Map.Layers

        let getTileLayerPhysicsIds (tileMap : Entity) tileMapData tileLayer tileLayerIndex =
            Seq.foldi
                (fun tileIndex physicsIds _ ->
                    let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex
                    match tileData.OptTileSetTile with
                    | Some tileSetTile ->
                        if tileSetTile.Properties.ContainsKey CollisionProperty then
                            let physicsId = { SourceId = tileMap.Id; BodyId = intsToGuid tileLayerIndex tileIndex }
                            physicsId :: physicsIds
                        else physicsIds
                    | None -> physicsIds)
                []
                tileLayer.Tiles

        let unregisterTileMapPhysics (tileMap : Entity) world =
            let tileMapData = Entity.makeTileMapData tileMap.TileMapAsset world
            Seq.foldi
                (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                    if tileLayer.Properties.ContainsKey CollisionProperty then
                        let physicsIds = getTileLayerPhysicsIds tileMap tileMapData tileLayer tileLayerIndex
                        World.destroyBodies physicsIds world
                    else world)
                world
                tileMapData.Map.Layers

        static member FieldDefinitions =
            [define? Friction 0.0f
             define? Restitution 0.0f
             define? CollisionCategories "1"
             define? CollisionMask "*"
             define? TileMapAsset { TileMapPackageName = DefaultPackageName; TileMapAssetName = "TileMap" }
             define? Parallax 0.0f]

        override dispatcher.Register (address, tileMap, world) =
            let world = registerTileMapPhysics address tileMap world
            (tileMap, world)

        override dispatcher.Unregister (_, tileMap, world) =
            let world = unregisterTileMapPhysics tileMap world
            (tileMap, world)
            
        override dispatcher.PropagatePhysics (address, tileMap, world) =
            world |>
                unregisterTileMapPhysics tileMap |>
                registerTileMapPhysics address tileMap

        override dispatcher.GetRenderDescriptors (tileMap, world) =
            if tileMap.Visible then
                let tileMapAssetTag = TileMapAsset.toAssetTag tileMap.TileMapAsset
                match Metadata.tryGetTileMapMetadata tileMapAssetTag world.State.AssetMetadataMap with
                | Some (_, images, map) ->
                    let layers = List.ofSeq map.Layers
                    let tileSourceSize = Vector2i (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                    List.foldi
                        (fun i descriptors (layer : TmxLayer) ->
                            let depth = tileMap.Depth + single i * 2.0f // MAGIC_VALUE: assumption
                            let parallaxTranslation =
                                match tileMap.ViewType with
                                | Absolute -> Vector2.Zero
                                | Relative -> tileMap.Parallax * depth * -world.Camera.EyeCenter
                            let parallaxPosition = tileMap.Position + parallaxTranslation
                            let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                            if Camera.inView3 tileMap.ViewType parallaxPosition size world.Camera then
                                let descriptor =
                                    LayerableDescriptor 
                                        { Depth = depth
                                          LayeredDescriptor =
                                            TileLayerDescriptor
                                                { Position = parallaxPosition
                                                  Size = size
                                                  Rotation = tileMap.Rotation
                                                  ViewType = tileMap.ViewType
                                                  MapSize = Vector2i (map.Width, map.Height)
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
            let tileMapAssetTag = TileMapAsset.toAssetTag tileMap.TileMapAsset
            match Metadata.tryGetTileMapMetadata tileMapAssetTag world.State.AssetMetadataMap with
            | Some (_, _, map) -> Vector2 (single <| map.Width * map.TileWidth, single <| map.Height * map.TileHeight)
            | None -> DefaultEntitySize