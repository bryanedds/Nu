// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open TiledSharp
open Nu

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with

        member this.GetMinorId world : Guid = (this.GetXtension world)?MinorId
        member this.SetMinorId (value : Guid) world = this.UpdateXtension (fun xtension -> xtension?MinorId <- value) world
        member this.GetBodyType world : BodyType = (this.GetXtension world)?BodyType
        member this.SetBodyType (value : BodyType) world = this.UpdateXtension (fun xtension -> xtension?BodyType <- value) world
        member this.GetAwake world : bool = (this.GetXtension world)?Awake
        member this.SetAwake (value : bool) world = this.UpdateXtension (fun xtension -> xtension?Awake <- value) world
        member this.GetDensity world : single = (this.GetXtension world)?Density
        member this.SetDensity (value : single) world = this.UpdateXtension (fun xtension -> xtension?Density <- value) world
        member this.GetFriction world : single = (this.GetXtension world)?Friction
        member this.SetFriction (value : single) world = this.UpdateXtension (fun xtension -> xtension?Friction <- value) world
        member this.GetRestitution world : single = (this.GetXtension world)?Restitution
        member this.SetRestitution (value : single) world = this.UpdateXtension (fun xtension -> xtension?Restitution <- value) world
        member this.GetFixedRotation world : bool = (this.GetXtension world)?FixedRotation
        member this.SetFixedRotation (value : bool) world = this.UpdateXtension (fun xtension -> xtension?FixedRotation <- value) world
        member this.GetAngularVelocity world : single = (this.GetXtension world)?AngularVelocity
        member this.SetAngularVelocity (value : single) world = this.UpdateXtension (fun xtension -> xtension?AngularVelocity <- value) world
        member this.GetAngularDamping world : single = (this.GetXtension world)?AngularDamping
        member this.SetAngularDamping (value : single) world = this.UpdateXtension (fun xtension -> xtension?AngularDamping <- value) world
        member this.GetLinearVelocity world : Vector2 = (this.GetXtension world)?LinearVelocity
        member this.SetLinearVelocity (value : Vector2) world = this.UpdateXtension (fun xtension -> xtension?LinearVelocity <- value) world
        member this.GetLinearDamping world : single = (this.GetXtension world)?LinearDamping
        member this.SetLinearDamping (value : single) world = this.UpdateXtension (fun xtension -> xtension?LinearDamping <- value) world
        member this.GetGravityScale world : single = (this.GetXtension world)?GravityScale
        member this.SetGravityScale (value : single) world = this.UpdateXtension (fun xtension -> xtension?GravityScale <- value) world
        member this.GetCollisionCategories world : string = (this.GetXtension world)?CollisionCategories
        member this.SetCollisionCategories (value : string) world = this.UpdateXtension (fun xtension -> xtension?CollisionCategories <- value) world
        member this.GetCollisionMask world : string = (this.GetXtension world)?CollisionMask
        member this.SetCollisionMask (value : string) world = this.UpdateXtension (fun xtension -> xtension?CollisionMask <- value) world
        member this.GetCollisionExpr world : string = (this.GetXtension world)?CollisionExpr
        member this.SetCollisionExpr (value : string) world = this.UpdateXtension (fun xtension -> xtension?CollisionExpr <- value) world
        member this.GetIsBullet world : bool = (this.GetXtension world)?IsBullet
        member this.SetIsBullet (value : bool) world = this.UpdateXtension (fun xtension -> xtension?IsBullet <- value) world
        member this.GetIsSensor world : bool = (this.GetXtension world)?IsSensor
        member this.SetIsSensor (value : bool) world = this.UpdateXtension (fun xtension -> xtension?IsSensor <- value) world
        member this.GetPhysicsId world = { SourceId = this.GetId world; BodyId = this.GetMinorId world }

    type RigidBodyFacet () =
        inherit Facet ()

        static let getBodyShape (entity : Entity) world =
            Physics.evalCollisionExpr (entity.GetSize world) (entity.GetCollisionExpr world)

        static member FieldDefinitions =
            [variable? MinorId ^ fun () -> Core.makeId ()
             define? BodyType Dynamic
             define? Awake true
             define? Density Constants.Physics.NormalDensity
             define? Friction 0.0f
             define? Restitution 0.0f
             define? FixedRotation false
             define? AngularVelocity 0.0f
             define? AngularDamping 1.0f
             define? LinearVelocity Vector2.Zero
             define? LinearDamping 1.0f
             define? GravityScale 1.0f
             define? CollisionCategories "1"
             define? CollisionMask "*"
             define? CollisionExpr "[BodyBox [[0.5 0.5] [0.0 0.0]]]"
             define? IsBullet false
             define? IsSensor false]

        override facet.RegisterPhysics (entity, world) =
            let bodyProperties = 
                { BodyId = (entity.GetPhysicsId world).BodyId
                  Position = entity.GetPosition world + entity.GetSize world * 0.5f
                  Rotation = entity.GetRotation world
                  Shape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  Awake = entity.GetAwake world
                  Density = entity.GetDensity world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  FixedRotation = entity.GetFixedRotation world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  GravityScale = entity.GetGravityScale world
                  CollisionCategories = Physics.toCollisionCategories ^ entity.GetCollisionCategories world
                  CollisionMask = Physics.toCollisionCategories ^ entity.GetCollisionMask world
                  IsBullet = entity.GetIsBullet world
                  IsSensor = entity.GetIsSensor world }
            World.createBody entity.EntityAddress (entity.GetId world) bodyProperties world

        override facet.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

        override facet.PropagatePhysics (entity, world) =
            let world = facet.UnregisterPhysics (entity, world)
            facet.RegisterPhysics (entity, world)

[<AutoOpen>]
module SpriteFacetModule =

    type Entity with

        member this.GetSpriteImage world : AssetTag = (this.GetXtension world)?SpriteImage
        member this.SetSpriteImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?SpriteImage <- value) world

    type SpriteFacet () =
        inherit Facet ()

        static member FieldDefinitions =
            [define? SpriteImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

        override facet.GetRenderDescriptors (entity, world) =
            if World.getCameraBy (Camera.inView3 (entity.GetViewType world) (entity.GetPosition world) (entity.GetSize world)) world then
                [LayerableDescriptor
                    { Depth = entity.GetDepth world
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.GetPosition world
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              ViewType = entity.GetViewType world
                              OptInset = None
                              Image = entity.GetSpriteImage world
                              Color = Vector4.One }}]
            else []

        override facet.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeAsVector2 (entity.GetSpriteImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
    
        member this.GetTileCount world : int = (this.GetXtension world)?TileCount
        member this.SetTileCount (value : int) world = this.UpdateXtension (fun xtension -> xtension?TileCount <- value) world
        member this.GetTileRun world : int = (this.GetXtension world)?TileRun
        member this.SetTileRun (value : int) world = this.UpdateXtension (fun xtension -> xtension?TileRun <- value) world
        member this.GetTileSize world : Vector2 = (this.GetXtension world)?TileSize
        member this.SetTileSize (value : Vector2) world = this.UpdateXtension (fun xtension -> xtension?TileSize <- value) world
        member this.GetAnimationStutter world : int64 = (this.GetXtension world)?AnimationStutter
        member this.SetAnimationStutter (value : int64) world = this.UpdateXtension (fun xtension -> xtension?AnimationStutter <- value) world
        member this.GetAnimationSheet world : AssetTag = (this.GetXtension world)?AnimationSheet
        member this.SetAnimationSheet (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?AnimationSheet <- value) world

    type AnimatedSpriteFacet () =
        inherit Facet ()

        static let getOptSpriteInset (entity : Entity) world =
            let tile = int (World.getTickTime world / entity.GetAnimationStutter world) % entity.GetTileCount world
            let tileRun = entity.GetTileRun world
            let tileSize = entity.GetTileSize world
            let tileI = tile % tileRun
            let tileJ = tile / tileRun
            let tileX = single tileI * tileSize.X
            let tileY = single tileJ * tileSize.Y
            let inset = Vector4 (tileX, tileY, tileX + tileSize.X, tileY + tileSize.Y)
            Some inset

        static member FieldDefinitions =
            [define? TileCount 16 
             define? TileRun 4
             define? TileSize ^ Vector2 (16.0f, 16.0f)
             define? AnimationStutter 4L
             define? AnimationSheet { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image7" }]

        override facet.GetRenderDescriptors (entity, world) =
            if World.getCameraBy (Camera.inView3 (entity.GetViewType world) (entity.GetPosition world) (entity.GetSize world)) world then
                [LayerableDescriptor
                    { Depth = entity.GetDepth world
                      LayeredDescriptor =
                        SpriteDescriptor
                            { Position = entity.GetPosition world
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              ViewType = entity.GetViewType world
                              OptInset = getOptSpriteInset entity world
                              Image = entity.GetAnimationSheet world
                              Color = Vector4.One }}]
            else []

        override facet.GetQuickSize (entity, world) =
            entity.GetTileSize world

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
    
        member this.GetEnabled world : bool = (this.GetXtension world)?Enabled
        member this.SetEnabled (value : bool) world = this.UpdateXtension (fun xtension -> xtension?Enabled <- value) world
        member this.GetDisabledColor world : Vector4 = (this.GetXtension world)?DisabledColor
        member this.SetDisabledColor (value : Vector4) world = this.UpdateXtension (fun xtension -> xtension?DisabledColor <- value) world
        member this.GetSwallowMouseLeft world : bool = (this.GetXtension world)?SwallowMouseLeft
        member this.SetSwallowMouseLeft (value : bool) world = this.UpdateXtension (fun xtension -> xtension?SwallowMouseLeft <- value) world

    type GuiDispatcher () =
        inherit EntityDispatcher ()

        static let handleMouseLeft event world =
            let gui = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            let eventHandling =
                if World.isSimulantSelected gui world && gui.GetVisible world then
                    let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (gui.GetViewType world) data.Position) world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds3 mousePositionWorld (gui.GetPosition world) (gui.GetSize world) then
                       Resolve
                    else Cascade
                else Cascade
            (eventHandling, world)
        
        static member FieldDefinitions =
            [define? ViewType Absolute
             define? Enabled true
             define? DisabledColor ^ Vector4 0.75f
             define? SwallowMouseLeft true]

        override dispatcher.Register (gui, world) =
            world |>
                World.monitor handleMouseLeft Events.MouseLeftDown gui |>
                World.monitor handleMouseLeft Events.MouseLeftUp gui

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
    
        member this.GetDown world : bool = (this.GetXtension world)?Down
        member this.SetDown (value : bool) world = this.UpdateXtension (fun xtension -> xtension?Down <- value) world
        member this.GetUpImage world : AssetTag = (this.GetXtension world)?UpImage
        member this.SetUpImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?UpImage <- value) world
        member this.GetDownImage world : AssetTag = (this.GetXtension world)?DownImage
        member this.SetDownImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?DownImage <- value) world
        member this.GetOptClickSound world : AssetTag option = (this.GetXtension world)?OptClickSound
        member this.SetOptClickSound (value : AssetTag option) world = this.UpdateXtension (fun xtension -> xtension?OptClickSound <- value) world

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown event world =
            let button = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected button world then
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (button.GetViewType world) data.Position) world
                if  button.GetVisible world &&
                    Math.isPointInBounds3 mousePositionWorld (button.GetPosition world) (button.GetSize world) then
                    if button.GetEnabled world then
                        let world = button.SetDown true world
                        let world = World.publish () (Events.Down ->- button) button world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp event world =
            let button = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected button world then
                let wasDown = button.GetDown world
                let world = button.SetDown false world
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (button.GetViewType world) data.Position) world
                if  button.GetVisible world &&
                    Math.isPointInBounds3 mousePositionWorld (button.GetPosition world) (button.GetSize world) then
                    if button.GetEnabled world && wasDown then
                        let world = World.publish () (Events.Up ->- button) button world
                        let world = World.publish () (Events.Click ->- button) button world
                        let world =
                            match button.GetOptClickSound world with
                            | Some clickSound -> World.playSound 1.0f clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? SwallowMouseLeft false
             define? Down false
             define? UpImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image" }
             define? DownImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image2" }
             define? OptClickSound ^ Some { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Sound" }]

        override dispatcher.Register (button, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown button |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp button

        override dispatcher.GetRenderDescriptors (button, world) =
            [LayerableDescriptor
                { Depth = button.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = button.GetPosition world
                          Size = button.GetSize world
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = if button.GetDown world then button.GetDownImage world else button.GetUpImage world
                          Color = if button.GetEnabled world then Vector4.One else button.GetDisabledColor world }}]

        override dispatcher.GetQuickSize (button, world) =
            match Metadata.tryGetTextureSizeAsVector2 (button.GetUpImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
    
        member this.GetLabelImage world : AssetTag = (this.GetXtension world)?LabelImage
        member this.SetLabelImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?LabelImage <- value) world

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member FieldDefinitions =
            [define? SwallowMouseLeft true
             define? LabelImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image4" }]

        override dispatcher.GetRenderDescriptors (label, world) =
            [LayerableDescriptor
                { Depth = label.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = label.GetPosition world
                          Size = label.GetSize world
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = label.GetLabelImage world
                          Color = if label.GetEnabled world then Vector4.One else label.GetDisabledColor world }}]

        override dispatcher.GetQuickSize (label, world) =
            match Metadata.tryGetTextureSizeAsVector2 (label.GetLabelImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
    
        member this.GetText world : string = (this.GetXtension world)?Text
        member this.SetText (value : string) world = this.UpdateXtension (fun xtension -> xtension?Text <- value) world
        member this.GetTextFont world : AssetTag = (this.GetXtension world)?TextFont
        member this.SetTextFont (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?TextFont <- value) world
        member this.GetTextOffset world : Vector2 = (this.GetXtension world)?TextOffset
        member this.SetTextOffset (value : Vector2) world = this.UpdateXtension (fun xtension -> xtension?TextOffset <- value) world
        member this.GetTextColor world : Vector4 = (this.GetXtension world)?TextColor
        member this.SetTextColor (value : Vector4) world = this.UpdateXtension (fun xtension -> xtension?TextColor <- value) world
        member this.GetBackgroundImage world : AssetTag = (this.GetXtension world)?BackgroundImage
        member this.SetBackgroundImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?BackgroundImage <- value) world

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member FieldDefinitions =
            [define? SwallowMouseLeft true
             define? Text String.Empty
             define? TextFont { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Font" }
             define? TextOffset Vector2.Zero
             define? TextColor Vector4.One
             define? BackgroundImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image4" }]

        override dispatcher.GetRenderDescriptors (text, world) =
            [LayerableDescriptor
                { Depth = text.GetDepth world
                  LayeredDescriptor =
                    TextDescriptor
                        { Text = text.GetText world
                          Position = (text.GetPosition world + text.GetTextOffset world)
                          Size = text.GetSize world - text.GetTextOffset world
                          ViewType = Absolute
                          Font = text.GetTextFont world
                          Color = text.GetTextColor world }}
             LayerableDescriptor
                { Depth = text.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = text.GetPosition world
                          Size = text.GetSize world
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = text.GetBackgroundImage world
                          Color = if text.GetEnabled world then Vector4.One else text.GetDisabledColor world }}]

        override dispatcher.GetQuickSize (text, world) =
            match Metadata.tryGetTextureSizeAsVector2 (text.GetBackgroundImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with
    
        member this.GetOn world : bool = (this.GetXtension world)?On
        member this.SetOn (value : bool) world = this.UpdateXtension (fun xtension -> xtension?On <- value) world
        member this.GetPressed world : bool = (this.GetXtension world)?Pressed
        member this.SetPressed (value : bool) world = this.UpdateXtension (fun xtension -> xtension?Pressed <- value) world
        member this.GetOffImage world : AssetTag = (this.GetXtension world)?OffImage
        member this.SetOffImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?OffImage <- value) world
        member this.GetOnImage world : AssetTag = (this.GetXtension world)?OnImage
        member this.SetOnImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?OnImage <- value) world
        member this.GetOptToggleSound world : AssetTag option = (this.GetXtension world)?OptToggleSound
        member this.SetOptToggleSound (value : AssetTag option) world = this.UpdateXtension (fun xtension -> xtension?OptToggleSound <- value) world

    type ToggleDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown event world =
            let toggle = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected toggle world then
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (toggle.GetViewType world) data.Position) world
                if  toggle.GetVisible world &&
                    Math.isPointInBounds3 mousePositionWorld (toggle.GetPosition world) (toggle.GetSize world) then
                    if toggle.GetEnabled world then
                        let world = toggle.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp event world =
            let toggle = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected toggle world then
                let wasPressed = toggle.GetPressed world
                let world = toggle.SetPressed false world
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (toggle.GetViewType world) data.Position) world
                if  toggle.GetVisible world &&
                    Math.isPointInBounds3 mousePositionWorld (toggle.GetPosition world) (toggle.GetSize world) then
                    if toggle.GetEnabled world && wasPressed then
                        let world = toggle.SetOn (not ^ toggle.GetOn world) world
                        let eventAddress = if toggle.GetOn world then Events.On else Events.Off
                        let world = World.publish () (eventAddress ->- toggle) toggle world
                        let world =
                            match toggle.GetOptToggleSound world with
                            | Some toggleSound -> World.playSound 1.0f toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? SwallowMouseLeft false
             define? On false
             define? Pressed false
             define? OffImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image" }
             define? OnImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image2" }
             define? OptToggleSound ^ Some { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Sound" }]

        override dispatcher.Register (toggle, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown toggle |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp toggle

        override dispatcher.GetRenderDescriptors (toggle, world) =
            [LayerableDescriptor
                { Depth = toggle.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = toggle.GetPosition world
                          Size = toggle.GetSize world
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = if toggle.GetOn world || toggle.GetPressed world then toggle.GetOnImage world else toggle.GetOffImage world
                          Color = if toggle.GetEnabled world then Vector4.One else toggle.GetDisabledColor world }}]

        override dispatcher.GetQuickSize (toggle, world) =
            match Metadata.tryGetTextureSizeAsVector2 (toggle.GetOffImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
    
        member this.GetTouched world : bool = (this.GetXtension world)?Touched
        member this.SetTouched (value : bool) world = this.UpdateXtension (fun xtension -> xtension?Touched <- value) world

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown event world =
            let feeler = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected feeler world then
                let mousePositionWorld = World.getCameraBy (Camera.mouseToWorld (feeler.GetViewType world) data.Position) world
                if  feeler.GetVisible world &&
                    Math.isPointInBounds3 mousePositionWorld (feeler.GetPosition world) (feeler.GetSize world) then
                    if feeler.GetEnabled world then
                        let world = feeler.SetTouched true world
                        let world = World.publish data.Position (Events.Touch ->- feeler) feeler world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp event world =
            let feeler = event.Subscriber : Entity
            let data = event.Data : MouseButtonData
            if World.isSimulantSelected feeler world && feeler.GetVisible world then
                if feeler.GetEnabled world then
                    let world = feeler.SetTouched false world
                    let world = World.publish data.Position (Events.Untouch ->- feeler) feeler world
                    (Resolve, world)
                else (Resolve, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? SwallowMouseLeft false
             define? Touched false]

        override dispatcher.Register (feeler, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown feeler |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp feeler

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member this.GetFill world : single = (this.GetXtension world)?Fill
        member this.SetFill (value : single) world = this.UpdateXtension (fun xtension -> xtension?Fill <- value) world
        member this.GetFillInset world : single = (this.GetXtension world)?FillInset
        member this.SetFillInset (value : single) world = this.UpdateXtension (fun xtension -> xtension?FillInset <- value) world
        member this.GetFillImage world : AssetTag = (this.GetXtension world)?FillImage
        member this.SetFillImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?FillImage <- value) world
        member this.GetBorderImage world : AssetTag = (this.GetXtension world)?BorderImage
        member this.SetBorderImage (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?BorderImage <- value) world

    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        let getFillBarSpriteDims (fillBar : Entity) world =
            let spriteSize = fillBar.GetSize world
            let spriteInset = spriteSize * fillBar.GetFillInset world * 0.5f
            let spritePosition = fillBar.GetPosition world + spriteInset
            let spriteWidth = (spriteSize.X - spriteInset.X * 2.0f) * fillBar.GetFill world
            let spriteHeight = spriteSize.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member FieldDefinitions =
            [define? SwallowMouseLeft true
             define? Fill 0.0f
             define? FillInset 0.0f
             define? FillImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image9" }
             define? BorderImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image10" }]

        override dispatcher.GetRenderDescriptors (fillBar, world) =
            let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar world
            let fillBarColor = if fillBar.GetEnabled world then Vector4.One else fillBar.GetDisabledColor world
            [LayerableDescriptor
                { Depth = fillBar.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = fillBar.GetPosition world
                          Size = fillBar.GetSize world
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = fillBar.GetBorderImage world
                          Color = fillBarColor }}
             LayerableDescriptor
                { Depth = fillBar.GetDepth world
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = fillBarSpritePosition
                          Size = fillBarSpriteSize
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = fillBar.GetFillImage world
                          Color = fillBarColor }}]

        override dispatcher.GetQuickSize (fillBar, world) =
            match Metadata.tryGetTextureSizeAsVector2 (fillBar.GetBorderImage world) world.State.AssetMetadataMap with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? BodyType Static
             define? SpriteImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module BoxDispatcherModule =

    type BoxDispatcher () =
        inherit EntityDispatcher ()

        static member FieldDefinitions =
            [define? SpriteImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

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
             define? CollisionExpr "[BodyCircle [0.5 [0.0 0.0]]]"
             define? SpriteImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image7" }]
        
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
             define? CollisionExpr "[BodyCapsule [0.5 0.25 [0.0 0.0]]]"
             define? SpriteImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image6" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with
    
        member this.GetTileMapAsset world : AssetTag = (this.GetXtension world)?TileMapAsset
        member this.SetTileMapAsset (value : AssetTag) world = this.UpdateXtension (fun xtension -> xtension?TileMapAsset <- value) world
        member this.GetParallax world : single = (this.GetXtension world)?Parallax
        member this.SetParallax (value : single) world = this.UpdateXtension (fun xtension -> xtension?Parallax <- value) world

        static member makeTileMapData (tileMapAsset : AssetTag) world =
            let map = __c ^ Metadata.getTileMapMetadata tileMapAsset world.State.AssetMetadataMap
            let mapSize = Vector2i (map.Width, map.Height)
            let tileSize = Vector2i (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
            let tileMapSize = Vector2i (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
            let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let tileSetSize =
                let optTileSetWidth = tileSet.Image.Width
                let optTileSetHeight = tileSet.Image.Height
                Vector2i (optTileSetWidth.Value / tileSize.X, optTileSetHeight.Value / tileSize.Y)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

        static member makeTileData (tm : Entity) tmd (tl : TmxLayer) tileIndex world =
            let mapRun = tmd.MapSize.X
            let tileSetRun = tmd.TileSetSize.X
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * tmd.TileSize.X
            let gid2 = Vector2i (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tm.GetPosition world
            let tilePosition =
                Vector2i
                    (int tileMapPosition.X + tmd.TileSize.X * i,
                     int tileMapPosition.Y - tmd.TileSize.Y * (j + 1)) // subtraction for right-handedness
            let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        let getTileBodyProperties6 (tm : Entity) tmd tli td ti cexpr world =
            let tileShape = Physics.evalCollisionExpr (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            { BodyId = intsToGuid tli ti
              Position =
                Vector2
                    (single ^ td.TilePosition.X + tmd.TileSize.X / 2,
                     single ^ td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y)
              Rotation = tm.GetRotation world
              Shape = tileShape
              BodyType = BodyType.Static
              Awake = false
              Density = Constants.Physics.NormalDensity
              Friction = tm.GetFriction world
              Restitution = tm.GetRestitution world
              FixedRotation = true
              AngularVelocity = 0.0f
              AngularDamping = 0.0f
              LinearVelocity = Vector2.Zero
              LinearDamping = 0.0f
              GravityScale = 0.0f
              CollisionCategories = Physics.toCollisionCategories ^ tm.GetCollisionCategories world
              CollisionMask = Physics.toCollisionCategories ^ tm.GetCollisionMask world
              IsBullet = false
              IsSensor = false }

        let getTileBodyProperties tm tmd (tl : TmxLayer) tli ti world =
            let td = Entity.makeTileData tm tmd tl ti world
            match td.OptTileSetTile with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.Physics.CollisionProperty with
                | (true, collisionProperty) ->
                    let collisionExpr = acstring collisionProperty
                    let tileBodyProperties = getTileBodyProperties6 tm tmd tli td ti collisionExpr world
                    Some tileBodyProperties
                | (false, _) -> None
            | None -> None

        let getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex (tileLayer : TmxLayer) world =
            if tileLayer.Properties.ContainsKey Constants.Physics.CollisionProperty then
                Seq.foldi
                    (fun i bodyPropertyList _ ->
                        match getTileBodyProperties tileMap tileMapData tileLayer tileLayerIndex i world with
                        | Some bodyProperties -> bodyProperties :: bodyPropertyList
                        | None -> bodyPropertyList)
                    []
                    tileLayer.Tiles
            else []

        let registerTileLayerPhysics (tileMap : Entity) tileMapData tileLayerIndex world tileLayer =
            let bodyPropertyList = getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex tileLayer world
            World.createBodies tileMap.EntityAddress (tileMap.GetId world) bodyPropertyList world

        let registerTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            let tileMapData = Entity.makeTileMapData tileMapAsset world
            Seq.foldi
                (registerTileLayerPhysics tileMap tileMapData)
                world
                tileMapData.Map.Layers

        let getTileLayerPhysicsIds (tileMap : Entity) tileMapData tileLayer tileLayerIndex world =
            Seq.foldi
                (fun tileIndex physicsIds _ ->
                    let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex world
                    match tileData.OptTileSetTile with
                    | Some tileSetTile ->
                        if tileSetTile.Properties.ContainsKey Constants.Physics.CollisionProperty then
                            let physicsId = { SourceId = tileMap.GetId world; BodyId = intsToGuid tileLayerIndex tileIndex }
                            physicsId :: physicsIds
                        else physicsIds
                    | None -> physicsIds)
                []
                tileLayer.Tiles

        let unregisterTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            let tileMapData = Entity.makeTileMapData tileMapAsset world
            Seq.foldi
                (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                    if tileLayer.Properties.ContainsKey Constants.Physics.CollisionProperty then
                        let physicsIds = getTileLayerPhysicsIds tileMap tileMapData tileLayer tileLayerIndex world
                        World.destroyBodies physicsIds world
                    else world)
                world
                tileMapData.Map.Layers

        static member FieldDefinitions =
            [define? Omnipresent true
             define? Friction 0.0f
             define? Restitution 0.0f
             define? CollisionCategories "1"
             define? CollisionMask "*"
             define? TileMapAsset { PackageName = Constants.Assets.DefaultPackageName; AssetName = "TileMap" }
             define? Parallax 0.0f]

        override dispatcher.Register (tileMap, world) =
            registerTileMapPhysics tileMap world

        override dispatcher.Unregister (tileMap, world) =
            unregisterTileMapPhysics tileMap world
            
        override dispatcher.PropagatePhysics (tileMap, world) =
            world |>
                unregisterTileMapPhysics tileMap |>
                registerTileMapPhysics tileMap

        override dispatcher.GetRenderDescriptors (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) world.State.AssetMetadataMap with
            | Some (_, images, map) ->
                let camera = World.getCamera world
                let layers = List.ofSeq map.Layers
                let tileSourceSize = Vector2i (map.TileWidth, map.TileHeight)
                let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                let viewType = tileMap.GetViewType world
                List.foldi
                    (fun i descriptors (layer : TmxLayer) ->
                        let depth = tileMap.GetDepth world + single i * 2.0f // MAGIC_VALUE: assumption
                        let parallaxTranslation =
                            match viewType with
                            | Absolute -> Vector2.Zero
                            | Relative -> tileMap.GetParallax world * depth * -camera.EyeCenter
                        let parallaxPosition = tileMap.GetPosition world + parallaxTranslation
                        let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                        if World.getCameraBy (Camera.inView3 viewType parallaxPosition size) world then
                            let descriptor =
                                LayerableDescriptor 
                                    { Depth = depth
                                      LayeredDescriptor =
                                        TileLayerDescriptor
                                            { Position = parallaxPosition
                                              Size = size
                                              Rotation = tileMap.GetRotation world
                                              ViewType = viewType
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

        override dispatcher.GetQuickSize (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) world.State.AssetMetadataMap with
            | Some (_, _, map) -> Vector2 (single ^ map.Width * map.TileWidth, single ^ map.Height * map.TileHeight)
            | None -> Constants.Engine.DefaultEntitySize