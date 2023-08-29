// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module EntityDispatcherModule =

    /// A 2d entity dispatcher.
    type EntityDispatcher2d (centered, physical) =
        inherit EntityDispatcher (true, centered, physical)

        new (physical) =
            EntityDispatcher2d (Constants.Engine.EntityCentered2dDefault, physical)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize2dDefault
             define Entity.Centered Constants.Engine.EntityCentered2dDefault]

    /// A 3d entity dispatcher.
    type EntityDispatcher3d (centered, physical) =
        inherit EntityDispatcher (false, centered, physical)

        new (physical) =
            EntityDispatcher3d (Constants.Engine.EntityCentered3dDefault, physical)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize3dDefault
             define Entity.Centered Constants.Engine.EntityCentered3dDefault]

        override this.RayCast (ray, entity, world) =
            if Array.isEmpty (entity.GetFacets world) then
                let intersectionOpt = ray.Intersects (entity.GetBounds world)
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            else base.RayCast (ray, entity, world)

[<AutoOpen>]
module StaticSpriteDispatcherModule =

    /// Gives an entity the base behavior of a static sprite.
    type StaticSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Box
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

[<AutoOpen>]
module AnimatedSpriteDispatcherModule =

    /// Gives an entity the base behavior of an animated sprite.
    type AnimatedSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AnimationSheet Assets.Default.Block // TODO: use proper animated sheet.
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
        member this.GetDisabledColor world : Color = this.Get (nameof this.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof this.DisabledColor) value world
        member this.DisabledColor = lens (nameof this.DisabledColor) this this.GetDisabledColor this.SetDisabledColor

    /// Gives an entity the base behavior of gui control.
    type GuiDispatcher () =
        inherit EntityDispatcher2d (Constants.Engine.EntityCenteredGuiDefault, false)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.Centered Constants.Engine.EntityCenteredGuiDefault
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Presence Omnipresent
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
        member this.GetDown world : bool = this.Get (nameof this.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof this.Down) value world
        member this.Down = lens (nameof this.Down) this this.GetDown this.SetDown
        member this.GetDownTextOffset world : Vector2 = this.Get (nameof this.DownTextOffset) world
        member this.SetDownTextOffset (value : Vector2) world = this.Set (nameof this.DownTextOffset) value world
        member this.DownTextOffset = lens (nameof this.DownTextOffset) this this.GetDownTextOffset this.SetDownTextOffset
        member this.GetUpImage world : Image AssetTag = this.Get (nameof this.UpImage) world
        member this.SetUpImage (value : Image AssetTag) world = this.Set (nameof this.UpImage) value world
        member this.UpImage = lens (nameof this.UpImage) this this.GetUpImage this.SetUpImage
        member this.GetDownImage world : Image AssetTag = this.Get (nameof this.DownImage) world
        member this.SetDownImage (value : Image AssetTag) world = this.Set (nameof this.DownImage) value world
        member this.DownImage = lens (nameof this.DownImage) this this.GetDownImage this.SetDownImage
        member this.GetClickSoundOpt world : Sound AssetTag option = this.Get (nameof this.ClickSoundOpt) world
        member this.SetClickSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ClickSoundOpt) value world
        member this.ClickSoundOpt = lens (nameof this.ClickSoundOpt) this this.GetClickSoundOpt this.SetClickSoundOpt
        member this.GetClickSoundVolume world : single = this.Get (nameof this.ClickSoundVolume) world
        member this.SetClickSoundVolume (value : single) world = this.Set (nameof this.ClickSoundVolume) value world
        member this.ClickSoundVolume = lens (nameof this.ClickSoundVolume) this this.GetClickSoundVolume this.SetClickSoundVolume
        member this.UpEvent = Events.Up --> this
        member this.DownEvent = Events.Down --> this
        member this.ClickEvent = Events.Click --> this

    /// Gives an entity the base behavior of a gui button.
    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetDown true world
                        let world = entity.SetTextOffset (entity.GetDownTextOffset world) world
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus () (Events.Down --> entity) eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasDown = entity.GetDown world
            let world = entity.SetDown false world
            let world = entity.SetTextOffset v2Zero world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasDown then
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publishPlus () (Events.Up --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publishPlus () (Events.Click --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Down false
             define Entity.DownTextOffset v2Zero
             define Entity.UpImage Assets.Default.ButtonUp
             define Entity.DownImage Assets.Default.ButtonDown
             define Entity.ClickSoundOpt (Some Assets.Default.Sound)
             define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
        member this.GetLabelImage world : Image AssetTag = this.Get (nameof this.LabelImage) world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set (nameof this.LabelImage) value world
        member this.LabelImage = lens (nameof this.LabelImage) this this.GetLabelImage this.SetLabelImage

    /// Gives an entity the base behavior of a gui label.
    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.LabelImage Assets.Default.Label]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage = entity.GetLabelImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetLabelImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
        member this.GetBackgroundImageOpt world : Image AssetTag option = this.Get (nameof this.BackgroundImageOpt) world
        member this.SetBackgroundImageOpt (value : Image AssetTag option) world = this.Set (nameof this.BackgroundImageOpt) value world
        member this.BackgroundImageOpt = lens (nameof this.BackgroundImageOpt) this this.GetBackgroundImageOpt this.SetBackgroundImageOpt

    /// Gives an entity the base behavior of a gui text control.
    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.BackgroundImageOpt None
             define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

        override this.Render (entity, world) =
            match entity.GetBackgroundImageOpt world with
            | Some spriteImage ->
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
                World.enqueueLayeredOperation2d
                    { Elevation = spriteTransform.Elevation
                      Horizon = spriteTransform.Horizon
                      AssetTag = AssetTag.generalize spriteImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = spriteTransform
                              InsetOpt = ValueNone
                              Image = spriteImage
                              Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> world

        override this.GetQuickSize (entity, world) =
            match entity.GetBackgroundImageOpt world with
            | Some image ->
                match Metadata.tryGetTextureSizeF image with
                | Some size -> size.V3
                | None -> Constants.Engine.EntitySizeGuiDefault
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module ToggleButtonDispatcherModule =

    type Entity with
        member this.GetToggled world : bool = this.Get (nameof this.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof this.Toggled) value world
        member this.Toggled = lens (nameof this.Toggled) this this.GetToggled this.SetToggled
        member this.GetToggledTextOffset world : Vector2 = this.Get (nameof this.ToggledTextOffset) world
        member this.SetToggledTextOffset (value : Vector2) world = this.Set (nameof this.ToggledTextOffset) value world
        member this.ToggledTextOffset = lens (nameof this.ToggledTextOffset) this this.GetToggledTextOffset this.SetToggledTextOffset
        member this.GetPressed world : bool = this.Get (nameof this.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof this.Pressed) value world
        member this.Pressed = lens (nameof this.Pressed) this this.GetPressed this.SetPressed
        member this.GetPressedTextOffset world : Vector2 = this.Get (nameof this.PressedTextOffset) world
        member this.SetPressedTextOffset (value : Vector2) world = this.Set (nameof this.PressedTextOffset) value world
        member this.PressedTextOffset = lens (nameof this.PressedTextOffset) this this.GetPressedTextOffset this.SetPressedTextOffset
        member this.GetUntoggledImage world : Image AssetTag = this.Get (nameof this.UntoggledImage) world
        member this.SetUntoggledImage (value : Image AssetTag) world = this.Set (nameof this.UntoggledImage) value world
        member this.UntoggledImage = lens (nameof this.UntoggledImage) this this.GetUntoggledImage this.SetUntoggledImage
        member this.GetToggledImage world : Image AssetTag = this.Get (nameof this.ToggledImage) world
        member this.SetToggledImage (value : Image AssetTag) world = this.Set (nameof this.ToggledImage) value world
        member this.ToggledImage = lens (nameof this.ToggledImage) this this.GetToggledImage this.SetToggledImage
        member this.GetToggleSoundOpt world : Sound AssetTag option = this.Get (nameof this.ToggleSoundOpt) world
        member this.SetToggleSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ToggleSoundOpt) value world
        member this.ToggleSoundOpt = lens (nameof this.ToggleSoundOpt) this this.GetToggleSoundOpt this.SetToggleSoundOpt
        member this.GetToggleSoundVolume world : single = this.Get (nameof this.ToggleSoundVolume) world
        member this.SetToggleSoundVolume (value : single) world = this.Set (nameof this.ToggleSoundVolume) value world
        member this.ToggleSoundVolume = lens (nameof this.ToggleSoundVolume) this this.GetToggleSoundVolume this.SetToggleSoundVolume
        member this.ToggleEvent = Events.Toggle --> this
        member this.ToggledEvent = Events.Toggled --> this
        member this.UntoggledEvent = Events.Untoggled --> this

    /// Gives an entity the base behavior of gui toggle button.
    type ToggleButtonDispatcher () =
        inherit GuiDispatcher ()
        
        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed then
                        let world = entity.SetToggled (not (entity.GetToggled world)) world
                        let toggled = entity.GetToggled world
                        let eventAddress = if toggled then Events.Toggled else Events.Untoggled
                        let eventTrace = EventTrace.debug "ToggleDispatcher" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () (eventAddress --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ToggleDispatcher" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publishPlus toggled (Events.Toggle --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound (entity.GetToggleSoundVolume world) toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Toggled false
             define Entity.ToggledTextOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v2Zero
             define Entity.UntoggledImage Assets.Default.ButtonUp
             define Entity.ToggledImage Assets.Default.ButtonDown
             define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
             define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetToggled world then entity.GetToggledTextOffset world
                else v2Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage =
                if entity.GetToggled world || entity.GetPressed world
                then entity.GetToggledImage world
                else entity.GetUntoggledImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUntoggledImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module RadioButtonDispatcherModule =

    type Entity with
        member this.GetDialed world : bool = this.Get (nameof this.Dialed) world
        member this.SetDialed (value : bool) world = this.Set (nameof this.Dialed) value world
        member this.Dialed = lens (nameof this.Dialed) this this.GetDialed this.SetDialed
        member this.GetDialedTextOffset world : Vector2 = this.Get (nameof this.DialedTextOffset) world
        member this.SetDialedTextOffset (value : Vector2) world = this.Set (nameof this.DialedTextOffset) value world
        member this.DialedTextOffset = lens (nameof this.DialedTextOffset) this this.GetDialedTextOffset this.SetDialedTextOffset
        member this.GetUndialedImage world : Image AssetTag = this.Get (nameof this.UndialedImage) world
        member this.SetUndialedImage (value : Image AssetTag) world = this.Set (nameof this.UndialedImage) value world
        member this.UndialedImage = lens (nameof this.UndialedImage) this this.GetUndialedImage this.SetUndialedImage
        member this.GetDialedImage world : Image AssetTag = this.Get (nameof this.DialedImage) world
        member this.SetDialedImage (value : Image AssetTag) world = this.Set (nameof this.DialedImage) value world
        member this.DialedImage = lens (nameof this.DialedImage) this this.GetDialedImage this.SetDialedImage
        member this.GetDialSoundOpt world : Sound AssetTag option = this.Get (nameof this.DialSoundOpt) world
        member this.SetDialSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.DialSoundOpt) value world
        member this.DialSoundOpt = lens (nameof this.DialSoundOpt) this this.GetDialSoundOpt this.SetDialSoundOpt
        member this.GetDialSoundVolume world : single = this.Get (nameof this.DialSoundVolume) world
        member this.SetDialSoundVolume (value : single) world = this.Set (nameof this.DialSoundVolume) value world
        member this.DialSoundVolume = lens (nameof this.DialSoundVolume) this this.GetDialSoundVolume this.SetDialSoundVolume
        member this.DialEvent = Events.Dial --> this
        member this.DialedEvent = Events.Dialed --> this
        member this.UndialedEvent = Events.Undialed --> this

    /// Gives an entity the base behavior of a gui radio button.
    type RadioButtonDispatcher () =
        inherit GuiDispatcher ()

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            let wasDialed = entity.GetDialed world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed && not wasDialed then
                        let world = entity.SetDialed true world
                        let dialed = entity.GetDialed world
                        let eventAddress = if dialed then Events.Dialed else Events.Undialed
                        let eventTrace = EventTrace.debug "RadioButtonDispatcher" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () (eventAddress --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "RadioButtonDispatcher" "handleMouseLeftUp" "Dial" EventTrace.empty
                        let world = World.publishPlus dialed (Events.Dial --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetDialSoundOpt world with
                            | Some dialSound -> World.playSound (entity.GetDialSoundVolume world) dialSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Dialed false
             define Entity.DialedTextOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v2Zero
             define Entity.UndialedImage Assets.Default.ButtonUp
             define Entity.DialedImage Assets.Default.ButtonDown
             define Entity.DialSoundOpt (Some Assets.Default.Sound)
             define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetDialed world then entity.GetDialedTextOffset world
                else v2Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage =
                if entity.GetDialed world || entity.GetPressed world
                then entity.GetDialedImage world
                else entity.GetUndialedImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
        member this.GetStartUpdateTime world : int64 = this.Get (nameof this.StartUpdateTime) world
        member this.SetStartUpdateTime (value : int64) world = this.Set (nameof this.StartUpdateTime) value world
        member this.StartUpdateTime = lens (nameof this.StartUpdateTime) this this.GetStartUpdateTime this.SetStartUpdateTime
        member this.GetStartDateTime world : DateTimeOffset = this.Get (nameof this.StartDateTime) world
        member this.SetStartDateTime (value : DateTimeOffset) world = this.Set (nameof this.StartDateTime) value world
        member this.StartDateTime = lens (nameof this.StartDateTime) this this.GetStartDateTime this.SetStartDateTime

    /// Gives an entity the base behavior of a gui FPS counter.
    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTimeOffset.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 5.0 then
                let world = entity.SetStartUpdateTime world.UpdateTime world
                entity.SetStartDateTime currentDateTime world
            else world

        static member Properties =
            [nonPersistent Entity.StartUpdateTime 0L
             nonPersistent Entity.StartDateTime DateTimeOffset.UtcNow]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let world = resetIntermittent entity world
                let startDateTime = entity.GetStartDateTime world
                let currentDateTime = DateTimeOffset.UtcNow
                let elapsedDateTime = currentDateTime - startDateTime
                let time = double (world.UpdateTime - entity.GetStartUpdateTime world)
                let frames = time / elapsedDateTime.TotalSeconds
                if not (Double.IsNaN frames) then
                    let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                    entity.SetText framesStr world
                else world
            else world

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
        member this.GetTouched world : bool = this.Get (nameof this.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof this.Touched) value world
        member this.Touched = lens (nameof this.Touched) this this.GetTouched this.SetTouched
        member this.TouchEvent = Events.Touch --> this
        member this.TouchingEvent = Events.Touching --> this
        member this.UntouchEvent = Events.Untouch --> this

    /// Gives an entity the base behavior of gui feeler (an invisible control that only takes mouse input).
    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetTouched true world
                        let eventTrace = EventTrace.debug "FeelerDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus data.Position (Events.Touch --> entity) eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasTouched = entity.GetTouched world
            let world = entity.SetTouched false world
            if entity.GetVisible world then
                if entity.GetEnabled world && wasTouched then
                    let eventTrace = EventTrace.debug "FeelerDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                    let world = World.publishPlus data.Position (Events.Untouch --> entity) eventTrace entity true false world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleIncoming evt world =
            let entity = evt.Subscriber : Entity
            if  MouseState.isButtonDown MouseLeft &&
                entity.GetVisible world &&
                entity.GetEnabled world then
                let mousePosition = MouseState.getPosition ()
                let world = entity.SetTouched true world
                let eventTrace = EventTrace.debug "FeelerDispatcher" "handleIncoming" "" EventTrace.empty
                let world = World.publishPlus mousePosition (Events.Touch --> entity) eventTrace entity true false world
                (Resolve, world)
            else (Cascade, world)

        static let handleOutgoing evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, entity.SetTouched false world)

        static member Properties =
            [define Entity.Touched false]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            let world = World.monitor handleIncoming (Events.IncomingFinish --> entity.Screen) entity world
            let world = World.monitor handleOutgoing (Events.OutgoingStart --> entity.Screen) entity world
            world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                if entity.GetTouched world then
                    let mousePosition = World.getMousePosition world
                    let eventTrace = EventTrace.debug "FeelerDispatcher" "Update" "" EventTrace.empty
                    let world = World.publishPlus mousePosition (Events.Touching --> entity) eventTrace entity true false world
                    world
                else world
            else world

        override this.GetQuickSize (_, _) =
            Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
        member this.GetFill world : single = this.Get (nameof this.Fill) world
        member this.SetFill (value : single) world = this.Set (nameof this.Fill) value world
        member this.Fill = lens (nameof this.Fill) this this.GetFill this.SetFill
        member this.GetFillInset world : single = this.Get (nameof this.FillInset) world
        member this.SetFillInset (value : single) world = this.Set (nameof this.FillInset) value world
        member this.FillInset = lens (nameof this.FillInset) this this.GetFillInset this.SetFillInset
        member this.GetFillColor world : Color = this.Get (nameof this.FillColor) world
        member this.SetFillColor (value : Color) world = this.Set (nameof this.FillColor) value world
        member this.FillColor = lens (nameof this.FillColor) this this.GetFillColor this.SetFillColor
        member this.GetFillImage world : Image AssetTag = this.Get (nameof this.FillImage) world
        member this.SetFillImage (value : Image AssetTag) world = this.Set (nameof this.FillImage) value world
        member this.FillImage = lens (nameof this.FillImage) this this.GetFillImage this.SetFillImage
        member this.GetBorderColor world : Color = this.Get (nameof this.BorderColor) world
        member this.SetBorderColor (value : Color) world = this.Set (nameof this.BorderColor) value world
        member this.BorderColor = lens (nameof this.BorderColor) this this.GetBorderColor this.SetBorderColor
        member this.GetBorderImage world : Image AssetTag = this.Get (nameof this.BorderImage) world
        member this.SetBorderImage (value : Image AssetTag) world = this.Set (nameof this.BorderImage) value world
        member this.BorderImage = lens (nameof this.BorderImage) this this.GetBorderImage this.SetBorderImage

    /// Gives an entity the base behavior of gui fill bar.
    type FillBarDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.White
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Border]

        override this.Render (entity, world) =

            // border sprite
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter // gui currently ignores rotation
            let horizon = transform.Horizon
            let mutable borderTransform = Transform.makeDefault transform.Centered
            borderTransform.Position <- perimeter.Min
            borderTransform.Size <- perimeter.Size
            borderTransform.Offset <- transform.Offset
            borderTransform.Elevation <- transform.Elevation + 0.5f
            borderTransform.Absolute <- transform.Absolute
            let color = if transform.Enabled then Color.White else entity.GetDisabledColor world
            let borderImageColor = entity.GetBorderColor world * color
            let borderImage = entity.GetBorderImage world
            let world =
                World.enqueueLayeredOperation2d
                    { Elevation = borderTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize borderImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = borderTransform
                              InsetOpt = ValueNone
                              Image = borderImage
                              Color = borderImageColor
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world

            // fill sprite
            let fillSize = perimeter.Size
            let fillInset = fillSize * entity.GetFillInset world * 0.5f
            let fillPosition = perimeter.Min + fillInset
            let fillWidth = (fillSize.X - fillInset.X * 2.0f) * entity.GetFill world
            let fillHeight = fillSize.Y - fillInset.Y * 2.0f
            let fillSize = v3 fillWidth fillHeight 0.0f
            let mutable fillTransform = Transform.makeDefault transform.Centered
            fillTransform.Position <- fillPosition
            fillTransform.Size <- fillSize
            fillTransform.Offset <- transform.Offset
            fillTransform.Elevation <- transform.Elevation
            fillTransform.Absolute <- transform.Absolute
            let fillImageColor = entity.GetFillColor world * color
            let fillImage = entity.GetFillImage world
            let world =
                World.enqueueLayeredOperation2d
                    { Elevation = fillTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize fillImage
                      RenderOperation2d =
                          RenderSprite
                              { Transform = fillTransform
                                InsetOpt = ValueNone
                                Image = fillImage
                                Color = fillImageColor
                                Blend = Transparent
                                Emission = Color.Zero
                                Flip = FlipNone }}
                    world

            // fin
            world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module BasicStaticSpriteEmitterDispatcherModule =

    /// Gives an entity the base behavior of basic static sprite emitter.
    type BasicStaticSpriteEmitterDispatcher () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<BasicStaticSpriteEmitterFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher2dModule =

    /// Gives an entity the base behavior of a 2d effect.
    type EffectDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<EffectFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

[<AutoOpen>]
module BlockDispatcher2dModule =

    /// Gives an entity the base behavior of a rigid 2d block using static physics.
    type BlockDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage Assets.Default.Block]

[<AutoOpen>]
module BoxDispatcher2dModule =

    /// Gives an entity the base behavior of a rigid 2d box using dynamic physics.
    type BoxDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Box]

[<AutoOpen>]
module SideViewCharacterDispatcherModule =

    type Entity with
        member this.GetSideViewCharacterIdleImage world : Image AssetTag = this.Get (nameof this.SideViewCharacterIdleImage) world
        member this.SetSideViewCharacterIdleImage (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterIdleImage) value world
        member this.SideViewCharacterIdleImage = lens (nameof this.SideViewCharacterIdleImage) this this.GetSideViewCharacterIdleImage this.SetSideViewCharacterIdleImage
        member this.GetSideViewCharacterJumpImage world : Image AssetTag = this.Get (nameof this.SideViewCharacterJumpImage) world
        member this.SetSideViewCharacterJumpImage (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterJumpImage) value world
        member this.SideViewCharacterJumpImage = lens (nameof this.SideViewCharacterJumpImage) this this.GetSideViewCharacterJumpImage this.SetSideViewCharacterJumpImage
        member this.GetSideViewCharacterWalkSheet world : Image AssetTag = this.Get (nameof this.SideViewCharacterWalkSheet) world
        member this.SetSideViewCharacterWalkSheet (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterWalkSheet) value world
        member this.SideViewCharacterWalkSheet = lens (nameof this.SideViewCharacterWalkSheet) this this.GetSideViewCharacterWalkSheet this.SetSideViewCharacterWalkSheet
        member this.GetSideViewCharacterFacingLeft world : bool = this.Get (nameof this.SideViewCharacterFacingLeft) world
        member this.SetSideViewCharacterFacingLeft (value : bool) world = this.Set (nameof this.SideViewCharacterFacingLeft) value world
        member this.SideViewCharacterFacingLeft = lens (nameof this.SideViewCharacterFacingLeft) this this.GetSideViewCharacterFacingLeft this.SetSideViewCharacterFacingLeft

    /// Gives an entity the base behavior of 2d physics-driven character in a platformer.
    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher2d (true)

        static let computeWalkCelInset time delay (celSize : Vector2) (celRun : int) =
            let compressedTime =
                match (time, delay) with
                | (UpdateTime time, UpdateTime delay) -> time / delay
                | (ClockTime time, ClockTime delay) -> time / delay |> int64
                | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
            let frame = compressedTime % int64 celRun
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            box2 offset celSize

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.CelSize (v2 28.0f 28.0f)
             define Entity.CelRun 8
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AngularFactor v3Zero
             define Entity.GravityOverride (Some (Constants.Physics.Gravity2dDefault * 3.0f))
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
             define Entity.SideViewCharacterIdleImage Assets.Default.SideViewCharacterIdleImage
             define Entity.SideViewCharacterJumpImage Assets.Default.SideViewCharacterJumpImage
             define Entity.SideViewCharacterWalkSheet Assets.Default.SideViewCharacterWalkImage
             define Entity.SideViewCharacterFacingLeft false]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                // we have to use a bit of hackery to remember whether the character is facing left or
                // right when there is no velocity
                let facingLeft = entity.GetSideViewCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity (entity.GetBodyId world) world
                if facingLeft && velocity.X > 1.0f then entity.SetSideViewCharacterFacingLeft false world
                elif not facingLeft && velocity.X < -1.0f then entity.SetSideViewCharacterFacingLeft true world
                else world
            else world

        override this.Render (entity, world) =
            let bodyId = entity.GetBodyId world
            let facingLeft = entity.GetSideViewCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity bodyId world
            let celSize = entity.GetCelSize world
            let celRun = entity.GetCelRun world
            let animationDelay = entity.GetAnimationDelay world
            let mutable transform = entity.GetTransform world
            let struct (insetOpt, image) =
                if not (World.getBodyGrounded bodyId world) then
                    let image = entity.GetSideViewCharacterJumpImage world
                    struct (ValueNone, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = entity.GetSideViewCharacterIdleImage world
                    struct (ValueNone, image)
                else
                    let image = entity.GetSideViewCharacterWalkSheet world
                    struct (ValueSome (computeWalkCelInset world.GameTime animationDelay celSize celRun), image)
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = AssetTag.generalize image
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = insetOpt
                          Image = image
                          Color = Color.One
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = if facingLeft then FlipH else FlipNone }}
                world

[<AutoOpen>]
module TileMapDispatcherModule =

    /// Gives an entity the base behavior of an asset-defined tile map.
    type TileMapDispatcher () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<TileMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Engine.WildcardName
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap]

[<AutoOpen>]
module TmxMapDispatcherModule =

    /// Gives an entity the base behavior of a user-defined tile map.
    type TmxMapDispatcher () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<TmxMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Engine.WildcardName
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())]

[<AutoOpen>]
module SkyBoxDispatcherModule =

    /// Gives an entity the base behavior of sky box.
    type SkyBoxDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<SkyBoxFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

[<AutoOpen>]
module LightProbeDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d light probe.
    type LightProbeDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightProbeFacet3d>]

        static member Properties =
            [define Entity.LightProbe true
             define Entity.Presence Omnipresent
             define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
             define Entity.ProbeStale false]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module LightDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d light.
    type LightDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightFacet3d>]

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness Constants.Render.BrightnessDefault
             define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
             define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
             define Entity.LightType PointLight]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardDispatcherModule =

    /// Gives an entity the base behavior of a static billboard.
    type StaticBillboardDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticBillboardFacet>]

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.MetallicImage Assets.Default.MaterialMetallic
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.EmissionImage Assets.Default.MaterialEmission
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.HeightImage Assets.Default.MaterialHeight
             define Entity.TextureMinFilterOpt None
             define Entity.TextureMagFilterOpt None
             define Entity.RenderStyle Deferred]

// TODO: AnimatedBillboardDispatcher.

[<AutoOpen>]
module StaticModelDispatcherModule =

    /// Gives an entity the base behavior of a static model.
    type StaticModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module RigidModelDispatcherModule =

    /// Gives an entity the base behavior of physics-driven rigid model.
    type RigidModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let bodyShape = entity.GetBodyShape world
            let staticModel = entity.GetStaticModel world
            if (match bodyShape with BodyStaticModel body -> body.StaticModel <> staticModel | _ -> false) then
                let bodyStaticModel = { StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                let world = entity.SetBodyShape (BodyStaticModel bodyStaticModel) world
                (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.BodyShape (BodyStaticModel { StaticModel = Assets.Default.StaticModel; TransformOpt = None; PropertiesOpt = None })
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

[<AutoOpen>]
module StaticModelSurfaceDispatcherModule =

    /// Gives an entity the base behavior of an indexed static model.
    type StaticModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module RigidModelSurfaceDispatcherModule =

    /// Gives an entity the base behavior of an indexed, physics-driven rigid model.
    type RigidModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let bodyShape = entity.GetBodyShape world
            let surfaceIndex = entity.GetSurfaceIndex world
            let staticModel = entity.GetStaticModel world
            if (match bodyShape with BodyStaticModelSurface body -> body.SurfaceIndex <> surfaceIndex || body.StaticModel <> staticModel | _ -> false) then
                let bodyStaticModel = { SurfaceIndex = surfaceIndex; StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                let world = entity.SetBodyShape (BodyStaticModelSurface bodyStaticModel) world
                (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.BodyShape (BodyStaticModelSurface { SurfaceIndex = 0; StaticModel = Assets.Default.StaticModel; TransformOpt = None; PropertiesOpt = None })
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.SurfaceIndex)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportHierarchy rigid presence staticModel (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                let mutable (world', i) = (world, 0) // using mutation due to imperative API
                staticModelMetadata.PhysicallyBasedStaticHierarchy.Traverse (fun nodes ->
                    for node in nodes do
                        match node with
                        | OpenGL.PhysicallyBased.PhysicallyBasedNode names ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (names.Length > 0, names, group)
                                | Right entity -> (true, Array.append entity.Surnames names, entity.Group)
                            let (child, world) = World.createEntity<EntityDispatcher3d> DefaultOverlay (Some surnames) group world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLightProbe lightProbe ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (lightProbe.LightProbeNames.Length > 0, lightProbe.LightProbeNames, group)
                                | Right entity -> (true, Array.append entity.Surnames lightProbe.LightProbeNames, entity.Group)
                            let (child, world) = World.createEntity<LightProbeDispatcher3d> DefaultOverlay (Some surnames) group world
                            let world = child.SetProbeBounds lightProbe.LightProbeBounds world
                            let world = child.SetPositionLocal lightProbe.LightProbeMatrix.Translation world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLight light ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (light.LightNames.Length > 0, light.LightNames, group)
                                | Right entity -> (true, Array.append entity.Surnames light.LightNames, entity.Group)
                            let (child, world) = World.createEntity<LightDispatcher3d> DefaultOverlay (Some surnames) group world
                            let world = child.SetColor light.LightColor world
                            let world = child.SetLightType light.PhysicallyBasedLightType world
                            let (position, rotation, world) =
                                let transform = light.LightMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, world)
                                else (transform.Translation, quatIdentity, world) // use translation, even from invalid transform
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedSurface surface ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (surface.SurfaceNames.Length > 0, surface.SurfaceNames, group)
                                | Right entity -> (true, Array.append entity.Surnames surface.SurfaceNames, entity.Group)
                            let (child, world) =
                                if rigid
                                then World.createEntity<RigidModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                                else World.createEntity<StaticModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                            let world = if rigid then child.SetBodyType Static world else world
                            let (position, rotation, scale, world) =
                                let transform = surface.SurfaceMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, scale, world)
                                else (transform.Translation, quatIdentity, transform.Scale, world) // use translation and scale, even from invalid transform
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let materialProperties =
                                { AlbedoOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Albedo
                                  MetallicOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Metallic
                                  RoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Roughness
                                  AmbientOcclusionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.AmbientOcclusion
                                  EmissionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Emission
                                  HeightOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Height
                                  InvertRoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.InvertRoughness }
                            let world = child.SetMaterialProperties materialProperties world
                            let world = child.QuickSize world
                            world' <- world
                            i <- inc i)
                world'
            | None -> world

    type Entity with
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportHierarchy false (entity.GetPresenceConferred world) (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.PresenceConferred Exposed
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportHierarchy false (entity.GetPresence world) (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world

[<AutoOpen>]
module RigidModelHierarchyDispatcherModule =

    /// Gives an entity the base behavior of a hierarchy of indexed, physics-driven rigid models.
    type RigidModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportHierarchy true (entity.GetPresenceConferred world) (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.PresenceConferred Exposed
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportHierarchy true (entity.GetPresence world) (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world

[<AutoOpen>]
module BasicStaticBillboardEmitterDispatcherModule =

    /// Gives an entity the base behavior of basic static billboard emitter.
    type BasicStaticBillboardEmitterDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<BasicStaticBillboardEmitterFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d effect.
    type EffectDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<EffectFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

[<AutoOpen>]
module BlockDispatcher3dModule =

    /// Gives an entity the base behavior of a rigid 3d block using static physics.
    type BlockDispatcher3d () =
        inherit EntityDispatcher3d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticModel Assets.Default.StaticModel]

[<AutoOpen>]
module BoxDispatcher3dModule =

    /// Gives an entity the base behavior of a rigid 3d box using dynamic physics.
    type BoxDispatcher3d () =
        inherit EntityDispatcher3d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.StaticModel Assets.Default.StaticModel]