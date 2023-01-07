// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

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
        inherit EntityDispatcher (false, centered, physical)

        new (physical) =
            EntityDispatcher2d (Constants.Engine.EntityCentered2dDefault, physical)

        static member Properties =
            [define Entity.Centered Constants.Engine.EntityCentered2dDefault
             define Entity.Size Constants.Engine.EntitySize2dDefault]

    /// A 3d entity dispatcher.
    type EntityDispatcher3d (centered, physical) =
        inherit EntityDispatcher (true, centered, physical)

        new (physical) =
            EntityDispatcher3d (Constants.Engine.EntityCentered3dDefault, physical)

        static member Properties =
            [define Entity.Centered Constants.Engine.EntityCentered3dDefault
             define Entity.Size Constants.Engine.EntitySize3dDefault]

[<AutoOpen>]
module StaticSpriteDispatcherModule =

    type StaticSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

[<AutoOpen>]
module AnimatedSpriteDispatcherModule =

    type AnimatedSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelRun 4
             define Entity.CelCount 16
             define Entity.AnimationDelay 4L
             define Entity.AnimationSheet Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.Flip FlipNone]

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
        member this.GetDisabledColor world : Color = this.Get (nameof this.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof this.DisabledColor) value world
        member this.DisabledColor = lens (nameof this.DisabledColor) this this.GetDisabledColor this.SetDisabledColor

    type GuiDispatcher () =
        inherit EntityDispatcher2d (Constants.Engine.EntityCenteredGuiDefault, false)

        static member Properties =
            [define Entity.Centered Constants.Engine.EntityCenteredGuiDefault
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Presence Omnipresent
             define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))]

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
        member this.GetDown world : bool = this.Get (nameof this.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof this.Down) value world
        member this.Down = lens (nameof this.Down) this this.GetDown this.SetDown
        member this.GetDownTextOffset world : Vector3 = this.Get (nameof this.DownTextOffset) world
        member this.SetDownTextOffset (value : Vector3) world = this.Set (nameof this.DownTextOffset) value world
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
            let world = entity.SetTextOffset v3Zero world
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
             define Entity.DownTextOffset v3Zero
             define Entity.UpImage Assets.Default.Image
             define Entity.DownImage Assets.Default.Image2
             define Entity.ClickSoundOpt (Some Assets.Default.Sound)
             define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
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

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.LabelImage Assets.Default.Image3]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage = entity.GetLabelImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
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
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
                World.enqueueRenderLayeredMessage2d
                    { Elevation = spriteTransform.Elevation
                      Horizon = spriteTransform.Horizon
                      AssetTag = AssetTag.generalize spriteImage
                      RenderDescriptor2d =
                        SpriteDescriptor
                            { Transform = spriteTransform
                              InsetOpt = ValueNone
                              Image = spriteImage
                              Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                              Blend = Transparent
                              Glow = Color.Zero
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
        member this.GetToggledTextOffset world : Vector3 = this.Get (nameof this.ToggledTextOffset) world
        member this.SetToggledTextOffset (value : Vector3) world = this.Set (nameof this.ToggledTextOffset) value world
        member this.ToggledTextOffset = lens (nameof this.ToggledTextOffset) this this.GetToggledTextOffset this.SetToggledTextOffset
        member this.GetPressed world : bool = this.Get (nameof this.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof this.Pressed) value world
        member this.Pressed = lens (nameof this.Pressed) this this.GetPressed this.SetPressed
        member this.GetPressedTextOffset world : Vector3 = this.Get (nameof this.PressedTextOffset) world
        member this.SetPressedTextOffset (value : Vector3) world = this.Set (nameof this.PressedTextOffset) value world
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
             define Entity.ToggledTextOffset v3Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v3Zero
             define Entity.UntoggledImage Assets.Default.Image
             define Entity.ToggledImage Assets.Default.Image2
             define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
             define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =

            // update text offset regardless of enabled state
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetToggled world then entity.GetToggledTextOffset world
                else v3Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage =
                if entity.GetToggled world || entity.GetPressed world
                then entity.GetToggledImage world
                else entity.GetUntoggledImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
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
        member this.GetDialedTextOffset world : Vector3 = this.Get (nameof this.DialedTextOffset) world
        member this.SetDialedTextOffset (value : Vector3) world = this.Set (nameof this.DialedTextOffset) value world
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
             define Entity.DialedTextOffset v3Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v3Zero
             define Entity.UndialedImage Assets.Default.Image
             define Entity.DialedImage Assets.Default.Image2
             define Entity.DialSoundOpt (Some Assets.Default.Sound)
             define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =

            // update text offset regardless of enabled state
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetDialed world then entity.GetDialedTextOffset world
                else v3Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage =
                if entity.GetDialed world || entity.GetPressed world
                then entity.GetDialedImage world
                else entity.GetUndialedImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
        member this.GetStartTime world : int64 = this.Get (nameof this.StartTime) world
        member this.SetStartTime (value : int64) world = this.Set (nameof this.StartTime) value world
        member this.StartTime = lens (nameof this.StartTime) this this.GetStartTime this.SetStartTime
        member this.GetStartDateTime world : DateTime = this.Get (nameof this.StartDateTime) world
        member this.SetStartDateTime (value : DateTime) world = this.Set (nameof this.StartDateTime) value world
        member this.StartDateTime = lens (nameof this.StartDateTime) this this.GetStartDateTime this.SetStartDateTime

    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTime.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 5.0 then
                let world = entity.SetStartTime (World.getUpdateTime world) world
                entity.SetStartDateTime currentDateTime world
            else world

        static member Properties =
            [define Entity.StartTime 0L
             define Entity.StartDateTime DateTime.UtcNow]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let world = resetIntermittent entity world
                let startDateTime = entity.GetStartDateTime world
                let currentDateTime = DateTime.UtcNow
                let elapsedDateTime = currentDateTime - startDateTime
                let time = double (World.getUpdateTime world - entity.GetStartTime world)
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

    type FillBarDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.Image9
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Image4]

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
            let disabledColor = entity.GetDisabledColor world
            let borderImageColor = (entity.GetBorderColor world).WithA disabledColor.A
            let borderImage = entity.GetBorderImage world
            let world =
                World.enqueueRenderLayeredMessage2d
                    { Elevation = borderTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize borderImage
                      RenderDescriptor2d =
                        SpriteDescriptor
                            { Transform = borderTransform
                              InsetOpt = ValueNone
                              Image = borderImage
                              Color = borderImageColor
                              Blend = Transparent
                              Glow = Color.Zero
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
            let fillImageColor = (entity.GetFillColor world).WithA disabledColor.A
            let fillImage = entity.GetFillImage world
            let world =
                World.enqueueRenderLayeredMessage2d
                    { Elevation = fillTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize fillImage
                      RenderDescriptor2d =
                          SpriteDescriptor
                              { Transform = fillTransform
                                InsetOpt = ValueNone
                                Image = fillImage
                                Color = fillImageColor
                                Blend = Transparent
                                Glow = Color.Zero
                                Flip = FlipNone }}
                    world

            // fin
            world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module BasicEmitterDispatcher2dModule =

    type BasicEmitterDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<BasicEmitter2dFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher2dModule =

    type EffectDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<Effect2dFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.Effect (scvalue<Effect> "[Effect None [] [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]")]

[<AutoOpen>]
module BlockDispatcher2dModule =

    type BlockDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage Assets.Default.Image6]

[<AutoOpen>]
module BoxDispatcher2dModule =

    type BoxDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6]

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

    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher2d (true)

        static let computeWalkCelInset (celSize : Vector2) (celRun : int) delay time =
            let compressedTime = time / delay
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
             define Entity.AnimationDelay 4L
             define Entity.FixedRotation true
             define Entity.GravityScale 3.0f
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v3Zero; PropertiesOpt = None })
             define Entity.SideViewCharacterIdleImage Assets.Default.SideViewCharacterIdleImage
             define Entity.SideViewCharacterJumpImage Assets.Default.SideViewCharacterJumpImage
             define Entity.SideViewCharacterWalkSheet Assets.Default.SideViewCharacterWalkImage
             define Entity.SideViewCharacterFacingLeft false]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                // we have to use a bit of hackery to remember whether the character is facing left or
                // right when there is no velocity
                let facingLeft = entity.GetSideViewCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
                if facingLeft && velocity.X > 1.0f then entity.SetSideViewCharacterFacingLeft false world
                elif not facingLeft && velocity.X < -1.0f then entity.SetSideViewCharacterFacingLeft true world
                else world
            else world

        override this.Render (entity, world) =
            let time = World.getUpdateTime world
            let physicsId = entity.GetPhysicsId world
            let facingLeft = entity.GetSideViewCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity physicsId world
            let celSize = entity.GetCelSize world
            let celRun = entity.GetCelRun world
            let animationDelay = entity.GetAnimationDelay world
            let mutable transform = entity.GetTransform world
            let struct (insetOpt, image) =
                if not (World.isBodyOnGround physicsId world) then
                    let image = entity.GetSideViewCharacterJumpImage world
                    struct (ValueNone, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = entity.GetSideViewCharacterIdleImage world
                    struct (ValueNone, image)
                else
                    let image = entity.GetSideViewCharacterWalkSheet world
                    struct (ValueSome (computeWalkCelInset celSize celRun animationDelay time), image)
            World.enqueueRenderLayeredMessage2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = AssetTag.generalize image
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = insetOpt
                          Image = image
                          Color = Color.One
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = if facingLeft then FlipH else FlipNone }}
                world

[<AutoOpen>]
module TileMapDispatcherModule =

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
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap]

[<AutoOpen>]
module TmxMapDispatcherModule =

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
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())]

[<AutoOpen>]
module SkyBoxDispatcherModule =

    type SkyBoxDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<SkyBoxFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

[<AutoOpen>]
module LightDispatcher3dModule =

    type LightDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightFacet3d>]

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness 10.0f
             define Entity.Intensity 1.0f
             define Entity.LightType PointLight]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardDispatcherModule =

    type StaticBillboardDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticBillboardFacet>]

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.AlbedoOpt None
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.MetalnessOpt None
             define Entity.MetalnessImage Assets.Default.MaterialMetalness
             define Entity.RoughnessOpt None
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.AmbientOcclusionOpt None
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelDispatcherModule =

    type StaticModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelSurfaceDispatcherModule =

    type StaticModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type World with

        /// Attempt to import scene below the target entity.
        static member tryImportScene static_ staticModel (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                // Unity Scene Export Instructions:
                // 1) have FBX Exporter package installed
                // 2) be in PBR Unity Project
                // 3) put all desired objects in empty root GameObject
                // 4) export root GameObject
                // 5) delete all fbx files except the one you exported
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
                            let world = child.SetStatic static_ world
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
                            let transform = light.LightMatrix
                            let position = transform.Translation
                            let mutable rotation = transform
                            rotation.Translation <- v3Zero
                            let rotation = Quaternion.CreateFromRotationMatrix rotation
                            let scale = transform.Scale
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetStatic static_ world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedSurface surface ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (surface.SurfaceNames.Length > 0, surface.SurfaceNames, group)
                                | Right entity -> (true, Array.append entity.Surnames surface.SurfaceNames, entity.Group)
                            let (child, world) = World.createEntity<StaticModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                            let transform = surface.SurfaceMatrix
                            let position = transform.Translation
                            let mutable rotation = transform
                            rotation.Translation <- v3Zero
                            let rotation = Quaternion.CreateFromRotationMatrix rotation
                            let scale = transform.Scale
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetStatic static_ world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let world = child.SetAlbedoOpt (Some surface.SurfaceMaterial.Albedo) world
                            let world = child.SetMetalnessOpt (Some surface.SurfaceMaterial.Metalness) world
                            let world = child.SetRoughnessOpt (Some surface.SurfaceMaterial.Roughness) world
                            let world = child.SetAmbientOcclusionOpt (Some surface.SurfaceMaterial.AmbientOcclusion) world
                            let world = child.QuickSize world
                            world' <- world
                            i <- inc i)
                world'
            | None -> world

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportScene true (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportScene true (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world