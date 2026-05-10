namespace SandBox2d
open System
open System.Diagnostics
open System.Numerics
open Prime
open Nu

/// this extends the Entity API to expose the user-defined properties.
[<AutoOpen>]
module LineSegmentsExtensions =
    type Entity with
        member this.GetLineSegments world : Vector2 array = this.Get (nameof Entity.LineSegments) world
        member this.SetLineSegments (value : Vector2 array) world = this.Set (nameof Entity.LineSegments) value world
        member this.LineSegments = lens (nameof Entity.LineSegments) this this.GetLineSegments this.SetLineSegments
        member this.GetLineWidth world : single = this.Get (nameof Entity.LineWidth) world
        member this.SetLineWidth (value : single) world = this.Set (nameof Entity.LineWidth) value world
        member this.LineWidth = lens (nameof Entity.LineWidth) this this.GetLineWidth this.SetLineWidth

/// this is the dispatcher that defines the behavior of the line segments used to contain the fluid.
type LineSegmentsDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.LineSegments [||]
         define Entity.LineWidth 2f
         define Entity.Color colorOne]

    override this.Register (lineSegments, world) =
        World.monitor (fun event world ->
            let lineSegments = event.Subscriber : Entity
            let segments = event.Data.Value :?> Vector2 array
            if Array.notEmpty segments && lineSegments.GetEnabled world then
                let box = Box2.Enclose segments
                let lineWidth = lineSegments.GetLineWidth world
                let size = v2 (box.Width + lineWidth) (box.Height + lineWidth)
                let segments = segments |> Array.map (fun p -> ((p - box.Center) / size).V3)
                lineSegments.SetPosition box.Center.V3 world
                lineSegments.SetSize size.V3 world
                lineSegments.SetBodyShape (
                    BodyShapes [
                        // NOTE: One contour only collides with the right hand side of its links.
                        // For two-sided collisions, we create two contours with opposite winding orders.
                        for segments in [segments; Array.rev segments] do
                            ContourShape
                                { Links = [|Array.head segments; yield! segments; Array.last segments|] // the first and last links provide no collision.
                                  Closed = false
                                  TransformOpt = None
                                  PropertiesOpt = None }
                    ]) world
            Cascade) lineSegments.LineSegments.ChangeEvent lineSegments world

    override this.Render (_, lineSegments, world) =
        let staticImage = Assets.Default.White
        let insetOpt : Box2 voption = ValueNone
        let clipOpt : Box2 voption = ValueNone
        let color = lineSegments.GetColor world
        let blend = Additive
        let emission = colorZero
        let flip = Unflipped
        let segments = lineSegments.GetLineSegments world
        let lineWidth = lineSegments.GetLineWidth world
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero v3Zero v3Zero (lineSegments.GetElevation world)
        for i in 0 .. segments.Length - 2 do
            let p1 = segments[i]
            let p2 = segments[inc i]
            transform.Position <- ((p1 + p2) * 0.5f).V3
            transform.Rotation <- Quaternion.CreateLookAt2d (p2 - p1)
            transform.Size <- v3 (p2 - p1).Magnitude lineWidth 0f
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

type SelectedTool = Water | Sand | Smoke | Oil | Bubble | Line | Box | Explosion

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module FluidSimExtensions =
    type Screen with
        member this.GetLineSegments world : Vector2 array list = this.Get (nameof Screen.LineSegments) world
        member this.SetLineSegments (value : Vector2 array list) world = this.Set (nameof Screen.LineSegments) value world
        member this.LineSegments = lens (nameof Screen.LineSegments) this this.GetLineSegments this.SetLineSegments
        member this.GetMouseBubbleSize world : single = this.Get (nameof Screen.MouseBubbleSize) world
        member this.SetMouseBubbleSize (value : single) world = this.Set (nameof Screen.MouseBubbleSize) value world
        member this.MouseBubbleSize = lens (nameof Screen.MouseBubbleSize) this this.GetMouseBubbleSize this.SetMouseBubbleSize
        member this.GetSelectedTool world : SelectedTool = this.Get (nameof Screen.SelectedTool) world
        member this.SetSelectedTool (value : SelectedTool) world = this.Set (nameof Screen.SelectedTool) value world
        member this.SelectedTool = lens (nameof Screen.SelectedTool) this this.GetSelectedTool this.SetSelectedTool
        member this.GetBoxes world : string FSet = this.Get (nameof Screen.Boxes) world
        member this.SetBoxes (value : string FSet) world = this.Set (nameof Screen.Boxes) value world
        member this.Boxes = lens (nameof Screen.Boxes) this this.GetBoxes this.SetBoxes

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type FluidSimDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.InfoOpened false
         define Screen.LineSegments []
         define Screen.MouseBubbleSize 0f
         define Screen.SelectedTool Water
         define Screen.Boxes FSet.empty]

    // here we define the screen's top-level behavior
    override this.Process (selectionResults, fluidSim, world) =

        // clean up lines and gravity when initializing
        if FQueue.contains Select selectionResults then
            fluidSim.SetInfoOpened false world
            fluidSim.SetLineSegments [] world
            fluidSim.SetMouseBubbleSize 0f world
            fluidSim.SetSelectedTool Water world
            fluidSim.SetBoxes FSet.empty world
            World.setGravity2d (World.getGravityDefault2d world) world
            World.setCursorType (UserDefinedCursor Assets.Gameplay.DropletCursor) world

        // process while selected
        if fluidSim.GetSelected world then

            // begin scene declaration
            World.beginGroup Simulants.FluidSimScene.Name [] world

            // create test geometry
            let scale = 25f
            World.doBlock2d "Bottom"
                [Entity.Size .= v3 12f 1f 0f * scale * 2f 
                 Entity.Position .= v3 0f -7f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Left"
                [Entity.Size .= v3 1f 10f 0f * scale * 2f
                 Entity.Position .= v3 -11f 0f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Right"
                [Entity.Size .= v3 1f 10f 0f * scale * 2f
                 Entity.Position .= v3 11f 0f 0f * scale
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doBlock2d "Ramp"
                [Entity.Size .= v3 5.5f 0.5f 0f * scale * 2f
                 Entity.Position .= v3 6f 2f 0f * scale
                 Entity.Rotation .= Quaternion.CreateFromAngle2d 0.25f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore
            World.doSphere2d "Circle"
                [Entity.Size .= v3 2f 2f 0f * scale * 2f
                 Entity.Position .= v3 0f 0.2f 0f * scale
                 Entity.Color .= Color.DeepSkyBlue] world |> ignore

            // define fluid system
            World.doEntity<FluidEmitter2dDispatcher> "Fluid Emitter"
                [Entity.Size .= v3 640f 640f 0f
                 // individual sprites on the same elevation are ordered from top to bottom then by asset tag.
                 // here, we don't draw particles above the borders, especially relevant for the water sprite.
                 Entity.Elevation .= -1f
                 Entity.StaticImage .= Assets.Default.Fluid] world
            let fluidEmitter = world.DeclaredEntity
            let fluidEmitterId = fluidEmitter.GetFluidEmitterId world

            // particle count button
            World.doText $"Particle Count"
                [Entity.Position .= v3 255f 170f 0f
                 Entity.Text @= $"{(fluidEmitter.GetFluidParticles world).Length} Particles"
                 Entity.Elevation .= 1f] world

            // clear button
            if World.doButton $"Clear"
                [Entity.Position .= v3 255f 140f 0f
                 Entity.Text .= "Clear"
                 Entity.Elevation .= 1f] world then
                World.clearFluidParticles fluidEmitterId world
                fluidSim.SetLineSegments [] world
                fluidSim.SetBoxes FSet.empty world

            // gravity button
            let gravities =
                [|("v", World.getGravityDefault2d world)
                  ("\\", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_4))
                  (">", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
                  ("0", v3Zero)
                  ("<", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))
                  ("/", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_4))|]
            for i in 0 .. dec gravities.Length do
                if World.getGravity2d world = snd gravities[i] then
                    if World.doButton $"Gravity"
                        [Entity.Position .= v3 255f 110f 0f
                         Entity.Text @= $"Gravity: {fst gravities[i]}"
                         Entity.Elevation .= 1f] world then
                        World.setGravity2d (snd gravities[(i + 1) % gravities.Length]) world

            // particle sprite button
            let particleImage = (fluidEmitter.GetFluidParticleRenders world).["Water"].Image
            if World.doButton $"Particle Sprite"
                [Entity.Position .= v3 255f 80f 0f
                 Entity.Text @= $"Particle Sprite: {particleImage.AssetName}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 8.f] world then
                if particleImage = Assets.Default.Ball then
                    fluidEmitter.FluidParticleRenders.Map (Map.map (fun key render -> 
                        let mutable transform = render.Transform
                        if key = "Smoke" then // smoke uses its own sprite.
                            transform.Size <- (Metadata.getTextureSizeF Assets.Default.Gas).V3
                            // image credit: https://github.com/a-piece-of-snake/sfml-box2d-fluid/blob/master/sfmlSetup/Assets/Textures/smoke.png
                            { render with Image = Assets.Default.Gas; Transform = transform }
                        else
                            // in Paint.NET (canvas size = 31 x 31), use the Brush (size = 31, hardness = 50%, fill = solid color #0094FF)
                            // and click the center once, to generate this Fluid image.
                            transform.Size <- (Metadata.getTextureSizeF Assets.Default.Fluid).V3
                            { render with Image = Assets.Default.Fluid; Transform = transform })) world
                elif particleImage = Assets.Default.Fluid then
                    fluidEmitter.FluidParticleRenders.Map (Map.map (fun _ render ->
                        let mutable transform = render.Transform
                        transform.Size <- v3 16f 16f 0f
                        // image credit: https://www.pngitem.com/middle/hbhTw_transparent-bubble-hd-png-download
                        { render with Image = Assets.Gameplay.BubbleImage; Transform = transform })) world
                elif particleImage = Assets.Gameplay.BubbleImage then
                    fluidEmitter.FluidParticleRenders.Map (Map.map (fun _ render ->
                        let mutable transform = render.Transform
                        transform.Size <- v3 8f 8f 0f
                        // image credit: https://github.com/nkast/Aether.Physics2D/blob/main/Samples/SamplesContent/Samples/goo.png
                        { render with Image = Assets.Gameplay.GooImage; Transform = transform })) world
                else
                    fluidEmitter.FluidParticleRenders.Map (Map.map (fun _ render ->
                        let mutable transform = render.Transform
                        transform.Size <- v3 2f 2f 0f
                        { render with Image = Assets.Default.Ball; Transform = transform })) world

            // tool palette panel
            World.beginPanel "Tool Panel"
                [Entity.Position .= v3 255f -10f 0f
                 Entity.Size .= v3 128f 153f 0f
                 Entity.Elevation .= 1f] world |> ignore

            // tool palette label
            let selectedTool = fluidSim.GetSelectedTool world
            let renders = fluidEmitter.GetFluidParticleRenders world
            World.doText "Tool Label"
                [Entity.PositionLocal .= v3 0f 60f 0f
                 Entity.Text @= $"Tool: {selectedTool}"] world

            // tool palette image buttons (grid below Particle Sprite button)
            let tools = [|Water; Sand; Oil; Smoke; Bubble; Line; Box; Explosion|]
            for i in 0 .. dec tools.Length do
                let tool = tools[i]
                let col = i % 3
                let row = i / 3
                let x = single (col - 1) * 35f
                let y = 25f - single row * 35f 
                if World.doButton $"{tool}"
                    [Entity.PositionLocal .= v3 x y 0f // relative to outer panel
                     Entity.Size .= v3 35f 35f 0f
                     Entity.Enabled @= (selectedTool <> tool)
                     Entity.ElevationLocal .= 1f] world then
                    fluidSim.SetSelectedTool tool world
                World.doStaticSprite $"{tool} Overlay"
                    [Entity.MountOpt .= Some world.DeclaredEntity.EntityAddress // inherits all properties with Local suffix like position, rotation and elevation
                     Entity.Absolute .= true
                     match tool with
                     | Water | Sand | Oil | Smoke ->
                         let toolName = string tool
                         Entity.StaticImage .= renders[toolName].Image
                         Entity.Color .= renders[toolName].Color
                     | Bubble ->
                         Entity.StaticImage .= Assets.Gameplay.BubbleImage
                     | Line ->
                         Entity.Size .= v3 25f 2f 0f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.RotationLocal .= Quaternion.CreateFromAngle2d MathF.PI_OVER_4
                     | Box -> Entity.Size .= v3 25f 25f 0f
                     | Explosion -> Entity.StaticImage .= Assets.Gameplay.BoomImage // image credit: https://www.pikpng.com/transpng/hxThxoJ
                     Entity.ElevationLocal .= 1f] world |> ignore

            World.endPanel world

            // squish button
            if World.doButton $"Squish"
                [Entity.Position .= v3 255f -100f 0f
                 Entity.Text .= "Squish"
                 Entity.Elevation .= 1f] world then
                let paddle = World.createEntity<BlockBody2dDispatcher> None DefaultOverlay None world.ContextGroup world
                paddle.SetPosition (v3 -270f 0f 0f) world
                paddle.SetSize (v3 30f 500f 0f) world
                paddle.SetStaticImage Assets.Default.Paddle world
                paddle.SetBodyType Kinematic world
                paddle.SetLinearVelocity (v3 50f 0f 0f) world
                coroutine world.Launcher {
                    do! Coroutine.sleep (GameTime.ofSeconds 10)
                    World.destroyEntity paddle world }

            // switch screen button
            World.doButton Simulants.ToyBoxSwitchScreen.Name
                [Entity.Position .= v3 255f -130f 0f
                 Entity.Text .= "Switch Screen"
                 Entity.Elevation .= 1f] world |> ignore

            // info button
            if World.doButton "Info"
                [Entity.Position .= v3 255f -160f 0f
                 Entity.Text .= "Info"
                 Entity.Elevation .= 1f] world then
                fluidSim.SetInfoOpened true world

            // info panel
            if fluidSim.GetInfoOpened world then

                // declare info background - block button interactions behind info panel while opened
                World.doPanel "Info Background"
                    [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                     Entity.Elevation .= 10f
                     Entity.BackdropImageOpt .= Some Assets.Default.Black
                     Entity.Color .= color 0f 0f 0f 0.5f] world

                // being info panel declaration
                World.beginPanel "Info Panel"
                    [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3 * 0.8f
                     Entity.Layout .= Grid (v2i 1 5, Some FlowDownward, true)
                     Entity.Elevation .= 10f] world

                // declare info entities
                World.doText "Info Origin 1"
                    [Entity.LayoutOrder .= 0
                     Entity.Text .= "Box2DFluid by klutch (Graeme Collins)"] world
                World.doText "Info Origin 2"
                    [Entity.LayoutOrder .= 1
                     Entity.Text .= "Ported to Nu by Happypig375 (Hadrian Tang)"] world
                World.doText "Info Controls"
                    [Entity.LayoutOrder .= 2
                     Entity.Justification .= Unjustified true // required for line breaks to render.
                     Entity.Text .=
                     "Controls: \n\
                        Mouse Left - Click button / Use tool. \n\
                        Mouse Right - Delete particles and boxes."
                     Entity.FontSizing .= Some 9.5f
                     Entity.TextMargin .= v2 5f 0f] world
                if World.doButton "Info Close"
                    [Entity.LayoutOrder .= 3
                     Entity.Text .= "Close"] world then
                    fluidSim.SetInfoOpened false world
                if World.doButton "Info Exit"
                    [Entity.LayoutOrder .= 4
                     Entity.Text .= "Exit"] world && world.Unaccompanied then
                    World.exit world

                // end info panel declaration
                World.endPanel world

                // declare info links
                for (position, size, url) in
                    [(v2 -115f 115f, v2 95f 32f, "https://github.com/klutch/Box2DFluid")
                     (v2 -12.5f 115f, v2 60f 32f, "https://github.com/klutch")
                     (v2 -127.5f 57.5f, v2 115f 32f, "https://github.com/bryanedds/Nu/pull/1162")
                     (v2 3.5f 57.5f, v2 105f 32f, "https://github.com/Happypig375")] do
                    if World.doButton $"Info Origin Button {url.Replace ('/', '\\')}"
                        [Entity.Position .= position.V3
                         Entity.Size .= size.V3
                         Entity.Elevation .= 11f] world then
                        Process.Start (ProcessStartInfo (url, UseShellExecute = true)) |> ignore
                  
            // declare boxes
            let mousePosition = World.getMousePosition2dWorld false world
            let boxes = fluidSim.GetBoxes world
            for box in boxes do
                World.doBox2d box
                    [Entity.Position |= mousePosition.V3
                     Entity.Color |= color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f] world |> ignore

            // mouse interactions with fluid system
            if fluidSim.GetSelected world && world.Advancing then
                let tool = fluidSim.GetSelectedTool world
                match (tool, World.doFeeler "Feeler" [Entity.Position @= mousePosition.V3] world) with // a feeler is a touch and mouse left button detector respecting elevation such that buttons with higher elevation prevent this interaction.
                | ((Water | Sand | Oil | Smoke), (true, _)) -> // doFeeler returns (isDown, justPressed) detecting touch and mouse left button.
                    // create particles
                    let particles =
                        SArray.init 4 (fun _ ->
                            let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * 16.0f
                            { FluidParticlePosition = (mousePosition + jitter).V3; FluidParticleVelocity = v3Zero; FluidParticleConfig = string tool })

                    // emit particles
                    World.emitFluidParticles particles fluidEmitterId world
                | (Bubble, (true, _)) ->
                    // summon a bubble
                    fluidSim.MouseBubbleSize.Map inc world
                    World.doSphere2d "Bubble"
                        [Entity.Position @= mousePosition.V3
                         Entity.Size @= v3Dup (fluidSim.GetMouseBubbleSize world)
                         Entity.StaticImage .= Assets.Gameplay.BubbleImage] world |> ignore
                | (Bubble, (false, _)) when World.isMouseButtonReleased MouseLeft world -> // the feeler detects only presses, not releases.
                    // reset size when mouse left button is just released
                    fluidSim.SetMouseBubbleSize 0f world
                | (Line, (true, true)) ->
                    // start a contour
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        List.cons [|mousePosition|] lineSegments) world
                | (Line, (true, false)) ->
                    // draw a contour
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        if lineSegments.NotEmpty && Vector2.Distance (mousePosition, Array.last (List.head lineSegments)) > 8f then
                            List.updateAt 0 (Array.add mousePosition (List.head lineSegments)) lineSegments
                        else lineSegments) world
                | (Box, (true, true)) ->
                    fluidSim.Boxes.Map (FSet.add Gen.name) world
                | (Explosion, (true, true)) ->
                    World.applyExplosion2d mousePosition.V3 200f 20f 100f UInt64.MaxValue world
                | _ -> ()

                if World.isMouseButtonDown MouseRight world then // there is no feeler equivalent for mouse buttons that aren't the left button. this doesn't respect elevation.

                    // mouse right - destroy particles
                    let discriminator (particle : FluidParticle) =
                        let bounds = box2 (mousePosition - v2Dup 16.0f) (v2Dup 32.0f)
                        if bounds.Contains particle.FluidParticlePosition.V2 = ContainmentType.Disjoint
                        then ValueSome particle
                        else ValueNone

                    // filter particles
                    World.chooseFluidParticles discriminator fluidEmitterId world

                    // remove boxes
                    for entity in World.getEntities2dAtPoint mousePosition (hashSetPlus HashIdentity.Structural []) world do
                        if boxes.Contains entity.Name then
                            fluidSim.Boxes.Map (FSet.remove entity.Name) world

            // declare containment contour
            for segment in fluidSim.GetLineSegments world do
                World.doEntity<LineSegmentsDispatcher> $"Contour {segment[0]}" [Entity.LineSegments @= segment] world

            // end scene declaration
            World.endGroup world

            // process camera as last task
            World.setEye2dCenter (v2 60f 10f) world
            
        // reset cursor when deselecting
        if FQueue.contains Deselecting selectionResults then
            World.setCursorType DefaultCursor world