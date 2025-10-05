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

        /// The line segments that define the fluid boundaries.
        member this.GetLineSegments world : Vector2 array = this.Get (nameof Entity.LineSegments) world
        member this.SetLineSegments (value : Vector2 array) world = this.Set (nameof Entity.LineSegments) value world
        member this.LineSegments = lens (nameof Entity.LineSegments) this this.GetLineSegments this.SetLineSegments

        /// The width of the line segments when rendered.
        member this.GetLineWidth world : single = this.Get (nameof Entity.LineWidth) world
        member this.SetLineWidth (value : single) world = this.Set (nameof Entity.LineWidth) value world
        member this.LineWidth = lens (nameof Entity.LineWidth) this this.GetLineWidth this.SetLineWidth

/// this is the dispatcher that defines the behavior of the line segments used to contain the fluid.
type LineSegmentsDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.LineSegments Array.empty
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
                lineSegments.SetPosition box.Center.V3 world
                lineSegments.SetSize size.V3 world
                lineSegments.SetBodyShape (
                    ContourShape
                        { Links = segments |> Array.map (fun p -> ((p - box.Center) / size).V3)
                          Closed = false
                          TransformOpt = None
                          PropertiesOpt = None }) world
            Cascade) lineSegments.LineSegments.ChangeEvent lineSegments world

    override this.Render (_, lineSegments, world) =
        let staticImage = Assets.Default.White
        let insetOpt : Box2 voption = ValueNone
        let clipOpt : Box2 voption = ValueNone
        let color = lineSegments.GetColor world
        let blend = Additive
        let emission = colorZero
        let flip = FlipNone
        let segments = lineSegments.GetLineSegments world
        let lineWidth = lineSegments.GetLineWidth world
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero v3Zero v3Zero (lineSegments.GetElevation world)
        for i in 0 .. segments.Length - 2 do
            let p1 = segments.[i]
            let p2 = segments.[inc i]
            transform.Position <- ((p1 + p2) * 0.5f).V3
            transform.Rotation <- Quaternion.CreateLookAt2d (p2 - p1)
            transform.Size <- v3 (p2 - p1).Magnitude lineWidth 0f
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module FluidSimExtensions =
    type Screen with

        /// The line segments drawn by the user to contain the fluid.
        member this.GetLineSegments world : Vector2 array list = this.Get (nameof Screen.LineSegments) world
        member this.SetLineSegments (value : Vector2 array list) world = this.Set (nameof Screen.LineSegments) value world
        member this.LineSegments = lens (nameof Screen.LineSegments) this this.GetLineSegments this.SetLineSegments

        /// The size of the bubble created when both mouse buttons are pressed.
        member this.GetMouseBubbleSize world : single = this.Get (nameof Screen.MouseBubbleSize) world
        member this.SetMouseBubbleSize (value : single) world = this.Set (nameof Screen.MouseBubbleSize) value world
        member this.MouseBubbleSize = lens (nameof Screen.MouseBubbleSize) this this.GetMouseBubbleSize this.SetMouseBubbleSize

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type FluidSimDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.InfoOpened false
         define Screen.LineSegments []
         define Screen.MouseBubbleSize 0f]

    // here we define the screen's top-level behavior
    override this.Process (selectionResults, fluidSim, world) =

        // clean up lines and gravity when initializing
        if FQueue.contains Select selectionResults then
            fluidSim.SetInfoOpened false world
            fluidSim.SetLineSegments [] world
            fluidSim.SetMouseBubbleSize 0f world
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

            // gravity button
            let gravities =
                [|("v", World.getGravityDefault2d world)
                  ("\\", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_4))
                  (">", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d MathF.PI_OVER_2))
                  ("0", v3Zero)
                  ("<", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_2))
                  ("/", (World.getGravityDefault2d world).Transform (Quaternion.CreateFromAngle2d -MathF.PI_OVER_4))|]
            for i in 0 .. dec gravities.Length do
                if World.getGravity2d world = snd gravities.[i] then
                    if World.doButton $"Gravity"
                        [Entity.Position .= v3 255f 110f 0f
                         Entity.Text @= $"Gravity: {fst gravities.[i]}"
                         Entity.Elevation .= 1f] world then
                        World.setGravity2d (snd gravities.[(i + 1) % gravities.Length]) world

            // particle sprite button
            if World.doButton $"Particle Sprite"
                [Entity.Position .= v3 255f 80f 0f
                 Entity.Text @= $"Particle Sprite: {(fluidEmitter.GetStaticImage world).AssetName}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 8] world then
                if fluidEmitter.GetStaticImage world = Assets.Default.Ball then
                    // in Paint.NET (canvas size = 50 x 50), use the Brush (size = 50, hardness = 50%, fill = solid color #0094FF)
                    // and click the center once, to generate this Particle image.
                    fluidEmitter.SetStaticImage Assets.Default.Fluid world
                    fluidEmitter.SetFluidParticleImageSizeOverride None world
                elif fluidEmitter.GetStaticImage world = Assets.Default.Fluid then
                    // credit: https://ena.our-dogs.info/spring-2023.html
                    fluidEmitter.SetStaticImage Assets.Gameplay.BubbleImage world
                    fluidEmitter.SetFluidParticleImageSizeOverride None world
                elif fluidEmitter.GetStaticImage world = Assets.Gameplay.BubbleImage then
                    // credit: Aether.Physics2D demos
                    fluidEmitter.SetStaticImage Assets.Gameplay.GooImage world
                    fluidEmitter.SetFluidParticleImageSizeOverride (v2Dup 8f |> Some) world
                else
                    fluidEmitter.SetStaticImage Assets.Default.Ball world
                    fluidEmitter.SetFluidParticleImageSizeOverride (v2Dup 2f |> Some) world

            // viscosity button
            if World.doButton $"Viscosity"
                [Entity.Position .= v3 255f 50f 0f
                 Entity.Text @= $"Viscosity: {fluidEmitter.GetViscocity world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 12] world then
                fluidEmitter.Viscocity.Map
                    (function
                     | 0.004f -> 0.01f
                     | 0.01f -> 0.1f
                     | 0.1f -> 1f
                     | 1f -> 2f
                     | 2f -> 5f
                     | 5f -> 20f
                     | _ -> 0.004f)
                    world

            // linear damping button
            if World.doButton $"Linear Damping"
                [Entity.Position .= v3 255f 20f 0f
                 Entity.Text @= $"Linear Damping: {fluidEmitter.GetLinearDamping world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 11] world then
                fluidEmitter.LinearDamping.Map
                    (function
                     | 0f -> 0.2f
                     | 0.2f -> 0.5f
                     | 0.5f -> 0.7f
                     | 0.7f -> 0.9f
                     | 0.9f -> 0.99f
                     | _ -> 0f)
                    world

            // particle radius button
            if World.doButton $"Particle Radius"
                [Entity.Position .= v3 255f -10f 0f
                 Entity.Text @= $"Particle Radius: {fluidEmitter.GetFluidParticleRadius world}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 10] world then
                fluidEmitter.FluidParticleRadius.Map
                    (function
                     | 28.8f -> fluidEmitter.LinearDamping.Map (max 0.5f) world; 22.2f // Particles would explode when tank is full without damping
                     | 22.2f -> 40.0f
                     | _ -> 28.8f)
                    world

            // draw cells button
            if World.doButton $"Draw Cells"
                [Entity.Position .= v3 255f -70f 0f
                 Entity.Text @= $"Draw Cells: {fluidEmitter.GetFluidParticleCellColor world |> Option.isSome}"
                 Entity.Elevation .= 1f
                 Entity.FontSizing .= Some 12] world then
                fluidEmitter.FluidParticleCellColor.Map (function Some _ -> None | None -> Some Color.LightBlue) world

            // squish button
            if World.doButton $"Squish"
                [Entity.Position .= v3 255f -100f 0f
                 Entity.Text .= "Squish"
                 Entity.Elevation .= 1f] world then
                let paddle = World.createEntity<Block2dDispatcher> None DefaultOverlay None world.ContextGroup world
                paddle.SetPosition (v3 -270f 0f 0f) world
                paddle.SetSize (v3 30f 500f 0f) world
                paddle.SetStaticImage Assets.Default.Paddle world
                paddle.SetBodyType Kinematic world
                paddle.SetLinearVelocity (v3 50f 0f 0f) world
                coroutine world.Launcher {
                    do! Coroutine.sleep (GameTime.ofSeconds 10f)
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
                     Entity.Justification .= Unjustified true
                     Entity.Text .=
                     "Controls: Mouse Left - Click button/Add particles. Mouse right - Delete particles.\n\
                        Mouse Left and Right - Summon a giant bubble that collides with particles.\n\
                        Mouse Middle - Draw contours that collide with particles. \n\
                        NOTE: Intersecting contours are not supported and will cause tunneling!"
                     Entity.FontSizing .= Some 10
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

            // mouse interactions with fluid system
            if fluidSim.GetSelected world && world.Advancing then
                let mousePosition = World.getMousePosition2dWorld false world
                match (World.isMouseButtonDown MouseLeft world, World.isMouseButtonDown MouseRight world) with
                | (true, false) ->

                    // mouse left - create particles
                    let particles =
                        seq {
                            for _ in 1 .. 4 do
                                let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * 16.0f
                                { FluidParticlePosition = (mousePosition + jitter).V3; FluidParticleVelocity = v3Zero; GravityOverride = ValueNone }}
                        |> SArray.ofSeq

                    // emit particles
                    World.emitFluidParticles particles fluidEmitterId world

                | (false, true) ->

                    // mouse right - delete particles
                    let predicate (particle : FluidParticle) =
                        let bounds = box2 (mousePosition - v2Dup 8.0f) (v2Dup 16.0f)
                        bounds.Contains particle.FluidParticlePosition.V2 = ContainmentType.Disjoint

                    // filter particles
                    World.filterFluidParticles predicate fluidEmitterId world

                | (true, true) ->

                    // mouse both - summon a bubble
                    fluidSim.MouseBubbleSize.Map inc world
                    World.doSphere2d "Bubble"
                        [Entity.Position @= mousePosition.V3
                         Entity.Size @= v3Dup (fluidSim.GetMouseBubbleSize world)
                         Entity.StaticImage .= Assets.Gameplay.BubbleImage] world |> ignore

                | (false, false) ->

                    // only reset size when both mouse buttons up
                    fluidSim.SetMouseBubbleSize 0f world

                // mouse middle - draw a contour
                if World.isMouseButtonPressed MouseMiddle world then
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        List.cons [|mousePosition|] lineSegments) world
                elif World.isMouseButtonDown MouseMiddle world then
                    fluidSim.LineSegments.Map (fun lineSegments ->
                        let active = lineSegments.[0]
                        if Vector2.Distance (mousePosition, Array.last active) > 8f then
                            List.updateAt 0 (Array.add mousePosition active) lineSegments
                        else lineSegments) world

            // declare containment contour
            for segment in fluidSim.GetLineSegments world do
                World.doEntity<LineSegmentsDispatcher> $"Contour {segment.[0]}" [Entity.LineSegments @= segment] world

            // end scene declaration
            World.endGroup world

            // process camera as last task
            World.setEye2dCenter (v2 60f 10f) world
            
        // reset cursor when deselecting
        if FQueue.contains Deselecting selectionResults then
            World.setCursorType DefaultCursor world