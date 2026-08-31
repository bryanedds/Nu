// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu

module Tests =
    [<Test>]
    let ``Car source contract preserves contour origin and wheel tuning`` () =
        Assert.That (Sandbox2dGeometry.CarContour.Length, Is.EqualTo 8)
        Assert.That (Sandbox2dGeometry.CarContourBounds.Center, Is.EqualTo (v2 0f 0.285f))
        Assert.That (Sandbox2dGeometry.CarContour[0], Is.EqualTo (v2 -2.5f -0.08f))
        Assert.That (Sandbox2dGeometry.CarContour[7], Is.EqualTo (v2 -2.25f -0.35f))
        Assert.That (Sandbox2dGeometry.CarRearWheelModelOffset, Is.EqualTo (v2 -1.709f 0.78f))
        Assert.That (Sandbox2dGeometry.CarWheelDampingRatio, Is.EqualTo 0.7f)
        Assert.That (Sandbox2dGeometry.CarChassisDensity, Is.EqualTo 2f)
        let back, front = Sandbox2dGeometry.CarWheelSpecs[0], Sandbox2dGeometry.CarWheelSpecs[1]
        Assert.That (back, Is.EqualTo ("Back", v2 -1.709f 0.78f, 0.8f, 4f, 0.9f, 20f, true))
        Assert.That (front, Is.EqualTo ("Front", v2 1.54f 0.8f, 1f, 4f, 0.2f, 10f, false))

    [<Test>]
    let ``Ragdoll arm center leaves torso clearance`` () =
        let centerX = Sandbox2dGeometry.ragdollArmCenterX 30f 24f 1f
        Assert.That (centerX - 12f - 15f, Is.EqualTo Sandbox2dGeometry.RagdollArmClearance)

    [<Test>]
    let ``Ragdoll torso joints use exact shared anchors`` () =
        let spacing = Sandbox2dGeometry.RagdollLimbSpacing
        let upper = Sandbox2dGeometry.ragdollTorsoJointLocalOffset spacing false
        let lower = Sandbox2dGeometry.ragdollTorsoJointLocalOffset spacing true
        Assert.That (upper + lower, Is.EqualTo 0f)
        Assert.That (abs upper, Is.EqualTo (spacing / 2f))

    [<Test>]
    let ``Right upper arm anchor preserves authored increment`` () =
        let anchor = Sandbox2dGeometry.limbJointAnchor (v3 120f 80f 0f) (v3 37f 40f 0f) (v3 30f 0f 0f)
        Assert.That (anchor, Is.EqualTo (v3 142f 120f 0f))

    [<Test>]
    let ``Bridge links share authored endpoints at arbitrary angle`` () =
        let first, last = v3 10f 20f 0f, v3 106f 52f 0f
        let endpoints = Sandbox2dGeometry.bridgeLinkEndpoints first last 6
        Assert.That (endpoints.Length, Is.EqualTo 6)
        for i in 1 .. endpoints.Length - 1 do Assert.That (snd endpoints[i - 1], Is.EqualTo (fst endpoints[i]))
        let point ((first : Vector3), (last : Vector3)) positive =
            let halfLength = (last - first).Magnitude / 2f
            (first + last) / 2f + ((Sandbox2dGeometry.bridgeJointLocalEndpoint halfLength positive).Transform (Sandbox2dGeometry.bridgeRotation first last)).V3
        for endpointPair in endpoints do
            Assert.That ((point endpointPair true - snd endpointPair).Magnitude, Is.LessThan 0.001f)
            Assert.That ((point endpointPair false - fst endpointPair).Magnitude, Is.LessThan 0.001f)

    [<Test>]
    let ``Bridge endpoints are separated by the requested minimum span`` () =
        let first, last = Sandbox2dGeometry.bridgeEndpoints v3Zero v2Zero
        Assert.That ((first - last).Magnitude, Is.GreaterThanOrEqualTo Sandbox2dGeometry.MinimumBridgeSpan)

    [<Test>]
    let ``Race track contour preserves Nu ghost endpoints and authored order`` () =
        let contour = Sandbox2dGeometry.RaceTrackContour
        Assert.That (contour.Length, Is.EqualTo 36)
        Assert.That (contour[0], Is.EqualTo (v2 310f 5f))
        Assert.That (contour[0], Is.EqualTo contour[1])
        Assert.That (contour[34], Is.EqualTo contour[35])
        Assert.That (contour[35], Is.EqualTo (v2 -20f 5f))

    [<Test>]
    let ``Teeter source contract preserves start pose and limits`` () =
        Assert.That (Sandbox2dGeometry.TeeterInitialAngle, Is.EqualTo 0f)
        Assert.That (Sandbox2dGeometry.TeeterCenterY, Is.EqualTo 1f)
        Assert.That (Sandbox2dGeometry.TeeterBoardHalfLength, Is.EqualTo 10f)
        Assert.That (Sandbox2dGeometry.TeeterBoardHalfThickness, Is.EqualTo 0.25f)
        Assert.That (Sandbox2dGeometry.TeeterAngleLimit, Is.EqualTo (8f * MathF.PI / 180f))

    let private makeIntegrationWorldWithPhysics () =
        let plugin = SandBox2dPlugin ()
        let windowSize = Constants.Render.DisplayVirtualResolution
        let windowViewport = Viewport.makeWindow1 windowSize
        let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size
        let renderer = StubRendererProcess () :> RendererProcess
        let dependencies =
            { SdlDepsOpt = None
              ImGui = ImGui (true, windowViewport.Bounds.Size)
              PhysicsEngine2d = plugin.MakePhysicsEngine2d ()
              PhysicsEngine3d = StubPhysicsEngine.make ()
              RendererPhysics3dOpt = None
              RendererProcess = renderer
              AudioPlayer = StubAudioPlayer.make ()
              CursorClient = StubCursorClient.make () }
        let world = World.make (constant None) (SuppliedDependencies dependencies) { WorldConfig.defaultConfig with Accompanied = true } windowSize geometryViewport windowViewport plugin
        world, dependencies.PhysicsEngine2d

    let private makeIntegrationWorld () = makeIntegrationWorldWithPhysics () |> fst

    [<Test; Category "Integration">]
    let ``FluidSim real bubble visual and physics sizes agree`` () =
        Nu.init ()
        let world, physicsEngine2d = makeIntegrationWorldWithPhysics ()
        let mutable overlayObserved = false
        let mutable bubbleObserved = false
        let mutable mousePressed = false
        let mutable overlayInset : Box2 option = None
        let mutable bubbleInset : Box2 option = None
        let mutable bubbleSize = v3Zero
        let mutable bubbleScale = v3Zero
        let mutable bubbleRadius = 0f
        let mutable bodyShapeRadius = 0f
        let mutable bubbleResizeFrames = 0
        let mutable previousBubbleSize = v3Zero
        let mutable colliderAligned = false
        let mutable overlapReady = false
        let mutable staleEventCycleCompleted = false
        let mutable firstIntegrationContact = false
        let mutable bubbleBodyExistedBeforePropagation = false
        let mutable bubbleBodyExistedAfterPropagation = false
        let runWhile (world : World) = world.UpdateTime < 120L
        let preProcess (world : World) =
            World.setFramePacing false world
            Game.SetGameState FluidSim world
            let overlay : Entity = Simulants.FluidSimScene / "Tool Panel" / "Bubble Overlay"
            if overlay.GetExists world then
                overlayObserved <- true
                overlayInset <- overlay.GetInsetOpt world
                let screen = Simulants.FluidSim
                screen.SetSelectedTool Bubble world
                let feeler : Entity = Simulants.FluidSimScene / "Feeler"
                if feeler.GetExists world && not mousePressed then
                    World.publishPlus
                        { Position = World.getMousePosition world
                          Button = MouseLeft
                          Down = true }
                        Nu.Game.Handle.MouseLeftDownEvent
                        EventTrace.empty
                        feeler
                        false
                        false
                        world
                    mousePressed <- true
                let bubble : Entity = Simulants.FluidSimScene / "Bubble"
                if bubble.GetExists world then
                    bubbleObserved <- true
                    bubbleInset <- bubble.GetInsetOpt world
                    bubbleSize <- bubble.GetSize world
                    if bubbleSize <> previousBubbleSize then
                        bubbleResizeFrames <- inc bubbleResizeFrames
                        previousBubbleSize <- bubbleSize
                    bubbleRadius <- screen.GetMouseBubbleSize world
                    bubbleScale <- bubble.GetScale world
                    let circle : Entity = Simulants.FluidSimScene / "Circle"
                    if circle.GetExists world then
                        circle.SetBodyType Dynamic world
                        circle.SetSensor true world
                        circle.SetPosition (bubble.GetPosition world) world
                        circle.SetLinearVelocity v3Zero world
                        colliderAligned <- true
                        if not staleEventCycleCompleted && overlapReady then
                            let bubbleBodyId = bubble.GetBodyId world
                            let circleBodyId = circle.GetBodyId world
                            circle.PropagatePhysics world
                            bubble.PropagatePhysics world
                            bubbleBodyExistedBeforePropagation <- physicsEngine2d.GetBodyExists bubbleBodyId
                            firstIntegrationContact <-
                                match physicsEngine2d.TryIntegrate (GameTime.ofUpdates 1L) with
                                | Some messages ->
                                    let mutable found = false
                                    for message in messages do
                                        match message with
                                        | BodyPenetrationMessage penetration ->
                                            found <- found ||
                                                ((penetration.BodyShapeSource.BodyId = bubbleBodyId && penetration.BodyShapeTarget.BodyId = circleBodyId) ||
                                                 (penetration.BodyShapeSource.BodyId = circleBodyId && penetration.BodyShapeTarget.BodyId = bubbleBodyId))
                                        | _ -> ()
                                    found
                                | None -> false
                            bubble.SetSize (bubble.GetSize world + v3 1f 1f 0f) world
                            bubble.PropagatePhysics world
                            bubbleBodyExistedAfterPropagation <- physicsEngine2d.GetBodyExists bubbleBodyId
                            physicsEngine2d.TryIntegrate (GameTime.ofUpdates 1L) |> ignore
                            staleEventCycleCompleted <- true
                        overlapReady <- true
                    match bubble.GetBodyShape world with
                    | SphereShape sphere -> bodyShapeRadius <- sphere.Radius
                    | _ -> ()
        let collision2dFrameCompensation = Constants.Physics.Collision2dFrameCompensation
        Constants.Physics.Collision2dFrameCompensation <- false
        let result =
            try World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore (Some ignore) world
            finally Constants.Physics.Collision2dFrameCompensation <- collision2dFrameCompensation
        Assert.That (result, Is.EqualTo Constants.Engine.ExitCodeSuccess)
        Assert.That (overlayObserved, Is.True)
        Assert.That (bubbleObserved, Is.True)
        Assert.That (colliderAligned, Is.True)
        Assert.That (staleEventCycleCompleted, Is.True)
        Assert.That (bubbleBodyExistedBeforePropagation, Is.True)
        Assert.That (bubbleBodyExistedAfterPropagation, Is.True)
        Assert.That (firstIntegrationContact, Is.True, "first integration did not report Bubble/Circle penetration")
        Assert.That (bubbleResizeFrames, Is.GreaterThan 10,
            $"bubble was resized for only {bubbleResizeFrames} frames")
        Assert.That (overlayInset, Is.EqualTo (Some Sandbox2dGeometry.BubbleImageInset))
        Assert.That (bubbleInset, Is.EqualTo (Some Sandbox2dGeometry.BubbleImageInset))
        Assert.That (bubbleInset.Value.Min, Is.EqualTo (v2 133f 129f))
        Assert.That (bubbleInset.Value.Size, Is.EqualTo (v2 748f 810f))
        Assert.That (bubbleSize.X, Is.EqualTo (Sandbox2dGeometry.bubbleDiameter bubbleRadius))
        Assert.That (bubbleSize.Y, Is.EqualTo bubbleSize.X)
        Assert.That (bubbleScale, Is.EqualTo v3One)
        Assert.That (bodyShapeRadius, Is.EqualTo 0.5f)

    [<Test; Category "Integration">]
    let ``RaceCourse real car reaches final wall`` () =
        Nu.init ()
        let world = makeIntegrationWorld ()
        let car : Entity = Simulants.RaceCourseScene / "Car"
        let boxes : Entity array = [| for i in 0 .. 2 -> Simulants.RaceCourseScene / $"Box {i}" |]
        let mutable maximumFront = Single.NegativeInfinity
        let mutable latestFront = Single.NegativeInfinity
        let maximumBoxFronts = Array.create boxes.Length Single.NegativeInfinity
        let latestBoxFronts = Array.create boxes.Length Single.NegativeInfinity
        let latestBoxWidths = Array.create boxes.Length 0f
        let latestBoxPositions = Array.create boxes.Length v3Zero
        let mutable observed = false
        let mutable maximumTime = 0L
        let mutable maximumPosition = v3Zero
        let mutable maximumVelocity = v3Zero
        let mutable maximumAngularVelocity = v3Zero
        let mutable maximumAngle = 0f
        let mutable latestTime = 0L
        let mutable latestPosition = v3Zero
        let mutable latestVelocity = v3Zero
        let mutable latestAngularVelocity = v3Zero
        let mutable latestAngle = 0f
        let mutable commandedAcceleration = 0f
        let accelerationStep =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate rate
            | DynamicFrameRate rate -> 2f / single rate
        let runWhile (world : World) = world.UpdateTime < 3600L && maximumFront < 310f * Sandbox2dGeometry.RaceCourseScale
        let preProcess (world : World) =
            World.setFramePacing false world
            Game.SetGameState RaceCourse world
            if Simulants.RaceCourse.GetExists world then
                commandedAcceleration <- max (commandedAcceleration - accelerationStep) -1f
                Simulants.RaceCourse.SetCarAcceleration commandedAcceleration world
            if car.GetExists world then
                observed <- true
                let position = car.GetPosition world
                let velocity = car.GetLinearVelocity world
                let angularVelocity = car.GetAngularVelocity world
                let angle = car.GetRotation world |> _.Angle2d
                let front = (car.GetPerimeterMax world).X
                latestFront <- front
                latestTime <- world.UpdateTime
                latestPosition <- position
                latestVelocity <- velocity
                latestAngularVelocity <- angularVelocity
                latestAngle <- angle
                if front > maximumFront then
                    maximumFront <- front
                    maximumTime <- world.UpdateTime
                    maximumPosition <- position
                    maximumVelocity <- velocity
                    maximumAngularVelocity <- angularVelocity
                    maximumAngle <- angle
            for i, box in Array.indexed boxes do
                if box.GetExists world then
                    let perimeterMax = box.GetPerimeterMax world
                    let perimeterMin = box.GetPerimeterMin world
                    let front = perimeterMax.X
                    maximumBoxFronts[i] <- max maximumBoxFronts[i] front
                    latestBoxFronts[i] <- front
                    latestBoxWidths[i] <- perimeterMax.X - perimeterMin.X
                    latestBoxPositions[i] <- box.GetPosition world
        let result = World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore (Some ignore) world
        Assert.That (result, Is.EqualTo Constants.Engine.ExitCodeSuccess)
        Assert.That (observed, Is.True)
        let boxFronts = maximumBoxFronts |> Array.map (fun x -> x / Sandbox2dGeometry.RaceCourseScale) |> Array.map string |> (fun values -> String.Join (",", values))
        let boxPositions = latestBoxPositions |> Array.map string |> (fun values -> String.Join (",", values))
        let latestBoxIndex = latestBoxFronts |> Array.mapi (fun i front -> i, front) |> Array.maxBy snd |> fst
        let latestBoxFront = latestBoxFronts[latestBoxIndex]
        let latestBoxWidth = latestBoxWidths[latestBoxIndex]
        let endpointTolerance = 2f * Constants.Physics.Collision2dLinearSlop * Constants.Engine.Meter2d
        Assert.That (latestBoxFront, Is.InRange (310f * Sandbox2dGeometry.RaceCourseScale - endpointTolerance, 310f * Sandbox2dGeometry.RaceCourseScale + endpointTolerance),
            $"latest box index={latestBoxIndex} front authored={latestBoxFront / Sandbox2dGeometry.RaceCourseScale}; width={latestBoxWidth / Sandbox2dGeometry.RaceCourseScale}; boxes max fronts authored={boxFronts}; latest positions={boxPositions}")
        Assert.That (latestFront, Is.GreaterThanOrEqualTo (310f * Sandbox2dGeometry.RaceCourseScale - latestBoxWidth - endpointTolerance),
            $"maximum front authored={maximumFront / Sandbox2dGeometry.RaceCourseScale}; max t={maximumTime} pos={maximumPosition} vel={maximumVelocity} angular={maximumAngularVelocity} angle={maximumAngle}; latest t={latestTime} pos={latestPosition} vel={latestVelocity} angular={latestAngularVelocity} angle={latestAngle}; boxes max fronts authored={boxFronts} latest positions={boxPositions}")
        Assert.That (maximumFront, Is.LessThanOrEqualTo (310f * Sandbox2dGeometry.RaceCourseScale + Constants.Physics.Collision2dLinearSlop * Constants.Engine.Meter2d))

    [<Test; Category "Integration">]
    let ``ToyBox real bridge links settle`` () =
        Nu.init ()
        let world = makeIntegrationWorld ()
        let toyBox = Simulants.ToyBox
        let avatar : Entity = Simulants.ToyBoxScene / "Avatar"
        let endpoints : Entity array = [| Simulants.ToyBoxScene / "Bridge"; Simulants.ToyBoxScene / "Bridge Opposite End" |]
        let links : Entity array = [| for i in 0 .. 5 -> Simulants.ToyBoxScene / $"Bridge Paddle {i}" |]
        let mutable observed = false
        let mutable latestEndpointPositions = Array.zeroCreate<Vector3> endpoints.Length
        let mutable latestEndpointSizes = Array.zeroCreate<Vector3> endpoints.Length
        let mutable endpointBodyTypes = Array.create endpoints.Length Dynamic
        let mutable endpointSensors = Array.create endpoints.Length false
        let mutable maximumLinear = 0f
        let mutable maximumAngular = 0f
        let mutable latestLinear = 0f
        let mutable latestAngular = 0f
        let mutable consecutiveSettled = 0
        let mutable dragFrame = 0
        let mutable dragStep = v3Zero
        let mutable resizedObserved = false
        let mutable dragCompleted = false
        let mutable initialLinkSizes = Array.zeroCreate<Vector3> links.Length
        let runWhile (world : World) = world.UpdateTime < 1800L && consecutiveSettled < 60
        let preProcess (world : World) =
            World.setFramePacing false world
            Game.SetGameState ToyBox world
            if toyBox.GetExists world then toyBox.SetToys (FMap.empty |> FMap.add "Bridge" Bridge) world
            // Disable the avatar because it spawns inside the chain; this isolates bridge self-stability from that unrelated contact.
            if avatar.GetExists world then avatar.SetBodyEnabled false world
            if endpoints |> Array.forall (fun endpoint -> endpoint.GetExists world) && links |> Array.forall (fun link -> link.GetExists world) then
                observed <- true
                if dragFrame = 0 then
                    for i in 0 .. links.Length - 1 do initialLinkSizes[i] <- links[i].GetSize world
                    dragStep <- (endpoints[1].GetPosition world - endpoints[0].GetPosition world) / 16f
                    endpoints[0].SetBodyType Dynamic world
                    dragFrame <- 1
                elif dragFrame <= 4 then
                    endpoints[0].SetPosition (endpoints[0].GetPosition world + dragStep) world
                    endpoints[0].SetLinearVelocity v3Zero world
                    dragFrame <- inc dragFrame
                elif not dragCompleted then
                    endpoints[0].SetBodyType Static world
                    dragCompleted <- true
                for i in 0 .. endpoints.Length - 1 do
                    latestEndpointPositions[i] <- endpoints[i].GetPosition world
                    latestEndpointSizes[i] <- endpoints[i].GetSize world
                    endpointBodyTypes[i] <- endpoints[i].GetBodyType world
                    endpointSensors[i] <- endpoints[i].GetSensor world
                let mutable currentLinear = 0f
                let mutable currentAngular = 0f
                for link in links do
                    currentLinear <- max currentLinear (link.GetLinearVelocity world).Magnitude
                    currentAngular <- max currentAngular (link.GetAngularVelocity world).Magnitude
                latestLinear <- currentLinear
                latestAngular <- currentAngular
                maximumLinear <- max maximumLinear currentLinear
                maximumAngular <- max maximumAngular currentAngular
                if dragFrame > 0 then
                    let endpointPairs = Sandbox2dGeometry.bridgeLinkEndpoints (endpoints[0].GetPosition world) (endpoints[1].GetPosition world) links.Length
                    resizedObserved <-
                        resizedObserved ||
                            Array.mapi2 (fun i ((endpoint1, endpoint2) : Vector3 * Vector3) (link : Entity) ->
                                let expectedLength = (endpoint2 - endpoint1).Magnitude
                                abs ((link.GetSize world).Y - expectedLength) < 0.01f && abs ((link.GetSize world).Y - initialLinkSizes[i].Y) > 0.01f) endpointPairs links
                            |> Array.forall id
                if dragCompleted && resizedObserved && currentLinear < 0.1f && currentAngular < 0.1f then consecutiveSettled <- consecutiveSettled + 1
                else consecutiveSettled <- 0
        let result = World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore (Some ignore) world
        Assert.That (result, Is.EqualTo Constants.Engine.ExitCodeSuccess)
        Assert.That (observed, Is.True)
        Assert.That (dragCompleted, Is.True)
        Assert.That (resizedObserved, Is.True, "bridge paddle sizes did not follow the dragged endpoint")
        for i in 0 .. endpoints.Length - 1 do
            Assert.That (endpointBodyTypes[i], Is.EqualTo Static,
                $"endpoint={endpoints[i].Name}; position={latestEndpointPositions[i]}; size={latestEndpointSizes[i]}; body type={endpointBodyTypes[i]}; sensor={endpointSensors[i]}")
            Assert.That (endpointSensors[i], Is.True,
                $"endpoint={endpoints[i].Name}; position={latestEndpointPositions[i]}; size={latestEndpointSizes[i]}; body type={endpointBodyTypes[i]}; sensor={endpointSensors[i]}")
        let finalEndpointPairs = Sandbox2dGeometry.bridgeLinkEndpoints latestEndpointPositions[0] latestEndpointPositions[1] links.Length
        let endpointTolerance = 4f * Constants.Physics.Collision2dLinearSlop * Constants.Engine.Meter2d
        let worldEndpoint (link : Entity) positive =
            let halfLength = link.GetSize world |> fun size -> size.Y / 2f
            let local = Sandbox2dGeometry.bridgeJointLocalEndpoint halfLength positive
            link.GetPosition world + (local.Transform (link.GetRotation world)).V3
        for i in 0 .. links.Length - 1 do
            let _, expectedLast = finalEndpointPairs[i]
            let expectedLength = (expectedLast - fst finalEndpointPairs[i]).Magnitude
            Assert.That ((links[i].GetSize world).Y, Is.InRange (expectedLength - endpointTolerance, expectedLength + endpointTolerance),
                $"paddle={links[i].Name}; expected span={(expectedLast - fst finalEndpointPairs[i]).Magnitude}; actual size={(links[i].GetSize world).Y}")
        let anchorStart = latestEndpointPositions[0]
        let anchorEnd = latestEndpointPositions[1]
        Assert.That ((worldEndpoint links[0] false - anchorStart).Magnitude, Is.LessThanOrEqualTo endpointTolerance)
        for i in 1 .. links.Length - 1 do
            Assert.That ((worldEndpoint links[i - 1] true - worldEndpoint links[i] false).Magnitude, Is.LessThanOrEqualTo endpointTolerance,
                $"joint={i}; separation={(worldEndpoint links[i - 1] true - worldEndpoint links[i] false).Magnitude}; tolerance={endpointTolerance}")
        Assert.That ((worldEndpoint (Array.last links) true - anchorEnd).Magnitude, Is.LessThanOrEqualTo endpointTolerance)
        let endpointSpan = (latestEndpointPositions[0] - latestEndpointPositions[1]).Magnitude
        Assert.That (consecutiveSettled, Is.GreaterThanOrEqualTo 60,
            $"settled frames={consecutiveSettled}; latest linear={latestLinear}; latest angular={latestAngular}; maximum linear={maximumLinear}; maximum angular={maximumAngular}; endpoint 1 position={latestEndpointPositions[0]}; size={latestEndpointSizes[0]}; body type={endpointBodyTypes[0]}; sensor={endpointSensors[0]}; endpoint 2 position={latestEndpointPositions[1]}; size={latestEndpointSizes[1]}; body type={endpointBodyTypes[1]}; sensor={endpointSensors[1]}; span={endpointSpan}")

    [<Test; Category "Integration">]
    let ``ToyBox real ragdoll head stays clear of torso`` () =
        Nu.init ()
        let world = makeIntegrationWorld ()
        let toyBox = Simulants.ToyBox
        let ragdollName = "Ragdoll"
        let head : Entity = Simulants.ToyBoxScene / ragdollName
        let torso : Entity = head / $"{ragdollName} Torso Upper"
        let mutable minimumClearance = Single.PositiveInfinity
        let mutable observed = false
        let mutable perturbed = false
        let runWhile (world : World) = world.UpdateTime < 240L
        let preProcess (world : World) =
            World.setFramePacing false world
            Game.SetGameState ToyBox world
            if toyBox.GetExists world then toyBox.SetToys (FMap.empty |> FMap.add ragdollName Ragdoll) world
            if head.GetExists world && torso.GetExists world then
                observed <- true
                if not perturbed then head.SetAngularVelocity (v3 0f 0f 8f) world; perturbed <- true
                let local = (head.GetPosition world - torso.GetPosition world).Transform (torso.GetRotation world).Inverted
                let halfSegment = (torso.GetSize world).X / 2f - (torso.GetSize world).Y / 2f
                let closestX = max (-halfSegment) (min halfSegment local.X)
                let distance = sqrt ((local.X - closestX) ** 2f + local.Y ** 2f)
                minimumClearance <- min minimumClearance (distance - ((head.GetSize world).X + (torso.GetSize world).Y) / 2f)
        let result = World.runWithCleanUp runWhile preProcess ignore ignore ignore ignore (Some ignore) world
        Assert.That (result, Is.EqualTo Constants.Engine.ExitCodeSuccess)
        Assert.That (observed, Is.True)
        Assert.That (minimumClearance, Is.GreaterThanOrEqualTo (-2f * Constants.Physics.Collision2dLinearSlop * Constants.Engine.Meter2d))