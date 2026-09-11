namespace SandBox2d
open System
open System.Numerics
open Box2D.NET
open Nu

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module RaceCourseScreenExtensions =
    type Screen with
        member this.GetCarAcceleration world : single = this.Get (nameof Screen.CarAcceleration) world
        member this.SetCarAcceleration (value : single) world = this.Set (nameof Screen.CarAcceleration) value world
        member this.CarAcceleration = lens (nameof Screen.CarAcceleration) this this.GetCarAcceleration this.SetCarAcceleration

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type RaceCourseDispatcher () =
    inherit ScreenDispatcherImSim ()
    
    // Keep the chassis and wheels above the first track segment at creation time.
    // Wheel-joint motor speed is radians per second, rather than screen pixels per second.
    static let RaceCourseScale = Sandbox2dGeometry.RaceCourseScale
    static let CarMotorSpeedMax = Sandbox2dGeometry.CarMotorSpeedMax

    static let RaceTrackPoints =
        Sandbox2dGeometry.RaceTrackContour
        |> Array.map ((*) RaceCourseScale)
        |> Array.map _.V3

    static let CarContour = Sandbox2dGeometry.CarContour
    static let CarContourBounds = Sandbox2dGeometry.CarContourBounds

    static let CarPoints =
        CarContour
        |> Array.map (fun position -> position - CarContourBounds.Center)
        |> Array.map (fun position -> position / CarContourBounds.Size)
        |> Array.map _.V3

    static let CarSize =
        Sandbox2dGeometry.carSize.V3

    static let CarSpawnPosition =
        (Sandbox2dGeometry.CarContourBounds.Center * RaceCourseScale).V3 + v3 0f RaceCourseScale 0f

    // here we define default property values
    static member Properties =
        [define Screen.CarAcceleration 0f]

    // here we define the screen's top-level behavior
    override this.Process (_, raceCourse, world) =

        // declare scene when selected
        if raceCourse.GetSelected world then

            // begin scene declaration
            World.beginGroup Simulants.RaceCourseScene.Name [] world

            // declare border
            World.doStaticSprite Simulants.RaceCourseBorder.Name
                [Entity.Size .= v3 640f 360f 0f
                 Entity.Elevation .= -1f
                 Entity.Absolute .= true // displays at the same screen location regardless of the eye position
                 Entity.StaticImage .= Assets.Gameplay.BackgroundImage] world

            // declare race track
            World.doBlockBody2d "Race Track"
                [Entity.Size .= v3 1f 1f 0f
                 Entity.BodyShape .= ContourShape { Links = RaceTrackPoints; Closed = false; TransformOpt = None; PropertiesOpt = None }
                 Entity.Friction .= 0.6f
                 Entity.CollisionDetection .= Continuous] world |> ignore // keep car wheels above ground
            for (p1, p2) in Array.pairwise RaceTrackPoints do
                World.doStaticSprite $"Race Track {p1} -> {p2}"
                    [Entity.Position .= (p1 + p2) / 2f
                     Entity.Size .= v3 (p2 - p1).Magnitude 2f 0f
                     Entity.Rotation .= Quaternion.CreateLookAt2d (p2 - p1).V2
                     Entity.StaticImage .= Assets.Default.Black] world

            // declare car
            World.doBoxBody2d "Car"
                [Entity.Position |= CarSpawnPosition
                 Entity.Rotation |= quatIdentity
                 Entity.Size .= CarSize
                 Entity.BodyShape .=
                    PointsShape
                        { Points = CarPoints
                          Profile = Convex
                          TransformOpt = None
                          PropertiesOpt = None }
                 Entity.StaticImage .= Assets.Gameplay.CarImage
                 Entity.Substance .= Density (Sandbox2dGeometry.CarChassisDensity * Sandbox2dGeometry.SourceMassScale)
                 Entity.Friction .= Sandbox2dGeometry.CarChassisFriction] world |> ignore
            let car = world.DeclaredEntity

            // declare wheels (and joints)
            for (relation, position, density, frequency, friction, maxTorque, isMotor) in Sandbox2dGeometry.CarWheelSpecs do
                let wheelPosition = v3 position.X position.Y 0f * RaceCourseScale
                World.doBallBody2d $"Wheel {relation}"
                    [Entity.Position |= wheelPosition
                     Entity.Rotation |= quatIdentity
                     Entity.Size .= v3One * RaceCourseScale
                     Entity.StaticImage .= Assets.Gameplay.WheelImage
                     Entity.Substance .= Density (density * Sandbox2dGeometry.SourceMassScale)
                     Entity.Friction .= friction
                     Entity.Elevation .= 0.1f] world |> ignore
                let (bodyJointId, _) =
                    World.doBodyJoint2d $"Wheel {relation} Joint"
                        [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ car wheel world ->
                            // a wheel joint fixes relative position of two bodies, labelled body A and body B,
                            // where body B is positionally anchored relative to body A, can exhibit
                            // spring movement along an axis (i.e. wheel suspension), and can rotate freely.
                            let mutable jointDef = B2Joints.b2DefaultWheelJointDef ()
                            jointDef.``base``.bodyIdA <- car
                            jointDef.``base``.bodyIdB <- wheel
                            // the joint local anchor point for body A (car) is the relative position of body B (wheel)
                            // and the joint local anchor point for body B (wheel) is the left empty (origin)
                            jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (car, B2Bodies.b2Body_GetPosition wheel)
                            jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (wheel, B2Bodies.b2Body_GetPosition wheel)
                            jointDef.``base``.localFrameA.q <- B2MathFunction.b2MakeRot MathF.PI_OVER_2 // wheel axis is vertical relative to car
                            jointDef.enableSpring <- true
                            jointDef.hertz <- frequency
                            jointDef.dampingRatio <- Sandbox2dGeometry.CarWheelDampingRatio
                            jointDef.maxMotorTorque <- maxTorque * Sandbox2dGeometry.SourceTorqueScale
                            B2Joints.b2CreateWheelJoint (world, &jointDef) }
                         Entity.BodyJointTarget .= Address.makeFromString "^/Car"
                         Entity.BodyJointTarget2 .= Address.makeFromString $"^/Wheel {relation}"
                         Entity.CollideConnected .= false] world
                if raceCourse.GetSelected world && isMotor then
                    let acceleration = raceCourse.GetCarAcceleration world
                    let motorSpeed = Sandbox2dGeometry.carMotorSpeed acceleration
                    World.setBodyJointMotorSpeed motorSpeed bodyJointId world
                    World.setBodyJointMotorEnabled (abs motorSpeed >= CarMotorSpeedMax * 0.06f) bodyJointId world

            // process car input
            if raceCourse.GetSelected world then
                if World.isKeyboardKeyDown KeyboardKey.Left world then
                    raceCourse.CarAcceleration.Map (fun a -> min (a + 2.0f * world.ClockDelta) 1f) world
                elif World.isKeyboardKeyDown KeyboardKey.Right world then
                    raceCourse.CarAcceleration.Map (fun a -> max (a - 2.0f * world.ClockDelta) -1f) world
                elif World.isKeyboardKeyPressed KeyboardKey.Down world then
                    raceCourse.SetCarAcceleration 0f world
                else raceCourse.CarAcceleration.Map (fun a -> a - single (sign a) * 2.0f * world.ClockDelta) world

            // declare teeter totter
            let teeterBodyId, _ =
                World.doBoxBody2d "Teeter Board"
                    [Entity.Position |= v3 140f Sandbox2dGeometry.TeeterCenterY 0f * RaceCourseScale
                     Entity.Rotation |= Quaternion.CreateFromAngle2d Sandbox2dGeometry.TeeterInitialAngle
                     Entity.Size .= v3 (2f * Sandbox2dGeometry.TeeterBoardHalfLength) (2f * Sandbox2dGeometry.TeeterBoardHalfThickness) 0f * RaceCourseScale
                     Entity.StaticImage .= Assets.Default.Paddle
                     Entity.Substance .= Density Sandbox2dGeometry.SourceMassScale
                     Entity.CollisionDetection .= Continuous] world
            let teeterInitializing = world.DeclaredInitializing
            World.doBodyJoint2d "Teeter Joint"
                [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, B2Bodies.b2Body_GetPosition b)
                    jointDef.enableLimit <- true // required for lowerAngle and upperAngle to take effect
                    jointDef.lowerAngle <- -Sandbox2dGeometry.TeeterAngleLimit
                    jointDef.upperAngle <- Sandbox2dGeometry.TeeterAngleLimit
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                 Entity.BodyJointTarget .= Address.makeFromString "^/Race Track"
                 Entity.BodyJointTarget2 .= Address.makeFromString "^/Teeter Board"
                 Entity.CollideConnected .= false] world |> ignore
            if teeterInitializing then
                World.applyBodyAngularImpulse (v3 0f 0f Sandbox2dGeometry.TeeterAngularImpulse) teeterBodyId world

            // declare bridge
            let bridgeEndpoints =
                Array.init Sandbox2dGeometry.BridgeLinkCount (fun i ->
                    v3 (160f + 2f * single i) -0.125f 0f, v3 (160f + 2f * single (i + 1)) -0.125f 0f)
                |> Array.map (fun (a, b) -> a * RaceCourseScale, b * RaceCourseScale)
            for i in 0 .. Sandbox2dGeometry.BridgeLinkCount do
                if i < Sandbox2dGeometry.BridgeLinkCount then
                    let endpoint1, endpoint2 = bridgeEndpoints[i]
                    World.doBoxBody2d $"Bridge {i}"
                        [Entity.Position |= (endpoint1 + endpoint2) / 2f
                         Entity.Rotation |= quatIdentity
                         Entity.Size .= v3 (endpoint2 - endpoint1).Magnitude Sandbox2dGeometry.BridgeLinkThickness 0f
                         Entity.Friction .= 0.6f
                         Entity.StaticImage .= Assets.Default.Paddle
                         Entity.CollisionDetection .= Continuous
                         Entity.Substance .= Density 1f] world |> ignore
                World.doBodyJoint2d $"Bridge {i} Link"
                    [Entity.BodyJoint |= Box2dNetBodyJoint {
                        CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                            let p =
                                if i = 0 then toPhysicsV2 (fst bridgeEndpoints[0])
                                elif i = Sandbox2dGeometry.BridgeLinkCount then toPhysicsV2 (snd bridgeEndpoints[i - 1])
                                else toPhysicsV2 (snd bridgeEndpoints[i - 1])
                            let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                            jointDef.``base``.bodyIdA <- a
                            jointDef.``base``.bodyIdB <- b
                            jointDef.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (a, p)
                            jointDef.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (b, p)
                            B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                     Entity.BodyJointTarget .= Address.makeFromString (if i = 0 then "^/Race Track" else $"^/Bridge {i-1}")
                     Entity.BodyJointTarget2 .= Address.makeFromString (if i < Sandbox2dGeometry.BridgeLinkCount then $"^/Bridge {i}" else "^/Race Track")
                     // Adjacent planks already touch at their joint anchors; avoid collision impulses fighting the hinge.
                     Entity.CollideConnected .= Sandbox2dGeometry.BridgeCollideConnected] world |> ignore

            // declare boxes
            for i in 0 .. 2 do
                World.doBoxBody2d $"Box {i}"
                    [Entity.Position |= v3 220f (0.5f + single i) 0f * RaceCourseScale
                     Entity.Size .= v3One * RaceCourseScale
                     Entity.Substance .= Density 1f] world |> ignore

            // switch screen button
            World.doButton Simulants.RaceCourseSwitchScreen.Name
                [Entity.Position .= v3 230f -140f 0f
                 Entity.Text .= "Switch Screen"
                 Entity.Elevation .= 1f] world |> ignore

            // end scene declaration
            World.endGroup world
            
            // reset gravity from ToyBox
            World.setGravity2d (v3 0f Sandbox2dGeometry.SourceGravity 0f) world

            // process car camera as the last task
            // menu offset (X = 60) + car lookahead (X = 40) + make objects spawn above ground (Y = 60)
            let carPosition = (car.GetPosition world).V2 + v2 100f 60f
            World.setEye2dCenter carPosition world