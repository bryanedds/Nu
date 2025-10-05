namespace SandBox2d
open System
open System.Numerics
open nkast.Aether.Physics2D.Dynamics.Joints
open Prime
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
    
    static let RaceCourseScale = 16f
    static let CarSpawnPosition = v3 0f 30f 0f
    static let CarSpeedMax = 50f

    static let RaceTrackContour =
        [|v2 -20f 5f; v2 -20f 0f; v2 20f 0f; v2 25f 0.25f; v2 30f 1f; v2 35f 4f; v2 40f 0f; v2 45f 0f;
          v2 50f -1f; v2 55f -2f; v2 60f -2f; v2 65f -1.25f; v2 70f 0f; v2 75f 0.3f; v2 80f 1.5f; v2 85f 3.5f;
          v2 90f 0f; v2 95f -0.5f; v2 100f -1f; v2 105f -2f; v2 110f -2.5f; v2 115f -1.3f; v2 120f 0f; v2 160f 0f;
          v2 159f -10f; v2 201f -10f; v2 200f 0f; v2 240f 0f; v2 250f 5f; v2 250f -10f; v2 270f -10f; v2 270f 0f;
          v2 310f 0f; v2 310f 5f|]

    static let RaceTrackPoints =
        RaceTrackContour
        |> Array.map ((*) RaceCourseScale)
        |> Array.map _.V3

    static let CarContour =
        [|v2 -2.5f 0.92f; v2 -2.375f 1.46f; v2 -0.58f 1.92f; v2 0.46f 1.92f
          v2 2.5f 1.17f; v2 2.5f 0.795f; v2 2.3f 0.67f; v2 -2.25f 0.65f|]

    static let CarContourBounds =
        Box2.Enclose CarContour

    static let CarPoints =
        CarContour
        |> Array.map (fun position -> position - CarContourBounds.Center)
        |> Array.map (fun position -> position / CarContourBounds.Size)
        |> Array.map _.V3

    static let CarSize =
        CarContour
        |> Array.map (fun position -> position - CarContourBounds.Center)
        |> Array.map (fun position -> position * RaceCourseScale)
        |> Box2.Enclose
        |> fun carBounds -> carBounds.Size.V3

    static let computeWheelOffset (position : Vector2) rotation =
        position - CarContourBounds.Center
        |> fun position -> position.Rotate rotation
        |> fun position -> position * RaceCourseScale
        |> fun position -> position.V3

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
            World.doBlock2d "Race Track"
                [Entity.Size .= v3 1f 1f 0f
                 Entity.BodyShape .= ContourShape { Links = RaceTrackPoints; Closed = false; TransformOpt = None; PropertiesOpt = None }
                 Entity.CollisionDetection .= Continuous] world |> ignore // keep car wheels above ground
            for (p1, p2) in Array.pairwise RaceTrackPoints do
                World.doStaticSprite $"Race Track {p1} -> {p2}"
                    [Entity.Position .= (p1 + p2) / 2f
                     Entity.Size .= v3 (p2 - p1).Magnitude 2f 0f
                     Entity.Rotation .= Quaternion.CreateLookAt2d (p2 - p1).V2
                     Entity.StaticImage .= Assets.Default.Black] world

            // declare car
            World.doBox2d "Car"
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
                 Entity.Substance .= Density 4f
                 Entity.Friction .= 0.2f] world |> ignore
            let car = world.DeclaredEntity

            // declare wheels (and joints)
            for (relation, position, density, frequency, friction, maxTorque) in
                [("Back", v2 -1.709f 0.78f, 0.8f, 5f, 0.9f, 20f)
                 ("Front", v2 1.54f 0.8f, 1f, 8.5f, 0.2f, 10f)] do
                let carRotation = (car.GetRotation world).Angle2d
                let wheelOffset = computeWheelOffset position carRotation
                let wheelPosition = CarSpawnPosition + wheelOffset
                World.doBall2d $"Wheel {relation}"
                    [Entity.Position |= wheelPosition
                     Entity.Rotation |= quatIdentity
                     Entity.Size .= v3One * RaceCourseScale
                     Entity.StaticImage .= Assets.Gameplay.WheelImage
                     Entity.Substance .= Density (density * 2f)
                     Entity.Friction .= friction
                     Entity.Elevation .= 0.1f] world |> ignore
                let (bodyJointId, _) =
                    World.doBodyJoint2d $"Wheel {relation} Joint"
                        [Entity.BodyJoint |= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ car wheel ->
                            // a wheel joint fixes relative position of two bodies, labelled body A and body B,
                            // where body B is positionally anchored relative to body A, can exhibit
                            // spring movement along an axis (i.e. wheel suspension), and can rotate freely.
                            WheelJoint
                                (car, wheel, wheel.Position, new _ (0f, 1.2f), true,
                                 Frequency = frequency, DampingRatio = 0.85f, MaxMotorTorque = maxTorque) }
                         Entity.BodyJointTarget .= Address.makeFromString "^/Car"
                         Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/Wheel {relation}")
                         Entity.CollideConnected .= false] world
                if raceCourse.GetSelected world && relation = "Back" then
                    let acceleration = raceCourse.GetCarAcceleration world
                    let motorSpeed = single (sign acceleration) * Math.SmoothStep (0f, CarSpeedMax, abs acceleration)
                    World.setBodyJointMotorSpeed motorSpeed bodyJointId world
                    World.setBodyJointMotorEnabled (abs motorSpeed >= CarSpeedMax * 0.06f) bodyJointId world

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
            World.doBox2d "Teeter Board"
                [Entity.Position |= v3 140f 1f 0f * RaceCourseScale
                 Entity.Rotation |= Quaternion.CreateFromAngle2d 0.15f
                 Entity.Size .= v3 20f 0.5f 0f * RaceCourseScale
                 Entity.StaticImage .= Assets.Default.Paddle
                 Entity.Substance .= Density 1f
                 Entity.CollisionDetection .= Continuous] world |> ignore
            World.doBodyJoint2d "Teeter Joint"
                [Entity.BodyJoint |= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                    RevoluteJoint (a, b, b.Position, b.Position, true,
                        LimitEnabled = true, LowerLimit = -8.0f * MathF.PI / 180.0f,
                        UpperLimit = 8.0f * MathF.PI / 180.0f) }
                 Entity.BodyJointTarget .= Address.makeFromString "^/Race Track"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString "^/Teeter Board")
                 Entity.CollideConnected .= false] world |> ignore

            // declare bridge
            for i in 0 .. 20 do
                if i < 20 then
                    World.doBox2d $"Bridge {i}"
                        [Entity.Position |= v3 (161f + 2f * single i) -0.125f 0f * RaceCourseScale
                         Entity.Rotation |= quatIdentity
                         Entity.Size .= v3 2f 0.25f 0f * RaceCourseScale
                         Entity.Friction .= 0.6f
                         Entity.StaticImage .= Assets.Default.Paddle
                         Entity.CollisionDetection .= Continuous
                         Entity.Substance .= Density 1f] world |> ignore
                World.doBodyJoint2d $"Bridge {i} Link"
                    [Entity.BodyJoint |= TwoBodyJoint2d {
                        CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                            let p =
                                if i < 20
                                then b.Position - toPhysicsV2 (v3 RaceCourseScale 0f 0f)
                                else a.Position + toPhysicsV2 (v3 RaceCourseScale 0f 0f)
                            RevoluteJoint (a, b, p, p, true) }
                     Entity.BodyJointTarget .= Address.makeFromString (if i = 0 then "^/Race Track" else $"^/Bridge {i-1}")
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString (if i < 20 then $"^/Bridge {i}" else "^/Race Track"))] world |> ignore

            // declare boxes
            for i in 0 .. 2 do
                World.doBox2d $"Box {i}"
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
            World.setGravity2d (World.getGravityDefault2d world) world

            // process car camera as the last task
            // menu offset (X = 60) + car lookahead (X = 40) + make objects spawn above ground (Y = 60)
            let carPosition = (car.GetPosition world).V2 + v2 100f 60f
            World.setEye2dCenter carPosition world