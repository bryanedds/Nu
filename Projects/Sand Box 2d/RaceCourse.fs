namespace SandBox2d
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module RaceCourseScreenExtensions =
    type Screen with
        member this.GetCarAcceleration world : float32 = this.Get (nameof Screen.CarAcceleration) world
        member this.SetCarAcceleration (value : float32) world = this.Set (nameof Screen.CarAcceleration) value world
        member this.CarAcceleration = lens (nameof Screen.CarAcceleration) this this.GetCarAcceleration this.SetCarAcceleration
        member this.GetCarWheelJoint world : WheelJoint option = this.Get (nameof Screen.CarWheelJoint) world
        member this.SetCarBackWheelJoint (value : WheelJoint option) world = this.Set (nameof Screen.CarWheelJoint) value world
        member this.CarWheelJoint = lens (nameof Screen.CarWheelJoint) this this.GetCarWheelJoint this.SetCarBackWheelJoint
        
// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type RaceCourseDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.CarAcceleration 0f
         define Screen.CarWheelJoint None]

    // here we define the screen's top-level behavior
    override this.Process (_, raceCourse, world) =
    
        // begin scene declaration
        World.beginGroup Simulants.RaceCourseScene.Name [] world

        // declare border
        World.doStaticSprite Simulants.RaceCourseBorder.Name
            [Entity.Size .= v3 640f 360f 0f
             Entity.Absolute .= true // makes this display at the same screen location regardless of the eye position.
             Entity.Elevation .= -1f
             Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world

        // define race course
        let objectScale = 16f
        let raceTrack =
            [|v2 -20f 5f; v2 -20f 0f; v2 20f 0f; v2 25f 0.25f; v2 30f 1f; v2 35f 4f; v2 40f 0f; v2 45f 0f; 
              v2 50f -1f; v2 55f -2f; v2 60f -2f; v2 65f -1.25f; v2 70f 0f; v2 75f 0.3f; v2 80f 1.5f; v2 85f 3.5f; 
              v2 90f 0f; v2 95f -0.5f; v2 100f -1f; v2 105f -2f; v2 110f -2.5f; v2 115f -1.3f; v2 120f 0f; v2 160f 0f; 
              v2 159f -10f; v2 201f -10f; v2 200f 0f; v2 240f 0f; v2 250f 5f; v2 250f -10f; v2 270f -10f; v2 270f 0f; 
              v2 310f 0f; v2 310f 5f|]
            |> Array.map (fun p -> p.V3 * objectScale)
        World.doBlock2d "RaceCourse"
            [Entity.Size .= v3 1f 1f 0f
             Entity.BodyShape .= ContourShape { Links = raceTrack; Closed = false; TransformOpt = None; PropertiesOpt = None }
             Entity.CollisionDetection .= Continuous // don't let the car wheels fall through the ground
             Entity.CollisionCategories .= "10"] world |> ignore // don't collide with fan dragging
        for (p1, p2) in Array.pairwise raceTrack do
            World.doStaticSprite $"RaceCourse {p1} -> {p2}"
                [Entity.Position .= (p1 + p2) / 2f
                 Entity.Size .= v3 (p2 - p1).Magnitude 2f 0f
                 Entity.Rotation .= Quaternion.CreateLookAt2d (p2 - p1).V2
                 Entity.StaticImage .= Assets.Default.Black] world

        // declare car
        let carMaxSpeed = 50f
        let carSpawnPosition = v3 0f 30f 0f
        let carPoints = [|
            v2 -2.5f 0.92f
            v2 -2.375f 1.46f
            v2 -0.58f 1.92f
            v2 0.46f 1.92f
            v2 2.5f 1.17f
            v2 2.5f 0.795f
            v2 2.3f 0.67f
            v2 -2.25f 0.65f|]
        let carPointsBox = Box2.Enclose carPoints
        let carGetRelativePosition p = (p - carPointsBox.Center) / carPointsBox.Size
        World.doBox2d "Car"
            [Entity.BodyShape .= PointsShape {
                Points = Array.map (carGetRelativePosition >> _.V3) carPoints
                Profile = Convex
                TransformOpt = None
                PropertiesOpt = None }
             Entity.StaticImage .= Assets.Gameplay.Car
             Entity.Position .= carSpawnPosition
             Entity.Size .= carPointsBox.Size.V3 * objectScale
             Entity.Substance .= Density 4f
             Entity.Friction .= 0.2f] world |> ignore
        let car = world.DeclaredEntity
        for (relation, position, density, frequency, friction, maxTorque) in
            [("Back", v2 -1.709f 0.78f, 0.8f, 5f, 0.9f, 20f)
             ("Front", v2 1.54f 0.8f, 1f, 8.5f, 0.2f, 10f)] do
            let wheelRelativePosition = (carGetRelativePosition position * carPointsBox.Size).V3 * objectScale
            World.doBall2d $"Wheel {relation}"
                [Entity.StaticImage .= Assets.Gameplay.Wheel
                 Entity.Position .= carSpawnPosition + wheelRelativePosition
                 Entity.Size .= v3Dup 1f * objectScale
                 Entity.Substance .= Density (density * 2f)
                 Entity.Friction .= friction
                 Entity.Elevation .= 0.1f] world |> ignore
            World.doBodyJoint2d $"Wheel {relation} joint"
                [Entity.BodyJoint .= TwoBodyJoint2d {
                    CreateTwoBodyJoint = fun _ _ car wheel ->
                        // A wheel joint fixes relative position of two bodies, labelled body A and body B,
                        // where body B is positionally anchored relative to body A, can exhibit
                        // spring movement along an axis (i.e. wheel suspension), and can rotate freely.
                        let wheelJoint =
                            WheelJoint (car, wheel, wheel.Position, new _(0f, 1.2f), true,
                                Frequency = frequency, DampingRatio = 0.85f, MaxMotorTorque = maxTorque)
                        if relation = "Back" then raceCourse.SetCarBackWheelJoint (Some wheelJoint) world
                        wheelJoint }
                 Entity.BodyJointTarget .= Address.makeFromString "^/Car"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/Wheel {relation}")
                 Entity.CollideConnected .= false] world |> ignore
            if raceCourse.GetSelected world then // Mutation is required to modify properties of the body joint.
                match raceCourse.GetCarWheelJoint world with
                | Some wheelJoint ->
                    let acceleration = raceCourse.GetCarAcceleration world
                    wheelJoint.MotorSpeed <- float32 (sign acceleration) * Math.SmoothStep(0f, carMaxSpeed, abs acceleration)
                    wheelJoint.MotorEnabled <- abs wheelJoint.MotorSpeed >= carMaxSpeed * 0.06f
                | None -> ()
        
        // process car input
        if raceCourse.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                raceCourse.CarAcceleration.Map (fun a -> min (a + 2.0f * world.ClockTime) 1f) world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then
                raceCourse.CarAcceleration.Map (fun a -> max (a - 2.0f * world.ClockTime) -1f) world
            elif World.isKeyboardKeyPressed KeyboardKey.Down world then
                raceCourse.SetCarAcceleration 0f world
            else raceCourse.CarAcceleration.Map (fun a -> a - float32 (sign a) * 2.0f * world.ClockTime) world

        // declare teeter board
        let (teeter, _) =
            World.doBox2d "Teeter"
                [Entity.Position .= v3 140f 1f 0f * objectScale
                 Entity.Size .= v3 20f 0.5f 0f * objectScale
                 Entity.StaticImage .= Assets.Default.Paddle
                 Entity.Substance .= Density 1f
                 Entity.CollisionDetection .= Continuous] world
        World.doBodyJoint2d "Teeter joint"
            [Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                World.applyBodyAngularImpulse (v3 0f 0f 100f) teeter world
                RevoluteJoint (a, b, b.Position, b.Position, true,
                    LimitEnabled = true, LowerLimit = -8.0f * MathF.PI / 180.0f,
                    UpperLimit = 8.0f * MathF.PI / 180.0f) }
             Entity.BodyJointTarget .= Address.makeFromString "^/RaceCourse"
             Entity.BodyJointTarget2Opt .= Some (Address.makeFromString "^/Teeter")
             Entity.CollideConnected .= false] world |> ignore

        // declare bridge
        for i in 0 .. 20 do
            if i < 20 then
                World.doBox2d $"Bridge {i}"
                    [Entity.Size .= v3 2f 0.25f 0f * objectScale
                     Entity.Position .= v3 (161f + 2f * float32 i) -0.125f 0f * objectScale
                     Entity.Friction .= 0.6f
                     Entity.StaticImage .= Assets.Default.Paddle
                     Entity.CollisionDetection .= Continuous
                     Entity.Substance .= Density 1f] world |> ignore
            World.doBodyJoint2d $"Bridge {i} Link"
                [Entity.BodyJoint .= TwoBodyJoint2d {
                    CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                        let p =
                            if i < 20
                            then b.Position - toPhysicsV2 (v3 objectScale 0f 0f)
                            else a.Position + toPhysicsV2 (v3 objectScale 0f 0f)
                        RevoluteJoint (a, b, p, p, true) }
                 Entity.BodyJointTarget .= Address.makeFromString (if i = 0 then "^/RaceCourse" else $"^/Bridge {i-1}")
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString (if i < 20 then $"^/Bridge {i}" else "^/RaceCourse"))] world |> ignore

        // declare boxes
        for i in 0 .. 2 do
            World.doBox2d $"Box {i}"
                [Entity.Position .= v3 220f (0.5f + float32 i) 0f * objectScale
                 Entity.Size .= v3Dup objectScale
                 Entity.Substance .= Density 1f] world |> ignore

        // switch scene button
        if World.doButton "Switch Scene"
            [Entity.Position .= v3 230f -140f 0f
             Entity.Text .= "Switch Scene"
             Entity.Elevation .= 1f] world then
            Game.SetDesiredScreen (Desire Simulants.SandBox) world

        // end scene declaration
        World.endGroup world

        // process car camera
        // menu offset (X = 60) + car lookahead (X = 40) + make objects spawn above ground (Y = 60)
        if raceCourse.GetSelected world then
            let carPosition = (car.GetPosition world).V2 + v2 100f 60f
            World.setEye2dCenter carPosition world