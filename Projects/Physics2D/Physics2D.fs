namespace Physics2D
open System
open System.Numerics
open Prime
open Nu

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState = Enclosure = 0 | Racecourse = 1

// this extends the Game API to expose GameState as a property.
[<AutoOpen>]
module Physics2DExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState
        member this.GetCarAcceleration world : float32 = this.Get (nameof Game.CarAcceleration) world
        member this.SetCarAcceleration (value : float32) world = this.Set (nameof Game.CarAcceleration) value world
        member this.CarAcceleration = lens (nameof Game.CarAcceleration) this this.GetCarAcceleration this.SetCarAcceleration
        
// this is the dispatcher that customizes the top-level behavior of our game.
type Physics2DDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState GameState.Enclosure
         define Game.CarAcceleration 0f]

    // here we define the game's top-level behavior
    override this.Process (game, world) =
        
        // declare Enclosure screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ =
            World.beginScreen<DemoScreenDispatcher> (nameof GameState.Enclosure) (game.GetGameState world = GameState.Enclosure) behavior
                [Screen.CameraPositionDefault .= CameraAbsolute (v2 60f 0f)] world
        World.beginGroup Simulants.SceneGroup [] world

        // define border
        let _ =
            World.doBlock2d Simulants.BorderEntity // A block uses static physics by default - it does not react to forces or collisions.
                [Entity.Size .= v3 500f 350f 0f
                 Entity.BodyShape .= ContourShape // The body shape handles collisions and is independent of how it's displayed.
                    { Links = // A contour shape, unlike other shapes, is hollow.
                        [|v3 -0.5f 0.5f 0f // Zero is the entity's center, one is the entity's size in positive direction.
                          v3 0.5f 0.5f 0f
                          v3 0.5f -0.5f 0f
                          v3 -0.5f -0.5f 0f|]
                      Closed = true // The last point connects to the first point.
                      // There can be multiple shapes per body, TransformOpt and PropertiesOpt 
                      TransformOpt = None
                      PropertiesOpt = None }
                 // Continuous collision detection adds additional checks between frame positions
                 // against high velocity objects tunneling through thin borders.
                 Entity.CollisionDetection .= Continuous
                 // Collision categories is a binary mask, defaulting to "1" (units place).
                 // The border is set to be in a different category, "10" (twos place)
                 // because we define fans later to not collide with the border.
                 // Meanwhile, unless we change the collision mask (Entity.CollisionMask),
                 // all entites default to collide with "*" (i.e. all collision categories).
                 Entity.CollisionCategories .= "10"
                 Entity.Elevation .= -1f // Draw order of the same elevation prioritizes entities with lower vertical position for 2D games.
                 Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world

        // define agent
        let (agentBody, _) =
            World.doCharacter2d "Agent"
                [Entity.Restitution .= 0.333f // bounciness
                 Entity.GravityOverride .= None // characters have 3x gravity by default, get rid of it
                 Entity.Friction .= 0.1f
                 ] world
        
        // Keyboard controls for agent
        if world.ContextScreen.GetSelected world then
            let agentForce = 200f
            let agentTorque = 1f
            if World.isKeyboardKeyDown KeyboardKey.A world then
                World.applyBodyForce (v3 -1f 0f 0f * agentForce) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.D world then
                World.applyBodyForce (v3 1f 0f 0f * agentForce) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.W world then
                // Fly up despite gravity
                World.applyBodyForce (v3 0f 1f 0f * agentForce - World.getGravity2d world) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.S world then
                // Glide down despite gravity
                World.applyBodyForce (v3 0f -1f 0f * agentForce - World.getGravity2d world) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.Q world then
                World.applyBodyTorque (v3 1f 0f 0f * agentTorque) agentBody world
            if World.isKeyboardKeyDown KeyboardKey.E world then
                World.applyBodyTorque (v3 -1f 0f 0f * agentTorque) agentBody world
        
        if World.doButton Simulants.BackEntity [] world && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world
        
        // declare Racecourse screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ =
            World.beginScreen<DemoScreenDispatcher> (nameof GameState.Racecourse) (game.GetGameState world = GameState.Racecourse) behavior
                [Screen.CameraPositionDefault .= CameraTracking (Relation.makeFromString "Car")] world
        World.beginGroup Simulants.SceneGroup [] world

        // define racecourse
        let racecourse =
            [|v2 -20f 5f
              v2 -20f 0f
              v2 20f 0f
              v2 25f 0.25f
              v2 30f 1f
              v2 35f 4f
              v2 40f 0f
              v2 45f 0f
              v2 50f -1f
              v2 55f -2f
              v2 60f -2f
              v2 65f -1.25f
              v2 70f 0f
              v2 75f 0.3f
              v2 80f 1.5f
              v2 85f 3.5f
              v2 90f 0f
              v2 95f -0.5f
              v2 100f -1f
              v2 105f -2f
              v2 110f -2.5f
              v2 115f -1.3f
              v2 120f 0f
              v2 160f 0f
              v2 159f -10f
              v2 201f -10f
              v2 200f 0f
              v2 240f 0f
              v2 250f 5f
              v2 250f -10f
              v2 270f -10f
              v2 270f 0f
              v2 310f 0f
              v2 310f 5f|] |> Array.map (fun p -> p.V3 * Constants.Engine.Entity2dSizeDefault)
        let _ =
            World.doBlock2d "Racecourse"
                [Entity.Size .= v3 1f 1f 0f
                 Entity.BodyShape .= ContourShape { Links = racecourse; Closed = false; TransformOpt = None; PropertiesOpt = None }]
                world
        for (p1, p2) in Array.pairwise racecourse do
            World.doStaticSprite $"Racecourse {p1} -> {p2}"
                [Entity.Position .= (p1 + p2) / 2f
                 Entity.Size .= v3 ((p2 - p1).Magnitude / 2f) 2f 0f
                 Entity.Rotation .= Quaternion.CreateLookAt2d (p2 - p1).V2
                 Entity.StaticImage .= Assets.Default.Black] world

        // define car
        let carMaxSpeed = 50f
        let carSpawnPosition = v3 0f 30f 0f
        let carPoints = [|
            v3 -2.5f -0.08f 0f
            v3 -2.375f 0.46f 0f
            v3 -0.58f 0.92f 0f
            v3 0.46f 0.92f 0f
            v3 2.5f 0.17f 0f
            v3 2.5f -0.205f 0f
            v3 2.3f -0.33f 0f
            v3 -2.25f -0.35f 0f|]
        let carBottomLeft = carPoints |> Array.reduce (fun a b -> v3 (min a.X b.X) (min a.Y b.Y) 0f)
        let carTopRight = carPoints |> Array.reduce (fun a b -> v3 (max a.X b.X) (max a.Y b.Y) 0f)
        let carSize = carTopRight - carBottomLeft
        let carGetRelativePosition p = (p - carBottomLeft) / carSize - v3Dup 0.5f
        let _ =
            World.doBox2d "Car"
                [Entity.BodyShape .= PointsShape {
                    Points = Array.map carGetRelativePosition carPoints
                    Profile = Convex
                    TransformOpt = None
                    PropertiesOpt = None }
                 Entity.StaticImage .= Assets.Gameplay.Car
                 Entity.Position .= carSpawnPosition
                 Entity.Size .= carSize * Constants.Engine.Entity2dSizeDefault
                 Entity.Substance .= Density 2f
                 ] world
        for (relation, position, density, frequency, friction, maxTorque, motorSpeed) in
            [("Back", v3 -1.709f 0.78f 0f, 0.8f, 5f, Some 0.9f, 20f,
              let acceleration = game.GetCarAcceleration world
              float32 (sign acceleration) * Math.SmoothStep(0f, carMaxSpeed, abs acceleration))
             ("Front", v3 1.54f 0.8f 0f, 1f, 8.5f, None, 10f, 0f)] do
            let _ =
                World.doBall2d $"Wheel {relation}"
                    [Entity.StaticImage .= Assets.Gameplay.Wheel
                     Entity.Position .= carSpawnPosition + carGetRelativePosition position * Constants.Engine.Entity2dSizeDefault
                     Entity.Size .= 0.5f * Constants.Engine.Entity2dSizeDefault
                     Entity.Substance .= Density density
                     match friction with Some f -> Entity.Friction .= f | _ -> ()
                     ] world
            let _ =
                World.doBodyJoint2d $"Wheel {relation} joint"
                    [Entity.BodyJoint .= TwoBodyJoint2d {
                        CreateTwoBodyJoint = fun _ _ car wheel ->
                            nkast.Aether.Physics2D.Dynamics.Joints.WheelJoint (car, wheel, wheel.Position, new _(0f, 1.2f), true,
                                Frequency = frequency, DampingRatio = 0.85f, MaxMotorTorque = maxTorque,
                                MotorSpeed = motorSpeed, MotorEnabled = (abs motorSpeed >= carMaxSpeed * 0.06f)) }
                     Entity.BodyJointTarget .= Relation.makeFromString "^/Car"
                     Entity.BodyJointTarget2Opt .= Some (Relation.makeFromString $"^/Wheel {relation}")

                     ] world
            ()
        
        // Keyboard controls for car
        let isAJustReleased =
            World.doSubscription "SubscribeARelease" Game.KeyboardKeyChangeEvent world
            |> Seq.exists (fun buttonChange -> buttonChange.KeyboardKey = KeyboardKey.A && not buttonChange.Down)
        if World.isKeyboardKeyDown KeyboardKey.A world then
            game.CarAcceleration.Map (fun a -> min (a + 2.0f * world.ClockTime) 1f) world
        elif World.isKeyboardKeyDown KeyboardKey.D world then
            game.CarAcceleration.Map (fun a -> max (a - 2.0f * world.ClockTime) -1f) world
        elif World.isKeyboardKeyPressed KeyboardKey.D world || isAJustReleased then
            game.SetCarAcceleration 0f world
        else game.CarAcceleration.Map (fun a -> a - float32 (sign a) * 2.0f * world.ClockTime) world
        
        if World.doButton Simulants.BackEntity [] world && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world