namespace Breakout
open System
open System.Numerics
open Prime
open Nu
open Breakout

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// the bricks to break out of.
type Brick =
    { Position : Vector3
      Size : Vector3
      Color : Color }

    static member make position =
        { Position = position
          Size = v3 64.0f 16.0f 0.0f
          Color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f }

// this extends the Screen API to expose the user-defined properties.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState
        member this.GetBricks world : Map<string, Brick> = this.Get (nameof Screen.Bricks) world
        member this.SetBricks (value : Map<string, Brick>) world = this.Set (nameof Screen.Bricks) value world
        member this.Bricks = lens (nameof Screen.Bricks) this this.GetBricks this.SetBricks
        member this.GetScore world : int = this.Get (nameof Screen.Score) world
        member this.SetScore (value : int) world = this.Set (nameof Screen.Score) value world
        member this.Score = lens (nameof Screen.Score) this this.GetScore this.SetScore
        member this.GetLives world : int = this.Get (nameof Screen.Lives) world
        member this.SetLives (value : int) world = this.Set (nameof Screen.Lives) value world
        member this.Lives = lens (nameof Screen.Lives) this this.GetLives this.SetLives

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define some gameplay-specific constants
    static let PaddleOrigin = v3 0.0f -160.0f 0.0f
    static let BallOrigin = v3 0.0f 48.0f 0.0f
    static let BallSpeed = 150.0f

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Bricks Map.empty
         define Screen.Score 0
         define Screen.Lives 0]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        // process initialization
        if FQueue.contains Select selectionResults then
            let bricks =
                Map.ofList
                    [for i in 0 .. dec 5 do
                        for j in 0 .. dec 6 do
                            (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))]
            Simulants.Gameplay.SetBricks bricks world
            Simulants.Gameplay.SetScore 0 world
            Simulants.Gameplay.SetLives 5 world
            Simulants.Gameplay.SetGameplayState Playing world

        // declare scene group
        World.beginGroup "Scene" [] world

        // declare background model
        //let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, world.UpdateTime % 360L |> single |> Math.DegreesToRadians)
        //World.doStaticModel "StaticModel" [Entity.Scale .= v3Dup 0.5f; Entity.Rotation @= rotation] world

        // declare walls
        let (leftWallBodyId, _) =
            World.doBlock2d "LeftWall"
                [Entity.Position .= v3 -164.0f 0.0f 0.0f
                 Entity.Size .= v3 8.0f 360.0f 0.0f
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Black] world
        let (rightWallBodyId, _) =
            World.doBlock2d "RightWall"
                [Entity.Position .= v3 164.0f 0.0f 0.0f
                 Entity.Size .= v3 8.0f 360.0f 0.0f
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Black] world
        let (topWallBodyId, _) =
            World.doBlock2d "TopWall"
                [Entity.Position .= v3 0.0f 176.0f 0.0f
                 Entity.Size .= v3 320.0f 8.0f 0.0f
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Black] world

        // declare paddle
        let (paddleBodyId, _) =
            World.doBlock2d "Paddle"
                [Entity.Position .= PaddleOrigin
                 Entity.Size .= v3 64.0f 16.0f 0.0f
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Paddle] world
        let paddle = world.DeclaredEntity
        
        let (chainBody, _) =
            World.doBox2d "Chain"
                [Entity.BodyType .= BodyType.Dynamic] world

        // process paddle movement
        if  world.Advancing &&
            screen.GetGameplayState world = Playing &&
            screen.GetLives world > 0 &&
            (screen.GetBricks world).Count > 0 then
            let paddlePosition = paddle.GetPosition world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                paddle.SetPosition (paddlePosition.MapX (fun x -> max -128.0f (x - 4.0f))) world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then
                paddle.SetPosition (paddlePosition.MapX (fun x -> min 128.0f (x + 4.0f))) world

        // declare ball
        let (ballBodyId, ballResults) =
            World.doBall2d "Ball"
                [Entity.Position .= BallOrigin
                 Entity.Size .= v3 8.0f 8.0f 0.0f
                 Entity.BodyType .= Dynamic
                 Entity.AngularFactor .= v3Zero
                 Entity.GravityOverride .= Some v3Zero
                 Entity.CollisionDetection .= Continuous
                 Entity.StaticImage .= Assets.Default.Ball] world
        let ball = world.DeclaredEntity

        // process ball life cycle
        if (ball.GetPosition world).Y < -180.0f then
            screen.Lives.Map dec world
            if screen.GetLives world > 0 then ball.SetPosition (v3 0.0f 48.0f 0.0f) world
        if (screen.GetBricks world).Count = 0 then
            World.setBodyLinearVelocity v3Zero ballBodyId world
        elif ball.GetLinearVelocity world = v3Zero then
            World.setBodyLinearVelocity ((v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized * BallSpeed) ballBodyId world

        // process ball collision
        for result in ballResults do
            match result with
            | BodyPenetrationData penetration ->
                let penetrateeId = penetration.BodyShapePenetratee.BodyId
                if penetrateeId = paddleBodyId then

                    // paddle collision
                    let bounce = (ball.GetPosition world - paddle.GetPosition world).Normalized * BallSpeed
                    World.setBodyLinearVelocity bounce ballBodyId world
                    World.playSound 1.0f Assets.Default.Sound world
                elif penetrateeId = chainBody then World.setBodyLinearVelocity -penetration.Normal ballBodyId world
                else

                    // brick collision
                    match (screen.GetBricks world).TryGetValue penetrateeId.BodySource.Name with
                    | (true, brick) ->
                        let bounce = (ball.GetPosition world - brick.Position).Normalized * BallSpeed
                        World.setBodyLinearVelocity bounce ballBodyId world
                        screen.Score.Map ((+) 100) world
                        screen.Bricks.Map (Map.remove penetrateeId.BodySource.Name) world
                        World.playSound 1.0f Assets.Default.Sound world

                    // wall collision
                    | (false, _) ->
                        let normal =
                            if penetrateeId = leftWallBodyId then v3Right
                            elif penetrateeId = rightWallBodyId then v3Left
                            elif penetrateeId = topWallBodyId then v3Down
                            else failwithumf ()
                        let velocity = ball.GetLinearVelocity world
                        let bounce = velocity - 2.0f * Vector3.Dot (velocity, normal) * normal
                        World.setBodyLinearVelocity bounce ballBodyId world
                        World.playSound 1.0f Assets.Default.Sound world

            | _ -> ()

        // declare bricks
        for (brickName, brick) in (screen.GetBricks world).Pairs do
            World.doBlock2d brickName
                [Entity.Position .= brick.Position
                 Entity.Size .= brick.Size
                 Entity.Sensor .= true
                 Entity.Color @= brick.Color
                 Entity.StaticImage .= Assets.Default.Brick] world |> ignore

        // declare score
        let scoreText = "Score: " + string (screen.GetScore world)
        World.doText "Score" [Entity.Position .= v3 248.0f 136.0f 0.0f; Entity.Text @= scoreText] world

        // declare lives
        World.doText "Lives" [Entity.Position .= v3 -240.0f 0.0f 0.0f; Entity.Text .= "Lives"] world
        for i in 0 .. dec (screen.GetLives world) do
            World.doStaticSprite ("Life+" + string i)
                [Entity.Position .= v3 -240.0f (single (inc i) * -16.0f) 0.0f
                 Entity.Size .= v3 32.0f 8.0f 0.0f
                 Entity.StaticImage .= Assets.Default.Paddle] world

        // declare message
        let messageText = if screen.GetLives world <= 0 then "Game over!" elif (screen.GetBricks world).Count = 0 then "You win!" else ""
        World.doText "Message" [Entity.Text @= messageText] world

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        // end scene declaration
        World.endGroup world