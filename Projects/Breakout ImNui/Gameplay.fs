namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quitting
    | Quit

// the user-controlled paddle.
type Paddle =
    { Origin : Vector3
      Size : Vector3 }

    static member initial =
        { Origin = v3 0.0f -160.0f 0.0f
          Size = v3 64.0f 16.0f 0.0f }

// the brick-breaking ball.
type Ball =
    { Origin : Vector3
      Size : Vector3
      Speed : single
      Direction : Vector3 }

    static member initial =
        { Origin = v3 0.0f 48.0f 0.0f
          Size = v3 8.0f 8.0f 0.0f
          Speed = 150.0f
          Direction = (v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized }

// the bricks to break out of.
type Brick =
    { Position : Vector3
      Size : Vector3
      Color : Color }

    static member make position =
        { Position = position
          Size = v3 64.0f 16.0f 0.0f
          Color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f }

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Paddle : Paddle
      Ball : Ball
      Bricks : Map<string, Brick>
      Score : int
      Lives : int }

    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quitting
          Paddle = Paddle.initial
          Ball = Ball.initial
          Bricks = Map.empty
          Score = 0
          Lives = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        let bricks =
            Map.ofList
                [for i in 0 .. dec 5 do
                    for j in 0 .. dec 6 do
                        (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))]
        { Gameplay.empty with
            GameplayState = Playing
            Bricks = bricks
            Lives = 5 }

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the behavior of our gameplay
    override this.Run (gameplay, screen, world) =

        // declare scene group while screen is selected
        let (gameplay, world) =
            if screen.GetSelected world then
                let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

                // background model
                let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)
                let world = World.doStaticModel "StaticModel" [Entity.Position .= v3 0.0f 0.0f -2.0f; Entity.Rotation @= rotation] world

                // left wall
                let (_, world) =
                    World.doBlock2d "LeftWall"
                        [Entity.Position .= v3 -164.0f 0.0f 0.0f
                         Entity.Size .= v3 8.0f 360.0f 0.0f
                         Entity.Sensor .= true
                         Entity.StaticImage .= Assets.Default.Black] world
                let leftWall = world.RecentEntity

                // right wall
                let (_, world) =
                    World.doBlock2d "RightWall"
                        [Entity.Position .= v3 164.0f 0.0f 0.0f
                         Entity.Size .= v3 8.0f 360.0f 0.0f
                         Entity.Sensor .= true
                         Entity.StaticImage .= Assets.Default.Black] world
                let rightWall = world.RecentEntity

                // top wall
                let (_, world) =
                    World.doBlock2d "TopWall"
                        [Entity.Position .= v3 0.0f 176.0f 0.0f
                         Entity.Size .= v3 320.0f 8.0f 0.0f
                         Entity.Sensor .= true
                         Entity.StaticImage .= Assets.Default.Black] world
                let topWall = world.RecentEntity

                // paddle
                let (_, world) =
                    World.doBlock2d "Paddle"
                        [Entity.Position .= gameplay.Paddle.Origin
                         Entity.Size .= gameplay.Paddle.Size
                         Entity.Sensor .= true
                         Entity.StaticImage .= Assets.Default.Paddle] world
                let paddle = world.RecentEntity
                let paddlePosition = paddle.GetPosition world

                // move paddle while game is playing / playable
                let world =
                    if gameplay.GameplayState = Playing && gameplay.Lives > 0 && gameplay.Bricks.Count > 0 then
                        let paddlePosition = paddle.GetPosition world
                        if World.isKeyboardKeyDown KeyboardKey.Left world then
                            paddle.SetPosition (paddlePosition.MapX (fun x -> max -128.0f (x - 4.0f))) world
                        elif World.isKeyboardKeyDown KeyboardKey.Right world then
                            paddle.SetPosition (paddlePosition.MapX (fun x -> min 128.0f (x + 4.0f))) world
                        else world
                    else world

                // ball
                let (results, world) =
                    World.doBall2d "Ball"
                        [Entity.Position .= gameplay.Ball.Origin
                         Entity.Size .= gameplay.Ball.Size
                         Entity.BodyType .= Dynamic
                         Entity.AngularFactor .= v3Zero
                         Entity.GravityOverride .= Some v3Zero
                         Entity.CollisionDetection .= Continuous (1.0f, 1.0f)
                         Entity.Observable .= true
                         Entity.StaticImage .= Assets.Default.Ball] world
                let ball = world.RecentEntity
                let ballBodyId = ball.GetBodyId world
                let ballPosition = ball.GetPosition world

                // ball life cycle
                let (gameplay, world) =
                    if (ball.GetPosition world).Y < -180.0f then
                        let gameplay = { gameplay with Lives = dec gameplay.Lives }
                        let world = if gameplay.Lives > 0 then ball.SetPosition gameplay.Ball.Origin world else world
                        (gameplay, world)
                    else (gameplay, world)
                let world =
                    if gameplay.Bricks.Count = 0 then
                        World.setBodyLinearVelocity v3Zero ballBodyId world
                    elif ball.GetLinearVelocity world = v3Zero then
                        World.setBodyLinearVelocity ((v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized * gameplay.Ball.Speed) ballBodyId world
                    else world

                // ball collision
                let (gameplay, world) =
                    FQueue.fold (fun (gameplay, world) result ->
                        match result with
                        | BodyPenetration data ->
                            let penetratee = data.BodyShapePenetratee.BodyId.BodySource
                            if penetratee = paddle then

                                // paddle collision
                                let bounce = (ballPosition - paddlePosition).Normalized * gameplay.Ball.Speed
                                let world = World.setBodyLinearVelocity bounce ballBodyId world
                                World.playSound 1.0f Assets.Default.Sound world
                                (gameplay, world)

                            else

                                // brick collision
                                match gameplay.Bricks.TryGetValue penetratee.Name with
                                | (true, brick) ->

                                    let bounce = (ballPosition - brick.Position).Normalized * gameplay.Ball.Speed
                                    let world = World.setBodyLinearVelocity bounce ballBodyId world
                                    let gameplay =
                                        { gameplay with
                                            Score = gameplay.Score + 100
                                            Bricks = Map.remove penetratee.Name gameplay.Bricks }
                                    World.playSound 1.0f Assets.Default.Sound world
                                    (gameplay, world)

                                // wall collision
                                | (false, _) ->
                                    let normal =
                                        if penetratee = leftWall then v3Right
                                        elif penetratee = rightWall then v3Left
                                        elif penetratee = topWall then v3Down
                                        else failwithumf ()
                                    let world =
                                        let velocity = ball.GetLinearVelocity world
                                        let bounce = velocity - 2.0f * Vector3.Dot(velocity, normal) * normal
                                        World.setBodyLinearVelocity bounce ballBodyId world
                                    World.playSound 1.0f Assets.Default.Sound world
                                    (gameplay, world)

                        | _ -> (gameplay, world))
                        (gameplay, world) results

                // bricks
                let world =
                    Seq.fold (fun world (brickName, brick) ->
                        World.doBlock2d brickName
                            [Entity.Position .= brick.Position
                             Entity.Size .= brick.Size
                             Entity.Sensor .= true
                             Entity.Color @= brick.Color
                             Entity.StaticImage .= Assets.Default.Brick] world |> snd)
                        world gameplay.Bricks.Pairs

                // end scene declaration
                let world = World.endGroup world
                (gameplay, world)

            // otherwise no scene
            else (gameplay, world)

        // declare gui group
        let world = World.beginGroup "Gui" [] world

        // declare score
        let world = World.doText "Score" [Entity.Position .= v3 248.0f 136.0f 0.0f; Entity.Text @= "Score: " + string gameplay.Score] world

        // declare lives
        let world = World.doText "Lives" [Entity.Position .= v3 -240.0f 0.0f 0.0f; Entity.Text .= "Lives"] world
        let world =
            List.fold (fun world i ->
                World.doStaticSprite ("Life+" + string i)
                    [Entity.Position .= v3 -240.0f (single (inc i) * -16.0f) 0.0f
                     Entity.Size .= v3 32.0f 8.0f 0.0f
                     Entity.StaticImage .= Assets.Default.Paddle] world) world [0 .. dec gameplay.Lives]

        // declare message
        let messageText = if gameplay.Lives <= 0 then "Game over!" elif gameplay.Bricks.Count = 0 then "You win!" else ""
        let world = World.doText "Message" [Entity.Text @= messageText] world

        // declare quit button
        let (gameplay, world) =
            match World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world with
            | (true, world) -> ({ gameplay with GameplayState = Quitting }, world)
            | (false, world) -> (gameplay, world)

        // end group declaration
        let world = World.endGroup world

        // advance gameplay time
        let gameDelta = world.GameDelta
        let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
        (gameplay, world)

    // this is a semantic fix-up that allows the editor to avoid creating an unused group. This is specific to the
    // ImNui API that is needed to patch a little semantic hole inherent in the immediate-mode programming idiom.
    override this.CreateDefaultGroup (screen, world) = World.createGroup (Some "Gui") screen world