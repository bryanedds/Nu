namespace Breakout
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// the state of our breakout paddle
type Paddle =
    { Position : Vector3
      Size : Vector3 }

    static member initial =
        { Position = v3 0.0f -160.0f 0.0f
          Size = v3 64f 16.0f 0.0f }

    member this.Perimeter =
        box3 (this.Position - this.Size * 0.5f) this.Size

// the state of our breakout ball
type Ball =
    { Position : Vector3
      Size : Vector3
      Velocity : Vector3 }

    static member initial =
        { Position = v3 0.0f 48.0f 0.0f
          Size = v3 8.0f 8.0f 0.0f
          Velocity = (v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized * 4.0f }

// the state of a breakout brick
type Brick =
    { Position : Vector3
      Size : Vector3
      Color : Color }

    member this.Perimeter =
        box3 (this.Position - this.Size * 0.5f) this.Size

    static member make position =
        { Position = position
          Size = v3 64f 16.0f 0.0f
          Color = Color (Gen.randomf1 0.5f + 0.5f, Gen.randomf1 0.5f + 0.5f, Gen.randomf1 0.5f + 0.5f, 1.0f) }

// this is our MMCC model type representing gameplay.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Paddle : Paddle
      Bricks : Map<string, Brick>
      Ball : Ball
      Lives : int }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit
          Paddle = Paddle.initial
          Ball = Ball.initial
          Bricks = Map.empty
          Lives = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        let bricks =
            Map.ofSeq
                [|for i in 0 .. dec 5 do
                    for j in 0 .. dec 6 do
                        (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))|]
        { Gameplay.empty with
            GameplayState = Playing
            Bricks = bricks
            Lives = 3 }

    // this updates the gameplay model every frame while advancing.
    static member update gameplay world =

        // update scene only while playing
        match gameplay.GameplayState with
        | Playing ->

            // ...and while living and bricks are present
            if gameplay.Lives > 0 && gameplay.Bricks.Count > 0 then

                // update paddle motion
                let gameplay =
                    if World.isKeyboardKeyDown KeyboardKey.Left world then
                        let paddle = gameplay.Paddle
                        let paddle = { paddle with Position = paddle.Position.MapX (fun x -> max -128.0f (x - 4.0f)) }
                        { gameplay with Paddle = paddle }
                    elif World.isKeyboardKeyDown KeyboardKey.Right world then
                        let paddle = gameplay.Paddle
                        let paddle = { paddle with Position = paddle.Position.MapX (fun x -> min 128.0f (x + 4.0f)) }
                        { gameplay with Paddle = paddle }
                    else gameplay

                // update ball motion against walls
                let gameplay =
                    let ball = gameplay.Ball
                    let ball = { ball with Position = ball.Position + ball.Velocity }
                    let ball =
                        if ball.Position.X <= -160.0f || ball.Position.X >= 160.0f
                        then { ball with Velocity = ball.Velocity.MapX negate }
                        else ball
                    let ball =
                        if ball.Position.Y >= 172.0f
                        then { ball with Velocity = ball.Velocity.MapY negate }
                        else ball
                    { gameplay with Ball = ball }

                // update ball motion against paddle
                let gameplay =
                    let paddle = gameplay.Paddle
                    let ball = gameplay.Ball
                    let ball =
                        let perimeter = paddle.Perimeter
                        if perimeter.Intersects ball.Position
                        then { ball with Velocity = (ball.Position - paddle.Position).Normalized * 4.0f }
                        else ball
                    { gameplay with Ball = ball }

                // update ball motion against bricks
                let gameplay =
                    let ball = gameplay.Ball
                    let bricks =
                        Map.filter (fun _ (brick : Brick) ->
                            let perimeter = brick.Perimeter
                            perimeter.Intersects ball.Position)
                            gameplay.Bricks
                    let ball =
                        if Map.notEmpty bricks then
                            let brick = Seq.head bricks.Values
                            { ball with Velocity = (ball.Position - brick.Position).Normalized * 4.0f }
                        else ball
                    let bricks = Map.removeMany bricks.Keys gameplay.Bricks
                    { gameplay with Ball = ball; Bricks = bricks }

                // update ball death
                let gameplay =
                    if gameplay.Ball.Position.Y < -180.0f then
                        let gameplay = { gameplay with Lives = dec gameplay.Lives }
                        let gameplay = if gameplay.Lives > 0 then { gameplay with Ball = Ball.initial } else gameplay
                        gameplay
                    else gameplay

                // gameplay still going
                Right gameplay

            // ...otherwise gameplay over
            else Left gameplay

        // nothing to do
        | _ -> Right gameplay