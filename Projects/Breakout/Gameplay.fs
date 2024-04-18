namespace Breakout
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of gameplay simulation.
    type GameplayState =
        | Playing
        | Quitting
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

        static member empty =
            { GameplayTime = 0L
              GameplayState = Quit
              Paddle = Paddle.initial
              Ball = Ball.initial
              Bricks = Map.empty
              Lives = 0 }

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

    // this is our MMCC message type.
    type GameplayMessage =
        | StartPlaying
        | FinishQuitting
        | Update
        | TimeUpdate
        interface Message

    // this is our MMCC command type.
    type GameplayCommand =
        | StartQuitting
        interface Command

    // this extends the Screen API to expose the Gameplay model as well as the gameplay quit event.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

        // here we define the screen's property values and event handling
        override this.Definitions (_, _) =
            [Screen.SelectEvent => StartPlaying
             Screen.DeselectingEvent => FinishQuitting
             Screen.UpdateEvent => Update
             Screen.TimeUpdateEvent => TimeUpdate]

        // here we handle the above messages
        override this.Message (gameplay, message, _, world) =

            match message with
            | StartPlaying ->
                let gameplay = Gameplay.initial
                just gameplay

            | FinishQuitting ->
                let gameplay = Gameplay.empty
                just gameplay

            | Update ->

                // update scene only while playing
                match gameplay.GameplayState with
                | Playing ->

                    // ...and while living and bticks are present
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
                        just gameplay

                    // ...otherwise we should start quitting game
                    else withSignal StartQuitting gameplay

                // nothing to do
                | _ -> just gameplay

            | TimeUpdate ->
                let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + (let d = world.GameDelta in d.Updates) }
                just gameplay

        // here we handle the above commands
        override this.Command (_, command, screen, world) =

            match command with
            | StartQuitting ->
                let world = World.publish () screen.QuitEvent screen world
                just world

        // here we describe the content of the game including the hud, the scene, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// lives
                 Content.text "Lives"
                    [Entity.Position == v3 -240.0f 0.0f 0.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.Text == "Lives"]
                 for i in 0 .. dec gameplay.Lives do
                    Content.staticSprite ("Life+" + string i)
                        [Entity.Position == v3 -240.0f (single (inc i) * -16.0f) 0.0f
                         Entity.Size == v3 32.0f 8.0f 0.0f
                         Entity.StaticImage == Assets.Default.Brick]

                 // message
                 Content.text "Message"
                    [Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.Text := if gameplay.Lives = 0 then "Game Over!" elif gameplay.Bricks.Count = 0 then "You win!" else ""]

                 // quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while playing
             match gameplay.GameplayState with
             | Playing | Quitting ->
                
                // loads scene from file edited in Gaia
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                    [// paddle
                     Content.staticSprite "Paddle"
                        [Entity.Position := gameplay.Paddle.Position
                         Entity.Size == gameplay.Paddle.Size
                         Entity.StaticImage == Assets.Default.Brick]

                     // left wall
                     Content.staticSprite "LeftWall"
                        [Entity.Position == v3 -164.0f 0.0f 0.0f
                         Entity.Size == v3 8.0f 360.0f 0.0f
                         Entity.StaticImage == Assets.Default.Black]

                     // right wall
                     Content.staticSprite "RightWall"
                        [Entity.Position == v3 164.0f 0.0f 0.0f
                         Entity.Size == v3 8.0f 360.0f 0.0f
                         Entity.StaticImage == Assets.Default.Black]

                     // top wall
                     Content.staticSprite "TopWall"
                        [Entity.Position == v3 0.0f 176.0f 0.0f
                         Entity.Size == v3 320.0f 8.0f 0.0f
                         Entity.StaticImage == Assets.Default.Black]

                     // ball
                     Content.staticSprite "Ball"
                        [Entity.Position := gameplay.Ball.Position
                         Entity.Size == gameplay.Ball.Size
                         Entity.StaticImage == Assets.Default.Ball]

                     // bricks
                     for (brickId, brick) in gameplay.Bricks.Pairs do
                        Content.staticSprite brickId
                            [Entity.Position == brick.Position
                             Entity.Size == brick.Size
                             Entity.Color == brick.Color
                             Entity.StaticImage == Assets.Default.Brick]]

             // no scene group otherwise
             | Quit -> ()]