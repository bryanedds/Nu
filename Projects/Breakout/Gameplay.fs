namespace MyGame
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of gameplay simulation.
    type GameplayState =
        | Commencing
        | Commence
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
              Velocity = (v3 Gen.randomf -2.0f 0.0f).Normalized * 4.0f }

    // the state of a breakout block
    type Block =
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
          Blocks : Map<string, Block>
          Ball : Ball
          Lives : int }

        static member quit =
            { GameplayTime = 0L
              GameplayState = Quit
              Paddle = Paddle.initial
              Ball = Ball.initial
              Blocks = Map.empty
              Lives = 0 }

        static member commencing =
            let blocks =
                Map.ofSeq
                    [|for i in 0 .. dec 5 do
                        for j in 0 .. dec 6 do
                            (Gen.name, Block.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))|]
            { Gameplay.quit with
                GameplayState = Commencing
                Blocks = blocks
                Lives = 3 }

        static member commence =
            { Gameplay.commencing with
                GameplayState = Commence }

    // this is our MMCC message type.
    type GameplayMessage =
        | FinishCommencing
        | StartQuitting
        | FinishQuitting
        | Update
        | TimeUpdate
        interface Message

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, Command> (Gameplay.quit)

        // here we define the screen's property values and event handling
        override this.Definitions (_, _) =
            [Screen.SelectEvent => FinishCommencing
             Screen.DeselectingEvent => FinishQuitting
             Screen.UpdateEvent => Update
             Screen.TimeUpdateEvent => TimeUpdate]

        // here we handle the above messages
        override this.Message (gameplay, message, _, world) =

            match message with
            | FinishCommencing ->
                let gameplay = { gameplay with GameplayState = Commence }
                just gameplay

            | StartQuitting ->
                let gameplay = { gameplay with GameplayState = Quitting }
                just gameplay

            | FinishQuitting ->
                let gameplay = { gameplay with GameplayState = Quit }
                just gameplay

            | Update ->

                match gameplay.GameplayState with
                | Commence ->

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
                            if paddle.Perimeter.Intersects ball.Position
                            then { ball with Velocity = (ball.Position - paddle.Position).Normalized * 4.0f }
                            else ball
                        { gameplay with Ball = ball }

                    // update ball motion against blocks
                    let gameplay =
                        let ball = gameplay.Ball
                        let blocks = Map.filter (fun _ (block : Block) -> block.Perimeter.Intersects ball.Position) gameplay.Blocks
                        let ball =
                            if Map.notEmpty blocks then
                                let block = Seq.head blocks.Values
                                { ball with Velocity = (ball.Position - block.Position).Normalized * 4.0f }
                            else ball
                        let blocks = Map.removeMany blocks.Keys gameplay.Blocks
                        { gameplay with Ball = ball; Blocks = blocks }

                    // update ball death
                    let gameplay =
                        if gameplay.Ball.Position.Y < -180.0f then
                            let gameplay = { gameplay with Lives = dec gameplay.Lives }
                            let gameplay = if gameplay.Lives > 0 then { gameplay with Ball = Ball.initial } else gameplay
                            gameplay
                        else gameplay

                    // check game ending
                    let gameplay =
                        if gameplay.Blocks.Count = 0 || gameplay.Lives = 0
                        then { gameplay with GameplayState = Quitting }
                        else gameplay
                    just gameplay

                | _ -> just gameplay

            | TimeUpdate ->
                let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + (let d = world.GameDelta in d.Updates) }
                just gameplay

        // here we describe the content of the game including the hud, the scene, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while gameplay commences or quitting
             match gameplay.GameplayState with
             | Commence | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                    [// paddle
                     Content.animatedSprite "Paddle"
                        [Entity.Position := gameplay.Paddle.Position
                         Entity.Size == gameplay.Paddle.Size]

                     // left wall
                     Content.staticSprite "LeftWall"
                        [Entity.Position == v3 -164.0f 0.0f 0.0f
                         Entity.Size == v3 8.0f 360.0f 0.0f]

                     // right wall
                     Content.staticSprite "RightWall"
                        [Entity.Position == v3 164.0f 0.0f 0.0f
                         Entity.Size == v3 8.0f 360.0f 0.0f]

                     // top wall
                     Content.staticSprite "TopWall"
                        [Entity.Position == v3 0.0f 176.0f 0.0f
                         Entity.Size == v3 320.0f 8.0f 0.0f]

                     // ball
                     Content.staticSprite "Ball"
                        [Entity.Position := gameplay.Ball.Position
                         Entity.Size == gameplay.Ball.Size]

                     // blocks
                     for (blockId, block) in gameplay.Blocks.Pairs do
                        Content.staticSprite blockId
                            [Entity.Position == block.Position
                             Entity.Size == block.Size
                             Entity.Color == block.Color]

                     // lives
                     Content.text "Lives"
                        [Entity.Position == v3 -200.0f 0.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Lives"]
                     for i in 0 .. dec gameplay.Lives do
                        Content.animatedSprite ("Life" + string i)
                            [Entity.Position == v3 -200.0f (single (inc i) * -16.0f) 0.0f
                             Entity.Size == v3 16.0f 8.0f 0.0f]

                     // message
                     Content.text "Message"
                        [Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text := if gameplay.Lives = 0 then "Game Over!" elif gameplay.Blocks.Count = 0 then "You win!" else ""]]

             // no scene group otherwise
             | Commencing | Quit -> ()]