namespace Breakout
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

type Paddle =
    { Position : Vector3
      Size : Vector3 }

    static member initial =
        { Position = v3 0.0f -160.0f 0.0f
          Size = v3 64.0f 16.0f 0.0f }

type Ball =
    { Position : Vector3
      Size : Vector3
      Velocity : Vector3 }

    static member initial =
        { Position = v3 0.0f 48.0f 0.0f
          Size = v3 8.0f 8.0f 0.0f
          Velocity = (v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized * 4.0f }

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
// if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
// you could use `GameplayTime : single`.
type [<SymbolicExpansion>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Paddle : Paddle
      Ball : Ball
      Bricks : Map<string, Brick>
      Lives : int
      Score : int }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit
          Paddle = Paddle.initial
          Ball = Ball.initial
          Bricks = Map.empty
          Lives = 3
          Score = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        let bricks =
            Map.ofSeq
                [for i in 0 .. dec 5 do
                    for j in 0 .. dec 6 do
                        (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))]
        { Gameplay.empty with
            GameplayState = Playing
            Bricks = bricks }

    static member update gameplay world =
        match GameplayState.Playing with
        | Playing when gameplay.Lives > 0 && gameplay.Bricks.Count > 0 ->

            // update paddle
            let gameplay =
                let paddle = gameplay.Paddle
                let paddle =
                    if World.isKeyboardKeyDown KeyboardKey.Left world then
                        { paddle with Position = paddle.Position.MapX (fun x -> max -128.0f (x - 4.0f)) }
                    elif World.isKeyboardKeyDown KeyboardKey.Right world then
                        { paddle with Position = paddle.Position.MapX (fun x -> min 128.0f (x + 4.0f)) }
                    else paddle
                { gameplay with Paddle = paddle }

            // update ball motion against walls
            let gameplay =
                let ball = gameplay.Ball
                let ball = { ball with Position = ball.Position + ball.Velocity }
                let ball =
                    if ball.Position.X <= -160.0f || ball.Position.X >= 160.0f then 
                        World.playSound 0.5f Assets.Default.Sound world
                        { ball with Velocity = ball.Velocity.MapX negate }
                    else ball
                let ball =
                    if ball.Position.Y >= 172.0f then
                        World.playSound 0.5f Assets.Default.Sound world
                        { ball with Velocity = ball.Velocity.MapY negate }
                    else ball
                { gameplay with Ball = ball }

            // update ball motion against paddle
            let gameplay =
                let paddle = gameplay.Paddle
                let ball = gameplay.Ball
                let ball =
                    let perimeter = box3 (paddle.Position - paddle.Size * 0.5f) paddle.Size
                    if perimeter.Intersects ball.Position then
                        World.playSound 0.5f Assets.Default.Sound world
                        { ball with Velocity = (ball.Position - paddle.Position).Normalized * 4.0f }
                    else ball
                { gameplay with Ball = ball }

            // update ball motion against bricks
            let gameplay =
                let ball = gameplay.Ball
                let bricks =
                    Map.filter (fun _ (brick : Brick) ->
                        let perimeter = box3 (brick.Position - brick.Size * 0.5f) brick.Size
                        perimeter.Intersects ball.Position)
                        gameplay.Bricks
                let ball =
                    if Map.notEmpty bricks then
                        World.playSound 0.5f Assets.Default.Sound world
                        let brick = Seq.head bricks.Values
                        { ball with Velocity = (ball.Position - brick.Position).Normalized * 4.0f }
                    else ball
                let scoring = Map.count bricks * 100
                let bricks = Map.removeMany bricks.Keys gameplay.Bricks
                let gameplay = { gameplay with Ball = ball; Bricks = bricks; Score = gameplay.Score + scoring }
                gameplay

            // update ball death
            let gameplay =
                if gameplay.Ball.Position.Y < -180.0f then
                    let gameplay = { gameplay with Lives = dec gameplay.Lives }
                    let gameplay = if gameplay.Lives > 0 then { gameplay with Ball = Ball.initial } else gameplay
                    gameplay
                else gameplay

            // fin
            gameplay

        | Playing | Quit -> gameplay

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.Selected world
        then Gameplay.initial
        else Gameplay.empty

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
            let gameplay = Gameplay.update gameplay world
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
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
                 Entity.Text == "Lives"]
             for i in 0 .. dec gameplay.Lives do
                Content.staticSprite ("Life+" + string i)
                    [Entity.Position == v3 -240.0f (single (inc i) * -16.0f) 0.0f
                     Entity.Size == v3 32.0f 8.0f 0.0f
                     Entity.StaticImage == Assets.Default.Paddle]

             // score
             Content.text "Score"
                [Entity.Position == v3 248.0f 136.0f 0.0f
                 Entity.Text := "Score: " + string gameplay.Score]

             // message
             Content.text "Message"
                [Entity.Text := if gameplay.Lives = 0 then "Game Over!" elif gameplay.Bricks.Count = 0 then "You win!" else ""]
             
             // quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]

         // the scene group while playing
         match gameplay.GameplayState with
         | Playing ->
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []
                [Content.staticModel "StaticModel"
                    [Entity.Position == v3 0.0f 1.0f 0.0f
                     Entity.Rotation := Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)]
                 Content.staticSprite "Paddle"
                    [Entity.Position := gameplay.Paddle.Position
                     Entity.Size == gameplay.Paddle.Size
                     Entity.StaticImage == Assets.Default.Paddle]
                 Content.staticSprite "Ball"
                    [Entity.Position := gameplay.Ball.Position
                     Entity.Size == gameplay.Ball.Size
                     Entity.StaticImage == Assets.Default.Ball]
                 Content.staticSprite "LeftWall"
                    [Entity.Position == v3 -164.0f 0.0f 0.0f
                     Entity.Size == v3 8.0f 360.0f 0.0f
                     Entity.StaticImage == Assets.Default.Black]
                 Content.staticSprite "RightWall"
                    [Entity.Position == v3 164.0f 0.0f 0.0f
                     Entity.Size == v3 8.0f 360.0f 0.0f
                     Entity.StaticImage == Assets.Default.Black]
                 Content.staticSprite "TopWall"
                    [Entity.Position == v3 0.0f 176.0f 0.0f
                     Entity.Size == v3 320.0f 8.0f 0.0f
                     Entity.StaticImage == Assets.Default.Black]
                 for (brickId, brick) in gameplay.Bricks.Pairs do
                    Content.staticSprite brickId
                        [Entity.Position == brick.Position
                         Entity.Size == brick.Size
                         Entity.Color == brick.Color
                         Entity.StaticImage == Assets.Default.Brick]]

         // no scene group otherwise
         | Quit -> ()]