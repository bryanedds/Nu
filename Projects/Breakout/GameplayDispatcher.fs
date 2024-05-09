namespace Breakout
open System
open System.Numerics
open Prime
open Nu

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

// this extends the Screen API to expose the Gameplay model as well as the gameplay quit event.
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
            match Gameplay.update gameplay world with
            | Right gameplay -> just gameplay
            | Left gameplay -> withSignal StartQuitting gameplay

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
         | Playing ->
            
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