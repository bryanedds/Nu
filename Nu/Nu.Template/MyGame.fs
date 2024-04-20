namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type MyGame =
    | Splash
    | Title
    | Credits
    | Gameplay

// this is our top-level MMCC message type.
type MyGameMessage =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    interface Message
        
// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type MyGameCommand =
    | Exit
    interface Command

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetMyGame world = this.GetModelGeneric<MyGame> world
        member this.SetMyGame value world = this.SetModelGeneric<MyGame> value world
        member this.MyGame = this.ModelGeneric<MyGame> ()

// this is the dispatcher customizes the top-level behavior of our game. In here, we create screens as content and bind
// them up with events and properties.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame, MyGameMessage, MyGameCommand> (Splash)

    // here we define the game's properties and event handling
    override this.Definitions (myGame, _) =
        [Game.DesiredScreen :=
            match myGame with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
            Simulants.Splash.DeselectingEvent => ShowTitle
            Simulants.TitleCredits.ClickEvent => ShowCredits
            Simulants.TitlePlay.ClickEvent => ShowGameplay
            Simulants.TitleExit.ClickEvent => Exit
            Simulants.CreditsBack.ClickEvent => ShowTitle
            Simulants.Gameplay.QuitEvent => ShowTitle]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just Gameplay

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    // here we describe the content of the game, including all of its screens.
    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]