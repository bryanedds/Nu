namespace MyGame
open Prime
open Nu
open Nu.Declarative

type MyGame =
    | Splash
    | Title
    | Credits
    | Gameplay of Gameplay

type MyGameMessage =
    | ShowTitle
    | ShowCredits
    | ShowGameplay

type MyGameCommand =
    | Exit

// this is the game dispatcher that is customized for our game. In here, we create screens and bind
// them up with events and properties.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame, MyGameMessage, MyGameCommand> (Splash)

    // here we channel from events to signals
    override this.Channel (_, _) =
        [Simulants.Title.Gui.Credits.ClickEvent => msg ShowCredits
         Simulants.Title.Gui.Play.ClickEvent => msg ShowGameplay
         Simulants.Title.Gui.Exit.ClickEvent => cmd Exit
         Simulants.Credits.Gui.Back.ClickEvent => msg ShowTitle]

    // here we back-bind gameplay's model to game's model as well as forward bind the desired
    // screen based on the state of the game (or None if splashing).
    override this.Initializers (myGame, _) =
        [Simulants.Game.Model<MyGame> () <== Simulants.Gameplay.Screen.Model<Gameplay> () --> Gameplay
         Simulants.Game.DesiredScreenOpt <== myGame --> fun myGame ->
            match myGame with
            | Splash -> None
            | Title -> Some Simulants.Title.Screen
            | Credits -> Some Simulants.Credits.Screen
            | Gameplay gameplay ->
                match gameplay with
                | Playing -> Some Simulants.Gameplay.Screen
                | Quitting -> Some Simulants.Title.Screen]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just (Gameplay Playing)

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit -> just (World.exit world)

    // here we describe the content of the game including all of its screens.
    override this.Content (myGame, _) =
        [Content.screen Simulants.Splash.Screen.Name (Nu.Splash (Constants.Dissolve.Default, Constants.Splash.Default, None, Simulants.Title.Screen)) [] []
         Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup"
         Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup"
         Content.screen<MyGameplayDispatcher> Simulants.Gameplay.Screen.Name (Dissolve (Constants.Dissolve.Default, None))
            [Screen.Model<Gameplay> () <== myGame --> function Gameplay gameplay -> gameplay | _ -> Quitting]
            []]