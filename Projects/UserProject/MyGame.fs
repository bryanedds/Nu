namespace MyGame
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module MyGame =

    // this is our Elm-style model type. It determines what state the game is in. To learn about the
    // Elm-style in Nu, see here - https://vsyncronicity.com/2020/03/01/a-game-engine-in-the-elm-style/
    type Model =
        | Splash
        | Title
        | Credits
        | Gameplay of Gameplay

    // this is our Elm-style message type. It provides a signal to show the various screens.
    type Message =
        | ShowTitle
        | ShowCredits
        | ShowGameplay

    // this is our Elm-style command type. Commands are used instead of messages when explicitly
    // updating the world is involved.
    type Command =
        | Exit

    // this extends the Game API to expose the above model as well as the model bimapped to Gameplay,
    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()
        member this.Gameplay = this.Model.Bimap (function Gameplay gameplay -> Some gameplay | _ -> None) (constant Gameplay) 

    // this is the game dispatcher that is customized for our game. In here, we create screens as
    // content and bind them up with events and properties.
    type MyGameDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Splash)

        // here we channel from gui events to signals
        override this.Channel (_, _) =
            [Simulants.Splash.Screen.DeselectEvent => msg ShowTitle
             Simulants.Title.Gui.Credits.ClickEvent => msg ShowCredits
             Simulants.Title.Gui.Play.ClickEvent => msg ShowGameplay
             Simulants.Title.Gui.Exit.ClickEvent => cmd Exit
             Simulants.Credits.Gui.Back.ClickEvent => msg ShowTitle]

        // here we link the game and gameplay models (two-way bind), then we bind the desired
        // screen based on the state of the game (or None if splashing),
        override this.Initializers (model, game) =
            [game.Gameplay <=> Simulants.Gameplay.Screen.Gameplay
             game.DesiredScreen <== model --> fun model ->
                match model with
                | Splash -> Desire Simulants.Splash.Screen
                | Title -> Desire Simulants.Title.Screen
                | Credits -> Desire Simulants.Credits.Screen
                | Gameplay gameplay ->
                    match gameplay with
                    | Playing -> Desire Simulants.Gameplay.Screen
                    | Quitting -> Desire Simulants.Title.Screen]

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

        // here we describe the content of the game, including all of its screens.
        override this.Content (_, _) =
            [Content.screen Simulants.Splash.Screen.Name (Nu.Splash (Constants.Dissolve.Default, Constants.Splash.Default, None, Simulants.Title.Screen)) [] []
             Content.screenFromGroupFile Simulants.Title.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup"
             Content.screenFromGroupFile Simulants.Credits.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup"
             Content.screen<MyGameplayDispatcher> Simulants.Gameplay.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]