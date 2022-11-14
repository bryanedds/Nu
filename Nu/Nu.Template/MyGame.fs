namespace MyGame
open Prime
open Nu

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
        | GameplayChanged of Gameplay

    // this is our Elm-style command type. Commands are used instead of messages when explicitly
    // updating the world is involved.
    type Command =
        | ModelChanged
        | Exit

    // this extends the Game API to expose the above model as well as the model bimapped to Gameplay,
    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        static member Model = Game.ModelGeneric<Model> ()

    // this is the game dispatcher that is customized for our game. In here, we create screens as
    // content and bind them up with events and properties.
    type MyGameDispatcher () =
        inherit GameForger<Model, Message, Command> (Splash)

        // here we handle the above messages
        override this.Message (model, message, _, _) =
            match message with
            | ShowTitle -> just Title
            | ShowCredits -> just Credits
            | ShowGameplay -> just (Gameplay Playing)
            | GameplayChanged gameplay -> match gameplay with Playing | Quitting -> just (Gameplay gameplay) | Quit -> just model

        // here we handle the above commands
        override this.Command (model, command, _, world) =
            match command with
            | ModelChanged -> match model with Gameplay gameplay -> just (Simulants.Gameplay.Screen.SetGameplay gameplay world) | _ -> just world
            | Exit -> just (World.exit world)

        // here we describe the content of the game, including all of its screens.
        override this.Forge (model, game) =
            Forge.game
                [Game.DesiredScreen ==
                    match model with
                    | Splash -> Desire Simulants.Splash.Screen
                    | Title -> Desire Simulants.Title.Screen
                    | Credits -> Desire Simulants.Credits.Screen
                    | Gameplay gameplay -> match gameplay with | Playing -> Desire Simulants.Gameplay.Screen | Quitting | Quit -> Desire Simulants.Title.Screen
                 Game.Event.ChangeEvent "Model" ==> cmd ModelChanged
                 Simulants.Splash.Screen.DeselectingEvent ==> msg ShowTitle
                 Simulants.Title.Gui.Credits.ClickEvent ==> msg ShowCredits
                 Simulants.Title.Gui.Play.ClickEvent ==> msg ShowGameplay
                 Simulants.Title.Gui.Exit.ClickEvent ==> cmd Exit
                 Simulants.Credits.Gui.Back.ClickEvent ==> msg ShowTitle
                 Simulants.Gameplay.Screen.ChangeEvent "Gameplay" ==|> fun event -> msg (GameplayChanged (event.Data.Value :?> Gameplay))]
                [Forge.screen Simulants.Splash.Screen.Name (WorldTypes.Splash (Constants.Dissolve.Default, Constants.Splash.Default, None, Simulants.Title.Screen)) [] []
                 Forge.screenWithGroupFromFile Simulants.Title.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
                 Forge.screenWithGroupFromFile Simulants.Credits.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
                 Forge.screen<MyGameplayDispatcher> Simulants.Gameplay.Screen.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]