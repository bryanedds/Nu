namespace MyGame
open System
open Prime
open Nu

[<AutoOpen>]
module MyGame =

    // this is our MMCC model type. It determines what state the game is in. To learn about MMCC in
    // Nu, see here - https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
    type Model =
        | Splash
        | Title
        | Credits
        | Gameplay of Gameplay

    // this is our MMCC message type.
    type Message =
        | ShowTitle
        | ShowCredits
        | ShowGameplay
        | Update
        interface Nu.Message
        
    // this is our MMCC command type. Commands are used instead of messages when the world is to be
    // transformed.
    type Command =
        | Exit
        interface Nu.Command

    // this extends the Game API to expose the above model.
    type Game with
        member this.GetModel world = this.GetModelGeneric<Model> world
        member this.SetModel value world = this.SetModelGeneric<Model> value world
        member this.Model = this.ModelGeneric<Model> ()

    // this is the game dispatcher that is customized for our game. In here, we create screens as
    // content and bind them up with events and properties.
    type MyGameDispatcher () =
        inherit GameDispatcher<Model, Message, Command> (Splash)

        // here we define the game's properties and event handling
        override this.Definitions (model, _) =
            [Game.DesiredScreen :=
                match model with
                | Splash -> Desire Simulants.Splash
                | Title -> Desire Simulants.Title
                | Credits -> Desire Simulants.Credits
                | Gameplay gameplay ->
                    match gameplay.GameplayState with
                    | Commencing | Commence -> Desire Simulants.Gameplay
                    | Quitting | Quit -> Desire Simulants.Title
             match model with Gameplay gameplay -> Simulants.Gameplay.Gameplay := gameplay | _ -> ()
             Game.UpdateEvent => Update
             Simulants.Splash.DeselectingEvent => ShowTitle
             Simulants.TitleCredits.ClickEvent => ShowCredits
             Simulants.TitlePlay.ClickEvent => ShowGameplay
             Simulants.TitleExit.ClickEvent => Exit
             Simulants.CreditsBack.ClickEvent => ShowTitle]

        // here we handle the above messages
        override this.Message (model, message, _, world) =
            match message with
            | ShowTitle -> just Title
            | ShowCredits -> just Credits
            | ShowGameplay -> just (Gameplay Gameplay.commencing)
            | Update ->
                match model with
                | Gameplay gameplay ->
                    let gameplay' = Simulants.Gameplay.GetGameplay world
                    if gameplay =/= gameplay' then just (Gameplay gameplay') else just model
                | _ -> just model

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