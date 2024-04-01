namespace MyGame
open System
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

    // this is our MMCC model type representing gameplay.
    // this model representation uses update time, that is, time based on number of engine updates.
    // if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
    // you could use `GameplayTime : single` instead. If you're going to use Split MMCC instead of Pure MMCC, you won't
    // need this field at all and should remove it, using world.UpdateTime or world.ClockTime instead (see
    // https://github.com/bryanedds/Nu/wiki/Pure-MMCC-vs.-Split-MMCC)
    type Gameplay =
        { GameplayTime : int64
          GameplayState : GameplayState }

    // this is our MMCC message type.
    type GameplayMessage =
        | FinishCommencing
        | StartQuitting
        | FinishQuitting
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
        inherit ScreenDispatcher<Gameplay, GameplayMessage, Command> ({ GameplayTime = 0; GameplayState = Quit })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Screen.SelectEvent => FinishCommencing
             Screen.DeselectingEvent => FinishQuitting
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

            | TimeUpdate ->
                let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + (let d = world.GameDelta in d.Updates) }
                just gameplay

        // here we describe the content of the game including the hud, the scene, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// time
                 Content.text Simulants.GameplayTime.Name
                    [Entity.Position == v3 0.0f 232.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.Text := string gameplay.GameplayTime]
                 
                 // quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while gameplay commences or quitting
             match gameplay.GameplayState with
             | Commence | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] []

             // no scene group otherwise
             | Commencing | Quit -> ()]