namespace MyGame
open System
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of the simulation during gameplay.
    type GameplayState =
        | Playing
        | Quitting
        | Quit

    // this is our MMCC model type representing gameplay.
    type Gameplay =
        { Score : int
          State : GameplayState }

    // this is our MMCC message type.
    type GameplayMessage =
        | Update
        | StartQutting
        | FinishQuitting
        interface Message

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, Command> ({ Score = 0; State = Quit })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Screen.UpdateEvent => Update
             Screen.DeselectingEvent => FinishQuitting]

        // here we handle the above messages
        override this.Message (gameplay, message, _, _) =
            match message with
            | Update -> just gameplay
            | StartQutting -> just { gameplay with State = Quitting }
            | FinishQuitting -> just { gameplay with State = Quit }

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []
                [Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQutting]]

             // the scene group while playing or quitting
             match gameplay.State with
             | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] []
             | Quit -> ()]