namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
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

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = Playing }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
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

            [// time
             Content.text Simulants.GameplayTime.Name
                [Entity.Position == v3 0.0f 150.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text := string gameplay.GameplayTime]

             // quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]

         // the scene group while playing or quitting
         match gameplay.GameplayState with
         | Playing -> Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] []

         // no scene group otherwise
         | Quit -> ()]