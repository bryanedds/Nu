namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
type [<SymbolicExpansion>] Gameplay =
    { GameplayState : GameplayState
      Score : int }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayState = Quit
          Score = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { GameplayState = Playing
          Score = 0 }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Score of int
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    | CreateSections
    | DestroySections
    | UpdateEye
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

    static let [<Literal>] SectionCount = 16

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.PostUpdateEvent => UpdateEye
         for i in 0 .. dec SectionCount do
            Events.DeathEvent --> Simulants.GameplaySection i --> Address.Wildcard => Score 100]

    // here we handle the above messages
    override this.Message (gameplay, message, _, _) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            withSignal CreateSections gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            withSignal DestroySections gameplay

        | Score score ->
            let gameplay = { gameplay with Score = gameplay.Score + score }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | CreateSections ->

            // create stage sections from random section files
            for sectionIndex in 0 .. dec SectionCount do

                // load a random section from file (except the first section which is always 0)
                let section = Simulants.GameplaySection sectionIndex
                let sectionFilePath = if sectionIndex = 0 then Assets.Gameplay.SectionFilePaths.[0] else Gen.randomItem Assets.Gameplay.SectionFilePaths
                World.readGroupFromFile sectionFilePath (Some section.Name) section.Screen world |> ignore<Group>

                // shift all entities in the loaded section so that they go after the previously loaded section
                let sectionXShift = 1024.0f * single sectionIndex
                let sectionEntities = World.getEntities section world
                for sectionEntity in sectionEntities do
                    sectionEntity.SetPosition (sectionEntity.GetPosition world + v3 sectionXShift 0.0f 0.0f) world

        | StartQuitting ->

            // publish the gameplay quit event
            World.publish () screen.QuitEvent screen world

        | DestroySections ->

            // destroy stage sections that were created from section files
            for sectionIndex in 0 .. dec SectionCount do
                let section = Simulants.GameplaySection sectionIndex
                World.destroyGroup section world

        | UpdateEye ->

            // update eye to look at player while game is advancing
            if world.Advancing then
                let playerPosition = Simulants.GameplayPlayer.GetPosition world
                let playerSize = Simulants.GameplayPlayer.GetSize world
                let eyeCenter = world.Eye2dCenter
                let eyeSize = world.Eye2dSize
                let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                World.setEye2dCenter eyeCenter world

    // here we describe the content of the game including the scene, the player, and the hud
    override this.Content (gameplay, _) =

        [// the scene group while playing
         if gameplay.GameplayState = Playing then
            Content.group Simulants.GameplayScene.Name []

                [// player
                 Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                    [Entity.Position != v3 -390.0f -50.0f 0.0f
                     Entity.Elevation == 1.0f
                     Entity.DeathEvent => StartQuitting]

                 // score
                 Content.text Simulants.GameplayScore.Name
                   [Entity.Position == v3 260.0f 155.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Text := "Score: " + string gameplay.Score]

                 // quit
                 Content.button Simulants.GameplayQuit.Name
                   [Entity.Position == v3 232.0f -144.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Text == "Quit"
                    Entity.ClickEvent => StartQuitting]]]