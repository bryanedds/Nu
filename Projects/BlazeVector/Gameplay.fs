namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu

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

    static let [<Literal>] SectionCount = 12

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.PostUpdateEvent => UpdateEye
         for i in 0 .. dec SectionCount do
            Events.DieEvent --> Simulants.GameplaySection i --> Address.Wildcard => Score 100]

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
            let world = (world, [0 .. dec SectionCount]) ||> List.fold (fun world sectionIndex ->

                // load a random section from file (except the first section which is always 0)
                let section = Simulants.GameplaySection sectionIndex
                let sectionFilePath = if sectionIndex = 0 then Assets.Gameplay.SectionFilePaths.[0] else Gen.randomItem Assets.Gameplay.SectionFilePaths
                let world = World.readGroupFromFile sectionFilePath (Some section.Name) section.Screen world |> snd

                // shift all entities in the loaded section so that they go after the previously loaded section
                let sectionXShift = 1024.0f * single sectionIndex
                let sectionEntities = World.getEntities section world
                Seq.fold (fun world (sectionEntity : Entity) ->
                    sectionEntity.SetPosition (sectionEntity.GetPosition world + v3 sectionXShift 0.0f 0.0f) world)
                    world sectionEntities)

            // fin
            just world

        | StartQuitting ->

            // publish the gameplay quit event
            let world = World.publish () screen.QuitEvent screen world
            just world

        | DestroySections ->

            // destroy stage sections that were created from section files
            let world = (world, [0 .. dec SectionCount]) ||> List.fold (fun world sectionIndex ->
                let section = Simulants.GameplaySection sectionIndex
                World.destroyGroup section world)
            just world

        | UpdateEye ->

            // update eye to look at player while game is advancing
            if world.Advancing then
                let playerPosition = Simulants.GameplayPlayer.GetPosition world
                let playerSize = Simulants.GameplayPlayer.GetSize world
                let eyeCenter = World.getEye2dCenter world
                let eyeSize = World.getEye2dSize world
                let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                let world = World.setEye2dCenter eyeCenter world
                just world
            else just world

    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) =

        [// the gui group
         Content.group Simulants.GameplayGui.Name []
             [Content.text Simulants.GameplayScore.Name
                [Entity.Position == v3 260.0f 155.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text := "Score: " + string gameplay.Score]
              Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]

         // the scene group while playing
         match gameplay.GameplayState with
         | Playing ->
            Content.group Simulants.GameplayScene.Name []
                [Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                    [Entity.Position == v3 -390.0f -50.0f 0.0f
                     Entity.Elevation == 1.0f
                     Entity.DieEvent => StartQuitting]]

         // no scene group otherwise
         | Quit -> ()]