namespace BlazeVector
open System
open Prime
open Nu
open Nu.Declarative
open BlazeVector

[<AutoOpen>]
module Gameplay =

    type GameplayState =
        | Playing
        | Quitting
        | Quit

    type [<SymbolicExpansion>] Gameplay =
        { State : GameplayState
          Score : int }

    type GameplayMessage =
        | Score of int
        | StartQutting
        | FinishQuitting
        interface Message

    type GameplayCommand =
        | CreateSections
        | DestroySections
        | UpdateEye
        interface Command

    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> ({ State = Quit; Score = 0 })

        static let [<Literal>] SectionName = "Section"
        static let [<Literal>] SectionCount = 16

        static let inductEntities xShift entities (screen : Screen) world =
            Seq.fold
                (fun world (entity : Entity) ->
                    let world = entity.SetPosition (entity.GetPosition world + v3 xShift 0.0f 0.0f) world
                    if entity.Is<EnemyDispatcher> world
                    then World.monitor (fun _ world -> (Cascade, screen.Signal (Score 100) world)) entity.DieEvent screen world
                    else world)
                world
                entities

        static let createSectionFromFile filePath sectionName xShift screen world =
            let (section, world) = World.readGroupFromFile filePath (Some sectionName) screen world
            let sectionEntities = World.getEntitiesFlattened section world
            inductEntities xShift sectionEntities screen world

        override this.Initialize (_, _) =
            [Screen.SelectEvent => CreateSections
             Screen.DeselectingEvent => DestroySections
             Screen.PostUpdateEvent => UpdateEye
             Simulants.GameplayGuiQuit.ClickEvent => StartQutting]

        override this.Message (gameplay, message, _, _) =
            match message with
            | Score score -> just { gameplay with Score = gameplay.Score + score }
            | StartQutting -> just { gameplay with State = Quitting }
            | FinishQuitting -> just { gameplay with State = Quit }

        override this.Command (_, command, screen, world) =

            match command with
            | CreateSections ->
                let world =
                    List.fold
                        (fun world i ->
                            let sectionFilePath =
                                if i = 0
                                then Assets.Gameplay.SectionFilePaths.[0]
                                else Gen.randomItem Assets.Gameplay.SectionFilePaths
                            let sectionName = SectionName + string i
                            let sectionXShift = 2048.0f * single i
                            createSectionFromFile sectionFilePath sectionName sectionXShift screen world)
                        world
                        [0 .. SectionCount - 1]
                just world

            | DestroySections ->
                let sectionNames = [for i in 0 .. SectionCount - 1 do yield SectionName + string i]
                let groupNames = Simulants.GameplayScene.Name :: sectionNames
                let groups = List.map (fun groupName -> screen / groupName) groupNames
                let world = World.destroyGroups groups world
                withSignal FinishQuitting world

            | UpdateEye ->
                if world.Advancing then
                    let playerPosition = Simulants.GameplayScenePlayer.GetPosition world
                    let playerSize = Simulants.GameplayScenePlayer.GetSize world
                    let eyeCenter = World.getEyeCenter2d world
                    let eyeSize = World.getEyeSize2d world
                    let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                    let world = World.setEyeCenter2d eyeCenter world
                    just world
                else just world

        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []
                 [Content.text Simulants.GameplayGuiScore.Name
                    [Entity.Position == v3 392.0f 232.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text := "Score: " + string gameplay.Score]
                  Content.button Simulants.GameplayGuiQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQutting]]

             // the scene group while playing
             match gameplay.State with
             | Playing | Quitting ->
                Content.group Simulants.GameplayScene.Name []
                    [Content.entity<PlayerDispatcher> Simulants.GameplayScenePlayer.Name
                        [Entity.Position == v3 -876.0f -127.6805f 0.0f
                         Entity.Elevation == 1.0f
                         Entity.DieEvent => StartQutting]]
             | Quit -> ()]