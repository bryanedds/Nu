namespace BlazeVector
open System
open System.Numerics
open Nu
open Nu.Declarative
open Prime
open BlazeVector

[<AutoOpen>]
module BulletDispatcher =

    type BulletCommand =
        | Update
        | Collision
        interface Command

    type BulletDispatcher () =
        inherit EntityDispatcher2d<int64, Message, BulletCommand> (true, fun world -> world.UpdateTime)

        static let [<Literal>] BulletLifeTime = 27L

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        override this.Initialize (_, _) =
            [Entity.Size == v3 20.0f 20.0f 0.0f
             Entity.Presence == Omnipresent
             Entity.Substance == Density 0.1f
             Entity.Restitution == 0.5f
             Entity.LinearDamping == 0.0f
             Entity.GravityOverride == Some v3Zero
             Entity.BodyShape == BodySphere { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None }
             Entity.StaticImage == Assets.Gameplay.PlayerBulletImage
             Entity.UpdateEvent => Update
             Entity.BodyCollisionEvent => Collision]

        override this.Command (startTime, command, entity, world) =
            match command with
            | Update ->
                let localTime = world.UpdateTime - startTime
                let world =
                    if localTime = BulletLifeTime
                    then World.destroyEntity entity world
                    else world
                just world
            | Collision ->
                let world = World.destroyEntity entity world
                just world

[<AutoOpen>]
module EnemyDispatcher =

    type Enemy =
        { Health : int }

    type EnemyMessage =
        | Collision of BodyCollisionData
        interface Message

    type EnemyCommand =
        | Update
        | Shot
        interface Command

    type Entity with
        member this.GetEnemy world : Enemy = this.GetModelGeneric<Enemy> world
        member this.SetEnemy enemy world = this.SetModelGeneric<Enemy> enemy world
        member this.Enemy = this.ModelGeneric<Enemy> ()
        member this.DyingEvent = Events.Dying --> this

    type EnemyDispatcher () =
        inherit EntityDispatcher2d<Enemy, EnemyMessage, EnemyCommand> (true, { Health = 7 })

        static let WalkForce = v3 -750.0f -5000.0f 0.0f

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        override this.Initialize (_, _) =
            [Entity.Size == v3 48.0f 96.0f 0.0f
             Entity.Friction == 0.0f
             Entity.AngularFactor == v3Zero
             Entity.LinearDamping == 3.0f
             Entity.GravityOverride == Some v3Zero
             Entity.BodyShape == BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None }
             Entity.CelCount == 6
             Entity.CelRun == 4
             Entity.CelSize == v2 48.0f 96.0f
             Entity.AnimationDelay == UpdateTime 8L
             Entity.AnimationSheet == Assets.Gameplay.EnemyImage
             Entity.UpdateEvent => Update
             Entity.BodyCollisionEvent =|> fun evt -> Collision evt.Data]

        override this.Message (enemy, message, _, world) =

            match message with
            | Collision collision ->
                match collision.BodyShapeCollidee.BodyId.BodySource with
                | :? Entity as collidee when collidee.Is<BulletDispatcher> world ->
                    let enemy = { enemy with Health = dec enemy.Health }
                    withSignal Shot enemy
                | _ -> just enemy

        override this.Command (enemy, command, entity, world) =

            match command with
            | Update ->
                let world =
                    if entity.GetInView2d world
                    then World.applyBodyForce WalkForce v3Zero (entity.GetBodyId world) world
                    else world
                let world =
                    if enemy.Health <= 0 then
                        let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world
                        let world = World.publish () entity.DyingEvent entity world
                        World.destroyEntity entity world
                    else world
                just world

            | Shot ->
                let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
                just world

[<AutoOpen>]
module PlayerDispatcher =

    type Player =
        { LastTimeOnGround : int64
          LastTimeJump : int64 }

    type PlayerMessage =
        | UpdateMessage
        | TryJumpByMouse
        | TryJumpByKeyboard of KeyboardKeyData
        interface Message

    type PlayerCommand =
        | UpdateCommand
        | Shoot
        | Jump
        interface Command

    type Entity with
        member this.GetPlayer world : Player = this.GetModelGeneric<Player> world
        member this.SetPlayer player world = this.SetModelGeneric<Player> player world
        member this.Player = this.ModelGeneric<Player> ()
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher2d<Player, PlayerMessage, PlayerCommand> (true, { LastTimeOnGround = Int64.MinValue; LastTimeJump = Int64.MinValue })

        static let [<Literal>] WalkForce = 1750.0f
        static let [<Literal>] FallForce = -5000.0f
        static let [<Literal>] ClimbForce = 2000.0f
        static let [<Literal>] JumpForce = 3000.0f
        static let [<Literal>] BulletForce = 25.0f

        static let createBullet (entity : Entity) world =
            let mutable playerTransform = entity.GetTransform world
            let (bullet, world) = World.createEntity<BulletDispatcher> NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
            let bulletPosition = playerTransform.Position + v3 (playerTransform.Size.X * 0.7f) 0.0f 0.0f
            let world = bullet.SetPosition bulletPosition world
            let world = bullet.SetElevation playerTransform.Elevation world
            (bullet, world)

        static let propelBullet (bullet : Entity) world =
            let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world
            World.applyBodyLinearImpulse (v3 BulletForce 0.0f 0.0f) v3Zero (bullet.GetBodyId world) world

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        override this.Initialize (_, _) =
            [Entity.Size == v3 48.0f 96.0f 0.0f
             Entity.AngularFactor == v3Zero
             Entity.Friction == 0.0f
             Entity.LinearDamping == 3.0f
             Entity.GravityOverride == Some v3Zero
             Entity.BodyShape == BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None }
             Entity.CelCount == 16
             Entity.CelRun == 4
             Entity.CelSize == v2 48.0f 96.0f
             Entity.AnimationDelay == UpdateTime 3L
             Entity.AnimationSheet == Assets.Gameplay.PlayerImage
             Entity.UpdateEvent => UpdateMessage
             Entity.UpdateEvent => UpdateCommand
             Simulants.Game.MouseLeftDownEvent => TryJumpByMouse
             Simulants.Game.KeyboardKeyDownEvent =|> fun evt -> TryJumpByKeyboard evt.Data]

        override this.Message (player, message, entity, world) =

            match message with
            | UpdateMessage ->
                let player =
                    if World.getBodyGrounded (entity.GetBodyId world) world
                    then { player with LastTimeOnGround = world.UpdateTime }
                    else player
                if world.Advancing && not (entity.HasFallen world) && world.UpdateTime % 5L = 0L
                then withSignal Shoot player
                else just player

            | TryJumpByMouse ->
                let time = world.UpdateTime
                if  time >= player.LastTimeJump + 12L &&
                    time <= player.LastTimeOnGround + 10L then
                    let player = { player with LastTimeJump = time }
                    withSignal Jump player
                else just player

            | TryJumpByKeyboard keyboardKeyData ->
                match (keyboardKeyData.KeyboardKey, keyboardKeyData.Repeated) with
                | (KeyboardKey.Space, false) ->
                    let time = world.UpdateTime
                    if  time >= player.LastTimeJump + 12L &&
                        time <= player.LastTimeOnGround + 10L then
                        let player = { player with LastTimeJump = time }
                        withSignal Jump player
                    else just player
                | _ -> just player

        override this.Command (_, command, entity, world) =

            match command with
            | UpdateCommand ->
                if world.Advancing then
                    let bodyId = entity.GetBodyId world
                    let groundTangentOpt = World.getBodyToGroundContactTangentOpt bodyId world
                    let force =
                        match groundTangentOpt with
                        | Some groundTangent ->
                            let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                            Vector3.Multiply (groundTangent, v3 WalkForce downForce 0.0f)
                        | None -> v3 WalkForce FallForce 0.0f
                    let world = World.applyBodyForce force v3Zero bodyId world
                    just world
                else just world

            | Jump ->
                let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.JumpSound world
                let world = World.applyBodyLinearImpulse (v3 0.0f JumpForce 0.0f) v3Zero (entity.GetBodyId world) world
                just world

            | Shoot ->
                let (bullet, world) = createBullet entity world
                let world = propelBullet bullet world
                just world

[<AutoOpen>]
module Gameplay =

    type GameplayState =
        | Playing
        | Quitting
        | Quit

    type Gameplay =
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
        | Update
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
                    then World.monitor (fun _ world -> (Cascade, screen.Signal (Score 100) world)) entity.DyingEvent screen world
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
             Screen.UpdateEvent => Update
             Simulants.GameplayGuiQuit.ClickEvent => StartQutting]

        override this.Message (gameplay, message, _, _) =
            match message with
            | Score score -> just { gameplay with Score = gameplay.Score + score }
            | StartQutting -> just { gameplay with State = Quitting }
            | FinishQuitting -> just { gameplay with State = Quit }

        override this.Command (gameplay, command, screen, world) =

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

            | Update ->

                // update eye
                let world =
                    if world.Advancing then
                        let playerPosition = Simulants.GameplayScenePlayer.GetPosition world
                        let playerSize = Simulants.GameplayScenePlayer.GetSize world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                        World.setEyeCenter2d eyeCenter world
                    else world

                // update player fall
                if Simulants.GameplayScenePlayer.HasFallen world && World.getSelectedScreenIdling world && gameplay.State = Playing then
                    let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.DeathSound world
                    withSignal StartQutting world
                else just world

        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []
                 [Content.text Simulants.GameplayGuiScore.Name
                    [Entity.Position == v3 392.0f 232.0f 0.0f
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
                         Entity.Elevation == 1.0f]]
             | Quit -> ()]