namespace BlazeVector
open System
open System.Numerics
open Nu
open Nu.Declarative
open BlazeVector

// TODO: convert BlazeVector entities to the Elmish style.

[<AutoOpen>]
module Bullet =

    type BulletDispatcher () =
        inherit EntityDispatcher2d (true)

        static let [<Literal>] BulletLifeTime = 27L

        static let handleBodyCollision evt world =
            let bullet = evt.Subscriber : Entity
            let world =
                if World.getAdvancing world
                then World.destroyEntity bullet world
                else world
            (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 20.0f 20.0f 0.0f)
             define Entity.Presence Omnipresent
             define Entity.Substance (Density 0.1f)
             define Entity.Restitution 0.5f
             define Entity.LinearDamping 0.0f
             define Entity.GravityOverride (Some v3Zero)
             define Entity.Bullet true
             define Entity.BodyShape (BodySphere { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
             define Entity.StaticImage Assets.Gameplay.PlayerBulletImage]

        override this.Register (entity, world) =
            let world = World.monitor handleBodyCollision entity.BodyCollisionEvent entity world
            let world = World.schedule BulletLifeTime (World.destroyEntity entity) entity world
            world

[<AutoOpen>]
module Enemy =

    type Entity with
        member this.GetHealth world : int = this.Get (nameof Entity.Health) world
        member this.SetHealth (value : int) world = this.Set (nameof Entity.Health) value world
        member this.Health = lens (nameof this.Health) this this.GetHealth this.SetHealth
        member this.DyingEvent = Events.Dying --> this

    type EnemyDispatcher () =
        inherit EntityDispatcher2d (true)

        static let move (entity : Entity) world =
            let force = v3 -750.0f -5000.0f 0.0f
            World.applyBodyForce force v3Zero (entity.GetBodyId world) world

        static let die (entity : Entity) world =
            let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world
            let world = World.publish () entity.DyingEvent entity world
            World.destroyEntity entity world

        static let handleUpdate evt world =
            let enemy = evt.Subscriber : Entity
            let world = if enemy.GetInView2d world then move enemy world else world
            let world = if enemy.GetHealth world <= 0 then die enemy world else world
            (Cascade, world)

        static let handleBodyCollision evt world =
            let enemy = evt.Subscriber : Entity
            let world =
                if World.getAdvancing world then
                    match evt.Data.BodyShapeCollidee.BodyId.BodySource with
                    | :? Entity as collidee when collidee.Is<BulletDispatcher> world ->
                        let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
                        enemy.SetHealth (enemy.GetHealth world - 1) world
                    | _ -> world
                else world
            (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 48.0f 96.0f 0.0f)
             define Entity.Friction 0.0f
             define Entity.AngularFactor v3Zero
             define Entity.LinearDamping 3.0f
             define Entity.GravityOverride (Some v3Zero)
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
             define Entity.CelCount 6
             define Entity.CelRun 4
             define Entity.CelSize (v2 48.0f 96.0f)
             define Entity.AnimationDelay (UpdateTime 8L)
             define Entity.AnimationSheet Assets.Gameplay.EnemyImage
             define Entity.Health 7]

        override this.Register (entity, world) =
            let world = World.monitor handleUpdate entity.UpdateEvent entity world
            let world = World.monitor handleBodyCollision entity.BodyCollisionEvent entity world
            world

[<AutoOpen>]
module Player =

    type Entity with
        member this.GetLastTimeOnGround world : int64 = this.Get (nameof Entity.LastTimeOnGround) world
        member this.SetLastTimeOnGround (value : int64) world = this.Set (nameof Entity.LastTimeOnGround) value world
        member this.LastTimeOnGround = lens (nameof this.LastTimeOnGround) this this.GetLastTimeOnGround this.SetLastTimeOnGround
        member this.GetLastTimeJump world : int64 = this.Get (nameof Entity.LastTimeJump) world
        member this.SetLastTimeJump (value : int64) world = this.Set (nameof Entity.LastTimeJump) value world
        member this.LastTimeJump = lens (nameof this.LastTimeJump) this this.GetLastTimeJump this.SetLastTimeJump
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher2d (true)

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

        static let shootBullet (entity : Entity) world =
            let (bullet, world) = createBullet entity world
            propelBullet bullet world

        static let handleSpawnBullet evt world =
            let player = evt.Subscriber : Entity
            let world =
                if World.getAdvancing world then
                    if not (player.HasFallen world) then
                        if World.getUpdateTime world % 5L = 0L
                        then shootBullet player world
                        else world
                    else world
                else world
            (Cascade, world)

        static let getLastTimeOnGround (entity : Entity) world =
            if not (World.getBodyGrounded (entity.GetBodyId world) world)
            then entity.GetLastTimeOnGround world
            else World.getUpdateTime world

        static let handleMovement evt world =
            let player = evt.Subscriber : Entity
            let lastTimeOnGround = getLastTimeOnGround player world
            let world = player.SetLastTimeOnGround lastTimeOnGround world
            let bodyId = player.GetBodyId world
            let groundTangentOpt = World.getBodyToGroundContactTangentOpt bodyId world
            let force =
                match groundTangentOpt with
                | Some groundTangent ->
                    let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                    Vector3.Multiply (groundTangent, v3 WalkForce downForce 0.0f)
                | None -> v3 WalkForce FallForce 0.0f
            let world = World.applyBodyForce force v3Zero bodyId world
            (Cascade, world)

        static let handleJump evt world =
            let player = evt.Subscriber : Entity
            let time = World.getUpdateTime world
            if  time >= player.GetLastTimeJump world + 12L &&
                time <= player.GetLastTimeOnGround world + 10L then
                let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.JumpSound world
                let world = player.SetLastTimeJump time world
                let world = World.applyBodyLinearImpulse (v3 0.0f JumpForce 0.0f) v3Zero (player.GetBodyId world) world
                (Cascade, world)
            else (Cascade, world)

        static let handleJumpByKeyboardKey evt world =
            if World.isSelectedScreenIdling world then
                match (evt.Data.KeyboardKey, evt.Data.Repeated) with
                | (KeyboardKey.Space, false) -> handleJump evt world
                | _ -> (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 48.0f 96.0f 0.0f)
             define Entity.AngularFactor v3Zero
             define Entity.Friction 0.0f
             define Entity.LinearDamping 3.0f
             define Entity.GravityOverride (Some v3Zero)
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.CelSize (v2 48.0f 96.0f)
             define Entity.AnimationDelay (UpdateTime 3L)
             define Entity.AnimationSheet Assets.Gameplay.PlayerImage
             nonPersistent Entity.LastTimeOnGround Int64.MinValue
             nonPersistent Entity.LastTimeJump Int64.MinValue]

        override this.Register (entity, world) =
            let world = World.monitor handleSpawnBullet entity.UpdateEvent entity world
            let world = World.monitor handleMovement entity.UpdateEvent entity world
            let world = World.monitor handleJump Events.MouseLeftDown entity world
            let world = World.monitor handleJumpByKeyboardKey Events.KeyboardKeyDown entity world
            world

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
                    if World.getAdvancing world then
                        let playerPosition = Simulants.GameplayScenePlayer.GetPosition world
                        let playerSize = Simulants.GameplayScenePlayer.GetSize world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                        World.setEyeCenter2d eyeCenter world
                    else world

                // update player fall
                if Simulants.GameplayScenePlayer.HasFallen world && World.isSelectedScreenIdling world && gameplay.State = Playing then
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