namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open BlazeVector

[<AutoOpen>]
module Bullet =

    type BulletDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static let [<Literal>] BulletLifeTime =
            27L

        static let handleBodyCollision evt world =
            let bullet = evt.Subscriber : Entity
            let world =
                if World.isAdvancing world
                then World.destroyEntity bullet world
                else world
            (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 20.0f 20.0f 0.0f)
             define Entity.Omnipresent true
             define Entity.Density 0.1f
             define Entity.Restitution 0.5f
             define Entity.LinearDamping 0.0f
             define Entity.GravityScale 0.0f
             define Entity.Bullet true
             define Entity.BodyShape (BodySphere { Radius = 0.5f; Center = v3Zero; PropertiesOpt = None })
             define Entity.StaticImage Assets.Gameplay.PlayerBulletImage]

        override this.Register (bullet, world) =
            let world = World.monitor handleBodyCollision bullet.BodyCollisionEvent bullet world
            let world = World.delay (World.destroyEntity bullet) BulletLifeTime bullet world
            world

[<AutoOpen>]
module Enemy =

    type Entity with
        member this.GetHealth world : int = this.Get Property? Health world
        member this.SetHealth (value : int) world = this.Set Property? Health value world
        member this.Health = lens<int> Property? Health this.GetHealth this.SetHealth this
        member this.IsOnScreen world =
            let viewBounds = World.getViewBounds2d world
            let perimeter = this.GetBounds world
            let center = perimeter.Center
            Math.isPointInBounds2d center.V2 viewBounds

    type EnemyDispatcher () =
        inherit EntityDispatcher2d (false, true)
        
        static let move (enemy : Entity) world =
            let force = v3 -750.0f -5000.0f 0.0f
            World.applyBodyForce force (enemy.GetPhysicsId world) world

        static let die (enemy : Entity) world =
            let world = World.destroyEntity enemy world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ExplosionSound world

        static let handleUpdate evt world =
            let enemy = evt.Subscriber : Entity
            let world = if enemy.IsOnScreen world then move enemy world else world
            let world = if enemy.GetHealth world <= 0 then die enemy world else world
            (Cascade, world)

        static let handleBodyCollision evt world =
            let enemy = evt.Subscriber : Entity
            let world =
                if World.isAdvancing world then
                    let collidee = evt.Data.BodyCollidee.Entity
                    let bullet = collidee.Is<BulletDispatcher> world
                    if bullet then
                        let world = enemy.SetHealth (enemy.GetHealth world - 1) world
                        let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.HitSound world
                        world
                    else world
                else world
            (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 48.0f 96.0f 0.0f)
             define Entity.Friction 0.0f
             define Entity.FixedRotation true
             define Entity.LinearDamping 3.0f
             define Entity.GravityScale 0.0f
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v3Zero; PropertiesOpt = None })
             define Entity.CelCount 6
             define Entity.CelRun 4
             define Entity.CelSize (v2 48.0f 96.0f)
             define Entity.AnimationDelay 8L
             define Entity.AnimationSheet Assets.Gameplay.EnemyImage
             define Entity.Health 7]

        override this.Register (enemy, world) =
            let world = World.monitor handleUpdate enemy.UpdateEvent enemy world
            let world = World.monitor handleBodyCollision enemy.BodyCollisionEvent enemy world
            world

[<AutoOpen>]
module Player =

    type Entity with
        member this.GetLastTimeOnGround world : int64 = this.Get Property? LastTimeOnGround world
        member this.SetLastTimeOnGround (value : int64) world = this.Set Property? LastTimeOnGround value world
        member this.LastTimeOnGround = lens Property? LastTimeOnGround this.GetLastTimeOnGround this.SetLastTimeOnGround this
        member this.GetLastTimeJump world : int64 = this.Get Property? LastTimeJump world
        member this.SetLastTimeJump (value : int64) world = this.Set Property? LastTimeJump value world
        member this.LastTimeJump = lens Property? LastTimeJump this.GetLastTimeJump this.SetLastTimeJump this
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static let [<Literal>] WalkForce = 1750.0f
        static let [<Literal>] FallForce = -5000.0f
        static let [<Literal>] ClimbForce = 2000.0f
        static let [<Literal>] JumpForce = 3000.0f
        static let [<Literal>] BulletForce = 25.0f

        static let createBullet (player : Entity) world =
            let mutable playerTransform = player.GetTransform world
            let (bullet, world) = World.createEntity<BulletDispatcher> None NoOverlay player.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
            let bulletPosition = playerTransform.Position + v3 (playerTransform.Size.X * 0.95f) (playerTransform.Size.Y * 0.4f) 0.0f
            let world = bullet.SetPosition bulletPosition world
            let world = bullet.SetElevation playerTransform.Elevation world
            (bullet, world)

        static let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (v3 BulletForce 0.0f 0.0f) (bullet.GetPhysicsId world) world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world

        static let shootBullet (player : Entity) world =
            let (bullet, world) = createBullet player world
            propelBullet bullet world

        static let handleSpawnBullet evt world =
            let player = evt.Subscriber : Entity
            let world =
                if World.isAdvancing world then
                    if not (player.HasFallen world) then
                        if World.getUpdateTime world % 5L = 0L
                        then shootBullet player world
                        else world
                    else world
                else world
            (Cascade, world)

        static let getLastTimeOnGround (player : Entity) world =
            if not (World.isBodyOnGround (player.GetPhysicsId world) world)
            then player.GetLastTimeOnGround world
            else World.getUpdateTime world

        static let handleMovement evt world =
            let player = evt.Subscriber : Entity
            let lastTimeOnGround = getLastTimeOnGround player world
            let world = player.SetLastTimeOnGround lastTimeOnGround world
            let physicsId = player.GetPhysicsId world
            let groundTangentOpt = World.getBodyToGroundContactTangentOpt physicsId world
            let force =
                match groundTangentOpt with
                | Some groundTangent ->
                    let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                    Vector3.Multiply (groundTangent, v3 WalkForce downForce 0.0f)
                | None -> v3 WalkForce FallForce 0.0f
            let world = World.applyBodyForce force physicsId world
            (Cascade, world)

        static let handleJump evt world =
            let player = evt.Subscriber : Entity
            let time = World.getUpdateTime world
            if  time >= player.GetLastTimeJump world + 12L &&
                time <= player.GetLastTimeOnGround world + 10L then
                let world = player.SetLastTimeJump time world
                let world = World.applyBodyLinearImpulse (v3 0.0f JumpForce 0.0f) (player.GetPhysicsId world) world
                let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.JumpSound world
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
             define Entity.FixedRotation true
             define Entity.Friction 0.0f
             define Entity.LinearDamping 3.0f
             define Entity.GravityScale 0.0f
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v3Zero; PropertiesOpt = None })
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.CelSize (v2 48.0f 96.0f)
             define Entity.AnimationDelay 3L
             define Entity.AnimationSheet Assets.Gameplay.PlayerImage
             nonPersistent Entity.LastTimeOnGround Int64.MinValue
             nonPersistent Entity.LastTimeJump Int64.MinValue]

        override this.Register (player, world) =
            let world = World.monitor handleSpawnBullet player.UpdateEvent player world
            let world = World.monitor handleMovement player.UpdateEvent player world
            let world = World.monitor handleJump Events.MouseLeftDown player world
            let world = World.monitor handleJumpByKeyboardKey Events.KeyboardKeyDown player world
            world

[<AutoOpen>]
module Gameplay =

    type Gameplay =
        | Playing
        | Quitting

    type GameplayMessage =
        | Quit

    type GameplayCommand =
        | CreateSections
        | DestroySections
        | Update

    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Quitting)

        static let [<Literal>] SectionName = "Section"
        static let [<Literal>] SectionCount = 16

        static let shiftEntities xShift entities world =
            Seq.fold
                (fun world (entity : Entity) -> entity.SetPosition (entity.GetPosition world + v3 xShift 0.0f 0.0f) world)
                world
                entities

        static let createSectionFromFile filePath sectionName xShift gameplay world =
            let (section, world) = World.readGroupFromFile filePath (Some sectionName) gameplay world
            let sectionEntities = World.getEntities section world
            shiftEntities xShift sectionEntities world

        override this.Channel (_, gameplay) =
            [gameplay.SelectEvent => cmd CreateSections
             gameplay.DeselectEvent => cmd DestroySections
             gameplay.UpdateEvent => cmd Update
             Simulants.Gameplay.Gui.Quit.ClickEvent => msg Quit]

        override this.Message (_, message, _, _) =
            match message with
            | Quit -> just Quitting

        override this.Command (_, command, gameplay, world) =

            match command with
            | CreateSections ->
                let world =
                    List.fold
                        (fun world i ->
                            let sectionFilePath =
                                if i = 0
                                then Assets.Gameplay.SectionFilePaths.[0]
                                else Gen.randomItem Assets.Gameplay.SectionFilePaths
                            let sectionName = SectionName + scstring i
                            let sectionXShift = 2048.0f * single i
                            createSectionFromFile sectionFilePath sectionName sectionXShift gameplay world)
                        world
                        [0 .. SectionCount - 1]
                just world

            | DestroySections ->
                let sectionNames = [for i in 0 .. SectionCount - 1 do yield SectionName + scstring i]
                let groupNames = Simulants.Gameplay.Scene.Group.Name :: sectionNames
                let groups = List.map (fun groupName -> gameplay / groupName) groupNames
                let world = World.destroyGroups groups world
                just world

            | Update ->

                // update eye
                let world =
                    if World.getUpdateRate world <> 0L then
                        let playerPosition = Simulants.Gameplay.Scene.Player.GetPosition world
                        let playerSize = Simulants.Gameplay.Scene.Player.GetSize world
                        let eyePosition = World.getEyePosition2d world
                        let eyeSize = World.getEyeSize2d world
                        let eyePosition = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyePosition.Y
                        Game.SetEyePosition2d eyePosition world
                    else world

                // update player fall
                if Simulants.Gameplay.Scene.Player.HasFallen world && World.isSelectedScreenIdling world then
                    let world = World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.DeathSound world
                    if Simulants.Title.Screen.Exists world
                    then withMsg Quit world
                    else just world
                else just world

        override this.Content (_, screen) =

            [// the gui group
             Content.group Simulants.Gameplay.Gui.Group.Name []
                 [Content.button Simulants.Gameplay.Gui.Quit.Name
                     [Entity.Text == "Quit"
                      Entity.Position == v3 260.0f -260.0f 0.0f
                      Entity.Elevation == 10.0f
                      Entity.ClickEvent ==> msg Quit]]

             // the scene group
             Content.groupIfScreenSelected screen $ fun _ _ ->
                Content.group Simulants.Gameplay.Scene.Group.Name []
                    [Content.entity<PlayerDispatcher> Simulants.Gameplay.Scene.Player.Name
                        [Entity.Position == v3 -300.0f -175.6805f 0.0f
                         Entity.Elevation == 1.0f]]]