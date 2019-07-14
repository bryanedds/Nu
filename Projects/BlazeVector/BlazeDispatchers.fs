namespace BlazeVector
open System
open SDL2
open Prime
open Nu
open Nu.Declarative
open BlazeVector

[<AutoOpen>]
module BulletModule =

    type Entity with
    
        member this.GetAge = this.Get Property? Age
        member this.SetAge = this.Set Property? Age
        member this.Age = Lens.make<int64, World> Property? Age this.GetAge this.SetAge this

    type BulletDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] BulletLifetime = 27L

        static let handleUpdate evt world =
            let bullet = evt.Subscriber : Entity
            let world = bullet.SetAge (inc (bullet.GetAge world)) world
            if bullet.GetAge world > BulletLifetime
            then World.destroyEntity bullet world
            else world

        static let handleCollision evt world =
            let bullet = evt.Subscriber : Entity
            if World.isTicking world
            then World.destroyEntity bullet world
            else world

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

        static member Properties =
            [define Entity.Size (Vector2 (20.0f, 20.0f))
             define Entity.Omnipresent true
             define Entity.Density 0.25f
             define Entity.Restitution 0.5f
             define Entity.LinearDamping 0.0f
             define Entity.GravityScale 0.0f
             define Entity.IsBullet true
             define Entity.CollisionBody (BodyCircle { Radius = 0.5f; Center = Vector2.Zero })
             define Entity.StaticImage Assets.PlayerBulletImage
             define Entity.Age 0L]

        override dispatcher.Register (bullet, world) =
            let world = World.monitor handleUpdate bullet.UpdateEvent bullet world
            let world = World.monitor handleCollision bullet.CollisionEvent bullet world
            world

[<AutoOpen>]
module EnemyModule =

    type Entity with
    
        member this.GetHealth = this.Get Property? Health
        member this.SetHealth = this.Set Property? Health
        member this.Health = Lens.make<int, World> Property? Health this.GetHealth this.SetHealth this
        
        member this.IsOnScreen world =
            let viewBounds = World.getViewBoundsRelative world
            Math.isPointInBounds (this.GetCenter world) viewBounds

    type EnemyDispatcher () =
        inherit EntityDispatcher ()
        
        static let move (enemy : Entity) world =
            let force = Vector2 (-2000.0f, -20000.0f)
            World.applyBodyForce force (enemy.GetPhysicsId world) world

        static let die (enemy : Entity) world =
            let world = World.destroyEntity enemy world
            World.playSound 1.0f Assets.ExplosionSound world

        static let handleUpdate evt world =
            let enemy = evt.Subscriber : Entity
            let world = if enemy.IsOnScreen world then move enemy world else world
            if enemy.GetHealth world <= 0 then die enemy world else world

        static let handleCollision evt world =
            let enemy = evt.Subscriber : Entity
            if World.isTicking world then
                let collidee = evt.Data.Collidee
                let isBullet = collidee.DispatchesAs<BulletDispatcher> world
                if isBullet then
                    let world = enemy.SetHealth (enemy.GetHealth world - 1) world
                    let world = World.playSound 1.0f Assets.HitSound world
                    world
                else world
            else world

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        static member Properties =
            [define Entity.Size (Vector2 (48.0f, 96.0f))
             define Entity.Friction 0.0f
             define Entity.FixedRotation true
             define Entity.LinearDamping 3.0f
             define Entity.GravityScale 0.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = Vector2.Zero })
             define Entity.CelCount 6
             define Entity.CelRun 4
             define Entity.CelSize (Vector2 (48.0f, 96.0f))
             define Entity.AnimationDelay 8L
             define Entity.AnimationSheet Assets.EnemyImage
             define Entity.Health 7]

        override dispatcher.Register (enemy, world) =
            let world = World.monitor handleUpdate enemy.UpdateEvent enemy world
            let world = World.monitor handleCollision enemy.CollisionEvent enemy world
            world

[<AutoOpen>]
module PlayerModule =

    type Entity with

        member this.GetLastTimeOnGroundNp = this.Get Property? LastTimeOnGroundNp
        member this.SetLastTimeOnGroundNp = this.Set Property? LastTimeOnGroundNp
        member this.LastTimeOnGroundNp = Lens.make<int64, World> Property? LastTimeOnGroundNp this.GetLastTimeOnGroundNp this.SetLastTimeOnGroundNp this
        member this.GetLastTimeJumpNp = this.Get Property? LastTimeJumpNp
        member this.SetLastTimeJumpNp = this.Set Property? LastTimeJumpNp
        member this.LastTimeJumpNp = Lens.make<int64, World> Property? LastTimeJumpNp this.GetLastTimeJumpNp this.SetLastTimeJumpNp this
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] WalkForce = 8000.0f
        static let [<Literal>] FallForce = -30000.0f
        static let [<Literal>] ClimbForce = 12000.0f

        static let createBullet (player : Entity) (playerTransform : Transform) world =
            let (bullet, world) = World.createEntity<BulletDispatcher> None DefaultOverlay (etol player) world
            let bulletPosition = playerTransform.Position + Vector2 (playerTransform.Size.X * 0.9f, playerTransform.Size.Y * 0.4f)
            let world = bullet.SetPosition bulletPosition world
            let world = bullet.SetDepth playerTransform.Depth world
            let world = bullet.PropagatePhysics world
            (bullet, world)

        static let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (Vector2 (35.0f, 0.0f)) (bullet.GetPhysicsId world) world
            World.playSound 1.0f Assets.ShotSound world

        static let shootBullet (player : Entity) world =
            let playerTransform = player.GetTransform world
            let (bullet, world) = createBullet player playerTransform world
            propelBullet bullet world

        static let handleSpawnBullet evt world =
            let player = evt.Subscriber : Entity
            if World.isTicking world then
                if not (player.HasFallen world) then
                    if World.getTickTime world % 5L = 0L
                    then shootBullet player world
                    else world
                else world
            else world

        static let getLastTimeOnGround (player : Entity) world =
            if not (World.isBodyOnGround (player.GetPhysicsId world) world)
            then player.GetLastTimeOnGroundNp world
            else World.getTickTime world

        static let handleMovement evt world =
            let player = evt.Subscriber : Entity
            let lastTimeOnGround = getLastTimeOnGround player world
            let world = player.SetLastTimeOnGroundNp lastTimeOnGround world
            let physicsId = player.GetPhysicsId world
            let groundTangentOpt = World.getBodyToGroundContactTangentOpt physicsId world
            let force =
                match groundTangentOpt with
                | Some groundTangent ->
                    let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                    Vector2.Multiply (groundTangent, Vector2 (WalkForce, downForce))
                | None -> Vector2 (WalkForce, FallForce)
            World.applyBodyForce force physicsId world

        static let handleJump evt world =
            let player = evt.Subscriber : Entity
            let tickTime = World.getTickTime world
            if  tickTime >= player.GetLastTimeJumpNp world + 12L &&
                tickTime <= player.GetLastTimeOnGroundNp world + 10L then
                let world = player.SetLastTimeJumpNp tickTime world
                let world = World.applyBodyLinearImpulse (Vector2 (0.0f, 18000.0f)) (player.GetPhysicsId world) world
                let world = World.playSound 1.0f Assets.JumpSound world
                world
            else world

        static let handleJumpByKeyboardKey evt world =
            if World.isSelectedScreenIdling world then
                match (enum<SDL.SDL_Scancode> evt.Data.ScanCode, evt.Data.Repeated) with
                | (SDL.SDL_Scancode.SDL_SCANCODE_SPACE, false) -> handleJump evt world
                | _ -> world
            else world

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        static member Properties =
            [define Entity.Size (Vector2 (48.0f, 96.0f))
             define Entity.FixedRotation true
             define Entity.Friction 0.0f
             define Entity.LinearDamping 3.0f
             define Entity.GravityScale 0.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = Vector2.Zero })
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.CelSize (Vector2 (48.0f, 96.0f))
             define Entity.AnimationDelay 3L
             define Entity.AnimationSheet Assets.PlayerImage
             define Entity.LastTimeOnGroundNp Int64.MinValue
             define Entity.LastTimeJumpNp Int64.MinValue]

        override dispatcher.Register (player, world) =
            let world = World.monitor handleSpawnBullet player.UpdateEvent player world
            let world = World.monitor handleMovement player.UpdateEvent player world
            let world = World.monitor handleJump Events.MouseLeftDown player world
            let world = World.monitor handleJumpByKeyboardKey Events.KeyboardKeyDown player world
            world

[<AutoOpen>]
module SceneModule =

    type SceneCommand =
        | AdjustCamera
        | PlayerFall

    type SceneDispatcher () =
        inherit LayerDispatcher<unit, unit, SceneCommand> ()

        override dispatcher.Bindings (_, scene, _) =
            [scene.UpdateEvent =>! AdjustCamera
             scene.UpdateEvent =>! PlayerFall]

        override dispatcher.Command (command, _, _, world) =
            match command with
            | AdjustCamera ->
                if World.getTickRate world <> 0L then
                    let playerPosition = Simulants.Player.GetPosition world
                    let playerSize = Simulants.Player.GetSize world
                    let eyeCenter = World.getEyeCenter world
                    let eyeSize = World.getEyeSize world
                    let eyeCenter = Vector2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f, eyeCenter.Y)
                    Game.SetEyeCenter eyeCenter world
                else world
            | PlayerFall ->
                if Simulants.Player.HasFallen world && World.isSelectedScreenIdling world then
                    let world = World.playSound 1.0f Assets.DeathSound world
                    if Simulants.Title.GetExists world
                    then World.transitionScreen Simulants.Title world
                    else world
                else world

[<AutoOpen>]
module GameplayModule =

    type GameplayCommand =
        | StartPlay
        | StoppingPlay
        | StopPlay

    type GameplayDispatcher () =
        inherit ScreenDispatcher<unit, unit, GameplayCommand> ()

        static let [<Literal>] SectionName = "Section"
        static let [<Literal>] SectionCount = 16

        static let shiftEntities xShift entities world =
            Seq.fold
                (fun world (entity : Entity) ->
                    let world = entity.SetPosition (entity.GetPosition world + Vector2 (xShift, 0.0f)) world
                    entity.PropagatePhysics world)
                world
                entities

        static let createSectionFromFile filePath sectionName xShift gameplay world =
            let (section, world) = World.readLayerFromFile filePath (Some sectionName) gameplay world
            let sectionEntities = World.getEntities section world
            shiftEntities xShift sectionEntities world

        static let createSectionLayers gameplay world =
            let random = System.Random ()
            let sectionFilePaths = List.toArray Assets.SectionFilePaths
            List.fold
                (fun world i ->
                    let sectionFilePathIndex = if i = 0 then 0 else random.Next () % sectionFilePaths.Length
                    let sectionFilePath = sectionFilePaths.[sectionFilePathIndex]
                    let sectionName = SectionName + scstring i
                    let sectionXShift = 2048.0f * single i
                    createSectionFromFile sectionFilePath sectionName sectionXShift gameplay world)
                world
                [0 .. SectionCount - 1]

        static let createScene gameplay world =
            World.readLayerFromFile Assets.SceneLayerFilePath (Some Simulants.Scene.Name) gameplay world |> snd

        override dispatcher.Bindings (_, gameplay, _) =
            [gameplay.SelectEvent =>! StartPlay
             gameplay.OutgoingStartEvent =>! StoppingPlay
             gameplay.DeselectEvent =>! StopPlay]

        override dispatcher.Command (command, _, gameplay, world) =
            match command with
            | StartPlay ->
                let world = createScene gameplay world
                let world = createSectionLayers gameplay world
                World.playSong 0 1.0f Assets.DeadBlazeSong world
            | StoppingPlay ->
                World.fadeOutSong Constants.Audio.DefaultTimeToFadeOutSongMs world
            | StopPlay ->
                let sectionNames = [for i in 0 .. SectionCount - 1 do yield SectionName + scstring i]
                let layerNames = Simulants.Scene.Name :: sectionNames
                let layers = List.map (fun layerName -> gameplay / layerName) layerNames
                World.destroyLayers layers world