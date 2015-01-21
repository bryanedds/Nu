namespace BlazeVector
open System
open System.Collections
open OpenTK
open SDL2
open FarseerPhysics
open FarseerPhysics.Dynamics
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
open BlazeVector
open BlazeVector.BlazeConstants

[<AutoOpen>]
module BulletModule =

    type Entity with
    
        member this.GetAge world : int64 = (this.GetXtension world)?Age
        member this.SetAge (value : int64) world = this.UpdateXtension (fun xtension -> xtension?Age <- value) world

    type BulletDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] BulletLifetime = 27L

        static let handleTick event world =
            let bullet = event.Subscriber : Entity
            if World.isGamePlaying world then
                let world = bullet.SetAge (bullet.GetAge world + 1L) world
                let world =
                    if bullet.GetAge world > BulletLifetime
                    then World.destroyEntity bullet world
                    else world
                (Cascade, world)
            else (Cascade, world)

        static let handleCollision event world =
            let bullet = event.Subscriber : Entity
            if World.isGamePlaying world then
                let world = World.destroyEntity bullet world
                (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (24.0f, 24.0f)
             define? Density 0.25f
             define? Restitution 0.5f
             define? LinearDamping 0.0f
             define? GravityScale 0.0f
             define? IsBullet true
             define? CollisionExpr "Circle"
             define? SpriteImage PlayerBulletImage
             define? Age 0L]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

        override dispatcher.Register bullet world =
            world |>
                World.monitor handleTick TickEventAddress bullet |>
                World.monitor handleCollision (CollisionEventAddress ->>- bullet.EntityAddress) bullet

[<AutoOpen>]
module EnemyModule =

    type Entity with
    
        member this.GetHealth world : int = (this.GetXtension world)?Health
        member this.SetHealth (value : int) world = this.UpdateXtension (fun xtension -> xtension?Health <- value) world
        
        member this.HasAppeared world =
            let camera = world.State.Camera
            (this.GetPosition world).X - (camera.EyeCenter.X + camera.EyeSize.X * 0.5f) < 0.0f

    type EnemyDispatcher () =
        inherit EntityDispatcher ()
        
        static let move (enemy : Entity) world =
            let force = Vector2 (-2000.0f, -20000.0f)
            World.applyBodyForce force (enemy.GetPhysicsId world) world

        static let die (enemy : Entity) world =
            let world = World.destroyEntity enemy world
            World.playSound 1.0f ExplosionSound world

        static let handleTick event world =
            let enemy = event.Subscriber : Entity
            if World.isGamePlaying world then
                let world = if enemy.HasAppeared world then move enemy world else world
                let world = if enemy.GetHealth world <= 0 then die enemy world else world
                (Cascade, world)
            else (Cascade, world)

        static let handleCollision event world =
            let enemy = event.Subscriber : Entity
            if World.isGamePlaying world then
                let collidee = event.Data.Collidee
                let isBullet = collidee.DispatchesAs typeof<BulletDispatcher> world
                if isBullet then
                    let world = enemy.SetHealth (enemy.GetHealth world - 1) world
                    let world = World.playSound 1.0f HitSound world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpr "Capsule"
             define? TileCount 6
             define? TileRun 4
             define? TileSize <| Vector2 (48.0f, 96.0f)
             define? AnimationStutter 8L
             define? AnimationSheet EnemyImage
             define? Health 6]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register enemy world =
            world |>
                World.monitor handleTick TickEventAddress enemy |>
                World.monitor handleCollision (CollisionEventAddress ->>- enemy.EntityAddress) enemy

[<AutoOpen>]
module PlayerModule =

    type Entity with
    
        member this.GetLastTimeOnGroundNp world : int64 = (this.GetXtension world)?LastTimeOnGroundNp
        member this.SetLastTimeOnGroundNp (value : int64) world = this.UpdateXtension (fun xtension -> xtension?LastTimeOnGroundNp <- value) world
        member this.GetLastTimeJumpNp world : int64 = (this.GetXtension world)?LastTimeJumpNp
        member this.SetLastTimeJumpNp (value : int64) world = this.UpdateXtension (fun xtension -> xtension?LastTimeJumpNp <- value) world
        member this.HasFallen world = (this.GetPosition world).Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher ()

        static let [<Literal>] WalkForce = 8000.0f
        static let [<Literal>] FallForce = -30000.0f
        static let [<Literal>] ClimbForce = 12000.0f

        static let createBullet (playerTransform : Transform) (group : Group) world =
            let (bullet, world) = World.createEntity typeof<BulletDispatcher>.Name None group world
            let bulletPosition = playerTransform.Position + Vector2 (playerTransform.Size.X * 0.9f, playerTransform.Size.Y * 0.4f)
            let world = bullet.SetPosition bulletPosition world
            let world = bullet.SetDepth playerTransform.Depth world
            let world = World.propagatePhysics bullet world
            (bullet, world)

        static let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (Vector2 (50.0f, 0.0f)) (bullet.GetPhysicsId world) world
            World.playSound 1.0f ShotSound world

        static let shootBullet (player : Entity) world =
            let playerTransform = player.GetTransform world
            let playerGroup = Group.proxy <| eatoga player.EntityAddress
            let (bullet, world) = createBullet playerTransform playerGroup world
            propelBullet bullet world

        static let handleSpawnBullet event world =
            let player = event.Subscriber : Entity
            if World.isGamePlaying world then
                if not <| player.HasFallen world then
                    if world.State.TickTime % 6L = 0L then
                        let world = shootBullet player world
                        (Cascade, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static let getLastTimeOnGround (player : Entity) world =
            if not <| World.bodyOnGround (player.GetPhysicsId world) world
            then player.GetLastTimeOnGroundNp world
            else world.State.TickTime

        static let handleMovement event world =
            let player = event.Subscriber : Entity
            if World.isGamePlaying world then
                let lastTimeOnGround = getLastTimeOnGround player world
                let world = player.SetLastTimeOnGroundNp lastTimeOnGround world
                let physicsId = player.GetPhysicsId world
                let optGroundTangent = World.getBodyOptGroundContactTangent physicsId world
                let force =
                    match optGroundTangent with
                    | Some groundTangent ->
                        let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                        Vector2.Multiply (groundTangent, Vector2 (WalkForce, downForce))
                    | None -> Vector2 (WalkForce, FallForce)
                let world = World.applyBodyForce force physicsId world
                (Cascade, world)
            else (Cascade, world)

        static let handleJump event world =
            let player = event.Subscriber : Entity
            if World.isGamePlaying world then
                if  world.State.TickTime >= player.GetLastTimeJumpNp world + 12L &&
                    world.State.TickTime <= player.GetLastTimeOnGroundNp world + 10L then
                    let world = player.SetLastTimeJumpNp world.State.TickTime world
                    let world = World.applyBodyLinearImpulse (Vector2 (0.0f, 18000.0f)) (player.GetPhysicsId world) world
                    let world = World.playSound 1.0f JumpSound world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleJumpByKeyboardKey event world =
            if World.isSelectedScreenIdling world then
                match (enum<SDL.SDL_Scancode> event.Data.ScanCode, event.Data.Repeated) with
                | (SDL.SDL_Scancode.SDL_SCANCODE_SPACE, false) -> handleJump event world
                | _ -> (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpr "Capsule"
             define? TileCount 16
             define? TileRun 4
             define? TileSize <| Vector2 (48.0f, 96.0f)
             define? AnimationStutter 3L
             define? AnimationSheet PlayerImage
             define? LastTimeOnGroundNp Int64.MinValue
             define? LastTimeJumpNp Int64.MinValue]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register player world =
            world |>
                World.monitor handleSpawnBullet TickEventAddress player |>
                World.monitor handleMovement TickEventAddress player |>
                World.monitor handleJump MouseLeftDownEventAddress player |>
                World.monitor handleJumpByKeyboardKey KeyboardKeyDownEventAddress player

[<AutoOpen>]
module StagePlayModule =

    type StagePlayDispatcher () =
        inherit GroupDispatcher ()

        static let getPlayer group =
            Entity.proxy <| gatoea group.GroupAddress StagePlayerName

        static let adjustCamera address world =
            if World.isGamePlaying world then
                World.updateCamera
                    (fun camera -> 
                        let player = getPlayer address
                        let playerPosition = player.GetPosition world
                        let playerSize = player.GetSize world
                        let eyeCenter = Vector2 (playerPosition.X + playerSize.X * 0.5f + camera.EyeSize.X * 0.33f, camera.EyeCenter.Y)
                        { camera with EyeCenter = eyeCenter })
                    world
            else world

        static let handleAdjustCamera event world =
            (Cascade, adjustCamera event.Subscriber world)

        static let handlePlayerFall event world =
            if World.isGamePlaying world then
                let player = getPlayer event.Subscriber
                if player.HasFallen world && World.isSelectedScreenIdling world then
                    let world = World.playSound 1.0f DeathSound world
                    let world = World.transitionScreen Title world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        override dispatcher.Register group world =
            world |>
                World.monitor handleAdjustCamera TickEventAddress group |>
                World.monitor handlePlayerFall TickEventAddress group// |>
                //adjustCamera group

[<AutoOpen>]
module StageScreenModule =

    type StageScreenDispatcher () =
        inherit ScreenDispatcher ()

        static let [<Literal>] SectionXShift = 2048.0f

        static let shiftEntities xShift entities world =
            Seq.fold
                (fun world (entity : Entity) ->
                    let world = entity.SetPosition (entity.GetPosition world + Vector2 (xShift, 0.0f)) world
                    World.propagatePhysics entity world)
                world
                entities

        static let createStageSectionFromFile filePath sectionName xShift world =
            let (section, world) = World.readGroupFromFile filePath (Some sectionName) Stage world
            let sectionEntities = World.getEntities section world
            shiftEntities xShift sectionEntities world

        static let createStageSections world =
            let random = Random ()
            let sectionFilePaths = List.toArray SectionFilePaths
            List.fold
                (fun world i ->
                    let sectionFilePathIndex = if i = 0 then 0 else random.Next () % sectionFilePaths.Length
                    let sectionFilePath = sectionFilePaths.[sectionFilePathIndex]
                    let sectionName = SectionName + acstring i
                    let sectionXShift = SectionXShift * single i
                    createStageSectionFromFile sectionFilePath sectionName sectionXShift world)
                world
                [0 .. SectionCount - 1]

        static let createStagePlay world =
            snd <| World.readGroupFromFile StagePlayFilePath (Some StagePlayName) Stage world

        static let handleStartPlay _ world =
            let world = createStageSections world
            let world = createStagePlay world
            let world = World.playSong 0 1.0f DeadBlazeSong world
            (Cascade, world)

        static let handleStoppingPlay _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            (Cascade, world)

        static let handleStopPlay event world =
            let screen = event.Subscriber : Screen
            let sectionNames = [for i in 0 .. SectionCount - 1 do yield SectionName + acstring i]
            let groupNames = StagePlayName :: sectionNames
            let groups = List.map (fun groupName -> Group.proxy <| satoga screen.ScreenAddress groupName) groupNames
            let world = World.destroyGroups groups world
            (Cascade, world)

        override dispatcher.Register screen world =
            world |>
                World.monitor handleStartPlay (SelectEventAddress ->>- screen.ScreenAddress) screen |>
                World.monitor handleStoppingPlay (OutgoingStartEventAddress ->>- screen.ScreenAddress) screen |>
                World.monitor handleStopPlay (DeselectEventAddress ->>- screen.ScreenAddress) screen