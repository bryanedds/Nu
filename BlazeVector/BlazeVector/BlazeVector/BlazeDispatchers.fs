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
open Nu.Observer
open BlazeVector
open BlazeVector.BlazeConstants

[<AutoOpen>]
module BulletModule =

    type Entity with

        member entity.Age = entity?Age : int64
        static member setAge (value : int64) (entity : Entity) = entity?Age <- value

    type BulletDispatcher () =
        inherit EntityDispatcher ()

        let handleTick event world =
            let (bullet : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                let bullet = Entity.setAge (bullet.Age + 1L) bullet
                let world =
                    if bullet.Age < 28L then World.setEntity bullet address world
                    else snd <| World.removeEntity bullet address world
                (Cascade, world)
            else (Cascade, world)

        let handleCollision event world =
            let (bullet : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                let world = snd <| World.removeEntity bullet address world
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

        override dispatcher.Register (bullet, address, world) =
            let world = World.monitor handleTick TickEventAddress address world
            let world = World.monitor handleCollision (CollisionEventAddress ->>- address) address world
            (bullet, world)

[<AutoOpen>]
module EnemyModule =

    type Entity with

        member enemy.Health = enemy?Health : int
        static member setHealth (value : int) (enemy : Entity) = enemy?Health <- value

    type EnemyDispatcher () =
        inherit EntityDispatcher ()
        
        let hasAppeared camera (enemy : Entity) =
            enemy.Position.X - (camera.EyeCenter.X + camera.EyeSize.X * 0.5f) < 0.0f

        let move (enemy : Entity) world =
            let force = Vector2 (-2000.0f, -20000.0f)
            World.applyBodyForce force enemy.PhysicsId world

        let die (enemy : Entity) address world =
            let world = snd <| World.removeEntity enemy address world
            World.playSound 1.0f ExplosionSound world

        let handleTick event world =
            let (enemy : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                let world = if hasAppeared world.Camera enemy then move enemy world else world
                let world = if enemy.Health <= 0 then die enemy address world else world
                (Cascade, world)
            else (Cascade, world)

        let handleCollision event world =
            let (collisionData, enemy : Entity, address) = World.unwrapDSA event world
            if World.isGamePlaying world then
                let collidee = World.getEntity collisionData.Collidee world
                let isBullet = Entity.dispatchesAs typeof<BulletDispatcher> collidee
                if isBullet then
                    let enemy = Entity.setHealth (enemy.Health - 1) enemy
                    let world = World.setEntity enemy address world
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

        override dispatcher.Register (enemy, address, world) =
            let world =
                world |>
                World.monitor handleTick TickEventAddress address |>
                World.monitor handleCollision (CollisionEventAddress ->>- address) address
            (enemy, world)

[<AutoOpen>]
module PlayerModule =

    type Entity with

        member player.LastTimeOnGroundNp = player?LastTimeOnGroundNp : int64
        static member setLastTimeOnGroundNp (value : int64) (player : Entity) = player?LastTimeOnGroundNp <- value
        member player.LastTimeJumpNp = player?LastTimeJumpNp : int64
        static member setLastTimeJumpNp (value : int64) (player : Entity) = player?LastTimeJumpNp <- value

        static member hasFallen (player : Entity) =
            player.Position.Y < -600.0f

    type PlayerDispatcher () =
        inherit EntityDispatcher ()

        let [<Literal>] WalkForce = 8000.0f
        let [<Literal>] FallForce = -30000.0f
        let [<Literal>] ClimbForce = 12000.0f

        let createBullet (playerTransform : Transform) bulletAddress world =
            let bullet = World.makeEntity typeof<BulletDispatcher>.Name (Some <| Address.last bulletAddress) world
            let bullet =
                bullet |>
                    Entity.setPosition (playerTransform.Position + Vector2 (playerTransform.Size.X * 0.9f, playerTransform.Size.Y * 0.4f)) |>
                    Entity.setDepth playerTransform.Depth
            World.addEntity bullet bulletAddress world

        let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (Vector2 (50.0f, 0.0f)) bullet.PhysicsId world
            let world = World.playSound 1.0f ShotSound world
            (bullet, world)

        let shootBullet (player : Entity) playerAddress world =
            let bulletAddress = gatoea (eatoga playerAddress) (acstring <| Core.makeId ())
            let playerTransform = Entity.getTransform player
            let (bullet, world) = createBullet playerTransform bulletAddress world
            propelBullet bullet world

        let handleSpawnBullet event world =
            let (player : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                if not <| Entity.hasFallen player then
                    if world.State.TickTime % 6L = 0L then
                        let world = snd <| shootBullet player address world
                        (Cascade, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        let getLastTimeOnGround (player : Entity) world =
            if not <| World.isBodyOnGround player.PhysicsId world
            then player.LastTimeOnGroundNp
            else world.State.TickTime

        let handleMovement event world =
            let (player : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                let lastTimeOnGround = getLastTimeOnGround player world
                let player = Entity.setLastTimeOnGroundNp lastTimeOnGround player
                let physicsId = player.PhysicsId
                let optGroundTangent = World.getBodyOptGroundContactTangent physicsId world
                let force =
                    match optGroundTangent with
                    | Some groundTangent ->
                        let downForce = if groundTangent.Y > 0.0f then ClimbForce else 0.0f
                        Vector2.Multiply (groundTangent, Vector2 (WalkForce, downForce))
                    | None -> Vector2 (WalkForce, FallForce)
                let world = World.applyBodyForce force physicsId world
                let world = World.setEntity player address world
                (Cascade, world)
            else (Cascade, world)

        let handleJump event world =
            let (player : Entity, address) = World.unwrapSA event world
            if World.isGamePlaying world then
                if  world.State.TickTime >= player.LastTimeJumpNp + 12L &&
                    world.State.TickTime <= player.LastTimeOnGroundNp + 10L then
                    let player = Entity.setLastTimeJumpNp world.State.TickTime player
                    let world = World.applyBodyLinearImpulse (Vector2 (0.0f, 18000.0f)) player.PhysicsId world
                    let world = World.playSound 1.0f JumpSound world
                    let world = World.setEntity player address world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleJumpByKeyboardKey event world =
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

        override dispatcher.Register (player, address, world) =
            let world =
                world |>
                World.monitor handleSpawnBullet TickEventAddress address |>
                World.monitor handleMovement TickEventAddress address |>
                World.monitor handleJump MouseLeftDownEventAddress address |>
                World.monitor handleJumpByKeyboardKey KeyboardKeyDownEventAddress address
            (player, world)

[<AutoOpen>]
module StagePlayModule =

    type StagePlayDispatcher () =
        inherit GroupDispatcher ()

        let getPlayer groupAddress world =
            let playerAddress = gatoea groupAddress StagePlayerName
            World.getEntity playerAddress world

        let adjustCamera groupAddress world =
            if World.isGamePlaying world then
                let player = getPlayer groupAddress world
                let eyeCenter = Vector2 (player.Position.X + player.Size.X * 0.5f + world.Camera.EyeSize.X * 0.33f, world.Camera.EyeCenter.Y)
                World.setCamera { world.Camera with EyeCenter = eyeCenter } world
            else world

        let handleAdjustCamera event world =
            (Cascade, adjustCamera event.SubscriberAddress world)

        let handlePlayerFall event world =
            if World.isGamePlaying world then
                let player = getPlayer event.SubscriberAddress world
                if Entity.hasFallen player && World.isSelectedScreenIdling world then
                    let world = World.playSound 1.0f DeathSound world
                    let world = World.transitionScreen TitleAddress world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        override dispatcher.Register (group, address, world) =
            let world =
                world |>
                World.monitor handleAdjustCamera TickEventAddress address |>
                World.monitor handlePlayerFall TickEventAddress address
            let world = adjustCamera address world
            (group, world)

[<AutoOpen>]
module StageScreenModule =

    type StageScreenDispatcher () =
        inherit ScreenDispatcher ()

        let shiftEntities xShift entities =
            Map.map
                (fun _ (entity : Entity) -> Entity.setPosition (entity.Position + Vector2 (xShift, 0.0f)) entity)
                entities

        let makeSectionFromFile filePath sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.readGroupHierarchyFromFile filePath world
            let sectionEntities = shiftEntities xShift sectionEntities
            (sectionName, (sectionGroup, sectionEntities))

        let handleStartPlay event world =
            let random = Random ()
            let sectionFilePaths = List.toArray SectionFilePaths
            let sectionHierarchies =
                [for i in 0 .. SectionCount do
                    let xShift = 2048.0f
                    let sectionFilePathIndex = if i = 0 then 0 else random.Next () % sectionFilePaths.Length
                    yield makeSectionFromFile sectionFilePaths.[sectionFilePathIndex] (SectionName + acstring i) (xShift * single i) world]
            let stagePlayHierarchy = (StagePlayName, World.readGroupHierarchyFromFile StagePlayFilePath world)
            let groupHierarchies = Map.ofList <| stagePlayHierarchy :: sectionHierarchies
            let world = snd <| World.addGroups groupHierarchies event.SubscriberAddress world
            let world = World.playSong 0 1.0f DeadBlazeSong world
            (Cascade, world)

        let handleStoppingPlay _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            (Cascade, world)

        let handleStopPlay event world =
            let sectionNames = [for i in 0 .. SectionCount do yield SectionName + acstring i]
            let groupNames = StagePlayName :: sectionNames
            let groupMap = World.getGroupMapInScreen3 groupNames event.SubscriberAddress world
            let world = snd <| World.removeGroups groupMap event.SubscriberAddress world
            (Cascade, world)

        override dispatcher.Register (screen, address, world) =
            let world =
                world |>
                World.monitor handleStartPlay (SelectEventAddress ->>- address) address |>
                World.monitor handleStoppingPlay (OutgoingStartEventAddress ->>- address) address |>
                World.monitor handleStopPlay (DeselectEventAddress ->>- address) address
            (screen, world)

[<AutoOpen>]
module BlazeVectorModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeVectorDispatcher () =
        inherit GameDispatcher ()