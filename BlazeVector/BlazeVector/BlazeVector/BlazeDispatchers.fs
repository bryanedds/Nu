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
open Nu.React
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
            let (address, bullet : Entity) = Event.unwrapAS event
            if World.isGamePlaying world then
                let bullet = Entity.setAge (bullet.Age + 1L) bullet
                let world =
                    if bullet.Age < 28L then World.setEntity address bullet world
                    else snd <| World.removeEntity address bullet world
                (Cascade, world)
            else (Cascade, world)

        let handleCollision event world =
            let (address, bullet : Entity) = Event.unwrapAS event
            if World.isGamePlaying world then
                let world = snd <| World.removeEntity address bullet world
                (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (24.0f, 24.0f)
             define? Density 0.25f
             define? Restitution 0.5f
             define? LinearDamping 0.0f
             define? GravityScale 0.0f
             define? IsBullet true
             define? CollisionExpression "Circle"
             define? SpriteImage PlayerBulletImage
             define? Age 0L]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<SpriteFacet>.Name]

        override dispatcher.Register (address, bullet, world) =
            let world = World.monitor TickEventAddress address handleTick world
            let world = World.monitor (CollisionEventAddress + address) address handleCollision world
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

        let die address (enemy : Entity) world =
            let world = snd <| World.removeEntity address enemy world
            World.playSound ExplosionSound 1.0f world

        let handleTick event world =
            let (address, enemy : Entity) = Event.unwrapAS event
            if World.isGamePlaying world then
                let world = if hasAppeared world.Camera enemy then move enemy world else world
                let world = if enemy.Health <= 0 then die address enemy world else world
                (Cascade, world)
            else (Cascade, world)

        let handleCollision event world =
            let (address, enemy : Entity, collisionData) = Event.unwrap event
            if World.isGamePlaying world then
                let collidee = World.getEntity collisionData.Collidee world
                let isBullet = Reflection.dispatchesAs typeof<BulletDispatcher> collidee.DispatcherNp
                if isBullet then
                    let enemy = Entity.setHealth (enemy.Health - 1) enemy
                    let world = World.setEntity address enemy world
                    let world = World.playSound HitSound 1.0f world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpression "Capsule"
             define? Stutter 8
             define? TileCount 6
             define? TileRun 4
             define? TileSize <| Vector2 (48.0f, 96.0f)
             define? AnimatedSpriteImage EnemyImage
             define? Health 6]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register (address, enemy, world) =
            let world =
                world |>
                World.monitor TickEventAddress address handleTick |>
                World.monitor (CollisionEventAddress + address) address handleCollision
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

        let createBullet bulletAddress (playerTransform : Transform) world =
            let bullet = World.makeEntity typeof<BulletDispatcher>.Name (Some <| Address.last bulletAddress) world
            let bullet =
                bullet |>
                    Entity.setPosition (playerTransform.Position + Vector2 (playerTransform.Size.X * 0.9f, playerTransform.Size.Y * 0.4f)) |>
                    Entity.setDepth playerTransform.Depth
            World.addEntity bulletAddress bullet world

        let propelBullet (bullet : Entity) world =
            let world = World.applyBodyLinearImpulse (Vector2 (50.0f, 0.0f)) bullet.PhysicsId world
            let world = World.playSound ShotSound 1.0f world
            (bullet, world)

        let shootBullet playerAddress (player : Entity) world =
            let bulletAddress = Address.allButLast playerAddress @+ [string <| Core.makeId ()]
            let playerTransform = Entity.getTransform player
            let (bullet, world) = createBullet bulletAddress playerTransform world
            propelBullet bullet world

        let handleSpawnBullet event world =
            let (address, player : Entity) = Event.unwrapAS event
            if World.isGamePlaying world then
                if not <| Entity.hasFallen player then
                    if world.State.TickTime % 6L = 0L then
                        let world = snd <| shootBullet address player world
                        (Cascade, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        let getLastTimeOnGround (player : Entity) world =
            if not <| World.isBodyOnGround player.PhysicsId world
            then player.LastTimeOnGroundNp
            else world.State.TickTime

        let handleMovement event world =
            let (address, player : Entity) = Event.unwrapAS event
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
                let world = World.setEntity address player world
                (Cascade, world)
            else (Cascade, world)

        let handleJump event world =
            let (address, player : Entity) = Event.unwrapAS event
            if World.isGamePlaying world then
                if  world.State.TickTime >= player.LastTimeJumpNp + 12L &&
                    world.State.TickTime <= player.LastTimeOnGroundNp + 10L then
                    let player = Entity.setLastTimeJumpNp world.State.TickTime player
                    let world = World.applyBodyLinearImpulse (Vector2 (0.0f, 18000.0f)) player.PhysicsId world
                    let world = World.playSound JumpSound 1.0f world
                    let world = World.setEntity address player world
                    (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleJumpByKeyboardKey event world =
            let keyboardKeyData = Event.unwrapD event
            match (enum<SDL.SDL_Scancode> keyboardKeyData.ScanCode, keyboardKeyData.IsRepeat) with
            | (SDL.SDL_Scancode.SDL_SCANCODE_SPACE, false) -> handleJump event world
            | _ -> (Cascade, world)

        static member FieldDefinitions =
            [define? Size <| Vector2 (48.0f, 96.0f)
             define? FixedRotation true
             define? LinearDamping 3.0f
             define? GravityScale 0.0f
             define? CollisionExpression "Capsule"
             define? Stutter 3
             define? TileCount 16
             define? TileRun 4
             define? TileSize <| Vector2 (48.0f, 96.0f)
             define? AnimatedSpriteImage PlayerImage
             define? LastTimeOnGroundNp Int64.MinValue
             define? LastTimeJumpNp Int64.MinValue]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<AnimatedSpriteFacet>.Name]

        override dispatcher.Register (address, player, world) =
            let world =
                world |>
                World.monitor TickEventAddress address handleSpawnBullet |>
                World.monitor TickEventAddress address handleMovement |>
                World.monitor DownMouseLeftEventAddress address handleJump |>
                World.monitor DownKeyboardKeyEventAddress address handleJumpByKeyboardKey
            (player, world)

[<AutoOpen>]
module StagePlayModule =

    type StagePlayDispatcher () =
        inherit GroupDispatcher ()

        let getPlayer groupAddress world =
            let playerAddress = groupAddress @+ [StagePlayerName]
            World.getEntity playerAddress world

        let adjustCamera groupAddress world =
            if World.isGamePlaying world then
                let player = getPlayer groupAddress world
                let eyeCenter = Vector2 (player.Position.X + player.Size.X * 0.5f + world.Camera.EyeSize.X * 0.33f, world.Camera.EyeCenter.Y)
                World.setCamera { world.Camera with EyeCenter = eyeCenter } world
            else world

        let handleAdjustCamera event world =
            let address = Event.unwrapA event
            (Cascade, adjustCamera address world)

        let handlePlayerFall event world =
            let address = Event.unwrapA event
            if World.isGamePlaying world then
                let player = getPlayer address world
                match World.getOptScreen TitleAddress world with
                | Some titleScreen ->
                    if Entity.hasFallen player && World.isSelectedScreenIdling world then
                        let oldWorld = world
                        let world = World.playSound DeathSound 1.0f world
                        match World.tryTransitionScreen TitleAddress titleScreen world with
                        | Some world -> (Cascade, world)
                        | None -> (Cascade, oldWorld)
                    else (Cascade, world)
                | None -> (Cascade, world)
            else (Cascade, world)

        override dispatcher.Register (address, group, world) =
            let world =
                world |>
                World.monitor TickEventAddress address handleAdjustCamera |>
                World.monitor TickEventAddress address handlePlayerFall
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

        let makeSectionFromFile fileName sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.loadGroupFromFile fileName world
            let sectionEntities = shiftEntities xShift sectionEntities
            (sectionName, (sectionGroup, sectionEntities))

        let handleStartPlay event world =
            let address = Event.unwrapA event
            let random = Random ()
            let sectionFileNames = List.toArray SectionFileNames
            let sectionDescriptors =
                [for i in 0 .. SectionCount do
                    let xShift = 2048.0f
                    let sectionFileNameIndex = if i = 0 then 0 else random.Next () % sectionFileNames.Length
                    yield makeSectionFromFile sectionFileNames.[sectionFileNameIndex] (SectionName + string i) (xShift * single i) world]
            let stagePlayDescriptor = (StagePlayName, World.loadGroupFromFile StagePlayFileName world)
            let groupDescriptors = Map.ofList <| stagePlayDescriptor :: sectionDescriptors
            let world = snd <| World.addGroups address groupDescriptors world
            let world = World.playSong DeadBlazeSong 1.0f 0 world
            (Cascade, world)

        let handleStoppingPlay _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            (Cascade, world)

        let handleStopPlay event world =
            let address = Event.unwrapA event
            let sectionNames = [for i in 0 .. SectionCount do yield SectionName + string i]
            let groupNames = StagePlayName :: sectionNames
            let groups = World.getGroups3 address groupNames world
            let world = snd <| World.removeGroups address groups world
            (Cascade, world)

        override dispatcher.Register (address, screen, world) =
            let world =
                world |>
                World.monitor (SelectEventAddress + address) address handleStartPlay |>
                World.monitor (StartOutgoingEventAddress + address) address handleStoppingPlay |>
                World.monitor (DeselectEventAddress + address) address handleStopPlay
            (screen, world)

[<AutoOpen>]
module BlazeVectorModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeVectorDispatcher () =
        inherit GameDispatcher ()