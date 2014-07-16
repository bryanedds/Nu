namespace BlazeVector
open System
open System.Collections
open OpenTK
open Microsoft.Xna
open FarseerPhysics
open FarseerPhysics.Dynamics
open Prime
open Nu
open Nu.NuConstants
open BlazeVector
open BlazeVector.BlazeConstants

[<AutoOpen>]
module BulletDispatcherModule =

    type Entity with

        member entity.BirthTime with get () = entity?BirthTime () : int64
        static member setBirthTime (value : int64) (entity : Entity) : Entity = entity?BirthTime <- value

    type BulletDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (bullet : Entity) -> CircleShape { Radius = bullet.Size.X * 0.5f; Center = Vector2.Zero })

        let tickHandler event world =
            let bullet = World.getEntity event.Subscriber world
            if world.Ticks = bullet.BirthTime + 28L then
                let world = World.removeEntity event.Subscriber world
                (Unhandled, world)
            else (Unhandled, world)

        let collisionHandler event world =
            let world = World.removeEntity event.Subscriber world
            (Unhandled, world)

        override dispatcher.Init (bullet, dispatcherContainer) =
            let bullet = base.Init (bullet, dispatcherContainer)
            let bullet = SimpleSpriteFacet.init bullet dispatcherContainer
            bullet |>
                Entity.setSize -<| Vector2 (24.0f, 24.0f) |>
                Entity.setDensity 0.25f |>
                Entity.setRestitution 0.5f |>
                Entity.setLinearDamping 0.0f |>
                Entity.setGravityScale 0.0f |>
                Entity.setIsBullet true |>
                Entity.setImageSprite { SpriteAssetName = "PlayerBullet"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setBirthTime Int64.MinValue

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            let world = World.observe TickEventName address (CustomSub tickHandler) world
            let world = World.observe (CollisionEventName @ address) address (CustomSub collisionHandler) world
            let bullet = World.getEntity address world
            let bullet = Entity.setBirthTime world.Ticks bullet
            let world = World.setEntity address bullet world
            World.applyLinearImpulse (Vector2 (50.0f, 0.0f)) (Entity.getPhysicsId bullet) world

        override dispatcher.GetRenderDescriptors (bullet, world) =
            SimpleSpriteFacet.getRenderDescriptors bullet Relative world

        override dispatcher.GetQuickSize (bullet, world) =
            SimpleSpriteFacet.getQuickSize bullet world

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        member entity.Health with get () = entity?Health () : int
        static member setHealth (value : int) (entity : Entity) : Entity = entity?Health <- value

    type EnemyDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (enemy : Entity) -> CapsuleShape { Height = enemy.Size.Y * 0.5f; Radius = enemy.Size.Y * 0.25f; Center = Vector2.Zero })

        let hasAppeared (entity : Entity) (world : World) =
            entity.Position.X - (world.Camera.EyeCenter.X + world.Camera.EyeSize.X * 0.5f) < 0.0f

        let move enemy world =
            let physicsId = Entity.getPhysicsId enemy
            let optGroundTangent = Physics.getOptGroundContactTangent physicsId world.Integrator
            let force =
                match optGroundTangent with
                | None -> Vector2 (-2000.0f, -30000.0f)
                | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (-2000.0f, if groundTangent.Y > 0.0f then 8000.0f else 0.0f))
            World.applyForce force physicsId world

        let die address world =
            let world = World.removeEntity address world
            World.playSound ExplosionSound 1.0f world

        let tickHandler event world =
            if World.gamePlaying world then
                let enemy = World.getEntity event.Subscriber world
                let world = if hasAppeared enemy world then move enemy world else world
                let world = if enemy.Health <= 0 then die event.Subscriber world else world
                (Unhandled, world)
            else (Unhandled, world)

        let collisionHandler event world =
            let collisionData = EventData.toEntityCollisionData event.Data
            let collidee = World.getEntity collisionData.Collidee world
            let isBullet = Entity.dispatchesAs typeof<BulletDispatcher> collidee world
            if isBullet then
                let world = World.withEntity (fun enemy -> Entity.setHealth (enemy.Health - 1) enemy) event.Subscriber world
                let world = World.playSound HitSound 1.0f world
                (Unhandled, world)
            else (Unhandled, world)

        override dispatcher.Init (enemy, dispatcherContainer) =
            let enemy = base.Init (enemy, dispatcherContainer)
            let enemy = SimpleAnimatedSpriteFacet.init enemy dispatcherContainer
            enemy |>
                Entity.setFixedRotation true |>
                Entity.setLinearDamping 3.0f |>
                Entity.setGravityScale 0.0f |>
                Entity.setStutter 8 |>
                Entity.setTileCount 6 |>
                Entity.setTileRun 4 |>
                Entity.setTileSize -<| Vector2 (48.0f, 96.0f) |>
                Entity.setImageSprite { SpriteAssetName = "Enemy"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setHealth 6

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            world |>
                World.observe TickEventName address -<| CustomSub tickHandler |>
                World.observe (CollisionEventName @ address) address -<| CustomSub collisionHandler

        override dispatcher.GetRenderDescriptors (enemy, world) =
            SimpleAnimatedSpriteFacet.getRenderDescriptors enemy Relative world

        override dispatcher.GetQuickSize (enemy, _) =
            SimpleAnimatedSpriteFacet.getQuickSize enemy

[<AutoOpen>]
module PlayerDispatcherModule =

    type Entity with

        member entity.LastTimeOnGround with get () = entity?LastTimeOnGround () : int64
        static member setLastTimeOnGround (value : int64) (entity : Entity) : Entity = entity?LastTimeOnGround <- value
        member entity.LastTimeJump with get () = entity?LastTimeJump () : int64
        static member setLastTimeJump (value : int64) (entity : Entity) : Entity = entity?LastTimeJump <- value
        
        static member hasFallen (entity : Entity) =
            entity.Position.Y < -600.0f

    type PlayerDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (player : Entity) -> CapsuleShape { Height = player.Size.Y * 0.5f; Radius = player.Size.Y * 0.25f; Center = Vector2.Zero })

        let createBullet (player : Entity) address world =
            let bullet = Entity.makeDefault typeof<BulletDispatcher>.Name None world
            let bullet =
                bullet |>
                    Entity.setPosition (player.Position + Vector2 (player.Size.X * 0.9f, player.Size.Y * 0.4f)) |>
                    Entity.setDepth player.Depth
            let bulletAddress = List.allButLast address @ [bullet.Name]
            World.addEntity bulletAddress bullet world

        let spawnBulletHandler event world =
            if World.gamePlaying world then
                let player = World.getEntity event.Subscriber world
                if not <| Entity.hasFallen player then
                    if world.Ticks % 6L = 0L then
                        let player = World.getEntity event.Subscriber world
                        let world = createBullet player event.Subscriber world
                        let world = World.playSound ShotSound 1.0f world
                        (Unhandled, world)
                    else (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        let getLastTimeOnGround (player : Entity) world =
            let physicsId = Entity.getPhysicsId player
            if not <| Physics.isBodyOnGround physicsId world.Integrator
            then player.LastTimeOnGround
            else world.Ticks

        let movementHandler event world =
            if World.gamePlaying world then
                let player = World.getEntity event.Subscriber world
                let lastTimeOnGround = getLastTimeOnGround player world
                let player = Entity.setLastTimeOnGround lastTimeOnGround player
                let world = World.setEntity event.Subscriber player world
                let physicsId = Entity.getPhysicsId player
                let optGroundTangent = Physics.getOptGroundContactTangent physicsId world.Integrator
                let force =
                    match optGroundTangent with
                    | None -> Vector2 (8000.0f, -30000.0f)
                    | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (8000.0f, if groundTangent.Y > 0.0f then 12000.0f else 0.0f))
                let world = World.applyForce force physicsId world
                (Unhandled, world)
            else (Unhandled, world)

        let jumpHandler event world =
            if World.gamePlaying world then
                let player = World.getEntity event.Subscriber world
                if  world.Ticks >= player.LastTimeJump + 12L &&
                    world.Ticks <= player.LastTimeOnGround + 10L then
                    let player = Entity.setLastTimeJump world.Ticks player
                    let world = World.setEntity event.Subscriber player world
                    let world = World.applyLinearImpulse (Vector2 (0.0f, 18000.0f)) (Entity.getPhysicsId player) world
                    let world = World.playSound JumpSound 1.0f world
                    (Unhandled, world)
                else (Unhandled, world)
            else (Unhandled, world)

        override dispatcher.Init (player, dispatcherContainer) =
            let player = base.Init (player, dispatcherContainer)
            let player = SimpleAnimatedSpriteFacet.init player dispatcherContainer
            player |>
                Entity.setFixedRotation true |>
                Entity.setLinearDamping 3.0f |>
                Entity.setGravityScale 0.0f |>
                Entity.setStutter 3 |>
                Entity.setTileCount 16 |>
                Entity.setTileRun 4 |>
                Entity.setTileSize -<| Vector2 (48.0f, 96.0f) |>
                Entity.setImageSprite { SpriteAssetName = "Player"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName } |>
                Entity.setLastTimeOnGround Int64.MinValue |>
                Entity.setLastTimeJump Int64.MinValue

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            world |>
                World.observe TickEventName address -<| CustomSub spawnBulletHandler |>
                World.observe TickEventName address -<| CustomSub movementHandler |>
                World.observe DownMouseLeftEventName address -<| CustomSub jumpHandler

        override dispatcher.GetRenderDescriptors (player, world) =
            SimpleAnimatedSpriteFacet.getRenderDescriptors player Relative world

        override dispatcher.GetQuickSize (player, _) =
            SimpleAnimatedSpriteFacet.getQuickSize player

[<AutoOpen>]
module StagePlayDispatcherModule =

    type StagePlayDispatcher () =
        inherit GroupDispatcher ()

        let getPlayer groupAddress world =
            let playerAddress = groupAddress @ [StagePlayerName]
            World.getEntity playerAddress world

        let adjustCamera groupAddress world =
            let player = getPlayer groupAddress world
            let eyeCenter = Vector2 (player.Position.X + player.Size.X * 0.5f + world.Camera.EyeSize.X * 0.33f, world.Camera.EyeCenter.Y)
            { world with Camera = { world.Camera with EyeCenter = eyeCenter }}

        let adjustCameraHandler event world =
            (Unhandled, adjustCamera event.Subscriber world)

        let playerFallHandler event world =
            let player = getPlayer event.Subscriber world
            if Entity.hasFallen player && (World.getSelectedScreen world).State = IdlingState then
                let world = World.playSound DeathSound 1.0f world
                let world = World.transitionScreen TitleAddress world
                (Unhandled, world)
            else (Unhandled, world)

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            let world =
                world |>
                World.observe TickEventName address -<| CustomSub adjustCameraHandler |>
                World.observe TickEventName address -<| CustomSub playerFallHandler
            adjustCamera address world

[<AutoOpen>]
module StageScreenModule =

    type StageScreenDispatcher () =
        inherit ScreenDispatcher ()

        let anonymizeEntities entities =
            List.map
                (fun (entity : Entity) -> let id = NuCore.makeId () in { entity with Id = id; Name = string id })
                entities

        let shiftEntities xShift entities world =
            List.map
                (fun (entity : Entity) ->
                    if Entity.dispatchesAs typeof<Entity2dDispatcher> entity world
                    then Entity.setPosition (entity.Position + Vector2 (xShift, 0.0f)) entity
                    else entity)
                entities

        let makeSectionFromFile fileName sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.loadGroupFromFile fileName world
            let sectionEntities = anonymizeEntities sectionEntities
            let sectionEntities = shiftEntities xShift sectionEntities world
            (sectionName, sectionGroup, sectionEntities)

        let startPlayHandler event world =
            let random = Random ()
            let sectionFileNames = List.toArray SectionFileNames
            let sectionDescriptors =
                [for i in 0 .. SectionCount do
                    let xShift = 2048.0f
                    let sectionFileNameIndex = if i = 0 then 0 else random.Next () % sectionFileNames.Length
                    yield makeSectionFromFile sectionFileNames.[sectionFileNameIndex] (SectionName + string i) (xShift * single i) world]
            let stagePlayDescriptor = Triple.prepend StagePlayName <| World.loadGroupFromFile StagePlayFileName world
            let groupDescriptors = stagePlayDescriptor :: sectionDescriptors
            let world = World.addGroups event.Subscriber groupDescriptors world
            let world = World.playSong DeadBlazeSong 1.0f 0 world
            (Unhandled, world)

        let stoppingPlayHandler _ world =
            let world = World.fadeOutSong DefaultTimeToFadeOutSongMs world
            (Unhandled, world)

        let stopPlayHandler event world =
            let sectionNames = [for i in 0 .. SectionCount do yield SectionName + string i]
            let groupNames = StagePlayName :: sectionNames
            let world = World.removeGroups event.Subscriber groupNames world
            (Unhandled, world)

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            world |>
                World.observe (SelectEventName @ address) address -<| CustomSub startPlayHandler |>
                World.observe (StartOutgoingEventName @ address) address -<| CustomSub stoppingPlayHandler |>
                World.observe (DeselectEventName @ address) address -<| CustomSub stopPlayHandler

[<AutoOpen>]
module BlazeVectorDispatcherModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeVectorDispatcher () =
        inherit GameDispatcher ()

        override dispatcher.Register world =
            let world = base.Register world
            // add the BlazeVector-specific dispatchers to the world
            let dispatchers =
                Map.addMany
                    [typeof<BulletDispatcher>.Name, BulletDispatcher () :> obj
                     typeof<PlayerDispatcher>.Name, PlayerDispatcher () :> obj
                     typeof<EnemyDispatcher>.Name, EnemyDispatcher () :> obj
                     typeof<StagePlayDispatcher>.Name, StagePlayDispatcher () :> obj
                     typeof<StageScreenDispatcher>.Name, StageScreenDispatcher () :> obj]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }