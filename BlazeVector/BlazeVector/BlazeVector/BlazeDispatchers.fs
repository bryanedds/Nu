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

        [<XField>] member this.BirthTime with get () = this?BirthTime () : int64
        member this.SetBirthTime (value : int64) : Entity = this?BirthTime <- value

    type BulletDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (bullet : Entity) -> CircleShape { Radius = bullet.Size.X * 0.5f; Center = Vector2.Zero })

        let tickHandler message world =
            let bullet = World.getEntity message.Subscriber world
            if world.Ticks < bullet.BirthTime + 28L then (Unhandled, world)
            else
                let world = World.removeEntity message.Subscriber world
                (Unhandled, world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, _) ->
                let world = World.removeEntity message.Subscriber world
                (Unhandled, world)
            | _ -> failwith <| "Expected CollisionData from event '" + addrToStr message.Event + "'."

        override dispatcher.Init (bullet, dispatcherContainer) =
            let bullet = base.Init (bullet, dispatcherContainer)
            let bullet = SimpleSpriteFacet.init bullet dispatcherContainer
            bullet
                .SetSize(Vector2 (24.0f, 24.0f))
                .SetDensity(0.25f)
                .SetRestitution(0.5f)
                .SetLinearDamping(0.0f)
                .SetGravityScale(0.0f)
                .SetIsBullet(true)
                .SetImageSprite({ SpriteAssetName = "PlayerBullet"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName })
                .SetBirthTime(0L)

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            let world = World.observe TickEvent address (CustomSub tickHandler) world
            let world = World.observe (CollisionEvent @ address) address (CustomSub collisionHandler) world
            let bullet = World.getEntity address world
            let bullet = bullet.SetBirthTime world.Ticks
            let world = World.setEntity address bullet world
            let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = bullet.PhysicsId; LinearImpulse = Vector2 (50.0f, 0.0f) }
            { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }

        override dispatcher.GetRenderDescriptors (bullet, viewAbsolute, viewRelative, _) =
            SimpleSpriteFacet.getRenderDescriptors bullet viewAbsolute viewRelative

        override dispatcher.GetQuickSize (bullet, world) =
            SimpleSpriteFacet.getQuickSize bullet world

[<AutoOpen>]
module EnemyDispatcherModule =

    type Entity with

        [<XField>] member this.Health with get () = this?Health () : int
        member this.SetHealth (value : int) : Entity = this?Health <- value

    type EnemyDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (enemy : Entity) -> CapsuleShape { Height = enemy.Size.Y * 0.5f; Radius = enemy.Size.Y * 0.25f; Center = Vector2.Zero })

        let movementHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                let enemy = World.getEntity message.Subscriber world
                let hasAppeared = enemy.Position.X - (world.Camera.EyeCenter.X + world.Camera.EyeSize.X * 0.5f) < 0.0f
                if not hasAppeared then (Unhandled, world)
                else
                    let optGroundTangent = Physics.getOptGroundContactTangent enemy.PhysicsId world.Integrator
                    let force =
                        match optGroundTangent with
                        | None -> Vector2 (-2000.0f, -30000.0f)
                        | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (-2000.0f, if groundTangent.Y > 0.0f then 8000.0f else 0.0f))
                    let applyForceMessage = ApplyForceMessage { PhysicsId = enemy.PhysicsId; Force = force }
                    let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                    (Unhandled, world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, colliderAddress) ->
                let isBullet =
                    match World.getOptEntity colliderAddress world with
                    | None -> true // HACK: assume collider is bullet if entity was just removed. TODO: implement a way to schedule simulant removal at end of frame
                    | Some collider -> Entity.dispatchesAs typeof<BulletDispatcher> collider world
                if not isBullet then (Unhandled, world)
                else
                    let enemy = World.getEntity message.Subscriber world
                    let enemy = enemy.SetHealth <| enemy.Health - 1
                    let world =
                        if enemy.Health <> 0 then World.setEntity message.Subscriber enemy world
                        else World.removeEntity message.Subscriber world 
                    (Unhandled, world)
            | _ -> failwith <| "Expected CollisionData from event '" + addrToStr message.Event + "'."

        override dispatcher.Init (enemy, dispatcherContainer) =
            let enemy = base.Init (enemy, dispatcherContainer)
            let enemy = SimpleAnimatedSpriteFacet.init enemy dispatcherContainer
            enemy
                .SetFixedRotation(true)
                .SetLinearDamping(3.0f)
                .SetGravityScale(0.0f)
                .SetStutter(8)
                .SetTileCount(6)
                .SetTileRun(4)
                .SetTileSize(Vector2 (48.0f, 96.0f))
                .SetImageSprite({ SpriteAssetName = "Enemy"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName })
                .SetHealth(6)

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            world |>
                World.observe TickEvent address -<| CustomSub movementHandler |>
                World.observe (CollisionEvent @ address) address -<| CustomSub collisionHandler

        override dispatcher.GetRenderDescriptors (enemy, viewAbsolute, viewRelative, world) =
            SimpleAnimatedSpriteFacet.getRenderDescriptors enemy viewAbsolute viewRelative world

        override dispatcher.GetQuickSize (enemy, _) =
            SimpleAnimatedSpriteFacet.getQuickSize enemy

[<AutoOpen>]
module PlayerDispatcherModule =

    type Entity with

        [<XField>] member this.LastTimeOnGround with get () = this?LastTimeOnGround () : int64
        member this.SetLastTimeOnGround (value : int64) : Entity = this?LastTimeOnGround <- value
        [<XField>] member this.LastTimeJump with get () = this?LastTimeJump () : int64
        member this.SetLastTimeJump (value : int64) : Entity = this?LastTimeJump <- value

    type PlayerDispatcher () =
        inherit SimpleBodyDispatcher
            (fun (player : Entity) -> CapsuleShape { Height = player.Size.Y * 0.5f; Radius = player.Size.Y * 0.25f; Center = Vector2.Zero })
             
        let createBullet (player : Entity) address world =
            let bullet = Entity.makeDefault typeof<BulletDispatcher>.Name None world
            let bullet =
                bullet
                    .SetPosition(player.Position + Vector2 (player.Size.X * 0.9f, player.Size.Y * 0.4f))
                    .SetDepth(player.Depth + 1.0f)
            let bulletAddress = List.allButLast address @ [bullet.Name]
            World.addEntity bulletAddress bullet world

        let spawnBulletHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                if world.Ticks % 6L <> 0L then (Unhandled, world)
                else
                    let player = World.getEntity message.Subscriber world
                    let world = createBullet player message.Subscriber world
                    (Unhandled, world)

        let getLastTimeOnGround (player : Entity) world =
            if not <| Physics.isBodyOnGround player.PhysicsId world.Integrator
            then player.LastTimeOnGround
            else world.Ticks

        let movementHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                let player = World.getEntity message.Subscriber world
                let lastTimeOnGround = getLastTimeOnGround player world
                let player = player.SetLastTimeOnGround lastTimeOnGround
                let world = World.setEntity message.Subscriber player world
                let optGroundTangent = Physics.getOptGroundContactTangent player.PhysicsId world.Integrator
                let force =
                    match optGroundTangent with
                    | None -> Vector2 (8000.0f, -30000.0f)
                    | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (8000.0f, if groundTangent.Y > 0.0f then 12000.0f else 0.0f))
                let applyForceMessage = ApplyForceMessage { PhysicsId = player.PhysicsId; Force = force }
                let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                (Unhandled, world)

        let jumpHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                let player = World.getEntity message.Subscriber world
                if  world.Ticks < player.LastTimeJump + 10L ||
                    world.Ticks > player.LastTimeOnGround + 10L then
                    (Unhandled, world)
                else
                    let player = player.SetLastTimeJump world.Ticks
                    let world = World.setEntity message.Subscriber player world
                    let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = player.PhysicsId; LinearImpulse = Vector2 (0.0f, 18000.0f) }
                    let world = { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }
                    (Unhandled, world)

        override dispatcher.Init (player, dispatcherContainer) =
            let player = base.Init (player, dispatcherContainer)
            let player = SimpleAnimatedSpriteFacet.init player dispatcherContainer
            player
                .SetFixedRotation(true)
                .SetLinearDamping(3.0f)
                .SetGravityScale(0.0f)
                .SetStutter(3)
                .SetTileCount(16)
                .SetTileRun(4)
                .SetTileSize(Vector2 (48.0f, 96.0f))
                .SetImageSprite({ SpriteAssetName = "Player"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName })
                .SetLastTimeOnGround(Int64.MinValue)
                .SetLastTimeJump(Int64.MinValue)

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            world |>
                World.observe TickEvent address -<| CustomSub spawnBulletHandler |>
                World.observe TickEvent address -<| CustomSub movementHandler |>
                World.observe DownMouseRightEvent address -<| CustomSub jumpHandler

        override dispatcher.GetRenderDescriptors (player, viewAbsolute, viewRelative, world) =
            SimpleAnimatedSpriteFacet.getRenderDescriptors player viewAbsolute viewRelative world

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

        let adjustCameraHandler message world =
            (Unhandled, adjustCamera message.Subscriber world)

        let playerFallHandler message world =
            let player = getPlayer message.Subscriber world
            let world = if player.Position.Y > -700.0f then world else World.transitionScreen TitleAddress world
            (Unhandled, adjustCamera message.Subscriber world)

        override dispatcher.Register (address, entities, world) =
            let world = base.Register (address, entities, world)
            let world =
                world |>
                World.observe TickEvent address -<| CustomSub adjustCameraHandler |>
                World.observe TickEvent address -<| CustomSub playerFallHandler
            adjustCamera address world

[<AutoOpen>]
module StageScreenModule =

    type StageScreenDispatcher () =
        inherit ScreenDispatcher ()

        let shiftEntities xShift entities world =
            List.map
                (fun (entity : Entity) ->
                    if not <| Entity.dispatchesAs typeof<Entity2dDispatcher> entity world then entity
                    else entity.SetPosition <| entity.Position + Vector2 (xShift, 0.0f))
                entities

        let makeSectionFromFile fileName sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.loadGroupFromFile fileName world
            let sectionEntities = shiftEntities xShift sectionEntities world
            (sectionName, sectionGroup, sectionEntities)

        let startPlayHandler message world =
            let stagePlay = World.loadGroupFromFile StagePlayFileName world
            let stagePlayDescriptor = Triple.prepend StagePlayName stagePlay
            let sectionDescriptors =
                [makeSectionFromFile Section0FileName Section0Name 0.0f world
                 makeSectionFromFile Section1FileName Section1Name 2048.0f world
                 makeSectionFromFile Section2FileName Section2Name 4096.0f world
                 makeSectionFromFile Section3FileName Section3Name 6144.0f world]
            let groupDescriptors = stagePlayDescriptor :: sectionDescriptors
            let world = World.addGroups message.Subscriber groupDescriptors world
            let gameSong = { SongAssetName = "DeadBlaze"; PackageName = StagePackageName; PackageFileName = AssetGraphFileName }
            let playSongMessage = PlaySong { Song = gameSong; TimeToFadeOutSongMs = 0 }
            let world = { world with AudioMessages = playSongMessage :: world.AudioMessages }
            (Unhandled, world)

        let stoppingPlayHandler _ world =
            let world = { world with AudioMessages = FadeOutSong DefaultTimeToFadeOutSongMs :: world.AudioMessages }
            (Unhandled, world)

        let stopPlayHandler message world =
            let sectionNames = [StagePlayName; Section0Name; Section1Name; Section2Name; Section3Name]
            let world = World.removeGroups message.Subscriber sectionNames world
            (Unhandled, world)

        override dispatcher.Register (address, groupDescriptors, world) =
            let world = base.Register (address, groupDescriptors, world)
            world |>
                World.observe (SelectEvent @ address) address -<| CustomSub startPlayHandler |>
                World.observe (StartOutgoingEvent @ address) address -<| CustomSub stoppingPlayHandler |>
                World.observe (DeselectEvent @ address) address -<| CustomSub stopPlayHandler

[<AutoOpen>]
module BlazeVectorDispatcherModule =

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeVectorDispatcher () =
        inherit GameDispatcher ()

        override dispatcher.Register world =
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