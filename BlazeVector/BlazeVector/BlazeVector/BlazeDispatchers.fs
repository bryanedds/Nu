namespace BlazeVector
open System
open System.Collections
open OpenTK
open Microsoft.Xna
open FarseerPhysics
open FarseerPhysics.Dynamics
open Prime
open Nu

[<AutoOpen>]
module BlazeDispatchersModule =

    type Entity with

        (* bullet xfields *)
        [<XField>] member this.BirthTime with get () = this?BirthTime () : int64
        member this.SetBirthTime (value : int64) : Entity = this?BirthTime <- value

        (* enemy xfields *)
        [<XField>] member this.Health with get () = this?Health () : int
        member this.SetHealth (value : int) : Entity = this?Health <- value

    type BlazeBulletDispatcher () =
        inherit Entity2dWithSimplePhysicsAndRenderingDispatcher ()

        let launch (bullet : Entity) world =
            let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = bullet.PhysicsId; LinearImpulse = Vector2 (50.0f, 0.0f) }
            { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }

        let tickHandler message world =
            let bullet = World.getEntity message.Subscriber world
            if bullet.BirthTime + 90L > world.Ticks then (Unhandled, Running, world)
            else
                let (liveness, world) = World.removeEntityPlus message.Subscriber world
                (Unhandled, liveness, world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, _) ->
                let (liveness, world) = World.removeEntityPlus message.Subscriber world
                (Unhandled, liveness, world)
            | _ -> failwith <| "Expected CollisionData from event '" + addrToStr message.Event + "'."

        override dispatcher.MakeBodyShape (bullet : Entity) =
            CircleShape { Radius = bullet.Size.X * 0.5f; Center = Vector2.Zero }

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "PlayerBullet"; PackageName = BlazeConstants.BlazeStagesPackageName; PackageFileName = NuConstants.AssetGraphFileName }

        override dispatcher.Init (bullet, dispatcherContainer) =
            let bullet = base.Init (bullet, dispatcherContainer)
            bullet
                .SetLinearDamping(0.0f)
                .SetGravityScale(0.0f)
                .SetBirthTime(0L)
                .SetSize(Vector2 (24.0f, 24.0f))
                .SetRestitution(0.5f)
                .SetDensity(0.25f)
                .SetIsBullet(true)

        override dispatcher.Register (bullet, address, world) =
            let world = base.Register (bullet, address, world)
            let world = World.subscribe NuConstants.TickEvent address (CustomSub tickHandler) world
            let world = World.subscribe (NuConstants.CollisionEvent @ address) address (CustomSub collisionHandler) world
            let world = launch bullet world
            let bullet = bullet.SetBirthTime world.Ticks
            World.setEntity address bullet world

        override dispatcher.Unregister (bullet, address, world) =
            let world = base.Unregister (bullet, address, world)
            let world = World.unsubscribe NuConstants.TickEvent address world
            World.unsubscribe (NuConstants.CollisionEvent @ address) address world

    type BlazeEnemyDispatcher () =
        inherit CharacterDispatcher ()

        let movementHandler message world =
            if not world.Interactive then (Unhandled, Running, world)
            else
                let enemy = World.getEntity message.Subscriber world
                let hasAppeared = enemy.Position.X - (world.Camera.EyeCenter.X + world.Camera.EyeSize.X * 0.5f) < 0.0f
                if not hasAppeared then (Unhandled, Running, world)
                else
                    let optGroundTangent = Physics.getOptGroundContactTangent enemy.PhysicsId world.Integrator
                    let force =
                        match optGroundTangent with
                        | None -> Vector2 (-1.0f, -2.5f) * 2000.0f
                        | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (-2000.0f, if groundTangent.Y > 0.0f then 8000.0f else 0.0f))
                    let applyForceMessage = ApplyForceMessage { PhysicsId = enemy.PhysicsId; Force = force }
                    let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                    (Unhandled, Running, world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, colliderAddress) ->
                match World.getOptEntity colliderAddress world with
                | None -> (Unhandled, Running, world)
                | Some collider ->
                    if not <| Entity.dispatchesAs typeof<BlazeBulletDispatcher> collider world then (Unhandled, Running, world)
                    else
                        let enemy = World.getEntity message.Subscriber world
                        let enemy = enemy.SetHealth <| enemy.Health - 1
                        let world = World.setEntity message.Subscriber enemy world
                        let (liveness, world) = if enemy.Health = 0 then World.removeEntityPlus message.Subscriber world else (Running, world)
                        (Unhandled, liveness, world)
            | _ -> failwith <| "Expected CollisionData from event '" + addrToStr message.Event + "'."

        override dispatcher.Init (enemy, dispatcherContainer) =
            let enemy = base.Init (enemy, dispatcherContainer)
            enemy.SetHealth 6

        override dispatcher.Register (enemy, address, world) =
            let world = base.Register (enemy, address, world)
            let world = World.subscribe NuConstants.TickEvent address (CustomSub movementHandler) world
            World.subscribe (NuConstants.CollisionEvent @ address) address (CustomSub collisionHandler) world

        override dispatcher.Unregister (enemy, address, world) =
            let world = base.Unregister (enemy, address, world)
            let world = World.unsubscribe NuConstants.TickEvent address world
            World.unsubscribe (NuConstants.CollisionEvent @ address) address world

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "Enemy"; PackageName = BlazeConstants.BlazeStagesPackageName; PackageFileName = NuConstants.AssetGraphFileName }

        override dispatcher.GetImageOptInset (_, world) =
            let tile = (world.Ticks / 8L) % 6L
            let tileI = tile % 4L
            let tileJ = tile / 4L
            let tileX = single tileI * 48.0f
            let tileY = single tileJ * 96.0f
            let inset = Vector4 (tileX, tileY, tileX + 48.0f, tileY + 96.0f)
            Some inset

    type BlazePlayerDispatcher () =
        inherit CharacterDispatcher ()

        let createBullet (player : Entity) playerAddress world =
            let bullet = Entity.makeDefault typeof<BlazeBulletDispatcher>.Name None world
            let bullet =
                bullet
                    .SetPosition(player.Position + Vector2 (player.Size.X * 0.9f, player.Size.Y * 0.4f))
                    .SetDepth(player.Depth + 1.0f)
            let bulletAddress = List.allButLast playerAddress @ [bullet.Name]
            World.addEntityPlus bulletAddress bullet world

        let spawnBulletHandler message world =
            if not world.Interactive then (Unhandled, Running, world)
            else
                if world.Ticks % 6L <> 0L then (Unhandled, Running, world)
                else
                    let player = World.getEntity message.Subscriber world
                    let (liveness, world) = createBullet player message.Subscriber world
                    (Unhandled, liveness, world)

        let movementHandler message world =
            if not world.Interactive then (Unhandled, Running, world)
            else
                let player = World.getEntity message.Subscriber world
                let optGroundTangent = Physics.getOptGroundContactTangent player.PhysicsId world.Integrator
                let force =
                    match optGroundTangent with
                    | None -> Vector2 (1.0f, -2.5f) * 8000.0f
                    | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (8000.0f, if groundTangent.Y > 0.0f then 12000.0f else 0.0f))
                let applyForceMessage = ApplyForceMessage { PhysicsId = player.PhysicsId; Force = force }
                let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                (Unhandled, Running, world)

        let jumpHandler message world =
            if not world.Interactive then (Unhandled, Running, world)
            else
                let player = World.getEntity message.Subscriber world
                if not <| Physics.isBodyOnGround player.PhysicsId world.Integrator
                then (Unhandled, Running, world)
                else
                    let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = player.PhysicsId; LinearImpulse = Vector2 (0.0f, 18000.0f) }
                    let world = { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }
                    (Unhandled, Running, world)

        override dispatcher.Init (player, dispatcherContainer) =
            let player = base.Init (player, dispatcherContainer)
            player.SetSize <| Vector2 (48.0f, 96.0f)

        override dispatcher.Register (player, address, world) =
            let world = base.Register (player, address, world)
            world |>
                World.subscribe NuConstants.TickEvent address -<| CustomSub spawnBulletHandler |>
                World.subscribe NuConstants.TickEvent address -<| CustomSub movementHandler |>
                World.subscribe NuConstants.DownMouseRightEvent address -<| CustomSub jumpHandler

        override dispatcher.Unregister (player, address, world) =
            let world = base.Unregister (player, address, world)
            world |>
                World.unsubscribe NuConstants.TickEvent address |>
                World.unsubscribe NuConstants.TickEvent address |>
                World.unsubscribe NuConstants.DownMouseRightEvent address

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "Player"; PackageName = BlazeConstants.BlazeStagesPackageName; PackageFileName = NuConstants.AssetGraphFileName }

        override dispatcher.GetImageOptInset (_, world) =
            let tile = (world.Ticks / 3L) % 16L
            let tileI = tile % 4L
            let tileJ = tile / 4L
            let tileX = single tileI * 48.0f
            let tileY = single tileJ * 96.0f
            let inset = Vector4 (tileX, tileY, tileX + 48.0f, tileY + 96.0f)
            Some inset

    /// TODO document.
    type BlazeStagePlayDispatcher () =
        inherit GroupDispatcher ()

        let getPlayer groupAddress world =
            let playerAddress = groupAddress @ [BlazeConstants.StagePlayerName]
            World.getEntity playerAddress world

        let adjustCamera groupAddress world =
            let player = getPlayer groupAddress world
            let eyeCenter = Vector2 (player.Position.X + player.Size.X * 0.5f + world.Camera.EyeSize.X * 0.33f, world.Camera.EyeCenter.Y)
            { world with Camera = { world.Camera with EyeCenter = eyeCenter }}

        let adjustCameraHandler message world =
            (Unhandled, Running, adjustCamera message.Subscriber world)

        override dispatcher.Register (group, address, entities, world) =
            let world = base.Register (group, address, entities, world)
            let world = World.subscribe NuConstants.TickEvent address (CustomSub adjustCameraHandler) world
            adjustCamera address world

        override dispatcher.Unregister (group, address, world) =
            let world = base.Unregister (group, address, world)
            World.unsubscribe NuConstants.TickEvent address world

    type Screen with

        (* blaze stage screen dispatches *)
        member this.BeginPlay (address : Address, world : World) = this?BeginPlay (address, world) : World
        member this.EndPlay (address : Address, world : World) = this?EndPlay (address, world) : World

    type BlazeStageDispatcher () =
        inherit ScreenDispatcher ()

        let shiftEntities xShift entities world =
            List.map
                (fun (entity : Entity) ->
                    if Entity.dispatchesAs typeof<Entity2dDispatcher> entity world then entity
                    else entity.SetPosition <| entity.Position + Vector2 (xShift, 0.0f))
                entities

        let makeSectionFromFile fileName sectionName xShift world =
            let (sectionGroup, sectionEntities) = World.loadGroupFromFile fileName world
            let sectionEntities = shiftEntities xShift sectionEntities world
            (sectionName, sectionGroup, sectionEntities)

        let beginPlay message world =
            let stagePlay = World.loadGroupFromFile BlazeConstants.StagePlayFileName world
            let stagePlayDescriptor = Triple.prepend BlazeConstants.StagePlayName stagePlay
            let sectionDescriptors =
                [makeSectionFromFile BlazeConstants.Section0FileName BlazeConstants.Section0Name 0.0f world
                 makeSectionFromFile BlazeConstants.Section1FileName BlazeConstants.Section1Name 2048.0f world
                 makeSectionFromFile BlazeConstants.Section2FileName BlazeConstants.Section2Name 4096.0f world
                 makeSectionFromFile BlazeConstants.Section3FileName BlazeConstants.Section3Name 6144.0f world]
            let groupDescriptors = stagePlayDescriptor :: sectionDescriptors
            let world = World.addGroups message.Subscriber groupDescriptors world
            (Unhandled, Running, world)

        let endPlay message world =
            let world =
                World.removeGroups
                    message.Subscriber
                    [BlazeConstants.StagePlayName
                     BlazeConstants.Section0Name
                     BlazeConstants.Section1Name
                     BlazeConstants.Section2Name
                     BlazeConstants.Section3Name]
                    world
            (Unhandled, Running, world)

        override dispatcher.Register (screen, address, groupDescriptors, world) =
            let world = base.Register (screen, address, groupDescriptors, world)
            let world = World.subscribe (NuConstants.SelectedEvent @ address) address (CustomSub beginPlay) world
            World.subscribe (NuConstants.DeselectedEvent @ address) address (CustomSub endPlay) world

        override dispatcher.Unregister (screen, address, world) =
            let world = base.Unregister (screen, address, world)
            let world = World.unsubscribe (NuConstants.SelectedEvent @ address) address world
            World.unsubscribe (NuConstants.DeselectedEvent @ address) address world

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeVectorDispatcher () =
        inherit GameDispatcher ()

        override dispatcher.Register (_, world) =
            // add the BlazeVector-specific dispatchers to the world
            let dispatchers =
                Map.addMany
                    [typeof<BlazeBulletDispatcher>.Name, BlazeBulletDispatcher () :> obj
                     typeof<BlazePlayerDispatcher>.Name, BlazePlayerDispatcher () :> obj
                     typeof<BlazeEnemyDispatcher>.Name, BlazeEnemyDispatcher () :> obj
                     typeof<BlazeStagePlayDispatcher>.Name, BlazeStagePlayDispatcher () :> obj
                     typeof<BlazeStageDispatcher>.Name, BlazeStageDispatcher () :> obj]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }
