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

        let tickHandler message world =
            let bullet = World.getEntity message.Subscriber world
            if world.Ticks < bullet.BirthTime + 90L then (Unhandled, world)
            else (Unhandled, World.removeEntity message.Subscriber world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, _) ->
                let world = World.removeEntity message.Subscriber world
                (Unhandled, world)
            | _ -> failwith <| "Expected CollisionData from event '" + addrToStr message.Event + "'."

        override dispatcher.MakeBodyShape (bullet : Entity) =
            CircleShape { Radius = bullet.Size.X * 0.5f; Center = Vector2.Zero }

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "PlayerBullet"; PackageName = BlazeStagesPackageName; PackageFileName = AssetGraphFileName }

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
            let world = World.observe TickEvent address (CustomSub tickHandler) world
            let world = World.observe (CollisionEvent @ address) address (CustomSub collisionHandler) world
            let bullet = bullet.SetBirthTime world.Ticks
            let world = World.setEntity address bullet world
            let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = bullet.PhysicsId; LinearImpulse = Vector2 (50.0f, 0.0f) }
            { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }

    type BlazeEnemyDispatcher () =
        inherit CharacterDispatcher ()

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
                        | None -> Vector2 (-1.0f, -2.5f) * 2000.0f
                        | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (-2000.0f, if groundTangent.Y > 0.0f then 8000.0f else 0.0f))
                    let applyForceMessage = ApplyForceMessage { PhysicsId = enemy.PhysicsId; Force = force }
                    let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                    (Unhandled, world)

        let collisionHandler message world =
            match message.Data with
            | CollisionData (_, _, colliderAddress) ->
                let isBullet =
                    match World.getOptEntity colliderAddress world with
                    | None -> true // HACK: assume is bullet if entity was just removed. TODO: implement a way to schedule simulant removal at end of frame
                    | Some collider -> Entity.dispatchesAs typeof<BlazeBulletDispatcher> collider world
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
            enemy.SetHealth 6

        override dispatcher.Register (enemy, address, world) =
            let world = base.Register (enemy, address, world)
            world |>
                World.observe TickEvent address -<| CustomSub movementHandler |>
                World.observe (CollisionEvent @ address) address -<| CustomSub collisionHandler

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "Enemy"; PackageName = BlazeStagesPackageName; PackageFileName = AssetGraphFileName }

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
            World.addEntity bulletAddress bullet world

        let spawnBulletHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                if world.Ticks % 6L <> 0L then (Unhandled, world)
                else
                    let player = World.getEntity message.Subscriber world
                    let world = createBullet player message.Subscriber world
                    (Unhandled, world)

        let movementHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                let player = World.getEntity message.Subscriber world
                let optGroundTangent = Physics.getOptGroundContactTangent player.PhysicsId world.Integrator
                let force =
                    match optGroundTangent with
                    | None -> Vector2 (1.0f, -2.5f) * 8000.0f
                    | Some groundTangent -> Vector2.Multiply (groundTangent, Vector2 (8000.0f, if groundTangent.Y > 0.0f then 12000.0f else 0.0f))
                let applyForceMessage = ApplyForceMessage { PhysicsId = player.PhysicsId; Force = force }
                let world = { world with PhysicsMessages = applyForceMessage :: world.PhysicsMessages }
                (Unhandled, world)

        let jumpHandler message world =
            if not world.Interactive then (Unhandled, world)
            else
                let player = World.getEntity message.Subscriber world
                if not <| Physics.isBodyOnGround player.PhysicsId world.Integrator then (Unhandled, world)
                else
                    let applyLinearImpulseMessage = ApplyLinearImpulseMessage { PhysicsId = player.PhysicsId; LinearImpulse = Vector2 (0.0f, 18000.0f) }
                    let world = { world with PhysicsMessages = applyLinearImpulseMessage :: world.PhysicsMessages }
                    (Unhandled, world)

        override dispatcher.Init (player, dispatcherContainer) =
            let player = base.Init (player, dispatcherContainer)
            player.SetSize <| Vector2 (48.0f, 96.0f)

        override dispatcher.Register (player, address, world) =
            let world = base.Register (player, address, world)
            world |>
                World.observe TickEvent address -<| CustomSub spawnBulletHandler |>
                World.observe TickEvent address -<| CustomSub movementHandler |>
                World.observe DownMouseRightEvent address -<| CustomSub jumpHandler

        override dispatcher.Unregister (player, address, world) =
            base.Unregister (player, address, world)

        override dispatcher.GetImageSprite () =
            { SpriteAssetName = "Player"; PackageName = BlazeStagesPackageName; PackageFileName = AssetGraphFileName }

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
            let playerAddress = groupAddress @ [StagePlayerName]
            World.getEntity playerAddress world

        let adjustCamera groupAddress world =
            let player = getPlayer groupAddress world
            let eyeCenter = Vector2 (player.Position.X + player.Size.X * 0.5f + world.Camera.EyeSize.X * 0.33f, world.Camera.EyeCenter.Y)
            { world with Camera = { world.Camera with EyeCenter = eyeCenter }}

        let adjustCameraHandler message world =
            (Unhandled, adjustCamera message.Subscriber world)

        override dispatcher.Register (group, address, entities, world) =
            let world = base.Register (group, address, entities, world)
            let world = World.observe TickEvent address (CustomSub adjustCameraHandler) world
            adjustCamera address world

    type BlazeStageScreenDispatcher () =
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

        let beginPlayHandler message world =
            let stagePlay = World.loadGroupFromFile StagePlayFileName world
            let stagePlayDescriptor = Triple.prepend StagePlayName stagePlay
            let sectionDescriptors =
                [makeSectionFromFile Section0FileName Section0Name 0.0f world
                 makeSectionFromFile Section1FileName Section1Name 2048.0f world
                 makeSectionFromFile Section2FileName Section2Name 4096.0f world
                 makeSectionFromFile Section3FileName Section3Name 6144.0f world]
            let groupDescriptors = stagePlayDescriptor :: sectionDescriptors
            let world = World.addGroups message.Subscriber groupDescriptors world
            (Unhandled, world)

        let endPlayHandler message world =
            let sectionNames = [StagePlayName; Section0Name; Section1Name; Section2Name; Section3Name]
            let world = World.removeGroups message.Subscriber sectionNames world
            (Unhandled, world)

        override dispatcher.Register (screen, address, groupDescriptors, world) =
            let world = base.Register (screen, address, groupDescriptors, world)
            world |>
                World.observe (SelectedEvent @ address) address -<| CustomSub beginPlayHandler |>
                World.observe (DeselectedEvent @ address) address -<| CustomSub endPlayHandler

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
                     typeof<BlazeStageScreenDispatcher>.Name, BlazeStageScreenDispatcher () :> obj]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }