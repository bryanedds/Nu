namespace BlazeVector
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module BlazeDispatchersModule =

    /// TODO document.
    type BlazeStageGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustStageCamera (groupAddress : Address) world =
            let avatarAddress = groupAddress @ [BlazeConstants.StageAvatarName]
            let avatar = get world <| Entity.worldEntityLens avatarAddress
            let camera = { world.Camera with EyeCenter = Vector2 (avatar.Position.X + avatar.Size.X * 0.5f, world.Camera.EyeCenter.Y) }
            { world with Camera = camera }

        let adjustStageCameraHandler _ _ groupAddress message world =
            (message, true, adjustStageCamera groupAddress world)

        let moveStageAvatarHandler _ _ (groupAddress : Address) message world =
            let avatarAddress = groupAddress @ [BlazeConstants.StageAvatarName]
            let avatar = get world <| Entity.worldEntityLens avatarAddress
            let impulseVector = Vector2 (1.0f, 0.0f)
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        
        override dispatcher.Register (omniFieldGroup, address, entities, world) =
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub moveStageAvatarHandler) world
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub adjustStageCameraHandler) world_
            let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
            let world_ = base.Register (omniFieldGroup, address, entities, world_)
            adjustStageCamera address world_

        override dispatcher.Unregister (omniFieldGroup, address, world) =
            let world_ = World.unsubscribe NuConstants.TickEvent address world
            let world_ = World.unsubscribe NuConstants.TickEvent address world_
            base.Unregister (omniFieldGroup, address, world_)

    /// The custom type for BlazeVector's game dispatcher.
    /// Currently just a placeholder as it doesn't yet have any special implementation.
    type BlazeGameDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register (blazeGame, world) =
            // add the BlazeVector-specific dispatchers to the world
            { world with Dispatchers = Map.add typeof<BlazeStageGroupDispatcher>.Name (BlazeStageGroupDispatcher () :> obj) world.Dispatchers }