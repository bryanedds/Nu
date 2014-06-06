namespace OmniBlade
open OpenTK
open Prime
open Nu
open Nu.World

[<AutoOpen>]
module OmniDispatchersModule =

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
            let avatar = get world <| Entity.worldEntityLens avatarAddress
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler _ _ groupAddress message world =
            (message, true, adjustFieldCamera groupAddress world)

        let moveFieldAvatarHandler _ _ groupAddress message world =
            let feelerAddress = groupAddress @ [OmniConstants.FieldFeelerName]
            let feeler = get world <| Entity.worldEntityLens feelerAddress
            if feeler.IsTouched then
                let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
                let avatar = get world <| Entity.worldEntityLens avatarAddress
                let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, true, world')
            else (message, true, world)
        
        override dispatcher.Register (omniFieldGroup, address, entities, world) =
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub moveFieldAvatarHandler) world
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub adjustFieldCameraHandler) world_
            let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
            let world_ = base.Register (omniFieldGroup, address, entities, world_)
            adjustFieldCamera address world_

        override dispatcher.Unregister (omniFieldGroup, address, world) =
            let world_ = World.unsubscribe NuConstants.TickEvent address world
            let world_ = World.unsubscribe NuConstants.TickEvent address world_
            base.Unregister (omniFieldGroup, address, world_)

    type OmniBattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override dispatcher.Register (omniBattleGroup, address, entities, world) =
            let world' = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            base.Register (omniBattleGroup, address, entities, world')

        override dispatcher.Unregister (omniBattleGroup, address, world) =
            base.Unregister (omniBattleGroup, address, world)

    type OmniGameDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register (omniGame, world) =
            let dispatchers =
                Map.addMany
                    [|typeof<OmniBattleGroupDispatcher>.Name, OmniBattleGroupDispatcher () :> obj
                      typeof<OmniFieldGroupDispatcher>.Name, OmniFieldGroupDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }