namespace OmniBlade
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module OmniDispatchersModule =

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
            let avatar = get world <| World.worldEntity avatarAddress
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler _ _ groupAddress _ world =
            (true, Unhandled, adjustFieldCamera groupAddress world)

        let moveFieldAvatarHandler _ _ groupAddress _ world =
            let feelerAddress = groupAddress @ [OmniConstants.FieldFeelerName]
            let feeler = get world <| World.worldEntity feelerAddress
            if feeler.IsTouched then
                let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
                let avatar = get world <| World.worldEntity avatarAddress
                let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let applyLinearImpulseMessage = { PhysicsId = avatar.PhysicsId; LinearImpulse = impulseVector }
                let world = { world with PhysicsMessages = ApplyLinearImpulseMessage applyLinearImpulseMessage :: world.PhysicsMessages }
                (true, Unhandled, world)
            else (true, Unhandled, world)
        
        override dispatcher.Register (omniFieldGroup, address, entities, world) =
            let world = World.subscribe NuConstants.TickEvent address (CustomSub moveFieldAvatarHandler) world
            let world = World.subscribe NuConstants.TickEvent address (CustomSub adjustFieldCameraHandler) world
            let world = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            let world = base.Register (omniFieldGroup, address, entities, world)
            adjustFieldCamera address world

        override dispatcher.Unregister (omniFieldGroup, address, world) =
            let world = World.unsubscribe NuConstants.TickEvent address world
            let world = World.unsubscribe NuConstants.TickEvent address world
            base.Unregister (omniFieldGroup, address, world)

    type OmniBattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override dispatcher.Register (omniBattleGroup, address, entities, world) =
            let world = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            base.Register (omniBattleGroup, address, entities, world)

        override dispatcher.Unregister (omniBattleGroup, address, world) =
            base.Unregister (omniBattleGroup, address, world)

    type OmniGameDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register (_, world) =
            let dispatchers =
                Map.addMany
                    [|typeof<OmniBattleGroupDispatcher>.Name, OmniBattleGroupDispatcher () :> obj
                      typeof<OmniFieldGroupDispatcher>.Name, OmniFieldGroupDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }