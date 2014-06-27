namespace OmniBlade
open OpenTK
open Prime
open Nu
open Nu.NuConstants
open OmniBlade
open OmniBlade.OmniConstants

[<AutoOpen>]
module FieldGroupDispatcherModule =

    type FieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = groupAddress @ [FieldAvatarName]
            let avatar = World.getEntity avatarAddress world
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler message world =
            (Unhandled, adjustFieldCamera message.Subscriber world)

        let moveFieldAvatarHandler message world =
            let feelerAddress = message.Subscriber @ [FieldFeelerName]
            let feeler = World.getEntity feelerAddress world
            if feeler.IsTouched then
                let avatarAddress = message.Subscriber @ [FieldAvatarName]
                let avatar = World.getEntity avatarAddress world
                let mousePositionEntity = Entity.mouseToEntity world.MouseState.MousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let applyLinearImpulseMessage = { PhysicsId = Entity.getPhysicsId avatar; LinearImpulse = impulseVector }
                let world = { world with PhysicsMessages = ApplyLinearImpulseMessage applyLinearImpulseMessage :: world.PhysicsMessages }
                (Unhandled, world)
            else (Unhandled, world)
        
        override dispatcher.Register (address, entities, world) =
            let world = World.observe TickEvent address (CustomSub moveFieldAvatarHandler) world
            let world = World.observe TickEvent address (CustomSub adjustFieldCameraHandler) world
            let world = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            let world = base.Register (address, entities, world)
            adjustFieldCamera address world

[<AutoOpen>]
module BattleGroupDispatcherModule =

    type BattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override dispatcher.Register (address, entities, world) =
            let world = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            base.Register (address, entities, world)

        override dispatcher.Unregister (address, world) =
            base.Unregister (address, world)

[<AutoOpen>]
module OmniBladeDispatcherModule =

    type OmniBladeDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register world =
            let dispatchers =
                Map.addMany
                    [|typeof<BattleGroupDispatcher>.Name, BattleGroupDispatcher () :> obj
                      typeof<FieldGroupDispatcher>.Name, FieldGroupDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }