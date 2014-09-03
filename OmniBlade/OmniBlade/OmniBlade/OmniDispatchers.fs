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
            let avatarAddress = addrlist groupAddress [FieldAvatarName]
            let avatar = World.getEntity avatarAddress world
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler event world =
            (Unhandled, adjustFieldCamera event.Subscriber world)

        let moveFieldAvatarHandler event world =
            let feelerAddress = addrlist event.Subscriber [FieldFeelerName]
            let feeler = World.getEntity feelerAddress world
            if feeler.IsTouched then
                let avatarAddress = addrlist event.Subscriber [FieldAvatarName]
                let avatar = World.getEntity avatarAddress world
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let world = World.applyLinearImpulse impulseVector (Entity.getPhysicsId avatar) world 
                (Unhandled, world)
            else (Unhandled, world)
        
        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            let world = World.observe TickEventName address (CustomSub moveFieldAvatarHandler) world
            let world = World.observe TickEventName address (CustomSub adjustFieldCameraHandler) world
            let world = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            adjustFieldCamera address world

[<AutoOpen>]
module BattleGroupDispatcherModule =

    type BattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override dispatcher.Register (address, world) =
            let world = base.Register (address, world)
            { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }

        override dispatcher.Unregister (address, world) =
            base.Unregister (address, world)

[<AutoOpen>]
module OmniBladeDispatcherModule =

    type OmniBladeDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register world =
            let world = base.Register world
            let dispatchers =
                Map.addMany
                    [|typeof<BattleGroupDispatcher>.Name, BattleGroupDispatcher () :> obj
                      typeof<FieldGroupDispatcher>.Name, FieldGroupDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }