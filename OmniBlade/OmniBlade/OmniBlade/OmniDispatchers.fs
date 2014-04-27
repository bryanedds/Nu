namespace OmniBlade
open OpenTK
open Prime
open Nu
open Nu.World

[<AutoOpen>]
module OmniDispatchersModule =

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera (groupAddress : Address) world =
            let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
            let avatar = get world <| Entity.worldEntityLens avatarAddress
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler _ _ groupAddress message world =
            (message, true, adjustFieldCamera groupAddress world)

        let moveFieldAvatarHandler _ _ (groupAddress : Address) message world =
            let feelerAddress = groupAddress @ [OmniConstants.FieldFeelerName]
            let feeler = get world <| Entity.worldEntityLens feelerAddress
            if feeler.IsTouched then
                let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
                let avatar = get world <| Entity.worldEntityLens avatarAddress
                let mousePositionAvatar = Entity.mouseToEntity world.MouseState.MousePosition avatar world
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionAvatar - avatarCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = impulseVector }
                let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, true, world')
            else (message, true, world)
        
        override this.Register (address, omniBattleGroup, entities, world) =
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub moveFieldAvatarHandler) world
            let world_ = World.subscribe NuConstants.TickEvent address (CustomSub adjustFieldCameraHandler) world_
            let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
            let world_ = base.Register (address, omniBattleGroup, entities, world_)
            adjustFieldCamera address world_

        override this.Unregister (address, omniFieldGroup, world) =
            let world_ = World.unsubscribe NuConstants.TickEvent address world
            let world_ = World.unsubscribe NuConstants.TickEvent address world_
            base.Unregister (address, omniFieldGroup, world_)

    type OmniBattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override this.Register (address, omniBattleGroup, entities, world) =
            let world' = { world with PhysicsMessages = SetGravityMessage Vector2.Zero :: world.PhysicsMessages }
            base.Register (address, omniBattleGroup, entities, world')

        override this.Unregister (address, omniBattleGroup, world) =
            base.Unregister (address, omniBattleGroup, world)

    type OmniGameDispatcher () =
        inherit GameDispatcher ()
        
        override this.Register (omniGame, world) =
            let dispatchers =
                Map.addMany
                    [|Lun.make typeof<OmniBattleGroupDispatcher>.Name, OmniBattleGroupDispatcher () :> obj
                      Lun.make typeof<OmniFieldGroupDispatcher>.Name, OmniFieldGroupDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }