namespace OmniBlade
open OpenTK
open Nu

[<AutoOpen>]
module OmniDispatchersModule =

    type OmniFieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
            let entity = get world <| Entity.worldEntityLens avatarAddress
            let camera = { world.Camera with EyePosition = entity?Position () + (entity?Size () : Vector2) * 0.5f }
            { world with Camera = camera }

        let adjustFieldCameraHandler groupAddress _ _ message world =
            (message, true, adjustFieldCamera groupAddress world)

        let moveFieldAvatarHandler groupAddress _ _ message world =
            let feelerAddress = groupAddress @ [OmniConstants.FieldFeelerName]
            let feeler = get world <| Entity.worldEntityLens feelerAddress
            if feeler?IsTouched () then
                let avatarAddress = groupAddress @ [OmniConstants.FieldAvatarName]
                let avatar = get world <| Entity.worldEntityLens avatarAddress
                let camera = world.Camera
                let view = Camera.getInverseViewF camera
                let mousePositionWorld = world.MouseState.MousePosition + view
                let avatarCenter = avatar?Position () + (avatar?Size () : Vector2) * 0.5f
                let impulseVector = (mousePositionWorld - avatarCenter) * 5.0f
                let applyImpulseMessage = { PhysicsId = avatar?PhysicsId (); Impulse = impulseVector }
                let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, true, world')
            else (message, true, world)
        
        override this.Register (address, omniBattleGroup, entities, world) =
            let world_ = World.subscribe NuConstants.TickAddress [] (moveFieldAvatarHandler address) world
            let world_ = World.subscribe NuConstants.TickAddress [] (adjustFieldCameraHandler address) world_
            let world_ = { world_ with PhysicsMessages = SetGravityMessage Vector2.Zero :: world_.PhysicsMessages }
            let world_ = base.Register (address, omniBattleGroup, entities, world_)
            adjustFieldCamera address world_

        override this.Unregister (address, omniFieldGroup, world) =
            let world_ = World.unsubscribe NuConstants.TickAddress [] world
            let world_ = World.unsubscribe NuConstants.TickAddress [] world_
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