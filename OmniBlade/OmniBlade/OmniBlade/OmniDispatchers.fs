namespace OmniBlade
open OpenTK
open Prime
open Nu
open Nu.Constants
open OmniBlade
open OmniBlade.OmniConstants

[<AutoOpen>]
module OmniDispatchersModule =

    type FieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = groupAddress @+ [FieldAvatarName]
            let avatar = World.getEntity avatarAddress world
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            World.setCamera camera world

        let handleAdjustFieldCamera event world =
            let (address, _, _) = Event.unwrap event
            (Propagate, adjustFieldCamera address world)

        let handleMoveFieldAvatar event world =
            let (address, _, _) = Event.unwrap event
            let feelerAddress = address @+ [FieldFeelerName]
            let feeler = World.getEntity feelerAddress world
            if feeler.IsTouched then
                let avatarAddress = address @+ [FieldAvatarName]
                let avatar = World.getEntity avatarAddress world
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let world = World.applyBodyLinearImpulse impulseVector avatar.PhysicsId world 
                (Propagate, world)
            else (Propagate, world)

        override dispatcher.Register (address, avatar, world) =
            let world = World.monitor TickEventAddress address handleMoveFieldAvatar world
            let world = World.monitor TickEventAddress address handleAdjustFieldCamera world
            let world = World.addPhysicsMessage (SetGravityMessage Vector2.Zero) world
            let world = adjustFieldCamera address world
            (avatar, world)

    type BattleGroupDispatcher () =
        inherit GroupDispatcher ()

        override dispatcher.Register (_, group, world) =
            let world = World.addPhysicsMessage (SetGravityMessage Vector2.Zero) world
            (group, world)

    type OmniBladeDispatcher () =
        inherit GameDispatcher ()