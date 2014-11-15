namespace OmniBlade
open OpenTK
open SDL2
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open OmniBlade
open OmniBlade.OmniConstants

[<AutoOpen>]
module OmniDispatchersModule =

    let [<Literal>] KeyboardMovementForce = 400.0f

    type FieldGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustFieldCamera groupAddress world =
            let avatarAddress = gatoea groupAddress FieldAvatarName
            let avatar = World.getEntity avatarAddress world
            let camera = { world.Camera with EyeCenter = avatar.Position + avatar.Size * 0.5f }
            World.setCamera camera world

        let handleAdjustFieldCamera event world =
            let address = World.unwrapA event world
            (Cascade, adjustFieldCamera address world)

        let handleMoveFieldAvatar event world =
            let address = World.unwrapA event world
            let avatarAddress = gatoea address FieldAvatarName
            let feelerAddress = gatoea address FieldFeelerName
            let avatar = World.getEntity avatarAddress world
            let feeler = World.getEntity feelerAddress world
            if feeler.IsTouched then
                let mousePosition = World.getMousePositionF world
                let mousePositionEntity = Entity.mouseToEntity mousePosition world avatar
                let avatarCenter = avatar.Position + avatar.Size * 0.5f
                let impulseVector = (mousePositionEntity - avatarCenter) * 5.0f
                let world = World.applyBodyLinearImpulse impulseVector avatar.PhysicsId world 
                (Cascade, world)
            else
                let impulses =
                    [(if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then Vector2 (-KeyboardMovementForce, 0.0f) else Vector2.Zero)
                     (if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then Vector2 (KeyboardMovementForce, 0.0f) else Vector2.Zero)
                     (if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then Vector2 (0.0f, KeyboardMovementForce) else Vector2.Zero)
                     (if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then Vector2 (0.0f, -KeyboardMovementForce) else Vector2.Zero)]
                let impulse = List.reduce add impulses
                let world = World.applyBodyLinearImpulse impulse avatar.PhysicsId world 
                (Cascade, world)

        override dispatcher.Register (address, avatar, world) =
            let world = World.monitor address TickEventAddress handleMoveFieldAvatar world
            let world = World.monitor address TickEventAddress handleAdjustFieldCamera world
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