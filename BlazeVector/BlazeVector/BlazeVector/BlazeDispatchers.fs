namespace BlazeVector
open System
open System.Collections
open OpenTK
open Microsoft.Xna
open FarseerPhysics
open FarseerPhysics.Dynamics
open Prime
open Nu

[<AutoOpen>]
module BlazeDispatchersModule =

    /// TODO document.
    type BlazeStageGroupDispatcher () =
        inherit GroupDispatcher ()

        let adjustCamera address world =
            let avatar = get world <| Entity.worldEntity (address @ [BlazeConstants.StageAvatarName])
            let camera = { world.Camera with EyeCenter = Vector2 (avatar.Position.X + avatar.Size.X * 0.5f, world.Camera.EyeCenter.Y) }
            { world with Camera = camera }

        let adjustCameraHandler _ _ address message world =
            (message, true, adjustCamera address world)

        let moveAvatarHandler _ _ address message world =
            let avatar = get world <| Entity.worldEntity (address @ [BlazeConstants.StageAvatarName])
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = Vector2 (100.0f, 0.0f) }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        
        let jumpAvatarHandler _ _ address message world =
            let avatar = get world <| Entity.worldEntity (address @ [BlazeConstants.StageAvatarName])
            if not <| Physics.isBodyOnGround avatar.PhysicsId world.Integrator then (message, true, world)
            else
                let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = Vector2 (0.0f, 10000.0f) }
                let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
                (message, true, world')
        
        override dispatcher.Register (group, address, entities, world) =
            let world' =
                world |>
                    World.subscribe NuConstants.TickEvent address (CustomSub moveAvatarHandler) |>
                    World.subscribe NuConstants.TickEvent address (CustomSub adjustCameraHandler) |>
                    World.subscribe NuConstants.DownMouseLeftEvent address (CustomSub jumpAvatarHandler)
            let world'' = base.Register (group, address, entities, world')
            adjustCamera address world''

        override dispatcher.Unregister (group, address, world) =
            let world' =
                world |>
                    World.unsubscribe NuConstants.TickEvent address |>
                    World.unsubscribe NuConstants.TickEvent address |>
                    World.unsubscribe NuConstants.DownMouseLeftEvent address
            base.Unregister (group, address, world')

    type BlazeStageScreenDispatcher () =
        inherit ScreenDispatcher ()

        override dispatcher.Register (screen, address, groupDescriptors, world) =
            let stagePlay = Triple.prepend BlazeConstants.StagePlayName <| World.loadGroupFile BlazeConstants.StagePlayFileName true world
            let section0 = Triple.prepend BlazeConstants.Section0Name <| World.loadGroupFile BlazeConstants.Section0FileName true world
            let groupDescriptors' = stagePlay :: section0 :: groupDescriptors
            base.Register (screen, address, groupDescriptors', world)

        override dispatcher.Unregister (screen, address, world) =
            base.Unregister (screen, address, world)

    /// The custom type for BlazeVector's game dispatcher.
    type BlazeGameDispatcher () =
        inherit GameDispatcher ()
        
        override dispatcher.Register (blazeGame, world) =

            // add the BlazeVector-specific dispatchers to the world
            let dispatchers =
                Map.addMany
                    [|typeof<BlazeStageGroupDispatcher>.Name, BlazeStageGroupDispatcher () :> obj
                      typeof<BlazeStageScreenDispatcher>.Name, BlazeStageScreenDispatcher () :> obj|]
                    world.Dispatchers
            { world with Dispatchers = dispatchers }