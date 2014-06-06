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
            let applyImpulseMessage = { PhysicsId = avatar.PhysicsId; Impulse = Vector2 (100.0f, 0.0f) }
            let world' = { world with PhysicsMessages = ApplyImpulseMessage applyImpulseMessage :: world.PhysicsMessages }
            (message, true, world')
        
        override dispatcher.Register (blazeStageGroup, address, entities, world) =
            let world' = World.subscribe NuConstants.TickEvent address (CustomSub moveStageAvatarHandler) world
            let world'' = World.subscribe NuConstants.TickEvent address (CustomSub adjustStageCameraHandler) world'
            let world'3 = base.Register (blazeStageGroup, address, entities, world'')
            adjustStageCamera address world'3

        override dispatcher.Unregister (blazeStageGroup, address, world) =
            let world' = World.unsubscribe NuConstants.TickEvent address world
            let world'' = World.unsubscribe NuConstants.TickEvent address world'
            base.Unregister (blazeStageGroup, address, world'')

    type BlazeStageScreenDispatcher () =
        inherit ScreenDispatcher ()

        override dispatcher.Register (screen, address, groupDescriptors, world) =
            let world' = base.Register (screen, address, groupDescriptors, world)
            let stagePlay = Triple.prepend BlazeConstants.StagePlayName <| World.loadGroupFile BlazeConstants.StagePlayFileName true world'
            let section0 = Triple.prepend BlazeConstants.Section0Name <| World.loadGroupFile BlazeConstants.Section0FileName true world'
            Group.addGroups address [stagePlay; section0] world'

        override dispatcher.Unregister (screen, address, world) =
            let world' = base.Unregister (screen, address, world)
            world'

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