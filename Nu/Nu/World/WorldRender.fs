// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldRender =

    type World with

        static member internal getRendererProcess world =
            world.Subsystems.RendererProcess

        static member internal withRendererProcess updater world =
            updater (World.getRendererProcess world)

        /// Enqueue a 2d rendering message.
        static member enqueueRenderMessage2d (message : RenderMessage2d) world =
            (World.getRendererProcess world).EnqueueMessage2d message
            world

        /// Enqueue multiple 2d rendering messages.
        static member enqueueRenderMessages2d (messages : RenderMessage2d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage2d message
            world
            
        /// Enqueue a layered message for 2d rendering.
        static member enqueueRenderLayeredMessage2d message world =
            (World.getRendererProcess world).EnqueueMessage2d (RenderLayeredMessage2d message)
            world

        /// Enqueue multiple layered messages for 2d rendering, bypassing enqueueRenderMessage for speed.
        static member enqueueRenderLayeredMessages2d (messages : RenderLayeredMessage2d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage2d (RenderLayeredMessage2d message)
            world
            
        /// Load an 2d render asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        [<FunctionBinding>]
        static member hintRenderPackageUse2d packageName world =
            let hintRenderPackageUseMessage = LoadRenderPackageMessage2d packageName
            World.enqueueRenderMessage2d hintRenderPackageUseMessage world
            
        /// Unload a 2d render package should be unloaded since its assets will not be used again soon.
        [<FunctionBinding>]
        static member unloadRenderPackage2d packageName world =
            let unloadRenderPackageMessage = UnloadRenderPackageMessage2d packageName
            World.enqueueRenderMessage2d unloadRenderPackageMessage world

        /// Send a message to the 2d renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadRenderAssets2d world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage2d
            World.enqueueRenderMessage2d reloadRenderAssetsMessage world

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage3d (message : RenderMessage3d) world =
            (World.getRendererProcess world).EnqueueMessage3d message

        /// Enqueue multiple 3d rendering messages to the world.
        static member enqueueRenderMessages3d (messages : RenderMessage3d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage3d message

        /// Send a message to the 3d renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadRenderAssets3d world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage3d
            World.enqueueRenderMessage3d reloadRenderAssetsMessage world
            world

        /// Send a message to the render to create the given user-defined static model.
        /// NOTE: this is available as a side-effect to allow use from inside a binding.
        static member createUserDefinedStaticModel staticModelSurfaces bounds assetTag world =
            let message = CreateUserDefinedStaticModelMessage (staticModelSurfaces, bounds, assetTag)
            World.enqueueRenderMessage3d message world

        /// Send a message to the render to destroy the given user-defined static model.
        /// NOTE: this is available as a side-effect to allow use from inside a binding.
        static member destroyUserDefinedStaticModel assetTag world =
            let message = DestroyUserDefinedStaticModelMessage assetTag
            World.enqueueRenderMessage3d message world