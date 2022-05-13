// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldRender =

    type World with

        static member internal getRenderProcess2d world =
            world.Subsystems.RenderProcess2d

        static member internal withRenderProcess2d updater world =
            updater (World.getRenderProcess2d world)

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage2d (message : RenderMessage2d) world =
            (World.getRenderProcess2d world).EnqueueMessage message
            world

        /// Enqueue multiple rendering messages to the world.
        static member enqueueRenderMessages2d (messages : RenderMessage2d seq) world =
            let renderProcess = World.getRenderProcess2d world
            for message in messages do renderProcess.EnqueueMessage message
            world
            
        /// Enqueue a layered message for rendering, bypassing enqueueRenderMessage for speed.
        static member enqueueRenderLayeredMessage2d (message : RenderLayeredMessage2d) world =
            (World.getRenderProcess2d world).EnqueueMessage (RenderLayeredMessage2d message)
            world

        /// Enqueue multiple layered rendering messages to the world, bypassing enqueueRenderMessage for speed.
        static member enqueueRenderLayeredMessages2d (messages : RenderLayeredMessage2d seq) world =
            let renderer = World.getRenderProcess2d world
            for message in messages do renderer.EnqueueMessage (RenderLayeredMessage2d message)
            world

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        [<FunctionBinding>]
        static member hintRenderPackageUse2d packageName world =
            let hintRenderPackageUseMessage = HintRenderPackageUseMessage2d packageName
            World.enqueueRenderMessage2d hintRenderPackageUseMessage world

        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderPackageUse).
        [<FunctionBinding>]
        static member hintRenderPackageDisuse2d packageName world =
            let hintRenderPackageDisuseMessage = HintRenderPackageDisuseMessage2d packageName
            World.enqueueRenderMessage2d hintRenderPackageDisuseMessage world

        /// Send a message to the renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadRenderAssets2d world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage2d
            World.enqueueRenderMessage2d reloadRenderAssetsMessage world