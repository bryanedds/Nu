// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

[<AutoOpen>]
module WorldRender =

    type World with

        static member internal getRendererProcess world =
            world.Subsystems.RendererProcess

        static member internal withRendererProcess fn world =
            fn (World.getRendererProcess world)

        /// Enqueue a 2d rendering message.
        static member enqueueRenderMessage2d (message : RenderMessage2d) world =
            (World.getRendererProcess world).EnqueueMessage2d message

        /// Enqueue multiple 2d rendering messages.
        static member enqueueRenderMessages2d (messages : RenderMessage2d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage2d message
            
        /// Enqueue a layered operation for 2d rendering.
        static member enqueueLayeredOperation2d operation world =
            (World.getRendererProcess world).EnqueueMessage2d (LayeredOperation2d operation)

        /// Enqueue multiple layered operation for 2d rendering, bypassing enqueueRenderMessage for speed.
        static member enqueueLayeredOperations2d (operations : LayeredOperation2d seq) world =
            let rendererProcess = World.getRendererProcess world
            for operation in operations do rendererProcess.EnqueueMessage2d (LayeredOperation2d operation)

        /// Send a message to the render system to render a static model using a fast path.
        static member renderLayeredSpriteFast (elevation, horizon, assetTag, transform : Transform inref, insetOpt : Box2 ValueOption inref, image, color : Color inref, blend, emission : Color inref, flip, world) =
            (World.getRendererProcess world).RenderLayeredSpriteFast (elevation, horizon, assetTag, &transform, &insetOpt, image, &color, blend, &emission, flip)

        /// Load a 2d render asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadRenderPackage2d packageName world =
            let loadRenderPackageUseMessage = LoadRenderPackage2d packageName
            World.enqueueRenderMessage2d loadRenderPackageUseMessage world
            world

        /// Unload a 2d render package should be unloaded since its assets will not be used again soon.
        static member unloadRenderPackage2d packageName world =
            let unloadRenderPackageMessage = UnloadRenderPackage2d packageName
            World.enqueueRenderMessage2d unloadRenderPackageMessage world
            world

        /// Send a message to the 2d renderer to reload its rendering assets.
        static member reloadRenderAssets2d world =
            let reloadRenderAssetsMessage = ReloadRenderAssets2d
            World.enqueueRenderMessage2d reloadRenderAssetsMessage world
            world

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage3d (message : RenderMessage3d) world =
            (World.getRendererProcess world).EnqueueMessage3d message

        /// Enqueue multiple 3d rendering messages to the world.
        static member enqueueRenderMessages3d (messages : RenderMessage3d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage3d message

        /// Send a message to the render system to render a static model using a fast path.
        static member renderStaticModelFast (absolute, modelMatrix : Matrix4x4 inref, presence, insetOpt, materialProperties : MaterialProperties inref, staticModel, renderType, renderPass, world) =
            (World.getRendererProcess world).RenderStaticModelFast (absolute, &modelMatrix, presence, insetOpt, &materialProperties, staticModel, renderType, renderPass)

        /// Send a message to the render system to render a static model surface using a fast path.
        static member renderStaticModelSurfaceFast (absolute, modelMatrix : Matrix4x4 inref, presence, insetOpt, materialProperties : MaterialProperties inref, material : Material inref, staticModel, surfaceIndex, renderType, renderPass, world) =
            (World.getRendererProcess world).RenderStaticModelSurfaceFast (absolute, &modelMatrix, presence, insetOpt, &materialProperties, &material, staticModel, surfaceIndex, renderType, renderPass)

        /// Send a message to the render system to render an animated model using a fast path.
        static member renderAnimatedModelFast (absolute, modelMatrix : Matrix4x4 inref, presence, insetOpt, materialProperties : MaterialProperties inref, animations, animatedModel, renderPass, world) =
            (World.getRendererProcess world).RenderAnimatedModelFast (absolute, &modelMatrix, presence, insetOpt, &materialProperties, animations, animatedModel, renderPass)

        /// Load a 3d render asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadRenderPackage3d packageName world =
            let loadRenderPackageUseMessage = LoadRenderPackage3d packageName
            World.enqueueRenderMessage3d loadRenderPackageUseMessage world
            world

        /// Unload a 3d render package should be unloaded since its assets will not be used again soon.
        static member unloadRenderPackage3d packageName world =
            let unloadRenderPackageMessage = UnloadRenderPackage3d packageName
            World.enqueueRenderMessage3d unloadRenderPackageMessage world
            world

        /// Send a message to the 3d renderer to reload its rendering assets.
        static member reloadRenderAssets3d world =
            let reloadRenderAssetsMessage = ReloadRenderAssets3d
            World.enqueueRenderMessage3d reloadRenderAssetsMessage world
            world

        /// Send a message to the render to create the given user-defined static model.
        static member createUserDefinedStaticModel surfaceDescriptors bounds staticModel world =
            let message = CreateUserDefinedStaticModel { StaticModelSurfaceDescriptors = surfaceDescriptors; Bounds = bounds; StaticModel = staticModel }
            World.enqueueRenderMessage3d message world

        /// Send a message to the render to destroy the given user-defined static model.
        static member destroyUserDefinedStaticModel staticModel world =
            let message = DestroyUserDefinedStaticModel { StaticModel = staticModel }
            World.enqueueRenderMessage3d message world