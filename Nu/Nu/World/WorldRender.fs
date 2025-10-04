// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// Rendering functions for the world.
[<AutoOpen>]
module WorldRender =

    type World with

        static member internal getRendererProcess (world : World) =
            world.Subsystems.RendererProcess

        /// Get the current configuration of the 3d renderer.
        static member getRenderer3dConfig (world : World) =
            world.Subsystems.RendererProcess.Renderer3dConfig

        /// Set the current configuration of the 3d renderer.
        static member setRenderer3dConfig config world =
            World.enqueueRenderMessage3d (ConfigureRenderer3d config) world 

        /// Map the configuration of the 3d renderer.
        static member mapRenderer3dConfig mapper world =
            let config = World.getRenderer3dConfig world
            let config = mapper config
            World.setRenderer3dConfig config world

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage3d (message : RenderMessage3d) world =
            (World.getRendererProcess world).EnqueueMessage3d message

        /// Enqueue multiple 3d rendering messages to the world.
        static member enqueueRenderMessages3d (messages : RenderMessage3d seq) world =
            let rendererProcess = World.getRendererProcess world
            for message in messages do rendererProcess.EnqueueMessage3d message

        /// Send a message to the render system to render a static model using a fast path.
        static member renderStaticModelFast (modelMatrix : Matrix4x4 inref, castShadow, presence, insetOpt, materialProperties : MaterialProperties inref, staticModel, clipped, depthTest, renderType, renderPass, world) =
            (World.getRendererProcess world).RenderStaticModelFast (&modelMatrix, castShadow, presence, insetOpt, &materialProperties, staticModel, clipped, depthTest, renderType, renderPass)

        /// Send a message to the render system to render a static model surface using a fast path.
        static member renderStaticModelSurfaceFast (modelMatrix : Matrix4x4 inref, castShadow, presence, insetOpt, materialProperties : MaterialProperties inref, material : Material inref, staticModel, surfaceIndex, depthTest, renderType, renderPass, world) =
            (World.getRendererProcess world).RenderStaticModelSurfaceFast (&modelMatrix, castShadow, presence, insetOpt, &materialProperties, &material, staticModel, surfaceIndex, depthTest, renderType, renderPass)

        /// Send a message to the render system to render an animated model using a fast path.
        static member renderAnimatedModelFast (modelMatrix : Matrix4x4 inref, castShadow, presence, insetOpt, materialProperties : MaterialProperties inref, animations, animatedModel, subsortOffsets, drsIndices, depthTest, renderType, renderPass, world) =
            (World.getRendererProcess world).RenderAnimatedModelFast (&modelMatrix, castShadow, presence, insetOpt, &materialProperties, animations, animatedModel, subsortOffsets, drsIndices, depthTest, renderType, renderPass)

        /// Load a 3d render asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadRenderPackage3d packageName world =
            let loadRenderPackageUseMessage = LoadRenderPackage3d packageName
            World.enqueueRenderMessage3d loadRenderPackageUseMessage world

        /// Unload a 3d render package should be unloaded since its assets will not be used again soon.
        static member unloadRenderPackage3d packageName world =
            let unloadRenderPackageMessage = UnloadRenderPackage3d packageName
            World.enqueueRenderMessage3d unloadRenderPackageMessage world

        /// Send a message to the 3d renderer to reload its rendering assets.
        static member reloadRenderAssets3d world =
            let reloadRenderAssetsMessage = ReloadRenderAssets3d
            World.enqueueRenderMessage3d reloadRenderAssetsMessage world

        /// Send a message to the render to create the given user-defined static model.
        static member createUserDefinedStaticModel surfaceDescriptors bounds staticModel world =
            let message = CreateUserDefinedStaticModel { StaticModelSurfaceDescriptors = surfaceDescriptors; Bounds = bounds; StaticModel = staticModel }
            World.enqueueRenderMessage3d message world

        /// Send a message to the render to destroy the given user-defined static model.
        static member destroyUserDefinedStaticModel staticModel world =
            let message = DestroyUserDefinedStaticModel { StaticModel = staticModel }
            World.enqueueRenderMessage3d message world

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
        static member renderLayeredSpriteFast (elevation, horizon, assetTag, transform : Transform inref, insetOpt : Box2 ValueOption inref, clipOpt : Box2 ValueOption inref, image, color : Color inref, blend, emission : Color inref, flip, world) =
            (World.getRendererProcess world).RenderLayeredSpriteFast (elevation, horizon, assetTag, &transform, &insetOpt, &clipOpt, image, &color, blend, &emission, flip)

        /// Load a 2d render asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadRenderPackage2d packageName world =
            let loadRenderPackageUseMessage = LoadRenderPackage2d packageName
            World.enqueueRenderMessage2d loadRenderPackageUseMessage world

        /// Unload a 2d render package should be unloaded since its assets will not be used again soon.
        static member unloadRenderPackage2d packageName world =
            let unloadRenderPackageMessage = UnloadRenderPackage2d packageName
            World.enqueueRenderMessage2d unloadRenderPackageMessage world

        /// Send a message to the 2d renderer to reload its rendering assets.
        static member reloadRenderAssets2d world =
            let reloadRenderAssetsMessage = ReloadRenderAssets2d
            World.enqueueRenderMessage2d reloadRenderAssetsMessage world

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessageImGui (message : RenderMessageImGui) world =
            (World.getRendererProcess world).EnqueueMessageImGui message

        /// Send a message to the ImGui renderer to reload its rendering assets.
        static member reloadRenderAssetsImGui world =
            let reloadRenderAssetsMessage = ReloadRenderAssets
            World.enqueueRenderMessageImGui reloadRenderAssetsMessage world

        /// Render a gui sprite.
        static member renderGuiSprite absolute perimeter spriteImage offset elevation color world =
            let mutable spriteTransform = Transform.makePerimeter absolute perimeter offset elevation // out-of-box gui ignores rotation
            let insetOpt = ValueOption<Box2>.None
            let perimeter = ValueSome perimeter.Box2
            let blend = Color.Zero
            World.renderLayeredSpriteFast (spriteTransform.Elevation, spriteTransform.Horizon, spriteImage, &spriteTransform, &insetOpt, &perimeter, spriteImage, &color, Transparent, &blend, FlipNone, world)

        /// Render a gui sprite with 9-way slicing.
        static member renderGuiSpriteSliced absolute perimeter margin spriteImage offset elevation color world =
            if margin <> v2Zero then
                for i in 0 .. dec 9 do
                    let slice = box3Slice i margin perimeter
                    let insetOpt =
                        match Metadata.tryGetTextureSizeF spriteImage with
                        | ValueSome imageSize -> ValueSome (box2SliceInverted i margin (box2 v2Zero imageSize))
                        | ValueNone -> ValueNone
                    let mutable spriteTransform = Transform.makePerimeter absolute slice offset elevation // out-of-box gui ignores rotation
                    let perimeter = ValueSome perimeter.Box2
                    let blend = Color.Zero
                    World.renderLayeredSpriteFast (spriteTransform.Elevation, spriteTransform.Horizon, spriteImage, &spriteTransform, &insetOpt, &perimeter, spriteImage, &color, Transparent, &blend, FlipNone, world)
            else World.renderGuiSprite absolute perimeter spriteImage offset elevation color world

        static member renderGuiText absolute (perimeter : Box3) offset elevation shift clipOpt justification caretOpt textMargin color font fontSizing fontStyling text world =
            if not (String.IsNullOrWhiteSpace text) || Option.isSome caretOpt then
                let mutable textTransform = Transform.makeDefault ()
                textTransform.Position <- perimeter.Center + textMargin + offset // out-of-box gui ignores rotation and scale
                textTransform.Size <- perimeter.Size - textMargin * 2.0f
                textTransform.Elevation <- elevation + shift
                textTransform.Absolute <- absolute
                let descriptor = { Transform = textTransform; ClipOpt = clipOpt; Text = text; Font = font; FontSizing = fontSizing; FontStyling = fontStyling; Color = color; Justification = justification; CaretOpt = caretOpt }
                let operation = { Elevation = textTransform.Elevation; Horizon = perimeter.Center.Y; AssetTag = font; RenderOperation2d = RenderText descriptor }
                World.enqueueLayeredOperation2d operation world