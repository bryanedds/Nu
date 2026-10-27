// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit, Size = 96)>]
type SpriteStruct =
    [<FieldOffset(0)>] val mutable perimeter : Vector4
    [<FieldOffset(16)>] val mutable pivot : Vector2
    [<FieldOffset(24)>] val mutable rotation : single
    [<FieldOffset(32)>] val mutable texCoords : Vector4
    [<FieldOffset(48)>] val mutable color : Vector4
    [<FieldOffset(64)>] val mutable userDefinedSettings : UserDefinedSettings

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ViewProjectionStruct =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4

/// The state of a sprite batch.
type [<Struct>] SpriteBatchState =
    { Absolute : bool
      ClipOpt : Box2 voption
      Blend : VulkanBlend
      TexturesOpt : struct (Texture * UserDefinedImageTextures) voption
      PipelineName : string }

    static member inline changed state state2 =
        state.Absolute <> state2.Absolute ||
        (match struct (state.ClipOpt, state2.ClipOpt) with
         | struct (ValueSome _, ValueNone) -> true
         | struct (ValueNone, ValueSome _) -> true
         | struct (ValueNone, ValueNone) -> false
         | struct (ValueSome c, ValueSome c2) -> c <> c2) ||
        state.Blend <> state2.Blend ||
        (match struct (state.TexturesOpt, state2.TexturesOpt) with
         | struct (ValueSome _, ValueNone) -> true
         | struct (ValueNone, ValueSome _) -> true
         | struct (ValueNone, ValueNone) -> false
         | struct (ValueSome t, ValueSome t2) -> t <> t2) ||
        state.PipelineName <> state2.PipelineName

    static member inline make absolute clipOpt blend texture userDefinedImageTextures pipelineName =
        { Absolute = absolute
          ClipOpt = clipOpt
          Blend = blend
          TexturesOpt = ValueSome struct (texture, userDefinedImageTextures)
          PipelineName = pipelineName }

    static member defaultState =
        { Absolute = false
          ClipOpt = ValueNone
          Blend = VulkanTransparent
          TexturesOpt = ValueNone
          PipelineName = Constants.Render.PipelineNameDefault }

/// The environment that contains the internal state required for batching sprites.
/// TODO: P1: consider altering the representation of batched vertex data so that all the vertex data can be uploaded
/// with a single driver call.
type [<ReferenceEquality>] SpriteBatchEnv =
    private
        { mutable SpriteIndex : int
          mutable ViewProjection2dAbsolute : Matrix4x4
          mutable ViewProjection2dRelative : Matrix4x4
          mutable ViewProjectionClipAbsolute : Matrix4x4
          mutable ViewProjectionClipRelative : Matrix4x4
          Pipelines : Dictionary<string, VulkanBuffer * VulkanBuffer * Pipeline>
          UnfilteredSampler : Sampler
          FilteredSampler : Sampler
          Perimeters : Vector4 array
          Pivots : Vector2 array
          Rotations : single array
          TexCoordses : Vector4 array
          Colors : Vector4 array
          UserDefinedSettingses : UserDefinedSettings array
          mutable State : SpriteBatchState
          VulkanContext : VulkanContext }

/// Sprite batch operations.
[<RequireQualifiedAccess>]
module SpriteBatch =

    /// Create a sprite batch pipeline.
    let private createSpriteBatchPipeline resolveTextureFormat (context : VulkanContext) =

        // create uniforms
        let spritesUniform = VulkanBuffer.create Uniform (Constants.Render.SpriteBatchSize * sizeof<SpriteStruct>) context
        let viewProjectionUniform = VulkanBuffer.create Uniform sizeof<ViewProjectionStruct> context
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                [|VulkanUnblended; VulkanTransparent; VulkanAdditive; VulkanOverwrite|] [|CullBack|] [||]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer VertexStage 1|]
                  Pipeline.descriptorSet<struct (Texture * UserDefinedImageTextures)>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1
                      Pipeline.descriptor 1 SampledImage FragmentStage 1
                      Pipeline.descriptor 2 SampledImage FragmentStage 1
                      Pipeline.descriptor 3 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|resolveTextureFormat|] None
                [|spritesUniform; viewProjectionUniform|]

        // fin
        (spritesUniform, viewProjectionUniform, pipeline)

    /// Attempt to create a user-defined pipeline
    let tryCreateUserDefinedPipeline (descriptor : UserDefinedPipeline2dDescriptor) resolveTextureFormat env context =
        match descriptor.Pipeline2dType with
        | Sprite ->
            let pipeline = createSpriteBatchPipeline resolveTextureFormat context
            match env.Pipelines.TryGetValue descriptor.PipelineName with
            | (true, (_, _, pipeline)) ->
                ConcurrentCommandQueue.waitIdle context.RenderQueue // pipeline may still be in use by previous frame
                Pipeline.destroy pipeline context
            | (false, _) -> ()
            env.Pipelines[descriptor.PipelineName] <- pipeline

    /// Reload the shaders used by the environment.
    let reloadShaders env (context : VulkanContext) =
        for (_, _, pipeline) in env.Pipelines.Values do
            Pipeline.reloadShaders pipeline context

    let private beginSpriteBatch state env =
        env.State <- state

    let private endSpriteBatch (viewport : Viewport) (resolveTexture : Texture) env =

        // ensure something to draw
        match env.State.TexturesOpt with
        | ValueSome textures when env.SpriteIndex > 0 ->

            // only draw if scissor (and therefore also viewport) is valid
            let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
            let mutable vkViewport = Hl.makeViewport true renderArea
            let mutable scissor = renderArea
            match env.State.ClipOpt with
            | ValueSome clip ->
                let viewProjection = if env.State.Absolute then env.ViewProjectionClipAbsolute else env.ViewProjectionClipRelative
                let minClip = Vector4.Transform(Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection).V2
                let minNdc = minClip * single viewport.DisplayScalar
                let minScissor = (minNdc + v2One) * 0.5f * viewport.Inner.Size.V2
                let sizeClip = Vector4.Transform(Vector4 (clip.Size, 0.0f, 1.0f), viewProjection).V2
                let sizeNdc = sizeClip * single viewport.DisplayScalar
                let sizeScissor = sizeNdc * 0.5f * viewport.Inner.Size.V2
                let offset = v2i viewport.Inner.Min.X (viewport.Outer.Max.Y - viewport.Inner.Max.Y)
                scissor <-
                    VkRect2D
                        ((minScissor.X |> round |> int) + offset.X,
                         (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                         uint sizeScissor.X,
                         uint sizeScissor.Y)
                scissor <- Hl.clipRect renderArea scissor
            | ValueNone -> ()
            if Hl.validateRect scissor then

                // compute pipeline
                let (spritesUniform, viewProjectionUniform, pipeline) =
                    match env.Pipelines.TryGetValue env.State.PipelineName with
                    | (true, pipeline) -> pipeline
                    | (false, _) -> env.Pipelines[Constants.Render.PipelineNameDefault]

                // only draw when required vkPipeline exists
                match Pipeline.tryGetVkPipeline env.State.Blend CullBack pipeline with
                | Some vkPipeline ->
                    
                    // specify uniforms
                    let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline $ fun vkSet ->

                        // specify sprites
                        let mutable sprite = SpriteStruct ()
                        use spritePtr = fixed &sprite
                        let spriteSize = sizeof<SpriteStruct>
                        for i in 0 .. dec env.SpriteIndex do
                            sprite.perimeter <- env.Perimeters[i]
                            sprite.pivot <- env.Pivots[i]
                            sprite.rotation <- env.Rotations[i]
                            sprite.texCoords <- env.TexCoordses[i]
                            sprite.color <- env.Colors[i]
                            sprite.userDefinedSettings <- env.UserDefinedSettingses[i]
                            VulkanBuffer.writeSubdata (i * spriteSize) 0 spriteSize 1 (NativePtr.toNativeInt spritePtr) spritesUniform env.VulkanContext
                        VulkanBuffer.flushSubdata 0 0 spriteSize env.SpriteIndex spritesUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 0 0 spritesUniform vkSet

                        // specify viewProjection
                        let mutable viewProjection = ViewProjectionStruct (viewProjection = if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative)
                        VulkanBuffer.uploadValue viewProjection viewProjectionUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 1 0 viewProjectionUniform vkSet

                    // specify textures
                    let struct (t, udits) = textures
                    let mutable texturesDescriptorSet = Pipeline.specifyDescriptorSet 1 textures pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorSampledTexture 0 0 t vkSet
                        Pipeline.writeDescriptorSampledTexture 1 0 udits.UserDefinedImage0Texture vkSet
                        Pipeline.writeDescriptorSampledTexture 2 0 udits.UserDefinedImage1Texture vkSet
                        Pipeline.writeDescriptorSampledTexture 3 0 udits.UserDefinedImage2Texture vkSet

                    // specify sampler
                    let sampler = if t.MipLevels = 1 then env.UnfilteredSampler else env.FilteredSampler
                    let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorSampler 0 0 sampler vkSet

                    // set up render
                    Hl.withRenderingInfo [|resolveTexture.ImageView|] None renderArea LoadAttachments $ fun renderingInfo ->
                        let mutable renderingInfo = renderingInfo
                        DeviceApi.vkCmdBeginRendering (env.VulkanContext.RenderCommandBuffer, &&renderingInfo)
                    DeviceApi.vkCmdSetViewport (env.VulkanContext.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                    DeviceApi.vkCmdSetScissor (env.VulkanContext.RenderCommandBuffer, 0u, 1u, &&scissor)

                    // set up pipeline
                    DeviceApi.vkCmdBindPipeline (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                    // bind descriptor sets
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 1u, 1u, &&texturesDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                    // draw
                    DeviceApi.vkCmdDraw (env.VulkanContext.RenderCommandBuffer, uint (6 * env.SpriteIndex), 1u, 0u, 0u)

                    // tear down render
                    DeviceApi.vkCmdEndRendering env.VulkanContext.RenderCommandBuffer

                    // report drawing
                    Hl.reportDrawCall env.SpriteIndex true

                    // advance pipeline
                    Pipeline.advance pipeline

                    // advance rendering command buffer
                    VulkanContext.advanceRenderCommandBuffer env.VulkanContext

                // abort
                | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

            // next batch
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private restartSpriteBatch state viewport resolveTexture env =
        endSpriteBatch viewport resolveTexture env
        beginSpriteBatch state env

    /// Begin a new sprite batch frame.
    let beginSpriteBatchFrame
        (viewProjection2dAbsolute : Matrix4x4 inref,
         viewProjection2dRelative : Matrix4x4 inref,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         env) =
        for (_, _, pipeline) in env.Pipelines.Values do Pipeline.beginFrame pipeline
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        beginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let endSpriteBatchFrame viewport env =
        endSpriteBatch viewport env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn viewport resolveTexture env =
        let state = env.State
        endSpriteBatch viewport resolveTexture env
        fn ()
        beginSpriteBatch state env

    let
#if !DEBUG
        inline
#endif
        private populateSpriteBatchVertex (perimeter : Box2) (pivot : Vector2) (rotation : single) (texCoords : Box2) (color : Color) userDefinedSettings env =
        env.Perimeters[env.SpriteIndex] <- v4 perimeter.Min.X perimeter.Min.Y perimeter.Size.X perimeter.Size.Y
        env.Pivots[env.SpriteIndex] <- pivot
        env.Rotations[env.SpriteIndex] <- rotation
        env.TexCoordses[env.SpriteIndex] <- v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y
        env.Colors[env.SpriteIndex] <- color.V4
        env.UserDefinedSettingses[env.SpriteIndex] <- userDefinedSettings

    /// Submit a sprite to the appropriate sprite batch.
    let submitSpriteBatchSprite
        (absolute : bool,
         min : Vector2,
         size : Vector2,
         pivot : Vector2,
         rotation : single,
         texCoords : Box2 inref,
         clipOpt : Box2 voption inref,
         texture : Texture,
         userDefinedImageTextures : UserDefinedImageTextures inref,
         pipelineName : string,
         color : Color inref,
         blend : VulkanBlend,
         userDefinedSettings : UserDefinedSettings inref,
         viewport,
         resolveTexture,
         env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt blend texture userDefinedImageTextures pipelineName
        if  SpriteBatchState.changed state env.State ||
            env.SpriteIndex = Constants.Render.SpriteBatchSize then
            restartSpriteBatch state viewport resolveTexture env

        // populate vertices
        let perimeter = box2 min size
        populateSpriteBatchVertex perimeter pivot rotation texCoords color userDefinedSettings env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let createSpriteBatchEnv unfilteredSampler filteredSampler resolveTexture context =
        
        // create pipelines
        let pipeline = createSpriteBatchPipeline resolveTexture context
        let pipelines = dictPlus StringComparer.Ordinal [(Constants.Render.PipelineNameDefault, pipeline)]

        // create env
        { SpriteIndex = 0;
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          Pipelines = pipelines
          UnfilteredSampler = unfilteredSampler
          FilteredSampler = filteredSampler
          Perimeters = Array.zeroCreate Constants.Render.SpriteBatchSize
          Pivots = Array.zeroCreate Constants.Render.SpriteBatchSize
          Rotations = Array.zeroCreate Constants.Render.SpriteBatchSize
          TexCoordses = Array.zeroCreate Constants.Render.SpriteBatchSize
          Colors = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSettingses = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SpriteBatchState.defaultState
          VulkanContext = context }

    /// Destroy the given sprite batch environment.
    let destroySpriteBatchEnv env =
        for (_, _, pipeline) in env.Pipelines.Values do
            Pipeline.destroy pipeline env.VulkanContext