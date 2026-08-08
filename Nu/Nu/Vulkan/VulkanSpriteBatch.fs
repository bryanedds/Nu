// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type SpriteStruct =
    [<FieldOffset(0)>] val mutable perimeter : Vector4
    [<FieldOffset(16)>] val mutable pivot : Vector2
    [<FieldOffset(24)>] val mutable rotation : single
    [<FieldOffset(32)>] val mutable texCoords : Vector4
    [<FieldOffset(48)>] val mutable color : Vector4
    
[<Struct; StructLayout (LayoutKind.Explicit)>]
type ViewProjectionStruct =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
    
type [<Struct>] SpriteBatchState =
    { Absolute : bool
      ClipOpt : Box2 voption
      Blend : VulkanBlend
      TextureOpt : Texture voption }

    static member inline changed state state2 =
        state.Absolute <> state2.Absolute ||
        (match struct (state.ClipOpt, state2.ClipOpt) with
         | struct (ValueSome _, ValueNone) -> true
         | struct (ValueNone, ValueSome _) -> true
         | struct (ValueNone, ValueNone) -> false
         | struct (ValueSome c, ValueSome c2) -> c <> c2) ||
        state.Blend <> state2.Blend ||
        (match struct (state.TextureOpt, state2.TextureOpt) with
         | struct (ValueSome _, ValueNone) -> true
         | struct (ValueNone, ValueSome _) -> true
         | struct (ValueNone, ValueNone) -> false
         | struct (ValueSome t, ValueSome t2) -> t <> t2)

    static member inline make absolute clipOpt blend texture =
        { Absolute = absolute; ClipOpt = clipOpt; Blend = blend; TextureOpt = ValueSome texture }

    static member defaultState =
        { Absolute = false; ClipOpt = ValueNone; Blend = VulkanTransparent; TextureOpt = ValueNone }

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
          Pipeline : Pipeline
          UnfilteredSampler : Sampler
          FilteredSampler : Sampler
          SpritesUniform : VulkanBuffer
          ViewProjectionUniform : VulkanBuffer
          Perimeters : Vector4 array
          Pivots : Vector2 array
          Rotations : single array
          TexCoordses : Vector4 array
          Colors : Vector4 array
          mutable State : SpriteBatchState
          VulkanContext : VulkanContext }

[<RequireQualifiedAccess>]
module SpriteBatch =

    /// Create a sprite batch pipeline.
    let private createSpriteBatchPipeline (context : VulkanContext) =

        // create uniforms
        let spritesUniform = VulkanBuffer.create Uniform (Constants.Render.SpriteBatchSize * sizeof<SpriteStruct>) context
        let viewProjectionUniform = VulkanBuffer.create Uniform sizeof<ViewProjectionStruct> context
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                [|VulkanTransparent; VulkanAdditive; VulkanOverwrite|] [|true|] [||]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer VertexStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|context.SwapFormat|] None
                [|spritesUniform; viewProjectionUniform|]

        // fin
        (spritesUniform, viewProjectionUniform, pipeline)
    
    /// Reload the shaders used by the environment.
    let reloadShaders env (context : VulkanContext) =
        Pipeline.reloadShaders env.Pipeline context

    let private beginSpriteBatch state env =
        env.State <- state

    let private endSpriteBatch (viewport : Viewport) env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

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

                // only draw when required vkPipeline exists
                match Pipeline.tryGetVkPipeline env.State.Blend true env.Pipeline with
                | Some vkPipeline ->
                    
                    // specify uniforms
                    let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 env.Pipeline.DrawIndex env.Pipeline $ fun vkSet ->

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
                            VulkanBuffer.writeSubdata (i * spriteSize) 0 spriteSize 1 (NativePtr.toNativeInt spritePtr) env.SpritesUniform env.VulkanContext
                        VulkanBuffer.flushSubdata 0 0 spriteSize env.SpriteIndex env.SpritesUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 0 0 env.SpritesUniform vkSet

                        // specify viewProjection
                        let mutable viewProjection = ViewProjectionStruct (viewProjection = if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative)
                        VulkanBuffer.uploadValue viewProjection env.ViewProjectionUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 1 0 env.ViewProjectionUniform vkSet

                    // specify material
                    let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 texture env.Pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorSampledTexture 0 0 texture vkSet

                    // specify sampler
                    let sampler = if texture.MipLevels = 1 then env.UnfilteredSampler else env.FilteredSampler
                    let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler env.Pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorSampler 0 0 sampler vkSet

                    // set up render
                    Hl.withRenderingInfo [|env.VulkanContext.SwapchainImageView|] None renderArea None $ fun renderingInfo ->
                        let mutable renderingInfo = renderingInfo
                        DeviceApi.vkCmdBeginRendering (env.VulkanContext.RenderCommandBuffer, &&renderingInfo)
                    DeviceApi.vkCmdSetViewport (env.VulkanContext.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                    DeviceApi.vkCmdSetScissor (env.VulkanContext.RenderCommandBuffer, 0u, 1u, &&scissor)

                    // set up pipeline
                    DeviceApi.vkCmdBindPipeline (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                    // bind descriptor sets
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                    // draw
                    DeviceApi.vkCmdDraw (env.VulkanContext.RenderCommandBuffer, uint (6 * env.SpriteIndex), 1u, 0u, 0u)

                    // tear down render
                    DeviceApi.vkCmdEndRendering env.VulkanContext.RenderCommandBuffer

                    // report drawing
                    Hl.reportDrawCall env.SpriteIndex true

                    // advance pipeline
                    Pipeline.advance env.Pipeline

                    // advance rendering command buffer
                    VulkanContext.advanceRenderCommandBuffer env.VulkanContext

                // abort
                | None -> Log.warnOnce ("Cannot draw " + getTypeName env.Pipeline + " because VkPipeline does not exist.")

            // next batch
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private restartSpriteBatch state viewport env =
        endSpriteBatch viewport env
        beginSpriteBatch state env

    /// Begin a new sprite batch frame.
    let beginSpriteBatchFrame
        (viewProjection2dAbsolute : Matrix4x4 inref,
         viewProjection2dRelative : Matrix4x4 inref,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         env) =
        Pipeline.beginFrame env.Pipeline
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        beginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let endSpriteBatchFrame viewport env =
        endSpriteBatch viewport env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn viewport env =
        let state = env.State
        endSpriteBatch viewport env
        fn ()
        beginSpriteBatch state env

    let
#if !DEBUG
        inline
#endif
        private populateSpriteBatchVertex (perimeter : Box2) (pivot : Vector2) (rotation : single) (texCoords : Box2) (color : Color) env =
        env.Perimeters[env.SpriteIndex] <- v4 perimeter.Min.X perimeter.Min.Y perimeter.Size.X perimeter.Size.Y
        env.Pivots[env.SpriteIndex] <- pivot
        env.Rotations[env.SpriteIndex] <- rotation
        env.TexCoordses[env.SpriteIndex] <- v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y
        env.Colors[env.SpriteIndex] <- color.V4

    /// Submit a sprite to the appropriate sprite batch.
    let submitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, clipOpt : Box2 voption inref, color : Color inref, blend, texture : Texture, viewport, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt blend texture
        if  SpriteBatchState.changed state env.State ||
            env.SpriteIndex = Constants.Render.SpriteBatchSize then
            restartSpriteBatch state viewport env

        // populate vertices
        let perimeter = box2 min size
        populateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let createSpriteBatchEnv unfilteredSampler filteredSampler context =
        
        // create pipeline
        let (spritesUniform, viewProjectionUniform, pipeline) = createSpriteBatchPipeline context

        // create env
        { SpriteIndex = 0;
          ViewProjection2dAbsolute = m4Identity
          ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity
          ViewProjectionClipRelative = m4Identity
          Pipeline = pipeline
          UnfilteredSampler = unfilteredSampler; FilteredSampler = filteredSampler
          SpritesUniform = spritesUniform; ViewProjectionUniform = viewProjectionUniform
          Perimeters = Array.zeroCreate Constants.Render.SpriteBatchSize
          Pivots = Array.zeroCreate Constants.Render.SpriteBatchSize
          Rotations = Array.zeroCreate Constants.Render.SpriteBatchSize
          TexCoordses = Array.zeroCreate Constants.Render.SpriteBatchSize
          Colors = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SpriteBatchState.defaultState
          VulkanContext = context }

    /// Destroy the given sprite batch environment.
    let destroySpriteBatchEnv env =
        Pipeline.destroy env.Pipeline env.VulkanContext