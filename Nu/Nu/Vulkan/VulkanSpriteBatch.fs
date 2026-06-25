// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type Sprite =
    [<FieldOffset(0)>] val mutable perimeter : Vector4
    [<FieldOffset(16)>] val mutable pivot : Vector2
    [<FieldOffset(24)>] val mutable rotation : single
    [<FieldOffset(32)>] val mutable texCoords : Vector4
    [<FieldOffset(48)>] val mutable color : Vector4
    
[<Struct; StructLayout (LayoutKind.Explicit)>]
type ViewProjection =
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
          VulkanContext : VulkanContext
          Pipeline : Pipeline
          UnfilteredSampler : Sampler
          FilteredSampler : Sampler
          SpritesUniform : Nu.Vulkan.Buffer
          ViewProjectionUniform : Nu.Vulkan.Buffer
          Perimeters : Vector4 array
          Pivots : Vector2 array
          Rotations : single array
          TexCoordses : Vector4 array
          Colors : Vector4 array
          mutable State : SpriteBatchState }

[<RequireQualifiedAccess>]
module SpriteBatch =

    /// Create a sprite batch pipeline.
    let private createSpriteBatchPipeline (vkc : VulkanContext) =

        // create uniforms
        let spritesUniform = Buffer.create (Constants.Render.SpriteBatchSize * sizeof<Sprite>) Storage vkc
        let viewProjectionUniform = Buffer.create sizeof<ViewProjection> Storage vkc
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                [|VulkanTransparent; VulkanAdditive; VulkanOverwrite|] [|true|] [||]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer VertexStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|vkc.SwapFormat|] None
                [|spritesUniform; viewProjectionUniform|] vkc

        // fin
        (spritesUniform, viewProjectionUniform, pipeline)
    
    /// Reload the shaders used by the environment.
    let reloadShaders env vkc =
        Pipeline.reloadShaders env.Pipeline vkc
    
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

                // only draw if required vkPipeline exists
                match Pipeline.tryGetVkPipeline env.State.Blend true env.Pipeline with
                | Some vkPipeline ->
                    
                    // specify uniforms
                    let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 env.Pipeline.DrawIndex env.Pipeline env.VulkanContext $ fun vkSet ->

                        // specify sprites
                        let mutable sprite = Sprite ()
                        use spritePtr = fixed &sprite
                        let spriteSize = sizeof<Sprite>
                        for i in 0 .. dec env.SpriteIndex do
                            sprite.perimeter <- env.Perimeters[i]
                            sprite.pivot <- env.Pivots[i]
                            sprite.rotation <- env.Rotations[i]
                            sprite.texCoords <- env.TexCoordses[i]
                            sprite.color <- env.Colors[i]
                            Buffer.uploadSubdata (i * spriteSize) 0 spriteSize 1 (NativePtr.toNativeInt spritePtr) env.SpritesUniform env.VulkanContext
                        Pipeline.writeDescriptorStorageBuffer 0 0 env.SpritesUniform vkSet env.VulkanContext

                        // specify viewProjection
                        let mutable viewProjection = ViewProjection (viewProjection = if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative)
                        Buffer.uploadValue viewProjection env.ViewProjectionUniform env.VulkanContext
                        Pipeline.writeDescriptorStorageBuffer 1 0 env.ViewProjectionUniform vkSet env.VulkanContext

                    // specify material
                    let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 texture env.Pipeline env.VulkanContext $ fun vkSet ->
                        Pipeline.writeDescriptorSampledImage 0 0 texture vkSet env.VulkanContext

                    // specify sampler
                    let sampler = if texture.MipLevels = 1 then env.UnfilteredSampler else env.FilteredSampler
                    let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler env.Pipeline env.VulkanContext $ fun vkSet ->
                        Pipeline.writeDescriptorSampler 0 0 sampler vkSet env.VulkanContext
    
                    // set up render
                    let mutable rendering = Hl.makeRenderingInfo [|env.VulkanContext.SwapchainImageView|] None renderArea None
                    Vulkan.vkCmdBeginRendering (env.VulkanContext.RenderCommandBuffer, asPointer &rendering)
                    Vulkan.vkCmdBindPipeline (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                    Vulkan.vkCmdSetViewport (env.VulkanContext.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                    Vulkan.vkCmdSetScissor (env.VulkanContext.RenderCommandBuffer, 0u, 1u, asPointer &scissor)

                    // bind descriptor sets
                    Vulkan.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
                    Vulkan.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 2u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)

                    // draw
                    Vulkan.vkCmdDraw (env.VulkanContext.RenderCommandBuffer, uint (6 * env.SpriteIndex), 1u, 0u, 0u)
                        
                    // tear down render
                    Vulkan.vkCmdEndRendering env.VulkanContext.RenderCommandBuffer

                    // advance pipeline
                    Pipeline.advance env.SpriteIndex env.Pipeline

                // abort
                | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

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
    let submitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, clipOpt : (Box2 voption) inref, color : Color inref, blend, texture : Texture, viewport, env) =

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
    let createSpriteBatchEnv unfilteredSampler filteredSampler vkc =
        
        // create pipeline
        let (spritesUniform, viewProjectionUniform, pipeline) = createSpriteBatchPipeline vkc

        // create env
        { SpriteIndex = 0;
          ViewProjection2dAbsolute = m4Identity; ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity; ViewProjectionClipRelative = m4Identity
          VulkanContext = vkc; Pipeline = pipeline; UnfilteredSampler = unfilteredSampler; FilteredSampler = filteredSampler
          SpritesUniform = spritesUniform; ViewProjectionUniform = viewProjectionUniform
          Perimeters = Array.zeroCreate Constants.Render.SpriteBatchSize
          Pivots = Array.zeroCreate Constants.Render.SpriteBatchSize
          Rotations = Array.zeroCreate Constants.Render.SpriteBatchSize
          TexCoordses = Array.zeroCreate Constants.Render.SpriteBatchSize
          Colors = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SpriteBatchState.defaultState }

    /// Destroy the given sprite batch environment.
    let destroySpriteBatchEnv env =
        Pipeline.destroy env.Pipeline env.VulkanContext
