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

[<Struct; StructLayout (LayoutKind.Explicit, Size = 96)>]
type SpriteStruct =
    [<FieldOffset(0)>] val mutable perimeter : Vector4
    [<FieldOffset(16)>] val mutable pivot : Vector2
    [<FieldOffset(24)>] val mutable rotation : single
    [<FieldOffset(32)>] val mutable texCoords : Vector4
    [<FieldOffset(48)>] val mutable color : Vector4
    [<FieldOffset(64)>] val mutable userDefinedSetting0 : single
    [<FieldOffset(68)>] val mutable userDefinedSetting1 : single
    [<FieldOffset(72)>] val mutable userDefinedSetting2 : single
    [<FieldOffset(76)>] val mutable userDefinedSetting3 : single
    [<FieldOffset(80)>] val mutable userDefinedSetting4 : single
    [<FieldOffset(84)>] val mutable userDefinedSetting5 : single
    [<FieldOffset(88)>] val mutable userDefinedSetting6 : single
    [<FieldOffset(92)>] val mutable userDefinedSetting7 : single

[<Struct; StructLayout (LayoutKind.Explicit)>]
type ViewProjectionStruct =
    [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
    
/// The state of a sprite batch.
type [<Struct>] SpriteBatchState =
    { Absolute : bool
      ClipOpt : Box2 voption
      Blend : VulkanBlend
      TexturesOpt : struct (Texture * Texture * Texture * Texture) voption }

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
         | struct (ValueSome t, ValueSome t2) -> t <> t2)

    static member inline make absolute clipOpt blend texture userDefinedImage0Texture userDefinedImage1Texture userDefinedImage2Texture =
        { Absolute = absolute
          ClipOpt = clipOpt
          Blend = blend
          TexturesOpt = ValueSome struct (texture, userDefinedImage0Texture, userDefinedImage1Texture, userDefinedImage2Texture) }

    static member defaultState =
        { Absolute = false
          ClipOpt = ValueNone
          Blend = VulkanTransparent
          TexturesOpt = ValueNone }

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
          UserDefinedSetting0s : single array
          UserDefinedSetting1s : single array
          UserDefinedSetting2s : single array
          UserDefinedSetting3s : single array
          UserDefinedSetting4s : single array
          UserDefinedSetting5s : single array
          UserDefinedSetting6s : single array
          UserDefinedSetting7s : single array
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
                  Pipeline.descriptorSet<struct (Texture * Texture * Texture * Texture)>
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
    
    /// Reload the shaders used by the environment.
    let reloadShaders env (context : VulkanContext) =
        Pipeline.reloadShaders env.Pipeline context

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

                // only draw when required vkPipeline exists
                match Pipeline.tryGetVkPipeline env.State.Blend CullBack env.Pipeline with
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
                            sprite.userDefinedSetting0 <- env.UserDefinedSetting0s[i]
                            sprite.userDefinedSetting1 <- env.UserDefinedSetting1s[i]
                            sprite.userDefinedSetting2 <- env.UserDefinedSetting2s[i]
                            sprite.userDefinedSetting3 <- env.UserDefinedSetting3s[i]
                            sprite.userDefinedSetting4 <- env.UserDefinedSetting4s[i]
                            sprite.userDefinedSetting5 <- env.UserDefinedSetting5s[i]
                            sprite.userDefinedSetting6 <- env.UserDefinedSetting6s[i]
                            sprite.userDefinedSetting7 <- env.UserDefinedSetting7s[i]
                            VulkanBuffer.writeSubdata (i * spriteSize) 0 spriteSize 1 (NativePtr.toNativeInt spritePtr) env.SpritesUniform env.VulkanContext
                        VulkanBuffer.flushSubdata 0 0 spriteSize env.SpriteIndex env.SpritesUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 0 0 env.SpritesUniform vkSet

                        // specify viewProjection
                        let mutable viewProjection = ViewProjectionStruct (viewProjection = if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative)
                        VulkanBuffer.uploadValue viewProjection env.ViewProjectionUniform env.VulkanContext
                        Pipeline.writeDescriptorUniformBuffer 1 0 env.ViewProjectionUniform vkSet

                    // specify textures
                    let struct (t, udt0, udt1, udt2) = textures
                    let mutable texturesDescriptorSet = Pipeline.specifyDescriptorSet 1 textures env.Pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorSampledTexture 0 0 t vkSet
                        Pipeline.writeDescriptorSampledTexture 1 0 udt0 vkSet
                        Pipeline.writeDescriptorSampledTexture 2 0 udt1 vkSet
                        Pipeline.writeDescriptorSampledTexture 3 0 udt2 vkSet

                    // specify sampler
                    let sampler = if t.MipLevels = 1 then env.UnfilteredSampler else env.FilteredSampler
                    let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler env.Pipeline $ fun vkSet ->
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
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
                    DeviceApi.vkCmdBindDescriptorSets (env.VulkanContext.RenderCommandBuffer, VkPipelineBindPoint.Graphics, env.Pipeline.PipelineLayout, 1u, 1u, &&texturesDescriptorSet, 0u, nullPtr)
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
    let InterruptSpriteBatchFrame fn viewport resolveTexture env =
        let state = env.State
        endSpriteBatch viewport resolveTexture env
        fn ()
        beginSpriteBatch state env

    let
#if !DEBUG
        inline
#endif
        private populateSpriteBatchVertex
            (perimeter : Box2) (pivot : Vector2) (rotation : single) (texCoords : Box2) (color : Color)
            userDefinedSetting0 userDefinedSetting1 userDefinedSetting2 userDefinedSetting3 userDefinedSetting4 userDefinedSetting5 userDefinedSetting6 userDefinedSetting7
            env =
        env.Perimeters[env.SpriteIndex] <- v4 perimeter.Min.X perimeter.Min.Y perimeter.Size.X perimeter.Size.Y
        env.Pivots[env.SpriteIndex] <- pivot
        env.Rotations[env.SpriteIndex] <- rotation
        env.TexCoordses[env.SpriteIndex] <- v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y
        env.Colors[env.SpriteIndex] <- color.V4
        env.UserDefinedSetting0s[env.SpriteIndex] <- userDefinedSetting0
        env.UserDefinedSetting1s[env.SpriteIndex] <- userDefinedSetting1
        env.UserDefinedSetting2s[env.SpriteIndex] <- userDefinedSetting2
        env.UserDefinedSetting3s[env.SpriteIndex] <- userDefinedSetting3
        env.UserDefinedSetting4s[env.SpriteIndex] <- userDefinedSetting4
        env.UserDefinedSetting5s[env.SpriteIndex] <- userDefinedSetting5
        env.UserDefinedSetting6s[env.SpriteIndex] <- userDefinedSetting6
        env.UserDefinedSetting7s[env.SpriteIndex] <- userDefinedSetting7

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
         userDefinedImage0Texture : Texture,
         userDefinedImage1Texture : Texture,
         userDefinedImage2Texture : Texture,
         color : Color inref,
         blend : VulkanBlend,
         userDefinedSetting0,
         userDefinedSetting1,
         userDefinedSetting2,
         userDefinedSetting3,
         userDefinedSetting4,
         userDefinedSetting5,
         userDefinedSetting6,
         userDefinedSetting7,
         viewport,
         resolveTexture,
         env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt blend texture userDefinedImage0Texture userDefinedImage1Texture userDefinedImage2Texture
        if  SpriteBatchState.changed state env.State ||
            env.SpriteIndex = Constants.Render.SpriteBatchSize then
            restartSpriteBatch state viewport resolveTexture env

        // populate vertices
        let perimeter = box2 min size
        populateSpriteBatchVertex
            perimeter pivot rotation texCoords color
            userDefinedSetting0 userDefinedSetting1 userDefinedSetting2 userDefinedSetting3 userDefinedSetting4 userDefinedSetting5 userDefinedSetting6 userDefinedSetting7
            env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let createSpriteBatchEnv unfilteredSampler filteredSampler resolveTexture context =
        
        // create pipeline
        let (spritesUniform, viewProjectionUniform, pipeline) = createSpriteBatchPipeline resolveTexture context

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
          UserDefinedSetting0s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting1s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting2s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting3s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting4s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting5s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting6s = Array.zeroCreate Constants.Render.SpriteBatchSize
          UserDefinedSetting7s = Array.zeroCreate Constants.Render.SpriteBatchSize
          State = SpriteBatchState.defaultState
          VulkanContext = context }

    /// Destroy the given sprite batch environment.
    let destroySpriteBatchEnv env =
        Pipeline.destroy env.Pipeline env.VulkanContext