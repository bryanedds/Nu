// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type Sprite =
        [<FieldOffset(0)>] val mutable perimeter : Vector4
        [<FieldOffset(16)>] val mutable pivot : Vector2
        [<FieldOffset(24)>] val mutable rotation : single
        [<FieldOffset(32)>] val mutable texCoords : Vector4
        [<FieldOffset(48)>] val mutable color : Vector4
    
    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type ViewProjection =
        [<FieldOffset(0)>] val mutable viewProjection : Matrix4x4
    
    type [<Struct>] private SpriteBatchState =
        { Absolute : bool
          ClipOpt : Box2 voption
          Blend : Pipeline.Blend
          TextureOpt : Texture.Texture voption }

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
            { Absolute = false; ClipOpt = ValueNone; Blend = Pipeline.Transparent; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching sprites.
    type [<ReferenceEquality>] SpriteBatchEnv =
        private
            { mutable DrawIndex : int
              mutable SpriteIndex : int
              mutable ViewProjection2dAbsolute : Matrix4x4
              mutable ViewProjection2dRelative : Matrix4x4
              mutable ViewProjectionClipAbsolute : Matrix4x4
              mutable ViewProjectionClipRelative : Matrix4x4
              VulkanContext : Hl.VulkanContext
              Pipeline : Pipeline.Pipeline
              SpriteUniform : Buffer.Buffer
              ViewProjectionUniform : Buffer.Buffer
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              mutable State : SpriteBatchState }

    /// Create a sprite batch pipeline.
    let private CreateSpriteBatchPipeline (vkc : Hl.VulkanContext) =
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                [|Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite|] [||]
                [|Pipeline.descriptorSet true
                    [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.UniformBuffer Hl.VertexStage 1
                      Pipeline.descriptor 2 Hl.CombinedImageSampler Hl.FragmentStage 1|]|]
                [|Pipeline.pushConstant 0 sizeof<int> Hl.VertexFragmentStage|]
                vkc.SwapFormat None vkc

        // create uniforms
        let spriteUniform = Buffer.Buffer.create sizeof<Sprite> Buffer.Uniform vkc
        let viewProjectionUniform = Buffer.Buffer.create sizeof<ViewProjection> Buffer.Uniform vkc

        // fin
        (spriteUniform, viewProjectionUniform, pipeline)
    
    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch (viewport : Viewport) env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

            // upload uniforms
            let vkc = env.VulkanContext
            let spriteUniform = env.SpriteUniform
            let viewProjectionUniform = env.ViewProjectionUniform
            for i in 0 .. dec env.SpriteIndex do
                let mutable sprite = Sprite ()
                sprite.perimeter <- v4 env.Perimeters.[i * 4] env.Perimeters.[i * 4 + 1] env.Perimeters.[i * 4 + 2] env.Perimeters.[i * 4 + 3]
                sprite.pivot <- v2 env.Pivots.[i * 2] env.Pivots.[i * 2 + 1]
                sprite.rotation <- env.Rotations.[i]
                sprite.texCoords <- v4 env.TexCoordses.[i * 4] env.TexCoordses.[i * 4 + 1] env.TexCoordses.[i * 4 + 2] env.TexCoordses.[i * 4 + 3]
                sprite.color <- v4 env.Colors.[i * 4] env.Colors.[i * 4 + 1] env.Colors.[i * 4 + 2] env.Colors.[i * 4 + 3]
                Buffer.Buffer.uploadValue (env.DrawIndex * Constants.Render.SpriteBatchSize + i) 0 0 sprite spriteUniform vkc
            let mutable viewProjection = ViewProjection ()
            viewProjection.viewProjection <- if env.State.Absolute then env.ViewProjection2dAbsolute else env.ViewProjection2dRelative
            Buffer.Buffer.uploadValue env.DrawIndex 0 0 viewProjection viewProjectionUniform vkc
            
            // update uniform descriptors
            Pipeline.Pipeline.updateDescriptorsUniform 0 0 spriteUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 0 1 viewProjectionUniform env.Pipeline vkc

            // bind texture
            Pipeline.Pipeline.writeDescriptorTexture env.DrawIndex 0 2 texture env.Pipeline vkc
            
            // make viewport and scissor
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
            
            // only draw if scissor (and therefore also viewport) is valid
            if Hl.validateRect scissor then
            
                // init render
                let cb = vkc.RenderCommandBuffer
                let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView None renderArea None
                Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

                // bind pipeline
                let vkPipeline = Pipeline.Pipeline.getVkPipeline env.State.Blend true env.Pipeline
                Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

                // set viewport and scissor
                Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)

                // bind descriptor set
                let mutable descriptorSet = env.Pipeline.VkDescriptorSet 0
                Vulkan.vkCmdBindDescriptorSets
                    (cb, VkPipelineBindPoint.Graphics,
                     env.Pipeline.PipelineLayout, 0u,
                     1u, asPointer &descriptorSet,
                     0u, nullPtr)

                // push draw index
                Vulkan.vkCmdPushConstants
                    (cb, env.Pipeline.PipelineLayout,
                     Hl.VertexFragmentStage.VkShaderStageFlags,
                     0u, 4u, asVoidPtr &env.DrawIndex)
                
                // draw
                Vulkan.vkCmdDraw (cb, uint (6 * env.SpriteIndex), 1u, 0u, 0u)
                Hl.reportDrawCall env.SpriteIndex
                
                // end render
                Vulkan.vkCmdEndRendering cb
            
            // next batch
            env.DrawIndex <- inc env.DrawIndex
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private RestartSpriteBatch state viewport env =
        EndSpriteBatch viewport env
        BeginSpriteBatch state env

    /// Begin a new sprite batch frame.
    let BeginSpriteBatchFrame
        (viewProjection2dAbsolute : Matrix4x4 inref,
         viewProjection2dRelative : Matrix4x4 inref,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         env) =
        env.DrawIndex <- 0
        env.ViewProjection2dAbsolute <- viewProjection2dAbsolute
        env.ViewProjection2dRelative <- viewProjection2dRelative
        env.ViewProjectionClipAbsolute <- viewProjectionClipAbsolute
        env.ViewProjectionClipRelative <- viewProjectionClipRelative
        BeginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let EndSpriteBatchFrame viewport env =
        EndSpriteBatch viewport env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn viewport env =
        let state = env.State
        EndSpriteBatch viewport env
        fn ()
        BeginSpriteBatch state env

    let
#if !DEBUG
        inline
#endif
        // TODO: DJL: maybe remove this process as it just gets reversed at draw time?
        private PopulateSpriteBatchVertex (perimeter : Box2) (pivot : Vector2) (rotation : single) (texCoords : Box2) (color : Color) env =
        let perimeterOffset = env.SpriteIndex * 4
        env.Perimeters.[perimeterOffset] <- perimeter.Min.X
        env.Perimeters.[perimeterOffset + 1] <- perimeter.Min.Y
        env.Perimeters.[perimeterOffset + 2] <- perimeter.Size.X
        env.Perimeters.[perimeterOffset + 3] <- perimeter.Size.Y
        let pivotOffset = env.SpriteIndex * 2
        env.Pivots.[pivotOffset] <- pivot.X
        env.Pivots.[pivotOffset + 1] <- pivot.Y
        let rotationOffset = env.SpriteIndex
        env.Rotations.[rotationOffset] <- rotation
        let texCoordsOffset = env.SpriteIndex * 4
        env.TexCoordses.[texCoordsOffset] <- texCoords.Min.X
        env.TexCoordses.[texCoordsOffset + 1] <- texCoords.Min.Y
        env.TexCoordses.[texCoordsOffset + 2] <- texCoords.Size.X
        env.TexCoordses.[texCoordsOffset + 3] <- texCoords.Size.Y
        let colorOffset = env.SpriteIndex * 4
        env.Colors.[colorOffset] <- color.R
        env.Colors.[colorOffset + 1] <- color.G
        env.Colors.[colorOffset + 2] <- color.B
        env.Colors.[colorOffset + 3] <- color.A

    /// Submit a sprite to the appropriate sprite batch.
    let SubmitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, clipOpt : (Box2 voption) inref, color : Color inref, blend, texture : Texture.Texture, viewport, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute clipOpt blend texture
        if SpriteBatchState.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartSpriteBatch state viewport env

        // populate vertices
        let perimeter = box2 min size
        PopulateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let CreateSpriteBatchEnv vkc =
        
        // create pipeline
        let (spriteUniform, viewProjectionUniform, pipeline) = CreateSpriteBatchPipeline vkc

        // create env
        { DrawIndex = 0; SpriteIndex = 0;
          ViewProjection2dAbsolute = m4Identity; ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity; ViewProjectionClipRelative = m4Identity
          VulkanContext = vkc; Pipeline = pipeline; SpriteUniform = spriteUniform; ViewProjectionUniform = viewProjectionUniform
          Perimeters = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Pivots = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2)
          Rotations = Array.zeroCreate (Constants.Render.SpriteBatchSize)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          State = SpriteBatchState.defaultState }

    /// Destroy the given sprite batch environment.
    let DestroySpriteBatchEnv env =
        let vkc = env.VulkanContext
        Pipeline.Pipeline.destroy env.Pipeline vkc
        Buffer.Buffer.destroy env.SpriteUniform vkc
        Buffer.Buffer.destroy env.ViewProjectionUniform vkc
