// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SpriteBatch =

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
             | struct (ValueSome c, ValueSome c2) -> box2Neq c c2) ||
            state.Blend <> state2.Blend ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> false
             | struct (ValueSome t, ValueSome t2) -> t.VulkanTexture <> t2.VulkanTexture)

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
              PerimetersUniform : Buffer.BufferAccumulator
              PivotsUniform : Buffer.BufferAccumulator
              RotationsUniform : Buffer.BufferAccumulator
              TexCoordsesUniform : Buffer.BufferAccumulator
              ColorsUniform : Buffer.BufferAccumulator
              ViewProjectionUniform : Buffer.BufferAccumulator
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              mutable State : SpriteBatchState }

    /// Create a sprite batch pipeline.
    let CreateSpriteBatchPipeline (vkc : Hl.VulkanContext) =
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                true true [|Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite|] [||] [||]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 2 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 4 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 5 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 6 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                [||] vkc

        // create sprite batch uniform buffers
        let perimetersUniform = Buffer.BufferAccumulator.createStrided16 Constants.Render.SpriteBatchSize Buffer.Uniform vkc
        let pivotsUniform = Buffer.BufferAccumulator.createStrided16 Constants.Render.SpriteBatchSize Buffer.Uniform vkc
        let rotationsUniform = Buffer.BufferAccumulator.createStrided16 Constants.Render.SpriteBatchSize Buffer.Uniform vkc
        let texCoordsesUniform = Buffer.BufferAccumulator.createStrided16 Constants.Render.SpriteBatchSize Buffer.Uniform vkc
        let colorsUniform = Buffer.BufferAccumulator.createStrided16 Constants.Render.SpriteBatchSize Buffer.Uniform vkc
        let viewProjectionUniform = Buffer.BufferAccumulator.create (sizeof<single> * 16) Buffer.Uniform vkc

        // fin
        (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline)
    
    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch (viewport : Viewport) env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

            // init render
            let vkc = env.VulkanContext
            let cb = vkc.RenderCommandBuffer
            let mutable renderArea = VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

            // pin uniform arrays to pass the addresses because we don't want to upload the entire arrays every frame
            use perimetersPin = new ArrayPin<_> (env.Perimeters)
            use pivotsPin = new ArrayPin<_> (env.Pivots)
            use rotationsPin = new ArrayPin<_> (env.Rotations)
            use texCoordsesPin = new ArrayPin<_> (env.TexCoordses)
            use colorsPin = new ArrayPin<_> (env.Colors)
            
            // update uniform buffers
            Buffer.BufferAccumulator.uploadStrided16 env.DrawIndex 0 16 env.SpriteIndex perimetersPin.NativeInt env.PerimetersUniform vkc
            Buffer.BufferAccumulator.uploadStrided16 env.DrawIndex 0 8 env.SpriteIndex pivotsPin.NativeInt env.PivotsUniform vkc
            Buffer.BufferAccumulator.uploadStrided16 env.DrawIndex 0 4 env.SpriteIndex rotationsPin.NativeInt env.RotationsUniform vkc
            Buffer.BufferAccumulator.uploadStrided16 env.DrawIndex 0 16 env.SpriteIndex texCoordsesPin.NativeInt env.TexCoordsesUniform vkc
            Buffer.BufferAccumulator.uploadStrided16 env.DrawIndex 0 16 env.SpriteIndex colorsPin.NativeInt env.ColorsUniform vkc
            Buffer.BufferAccumulator.uploadArray env.DrawIndex 0 (if env.State.Absolute then env.ViewProjection2dAbsolute.ToArray () else env.ViewProjection2dRelative.ToArray ()) env.ViewProjectionUniform vkc

            // update descriptors
            Pipeline.Pipeline.updateDescriptorsUniform 0 env.PerimetersUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 1 env.PivotsUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 2 env.RotationsUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 3 env.TexCoordsesUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 4 env.ColorsUniform env.Pipeline vkc
            Pipeline.Pipeline.updateDescriptorsUniform 5 env.ViewProjectionUniform env.Pipeline vkc
            Pipeline.Pipeline.writeDescriptorTexture 6 env.DrawIndex texture.VulkanTexture env.Pipeline vkc

            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline env.State.Blend env.Pipeline
            Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

            // set viewport and scissor
            let mutable vkViewport = Hl.makeViewport true renderArea
            let mutable scissor = renderArea
            match env.State.ClipOpt with
            | ValueSome clip ->
                let viewProjection = if env.State.Absolute then env.ViewProjectionClipAbsolute else env.ViewProjectionClipRelative
                let minClip = Vector4.Transform (Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection)
                let minNdc = minClip / minClip.W * single viewport.DisplayScalar
                let minScissor = (minNdc.V2 + v2One) * 0.5f * viewport.Bounds.Size.V2
                let sizeScissor = clip.Size * v2Dup (single viewport.DisplayScalar)
                let offset = viewport.Bounds.Min
                scissor <-
                    VkRect2D
                        ((minScissor.X |> round |> int) + offset.X,
                         (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                         uint sizeScissor.X,
                         uint sizeScissor.Y)
                scissor <- Hl.clampRectToRect renderArea scissor // TODO: DJL: check post clamp validity.
            | ValueNone -> ()
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)

            // push draw index
            Vulkan.vkCmdPushConstants (cb, env.Pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT ||| Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT, 0u, 4u, asVoidPtr &env.DrawIndex)
            
            // bind descriptor set
            let mutable descriptorSet = env.Pipeline.DescriptorSet
            Vulkan.vkCmdBindDescriptorSets
                (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS,
                 env.Pipeline.PipelineLayout, 0u,
                 1u, asPointer &descriptorSet,
                 0u, nullPtr)

            // draw
            Vulkan.vkCmdDraw (cb, uint (6 * env.SpriteIndex), 1u, 0u, 0u)
            Hl.reportDrawCall env.SpriteIndex
            
            // reset scissor
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &renderArea)
            
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
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline) = CreateSpriteBatchPipeline vkc

        // create env
        { DrawIndex = 0; SpriteIndex = 0;
          ViewProjection2dAbsolute = m4Identity; ViewProjection2dRelative = m4Identity
          ViewProjectionClipAbsolute = m4Identity; ViewProjectionClipRelative = m4Identity
          VulkanContext = vkc; Pipeline = pipeline
          PerimetersUniform = perimetersUniform; PivotsUniform = pivotsUniform; RotationsUniform = rotationsUniform
          TexCoordsesUniform = texCoordsesUniform; ColorsUniform = colorsUniform; ViewProjectionUniform = viewProjectionUniform
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
        Buffer.BufferAccumulator.destroy env.PerimetersUniform vkc
        Buffer.BufferAccumulator.destroy env.PivotsUniform vkc
        Buffer.BufferAccumulator.destroy env.RotationsUniform vkc
        Buffer.BufferAccumulator.destroy env.TexCoordsesUniform vkc
        Buffer.BufferAccumulator.destroy env.ColorsUniform vkc
        Buffer.BufferAccumulator.destroy env.ViewProjectionUniform vkc
