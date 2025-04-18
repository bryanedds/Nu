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
          Blend : Pipeline.Blend
          TextureOpt : Texture.Texture ValueOption }

        static member inline changed state state2 =
            state.Absolute <> state2.Absolute ||
            state.Blend <> state2.Blend ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> true
             | struct (ValueSome t, ValueSome t2) -> t.VulkanTexture <> t2.VulkanTexture) // TODO: consider implementing Texture.equals and maybe texEq / texNeq.

        static member make absolute blend texture =
            { Absolute = absolute; Blend = blend; TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false; Blend = Pipeline.Transparent; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching sprites.
    type [<ReferenceEquality>] SpriteBatchEnv =
        private
            { mutable SpriteIndex : int
              mutable ViewProjectionAbsolute : Matrix4x4
              mutable ViewProjectionRelative : Matrix4x4
              VulkanGlobal : Hl.VulkanContext
              Pipeline : Pipeline.Pipeline
              PerimetersUniform : VulkanMemory.FifBuffer
              PivotsUniform : VulkanMemory.FifBuffer
              RotationsUniform : VulkanMemory.FifBuffer
              TexCoordsesUniform : VulkanMemory.FifBuffer
              ColorsUniform : VulkanMemory.FifBuffer
              ViewProjectionUniform : VulkanMemory.FifBuffer
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
                true [|Pipeline.Transparent; Pipeline.Additive; Pipeline.Overwrite|] [||] [||]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 2 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 4 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 5 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 6 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                [||] vkc.RenderPass vkc.Device

        // create sprite batch uniform buffers
        let perimetersUniform = VulkanMemory.FifBuffer.createUniformStrided16 Constants.Render.SpriteBatchSize vkc
        let pivotsUniform = VulkanMemory.FifBuffer.createUniformStrided16 Constants.Render.SpriteBatchSize vkc
        let rotationsUniform = VulkanMemory.FifBuffer.createUniformStrided16 Constants.Render.SpriteBatchSize vkc
        let texCoordsesUniform = VulkanMemory.FifBuffer.createUniformStrided16 Constants.Render.SpriteBatchSize vkc
        let colorsUniform = VulkanMemory.FifBuffer.createUniformStrided16 Constants.Render.SpriteBatchSize vkc
        let viewProjectionUniform = VulkanMemory.FifBuffer.createUniform (sizeof<single> * 16) vkc

        // write sprite batch descriptor set
        Pipeline.Pipeline.writeDescriptorUniform 0 0 perimetersUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 1 0 pivotsUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 2 0 rotationsUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 3 0 texCoordsesUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 4 0 colorsUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 5 0 viewProjectionUniform.PerFrameBuffers pipeline vkc.Device
        
        // fin
        (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline)
    
    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch (viewport : Viewport) env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

            // init render
            let vkc = env.VulkanGlobal
            let cb = vkc.RenderCommandBuffer
            let mutable renderArea = VkRect2D (VkOffset2D.Zero, vkc.SwapExtent)
            Hl.beginRenderBlock cb vkc.RenderPass vkc.SwapchainFramebuffer renderArea [||] vkc.InFlightFence vkc.Device

            // pin uniform arrays
            use perimetersPin = new ArrayPin<_> (env.Perimeters)
            use pivotsPin = new ArrayPin<_> (env.Pivots)
            use rotationsPin = new ArrayPin<_> (env.Rotations)
            use texCoordsesPin = new ArrayPin<_> (env.TexCoordses)
            use colorsPin = new ArrayPin<_> (env.Colors)
            
            // update uniform buffers
            VulkanMemory.FifBuffer.uploadStrided16 0 16 env.SpriteIndex perimetersPin.NativeInt env.PerimetersUniform vkc
            VulkanMemory.FifBuffer.uploadStrided16 0 8 env.SpriteIndex pivotsPin.NativeInt env.PivotsUniform vkc
            VulkanMemory.FifBuffer.uploadStrided16 0 4 env.SpriteIndex rotationsPin.NativeInt env.RotationsUniform vkc
            VulkanMemory.FifBuffer.uploadStrided16 0 16 env.SpriteIndex texCoordsesPin.NativeInt env.TexCoordsesUniform vkc
            VulkanMemory.FifBuffer.uploadStrided16 0 16 env.SpriteIndex colorsPin.NativeInt env.ColorsUniform vkc
            VulkanMemory.FifBuffer.uploadArray 0 (if env.State.Absolute then env.ViewProjectionAbsolute.ToArray () else env.ViewProjectionRelative.ToArray ()) env.ViewProjectionUniform vkc

            // write texture to descriptor set
            Pipeline.Pipeline.writeDescriptorTextureSingleFrame 6 0 texture.VulkanTexture env.Pipeline vkc.Device

            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline env.State.Blend env.Pipeline
            Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

            // set viewport and scissor
            let mutable viewport = Hl.makeViewport renderArea
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &viewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &renderArea)

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
            
            // flush render commands
            Hl.endRenderBlock cb vkc.GraphicsQueue [||] [||] vkc.InFlightFence
            
            // next batch
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private RestartSpriteBatch state viewport env =
        EndSpriteBatch viewport env
        BeginSpriteBatch state env

    /// Begin a new sprite batch frame.
    let BeginSpriteBatchFrame (viewProjectionAbsolute : Matrix4x4 inref, viewProjectionRelative : Matrix4x4 inref, env) =
        env.ViewProjectionAbsolute <- viewProjectionAbsolute
        env.ViewProjectionRelative <- viewProjectionRelative
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
        let state = SpriteBatchState.make absolute blend texture
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
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity; VulkanGlobal = vkc
          PerimetersUniform = perimetersUniform; PivotsUniform = pivotsUniform; RotationsUniform = rotationsUniform
          TexCoordsesUniform = texCoordsesUniform; ColorsUniform = colorsUniform; ViewProjectionUniform = viewProjectionUniform
          Pipeline = pipeline
          Perimeters = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Pivots = Array.zeroCreate (Constants.Render.SpriteBatchSize * 2)
          Rotations = Array.zeroCreate (Constants.Render.SpriteBatchSize)
          TexCoordses = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          Colors = Array.zeroCreate (Constants.Render.SpriteBatchSize * 4)
          State = SpriteBatchState.defaultState }

    /// Destroy the given sprite batch environment.
    let DestroySpriteBatchEnv env =
        
        // destroy Vulkan resources
        let vkc = env.VulkanGlobal
        Pipeline.Pipeline.destroy env.Pipeline vkc.Device
        VulkanMemory.FifBuffer.destroy env.PerimetersUniform vkc
        VulkanMemory.FifBuffer.destroy env.PivotsUniform vkc
        VulkanMemory.FifBuffer.destroy env.RotationsUniform vkc
        VulkanMemory.FifBuffer.destroy env.TexCoordsesUniform vkc
        VulkanMemory.FifBuffer.destroy env.ColorsUniform vkc
        VulkanMemory.FifBuffer.destroy env.ViewProjectionUniform vkc
        
        // reset sprite index
        env.SpriteIndex <- 0