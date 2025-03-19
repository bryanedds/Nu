// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

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
              VulkanGlobal : Hl.VulkanGlobal
              Pipeline : Pipeline.Pipeline
              PerimetersUniform : Hl.FifBuffer
              PivotsUniform : Hl.FifBuffer
              RotationsUniform : Hl.FifBuffer
              TexCoordsesUniform : Hl.FifBuffer
              ColorsUniform : Hl.FifBuffer
              ViewProjectionUniform : Hl.FifBuffer
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              mutable State : SpriteBatchState }

    /// Create a sprite batch pipeline.
    let CreateSpriteBatchPipeline (vkg : Hl.VulkanGlobal) =
        
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
                [||] vkg.RenderPass vkg.Device

        // create sprite batch uniform buffers
        let perimetersUniform = Hl.FifBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) vkg
        let pivotsUniform = Hl.FifBuffer.createUniform (sizeof<single> * 2 * Constants.Render.SpriteBatchSize) vkg
        let rotationsUniform = Hl.FifBuffer.createUniform (sizeof<single> * 1 * Constants.Render.SpriteBatchSize) vkg
        let texCoordsesUniform = Hl.FifBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) vkg
        let colorsUniform = Hl.FifBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) vkg
        let viewProjectionUniform = Hl.FifBuffer.createUniform (sizeof<single> * 16) vkg

        // write sprite batch descriptor set
        Pipeline.Pipeline.writeDescriptorUniform 0 0 perimetersUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 1 0 pivotsUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 2 0 rotationsUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 3 0 texCoordsesUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 4 0 colorsUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 5 0 viewProjectionUniform pipeline vkg.Device
        
        // fin
        (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline)
    
    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->

            // init render
            let vkg = env.VulkanGlobal
            let cb = vkg.RenderCommandBuffer
            let mutable renderArea = VkRect2D (VkOffset2D.Zero, vkg.SwapExtent)
            Hl.beginRenderBlock cb vkg.RenderPass vkg.SwapchainFramebuffer renderArea [||] vkg.InFlightFence vkg.Device

            // pin uniform arrays
            use perimetersPin = new ArrayPin<_> (env.Perimeters)
            use pivotsPin = new ArrayPin<_> (env.Pivots)
            use rotationsPin = new ArrayPin<_> (env.Rotations)
            use texCoordsesPin = new ArrayPin<_> (env.TexCoordses)
            use colorsPin = new ArrayPin<_> (env.Colors)
            
            // update uniform buffers
            Hl.FifBuffer.upload 0 (sizeof<single> * 4 * env.SpriteIndex) perimetersPin.NativeInt env.PerimetersUniform vkg
            Hl.FifBuffer.upload 0 (sizeof<single> * 2 * env.SpriteIndex) pivotsPin.NativeInt env.PivotsUniform vkg
            Hl.FifBuffer.upload 0 (sizeof<single> * 1 * env.SpriteIndex) rotationsPin.NativeInt env.RotationsUniform vkg
            Hl.FifBuffer.upload 0 (sizeof<single> * 4 * env.SpriteIndex) texCoordsesPin.NativeInt env.TexCoordsesUniform vkg
            Hl.FifBuffer.upload 0 (sizeof<single> * 4 * env.SpriteIndex) colorsPin.NativeInt env.ColorsUniform vkg
            Hl.FifBuffer.uploadArray 0 (if env.State.Absolute then env.ViewProjectionAbsolute.ToArray () else env.ViewProjectionRelative.ToArray ()) env.ViewProjectionUniform vkg

            // write texture to descriptor set
            Pipeline.Pipeline.writeDescriptorTextureSingleFrame 6 0 texture.VulkanTexture env.Pipeline vkg.Device

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
            Hl.endRenderBlock cb vkg.GraphicsQueue [||] [||] vkg.InFlightFence
            
            // next batch
            env.SpriteIndex <- 0

        // not ready
        | ValueSome _ | ValueNone -> ()

    let private RestartSpriteBatch state env =
        EndSpriteBatch env
        BeginSpriteBatch state env

    /// Begin a new sprite batch frame.
    let BeginSpriteBatchFrame (viewProjectionAbsolute : Matrix4x4 inref, viewProjectionRelative : Matrix4x4 inref, env) =
        env.ViewProjectionAbsolute <- viewProjectionAbsolute
        env.ViewProjectionRelative <- viewProjectionRelative
        BeginSpriteBatch SpriteBatchState.defaultState env

    /// End the current sprite batch frame, if any.
    let EndSpriteBatchFrame env =
        EndSpriteBatch env

    /// Forcibly end the current sprite batch frame, if any, run the given fn, then restart the sprite batch frame.
    let InterruptSpriteBatchFrame fn env =
        let state = env.State
        EndSpriteBatch env
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
    let SubmitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, color : Color inref, blend, texture : Texture.Texture, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute blend texture
        if SpriteBatchState.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartSpriteBatch state env

        // populate vertices
        let perimeter = box2 min size
        PopulateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let CreateSpriteBatchEnv vkg =
        
        // create pipeline
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline) = CreateSpriteBatchPipeline vkg

        // create env
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity; VulkanGlobal = vkg
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
        let vkg = env.VulkanGlobal
        Pipeline.Pipeline.destroy env.Pipeline vkg.Device
        Hl.FifBuffer.destroy env.PerimetersUniform vkg
        Hl.FifBuffer.destroy env.PivotsUniform vkg
        Hl.FifBuffer.destroy env.RotationsUniform vkg
        Hl.FifBuffer.destroy env.TexCoordsesUniform vkg
        Hl.FifBuffer.destroy env.ColorsUniform vkg
        Hl.FifBuffer.destroy env.ViewProjectionUniform vkg
        
        // reset sprite index
        env.SpriteIndex <- 0