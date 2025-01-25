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
          TextureOpt : Texture.Texture ValueOption }

        static member inline changed state state2 =
            state.Absolute <> state2.Absolute ||
            (match struct (state.TextureOpt, state2.TextureOpt) with
             | struct (ValueSome _, ValueNone) -> true
             | struct (ValueNone, ValueSome _) -> true
             | struct (ValueNone, ValueNone) -> true
             // TODO: DJL: figure out how this works with VulkanTexture.
             | struct (ValueSome t, ValueSome t2) -> t.VulkanTexture <> t2.VulkanTexture) // TODO: consider implementing Texture.equals and maybe texEq / texNeq.

        static member make absolute bfs bfd beq texture =
            { Absolute = absolute; TextureOpt = ValueSome texture }

        static member defaultState =
            { Absolute = false; TextureOpt = ValueNone }

    /// The environment that contains the internal state required for batching sprites.
    type [<ReferenceEquality>] SpriteBatchEnv =
        private
            { mutable SpriteIndex : int
              mutable ViewProjectionAbsolute : Matrix4x4
              mutable ViewProjectionRelative : Matrix4x4
              PerimetersUniform : Hl.AllocatedBuffer
              TexCoordsesUniform : Hl.AllocatedBuffer
              PivotsUniform : Hl.AllocatedBuffer
              RotationsUniform : Hl.AllocatedBuffer
              ColorsUniform : Hl.AllocatedBuffer
              ViewProjectionUniform : Hl.AllocatedBuffer
              Pipeline : Pipeline.SpriteBatchPipeline
              Perimeters : single array
              Pivots : single array
              Rotations : single array
              TexCoordses : single array
              Colors : single array
              mutable State : SpriteBatchState }

    /// Create a sprite batch pipeline.
    let CreateSpriteBatchPipeline (vulkanGlobal : Hl.VulkanGlobal) =
        
        // commonly used handles
        let device = vulkanGlobal.Device
        let allocator = vulkanGlobal.VmaAllocator
        
        // create sprite batch pipeline
        let pipeline =
            Pipeline.SpriteBatchPipeline.create
                Constants.Paths.SpriteBatchShaderFilePath
                true (Hl.makeBlendAttachmentAlpha ()) [||] [||] // TODO: DJL: integrate actual blend values.
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 2 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 4 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 5 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 6 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                [||] vulkanGlobal.RenderPass device

        // create sprite batch uniform buffers
        let perimetersUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) allocator
        let pivotsUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 2 * Constants.Render.SpriteBatchSize) allocator
        let rotationsUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 1 * Constants.Render.SpriteBatchSize) allocator
        let texCoordsesUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) allocator
        let colorsUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 4 * Constants.Render.SpriteBatchSize) allocator
        let viewProjectionUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 16) allocator

        // write sprite batch descriptor set
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 0 0 perimetersUniform pipeline device
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 1 0 pivotsUniform pipeline device
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 2 0 rotationsUniform pipeline device
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 3 0 texCoordsesUniform pipeline device
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 4 0 colorsUniform pipeline device
        Pipeline.SpriteBatchPipeline.writeDescriptorUniform 5 0 viewProjectionUniform pipeline device
        
        // fin
        (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline)
    
    let private BeginSpriteBatch state env =
        env.State <- state

    let private EndSpriteBatch env =

        // ensure something to draw
        match env.State.TextureOpt with
        | ValueSome texture when env.SpriteIndex > 0 ->


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
    let SubmitSpriteBatchSprite (absolute, min : Vector2, size : Vector2, pivot : Vector2, rotation, texCoords : Box2 inref, color : Color inref, bfs, bfd, beq, texture : Texture.Texture, env) =

        // adjust to potential sprite batch state changes
        let state = SpriteBatchState.make absolute bfs bfd beq texture
        if SpriteBatchState.changed state env.State || env.SpriteIndex = Constants.Render.SpriteBatchSize then
            RestartSpriteBatch state env

        // populate vertices
        let perimeter = box2 min size
        PopulateSpriteBatchVertex perimeter pivot rotation texCoords color env

        // advance sprite index
        env.SpriteIndex <- inc env.SpriteIndex

    /// Destroy the given sprite batch environment.
    let CreateSpriteBatchEnv vulkanGlobal =
        
        // create pipeline
        let (perimetersUniform, pivotsUniform, rotationsUniform, texCoordsesUniform, colorsUniform, viewProjectionUniform, pipeline) = CreateSpriteBatchPipeline vulkanGlobal

        // create env
        { SpriteIndex = 0; ViewProjectionAbsolute = m4Identity; ViewProjectionRelative = m4Identity
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
    let DestroySpriteBatchEnv (vulkanGlobal : Hl.VulkanGlobal) env =
        
        // common handle
        let allocator = vulkanGlobal.VmaAllocator

        // destroy Vulkan resources
        Pipeline.SpriteBatchPipeline.destroy env.Pipeline vulkanGlobal.Device
        Hl.AllocatedBuffer.destroy env.PerimetersUniform allocator
        Hl.AllocatedBuffer.destroy env.PivotsUniform allocator
        Hl.AllocatedBuffer.destroy env.RotationsUniform allocator
        Hl.AllocatedBuffer.destroy env.TexCoordsesUniform allocator
        Hl.AllocatedBuffer.destroy env.ColorsUniform allocator
        Hl.AllocatedBuffer.destroy env.ViewProjectionUniform allocator
        
        // reset sprite index
        env.SpriteIndex <- 0