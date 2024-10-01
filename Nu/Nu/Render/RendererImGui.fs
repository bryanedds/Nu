// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open System.Runtime.CompilerServices
open ImGuiNET
open Vulkan.Hl
open Vortice.Vulkan
open type Vulkan
open Vortice.ShaderCompiler
open Prime

/// Renders an imgui view.
/// NOTE: API is object-oriented / mutation-based because it's ported from a port.
type RendererImGui =
    abstract Initialize : ImFontAtlasPtr -> unit
    abstract Render : ImDrawDataPtr -> unit
    abstract CleanUp : unit -> unit

/// A stub imgui renderer.
type StubRendererImGui () =
    interface RendererImGui with
        member this.Initialize fonts =
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontTextureWidth = 0
            let mutable fontTextureHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            fonts.ClearTexData ()
        member this.Render _ = ()
        member this.CleanUp () = ()

[<RequireQualifiedAccess>]
module StubRendererImGui =

    /// Make a stub imgui renderer.
    let make fonts =
        let rendererImGui = StubRendererImGui ()
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via OpenGL.
type GlRendererImGui (windowWidth : int, windowHeight : int) =

    let mutable vertexArrayObject = 0u
    let mutable vertexBufferSize = 8192u
    let mutable vertexBuffer = 0u
    let mutable indexBufferSize = 1024u
    let mutable indexBuffer = 0u
    let mutable shader = 0u
    let mutable shaderProjectionMatrixUniform = 0
    let mutable shaderFontTextureUniform = 0
    let mutable fontTextureWidth = 0
    let mutable fontTextureHeight = 0
    let mutable fontTexture = Unchecked.defaultof<OpenGL.Texture.Texture>
    do ignore windowWidth

    interface RendererImGui with

        member this.Initialize (fonts : ImFontAtlasPtr) =
        
            // initialize vao
            vertexArrayObject <- OpenGL.Gl.GenVertexArray ()
            OpenGL.Gl.BindVertexArray vertexArrayObject
            OpenGL.Hl.Assert ()

            // create vertex buffer
            vertexBuffer <- OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, vertexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Hl.Assert ()

            // create index buffer
            indexBuffer <- OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Hl.Assert ()

            // configure vao
            let stride = Unsafe.SizeOf<ImDrawVert> ()
            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 0)
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 8)
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribPointerType.UnsignedByte, true, stride, 16)
            OpenGL.Hl.Assert ()

            // finalize vao
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Hl.Assert ()

            // vertex shader code
            // TODO: let's put this code into a .glsl file and load it from there.
            let vertexStr =
                [Constants.OpenGL.GlslVersionPragma
                 ""
                 "uniform mat4 projection;"
                 ""
                 "layout (location = 0) in vec2 position;"
                 "layout (location = 1) in vec2 texCoords;"
                 "layout (location = 2) in vec4 color;"
                 ""
                 "out vec4 colorOut;"
                 "out vec2 texCoordsOut;"
                 ""
                 "void main()"
                 "{"
                 "    colorOut = color;"
                 "    texCoordsOut = texCoords;"
                 "    gl_Position = projection * vec4(position, 0, 1);"
                 "}"] |> String.join "\n"

            // fragment shader code
            let fragmentStr =
                [Constants.OpenGL.GlslVersionPragma
                 ""
                 "uniform sampler2D fontTexture;"
                 ""
                 "in vec4 colorOut;"
                 "in vec2 texCoordsOut;"
                 ""
                 "out vec4 frag;"
                 ""
                 "void main()"
                 "{"
                 "    frag = colorOut * texture(fontTexture, texCoordsOut);"
                 "}"] |> String.join "\n"

            // create shader
            shader <- OpenGL.Shader.CreateShaderFromStrs (vertexStr, fragmentStr)
            shaderProjectionMatrixUniform <- OpenGL.Gl.GetUniformLocation (shader, "projection")
            shaderFontTextureUniform <- OpenGL.Gl.GetUniformLocation (shader, "fontTexture")
            OpenGL.Hl.Assert ()

            // create font texture
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            let fontTextureId = OpenGL.Gl.GenTexture ()
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, fontTextureId)
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, Constants.OpenGL.UncompressedTextureFormat, fontTextureWidth, fontTextureHeight, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, pixels)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapS, int OpenGL.TextureWrapMode.Repeat)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapT, int OpenGL.TextureWrapMode.Repeat)
            let fontTextureMetadata = OpenGL.Texture.TextureMetadata.make fontTextureWidth fontTextureHeight
            fontTexture <- OpenGL.Texture.EagerTexture { TextureMetadata = fontTextureMetadata; TextureId = fontTextureId }
            fonts.SetTexID (nativeint fontTexture.TextureId)
            fonts.ClearTexData ()

        member this.Render (drawData : ImDrawDataPtr) =

            // attempt to draw imgui draw data
            let mutable vertexOffsetInVertices = 0
            let mutable indexOffsetInElements = 0
            if drawData.CmdListsCount <> 0 then

                // resize vertex buffer if necessary
                let vertexBufferSizeNeeded = uint (drawData.TotalVtxCount * Unsafe.SizeOf<ImDrawVert>())
                if vertexBufferSizeNeeded > vertexBufferSize then
                    vertexBufferSize <- max (vertexBufferSize * 2u) vertexBufferSizeNeeded
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, vertexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
                    OpenGL.Hl.Assert ()

                // resize index buffer if necessary
                let indexBufferSizeNeeded = uint (drawData.TotalIdxCount * sizeof<uint16>)
                if indexBufferSizeNeeded > indexBufferSize then
                    indexBufferSize <- max (indexBufferSize * 2u) indexBufferSizeNeeded
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
                    OpenGL.Hl.Assert ()

                // compute offsets
                let cmdsRange = drawData.CmdListsRange
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmds = cmdsRange.[i]
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, nativeint (vertexOffsetInVertices * Unsafe.SizeOf<ImDrawVert> ()), uint (cmds.VtxBuffer.Size * Unsafe.SizeOf<ImDrawVert> ()), cmds.VtxBuffer.Data)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ElementArrayBuffer, nativeint (indexOffsetInElements * sizeof<uint16>), uint (cmds.IdxBuffer.Size * sizeof<uint16>), cmds.IdxBuffer.Data)
                    vertexOffsetInVertices <- vertexOffsetInVertices + cmds.VtxBuffer.Size
                    indexOffsetInElements <- indexOffsetInElements + cmds.IdxBuffer.Size
                    OpenGL.Hl.Assert ()

                // compute orthographic projection
                let projection = Matrix4x4.CreateOrthographicOffCenter (0.0f, single windowWidth, single windowHeight, 0.0f, -1.0f, 1.0f)
                let projectionArray = projection.ToArray ()

                // setup state
                OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
                OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
                OpenGL.Gl.Enable OpenGL.EnableCap.Blend
                OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
                OpenGL.Hl.Assert ()

                // setup vao
                OpenGL.Gl.BindVertexArray vertexArrayObject
                OpenGL.Hl.Assert ()

                // setup shader
                OpenGL.Gl.UseProgram shader
                OpenGL.Gl.UniformMatrix4 (shaderProjectionMatrixUniform, false, projectionArray)
                OpenGL.Gl.Uniform1 (shaderFontTextureUniform, 0) // TODO: use bindless textures for imgui?
                OpenGL.Hl.Assert ()

                // draw command lists
                let mutable vertexOffset = 0
                let mutable indexOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmdsRange = drawData.CmdListsRange
                    let cmds = cmdsRange.[i]
                    for cmd in 0 .. dec cmds.CmdBuffer.Size do
                        let pcmds = cmds.CmdBuffer
                        let pcmd = pcmds.[cmd]
                        if pcmd.UserCallback = nativeint 0 then
                            let clip = pcmd.ClipRect
                            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
                            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, uint pcmd.TextureId)
                            OpenGL.Gl.Scissor (int clip.X, windowHeight - int clip.W, int (clip.Z - clip.X), int (clip.W - clip.Y))
                            OpenGL.Gl.DrawElementsBaseVertex (OpenGL.PrimitiveType.Triangles, int pcmd.ElemCount, OpenGL.DrawElementsType.UnsignedShort, nativeint (indexOffset * sizeof<uint16>), int pcmd.VtxOffset + vertexOffset)
                            OpenGL.Hl.ReportDrawCall 1
                            OpenGL.Hl.Assert ()
                        else raise (NotImplementedException ())
                        indexOffset <- indexOffset + int pcmd.ElemCount
                    vertexOffset <- vertexOffset + cmds.VtxBuffer.Size

                // teardown shader
                OpenGL.Gl.UseProgram 0u
                OpenGL.Hl.Assert ()

                // teardown vao
                OpenGL.Gl.BindVertexArray 0u
                OpenGL.Hl.Assert ()

                // teardown state
                OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
                OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero)
                OpenGL.Gl.Disable OpenGL.EnableCap.Blend
                OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest

        member this.CleanUp () =

            // destroy vao
            OpenGL.Gl.BindVertexArray vertexArrayObject
            OpenGL.Gl.DeleteBuffers [|vertexBuffer|]
            OpenGL.Gl.DeleteBuffers [|indexBuffer|]
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Gl.DeleteVertexArrays [|vertexArrayObject|]
            OpenGL.Hl.Assert ()

            // destroy shader
            OpenGL.Gl.DeleteProgram shader
            OpenGL.Hl.Assert ()

            // destroy font texture
            fontTexture.Destroy ()

[<RequireQualifiedAccess>]
module GlRendererImGui =

    /// Make a gl-based imgui renderer.
    let make fonts =
        let rendererImGui = GlRendererImGui (Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via Vulkan.
type VulkanRendererImGui (vulkanGlobal : VulkanGlobal) =
    
    let device = vulkanGlobal.Device
    let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
    let mutable sampler = Unchecked.defaultof<VkSampler>
    let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
    let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
    let mutable pipeline = Unchecked.defaultof<VkPipeline>
    
    /// Create the descriptor pool for the font atlas.
    static member createDescriptorPool device =
        
        // handle
        let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
        
        // create pool size
        let mutable poolSize = VkDescriptorPoolSize ()
        poolSize.``type`` <- VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        poolSize.descriptorCount <- 1u

        // populate create info
        let mutable createInfo = VkDescriptorPoolCreateInfo ()
        createInfo.maxSets <- 1u
        createInfo.poolSizeCount <- 1u
        createInfo.pPoolSizes <- asPointer &poolSize

        // create descriptor pool
        vkCreateDescriptorPool (device, &createInfo, nullPtr, &descriptorPool) |> check

        // fin
        descriptorPool

    /// Create the sampler used to sample the font atlas.
    static member createSampler device =
        
        // handle
        let mutable sampler = Unchecked.defaultof<VkSampler>

        // populate create info
        let mutable createInfo = VkSamplerCreateInfo ()
        createInfo.magFilter <- VK_FILTER_LINEAR
        createInfo.minFilter <- VK_FILTER_LINEAR
        createInfo.mipmapMode <- VK_SAMPLER_MIPMAP_MODE_LINEAR
        createInfo.addressModeU <- VK_SAMPLER_ADDRESS_MODE_REPEAT
        createInfo.addressModeV <- VK_SAMPLER_ADDRESS_MODE_REPEAT
        createInfo.addressModeW <- VK_SAMPLER_ADDRESS_MODE_REPEAT
        createInfo.mipLodBias <- 0.0f
        createInfo.anisotropyEnable <- false
        createInfo.maxAnisotropy <- 1.0f
        createInfo.compareEnable <- false
        createInfo.compareOp <- VK_COMPARE_OP_ALWAYS
        createInfo.minLod <- -1000f
        createInfo.maxLod <- 1000f
        createInfo.borderColor <- VK_BORDER_COLOR_INT_OPAQUE_BLACK
        createInfo.unnormalizedCoordinates <- false

        // create sampler
        vkCreateSampler (device, &createInfo, nullPtr, &sampler) |> check

        // fin
        sampler
    
    /// Create the descriptor set layout for the font atlas.
    static member createDescriptorSetLayout device =
        
        // handle
        let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>

        // populate binding
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.binding <- 0u
        binding.descriptorType <- VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        binding.descriptorCount <- 1u
        binding.stageFlags <- VK_SHADER_STAGE_FRAGMENT_BIT

        // populate create info
        let mutable createInfo = VkDescriptorSetLayoutCreateInfo ()
        createInfo.bindingCount <- 1u
        createInfo.pBindings <- asPointer &binding

        // create descriptor set layout
        vkCreateDescriptorSetLayout (device, &createInfo, nullPtr, &descriptorSetLayout) |> check

        // fin
        descriptorSetLayout
    
    /// Create the pipeline layout for the font atlas.
    static member createPipelineLayout descriptorSetLayout device =
        
        // handles
        let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
        let mutable descriptorSetLayout = descriptorSetLayout

        // populate push constant range
        let mutable pushConstantRange = VkPushConstantRange ()
        pushConstantRange.stageFlags <- VK_SHADER_STAGE_VERTEX_BIT
        pushConstantRange.offset <- 0u
        pushConstantRange.size <- uint sizeof<Single>

        // populate create info
        let mutable createInfo = VkPipelineLayoutCreateInfo ()
        createInfo.setLayoutCount <- 1u
        createInfo.pSetLayouts <- asPointer &descriptorSetLayout
        createInfo.pushConstantRangeCount <- 1u
        createInfo.pPushConstantRanges <- asPointer &pushConstantRange

        // create pipeline layout
        vkCreatePipelineLayout (device, &createInfo, nullPtr, &pipelineLayout) |> check

        // fin
        pipelineLayout

    /// Create the pipeline.
    static member createPipeline device =
        
        // handle
        let mutable pipeline = Unchecked.defaultof<VkPipeline>

        // create shader modules
        let vertModule = createShaderModuleFromGLSL "./Assets/Default/ImGuiVert.glsl" ShaderKind.VertexShader device
        let fragModule = createShaderModuleFromGLSL "./Assets/Default/ImGuiFrag.glsl" ShaderKind.FragmentShader device

        // populate shader stage infos
        use entryPoint = StringWrap "main"
        let shaderStageInfos = Array.zeroCreate<VkPipelineShaderStageCreateInfo> 2
        shaderStageInfos[0] <- VkPipelineShaderStageCreateInfo ()
        shaderStageInfos[0].stage <- VK_SHADER_STAGE_VERTEX_BIT
        shaderStageInfos[0].``module`` <- vertModule
        shaderStageInfos[0].pName <- entryPoint.Pointer
        shaderStageInfos[1] <- VkPipelineShaderStageCreateInfo ()
        shaderStageInfos[1].stage <- VK_SHADER_STAGE_FRAGMENT_BIT
        shaderStageInfos[1].``module`` <- fragModule
        shaderStageInfos[1].pName <- entryPoint.Pointer
        use shaderStageInfosPin = ArrayPin shaderStageInfos

        // populate vertex input binding description
        let mutable bindingDescription = VkVertexInputBindingDescription ()
        bindingDescription.binding <- 0u
        bindingDescription.stride <- sizeOf<ImDrawVert> ()
        bindingDescription.inputRate <- VK_VERTEX_INPUT_RATE_VERTEX

        // populate vertex input attribute descriptions
        let attributeDescriptions = Array.zeroCreate<VkVertexInputAttributeDescription> 3
        attributeDescriptions[0] <- VkVertexInputAttributeDescription ()
        attributeDescriptions[0].location <- 0u
        attributeDescriptions[0].binding <- 0u
        attributeDescriptions[0].format <- VK_FORMAT_R32G32_SFLOAT
        attributeDescriptions[0].offset <- offsetOf<ImDrawVert> "pos"
        attributeDescriptions[1] <- VkVertexInputAttributeDescription ()
        attributeDescriptions[1].location <- 1u
        attributeDescriptions[1].binding <- 0u
        attributeDescriptions[1].format <- VK_FORMAT_R32G32_SFLOAT
        attributeDescriptions[1].offset <- offsetOf<ImDrawVert> "uv"
        attributeDescriptions[2] <- VkVertexInputAttributeDescription ()
        attributeDescriptions[2].location <- 2u
        attributeDescriptions[2].binding <- 0u
        attributeDescriptions[2].format <- VK_FORMAT_R8G8B8A8_UNORM
        attributeDescriptions[2].offset <- offsetOf<ImDrawVert> "col"
        use attributeDescriptionsPin = ArrayPin attributeDescriptions

        // populate vertex input info
        let mutable vertexInfo = VkPipelineVertexInputStateCreateInfo ()
        vertexInfo.vertexBindingDescriptionCount <- 1u
        vertexInfo.pVertexBindingDescriptions <- asPointer &bindingDescription
        vertexInfo.vertexAttributeDescriptionCount <- 3u
        vertexInfo.pVertexAttributeDescriptions <- attributeDescriptionsPin.Pointer

        // populate input assembly info
        let mutable inputAssemblyInfo = VkPipelineInputAssemblyStateCreateInfo ()
        inputAssemblyInfo.topology <- VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        inputAssemblyInfo.primitiveRestartEnable <- false

        // populate viewport info
        let mutable viewportInfo = VkPipelineViewportStateCreateInfo ()
        viewportInfo.viewportCount <- 1u
        viewportInfo.scissorCount <- 1u

        // populate rasterization info
        let mutable rasterInfo = VkPipelineRasterizationStateCreateInfo ()
        rasterInfo.polygonMode <- VK_POLYGON_MODE_FILL
        rasterInfo.cullMode <- VK_CULL_MODE_NONE
        rasterInfo.frontFace <- VK_FRONT_FACE_COUNTER_CLOCKWISE
        rasterInfo.lineWidth <- 1.0f

        // populate multisample info
        let mutable multisampleInfo = VkPipelineMultisampleStateCreateInfo ()
        multisampleInfo.rasterizationSamples <- VK_SAMPLE_COUNT_1_BIT

        // populate color attachment
        let mutable colorAttachment = VkPipelineColorBlendAttachmentState ()
        colorAttachment.blendEnable <- true
        colorAttachment.srcColorBlendFactor <- VK_BLEND_FACTOR_SRC_ALPHA
        colorAttachment.dstColorBlendFactor <- VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        colorAttachment.colorBlendOp <- VK_BLEND_OP_ADD
        colorAttachment.srcAlphaBlendFactor <- VK_BLEND_FACTOR_ONE
        colorAttachment.dstAlphaBlendFactor <- VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        colorAttachment.alphaBlendOp <- VK_BLEND_OP_ADD
        colorAttachment.colorWriteMask <- VK_COLOR_COMPONENT_R_BIT ||| VK_COLOR_COMPONENT_G_BIT ||| VK_COLOR_COMPONENT_B_BIT ||| VK_COLOR_COMPONENT_A_BIT

        // populate depth and blend info
        let depthInfo = VkPipelineDepthStencilStateCreateInfo ()
        let mutable blendInfo = VkPipelineColorBlendStateCreateInfo ()
        blendInfo.attachmentCount <- 1u
        blendInfo.pAttachments <- asPointer &colorAttachment
        // TODO: see if Vortice.Vulkan offers a workaround for supplying blendConstants.


        // destroy shader modules
        vkDestroyShaderModule (device, vertModule, nullPtr)
        vkDestroyShaderModule (device, fragModule, nullPtr)

        // fin
        pipeline
    
    interface RendererImGui with
        
        member this.Initialize fonts =
            
            // create font atlas resources
            descriptorPool <- VulkanRendererImGui.createDescriptorPool device
            sampler <- VulkanRendererImGui.createSampler device
            descriptorSetLayout <- VulkanRendererImGui.createDescriptorSetLayout device
            pipelineLayout <- VulkanRendererImGui.createPipelineLayout descriptorSetLayout device
            
            // create pipeline
            pipeline <- VulkanRendererImGui.createPipeline device
            
            
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontTextureWidth = 0
            let mutable fontTextureHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            fonts.ClearTexData ()
        
        member this.Render _ = ()
        
        member this.CleanUp () =
            vkDestroyPipelineLayout (device, pipelineLayout, nullPtr)
            vkDestroyDescriptorSetLayout (device, descriptorSetLayout, nullPtr)
            vkDestroySampler (device, sampler, nullPtr)
            vkDestroyDescriptorPool (device, descriptorPool, nullPtr)

[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make fonts vulkanGlobal =
        let rendererImGui = VulkanRendererImGui vulkanGlobal
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui