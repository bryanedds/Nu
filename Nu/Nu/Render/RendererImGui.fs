// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open System.Runtime.CompilerServices
open ImGuiNET
open Vortice.Vulkan
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
type VulkanRendererImGui (vulkanGlobal : Hl.VulkanGlobal) =
    
    let mutable pipeline = Unchecked.defaultof<Pipeline.Pipeline>
//    let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
//    let mutable fontDescriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
//    let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
//    let mutable vulkanPipeline = Unchecked.defaultof<VkPipeline>
    let mutable fontTexture = Unchecked.defaultof<Texture.VulkanTexture>
//    let mutable fontDescriptorSet = Unchecked.defaultof<VkDescriptorSet>
    let mutable vertexBuffer = Unchecked.defaultof<Hl.AllocatedBuffer>
    let mutable indexBuffer = Unchecked.defaultof<Hl.AllocatedBuffer>
    let mutable vertexBufferSize = 8192
    let mutable indexBufferSize = 1024
    
    /// Create the descriptor pool for the font atlas.
    static member createDescriptorPool device =
        
        // handle
        let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
        
        // pool size
        let mutable poolSize = VkDescriptorPoolSize ()
        poolSize.``type`` <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        poolSize.descriptorCount <- 1u

        // create descriptor pool
        let mutable info = VkDescriptorPoolCreateInfo ()
        info.maxSets <- 1u
        info.poolSizeCount <- 1u
        info.pPoolSizes <- asPointer &poolSize
        Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
        descriptorPool

    /// Create the descriptor set layout for the font atlas.
    static member createFontDescriptorSetLayout device =
        
        // handle
        let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>

        // binding
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        binding.descriptorCount <- 1u
        binding.stageFlags <- Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT

        // create descriptor set layout
        let mutable info = VkDescriptorSetLayoutCreateInfo ()
        info.bindingCount <- 1u
        info.pBindings <- asPointer &binding
        Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
        descriptorSetLayout
    
    /// Create the pipeline layout for the font atlas.
    static member createPipelineLayout descriptorSetLayout device =
        
        // handles
        let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
        let mutable descriptorSetLayout = descriptorSetLayout

        // push constant range
        let mutable pushConstantRange = VkPushConstantRange ()
        pushConstantRange.stageFlags <- Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        pushConstantRange.offset <- 0u
        pushConstantRange.size <- uint sizeof<Single> * 4u

        // create pipeline layout
        let mutable info = VkPipelineLayoutCreateInfo ()
        info.setLayoutCount <- 1u
        info.pSetLayouts <- asPointer &descriptorSetLayout
        info.pushConstantRangeCount <- 1u
        info.pPushConstantRanges <- asPointer &pushConstantRange
        Vulkan.vkCreatePipelineLayout (device, &info, nullPtr, &pipelineLayout) |> Hl.check
        pipelineLayout

    /// Create the pipeline.
    static member createPipeline pipelineLayout renderPass device =
        
        // handle
        let mutable pipeline = Unchecked.defaultof<VkPipeline>

        // create shader modules
        let vertModule = Hl.createShaderModuleFromGlsl "./Assets/Default/ImGui.vert" ShaderKind.VertexShader device
        let fragModule = Hl.createShaderModuleFromGlsl "./Assets/Default/ImGui.frag" ShaderKind.FragmentShader device

        // shader stage infos
        use entryPoint = new StringWrap ("main")
        let ssInfos = Array.zeroCreate<VkPipelineShaderStageCreateInfo> 2
        ssInfos[0] <- VkPipelineShaderStageCreateInfo ()
        ssInfos[0].stage <- Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        ssInfos[0].``module`` <- vertModule
        ssInfos[0].pName <- entryPoint.Pointer
        ssInfos[1] <- VkPipelineShaderStageCreateInfo ()
        ssInfos[1].stage <- Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        ssInfos[1].``module`` <- fragModule
        ssInfos[1].pName <- entryPoint.Pointer
        use ssInfosPin = new ArrayPin<_> (ssInfos)

        // vertex input binding description
        let mutable binding = VkVertexInputBindingDescription ()
        binding.stride <- uint sizeof<ImDrawVert>
        binding.inputRate <- Vulkan.VK_VERTEX_INPUT_RATE_VERTEX

        // vertex input attribute descriptions
        let attributes = Array.zeroCreate<VkVertexInputAttributeDescription> 3
        attributes[0] <- VkVertexInputAttributeDescription ()
        attributes[0].location <- 0u
        attributes[0].binding <- 0u
        attributes[0].format <- Vulkan.VK_FORMAT_R32G32_SFLOAT
        attributes[0].offset <- uint (NativePtr.offsetOf<ImDrawVert> "pos")
        attributes[1] <- VkVertexInputAttributeDescription ()
        attributes[1].location <- 1u
        attributes[1].binding <- 0u
        attributes[1].format <- Vulkan.VK_FORMAT_R32G32_SFLOAT
        attributes[1].offset <- uint (NativePtr.offsetOf<ImDrawVert> "uv")
        attributes[2] <- VkVertexInputAttributeDescription ()
        attributes[2].location <- 2u
        attributes[2].binding <- 0u
        attributes[2].format <- Vulkan.VK_FORMAT_R8G8B8A8_UNORM
        attributes[2].offset <- uint (NativePtr.offsetOf<ImDrawVert> "col")
        use attributesPin = new ArrayPin<_> (attributes)

        // vertex input info
        let mutable viInfo = VkPipelineVertexInputStateCreateInfo ()
        viInfo.vertexBindingDescriptionCount <- 1u
        viInfo.pVertexBindingDescriptions <- asPointer &binding
        viInfo.vertexAttributeDescriptionCount <- 3u
        viInfo.pVertexAttributeDescriptions <- attributesPin.Pointer

        // input assembly info
        let mutable iaInfo = VkPipelineInputAssemblyStateCreateInfo (topology = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

        // viewport info
        let mutable vInfo = VkPipelineViewportStateCreateInfo ()
        vInfo.viewportCount <- 1u
        vInfo.scissorCount <- 1u

        // rasterization info
        let mutable rInfo = VkPipelineRasterizationStateCreateInfo ()
        rInfo.polygonMode <- Vulkan.VK_POLYGON_MODE_FILL
        rInfo.cullMode <- Vulkan.VK_CULL_MODE_NONE
        rInfo.frontFace <- Vulkan.VK_FRONT_FACE_COUNTER_CLOCKWISE
        rInfo.lineWidth <- 1.0f

        // multisample info
        let mutable mInfo = VkPipelineMultisampleStateCreateInfo (rasterizationSamples = Vulkan.VK_SAMPLE_COUNT_1_BIT)

        // color attachment
        let mutable color = VkPipelineColorBlendAttachmentState ()
        color.blendEnable <- true
        color.srcColorBlendFactor <- Vulkan.VK_BLEND_FACTOR_SRC_ALPHA
        color.dstColorBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        color.colorBlendOp <- Vulkan.VK_BLEND_OP_ADD
        color.srcAlphaBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE
        color.dstAlphaBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        color.alphaBlendOp <- Vulkan.VK_BLEND_OP_ADD
        color.colorWriteMask <- Vulkan.VK_COLOR_COMPONENT_R_BIT ||| Vulkan.VK_COLOR_COMPONENT_G_BIT ||| Vulkan.VK_COLOR_COMPONENT_B_BIT ||| Vulkan.VK_COLOR_COMPONENT_A_BIT

        // depth and blend info
        let mutable dInfo = VkPipelineDepthStencilStateCreateInfo ()
        let mutable bInfo = VkPipelineColorBlendStateCreateInfo ()
        bInfo.attachmentCount <- 1u
        bInfo.pAttachments <- asPointer &color

        // dynamic state info
        let dynamicStates = [|Vulkan.VK_DYNAMIC_STATE_VIEWPORT; Vulkan.VK_DYNAMIC_STATE_SCISSOR|]
        use dynamicStatesPin = new ArrayPin<_> (dynamicStates)
        let mutable dsInfo = VkPipelineDynamicStateCreateInfo ()
        dsInfo.dynamicStateCount <- 2u
        dsInfo.pDynamicStates <- dynamicStatesPin.Pointer

        // create pipeline
        let mutable info = VkGraphicsPipelineCreateInfo ()
        info.stageCount <- 2u
        info.pStages <- ssInfosPin.Pointer
        info.pVertexInputState <- asPointer &viInfo
        info.pInputAssemblyState <- asPointer &iaInfo
        info.pViewportState <- asPointer &vInfo
        info.pRasterizationState <- asPointer &rInfo
        info.pMultisampleState <- asPointer &mInfo
        info.pDepthStencilState <- asPointer &dInfo
        info.pColorBlendState <- asPointer &bInfo
        info.pDynamicState <- asPointer &dsInfo
        info.layout <- pipelineLayout
        info.renderPass <- renderPass
        info.subpass <- 0u
        Vulkan.vkCreateGraphicsPipelines (device, VkPipelineCache.Null, 1u, &info, nullPtr, asPointer &pipeline) |> Hl.check

        // destroy shader modules
        Vulkan.vkDestroyShaderModule (device, vertModule, nullPtr)
        Vulkan.vkDestroyShaderModule (device, fragModule, nullPtr)

        // fin
        pipeline

    /// Create the descriptor set for the font atlas.
    static member createFontDescriptorSet descriptorSetLayout descriptorPool device =
        let mutable descriptorSet = Unchecked.defaultof<VkDescriptorSet>
        let mutable descriptorSetLayout = descriptorSetLayout
        let mutable info = VkDescriptorSetAllocateInfo ()
        info.descriptorPool <- descriptorPool
        info.descriptorSetCount <- 1u
        info.pSetLayouts <- asPointer &descriptorSetLayout
        Vulkan.vkAllocateDescriptorSets (device, asPointer &info, asPointer &descriptorSet) |> Hl.check
        descriptorSet

    /// Write the data to the descriptor set.
    static member writeFontDescriptorSet sampler imageView descriptorSet device =
        
        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.sampler <- sampler
        info.imageView <- imageView
        info.imageLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- descriptorSet
        write.descriptorCount <- 1u
        write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        write.pImageInfo <- asPointer &info
        Vulkan.vkUpdateDescriptorSets (device, 1u, asPointer &write, 0u, nullPtr)
    
    interface RendererImGui with
        
        member this.Initialize (fonts : ImFontAtlasPtr) =
            
            // commonly used handles
            let device = vulkanGlobal.Device
            let allocator = vulkanGlobal.VmaAllocator
            
            // get font atlas data
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontWidth = 0
            let mutable fontHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontWidth, &fontHeight, &bytesPerPixel)

            // create the font atlas texture
            let metadata = Texture.TextureMetadata.make fontWidth fontHeight
            fontTexture <- Texture.VulkanTexture.createRgba Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR metadata pixels vulkanGlobal
            
            // create general resources
//            descriptorPool <- VulkanRendererImGui.createDescriptorPool device
//            fontDescriptorSetLayout <- VulkanRendererImGui.createFontDescriptorSetLayout device
//            pipelineLayout <- VulkanRendererImGui.createPipelineLayout fontDescriptorSetLayout device
//            vulkanPipeline <- VulkanRendererImGui.createPipeline pipelineLayout vulkanGlobal.RenderPass device
            
            // create and write descriptor set for font atlas
//            fontDescriptorSet <- VulkanRendererImGui.createFontDescriptorSet fontDescriptorSetLayout descriptorPool device
//            VulkanRendererImGui.writeFontDescriptorSet fontTexture.Sampler fontTexture.ImageView fontDescriptorSet device


            let vertexBinding =
                [|Hl.makeVertexBindingVertex 0 sizeof<ImDrawVert>|]

            let vertexAttributes =
                [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "pos")
                  Hl.makeVertexAttribute 1 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "uv")
                  Hl.makeVertexAttribute 2 0 Vulkan.VK_FORMAT_R8G8B8A8_UNORM (NativePtr.offsetOf<ImDrawVert> "col")|]

            let descriptorBinding =
                [|Hl.makeDescriptorBindingFragment 0 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]

            let pushConstant =
                [|Hl.makePushConstantRange Vulkan.VK_SHADER_STAGE_VERTEX_BIT 0 (sizeof<Single> * 4)|]
            
            pipeline <-
                Pipeline.Pipeline.create
                    "./Assets/Default/ImGui"
                    vertexBinding
                    vertexAttributes
                    descriptorBinding
                    pushConstant
                    vulkanGlobal.RenderPass
                    device

            Pipeline.Pipeline.writeDescriptorTexture 0 0 fontTexture pipeline device


            // store identifier
            fonts.SetTexID (nativeint pipeline.DescriptorSet.Handle)
            
            // NOTE: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- Hl.AllocatedBuffer.createVertex true vertexBufferSize allocator
            indexBuffer <- Hl.AllocatedBuffer.createIndex true indexBufferSize allocator
        
        member this.Render (drawData : ImDrawDataPtr) =
            
            // commonly used handles
            let allocator = vulkanGlobal.VmaAllocator
            let commandBuffer = vulkanGlobal.RenderCommandBuffer
            
            // get total resolution from imgui
            // NOTE: if this ever differs from the swapchain then something is wrong.
            let framebufferWidth = drawData.DisplaySize.X * drawData.FramebufferScale.X
            let framebufferHeight = drawData.DisplaySize.Y * drawData.FramebufferScale.Y

            // only proceed if window is not minimized
            if int framebufferWidth > 0 && int framebufferHeight > 0 then

                if drawData.TotalVtxCount > 0 then
                    
                    // get data size for vertices and indices
                    let vertexSize = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexSize = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge vertex buffer if needed
                    if vertexSize > vertexBufferSize then
                        while vertexSize > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                        Hl.AllocatedBuffer.destroy vertexBuffer allocator
                        vertexBuffer <- Hl.AllocatedBuffer.createVertex true vertexBufferSize allocator

                    // enlarge index buffer if needed
                    if indexSize > indexBufferSize then
                        while indexSize > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                        Hl.AllocatedBuffer.destroy indexBuffer allocator
                        indexBuffer <- Hl.AllocatedBuffer.createIndex true indexBufferSize allocator

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdListsRange in range.[i]
                        let vertexSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        
                        // TODO: DJL: try a persistently mapped buffer and compare performance
                        Hl.AllocatedBuffer.upload vertexOffset vertexSize drawList.VtxBuffer.Data vertexBuffer allocator
                        Hl.AllocatedBuffer.upload indexOffset indexSize drawList.IdxBuffer.Data indexBuffer allocator
                        vertexOffset <- vertexOffset + vertexSize
                        indexOffset <- indexOffset + indexSize

                // bind pipeline
                Vulkan.vkCmdBindPipeline (commandBuffer, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.Pipeline)

                // bind vertex and index buffer
                if drawData.TotalVtxCount > 0 then
                    let mutable vertexBuffer = vertexBuffer.Buffer
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (commandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                    Vulkan.vkCmdBindIndexBuffer (commandBuffer, indexBuffer.Buffer, 0UL, Vulkan.VK_INDEX_TYPE_UINT16)

                // set up viewport
                let mutable viewport = VkViewport ()
                viewport.x <- 0.0f
                viewport.y <- 0.0f
                viewport.width <- framebufferWidth
                viewport.height <- framebufferHeight
                viewport.minDepth <- 0.0f
                viewport.maxDepth <- 1.0f
                Vulkan.vkCmdSetViewport (commandBuffer, 0u, 1u, asPointer &viewport)

                // set up scale and translation
                let scale = Array.zeroCreate<single> 2
                scale[0] <- 2.0f / drawData.DisplaySize.X
                scale[1] <- 2.0f / drawData.DisplaySize.Y
                use scalePin = new ArrayPin<_> (scale)
                let translate = Array.zeroCreate<single> 2
                translate[0] <- -1.0f - drawData.DisplayPos.X * scale[0]
                translate[1] <- -1.0f - drawData.DisplayPos.Y * scale[1]
                use translatePin = new ArrayPin<_> (translate)
                Vulkan.vkCmdPushConstants (commandBuffer, pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT, 0u, 8u, scalePin.VoidPtr)
                Vulkan.vkCmdPushConstants (commandBuffer, pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT, 8u, 8u, translatePin.VoidPtr)

                // draw command lists
                let mutable globalVtxOffset = 0
                let mutable globalIdxOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    let drawList = let range = drawData.CmdListsRange in range.[i]
                    for j in 0 .. dec drawList.CmdBuffer.Size do
                        let pcmd = let buffer = drawList.CmdBuffer in buffer.[j]
                        if pcmd.UserCallback = nativeint 0 then
                            
                            // project scissor/clipping rectangles into framebuffer space
                            let mutable clipMin =
                                v2
                                    ((pcmd.ClipRect.X - drawData.DisplayPos.X) * drawData.FramebufferScale.X)
                                    ((pcmd.ClipRect.Y - drawData.DisplayPos.Y) * drawData.FramebufferScale.Y)
                            
                            let mutable clipMax =
                                v2
                                    ((pcmd.ClipRect.Z - drawData.DisplayPos.X) * drawData.FramebufferScale.X)
                                    ((pcmd.ClipRect.W - drawData.DisplayPos.Y) * drawData.FramebufferScale.Y)

                            // clamp to viewport as Vulkan.vkCmdSetScissor won't accept values that are off bounds
                            if clipMin.X < 0.0f then clipMin.X <- 0.0f
                            if clipMin.Y < 0.0f then clipMin.Y <- 0.0f
                            if clipMax.X > framebufferWidth then clipMax.X <- framebufferWidth
                            if clipMax.Y > framebufferHeight then clipMax.Y <- framebufferHeight

                            // check rectangle is valid
                            if clipMax.X > clipMin.X && clipMax.Y > clipMin.Y then
                                
                                // apply scissor/clipping rectangle
                                let width = uint (clipMax.X - clipMin.X)
                                let height = uint (clipMax.Y - clipMin.Y)
                                let mutable scissor = VkRect2D (int clipMin.X, int clipMin.Y, width, height)
                                Vulkan.vkCmdSetScissor (commandBuffer, 0u, 1u, asPointer &scissor)

                                // bind font descriptor set
                                let mutable descriptorSet = VkDescriptorSet (uint64 pcmd.TextureId)
                                Vulkan.vkCmdBindDescriptorSets
                                    (commandBuffer,
                                     Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS,
                                     pipeline.PipelineLayout, 0u,
                                     1u, asPointer &descriptorSet,
                                     0u, nullPtr)

                                // draw
                                Vulkan.vkCmdDrawIndexed
                                    (commandBuffer,
                                     pcmd.ElemCount, 1u,
                                     pcmd.IdxOffset + uint globalIdxOffset,
                                     int pcmd.VtxOffset + globalVtxOffset, 0u)

                        else raise (NotImplementedException ())

                    globalIdxOffset <- globalIdxOffset + drawList.IdxBuffer.Size
                    globalVtxOffset <- globalVtxOffset + drawList.VtxBuffer.Size

                // reset scissor
                let mutable scissor = VkRect2D (0, 0, uint framebufferWidth, uint framebufferHeight)
                Vulkan.vkCmdSetScissor (commandBuffer, 0u, 1u, asPointer &scissor)
        
        member this.CleanUp () =
            
            // commonly used handles
            let device = vulkanGlobal.Device
            let allocator = vulkanGlobal.VmaAllocator
            
            //
            Hl.AllocatedBuffer.destroy indexBuffer allocator
            Hl.AllocatedBuffer.destroy vertexBuffer allocator
            Texture.VulkanTexture.destroy fontTexture vulkanGlobal
            Pipeline.Pipeline.destroy pipeline device
//            Vulkan.vkDestroyPipeline (device, vulkanPipeline, nullPtr)
//            Vulkan.vkDestroyPipelineLayout (device, pipelineLayout, nullPtr)
//            Vulkan.vkDestroyDescriptorSetLayout (device, fontDescriptorSetLayout, nullPtr)
//            Vulkan.vkDestroyDescriptorPool (device, descriptorPool, nullPtr)

[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make fonts vulkanGlobal =
        let rendererImGui = VulkanRendererImGui vulkanGlobal
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui