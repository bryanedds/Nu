﻿// Nu Game Engine.
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
type VulkanRendererImGui (vkg : Hl.VulkanGlobal) =
    
    let mutable pipeline = Unchecked.defaultof<Pipeline.Pipeline>
    let mutable fontTexture = Unchecked.defaultof<Texture.VulkanTexture>
    let mutable vertexBuffer = Unchecked.defaultof<Hl.FifBuffer>
    let mutable indexBuffer = Unchecked.defaultof<Hl.FifBuffer>
    let mutable vertexBufferSize = 8192
    let mutable indexBufferSize = 1024
    
    interface RendererImGui with
        
        member this.Initialize (fonts : ImFontAtlasPtr) =
            
            // get font atlas data
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontWidth = 0
            let mutable fontHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontWidth, &fontHeight, &bytesPerPixel)

            // create the font atlas texture
            let metadata = Texture.TextureMetadata.make fontWidth fontHeight
            fontTexture <- Texture.VulkanTexture.createRgba Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR metadata (Some pixels) vkg
            
            // create pipeline
            pipeline <-
                Pipeline.Pipeline.create
                    Constants.Paths.ImGuiShaderFilePath
                    false [|Pipeline.ImGui|]
                    [|Hl.makeVertexBindingVertex 0 sizeof<ImDrawVert>|]
                    [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "pos")
                      Hl.makeVertexAttribute 1 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "uv")
                      Hl.makeVertexAttribute 2 0 Vulkan.VK_FORMAT_R8G8B8A8_UNORM (NativePtr.offsetOf<ImDrawVert> "col")|]
                    [|Hl.makeDescriptorBindingFragment 0 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                    [|Hl.makePushConstantRange Vulkan.VK_SHADER_STAGE_VERTEX_BIT 0 (sizeof<Single> * 4)|]
                    vkg.RenderPass
                    vkg.Device

            // load font atlas texture to descriptor set
            Pipeline.Pipeline.writeDescriptorTexture 0 0 fontTexture pipeline vkg.Device

            // store identifier
            fonts.SetTexID (nativeint pipeline.DescriptorSet.Handle)
            
            // NOTE: DJL: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- Hl.FifBuffer.createVertex vertexBufferSize vkg.VmaAllocator
            indexBuffer <- Hl.FifBuffer.createIndex indexBufferSize vkg.VmaAllocator
        
        member this.Render (drawData : ImDrawDataPtr) =
            
            // get total resolution from imgui
            // NOTE: DJL: if this ever differs from the swapchain then something is wrong.
            let framebufferWidth = drawData.DisplaySize.X * drawData.FramebufferScale.X
            let framebufferHeight = drawData.DisplaySize.Y * drawData.FramebufferScale.Y

            // only proceed if window is not minimized
            if int framebufferWidth > 0 && int framebufferHeight > 0 then

                // init render
                let cb = vkg.RenderCommandBuffer
                let mutable renderArea = VkRect2D (0, 0, uint framebufferWidth, uint framebufferHeight)
                Hl.beginRenderBlock cb vkg.RenderPass vkg.SwapchainFramebuffer renderArea [||] vkg.InFlightFence vkg.Device
                
                if drawData.TotalVtxCount > 0 then
                    
                    // get data size for vertices and indices
                    let vertexSize = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexSize = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge buffer sizes if needed
                    while vertexSize > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                    while indexSize > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                    vertexBuffer.UpdateSize vertexBufferSize vkg.VmaAllocator
                    indexBuffer.UpdateSize indexBufferSize vkg.VmaAllocator

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdListsRange in range.[i]
                        let vertexSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        Hl.FifBuffer.upload vertexOffset vertexSize drawList.VtxBuffer.Data vertexBuffer
                        Hl.FifBuffer.upload indexOffset indexSize drawList.IdxBuffer.Data indexBuffer
                        vertexOffset <- vertexOffset + vertexSize
                        indexOffset <- indexOffset + indexSize

                // bind pipeline
                let vkPipeline = Pipeline.Pipeline.getPipeline Pipeline.ImGui pipeline
                Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

                // bind vertex and index buffer
                if drawData.TotalVtxCount > 0 then
                    let mutable vertexBuffer = vertexBuffer.Buffer
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                    Vulkan.vkCmdBindIndexBuffer (cb, indexBuffer.Buffer, 0UL, Vulkan.VK_INDEX_TYPE_UINT16)

                // set up viewport
                let mutable viewport = Hl.makeViewport renderArea
                Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &viewport)

                // set up scale and translation
                let scale = Array.zeroCreate<single> 2
                scale[0] <- 2.0f / drawData.DisplaySize.X
                scale[1] <- 2.0f / drawData.DisplaySize.Y
                use scalePin = new ArrayPin<_> (scale)
                let translate = Array.zeroCreate<single> 2
                translate[0] <- -1.0f - drawData.DisplayPos.X * scale[0]
                translate[1] <- -1.0f - drawData.DisplayPos.Y * scale[1]
                use translatePin = new ArrayPin<_> (translate)
                Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT, 0u, 8u, scalePin.VoidPtr)
                Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT, 8u, 8u, translatePin.VoidPtr)

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
                                Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)

                                // bind font descriptor set
                                let mutable descriptorSet = VkDescriptorSet (uint64 pcmd.TextureId)
                                Vulkan.vkCmdBindDescriptorSets
                                    (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS,
                                     pipeline.PipelineLayout, 0u,
                                     1u, asPointer &descriptorSet,
                                     0u, nullPtr)

                                // draw
                                Vulkan.vkCmdDrawIndexed (cb, pcmd.ElemCount, 1u, pcmd.IdxOffset + uint globalIdxOffset, int pcmd.VtxOffset + globalVtxOffset, 0u)
                                Hl.reportDrawCall 1

                        else raise (NotImplementedException ())

                    globalIdxOffset <- globalIdxOffset + drawList.IdxBuffer.Size
                    globalVtxOffset <- globalVtxOffset + drawList.VtxBuffer.Size

                // reset scissor
                Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &renderArea)

                // flush render commands
                Hl.endRenderBlock cb vkg.GraphicsQueue [||] [||] vkg.InFlightFence
        
        member this.CleanUp () =
            Hl.FifBuffer.destroy indexBuffer vkg.VmaAllocator
            Hl.FifBuffer.destroy vertexBuffer vkg.VmaAllocator
            Texture.VulkanTexture.destroy fontTexture vkg
            Pipeline.Pipeline.destroy pipeline vkg.Device

[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make fonts vkg =
        let rendererImGui = VulkanRendererImGui vkg
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui