﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Numerics
open System.Runtime.CompilerServices
open ImGuiNET
open Vortice.Vulkan
open Vortice.ShaderCompiler
open Prime

/// A message to the ImGui rendering subsystem.
type RenderMessageImGui =
    | ReloadRenderAssets

/// Renders an imgui view.
/// NOTE: API is object-oriented / mutation-based because it's ported from a port. 
type RendererImGui =
    abstract Initialize : fonts : ImFontAtlasPtr -> unit
    abstract Render : viewport_ : Viewport -> drawData : ImDrawDataPtr -> renderMessages : RenderMessageImGui List -> unit
    abstract CleanUp : unit -> unit

/// A stub imgui renderer.
type StubRendererImGui () =
    interface RendererImGui with
        member renderer.Initialize fonts =
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontTextureWidth = 0
            let mutable fontTextureHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            fonts.ClearTexData ()
        member renderer.Render _ _ _ = ()
        member renderer.CleanUp () = ()

[<RequireQualifiedAccess>]
module StubRendererImGui =

    /// Make a stub imgui renderer.
    let make fonts =
        let rendererImGui = StubRendererImGui ()
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via OpenGL.
type GlRendererImGui
    (assetTextureRequests : ConcurrentDictionary<AssetTag, unit>,
     assetTextureOpts : ConcurrentDictionary<AssetTag, uint32 voption>,
     viewport : Viewport) =

    let mutable viewport = viewport
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

    member private renderer.DestroyAssetTextures (destroyedTextureIdsOpt : uint HashSet option) =
        for assetTextureOpt in assetTextureOpts.Values do
            match assetTextureOpt with
            | ValueSome textureId ->
                OpenGL.Gl.DeleteTextures [|textureId|]
                match destroyedTextureIdsOpt with
                | Some destroyedTextureIds -> destroyedTextureIds.Add textureId |> ignore<bool>
                | None -> ()
            | ValueNone -> ()
        OpenGL.Hl.Assert ()
        assetTextureOpts.Clear ()

    interface RendererImGui with

        member renderer.Initialize (fonts : ImFontAtlasPtr) =
        
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
            // TODO: P1: let's put this code into a .glsl file and load it from there.
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

        member renderer.Render viewport_ (drawData : ImDrawDataPtr) renderMessages =

            // update viewport, updating the imgui display size as needed
            if viewport <> viewport_ then
                let io = ImGui.GetIO ()
                io.DisplaySize <- viewport_.Bounds.Size.V2
                viewport <- viewport_

            // in the event of clearing asset textures, we keep a blacklist of texture ids that have been recently
            // destroyed. In the following code, we make an attempt to clear all artifacts that might have a
            // potentially invalidated texture id laying around, but one might already be on the stack on another
            // thread, so we use this extra caution. This blacklist only lasts for the current render frame, so if any
            // texture id lasts beyond one frame, it indicates a bug.
            let textureIdBlacklist = hashSetPlus HashIdentity.Structural []

            // handle render messages
            for renderMessage in renderMessages do
                match renderMessage with
                | ReloadRenderAssets -> renderer.DestroyAssetTextures (Some textureIdBlacklist)

            // prepare asset textures for a finite period of time
            let now = DateTimeOffset.Now
            let assetTags = Array.ofSeq assetTextureRequests.Keys // eager copy to allow modification during enumeration
            let mutable assetTagsEnr = (seq assetTags).GetEnumerator ()
            while assetTagsEnr.MoveNext () && DateTimeOffset.Now - now <= TimeSpan.FromMilliseconds 4 do
                let assetTag = assetTagsEnr.Current
                if not (assetTextureOpts.ContainsKey assetTag) then
                    match Metadata.tryGetFilePath assetTag with
                    | Some filePath ->
                        match OpenGL.Texture.TryCreateTextureGl (true, OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, false, false, OpenGL.Texture.BlockCompressable filePath, filePath) with
                        | Right (_, textureId) -> assetTextureOpts.[assetTag] <- ValueSome textureId
                        | Left _ -> assetTextureOpts.[assetTag] <- ValueNone
                    | None -> ()
                let mutable removed = ()
                assetTextureRequests.TryRemove (assetTag, &removed) |> ignore<bool>

            // set viewport to offset bounds
            let bounds = viewport.Bounds
            OpenGL.Gl.Viewport (bounds.Min.X, bounds.Min.Y, bounds.Size.X, bounds.Size.Y)

            // attempt to draw imgui draw data
            let mutable vertexOffsetInVertices = 0
            let mutable indexOffsetInElements = 0
            if drawData.CmdListsCount <> 0 then

                // resize vertex buffer if necessary
                let vertexBufferSizeNeeded = uint (drawData.TotalVtxCount * sizeof<ImDrawVert>)
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
                let cmdLists = drawData.CmdLists
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmdList = cmdLists.[i]
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, nativeint (vertexOffsetInVertices * sizeof<ImDrawVert>), uint (cmdList.VtxBuffer.Size * Unsafe.SizeOf<ImDrawVert> ()), cmdList.VtxBuffer.Data)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ElementArrayBuffer, nativeint (indexOffsetInElements * sizeof<uint16>), uint (cmdList.IdxBuffer.Size * sizeof<uint16>), cmdList.IdxBuffer.Data)
                    vertexOffsetInVertices <- vertexOffsetInVertices + cmdList.VtxBuffer.Size
                    indexOffsetInElements <- indexOffsetInElements + cmdList.IdxBuffer.Size
                    OpenGL.Hl.Assert ()

                // compute orthographic projection
                let projection = Matrix4x4.CreateOrthographicOffCenter (0.0f, single viewport.Bounds.Size.X, single viewport.Bounds.Size.Y, 0.0f, -1.0f, 1.0f)
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

                // draw command lists, ignoring any commands that use blacklisted textures
                let mutable vertexOffset = 0
                let mutable indexOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmdLists = drawData.CmdLists
                    let cmdList = cmdLists.[i]
                    for cmd in 0 .. dec cmdList.CmdBuffer.Size do
                        let pcmds = cmdList.CmdBuffer
                        let pcmd = pcmds.[cmd]
                        if not (textureIdBlacklist.Contains (uint32 pcmd.TextureId)) then
                            if pcmd.UserCallback = nativeint 0 then
                                let clip = pcmd.ClipRect
                                OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
                                OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, uint pcmd.TextureId)
                                OpenGL.Gl.Scissor
                                    (int clip.X + bounds.Min.X,
                                     viewport.Bounds.Size.Y - int clip.W + bounds.Min.Y,
                                     int (clip.Z - clip.X),
                                     int (clip.W - clip.Y))
                                OpenGL.Gl.DrawElementsBaseVertex (OpenGL.PrimitiveType.Triangles, int pcmd.ElemCount, OpenGL.DrawElementsType.UnsignedShort, nativeint (indexOffset * sizeof<uint16>), int pcmd.VtxOffset + vertexOffset)
                                OpenGL.Hl.ReportDrawCall 1
                                OpenGL.Hl.Assert ()
                            else raise (NotImplementedException ())
                        indexOffset <- indexOffset + int pcmd.ElemCount
                    vertexOffset <- vertexOffset + cmdList.VtxBuffer.Size

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

        member renderer.CleanUp () =

            // destroy asset textures
            renderer.DestroyAssetTextures None

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
    let make assetTextureRequests assetTextures fonts viewport =
        let rendererImGui = GlRendererImGui (assetTextureRequests, assetTextures, viewport)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via Vulkan.
type VulkanRendererImGui (vkc : Hl.VulkanContext, viewport : Viewport) =

    let mutable viewport = viewport
    let mutable pipeline = Unchecked.defaultof<Pipeline.Pipeline>
    let mutable fontTexture = Unchecked.defaultof<Texture.VulkanTexture>
    let mutable vertexBuffer = Unchecked.defaultof<Buffer.Buffer>
    let mutable indexBuffer = Unchecked.defaultof<Buffer.Buffer>
    let mutable vertexBufferSize = 8192
    let mutable indexBufferSize = 1024
    
    interface RendererImGui with
        
        member renderer.Initialize (fonts : ImFontAtlasPtr) =
            
            // get font atlas data
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontWidth = 0
            let mutable fontHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontWidth, &fontHeight, &bytesPerPixel)

            // create the font atlas texture
            let metadata = Texture.TextureMetadata.make fontWidth fontHeight
            fontTexture <- Texture.VulkanTexture.create Texture.Rgba Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone metadata vkc
            Texture.VulkanTexture.upload metadata 0 pixels fontTexture vkc
            
            // create pipeline
            pipeline <-
                Pipeline.Pipeline.create
                    Constants.Paths.ImGuiShaderFilePath
                    false false [|Pipeline.ImGui|]
                    [|Hl.makeVertexBindingVertex 0 sizeof<ImDrawVert>|]
                    [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "pos")
                      Hl.makeVertexAttribute 1 0 Vulkan.VK_FORMAT_R32G32_SFLOAT (NativePtr.offsetOf<ImDrawVert> "uv")
                      Hl.makeVertexAttribute 2 0 Vulkan.VK_FORMAT_R8G8B8A8_UNORM (NativePtr.offsetOf<ImDrawVert> "col")|]
                    [|Hl.makeDescriptorBindingFragment 0 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                    [|Hl.makePushConstantRange Vulkan.VK_SHADER_STAGE_VERTEX_BIT 0 (sizeof<Single> * 4)|]
                    vkc.RenderPass
                    vkc

            // load font atlas texture to descriptor set
            Pipeline.Pipeline.writeDescriptorTextureInit 0 0 fontTexture pipeline vkc

            // store identifier
            fonts.SetTexID (nativeint pipeline.DescriptorSet.Handle)
            
            // NOTE: DJL: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- Buffer.Buffer.create vertexBufferSize (Buffer.Vertex true) vkc
            indexBuffer <- Buffer.Buffer.create indexBufferSize (Buffer.Index true) vkc

        member renderer.Render viewport_ (drawData : ImDrawDataPtr) _ =

            // update viewport, updating the imgui display size as needed
            if viewport <> viewport_ then
                let io = ImGui.GetIO ()
                io.DisplaySize <- viewport_.Bounds.Size.V2 // NOTE: DJL: this is not set in the dear imgui vulkan backend.
                viewport <- viewport_

            // get total resolution from imgui
            let framebufferWidth = drawData.DisplaySize.X * drawData.FramebufferScale.X
            let framebufferHeight = drawData.DisplaySize.Y * drawData.FramebufferScale.Y

            // only proceed if render is desired and if the framebuffer bounds given by drawData a) aren't minimized and
            // b) don't exceed the current viewport, as they sometimes lag behind upon resize.
            if int framebufferWidth > 0 &&
               int framebufferHeight > 0 &&
               int framebufferWidth <= viewport.Bounds.Width &&
               int framebufferHeight <= viewport.Bounds.Height &&
               vkc.RenderDesired then

                // init render
                let cb = vkc.RenderCommandBuffer
                let mutable renderArea = VkRect2D (0, 0, uint framebufferWidth, uint framebufferHeight)
                Hl.beginRenderBlock cb vkc.RenderPass vkc.SwapchainFramebuffer renderArea [||]
                
                if drawData.TotalVtxCount > 0 then
                    
                    // get data size for vertices and indices
                    let vertexSize = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexSize = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge buffer sizes if needed
                    while vertexSize > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                    while indexSize > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                    Buffer.Buffer.updateSize vertexBufferSize vertexBuffer vkc
                    Buffer.Buffer.updateSize indexBufferSize indexBuffer vkc

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdLists in range.[i]
                        let vertexSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        Buffer.Buffer.upload vertexOffset vertexSize drawList.VtxBuffer.Data vertexBuffer vkc
                        Buffer.Buffer.upload indexOffset indexSize drawList.IdxBuffer.Data indexBuffer vkc
                        vertexOffset <- vertexOffset + vertexSize
                        indexOffset <- indexOffset + indexSize

                // bind pipeline
                let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.ImGui pipeline
                Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

                // bind vertex and index buffer
                if drawData.TotalVtxCount > 0 then
                    let mutable vertexBuffer = vertexBuffer.VkBuffer
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                    Vulkan.vkCmdBindIndexBuffer (cb, indexBuffer.VkBuffer, 0UL, Vulkan.VK_INDEX_TYPE_UINT16)

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
                    let drawList = let range = drawData.CmdLists in range.[i]
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

                // end render
                Hl.endRenderBlock cb
        
        member renderer.CleanUp () =
            Buffer.Buffer.destroy indexBuffer vkc
            Buffer.Buffer.destroy vertexBuffer vkc
            Texture.VulkanTexture.destroy fontTexture vkc
            Pipeline.Pipeline.destroy pipeline vkc

[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make fonts vkc viewport =
        let rendererImGui = VulkanRendererImGui (vkc, viewport)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui