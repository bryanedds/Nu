// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Numerics
open System.Runtime.CompilerServices
open ImGuiNET
open Vortice.Vulkan
open Prime
open Nu.Vulkan

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

/// StubRendererImGui functions.
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
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.Texture.Uncompressed.InternalFormat, fontTextureWidth, fontTextureHeight, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, pixels)
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
            while assetTagsEnr.MoveNext () && DateTimeOffset.Now - now <= TimeSpan.FromMilliseconds 4.0 do
                let assetTag = assetTagsEnr.Current
                if not (assetTextureOpts.ContainsKey assetTag) then
                    match Metadata.tryGetFilePath assetTag with
                    | Some filePath ->
                        match OpenGL.Texture.TryCreateTextureGl (true, OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, false, false, OpenGL.Texture.InferCompression filePath, filePath) with
                        | Right (_, textureId) -> assetTextureOpts[assetTag] <- ValueSome textureId
                        | Left _ -> assetTextureOpts[assetTag] <- ValueNone
                    | None -> ()
                let mutable removed = ()
                assetTextureRequests.TryRemove (assetTag, &removed) |> ignore<bool>

            // set viewport to bounds
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
                    let cmdList = cmdLists[i]
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
                    let cmdList = cmdLists[i]
                    for cmd in 0 .. dec cmdList.CmdBuffer.Size do
                        let pcmds = cmdList.CmdBuffer
                        let pcmd = pcmds[cmd]
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

/// GlRendererImGui functions.
[<RequireQualifiedAccess>]
module GlRendererImGui =

    /// Make a gl-based imgui renderer.
    let make assetTextureRequests assetTextures fonts viewport =
        let rendererImGui = GlRendererImGui (assetTextureRequests, assetTextures, viewport)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via Vulkan.
type VulkanRendererImGui
    (assetTextureRequests : ConcurrentDictionary<AssetTag, unit>,
     assetTextureOpts : ConcurrentDictionary<AssetTag, uint32 voption>,
     viewport : Viewport,
     vkc : VulkanContext) =

    let assetTextureStorage = dictPlus<uint32, Texture> HashIdentity.Structural []
    let mutable viewport = viewport
    let mutable pipeline = Unchecked.defaultof<Pipeline>
    let mutable fontSampler = Unchecked.defaultof<Sampler>
    let mutable assetSampler = Unchecked.defaultof<Sampler>
    let mutable fontTexture = Unchecked.defaultof<Texture>
    let mutable vertexBufferSize = 8192 // TODO: populate from a constant.
    let mutable vertexBuffer = Unchecked.defaultof<Nu.Vulkan.Buffer>
    let mutable indexBufferSize = 1024 // TODO: populate from a constant.
    let mutable indexBuffer = Unchecked.defaultof<Nu.Vulkan.Buffer>
    let mutable textureIdCounter = 0u
    
    member private renderer.DestroyAssetTextures (destroyedTextureIdsOpt : uint32 HashSet option) =
        CommandQueue.waitIdle vkc.RenderQueue
        for assetTextureOpt in assetTextureOpts.Values do
            match assetTextureOpt with
            | ValueSome textureId ->
                match Dictionary.tryFind textureId assetTextureStorage with
                | Some texture ->
                    Texture.destroy texture vkc
                    assetTextureStorage.Remove textureId |> ignore<bool>
                | None -> ()
                match destroyedTextureIdsOpt with
                | Some destroyedTextureIds -> destroyedTextureIds.Add textureId |> ignore<bool>
                | None -> ()
            | ValueNone -> ()
        assetTextureOpts.Clear ()
    
    member private renderer.GetTexture textureId =
        if textureId = 0u then fontTexture
        else
            match Dictionary.tryFind textureId assetTextureStorage with
            | Some texture -> texture
            | None -> Texture.EmptyTexture
    
    member private renderer.GetSampler textureId =
        if textureId = 0u then fontSampler else assetSampler
    
    interface RendererImGui with
        
        member renderer.Initialize (fonts : ImFontAtlasPtr) =
            
            // get font atlas data
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontWidth = 0
            let mutable fontHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontWidth, &fontHeight, &bytesPerPixel)

            // create the font atlas texture
            let metadata = TextureMetadata.make fontWidth fontHeight
            let textureParallel = TextureParallel.create MipmapNone AttachmentNone Texture2d [||] Uncompressed.ImageFormat Rgba metadata vkc
            TextureParallel.upload metadata 0 0 pixels RenderThread textureParallel vkc
            fontTexture <- EagerTexture { TextureMetadata = metadata; TextureParallel = textureParallel }
            
            // create samplers
            fontSampler <- Sampler.create VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false vkc
            assetSampler <- Sampler.create VkSamplerAddressMode.Repeat VkFilter.Nearest VkFilter.Nearest false vkc
            
            // set font atlas TexId to 0
            fonts.SetTexID (nativeint textureIdCounter)
            textureIdCounter <- inc textureIdCounter
            
            // NOTE: DJL: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- Nu.Vulkan.Buffer.create vertexBufferSize (Vertex true) vkc
            indexBuffer <- Nu.Vulkan.Buffer.create indexBufferSize (Index true) vkc

            // create pipeline
            pipeline <-
                Pipeline.create
                    Constants.Paths.ImGuiShaderFilePath
                    [|VulkanImGui|] [|false|]
                    [|Pipeline.vertex 0 sizeof<ImDrawVert> VkVertexInputRate.Vertex
                        [|Pipeline.attribute 0 Single2 (NativePtr.offsetOf<ImDrawVert> (nameof Unchecked.defaultof<ImDrawVert>.pos))
                          Pipeline.attribute 1 Single2 (NativePtr.offsetOf<ImDrawVert> (nameof Unchecked.defaultof<ImDrawVert>.uv))
                          Pipeline.attribute 2 Quarter4 (NativePtr.offsetOf<ImDrawVert> (nameof Unchecked.defaultof<ImDrawVert>.col))|]|] // format must match size of actual data (uint32), even though it is read as vec4 in the shader!
                    [|Pipeline.descriptorSet<Texture * Sampler>
                        [|Pipeline.descriptor 0 CombinedImageSampler FragmentStage 1|]|]
                    [|Pipeline.pushConstant 0 (sizeof<Single> * 4) VertexStage|]
                    [|vkc.SwapFormat|] None
                    [|vertexBuffer; indexBuffer|] vkc

        member renderer.Render viewport_ (drawData : ImDrawDataPtr) renderMessages =

            // update viewport, updating the imgui display size as needed
            if viewport <> viewport_ then
                let io = ImGui.GetIO ()
                io.DisplaySize <- viewport_.Bounds.Size.V2 // NOTE: DJL: this is not set in the dear imgui vulkan backend but IS necessary!
                viewport <- viewport_

            // in the event of clearing asset textures, we keep a blacklist of texture ids that have been recently
            // destroyed. In the following code, we make an attempt to clear all artifacts that might have a
            // potentially invalidated texture id laying around, but one might already be on the stack on another
            // thread, so we use this extra caution. This blacklist only lasts for the current render frame, so if any
            // texture id lasts beyond one frame, it indicates a bug.
            let textureIdBlacklist = hashSetPlus HashIdentity.Structural []

            // begin buffer usage
            Buffer.beginFrame vertexBuffer
            Buffer.beginFrame indexBuffer

            // handle render messages
            for renderMessage in renderMessages do
                match renderMessage with
                | ReloadRenderAssets -> renderer.DestroyAssetTextures (Some textureIdBlacklist)

            // prepare asset textures for a finite period of time
            let now = DateTimeOffset.Now
            let assetTags = Array.ofSeq assetTextureRequests.Keys // eager copy to allow modification during enumeration
            let mutable assetTagsEnr = (seq assetTags).GetEnumerator ()
            while assetTagsEnr.MoveNext () && DateTimeOffset.Now - now <= TimeSpan.FromMilliseconds 4.0 do
                let assetTag = assetTagsEnr.Current
                if not (assetTextureOpts.ContainsKey assetTag) then
                    match Metadata.tryGetFilePath assetTag with
                    | Some filePath ->
                        match Hl.tryCreateTextureVulkan true false (Hl.inferTextureCompression filePath) filePath RenderThread vkc with
                        | Right (_, textureParallel) ->
                            let texture = EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = textureParallel }
                            let textureId = textureIdCounter
                            textureIdCounter <- inc textureIdCounter
                            assetTextureStorage.Add (textureId, texture)
                            assetTextureOpts.[assetTag] <- ValueSome textureId
                        | Left _ -> assetTextureOpts.[assetTag] <- ValueNone
                    | None -> ()
                let mutable removed = ()
                assetTextureRequests.TryRemove (assetTag, &removed) |> ignore<bool>

            // check that viewport bounds assumed by drawData match the actual viewport, as they sometimes lag behind upon resize, triggering validation errors when viewport bounds are exceeded.
            let drawDataMatchesViewport =
                int drawData.DisplaySize.X * int drawData.FramebufferScale.X = viewport.Bounds.Width &&
                int drawData.DisplaySize.Y * int drawData.FramebufferScale.Y = viewport.Bounds.Height
            
            // render when desired and drawData matches viewport
            if vkc.RenderAllowed && drawDataMatchesViewport then

                // images added as needed for current frame, associated with descriptor sets by index
                let usedImages = List ()
                
                // set up render
                let mutable renderArea = VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
                let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                let vkPipeline = Pipeline.tryGetVkPipeline VulkanImGui false pipeline |> Option.get // not supporting shader reload of Gaia itself
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                let mutable viewport = Hl.makeViewport false renderArea
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &viewport)
                
                // compute offsets
                if drawData.TotalVtxCount > 0 then
                    
                    // get data size for vertices and indices
                    let vertexSize = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexSize = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge buffer sizes if needed
                    while vertexSize > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                    while indexSize > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                    Nu.Vulkan.Buffer.update vertexBufferSize vertexBuffer vkc
                    Nu.Vulkan.Buffer.update indexBufferSize indexBuffer vkc

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdLists in range.[i]
                        let vertexSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        Nu.Vulkan.Buffer.uploadSubdata vertexOffset 0 vertexSize 1 drawList.VtxBuffer.Data vertexBuffer vkc
                        Nu.Vulkan.Buffer.uploadSubdata indexOffset 0 indexSize 1 drawList.IdxBuffer.Data indexBuffer vkc
                        vertexOffset <- vertexOffset + vertexSize
                        indexOffset <- indexOffset + indexSize

                // bind vertex and index buffers
                if drawData.TotalVtxCount > 0 then // TODO: P1: see if this conditional is actually necessary.
                    let mutable vertexBuffer = vertexBuffer.VkBuffer
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                    Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, indexBuffer.VkBuffer, 0UL, VkIndexType.Uint16)

                // set up scale and translation
                let scale = Array.zeroCreate<single> 2
                scale[0] <- 2.0f / drawData.DisplaySize.X
                scale[1] <- 2.0f / drawData.DisplaySize.Y
                use scalePin = new ArrayPin<_> (scale)
                let translate = Array.zeroCreate<single> 2
                translate[0] <- -1.0f - drawData.DisplayPos.X * scale[0]
                translate[1] <- -1.0f - drawData.DisplayPos.Y * scale[1]
                use translatePin = new ArrayPin<_> (translate)
                Vulkan.vkCmdPushConstants (vkc.RenderCommandBuffer, pipeline.PipelineLayout, VertexStage.VkShaderStageFlags, 0u, 8u, scalePin.VoidPtr)
                Vulkan.vkCmdPushConstants (vkc.RenderCommandBuffer, pipeline.PipelineLayout, VertexStage.VkShaderStageFlags, 8u, 8u, translatePin.VoidPtr)

                // draw command lists, ignoring any commands that use blacklisted textures
                let mutable globalVtxOffset = 0
                let mutable globalIdxOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    
                    // draw commands from list
                    let drawList = let range = drawData.CmdLists in range.[i]
                    for j in 0 .. dec drawList.CmdBuffer.Size do

                        // only render when required texture is not in blacklist
                        let pcmd = let buffer = drawList.CmdBuffer in buffer.[j]
                        if not (textureIdBlacklist.Contains (uint32 pcmd.TextureId)) then

                            // only process when no user callback is provided
                            if pcmd.UserCallback = nativeint 0 then
                                
                                // project scissor/clipping rectangles into framebuffer space
                                let mutable clipMin =
                                    v2
                                        ((pcmd.ClipRect.X - drawData.DisplayPos.X) * drawData.FramebufferScale.X + viewport.x)
                                        ((pcmd.ClipRect.Y - drawData.DisplayPos.Y) * drawData.FramebufferScale.Y + viewport.y)
                                let mutable clipMax =
                                    v2
                                        ((pcmd.ClipRect.Z - drawData.DisplayPos.X) * drawData.FramebufferScale.X + viewport.x)
                                        ((pcmd.ClipRect.W - drawData.DisplayPos.Y) * drawData.FramebufferScale.Y + viewport.y)

                                // only draw if scissor is valid
                                let width = uint (clipMax.X - clipMin.X)
                                let height = uint (clipMax.Y - clipMin.Y)
                                let mutable scissor = VkRect2D (int clipMin.X, int clipMin.Y, width, height)
                                scissor <- Hl.clipRect renderArea scissor
                                if Hl.validateRect scissor then

                                    // set scissor
                                    Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &scissor)

                                    // identify requested texture and assign to it a descriptor set index
                                    let textureId = uint32 pcmd.TextureId
                                    if not (usedImages.Contains textureId) then usedImages.Add textureId
                                    let descriptorSetIndex = usedImages.IndexOf textureId

                                    // specify material
                                    let (texture, sampler) as combined = (renderer.GetTexture textureId, renderer.GetSampler textureId)
                                    let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet descriptorSetIndex combined pipeline vkc $ fun vkSet ->
                                        Pipeline.writeDescriptorCombinedImageSampler 0 0 texture sampler vkSet vkc

                                    // bind descriptor set
                                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)

                                    // draw
                                    Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, pcmd.ElemCount, 1u, pcmd.IdxOffset + uint globalIdxOffset, int pcmd.VtxOffset + globalVtxOffset, 0u)

                                    // advance pipeline
                                    Pipeline.advance 1 pipeline

                            // otherwise we don't have a way to handle user callbacks, so throw in that case
                            else Log.warn "Encountered ImGui user callback; ignoring."

                    // update offsets
                    globalIdxOffset <- globalIdxOffset + drawList.IdxBuffer.Size
                    globalVtxOffset <- globalVtxOffset + drawList.VtxBuffer.Size

                // tear down render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

        member renderer.CleanUp () =
            Sampler.destroy fontSampler vkc
            Sampler.destroy assetSampler vkc
            renderer.DestroyAssetTextures None
            Texture.destroy fontTexture vkc
            Pipeline.destroy pipeline vkc

/// VulkanRendererImGui functions.
[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make assetTextureRequests assetTextures fonts viewport vkc =
        let rendererImGui = VulkanRendererImGui (assetTextureRequests, assetTextures, viewport, vkc)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui