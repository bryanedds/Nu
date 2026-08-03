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
    abstract PreRender : renderMessages : RenderMessageImGui List -> unit
    abstract Render : viewport_ : Viewport -> drawData : ImDrawDataPtr -> unit
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
        member renderer.PreRender _ = ()
        member renderer.Render _ _ = ()
        member renderer.CleanUp () = ()

/// Renders an imgui view via Vulkan.
type VulkanRendererImGui
    (assetTextureRequests : ConcurrentDictionary<AssetTag, unit>,
     assetTextureOpts : ConcurrentDictionary<AssetTag, uint32 voption>,
     viewport : Viewport,
     context : VulkanContext) =

    let assetTextureStorage = dictPlus<uint32, Texture> HashIdentity.Structural []
    let mutable viewport = viewport
    let mutable pipeline = Unchecked.defaultof<Pipeline>
    let mutable fontSampler = Unchecked.defaultof<Sampler>
    let mutable assetSampler = Unchecked.defaultof<Sampler>
    let mutable fontTexture = Unchecked.defaultof<Texture>
    let mutable vertexBufferSize = 8192 // TODO: populate from a constant.
    let mutable vertexBuffer = Unchecked.defaultof<VulkanBuffer>
    let mutable indexBufferSize = 1024 // TODO: populate from a constant.
    let mutable indexBuffer = Unchecked.defaultof<VulkanBuffer>
    
    // in the event of clearing asset textures, we keep a blacklist of texture ids that have been recently destroyed.
    // In the code, we make an attempt to clear all artifacts that might have a potentially invalidated texture id
    // laying around, but one might already be on the stack on another thread, so we use this extra caution. This
    // blacklist only lasts for a single render frame.
    let mutable textureIdBlacklist = hashSetPlus<uint32> HashIdentity.Structural []
    
    member private renderer.DestroyAssetTextures (destroyedTextureIdsOpt : uint32 HashSet option) =
        ConcurrentCommandQueue.waitIdle context.RenderQueue
        for texturedIdOpt in assetTextureOpts.Values do
            match texturedIdOpt with
            | ValueSome textureId ->
                match Dictionary.tryFind textureId assetTextureStorage with
                | Some texture ->
                    Texture.destroy texture context
                    assetTextureStorage.Remove textureId |> ignore<bool>
                | None -> ()
                match destroyedTextureIdsOpt with
                | Some destroyedTextureIds -> destroyedTextureIds.Add textureId |> ignore<bool>
                | None -> ()
            | ValueNone -> ()
        assetTextureOpts.Clear ()
    
    member private renderer.GetTexture textureId =
        if textureId = fontTexture.Id
        then fontTexture
        else
            match Dictionary.tryFind textureId assetTextureStorage with
            | Some texture -> texture
            | None -> Texture.EmptyTexture

    member private renderer.GetSampler textureId =
        if textureId = fontTexture.Id
        then fontSampler
        else assetSampler

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
            let textureInternal = TextureInternal.create MipmapNone AttachmentNone Texture2d VkImageUsageFlags.None Uncompressed.ImageFormat Rgba metadata context
            TextureInternal.upload metadata 0 0 pixels RenderThread textureInternal context
            fontTexture <- EagerTexture textureInternal
            
            // create samplers
            fontSampler <- Sampler.create VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false context
            assetSampler <- Sampler.create VkSamplerAddressMode.Repeat VkFilter.Nearest VkFilter.Nearest false context

            // set font atlas TexId
            fonts.SetTexID (nativeint fontTexture.Id)
            
            // NOTE: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- VulkanBuffer.create (Vertex true) vertexBufferSize context
            indexBuffer <- VulkanBuffer.create (Index true) indexBufferSize context

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
                    [|context.SwapFormat|] None
                    [|vertexBuffer; indexBuffer|]

        member renderer.PreRender renderMessages =

            // begin buffer usage
            VulkanBuffer.beginFrame vertexBuffer
            VulkanBuffer.beginFrame indexBuffer

            // clear blacklist
            textureIdBlacklist.Clear ()

            // categorize render messages
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
                        let compression = Hl.inferTextureCompression filePath
                        match TextureInternal.tryCreate true false compression filePath RenderThread context with
                        | Right textureInternal ->
                            let texture = EagerTexture textureInternal
                            assetTextureStorage.Add (texture.Id, texture)
                            assetTextureOpts[assetTag] <- ValueSome texture.Id
                        | Left _ -> assetTextureOpts[assetTag] <- ValueNone
                    | None -> ()
                let mutable removed = ()
                assetTextureRequests.TryRemove (assetTag, &removed) |> ignore<bool>

        member renderer.Render viewport_ (drawData : ImDrawDataPtr) =

            // update viewport
            viewport <- viewport_

            // update imgui's display properties from the viewport's physical window extent
            let io = ImGui.GetIO ()
            io.DisplaySize <- viewport.Bounds.Size.V2
            io.DisplayFramebufferScale <- v2One

            // render when allowed
            if context.RenderAllowed then

                // grab pipeline, asserting non-None since shader reload for ImGui isn't supported
                let vkPipeline = Pipeline.tryGetVkPipeline VulkanImGui false pipeline |> Option.get

                // set up render
                let mutable renderArea = VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
                let mutable vkViewport = Hl.makeViewport false renderArea
                Hl.withRenderingInfo [|context.SwapchainImageView|] None renderArea None $ fun renderingInfo ->
                    let mutable renderingInfo = renderingInfo
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                // compute offsets
                if drawData.TotalVtxCount > 0 then
                    
                    // get data buffer size totals for vertices and indices
                    let vertexBufferSizeTotal = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexBufferSizeTotal = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge buffer sizes if needed
                    while vertexBufferSizeTotal > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                    while indexBufferSizeTotal > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                    VulkanBuffer.ensureWidth vertexBufferSize vertexBuffer context
                    VulkanBuffer.ensureWidth indexBufferSize indexBuffer context

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdLists in range[i]
                        let vertexBufferSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexBufferSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        VulkanBuffer.writeSubdata vertexOffset 0 vertexBufferSize 1 drawList.VtxBuffer.Data vertexBuffer context
                        VulkanBuffer.writeSubdata indexOffset 0 indexBufferSize 1 drawList.IdxBuffer.Data indexBuffer context
                        vertexOffset <- vertexOffset + vertexBufferSize
                        indexOffset <- indexOffset + indexBufferSize

                    // flush data
                    VulkanBuffer.flushSubdata 0 0 vertexBufferSizeTotal 1 vertexBuffer context
                    VulkanBuffer.flushSubdata 0 0 indexBufferSizeTotal 1 indexBuffer context

                    // bind vertex and index buffers
                    let mutable vertexBuffer = vertexBuffer.VkBuffer
                    let mutable vertexOffset = 0UL
                    DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
                    DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, indexBuffer.VkBuffer, 0UL, VkIndexType.Uint16)

                // set up scale and translation
                let scale = Array.zeroCreate<single> 2
                scale[0] <- 2.0f / drawData.DisplaySize.X
                scale[1] <- 2.0f / drawData.DisplaySize.Y
                use scalePin = new ArrayPin<_> (scale)
                let translate = Array.zeroCreate<single> 2
                translate[0] <- -1.0f - drawData.DisplayPos.X * scale[0]
                translate[1] <- -1.0f - drawData.DisplayPos.Y * scale[1]
                use translatePin = new ArrayPin<_> (translate)
                DeviceApi.vkCmdPushConstants (context.RenderCommandBuffer, pipeline.PipelineLayout, VertexStage.VkShaderStageFlags, 0u, 8u, scalePin.VoidPtr)
                DeviceApi.vkCmdPushConstants (context.RenderCommandBuffer, pipeline.PipelineLayout, VertexStage.VkShaderStageFlags, 8u, 8u, translatePin.VoidPtr)

                // draw command lists, ignoring any commands that use blacklisted textures
                let mutable globalVtxOffset = 0
                let mutable globalIdxOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    
                    // draw commands from list
                    let drawList = let range = drawData.CmdLists in range[i]
                    for j in 0 .. dec drawList.CmdBuffer.Size do

                        // only render when required texture is not in blacklist
                        let pcmd = let buffer = drawList.CmdBuffer in buffer[j]
                        if not (textureIdBlacklist.Contains (uint32 pcmd.TextureId)) then

                            // only process when no user callback is provided
                            if pcmd.UserCallback = nativeint 0 then
                                
                                // project scissor/clipping rectangles into framebuffer space
                                let mutable clipMin =
                                    v2
                                        (pcmd.ClipRect.X - drawData.DisplayPos.X + vkViewport.x)
                                        (pcmd.ClipRect.Y - drawData.DisplayPos.Y + vkViewport.y)
                                let mutable clipMax =
                                    v2
                                        (pcmd.ClipRect.Z - drawData.DisplayPos.X + vkViewport.x)
                                        (pcmd.ClipRect.W - drawData.DisplayPos.Y + vkViewport.y)

                                // only draw if scissor is valid
                                let width = uint (clipMax.X - clipMin.X)
                                let height = uint (clipMax.Y - clipMin.Y)
                                let mutable vkScissor = VkRect2D (int clipMin.X, int clipMin.Y, width, height)
                                vkScissor <- Hl.clipRect renderArea vkScissor
                                if Hl.validateRect vkScissor then

                                    // set scissor
                                    DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&vkScissor)

                                    // specify material
                                    let textureId = uint32 pcmd.TextureId
                                    let (texture, sampler) as combined = (renderer.GetTexture textureId, renderer.GetSampler textureId)
                                    let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 0 combined pipeline $ fun vkSet ->
                                        Pipeline.writeDescriptorCombinedTextureSampler 0 0 texture sampler vkSet

                                    // bind descriptor set
                                    DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&materialDescriptorSet, 0u, nullPtr)

                                    // draw
                                    DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, pcmd.ElemCount, 1u, pcmd.IdxOffset + uint globalIdxOffset, int pcmd.VtxOffset + globalVtxOffset, 0u)

                                    // report drawing
                                    Hl.reportDrawCall 1 false

                                    // advance pipeline
                                    Pipeline.advance pipeline

                            // otherwise we don't have a way to handle user callbacks, so throw in that case
                            else Log.warn "Encountered ImGui user callback; ignoring."

                    // update offsets
                    globalIdxOffset <- globalIdxOffset + drawList.IdxBuffer.Size
                    globalVtxOffset <- globalVtxOffset + drawList.VtxBuffer.Size

                // tear down render
                DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

                // report draw scope
                Hl.reportDrawScope ()

                // advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer context

        member renderer.CleanUp () =
            Sampler.destroy fontSampler
            Sampler.destroy assetSampler
            renderer.DestroyAssetTextures None
            Texture.destroy fontTexture context
            Pipeline.destroy pipeline context

/// VulkanRendererImGui functions.
[<RequireQualifiedAccess>]
module VulkanRendererImGui =

    /// Make a Vulkan imgui renderer.
    let make assetTextureRequests assetTextures fonts viewport context =
        let rendererImGui = VulkanRendererImGui (assetTextureRequests, assetTextures, viewport, context)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui