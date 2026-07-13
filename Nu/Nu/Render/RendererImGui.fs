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
    
    // in the event of clearing asset textures, we keep a blacklist of texture ids that have been recently destroyed.
    // In the code, we make an attempt to clear all artifacts that might have a potentially invalidated texture id
    // laying around, but one might already be on the stack on another thread, so we use this extra caution. This
    // blacklist only lasts for a single render frame.
    let mutable textureIdBlacklist = hashSetPlus<uint32> HashIdentity.Structural []
    
    member private renderer.DestroyAssetTextures (destroyedTextureIdsOpt : uint32 HashSet option) =
        ConcurrentCommandQueue.waitIdle vkc.RenderQueue
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
            let textureInternal = TextureInternal.create MipmapNone AttachmentNone Texture2d VkImageUsageFlags.None Uncompressed.ImageFormat Rgba metadata vkc
            TextureInternal.upload metadata 0 0 pixels RenderThread textureInternal vkc
            fontTexture <- EagerTexture textureInternal
            
            // create samplers
            fontSampler <- Sampler.create VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false vkc
            assetSampler <- Sampler.create VkSamplerAddressMode.Repeat VkFilter.Nearest VkFilter.Nearest false vkc
            
            // set font atlas TexId to 0
            fonts.SetTexID (nativeint textureIdCounter)
            textureIdCounter <- inc textureIdCounter
            
            // NOTE: DJL: this is not used in the dear imgui vulkan backend.
            fonts.ClearTexData ()

            // create vertex and index buffers
            vertexBuffer <- Nu.Vulkan.Buffer.create (Vertex true) vertexBufferSize vkc
            indexBuffer <- Nu.Vulkan.Buffer.create (Index true) indexBufferSize vkc

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

        member renderer.PreRender renderMessages =

            // begin buffer usage
            Buffer.beginFrame vertexBuffer
            Buffer.beginFrame indexBuffer

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
                        match Hl.tryCreateTextureInternal true false (Hl.inferTextureCompression filePath) filePath RenderThread vkc with
                        | Right textureInternal ->
                            let texture = EagerTexture textureInternal
                            let textureId = textureIdCounter
                            textureIdCounter <- inc textureIdCounter
                            assetTextureStorage.Add (textureId, texture)
                            assetTextureOpts[assetTag] <- ValueSome textureId
                        | Left _ -> assetTextureOpts[assetTag] <- ValueNone
                    | None -> ()
                let mutable removed = ()
                assetTextureRequests.TryRemove (assetTag, &removed) |> ignore<bool>

        member renderer.Render viewport_ (drawData : ImDrawDataPtr) =

            // update imgui's display frame buffer scale
            let pixelDensity = Hl.getWindowPixelDensity vkc.Window
            let io = ImGui.GetIO ()
            io.DisplayFramebufferScale <- v2Dup pixelDensity

            // update viewport, updating the imgui display size as needed
            if viewport <> viewport_ then
                io.DisplaySize <- viewport_.Bounds.Size.V2 // NOTE: DJL: this is not set in the dear imgui vulkan backend but IS necessary!
                viewport <- viewport_

            // check that viewport bounds assumed by drawData match the actual viewport, as they sometimes lag behind upon resize, triggering validation errors when viewport bounds are exceeded.
            let pixelDensity = Hl.getWindowPixelDensity vkc.Window
            let viewportPixelWidth = int (round (single viewport.Bounds.Width * pixelDensity))
            let viewportPixelHeight = int (round (single viewport.Bounds.Height * pixelDensity))
            let drawDataMatchesViewport =
                int (round (drawData.DisplaySize.X * drawData.FramebufferScale.X)) = viewportPixelWidth &&
                int (round (drawData.DisplaySize.Y * drawData.FramebufferScale.Y)) = viewportPixelHeight
            
            // render when allowed and drawData matches viewport
            if vkc.RenderAllowed && drawDataMatchesViewport then

                // images added as needed for current frame, associated with descriptor sets by index
                let usedImages = List ()
                
                // set up render
                let mutable renderArea =
                    VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
                    |> Hl.scaleRectForPixelDensity pixelDensity
                let mutable renderingInfo = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, &&renderingInfo)
                let mutable viewport = Hl.makeViewport false renderArea
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, &&viewport)
                let vkPipeline = Pipeline.tryGetVkPipeline VulkanImGui false pipeline |> Option.get // not supporting shader reload of Gaia itself
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                
                // compute offsets
                if drawData.TotalVtxCount > 0 then
                    
                    // get data buffer size totals for vertices and indices
                    let vertexBufferSizeTotal = drawData.TotalVtxCount * sizeof<ImDrawVert>
                    let indexBufferSizeTotal = drawData.TotalIdxCount * sizeof<uint16>

                    // enlarge buffer sizes if needed
                    while vertexBufferSizeTotal > vertexBufferSize do vertexBufferSize <- vertexBufferSize * 2
                    while indexBufferSizeTotal > indexBufferSize do indexBufferSize <- indexBufferSize * 2
                    Nu.Vulkan.Buffer.ensureWidth vertexBufferSize vertexBuffer vkc
                    Nu.Vulkan.Buffer.ensureWidth indexBufferSize indexBuffer vkc

                    // upload vertices and indices
                    let mutable vertexOffset = 0
                    let mutable indexOffset = 0
                    for i in 0 .. dec drawData.CmdListsCount do
                        let drawList = let range = drawData.CmdLists in range[i]
                        let vertexBufferSize = drawList.VtxBuffer.Size * sizeof<ImDrawVert>
                        let indexBufferSize = drawList.IdxBuffer.Size * sizeof<uint16>
                        Nu.Vulkan.Buffer.writeSubdata vertexOffset 0 vertexBufferSize 1 drawList.VtxBuffer.Data vertexBuffer vkc
                        Nu.Vulkan.Buffer.writeSubdata indexOffset 0 indexBufferSize 1 drawList.IdxBuffer.Data indexBuffer vkc
                        vertexOffset <- vertexOffset + vertexBufferSize
                        indexOffset <- indexOffset + indexBufferSize

                    // flush data
                    Nu.Vulkan.Buffer.flushSubdata 0 0 vertexBufferSizeTotal 1 vertexBuffer vkc
                    Nu.Vulkan.Buffer.flushSubdata 0 0 indexBufferSizeTotal 1 indexBuffer vkc

                    // bind vertex and index buffers
                    let mutable vertexBuffer = vertexBuffer.VkBuffer
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
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
                                    Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, &&scissor)

                                    // identify requested texture and assign to it a descriptor set index
                                    let textureId = uint32 pcmd.TextureId
                                    if not (usedImages.Contains textureId) then usedImages.Add textureId
                                    let descriptorSetIndex = usedImages.IndexOf textureId

                                    // specify material
                                    let (texture, sampler) as combined = (renderer.GetTexture textureId, renderer.GetSampler textureId)
                                    let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet descriptorSetIndex combined pipeline vkc $ fun vkSet ->
                                        Pipeline.writeDescriptorCombinedTextureSampler 0 0 texture sampler vkSet vkc

                                    // bind descriptor set
                                    Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&materialDescriptorSet, 0u, nullPtr)

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

                // report draw scope
                Hl.reportDrawScope ()

                // advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer vkc

                // clear blacklist
                textureIdBlacklist.Clear ()

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