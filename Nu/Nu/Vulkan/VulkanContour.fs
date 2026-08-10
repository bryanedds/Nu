// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

/// A unit quad used for contour rendering.
/// Vertices are in [0,1] range so the vertex shader can map them
/// directly to the bounding box via  bboxMin + position * bboxSize.
module private ContourQuad =

    let vertices : Vector2 array =
        [| v2 0.0f 0.0f; v2 0.0f 1.0f; v2 1.0f 0.0f; v2 1.0f 1.0f |]

    let indices : uint32 array =
        [| 0u; 2u; 1u; 1u; 2u; 3u |]

/// Per-shape data uploaded to the GPU uniform buffer.
/// Layout matches the GLSL std430 layout in Contour.vert/frag:
///   mat4 MVP (64B), vec4 color (16B), uint curveCount (4B), uint flags (4B),
///   uint hBands (4B), uint vBands (4B), vec2 bboxMin (8B), vec2 bboxSize (8B),
///   vec4 bandTransform (16B) = 128B.
/// The vertex shader maps a [0,1] unit quad to [bboxMin, bboxMin+bboxSize].
//
// Flags bit layout (must match Contour.frag):
//   bit 0: fillMode (0=NonZero, 1=EvenOdd)
[<Struct; StructLayout (LayoutKind.Sequential)>]
type private ShapeGPU =
    { MVP : Matrix4x4
      Color : Color
      CurveCount : uint32
      Flags : uint32 // bit 0: fillMode
      HBands : uint32
      VBands : uint32
      BboxMinX : single
      BboxMinY : single
      BboxSizeX : single
      BboxSizeY : single
      BandTransform : Vector4 }

/// Slug GPU contour pipeline (analytic coverage via horizontal + vertical ray casting).
[<RequireQualifiedAccess>]
module Contour =

    let private maxCurves = 4096
    let private initialShapeDataSize = 256
    let private initialBandDataSize = 65536 // uint32 entries (256KB)

    let createPipeline vkc =

        let quadVertexBuffer = VulkanBuffer.create (Vertex true) (sizeof<Vector2> * ContourQuad.vertices.Length) vkc
        let quadIndexBuffer = VulkanBuffer.create (BufferType.Index true) (sizeof<uint32> * ContourQuad.indices.Length) vkc

        VulkanBuffer.uploadArray ContourQuad.vertices quadVertexBuffer vkc
        VulkanBuffer.uploadArray ContourQuad.indices quadIndexBuffer vkc

        let shapeDataBuffer = VulkanBuffer.create Uniform (sizeof<ShapeGPU> * initialShapeDataSize) vkc
        let curveDataBuffer = VulkanBuffer.create Storage (sizeof<Vector4> * 2 * maxCurves) vkc
        let bandDataBuffer = VulkanBuffer.create Storage (sizeof<uint32> * initialBandDataSize) vkc

        let vertexSize = sizeof<Vector2>
        let pipeline =
            Pipeline.create
                Constants.Paths.ContourShaderFilePath
                [|VulkanTransparent|] [|true|]
                [|Pipeline.vertex 0 vertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single2 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexAndFragmentStage 1
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1
                      Pipeline.descriptor 2 StorageBuffer FragmentStage 1|]|]
                [||] [|vkc.SwapFormat|] None
                [|quadVertexBuffer; quadIndexBuffer; shapeDataBuffer; curveDataBuffer; bandDataBuffer|]

        (quadVertexBuffer, quadIndexBuffer, shapeDataBuffer, curveDataBuffer, bandDataBuffer, pipeline)

    /// Low-level draw helper: uploads a ContourSlugGeometry + color to the GPU and issues one draw call.
    let private drawContourSlugGeometry
        (geometry : ContourSlugGeometry)
        (color : Color)
        (absolute : bool)
        (viewProjectionClipAbsolute : Matrix4x4 inref)
        (viewProjectionClipRelative : Matrix4x4 inref)
        (modelViewProjection : Matrix4x4 inref)
        (clipOpt : Box2 voption inref)
        (viewport : Viewport)
        ((quadVertexBuffer, quadIndexBuffer, shapeDataBuffer, curveDataBuffer, bandDataBuffer, pipeline) : VulkanBuffer * VulkanBuffer * VulkanBuffer * VulkanBuffer * VulkanBuffer * Pipeline)
        (vkc : VulkanContext) =

        if geometry.Curves.Length > 0 then

            if geometry.BandEntries.Length <> geometry.HBands + geometry.VBands then
                Log.warnOnce "Slug geometry band entry count mismatch - skipping draw"
            else
                match Pipeline.tryGetVkPipeline VulkanTransparent true pipeline with
                | Some vkPipeline ->

                    // upload curve data
                    let packedCurves = Contour.packCurvesGPU geometry.Curves
                    VulkanBuffer.uploadArray packedCurves curveDataBuffer vkc

                    // Upload band data: header (curveCount + curveOffset per band entry)
                    // followed by flat curve index array (H then V, with absolute offsets).
                    let headerSize = geometry.BandEntries.Length * 2
                    let totalBandDataSize = headerSize + geometry.BandCurveIndices.Length
                    let bandDataPacked = Array.zeroCreate<uint32> totalBandDataSize
                    for i in 0 .. geometry.BandEntries.Length - 1 do
                        bandDataPacked[i * 2] <- geometry.BandEntries[i].CurveCount
                        bandDataPacked[i * 2 + 1] <- geometry.BandEntries[i].CurveOffset
                    Array.Copy (geometry.BandCurveIndices, 0, bandDataPacked, headerSize, geometry.BandCurveIndices.Length)
                    VulkanBuffer.uploadArray bandDataPacked bandDataBuffer vkc

                    // Upload shape uniforms: MVP, color, bounding box, band layout.
                    // Expand the quad by one physical pixel in each local axis so rasterization
                    // cannot clip analytic edge coverage when the shape is minified.
                    let bbox = geometry.LocalBounds
                    let physicalViewportSize =
                        Vector2
                            (single viewport.Inner.Size.X,
                             single viewport.Inner.Size.Y)
                    let center = bbox.Min + bbox.Size * 0.5f
                    let centerPixels =
                        let clip = Vector4.Transform (Vector4 (center.X, center.Y, 0.0f, 1.0f), modelViewProjection)
                        if abs clip.W > 1.0e-6f then
                            let invW = 1.0f / clip.W
                            Vector2
                                ((clip.X * invW + 1.0f) * 0.5f * physicalViewportSize.X,
                                 (clip.Y * invW + 1.0f) * 0.5f * physicalViewportSize.Y)
                        else Vector2.Zero
                    let paddingX =
                        let axis = v2UnitX
                        let pixelsPerLocalUnit =
                            Vector2.Distance (centerPixels,
                                let point = center + axis
                                let clip = Vector4.Transform (Vector4 (point.X, point.Y, 0.0f, 1.0f), modelViewProjection)
                                if abs clip.W > 1.0e-6f then
                                    let invW = 1.0f / clip.W
                                    Vector2
                                        ((clip.X * invW + 1.0f) * 0.5f * physicalViewportSize.X,
                                        (clip.Y * invW + 1.0f) * 0.5f * physicalViewportSize.Y)
                                else Vector2.Zero)
                        if pixelsPerLocalUnit > 1.0e-6f then max 0.001f (1.0f / pixelsPerLocalUnit)
                        else 0.001f
                    let paddingY =
                        let axis = v2UnitY
                        let pixelsPerLocalUnit = // ideally we deduplicate this with above but modelViewProjection is inref
                            Vector2.Distance (centerPixels,
                                let point = center + axis
                                let clip = Vector4.Transform (Vector4 (point.X, point.Y, 0.0f, 1.0f), modelViewProjection)
                                if abs clip.W > 1.0e-6f then
                                    let invW = 1.0f / clip.W
                                    Vector2
                                        ((clip.X * invW + 1.0f) * 0.5f * physicalViewportSize.X,
                                        (clip.Y * invW + 1.0f) * 0.5f * physicalViewportSize.Y)
                                else Vector2.Zero)
                        if pixelsPerLocalUnit > 1.0e-6f then max 0.001f (1.0f / pixelsPerLocalUnit)
                        else 0.001f
                    let flags = match geometry.FillWinding with EvenOdd -> 1u | NonZero -> 0u
                    let shapeGPU =
                        { MVP = modelViewProjection
                          Color = color
                          CurveCount = uint32 geometry.Curves.Length
                          Flags = flags
                          HBands = uint32 geometry.HBands
                          VBands = uint32 geometry.VBands
                          BboxMinX = bbox.Min.X - paddingX
                          BboxMinY = bbox.Min.Y - paddingY
                          BboxSizeX = bbox.Size.X + paddingX * 2.0f
                          BboxSizeY = bbox.Size.Y + paddingY * 2.0f
                          BandTransform = geometry.BandTransform }
                    VulkanBuffer.uploadValue shapeGPU shapeDataBuffer vkc

                    // specify descriptor set
                    let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline $ fun vkSet ->
                        Pipeline.writeDescriptorUniformBuffer 0 0 shapeDataBuffer vkSet
                        Pipeline.writeDescriptorStorageBuffer 1 0 curveDataBuffer vkSet
                        Pipeline.writeDescriptorStorageBuffer 2 0 bandDataBuffer vkSet

                    // set up render area
                    let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
                    let mutable vkViewport = Hl.makeViewport true renderArea
                    let mutable scissor = renderArea

                    // apply clipping if specified
                    match clipOpt with
                    | ValueSome clip ->
                        let viewProjection = if absolute then &viewProjectionClipAbsolute else &viewProjectionClipRelative
                        let minClip = Vector4.Transform(Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection).V2
                        let minNdc = minClip * single viewport.DisplayScalar
                        let minScissor = (minNdc + v2One) * 0.5f * viewport.Inner.Size.V2
                        let sizeClip = Vector4.Transform(Vector4 (clip.Size, 0.0f, 1.0f), viewProjection).V2
                        let sizeNdc = sizeClip * single viewport.DisplayScalar
                        let sizeScissor = sizeNdc * 0.5f * viewport.Inner.Size.V2
                        let offset = v2i viewport.Inner.Min.X (viewport.Outer.Max.Y - viewport.Inner.Max.Y)
                        scissor <-
                            VkRect2D
                                ((minScissor.X |> round |> int) + offset.X,
                                 (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                                 uint sizeScissor.X,
                                 uint sizeScissor.Y)
                        scissor <- Hl.clipRect renderArea scissor
                    | ValueNone -> ()

                    // only draw if scissor is valid
                    if Hl.validateRect scissor then
                        Hl.withRenderingInfo [|vkc.SwapchainImageView|] None renderArea None $ fun renderingInfo ->
                            let mutable renderingInfo = renderingInfo
                            DeviceApi.vkCmdBeginRendering (vkc.RenderCommandBuffer, &&renderingInfo)
                        DeviceApi.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                        DeviceApi.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, &&scissor)

                        // bind pipeline
                        DeviceApi.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                        // bind quad vertex and index buffers
                        let mutable vkVertexBuffer = quadVertexBuffer.VkBuffer
                        let mutable vkVertexOffset = 0UL
                        DeviceApi.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, &&vkVertexBuffer, &&vkVertexOffset)
                        DeviceApi.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, quadIndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                        // bind descriptor set
                        DeviceApi.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)

                        // draw the quad
                        DeviceApi.vkCmdDrawIndexed (vkc.RenderCommandBuffer, 6u, 1u, 0u, 0, 0u)

                        // tear down render
                        DeviceApi.vkCmdEndRendering vkc.RenderCommandBuffer

                        // report drawing
                        Hl.reportDrawCall 1 true

                        // advance pipeline
                        Pipeline.advance pipeline

                        // advance rendering command buffer
                        VulkanContext.advanceRenderCommandBuffer vkc

                // abort
                | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")

    /// Draw a Contour as up to two separate Slug passes (fill then stroke).
    let drawContour
        (contour : Contour)
        (absolute : bool)
        (viewProjectionClipAbsolute : Matrix4x4 inref)
        (viewProjectionClipRelative : Matrix4x4 inref)
        (modelViewProjection : Matrix4x4 inref)
        (clipOpt : Box2 voption inref)
        (viewport : Viewport)
        (buffers : VulkanBuffer * VulkanBuffer * VulkanBuffer * VulkanBuffer * VulkanBuffer * Pipeline)
        (vkc : VulkanContext) =

        // Fill pass (if geometry present)
        match contour.FillGeometryOpt with
        | ValueSome geom ->
            drawContourSlugGeometry geom contour.FillColor absolute
                &viewProjectionClipAbsolute &viewProjectionClipRelative
                &modelViewProjection &clipOpt viewport buffers vkc
        | ValueNone -> ()

        // Stroke pass (if geometry present)
        match contour.StrokeGeometryOpt with
        | ValueSome geom ->
            drawContourSlugGeometry geom contour.StrokeColor absolute
                &viewProjectionClipAbsolute &viewProjectionClipRelative
                &modelViewProjection &clipOpt viewport buffers vkc
        | ValueNone -> ()