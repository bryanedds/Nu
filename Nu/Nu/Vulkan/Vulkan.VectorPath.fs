// Nu Game Engine.
// Copyright (C) Bryan Edds.
namespace Nu
open System.Numerics
/// Describes a vector path command.
type [<Struct>] VectorPathCommand =
    | MoveTo of endPoint: Vector2
    | LineTo of endPoint: Vector2
    | QuadraticCurveTo of control1: Vector2 * endPoint: Vector2
    | CubicCurveTo of control1: Vector2 * control2: Vector2 * endPoint: Vector2
    | CloseContour

/// The winding rule to fill a vector path.
type [<Struct>] WindingRule =
    | EvenOdd
    | NonZero
    | Positive
    | Negative
    | AbsGeqTwo
    with static member Default = EvenOdd // Default from LibTessDotNet

namespace Vortice.Vulkan
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open LibTessDotNet
open Prime
open Nu

[<RequireQualifiedAccess>]
module VectorPath =

    /// Represents a tesselated vertex for vector path rendering.
    [<Struct; StructLayout (LayoutKind.Sequential)>]
    type VectorPathVertex =
        { Position : Vector2
          Color : Color }

    /// Helper to create bezier curve points.
    let private evaluateQuadraticBezier (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (t : single) =
        let t1 = 1.0f - t
        t1 * t1 * p0 + 2.0f * t1 * t * p1 + t * t * p2

    /// Helper to create cubic bezier curve points.
    let private evaluateCubicBezier (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (t : single) =
        let t1 = 1.0f - t
        t1 * t1 * t1 * p0 + 3.0f * t1 * t1 * t * p1 + 3.0f * t1 * t * t * p2 + t * t * t * p3
        
    let private fromTess (v : ContourVertex inref) = Vector2 (v.Position.X, v.Position.Y)
    let private toTess (v : Vector2 inref) = ContourVertex (Vec3 (v.X, v.Y, 0.0f))
    
    /// Compute miter joint for two consecutive line segments with anti-aliasing fringe.
    let private computeMiterWithFringe (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (halfWidth : single) (fringeWidth : single) =
        let dir1 = Vector2.Normalize (p1 - p0)
        let dir2 = Vector2.Normalize (p2 - p1)
        let n1 = v2 -dir1.Y dir1.X
        let n2 = v2 -dir2.Y dir2.X
        let miterDir = Vector2.Normalize (n1 + n2)
        let miterLength = 
            let sinHalfAngle = Vector2.Cross (dir1, dir2) |> abs
            if sinHalfAngle > 0.01f then halfWidth / sinHalfAngle else halfWidth
        let miterLimit = halfWidth * 4.0f
        let actualMiterLength = if miterLength <= miterLimit then miterLength else halfWidth
        
        // Inner edge (solid)
        let innerOffset1 = miterDir * actualMiterLength
        let innerOffset2 = miterDir * -actualMiterLength
        
        // Outer edge (transparent for anti-aliasing)
        let fringeMiterLength = 
            let fringeHalfWidth = halfWidth + fringeWidth
            let fringeMiterLength = 
                let sinHalfAngle = Vector2.Cross (dir1, dir2) |> abs
                if sinHalfAngle > 0.01f then fringeHalfWidth / sinHalfAngle else fringeHalfWidth
            if fringeMiterLength <= miterLimit * 1.5f then fringeMiterLength else fringeHalfWidth
        let outerOffset1 = miterDir * fringeMiterLength
        let outerOffset2 = miterDir * -fringeMiterLength
        
        (innerOffset1, innerOffset2, outerOffset1, outerOffset2)
    
    /// Tesselate vector path commands into triangle vertices.
    let tesselateVectorPath (commands : VectorPathCommand array) (fillColor : Color) windingRule (strokeColor : Color) (strokeThickness : single) =
        
        let fillTess = Tess ()
        let fill = fillColor.A > 0.0f
        let stroke = strokeColor.A > 0.0f && strokeThickness > 0.0f
        let epsilon = 0.0001f
        let mutable currentPoint = v2Zero
        let mutable pathStart = v2Zero // for contour closing
        let fillContourPoints = List<ContourVertex> ()
        let strokeContours = List<List<Vector2>> ()
        let mutable currentStrokeContour = List<Vector2> ()
        
        let saveCurrentContours () =
            if fillContourPoints.Count > 0 then
                fillTess.AddContour fillContourPoints
                fillContourPoints.Clear ()
            if currentStrokeContour.Count > 0 then
                strokeContours.Add currentStrokeContour
                currentStrokeContour <- List<Vector2> ()
        
        for command in commands do
            match command with
            | MoveTo point ->
                saveCurrentContours ()
                currentPoint <- point
                pathStart <- point
                if fill then fillContourPoints.Add (toTess &point)
                if stroke then currentStrokeContour.Add point
                
            | LineTo point ->
                if fill then fillContourPoints.Add (toTess &point)
                if stroke then currentStrokeContour.Add point
                currentPoint <- point
                
            | QuadraticCurveTo (control, endpoint) ->
                let steps = 20
                for i in 1 .. steps do
                    let t = single i / single steps
                    let point = evaluateQuadraticBezier currentPoint control endpoint t
                    if fill then fillContourPoints.Add (toTess &point)
                    if stroke then currentStrokeContour.Add point
                currentPoint <- endpoint
                
            | CubicCurveTo (control1, control2, endpoint) ->
                let steps = 20
                for i in 1 .. steps do
                    let t = single i / single steps
                    let point = evaluateCubicBezier currentPoint control1 control2 endpoint t
                    if fill then fillContourPoints.Add (toTess &point)     
                    if stroke then currentStrokeContour.Add point
                currentPoint <- endpoint
                
            | CloseContour ->
                if Vector2.DistanceSquared (currentPoint, pathStart) >= epsilon then
                    if fillContourPoints.Count > 0 then fillContourPoints.Add (toTess &pathStart)
                    if currentStrokeContour.Count > 0 then currentStrokeContour.Add pathStart
                saveCurrentContours ()
        
        // Tesselate any remaining contours
        saveCurrentContours ()
        
        let triangle = 3
        let tessWindingRule =
            match windingRule with
            | EvenOdd -> LibTessDotNet.WindingRule.EvenOdd
            | NonZero -> LibTessDotNet.WindingRule.NonZero
            | Positive -> LibTessDotNet.WindingRule.Positive
            | Negative -> LibTessDotNet.WindingRule.Negative
            | AbsGeqTwo -> LibTessDotNet.WindingRule.AbsGeqTwo
        fillTess.Tessellate (tessWindingRule, polySize = triangle)
        
        // Build stroke geometry with proper miter joins directly from path commands
        let strokeVertices = List<VectorPathVertex> ()
        let strokeIndices = List<uint32> ()
        let halfWidth = strokeThickness * 0.5f
        let fringeWidth = 0.01f // Anti-aliasing fringe width
        
        // Helper to add stroke segment indices
        let addStrokeSegment vertexBase currIdx nextIdx =
            let c0 = vertexBase + uint32 currIdx       // current inner edge 1
            let c1 = vertexBase + uint32 currIdx + 1u  // current inner edge 2
            let c2 = vertexBase + uint32 currIdx + 2u  // current outer edge 1
            let c3 = vertexBase + uint32 currIdx + 3u  // current outer edge 2
            let n0 = vertexBase + uint32 nextIdx       // next inner edge 1
            let n1 = vertexBase + uint32 nextIdx + 1u  // next inner edge 2
            let n2 = vertexBase + uint32 nextIdx + 2u  // next outer edge 1
            let n3 = vertexBase + uint32 nextIdx + 3u  // next outer edge 2
            
            // Solid center quad
            strokeIndices.Add c0
            strokeIndices.Add c1
            strokeIndices.Add n0
            strokeIndices.Add c1
            strokeIndices.Add n1
            strokeIndices.Add n0
            
            // Anti-aliasing fringe quad (top edge)
            strokeIndices.Add c0
            strokeIndices.Add n0
            strokeIndices.Add c2
            strokeIndices.Add n0
            strokeIndices.Add n2
            strokeIndices.Add c2
            
            // Anti-aliasing fringe quad (bottom edge)
            strokeIndices.Add c1
            strokeIndices.Add c3
            strokeIndices.Add n1
            strokeIndices.Add c3
            strokeIndices.Add n3
            strokeIndices.Add n1
        
        if stroke && strokeContours.Count > 0 then
            for contour in strokeContours do
                let contourCount = contour.Count
                
                if contourCount >= 2 then
                    let vertexBase = uint32 strokeVertices.Count
                    
                    // Determine if this is a closed contour (last point equals first point)
                    let isClosed = 
                        contourCount >= 3 && 
                        Vector2.DistanceSquared (contour.[0], contour.[contourCount - 1]) < epsilon
                    
                    // Generate vertices with proper miter joins and anti-aliasing fringe
                    let vertexCount = if isClosed then contourCount - 1 else contourCount
                    for i in 0 .. vertexCount - 1 do
                        let idxCurr = i
                        
                        let (pPrev, pCurr, pNext) =
                            if isClosed then
                                let idxPrev = if i = 0 then vertexCount - 1 else i - 1
                                let idxNext = if i = vertexCount - 1 then 0 else i + 1
                                (contour.[idxPrev], contour.[idxCurr], contour.[idxNext])
                            else
                                // For open contours, replace actual prev/next with straight perpendiculars at endpoints
                                if i = 0 then
                                    let pCurr = contour.[0]
                                    let pNext = contour.[1]
                                    let dir = Vector2.Normalize (pNext - pCurr)
                                    let pPrev = pCurr - dir * 0.1f // Virtual prev point for perpendicular (any closer would produce too wide stroke ends)
                                    (pPrev, pCurr, pNext)
                                elif i = vertexCount - 1 then
                                    let pPrev = contour.[i - 1]
                                    let pCurr = contour.[i]
                                    let dir = Vector2.Normalize (pCurr - pPrev)
                                    let pNext = pCurr + dir * 0.1f // Virtual next point for perpendicular (any closer would produce too wide stroke ends)
                                    (pPrev, pCurr, pNext)
                                else
                                    (contour.[i - 1], contour.[i], contour.[i + 1])
                        
                        // Compute miter offset with fringe for anti-aliasing
                        let (innerOffset1, innerOffset2, outerOffset1, outerOffset2) = 
                            computeMiterWithFringe pPrev pCurr pNext halfWidth fringeWidth
                        
                        // Add 4 vertices per point: 2 inner (solid) + 2 outer (transparent)
                        strokeVertices.Add { Position = pCurr + innerOffset1; Color = strokeColor }
                        strokeVertices.Add { Position = pCurr + innerOffset2; Color = strokeColor }
                        strokeVertices.Add { Position = pCurr + outerOffset1; Color = Color.Zero }
                        strokeVertices.Add { Position = pCurr + outerOffset2; Color = Color.Zero }
                    
                    // Generate triangle indices connecting the stroke segments
                    let segmentCount = if isClosed then vertexCount else vertexCount - 1
                    for i in 0 .. segmentCount - 1 do
                        let nextIdx = if isClosed && i = segmentCount - 1 then 0 else i + 1
                        addStrokeSegment vertexBase (i * 4) (nextIdx * 4)
        
        // Combine fill and stroke geometry
        let fillVertexCount = fillTess.VertexCount
        let totalVertexCount = fillVertexCount + strokeVertices.Count
        let vertices = Array.init totalVertexCount (fun i ->
            if i < fillVertexCount then
                { Position = fromTess &fillTess.Vertices.[i]; Color = fillColor }
            else
                strokeVertices.[i - fillVertexCount])
        
        let fillIndexCount = fillTess.ElementCount * triangle
        let totalIndexCount = fillIndexCount + strokeIndices.Count
        let elements = Array.init totalIndexCount (fun i ->
            if i < fillIndexCount then
                uint32 fillTess.Elements.[i]
            else
                let strokeIdx = i - fillIndexCount
                strokeIndices.[strokeIdx] + uint32 fillVertexCount)
        
        (vertices, elements)

    /// Create pipeline for vector path rendering.
    let createVectorPathPipeline vkc =

        // Create uniform buffer for model-view-projection matrix
        let uniformBufferSize = sizeof<single> * 16
        let modelViewProjectionUniform = Buffer.Buffer.create uniformBufferSize Buffer.Uniform vkc
        
        // Create the vertex and index buffers at init; size doesn't particularly matter here (VkBuffer will re-allocate itself with a larger size
        // if necessary, when Buffer.Buffer.uploadArray is called). just guess the likely maximum
        let size = 1024
        let vertexBuffer = Buffer.Buffer.create (size * sizeof<VectorPathVertex>) (Buffer.Vertex true) vkc
        let indexBuffer = Buffer.Buffer.create (size * sizeof<uint32>) (Buffer.Index true) vkc
        
        // Create pipeline
        let shaderPath = "Assets/Default/VectorPath"
        let vertexSize = sizeof<VectorPathVertex> // = sizeof<Vector2> + sizeof<Color> = 2 * sizeof<single> + 4 * sizeof<single>
        let pipeline =
            Pipeline.Pipeline.create
                shaderPath
                true
                true
                [|Pipeline.Transparent|]
                [|Pipeline.vertex 0 vertexSize
                    [|Pipeline.attribute 0 Hl.Single2 0
                      Pipeline.attribute 1 Hl.Single4 8|]|]
                [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage|]
                [|Pipeline.pushConstant 0 sizeof<int> Hl.VertexFragmentStage|]
                vkc.SwapFormat
                vkc
        
        (modelViewProjectionUniform, vertexBuffer, indexBuffer, pipeline)

    /// Draw a vector path.
    let drawVectorPath
        (drawIndex : int)
        (vertices : VectorPathVertex array, indices : uint32 array)
        (absolute : bool)
        (viewProjectionClipAbsolute : Matrix4x4 inref)
        (viewProjectionClipRelative : Matrix4x4 inref)
        (modelViewProjection : single array)
        (clipOpt : Box2 voption inref)
        (viewport : Viewport)
        (modelViewProjectionUniform : Buffer.Buffer, vertexBuffer : Buffer.Buffer, indexBuffer : Buffer.Buffer, pipeline : Pipeline.Pipeline)
        (vkc : Hl.VulkanContext) =
        
        // Upload data to the relevant buffers. This will create a bigger VkBuffer if necessary
        Buffer.Buffer.uploadArray drawIndex 0 modelViewProjection modelViewProjectionUniform vkc
        Buffer.Buffer.uploadArray drawIndex 0 vertices vertexBuffer vkc
        Buffer.Buffer.uploadArray drawIndex 0 indices indexBuffer vkc
        
        // Update descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 modelViewProjectionUniform pipeline vkc
        
        // Make viewport and scissor
        let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea
        match clipOpt with
        | ValueSome clip ->
            let viewProjection = if absolute then viewProjectionClipAbsolute else viewProjectionClipRelative
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
        
        // Only draw if scissor is valid
        if Hl.validateRect scissor then
            
            // Init render
            let cb = vkc.RenderCommandBuffer
            let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)
            
            // Bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.Transparent pipeline
            Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)
            
            // Set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // Bind vertex and index buffers
            let mutable vertexBuf = vertexBuffer.[drawIndex]
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuf, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, indexBuffer.[drawIndex], 0UL, VkIndexType.Uint32)
            
            // Bind descriptor set
            let mutable descriptorSet = pipeline.DescriptorSet
            Vulkan.vkCmdBindDescriptorSets
                (cb, VkPipelineBindPoint.Graphics,
                 pipeline.PipelineLayout, 0u,
                 1u, asPointer &descriptorSet,
                 0u, nullPtr)
            
            // Push draw index
            let mutable drawIdx = drawIndex
            Vulkan.vkCmdPushConstants
                (cb, pipeline.PipelineLayout,
                 Hl.VertexFragmentStage.VkShaderStageFlags,
                 0u, 4u, asVoidPtr &drawIdx)
            
            // Draw
            Vulkan.vkCmdDrawIndexed (cb, uint32 indices.Length, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
            
            // End render
            Vulkan.vkCmdEndRendering cb