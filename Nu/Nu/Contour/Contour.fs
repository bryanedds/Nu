// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open LibTessDotNet
open Prime
open Nu

/// Represents a contour command.
type [<Struct>] ContourCommand =
    | MoveTo of EndPoint : Vector2
    | LineTo of EndPoint : Vector2
    | QuadraticCurveTo of Control : Vector2 * EndPoint : Vector2
    | CubicCurveTo of Control1 : Vector2 * Control2 : Vector2 * EndPoint : Vector2
    | CloseContour

/// The winding rule to fill a contour.
type [<Struct>] ContourWinding =
    | EvenOdd // default winding from LibTessDotNet
    | NonZero
    | Positive
    | Negative
    | AbsGeqTwo

/// Describes how to fill a contour.
type [<Struct>] ContourFill =
    { Color : Color
      Winding : ContourWinding }
    static member val none = { Color = Color.Zero; Winding = ContourWinding.EvenOdd }
    static member ofColor color = { Color = color; Winding = ContourWinding.EvenOdd }
    static member ofColorWinding color winding = { Color = color; Winding = winding }
    
/// Represents the stroke of a contour.
type [<Struct>] ContourStroke =
    { Color : Color
      Thickness : single
      FringeWidth : single }
    static member val defaultFringeWidth = 0.01f
    static member val none = { Color = Color.Zero; Thickness = 0.0f; FringeWidth = 0.0f }
    static member aliased color thickness = { Color = color; Thickness = thickness; FringeWidth = 0.0f }
    static member antiAliased color thickness = { Color = color; Thickness = thickness; FringeWidth = ContourStroke.defaultFringeWidth }
    static member antiAliasedWithFringe color thickness fringeWidth = { Color = color; Thickness = thickness; FringeWidth = fringeWidth }

/// Represents a tessellated vertex for vector path rendering.
type [<Struct; StructLayout (LayoutKind.Sequential)>] ContourVertex =
    { Position : Vector2
      Color : Color }

/// Represents a tessellated contour.
type ContourTessellation =
    { Vertices : ContourVertex array
      Indices : uint32 array }

[<RequireQualifiedAccess>]
module ContourTessellation =
        
    let private fromTess (v : LibTessDotNet.ContourVertex inref) =
        Vector2 (v.Position.X, v.Position.Y)

    let private toTess (v : Vector2 inref) =
        LibTessDotNet.ContourVertex (Vec3 (v.X, v.Y, 0.0f))

    /// Helper to create bezier curve points.
    let private evaluateQuadraticBezier (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (t : single) =
        let t1 = 1.0f - t
        t1 * t1 * p0 + 2.0f * t1 * t * p1 + t * t * p2

    /// Helper to create cubic bezier curve points.
    let private evaluateCubicBezier (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (t : single) =
        let t1 = 1.0f - t
        t1 * t1 * t1 * p0 + 3.0f * t1 * t1 * t * p1 + 3.0f * t1 * t * t * p2 + t * t * t * p3
    
    /// Compute miter joint for two consecutive line segments with anti-aliasing fringe.
    let private computeMiterWithFringe (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (halfWidth : single) (fringeWidth : single) =

        // compute miter direction and length
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
        
        // inner edge (solid)
        let innerOffset1 = miterDir * actualMiterLength
        let innerOffset2 = miterDir * -actualMiterLength
        
        // outer edge (transparent for anti-aliasing)
        let fringeMiterLength = 
            let fringeHalfWidth = halfWidth + fringeWidth
            let fringeMiterLength = 
                let sinHalfAngle = Vector2.Cross (dir1, dir2) |> abs
                if sinHalfAngle > 0.01f then fringeHalfWidth / sinHalfAngle else fringeHalfWidth
            if fringeMiterLength <= miterLimit * 1.5f then fringeMiterLength else fringeHalfWidth
        let outerOffset1 = miterDir * fringeMiterLength
        let outerOffset2 = miterDir * -fringeMiterLength
        
        // fin
        (innerOffset1, innerOffset2, outerOffset1, outerOffset2)

    /// The empty tessellation.
    let empty =
        { Vertices = Array.empty; Indices = Array.empty }

    /// Make from contour commands into tessellated triangle vertices.
    let make (commands : ContourCommand seq) (fill : ContourFill) (stroke : ContourStroke) (scale: Vector2) =
        
        // prepare context variables for processing
        let fillTess = Tess ()
        let isFill = fill.Color.A > 0.0f
        let isStroke = stroke.Color.A > 0.0f && stroke.Thickness > 0.0f
        let epsilon = 0.0001f
        let mutable currentPoint = v2Zero
        let mutable pathStart = v2Zero // for contour closing
        let fillContourPoints = List<LibTessDotNet.ContourVertex> ()
        let strokeContours = List<List<Vector2>> ()
        let mutable currentStrokeContour = List<Vector2> ()
        
        /// Helper to save current contours to tessellator and stroke contour list, and clear them for the next contour.
        let saveCurrentContours () =
            if fillContourPoints.Count > 0 then
                fillTess.AddContour fillContourPoints
                fillContourPoints.Clear ()
            if currentStrokeContour.Count > 0 then
                strokeContours.Add currentStrokeContour
                currentStrokeContour <- List<Vector2> ()
        
        // process contour commands
        for command in commands do

            // process contour command
            match command with
            | MoveTo point ->
                let point = point * scale
                saveCurrentContours ()
                currentPoint <- point
                pathStart <- point
                if isFill then fillContourPoints.Add (toTess &point)
                if isStroke then currentStrokeContour.Add point
                
            | LineTo point ->
                let point = point * scale
                if isFill then fillContourPoints.Add (toTess &point)
                if isStroke then currentStrokeContour.Add point
                currentPoint <- point
                
            | QuadraticCurveTo (control, endpoint) ->
                let (control, endpoint) = (control * scale, endpoint * scale) 
                let steps = 20
                for i in 1 .. steps do
                    let t = single i / single steps
                    let point = evaluateQuadraticBezier currentPoint control endpoint t
                    if isFill then fillContourPoints.Add (toTess &point)
                    if isStroke then currentStrokeContour.Add point
                currentPoint <- endpoint
                
            | CubicCurveTo (control1, control2, endpoint) ->
                let (control1, control2, endpoint) = (control1 * scale, control2 * scale, endpoint * scale)
                let steps = 20
                for i in 1 .. steps do
                    let t = single i / single steps
                    let point = evaluateCubicBezier currentPoint control1 control2 endpoint t
                    if isFill then fillContourPoints.Add (toTess &point)     
                    if isStroke then currentStrokeContour.Add point
                currentPoint <- endpoint
                
            | CloseContour ->
                if Vector2.DistanceSquared (currentPoint, pathStart) >= epsilon then
                    if fillContourPoints.Count > 0 then fillContourPoints.Add (toTess &pathStart)
                    if currentStrokeContour.Count > 0 then currentStrokeContour.Add pathStart
                saveCurrentContours ()
        
        // tessellate any remaining contours
        saveCurrentContours ()
        
        // tessellate fill geometry
        let triangle = 3
        let tessWindingRule =
            match fill.Winding with
            | EvenOdd -> LibTessDotNet.WindingRule.EvenOdd
            | NonZero -> LibTessDotNet.WindingRule.NonZero
            | Positive -> LibTessDotNet.WindingRule.Positive
            | Negative -> LibTessDotNet.WindingRule.Negative
            | AbsGeqTwo -> LibTessDotNet.WindingRule.AbsGeqTwo
        fillTess.Tessellate (tessWindingRule, polySize = triangle)
        
        // build stroke geometry with proper miter joins directly from contour commands
        let strokeVertices = List<ContourVertex> ()
        let strokeIndices = List<uint32> ()
        let halfWidth = stroke.Thickness * 0.5f
        
        /// Helper to add stroke segment indices
        let addStrokeSegment vertexBase currIdx nextIdx =
            let c0 = vertexBase + uint32 currIdx        // current inner edge 1
            let c1 = vertexBase + uint32 currIdx + 1u   // current inner edge 2
            let c2 = vertexBase + uint32 currIdx + 2u   // current outer edge 1
            let c3 = vertexBase + uint32 currIdx + 3u   // current outer edge 2
            let n0 = vertexBase + uint32 nextIdx        // next inner edge 1
            let n1 = vertexBase + uint32 nextIdx + 1u   // next inner edge 2
            let n2 = vertexBase + uint32 nextIdx + 2u   // next outer edge 1
            let n3 = vertexBase + uint32 nextIdx + 3u   // next outer edge 2
            
            // solid center quad
            strokeIndices.Add c0
            strokeIndices.Add c1
            strokeIndices.Add n0
            strokeIndices.Add c1
            strokeIndices.Add n1
            strokeIndices.Add n0
            
            // anti-aliasing fringe quad (top edge)
            strokeIndices.Add c0
            strokeIndices.Add n0
            strokeIndices.Add c2
            strokeIndices.Add n0
            strokeIndices.Add n2
            strokeIndices.Add c2
            
            // anti-aliasing fringe quad (bottom edge)
            strokeIndices.Add c1
            strokeIndices.Add c3
            strokeIndices.Add n1
            strokeIndices.Add c3
            strokeIndices.Add n3
            strokeIndices.Add n1
        
        // generate stroke geometry with proper miter joins and anti-aliasing fringe
        if isStroke && strokeContours.Count > 0 then
            for contour in strokeContours do
                let contourCount = contour.Count
                if contourCount >= 2 then
                    let vertexBase = uint32 strokeVertices.Count
                    
                    // determine if this is a closed contour (last point equals first point)
                    let isClosed = 
                        contourCount >= 3 && 
                        Vector2.DistanceSquared (contour.[0], contour.[contourCount - 1]) < epsilon
                    
                    // generate vertices with proper miter joins and anti-aliasing fringe
                    let vertexCount = if isClosed then contourCount - 1 else contourCount
                    for i in 0 .. vertexCount - 1 do
                        let idxCurr = i
                        let (pPrev, pCurr, pNext) =
                            if isClosed then
                                let idxPrev = if i = 0 then vertexCount - 1 else i - 1
                                let idxNext = if i = vertexCount - 1 then 0 else i + 1
                                (contour.[idxPrev], contour.[idxCurr], contour.[idxNext])
                            else
                                // for open contours, replace actual prev/next with straight perpendiculars at endpoints
                                if i = 0 then
                                    let pCurr = contour.[0]
                                    let pNext = contour.[1]
                                    let dir = Vector2.Normalize (pNext - pCurr)
                                    let pPrev = pCurr - dir * 0.1f // virtual prev point for perpendicular (any closer would produce too wide stroke ends)
                                    (pPrev, pCurr, pNext)
                                elif i = vertexCount - 1 then
                                    let pPrev = contour.[i - 1]
                                    let pCurr = contour.[i]
                                    let dir = Vector2.Normalize (pCurr - pPrev)
                                    let pNext = pCurr + dir * 0.1f // virtual next point for perpendicular (any closer would produce too wide stroke ends)
                                    (pPrev, pCurr, pNext)
                                else
                                    (contour.[i - 1], contour.[i], contour.[i + 1])
                        
                        // compute miter offset with fringe for anti-aliasing
                        let (innerOffset1, innerOffset2, outerOffset1, outerOffset2) = 
                            computeMiterWithFringe pPrev pCurr pNext halfWidth stroke.FringeWidth
                        
                        // add 4 vertices per point: 2 inner (solid) + 2 outer (transparent)
                        strokeVertices.Add { Position = pCurr + innerOffset1; Color = stroke.Color }
                        strokeVertices.Add { Position = pCurr + innerOffset2; Color = stroke.Color }
                        strokeVertices.Add { Position = pCurr + outerOffset1; Color = Color.Zero }
                        strokeVertices.Add { Position = pCurr + outerOffset2; Color = Color.Zero }
                    
                    // generate triangle indices connecting the stroke segments
                    let segmentCount = if isClosed then vertexCount else vertexCount - 1
                    for i in 0 .. segmentCount - 1 do
                        let nextIdx = if isClosed && i = segmentCount - 1 then 0 else i + 1
                        addStrokeSegment vertexBase (i * 4) (nextIdx * 4)
        
        // combine fill and stroke geometry
        let fillVertexCount = fillTess.VertexCount
        let totalVertexCount = fillVertexCount + strokeVertices.Count
        let vertices = Array.init totalVertexCount (fun i ->
            if i < fillVertexCount
            then { Position = fromTess &fillTess.Vertices.[i]; Color = fill.Color }
            else strokeVertices.[i - fillVertexCount])
        
        // combine fill and stroke geometry at offset (stroke indices need to be shifted by fill vertex count)
        let fillIndexCount = fillTess.ElementCount * triangle
        let totalIndexCount = fillIndexCount + strokeIndices.Count
        let indices = Array.init totalIndexCount (fun i ->
            if i < fillIndexCount
            then uint32 fillTess.Elements.[i]
            else strokeIndices.[i - fillIndexCount] + uint32 fillVertexCount)
        
        // fin
        { Vertices = vertices; Indices = indices }