// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

// TODO: P0: get rid of excess frame-based allocation here, such as potentially by using memoization.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Contour =

    // The epsilon used by the reference Slug implementation for nearly-linear curves.
    let [<Literal>] private kSlugEpsilon = 1.0f / 65536.0f

    // Epsilon used for band overlap, in em-space (reference recommends 1/1024).
    let [<Literal>] private kBandOverlap = 1.0f / 1024.0f

    // Default number of bands when bounds are degenerate.
    let [<Literal>] private kDefaultBands = 1

    // Maximum number of subdivision iterations for adaptive cubic-to-quadratic conversion.
    let [<Literal>] private kMaxCubicSubdivisions = 8

    /// Compute the maximum x-coordinate among the three control points of a curve.
    let private curveMaxX (c : ContourCurve) =
        max (max c.P1X c.P2X) c.P3X

    /// Compute the maximum y-coordinate among the three control points of a curve.
    let private curveMaxY (c : ContourCurve) =
        max (max c.P1Y c.P2Y) c.P3Y

    /// Determine whether a quadratic Bézier curve is a straight horizontal line
    /// (all three control points have the same y). Such curves never contribute
    /// to horizontal ray winding.
    let private isStraightHorizontal (c : ContourCurve) =
        c.P1Y = c.P2Y && c.P2Y = c.P3Y

    /// Determine whether a quadratic Bézier curve is a straight vertical line
    /// (all three control points have the same x). Such curves never contribute
    /// to vertical ray winding.
    let private isStraightVertical (c : ContourCurve) =
        c.P1X = c.P2X && c.P2X = c.P3X

    /// Pack a ContourCurve into two float4 values for GPU consumption:
    ///   field0 = (p1.x, p1.y, p2.x, p2.y)
    ///   field1 = (p3.x, p3.y, 0, 0)
    let private packCurveGPU (curve : ContourCurve) =
        struct (Vector4 (curve.P1X, curve.P1Y, curve.P2X, curve.P2Y),
                Vector4 (curve.P3X, curve.P3Y, 0.0f, 0.0f))

    /// Pack an array of ContourCurves into a flat float4 array for GPU upload.
    let packCurvesGPU (curves : ContourCurve array) =
        let gpuData = Array.zeroCreate<Vector4> (curves.Length * 2)
        for i in 0 .. dec curves.Length do
            let struct (f0, f1) = packCurveGPU curves[i]
            gpuData[i * 2] <- f0
            gpuData[i * 2 + 1] <- f1
        gpuData

    // ---- Adaptive cubic-to-quadratic conversion ----

    /// Fit one quadratic Bézier to a cubic while preserving both endpoints.
    /// This averages the control points implied by degree elevation at each end.
    let private fitQuadraticToCubic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) =
        let control = (3.0f * p1 + 3.0f * p2 - p0 - p3) * 0.25f
        { P1X = p0.X; P1Y = p0.Y
          P2X = control.X; P2Y = control.Y
          P3X = p3.X; P3Y = p3.Y }

    /// Return a conservative, global error bound for the fitted quadratic.
    /// Degree-elevating the candidate produces a cubic whose difference from the source
    /// has Bézier controls {0, p1-q1, p2-q2, 0}; the curve remains in their convex hull.
    let private cubicToQuadError (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (candidate : ContourCurve) =
        let control = v2 candidate.P2X candidate.P2Y
        let q1 = (p0 + 2.0f * control) / 3.0f
        let q2 = (p3 + 2.0f * control) / 3.0f
        max (Vector2.Distance (p1, q1)) (Vector2.Distance (p2, q2))

    /// Recursively subdivide a cubic Bézier until the fitted quadratic error is below tolerance.
    let rec private subdivideCubic (tolerance : single) (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) (depth : int) (quads : List<ContourCurve>) =
        let candidate = fitQuadraticToCubic p0 p1 p2 p3
        if depth >= kMaxCubicSubdivisions || cubicToQuadError p0 p1 p2 p3 candidate <= tolerance then
            quads.Add candidate
        else
            // Subdivide at t = 0.5 using de Casteljau construction.
            let p01 = (p0 + p1) * 0.5f
            let p12 = (p1 + p2) * 0.5f
            let p23 = (p2 + p3) * 0.5f
            let p012 = (p01 + p12) * 0.5f
            let p123 = (p12 + p23) * 0.5f
            let mid = (p012 + p123) * 0.5f
            subdivideCubic tolerance p0 p01 p012 mid (depth + 1) quads
            subdivideCubic tolerance mid p123 p23 p3 (depth + 1) quads

    /// Convert a single cubic Bézier to quadratic approximations using adaptive subdivision.
    let private cubicToQuadratics (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (p3 : Vector2) =
        let quads = List<ContourCurve> ()
        subdivideCubic 0.001f p0 p1 p2 p3 0 quads
        quads.ToArray ()

    // ---- Contour decomposition ----

    /// Convert contour commands to quadratic Bézier curves.
    let private decomposeToCurves (commands : ContourCommand seq) =

        let curves = List<ContourCurve> ()
        let mutable currentPoint = v2Zero
        let mutable contourStart = v2Zero
        let mutable hasSubpath = false

        // SVG-style fills implicitly close every subpath at a subsequent MoveTo and at end of path.
        let closeSubpath () =
            if hasSubpath then
                if Vector2.DistanceSquared (currentPoint, contourStart) > 0.0001f then
                    curves.Add { P1X = currentPoint.X; P1Y = currentPoint.Y
                                 P2X = contourStart.X; P2Y = contourStart.Y
                                 P3X = contourStart.X; P3Y = contourStart.Y }
                currentPoint <- contourStart

        let beginImplicitSubpath () =
            if not hasSubpath then
                contourStart <- currentPoint
                hasSubpath <- true

        for command in commands do
            match command with
            | MoveTo point ->
                closeSubpath ()
                currentPoint <- point
                contourStart <- point
                hasSubpath <- true

            | LineTo point ->
                beginImplicitSubpath ()
                // Line encoded as {p1, p2, p2} per Slug reference.
                curves.Add { P1X = currentPoint.X; P1Y = currentPoint.Y
                             P2X = point.X; P2Y = point.Y
                             P3X = point.X; P3Y = point.Y }
                currentPoint <- point

            | QuadraticCurveTo (control, endpoint) ->
                beginImplicitSubpath ()
                curves.Add { P1X = currentPoint.X; P1Y = currentPoint.Y
                             P2X = control.X; P2Y = control.Y
                             P3X = endpoint.X; P3Y = endpoint.Y }
                currentPoint <- endpoint

            | CubicCurveTo (control1, control2, endpoint) ->
                beginImplicitSubpath ()
                let quads = cubicToQuadratics currentPoint control1 control2 endpoint
                curves.AddRange quads
                currentPoint <- endpoint

            | CloseContour ->
                closeSubpath ()
                hasSubpath <- false

        closeSubpath ()

        curves.ToArray ()

    // ---- Bounding box ----

    /// Compute the bounding box of a set of curves.
    let private computeBounds (curves : ContourCurve array) =
        if curves.Length = 0 then Box2 (v2Zero, v2One)
        else
            let mutable minX = Single.MaxValue
            let mutable minY = Single.MaxValue
            let mutable maxX = Single.MinValue
            let mutable maxY = Single.MinValue
            for c in curves do
                for p in [| v2 c.P1X c.P1Y; v2 c.P2X c.P2Y; v2 c.P3X c.P3Y |] do
                    if p.X < minX then minX <- p.X
                    if p.Y < minY then minY <- p.Y
                    if p.X > maxX then maxX <- p.X
                    if p.Y > maxY then maxY <- p.Y
            Box2 (v2 minX minY, v2 (maxX - minX) (maxY - minY))

    // ---- Band building ----

    /// Build horizontal and vertical band data for Slug rendering.
    /// Returns (bandEntries, bandCurveIndices, numHBands, numVBands, bandTransform).
    let private buildBands (curves : ContourCurve array) (bounds : Box2) =

        if Array.isEmpty curves then
            (Array.empty, Array.empty, 0, 0, Vector4.Zero)
        else
            let emEpsilon = kBandOverlap

            // ---- Horizontal bands (split y-range) ----
            let hBandThickness =
                let ideal = bounds.Size.Y / 8.0f
                if ideal < emEpsilon then emEpsilon else ideal
            let nHBands = max 1 (int (bounds.Size.Y / hBandThickness))

            // For each curve, determine which horizontal bands it belongs to.
            let hAssignments = List<int * int>() // (bandIndex, curveIndex)
            for ci = 0 to curves.Length - 1 do
                let c = curves.[ci]
                if not (isStraightHorizontal c) then
                    let minY = min (min c.P1Y c.P2Y) c.P3Y
                    let maxY = max (max c.P1Y c.P2Y) c.P3Y
                    // With epsilon overlap so pixels near band boundaries get both bands.
                    let firstBand = max 0 (int ((minY - emEpsilon - bounds.Min.Y) / hBandThickness))
                    let lastBand = min (nHBands - 1) (int ((maxY + emEpsilon - bounds.Min.Y) / hBandThickness))
                    for b = firstBand to lastBand do
                        hAssignments.Add (b, ci)

            // Group by band index and sort each group by descending max x.
            let hGroups =
                hAssignments
                |> Seq.groupBy fst
                |> Seq.map (fun (band, items) ->
                    let indices = items |> Seq.map snd |> Seq.toArray
                    let sorted = indices |> Array.sortByDescending (fun ci -> curveMaxX curves.[ci])
                    (band, sorted))
                |> Seq.sortWith (fun (band, _) (band2, _) -> band.CompareTo band2)
                |> Seq.toArray

            // Build flat arrays.
            let hEntries = List<ContourBandEntry> ()
            let hIndices = List<uint32> ()
            for (band, indices) in hGroups do
                // Pad with empty entries for bands that have no curves (they still need an entry slot).
                while hEntries.Count < band do
                    hEntries.Add { CurveCount = 0u; CurveOffset = uint32 hIndices.Count }
                let offset = uint32 hIndices.Count
                for ci in indices do
                    hIndices.Add (uint32 ci)
                hEntries.Add { CurveCount = uint32 indices.Length; CurveOffset = offset }

            // Pad remaining bands.
            while hEntries.Count < nHBands do
                hEntries.Add { CurveCount = 0u; CurveOffset = uint32 hIndices.Count }

            // ---- Vertical bands (split x-range) ----
            let vBandThickness =
                let ideal = bounds.Size.X / 8.0f
                if ideal < emEpsilon then emEpsilon else ideal
            let nVBands = max 1 (int (bounds.Size.X / vBandThickness))

            let vAssignments = List<int * int>()
            for ci = 0 to curves.Length - 1 do
                let c = curves.[ci]
                if not (isStraightVertical c) then
                    let minX = min (min c.P1X c.P2X) c.P3X
                    let maxX = max (max c.P1X c.P2X) c.P3X
                    let firstBand = max 0 (int ((minX - emEpsilon - bounds.Min.X) / vBandThickness))
                    let lastBand = min (nVBands - 1) (int ((maxX + emEpsilon - bounds.Min.X) / vBandThickness))
                    for b = firstBand to lastBand do
                        vAssignments.Add (b, ci)

            let vGroups =
                vAssignments
                |> Seq.groupBy fst
                |> Seq.map (fun (band, items) ->
                    let indices = items |> Seq.map snd |> Seq.toArray
                    let sorted = indices |> Array.sortByDescending (fun ci -> curveMaxY curves.[ci])
                    (band, sorted))
                |> Seq.sortBy fst
                |> Seq.toArray

            let vEntries = List<ContourBandEntry> ()
            let vIndices = List<uint32> ()
            for (band, indices) in vGroups do
                while vEntries.Count < band do
                    vEntries.Add { CurveCount = 0u; CurveOffset = uint32 vIndices.Count }
                let offset = uint32 vIndices.Count
                for ci in indices do
                    vIndices.Add (uint32 ci)
                vEntries.Add { CurveCount = uint32 indices.Length; CurveOffset = offset }
            while vEntries.Count < nVBands do
                vEntries.Add { CurveCount = 0u; CurveOffset = uint32 vIndices.Count }

            // ---- Band transform ----
            // Maps renderCoord -> band index: bandIndex = renderCoord * scale + offset
            let hbScale =
                if bounds.Size.Y > 0.0f then single nHBands / bounds.Size.Y else 1.0f
            let hbOffset =
                -bounds.Min.Y * hbScale
            let vbScale =
                if bounds.Size.X > 0.0f then single nVBands / bounds.Size.X else 1.0f
            let vbOffset =
                -bounds.Min.X * vbScale

            // Offset vertical-band CurveOffsets so they point into the
            // concatenated flat index array (not just the vIndices section).
            let hIndicesCount = hIndices.Count
            let vEntriesFixed =
                vEntries.ToArray ()
                |> Array.map (fun entry ->
                    if entry.CurveCount > 0u
                    then { entry with CurveOffset = entry.CurveOffset + uint32 hIndicesCount }
                    else entry)

            // Pack entries: first all H-band entries, then all V-band entries.
            let allEntries = Array.append (hEntries.ToArray ()) (vEntriesFixed)
            let allIndices = Array.append (hIndices.ToArray ()) (vIndices.ToArray ())

            (allEntries, allIndices, nHBands, nVBands,
             Vector4 (vbScale, hbScale, vbOffset, hbOffset))

    // ---- Curve scaling ----

    /// Scale a curve's control points by the given factor.
    let private scaleCurve (scale : Vector2) (curve : ContourCurve) =
        { P1X = curve.P1X * scale.X; P1Y = curve.P1Y * scale.Y
          P2X = curve.P2X * scale.X; P2Y = curve.P2Y * scale.Y
          P3X = curve.P3X * scale.X; P3Y = curve.P3Y * scale.Y }

    // ---- Adaptive polyline sampling for stroke offset ----

    /// Tolerance for chord error when flattening curves to polylines (in em-space).
    let [<Literal>] private kStrokeFlatness = 0.05f

    /// Sample a quadratic Bézier with adaptive subdivision so chord error < tolerance.
    let private sampleQuadratic (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) (tolerance : single) =
        let points = List<Vector2> ()
        let rec sub (p0 : Vector2) (p1 : Vector2) (p2 : Vector2) depth =
            if depth > 8 then
                points.Add p2
            else
                // Midpoint of the quadratic
                let mid = (p0 + 2.0f * p1 + p2) * 0.25f
                // Distance from midpoint to chord
                let chord = p2 - p0
                let chordLen = chord.Length ()
                let err =
                    if chordLen > 0.0001f then
                        let perp = v2 -chord.Y chord.X / chordLen
                        abs (Vector2.Dot (mid - p0, perp))
                    else 0.0f
                if err <= tolerance then
                    points.Add p2
                else
                    // Split at t = 0.5
                    let p01 = (p0 + p1) * 0.5f
                    let p12 = (p1 + p2) * 0.5f
                    let p012 = (p01 + p12) * 0.5f
                    sub p0 p01 p012 (depth + 1)
                    sub p012 p12 p2 (depth + 1)
        sub p0 p1 p2 0
        points.ToArray ()

    // ---- Command scaling ----

    /// Scale all coordinates in a contour command by the given factor.
    let private scaleCommand (scale : Vector2) (cmd : ContourCommand) =
        match cmd with
        | MoveTo pos -> MoveTo (pos * scale)
        | LineTo pos -> LineTo (pos * scale)
        | QuadraticCurveTo (ctl, endp) -> QuadraticCurveTo (ctl * scale, endp * scale)
        | CubicCurveTo (ctl1, ctl2, endp) -> CubicCurveTo (ctl1 * scale, ctl2 * scale, endp * scale)
        | CloseContour -> CloseContour

    // ---- Subpath parsing ----

    /// Parse a command sequence into separate closed subpaths.
    /// Each subpath begins with a MoveTo and ends when another MoveTo or end-of-sequence is reached.
    let private parseSubpaths (commands : ContourCommand seq) =
        let subpaths = List<ContourCommand list> ()
        let mutable current = []
        let mutable hasContent = false
        let flush () =
            if hasContent && not (List.isEmpty current) then
                subpaths.Add (List.rev current)
                current <- []
                hasContent <- false
        for cmd in commands do
            match cmd with
            | MoveTo _ ->
                flush ()
                current <- cmd :: current
                hasContent <- true
            | _ ->
                if hasContent then
                    current <- cmd :: current
        flush ()
        subpaths.ToArray ()

    // ---- Subpath flattening ----

    /// Flatten a subpath into a polyline using adaptive sampling.
    /// Closed subpaths omit the duplicated final start point because the offsetter closes them explicitly.
    let private flattenSubpath (commands : ContourCommand list) (tolerance : single) (closed : bool) =
        let points = List<Vector2> ()
        let mutable current = v2Zero
        let mutable start = v2Zero
        let mutable first = true
        for cmd in commands do
            match cmd with
            | MoveTo pt ->
                current <- pt
                start <- pt
                points.Add pt
                first <- false
            | LineTo pt ->
                if first then
                    start <- current
                    points.Add current
                    first <- false
                current <- pt
                points.Add pt
            | QuadraticCurveTo (ctrl, endp) ->
                if first then
                    start <- current
                    points.Add current
                    first <- false
                let samples = sampleQuadratic current ctrl endp tolerance
                points.AddRange samples
                current <- endp
            | CubicCurveTo (ctrl1, ctrl2, endp) ->
                if first then
                    start <- current
                    points.Add current
                    first <- false
                // Convert cubic to quadratics first, then sample each quadratic adaptively.
                let quads = cubicToQuadratics current ctrl1 ctrl2 endp
                for q in quads do
                    let p1 = v2 q.P1X q.P1Y
                    let p2 = v2 q.P2X q.P2Y
                    let p3 = v2 q.P3X q.P3Y
                    let samples = sampleQuadratic p1 p2 p3 tolerance
                    points.AddRange samples
                current <- endp
            | CloseContour ->
                current <- start

        // Remove an explicit closing endpoint; the closed offset path supplies its own closing edge.
        if closed && points.Count > 1 && Vector2.DistanceSquared (points.[points.Count - 1], start) <= 0.0001f then
            points.RemoveAt (points.Count - 1)
        points.ToArray ()

    // ---- Stroke offset helpers ----

    /// Offset an open or closed polyline by halfWidth, producing left and right rails
    /// with miter joints and square caps for open paths.
    let private offsetPolyline (points : Vector2 array) (halfWidth : single) (closed : bool) =
        let n = points.Length
        if n < 2 then (Array.empty, Array.empty)
        else
            let edgeCount = if closed then n else n - 1
            let normals = Array.zeroCreate<Vector2> edgeCount
            for i in 0 .. edgeCount - 1 do
                let next = if i = n - 1 then 0 else i + 1
                let dir = points.[next] - points.[i]
                let len = dir.Length ()
                if len > 0.0001f then
                    let dirN = dir / len
                    normals.[i] <- v2 -dirN.Y dirN.X
                else
                    normals.[i] <- v2Zero

            let miterOffset (previousNormal : Vector2) nextNormal =
                let sum = previousNormal + nextNormal
                let lenSq = sum.LengthSquared ()
                let miterDir =
                    if lenSq > 0.001f then sum / sqrt lenSq
                    else nextNormal
                let dot = abs (Vector2.Dot (nextNormal, miterDir))
                let miterLen = if dot > 0.001f then halfWidth / dot else halfWidth
                miterDir * min miterLen (halfWidth * 3.0f)

            let left = Array.zeroCreate<Vector2> n
            let right = Array.zeroCreate<Vector2> n
            for i in 0 .. n - 1 do
                let previousNormal, nextNormal =
                    if not closed && i = 0 then normals.[0], normals.[0]
                    elif not closed && i = n - 1 then normals.[edgeCount - 1], normals.[edgeCount - 1]
                    else
                        let previous = if i = 0 then normals.[edgeCount - 1] else normals.[i - 1]
                        previous, normals.[i]
                let offset = miterOffset previousNormal nextNormal
                left.[i] <- points.[i] + offset
                right.[i] <- points.[i] - offset
            left, right

    /// Convert offset rails into a filled stroke contour.
    let private polygonToCommands (leftRail : Vector2 array) (rightRail : Vector2 array) (closed : bool) =
        let cmds = List<ContourCommand> ()
        if leftRail.Length > 0 then
            cmds.Add (MoveTo leftRail[0])
            for i in 1 .. leftRail.Length - 1 do
                cmds.Add (LineTo leftRail[i])
            if closed then
                // Close the first rail before traversing the opposite rail in reverse.
                // The two rail loops have opposite winding and the connecting edge is retraced.
                cmds.Add (LineTo leftRail[0])
                cmds.Add (LineTo rightRail[0])
            for i in rightRail.Length - 1 .. -1 .. 0 do
                cmds.Add (LineTo rightRail[i])
            cmds.Add CloseContour
        cmds

    // ---- Stroke outline generation ----

    /// Generate a centered stroke outline from contour commands.
    /// Supports compound contours with multiple closed subpaths:
    /// each subpath is flattened independently, offset left and right,
    /// and converted into a closed ring.  All rings are concatenated
    /// into a single command sequence that can be fed to
    /// makeFillSlugData with NonZero winding.
    ///
    /// Join style: miter with clamped miter length (3× halfWidth).
    /// Cap style: only closed subpaths are supported; open contours
    /// will produce artifacts.  Round caps/joins are future work.
    let private buildStrokeOutline (commands : ContourCommand seq) (thickness : single) =
        if thickness <= 0.0f then Seq.empty
        else
            let halfWidth = thickness * 0.5f
            let subpaths = parseSubpaths commands
            if Array.isEmpty subpaths then Seq.empty
            else
                let allCmds = List<ContourCommand> ()
                let tolerance = kStrokeFlatness
                for sub in subpaths do
                    let isClosed = sub |> List.exists (function CloseContour -> true | _ -> false)
                    let polyline = flattenSubpath sub tolerance isClosed
                    if polyline.Length >= 2 then
                        let leftRail, rightRail = offsetPolyline polyline halfWidth isClosed
                        if leftRail.Length >= 2 then
                            let cmds = polygonToCommands leftRail rightRail isClosed
                            allCmds.AddRange cmds
                allCmds :> ContourCommand seq

    // ---- Main entry points ----

    /// Convert contour commands into pure Slug geometry (curves + bands only).
    /// No paint or stroke data.
    let private makeFillSlugData
        (fillWinding : ContourWinding)
        (commands : ContourCommand seq)
        (scale : Vector2) =

        let curves = decomposeToCurves commands
        let curves = Array.map (scaleCurve scale) curves
        let bounds = computeBounds curves
        let (bandEntries, bandCurveIndices, nHBands, nVBands, bandTransform) =
            buildBands curves bounds

        { Curves = curves
          BandEntries = bandEntries
          BandCurveIndices = bandCurveIndices
          HBands = nHBands
          VBands = nVBands
          BandTransform = bandTransform
          LocalBounds = bounds
          FillWinding = fillWinding }

    /// Prepare a contour for two-pass rendering with optional fill and optional stroke geometries.
    let make
        (fill : ContourFill)
        (stroke : ContourStroke)
        (commands : ContourCommand seq)
        (scale : Vector2) =

        let fillGeomOpt =
            if fill.Color.A > 0.0f then
                let geom = makeFillSlugData fill.Winding commands scale
                ValueSome geom
            else ValueNone

        let strokeGeomOpt =
            if stroke.Thickness > 0.0f && stroke.Color.A > 0.0f then
                // Scale commands to world space so the stroke offset is uniform in virtual pixels
                let scaledCommands = commands |> Seq.map (scaleCommand scale)
                let strokeCommands = buildStrokeOutline scaledCommands stroke.Thickness
                if Seq.isEmpty strokeCommands then ValueNone
                else
                    let geom = makeFillSlugData NonZero strokeCommands v2One
                    ValueSome geom
            else ValueNone

        { FillGeometryOpt = fillGeomOpt
          FillColor = fill.Color
          StrokeGeometryOpt = strokeGeomOpt
          StrokeColor = stroke.Color }
