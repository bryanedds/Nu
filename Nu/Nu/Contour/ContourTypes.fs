// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

// ============================================================
//  Slug-based GPU Contour Pipeline — Semantic Model
// ============================================================
//  This module describes the data types for Nu's GPU-accelerated
//  contour renderer, which is a port of Eric Lengyel's Slug
//  algorithm (JCGT 2017).  The pipeline works as follows:
//
//    1. ContourCommand[]  —  user-facing shape description
//       (move/line/quad/cubic/close).  Cubics are adaptively
//       split into quadratics during packing.
//
//    2. Contour.decomposeToCurves  —  converts commands
//       to ContourCurve[] (quadratic Béziers).  Lines are
//       encoded as {p1, p2, p2} per the Slug reference.
//
//    3. Contour.buildBands  —  groups curves into
//       horizontal and vertical bands, sorts each band by
//       descending max-x (H) / max-y (V), and produces a flat
//       index array.  Straight-horizontal curves are excluded
//       from H-bands; straight-vertical from V-bands.
//
//    4. ContourSlugGeometry  —  pure GPU-ready data for one draw pass.
//       Contains only curve/band data; paint is separate so the
//       same geometry can be reused with different colors.
//
//    5. Contour  —  up to two ContourSlugGeometry values:
//       an optional fill pass and an optional stroke pass.
//       Stroke is rendered as a filled Slug contour built from
//       a CPU-generated offset outline (miter joints, closed
//       ring → NonZero fill).
//
//    6. drawContour  —  uploads each ContourSlugGeometry +
//       color to SSBOs, draws a full-screen quad mapped to the
//       bounding box, and computes analytic coverage in the
//       fragment shader via horizontal + vertical ray casting.
//
//  Fill rules: EvenOdd and NonZero only.  The fragment shader
//  distinguishes them via a single flag bit (bit 0 of flags).
//
//  Reference: https://github.com/EricLengyel/Slug
// ============================================================

/// Represents a contour command.
type [<Struct>] ContourCommand =
    | MoveTo of EndPoint : Vector2
    | LineTo of EndPoint : Vector2
    | QuadraticCurveTo of Control : Vector2 * EndPoint : Vector2
    | CubicCurveTo of Control1 : Vector2 * Control2 : Vector2 * EndPoint : Vector2
    | CloseContour

/// The winding rule to fill a contour.
type [<Struct>] ContourWinding =
    | EvenOdd
    | NonZero

/// Describes how to fill a contour.
type [<Struct>] ContourFill =
    { Color : Color
      Winding : ContourWinding }
    static member val none = { Color = Color.Zero; Winding = ContourWinding.NonZero }
    static member ofColor color = { Color = color; Winding = ContourWinding.NonZero }
    static member ofColorWinding color winding = { Color = color; Winding = winding }

/// Represents the stroke of a contour.
type [<Struct>] ContourStroke =
    { Color : Color
      Thickness : single }
    static member val none = { Color = Color.Zero; Thickness = 0.0f }
    static member ofColorThickness color thickness = { Color = color; Thickness = thickness }

/// A quadratic Bézier curve for analytic contour rendering.
type [<Struct; StructLayout (LayoutKind.Sequential)>] ContourCurve =
    { P1X : single; P1Y : single; P2X : single; P2Y : single; P3X : single; P3Y : single }

/// A band entry: number of curves in the band and offset into the band-curve index list.
type [<Struct; StructLayout (LayoutKind.Sequential)>] ContourBandEntry =
    { CurveCount : uint32
      CurveOffset : uint32 }

/// Pure Slug geometry for one GPU draw (fill or stroke).
/// Contains only curve and band data — no paint/color fields.
/// The fill-rule flag distinguishes EvenOdd from NonZero winding.
type [<Struct>] ContourSlugGeometry =
    { Curves : ContourCurve array
      /// Per-band curve indices, packed as a flat uint array.
      /// First H-band entries, then V-band entries.
      BandEntries : ContourBandEntry array
      /// For each band entry, the list of curve indices in that band.
      BandCurveIndices : uint32 array
      /// Number of horizontal bands.
      HBands : int
      /// Number of vertical bands.
      VBands : int
      /// Band transform: (scaleX, scaleY, offsetX, offsetY) to map renderCoord -> band index.
      BandTransform : Vector4
      /// Local bounding box of the geometry.
      LocalBounds : Box2
      /// Winding rule for the geometry.
      FillWinding : ContourWinding }

/// Prepared contour for rendering: up to two separate Slug passes.
type Contour =
    { FillGeometryOpt : ContourSlugGeometry voption
      FillColor : Color
      StrokeGeometryOpt : ContourSlugGeometry voption
      StrokeColor : Color }
    static member val empty =
        { FillGeometryOpt = ValueNone
          FillColor = Color.Zero
          StrokeGeometryOpt = ValueNone
          StrokeColor = Color.Zero }