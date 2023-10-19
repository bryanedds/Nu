// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

/// The endianness which indicates byte order in a raw asset.
type [<StructuralEquality; NoComparison; Struct>] Endianness =
    | LittleEndian
    | BigEndian

/// The format of a raw asset.
type [<StructuralEquality; NoComparison>] RawFormat =
    | RawUInt8
    | RawUInt16 of Endianness
    | RawUInt32 of Endianness
    | RawSingle of Endianness

/// The blend mode of a sprite.
[<Syntax
    ("Transparent Additive Overwrite", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Blend =
    | Transparent
    | Additive
    | Overwrite

/// Horizontal justification.
[<Syntax
    ("JustifyLeft JustifyRight JustifyCenter", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

/// Vertical justification.
[<Syntax
    ("JustifyTop JustifyMiddle JustifyBottom", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

/// Justification (such as for text alignement).
[<Syntax
    ("Justified Unjustified", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

/// A mutable particle type.
type [<NoEquality; NoComparison; Struct>] Particle =
    { mutable Transform : Transform
      mutable InsetOpt : Box2 ValueOption
      mutable Color : Color
      mutable Emission : Color
      mutable Flip : Flip }

/// A height map for 3d terrain constructed from a raw asset.
type [<StructuralEquality; NoComparison; Struct>] RawHeightMap =
    { Resolution : Vector2i
      RawFormat : RawFormat
      RawAsset : Raw AssetTag }

/// A height map for 3d terrain constructed from a dynamically-specifiable array.
type [<CustomEquality; NoComparison; Struct>] DynamicHeightMap =
    { Resolution : Vector2i
      Vertices : Vector3 array // NOTE: do not mutate these vertices in-place or else changes won't be detected.
      HashCode : int }
    override this.GetHashCode () =
        this.HashCode
    override this.Equals that =
        match that with
        | :? DynamicHeightMap as that ->
            this.Resolution = that.Resolution &&
            refEq this.Vertices that.Vertices // OPTIMIZATION: refEq for speed.
        | _ -> failwithumf ()
    static member make (resolution : Vector2i) (vertices : Vector3 array) =
        let requiredElements = resolution.X * resolution.Y
        if vertices.Length <> requiredElements then
            failwith ("Invalid vertices; array must contain " + string requiredElements + "elements.")
        { Resolution = resolution
          Vertices = vertices
          HashCode = hash vertices }

/// A height map for 3d terrain.
[<Syntax
    ("ImageHeightMap RawHeightMap DynamicHeightMap", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison>] HeightMap =
    | ImageHeightMap of Image AssetTag // only supports 8-bit depth on Red channel
    | RawHeightMap of RawHeightMap
    | DynamicHeightMap of DynamicHeightMap

/// An asset that is used for rendering.
type [<StructuralEquality; NoComparison>] RenderAsset =
    | TextureAsset of string * OpenGL.Texture.TextureMetadata * uint
    | FontAsset of string * int * nativeint
    | CubeMapAsset of OpenGL.CubeMap.CubeMapMemoKey * uint * (uint * uint) option ref
    | StaticModelAsset of bool * OpenGL.PhysicallyBased.PhysicallyBasedStaticModel
    | RawAsset of byte array

/// The type of rendering used on a surface.
type [<StructuralEquality; NoComparison; Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType of Subsort : single * Sort : single

/// A renderer tag interface.
type Renderer = interface end