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

/// A height map for 3d terrain.
[<Syntax
    ("ImageHeightMap RawHeightMap", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison>] HeightMap =
    | ImageHeightMap of Image AssetTag // only supports 8-bit depth on Red channel
    | RawHeightMap of RawHeightMap

/// Determines how an animation is played.
type [<StructuralEquality; NoComparison>] Playback =
    | Once
    | Loop
    | Bounce

/// Describes an animation.
type [<NoEquality; NoComparison>] Animation =
    { StartTime : GameTime
      LifeTimeOpt : GameTime option
      Name : string
      Playback : Playback
      Rate : single
      Weight : single
      BonesOpt : string array option }

/// An asset that is used for rendering.
type RenderAsset =
    | TextureAsset of FilePath : string * TextureMetadata : OpenGL.Texture.TextureMetadata * Texture : uint
    | FontAsset of FilePath : string * PointSize : int * Font : nativeint
    | CubeMapAsset of FilePaths : OpenGL.CubeMap.CubeMapMemoKey * CubeMap : uint * IrradianceAndEnvironmentMapOptRef : (uint * uint) option ref
    | StaticModelAsset of UserDefined : bool * Model : OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelAsset of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | RawAsset of RawAsset : byte array

/// The type of rendering used on a surface.
type [<StructuralEquality; NoComparison; Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType of Subsort : single * Sort : single

/// A renderer tag interface.
type Renderer = interface end