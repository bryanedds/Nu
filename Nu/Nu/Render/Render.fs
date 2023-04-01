// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open TiledSharp
open Prime
open Nu

/// An image. Currently just used as a phantom type.
type Image = private { __ : unit }

/// A font. Currently just used as a phantom type.
type Font = private { __ : unit }

/// A tile map. Currently just used as a phantom type.
type TileMap = private { __ : unit }

/// A static model. Currently just used as a phantom type.
type CubeMap = private { __ : unit }

/// An asset that is used for rendering.
type RenderAsset =
    | TextureAsset of string * OpenGL.Texture.TextureMetadata * uint
    | FontAsset of string * int * nativeint
    | CubeMapAsset of OpenGL.CubeMap.CubeMapMemoKey * uint * (uint * uint) option ref
    | StaticModelAsset of bool * OpenGL.PhysicallyBased.PhysicallyBasedStaticModel

/// The type of rendering used on a surface.
type [<StructuralEquality; NoComparison; Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType of Sort : single * Subsort : single

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
      mutable Glow : Color
      mutable Flip : Flip }

/// A renderer tag interface.
type Renderer = interface end

/// Configures a renderer.
type RendererConfig =
    { ShouldInitializeContext : bool
      ShouldBeginFrame : bool
      ShouldEndFrame : bool }