// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

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

/// A static model. Currently just used as a phantom type.
type StaticModel = private { __ : unit }

/// An asset that is used for rendering.
type RenderAsset =
    | TextureAsset of string * OpenGL.Texture.TextureMetadata * uint
    | FontAsset of string * int * nativeint
    | CubeMapAsset of OpenGL.CubeMap.CubeMapMemoKey * uint * (uint * uint) option ref
    | StaticModelAsset of bool * OpenGL.PhysicallyBased.PhysicallyBasedStaticModel

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

/// A mutable sprite value.
type [<NoEquality; NoComparison; Struct>] Sprite =
    { mutable Transform : Transform
      mutable InsetOpt : Box2 ValueOption
      mutable Image : Image AssetTag
      mutable Color : Color
      mutable Blend : Blend
      mutable Glow : Color
      mutable Flip : Flip }

/// A mutable particle value.
type [<NoEquality; NoComparison; Struct>] Particle =
    { mutable Transform : Transform
      mutable InsetOpt : Box2 ValueOption
      mutable Color : Color
      mutable Glow : Color
      mutable Flip : Flip }

/// Describes how to render a sprite to the rendering system.
type [<NoEquality; NoComparison>] SpriteDescriptor =
    { mutable Transform : Transform
      InsetOpt : Box2 ValueOption
      Image : Image AssetTag
      Color : Color
      Blend : Blend
      Glow : Color
      Flip : Flip }

/// Describes how to render multiple sprites to the rendering system.
type [<NoEquality; NoComparison>] SpritesDescriptor =
    { Sprites : Sprite SegmentedArray }

/// Describes how to render multiple sprite descriptors to the rendering system.
type [<NoEquality; NoComparison>] SpriteDescriptors =
    { SpriteDescriptors : SpriteDescriptor SegmentedList }

/// Describes an internally cached sprite used to avoid GC promotion of sprite descriptors.
type [<NoEquality; NoComparison>] CachedSpriteDescriptor =
    { mutable CachedSprite : Sprite }

/// Describes how to render tile map tiles to the rendering system.
type [<NoEquality; NoComparison>] TilesDescriptor =
    { mutable Transform : Transform
      Color : Color
      Glow : Color
      MapSize : Vector2i
      Tiles : TmxLayerTile SegmentedList
      TileSourceSize : Vector2i
      TileSize : Vector2
      TileAssets : (TmxTileset * Image AssetTag) array }

/// Describes how to render text to the rendering system.
type [<NoEquality; NoComparison>] TextDescriptor =
    { mutable Transform : Transform
      Text : string
      Font : Font AssetTag
      Color : Color
      Justification : Justification }

/// Describes particles.
type [<NoEquality; NoComparison>] ParticlesDescriptor =
    { Elevation : single
      Horizon : single
      Absolute : bool
      Blend : Blend
      Image : Image AssetTag
      Particles : Particle SegmentedArray }

/// A renderer tag interface.
and Renderer = interface end

/// Configures a renderer.
type RendererConfig =
    { ShouldInitializeContext : bool
      ShouldBeginFrame : bool
      ShouldEndFrame : bool }