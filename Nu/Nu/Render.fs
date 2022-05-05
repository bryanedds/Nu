// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open SDL2
open TiledSharp
open Prime
open Nu

/// An image. Currently just used as a phantom type.
type Image = private { __ : unit }

/// A font. Currently just used as a phantom type.
type Font = private { __ : unit }

/// A tile map. Currently just used as a phantom type.
type TileMap = private { __ : unit }

/// An asset that is used for rendering.
type [<NoEquality; NoComparison>] RenderAsset =
    | TextureAsset of TextureMetadata * uint
    | FontAsset of int * nativeint

/// The flipness of a sprite.
[<Syntax
    ("FlipNone FlipH FlipV FlipHV", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Flip =
    | FlipNone
    | FlipH
    | FlipV
    | FlipHV

    /// Convert to a flip value recognized by SDL.
    static member toSdlFlip flip =
        match flip with
        | FlipHV -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL ||| SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL
        | FlipH -> SDL.SDL_RendererFlip.SDL_FLIP_HORIZONTAL
        | FlipV -> SDL.SDL_RendererFlip.SDL_FLIP_VERTICAL
        | FlipNone -> SDL.SDL_RendererFlip.SDL_FLIP_NONE

/// The blend mode of a sprite.
[<Syntax
    ("Transparent Additive Modulate Overwrite", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Blend =
    | Transparent
    | Additive
    | Modulate
    | Overwrite

    /// Convert to a blend mode value recognized by SDL.
    static member toSdlBlendMode flip =
        match flip with
        | Transparent -> SDL.SDL_BlendMode.SDL_BLENDMODE_BLEND
        | Additive -> SDL.SDL_BlendMode.SDL_BLENDMODE_ADD
        | Modulate -> SDL.SDL_BlendMode.SDL_BLENDMODE_MOD
        | Overwrite -> SDL.SDL_BlendMode.SDL_BLENDMODE_NONE

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
type [<StructuralEquality; NoComparison>] Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

/// A mutable sprite value.
type [<NoEquality; NoComparison; Struct>] Sprite =
    { mutable Transform : Transform
      mutable Inset : Box2 // OPTIMIZATION: elides optionality to avoid pointer indirection; zero is full texture.
      mutable Image : Image AssetTag
      mutable Color : Color
      mutable Blend : Blend
      mutable Glow : Color
      mutable Flip : Flip }

/// A mutable particle value.
type [<NoEquality; NoComparison; Struct>] Particle =
    { mutable Transform : Transform
      mutable Inset : Box2 // OPTIMIZATION: elides optionality to avoid pointer indirection; zero is full texture.
      mutable Color : Color
      mutable Glow : Color
      mutable Flip : Flip }

/// Describes how to render a sprite to the rendering system.
type [<NoEquality; NoComparison>] SpriteDescriptor =
    { mutable Transform : Transform
      InsetOpt : Box2 option
      Image : Image AssetTag
      Color : Color
      Blend : Blend
      Glow : Color
      Flip : Flip }

/// Describes how to render multiple sprites to the rendering system.
type [<NoEquality; NoComparison>] SpritesDescriptor =
    { Sprites : Sprite array }

/// Describes how to render a tile map layer to the rendering system.
type [<NoEquality; NoComparison>] TileLayerDescriptor =
    { mutable Transform : Transform
      Color : Color
      Glow : Color
      MapSize : Vector2i
      Tiles : TmxLayerTile array
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
      Particles : Particle array }

/// A renderer tag interface.
and Renderer = interface end