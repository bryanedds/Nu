// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// The blend mode of a sprite.
[<Syntax
    ("Transparent Additive Overwrite", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<Struct>] Blend =
    | Transparent
    | Additive
    | Overwrite

/// Represents an aspect of font styling.
[<Syntax
    ("Bold Italic Underline Strikethrough", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<Struct>] FontStyle =
    | Bold
    | Italic
    | Underline
    | Strikethrough

/// Horizontal justification.
[<Syntax
    ("JustifyLeft JustifyRight JustifyCenter", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<Struct>] JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

/// Vertical justification.
[<Syntax
    ("JustifyTop JustifyMiddle JustifyBottom", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<Struct>] JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

/// Justification (such as for text alignment).
[<Syntax
    ("Justified Unjustified", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of bool

/// A mutable particle type.
type [<Struct>] Particle =
    { mutable Transform : Transform
      mutable InsetOpt : Box2 ValueOption
      mutable Color : Color
      mutable Emission : Color
      mutable Flip : Flip }

/// The type of rendering used on a surface (for use by the lower-level renderer API).
type [<Struct>] RenderType =
    | DeferredRenderType
    | ForwardRenderType of Subsort : single * Sort : single

/// Desribes the render pass at play.
/// OPTIMIZATION: uses partial hashing for speed.
type [<CustomEquality; NoComparison>] RenderPass =
    | NormalPass of SkipCulling : bool
    | ShadowPass of LightId : uint64 * ShadowDirectional : bool * ShadowFrustum : Frustum
    | ReflectionPass of ReflectorId : int64 * ShadowFrustum : Frustum

    static member hash renderPass =
        match renderPass with
        | NormalPass _ -> 0
        | ShadowPass (lightId, _, _) -> hash lightId
        | ReflectionPass (reflectorId, _) -> hash reflectorId

    static member equals left right =
        match struct (left, right) with
        | struct (NormalPass _, NormalPass _) -> true
        | struct (ShadowPass (lightId, _, _), ShadowPass (lightId2, _, _)) -> lightId = lightId2
        | struct (ReflectionPass (lightId, _), ReflectionPass (lightId2, _)) -> lightId = lightId2
        | struct (_, _) -> false

    static member comparer =
        HashIdentity.FromFunctions RenderPass.hash RenderPass.equals

    override this.Equals thatObj =
        match thatObj with
        | :? RenderPass as that -> RenderPass.equals this that
        | _ -> false

    override this.GetHashCode () =
        RenderPass.hash this

/// An asset that is used for rendering.
type RenderAsset =
    | RawAsset
    | TextureAsset of TextureMetadata : OpenGL.Texture.TextureMetadata * Texture : OpenGL.Texture.Texture
    | FontAsset of FontSizeDefault : int * Font : nativeint
    | CubeMapAsset of FilePaths : OpenGL.CubeMap.CubeMapMemoKey * CubeMap : OpenGL.Texture.Texture * IrradianceAndEnvironmentMapOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref
    | StaticModelAsset of UserDefined : bool * StaticModel : OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelAsset of AnimatedModel : OpenGL.PhysicallyBased.PhysicallyBasedModel