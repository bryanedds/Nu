// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// The blend mode of a sprite.
type [<Struct>] Blend =
    | Transparent
    | Additive
    | Overwrite

/// Represents an aspect of font styling.
type [<Struct>] FontStyle =
    | Bold
    | Italic
    | Underline
    | Strikethrough

/// Horizontal justification.
type [<Struct>] JustificationH =
    | JustifyLeft
    | JustifyCenter
    | JustifyRight

/// Vertical justification.
type [<Struct>] JustificationV =
    | JustifyTop
    | JustifyMiddle
    | JustifyBottom

/// Justification (such as for text alignment).
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
type RenderPass =
    | NormalPass
    | LightMapPass of LightProbeId : uint64 * LightMapBounds : Box3
    | ShadowPass of LightId : uint64 * FaceInfoOpt : (int * Matrix4x4 * Matrix4x4) option * LightType : LightType * ShadowRotation : Quaternion * ShadowFrustum : Frustum
    | ReflectionPass of ReflectorId : int64 * ShadowFrustum : Frustum

    /// Check that a render pass should displace another.
    static member displaces renderPass renderPass2 =
        if renderPass <> renderPass2 then
            match (renderPass, renderPass2) with
            | (NormalPass, NormalPass) -> failwithumf ()
            | (LightMapPass (lightProbeId, _), LightMapPass (lightProbeId2, _)) -> lightProbeId = lightProbeId2
            | (ShadowPass (lightId, faceInfoOpt, _, _, _), ShadowPass (lightId2, faceInfoOpt2, _, _, _)) ->
                lightId = lightId2 &&
                match struct (faceInfoOpt, faceInfoOpt2) with
                | struct (Some faceInfo, Some faceInfo2) -> Triple.fst faceInfo = Triple.fst faceInfo2
                | struct (None, None) -> true
                | struct (_, _) ->  false
            | (ReflectionPass (reflectorId, _), ReflectionPass (reflectorId2, _)) -> reflectorId = reflectorId2
            | (_, _) -> false
        else false

/// An asset that is used for rendering.
type RenderAsset =
    | RawAsset
    | TextureAsset of Texture : OpenGL.Texture.Texture
    | FontAsset of FontSizeDefault : int * Font : nativeint
    | CubeMapAsset of FilePaths : OpenGL.CubeMap.CubeMapKey * CubeMap : OpenGL.Texture.Texture * IrradianceAndEnvironmentMapOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref
    | StaticModelAsset of UserDefined : bool * StaticModel : OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelAsset of AnimatedModel : OpenGL.PhysicallyBased.PhysicallyBasedModel