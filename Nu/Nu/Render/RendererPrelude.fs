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
/// Use `Unjustified true` in order to render line breaks.
type Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of Wrapped : bool

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

/// Describes the nature of the rendering that takes place.
type [<CustomEquality; NoComparison>] RenderPass =
    | LightMapPass of LightProbeId : uint64 * LightMapBounds : Box3
    | ShadowPass of LightId : uint64 * FaceInfoOpt : (int * Matrix4x4 * Matrix4x4) option * LightType : LightType * ShadowRotation : Quaternion * ShadowFrustum : Frustum
    | ReflectionPass of ReflectorId : int64 * ShadowFrustum : Frustum
    | NormalPass

    /// Check that a render pass should displace another.
    static member displaces renderPass renderPass2 =
        if renderPass <> renderPass2 then
            match (renderPass, renderPass2) with
            | (LightMapPass (id, _), LightMapPass (id2, _)) -> id = id2
            | (ShadowPass (id, indexInfoOpt, _, _, _), ShadowPass (id2, indexInfoOpt2, _, _, _)) ->
                id = id2 &&
                match struct (indexInfoOpt, indexInfoOpt2) with
                | struct (Some indexInfo, Some indexInfo2) -> Triple.fst indexInfo = Triple.fst indexInfo2
                | struct (None, None) -> true
                | struct (_, _) ->  false
            | (ReflectionPass (id, _), ReflectionPass (id2, _)) -> id = id2
            | (NormalPass, NormalPass) -> failwithumf ()
            | (_, _) -> false
        else false

    static member private equals this that =
        refEq this that ||
        match this with
        | LightMapPass (id, bounds) ->
            match that with
            | LightMapPass (id2, bounds2) -> id = id2 && bounds = bounds2
            | _ -> false
        | ShadowPass (id, indexInfoOpt, lightType, rotation, frustum) ->
            match that with
            | ShadowPass (id2, indexInfoOpt2, lightType2, rotation2, frustum2) ->
                id = id2 &&
                (match indexInfoOpt with
                 | Some (index, view, projection) ->
                    match indexInfoOpt2 with
                    | Some (index2, view2, projection2) -> index = index2 && view = view2 && projection = projection2
                    | None -> false
                 | None -> indexInfoOpt2.IsNone) &&
                lightType = lightType2 &&
                rotation = rotation2 &&
                frustum = frustum2
            | _ -> false
        | ReflectionPass (id, frustum) ->
            match that with
            | ReflectionPass (id2, frustum2) -> id = id2 && frustum = frustum2
            | _ -> false
        | NormalPass -> that.IsNormalPass

    override this.GetHashCode () =
        // OPTIMIZATION: we only hash certain parts of the render pass in order to make hashing cheaper.
        match this with
        | LightMapPass (id, _) -> hash id
        | ShadowPass (id, indexInfoOpt, _, _, _) -> 1 ^^^ hash id ^^^ match indexInfoOpt with Some (index, _, _) -> hash index | None -> 0
        | ReflectionPass (id, _) -> 2 ^^^ hash id
        | NormalPass -> 3

    override this.Equals that =
        match that with
        | :? RenderPass as that -> RenderPass.equals this that
        | _ -> false

    interface IEquatable<RenderPass> with
        member this.Equals that = RenderPass.equals this that

/// An asset that is used for rendering.
type RenderAsset =
    | RawAsset
    | TextureAsset of Texture : OpenGL.Texture.Texture
    | FontAsset of FontSizeDefault : int * Font : nativeint
    | CubeMapAsset of FilePaths : OpenGL.CubeMap.CubeMapKey * CubeMap : OpenGL.Texture.Texture * IrradianceAndEnvironmentMapOptRef : (OpenGL.Texture.Texture * OpenGL.Texture.Texture) option ref
    | StaticModelAsset of UserDefined : bool * StaticModel : OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelAsset of AnimatedModel : OpenGL.PhysicallyBased.PhysicallyBasedModel