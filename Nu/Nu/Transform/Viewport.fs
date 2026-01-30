// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime

/// Describes the bounds of a viewport.
/// TODO: add missing doc comments to this type's functions.
type [<StructuralEquality; NoComparison>] Viewport =
    { DistanceNear : single
      DistanceFar : single
      Inner : Box2i
      Bounds : Box2i
      Outer : Box2i
      DisplayScalar : int
      SsaoResolutionDivisor : int }

    /// The aspect ratio of this viewport.
    member this.AspectRatio =
        single this.Bounds.Size.X / single this.Bounds.Size.Y

    /// The shadow texture buffer resolution appropriate for this viewport.
    member this.ShadowTextureResolution =
        let shadowDisplayScalar = min Constants.Render.ShadowDisplayScalarMax this.DisplayScalar
        v2iDup (Constants.Render.ShadowVirtualResolution * Globals.Render.ShadowScalar * shadowDisplayScalar)

    /// The shadow map buffer resolution appropriate for this viewport.
    member this.ShadowMapResolution =
        this.ShadowTextureResolution / 2

    /// The shadow cascade buffer resolution appropriate for this viewport.
    member this.ShadowCascadeResolution =
        this.ShadowTextureResolution / 2

    /// The screen-space ambient occlusion texture buffer resolution appropriate for this viewport.
    member this.SsaoResolution = this.Inner.Size / this.SsaoResolutionDivisor

    /// Project to the given frame.
    static member project (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable vector = Vector3.Transform(source, frame)
        let a = source.X * frame.M14 + source.Y * frame.M24 + source.Z * frame.M34 + frame.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        let offset = (viewport.Outer.Size - viewport.Bounds.Size) / 2
        let inner = box2i (viewport.Inner.Min + offset) viewport.Inner.Size
        vector.X <- (vector.X + 1.0f) * 0.5f * single inner.Size.X + single inner.Min.X
        vector.Y <- (-vector.Y + 1.0f) * 0.5f * single inner.Size.Y + single inner.Min.Y
        vector.Z <- vector.Z * (viewport.DistanceFar - viewport.DistanceNear) + viewport.DistanceNear
        vector

    /// Unproject from the given frame.
    static member unproject (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable matrix = Unchecked.defaultof<_>
        Matrix4x4.Invert (frame, &matrix) |> ignore<bool>
        let offset = (viewport.Outer.Size - viewport.Bounds.Size) / -2
        let inner = box2i (viewport.Inner.Min + offset) viewport.Inner.Size
        let mutable source = source
        source.X <- (source.X - single inner.Min.X) / single inner.Size.X * 2.0f - 1.0f
        source.Y <- -((source.Y - single inner.Min.Y) / single inner.Size.Y * 2.0f - 1.0f)
        source.Z <- (source.Z - viewport.DistanceNear) / (viewport.DistanceFar - viewport.DistanceNear)
        let mutable vector = Vector3.Transform (source, matrix)
        let a = source.X * matrix.M14 + source.Y * matrix.M24 + source.Z * matrix.M34 + matrix.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        vector

    /// Compute the 2d absolute view matrix.
    static member getView2dAbsolute (_ : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = v2Dup (single viewport.DisplayScalar)
        let translation = eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the 2d relative view matrix.
    static member getView2dRelative (eyeCenter : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = v2Dup (single viewport.DisplayScalar)
        let translation = -eyeCenter * virtualScalar + eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute a 2d view matrix.
    static member getView2d absolute eyeCenter eyeSize viewport =
        if absolute
        then Viewport.getView2dAbsolute eyeCenter eyeSize viewport
        else Viewport.getView2dRelative eyeCenter eyeSize viewport

    /// Compute the 2d projection matrix.
    member this.Projection2d =
        Matrix4x4.CreateOrthographicOffCenter
            (0.0f, single this.Bounds.Size.X,
             0.0f, single this.Bounds.Size.Y,
             -1.0f,
             1.0f)

    /// Compute the 2d view projection matrix.
    static member getViewProjection2d absolute eyeCenter eyeSize viewport =
        let view = Viewport.getView2d absolute eyeCenter eyeSize viewport
        let projection = viewport.Projection2d
        view * projection

    /// Compute the scissor clip absolute view matrix.
    static member getViewClipAbsolute (_ : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = v2Dup (single viewport.DisplayScalar)
        let translation = eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the scissor clip relative view matrix.
    static member getViewClipRelative (eyeCenter : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = v2Dup (single viewport.DisplayScalar)
        let translation = -eyeCenter + eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the scissor clip view matrix.
    static member getViewClip absolute (eyeCenter : Vector2) eyeSize viewport =
        if absolute
        then Viewport.getViewClipAbsolute eyeCenter eyeSize viewport
        else Viewport.getViewClipRelative eyeCenter eyeSize viewport

    /// Compute the scissor clip view projection matrix.
    static member getViewProjectionClip absolute eyeCenter eyeSize viewport =
        let view = Viewport.getViewClip absolute eyeCenter eyeSize viewport
        let projection = viewport.Projection2d
        view * projection

    /// Compute the absolute 2d position from the given relative 3d position.
    static member position3dToPosition2d eyeCenter (eyeRotation : Quaternion) eyeFieldOfView (position : Vector3) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjection : Matrix4x4 = view * projection
        let positionViewProjection = (Vector4 (position, 1.0f)).Transform viewProjection
        let positionNdc = positionViewProjection.V3 / positionViewProjection.W
        let position2d =
            v2
                (positionNdc.X * single (viewport.Bounds.Size.X / 2 / viewport.DisplayScalar))
                (positionNdc.Y * single (viewport.Bounds.Size.Y / 2 / viewport.DisplayScalar))
        position2d

    /// Compute the relative 3d ray from the given absolute 2d position.
    static member position2dToRay3d (eyeCenter : Vector3) (eyeRotation : Quaternion) eyeFieldOfView (position : Vector2) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjectionInverse = (view * projection).Inverted
        let positionNdc =
            v3
                (position.X / single (viewport.Bounds.Size.X * 2 * viewport.DisplayScalar))
                (position.Y / single (viewport.Bounds.Size.Y * 2 * viewport.DisplayScalar))
                0.0f
        let positionViewProjection = positionNdc.Transform viewProjectionInverse
        let positionView = Vector4 (positionViewProjection.X, positionViewProjection.Y, -1.0f, 0.0f)
        let position3d = (positionView.Transform (Matrix4x4.CreateFromQuaternion eyeRotation.Inverted)).V3
        let rayDirection = (position3d - eyeCenter).Normalized
        let ray = Ray3 (eyeCenter, rayDirection)
        ray

    /// Transform the given mouse position to 2d inner space.
    static member mouseTo2dInner (_ : Vector2) (eyeSize : Vector2) (mousePosition : Vector2) viewport =
        let mousePositionVirtual =
            v2
                +(mousePosition.X / single viewport.DisplayScalar - eyeSize.X * 0.5f)
                -(mousePosition.Y / single viewport.DisplayScalar - eyeSize.Y * 0.5f) // negation for right-handedness
        let inner = box2 (viewport.Inner.Min.V2 / single viewport.DisplayScalar) (viewport.Inner.Size.V2 / single viewport.DisplayScalar)
        let bounds = box2 (viewport.Bounds.Min.V2 / single viewport.DisplayScalar) (viewport.Bounds.Size.V2 / single viewport.DisplayScalar)
        let boundsRatio = bounds.Size / inner.Size
        let offset = (bounds.Min - inner.Min) * boundsRatio
        let mousePositionPositive = (mousePositionVirtual + bounds.Size * 0.5f) * boundsRatio
        let mousePositionInner = mousePositionPositive - bounds.Size * 0.5f + offset
        mousePositionInner

    /// Transform the given mouse position to 3d inner space.
    static member mouseTo3dInner (mousePosition : Vector2) viewport =
        let offset =
            (viewport.Bounds.Min.Y - viewport.Inner.Min.Y) +
            (viewport.Bounds.Max.Y - viewport.Inner.Max.Y)
        v2
            mousePosition.X
            (mousePosition.Y - single offset)

    /// Transform the given mouse position to 2d world space.
    static member mouseToWorld2d absolute (eyeCenter : Vector2) (eyeSize : Vector2) mousePosition viewport =
        let mouseInner = Viewport.mouseTo2dInner eyeCenter eyeSize mousePosition viewport
        let view = if absolute then Matrix4x4.Identity else Matrix4x4.CreateTranslation eyeCenter.V3
        (mouseInner.V3.Transform view).V2

    /// Transform the given mouse position to 2d entity space (eye 2d coordinates).
    static member mouseToEntity2d absolute entityPosition entitySize mousePosition viewport =
        let mouseWorld = Viewport.mouseToWorld2d absolute entityPosition entitySize mousePosition viewport
        entityPosition - mouseWorld

    /// Compute the 3d view matrix.
    static member getView3d (eyeCenter : Vector3) (eyeRotation : Quaternion) : Matrix4x4 =
        (Matrix4x4.CreateFromQuaternion eyeRotation * Matrix4x4.CreateTranslation eyeCenter).Inverted

    /// Compute the 3d projection matrix.
    static member getProjection3d (fieldOfView : single) (viewport : Viewport) : Matrix4x4 =
        Matrix4x4.CreatePerspectiveFieldOfView
            (fieldOfView,
             viewport.AspectRatio,
             viewport.DistanceNear,
             viewport.DistanceFar)

    /// Compute a 3d view projection matrix.
    static member getViewProjection3d eyeCenter eyeRotation eyeFieldOfView viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        view * projection

    /// Compute a 3d view frustum.
    static member getFrustum eyeCenter eyeRotation eyeFieldOfView viewport =
        let viewProjection = Viewport.getViewProjection3d eyeCenter eyeRotation eyeFieldOfView viewport
        Frustum viewProjection

    /// Transform the given mouse position to screen (normalized device coordinates).
    static member mouseToScreen3d (mousePosition : Vector2) (viewport : Viewport) =
        v2
            (mousePosition.X / single viewport.Inner.Size.X)
            (1.0f - (mousePosition.Y / single viewport.Inner.Size.Y)) // inversion for right-handedness

    /// Transform the given mouse position to 3d world space.
    static member mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView (mousePosition : Vector2) viewport =
        let mouseInner = Viewport.mouseTo3dInner mousePosition viewport
        let viewProjection = Viewport.getViewProjection3d v3Zero eyeRotation eyeFieldOfView viewport
        let near = Viewport.unproject (mouseInner.V3.WithZ 0.0f) viewProjection viewport
        let far = Viewport.unproject (mouseInner.V3.WithZ 1.0f) viewProjection viewport
        ray3 (near + eyeCenter) (far - near).Normalized

    static member private withinEpsilon (a : single) (b : single) =
        let c = a - b
        -Single.Epsilon <= c && c <= Single.Epsilon

    static member make distanceNear distanceFar inner bounds outer =
        { DistanceNear = distanceNear
          DistanceFar = distanceFar
          Inner = inner
          Bounds = bounds
          Outer = outer
          DisplayScalar = Globals.Render.DisplayScalar
          SsaoResolutionDivisor = Constants.Render.SsaoResolutionDivisor }

    static member makeGeometry (resolution : Vector2i) =
        let bounds = box2i v2iZero resolution
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds bounds

    static member makeWindow inner bounds (windowSize : Vector2i) =
        let outer = box2i v2iZero windowSize
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent inner bounds outer

    static member makeWindow1 (windowSize : Vector2i) =
        let boundsSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let boundsMin = Vector2i ((windowSize.X - boundsSize.X) / 2, (windowSize.Y - boundsSize.Y) / 2)
        let bounds = box2i boundsMin boundsSize
        Viewport.makeWindow bounds bounds windowSize // presume inner = bounds

    static member makeInterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceInterior Constants.Render.FarPlaneDistanceInterior bounds bounds bounds

    static member makeExterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceExterior Constants.Render.FarPlaneDistanceExterior bounds bounds bounds

    static member makeImposter () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceImposter Constants.Render.FarPlaneDistanceImposter bounds bounds bounds