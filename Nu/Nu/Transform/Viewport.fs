// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// Describes the bounds of a viewport.
/// TODO: add missing doc comments to this type's functions.
type [<StructuralEquality; NoComparison>] Viewport =
    { DistanceNear : single
      DistanceFar : single
      Inset : Box2i
      Bounds : Box2i
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
    member this.SsaoResolution = this.Inset.Size / this.SsaoResolutionDivisor

    /// Project to the given frame.
    static member project (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable vector = Vector3.Transform(source, frame)
        let a = source.X * frame.M14 + source.Y * frame.M24 + source.Z * frame.M34 + frame.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        vector.X <- (vector.X + 1.0f) * 0.5f * single viewport.Inset.Size.X + single viewport.Inset.Min.X
        vector.Y <- (-vector.Y + 1.0f) * 0.5f * single viewport.Inset.Size.Y + single viewport.Inset.Min.Y
        vector.Z <- vector.Z * (viewport.DistanceFar - viewport.DistanceNear) + viewport.DistanceNear
        vector

    /// Unproject from the given frame.
    static member unproject (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable matrix = Unchecked.defaultof<_>
        Matrix4x4.Invert (frame, &matrix) |> ignore<bool>
        let mutable source = source
        source.X <- (source.X - single viewport.Inset.Min.X) / single viewport.Inset.Size.X * 2.0f - 1.0f
        source.Y <- -((source.Y - single viewport.Inset.Min.Y) / single viewport.Inset.Size.Y * 2.0f - 1.0f)
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
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        let translation = eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the 2d relative view matrix.
    static member getView2dRelative (eyeCenter : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        let translation = -eyeCenter * virtualScalar + eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute a 2d view matrix.
    static member getView2d absolute eyeCenter eyeSize viewport =
        if absolute
        then Viewport.getView2dAbsolute eyeCenter eyeSize viewport
        else Viewport.getView2dRelative eyeCenter eyeSize viewport

    /// Compute the 2d projection matrix in world terms.
    member this.Projection2dWorld =
        Matrix4x4.CreateOrthographicOffCenter
            (single this.Bounds.Min.X,
             single (this.Bounds.Min.X + this.Bounds.Size.X),
             single this.Bounds.Min.Y,
             single (this.Bounds.Min.Y + this.Bounds.Size.Y),
             -1.0f,
             1.0f)

    /// Compute the 2d view projection matrix.
    static member getViewProjection2d absolute eyeCenter eyeSize viewport =
        let view = Viewport.getView2d absolute eyeCenter eyeSize viewport
        let projection = viewport.Projection2dWorld
        view * projection

    /// Compute the scissor clip absolute view matrix.
    static member getViewClipAbsolute (_ : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        let translation = eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the scissor clip relative view matrix.
    static member getViewClipRelative (eyeCenter : Vector2) (eyeSize : Vector2) viewport =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
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
        let projection = viewport.Projection2dWorld
        view * projection

    /// Compute the absolute 2d position from the given relative 3d position.
    static member position3dToPosition2d eyeCenter (eyeRotation : Quaternion) eyeFieldOfView (position : Vector3) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjection : Matrix4x4 = view * projection
        let positionViewProjection = (Vector4 (position, 1.0f)).Transform viewProjection
        let positionNdc = positionViewProjection.V3 / positionViewProjection.W
        let position2d = v3 (positionNdc.X * single (viewport.Inset.Size.X / 2)) (positionNdc.Y * single (viewport.Inset.Size.Y / 2)) positionNdc.Z
        position2d

    /// Compute the relative 3d ray from the given absolute 2d position.
    /// TODO: also implement Position2dToPosition3d.
    static member position2dToRay3d (eyeCenter : Vector3) (eyeRotation : Quaternion) eyeFieldOfView (position : Vector3) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjectionInverse = (view * projection).Inverted
        let positionNdc = v3 (position.X / single (viewport.Inset.Size.X * 2)) (position.Y / single (viewport.Inset.Size.Y * 2)) 0.0f
        let positionViewProjection = positionNdc.Transform viewProjectionInverse
        let positionView = Vector4 (positionViewProjection.X, positionViewProjection.Y, -1.0f, 0.0f)
        let position3d = (positionView.Transform (Matrix4x4.CreateFromQuaternion eyeRotation.Inverted)).V3
        let rayDirection = (position3d - eyeCenter).Normalized
        let ray = Ray3 (eyeCenter, rayDirection)
        ray

    /// Transform the given mouse position to 2d inset space.
    static member mouseTo2dInset (_ : Vector2) (eyeSize : Vector2) (mousePosition : Vector2) viewport =
        let mousePositionVirtual =
            v2
                +(mousePosition.X / single viewport.DisplayScalar - eyeSize.X * 0.5f)
                -(mousePosition.Y / single viewport.DisplayScalar - eyeSize.Y * 0.5f) // negation for right-handedness
        let inset = box2 (viewport.Inset.Min.V2 / single viewport.DisplayScalar) (viewport.Inset.Size.V2 / single viewport.DisplayScalar)
        let bounds = box2 (viewport.Bounds.Min.V2 / single viewport.DisplayScalar) (viewport.Bounds.Size.V2 / single viewport.DisplayScalar)
        let boundsRatio = bounds.Size / inset.Size
        let insetOffset = (bounds.Min - inset.Min) * boundsRatio
        let mousePositionPositive = (mousePositionVirtual + bounds.Size * 0.5f) * boundsRatio
        let mousePositionInset = mousePositionPositive - bounds.Size * 0.5f + insetOffset
        mousePositionInset

    /// Transform the given mouse position to 3d inset space.
    static member mouseTo3dInset (mousePosition : Vector2) viewport =
        let offset =
            (viewport.Bounds.Min.Y - viewport.Inset.Min.Y) +
            (viewport.Bounds.Max.Y - viewport.Inset.Max.Y)
        v2
            mousePosition.X
            (mousePosition.Y - single offset)

    /// Transform the given mouse position to 2d world space.
    static member mouseToWorld2d absolute (eyeCenter : Vector2) (eyeSize : Vector2) mousePosition viewport =
        let mouseInset = Viewport.mouseTo2dInset eyeCenter eyeSize mousePosition viewport
        let view = if absolute then Matrix4x4.Identity else Matrix4x4.CreateTranslation eyeCenter.V3
        (mouseInset.V3.Transform view).V2

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
    static member getFrustum eyeCenter (eyeRotation : Quaternion) (eyeFieldOfView : single) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjection = view * projection
        Frustum viewProjection

    /// Transform the given mouse position to screen (normalized device coordinates).
    static member mouseToScreen3d (mousePosition : Vector2) (viewport : Viewport) =
        v2
            (mousePosition.X / single viewport.Inset.Size.X)
            (1.0f - (mousePosition.Y / single viewport.Inset.Size.Y)) // inversion for right-handedness

    /// Transform the given mouse position to 3d world space.
    static member mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView (mousePosition : Vector2) viewport =
        let mouseInset = Viewport.mouseTo3dInset mousePosition viewport
        let viewProjection = Viewport.getViewProjection3d v3Zero eyeRotation eyeFieldOfView viewport
        let near = Viewport.unproject (mouseInset.V3.WithZ 0.0f) viewProjection viewport
        let far = Viewport.unproject (mouseInset.V3.WithZ 1.0f) viewProjection viewport
        ray3 (near + eyeCenter) (far - near).Normalized

    static member private withinEpsilon (a : single) (b : single) =
        let c = a - b
        -Single.Epsilon <= c && c <= Single.Epsilon

    static member make distanceNear distanceFar inset bounds =
        { DistanceNear = distanceNear
          DistanceFar = distanceFar
          Inset = inset
          Bounds = bounds
          DisplayScalar = Globals.Render.DisplayScalar
          SsaoResolutionDivisor = Constants.Render.SsaoResolutionDivisor }

    static member makeGeometry (resolution : Vector2i) =
        let bounds = box2i v2iZero resolution
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds

    static member makeRaster (inset : Box2i) (bounds : Box2i) =
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent inset bounds

    static member makeOuter (windowSize : Vector2i) =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let offsetMargin = Vector2i ((windowSize.X - outerResolution.X) / 2, (windowSize.Y - outerResolution.Y) / 2)
        let bounds = box2i offsetMargin outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds

    static member makeInterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceInterior Constants.Render.FarPlaneDistanceInterior bounds bounds

    static member makeExterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceExterior Constants.Render.FarPlaneDistanceExterior bounds bounds

    static member makeImposter () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceImposter Constants.Render.FarPlaneDistanceImposter bounds bounds