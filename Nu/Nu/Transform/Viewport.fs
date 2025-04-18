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
      Bounds : Box2i
      DisplayScalar : int
      SsaoResolutionDivisor : int }

    /// The aspect ratio of this viewport.
    member this.AspectRatio = single this.Bounds.Size.X / single this.Bounds.Size.Y

    /// The shadow texture buffer resolution appropriate for this viewport.
    member this.ShadowResolution = v2iDup (Constants.Render.ShadowVirtualResolution * Globals.Render.ShadowScalar * this.DisplayScalar)

    /// The screen-space ambient occlusion texture buffer resolution appropriate for this viewport.
    member this.SsaoResolution = this.Bounds.Size / this.SsaoResolutionDivisor

    /// The shadow texture buffer resolution appropriate for this viewport and shadow buffer index.
    static member getShadowTextureBufferResolution shadowBufferIndex (viewport : Viewport) =
        let scalar = if shadowBufferIndex = 0 then Constants.Render.ShadowDetailedResolutionScalar else 1
        viewport.ShadowResolution * scalar

    /// Project to the given frame.
    static member project (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable vector = Vector3.Transform(source, frame)
        let a = source.X * frame.M14 + source.Y * frame.M24 + source.Z * frame.M34 + frame.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        vector.X <- (vector.X + 1.0f) * 0.5f * single viewport.Bounds.Size.X + single viewport.Bounds.Min.X
        vector.Y <- (-vector.Y + 1.0f) * 0.5f * single viewport.Bounds.Size.Y + single viewport.Bounds.Min.Y
        vector.Z <- vector.Z * (viewport.DistanceFar - viewport.DistanceNear) + viewport.DistanceNear
        vector

    /// Unproject from the given frame.
    static member unproject (source : Vector3) (frame : Matrix4x4) viewport =
        let mutable matrix = Unchecked.defaultof<_>
        Matrix4x4.Invert (frame, &matrix) |> ignore<bool>
        let mutable source = source
        source.X <- (source.X - single viewport.Bounds.Min.X) / single viewport.Bounds.Size.X * 2.0f - 1.0f
        source.Y <- -((source.Y - single viewport.Bounds.Min.Y) / single viewport.Bounds.Size.Y * 2.0f - 1.0f)
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

    /// Compute the 2d projection matrix.
    member this.Projection2d =
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
        let projection = viewport.Projection2d
        view * projection

    /// Compute the absolute 2d position from the given relative 3d position.
    static member position3dToPosition2d eyeCenter (eyeRotation : Quaternion) eyeFieldOfView (position : Vector3) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjection : Matrix4x4 = view * projection
        let positionViewProjection = (Vector4 (position, 1.0f)).Transform viewProjection
        let positionNdc = positionViewProjection.V3 / positionViewProjection.W
        let position2d = v3 (positionNdc.X * single (viewport.Bounds.Size.X / 2)) (positionNdc.Y * single (viewport.Bounds.Size.Y / 2)) positionNdc.Z
        position2d

    /// Compute the relative 3d ray from the given absolute 2d position.
    /// TODO: also implement Position2dToPosition3d.
    static member position2dToRay3d (eyeCenter : Vector3) (eyeRotation : Quaternion) eyeFieldOfView (position : Vector3) viewport =
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjectionInverse = (view * projection).Inverted
        let positionNdc = v3 (position.X / single (viewport.Bounds.Size.X * 2)) (position.Y / single (viewport.Bounds.Size.Y * 2)) 0.0f
        let positionViewProjection = positionNdc.Transform viewProjectionInverse
        let positionView = Vector4 (positionViewProjection.X, positionViewProjection.Y, -1.0f, 0.0f)
        let position3d = (positionView.Transform (Matrix4x4.CreateFromQuaternion eyeRotation.Inverted)).V3
        let rayDirection = (position3d - eyeCenter).Normalized
        let ray = Ray3 (eyeCenter, rayDirection)
        ray

    /// Transform the given mouse position to 2d screen space.
    static member mouseTo2dScreen (_ : Vector2) (eyeSize : Vector2) (mousePosition : Vector2) viewport =
        v2
            +(mousePosition.X / single viewport.DisplayScalar - eyeSize.X * 0.5f)
            -(mousePosition.Y / single viewport.DisplayScalar - eyeSize.Y * 0.5f) // negation for right-handedness

    /// Transform the given mouse position to 2d world space.
    static member mouseToWorld2d absolute (eyeCenter : Vector2) (eyeSize : Vector2) mousePosition viewport =
        let mouseScreen = Viewport.mouseTo2dScreen eyeCenter eyeSize mousePosition viewport
        let view = if absolute then Matrix4x4.Identity else Matrix4x4.CreateTranslation eyeCenter.V3
        (mouseScreen.V3.Transform view).V2

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
            (mousePosition.X / single viewport.Bounds.Size.X)
            (1.0f - (mousePosition.Y / single viewport.Bounds.Size.Y)) // inversion for right-handedness

    /// Transform the given mouse position to 3d world space.
    static member mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView (mousePosition : Vector2) viewport =
        let viewProjection = Viewport.getViewProjection3d v3Zero eyeRotation eyeFieldOfView viewport
        let near = Viewport.unproject (mousePosition.V3.WithZ 0.0f) viewProjection viewport
        let far = Viewport.unproject (mousePosition.V3.WithZ 1.0f) viewProjection viewport
        ray3 (near + eyeCenter) (far - near).Normalized

    static member private withinEpsilon (a : single) (b : single) =
        let c = a - b
        -Single.Epsilon <= c && c <= Single.Epsilon

    static member make distanceNear distanceFar bounds =
        { DistanceNear = distanceNear
          DistanceFar = distanceFar
          Bounds = bounds
          DisplayScalar = Globals.Render.DisplayScalar
          SsaoResolutionDivisor = Constants.Render.SsaoResolutionDivisor }

    static member makeGeometry (resolution : Vector2i) =
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent (box2i v2iZero resolution)

    static member makeRaster (bounds : Box2i) =
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds

    static member makeOuter (windowSize : Vector2i) =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let offsetMargin = Vector2i ((windowSize.X - outerResolution.X) / 2, (windowSize.Y - outerResolution.Y) / 2)
        let bounds = box2i offsetMargin outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds

    static member makeInterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceInterior Constants.Render.FarPlaneDistanceInterior bounds

    static member makeExterior () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceExterior Constants.Render.FarPlaneDistanceExterior bounds

    static member makeImposter () =
        let outerResolution = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        let bounds = box2i v2iZero outerResolution
        Viewport.make Constants.Render.NearPlaneDistanceImposter Constants.Render.FarPlaneDistanceImposter bounds