// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Configuration
open System.Numerics
open Prime

[<RequireQualifiedAccess>]
module Viewport =

    let mutable DisplayVirtualScalar = match ConfigurationManager.AppSettings.["DisplayVirtualScalar"] with null -> 3 | scalar -> scvalue scalar
    let mutable ShadowVirtualResolution = match ConfigurationManager.AppSettings.["ShadowVirtualResolution"] with null -> 512 | scalar -> scvalue scalar

/// Describes the bounds of a viewport.
/// TODO: P0: replace non-property members with static members.
type [<StructuralEquality; NoComparison>] Viewport =
    { DistanceNear : single
      DistanceFar : single
      Bounds : Box2i
      DisplayVirtualScalar : int
      SsaoResolutionDivisor : int }

    member this.Resolution = this.Bounds.Size
    member this.AspectRatio = single this.Resolution.X / single this.Resolution.Y
    member this.DisplayResolution = Constants.Render.DisplayVirtualResolution * this.DisplayVirtualScalar
    member this.ShadowResolution = v2iDup (Viewport.ShadowVirtualResolution * this.DisplayVirtualScalar)
    member this.SsaoResolution = this.Bounds.Size / this.SsaoResolutionDivisor
    member this.ReflectionMapResolution = Constants.Render.ReflectionMapResolution

    member this.ShadowTextureBufferResolution shadowBufferIndex =
        let scalar = if shadowBufferIndex = 0 then Constants.Render.ShadowDetailedResolutionScalar else 1
        this.ShadowResolution * scalar

    member this.OffsetMargin (windowSize : Vector2i) =
        Vector2i ((windowSize.X - this.DisplayResolution.X) / 2, (windowSize.Y - this.DisplayResolution.Y) / 2)

    member this.OffsetBounds (windowSize : Vector2i) =
        box2i (this.OffsetMargin windowSize) this.DisplayResolution

    /// Project to the given frame.
    member this.Project (source: Vector3, frame: Matrix4x4) =
        let mutable vector = Vector3.Transform(source, frame)
        let a = source.X * frame.M14 + source.Y * frame.M24 + source.Z * frame.M34 + frame.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        vector.X <- (vector.X + 1.0f) * 0.5f * single this.Bounds.Size.X + single this.Bounds.Min.X
        vector.Y <- (-vector.Y + 1.0f) * 0.5f * single this.Bounds.Size.Y + single this.Bounds.Min.Y
        vector.Z <- vector.Z * (this.DistanceFar - this.DistanceNear) + this.DistanceNear
        vector

    /// Unproject from the given frame.
    member this.Unproject (source : Vector3, frame : Matrix4x4) =
        let mutable matrix = Unchecked.defaultof<_>
        Matrix4x4.Invert (frame, &matrix) |> ignore<bool>
        let mutable source = source
        source.X <- (source.X - single this.Bounds.Min.X) / single this.Bounds.Size.X * 2.0f - 1.0f
        source.Y <- -((source.Y - single this.Bounds.Min.Y) / single this.Bounds.Size.Y * 2.0f - 1.0f)
        source.Z <- (source.Z - this.DistanceNear) / (this.DistanceFar - this.DistanceNear)
        let mutable vector = Vector3.Transform (source, matrix)
        let a = source.X * matrix.M14 + source.Y * matrix.M24 + source.Z * matrix.M34 + matrix.M44
        if not (Viewport.withinEpsilon a 1.0f) then
            vector.X <- vector.X / a
            vector.Y <- vector.Y / a
            vector.Z <- vector.Z / a
        vector

    /// Compute the 2d absolute view matrix.
    member this.View2dAbsolute (_ : Vector2, eyeSize : Vector2) =
        let virtualScalar = (v2iDup this.DisplayVirtualScalar).V2
        let translation = eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute the 2d relative view matrix.
    member this.View2dRelative (eyeCenter : Vector2, eyeSize : Vector2) =
        let virtualScalar = (v2iDup this.DisplayVirtualScalar).V2
        let translation = -eyeCenter * virtualScalar + eyeSize * 0.5f * virtualScalar
        Matrix4x4.CreateTranslation translation.V3

    /// Compute a 2d view matrix.
    member this.View2d (absolute, eyeCenter, eyeSize) =
        if absolute
        then this.View2dAbsolute (eyeCenter, eyeSize)
        else this.View2dRelative (eyeCenter, eyeSize)

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
    member this.ViewProjection2d (absolute, eyeCenter, eyeSize) =
        let view = this.View2d (absolute, eyeCenter, eyeSize)
        let projection = this.Projection2d
        view * projection

    /// Compute the absolute 2d position from the given relative 3d position.
    member this.Position3dToPosition2d (position : Vector3, eyeCenter, eyeRotation : Quaternion, eyeFieldOfView, resolution : Vector2i) =
        let view = this.View3d (eyeCenter, eyeRotation)
        let projection = this.Projection3d eyeFieldOfView
        let viewProjection : Matrix4x4 = view * projection
        let positionViewProjection = (Vector4 (position, 1.0f)).Transform viewProjection
        let positionNdc = positionViewProjection.V3 / positionViewProjection.W
        let position2d = v3 (positionNdc.X * single (resolution.X / 2)) (positionNdc.Y * single (resolution.Y / 2)) positionNdc.Z
        position2d

    /// Compute the relative 3d ray from the given absolute 2d position.
    /// TODO: also implement Position2dToPosition3d.
    member this.Position2dToRay3d (position : Vector3, eyeCenter : Vector3, eyeRotation : Quaternion, eyeFieldOfView, resolution : Vector2i) =
        let view = this.View3d (eyeCenter, eyeRotation)
        let projection = this.Projection3d eyeFieldOfView
        let viewProjectionInverse = (view * projection).Inverted
        let positionNdc = v3 (position.X / single (resolution.X * 2)) (position.Y / single (resolution.Y * 2)) 0.0f
        let positionViewProjection = positionNdc.Transform viewProjectionInverse
        let positionView = Vector4 (positionViewProjection.X, positionViewProjection.Y, -1.0f, 0.0f)
        let position3d = (positionView.Transform (Matrix4x4.CreateFromQuaternion eyeRotation.Inverted)).V3
        let rayDirection = (position3d - eyeCenter).Normalized
        let ray = Ray3 (eyeCenter, rayDirection)
        ray

    /// Transform the given mouse position to 2d screen space.
    member this.MouseTo2dScreen (mousePosition : Vector2, _ : Vector2, eyeSize : Vector2) =
        v2
            +(mousePosition.X / single this.DisplayVirtualScalar - eyeSize.X * 0.5f)
            -(mousePosition.Y / single this.DisplayVirtualScalar - eyeSize.Y * 0.5f) // negation for right-handedness

    /// Transform the given mouse position to 2d world space.
    member this.MouseToWorld2d (absolute, mousePosition, eyeCenter : Vector2, eyeSize : Vector2) =
        let mouseScreen = this.MouseTo2dScreen (mousePosition, eyeCenter, eyeSize)
        let view = if absolute then Matrix4x4.Identity else Matrix4x4.CreateTranslation eyeCenter.V3
        (mouseScreen.V3.Transform view).V2

    /// Transform the given mouse position to 2d entity space (eye 2d coordinates).
    member this.MouseToEntity2d (absolute, mousePosition, entityPosition, entitySize) =
        let mouseWorld = this.MouseToWorld2d (absolute, mousePosition, entityPosition, entitySize)
        entityPosition - mouseWorld

    /// Compute the 3d view matrix.
    member this.View3d (eyeCenter : Vector3, eyeRotation : Quaternion) : Matrix4x4 =
        (Matrix4x4.CreateFromQuaternion eyeRotation * Matrix4x4.CreateTranslation eyeCenter).Inverted

    /// Compute the 3d projection matrix.
    member this.Projection3d (fieldOfView : single) : Matrix4x4 =
        Matrix4x4.CreatePerspectiveFieldOfView
            (fieldOfView,
             this.AspectRatio,
             this.DistanceNear,
             this.DistanceFar)

    /// Compute a 3d view projection matrix.
    member this.ViewProjection3d (eyeCenter, eyeRotation, eyeFieldOfView) =
        let view = this.View3d (eyeCenter, eyeRotation)
        let projection = this.Projection3d eyeFieldOfView
        view * projection

    /// Compute a 3d view frustum.
    member this.Frustum (eyeCenter, eyeRotation : Quaternion, eyeFieldOfView : single) =
        let view = this.View3d (eyeCenter, eyeRotation)
        let projection = this.Projection3d eyeFieldOfView
        let viewProjection = view * projection
        Frustum viewProjection

    /// Transform the given mouse position to screen (normalized device coordinates).
    member this.MouseToScreen3d (mousePosition : Vector2) =
        v2
            (mousePosition.X / single this.DisplayResolution.X)
            (1.0f - (mousePosition.Y / single this.DisplayResolution.Y)) // inversion for right-handedness

    /// Transform the given mouse position to 3d world space.
    member this.MouseToWorld3d (mousePosition : Vector2, eyeCenter, eyeRotation, eyeFieldOfView) =
        let viewProjection = this.ViewProjection3d (v3Zero, eyeRotation, eyeFieldOfView)
        let near = this.Unproject (mousePosition.V3.WithZ 0.0f, viewProjection)
        let far = this.Unproject (mousePosition.V3.WithZ 1.0f, viewProjection)
        ray3 (near + eyeCenter) (far - near).Normalized

    static member private withinEpsilon (a : single) (b : single) =
        let c = a - b
        -Single.Epsilon <= c && c <= Single.Epsilon

    static member make distanceNear distanceFar bounds =
        { DistanceNear = distanceNear
          DistanceFar = distanceFar
          Bounds = bounds
          DisplayVirtualScalar = Viewport.DisplayVirtualScalar
          SsaoResolutionDivisor = Constants.Render.SsaoResolutionDivisor }

    static member makeDisplay () =
        let viewport = Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent box2iZero
        { viewport with Bounds = box2i v2iZero viewport.DisplayResolution }

    static member makeInterior () =
        let viewport = Viewport.make Constants.Render.NearPlaneDistanceInterior Constants.Render.FarPlaneDistanceInterior box2iZero
        { viewport with Bounds = box2i v2iZero viewport.DisplayResolution }

    static member makeExterior () =
        let viewport = Viewport.make Constants.Render.NearPlaneDistanceExterior Constants.Render.FarPlaneDistanceExterior box2iZero
        { viewport with Bounds = box2i v2iZero viewport.DisplayResolution }

    static member makeImposter () =
        let viewport = Viewport.make Constants.Render.NearPlaneDistanceImposter Constants.Render.FarPlaneDistanceImposter box2iZero
        { viewport with Bounds = box2i v2iZero viewport.DisplayResolution }