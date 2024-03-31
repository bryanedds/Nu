// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

[<AutoOpen>]
module Viewport =

    /// Viewport extensions.
    type Viewport with

        /// Compute the 2d absolute view matrix.
        member this.View2dAbsolute (_ : Vector2, eyeSize : Vector2) =
            let translation = eyeSize * 0.5f * Constants.Render.VirtualScalar2
            Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

        /// Compute the 2d relative view matrix.
        member this.ViewRelative2d (eyeCenter : Vector2, eyeSize : Vector2) =
            let translation = -eyeCenter * Constants.Render.VirtualScalar2 + eyeSize * 0.5f * Constants.Render.VirtualScalar2
            Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

        /// Compute a 2d view matrix.
        member this.View2d (absolute, eyeCenter, eyeSize) =
            if absolute
            then this.View2dAbsolute (eyeCenter, eyeSize)
            else this.ViewRelative2d (eyeCenter, eyeSize)

        /// Compute the 2d projection matrix.
        member this.Projection2d =
            Matrix4x4.CreateOrthographicOffCenter
                (single (this.Bounds.Min.X),
                 single (this.Bounds.Min.X + this.Bounds.Size.X),
                 single (this.Bounds.Min.Y),
                 single (this.Bounds.Min.Y + this.Bounds.Size.Y),
                 -1.0f, 1.0f)

        /// Compute the 2d view projection matrix.
        member this.ViewProjection2d (absolute, eyeCenter, eyeSize) =
            let view = this.View2d (absolute, eyeCenter, eyeSize)
            let projection = this.Projection2d
            view * projection

        member this.Position3dToPosition2d (position : Vector3, eyeCenter, eyeRotation : Quaternion) =
            let eyeTarget = eyeCenter + Vector3.Transform (v3Forward, eyeRotation)
            let view = Matrix4x4.CreateLookAt (eyeCenter, eyeTarget, v3Up)
            let projection = this.Projection3d (Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent)
            let viewProjection : Matrix4x4 = view * projection
            let positionViewProjection = Vector4.Transform (Vector4 (position, 1.0f), viewProjection)
            let positionNdc = positionViewProjection.V3 / positionViewProjection.W
            let position2d =
                v3
                    (positionNdc.X * single (Constants.Render.VirtualResolutionX / 2))
                    (positionNdc.Y * single (Constants.Render.VirtualResolutionY / 2))
                    positionNdc.Z
            position2d

        /// Transform the given mouse position to 2d screen space.
        member this.MouseTo2dScreen (mousePosition : Vector2, _ : Vector2, eyeSize : Vector2) =
            v2
                +(mousePosition.X / single Constants.Render.VirtualScalar - eyeSize.X * 0.5f)
                -(mousePosition.Y / single Constants.Render.VirtualScalar - eyeSize.Y * 0.5f) // negation for right-handedness

        /// Transform the given mouse position to 2d world space.
        member this.MouseToWorld2d (absolute, mousePosition, eyeCenter : Vector2, eyeSize : Vector2) =
            let mouseScreen = this.MouseTo2dScreen (mousePosition, eyeCenter, eyeSize)
            let view =
                if absolute
                then Matrix4x4.Identity
                else Matrix4x4.CreateTranslation eyeCenter.V3
            (Vector3.Transform (mouseScreen.V3, view)).V2

        /// Transform the given mouse position to 2d entity space (eye 2d coordinates).
        member this.MouseToEntity2d (absolute, mousePosition, entityPosition, entitySize) =
            let mouseWorld = this.MouseToWorld2d (absolute, mousePosition, entityPosition, entitySize)
            entityPosition - mouseWorld

        /// Compute the 3d absolute view matrix.
        member this.ViewAbsolute3d (_ : Vector3, _ : Quaternion) =
            m4Identity

        /// Compute the 3d relative view matrix.
        member this.ViewRelative3d (eyeCenter : Vector3, eyeRotation : Quaternion) =
            let eyeTarget = eyeCenter + Vector3.Transform (v3Forward, eyeRotation)
            Matrix4x4.CreateLookAt (eyeCenter, eyeTarget, v3Up)

        /// Compute a 3d view matrix.
        member this.View3d (absolute, eyeCenter, eyeRotation) =
            if absolute
            then this.ViewAbsolute3d (eyeCenter, eyeRotation)
            else this.ViewRelative3d (eyeCenter, eyeRotation)

        /// Compute the 3d projection matrix.
        member this.Projection3d (nearPlaneDistance, farPlaneDistance) =
            Matrix4x4.CreatePerspectiveFieldOfView
                (Constants.Render.FieldOfView,
                 this.AspectRatio,
                 nearPlaneDistance,
                 farPlaneDistance)

        /// Compute a 3d view projection matrix.
        member this.ViewProjection3d (absolute, nearPlaneDistance, farPlaneDistance, eyeCenter, eyeRotation) =
            let view = this.View3d (absolute, eyeCenter, eyeRotation)
            let projection = this.Projection3d (nearPlaneDistance, farPlaneDistance)
            view * projection

        /// Compute a 3d view frustum.
        member this.Frustum (nearPlaneDistance, farPlaneDistance, eyeCenter, eyeRotation : Quaternion) =
            let eyeTarget = eyeCenter + Vector3.Transform (v3Forward, eyeRotation)
            let view = Matrix4x4.CreateLookAt (eyeCenter, eyeTarget, v3Up)
            let projection = this.Projection3d (nearPlaneDistance, farPlaneDistance)
            let viewProjection = view * projection
            Frustum viewProjection

        /// Transform the given mouse position to 3d screen space (normalized device coordinates).
        member this.MouseToScreen3d (mousePosition : Vector2) =
            v2
                (mousePosition.X / single Constants.Render.ResolutionX)
                (1.0f - (mousePosition.Y / single Constants.Render.ResolutionY)) // inversion for right-handedness

        /// Transform the given mouse position to 3d world space.
        member this.MouseToWorld3d (absolute, mousePosition : Vector2, eyeCenter, eyeRotation) =
            let viewProjection = this.ViewProjection3d (absolute, Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent, v3Zero, eyeRotation)
            let near = this.Unproject (mousePosition.V3.WithZ 0.0f, viewProjection)
            let far = this.Unproject (mousePosition.V3.WithZ 1.0f, viewProjection)
            ray (near + eyeCenter) (Vector3.Normalize (far - near))