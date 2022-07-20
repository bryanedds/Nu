namespace Nu
open System.Numerics
open Prime
open Nu

/// Describes the form of an element's presence.
[<Syntax
        ("Enclosed Exposed Imposter Prominent Omnipresent", "", "", "", "",
         Constants.PrettyPrinter.DefaultThresholdMin,
         Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Presence =
    | Enclosed
    | Exposed
    | Imposter
    | Prominent // both exposed and imposter
    | Omnipresent
    member this.EnclosedType with get () = match this with Enclosed -> true | _ -> false
    member this.ExposedType with get () = match this with Exposed -> true | _ -> false
    member this.ImposterType with get () = match this with Imposter -> true | _ -> false
    member this.ProminentType with get () = match this with Prominent -> true | _ -> false
    member this.OmnipresentType with get () = match this with Omnipresent -> true | _ -> false
    member this.Cullable with get () = match this with Enclosed | Exposed -> true | _ -> false
    member this.Uncullable with get () = not this.Cullable

[<AutoOpen>]
module PresenceOperators =
    
    /// Test two presence values for equality.
    let presenceEq left right =
        match (left, right) with
        | (Enclosed, Enclosed)
        | (Exposed, Exposed)
        | (Imposter, Imposter)
        | (Prominent, Prominent)
        | (Omnipresent, Omnipresent) -> true
        | (_, _) -> false

    /// Test two presence values for inequality.
    let presenceNeq left right =
        not (presenceEq left right)

[<AutoOpen>]
module Viewport =

    type Viewport with

        /// Compute the 2d absolute view matrix.
        member this.View2dAbsolute (_ : Vector2, eyeSize : Vector2) =
            let translation = eyeSize * 0.5f * Constants.Render.VirtualScalar2
            Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

        /// Compute the 2d relative view matrix.
        member this.ViewRelative2d (eyePosition : Vector2, eyeSize : Vector2) =
            let translation = -eyePosition * Constants.Render.VirtualScalar2 + eyeSize * 0.5f * Constants.Render.VirtualScalar2
            Matrix4x4.CreateTranslation (v3 translation.X translation.Y 1.0f)

        /// Compute a 2d view matrix.
        member this.View2d (absolute, eyePosition, eyeSize) =
            if absolute
            then this.View2dAbsolute (eyePosition, eyeSize)
            else this.ViewRelative2d (eyePosition, eyeSize)

        /// Compute the 2d projection matrix.
        member this.Projection2d =
            Matrix4x4.CreateOrthographicOffCenter
                (single (this.Bounds.Position.X),
                 single (this.Bounds.Position.X + this.Bounds.Size.X),
                 single (this.Bounds.Position.Y),
                 single (this.Bounds.Position.Y + this.Bounds.Size.Y),
                 -1.0f, 1.0f)

        /// Compute the 2d view projection matrix.
        member this.ViewProjection2d (absolute, eyePosition, eyeSize) =
            let view = this.View2d (absolute, eyePosition, eyeSize)
            let projection = this.Projection2d
            view * projection

        /// Transform the given mouse position to 2d screen space.
        member this.MouseTo2dScreen (mousePosition : Vector2, _ : Vector2, eyeSize : Vector2) =
            v2
                +(mousePosition.X / single Constants.Render.VirtualScalar - eyeSize.X * 0.5f)
                -(mousePosition.Y / single Constants.Render.VirtualScalar - eyeSize.Y * 0.5f) // negation for right-handedness

        /// Transform the given mouse position to 2d world space.
        member this.MouseToWorld2d (absolute, mousePosition, eyePosition : Vector2, eyeSize : Vector2) =
            let mouseScreen = this.MouseTo2dScreen (mousePosition, eyePosition, eyeSize)
            let view =
                if absolute
                then Matrix4x4.Identity
                else Matrix4x4.CreateTranslation eyePosition.V3
            (Vector3.Transform (mouseScreen.V3, view)).V2

        /// Transform the given mouse position to 2d entity space (eye 2d coordinates).
        member this.MouseToEntity2d (absolute, mousePosition, entityPosition, entitySize) =
            let mouseWorld = this.MouseToWorld2d (absolute, mousePosition, entityPosition, entitySize)
            entityPosition - mouseWorld

        /// Compute the 3d absolute view matrix.
        member this.View3dAbsolute (_ : Vector3, _ : Quaternion) =
            m4Identity

        /// Compute the 3d relative view matrix.
        member this.ViewRelative3d (eyePosition : Vector3, eyeRotation : Quaternion) =
            let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
            Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)

        /// Compute a 3d view matrix.
        member this.View3d (absolute, eyePosition, eyeRotation) =
            if absolute
            then this.View3dAbsolute (eyePosition, eyeRotation)
            else this.ViewRelative3d (eyePosition, eyeRotation)

        /// Compute the 3d projection matrix.
        member this.Projection3d nearPlaneDistance farPlaneDistance =
            Matrix4x4.CreatePerspectiveFieldOfView
                (Constants.Render.FieldOfView,
                 this.AspectRatio,
                 nearPlaneDistance,
                 farPlaneDistance)

        /// Compute a 3d view projection matrix.
        member this.ViewProjection3d (absolute, nearPlaneDistance, farPlaneDistance, eyePosition, eyeSize) =
            let view = this.View3d (absolute, eyePosition, eyeSize)
            let projection = this.Projection3d nearPlaneDistance farPlaneDistance
            view * projection

        /// Compute a 3d view frustum.
        member this.Frustum (nearPlaneDistance, farPlaneDistance, eyePosition, eyeRotation : Quaternion) =
            let eyeTarget = eyePosition + Vector3.Transform (v3Forward, eyeRotation)
            let view = Matrix4x4.CreateLookAt (eyePosition, eyeTarget, v3Up)
            let projection = this.Projection3d nearPlaneDistance farPlaneDistance
            let viewProjection = view * projection
            Frustum viewProjection

        /// Transform the given mouse position to 3d screen space (normalized device coordinates).
        member this.MouseTo3dScreen (mousePosition : Vector2) =
            v2
                (mousePosition.X / single Constants.Render.ResolutionX)
                (1.0f - (mousePosition.Y / single Constants.Render.ResolutionY)) // inversion for right-handedness

        /// Transform the given mouse position to 3d world space.
        member this.MouseToWorld3d (absolute, mousePosition : Vector2, eyePosition, eyeRotation) =
            let viewProjection = this.ViewProjection3d (absolute, Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent, v3Zero, eyeRotation)
            let near = this.Unproject (mousePosition.V3.WithZ 0.0f, viewProjection)
            let far = this.Unproject (mousePosition.V3.WithZ 1.0f, viewProjection)
            ray (near + eyePosition) (Vector3.Normalize (far - near))