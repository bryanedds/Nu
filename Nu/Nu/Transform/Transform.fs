﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open FSharp.NativeInterop
open Prime

/// Masks for Transform flags.
module TransformMasks =

    let [<Literal>] ActiveMask =                    0b000000000000000000001u // for use as a component in an ECS or other data-oriented context
    let [<Literal>] DirtyMask =                     0b000000000000000000010u // for use as a component in an ECS or other data-oriented context
    let [<Literal>] InvalidatedMask =               0b000000000000000000100u
    let [<Literal>] AbsoluteMask =                  0b000000000000000001000u
    let [<Literal>] ImperativeMask =                0b000000000000000010000u
    let [<Literal>] EnabledMask =                   0b000000000000000100000u
    let [<Literal>] VisibleMask =                   0b000000000000001000000u
    let [<Literal>] CastShadowMask =                0b000000000000010000000u
    let [<Literal>] PickableMask =                  0b000000000000100000000u
    let [<Literal>] AlwaysUpdateMask =              0b000000000001000000000u
    let [<Literal>] AlwaysRenderMask =              0b000000000010000000000u
    let [<Literal>] PublishChangeEventsMask =       0b000000000100000000000u
    let [<Literal>] PublishUpdatesMask =            0b000000001000000000000u
    let [<Literal>] ProtectedMask =                 0b000000010000000000000u
    let [<Literal>] PersistentMask =                0b000000100000000000000u
    let [<Literal>] MountedMask =                   0b000001000000000000000u
    let [<Literal>] EnabledLocalMask =              0b000010000000000000000u
    let [<Literal>] VisibleLocalMask =              0b000100000000000000000u
    let [<Literal>] StaticMask =                    0b001000000000000000000u
    let [<Literal>] AnglesDirtyMask =               0b010000000000000000000u
    let [<Literal>] RotationMatrixDirtyMask =       0b100000000000000000000u
    let [<Literal>] FlagsDefault =                  0b100110100000111110001u

// opening masks for succinctness
open TransformMasks

/// Carries transformation data specific to an Entity.
type [<NoEquality; NoComparison>] Transform =
    struct
        val mutable private Flags_ : uint
        val mutable private Position_ : Vector3
        val mutable private Rotation_ : Quaternion
        val mutable private Scale_ : Vector3
        val mutable private Offset_ : Vector3
        val mutable private RotationMatrixOpt_ : Matrix4x4
        val mutable private Angles_ : Vector3
        val mutable private Size_ : Vector3
        val mutable private Elevation_ : single
        val mutable private Overflow_ : single
        val mutable private Presence_ : Presence
        val mutable private PresenceOverride_ : Presence voption
        end

    member this.Active                  with get () = this.Flags_ &&& ActiveMask <> 0u                  and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask
    member this.Dirty                   with get () = this.Flags_ &&& DirtyMask <> 0u                   and set value = this.Flags_ <- if value then this.Flags_ ||| DirtyMask else this.Flags_ &&& ~~~DirtyMask
    member this.Invalidated             with get () = this.Flags_ &&& InvalidatedMask <> 0u             and set value = this.Flags_ <- if value then this.Flags_ ||| InvalidatedMask else this.Flags_ &&& ~~~InvalidatedMask
    member this.Absolute                with get () = this.Flags_ &&& AbsoluteMask <> 0u                and set value = this.Flags_ <- if value then this.Flags_ ||| AbsoluteMask else this.Flags_ &&& ~~~AbsoluteMask
    member this.Imperative              with get () = this.Flags_ &&& ImperativeMask <> 0u              and set value = this.Flags_ <- if value then this.Flags_ ||| ImperativeMask else this.Flags_ &&& ~~~ImperativeMask
    member this.PublishChangeEvents     with get () = this.Flags_ &&& PublishChangeEventsMask <> 0u     and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeEventsMask else this.Flags_ &&& ~~~PublishChangeEventsMask
    member this.Enabled                 with get () = this.Flags_ &&& EnabledMask <> 0u                 and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledMask else this.Flags_ &&& ~~~EnabledMask
    member this.Visible                 with get () = this.Flags_ &&& VisibleMask <> 0u                 and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleMask else this.Flags_ &&& ~~~VisibleMask
    member this.CastShadow              with get () = this.Flags_ &&& CastShadowMask <> 0u              and set value = this.Flags_ <- if value then this.Flags_ ||| CastShadowMask else this.Flags_ &&& ~~~CastShadowMask
    member this.Pickable                with get () = this.Flags_ &&& PickableMask <> 0u                and set value = this.Flags_ <- if value then this.Flags_ ||| PickableMask else this.Flags_ &&& ~~~PickableMask
    member this.AlwaysUpdate            with get () = this.Flags_ &&& AlwaysUpdateMask <> 0u            and set value = this.Flags_ <- if value then this.Flags_ ||| AlwaysUpdateMask else this.Flags_ &&& ~~~AlwaysUpdateMask
    member this.AlwaysRender            with get () = this.Flags_ &&& AlwaysRenderMask <> 0u            and set value = this.Flags_ <- if value then this.Flags_ ||| AlwaysRenderMask else this.Flags_ &&& ~~~AlwaysRenderMask
    member this.PublishUpdates          with get () = this.Flags_ &&& PublishUpdatesMask <> 0u          and set value = this.Flags_ <- if value then this.Flags_ ||| PublishUpdatesMask else this.Flags_ &&& ~~~PublishUpdatesMask
    member this.Protected               with get () = this.Flags_ &&& ProtectedMask <> 0u               and set value = this.Flags_ <- if value then this.Flags_ ||| ProtectedMask else this.Flags_ &&& ~~~ProtectedMask
    member this.Persistent              with get () = this.Flags_ &&& PersistentMask <> 0u              and set value = this.Flags_ <- if value then this.Flags_ ||| PersistentMask else this.Flags_ &&& ~~~PersistentMask
    member this.Mounted                 with get () = this.Flags_ &&& MountedMask <> 0u                 and set value = this.Flags_ <- if value then this.Flags_ ||| MountedMask else this.Flags_ &&& ~~~MountedMask
    member this.EnabledLocal            with get () = this.Flags_ &&& EnabledLocalMask <> 0u            and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledLocalMask else this.Flags_ &&& ~~~EnabledLocalMask
    member this.VisibleLocal            with get () = this.Flags_ &&& VisibleLocalMask <> 0u            and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleLocalMask else this.Flags_ &&& ~~~VisibleLocalMask
    member this.Static                  with get () = this.Flags_ &&& StaticMask <> 0u                  and set value = this.Flags_ <- if value then this.Flags_ ||| StaticMask else this.Flags_ &&& ~~~StaticMask
    member this.RotationMatrixDirty     with get () = this.Flags_ &&& RotationMatrixDirtyMask <> 0u     and set value = this.Flags_ <- if value then this.Flags_ ||| RotationMatrixDirtyMask else this.Flags_ &&& ~~~RotationMatrixDirtyMask
    member this.AnglesDirty             with get () = this.Flags_ &&& AnglesDirtyMask <> 0u             and set value = this.Flags_ <- if value then this.Flags_ ||| AnglesDirtyMask else this.Flags_ &&& ~~~AnglesDirtyMask
    member this.Position                with get () = this.Position_                                    and set value = this.Position_ <- value
    member this.Scale                   with get () = this.Scale_                                       and set value = this.Scale_ <- value
    member this.Offset                  with get () = this.Offset_                                      and set value = this.Offset_ <- value
    member this.Size                    with get () = this.Size_                                        and set value = this.Size_ <- value
    member this.Elevation               with get () = this.Elevation_                                   and set value = this.Elevation_ <- value
    member this.Overflow                with get () = this.Overflow_                                    and set value = this.Overflow_ <- value
    member this.Presence                with get () = this.Presence_                                    and set value = this.Presence_ <- value
    member this.PresenceOverride        with get () = this.PresenceOverride_                            and set value = this.PresenceOverride_ <- value

    member this.Optimized =
        let presence = ValueOption.defaultValue this.Presence_ this.PresenceOverride
        this.Imperative &&
        presence.IsOmnipresent &&
        not this.PublishChangeEvents

    member this.Rotation
        with get () = this.Rotation_
        and set value =
            this.Rotation_ <- value
            this.RotationMatrixDirty <- true
            this.AnglesDirty <- true

    member this.Angles
        with get () =
            Transform.cleanAngles &this
            this.Angles_
        and set (value : Vector3) =
            this.Angles_ <- value
            this.AnglesDirty <- false
            this.Rotation_ <- value.RollPitchYaw
            this.RotationMatrixDirty <- true

    member this.Degrees
        with get () =
            Math.RadiansToDegrees3d this.Angles
        and set value =
            this.Angles <- Math.DegreesToRadians3d value

    member this.RotationMatrix =
        Transform.cleanRotationMatrixInternal &this
        this.RotationMatrixOpt_

    member this.AffineMatrix =
        let scale = this.Scale_
        let mutable affineMatrix = this.RotationMatrix
        affineMatrix.M11 <- affineMatrix.M11 * scale.X
        affineMatrix.M12 <- affineMatrix.M12 * scale.X
        affineMatrix.M13 <- affineMatrix.M13 * scale.X
        affineMatrix.M21 <- affineMatrix.M21 * scale.Y
        affineMatrix.M22 <- affineMatrix.M22 * scale.Y
        affineMatrix.M23 <- affineMatrix.M23 * scale.Y
        affineMatrix.M31 <- affineMatrix.M31 * scale.Z
        affineMatrix.M32 <- affineMatrix.M32 * scale.Z
        affineMatrix.M33 <- affineMatrix.M33 * scale.Z
        affineMatrix.Translation <- this.Position_
        affineMatrix

    member this.Right = Vector3 (this.RotationMatrix.M11, this.RotationMatrix.M12, this.RotationMatrix.M13) // TODO: implement row properties.
    member this.Up = Vector3 (this.RotationMatrix.M21, this.RotationMatrix.M22, this.RotationMatrix.M23)
    member this.Forward = -Vector3 (this.RotationMatrix.M31, this.RotationMatrix.M32, this.RotationMatrix.M33)
    member this.Left = -this.Right
    member this.Down = -this.Up
    member this.Back = -this.Forward

    member this.PerimeterCenter
        with get () =
            let perimeter = this.Perimeter
            perimeter.Center
        and set (value : Vector3) =
            let delta = value - this.PerimeterCenter
            this.Position <- this.Position + delta

    member this.PerimeterBottom
        with get () =
            let perimeter = this.Perimeter
            perimeter.Bottom
        and set (value : Vector3) =
            let delta = value - this.PerimeterBottom
            this.Position <- this.Position + delta

    member this.PerimeterBottomLeft
        with get () =
            let perimeter = this.Perimeter
            perimeter.BottomLeft
        and set (value : Vector3) =
            let delta = value - this.PerimeterBottomLeft
            this.Position <- this.Position + delta

    member this.PerimeterMin
        with get () =
            let perimeter = this.Perimeter
            perimeter.Min
        and set (value : Vector3) =
            let delta = value - this.PerimeterMin
            this.Position <- this.Position + delta

    member this.PerimeterMax
        with get () =
            let perimeter = this.Perimeter
            perimeter.Max
        and set (value : Vector3) =
            let delta = value - this.PerimeterMax
            this.Position <- this.Position + delta

    member this.PerimeterUnscaled
        with get () =
            let perimeterUnscaledOffset = this.Offset_ - v3UncenteredOffset
            Box3 (this.Position_ + perimeterUnscaledOffset * this.Size_, this.Size_)
        and set (value : Box3) =
            let perimeterUnscaledOffset = this.Offset_ - v3UncenteredOffset
            this.Position <- value.Min - perimeterUnscaledOffset * value.Size
            this.Size <- value.Size

    member this.Perimeter
        with get () : Box3 =
            let scale = this.Scale_
            let sizeScaled = this.Size_ * scale
            let perimeterUnscaledOffset = this.Offset_ - v3UncenteredOffset
            Box3 (this.Position_ + perimeterUnscaledOffset * sizeScaled, sizeScaled)
        and set (value : Box3) =
            this.Scale_ <- Vector3.One
            this.PerimeterUnscaled <- value

    member this.Bounds2d =
        let perimeterOriented =
            let perimeter = this.Perimeter
            let rotation = this.Rotation_
            if not rotation.IsIdentity then
                let pivot = this.PerimeterPivot
                let min = perimeter.Min
                let max = perimeter.Max
                let corners = NativePtr.stackalloc<Vector3> 8 // OPTIMIZATION: computing corners on the stack.
                NativePtr.set corners 0 (Vector3 (min.X, min.Y, min.Z))
                NativePtr.set corners 1 (Vector3 (min.X, min.Y, max.Z))
                NativePtr.set corners 2 (Vector3 (max.X, min.Y, max.Z))
                NativePtr.set corners 3 (Vector3 (max.X, min.Y, min.Z))
                NativePtr.set corners 4 (Vector3 (max.X, max.Y, max.Z))
                NativePtr.set corners 5 (Vector3 (min.X, max.Y, max.Z))
                NativePtr.set corners 6 (Vector3 (min.X, max.Y, min.Z))
                NativePtr.set corners 7 (Vector3 (max.X, max.Y, min.Z))
                let mutable minX = Single.MaxValue
                let mutable minY = Single.MaxValue
                let mutable minZ = Single.MaxValue
                let mutable maxX = Single.MinValue
                let mutable maxY = Single.MinValue
                let mutable maxZ = Single.MinValue
                for i in 0 .. 8 - 1 do
                    let mutable corner = NativePtr.get corners i
                    corner <- (corner + pivot).Transform rotation - pivot
                    minX <- Operators.min minX corner.X
                    minY <- Operators.min minY corner.Y
                    minZ <- Operators.min minZ corner.Z
                    maxX <- Operators.max maxX corner.X
                    maxY <- Operators.max maxY corner.Y
                    maxZ <- Operators.max maxZ corner.Z
                Box3 (minX, minY, minZ, maxX - minX, maxY - minY, maxZ - minZ)
            else perimeter
        let sizeOverflowed = perimeterOriented.Size * this.Overflow_
        let center = perimeterOriented.Center
        let positionOverflowed = center - sizeOverflowed * 0.5f
        Box3 (positionOverflowed, sizeOverflowed)

    member this.Bounds3d =
        let bounds =
            let position = this.Position_ + this.Offset * this.Scale_
            let size = this.Size_ * this.Scale_ * this.Overflow_
            Box3 (position - size * 0.5f, size)
        let rotation = this.Rotation_
        if not rotation.IsIdentity then
            let min = bounds.Min
            let max = bounds.Max
            let corners = NativePtr.stackalloc<Vector3> 8 // OPTIMIZATION: computing corners on the stack.
            NativePtr.set corners 0 (Vector3 (min.X, min.Y, min.Z))
            NativePtr.set corners 1 (Vector3 (min.X, min.Y, max.Z))
            NativePtr.set corners 2 (Vector3 (max.X, min.Y, max.Z))
            NativePtr.set corners 3 (Vector3 (max.X, min.Y, min.Z))
            NativePtr.set corners 4 (Vector3 (max.X, max.Y, max.Z))
            NativePtr.set corners 5 (Vector3 (min.X, max.Y, max.Z))
            NativePtr.set corners 6 (Vector3 (min.X, max.Y, min.Z))
            NativePtr.set corners 7 (Vector3 (max.X, max.Y, min.Z))
            let mutable minX = Single.MaxValue
            let mutable minY = Single.MaxValue
            let mutable minZ = Single.MaxValue
            let mutable maxX = Single.MinValue
            let mutable maxY = Single.MinValue
            let mutable maxZ = Single.MinValue
            for i in 0 .. 8 - 1 do
                let mutable corner = NativePtr.get corners i
                corner <- (corner - this.Position_).Transform rotation + this.Position_
                minX <- Operators.min minX corner.X
                minY <- Operators.min minY corner.Y
                minZ <- Operators.min minZ corner.Z
                maxX <- Operators.max maxX corner.X
                maxY <- Operators.max maxY corner.Y
                maxZ <- Operators.max maxZ corner.Z
            Box3 (minX, minY, minZ, maxX - minX, maxY - minY, maxZ - minZ)
        else bounds

    member this.HorizonUnscaled =
        this.PerimeterUnscaled.Center.Y

    member this.Horizon =
        this.Perimeter.Center.Y

    member this.PerimeterPivot =
        let perimeter = this.Perimeter
        -perimeter.Center + perimeter.Size * this.Offset_

    static member private cleanAngles (this : Transform byref) =
        if this.AnglesDirty then
            let rollPitchYaw = this.Rotation_.RollPitchYaw
            this.Angles_.X <- rollPitchYaw.X
            this.Angles_.Y <- rollPitchYaw.Y
            this.Angles_.Z <- rollPitchYaw.Z
            this.AnglesDirty <- false

    static member cleanRotationMatrixInternal (this : Transform byref) =
        if this.RotationMatrixDirty || this.RotationMatrixOpt_.IsZero then
            this.RotationMatrixOpt_ <- Matrix4x4.CreateFromQuaternion this.Rotation_
            this.RotationMatrixDirty <- false

    static member invalidateFastInternal (this : Transform byref) =
        this.Flags_ <- this.Flags_ ||| TransformMasks.InvalidatedMask

    static member snapPosition (positionSnap, transform : Transform byref) =
        transform.Position <- Math.SnapF3d (positionSnap, transform.Position)

    /// Test transforms for equality.
    static member equalsByRef (left : Transform inref, right : Transform inref) =
        left.Flags_ = right.Flags_ &&
        v3Eq left.Position_ right.Position_ &&
        quatEq left.Rotation_ right.Rotation_ &&
        v3EqApprox left.Scale_ right.Scale_ 0.0001f && // NOTE: using approx here since scale tends to be pulled from an affine matrix. Also, just guessing at espilon...
        left.Offset_.Equals right.Offset_ &&
        left.Size_.Equals right.Size_ &&
        left.Elevation_ = right.Elevation_ &&
        left.Overflow_ = right.Overflow_

    /// Test transforms for equality.
    static member inline equals (left : Transform) (right : Transform) =
        Transform.equalsByRef (&left, &right)

    /// Assign the value of the left transform to the right.
    static member assignByRef (source : Transform inref, target : Transform byref) =
        target.Flags_ <- source.Flags_
        target.Position_ <- source.Position_
        target.Rotation_ <- source.Rotation_
        target.Scale_ <- source.Scale_
        target.Offset_ <- source.Offset_
        if source.Flags_ &&& RotationMatrixDirtyMask = 0u then target.RotationMatrixOpt_ <- source.RotationMatrixOpt_; target.RotationMatrixDirty <- false
        target.Angles_ <- source.Angles_
        target.Size_ <- source.Size_
        target.Elevation_ <- source.Elevation_
        target.Overflow_ <- source.Overflow_

    /// Assign the value of the left transform to the right.
    static member inline assign (source : Transform byref, target : Transform byref) =
        Transform.assignByRef (&source, &target)

    /// Make an empty transform.
    static member inline makeEmpty () =
        Unchecked.defaultof<Transform>

    /// Make a transform with default values.
    static member makeDefault () =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- FlagsDefault
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- Vector3.One
        transform.Size_ <- Vector3.One
        transform.Overflow_ <- 1.0f
        transform

    /// Make a transform based on a perimeter.
    static member makePerimeter absolute (perimeter : Box3) offset elevation =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- FlagsDefault ||| if absolute then AbsoluteMask else 0u
        transform.Position_ <- perimeter.Center
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- v3One
        transform.Offset_ <- offset
        transform.Size_ <- perimeter.Size
        transform.Angles_ <- v3Zero
        transform.Elevation_ <- elevation
        transform.Overflow_ <- 1.0f
        transform

    /// Make a transform based on human-intuitive values.
    static member makeIntuitive absolute position scale offset size angles elevation =
        let mutable transform = Transform.makeDefault ()
        transform.Flags_ <- FlagsDefault ||| if absolute then AbsoluteMask else 0u
        transform.Position_ <- position
        transform.Scale_ <- scale
        transform.Offset_ <- offset
        transform.Size_ <- size
        transform.Elevation_ <- elevation
        transform.Angles <- angles
        transform.Overflow_ <- 1.0f
        transform