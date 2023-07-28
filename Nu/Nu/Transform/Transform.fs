// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open FSharp.NativeInterop
open Prime
open Nu

/// Masks for Transform flags.
module TransformMasks =

    let [<Literal>] ActiveMask =                    0b00000000000000000000000001u
    let [<Literal>] DirtyMask =                     0b00000000000000000000000010u
    let [<Literal>] InvalidatedMask =               0b00000000000000000000000100u
    let [<Literal>] AbsoluteMask =                  0b00000000000000000000001000u
    let [<Literal>] ImperativeMask =                0b00000000000000000000010000u
    let [<Literal>] EnabledMask =                   0b00000000000000000000100000u
    let [<Literal>] VisibleMask =                   0b00000000000000000001000000u
    let [<Literal>] PickableMask =                  0b00000000000000000010000000u
    let [<Literal>] AlwaysUpdateMask =              0b00000000000000000100000000u
    let [<Literal>] PublishChangeEventsMask =       0b00000000000000001000000000u
    let [<Literal>] PublishPreUpdatesMask =         0b00000000000000010000000000u
    let [<Literal>] PublishUpdatesMask =            0b00000000000000100000000000u
    let [<Literal>] PublishPostUpdatesMask =        0b00000000000001000000000000u
    let [<Literal>] PublishRendersMask =            0b00000000000010000000000000u
    let [<Literal>] ProtectedMask =                 0b00000000000100000000000000u
    let [<Literal>] PersistentMask =                0b00000000001000000000000000u
    let [<Literal>] MountedMask =                   0b00000000010000000000000000u
    let [<Literal>] EnabledLocalMask =              0b00000000100000000000000000u
    let [<Literal>] VisibleLocalMask =              0b00000001000000000000000000u
    let [<Literal>] CenteredMask =                  0b00000010000000000000000000u
    let [<Literal>] StaticMask =                    0b00000100000000000000000000u
    let [<Literal>] LightProbeMask =                0b00001000000000000000000000u
    let [<Literal>] LightMask =                     0b00010000000000000000000000u
    let [<Literal>] AnglesDirtyMask =               0b00100000000000000000000000u
    let [<Literal>] RotationMatrixDirtyMask =       0b01000000000000000000000000u
    let [<Literal>] PerimeterOrientedDirtyMask =    0b10000000000000000000000000u
    let [<Literal>] FlagsDefault =                  0b11000011101000000011110001u

// NOTE: opening masks for succintness.
open TransformMasks

/// Carries transformation data specific to an Entity.
type [<NoEquality; NoComparison>] Transform =
    struct
        val mutable private Flags_ : uint
        val mutable private Position_ : Vector3
        val mutable private Rotation_ : Quaternion
        val mutable private Scale_ : Vector3
        val mutable private Offset_ : Vector3
        val mutable private RotationMatrixOpt_ : Matrix4x4 ref
        val mutable private PerimeterOrientedOpt_ : Box3 ref
        val mutable private Angles_ : Vector3
        val mutable private Size_ : Vector3
        val mutable private Elevation_ : single
        val mutable private Overflow_ : single
        val mutable private Presence_ : Presence
        end

    member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask
    member this.Dirty with get () = this.Flags_ &&& DirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| DirtyMask else this.Flags_ &&& ~~~DirtyMask
    member this.Invalidated with get () = this.Flags_ &&& InvalidatedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| InvalidatedMask else this.Flags_ &&& ~~~InvalidatedMask
    member this.Imperative with get () = this.Flags_ &&& ImperativeMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ImperativeMask else this.Flags_ &&& ~~~ImperativeMask
    member this.PublishChangeEvents with get () = this.Flags_ &&& PublishChangeEventsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeEventsMask else this.Flags_ &&& ~~~PublishChangeEventsMask
    member this.Enabled with get () = this.Flags_ &&& EnabledMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledMask else this.Flags_ &&& ~~~EnabledMask
    member this.Visible with get () = this.Flags_ &&& VisibleMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleMask else this.Flags_ &&& ~~~VisibleMask
    member this.Pickable with get () = this.Flags_ &&& PickableMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PickableMask else this.Flags_ &&& ~~~PickableMask
    member this.AlwaysUpdate with get () = this.Flags_ &&& AlwaysUpdateMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AlwaysUpdateMask else this.Flags_ &&& ~~~AlwaysUpdateMask
    member this.PublishPreUpdates with get () = this.Flags_ &&& PublishPreUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishPreUpdatesMask else this.Flags_ &&& ~~~PublishPreUpdatesMask
    member this.PublishUpdates with get () = this.Flags_ &&& PublishUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishUpdatesMask else this.Flags_ &&& ~~~PublishUpdatesMask
    member this.PublishPostUpdates with get () = this.Flags_ &&& PublishPostUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishPostUpdatesMask else this.Flags_ &&& ~~~PublishPostUpdatesMask
    member this.PublishRenders with get () = this.Flags_ &&& PublishRendersMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishRendersMask else this.Flags_ &&& ~~~PublishRendersMask
    member this.Protected with get () = this.Flags_ &&& ProtectedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ProtectedMask else this.Flags_ &&& ~~~ProtectedMask
    member this.Persistent with get () = this.Flags_ &&& PersistentMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PersistentMask else this.Flags_ &&& ~~~PersistentMask
    member this.Mounted with get () = this.Flags_ &&& MountedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| MountedMask else this.Flags_ &&& ~~~MountedMask
    member this.EnabledLocal with get () = this.Flags_ &&& EnabledLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledLocalMask else this.Flags_ &&& ~~~EnabledLocalMask
    member this.VisibleLocal with get () = this.Flags_ &&& VisibleLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleLocalMask else this.Flags_ &&& ~~~VisibleLocalMask
    member this.Static with get () = this.Flags_ &&& StaticMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| StaticMask else this.Flags_ &&& ~~~StaticMask
    member this.LightProbe with get () = this.Flags_ &&& LightProbeMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| LightProbeMask else this.Flags_ &&& ~~~LightProbeMask
    member this.Light with get () = this.Flags_ &&& LightMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| LightMask else this.Flags_ &&& ~~~LightMask
    member this.RotationMatrixDirty with get () = this.Flags_ &&& RotationMatrixDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| RotationMatrixDirtyMask else this.Flags_ &&& ~~~RotationMatrixDirtyMask
    member this.PerimeterOrientedDirty with get () = this.Flags_ &&& PerimeterOrientedDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PerimeterOrientedDirtyMask else this.Flags_ &&& ~~~PerimeterOrientedDirtyMask
    member this.AnglesDirty with get () = this.Flags_ &&& AnglesDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AnglesDirtyMask else this.Flags_ &&& ~~~AnglesDirtyMask
    member this.Optimized with get () = this.Imperative && this.Presence_.OmnipresentType && not this.PublishChangeEvents

    member this.Position with get () = this.Position_ and set value = this.Position_ <- value; this.PerimeterOrientedDirty <- true
    member this.Scale with get () = this.Scale_ and set value = this.Scale_ <- value; this.PerimeterOrientedDirty <- true
    member this.Offset with get () = this.Offset_ and set value = this.Offset_ <- value; this.PerimeterOrientedDirty <- true
    member this.Size with get () = this.Size_ and set value = this.Size_ <- value; this.PerimeterOrientedDirty <- true
    member this.Elevation with get () = this.Elevation_ and set value = this.Elevation_ <- value
    member this.Overflow with get () = this.Overflow_ and set value = this.Overflow_ <- value; this.PerimeterOrientedDirty <- true

    member this.Absolute
        with get () = this.Flags_ &&& AbsoluteMask <> 0u
        and set value =
            this.Flags_ <- if value then this.Flags_ ||| AbsoluteMask else this.Flags_ &&& ~~~AbsoluteMask
            if this.Absolute then // setting a transform to Absolute requires that it also be Omnipresent
                this.Presence_ <- Omnipresent

    member this.Centered
        with get () = this.Flags_ &&& CenteredMask <> 0u
        and set value =
            this.Flags_ <- if value then this.Flags_ ||| CenteredMask else this.Flags_ &&& ~~~CenteredMask
            this.PerimeterOrientedDirty <- true

    member this.Presence
        with get () = this.Presence_
        and set (value : Presence) =
            let omnipresent = value.OmnipresentType
            if omnipresent || not this.Absolute then // a transform that is Absolute must remain Omnipresent
                this.Presence_ <- if omnipresent then Omnipresent else value

    member this.Rotation
        with get () = this.Rotation_
        and set value =
            this.Rotation_ <- value
            this.RotationMatrixDirty <- true
            this.PerimeterOrientedDirty <- true
            this.AnglesDirty <- true

    member this.Angles
        with get () =
            this.CleanAngles ()
            this.Angles_
        and set (value : Vector3) =
            this.Angles_ <- value
            this.AnglesDirty <- false
            this.Rotation_ <- value.RollPitchYaw
            this.RotationMatrixDirty <- true
            this.PerimeterOrientedDirty <- true

    member this.Degrees
        with get () =
            Math.radiansToDegrees3d this.Angles
        and set value =
            this.Angles <- Math.degreesToRadians3d value

    member this.RotationMatrix =
        this.CleanRotationMatrix ()
        this.RotationMatrixOpt_.Value

    member this.AffineMatrix =
        let mutable affineMatrix = this.RotationMatrix
        affineMatrix.M11 <- affineMatrix.M11 * this.Scale_.X
        affineMatrix.M12 <- affineMatrix.M12 * this.Scale_.X
        affineMatrix.M13 <- affineMatrix.M13 * this.Scale_.X
        affineMatrix.M21 <- affineMatrix.M21 * this.Scale_.Y
        affineMatrix.M22 <- affineMatrix.M22 * this.Scale_.Y
        affineMatrix.M23 <- affineMatrix.M23 * this.Scale_.Y
        affineMatrix.M31 <- affineMatrix.M31 * this.Scale_.Z
        affineMatrix.M32 <- affineMatrix.M32 * this.Scale_.Z
        affineMatrix.M33 <- affineMatrix.M33 * this.Scale_.Z
        affineMatrix.Translation <- this.Position_
        affineMatrix

    member this.Right = Vector3 (this.RotationMatrix.M11, this.RotationMatrix.M12, this.RotationMatrix.M13) // TODO: implement Row properties.
    member this.Up = Vector3 (this.RotationMatrix.M21, this.RotationMatrix.M22, this.RotationMatrix.M23)
    member this.Forward = -Vector3 (this.RotationMatrix.M31, this.RotationMatrix.M32, this.RotationMatrix.M33)
    member this.Left = -this.Right
    member this.Down = -this.Up
    member this.Back = -this.Forward

    member this.Center
        with get () =
            let perimeter = this.Perimeter
            perimeter.Center
        and set (value : Vector3) =
            let delta = value - this.Center
            this.Position <- this.Position + delta

    member this.Bottom
        with get () =
            let perimeter = this.Perimeter
            perimeter.Bottom
        and set (value : Vector3) =
            let delta = value - this.Bottom
            this.Position <- this.Position + delta

    member this.BottomLeft
        with get () =
            let perimeter = this.Perimeter
            perimeter.BottomLeft
        and set (value : Vector3) =
            let delta = value - this.BottomLeft
            this.Position <- this.Position + delta

    member this.Min
        with get () =
            let perimeter = this.Perimeter
            perimeter.Min
        and set (value : Vector3) =
            let delta = value - this.Min
            this.Position <- this.Position + delta

    member this.Max
        with get () =
            let perimeter = this.Perimeter
            perimeter.Max
        and set (value : Vector3) =
            let delta = value - this.Max
            this.Position <- this.Position + delta

    member this.PerimeterUnscaled
        with get () =
            let perimeterUnscaledOffset = if this.Centered then this.Offset_ - v3UncenteredOffset else this.Offset_
            Box3 (this.Position_ + perimeterUnscaledOffset * this.Size_, this.Size_)
        and set (value : Box3) =
            let perimeterUnscaledOffset = if this.Centered then this.Offset_ - v3UncenteredOffset else this.Offset_
            this.Position <- value.Min - perimeterUnscaledOffset * value.Size
            this.Size <- value.Size

    member this.Perimeter
        with get () : Box3 =
            let scale = this.Scale_
            let sizeScaled = this.Size_ * scale
            let perimeterUnscaledOffset = if this.Centered then this.Offset_ - v3UncenteredOffset else this.Offset_
            Box3 (this.Position_ + perimeterUnscaledOffset * sizeScaled, sizeScaled)
        and set (value : Box3) =
            this.Scale_ <- Vector3.One
            this.PerimeterUnscaled <- value

    member this.PerimeterOriented =
        this.CleanPerimeterOriented ()
        this.PerimeterOrientedOpt_.Value

    member this.Bounds =
        let perimeterOriented = this.PerimeterOriented
        let sizeOverflowed = perimeterOriented.Size * this.Overflow_
        let center = perimeterOriented.Center
        let positionOverflowed = center - sizeOverflowed * 0.5f
        Box3 (positionOverflowed, sizeOverflowed)

    member this.HorizonUnscaled =
        if this.Centered
        then this.PerimeterUnscaled.Center.Y
        else this.PerimeterUnscaled.Bottom.Y

    member this.Horizon =
        if this.Centered
        then this.Perimeter.Center.Y
        else this.Perimeter.Bottom.Y

    member this.HorizonOriented =
        if this.Centered
        then this.PerimeterOriented.Center.Y
        else this.PerimeterOriented.Bottom.Y

    member this.Pivot =
        let perimeter = this.Perimeter
        -perimeter.Center + perimeter.Size * this.Offset_

    member this.CleanAngles () =
        if this.AnglesDirty then
            let rollPitchYaw = this.Rotation_.RollPitchYaw
            this.Angles_.X <- rollPitchYaw.X
            this.Angles_.Y <- rollPitchYaw.Y
            this.Angles_.Z <- rollPitchYaw.Z
            this.AnglesDirty <- false

    member this.CleanRotationMatrix () =
        if isNull (this.RotationMatrixOpt_ :> obj) || this.RotationMatrixDirty then
            this.RotationMatrixOpt_ <- ref (Matrix4x4.CreateFromQuaternion this.Rotation_)
            this.RotationMatrixDirty <- false

    member this.CleanPerimeterOriented () =
        if isNull (this.PerimeterOrientedOpt_ :> obj) || this.PerimeterOrientedDirty then
            let perimeterOriented =
                let perimeter = this.Perimeter
                let rotation = this.Rotation_
                if not rotation.IsIdentity then
                    let pivot = this.Pivot
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
                        corner <- Vector3.Transform (corner + pivot, rotation) - pivot
                        minX <- Operators.min minX corner.X
                        minY <- Operators.min minY corner.Y
                        minZ <- Operators.min minZ corner.Z
                        maxX <- Operators.max maxX corner.X
                        maxY <- Operators.max maxY corner.Y
                        maxZ <- Operators.max maxZ corner.Z
                    Box3 (minX, minY, minZ, maxX - minX, maxY - minY, maxZ - minZ)
                else perimeter
            this.PerimeterOrientedOpt_ <- ref perimeterOriented
            this.PerimeterOrientedDirty <- false

    member this.Snap (positionSnap, degreesSnap, scaleSnap) =
        this.Position <- Math.snapF3d positionSnap this.Position
        this.Degrees <- Math.snapF3d degreesSnap this.Degrees
        this.Scale <- Math.snapF3d scaleSnap this.Scale

    member this.InvalidateFast () =
        this.Flags_ <- this.Flags_ ||| TransformMasks.InvalidatedMask

    /// Test transforms for equality.
    static member equalsByRef (left : Transform inref, right : Transform inref) =
        left.Flags_ = right.Flags_ &&
        left.Position_.Equals right.Position_ &&
        left.Rotation_.Equals right.Rotation_ &&
        left.Scale_.Equals right.Scale_ &&
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
        if source.Flags_ &&& RotationMatrixDirtyMask = 0u then target.RotationMatrixOpt_ <- ref source.RotationMatrixOpt_.Value; target.RotationMatrixDirty <- false
        if source.Flags_ &&& PerimeterOrientedDirtyMask = 0u then target.PerimeterOrientedOpt_ <- ref source.PerimeterOrientedOpt_.Value; target.PerimeterOrientedDirty <- false
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
    static member makeDefault centered =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- FlagsDefault
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- Vector3.One
        transform.Size_ <- Vector3.One
        transform.Overflow_ <- 1.0f
        transform.Centered <- centered
        transform

    /// Make a transform based on a perimeter.
    static member makePerimeter (perimeter : Box3) offset elevation absolute centered =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- FlagsDefault ||| if absolute then AbsoluteMask else 0u
        transform.Position_ <- if centered then perimeter.Center else perimeter.Min
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- v3One
        transform.Offset_ <- offset
        transform.Size_ <- perimeter.Size
        transform.Angles_ <- v3Zero
        transform.Elevation_ <- elevation
        transform.Centered <- centered
        transform

    /// Make a transform based on human-intuited values.
    static member makeIntuitive position scale offset size angles elevation absolute centered =
        let mutable transform = Transform.makeDefault centered
        transform.Flags_ <- FlagsDefault ||| if absolute then AbsoluteMask else 0u
        transform.Position_ <- position
        transform.Scale_ <- scale
        transform.Offset_ <- offset
        transform.Size_ <- size
        transform.Elevation_ <- elevation
        transform.Angles <- angles
        transform

    interface Transform Ecs.Component with
        member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask