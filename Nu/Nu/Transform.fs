// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// Masks for Transform flags.
module TransformMasks =

    // OPTIMIZATION: Transform flag bit-masks for performance.
    let [<Literal>] ActiveMask =                    0b000000000000000000001u
    let [<Literal>] DirtyMask =                     0b000000000000000000010u
    let [<Literal>] InvalidatedMask =               0b000000000000000000100u
    let [<Literal>] OmnipresentMask =               0b000000000000000001000u
    let [<Literal>] AbsoluteMask =                  0b000000000000000010000u
    let [<Literal>] ImperativeMask =                0b000000000000000100000u
    let [<Literal>] PublishChangeBindingsMask =     0b000000000000001000000u
    let [<Literal>] PublishChangeEventsMask =       0b000000000000010000000u
    let [<Literal>] EnabledMask =                   0b000000000000100000000u
    let [<Literal>] VisibleMask =                   0b000000000001000000000u
    let [<Literal>] AlwaysUpdateMask =              0b000000000010000000000u
    let [<Literal>] PublishUpdatesMask =            0b000000000100000000000u
    let [<Literal>] PublishPostUpdatesMask =        0b000000001000000000000u
    let [<Literal>] PersistentMask =                0b000000010000000000000u
    let [<Literal>] IgnorePropertyBindingsMask =    0b000000100000000000000u
    let [<Literal>] MountedMask =                   0b000001000000000000000u
    let [<Literal>] EnabledLocalMask =              0b000010000000000000000u
    let [<Literal>] VisibleLocalMask =              0b000100000000000000000u
    let [<Literal>] RotationMatrixDirtyMask =       0b001000000000000000000u
    let [<Literal>] AffineMatrixDirtyMask =         0b010000000000000000000u
    let [<Literal>] DefaultFlags =                  0b000110010001100100001u

// NOTE: opening this in order to make the Transform property implementations reasonably succinct.
open TransformMasks

/// Carries transformation data specific to an Entity.
type [<NoEquality; NoComparison>] Transform =
    struct
        // cache line 1
        val mutable private Flags_ : uint
        val mutable private Position_ : Vector3
        val mutable private Rotation_ : Quaternion
        // cache line 2
        val mutable private Scale_ : Vector3
        val mutable private Offset_ : Vector3
        val mutable private RotationMatrixOpt_ : Matrix4x4 ref
        val mutable private AffineMatrixOpt_ : Matrix4x4 ref
        // cache line 3
        val mutable private Angles_ : Vector3
        val mutable private Size_ : Vector3
        val mutable private Overflow_ : single
        val mutable private Elevation_ : single
        end

    member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask
    member this.Dirty with get () = this.Flags_ &&& DirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| DirtyMask else this.Flags_ &&& ~~~DirtyMask
    member this.Invalidated with get () = this.Flags_ &&& InvalidatedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| InvalidatedMask else this.Flags_ &&& ~~~InvalidatedMask
    member this.Omnipresent with get () = this.Flags_ &&& OmnipresentMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| OmnipresentMask else this.Flags_ &&& ~~~OmnipresentMask
    member this.Absolute with get () = this.Flags_ &&& AbsoluteMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AbsoluteMask else this.Flags_ &&& ~~~AbsoluteMask
    member this.Imperative with get () = this.Flags_ &&& ImperativeMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ImperativeMask else this.Flags_ &&& ~~~ImperativeMask
    member this.PublishChangeBindings with get () = this.Flags_ &&& PublishChangeBindingsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeBindingsMask else this.Flags_ &&& ~~~PublishChangeBindingsMask
    member this.PublishChangeEvents with get () = this.Flags_ &&& PublishChangeEventsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeEventsMask else this.Flags_ &&& ~~~PublishChangeEventsMask
    member this.Enabled with get () = this.Flags_ &&& EnabledMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledMask else this.Flags_ &&& ~~~EnabledMask
    member this.Visible with get () = this.Flags_ &&& VisibleMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleMask else this.Flags_ &&& ~~~VisibleMask
    member this.AlwaysUpdate with get () = this.Flags_ &&& AlwaysUpdateMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AlwaysUpdateMask else this.Flags_ &&& ~~~AlwaysUpdateMask
    member this.PublishUpdates with get () = this.Flags_ &&& PublishUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishUpdatesMask else this.Flags_ &&& ~~~PublishUpdatesMask
    member this.PublishPostUpdates with get () = this.Flags_ &&& PublishPostUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishPostUpdatesMask else this.Flags_ &&& ~~~PublishPostUpdatesMask
    member this.Persistent with get () = this.Flags_ &&& PersistentMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PersistentMask else this.Flags_ &&& ~~~PersistentMask
    member this.IgnorePropertyBindings with get () = this.Flags_ &&& IgnorePropertyBindingsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| IgnorePropertyBindingsMask else this.Flags_ &&& ~~~IgnorePropertyBindingsMask
    member this.Mounted with get () = this.Flags_ &&& MountedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| MountedMask else this.Flags_ &&& ~~~MountedMask
    member this.EnabledLocal with get () = this.Flags_ &&& EnabledLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledLocalMask else this.Flags_ &&& ~~~EnabledLocalMask
    member this.VisibleLocal with get () = this.Flags_ &&& VisibleLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleLocalMask else this.Flags_ &&& ~~~VisibleLocalMask
    member this.RotationMatrixDirty with get () = this.Flags_ &&& RotationMatrixDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| RotationMatrixDirtyMask else this.Flags_ &&& ~~~RotationMatrixDirtyMask
    member this.AffineMatrixDirty with get () = this.Flags_ &&& AffineMatrixDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AffineMatrixDirtyMask else this.Flags_ &&& ~~~AffineMatrixDirtyMask
    member this.Optimized with get () = this.Imperative && this.Omnipresent && not this.PublishChangeBindings && not this.PublishChangeEvents // TODO: see if I can remove all conditionals from here.

    member this.Position with get () = this.Position_ and set value = this.Position_ <- value; this.AffineMatrixDirty <- true
    member this.Scale with get () = this.Scale_ and set value = this.Scale_ <- value; this.AffineMatrixDirty <- true
    member this.Offset with get () = this.Offset_ and set value = this.Offset_ <- value; this.AffineMatrixDirty <- true
    member this.Size with get () = this.Size_ and set value = this.Size_ <- value; this.AffineMatrixDirty <- true
    member this.Overflow with get () = this.Overflow_ and set value = this.Overflow_ <- value; this.AffineMatrixDirty <- true
    member this.Elevation with get () = this.Elevation_ and set value = this.Elevation_ <- value

    member this.Rotation
        with get () = this.Rotation_
        and set value =
            this.Rotation_ <- value
            let pitchYawRoll = value.PitchYawRoll
            this.Angles_.X <- pitchYawRoll.X
            this.Angles_.Y <- pitchYawRoll.Y
            this.Angles_.Z <- pitchYawRoll.Z
            this.RotationMatrixDirty <- true
            this.AffineMatrixDirty <- true

    member this.Angles
        with get () = this.Angles_
        and set value =
            this.Angles_ <- value
            this.Rotation_ <- Quaternion.CreateFromYawPitchRoll (value.Y, value.X, value.Z)
            this.RotationMatrixDirty <- true
            this.AffineMatrixDirty <- true

    member this.RotationMatrix =
        if notNull (this.RotationMatrixOpt_ :> obj) then
            if this.RotationMatrixDirty then this.RotationMatrixOpt_ <- ref (Matrix4x4.CreateFromQuaternion this.Rotation_)
            this.RotationMatrixOpt_.Value
        else Matrix4x4.Identity

    member this.AffineMatrix =
        if notNull (this.AffineMatrixOpt_ :> obj) then
            if this.AffineMatrixDirty then
                // TODO: P1: optimize this hella!
                let positionMatrix = Matrix4x4.CreateTranslation this.Position_
                let rotationMatrix = this.RotationMatrix
                let scaleMatrix = Matrix4x4.CreateScale this.Scale_
                this.AffineMatrixOpt_ <- ref (positionMatrix * rotationMatrix * scaleMatrix)
            this.AffineMatrixOpt_.Value
        else Matrix4x4.Identity

    member this.Right = Vector3 (this.RotationMatrix.M11, this.RotationMatrix.M12, this.RotationMatrix.M13) // TODO: implement Row properties.
    member this.Up = Vector3 (this.RotationMatrix.M21, this.RotationMatrix.M22, this.RotationMatrix.M23)
    member this.Forward = -Vector3 (this.RotationMatrix.M31, this.RotationMatrix.M32, this.RotationMatrix.M33)
    member this.Left = -this.Right
    member this.Down = -this.Up
    member this.Backward = -this.Forward

    member this.PerimeterUnscaled
        with get () =
            let size = this.Size_
            let extent = size * 0.5f
            let alpha = this.Position_ - extent
            let offset = this.Offset_ * size
            let position = alpha + offset
            Box3 (position, size)
        and set (value : Box3) =
            let size = value.Size
            let extent = size * 0.5f
            let offset = this.Offset_ * size
            let position = value.Position + extent - offset
            this.Position_ <- position
            this.Size <- size

    member this.Perimeter
        with get () =
            let scale = this.Scale_
            let sizeScaled = this.Size_ * scale
            let extentScaled = sizeScaled * 0.5f
            let alphaScaled = this.Position_ - extentScaled
            let offsetScaled = this.Offset_ * sizeScaled
            let position = alphaScaled + offsetScaled
            Box3 (position, sizeScaled)
        and set (value : Box3) =
            this.Scale_ <- Vector3.One
            this.PerimeterUnscaled <- value

    member this.PerimeterCenter
        with get () =
            let perimeter = this.Perimeter
            perimeter.Center
        and set (value : Vector3) =
            this.Scale_ <- Vector3.One
            let perimeter = this.Perimeter
            let perimeterCenter = perimeter.Translate (value - perimeter.Center)
            this.PerimeterUnscaled <- perimeterCenter

    member this.PerimeterBottom
        with get () =
            let perimeter = this.Perimeter
            perimeter.Bottom
        and set (value : Vector3) =
            this.Scale_ <- Vector3.One
            let perimeter = this.Perimeter
            let perimeterBottom = perimeter.Translate (value - perimeter.Bottom)
            this.PerimeterUnscaled <- perimeterBottom

    member this.PerimeterOriented =
        let perimeter = this.Perimeter
        perimeter.Orient this.Rotation_

    member this.Bounds =
        let perimeterOriented = this.PerimeterOriented
        let sizeOverflowed = perimeterOriented.Size * this.Overflow_
        let center = perimeterOriented.Center
        let positionOverflowed = center - sizeOverflowed * 0.5f
        Box3 (positionOverflowed, sizeOverflowed)

    member this.InvalidateFast () =
        this.Flags_ <- this.Flags_ ||| TransformMasks.InvalidatedMask

    /// Test transforms for equality.
    static member equalsByRef (left : Transform inref, right : Transform inref) =
        left.Flags_ = right.Flags_ &&
        left.Position_.Equals right.Position_ &&
        left.Rotation_.Equals right.Rotation_ &&
        left.Scale_.Equals right.Scale_ &&
        left.Offset_.Equals right.Offset_ &&
        left.Angles_.Equals right.Angles_ &&
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
        if notNull (source.RotationMatrixOpt_ :> obj) then target.RotationMatrixOpt_ <- ref source.RotationMatrixOpt_.Value
        if notNull (source.AffineMatrixOpt_ :> obj) then target.AffineMatrixOpt_ <- ref source.AffineMatrixOpt_.Value
        target.Angles_ <- source.Angles_
        target.Size_ <- source.Size_
        target.Elevation_ <- source.Elevation_
        target.Overflow_ <- source.Overflow_

    /// Assign the value of the left transform to the right.
    static member inline assign (source : Transform, target : Transform byref) =
        Transform.assignByRef (&source, &target)

    /// Make an empty transform.
    static member inline makeEmpty () =
        Unchecked.defaultof<Transform>

    /// Make a transform with default values.
    static member makeDefault offset =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- DefaultFlags
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- Vector3.One
        transform.Offset_ <- offset
        transform.Size_ <- Vector3.One
        transform.Overflow_ <- 1.0f
        transform

    /// Make a transform based on a perimeter.
    static member makePerimeter (perimeter : Box3) offset elevation absolute =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- DefaultFlags
        transform.Position_ <- perimeter.Position
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- v3One
        transform.Offset_ <- offset
        transform.Size_ <- perimeter.Size
        transform.Angles_ <- v3Zero
        transform.Elevation_ <- elevation
        transform.Absolute <- absolute
        transform

    /// Make a transform based human-intuited values.
    static member makeIntuitive position scale offset size angles elevation absolute =
        let mutable transform = Transform.makeDefault offset
        transform.Flags_ <- DefaultFlags
        transform.Position_ <- position
        transform.Scale_ <- scale
        transform.Size_ <- size
        transform.Elevation_ <- elevation
        transform.Angles <- angles
        transform.Absolute <- absolute
        transform

    interface Transform Component with
        member this.TypeName = nameof Transform
        member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask

[<RequireQualifiedAccess>]
module Math =

    open tainicom.Aether.Physics2D // NOTE: for implementation of some ray-cast code in terms of Aether.

    let mutable private Initialized = false

    let Epsilon = 1.1920929E-07f

    /// Initializes the type converters found in NuMathModule.
    let init () =
        if not Initialized then
            assignTypeConverter<Vector2, Vector2Converter> ()
            assignTypeConverter<Vector3, Vector3Converter> ()
            assignTypeConverter<Vector4, Vector4Converter> ()
            assignTypeConverter<Vector2i, Vector2iConverter> ()
            assignTypeConverter<Vector3i, Vector3iConverter> ()
            assignTypeConverter<Vector4i, Vector4iConverter> ()
            assignTypeConverter<Quaternion, QuaternionConverter> ()
            assignTypeConverter<Color, ColorConverter> ()
            Initialized <- true

    /// Convert radians to degrees.
    let radiansToDegrees (radians : single) =
        MathHelper.RadiansToDegrees -radians

    /// Convert radians to degrees in 3d.
    let radiansToDegrees3d (radians : Vector3) =
        v3
            (radiansToDegrees radians.X)
            (radiansToDegrees radians.Y)
            (radiansToDegrees radians.Z)

    /// Convert degrees to radians.
    let degreesToRadians (degrees : single) =
        MathHelper.DegreesToRadians -degrees

    /// Convert degrees to radians in 3d.
    let degreesToRadians3d (degrees : Vector3) =
        v3
            (degreesToRadians degrees.X)
            (degreesToRadians degrees.Y)
            (degreesToRadians degrees.Z)

    /// Snap an int value to an offset.
    let snap offset value =
        if offset <> 0 then
            let (div, rem) = Math.DivRem (value, offset)
            let rem = if rem < offset / 2 then 0 else offset
            div * offset + rem
        else value

    /// Snap a radian value to an offset.
    let snapR offset value =
        radiansToDegrees value |>
        int |>
        snap offset |>
        single |>
        degreesToRadians

    /// Snap a Vector3 radian value to an offset.
    let snapR3d offset (v3 : Vector3) =
        Vector3 (snapR offset v3.X, snapR offset v3.Y, snapR offset v3.Z)

    /// Snap an single float value to an offset.
    let snapF offset (value : single) =
        single (snap offset (int value))

    /// Snap a Vector3 value to an offset.
    let snapF3d offset (v3 : Vector3) =
        Vector3 (snapF offset v3.X, snapF offset v3.Y, snapF offset v3.Z)

    /// Snap a Transform value to an offset.
    let snapTransform positionSnap rotationSnap (transform : Transform) =
        let mutable transform = transform
        transform.Position <- snapF3d positionSnap transform.Position
        transform.Angles <- snapR3d rotationSnap transform.Angles
        transform

    /// Check that a point is within the given bounds.
    let isPointInBounds3d (point : Vector3) (bounds : Box3) =
        point.X >= bounds.Position.X &&
        point.Y >= bounds.Position.Y &&
        point.Z >= bounds.Position.Z &&
        point.X <= bounds.Position.X + bounds.Size.X &&
        point.Y <= bounds.Position.Y + bounds.Size.Y &&
        point.Z <= bounds.Position.Z + bounds.Size.Z

    /// Check that a point is within the given bounds.
    let isPointInBounds2d (point : Vector2) (bounds : Box2) =
        point.X >= bounds.Position.X &&
        point.Y >= bounds.Position.Y &&
        point.X <= bounds.Position.X + bounds.Size.X &&
        point.Y <= bounds.Position.Y + bounds.Size.Y

    /// Check that a bounds is within the given bounds.
    let isBoundsInBounds3d (bounds : Box3) (bounds2 : Box3) =
        bounds.Position.X >= bounds2.Position.X &&
        bounds.Position.Y >= bounds2.Position.Y &&
        bounds.Position.Z >= bounds2.Position.Z &&
        bounds.Position.X + bounds.Size.X <= bounds2.Position.X + bounds2.Size.X &&
        bounds.Position.Y + bounds.Size.Y <= bounds2.Position.Y + bounds2.Size.Y &&
        bounds.Position.Z + bounds.Size.Z <= bounds2.Position.Z + bounds2.Size.Z

    /// Check that a bounds is within the given bounds.
    let isBoundsInBounds2d (bounds : Box2) (bounds2 : Box2) =
        bounds.Position.X >= bounds2.Position.X &&
        bounds.Position.Y >= bounds2.Position.Y &&
        bounds.Position.X + bounds.Size.X <= bounds2.Position.X + bounds2.Size.X &&
        bounds.Position.Y + bounds.Size.Y <= bounds2.Position.Y + bounds2.Size.Y

    /// Check that a bounds is intersecting the given bounds.
    /// TODO: move this into Box3 definition.
    let isBoundsIntersectingBounds3d (bounds : Box3) (bounds2 : Box3) =
        bounds.Position.X < bounds2.Position.X + bounds2.Size.X &&
        bounds.Position.Y < bounds2.Position.Y + bounds2.Size.Y &&
        bounds.Position.Z < bounds2.Position.Z + bounds2.Size.Z &&
        bounds.Position.X + bounds.Size.X > bounds2.Position.X &&
        bounds.Position.Y + bounds.Size.Y > bounds2.Position.Y &&
        bounds.Position.Z + bounds.Size.Z > bounds2.Position.Z

    /// Check that a bounds is intersecting the given bounds.
    /// TODO: move this into Box2 definition.
    let isBoundsIntersectingBounds2d (bounds : Box2) (bounds2 : Box2) =
        bounds.Position.X < bounds2.Position.X + bounds2.Size.X &&
        bounds.Position.Y < bounds2.Position.Y + bounds2.Size.Y &&
        bounds.Position.X + bounds.Size.X > bounds2.Position.X &&
        bounds.Position.Y + bounds.Size.Y > bounds2.Position.Y

    /// Get the 2d view of the eye in absolute terms (world space).
    let getViewAbsolute2d (_ : Vector2) (_ : Vector2) =
        Matrix3x3.Identity
        
    /// Get the 2d view of the eye in absolute terms (world space) with translation sliced on
    /// integers.
    let getViewAbsoluteI2d (_ : Vector2) (_ : Vector2) =
        Matrix3x3.Identity

    /// The relative 2d view of the eye with original single values. Due to the problems with
    /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
    /// coordinates is very, very bad for rendering.
    let getViewRelative2d (eyePosition : Vector2) (_ : Vector2) =
        Matrix3x3.CreateTranslation eyePosition

    /// The relative 2d view of the eye with translation sliced on integers. Good for rendering.
    let getViewRelativeI2d (eyePosition : Vector2) (_ : Vector2) =
        let translation = eyePosition
        let translationI = Vector2 (single (int translation.X), single (int translation.Y))
        Matrix3x3.CreateTranslation translationI

    /// Perform a 2d ray cast on a circle.
    /// Code adapted from - https://github.com/tainicom/Aether.Physics2D/blob/aa8a6b45c63e26c2f408ffde40f03cbe78ecfa7c/Physics2D/Collision/Shapes/CircleShape.cs#L93-L134
    let rayCastCircle2d (position : Vector2) (radius : single) (input : RayCast2Input inref) (output : RayCast2Output outref) =
        let mutable s = input.RayBegin - position
        let b = Vector2.Dot (s, s) - 2.0f * radius
        let mutable r = input.RayEnd - input.RayBegin
        let c = Vector2.Dot (s, r)
        let rr = Vector2.Dot (r, r)
        let sigma = c * c - rr * b
        if sigma >= 0f && rr >= Epsilon then
            let a = 0f - (c + single (Math.Sqrt (float sigma)))
            if 0f <= a && a <= rr then
                output.Fraction <- a / rr
                output.Normal <- Vector2.Normalize (s + output.Fraction * r)
                true
            else false
        else false

    /// Perform a 2d ray cast on a rectangle.
    /// BUG: There's a bug in AABB.RayCast that produces invalid normals.
    let rayCastRectangle2d (rectangle : Vector4) (input : RayCast2Input inref) (output : RayCast2Output outref) =
        let point1 = Common.Vector2 (input.RayBegin.X, input.RayBegin.Y)
        let point2 = Common.Vector2 (input.RayEnd.X, input.RayEnd.Y)
        let mutable inputAether = Collision.RayCastInput (MaxFraction = 1.0f, Point1 = point1, Point2 = point2)
        let mutable outputAether = Unchecked.defaultof<Collision.RayCastOutput>
        let aabb  = Collision.AABB (Common.Vector2 (rectangle.X, rectangle.Y), Common.Vector2 (rectangle.X + rectangle.Z, rectangle.Y + rectangle.W))
        let result = aabb.RayCast (&outputAether, &inputAether)
        output.Normal <- Vector2 (outputAether.Normal.X, outputAether.Normal.Y)
        output.Fraction <- outputAether.Fraction
        result

    /// Perform a 2d ray-cast on a line segment (edge).
    /// NOTE: due to unoptimized implementation, this function allocates one object per call!
    /// TODO: adapt the Aether code as was done for circle to improve performance and get rid of said
    /// allocation.
    let rayCastSegment2d segmentBegin segmentEnd (input : RayCast2Input inref) (output : RayCast2Output outref) =
        let point1 = Common.Vector2 (input.RayBegin.X, input.RayBegin.Y)
        let point2 = Common.Vector2 (input.RayEnd.X, input.RayEnd.Y)
        let mutable identity = Common.Transform.Identity // NOTE: superfluous copy of identity to satisfy EdgeShap.RayCast's use of byref instead of inref.
        let mutable inputAether = Collision.RayCastInput (MaxFraction = 1.0f, Point1 = point1, Point2 = point2)
        let mutable outputAether = Unchecked.defaultof<Collision.RayCastOutput>
        let edgeShape = Collision.Shapes.EdgeShape (segmentBegin, segmentEnd) // NOTE: unecessary allocation, ugh!
        let result = edgeShape.RayCast (&outputAether, &inputAether, &identity, 0)
        output.Normal <- Vector2 (outputAether.Normal.X, outputAether.Normal.Y)
        output.Fraction <- outputAether.Fraction
        result