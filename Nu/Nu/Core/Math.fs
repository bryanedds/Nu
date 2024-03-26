// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.ComponentModel
open System.Globalization
open System.Numerics
open FSharp.NativeInterop
open Prime

[<AutoOpen>]
module Vector2 =

    type Vector2 with

        member this.V2i = Vector2i (int this.X, int this.Y)
        member this.V3i = Vector3i (int this.X, int this.Y, 0)
        member this.V3 = Vector3 (this.X, this.Y, 0.0f)
        member this.Normalized = Vector2.Normalize this
        member this.Magnitude = this.Length ()
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.Absolute = Vector2 (abs this.X, abs this.Y)
        member this.MapX mapper = Vector2 (mapper this.X, this.Y)
        member this.MapY mapper = Vector2 (this.X, mapper this.Y)
        member this.WithX x = Vector2 (x, this.Y)
        member this.WithY y = Vector2 (this.X, y)
        member this.Rotate r = Vector2 (cos r * this.X - sin r * this.Y, sin r * this.X + cos r * this.Y)

        member this.AngleBetween (that : Vector2) =
            let a = Vector2.Normalize this
            let b = Vector2.Normalize that
            acos (Vector2.Dot (a, b))

        static member Pow (a : Vector2, b : Vector2) =
            Vector2
                (single (Math.Pow (double a.X, double b.X)),
                 single (Math.Pow (double a.Y, double b.Y)))

        static member Modulo (a : Vector2, b : Vector2) =
            Vector2
                (a.X % b.X,
                 a.Y % b.Y)

    let inline v2 x y = Vector2 (x, y)
    let inline v2Eq (v : Vector2) (v2 : Vector2) = v.X = v2.X && v.Y = v2.Y
    let inline v2Neq (v : Vector2) (v2 : Vector2) = v.X <> v2.X || v.Y <> v2.Y
    let inline v2Dup (a : single) = v2 a a
    let v2One = Vector2.One
    let v2Zero = Vector2.Zero
    let v2UnitX = Vector2.UnitX
    let v2UnitY = Vector2.UnitY
    let v2Up = v2 0.0f 1.0f
    let v2Right = v2 1.0f 0.0f
    let v2Down = v2 0.0f -1.0f
    let v2Left = v2 -1.0f 0.0f

/// Converts Vector2 types.
type Vector2Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector2>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v2 = source :?> Vector2
            Symbols
                ([Number (string v2.X, ValueNone)
                  Number (string v2.Y, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector2> then source
        else failconv "Invalid Vector2Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2 (Single.Parse x, Single.Parse y) :> obj
            | _ -> failconv "Invalid Vector2Converter conversion from source." (Some symbol)
        | :? Vector2 -> source
        | _ -> failconv "Invalid Vector2Converter conversion from source." None

[<AutoOpen>]
module Vector3 =

    type Vector3 with

        member this.V3i = Vector3i (int this.X, int this.Y, int this.Z)
        member this.V2i = Vector2i (int this.X, int this.Y)
        member this.V2 = Vector2 (this.X, this.Y)
        member this.Normalized = Vector3.Normalize this
        member this.Magnitude = this.Length ()
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.Absolute = Vector3 (abs this.X, abs this.Y, abs this.Z)
        member this.MapX mapper = Vector3 (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3 (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3 (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3 (x, this.Y, this.Z)
        member this.WithY y = Vector3 (this.X, y, this.Z)
        member this.WithZ z = Vector3 (this.X, this.Y, z)
        member this.RollPitchYaw = Math.RollPitchYaw &this

        /// Compute angle between vectors.
        member this.AngleBetween (that : Vector3) =
            let a = Vector3.Normalize this
            let b = Vector3.Normalize that
            acos (Vector3.Dot (a, b))

        /// Compute power of vector components.
        static member Pow (a : Vector3, b : Vector3) =
            Vector3
                (single (Math.Pow (double a.X, double b.X)),
                 single (Math.Pow (double a.Y, double b.Y)),
                 single (Math.Pow (double a.Z, double b.Z)))

        /// Compute modulo of vector components.
        static member Modulo (a : Vector3, b : Vector3) =
            Vector3
                (a.X % b.X,
                 a.Y % b.Y,
                 a.Z % b.Z)

        /// Project a vector onto a plane.
        static member Project (v : Vector3, p : Plane3) =
            let mutable dc = Unchecked.defaultof<_>
            p.DotCoordinate (&v, &dc)
            v - dc * p.Normal

        /// Reflect a vector on a plane.
        static member Reflect (v : Vector3, p : Plane3) =
            let mutable dc = Unchecked.defaultof<_>
            p.DotCoordinate (&v, &dc)
            v - 2.0f * dc * p.Normal

        /// Compute distance of a vector from the nearest point on a plane.
        static member Distance (v : Vector3, p : Plane3) =
            let mutable dc = Unchecked.defaultof<_>
            p.DotCoordinate (&v, &dc)
            let distance = dc + p.D
            abs (distance / p.Normal.Magnitude)

        /// Compute offset from a vector to the nearest point on a plane.
        static member ToPlane (v : Vector3, p : Plane3) =
            let mutable dc = Unchecked.defaultof<_>
            p.DotCoordinate (&v, &dc)
            let distance = dc + p.D
            -distance * p.Normal

        /// Compute offset to a vector from the nearest point on a plane.
        static member FromPlane (v : Vector3, p : Plane3) =
            -Vector3.ToPlane (v, p)

    let inline v3 x y z = Vector3 (x, y, z)
    let inline v3Eq (v : Vector3) (v2 : Vector3) = v.X = v2.X && v.Y = v2.Y && v.Z = v2.Z
    let inline v3Neq (v : Vector3) (v2 : Vector3) = v.X <> v2.X || v.Y <> v2.Y || v.Z <> v2.Z
    let inline v3Dup (a : single) = v3 a a a
    let v3UncenteredOffset = v3Dup 0.5f
    let v3One = Vector3.One
    let v3Zero = Vector3.Zero
    let v3UnitX = Vector3.UnitX
    let v3UnitY = Vector3.UnitY
    let v3UnitZ = Vector3.UnitZ
    let v3Up = v3 0.0f 1.0f 0.0f
    let v3Down = v3 0.0f -1.0f 0.0f
    let v3Right = v3 1.0f 0.0f 0.0f
    let v3Left = v3 -1.0f 0.0f 0.0f
    let v3Forward = v3 0.0f 0.0f -1.0f
    let v3Back = v3 0.0f 0.0f 1.0f

/// Converts Vector3 types.
type Vector3Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector3>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v3 = source :?> Vector3
            Symbols
                ([Number (string v3.X, ValueNone)
                  Number (string v3.Y, ValueNone)
                  Number (string v3.Z, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector3> then source
        else failconv "Invalid Vector3Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector3>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _)], _) ->
                Vector3 (Single.Parse x, Single.Parse y, Single.Parse z) :> obj
            | _ -> failconv "Invalid Vector3Converter conversion from source." (Some symbol)
        | :? Vector3 -> source
        | _ -> failconv "Invalid Vector3Converter conversion from source." None

[<AutoOpen>]
module Vector4 =

    type Vector4 with

        member this.V2 = Vector2 (this.X, this.Y)
        member this.V2i = Vector2i (int this.X, int this.Y)
        member this.V3 = Vector3 (this.X, this.Y, this.Z)
        member this.V3i = Vector3i (int this.X, int this.Y, int this.Z)
        member this.V4i = Vector4i (int this.X, int this.Y, int this.Z, int this.W)
        member this.Magnitude = this.Magnitude
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.Absolute = Vector4 (abs this.X, abs this.Y, abs this.Z, abs this.W)
        member this.MapX mapper = Vector4 (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4 (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4 (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4 (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4 (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4 (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4 (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4 (this.X, this.Y, this.Z, w)

        static member Pow (a : Vector4, b : Vector4) =
            Vector4
                (single (Math.Pow (double a.X, double b.X)),
                 single (Math.Pow (double a.Y, double b.Y)),
                 single (Math.Pow (double a.Z, double b.Z)),
                 single (Math.Pow (double a.W, double b.W)))

        static member Modulo (a : Vector4, b : Vector4) =
            Vector4
                (a.X % b.X,
                 a.Y % b.Y,
                 a.Z % b.Z,
                 a.W % b.W)

    let inline v4 x y z w = Vector4 (x, y, z, w)
    let inline v4Eq (v : Vector4) (v2 : Vector4) = v.X = v2.X && v.Y = v2.Y && v.Z = v2.Z && v.W = v2.W
    let inline v4Neq (v : Vector4) (v2 : Vector4) = v.X <> v2.X || v.Y <> v2.Y || v.Z <> v2.Z || v.W <> v2.W
    let inline v4Dup (a : single) = v4 a a a a
    let v4One = Vector4.One
    let v4Zero = Vector4.Zero
    let v4UnitX = Vector4.UnitX
    let v4UnitY = Vector4.UnitY
    let v4UnitZ = Vector4.UnitZ
    let v4UnitW = Vector4.UnitW

/// Converts Vector4 types.
type Vector4Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector4>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v4 = source :?> Vector4
            Symbols
                ([Number (string v4.X, ValueNone)
                  Number (string v4.Y, ValueNone)
                  Number (string v4.Z, ValueNone)
                  Number (string v4.W, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector4> then source
        else failconv "Invalid Vector4Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector4>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _); Number (w, _)], _) ->
                Vector4 (Single.Parse x, Single.Parse y, Single.Parse z, Single.Parse w) :> obj
            | _ -> failconv "Invalid Vector4Converter conversion from source." (Some symbol)
        | :? Vector4 -> source
        | _ -> failconv "Invalid Vector4Converter conversion from source." None

[<AutoOpen>]
module Vector2i =

    type Vector2i with

        member this.V2 = Vector2 (single this.X, single this.Y)
        member this.V3 = Vector3 (single this.X, single this.Y, 0.0f)
        member this.V3i = Vector3i (this.X, this.Y, 0)
        member this.MapX mapper = Vector2i (mapper this.X, this.Y)
        member this.MapY mapper = Vector2i (this.X, mapper this.Y)
        member this.WithX x = Vector2i (x, this.Y)
        member this.WithY y = Vector2i (this.X, y)

        static member Pow (a : Vector2i, b : Vector2i) =
            Vector2i
                (pown a.X b.X,
                 pown a.Y b.Y)

        static member Modulo (a : Vector2i, b : Vector2i) =
            Vector2i
                (a.X % b.X,
                 a.Y % b.Y)

    let inline v2i x y = Vector2i (x, y)
    let inline v2iEq (v : Vector2i) (v2 : Vector2i) = v.X = v2.X && v.Y = v2.Y
    let inline v2iNeq (v : Vector2i) (v2 : Vector2i) = v.X <> v2.X || v.Y <> v2.Y
    let inline v2iDup (a : int) = v2i a a
    let v2iOne = Vector2i.One
    let v2iZero = Vector2i.Zero
    let v2iUnitX = Vector2i.UnitX
    let v2iUnitY = Vector2i.UnitY
    let v2iUp = Vector2i.Up
    let v2iRight = Vector2i.Right
    let v2iDown = Vector2i.Down
    let v2iLeft = Vector2i.Left

/// Converts Vector2i types.
type Vector2iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector2i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v2i = source :?> Vector2i
            Symbols ([Number (string v2i.X, ValueNone); Number (string v2i.Y, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector2i> then source
        else failconv "Invalid Vector2iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2i (Int32.Parse x, Int32.Parse y) :> obj
            | _ -> failconv "Invalid Vector2iConverter conversion from source." (Some symbol)
        | :? Vector2i -> source
        | _ -> failconv "Invalid Vector2iConverter conversion from source." None

[<AutoOpen>]
module Vector3i =

    type Vector3i with

        member this.V3 = Vector3 (single this.X, single this.Y, single this.Z)
        member this.V2 = Vector2 (single this.X, single this.Y)
        member this.V2i = Vector2i (this.X, this.Y)
        member this.MapX mapper = Vector3i (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3i (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3i (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3i (x, this.Y, this.Z)
        member this.WithY y = Vector3i (this.X, y, this.Z)
        member this.WithZ z = Vector3i (this.X, this.Y, z)

        static member Pow (a : Vector3i, b : Vector3i) =
            Vector3i
                (pown a.X b.X,
                 pown a.Y b.Y,
                 pown a.Z b.Z)

        static member Modulo (a : Vector3i, b : Vector3i) =
            Vector3i
                (a.X % b.X,
                 a.Y % b.Y,
                 a.Z % b.Z)

    let inline v3i x y z = Vector3i (x, y, z)
    let inline v3iEq (v : Vector3i) (v2 : Vector3i) = v.X = v2.X && v.Y = v2.Y && v.Z = v2.Z
    let inline v3iNeq (v : Vector3i) (v2 : Vector3i) = v.X <> v2.X || v.Y <> v2.Y || v.Z <> v2.Z
    let inline v3iDup (a : int) = v3i a a a
    let v3iOne = Vector3i.One
    let v3iZero = Vector3i.Zero
    let v3iUnitX = Vector3i.UnitX
    let v3iUnitY = Vector3i.UnitY
    let v3iUnitZ = Vector3i.UnitZ

/// Converts Vector3i types.
type Vector3iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector3i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v3i = source :?> Vector3i
            Symbols
                ([Number (string v3i.X, ValueNone)
                  Number (string v3i.Y, ValueNone)
                  Number (string v3i.Z, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector3i> then source
        else failconv "Invalid Vector3iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector3i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _)], _) ->
                Vector3i (Int32.Parse x, Int32.Parse y, Int32.Parse z) :> obj
            | _ -> failconv "Invalid Vector3iConverter conversion from source." (Some symbol)
        | :? Vector3i -> source
        | _ -> failconv "Invalid Vector3iConverter conversion from source." None

[<AutoOpen>]
module Vector4i =

    type Vector4i with
    
        member this.V2 = Vector2 (single this.X, single this.Y)
        member this.V2i = Vector2i (this.X, this.Y)
        member this.V3 = Vector3 (single this.X, single this.Y, single this.Z)
        member this.V3i = Vector3i (this.X, this.Y, this.Z)
        member this.V4 = Vector4 (single this.X, single this.Y, single this.Z, single this.W)
        member this.MapX mapper = Vector4i (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4i (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4i (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4i (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4i (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4i (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4i (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4i (this.X, this.Y, this.Z, w)

        static member Pow (a : Vector4i, b : Vector4i) =
            Vector4i
                (pown a.X b.X,
                 pown a.Y b.Y,
                 pown a.Z b.Z,
                 pown a.W b.W)

        static member Modulo (a : Vector4i, b : Vector4i) =
            Vector4i
                (a.X % b.X,
                 a.Y % b.Y,
                 a.Z % b.Z,
                 a.W % b.W)

    let inline v4i x y z w = Vector4i (x, y, z, w)
    let inline v4iEq (v : Vector4i) (v2 : Vector4i) = v.X = v2.X && v.Y = v2.Y && v.Z = v2.Z && v.W = v2.W
    let inline v4iNeq (v : Vector4i) (v2 : Vector4i) = v.X <> v2.X || v.Y <> v2.Y || v.Z <> v2.Z || v.W <> v2.W
    let inline v4iDup (a : int) = v4i a a a a
    let v4iOne = Vector4i.One
    let v4iZero = Vector4i.Zero
    let v4iUnitX = Vector4i.UnitX
    let v4iUnitY = Vector4i.UnitY
    let v4iUnitZ = Vector4i.UnitZ
    let v4iUnitW = Vector4i.UnitW

/// Converts Vector4i types.
type Vector4iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector4i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v4i = source :?> Vector4i
            Symbols
                ([Number (string v4i.X, ValueNone)
                  Number (string v4i.Y, ValueNone)
                  Number (string v4i.Z, ValueNone)
                  Number (string v4i.W, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Vector4i> then source
        else failconv "Invalid Vector4iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector4i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _); Number (w, _)], _) ->
                Vector4i (Int32.Parse x, Int32.Parse y, Int32.Parse z, Int32.Parse w) :> obj
            | _ -> failconv "Invalid Vector4iConverter conversion from source." (Some symbol)
        | :? Vector4i -> source
        | _ -> failconv "Invalid Vector4iConverter conversion from source." None

[<AutoOpen>]
module Quaternion =

    type Quaternion with

        /// The right vector of the quaternion.
        member inline this.Right =
            Vector3.Transform (v3Right, this)

        /// The left vector of the quaternion.
        member inline this.Left =
            Vector3.Transform (v3Left, this)

        /// The up vector of the quaternion.
        member inline this.Up =
            Vector3.Transform (v3Up, this)

        /// The down vector of the quaternion.
        member inline this.Down =
            Vector3.Transform (v3Down, this)

        /// The back vector of the quaternion.
        member inline this.Back =
            Vector3.Transform (v3Back, this)

        /// The forward vector of the quaternion.
        member inline this.Forward =
            Vector3.Transform (v3Forward, this)

        /// Right, up, and forward quaternion vectors.
        member inline this.RightUpForward =
            (this.Right, this.Up, this.Forward)

        /// Decompose the quaternion into roll, pitch, and yaw angles.
        member this.RollPitchYaw =
            Math.RollPitchYaw &this

        /// Derive an axis angle from the quaternion.
        member this.AxisAngle =
            let w = this.W
            let angle = 2.0f * acos w
            let magnitude = sqrt (1.0f - w * w)
            let axis =
                if magnitude >= 0.0001f
                then v3 (this.X / magnitude) (this.Y / magnitude) (this.Z / magnitude)
                else v3Up // any unit vector can be chosen
            (axis, angle)

        /// Normalize the quaternion.
        member this.Normalized =
            Quaternion.Normalize this

        /// The inverted value of a quaternion.
        member inline this.Inverted =
            Quaternion.Inverse this

    let quatIdentity = Quaternion.Identity
    let inline quat x y z w = Quaternion (x, y, z, w)
    let inline quatEq (q : Quaternion) (q2 : Quaternion) = q.Equals q2
    let inline quatNeq (q : Quaternion) (q2 : Quaternion) = not (q.Equals q2)

    /// Create a look-at rotation quaternion.
    /// NOTE: this might be less efficient since it uses Matrix4x4's look-at function then converts to quaternion.
    let CreateLookAt (position, direction, up) =
        Quaternion.CreateFromRotationMatrix
            (Matrix4x4.CreateLookAt (position, direction, up))

/// Converts Quaternion types.
type QuaternionConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Quaternion>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let quat = source :?> Quaternion
            Symbols
                ([Number (string quat.X, ValueNone)
                  Number (string quat.Y, ValueNone)
                  Number (string quat.Z, ValueNone)
                  Number (string quat.W, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Quaternion> then source
        else failconv "Invalid QuaternionConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Quaternion>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _); Number (w, _)], _) ->
                Quaternion (Single.Parse x, Single.Parse y, Single.Parse z, Single.Parse w) :> obj
            | _ -> failconv "Invalid QuaternionConverter conversion from source." (Some symbol)
        | :? Quaternion -> source
        | _ -> failconv "Invalid QuaternionConverter conversion from source." None

[<AutoOpen>]
module Box2 =

    type Box2 with

        member this.Max = this.Min + this.Size
        member this.Extent = this.Size * 0.5f
        member this.Width = this.Size.X
        member this.Height = this.Size.Y
        member this.Center = this.Min + this.Extent
        member this.Top = v2 (this.Min.X + this.Size.X * 0.5f) (this.Min.Y + this.Size.Y)
        member this.Bottom = v2 (this.Min.X + this.Size.X * 0.5f) this.Min.Y
        member this.Right = v2 (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y * 0.5f)
        member this.Left = v2 this.Min.X (this.Min.Y + this.Size.Y * 0.5f)
        member this.TopLeft = v2 this.Min.X (this.Min.Y + this.Size.Y)
        member this.TopRight = v2 (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y)
        member this.BottomLeft = this.Min
        member this.BottomRight = v2 (this.Min.X + this.Size.X) this.Min.Y
        member this.IsEmpty = this.Equals Box2.Zero
        member this.Translate translation = Box2 (this.Min + translation, this.Size)
        member this.WithMin min = Box2 (min, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithBottomLeft bottomLeft = this.Translate (bottomLeft - this.BottomLeft)
        member this.WithSize size = Box2 (this.Min, size)
        member this.Box3 = Box3 (v3 this.Min.X this.Min.Y 0.0f, v3 this.Size.X this.Size.Y 0.0f)

    let box2Zero = Box2.Zero
    let inline box2 min size = Box2 (min, size)
    let inline box2Eq (b : Box2) (b2 : Box2) = b.Equals b2
    let inline box2Neq (b : Box2) (b2 : Box2) = not (b.Equals b2)

/// Converts Box2 types.
type Box2Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Box2>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let box2 = source :?> Box2
            Symbols
                ([Symbols ([Number (string box2.Min.X, ValueNone); Number (string box2.Min.Y, ValueNone)], ValueNone)
                  Symbols ([Number (string box2.Size.X, ValueNone); Number (string box2.Size.Y, ValueNone)], ValueNone)], ValueNone) :> obj
        elif destType = typeof<Box2> then source
        else failconv "Invalid Box2Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([minSymbol; sizeSymbol], _) ->
                match (minSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _)], _), Symbols ([Number (sx, _); Number (sy, _)], _)) ->
                    Box2 (Single.Parse px, Single.Parse py, Single.Parse sx, Single.Parse sy) :> obj
                | _ -> failconv "Invalid Box2Converter conversion from source." (Some symbol)
            | _ -> failconv "Invalid Box2Converter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box2Converter conversion from source." None

[<AutoOpen>]
module Box3 =

    type Box3 with

        member this.Max = this.Min + this.Size
        member this.Extent = this.Size * 0.5f
        member this.Width = this.Size.X
        member this.Height = this.Size.Y
        member this.Depth = this.Size.Z
        member this.Center = this.Min + this.Extent
        member this.Top = v3 (this.Min.X + this.Size.X * 0.5f) (this.Min.Y + this.Size.Y) (this.Min.Z + this.Size.Z * 0.5f)
        member this.Bottom = v3 (this.Min.X + this.Size.X * 0.5f) this.Min.Y (this.Min.Z + this.Size.Z * 0.5f)
        member this.Right = v3 (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y * 0.5f) (this.Min.Z + this.Size.Z * 0.5f)
        member this.Left = v3 this.Min.X (this.Min.Y + this.Size.Y * 0.5f) (this.Min.Z + this.Size.Z * 0.5f)
        member this.TopLeft = v3 this.Min.X (this.Min.Y + this.Size.Y) (this.Min.Z + this.Size.Z * 0.5f)
        member this.TopRight = v3 (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y) (this.Min.Z + this.Size.Z * 0.5f)
        member this.BottomLeft = v3 this.Min.X this.Min.Y (this.Min.Z + this.Size.Z * 0.5f)
        member this.BottomRight = v3 (this.Min.X + this.Size.X) this.Min.Y (this.Min.Z + this.Size.Z * 0.5f)
        member this.IsEmpty = this.Equals Box3.Zero
        member this.WithMin min = Box3 (min, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithBottomLeft bottomLeft = this.Translate (bottomLeft - this.BottomLeft)
        member this.WithSize size = Box3 (this.Min, size)
        member this.Box2 = Box2 (v2 this.Min.X this.Min.Y, v2 this.Size.X this.Size.Y)

        member this.Translate translation =
            Box3 (this.Min + translation, this.Size)

        member this.Transform (transformation : Matrix4x4) =
            if not transformation.IsIdentity then
                let min = this.Min
                let max = this.Min + this.Size
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
                    corner <- Vector3.Transform (corner, transformation)
                    minX <- Operators.min minX corner.X
                    minY <- Operators.min minY corner.Y
                    minZ <- Operators.min minZ corner.Z
                    maxX <- Operators.max maxX corner.X
                    maxY <- Operators.max maxY corner.Y
                    maxZ <- Operators.max maxZ corner.Z
                Box3 (minX, minY, minZ, maxX- minX, maxY - minY, maxZ - minZ)
            else this

    let box3Zero = Box3.Zero
    let inline box3 min size = Box3 (min, size)
    let inline box3Eq (b : Box3) (b2 : Box3) = b.Equals b2
    let inline box3Neq (b : Box3) (b2 : Box3) = not (b.Equals b2)

/// Converts Box3 types.
type Box3Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Box3>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let box3 = source :?> Box3
            Symbols
                ([Symbols ([Number (string box3.Min.X, ValueNone); Number (string box3.Min.Y, ValueNone); Number (string box3.Min.Z, ValueNone)], ValueNone)
                  Symbols ([Number (string box3.Size.X, ValueNone); Number (string box3.Size.Y, ValueNone); Number (string box3.Size.Z, ValueNone)], ValueNone)], ValueNone) :> obj
        elif destType = typeof<Box3> then source
        else failconv "Invalid Box3Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box3>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([minSymbol; sizeSymbol], _) ->
                match (minSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _); Number (pz, _)], _), Symbols ([Number (sx, _); Number (sy, _); Number (sz, _)], _)) ->
                    Box3 (Single.Parse px, Single.Parse py, Single.Parse pz, Single.Parse sx, Single.Parse sy, Single.Parse sz) :> obj
                | _ -> failconv "Invalid Box3Converter conversion from source." (Some symbol)
            | _ -> failconv "Invalid Box3Converter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box3Converter conversion from source." None

[<AutoOpen>]
module Box2i =

    type Box2i with

        member this.Max = this.Min + this.Size
        member this.Extent = this.Size / 2
        member this.Width = this.Size.X
        member this.Height = this.Size.Y
        member this.Center = this.Min + this.Extent
        member this.Top = v2i (this.Min.X + this.Size.X / 2) (this.Min.Y + this.Size.Y)
        member this.Bottom = v2i (this.Min.X + this.Size.X / 2) this.Min.Y
        member this.Right = v2i (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y / 2)
        member this.Left = v2i this.Min.X (this.Min.Y + this.Size.Y / 2)
        member this.TopLeft = v2i this.Min.X (this.Min.Y + this.Size.Y)
        member this.TopRight = v2i (this.Min.X + this.Size.X) (this.Min.Y + this.Size.Y)
        member this.BottomLeft = this.Min
        member this.BottomRight = v2i (this.Min.X + this.Size.X) this.Min.Y
        member this.IsEmpty = this.Equals Box2i.Zero
        member this.Translate translation = Box2i (this.Min + translation, this.Size)
        member this.WithMin min = Box2i (min, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithBottomLeft bottomLeft = this.Translate (bottomLeft - this.BottomLeft)
        member this.WithSize size = Box2i (this.Min, size)

    let box2iZero = Box2i.Zero
    let inline box2i min size = Box2i (min, size)
    let inline box2iEq (b : Box2i) (b2 : Box2i) = b.Equals b2
    let inline box2iNeq (b : Box2i) (b2 : Box2i) = not (b.Equals b2)

/// Converts Box2i types.
type Box2iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Box2i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let box2i = source :?> Box2i
            Symbols
                ([Symbols ([Number (string box2i.Min.X, ValueNone); Number (string box2i.Min.Y, ValueNone)], ValueNone)
                  Symbols ([Number (string box2i.Size.X, ValueNone); Number (string box2i.Size.Y, ValueNone)], ValueNone)], ValueNone) :> obj
        elif destType = typeof<Box2i> then source
        else failconv "Invalid Box2iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box2i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([minSymbol; sizeSymbol], _) ->
                match (minSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _)], _), Symbols ([Number (sx, _); Number (sy, _)], _)) ->
                    Box2i (Int32.Parse px, Int32.Parse py, Int32.Parse sx, Int32.Parse sy) :> obj
                | _ -> failconv "Invalid Box2iConverter conversion from source." (Some symbol)
            | _ -> failconv "Invalid Box2iConverter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box2iConverter conversion from source." None

/// Converts Matrix4x4 types.
type Matrix4x4Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Matrix4x4>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v4 = source :?> Matrix4x4
            Symbols
                ([Number (string v4.M11, ValueNone); Number (string v4.M12, ValueNone); Number (string v4.M13, ValueNone); Number (string v4.M14, ValueNone)
                  Number (string v4.M21, ValueNone); Number (string v4.M22, ValueNone); Number (string v4.M23, ValueNone); Number (string v4.M24, ValueNone)
                  Number (string v4.M31, ValueNone); Number (string v4.M32, ValueNone); Number (string v4.M33, ValueNone); Number (string v4.M34, ValueNone)
                  Number (string v4.M41, ValueNone); Number (string v4.M42, ValueNone); Number (string v4.M43, ValueNone); Number (string v4.M44, ValueNone)],
                 ValueNone) :> obj
        elif destType = typeof<Matrix4x4> then source
        else failconv "Invalid Matrix4x4Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Matrix4x4>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols
                ([Number (m11, _); Number (m12, _); Number (m13, _); Number (m14, _)
                  Number (m21, _); Number (m22, _); Number (m23, _); Number (m24, _)
                  Number (m31, _); Number (m32, _); Number (m33, _); Number (m34, _)
                  Number (m41, _); Number (m42, _); Number (m43, _); Number (m44, _)],
                 _) ->
                Matrix4x4
                    (Single.Parse m11, Single.Parse m12, Single.Parse m13, Single.Parse m14,
                     Single.Parse m21, Single.Parse m22, Single.Parse m23, Single.Parse m24,
                     Single.Parse m31, Single.Parse m32, Single.Parse m33, Single.Parse m34,
                     Single.Parse m41, Single.Parse m42, Single.Parse m43, Single.Parse m44) :> obj
            | _ -> failconv "Invalid Matrix4x4Converter conversion from source." (Some symbol)
        | :? Matrix4x4 -> source
        | _ -> failconv "Invalid Matrix4x4Converter conversion from source." None

[<AutoOpen>]
module Matrix4x4 =

    type Matrix4x4 with

        /// The scale extracted from an affine matrix.
        member inline this.Scale =
            Vector3
                (Vector3(this.M11, this.M21, this.M31).Magnitude,
                 Vector3(this.M12, this.M22, this.M32).Magnitude,
                 Vector3(this.M13, this.M23, this.M33).Magnitude)

        /// The right vector of the matrix.
        member inline this.Right =
            v3 this.M11 this.M21 this.M31

        /// The up vector of the matrix.
        member inline this.Up =
            v3 this.M12 this.M22 this.M32

        /// The forward vector of the matrix.
        member inline this.Forward =
            v3 this.M13 this.M23 this.M33

        /// Right, up, and forward matrix vectors.
        member inline this.RightUpForward =
            (this.Right, this.Up, this.Forward)

        /// The rotation extracted from an affine matrix.
        member inline this.Rotation =
            let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
            if Matrix4x4.Decompose (this, &scale, &rotation, &position) then rotation
            else quatIdentity

        /// The inverted value of a matrix.
        /// Throws if no determinant.
        member inline this.Inverted =
            let mutable result = Unchecked.defaultof<_>
            if not (Matrix4x4.Invert (this, &result)) then failwith "Failed to invert matrix."
            result

        /// Create a matrix from an array of 16 single values.
        static member CreateFromArray (arr : single array) =
            Matrix4x4
                (arr.[00], arr.[01], arr.[02], arr.[03],
                 arr.[04], arr.[05], arr.[06], arr.[07],
                 arr.[08], arr.[09], arr.[10], arr.[11],
                 arr.[12], arr.[13], arr.[14], arr.[15])

        /// Convert a Matrix4x4 to an array.
        member this.ToArray () =
            let value = Array.zeroCreate 16
            value.[00] <- this.M11; value.[01] <- this.M12; value.[02] <- this.M13; value.[03] <- this.M14
            value.[04] <- this.M21; value.[05] <- this.M22; value.[06] <- this.M23; value.[07] <- this.M24
            value.[08] <- this.M31; value.[09] <- this.M32; value.[10] <- this.M33; value.[11] <- this.M34
            value.[12] <- this.M41; value.[13] <- this.M42; value.[14] <- this.M43; value.[15] <- this.M44
            value

        /// Convert a Matrix4x4 to an array.
        member this.ToArray (value : single array, offset) =
            value.[offset+00] <- this.M11; value.[offset+01] <- this.M12; value.[offset+02] <- this.M13; value.[offset+03] <- this.M14
            value.[offset+04] <- this.M21; value.[offset+05] <- this.M22; value.[offset+06] <- this.M23; value.[offset+07] <- this.M24
            value.[offset+08] <- this.M31; value.[offset+09] <- this.M32; value.[offset+10] <- this.M33; value.[offset+11] <- this.M34
            value.[offset+12] <- this.M41; value.[offset+13] <- this.M42; value.[offset+14] <- this.M43; value.[offset+15] <- this.M44

    let inline m4 (r0 : Vector4) (r1 : Vector4) (r2 : Vector4) (r3 : Vector4) =
        Matrix4x4
            (r0.X, r0.Y, r0.Z, r0.W,
             r1.X, r1.Y, r1.Z, r1.W,
             r2.X, r2.Y, r2.Z, r2.W,
             r3.X, r3.Y, r3.Z, r3.W)

    let inline m4Eq (x : Matrix4x4) (y : Matrix4x4) = x.Equals y
    let inline m4Neq (x : Matrix4x4) (y : Matrix4x4) = not (x.Equals y)
    let m4Identity = Matrix4x4.Identity
    let m4Zero = Unchecked.defaultof<Matrix4x4>

    /// Create a matrix from translation, rotation, and scale.
    let CreateFromTrs (translation, rotation, scale : Vector3) =
        let rotationMatrix = Matrix4x4.CreateFromQuaternion rotation
        let scaleMatrix = Matrix4x4.CreateScale scale
        let mutable trs = scaleMatrix * rotationMatrix
        trs.Translation <- translation
        trs

    /// Create a rotation matrix from three orthogonal vectors.
    let CreateRotation (right : Vector3, up : Vector3, forward : Vector3) =
        Matrix4x4
            (right.X, up.X, forward.X, 0.0f,
             right.Y, up.Y, forward.Y, 0.0f,
             right.Z, up.Z, forward.Z, 0.0f,
             0.0f, 0.0f, 0.0f, 1.0f)

[<AutoOpen>]
module Color =

    type Color with

        member this.MapR mapper = Color (mapper this.R, this.G, this.B, this.A)
        member this.MapG mapper = Color (this.R, mapper this.G, this.B, this.A)
        member this.MapB mapper = Color (this.R, this.G, mapper this.B, this.A)
        member this.MapA mapper = Color (this.R, this.G, this.B, mapper this.A)
        member this.MapR8 mapper = Color (mapper this.R8, this.G8, this.B8, this.A8)
        member this.MapG8 mapper = Color (this.R8, mapper this.G8, this.B8, this.A8)
        member this.MapB8 mapper = Color (this.R8, this.G8, mapper this.B8, this.A8)
        member this.MapA8 mapper = Color (this.R8, this.G8, this.B8, mapper this.A8)
        member this.ScaleR scalar = Color (this.R * scalar, this.G, this.B, this.A)
        member this.ScaleG scalar = Color (this.R, this.G * scalar, this.B, this.A)
        member this.ScaleB scalar = Color (this.R, this.G, this.B * scalar, this.A)
        member this.ScaleA scalar = Color (this.R, this.G, this.B, this.A * scalar)
        member this.ScaleR8 scalar = Color (byte (single this.R8 * scalar), this.G8, this.B8, this.A8)
        member this.ScaleG8 scalar = Color (this.R8, byte (single this.G8 * scalar), this.B8, this.A8)
        member this.ScaleB8 scalar = Color (this.R8, this.G8, byte (single this.B * scalar), this.A8)
        member this.ScaleA8 scalar = Color (this.R8, this.G8, this.B8, byte (single this.A8 * scalar))
        member this.WithR r = Color (r, this.G, this.B, this.A)
        member this.WithG g = Color (this.R, g, this.B, this.A)
        member this.WithB b = Color (this.R, this.G, b, this.A)
        member this.WithA a = Color (this.R, this.G, this.B, a)
        member this.WithR8 r = Color (r, this.G8, this.B8, this.A8)
        member this.WithG8 g = Color (this.R8, g, this.B8, this.A8)
        member this.WithB8 b = Color (this.R8, this.G8, b, this.A8)
        member this.WithA8 a = Color (this.R8, this.G8, this.B8, a)

        static member Pow (a : Color, b : Color) =
            Color
                (single (Math.Pow (double a.R, double b.R)),
                 single (Math.Pow (double a.G, double b.G)),
                 single (Math.Pow (double a.B, double b.B)),
                 single (Math.Pow (double a.A, double b.A)))

        static member Modulo (a : Color, b : Color) =
            Color
                (a.R % b.R,
                 a.G % b.G,
                 a.B % b.B,
                 a.A % b.A)

    let inline color (r : single) (g : single) (b : single) (a : single) = Color (r, g, b, a)
    let inline colorDup (a : single) = color a a a a
    let inline color8 (r : byte) (g : byte) (b : byte) (a : byte) = Color (r, g, b, a)
    let inline color8Dup (a : byte) = color8 a a a a
    let inline colorPacked (u : uint) = Color u
    let inline colorEq (x : Color) (y : Color) = x.R = y.R && x.G = y.G && x.B = y.B && x.A = y.A
    let inline colorNeq (x : Color) (y : Color) = x.R <> y.R || x.G <> y.G || x.B <> y.B || x.A <> y.A
    let colorZero = Color.Zero
    let colorOne = Color.One

/// Converts Color types.
type ColorConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Color>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let color = source :?> Color
            Symbols
                ([Number (string color.R, ValueNone)
                  Number (string color.G, ValueNone)
                  Number (string color.B, ValueNone)
                  Number (string color.A, ValueNone)], ValueNone) :> obj
        elif destType = typeof<Color> then source
        else failconv "Invalid ColorConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Color>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Atom (str, _)
            | Text (str, _) -> // NOTE: can also come in as text from a csv file.
                let packed = match UInt32.TryParse (str.Substring 1, NumberStyles.HexNumber, CultureInfo.InvariantCulture) with (true, color) -> uint color | (false, _) -> 0u
                Color packed :> obj
            | Symbols ([Number (r, _); Number (g, _); Number (b, _); Number (a, _)], _) ->
                Color (Single.Parse r, Single.Parse g, Single.Parse b, Single.Parse a) :> obj
            | _ -> failconv "Invalid ColorConverter conversion from source." (Some symbol)
        | :? Color -> source
        | _ -> failconv "Invalid ColorConverter conversion from source." None

[<AutoOpen>]
module Plane3 =

    let inline plane3 (pointOnPlane : Vector3) (normal : Vector3) = Plane3 (pointOnPlane, normal)
    let inline plane3Equation (normal : Vector3) (d : single) = Plane3 (normal, d)
    let inline plane3Eq (left : Plane3) (right : Plane3) = left.Equals right
    let inline plane3Neq (left : Plane3) (right : Plane3) = not (left.Equals right)

    type Plane3 with

        /// Attempt to find the intersection of the given ray with the plane.
        /// TODO: move this to Ray3.cs.
        member this.Intersection (ray : Ray3) =
            ray.Intersection this

[<AutoOpen>]
module Ray3 =

    let inline ray (origin : Vector3) (direction : Vector3) = Ray3 (origin, direction)
    let inline rayEq (left : Ray3) (right : Ray3) = left.Equals right
    let inline rayNeq (left : Ray3) (right : Ray3) = not (left.Equals right)

/// Composition of individual affine matrix components.
type [<Struct>] Affine =
    { mutable Translation : Vector3
      mutable Rotation : Quaternion
      mutable Scale : Vector3 }
    
    member this.Matrix =
        Matrix4x4.CreateFromTrs (this.Translation, this.Rotation, this.Scale)
    
    static member make translation rotation scale =
        { Translation = translation; Rotation = rotation; Scale = scale }
    
    static member makeFromMatrix affineMatrix =
        let mutable scale = v3One
        let mutable rotation = quatIdentity
        let mutable translation = v3Zero
        if not (Matrix4x4.Decompose (affineMatrix, &scale, &rotation, &translation)) then
            Log.info "Matrix4x4.Decompose failed to find determinant. Using identity instead."
            Affine.Identity
        else Affine.make translation rotation scale
    
    static member makeTranslation translation =
        Affine.make translation quatIdentity v3One
    
    static member makeRotation translation =
        Affine.make v3Zero translation v3One
    
    static member makeScale scale =
        Affine.make v3Zero quatIdentity scale
    
    static member Identity =
        Affine.make v3Zero quatIdentity v3One

/// The flipness of an image.
type [<Struct>] Flip =
    | FlipNone
    | FlipH
    | FlipV
    | FlipHV

/// Type of light.
type LightType =
    | PointLight
    | DirectionalLight
    | SpotLight of ConeInner : single * ConeOuter : single

[<RequireQualifiedAccess>]
module Math =

    let mutable private Initialized = false

    /// Initializes the type converters found in Math.fs.
    let Init () =
        if not Initialized then
            assignTypeConverter<Vector2, Vector2Converter> ()
            assignTypeConverter<Vector3, Vector3Converter> ()
            assignTypeConverter<Vector4, Vector4Converter> ()
            assignTypeConverter<Vector2i, Vector2iConverter> ()
            assignTypeConverter<Vector3i, Vector3iConverter> ()
            assignTypeConverter<Vector4i, Vector4iConverter> ()
            assignTypeConverter<Quaternion, QuaternionConverter> ()
            assignTypeConverter<Box2, Box2Converter> ()
            assignTypeConverter<Box3, Box3Converter> ()
            assignTypeConverter<Box2i, Box2iConverter> ()
            assignTypeConverter<Matrix4x4, Matrix4x4Converter> ()
            assignTypeConverter<Color, ColorConverter> ()
            Initialized <- true

    /// Convert radians to degrees.
    let RadiansToDegrees (radians : single) =
        radians.ToDegrees ()

    /// Convert radians to degrees in 3d.
    let RadiansToDegrees3d (radians : Vector3) =
        v3
            (RadiansToDegrees radians.X)
            (RadiansToDegrees radians.Y)
            (RadiansToDegrees radians.Z)

    /// Convert degrees to radians.
    let DegreesToRadians (degrees : single) =
        degrees.ToRadians ()

    /// Convert degrees to radians in 3d.
    let DegreesToRadians3d (degrees : Vector3) =
        v3
            (DegreesToRadians degrees.X)
            (DegreesToRadians degrees.Y)
            (DegreesToRadians degrees.Z)

    /// Snap an int value to an offset.
    let SnapI offset (value : int) =
        if offset <> 0 then
            let (div, rem) = Math.DivRem (value, offset)
            let rem = if single rem < single offset * 0.5f then 0 else offset
            div * offset + rem
        else value

    /// Snap a single value to an offset.
    /// Has a minimum granularity of 0.01f.
    let SnapF (offset : single) (value : single) =
        single (SnapI (int (round (offset * 100.0f))) (int (round (value * 100.0f)))) / 100.0f

    /// Snap a Vector3 value to an offset.
    /// Has a minimum granularity of 0.001f.
    let SnapF3d offset (v3 : Vector3) =
        Vector3 (SnapF offset v3.X, SnapF offset v3.Y, SnapF offset v3.Z)

    /// Snap a degree value to an offset.
    /// Has a minimum granularity of 1.0f.
    let SnapDegree (offset : single) (value : single) =
        single (SnapI (int (round offset)) (int (round value)))

    /// Snap a degree value to an offset.
    /// Has a minimum granularity of 1.0f.
    let SnapDegree3d offset (v3 : Vector3) =
        Vector3 (SnapDegree offset v3.X, SnapDegree offset v3.Y, SnapDegree offset v3.Z)

    /// Find the the union of a line segment and a frustum if one exists.
    let tryUnionSegmentAndFrustum (start : Vector3) (stop : Vector3) (frustum : Frustum) =
        let startContained = frustum.Contains start <> ContainmentType.Disjoint
        let stopContained = frustum.Contains stop <> ContainmentType.Disjoint
        if startContained || stopContained then
            let start =
                if not startContained then
                    let ray = Ray3 (start, (stop - start).Normalized)
                    let tOpt = frustum.Intersects ray
                    if tOpt.HasValue
                    then Vector3.Lerp (start, stop, tOpt.Value)
                    else start // TODO: figure out why intersection could fail here.
                else start
            let stop =
                if not stopContained then
                    let ray = Ray3 (stop, (start - stop).Normalized)
                    let tOpt = frustum.Intersects ray
                    if tOpt.HasValue
                    then Vector3.Lerp (stop, start, tOpt.Value)
                    else stop // TODO: figure out why intersection could fail here.
                else stop
            Some (start, stop)
        else None