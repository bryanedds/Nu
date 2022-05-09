// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.ComponentModel
open System.Globalization
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module Vector2 =

    type Vector2 with
        member this.V3 = Vector3 (this.X, this.Y, 0.0f)
        member this.Magnitude = this.Length ()
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.MapX mapper = Vector2 (mapper this.X, this.Y)
        member this.MapY mapper = Vector2 (this.X, mapper this.Y)
        member this.WithX x = Vector2 (x, this.Y)
        member this.WithY y = Vector2 (this.X, y)
        member this.Rotate r = Vector2 (cos r * this.X - sin r * this.Y, sin r * this.X + cos r * this.Y)

    let inline v2 x y = Vector2 (x, y)
    let inline v2Eq (x : Vector2) (y : Vector2) = x.X = y.X && x.Y = y.Y
    let inline v2Neq (x : Vector2) (y : Vector2) = x.X <> y.X || x.Y <> y.Y
    let inline v2Dup (a : single) = v2 a a
    let v2One = Vector2.One
    let v2Zero = Vector2.Zero
    let v2UnitX = Vector2.UnitX
    let v2UnitY = Vector2.UnitY
    let v2Up = v2 0.0f 1.0f
    let v2Right = v2 1.0f 0.0f
    let v2Down = v2 0.0f -1.0f
    let v2Left = v2 -1.0f 0.0f

/// The Vector2 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector2Pluggable =
    { Vector2 : Vector2 }

    static member equals left right =
        v2Eq left.Vector2 right.Vector2

    static member compare left right =
        compare
            struct (left.Vector2.X, left.Vector2.Y)
            struct (right.Vector2.X, right.Vector2.Y)

    override this.GetHashCode () =
        hash this.Vector2

    override this.Equals that =
        match that with
        | :? Vector2Pluggable as that -> Vector2Pluggable.equals this that
        | _ -> failwithumf ()

    interface Vector2Pluggable IComparable with
        member this.CompareTo that =
            Vector2Pluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector2Pluggable as that -> (this :> Vector2Pluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector2"

        member this.FSharpType =
            getType this.Vector2

        member this.ToSymbol () =
            let v2 = Symbol.Atom ("v2", None)
            let x = Symbol.Number (scstring this.Vector2.X, None)
            let y = Symbol.Number (scstring this.Vector2.Y, None)
            Symbol.Symbols ([v2; x; y], None)

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
                ([Number (scstring v2.X, None)
                  Number (scstring v2.Y, None)], None) :> obj
        elif destType = typeof<Vector2> then source
        else failconv "Invalid Vector2Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2 (scvalue x, scvalue y) :> obj
            | _ -> failconv "Invalid Vector2Converter conversion from source." (Some symbol)
        | :? Vector2 -> source
        | _ -> failconv "Invalid Vector2Converter conversion from source." None

[<AutoOpen>]
module Vector3 =

    type Vector3 with
        member this.V2 = Vector2 (this.X, this.Y)
        member this.Magnitude = this.Length ()
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.MapX mapper = Vector3 (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3 (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3 (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3 (x, this.Y, this.Z)
        member this.WithY y = Vector3 (this.X, y, this.Z)
        member this.WithZ z = Vector3 (this.X, this.Y, z)

    let inline v3 x y z = Vector3 (x, y, z)
    let inline v3Eq (x : Vector3) (y : Vector3) = x.X = y.X && x.Y = y.Y && x.Z = y.Z
    let inline v3Neq (x : Vector3) (y : Vector3) = x.X <> y.X || x.Y <> y.Y || x.Z <> y.Z
    let inline v3Dup (a : single) = v3 a a a
    let v3Cartesian2d = v3 0.5f 0.5f 0.0f
    let v3Cartesian3d = v3 0.5f 0.5f 0.5f
    let v3One = Vector3.One
    let v3Zero = Vector3.Zero
    let v3UnitX = Vector3.UnitX
    let v3UnitY = Vector3.UnitY
    let v3UnitZ = Vector3.UnitZ
    let v3Up = v3 0.0f 1.0f 0.0f
    let v3Right = v3 1.0f 0.0f 0.0f
    let v3Down = v3 0.0f -1.0f 0.0f
    let v3Left = v3 -1.0f 0.0f 0.0f
    let v3Forward = v3 -1.0f 0.0f 0.0f
    let v3Backward = v3 1.0f 0.0f 0.0f

/// The Vector3 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector3Pluggable =
    { Vector3 : Vector3 }

    static member equals left right =
        v3Eq left.Vector3 right.Vector3

    static member compare left right =
        compare
            struct (left.Vector3.X, left.Vector3.Y, left.Vector3.Z)
            struct (right.Vector3.X, right.Vector3.Y, right.Vector3.Z)

    override this.GetHashCode () =
        hash this.Vector3

    override this.Equals that =
        match that with
        | :? Vector3Pluggable as that -> Vector3Pluggable.equals this that
        | _ -> failwithumf ()

    interface Vector3Pluggable IComparable with
        member this.CompareTo that =
            Vector3Pluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector3Pluggable as that -> (this :> Vector3Pluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector3"

        member this.FSharpType =
            getType this.Vector3

        member this.ToSymbol () =
            let v3 = Symbol.Atom ("v3", None)
            let x = Symbol.Number (scstring this.Vector3.X, None)
            let y = Symbol.Number (scstring this.Vector3.Y, None)
            let z = Symbol.Number (scstring this.Vector3.Z, None)
            Symbol.Symbols ([v3; x; y; z], None)

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
                ([Number (scstring v3.X, None)
                  Number (scstring v3.Y, None)
                  Number (scstring v3.Z, None)], None) :> obj
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
                Vector3 (scvalue x, scvalue y, scvalue z) :> obj
            | _ ->
                failconv "Invalid Vector3Converter conversion from source." (Some symbol)
        | :? Vector3 -> source
        | _ -> failconv "Invalid Vector3Converter conversion from source." None

[<AutoOpen>]
module Vector4 =

    type Vector4 with
        member this.V3 = v3 this.X this.Y this.Z
        member this.Magnitude = this.Magnitude
        member this.MagnitudeSquared = this.LengthSquared ()
        member this.MapX mapper = Vector4 (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4 (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4 (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4 (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4 (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4 (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4 (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4 (this.X, this.Y, this.Z, w)

    let inline v4 x y z w = Vector4 (x, y, z, w)
    let inline v4Eq (x : Vector4) (y : Vector4) = x.X = y.X && x.Y = y.Y && x.Z = y.Z && x.W = y.W
    let inline v4Neq (x : Vector4) (y : Vector4) = x.X <> y.X || x.Y <> y.Y || x.Z <> y.Z || x.W <> y.W
    let inline v4Dup (a : single) = v4 a a a a
    let v4One = Vector4.One
    let v4Zero = Vector4.Zero
    let v4UnitX = Vector4.UnitX
    let v4UnitY = Vector4.UnitY
    let v4UnitZ = Vector4.UnitZ
    let v4UnitW = Vector4.UnitW

/// The Vector4 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector4Pluggable =
    { Vector4 : Vector4 }

    static member equals left right =
        v4Eq left.Vector4 right.Vector4

    static member compare left right =
        compare
            struct (left.Vector4.X, left.Vector4.Y, left.Vector4.Z, left.Vector4.W)
            struct (right.Vector4.X, right.Vector4.Y, right.Vector4.Z, right.Vector4.W)

    override this.GetHashCode () =
        hash this.Vector4

    override this.Equals that =
        match that with
        | :? Vector4Pluggable as that -> Vector4Pluggable.equals this that
        | _ -> failwithumf ()

    interface Vector4Pluggable IComparable with
        member this.CompareTo that =
            Vector4Pluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector4Pluggable as that -> (this :> Vector4Pluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector4"

        member this.FSharpType =
            getType this.Vector4

        member this.ToSymbol () =
            let v4 = Symbol.Atom ("v4", None)
            let x = Symbol.Number (scstring this.Vector4.X, None)
            let y = Symbol.Number (scstring this.Vector4.Y, None)
            let z = Symbol.Number (scstring this.Vector4.Z, None)
            let w = Symbol.Number (scstring this.Vector4.W, None)
            Symbol.Symbols ([v4; x; y; z; w], None)

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
                ([Number (scstring v4.X, None)
                  Number (scstring v4.Y, None)
                  Number (scstring v4.Z, None)
                  Number (scstring v4.W, None)], None) :> obj
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
                Vector4 (scvalue x, scvalue y, scvalue z, scvalue w) :> obj
            | _ ->
                failconv "Invalid Vector4Converter conversion from source." (Some symbol)
        | :? Vector4 -> source
        | _ -> failconv "Invalid Vector4Converter conversion from source." None

[<AutoOpen>]
module Vector2i =

    type Vector2i with
        member this.V3i = Vector3i (this.X, this.Y, 0)
        member this.MapX mapper = Vector2i (mapper this.X, this.Y)
        member this.MapY mapper = Vector2i (this.X, mapper this.Y)
        member this.WithX x = Vector2i (x, this.Y)
        member this.WithY y = Vector2i (this.X, y)

    let inline v2i x y = Vector2i (x, y)
    let inline v2iEq (x : Vector2i) (y : Vector2i) = x.X = y.X && x.Y = y.Y
    let inline v2iNeq (x : Vector2i) (y : Vector2i) = x.X <> y.X || x.Y <> y.Y
    let inline v2iDup (a : int) = v2i a a
    let v2iOne = Vector2i.One
    let v2iZero = Vector2i.Zero
    let v2iUnitX = Vector2i.UnitX
    let v2iUnitY = Vector2i.UnitY
    let v2iUp = Vector2i.Up
    let v2iRight = Vector2i.Right
    let v2iDown = Vector2i.Down
    let v2iLeft = Vector2i.Left

/// The Vector2i value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector2iPluggable =
    { Vector2i : Vector2i }

    static member equals left right =
        v2iEq left.Vector2i right.Vector2i

    static member compare left right =
        compare
            struct (left.Vector2i.X, left.Vector2i.Y)
            struct (right.Vector2i.X, right.Vector2i.Y)

    override this.GetHashCode () =
        hash this.Vector2i

    override this.Equals that =
        match that with
        | :? Vector2iPluggable as that -> Vector2iPluggable.equals this that
        | _ -> failwithumf ()

    interface Vector2iPluggable IComparable with
        member this.CompareTo that =
            Vector2iPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector2iPluggable as that -> (this :> Vector2iPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector2i"

        member this.FSharpType =
            getType this.Vector2i

        member this.ToSymbol () =
            let v2i = Symbol.Atom ("v2i", None)
            let x = Symbol.Number (scstring this.Vector2i.X, None)
            let y = Symbol.Number (scstring this.Vector2i.Y, None)
            Symbol.Symbols ([v2i; x; y], None)

/// Converts Vector2i types.
type Vector2iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector2i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v2i = source :?> Vector2i
            Symbols ([Number (scstring v2i.X, None); Number (scstring v2i.Y, None)], None) :> obj
        elif destType = typeof<Vector2i> then source
        else failconv "Invalid Vector2iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2i (scvalue x, scvalue y) :> obj
            | _ -> failconv "Invalid Vector2iConverter conversion from source." (Some symbol)
        | :? Vector2i -> source
        | _ -> failconv "Invalid Vector2iConverter conversion from source." None

[<AutoOpen>]
module Vector3i =

    type Vector3i with
        member this.V2i = Vector2i (this.X, this.Y)
        member this.MapX mapper = Vector3i (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3i (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3i (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3i (x, this.Y, this.Z)
        member this.WithY y = Vector3i (this.X, y, this.Z)
        member this.WithZ z = Vector3i (this.X, this.Y, z)

    let inline v3i x y z = Vector3i (x, y, z)
    let inline v3iEq (x : Vector3i) (y : Vector3i) = x.X = y.X && x.Y = y.Y && x.Z = y.Z
    let inline v3iNeq (x : Vector3i) (y : Vector3i) = x.X <> y.X || x.Y <> y.Y || x.Z <> y.Z
    let inline v3iDup (a : int) = v3i a a a
    let v3iOne = Vector3i.One
    let v3iZero = Vector3i.Zero
    let v3iUnitX = Vector3i.UnitX
    let v3iUnitY = Vector3i.UnitY
    let v3iUnitZ = Vector3i.UnitZ

/// The Vector3 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector3iPluggable =
    { Vector3i : Vector3i }

    static member equals left right =
        v3iEq left.Vector3i right.Vector3i

    static member compare left right =
        compare
            struct (left.Vector3i.X, left.Vector3i.Y, left.Vector3i.Z)
            struct (right.Vector3i.X, right.Vector3i.Y, right.Vector3i.Z)

    override this.GetHashCode () =
        hash this.Vector3i

    override this.Equals that =
        match that with
        | :? Vector3iPluggable as that -> Vector3iPluggable.equals this that
        | _ -> failwithumf ()

    interface Vector3iPluggable IComparable with
        member this.CompareTo that =
            Vector3iPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector3iPluggable as that -> (this :> Vector3iPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector3i"

        member this.FSharpType =
            getType this.Vector3i

        member this.ToSymbol () =
            let v3i = Symbol.Atom ("v3i", None)
            let x = Symbol.Number (scstring this.Vector3i.X, None)
            let y = Symbol.Number (scstring this.Vector3i.Y, None)
            let z = Symbol.Number (scstring this.Vector3i.Z, None)
            Symbol.Symbols ([v3i; x; y; z], None)

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
                ([Number (scstring v3i.X, None)
                  Number (scstring v3i.Y, None)
                  Number (scstring v3i.Z, None)], None) :> obj
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
                Vector3i (scvalue x, scvalue y, scvalue z) :> obj
            | _ ->
                failconv "Invalid Vector3iConverter conversion from source." (Some symbol)
        | :? Vector3i -> source
        | _ -> failconv "Invalid Vector3iConverter conversion from source." None

[<AutoOpen>]
module Vector4i =

    type Vector4i with
        member this.MapX mapper = Vector4i (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4i (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4i (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4i (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4i (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4i (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4i (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4i (this.X, this.Y, this.Z, w)

    let inline v4i x y z w = Vector4i (x, y, z, w)
    let inline v4iEq (x : Vector4i) (y : Vector4i) = x.X = y.X && x.Y = y.Y && x.Z = y.Z && x.W = y.W
    let inline v4iNeq (x : Vector4i) (y : Vector4i) = x.X <> y.X || x.Y <> y.Y || x.Z <> y.Z || x.W <> y.W
    let inline v4iDup (a : int) = v4i a a a a
    let v4iOne = Vector4i.One
    let v4iZero = Vector4i.Zero
    let v4iUnitX = Vector4i.UnitX
    let v4iUnitY = Vector4i.UnitY
    let v4iUnitZ = Vector4i.UnitZ
    let v4iUnitW = Vector4i.UnitW

/// The Vector4i value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector4iPluggable =
    { Vector4i : Vector4i }

    static member equals left right =
        v4iEq left.Vector4i right.Vector4i

    static member compare left right =
        compare
            struct (left.Vector4i.X, left.Vector4i.Y, left.Vector4i.Z, left.Vector4i.W)
            struct (right.Vector4i.X, right.Vector4i.Y, right.Vector4i.Z, right.Vector4i.W)

    override this.GetHashCode () =
        hash this.Vector4i

    override this.Equals that =
        match that with
        | :? Vector4iPluggable as that -> Vector4iPluggable.equals this that
        | _ -> failwithumf ()

    interface Vector4iPluggable IComparable with
        member this.CompareTo that =
            Vector4iPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? Vector4iPluggable as that -> (this :> Vector4iPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Vector4i"

        member this.FSharpType =
            getType this.Vector4i

        member this.ToSymbol () =
            let v4i = Symbol.Atom ("v4i", None)
            let x = Symbol.Number (scstring this.Vector4i.X, None)
            let y = Symbol.Number (scstring this.Vector4i.Y, None)
            let z = Symbol.Number (scstring this.Vector4i.Z, None)
            let w = Symbol.Number (scstring this.Vector4i.W, None)
            Symbol.Symbols ([v4i; x; y; z; w], None)

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
                ([Number (scstring v4i.X, None)
                  Number (scstring v4i.Y, None)
                  Number (scstring v4i.Z, None)
                  Number (scstring v4i.W, None)], None) :> obj
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
                Vector4i (scvalue x, scvalue y, scvalue z, scvalue w) :> obj
            | _ ->
                failconv "Invalid Vector4iConverter conversion from source." (Some symbol)
        | :? Vector4i -> source
        | _ -> failconv "Invalid Vector4iConverter conversion from source." None

[<AutoOpen>]
module Quaternion =
    type Quaternion with
        member this.PitchYawRoll = MathHelper.PitchYawRoll &this

    let quatId = Quaternion.Identity
    let inline quatEq (q : Quaternion) (q2 : Quaternion) = q.X = q2.X && q.Y = q2.Y && q.Z = q2.Z && q.W = q2.W
    let inline quatNeq (q : Quaternion) (q2 : Quaternion) = q.X <> q2.X || q.Y <> q2.Y || q.Z <> q2.Z || q.W <> q2.W

/// The Quaternion value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] QuaternionPluggable =
    { Quaternion : Quaternion }

    static member equals left right =
        quatEq left.Quaternion right.Quaternion

    static member compare left right =
        compare
            struct (left.Quaternion.X, left.Quaternion.Y, left.Quaternion.Z, left.Quaternion.W)
            struct (right.Quaternion.X, right.Quaternion.Y, right.Quaternion.Z, right.Quaternion.W)

    override this.GetHashCode () =
        hash this.Quaternion

    override this.Equals that =
        match that with
        | :? QuaternionPluggable as that -> QuaternionPluggable.equals this that
        | _ -> failwithumf ()

    interface QuaternionPluggable IComparable with
        member this.CompareTo that =
            QuaternionPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? QuaternionPluggable as that -> (this :> QuaternionPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Quaternion"

        member this.FSharpType =
            getType this.Quaternion

        member this.ToSymbol () =
            let quat = Symbol.Atom ("quat", None)
            let x = Symbol.Number (scstring this.Quaternion.X, None)
            let y = Symbol.Number (scstring this.Quaternion.Y, None)
            let z = Symbol.Number (scstring this.Quaternion.Z, None)
            let w = Symbol.Number (scstring this.Quaternion.W, None)
            Symbol.Symbols ([quat; x; y; z; w], None)

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
                ([Number (scstring quat.X, None)
                  Number (scstring quat.Y, None)
                  Number (scstring quat.Z, None)
                  Number (scstring quat.W, None)], None) :> obj
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
                Quaternion (scvalue x, scvalue y, scvalue z, scvalue w) :> obj
            | _ ->
                failconv "Invalid QuaternionConverter conversion from source." (Some symbol)
        | :? Quaternion -> source
        | _ -> failconv "Invalid QuaternionConverter conversion from source." None

[<AutoOpen>]
module Box2 =
    type Box2 with
        member this.Box3 = Box3 (v3 this.Position.X this.Position.Y 0.0f, v3 this.Size.X this.Size.Y 0.0f)
        member this.Width = this.Size.X
        member this.Height = this.Size.Y
        member this.Extent = this.Size * 0.5f
        member this.Center = this.Position + this.Extent
        member this.Top = v2 (this.Position.X + this.Size.X * 0.5f) (this.Position.Y + this.Size.Y)
        member this.Bottom = v2 (this.Position.X + this.Size.X * 0.5f) this.Position.Y
        member this.Right = v2 (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y * 0.5f)
        member this.Left = v2 this.Position.X (this.Position.Y + this.Size.Y * 0.5f)
        member this.TopLeft = v2 this.Position.X (this.Position.Y + this.Size.Y)
        member this.TopRight = v2 (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y)
        member this.BottomLeft = this.Position
        member this.BottomRight = v2 (this.Position.X + this.Size.X) this.Position.Y
        member this.IsEmpty = this.Equals Box2.Zero
        member this.Translate translation = Box2 (this.Position + translation, this.Size)
        member this.WithPosition position = Box2 (position, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithSize size = Box2 (this.Position, size)

    let box2Zero = Box2.Zero
    let inline box2 position size = Box2 (position, size)
    let inline box2Eq (b : Box2) (b2 : Box2) =
        b.Position.X = b2.Position.X && b.Position.Y = b2.Position.Y &&
        b.Size.X = b2.Size.X && b.Size.Y = b2.Size.Y
    let inline box2Neq (b : Box2) (b2 : Box2) =
        b.Position.X <> b2.Position.X || b.Position.Y <> b2.Position.Y ||
        b.Size.X <> b2.Size.X || b.Size.Y <> b2.Size.Y

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
                ([Symbols ([Number (scstring box2.Position.X, None); Number (scstring box2.Position.Y, None)], None)
                  Symbols ([Number (scstring box2.Size.X, None); Number (scstring box2.Size.Y, None)], None)], None) :> obj
        elif destType = typeof<Box2> then source
        else failconv "Invalid Box2Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([positionSymbol; sizeSymbol], _) ->
                match (positionSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _)], _), Symbols ([Number (sx, _); Number (sy, _)], _)) ->
                    Box2 (scvalue px, scvalue py, scvalue sx, scvalue sy) :> obj
                | _ ->
                    failconv "Invalid Box2Converter conversion from source." (Some symbol)
            | _ ->
                failconv "Invalid Box2Converter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box2Converter conversion from source." None

[<AutoOpen>]
module Box3 =
    type Box3 with
        member this.Box2 = Box2 (v2 this.Position.X this.Position.Y, v2 this.Size.X this.Size.Y)
        member this.Extent = this.Size * 0.5f
        member this.Center = this.Position + this.Extent
        member this.Top = v3 (this.Position.X + this.Size.X * 0.5f) (this.Position.Y + this.Size.Y) (this.Position.Z + this.Size.Z * 0.5f)
        member this.Bottom = v3 (this.Position.X + this.Size.X * 0.5f) this.Position.Y (this.Position.Z + this.Size.Z * 0.5f)
        member this.Right = v3 (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y * 0.5f) (this.Position.Z + this.Size.Z * 0.5f)
        member this.Left = v3 this.Position.X (this.Position.Y + this.Size.Y * 0.5f) (this.Position.Z + this.Size.Z * 0.5f)
        member this.TopLeft = v3 this.Position.X (this.Position.Y + this.Size.Y) (this.Position.Z + this.Size.Z * 0.5f)
        member this.TopRight = v3 (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y) (this.Position.Z + this.Size.Z * 0.5f)
        member this.BottomLeft = v3 this.Position.X this.Position.Y (this.Position.Z + this.Size.Z * 0.5f)
        member this.BottomRight = v3 (this.Position.X + this.Size.X) this.Position.Y (this.Position.Z + this.Size.Z * 0.5f)
        member this.Translate translation = Box3 (this.Position + translation, this.Size)
        member this.WithPosition position = Box3 (position, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithSize size = Box3 (this.Position, size)

    let box3Zero = Box3.Zero
    let inline box3 position size = Box3 (position, size)
    let inline box3Eq (b : Box3) (b2 : Box3) =
        b.Position.X = b2.Position.X && b.Position.Y = b2.Position.Y && b.Position.Z = b2.Position.Z &&
        b.Size.X = b2.Size.X && b.Size.Y = b2.Size.Y && b.Size.Z = b2.Size.Z
    let inline box3Neq (b : Box3) (b2 : Box3) =
        b.Position.X <> b2.Position.X || b.Position.Y <> b2.Position.Y || b.Position.Z <> b2.Position.Z ||
        b.Size.X <> b2.Size.X || b.Size.Y <> b2.Size.Y || b.Size.Z <> b2.Size.Z

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
                ([Symbols ([Number (scstring box3.Position.X, None); Number (scstring box3.Position.Y, None); Number (scstring box3.Position.Z, None)], None)
                  Symbols ([Number (scstring box3.Size.X, None); Number (scstring box3.Size.Y, None); Number (scstring box3.Size.Z, None)], None)], None) :> obj
        elif destType = typeof<Box3> then source
        else failconv "Invalid Box3Converter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box3>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([positionSymbol; sizeSymbol], _) ->
                match (positionSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _); Number (pz, _)], _), Symbols ([Number (sx, _); Number (sy, _); Number (sz, _)], _)) ->
                    Box3 (scvalue px, scvalue py, scvalue pz, scvalue sx, scvalue sy, scvalue sz) :> obj
                | _ ->
                    failconv "Invalid Box3Converter conversion from source." (Some symbol)
            | _ ->
                failconv "Invalid Box3Converter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box3Converter conversion from source." None

[<AutoOpen>]
module Box2i =
    type Box2i with
        member this.Extent = this.Size / 2
        member this.Center = this.Position + this.Extent
        member this.Top = v2i (this.Position.X + this.Size.X / 2) (this.Position.Y + this.Size.Y)
        member this.Bottom = v2i (this.Position.X + this.Size.X / 2) this.Position.Y
        member this.Right = v2i (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y / 2)
        member this.Left = v2i this.Position.X (this.Position.Y + this.Size.Y / 2)
        member this.TopLeft = v2i this.Position.X (this.Position.Y + this.Size.Y)
        member this.TopRight = v2i (this.Position.X + this.Size.X) (this.Position.Y + this.Size.Y)
        member this.BottomLeft = this.Position
        member this.BottomRight = v2i (this.Position.X + this.Size.X) this.Position.Y
        member this.Translate translation = Box2i (this.Position + translation, this.Size)
        member this.WithPosition position = Box2i (position, this.Size)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithSize size = Box2i (this.Position, size)

    let box2iZero = Box2i.Zero
    let inline box2i position size = Box2i (position, size)
    let inline box2iEq (b : Box2i) (b2 : Box2i) =
        b.Position.X = b2.Position.X && b.Position.Y = b2.Position.Y &&
        b.Size.X = b2.Size.X && b.Size.Y = b2.Size.Y
    let inline box2iNeq (b : Box2i) (b2 : Box2i) =
        b.Position.X <> b2.Position.X || b.Position.Y <> b2.Position.Y ||
        b.Size.X <> b2.Size.X || b.Size.Y <> b2.Size.Y

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
                ([Symbols ([Number (scstring box2i.Position.X, None); Number (scstring box2i.Position.Y, None)], None)
                  Symbols ([Number (scstring box2i.Size.X, None); Number (scstring box2i.Size.Y, None)], None)], None) :> obj
        elif destType = typeof<Box2i> then source
        else failconv "Invalid Box2iConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Box2i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([positionSymbol; sizeSymbol], _) ->
                match (positionSymbol, sizeSymbol) with
                | (Symbols ([Number (px, _); Number (py, _)], _), Symbols ([Number (sx, _); Number (sy, _)], _)) ->
                    Box2i (scvalue px, scvalue py, scvalue sx, scvalue sy) :> obj
                | _ ->
                    failconv "Invalid Box2iConverter conversion from source." (Some symbol)
            | _ ->
                failconv "Invalid Box2iConverter conversion from source." (Some symbol)
        | :? Box2 -> source
        | _ -> failconv "Invalid Box2iConverter conversion from source." None

[<AutoOpen>]
module Matrix3x3 =

    type Matrix3x3 with

        /// Gets the inverse view matrix with a terribly hacky method custom-designed to satisfy SDL2's
        /// SDL_RenderCopyEx requirement that all corrdinates be arbitrarily converted to ints.
        /// TODO: See if we can expose an SDL_RenderCopyEx from SDL2(#) that takes floats instead.
        member this.InvertedView () =
            let mutable m = this
            m.M13 <- -m.M13
            m.M23 <- -m.M23
            m.M11 <- 1.0f / m.M11
            m.M22 <- 1.0f / m.M22
            m

    let inline m3 r0 r1 r2 = Matrix3x3 (r0, r1, r2)
    let inline m3Eq (x : Matrix3x3) (y : Matrix3x3) = x.Equals y
    let inline m3Neq (x : Matrix3x3) (y : Matrix3x3) = not (x.Equals y)
    let m3Identity = Matrix3x3.Identity
    let m3Zero = Matrix3x3.Zero

[<AutoOpen>]
module Matrix4x4 =

    type Matrix4x4 with

        /// Computes the matrix determinant.
        /// NOTE: this copies the matrix to a local variable to elide the FS0052 warning.
        member this.Determinant =
            let copy = this
            copy.GetDeterminant ()

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

    let inline color (r : single) (g : single) (b : single) (a : single) = Color (r, g, b, a)
    let inline colorDup (a : single) = color a a a a
    let inline color8 (r : byte) (g : byte) (b : byte) (a : byte) = Color (r, g, b, a)
    let inline color8Dup (a : byte) = color8 a a a a
    let inline colorEq (x : Color) (y : Color) = x.R = y.R && x.G = y.G && x.B = y.B && x.A = y.A
    let inline colorNeq (x : Color) (y : Color) = x.R <> y.R || x.G <> y.G || x.B <> y.B || x.A <> y.A
    let colorZero = Color.Zero
    let colorOne = Color.One

/// The Color value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] ColorPluggable =
    { Color : Color }

    static member equals left right =
        colorEq left.Color right.Color

    static member compare left right =
        compare
            struct (left.Color.R, left.Color.G, left.Color.B, left.Color.A)
            struct (right.Color.R, right.Color.G, right.Color.B, right.Color.A)

    override this.GetHashCode () =
        hash this.Color

    override this.Equals that =
        match that with
        | :? ColorPluggable as that -> ColorPluggable.equals this that
        | _ -> failwithumf ()

    interface ColorPluggable IComparable with
        member this.CompareTo that =
            ColorPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? ColorPluggable as that -> (this :> ColorPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Color"

        member this.FSharpType =
            getType this.Color

        member this.ToSymbol () =
            let packed = this.Color.Packed
            let unpacked = Color packed
            if this.Color.Equals unpacked then
                Symbol.Atom ("#" + packed.ToString "X8", None)
            else
                let col = Symbol.Atom ("col", None)
                let r = Symbol.Number (scstring this.Color.R, None)
                let g = Symbol.Number (scstring this.Color.G, None)
                let b = Symbol.Number (scstring this.Color.B, None)
                let a = Symbol.Number (scstring this.Color.A, None)
                Symbol.Symbols ([col; r; g; b; a], None)

/// Converts Color types.
type ColorConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Color>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let col = source :?> Color
            let packed = col.Packed
            let unpacked = Color packed
            if col.Equals unpacked then
                Symbol.Atom ("#" + packed.ToString "X8", None) :> obj
            else
                Symbols
                    ([Number (scstring col.R, None)
                      Number (scstring col.G, None)
                      Number (scstring col.B, None)
                      Number (scstring col.A, None)], None) :> obj
        elif destType = typeof<Color> then source
        else failconv "Invalid ColorConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Color>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Atom (atom, _) ->
                let packed = match UInt32.TryParse (atom.Substring 1, NumberStyles.HexNumber, CultureInfo.InvariantCulture) with (true, color) -> uint color | (false, _) -> 0u
                Color packed :> obj
            | Symbols ([Number (r, _); Number (g, _); Number (b, _); Number (a, _)], _) ->
                Color (scvalue<single> r, scvalue<single> g, scvalue<single> b, scvalue<single> a) :> obj
            | _ ->
                failconv "Invalid ColorConverter conversion from source." (Some symbol)
        | :? Color -> source
        | _ -> failconv "Invalid ColorConverter conversion from source." None

/// The input for a 2d ray cast operation.
type [<StructuralEquality; NoComparison; Struct>] RayCast2Input =
    { RayBegin : Vector2
      RayEnd : Vector2 }
      
/// The output of a 2d ray cast operation.
type [<StructuralEquality; NoComparison; Struct>] RayCast2Output =
    { mutable Normal : Vector2
      mutable Fraction : single }
    static member inline defaultOutput =
        Unchecked.defaultof<RayCast2Output>

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
            assignTypeConverter<Box2, Box2Converter> ()
            assignTypeConverter<Box3, Box3Converter> ()
            assignTypeConverter<Box2i, Box2iConverter> ()
            assignTypeConverter<Color, ColorConverter> ()
            Initialized <- true

    /// Convert radians to degrees.
    let radiansToDegrees (radians : single) =
        -radians.ToDegrees ()

    /// Convert radians to degrees in 3d.
    let radiansToDegrees3d (radians : Vector3) =
        v3
            (radiansToDegrees radians.X)
            (radiansToDegrees radians.Y)
            (radiansToDegrees radians.Z)

    /// Convert degrees to radians.
    let degreesToRadians (degrees : single) =
        -degrees.ToRadians ()

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