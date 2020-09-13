// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.ComponentModel
open Prime
open Nu

[<AutoOpen>]
module internal TransformMasks =

    // OPTIMIZATION: Transform flag bit-masks for performance.
    let [<Literal>] DirtyMask =                0b000000000001
    let [<Literal>] InvalidatedMask =          0b000000000010
    let [<Literal>] OmnipresentMask =          0b000000000100
    let [<Literal>] AbsoluteMask =             0b000000001000
    let [<Literal>] PublishChangesMask =       0b000000010000
    let [<Literal>] EnabledMask =              0b000000100000
    let [<Literal>] VisibleMask =              0b000001000000
    let [<Literal>] AlwaysUpdateMask =         0b000010000000
    let [<Literal>] PublishUpdatesMask =       0b000100000000
    let [<Literal>] PublishPostUpdatesMask =   0b001000000000
    let [<Literal>] PersistentMask =           0b010000000000

/// Carries transformation data specific to an Entity.
type [<StructuralEquality; NoComparison; Struct>] Transform =
    { // cache line begin
      mutable Position : Vector2 // NOTE: will become a Vector3 if Nu gets 3D capabilities
      mutable Size : Vector2 // NOTE: will become a Vector3 if Nu gets 3D capabilities
      mutable Rotation : single // NOTE: will become a Vector3 if Nu gets 3D capabilities
      mutable Depth : single // NOTE: will become part of position if Nu gets 3D capabilities
      mutable Flags : int
      mutable RefCount : int }
      // cache line end

    interface Transform Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
        member this.SystemNames = [||]
        member this.Junction _ _ _ = this
        member this.Disjunction _ _ _ = ()

    member this.Dirty with get () = this.Flags &&& DirtyMask <> 0 and set value = this.Flags <- if value then this.Flags ||| DirtyMask else this.Flags &&& ~~~DirtyMask
    member this.Invalidated with get () = this.Flags &&& InvalidatedMask <> 0 and set value = this.Flags <- if value then this.Flags ||| InvalidatedMask else this.Flags &&& ~~~InvalidatedMask
    member this.Omnipresent with get () = this.Flags &&& OmnipresentMask <> 0 and set value = this.Flags <- if value then this.Flags ||| OmnipresentMask else this.Flags &&& ~~~OmnipresentMask
    member this.Absolute with get () = this.Flags &&& AbsoluteMask <> 0 and set value = this.Flags <- if value then this.Flags ||| AbsoluteMask else this.Flags &&& ~~~AbsoluteMask
    member this.PublishChanges with get () = this.Flags &&& PublishChangesMask <> 0 and set value = this.Flags <- if value then this.Flags ||| PublishChangesMask else this.Flags &&& ~~~PublishChangesMask
    member this.Enabled with get () = this.Flags &&& EnabledMask <> 0 and set value = this.Flags <- if value then this.Flags ||| EnabledMask else this.Flags &&& ~~~EnabledMask
    member this.Visible with get () = this.Flags &&& VisibleMask <> 0 and set value = this.Flags <- if value then this.Flags ||| VisibleMask else this.Flags &&& ~~~VisibleMask
    member this.AlwaysUpdate with get () = this.Flags &&& AlwaysUpdateMask <> 0 and set value = this.Flags <- if value then this.Flags ||| AlwaysUpdateMask else this.Flags &&& ~~~AlwaysUpdateMask
    member this.PublishUpdates with get () = this.Flags &&& PublishUpdatesMask <> 0 and set value = this.Flags <- if value then this.Flags ||| PublishUpdatesMask else this.Flags &&& ~~~PublishUpdatesMask
    member this.PublishPostUpdates with get () = this.Flags &&& PublishPostUpdatesMask <> 0 and set value = this.Flags <- if value then this.Flags ||| PublishPostUpdatesMask else this.Flags &&& ~~~PublishPostUpdatesMask
    member this.Persistent with get () = this.Flags &&& PersistentMask <> 0 and set value = this.Flags <- if value then this.Flags ||| PersistentMask else this.Flags &&& ~~~PersistentMask
    member this.Optimized with get () = ~~~this.Flags &&& OmnipresentMask ||| ~~~this.Flags &&& PublishChangesMask = 0

    /// Assign a transform in-place.
    member this.Assign that =
        this.Position <- that.Position
        this.Size <- that.Size
        this.Rotation <- that.Rotation
        this.Depth <- that.Depth
        this.Flags <- that.Flags

    static member makeEmpty () =
        { Position = Vector2.Zero
          Size = Vector2.One
          Rotation = 0.0f
          Depth = 0.0f
          Flags = 0
          RefCount = 0 }

[<AutoOpen>]
module Vector2 =

    type Vector2 with
        member this.MapX mapper = Vector2 (mapper this.X, this.Y)
        member this.MapY mapper = Vector2 (this.X, mapper this.Y)
        member this.WithX x = Vector2 (x, this.Y)
        member this.WithY y = Vector2 (this.X, y)

    let inline v2 x y = Vector2 (x, y)
    let inline v2Dup a = v2 a a
    let v2One = Vector2.One
    let v2Zero = Vector2.Zero
    let v2UnitX = Vector2.UnitX
    let v2UnitY = Vector2.UnitY
    let v2Center (position : Vector2) (size : Vector2) = position + size * 0.5f

/// The Vector2 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector2Pluggable =
    { Vector2 : Vector2 }

    static member equals left right =
        left.Vector2 = right.Vector2

    static member compare left right =
        compare (left.Vector2.X, left.Vector2.Y) (right.Vector2.X, right.Vector2.Y)

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

[<AutoOpen>]
module Vector3 =

    type Vector3 with
        member this.MapX mapper = Vector3 (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3 (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3 (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3 (x, this.Y, this.Z)
        member this.WithY y = Vector3 (this.X, y, this.Z)
        member this.WithZ z = Vector3 (this.X, this.Y, z)

    let inline v3 x y z = Vector3 (x, y, z)
    let inline v3Dup a = v3 a a a
    let v3One = Vector3.One
    let v3Zero = Vector3.Zero
    let v3UnitX = Vector3.UnitX
    let v3UnitY = Vector3.UnitY
    let v3UnitZ = Vector3.UnitZ

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
        member this.Translate (translation : Vector2) = Vector4 (this.X + translation.X, this.Y + translation.Y, this.Z + translation.X, this.W + translation.Y)
        member this.MapX mapper = Vector4 (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4 (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4 (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4 (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4 (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4 (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4 (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4 (this.X, this.Y, this.Z, w)
        member this.WithPosition position = this.Translate (position - this.Position)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithTop top = this.Translate (top - this.Top)
        member this.WithLeft left = this.Translate (left - this.Left)
        member this.WithRight right = this.Translate (right - this.Right)

    let inline v4 x y z w = Vector4 (x, y, z, w)
    let inline v4Dup a = v4 a a a a
    let v4One = Vector4.One
    let v4Zero = Vector4.Zero
    let v4UnitX = Vector4.UnitX
    let v4UnitY = Vector4.UnitY
    let v4UnitZ = Vector4.UnitZ
    let v4UnitW = Vector4.UnitW
    let v4Bounds (position : Vector2) (size : Vector2) = v4 position.X position.Y (position.X + size.X) (position.Y + size.Y)
    let v4BoundsOverflow (position : Vector2) (size : Vector2) (overflow : Vector2) =
        let sizeHalf = size * 0.5f
        let center = position + sizeHalf
        let sizeHalfOverflow = Vector2.Multiply (sizeHalf, overflow + Vector2.One)
        let xy = center - sizeHalfOverflow
        let x2y2 = center + sizeHalfOverflow
        v4 xy.X xy.Y x2y2.X x2y2.Y

/// The Vector4 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector4Pluggable =
    { Vector4 : Vector4 }

    static member equals left right =
        left.Vector4 = right.Vector4

    static member compare left right =
        compare (left.Vector4.X, left.Vector4.Y) (right.Vector4.X, right.Vector4.Y)

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
        member this.MapX mapper = Vector2i (mapper this.X, this.Y)
        member this.MapY mapper = Vector2i (this.X, mapper this.Y)
        member this.WithX x = Vector2i (x, this.Y)
        member this.WithY y = Vector2i (this.X, y)

    let inline v2i x y = Vector2i (x, y)
    let inline v2iDup a = v2i a a
    let v2iOne = Vector2i.One
    let v2iZero = Vector2i.Zero
    let v2iUnitX = Vector2i.UnitX
    let v2iUnitY = Vector2i.UnitY

/// The Vector2i value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector2iPluggable =
    { Vector2i : Vector2i }

    static member equals left right =
        left.Vector2i = right.Vector2i

    static member compare left right =
        compare (left.Vector2i.X, left.Vector2i.Y) (right.Vector2i.X, right.Vector2i.Y)

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
        member this.MapX mapper = Vector3i (mapper this.X, this.Y, this.Z)
        member this.MapY mapper = Vector3i (this.X, mapper this.Y, this.Z)
        member this.MapZ mapper = Vector3i (this.X, this.Y, mapper this.Z)
        member this.WithX x = Vector3i (x, this.Y, this.Z)
        member this.WithY y = Vector3i (this.X, y, this.Z)
        member this.WithZ z = Vector3i (this.X, this.Y, z)

    let inline v3i x y z = Vector3i (x, y, z)
    let inline v3iDup a = v3i a a a
    let v3iOne = Vector3i.One
    let v3iZero = Vector3i.Zero
    let v3iUnitX = Vector3i.UnitX
    let v3iUnitY = Vector3i.UnitY
    let v3iUnitZ = Vector3i.UnitZ

/// Converts Vector4 types.
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
                failconv "Invalid Vector4Converter conversion from source." (Some symbol)
        | :? Vector3i -> source
        | _ -> failconv "Invalid Vector4Converter conversion from source." None

[<AutoOpen>]
module Vector4i =

    type Vector4i with
        member this.Translate (translation : Vector2i) = Vector4i (this.X + translation.X, this.Y + translation.Y, this.Z + translation.X, this.W + translation.Y)
        member this.MapX mapper = Vector4i (mapper this.X, this.Y, this.Z, this.W)
        member this.MapY mapper = Vector4i (this.X, mapper this.Y, this.Z, this.W)
        member this.MapZ mapper = Vector4i (this.X, this.Y, mapper this.Z, this.W)
        member this.MapW mapper = Vector4i (this.X, this.Y, this.Z, mapper this.W)
        member this.WithX x = Vector4i (x, this.Y, this.Z, this.W)
        member this.WithY y = Vector4i (this.X, y, this.Z, this.W)
        member this.WithZ z = Vector4i (this.X, this.Y, z, this.W)
        member this.WithW w = Vector4i (this.X, this.Y, this.Z, w)
        member this.WithPosition position = this.Translate (position - this.Position)
        member this.WithCenter center = this.Translate (center - this.Center)
        member this.WithBottom bottom = this.Translate (bottom - this.Bottom)
        member this.WithTop top = this.Translate (top - this.Top)
        member this.WithLeft left = this.Translate (left - this.Left)
        member this.WithRight right = this.Translate (right - this.Right)

    let inline v4i x y z w = Vector4i (x, y, z, w)
    let inline v4iDup a = v4 a a a a
    let v4iOne = Vector4i.One
    let v4iZero = Vector4i.Zero
    let v4iUnitX = Vector4i.UnitX
    let v4iUnitY = Vector4i.UnitY
    let v4iUnitZ = Vector4i.UnitZ
    let v4iUnitW = Vector4i.UnitW
    let v4iBounds (position : Vector2i) (size : Vector2i) = v4i position.X position.Y (position.X + size.X) (position.Y + size.Y)
    let v4iBoundsOverflow (position : Vector2i) (size : Vector2i) (overflow : Vector2i) =
        let sizeHalf = size / 2
        let center = position + sizeHalf
        let sizeHalfOverflow = Vector2i.Multiply (sizeHalf, overflow + Vector2i.One)
        let xy = center - sizeHalfOverflow
        let x2y2 = center + sizeHalfOverflow
        v4i xy.X xy.Y x2y2.X x2y2.Y

/// The Vector4 value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] Vector4iPluggable =
    { Vector4i : Vector4i }

    static member equals left right =
        left.Vector4i = right.Vector4i

    static member compare left right =
        compare (left.Vector4i.X, left.Vector4i.Y) (right.Vector4i.X, right.Vector4i.Y)

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

/// Converts Vector4 types.
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
                failconv "Invalid Vector4Converter conversion from source." (Some symbol)
        | :? Vector4i -> source
        | _ -> failconv "Invalid Vector4Converter conversion from source." None

[<AutoOpen>]
module Matrix3 =

    type Matrix3 with

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

    let inline m3 r0 r1 r2 = Matrix3 (r0, r1, r2)
    let m3Identity = Matrix3.Identity
    let m3Zero = Matrix3.Zero

[<RequireQualifiedAccess>]
module Math =

    let mutable private Initialized = false

    /// Initializes the type converters found in NuMathModule.
    let init () =
        if not Initialized then
            assignTypeConverter<Vector2, Vector2Converter> ()
            assignTypeConverter<Vector3, Vector3Converter> ()
            assignTypeConverter<Vector4, Vector4Converter> ()
            assignTypeConverter<Vector2i, Vector2iConverter> ()
            Initialized <- true

    /// Snap an int value to an offset.
    let snap offset value =
        if offset <> 0 then
            let (div, rem) = Math.DivRem (value, offset)
            let rem = if rem < offset / 2 then 0 else offset
            div * offset + rem
        else value

    /// Snap an radian value to an offset.
    let snapR offset value =
        mul Constants.Math.RadiansToDegreesF value |>
        int |>
        snap offset |>
        single |>
        mul Constants.Math.DegreesToRadiansF

    /// Snap an single float value to an offset.
    let snapF offset (value : single) =
        single (snap offset (int value))

    /// Snap an Vector2 value to an offset.
    let snap2F offset (v2 : Vector2) =
        Vector2 (snapF offset v2.X, snapF offset v2.Y)

    /// Snap an Transform value to an offset.
    let snapTransform positionSnap rotationSnap transform =
        let transform = { transform with Position = snap2F positionSnap transform.Position }
        { transform with Rotation = snapR rotationSnap transform.Rotation }

    /// Check that a point is within the given bounds.
    let isPointInBounds (point : Vector2) (bounds : Vector4) =
        point.X >= bounds.X &&
        point.X <= bounds.Z &&
        point.Y >= bounds.Y &&
        point.Y <= bounds.W

    /// Check that a bounds is within the given bounds.
    let isBoundsInBounds (bounds : Vector4) (bounds2 : Vector4) =
        bounds.X >= bounds2.X &&
        bounds.Z <= bounds2.Z &&
        bounds.Y >= bounds2.Y &&
        bounds.W <= bounds2.W

    /// Check that a bounds is intersecting the given bounds.
    let isBoundsIntersectingBounds (bounds : Vector4) (bounds2 : Vector4) =
        bounds.X < bounds2.Z &&
        bounds.Z > bounds2.X &&
        bounds.Y < bounds2.W &&
        bounds.W > bounds2.Y

    /// Make a Vector2 center value.
    let makeCenter position size =
        v2Center position size

    /// Make a Vector4 bounds value.
    let makeBounds position size =
        v4Bounds position size

    /// Make a Vector4 bounds value, taking into consideration overflow.
    let makeBoundsOverflow position size overflow =
        v4BoundsOverflow position size overflow

    /// Get the view of the eye in absolute terms (world space).
    let getViewAbsolute (_ : Vector2) (_ : Vector2) =
        Matrix3.Identity
        
    /// Get the view of the eye in absolute terms (world space) with translation sliced on
    /// integers.
    let getViewAbsoluteI (_ : Vector2) (_ : Vector2) =
        Matrix3.Identity

    /// The relative view of the eye with original single values. Due to the problems with
    /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
    /// coordinates is very, very bad for rendering.
    let getViewRelative (eyeCenter : Vector2) (_ : Vector2) =
        let translation = eyeCenter
        Matrix3.CreateFromTranslation translation

    /// The relative view of the eye with translation sliced on integers. Good for rendering.
    let getViewRelativeI (eyeCenter : Vector2) (_ : Vector2) =
        let translation = eyeCenter
        let translationI = Vector2 (single (int translation.X), single (int translation.Y))
        Matrix3.CreateFromTranslation translationI