// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open Nu

/// Describes all the elements of a 2d transformation.
type [<Struct; StructuralEquality; NoComparison>] Transform =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Depth : single }

/// Depicts whether a view is purposed to render in relative or absolute space. For
/// example, Gui entities are rendered in absolute space since they remain still no matter
/// where the camera moves, and vice versa for non-Gui entities.
[<Syntax
    ("Absolute Relative", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type [<Struct>] ViewType =
    | Absolute
    | Relative

/// The Vector2 value that can be plugged into the scripting language.
type [<Struct; CustomEquality; CustomComparison>] Vector2Pluggable =
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
            let x = Symbol.Number (String.singleToCodeString this.Vector2.X, None)
            let y = Symbol.Number (String.singleToCodeString this.Vector2.Y, None)
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
                ([Number (String.singleToCodeString v2.X, None)
                  Number (String.singleToCodeString v2.Y, None)], None) :> obj
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
                ([Number (String.singleToCodeString v3.X, None)
                  Number (String.singleToCodeString v3.Y, None)
                  Number (String.singleToCodeString v3.Z, None)], None) :> obj
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

/// The Vector4 value that can be plugged into the scripting language.
type [<Struct; CustomEquality; CustomComparison>] Vector4Pluggable =
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
            let x = Symbol.Number (String.singleToCodeString this.Vector4.X, None)
            let y = Symbol.Number (String.singleToCodeString this.Vector4.Y, None)
            let z = Symbol.Number (String.singleToCodeString this.Vector4.Z, None)
            let w = Symbol.Number (String.singleToCodeString this.Vector4.W, None)
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
                ([Number (String.singleToCodeString v4.X, None)
                  Number (String.singleToCodeString v4.Y, None)
                  Number (String.singleToCodeString v4.Z, None)
                  Number (String.singleToCodeString v4.W, None)], None) :> obj
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

/// The Vector2i value that can be plugged into the scripting language.
type [<Struct; CustomEquality; CustomComparison>] Vector2iPluggable =
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

[<RequireQualifiedAccess>]
module Matrix3 =

    /// Gets the invertse view matrix with a terribly hacky method custom-designed to satisfy SDL2's
    /// SDL_RenderCopyEx requirement that all corrdinates be arbitrarily converted to ints.
    /// TODO: See if we can expose an SDL_RenderCopyEx from SDL2(#) that takes floats instead.
    let InvertView (m : Matrix3) =
        let mutable m = m
        m.M13 <- -m.M13
        m.M23 <- -m.M23
        m.M11 <- 1.0f / m.M11
        m.M22 <- 1.0f / m.M22
        m

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

    /// The identity transform.
    let transformIdentity =
        { Position = Vector2.Zero
          Size = Vector2.One
          Rotation = 0.0f
          Depth = 0.0f }

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

    /// Queries that a point is within the given bounds.
    let isPointInBounds (point : Vector2) (bounds : Vector4) =
        point.X >= bounds.X &&
        point.X <= bounds.Z &&
        point.Y >= bounds.Y &&
        point.Y <= bounds.W

    /// Queries that a bounds is within the given bounds.
    let isBoundsInBounds (bounds : Vector4) (bounds2 : Vector4) =
        bounds.X >= bounds2.X &&
        bounds.Z <= bounds2.Z &&
        bounds.Y >= bounds2.Y &&
        bounds.W <= bounds2.W

    /// Queries that a bounds is intersecting the given bounds.
    let isBoundsIntersectingBounds (bounds : Vector4) (bounds2 : Vector4) =
        bounds.X < bounds2.Z &&
        bounds.Z > bounds2.X &&
        bounds.Y < bounds2.W &&
        bounds.W > bounds2.Y

    /// Make a Vector4 bounds value.
    let makeBounds (position : Vector2) (size : Vector2) =
        Vector4 (position.X, position.Y, position.X + size.X, position.Y + size.Y)

    /// Make a Vector4 bounds value, taking into consideration overflow.
    let makeBoundsOverflow (position : Vector2) (size : Vector2) (overflow : Vector2) =
        let sizeHalf = size * 0.5f
        let center = position + sizeHalf
        let sizeHalfOverflow = Vector2.Multiply (sizeHalf, overflow + Vector2.One)
        let xy = center - sizeHalfOverflow
        let x2y2 = center + sizeHalfOverflow
        Vector4 (xy.X, xy.Y, x2y2.X, x2y2.Y)

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