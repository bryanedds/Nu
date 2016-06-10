// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open Nu

/// Describes all the elements of a 2d transformation.
type [<StructuralEquality; NoComparison>] Transform =
    { Position : Vector2
      Size : Vector2
      Rotation : single
      Depth : single }

/// Depicts whether a view is purposed to render in relative or absolute space. For
/// example, Gui entities are rendered in absolute space since they remain still no matter
/// where the camera moves, and vice versa for non-Gui entities.
type ViewType =
    | Absolute
    | Relative

/// Converts Vector2 types.
type Vector2Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector2>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v2 = source :?> Vector2
            Symbols ([Number (string v2.X, None); Number (string v2.Y, None)], None) :> obj
        elif destType = typeof<Vector2> then source
        else failwith "Invalid Vector2Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2 (Single.Parse x, Single.Parse y) :> obj
            | _ -> failwith "Invalid Vector2Converter conversion from source."
        | :? Vector2 -> source
        | _ -> failwith "Invalid Vector2Converter conversion from source."

/// Converts Vector3 types.
type Vector3Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector3>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v3 = source :?> Vector3
            Symbols ([Number (string v3.X, None); Number (string v3.Y, None); Number (string v3.Z, None)], None) :> obj
        elif destType = typeof<Vector3> then source
        else failwith "Invalid Vector3Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector3>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _)], _) -> Vector3 (Single.Parse x, Single.Parse y, Single.Parse z) :> obj
            | _ -> failwith "Invalid Vector3Converter conversion from source."
        | :? Vector3 -> source
        | _ -> failwith "Invalid Vector3Converter conversion from source."

/// Converts Vector4 types.
type Vector4Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector4>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v4 = source :?> Vector4
            Symbols ([Number (string v4.X, None); Number (string v4.Y, None); Number (string v4.Z, None); Number (string v4.W, None)], None) :> obj
        elif destType = typeof<Vector4> then source
        else failwith "Invalid Vector4Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector4>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _); Number (z, _); Number (w, _)], None) -> Vector4 (Single.Parse x, Single.Parse y, Single.Parse z, Single.Parse w) :> obj
            | _ -> failwith "Invalid Vector4Converter conversion from source."
        | :? Vector4 -> source
        | _ -> failwith "Invalid Vector4Converter conversion from source."

/// Converts Vector2i types.
type Vector2iConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Vector2i>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            let v2i = source :?> Vector2i
            Symbols ([Number (string v2i.X, None); Number (string v2i.Y, None)], None) :> obj
        elif destType = typeof<Vector2i> then source
        else failwith "Invalid Vector2iConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Vector2i>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([Number (x, _); Number (y, _)], _) -> Vector2i (Int32.Parse x, Int32.Parse y) :> obj
            | _ -> failwith "Invalid Vector2iConverter conversion from source."
        | :? Vector2i -> source
        | _ -> failwith "Invalid Vector2iConverter conversion from source."

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
        single (snap offset ^ int value)

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