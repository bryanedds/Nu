// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open Nu

/// Describes all the elements of a 2d transformation.
type [<StructuralEquality; NoComparison>] Transform =
    { Position : Vector2
      Depth : single
      Size : Vector2
      Rotation : single }

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
        destType = typeof<string> ||
        destType = typeof<Vector2>

    override this.ConvertTo (_, culture, source, destType) =
        if destType = typeof<string> then
            let v2 = source :?> Vector2
            String.Format (culture, "[{0} {1}]", v2.X, v2.Y) :> obj
        elif destType = typeof<Vector2> then source
        else failwith "Invalid Vector2Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<AlgebraicSource> ||
        sourceType = typeof<Vector2>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? AlgebraicSource as algebraic ->
            match algebraic.AlgebraicValue :?> obj list |> List.map (string >> Single.Parse) with
            | [x; y] -> Vector2 (x, y) :> obj
            | _ -> failwith "Invalid Vector2Converter conversion from source."
        | :? Vector2 -> source
        | _ -> failwith "Invalid Vector2Converter conversion from source."

/// Converts Vector3 types.
type Vector3Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Vector3>

    override this.ConvertTo (_, culture, source, destType) =
        if destType = typeof<string> then
            let v3 = source :?> Vector3
            String.Format (culture, "[{0} {1} {2}]", v3.X, v3.Y, v3.Z) :> obj
        elif destType = typeof<Vector3> then source
        else failwith "Invalid Vector3Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Vector3>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? AlgebraicSource as algebraic ->
            match algebraic.AlgebraicValue :?> obj list |> List.map (string >> Single.Parse) with
            | [x; y; z] -> Vector3 (x, y, z) :> obj
            | _ -> failwith "Invalid Vector3Converter conversion from source."
        | :? Vector3 -> source
        | _ -> failwith "Invalid Vector3Converter conversion from source."

/// Converts Vector4 types.
type Vector4Converter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Vector4>

    override this.ConvertTo (_, culture, source, destType) =
        if destType = typeof<string> then
            let v4 = source :?> Vector4
            String.Format (culture, "[{0} {1} {2} {3}]", v4.X, v4.Y, v4.Z, v4.W) :> obj
        elif destType = typeof<Vector4> then source
        else failwith "Invalid Vector4Converter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<AlgebraicSource> ||
        sourceType = typeof<Vector4>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? AlgebraicSource as algebraic ->
            match algebraic.AlgebraicValue :?> obj list |> List.map (string >> Single.Parse) with
            | [x; y; z; w] -> Vector4 (x, y, z, w) :> obj
            | _ -> failwith "Invalid Vector4Converter conversion from source."
        | :? Vector4 -> source
        | _ -> failwith "Invalid Vector4Converter conversion from source."

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

    /// Initializes the type converters found in NuMathModule.
    let initTypeConverters () =
        assignTypeConverter<Vector2, Vector2Converter> ()
        assignTypeConverter<Vector3, Vector3Converter> ()
        assignTypeConverter<Vector4, Vector4Converter> ()

    /// The identity transform.
    let transformIdentity =
        { Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One
          Rotation = 0.0f }

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
        not
            (point.X < bounds.X ||
             point.X > bounds.Z ||
             point.Y < bounds.Y ||
             point.Y > bounds.W)

    /// Queries that a point is within the given bounds.
    let isPointInBounds3 (point : Vector2) (boxPos : Vector2) (boxSize : Vector2) =
        isPointInBounds point ^ Vector4 (boxPos.X, boxPos.Y, boxPos.X + boxSize.X, boxPos.Y + boxSize.Y)

    /// Queries that a bounds is within the given bounds.
    let isBoundsInBounds (bounds : Vector4) (bounds2 : Vector4) =
        not
            (bounds.X > bounds2.Z || bounds.Z < bounds2.X ||
             bounds.Y > bounds2.W || bounds.W < bounds2.Y)

    /// Queries that a bounds is within the given bounds.
    let isBoundsInBounds3 (position : Vector2) (size : Vector2) bounds =
        let bounds2 = Vector4 (position.X, position.Y, position.X + size.X, position.Y + size.Y)
        isBoundsInBounds bounds2 bounds