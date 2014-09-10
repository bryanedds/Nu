// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.ComponentModel
open OpenTK
open Prime
open Nu.NuConstants

[<AutoOpen>]
module NuMathModule =

    /// Describes all the elements of a 2d transformation.
    type [<StructuralEquality; NoComparison>] Transform =
        { Position : Vector2
          Depth : single
          Size : Vector2
          Rotation : single }

    /// Converts Vector2 types.
    type Vector2TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, source, _) =
            let v2 = source :?> Vector2
            String.Format (culture, "{0};{1}", v2.X, v2.Y) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector2> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<Vector2> then source
            else
                let args = (source :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector2 (argFs.[0], argFs.[1]) :> obj

    /// Converts Vector3 types.
    type Vector3TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, source, _) =
            let v3 = source :?> Vector3
            String.Format (culture, "{0};{1};{2}", v3.X, v3.Y, v3.Z) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector3> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<Vector3> then source
            else
                let args = (source :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector3 (argFs.[0], argFs.[1], argFs.[2]) :> obj

    /// Converts Vector4 types.
    type Vector4TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, source, _) =
            let v4 = source :?> Vector4
            String.Format (culture, "{0};{1};{2};{3}", v4.X, v4.Y, v4.Z, v4.W) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector4> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<Vector4> then source
            else
                let args = (source :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector4 (argFs.[0], argFs.[1], argFs.[2], argFs.[3]) :> obj

    /// Converts string option types.
    /// TODO: find a better place for this?
    type StringOptionTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, _, source, _) =
            let optValue = source :?> string option
            match optValue with
            | Some value -> "Some(" + string value + ")" :> obj
            | None -> "" :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<string option> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<string option> then source
            else
                let valueStr = source :?> string
                match valueStr with
                | "" -> None :> obj
                | _ ->
                    let innerStr = valueStr.Substring (5, valueStr.Length - 6)
                    let innerStr = innerStr.Trim ()
                    Some innerStr :> obj

    /// Converts string list types.
    /// TODO: find a better place for this?
    type StringListTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, _, source, _) =
            let value = source :?> string list
            let valueStr = String.Join (";", Array.ofList value)
            let valueStr = "[" + valueStr + "]"
            valueStr :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<string option> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<string option> then source
            else
                let valueStr = source :?> string
                let valueStr = valueStr.Trim ()
                let valueStr = valueStr.Substring (1, valueStr.Length - 2)
                let valueStrs = valueStr.Split (';')
                let valueStrs = Array.map (fun (valueStr : string) -> valueStr.Trim ()) valueStrs
                let valueStrs = Array.filter (fun (valueStr : string) -> valueStr.Length <> 0) valueStrs
                let value = List.ofArray valueStrs
                value :> obj

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
module NuMath =

    /// Initializes the type converters found in NuMathModule.
    let initTypeConverters () =
        assignTypeConverter<Vector2, Vector2TypeConverter> ()
        assignTypeConverter<Vector3, Vector3TypeConverter> ()
        assignTypeConverter<Vector4, Vector4TypeConverter> ()
        assignTypeConverter<string option, StringOptionTypeConverter> ()
        assignTypeConverter<string list, StringListTypeConverter> ()

    /// The identity transform.
    let transformIdentity =
        { Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One
          Rotation = 0.0f }

    /// Snap an int value to an offset.
    let snap offset value =
        if offset = 0 then value
        else
            let rem = ref 0
            let div = Math.DivRem (value, offset, rem)
            let rem = if !rem < offset / 2 then 0 else offset
            div * offset + rem

    /// Snap an radian value to an offset.
    let snapR offset value =
        value |>
            mul RadiansToDegreesF |>
            int |>
            snap offset |>
            single |>
            mul DegreesToRadiansF

    /// Snap an single float value to an offset.
    let snapF offset (value : single) =
        single (snap offset <| int value)

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
            (point.X > bounds.Z || point.X < bounds.X ||
             point.Y > bounds.W || point.Y < bounds.Y)

    /// Queries that a point is within the given bounds.
    let isPointInBounds3 (point : Vector2) (boxPos : Vector2) (boxSize : Vector2) =
        isPointInBounds point <| Vector4 (boxPos.X, boxPos.Y, boxPos.X + boxSize.X, boxPos.Y + boxSize.Y)

    /// Queries that a bounds is within the given bounds.
    let isBoundsInBounds (bounds : Vector4) (bounds2 : Vector4) =
        not
            (bounds.X > bounds2.Z || bounds.Z < bounds2.X ||
             bounds.Y > bounds2.W || bounds.W < bounds2.Y)

    /// Queries that a bounds is within the given bounds.
    let isBoundsInBounds3 (position : Vector2) (size : Vector2) bounds =
        let bounds2 = Vector4 (position.X, position.Y, position.X + size.X, position.Y + size.Y)
        isBoundsInBounds bounds2 bounds