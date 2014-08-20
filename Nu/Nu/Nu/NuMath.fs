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

    type [<StructuralEquality; NoComparison>] Transform =
        { Position : Vector2
          Depth : single
          Size : Vector2
          Rotation : single }

    type Vector2TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let v2 = obj :?> Vector2
            String.Format (culture, "{0};{1}", v2.X, v2.Y) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector2> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Vector2> then obj
            else
                let args = (obj :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector2 (argFs.[0], argFs.[1]) :> obj

    type Vector3TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let v3 = obj :?> Vector3
            String.Format (culture, "{0};{1};{2}", v3.X, v3.Y, v3.Z) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector3> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Vector3> then obj
            else
                let args = (obj :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector3 (argFs.[0], argFs.[1], argFs.[2]) :> obj

    type Vector4TypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj, _) =
            let v4 = obj :?> Vector4
            String.Format (culture, "{0};{1};{2};{3}", v4.X, v4.Y, v4.Z, v4.W) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector4> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Vector4> then obj
            else
                let args = (obj :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector4 (argFs.[0], argFs.[1], argFs.[2], argFs.[3]) :> obj

    // TODO: find a better place for this?
    type StringOptionTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, _, obj, _) =
            let optValue = obj :?> string option
            match optValue with
            | None -> "None" :> obj
            | Some value -> "Some(" + string value + ")" :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<string option> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<string option> then obj
            else
                let str = obj :?> string
                match str with
                | "None" -> None :> obj
                | _ ->
                    let innerStr = str.Substring (5, str.Length - 6)
                    let innerStr = innerStr.Trim ()
                    Some innerStr :> obj

    /// A very shitty, poorly tested Matrix3 I hacked up when I realized OpenTK didn't have one. Since I only use it
    /// for 2D view manipulation with SDL, it doesn't have any convenient rotation features. Then there's the very
    /// hacky getInverseViewMatrix function below...
    type Matrix3 =
        { M11 : single; M21 : single; M31 : single
          M12 : single; M22 : single; M32 : single
          M13 : single; M23 : single; M33 : single }

        static member col1 m = Vector3 (m.M11, m.M12, m.M13)
        static member col2 m = Vector3 (m.M21, m.M22, m.M23)
        static member col3 m = Vector3 (m.M31, m.M32, m.M33)

        static member row1 m = Vector3 (m.M11, m.M21, m.M31)
        static member row2 m = Vector3 (m.M12, m.M22, m.M32)
        static member row3 m = Vector3 (m.M13, m.M23, m.M33)

        static member getTranslation m =
            Vector2 (m.M13, m.M23)

        static member setTranslation (t : Vector2) m =
            { m with M13 = t.X; M23 = t.Y }

        static member setScale s m =
            { m with M11 = s; M22 = s }

        static member translate (t : Vector2) m =
            { m with M13 = m.M13 + t.X; M23 = m.M23 + t.Y }

        static member scale s m =
            { m with M11 = m.M11 * s; M22 = m.M22 * s }

        static member identity =
            { M11 = 1.0f; M21 = 0.0f; M31 = 0.0f
              M12 = 0.0f; M22 = 1.0f; M32 = 0.0f
              M13 = 0.0f; M23 = 0.0f; M33 = 1.0f }

        static member make
            m11 m21 m31
            m12 m22 m32
            m13 m23 m33 =
            { M11 = m11; M21 = m21; M31 = m31
              M12 = m12; M22 = m22; M32 = m32
              M13 = m13; M23 = m23; M33 = m33 }

        static member makeFromRows
            (r1 : Vector3)
            (r2 : Vector3)
            (r3 : Vector3) =
            Matrix3.make
                r1.X r1.Y r1.Z
                r2.X r2.Y r2.Z
                r3.X r3.Y r3.Z

        static member makeFromTranslation (t : Vector2) =
            { Matrix3.identity with M13 = t.X; M23 = t.Y }

        static member makeFromScale s =
            Matrix3.identity |> Matrix3.scale s

        static member getTranslationMatrix m =
            Matrix3.makeFromTranslation <| Vector2 (m.M13, m.M23)

        static member getScaleMatrix m =
            { Matrix3.identity with M11 = m.M11; M22 = m.M22; M33 = m.M33 }

        static member getTranslationAndScaleMatrix m =
            Matrix3.setTranslation <| Matrix3.getTranslation m <| Matrix3.getScaleMatrix m

        /// Gets the invertse view matrix with a terribly hacky method custom-designed to satisfy SDL2's
        /// SDL_RenderCopyEx requirement that all corrdinates be arbitrarily converted to ints.
        /// TODO: See if we can expose an SDL_RenderCopyEx from SDL2(#) that takes floats instead.
        static member getInverseViewMatrix m =
            { m with
                M13 = -m.M13; M23 = -m.M23
                M11 = 1.0f / m.M11; M22 = 1.0f / m.M22 }

        static member getInverseMatrix m =
            // borrows inversion functionality from Matrix4
            // TODO: ensure this function actually works
            let mutable n = Matrix4.Identity
            n.M11 <- m.M11; n.M21 <- m.M21; n.M41 <- m.M13
            n.M12 <- m.M12; n.M22 <- m.M22; n.M42 <- m.M23
            n.Invert ()
            { M11 = n.M11; M21 = n.M21; M31 = n.M31
              M12 = n.M21; M22 = n.M22; M32 = n.M32
              M13 = n.M41; M23 = n.M42; M33 = n.M33 }

        static member (*) (l : Matrix3, r : Matrix3) =
            let m11 = Vector3.Dot (Matrix3.row1 l, Matrix3.col1 r)
            let m21 = Vector3.Dot (Matrix3.row1 l, Matrix3.col2 r)
            let m31 = Vector3.Dot (Matrix3.row1 l, Matrix3.col3 r)
            let m12 = Vector3.Dot (Matrix3.row2 l, Matrix3.col1 r)
            let m22 = Vector3.Dot (Matrix3.row2 l, Matrix3.col2 r)
            let m32 = Vector3.Dot (Matrix3.row2 l, Matrix3.col3 r)
            let m13 = Vector3.Dot (Matrix3.row3 l, Matrix3.col1 r)
            let m23 = Vector3.Dot (Matrix3.row3 l, Matrix3.col2 r)
            let m33 = Vector3.Dot (Matrix3.row3 l, Matrix3.col3 r)
            Matrix3.make
                m11 m21 m31
                m12 m22 m32
                m13 m23 m33

        static member (*) (v : Vector2, m : Matrix3) =
            let x = v.X * m.M11 + v.Y * m.M12 + m.M13
            let y = v.X * m.M21 + v.Y * m.M22 + m.M23
            let z = v.X * m.M31 + v.Y * m.M32 + m.M33
            Vector2 (x / z, y / z)

[<RequireQualifiedAccess>]
module NuMath =

    let initTypeConverters () =
        assignTypeConverter<Vector2, Vector2TypeConverter> ()
        assignTypeConverter<Vector3, Vector3TypeConverter> ()
        assignTypeConverter<Vector4, Vector4TypeConverter> ()
        assignTypeConverter<string option, StringOptionTypeConverter> ()

    let transformIdentity =
        { Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One
          Rotation = 0.0f }

    let snap offset value =
        if offset = 0 then value
        else
            let rem = ref 0
            let div = Math.DivRem (value, offset, rem)
            let rem = if !rem < offset / 2 then 0 else offset
            div * offset + rem

    let snapR offset value =
        value |>
            mul RadiansToDegreesF |>
            int |>
            snap offset |>
            single |>
            mul DegreesToRadiansF

    let snapF offset (value : single) =
        single (snap offset <| int value)

    let snap2F offset (v2 : Vector2) =
        Vector2 (snapF offset v2.X, snapF offset v2.Y)

    let snapTransform positionSnap rotationSnap (transform : Transform) =
        let transform = { transform with Position = snap2F positionSnap transform.Position }
        { transform with Rotation = snapR rotationSnap transform.Rotation }

    let isPointInBounds (point : Vector2) (bounds : Vector4) =
        not
            (point.X > bounds.Z || point.X < bounds.X ||
             point.Y > bounds.W || point.Y < bounds.Y)

    let isPointInBounds3 (point : Vector2) (boxPos : Vector2) (boxSize : Vector2) =
        isPointInBounds point <| Vector4 (boxPos.X, boxPos.Y, boxPos.X + boxSize.X, boxPos.Y + boxSize.Y)

    let isBoundsInBounds (bounds : Vector4) (bounds2 : Vector4) =
        not
            (bounds.X > bounds2.Z || bounds.Z < bounds2.X ||
             bounds.Y > bounds2.W || bounds.W < bounds2.Y)

    let isBoundsInBounds3 (position : Vector2) (size : Vector2) bounds =
        let bounds2 = Vector4 (position.X, position.Y, position.X + size.X, position.Y + size.Y)
        isBoundsInBounds bounds2 bounds