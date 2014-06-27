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

    /// A very shitty, poorly tested Matrix3 I hacked up when I realized OpenTK didn't have one. Since I only use it
    /// for 2D view manipulation with SDL, it doesn't have any convenient rotation features. Then there's the very
    /// hacky getInverseViewMatrix function below...
    type Matrix3 =
        { M00 : single; M10 : single; M20 : single
          M01 : single; M11 : single; M21 : single
          M02 : single; M12 : single; M22 : single }

        static member col0 m = Vector3 (m.M00, m.M01, m.M02)
        static member col1 m = Vector3 (m.M10, m.M11, m.M12)
        static member col2 m = Vector3 (m.M20, m.M21, m.M22)

        static member row0 m = Vector3 (m.M00, m.M10, m.M20)
        static member row1 m = Vector3 (m.M01, m.M11, m.M21)
        static member row2 m = Vector3 (m.M02, m.M12, m.M22)

        static member getTranslation m =
            Vector2 (m.M02, m.M12)

        static member setTranslation (t : Vector2) m =
            { m with M02 = t.X; M12 = t.Y }

        static member setScale s m =
            { m with M00 = s; M11 = s }

        static member translate (t : Vector2) m =
            { m with M02 = m.M02 + t.X; M12 = m.M12 + t.Y }

        static member scale s m =
            { m with M00 = m.M00 * s; M11 = m.M11 * s }

        static member identity =
            { M00 = 1.0f; M10 = 0.0f; M20 = 0.0f
              M01 = 0.0f; M11 = 1.0f; M21 = 0.0f
              M02 = 0.0f; M12 = 0.0f; M22 = 1.0f }

        static member make
            m00 m10 m20
            m01 m11 m21
            m02 m12 m22 =
            { M00 = m00; M10 = m10; M20 = m20
              M01 = m01; M11 = m11; M21 = m21
              M02 = m02; M12 = m12; M22 = m22 }

        static member makeFromRows
            (r0 : Vector3)
            (r1 : Vector3)
            (r2 : Vector3) =
            Matrix3.make
                r0.X r0.Y r0.Z
                r1.X r1.Y r1.Z
                r2.X r2.Y r2.Z

        static member makeFromTranslation (t : Vector2) =
            { Matrix3.identity with M02 = t.X; M12 = t.Y }

        static member makeFromScale s =
            Matrix3.identity |> Matrix3.scale s

        static member getTranslationMatrix m =
            Matrix3.makeFromTranslation <| Vector2 (m.M02, m.M12)

        static member getScaleMatrix m =
            { Matrix3.identity with M00 = m.M00; M11 = m.M11; M22 = m.M22 }

        static member getTranslationAndScaleMatrix m =
            Matrix3.setTranslation <| Matrix3.getTranslation m <| Matrix3.getScaleMatrix m

        /// Gets the invertse view matrix with a terribly hacky method custom-designed to satisfy SDL2's
        /// SDL_RenderCopyEx requirement that all corrdinates be arbitrarily converted to ints.
        /// TODO: See if we can expose an SDL_RenderCopyEx from SDL2(#) that takes floats instead.
        static member getInverseViewMatrix m =
            { m with
                M02 = -m.M02; M12 = -m.M12
                M00 = 1.0f / m.M00; M11 = 1.0f / m.M11 }

        static member getInverseMatrix m =
            // borrows inversion functionality from Matrix4
            // TODO: ensure this function actually works
            let mutable n = Matrix4.Identity
            n.M11 <- m.M00; n.M21 <- m.M10; n.M41 <- m.M02
            n.M12 <- m.M01; n.M22 <- m.M11; n.M42 <- m.M12
            n.Invert ()
            { M00 = n.M11; M10 = n.M21; M20 = n.M31
              M01 = n.M21; M11 = n.M22; M21 = n.M32
              M02 = n.M41; M12 = n.M42; M22 = n.M33 }

        static member (*) (l : Matrix3, r : Matrix3) =
            let m00 = Vector3.Dot (Matrix3.row0 l, Matrix3.col0 r)
            let m10 = Vector3.Dot (Matrix3.row0 l, Matrix3.col1 r)
            let m20 = Vector3.Dot (Matrix3.row0 l, Matrix3.col2 r)
            let m01 = Vector3.Dot (Matrix3.row1 l, Matrix3.col0 r)
            let m11 = Vector3.Dot (Matrix3.row1 l, Matrix3.col1 r)
            let m21 = Vector3.Dot (Matrix3.row1 l, Matrix3.col2 r)
            let m02 = Vector3.Dot (Matrix3.row2 l, Matrix3.col0 r)
            let m12 = Vector3.Dot (Matrix3.row2 l, Matrix3.col1 r)
            let m22 = Vector3.Dot (Matrix3.row2 l, Matrix3.col2 r)
            Matrix3.make
                m00 m10 m20
                m01 m11 m21
                m02 m12 m22

        static member (*) (v : Vector2, m : Matrix3) =
            let x = v.X * m.M00 + v.Y * m.M01 + m.M02
            let y = v.X * m.M10 + v.Y * m.M11 + m.M12
            let z = v.X * m.M20 + v.Y * m.M21 + m.M22
            Vector2 (x / z, y / z)

[<RequireQualifiedAccess>]
module NuMath =

    let initTypeConverters () =
        assignTypeConverter<Vector2, Vector2TypeConverter> ()
        assignTypeConverter<Vector3, Vector3TypeConverter> ()
        assignTypeConverter<Vector4, Vector4TypeConverter> ()

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

    let snapF offset value =
        single <| snap offset -<| int (value : single)

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