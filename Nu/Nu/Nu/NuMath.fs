namespace Nu
open System
open System.ComponentModel
open OpenTK
open Nu.NuCore
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
        override this.ConvertTo (_, culture, obj : obj, _) =
            let v2 = obj :?> Vector2
            String.Format (culture, "{0};{1}", v2.X, v2.Y) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector2> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
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
        override this.ConvertTo (_, culture, obj : obj, _) =
            let v3 = obj :?> Vector3
            String.Format (culture, "{0};{1};{2}", v3.X, v3.Y, v3.Z) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector3> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
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
        override this.ConvertTo (_, culture, obj : obj, _) =
            let v4 = obj :?> Vector4
            String.Format (culture, "{0};{1};{2};{3}", v4.X, v4.Y, v4.Z, v4.W) :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector4> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<Vector4> then obj
            else
                let args = (obj :?> string).Split ';'
                let argFs = Array.map (fun arg -> Single.Parse arg) args
                Vector4 (argFs.[0], argFs.[1], argFs.[2], argFs.[3]) :> obj

module NuMath =

    let initMathConverters () =
        assignTypeConverter<Vector2, Vector2TypeConverter> ()
        assignTypeConverter<Vector3, Vector3TypeConverter> ()
        assignTypeConverter<Vector4, Vector4TypeConverter> ()

    let identity =
        { Position = Vector2.Zero
          Depth = 0.0f
          Size = Vector2.One
          Rotation = 0.0f }

    let snap value offset =
        if offset = 0 then value
        else
            let rem = ref 0
            let div = Math.DivRem (value, offset, rem)
            let rem' = if !rem < offset / 2 then 0 else offset
            div * offset + rem'

    let snapR (value : single) offset =
        DegreesToRadiansF * single (snap (int <| value * RadiansToDegreesF) offset)

    let snapF (value : single) offset =
        single <| snap (int value) offset

    let snap2F (v2 : Vector2) offset =
        Vector2 (snapF v2.X offset, snapF v2.Y offset)

    let snapTransform positionSnap rotationSnap (transform : Transform) =
        let transform' = { transform with Position = snap2F transform.Position positionSnap }
        { transform' with Rotation = snapR transform'.Rotation rotationSnap }

    let isInBox3 (point : Vector2) (boxPos : Vector2) (boxSize : Vector2) =
        point.X >= boxPos.X &&
        point.X < boxPos.X + boxSize.X &&
        point.Y >= boxPos.Y &&
        point.Y < boxPos.Y + boxSize.Y

    let isInBox (point : Vector2) (box : Box2) =
        isInBox3
            point
            (Vector2 (box.Left, box.Top))
            (Vector2 (box.Right, box.Bottom))