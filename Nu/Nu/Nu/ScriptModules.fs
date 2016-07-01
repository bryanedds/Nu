// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open OpenTK
open Prime
open Nu
open Nu.Scripting

[<RequireQualifiedAccess>]
module Unary =

    open Scripting

    type [<NoEquality; NoComparison>] Fns =
        { Boolean : bool -> Origin option -> Expr
          Integer : int -> Origin option -> Expr
          Integer64 : int64 -> Origin option -> Expr
          Single : single -> Origin option -> Expr
          Double : double -> Origin option -> Expr
          Vector2 : Vector2 -> Origin option -> Expr
          String : string -> Origin option -> Expr
          List : Expr list -> Origin option -> Expr }

    let Sqr =
        { Boolean = fun _ optOrigin -> Violation ("Cannot square a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (value * value, optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (value * value, optOrigin)
          Single = fun value optOrigin -> Single (value * value, optOrigin)
          Double = fun value optOrigin -> Double (value * value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (Vector2.Multiply (value, value), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot square a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot square a list.", optOrigin) }

    let Sqrt =
        { Boolean = fun _ optOrigin -> Violation ("Cannot square root a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Sqrt (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Sqrt (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Sqrt (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Sqrt value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot square root a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot square root a list.", optOrigin) }

    let Floor =
        { Boolean = fun _ optOrigin -> Violation ("Cannot floor a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Floor (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Floor (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Floor (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Floor value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot floor a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot floor a list.", optOrigin) }

    let Ceiling =
        { Boolean = fun _ optOrigin -> Violation ("Cannot ceiling a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Ceiling (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Ceiling (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Ceiling (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Ceiling value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot ceiling a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot ceiling a list.", optOrigin) }

    let Truncate =
        { Boolean = fun _ optOrigin -> Violation ("Cannot truncate a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Truncate (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Truncate (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Truncate (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Truncate value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot truncate a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot truncate a list.", optOrigin) }

    let Exp =
        { Boolean = fun _ optOrigin -> Violation ("Cannot exponentiate a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Exp (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Exp (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Exp (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Exp value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot exponentiate a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot exponentiate a list.", optOrigin) }

    let Round =
        { Boolean = fun _ optOrigin -> Violation ("Cannot round a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Round (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Round (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Round (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Round value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot round a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot round a list.", optOrigin) }

    let Log =
        { Boolean = fun _ optOrigin -> Violation ("Cannot log a boolean.", optOrigin)
          Integer = fun value optOrigin -> if value = 0 then Violation ("Cannot log a zero integer.", optOrigin) else Integer (int ^ Math.Log (double value), optOrigin)
          Integer64 = fun value optOrigin -> if value = 0L then Violation ("Cannot log a zero 64-bit integer.", optOrigin) else Integer64 (int64 ^ Math.Log (double value), optOrigin)
          Single = fun value optOrigin -> if value = 0.0f then Violation ("Cannot log a zero single.", optOrigin) else Single (single ^ Math.Log (double value), optOrigin)
          Double = fun value optOrigin -> if value = 0.0 then Violation ("Cannot log a zero double.", optOrigin) else Double (Math.Log value, optOrigin)
          Vector2 = fun value optOrigin -> if value.X = 0.0f || value.Y == 0.0f then Violation ("Cannot log a vector containing a zero member.", optOrigin) else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot log a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot log a list.", optOrigin) }

    let Sin =
        { Boolean = fun _ optOrigin -> Violation ("Cannot sin a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Sin (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Sin (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Sin (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Sin value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot sin a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot sin a list.", optOrigin) }

    let Cos =
        { Boolean = fun _ optOrigin -> Violation ("Cannot cos a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Cos (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Cos (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Cos (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Cos value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot cos a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot cos a list.", optOrigin) }

    let Tan =
        { Boolean = fun _ optOrigin -> Violation ("Cannot tan a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Tan (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Tan (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Tan (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Tan value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot tan a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot tan a list.", optOrigin) }

    let Asin =
        { Boolean = fun _ optOrigin -> Violation ("Cannot asin a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Asin (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Asin (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Asin (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Asin value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot asin a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot asin a list.", optOrigin) }

    let Acos =
        { Boolean = fun _ optOrigin -> Violation ("Cannot acos a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Acos (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Acos (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Acos (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Acos value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot acos a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot acos a list.", optOrigin) }

    let Atan =
        { Boolean = fun _ optOrigin -> Violation ("Cannot atan a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (int ^ Math.Atan (double value), optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (int64 ^ Math.Atan (double value), optOrigin)
          Single = fun value optOrigin -> Single (single ^ Math.Atan (double value), optOrigin)
          Double = fun value optOrigin -> Double (Math.Atan value, optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)), optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot atan a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot atan a list.", optOrigin) }

    let Length =
        { Boolean = fun _ optOrigin -> Violation ("Cannot get length of a boolean.", optOrigin)
          Integer = fun value optOrigin -> Integer (Math.Abs value, optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (Math.Abs value, optOrigin)
          Single = fun value optOrigin -> Single (Math.Abs value, optOrigin)
          Double = fun value optOrigin -> Double (Math.Abs value, optOrigin)
          Vector2 = fun value optOrigin -> Single (value.Length, optOrigin)
          String = fun value optOrigin -> Integer (value.Length, optOrigin)
          List = fun value optOrigin -> Integer (List.length value, optOrigin) }

    let Normal =
        { Boolean = fun _ optOrigin -> Violation ("Cannot normalize a boolean.", optOrigin)
          Integer = fun value optOrigin -> if value = 0 then Violation ("Cannot get the normal of a zero integer.", optOrigin) elif value < 0 then Integer (-1, optOrigin) else Integer (1, optOrigin)
          Integer64 = fun value optOrigin -> if value = 0L then Violation ("Cannot get the normal of a zero 64-bit integer.", optOrigin) elif value < 0L then Integer64 (-1L, optOrigin) else Integer64 (1L, optOrigin)
          Single = fun value optOrigin -> if value = 0.0f then Violation ("Cannot get the normal of a zero single.", optOrigin) elif value < 0.0f then Single (-1.0f, optOrigin) else Single (1.0f, optOrigin)
          Double = fun value optOrigin -> if value = 0.0 then Violation ("Cannot get the normal of a zero double.", optOrigin) elif value < 0.0 then Double (-1.0, optOrigin) else Double (1.0, optOrigin)
          Vector2 = fun value optOrigin -> if value = Vector2.Zero then Violation ("Cannot get the normal of a zero vector.", optOrigin) else Vector2 (Vector2.Normalize value, optOrigin)
          String = fun _ optOrigin -> Violation ("Cannot normalize a string.", optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot normalize a list.", optOrigin) }

    let ToInteger =
        { Boolean = fun value optOrigin -> Integer ((if value then 1 else 0), optOrigin)
          Integer = fun value optOrigin -> Integer (value, optOrigin)
          Integer64 = fun value optOrigin -> Integer (int value, optOrigin)
          Single = fun value optOrigin -> Integer (int value, optOrigin)
          Double = fun value optOrigin -> Integer (int value, optOrigin)
          Vector2 = fun _ optOrigin -> Violation ("Cannot convert a vector to an integer.", optOrigin)
          String = fun value optOrigin -> Integer (scvalue value, optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot convert a list to an integer.", optOrigin) }

    let ToInteger64 =
        { Boolean = fun value optOrigin -> Integer64 ((if value then 1L else 0L), optOrigin)
          Integer = fun value optOrigin -> Integer64 (int64 value, optOrigin)
          Integer64 = fun value optOrigin -> Integer64 (value, optOrigin)
          Single = fun value optOrigin -> Integer64 (int64 value, optOrigin)
          Double = fun value optOrigin -> Integer64 (int64 value, optOrigin)
          Vector2 = fun _ optOrigin -> Violation ("Cannot convert a vector to a 64-bit integer.", optOrigin)
          String = fun value optOrigin -> Integer64 (scvalue value, optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot convert a list to a 64-bit integer.", optOrigin) }

    let ToSingle =
        { Boolean = fun value optOrigin -> Single ((if value then 1.0f else 0.0f), optOrigin)
          Integer = fun value optOrigin -> Single (single value, optOrigin)
          Integer64 = fun value optOrigin -> Single (single value, optOrigin)
          Single = fun value optOrigin -> Single (value, optOrigin)
          Double = fun value optOrigin -> Single (single value, optOrigin)
          Vector2 = fun _ optOrigin -> Violation ("Cannot convert a vector to a single.", optOrigin)
          String = fun value optOrigin -> Single (scvalue value, optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot convert a list to a single.", optOrigin) }

    let ToDouble =
        { Boolean = fun value optOrigin -> Double ((if value then 1.0 else 0.0), optOrigin)
          Integer = fun value optOrigin -> Double (double value, optOrigin)
          Integer64 = fun value optOrigin -> Double (double value, optOrigin)
          Single = fun value optOrigin -> Double (double value, optOrigin)
          Double = fun value optOrigin -> Double (value, optOrigin)
          Vector2 = fun _ optOrigin -> Violation ("Cannot convert a vector to a double.", optOrigin)
          String = fun value optOrigin -> Double (scvalue value, optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot convert a list to a double.", optOrigin) }

    let ToVector2 =
        { Boolean = fun value optOrigin -> Vector2 (OpenTK.Vector2 (if value then 1.0f else 0.0f), optOrigin)
          Integer = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single value), optOrigin)
          Integer64 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single value), optOrigin)
          Single = fun value optOrigin -> Vector2 (OpenTK.Vector2 value, optOrigin)
          Double = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single value), optOrigin)
          Vector2 = fun value optOrigin -> Vector2 (value, optOrigin)
          String = fun value optOrigin -> Vector2 (OpenTK.Vector2 (scvalue<single> value), optOrigin)
          List = fun _ optOrigin -> Violation ("Cannot convert a list to a vector.", optOrigin) }

    let ToString =
        { Boolean = fun value optOrigin -> String (scstring value, optOrigin)
          Integer = fun value optOrigin -> String (scstring value, optOrigin)
          Integer64 = fun value optOrigin -> String (scstring value, optOrigin)
          Single = fun value optOrigin -> String (scstring value, optOrigin)
          Double = fun value optOrigin -> String (scstring value, optOrigin)
          Vector2 = fun value optOrigin -> String (scstring value, optOrigin)
          String = fun value optOrigin -> String (value, optOrigin)
          List = fun value optOrigin -> String (scstring value, optOrigin) }

[<RequireQualifiedAccess>]
module Binary =

    type [<NoEquality; NoComparison>] Fns =
        { Boolean : bool -> bool -> Origin option -> Expr
          Integer : int -> int -> Origin option -> Expr
          Integer64 : int64 -> int64 -> Origin option -> Expr
          Single : single -> single -> Origin option -> Expr
          Double : double -> double -> Origin option -> Expr
          Vector2 : Vector2 -> Vector2 -> Origin option -> Expr
          String : string -> string -> Origin option -> Expr
          List : Expr list -> Expr list -> Origin option -> Expr }

    let Eq =
        { Boolean = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          String = fun left right optOrigin -> Boolean ((left = right), optOrigin)
          List = fun left right optOrigin -> Boolean ((left = right), optOrigin) }

    let NotEq =
        { Boolean = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          String = fun left right optOrigin -> Boolean ((left <> right), optOrigin)
          List = fun left right optOrigin -> Boolean ((left <> right), optOrigin) }

    let Lt =
        { Boolean = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left.LengthSquared < right.LengthSquared), optOrigin)
          String = fun left right optOrigin -> Boolean ((left < right), optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot compare lists.", optOrigin) }

    let Gt =
        { Boolean = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left.LengthSquared > right.LengthSquared), optOrigin)
          String = fun left right optOrigin -> Boolean ((left > right), optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot compare lists.", optOrigin) }

    let LtEq =
        { Boolean = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left.LengthSquared <= right.LengthSquared), optOrigin)
          String = fun left right optOrigin -> Boolean ((left <= right), optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot compare lists.", optOrigin) }

    let GtEq =
        { Boolean = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          Integer = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          Integer64 = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          Single = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          Double = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          Vector2 = fun left right optOrigin -> Boolean ((left.LengthSquared >= right.LengthSquared), optOrigin)
          String = fun left right optOrigin -> Boolean ((left >= right), optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot compare lists.", optOrigin) }

    let Add =
        { Boolean = fun left right optOrigin -> Boolean ((if left && right then false elif left then true elif right then true else false), optOrigin)
          Integer = fun left right optOrigin -> Integer ((left + right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 ((left + right), optOrigin)
          Single = fun left right optOrigin -> Single ((left + right), optOrigin)
          Double = fun left right optOrigin -> Double ((left + right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 ((left + right), optOrigin)
          String = fun left right optOrigin -> String ((left + right), optOrigin)
          List = fun left right optOrigin -> List ((left @ right), optOrigin) }

    let Sub =
        { Boolean = fun left right optOrigin -> Boolean ((if left && right then false elif left then true elif right then true else false), optOrigin)
          Integer = fun left right optOrigin -> Integer ((left - right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 ((left - right), optOrigin)
          Single = fun left right optOrigin -> Single ((left - right), optOrigin)
          Double = fun left right optOrigin -> Double ((left - right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 ((left - right), optOrigin)
          String = fun left right optOrigin -> String (left.Replace (right, String.Empty), optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot subtract lists. TODO: implement this like string subtraction!", optOrigin) }

    let Mul =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot multiply booleans.", optOrigin)
          Integer = fun left right optOrigin -> Integer ((left * right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 ((left * right), optOrigin)
          Single = fun left right optOrigin -> Single ((left * right), optOrigin)
          Double = fun left right optOrigin -> Double ((left * right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 (Vector2.Multiply (left, right), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot multiply strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot multiply lists.", optOrigin) }

    let Div =
        { Boolean = fun left right optOrigin -> if right = false then Violation ("Cannot divide by a false boolean.", optOrigin) else Boolean ((if left && right then true else false), optOrigin)
          Integer = fun left right optOrigin -> if right = 0 then Violation ("Cannot divide by a zero integer.", optOrigin) else Integer ((left / right), optOrigin)
          Integer64 = fun left right optOrigin -> if right = 0L then Violation ("Cannot divide by a zero 64-bit integer.", optOrigin) else Integer64 ((left / right), optOrigin)
          Single = fun left right optOrigin -> Single ((left / right), optOrigin)
          Double = fun left right optOrigin -> Double ((left / right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 (Vector2.Divide (left, right), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot divide strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot divide lists.", optOrigin) }

    let Mod =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot modulate booleans.", optOrigin)
          Integer = fun left right optOrigin -> if right = 0 then Violation ("Cannot modulate by a zero integer.", optOrigin) else Integer ((left % right), optOrigin)
          Integer64 = fun left right optOrigin -> if right = 0L then Violation ("Cannot divide by a zero 64-bit integer.", optOrigin) else Integer64 ((left % right), optOrigin)
          Single = fun left right optOrigin -> Single ((left % right), optOrigin)
          Double = fun left right optOrigin -> Double ((left % right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot modulate strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot modulate lists.", optOrigin) }

    let Pow =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot power booleans.", optOrigin)
          Integer = fun left right optOrigin -> Integer (int ^ Math.Pow (double left, double right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 (int64 ^ Math.Pow (double left, double right), optOrigin)
          Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, double right), optOrigin)
          Double = fun left right optOrigin -> Double (Math.Pow (double left, double right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot power strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot power lists.", optOrigin) }

    let Root =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot root booleans.", optOrigin)
          Integer = fun left right optOrigin -> Integer (int ^ Math.Pow (double left, 1.0 / double right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 (int64 ^ Math.Pow (double left, 1.0 / double right), optOrigin)
          Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, 1.0 / double right), optOrigin)
          Double = fun left right optOrigin -> Double (Math.Pow (double left, 1.0 / double right), optOrigin)
          Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot root strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot root lists.", optOrigin) }

    let Cross =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot cross multiply booleans.", optOrigin)
          Integer = fun left right optOrigin -> Integer ((left * right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 ((left * right), optOrigin)
          Single = fun left right optOrigin -> Single ((left * right), optOrigin)
          Double = fun left right optOrigin -> Double ((left * right), optOrigin)
          Vector2 = fun _ _ optOrigin -> Violation ("Cannot cross multiply 2-dimensional vectors.", optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot cross multiply strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot cross multiply lists.", optOrigin) }

    let Dot =
        { Boolean = fun _ _ optOrigin -> Violation ("Cannot dot multiply booleans.", optOrigin)
          Integer = fun left right optOrigin -> Integer ((left * right), optOrigin)
          Integer64 = fun left right optOrigin -> Integer64 ((left * right), optOrigin)
          Single = fun left right optOrigin -> Single ((left * right), optOrigin)
          Double = fun left right optOrigin -> Double ((left * right), optOrigin)
          Vector2 = fun left right optOrigin -> Single (Vector2.Dot (left, right), optOrigin)
          String = fun _ _ optOrigin -> Violation ("Cannot dot multiply strings.", optOrigin)
          List = fun _ _ optOrigin -> Violation ("Cannot dot multiply lists.", optOrigin) }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Scripting =

    let combine optOriginLeft optOriginRight =
        match (optOriginLeft, optOriginRight) with
        | (Some originLeft, Some originRight) -> Some { Start = originLeft.Start; Stop = originRight.Stop }
        | (_, _) -> None

    let evalBooleanUnary fnOptOrigin fnName fn evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Boolean (bool, optOrigin) -> (Boolean (fn bool, optOrigin), env)
            | _ -> (Violation ("Cannot apply a boolean function to a non-boolean value.", Expr.getOptOrigin evaled), env)
        | _ -> (Violation ("Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

    let evalBooleanBinary fnOptOrigin fnName fn evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Boolean (boolLeft, optOriginLeft), Boolean (boolRight, optOriginRight)) -> (Boolean (fn boolLeft boolRight, combine optOriginLeft optOriginRight), env)
            | _ -> (Violation ("Cannot apply a boolean function to a non-boolean value.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
        | _ -> (Violation ("Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

    let evalUnary fnOptOrigin fnName (fns : Unary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Boolean (boolValue, optOrigin) -> ((fns.Boolean boolValue optOrigin), env)
            | Integer (intValue, optOrigin) -> ((fns.Integer intValue optOrigin), env)
            | Integer64 (int64Value, optOrigin) -> ((fns.Integer64 int64Value optOrigin), env)
            | Single (singleValue, optOrigin) -> ((fns.Single singleValue optOrigin), env)
            | Double (doubleValue, optOrigin) -> ((fns.Double doubleValue optOrigin), env)
            | Vector2 (vector2Value, optOrigin) -> ((fns.Vector2 vector2Value optOrigin), env)
            | String (stringValue, optOrigin) -> ((fns.String stringValue optOrigin), env)
            | List (listValue, optOrigin) -> ((fns.List listValue optOrigin), env)
            | _ -> (Violation ("Cannot apply an unary function on an incompatible value.", Expr.getOptOrigin evaled), env)
        | _ -> (Violation ("Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

    let evalBinary fnOptOrigin fnName (fns : Binary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Boolean (boolLeft, optOriginLeft), Boolean (boolRight, optOriginRight)) -> ((fns.Boolean boolLeft boolRight (combine optOriginLeft optOriginRight)), env)
            | (Integer (intLeft, optOriginLeft), Integer (intRight, optOriginRight)) -> ((fns.Integer intLeft intRight (combine optOriginLeft optOriginRight)), env)
            | (Integer64 (int64Left, optOriginLeft), Integer64 (int64Right, optOriginRight)) -> ((fns.Integer64 int64Left int64Right (combine optOriginLeft optOriginRight)), env)
            | (Single (singleLeft, optOriginLeft), Single (singleRight, optOriginRight)) -> ((fns.Single singleLeft singleRight (combine optOriginLeft optOriginRight)), env)
            | (Double (doubleLeft, optOriginLeft), Double (doubleRight, optOriginRight)) -> ((fns.Double doubleLeft doubleRight (combine optOriginLeft optOriginRight)), env)
            | (Vector2 (vector2Left, optOriginLeft), Vector2 (vector2Right, optOriginRight)) -> ((fns.Vector2 vector2Left vector2Right (combine optOriginLeft optOriginRight)), env)
            | (String (stringLeft, optOriginLeft), String (stringRight, optOriginRight)) -> ((fns.String stringLeft stringRight (combine optOriginLeft optOriginRight)), env)
            | (List (listLeft, optOriginLeft), List (listRight, optOriginRight)) -> ((fns.List listLeft listRight (combine optOriginLeft optOriginRight)), env)
            | _ -> (Violation ("Cannot apply a binary function on unlike or incompatible values.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
        | _ -> (Violation ("Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

    let evalFn _ _ env =
        // TODO: implement
        (Unit None, env)

    let rec eval expr (env : Env) =
        match expr with
        | Unit optOrigin
        | Boolean (_, optOrigin)
        | Integer (_, optOrigin)
        | Integer64 (_, optOrigin)
        | Single (_, optOrigin)
        | Double (_, optOrigin)
        | Vector2 (_, optOrigin)
        | String (_, optOrigin)
        | List (_, optOrigin)
        | Mapping optOrigin -> (Violation ("Cannot apply a non-function term.", optOrigin), env)
        | Violation _ -> (expr, env)
        | Reference _ -> failwithumf ()
        | Break (expr, _) -> (expr, env)
        | Call (exprs, optOrigin) ->
            let (evaleds, env) = List.foldBack (fun expr (evaleds, env) -> mapFst (fun evaled -> evaled :: evaleds) (eval expr env)) exprs ([], env)
            match evaleds with
            | [] -> (Unit optOrigin, env)
            | fn :: args ->
                match fn with
                | Binding (name, optOrigin) ->
                    match env.Bindings.TryGetValue name with
                    | (true, binding) -> evalFn binding args env
                    | (false, _) ->
                        match Name.getNameStr name with
                        | "Not" -> evalBooleanUnary optOrigin "!" not args env
                        | "And" -> evalBooleanBinary optOrigin "&" (&&) args env
                        | "Or" -> evalBooleanBinary optOrigin "|" (||) args env
                        | "Eq" -> evalBinary optOrigin "=" Binary.Eq args env
                        | "Not_Eq" -> evalBinary optOrigin "!=" Binary.NotEq args env
                        | "Lt" -> evalBinary optOrigin "<" Binary.Lt args env
                        | "Gt" -> evalBinary optOrigin ">" Binary.Gt args env
                        | "Lt_Eq" -> evalBinary optOrigin "<=" Binary.LtEq args env
                        | "Gt_Eq" -> evalBinary optOrigin ">=" Binary.GtEq args env
                        | "Add" -> evalBinary optOrigin "+" Binary.Add args env
                        | "Sub" -> evalBinary optOrigin "-" Binary.Sub args env
                        | "Mul" -> evalBinary optOrigin "*" Binary.Mul args env
                        | "Div" -> evalBinary optOrigin "/" Binary.Div args env
                        | "Mod" -> evalBinary optOrigin "%" Binary.Mod args env
                        | "Pow" -> evalBinary optOrigin "Pow" Binary.Pow args env
                        | "Root" -> evalBinary optOrigin "Root" Binary.Root args env
                        | "Sqr" -> evalUnary optOrigin "Sqr" Unary.Sqr args env
                        | "Sqrt" -> evalUnary optOrigin "Sqrt" Unary.Sqrt args env
                        | "Floor" -> evalUnary optOrigin "Floor" Unary.Floor args env
                        | "Ceiling" -> evalUnary optOrigin "Ceiling" Unary.Ceiling args env
                        | "Truncate" -> evalUnary optOrigin "Truncate" Unary.Truncate args env
                        | "Round" -> evalUnary optOrigin "Round" Unary.Round args env
                        | "Exp" -> evalUnary optOrigin "Exp" Unary.Exp args env
                        | "Log" -> evalUnary optOrigin "Log" Unary.Log args env
                        | "Sin" -> evalUnary optOrigin "Sin" Unary.Sin args env
                        | "Cos" -> evalUnary optOrigin "Cos" Unary.Cos args env
                        | "Tan" -> evalUnary optOrigin "Tan" Unary.Tan args env
                        | "Asin" -> evalUnary optOrigin "Asin" Unary.Asin args env
                        | "Acos" -> evalUnary optOrigin "Acos" Unary.Acos args env
                        | "Atan" -> evalUnary optOrigin "Atan" Unary.Atan args env
                        | "Length" -> evalUnary optOrigin "Length" Unary.Length args env
                        | "Normal" -> evalUnary optOrigin "Normal" Unary.Normal args env
                        | "Cross" -> evalBinary optOrigin "Cross" Binary.Cross args env
                        | "Dot" -> evalBinary optOrigin "Dot" Binary.Dot args env
                        | "ToInteger" -> evalUnary optOrigin "ToInteger" Unary.ToInteger args env
                        | "ToInteger64" -> evalUnary optOrigin "ToInteger64" Unary.ToInteger64 args env
                        | "ToSingle" -> evalUnary optOrigin "ToSingle" Unary.ToSingle args env
                        | "ToDouble" -> evalUnary optOrigin "ToDouble" Unary.ToDouble args env
                        | "Head"
                        | "Tail"
                        | "Empty"
                        | "Cons"
                        | "Expr.Map"
                        | "Filter"
                        | "Fold"
                        | "All"
                        | "Any"
                        | "NotAny"
                        | _ -> (Violation ("Cannot apply an non-existent binding.", Expr.getOptOrigin fn), env)
                | _ -> (Violation ("Cannot apply an non-binding.", Expr.getOptOrigin fn), env)
        | _ -> (expr, env)

[<AutoOpen>]
module ScriptSystemModule =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : Vmap<Guid, Script>
              Debugging : bool }