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

    type [<NoEquality; NoComparison>] Fns =
        { Boolean : bool -> Value
          Integer : int -> Value
          Integer64 : int64 -> Value
          Single : single -> Value
          Double : double -> Value
          Vector2 : Vector2 -> Value
          String : string -> Value }

    let Sqr =
        { Boolean = fun _ -> Violation "Cannot square a boolean."
          Integer = fun value -> Integer (value * value)
          Integer64 = fun value -> Integer64 (value * value)
          Single = fun value -> Single (value * value)
          Double = fun value -> Double (value * value)
          Vector2 = fun value -> Vector2 (Vector2.Multiply (value, value))
          String = fun _ -> Violation "Cannot square a string." }

    let Sqrt =
        { Boolean = fun _ -> Violation "Cannot square root a boolean."
          Integer = fun value -> Integer (int ^ Math.Sqrt (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Sqrt (double value))
          Single = fun value -> Single (single ^ Math.Sqrt (double value))
          Double = fun value -> Double (Math.Sqrt value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)))
          String = fun _ -> Violation "Cannot square root a string." }

    let Floor =
        { Boolean = fun _ -> Violation "Cannot floor a boolean."
          Integer = fun value -> Integer (int ^ Math.Floor (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Floor (double value))
          Single = fun value -> Single (single ^ Math.Floor (double value))
          Double = fun value -> Double (Math.Floor value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)))
          String = fun _ -> Violation "Cannot floor a string." }

    let Ceiling =
        { Boolean = fun _ -> Violation "Cannot ceiling a boolean."
          Integer = fun value -> Integer (int ^ Math.Ceiling (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Ceiling (double value))
          Single = fun value -> Single (single ^ Math.Ceiling (double value))
          Double = fun value -> Double (Math.Ceiling value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)))
          String = fun _ -> Violation "Cannot ceiling a string." }

    let Truncate =
        { Boolean = fun _ -> Violation "Cannot truncate a boolean."
          Integer = fun value -> Integer (int ^ Math.Truncate (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Truncate (double value))
          Single = fun value -> Single (single ^ Math.Truncate (double value))
          Double = fun value -> Double (Math.Truncate value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)))
          String = fun _ -> Violation "Cannot truncate a string." }

    let Exp =
        { Boolean = fun _ -> Violation "Cannot exponentiate a boolean."
          Integer = fun value -> Integer (int ^ Math.Exp (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Exp (double value))
          Single = fun value -> Single (single ^ Math.Exp (double value))
          Double = fun value -> Double (Math.Exp value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)))
          String = fun _ -> Violation "Cannot exponentiate a string." }

    let Round =
        { Boolean = fun _ -> Violation "Cannot round a boolean."
          Integer = fun value -> Integer (int ^ Math.Round (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Round (double value))
          Single = fun value -> Single (single ^ Math.Round (double value))
          Double = fun value -> Double (Math.Round value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)))
          String = fun _ -> Violation "Cannot round a string." }

    let Log =
        { Boolean = fun _ -> Violation "Cannot log a boolean."
          Integer = fun value -> if value = 0 then Violation "Cannot log a zero integer." else Integer (int ^ Math.Log (double value))
          Integer64 = fun value -> if value = 0L then Violation "Cannot log a zero 64-bit integer." else Integer64 (int64 ^ Math.Log (double value))
          Single = fun value -> if value = 0.0f then Violation "Cannot log a zero single." else Single (single ^ Math.Log (double value))
          Double = fun value -> if value = 0.0 then Violation "Cannot log a zero double." else Double (Math.Log value)
          Vector2 = fun value -> if value.X = 0.0f || value.Y == 0.0f then Violation "Cannot log a vector containing a zero member." else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)))
          String = fun _ -> Violation "Cannot log a string." }

    let Sin =
        { Boolean = fun _ -> Violation "Cannot sin a boolean."
          Integer = fun value -> Integer (int ^ Math.Sin (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Sin (double value))
          Single = fun value -> Single (single ^ Math.Sin (double value))
          Double = fun value -> Double (Math.Sin value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)))
          String = fun _ -> Violation "Cannot sin a string." }

    let Cos =
        { Boolean = fun _ -> Violation "Cannot cos a boolean."
          Integer = fun value -> Integer (int ^ Math.Cos (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Cos (double value))
          Single = fun value -> Single (single ^ Math.Cos (double value))
          Double = fun value -> Double (Math.Cos value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)))
          String = fun _ -> Violation "Cannot cos a string." }

    let Tan =
        { Boolean = fun _ -> Violation "Cannot tan a boolean."
          Integer = fun value -> Integer (int ^ Math.Tan (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Tan (double value))
          Single = fun value -> Single (single ^ Math.Tan (double value))
          Double = fun value -> Double (Math.Tan value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)))
          String = fun _ -> Violation "Cannot tan a string." }

    let Asin =
        { Boolean = fun _ -> Violation "Cannot asin a boolean."
          Integer = fun value -> Integer (int ^ Math.Asin (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Asin (double value))
          Single = fun value -> Single (single ^ Math.Asin (double value))
          Double = fun value -> Double (Math.Asin value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)))
          String = fun _ -> Violation "Cannot asin a string." }

    let Acos =
        { Boolean = fun _ -> Violation "Cannot acos a boolean."
          Integer = fun value -> Integer (int ^ Math.Acos (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Acos (double value))
          Single = fun value -> Single (single ^ Math.Acos (double value))
          Double = fun value -> Double (Math.Acos value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)))
          String = fun _ -> Violation "Cannot acos a string." }

    let Atan =
        { Boolean = fun _ -> Violation "Cannot atan a boolean."
          Integer = fun value -> Integer (int ^ Math.Atan (double value))
          Integer64 = fun value -> Integer64 (int64 ^ Math.Atan (double value))
          Single = fun value -> Single (single ^ Math.Atan (double value))
          Double = fun value -> Double (Math.Atan value)
          Vector2 = fun value -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)))
          String = fun _ -> Violation "Cannot atan a string." }

    let Length =
        { Boolean = fun value -> Integer (if value then 1 else 0)
          Integer = fun value -> Integer (Math.Abs value)
          Integer64 = fun value -> Integer64 (Math.Abs value)
          Single = fun value -> Single (Math.Abs value)
          Double = fun value -> Double (Math.Abs value)
          Vector2 = fun value -> Single value.Length
          String = fun value -> Integer value.Length }

    let Cross =
        { Boolean = fun _ -> Violation "Cannot cross multiply a boolean."
          Integer = fun value -> Integer (Math.Abs value)
          Integer64 = fun value -> Integer64 (Math.Abs value)
          Single = fun value -> Single (Math.Abs value)
          Double = fun value -> Double (Math.Abs value)
          Vector2 = fun value -> Single value.Length
          String = fun _ -> Violation "Cannot cross multiply a string." }

    let Dot =
        { Boolean = fun _ -> Violation "Cannot dot multiply a boolean."
          Integer = fun value -> Integer (Math.Abs value)
          Integer64 = fun value -> Integer64 (Math.Abs value)
          Single = fun value -> Single (Math.Abs value)
          Double = fun value -> Double (Math.Abs value)
          Vector2 = fun value -> Single value.Length
          String = fun _ -> Violation "Cannot dot multiply a boolean." }

    let Normal =
        { Boolean = fun _ -> Violation "Cannot normalize a boolean."
          Integer = fun value -> if value = 0 then Violation "Cannot get the normal of a zero integer." elif value < 0 then Integer -1 else Integer 1
          Integer64 = fun value -> if value = 0L then Violation "Cannot get the normal of a zero 64-bit integer." elif value < 0L then Integer64 -1L else Integer64 1L
          Single = fun value -> if value = 0.0f then Violation "Cannot get the normal of a zero single." elif value < 0.0f then Single -1.0f else Single 1.0f
          Double = fun value -> if value = 0.0 then Violation "Cannot get the normal of a zero double." elif value < 0.0 then Double -1.0 else Double 1.0
          Vector2 = fun value -> if value = Vector2.Zero then Violation "Cannot get the normal of a zero vector." else Vector2 ^ Vector2.Normalize value
          String = fun _ -> Violation "Cannot normalize a string." }

    let ToInteger =
        { Boolean = fun value -> Integer (if value then 1 else 0)
          Integer = fun value -> Integer value
          Integer64 = fun value -> Integer (int value)
          Single = fun value -> Integer (int value)
          Double = fun value -> Integer (int value)
          Vector2 = fun _ -> Violation "Cannot convert a vector to an integer."
          String = fun value -> match Int32.TryParse value with (true, integer) -> Integer integer | (false, _) -> Violation ^ "Could not parse string '" + value + "' to integer." }

    let ToInteger64 =
        { Boolean = fun value -> Integer64 (if value then 1L else 0L)
          Integer = fun value -> Integer64 (int64 value)
          Integer64 = fun value -> Integer64 value
          Single = fun value -> Integer64 (int64 value)
          Double = fun value -> Integer64 (int64 value)
          Vector2 = fun _ -> Violation "Cannot convert a vector to a 64-bit integer."
          String = fun value -> match Int64.TryParse value with (true, integer64) -> Integer64 integer64 | (false, _) -> Violation ^ "Could not parse string '" + value + "' to 64-bit integer." }

    let ToSingle =
        { Boolean = fun value -> Single (if value then 1.0f else 0.0f)
          Integer = fun value -> Single (single value)
          Integer64 = fun value -> Single (single value)
          Single = fun value -> Single value
          Double = fun value -> Single (single value)
          Vector2 = fun _ -> Violation "Cannot convert a vector to a single."
          String = fun value -> match Single.TryParse value with (true, single) -> Single single | (false, _) -> Violation ^ "Could not parse string '" + value + "' to single." }

    let ToDouble =
        { Boolean = fun value -> Double (if value then 1.0 else 0.0)
          Integer = fun value -> Double (double value)
          Integer64 = fun value -> Double (double value)
          Single = fun value -> Double (double value)
          Double = fun value -> Double value
          Vector2 = fun _ -> Violation "Cannot convert a vector to a double."
          String = fun value -> match Double.TryParse value with (true, double) -> Double double | (false, _) -> Violation ^ "Could not parse string '" + value + "' to double." }

[<RequireQualifiedAccess>]
module Binary =

    type [<NoEquality; NoComparison>] Fns =
        { Boolean : bool -> bool -> Value
          Integer : int -> int -> Value
          Integer64 : int64 -> int64 -> Value
          Single : single -> single -> Value
          Double : double -> double -> Value
          Vector2 : Vector2 -> Vector2 -> Value
          String : string -> string -> Value }

    let Eq =
        { Boolean = fun left right -> Boolean (left = right)
          Integer = fun left right -> Boolean (left = right)
          Integer64 = fun left right -> Boolean (left = right)
          Single = fun left right -> Boolean (left = right)
          Double = fun left right -> Boolean (left = right)
          Vector2 = fun left right -> Boolean (left = right)
          String = fun left right -> Boolean (left = right) }

    let NotEq =
        { Boolean = fun left right -> Boolean (left <> right)
          Integer = fun left right -> Boolean (left <> right)
          Integer64 = fun left right -> Boolean (left <> right)
          Single = fun left right -> Boolean (left <> right)
          Double = fun left right -> Boolean (left <> right)
          Vector2 = fun left right -> Boolean (left <> right)
          String = fun left right -> Boolean (left <> right) }

    let Lt =
        { Boolean = fun left right -> Boolean (left < right)
          Integer = fun left right -> Boolean (left < right)
          Integer64 = fun left right -> Boolean (left < right)
          Single = fun left right -> Boolean (left < right)
          Double = fun left right -> Boolean (left < right)
          Vector2 = fun left right -> Boolean (left.LengthSquared < right.LengthSquared)
          String = fun left right -> Boolean (left < right) }

    let Gt =
        { Boolean = fun left right -> Boolean (left > right)
          Integer = fun left right -> Boolean (left > right)
          Integer64 = fun left right -> Boolean (left > right)
          Single = fun left right -> Boolean (left > right)
          Double = fun left right -> Boolean (left > right)
          Vector2 = fun left right -> Boolean (left.LengthSquared > right.LengthSquared)
          String = fun left right -> Boolean (left > right) }

    let LtEq =
        { Boolean = fun left right -> Boolean (left <= right)
          Integer = fun left right -> Boolean (left <= right)
          Integer64 = fun left right -> Boolean (left <= right)
          Single = fun left right -> Boolean (left <= right)
          Double = fun left right -> Boolean (left <= right)
          Vector2 = fun left right -> Boolean (left.LengthSquared <= right.LengthSquared)
          String = fun left right -> Boolean (left <= right) }

    let GtEq =
        { Boolean = fun left right -> Boolean (left >= right)
          Integer = fun left right -> Boolean (left >= right)
          Integer64 = fun left right -> Boolean (left >= right)
          Single = fun left right -> Boolean (left >= right)
          Double = fun left right -> Boolean (left >= right)
          Vector2 = fun left right -> Boolean (left.LengthSquared >= right.LengthSquared)
          String = fun left right -> Boolean (left >= right) }

    let Add =
        { Boolean = fun left right -> Boolean (if left && right then false elif left then true elif right then true else false)
          Integer = fun left right -> Integer (left + right)
          Integer64 = fun left right -> Integer64 (left + right)
          Single = fun left right -> Single (left + right)
          Double = fun left right -> Double (left + right)
          Vector2 = fun left right -> Vector2 (left + right)
          String = fun left right -> String (left + right) }

    let Sub =
        { Boolean = fun left right -> Boolean (if left && right then false elif left then true elif right then true else false)
          Integer = fun left right -> Integer (left - right)
          Integer64 = fun left right -> Integer64 (left - right)
          Single = fun left right -> Single (left - right)
          Double = fun left right -> Double (left - right)
          Vector2 = fun left right -> Vector2 (left - right)
          String = fun left right -> String (left.Replace (right, String.Empty)) }

    let Mul =
        { Boolean = fun left right -> Boolean (if left && right then true else false)
          Integer = fun left right -> Integer (left * right)
          Integer64 = fun left right -> Integer64 (left * right)
          Single = fun left right -> Single (left * right)
          Double = fun left right -> Double (left * right)
          Vector2 = fun left right -> Vector2 (Vector2.Multiply (left, right))
          String = fun _ _ -> Violation "Cannot multiply strings." }

    let Div =
        { Boolean = fun left right -> if right = false then Violation "Cannot divide by a false boolean." else Boolean (if left && right then true else false)
          Integer = fun left right -> if right = 0 then Violation "Cannot divide by a zero integer." else Integer (left / right)
          Integer64 = fun left right -> if right = 0L then Violation "Cannot divide by a zero 64-bit integer." else Integer64 (left / right)
          Single = fun left right -> Single (left / right)
          Double = fun left right -> Double (left / right)
          Vector2 = fun left right -> Vector2 (Vector2.Divide (left, right))
          String = fun _ _ -> Violation "Cannot divide strings." }

    let Mod =
        { Boolean = fun _ _ -> Violation "Cannot modulate booleans."
          Integer = fun left right -> if right = 0 then Violation "Cannot modulate by a zero integer." else Integer (left % right)
          Integer64 = fun left right -> if right = 0L then Violation "Cannot divide by a zero 64-bit integer." else Integer64 (left % right)
          Single = fun left right -> Single (left % right)
          Double = fun left right -> Double (left % right)
          Vector2 = fun left right -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y))
          String = fun _ _ -> Violation "Cannot modulate strings." }

    let Pow =
        { Boolean = fun _ _ -> Violation "Cannot power booleans."
          Integer = fun left right -> Integer (int ^ Math.Pow (double left, double right))
          Integer64 = fun left right -> Integer64 (int64 ^ Math.Pow (double left, double right))
          Single = fun left right -> Single (single ^ Math.Pow (double left, double right))
          Double = fun left right -> Double (Math.Pow (double left, double right))
          Vector2 = fun left right -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)))
          String = fun _ _ -> Violation "Cannot power strings." }

    let Root =
        { Boolean = fun _ _ -> Violation "Cannot root booleans."
          Integer = fun left right -> Integer (int ^ Math.Pow (double left, 1.0 / double right))
          Integer64 = fun left right -> Integer64 (int64 ^ Math.Pow (double left, 1.0 / double right))
          Single = fun left right -> Single (single ^ Math.Pow (double left, 1.0 / double right))
          Double = fun left right -> Double (Math.Pow (double left, 1.0 / double right))
          Vector2 = fun left right -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)))
          String = fun _ _ -> Violation "Cannot root strings." }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Scripting =

    module Metadata =

        let empty =
            { Documentation = String.Empty
              BreakpointEnabled = false
              BreakpointCondition = None }

    let evalBooleanUnary fnName fn evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Value (value, _) ->
                match value with
                | Boolean bool -> (Value (Boolean ^ fn bool, Metadata.empty), env)
                | _ -> (Value (Violation "Cannot apply a boolean function to a non-boolean value.", Metadata.empty), env)
            | Exprs _ -> failwithumf ()
        | _ -> (Value (Violation ^ "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", Metadata.empty), env)

    let evalBooleanBinary fnName fn evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Value (valueLeft, _), (Value (valueRight, _))) ->
                match (valueLeft, valueRight) with
                | (Boolean boolLeft, Boolean boolRight) -> (Value (Boolean ^ fn boolLeft boolRight, Metadata.empty), env)
                | _ -> (Value (Violation "Cannot apply a boolean function to a non-boolean value.", Metadata.empty), env)
            | (_, _) -> failwithumf ()
        | _ -> (Value (Violation ^ "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", Metadata.empty), env)

    let evalUnary fnName (fns : Unary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Value (value, _) ->
                match value with
                | (Boolean boolValue) -> (Value (fns.Boolean boolValue, Metadata.empty), env)
                | (Integer intValue) -> (Value (fns.Integer intValue, Metadata.empty), env)
                | (Integer64 int64Value) -> (Value (fns.Integer64 int64Value, Metadata.empty), env)
                | (Single singleValue) -> (Value (fns.Single singleValue, Metadata.empty), env)
                | (Double doubleValue) -> (Value (fns.Double doubleValue, Metadata.empty), env)
                | (Vector2 vector2Value) -> (Value (fns.Vector2 vector2Value, Metadata.empty), env)
                | (String stringValue) -> (Value (fns.String stringValue, Metadata.empty), env)
                | _ -> (Value (Violation "Cannot apply an unary function on an incompatible value.", Metadata.empty), env)
            | _ -> failwithumf ()
        | _ -> (Value (Violation ^ "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", Metadata.empty), env)

    let evalBinary fnName (fns : Binary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Value (valueLeft, _), (Value (valueRight, _))) ->
                match (valueLeft, valueRight) with
                | (Boolean boolLeft, Boolean boolRight) -> (Value (fns.Boolean boolLeft boolRight, Metadata.empty), env)
                | (Integer intLeft, Integer intRight) -> (Value (fns.Integer intLeft intRight, Metadata.empty), env)
                | (Integer64 int64Left, Integer64 int64Right) -> (Value (fns.Integer64 int64Left int64Right, Metadata.empty), env)
                | (Single singleLeft, Single singleRight) -> (Value (fns.Single singleLeft singleRight, Metadata.empty), env)
                | (Double doubleLeft, Double doubleRight) -> (Value (fns.Double doubleLeft doubleRight, Metadata.empty), env)
                | (Vector2 vector2Left, Vector2 vector2Right) -> (Value (fns.Vector2 vector2Left vector2Right, Metadata.empty), env)
                | (String stringLeft, String stringRight) -> (Value (fns.String stringLeft stringRight, Metadata.empty), env)
                | _ -> (Value (Violation "Cannot apply a binary function on unlike or incompatible values.", Metadata.empty), env)
            | (_, _) -> failwithumf ()
        | _ -> (Value (Violation ^ "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", Metadata.empty), env)

    let rec eval expr env =
        match expr with
        | Value _ as value -> (value, env)
        | Exprs (exprs, _, _) ->
            let evaleds = List.foldBack (fun expr (evaleds, env) -> mapFst (fun evaled -> evaled :: evaleds) (eval env expr)) exprs ([], env)
            match evaleds with
            | ([], env) -> (Value (Unit, Metadata.empty), env)
            | (fn :: args, env) ->
                match fn with
                | Value (value, _) ->
                    match value with
                    | Unit
                    | Boolean _
                    | Integer _
                    | Integer64 _
                    | Single _
                    | Double _
                    | Vector2 _
                    | String _ -> (Value (Violation "Cannot apply a literal value.", Metadata.empty), env)
                    | Violation _ -> (fn, env)
                    | Reference _ -> failwithumf ()
                    | Lambda lambda ->
                        match lambda with
                        | Not -> evalBooleanUnary "!" not args env
                        | And -> evalBooleanBinary "&" (&&) args env
                        | Or -> evalBooleanBinary "|" (||) args env
                        | Eq -> evalBinary "=" Binary.Eq args env
                        | Not_Eq -> evalBinary "!=" Binary.NotEq args env
                        | Lt -> evalBinary "<" Binary.Lt args env
                        | Gt -> evalBinary ">" Binary.Gt args env
                        | Lt_Eq -> evalBinary "<=" Binary.LtEq args env
                        | Gt_Eq -> evalBinary ">=" Binary.GtEq args env
                        | Add -> evalBinary "+" Binary.Add args env
                        | Sub -> evalBinary "-" Binary.Sub args env
                        | Mul -> evalBinary "*" Binary.Mul args env
                        | Div -> evalBinary "/" Binary.Div args env
                        | Mod -> evalBinary "%" Binary.Mod args env
                        | Pow -> evalBinary "Pow" Binary.Pow args env
                        | Root -> evalBinary "Root" Binary.Root args env
                        | Sqr -> evalUnary "Sqr" Unary.Sqr args env
                        | Sqrt -> evalUnary "Sqrt" Unary.Sqrt args env
                        | Floor -> evalUnary "Floor" Unary.Floor args env
                        | Ceiling -> evalUnary "Ceiling" Unary.Ceiling args env
                        | Truncate -> evalUnary "Truncate" Unary.Truncate args env
                        | Round -> evalUnary "Round" Unary.Round args env
                        | Exp -> evalUnary "Exp" Unary.Exp args env
                        | Log -> evalUnary "Log" Unary.Log args env
                        | Sin -> evalUnary "Sin" Unary.Sin args env
                        | Cos -> evalUnary "Cos" Unary.Cos args env
                        | Tan -> evalUnary "Tan" Unary.Tan args env
                        | Asin -> evalUnary "Asin" Unary.Asin args env
                        | Acos -> evalUnary "Acos" Unary.Acos args env
                        | Atan -> evalUnary "Atan" Unary.Atan args env
                        | Length -> evalUnary "Length" Unary.Length args env
                        | Cross -> evalUnary "Cross" Unary.Cross args env
                        | Dot -> evalUnary "Dot" Unary.Dot args env
                        | Normal -> evalUnary "Normal" Unary.Normal args env
                        | ToInteger -> evalUnary "ToInteger" Unary.ToInteger args env
                        | ToInteger64 -> evalUnary "ToInteger64" Unary.ToInteger64 args env
                        | ToSingle -> evalUnary "ToSingle" Unary.ToSingle args env
                        | ToDouble -> evalUnary "ToDouble" Unary.ToDouble args env
                        | _ -> (Value (Unit, Metadata.empty), env)
                | Exprs _ -> failwithumf ()

[<AutoOpen>]
module ScriptEnvModule =

    /// An environemt in which scripting declarations can be found.
    /// Internally a Dictionary for fast look-ups, but without add functions for immutability. Append, however, stays
    /// functional without greater big-O compexity!
    type [<NoEquality; NoComparison>] ScriptEnv =
        private
            ScriptEnv of Dictionary<string, Declaration>
            
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module ScriptEnv =

        let tryFind key (ScriptEnv env) =
            Dictionary.tryFind key env

        let make declarations =
            ScriptEnv ^ dictC declarations

        let append (ScriptEnv env) (ScriptEnv env2) =
            make (Seq.append env env2)

[<AutoOpen>]
module ScriptSystemModule =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { ScriptEnvs : Vmap<obj Address, ScriptEnv>
              Scripts : Vmap<Guid, Script>
              Debugging : bool }