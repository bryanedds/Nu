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

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Scripting =

    [<RequireQualifiedAccess>]
    module Unary =
    
        type [<NoEquality; NoComparison>] Fns =
            { Bool : bool -> Origin option -> Expr
              Int : int -> Origin option -> Expr
              Int64 : int64 -> Origin option -> Expr
              Single : single -> Origin option -> Expr
              Double : double -> Origin option -> Expr
              Vector2 : Vector2 -> Origin option -> Expr
              String : string -> Origin option -> Expr
              List : Expr list -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Origin option -> Expr
              Keyphrase : Map<int, Expr> -> Origin option -> Expr }
    
        let Sqr =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqr"], "Cannot square a bool.", optOrigin)
              Int = fun value optOrigin -> Int (value * value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (value * value, optOrigin)
              Single = fun value optOrigin -> Single (value * value, optOrigin)
              Double = fun value optOrigin -> Double (value * value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (Vector2.Multiply (value, value), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqr"], "Cannot square a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqr"], "Cannot square a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqr"], "Cannot square a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqr"], "Cannot square a keyphrase.", optOrigin) }
    
        let Sqrt =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqrt"], "Cannot square root a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Sqrt (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Sqrt (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Sqrt (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Sqrt value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqrt"], "Cannot square root a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqrt"], "Cannot square root a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqrt"], "Cannot square root a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sqrt"], "Cannot square root a keyphrase.", optOrigin) }
    
        let Floor =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "floor"], "Cannot floor a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Floor (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Floor (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Floor (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Floor value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "floor"], "Cannot floor a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "floor"], "Cannot floor a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "floor"], "Cannot floor a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "floor"], "Cannot floor a keyphrase.", optOrigin) }
    
        let Ceiling =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "ceiling"], "Cannot ceiling a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Ceiling (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Ceiling (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Ceiling (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Ceiling value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "ceiling"], "Cannot ceiling a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "ceiling"], "Cannot ceiling a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "ceiling"], "Cannot ceiling a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "ceiling"], "Cannot ceiling a keyphrase.", optOrigin) }
    
        let Truncate =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "truncate"], "Cannot truncate a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Truncate (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Truncate (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Truncate (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Truncate value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "truncate"], "Cannot truncate a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "truncate"], "Cannot truncate a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "truncate"], "Cannot truncate a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "truncate"], "Cannot truncate a keyphrase.", optOrigin) }
    
        let Exp =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "exp"], "Cannot exponentiate a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Exp (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Exp (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Exp (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Exp value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "exp"], "Cannot exponentiate a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "exp"], "Cannot exponentiate a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "exp"], "Cannot exponentiate a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "exp"], "Cannot exponentiate a keyphrase.", optOrigin) }
    
        let Round =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "round"], "Cannot round a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Round (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Round (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Round (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Round value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "round"], "Cannot round a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "round"], "Cannot round a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "round"], "Cannot round a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "round"], "Cannot round a keyphrase.", optOrigin) }
    
        let Log =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "log"], "Cannot log a bool.", optOrigin)
              Int = fun value optOrigin -> if value = 0 then Violation (["invalidArgumentValue"; "unary"; "log"], "Cannot log a zero int.", optOrigin) else Int (int ^ Math.Log (double value), optOrigin)
              Int64 = fun value optOrigin -> if value = 0L then Violation (["invalidArgumentValue"; "unary"; "log"], "Cannot log a zero 64-bit int.", optOrigin) else Int64 (int64 ^ Math.Log (double value), optOrigin)
              Single = fun value optOrigin -> if value = 0.0f then Violation (["invalidArgumentValue"; "unary"; "log"], "Cannot log a zero single.", optOrigin) else Single (single ^ Math.Log (double value), optOrigin)
              Double = fun value optOrigin -> if value = 0.0 then Violation (["invalidArgumentValue"; "unary"; "log"], "Cannot log a zero double.", optOrigin) else Double (Math.Log value, optOrigin)
              Vector2 = fun value optOrigin -> if value.X = 0.0f || value.Y == 0.0f then Violation (["invalidArgumentValue"; "unary"; "log"], "Cannot log a vector containing a zero member.", optOrigin) else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "log"], "Cannot log a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "log"], "Cannot log a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "log"], "Cannot log a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "log"], "Cannot log a keyphrase.", optOrigin) }
    
        let Sin =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sin"], "Cannot sin a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Sin (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Sin (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Sin (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Sin value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sin"], "Cannot sin a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sin"], "Cannot sin a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sin"], "Cannot sin a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "sin"], "Cannot sin a keyphrase.", optOrigin) }
    
        let Cos =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "cos"], "Cannot cos a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Cos (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Cos (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Cos (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Cos value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "cos"], "Cannot cos a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "cos"], "Cannot cos a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "cos"], "Cannot cos a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "cos"], "Cannot cos a keyphrase.", optOrigin) }
    
        let Tan =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "tan"], "Cannot tan a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Tan (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Tan (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Tan (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Tan value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "tan"], "Cannot tan a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "tan"], "Cannot tan a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "tan"], "Cannot tan a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "tan"], "Cannot tan a keyphrase.", optOrigin) }
    
        let Asin =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "asin"], "Cannot asin a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Asin (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Asin (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Asin (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Asin value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "asin"], "Cannot asin a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "asin"], "Cannot asin a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "asin"], "Cannot asin a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "asin"], "Cannot asin a keyphrase.", optOrigin) }
    
        let Acos =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "acos"], "Cannot acos a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Acos (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Acos (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Acos (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Acos value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "acos"], "Cannot acos a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "acos"], "Cannot acos a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "acos"], "Cannot acos a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "acos"], "Cannot acos a keyphrase.", optOrigin) }
    
        let Atan =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "atan"], "Cannot atan a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Atan (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Atan (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Atan (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Atan value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "atan"], "Cannot atan a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "atan"], "Cannot atan a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "atan"], "Cannot atan a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "atan"], "Cannot atan a keyphrase.", optOrigin) }
    
        let Length =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "length"], "Cannot get length of a bool.", optOrigin)
              Int = fun value optOrigin -> Int (Math.Abs value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (Math.Abs value, optOrigin)
              Single = fun value optOrigin -> Single (Math.Abs value, optOrigin)
              Double = fun value optOrigin -> Double (Math.Abs value, optOrigin)
              Vector2 = fun value optOrigin -> Single (value.Length, optOrigin)
              String = fun value optOrigin -> Int (value.Length, optOrigin)
              List = fun value optOrigin -> Int (List.length value, optOrigin)
              Tuple = fun value optOrigin -> Int (Array.length ^ Map.toArray value, optOrigin)
              Keyphrase = fun value optOrigin -> Int (Array.length ^ Map.toArray value, optOrigin) }
    
        let Normal =
            { Bool = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "normal"], "Cannot normalize a bool.", optOrigin)
              Int = fun value optOrigin -> if value = 0 then Violation (["invalidArgumentValue"; "unary"; "normal"], "Cannot get the normal of a zero int.", optOrigin) elif value < 0 then Int (-1, optOrigin) else Int (1, optOrigin)
              Int64 = fun value optOrigin -> if value = 0L then Violation (["invalidArgumentValue"; "unary"; "normal"], "Cannot get the normal of a zero 64-bit int.", optOrigin) elif value < 0L then Int64 (-1L, optOrigin) else Int64 (1L, optOrigin)
              Single = fun value optOrigin -> if value = 0.0f then Violation (["invalidArgumentValue"; "unary"; "normal"], "Cannot get the normal of a zero single.", optOrigin) elif value < 0.0f then Single (-1.0f, optOrigin) else Single (1.0f, optOrigin)
              Double = fun value optOrigin -> if value = 0.0 then Violation (["invalidArgumentValue"; "unary"; "normal"], "Cannot get the normal of a zero double.", optOrigin) elif value < 0.0 then Double (-1.0, optOrigin) else Double (1.0, optOrigin)
              Vector2 = fun value optOrigin -> if value = Vector2.Zero then Violation (["invalidArgumentValue"; "unary"; "normal"], "Cannot get the normal of a zero vector.", optOrigin) else Vector2 (Vector2.Normalize value, optOrigin)
              String = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "normal"], "Cannot normalize a string.", optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "normal"], "Cannot normalize a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "normal"], "Cannot normal a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "normal"], "Cannot normal a keyphrase.", optOrigin) }
    
        let Bool =
            { Bool = fun value optOrigin -> Bool (value, optOrigin)
              Int = fun value optOrigin -> Bool ((value = 0), optOrigin)
              Int64 = fun value optOrigin -> Bool ((value = 0L), optOrigin)
              Single = fun value optOrigin -> Bool ((value = 0.0f), optOrigin)
              Double = fun value optOrigin -> Bool ((value = 0.0), optOrigin)
              Vector2 = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "bool"], "Cannot convert a vector to a bool.", optOrigin)
              String = fun value optOrigin -> Bool (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "bool"], "Cannot convert a list to a bool.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "bool"], "Cannot bool a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "bool"], "Cannot bool a keyphrase.", optOrigin) }
    
        let Int =
            { Bool = fun value optOrigin -> Int ((if value then 1 else 0), optOrigin)
              Int = fun value optOrigin -> Int (value, optOrigin)
              Int64 = fun value optOrigin -> Int (int value, optOrigin)
              Single = fun value optOrigin -> Int (int value, optOrigin)
              Double = fun value optOrigin -> Int (int value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "int"], "Cannot convert a vector to an int.", optOrigin)
              String = fun value optOrigin -> Int (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "int"], "Cannot convert a list to an int.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "int"], "Cannot int a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "int"], "Cannot int a keyphrase.", optOrigin) }
    
        let Int64 =
            { Bool = fun value optOrigin -> Int64 ((if value then 1L else 0L), optOrigin)
              Int = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (value, optOrigin)
              Single = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Double = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "int64"], "Cannot convert a vector to a 64-bit int.", optOrigin)
              String = fun value optOrigin -> Int64 (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "int64"], "Cannot convert a list to a 64-bit int.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "int64"], "Cannot int64 a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "int64"], "Cannot int64 a keyphrase.", optOrigin) }
    
        let Single =
            { Bool = fun value optOrigin -> Single ((if value then 1.0f else 0.0f), optOrigin)
              Int = fun value optOrigin -> Single (single value, optOrigin)
              Int64 = fun value optOrigin -> Single (single value, optOrigin)
              Single = fun value optOrigin -> Single (value, optOrigin)
              Double = fun value optOrigin -> Single (single value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "single"], "Cannot convert a vector to a single.", optOrigin)
              String = fun value optOrigin -> Single (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "single"], "Cannot convert a list to a single.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "single"], "Cannot single a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "single"], "Cannot single a keyphrase.", optOrigin) }
    
        let Double =
            { Bool = fun value optOrigin -> Double ((if value then 1.0 else 0.0), optOrigin)
              Int = fun value optOrigin -> Double (double value, optOrigin)
              Int64 = fun value optOrigin -> Double (double value, optOrigin)
              Single = fun value optOrigin -> Double (double value, optOrigin)
              Double = fun value optOrigin -> Double (value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "double"], "Cannot convert a vector to a double.", optOrigin)
              String = fun value optOrigin -> Double (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "conversion"; "double"], "Cannot convert a list to a double.", optOrigin)
              Tuple = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "double"], "Cannot double a tuple.", optOrigin)
              Keyphrase = fun _ optOrigin -> Violation (["invalidArgumentType"; "unary"; "double"], "Cannot double a keyphrase.", optOrigin) }
    
        let String =
            { Bool = fun value optOrigin -> String (scstring value, optOrigin)
              Int = fun value optOrigin -> String (scstring value, optOrigin)
              Int64 = fun value optOrigin -> String (scstring value, optOrigin)
              Single = fun value optOrigin -> String (scstring value, optOrigin)
              Double = fun value optOrigin -> String (scstring value, optOrigin)
              Vector2 = fun value optOrigin -> String (scstring value, optOrigin)
              String = fun value optOrigin -> String (value, optOrigin)
              List = fun value optOrigin -> String (scstring value, optOrigin)
              Tuple = fun value optOrigin -> String (scstring value, optOrigin)
              Keyphrase = fun value optOrigin -> String (scstring value, optOrigin) }
    
    [<RequireQualifiedAccess>]
    module Binary =
    
        type [<NoEquality; NoComparison>] Fns =
            { Bool : bool -> bool -> Origin option -> Expr
              Int : int -> int -> Origin option -> Expr
              Int64 : int64 -> int64 -> Origin option -> Expr
              Single : single -> single -> Origin option -> Expr
              Double : double -> double -> Origin option -> Expr
              Vector2 : Vector2 -> Vector2 -> Origin option -> Expr
              String : string -> string -> Origin option -> Expr
              List : Expr list -> Expr list -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr
              Keyphrase : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr }
    
        let Eq =
            { Bool = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left = right), optOrigin)
              String = fun left right optOrigin -> Bool ((left = right), optOrigin)
              List = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Tuple = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Keyphrase = fun left right optOrigin -> Bool ((left = right), optOrigin) }
    
        let NotEq =
            { Bool = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              String = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              List = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Tuple = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Keyphrase = fun left right optOrigin -> Bool ((left <> right), optOrigin) }
    
        let Lt =
            { Bool = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared < right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left < right), optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt"], "Cannot compare tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt"], "Cannot compare keyphrases.", optOrigin) }
    
        let Gt =
            { Bool = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared > right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left > right), optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt"], "Cannot compare tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt"], "Cannot compare keyphrases.", optOrigin) }
    
        let LtEq =
            { Bool = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared <= right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt_eq"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt_eq"], "Cannot compare tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "lt_eq"], "Cannot compare keyphrases.", optOrigin) }
    
        let GtEq =
            { Bool = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared >= right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt_eq"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt_eq"], "Cannot compare tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "comparison"; "gt_eq"], "Cannot compare keyphrases.", optOrigin) }
    
        let Add =
            { Bool = fun left right optOrigin -> Bool ((if left && right then false elif left then true elif right then true else false), optOrigin)
              Int = fun left right optOrigin -> Int ((left + right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left + right), optOrigin)
              Single = fun left right optOrigin -> Single ((left + right), optOrigin)
              Double = fun left right optOrigin -> Double ((left + right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 ((left + right), optOrigin)
              String = fun left right optOrigin -> String ((left + right), optOrigin)
              List = fun left right optOrigin -> List ((left @ right), optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "add"], "Cannot add tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "add"], "Cannot add keyphrases.", optOrigin) }
    
        let Sub =
            { Bool = fun left right optOrigin -> Bool ((if left && right then false elif left then true elif right then true else false), optOrigin)
              Int = fun left right optOrigin -> Int ((left - right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left - right), optOrigin)
              Single = fun left right optOrigin -> Single ((left - right), optOrigin)
              Double = fun left right optOrigin -> Double ((left - right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 ((left - right), optOrigin)
              String = fun left right optOrigin -> String (left.Replace (right, String.Empty), optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "sub"], "Cannot subtract lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "sub"], "Cannot subtract tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "sub"], "Cannot subtract keyphrases.", optOrigin) }
    
        let Mul =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mul"], "Cannot multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (Vector2.Multiply (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mul"], "Cannot multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mul"], "Cannot multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mul"], "Cannot multiply tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mul"], "Cannot multiply keyphrases.", optOrigin) }
    
        let Div =
            { Bool = fun left right optOrigin -> if right = false then Violation (["invalidArgumentValue"; "binary"; "div"], "Cannot divide by a false bool.", optOrigin) else Bool ((if left && right then true else false), optOrigin)
              Int = fun left right optOrigin -> if right = 0 then Violation (["invalidArgumentValue"; "binary"; "div"], "Cannot divide by a zero int.", optOrigin) else Int ((left / right), optOrigin)
              Int64 = fun left right optOrigin -> if right = 0L then Violation (["invalidArgumentValue"; "binary"; "div"], "Cannot divide by a zero 64-bit int.", optOrigin) else Int64 ((left / right), optOrigin)
              Single = fun left right optOrigin -> Single ((left / right), optOrigin)
              Double = fun left right optOrigin -> Double ((left / right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (Vector2.Divide (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "div"], "Cannot divide strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "div"], "Cannot divide lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "div"], "Cannot divide tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "div"], "Cannot divide keyphrases.", optOrigin) }
    
        let Mod =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mod"], "Cannot modulate bools.", optOrigin)
              Int = fun left right optOrigin -> if right = 0 then Violation (["invalidArgumentValue"; "binary"; "mod"], "Cannot modulate by a zero int.", optOrigin) else Int ((left % right), optOrigin)
              Int64 = fun left right optOrigin -> if right = 0L then Violation (["invalidArgumentValue"; "binary"; "mod"], "Cannot divide by a zero 64-bit int.", optOrigin) else Int64 ((left % right), optOrigin)
              Single = fun left right optOrigin -> Single ((left % right), optOrigin)
              Double = fun left right optOrigin -> Double ((left % right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mod"], "Cannot modulate strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mod"], "Cannot modulate lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mod"], "Cannot modulate tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "mod"], "Cannot modulate keyphrases.", optOrigin) }
    
        let Pow =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "pow"], "Cannot power bools.", optOrigin)
              Int = fun left right optOrigin -> Int (int ^ Math.Pow (double left, double right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 (int64 ^ Math.Pow (double left, double right), optOrigin)
              Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, double right), optOrigin)
              Double = fun left right optOrigin -> Double (Math.Pow (double left, double right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "pow"], "Cannot power strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "pow"], "Cannot power lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "pow"], "Cannot power tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "pow"], "Cannot power keyphrases.", optOrigin) }
    
        let Root =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "root"], "Cannot root bools.", optOrigin)
              Int = fun left right optOrigin -> Int (int ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Double = fun left right optOrigin -> Double (Math.Pow (double left, 1.0 / double right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "root"], "Cannot root strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "root"], "Cannot root lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "root"], "Cannot root tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "root"], "Cannot root keyphrases.", optOrigin) }
    
        let Cross =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiply 2-dimensional vectors.", optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiply tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "cross"], "Cannot cross multiple keyphrases.", optOrigin) }
    
        let Dot =
            { Bool = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "dot"], "Cannot dot multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun left right optOrigin -> Single (Vector2.Dot (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "dot"], "Cannot dot multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "dot"], "Cannot dot multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "dot"], "Cannot dot multiply tuples.", optOrigin)
              Keyphrase = fun _ _ optOrigin -> Violation (["invalidArgumentType"; "binary"; "dot"], "Cannot dot multiply keyphrases.", optOrigin) }

    let InputDictionary =
        [(typeof<bool>, fun (value : obj) (optOrigin : Origin option) -> Bool (value :?> bool, optOrigin))
         (typeof<int>, fun (value : obj) (optOrigin : Origin option) -> Int (value :?> int, optOrigin))
         (typeof<int64>, fun (value : obj) (optOrigin : Origin option) -> Int64 (value :?> int64, optOrigin))
         (typeof<single>, fun (value : obj) (optOrigin : Origin option) -> Single (value :?> single, optOrigin))
         (typeof<double>, fun (value : obj) (optOrigin : Origin option) -> Double (value :?> double, optOrigin))
         (typeof<Vector2>, fun (value : obj) (optOrigin : Origin option) -> Vector2 (value :?> Vector2, optOrigin))
         (typeof<string>, fun (value : obj) (optOrigin : Origin option) -> String (value :?> string, optOrigin))] |>
        Seq.map KeyValuePair |>
        dictC

    let combine optOriginLeft optOriginRight =
        match (optOriginLeft, optOriginRight) with
        | (Some originLeft, Some originRight) -> Some { Start = originLeft.Start; Stop = originRight.Stop }
        | (_, _) -> None

    let evalBoolUnary fnOptOrigin fnName fn evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Bool (bool, optOrigin) -> Result (Bool (fn bool, optOrigin), env)
            | _ -> Result (Violation (["invalidArgumentType"; "unary"; fnName], "Cannot apply a bool function to a non-bool value.", Expr.getOptOrigin evaled), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "unary"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

    let evalBoolBinary fnOptOrigin fnName fn evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Bool (boolLeft, optOriginLeft), Bool (boolRight, optOriginRight)) -> Result (Bool (fn boolLeft boolRight, combine optOriginLeft optOriginRight), env)
            | _ -> Result (Violation (["invalidArgumentType"; "binary"; fnName], "Cannot apply a bool function to a non-bool value.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "binary"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

    let evalUnary fnOptOrigin fnName (fns : Unary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaled] ->
            match evaled with
            | Bool (boolValue, optOrigin) -> Result ((fns.Bool boolValue optOrigin), env)
            | Int (intValue, optOrigin) -> Result ((fns.Int intValue optOrigin), env)
            | Int64 (int64Value, optOrigin) -> Result ((fns.Int64 int64Value optOrigin), env)
            | Single (singleValue, optOrigin) -> Result ((fns.Single singleValue optOrigin), env)
            | Double (doubleValue, optOrigin) -> Result ((fns.Double doubleValue optOrigin), env)
            | Vector2 (vector2Value, optOrigin) -> Result ((fns.Vector2 vector2Value optOrigin), env)
            | String (stringValue, optOrigin) -> Result ((fns.String stringValue optOrigin), env)
            | List (listValue, optOrigin) -> Result ((fns.List listValue optOrigin), env)
            | Tuple (tupleValue, optOrigin) -> Result ((fns.Tuple tupleValue optOrigin), env)
            | Keyphrase (keyphraseValue, optOrigin) -> Result ((fns.Keyphrase keyphraseValue optOrigin), env)
            | _ -> Result (Violation (["invalidArgumentType"; "unary"; fnName], "Cannot apply an unary function on an incompatible value.", Expr.getOptOrigin evaled), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "unary"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

    let evalBinary fnOptOrigin fnName (fns : Binary.Fns) evaledArgs env =
        match evaledArgs with
        | [evaledLeft; evaledRight] ->
            match (evaledLeft, evaledRight) with
            | (Bool (boolLeft, optOriginLeft), Bool (boolRight, optOriginRight)) -> Result ((fns.Bool boolLeft boolRight (combine optOriginLeft optOriginRight)), env)
            | (Int (intLeft, optOriginLeft), Int (intRight, optOriginRight)) -> Result ((fns.Int intLeft intRight (combine optOriginLeft optOriginRight)), env)
            | (Int64 (int64Left, optOriginLeft), Int64 (int64Right, optOriginRight)) -> Result ((fns.Int64 int64Left int64Right (combine optOriginLeft optOriginRight)), env)
            | (Single (singleLeft, optOriginLeft), Single (singleRight, optOriginRight)) -> Result ((fns.Single singleLeft singleRight (combine optOriginLeft optOriginRight)), env)
            | (Double (doubleLeft, optOriginLeft), Double (doubleRight, optOriginRight)) -> Result ((fns.Double doubleLeft doubleRight (combine optOriginLeft optOriginRight)), env)
            | (Vector2 (vector2Left, optOriginLeft), Vector2 (vector2Right, optOriginRight)) -> Result ((fns.Vector2 vector2Left vector2Right (combine optOriginLeft optOriginRight)), env)
            | (String (stringLeft, optOriginLeft), String (stringRight, optOriginRight)) -> Result ((fns.String stringLeft stringRight (combine optOriginLeft optOriginRight)), env)
            | (List (listLeft, optOriginLeft), List (listRight, optOriginRight)) -> Result ((fns.List listLeft listRight (combine optOriginLeft optOriginRight)), env)
            | (Tuple (tupleLeft, optOriginLeft), Tuple (tupleRight, optOriginRight)) -> Result ((fns.Tuple tupleLeft tupleRight (combine optOriginLeft optOriginRight)), env)
            | (Keyphrase (keyphraseLeft, optOriginLeft), Keyphrase (keyphraseRight, optOriginRight)) -> Result ((fns.Keyphrase keyphraseLeft keyphraseRight (combine optOriginLeft optOriginRight)), env)
            | _ -> Result (Violation (["invalidArgumentType"; "binary"; fnName], "Cannot apply a binary function on unlike or incompatible values.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "binary"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)
        
    let evalSome fnOptOrigin fnName args env =
        match args with
        | [evaled] -> Result (Option (Some evaled, fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "option"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalIsSome fnOptOrigin fnName args env =
        match args with
        | [Option (evaled, optOrigin)] -> Result (Bool (Option.isSome evaled, optOrigin), env)
        | [_] -> Result (Violation (["invalidArgumentType"; "list"; fnName], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "list"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalHead fnOptOrigin fnName args env =
        match args with
        | [List (evaleds, _)] ->
            match evaleds with
            | evaledHead :: _ -> Result (evaledHead, env)
            | _ -> Result (Violation (["invalidArgumentValue"; "list"; fnName], "Cannot apply " + fnName + " to a list with no members.", fnOptOrigin), env)
        | [_] -> Result (Violation (["invalidArgumentType"; "list"; fnName], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "list"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalTail fnOptOrigin fnName args env =
        match args with
        | [List (evaleds, optOrigin)] ->
            match evaleds with
            | _ :: evaledTail -> Result (List (evaledTail, optOrigin), env)
            | _ -> Result (Violation (["invalidArgumentValue"; "list"; fnName], "Cannot apply " + fnName + " to a list with no members.", fnOptOrigin), env)
        | [_] -> Result (Violation (["invalidArgumentType"; "list"; fnName], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "list"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalCons fnOptOrigin fnName args env =
        match args with
        | [evaled; List (evaleds, optOrigin)] -> Result (List (evaled :: evaleds, optOrigin), env)
        | [_; _] -> Result (Violation (["invalidArgumentType"; "list"; fnName], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "list"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalIsEmpty fnOptOrigin fnName args env =
        match args with
        | [List (evaleds, optOrigin)] -> Result (Bool (List.isEmpty evaleds, optOrigin), env)
        | [_] -> Result (Violation (["invalidArgumentType"; "list"; fnName], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "list"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
    let evalNth5 fnOptOrigin fnName index args env =
        match args with
        | [List (evaleds, optOrigin)] ->
            match List.tryFindAt index evaleds with
            | Some _ as optEvaled -> Result (Option (optEvaled, optOrigin), env)
            | None -> Result (Option (None, optOrigin), env)
        | [Tuple (evaleds, optOrigin)] | [Keyphrase (evaleds, optOrigin)] ->
            match Map.tryFind index evaleds with
            | Some _ as optEvaled -> Result (Option (optEvaled, optOrigin), env)
            | None -> Result (Option (None, optOrigin), env)
        | [_] -> Result (Violation (["invalidArgumentType"; "sequence"; fnName], "Cannot apply " + fnName + " to a non-sequence.", fnOptOrigin), env)
        | _ -> Result (Violation (["invalidArgumentCount"; "sequence"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
        
    let evalNth fnOptOrigin fnName args env =
        match args with
        | [head; foot] ->
            match head with
            | Int (int, _) -> evalNth5 fnOptOrigin fnName int [foot] env
            | _ -> Result (Violation (["invalidNthArgumentType"; "sequence"; fnName], "Application of " + fnName + " requires an int for the first argument.", fnOptOrigin), env)
        | _ ->  Result (Violation (["invalidArgumentCount"; "sequence"; fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

    let evalIntrinsic optOrigin name args env =
        let nameStr = Name.getNameStr name
        match nameStr with
        | "!"  | "not" -> evalBoolUnary optOrigin nameStr not args env
        | "&"  | "and" -> evalBoolBinary optOrigin nameStr (&&) args env
        | "|"  | "or" -> evalBoolBinary optOrigin nameStr (||) args env
        | "="  | "eq" -> evalBinary optOrigin nameStr Binary.Eq args env
        | "<>" | "not_eq" -> evalBinary optOrigin nameStr Binary.NotEq args env
        | "<"  | "lt" -> evalBinary optOrigin nameStr Binary.Lt args env
        | ">"  | "gt" -> evalBinary optOrigin nameStr Binary.Gt args env
        | "<=" | "lt_eq" -> evalBinary optOrigin nameStr Binary.LtEq args env
        | ">=" | "gt_eq" -> evalBinary optOrigin nameStr Binary.GtEq args env
        | "+"  | "add" -> evalBinary optOrigin nameStr Binary.Add args env
        | "-"  | "sub" -> evalBinary optOrigin nameStr Binary.Sub args env
        | "*"  | "mul" -> evalBinary optOrigin nameStr Binary.Mul args env
        | "/"  | "div" -> evalBinary optOrigin nameStr Binary.Div args env
        | "%"  | "mod" -> evalBinary optOrigin nameStr Binary.Mod args env
        | "pow" -> evalBinary optOrigin nameStr Binary.Pow args env
        | "root" -> evalBinary optOrigin nameStr Binary.Root args env
        | "sqr" -> evalUnary optOrigin nameStr Unary.Sqr args env
        | "sqrt" -> evalUnary optOrigin nameStr Unary.Sqrt args env
        | "floor" -> evalUnary optOrigin nameStr Unary.Floor args env
        | "ceiling" -> evalUnary optOrigin nameStr Unary.Ceiling args env
        | "truncate" -> evalUnary optOrigin nameStr Unary.Truncate args env
        | "round" -> evalUnary optOrigin nameStr Unary.Round args env
        | "exp" -> evalUnary optOrigin nameStr Unary.Exp args env
        | "log" -> evalUnary optOrigin nameStr Unary.Log args env
        | "sin" -> evalUnary optOrigin nameStr Unary.Sin args env
        | "cos" -> evalUnary optOrigin nameStr Unary.Cos args env
        | "tan" -> evalUnary optOrigin nameStr Unary.Tan args env
        | "asin" -> evalUnary optOrigin nameStr Unary.Asin args env
        | "acos" -> evalUnary optOrigin nameStr Unary.Acos args env
        | "atan" -> evalUnary optOrigin nameStr Unary.Atan args env
        | "length" -> evalUnary optOrigin nameStr Unary.Length args env
        | "normal" -> evalUnary optOrigin nameStr Unary.Normal args env
        | "cross" -> evalBinary optOrigin nameStr Binary.Cross args env
        | "dot" -> evalBinary optOrigin nameStr Binary.Dot args env
        | "bool" -> evalUnary optOrigin nameStr Unary.Bool args env
        | "int" -> evalUnary optOrigin nameStr Unary.Int args env
        | "int64" -> evalUnary optOrigin nameStr Unary.Int64 args env
        | "single" -> evalUnary optOrigin nameStr Unary.Single args env
        | "double" -> evalUnary optOrigin nameStr Unary.Double args env
        | "string" -> evalUnary optOrigin nameStr Unary.String args env
        | "some" -> evalSome optOrigin nameStr args env
        | "isSome" -> evalIsSome optOrigin nameStr args env
        | "head" -> evalHead optOrigin nameStr args env
        | "tail" -> evalTail optOrigin nameStr args env
        | "cons" -> evalCons optOrigin nameStr args env
        | "isEmpty" -> evalIsEmpty optOrigin nameStr args env
        | "first" -> evalNth5 optOrigin nameStr 0 args env
        | "second" -> evalNth5 optOrigin nameStr 1 args env
        | "third" -> evalNth5 optOrigin nameStr 2 args env
        | "fourth" -> evalNth5 optOrigin nameStr 3 args env
        | "fifth" -> evalNth5 optOrigin nameStr 4 args env
        | "nth" -> evalNth optOrigin nameStr args env
        | _ -> Result (Violation (["invalidFunctionTargetBinding"], "Cannot apply an non-existent binding.", optOrigin), env)

    let rec evalExprs exprs env =
        List.foldBack
            (fun expr (violations, evaleds, env) ->
                let result = eval expr env
                match result.Evaled with
                | Violation _ as violation -> (violation :: violations, evaleds, env)
                | evaled -> (violations, evaled :: evaleds, env))
            exprs
            ([], [], env)

    and evalApply exprs optOrigin env =
        let oldEnv = env
        let (violations, evaleds, env) = evalExprs exprs env
        match violations with
        | Violation _ as violation :: _ -> Result (violation, oldEnv)
        | _ ->
            match evaleds with
            | fn :: args ->
                match fn with
                | Get (name, optOrigin) ->
                    match args with
                    | [Entity (entity, optOrigin)] ->
                        let world = Env.getWorld env
                        if World.containsEntity entity world then
                            let property = entity.GetProperty name world
                            match InputDictionary.TryGetValue property.PropertyType with
                            | (true, fn) -> Result (fn property.PropertyValue optOrigin, env)
                            | (false, _) -> Result (Violation (["unsupportedPropertyType"], "TODO: proper error msg.", optOrigin), env)
                        else Result (Violation (["invalidEntity"], "Entity '" + scstring entity + "' does not exist.", optOrigin), env)
                    | [_] -> Result (Violation (["invalidGetTargetType"], "TODO: proper error msg.", optOrigin), env)
                    | _ -> Result (Violation (["invalidGetForm"], "TODO: proper error msg.", optOrigin), env)
                // TODO: | Set (name, optOrigin) ->
                | Keyword (name, optOrigin) ->
                    let map = String (name, optOrigin) :: args |> List.indexed |> Map.ofList
                    Result (Keyphrase (map, optOrigin), env)
                | Binding (name, optOrigin) ->
                    match Env.tryGetBinding name env with
                    | Some binding -> evalFn binding args env
                    | None -> evalIntrinsic optOrigin name args env
                | _ -> Result (Violation (["TODO: proper violation category."], "Cannot apply a non-binding.", optOrigin), env)
            | [] -> Result (Unit optOrigin, env)

    and evalIf condition consequent alternative optOrigin env =
        let oldEnv = env
        let conditionEvaled = eval condition env
        match conditionEvaled.Evaled with
        | Violation _ -> Result (conditionEvaled.Evaled, oldEnv)
        | Bool (bool, _) -> if bool then eval consequent env else eval alternative env
        | _ -> Result (Violation (["invalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", optOrigin), env)

    and evalCase exprPairs optOrigin env =
        List.foldUntil
            (fun (result : Result) (condition, consequent) ->
                let conditionEvaled = eval condition env
                match conditionEvaled.Evaled with
                | Violation _ -> Some (Result (conditionEvaled.Evaled, result.Env))
                | Bool (bool, _) -> if bool then Some (eval consequent env) else None
                | _ -> Some (Result (Violation (["invalidCaseCondition"], "Must provide an expression that evaluates to a bool in a case condition.", optOrigin), env)))
            (Result (Unit optOrigin, env))
            exprPairs

    and evalTry body handlers _ env =
        let oldEnv = env
        let result = eval body env
        match result.Evaled with
        | Violation (categories, _, optOrigin) ->
            List.foldUntil (fun result (handlerCategories, handlerBody) ->
                let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                if categoriesTrunc = handlerCategories then Some (eval handlerBody result.Env) else None)
                (Result (Unit optOrigin, oldEnv))
                handlers
        | _ -> result

    and evalFn _ _ env =
        // TODO: implement
        Result (Unit None, env)

    and eval expr env : Result =
        match expr with
        | Violation _ -> Result (expr, env)
        | Unit _ -> Result (expr, env)
        | Bool _ -> Result (expr, env)
        | Int _ -> Result (expr, env)
        | Int64 _ -> Result (expr, env)
        | Single _ -> Result (expr, env)
        | Double _ -> Result (expr, env)
        | Vector2 _ -> Result (expr, env)
        | String _ -> Result (expr, env)
        | Option _ -> Result (expr, env)
        | List _ -> Result (expr, env)
        | Tuple _ -> Result (expr, env)
        | Keyphrase _ -> Result (expr, env)
        | Apply (exprs, optOrigin) -> evalApply exprs optOrigin env
        | Get _ -> Result (expr, env)
        | Set _ -> Result (expr, env)
        | Entity _ -> Result (expr, env)
        | Camera _ -> Result (expr, env)
        | Do _ -> Result (expr, env)
        | DoMany _ -> Result (expr, env)
        | Let _ -> Result (expr, env)
        | Fun _ -> Result (expr, env)
        | LetMany _ -> Result (expr, env)
        | If (condition, consequent, alternative, optOrigin) -> evalIf condition consequent alternative optOrigin env
        | Case (exprPairs, optOrigin) -> evalCase exprPairs optOrigin env
        | Try (body, handlers, optOrigin) -> evalTry body handlers optOrigin env
        | Keyword _ -> Result (expr, env)
        | Binding _ -> Result (expr, env)
        | Quote _  -> Result (expr, env)
        | Break (expr, _) -> Result (expr, env)
        | Define (_, _, optOrigin) -> Result (Unit optOrigin, env)
        | Handle (_, optOrigin) -> Result (Unit optOrigin, env)
        | Equate (_, _, optOrigin) -> Result (Unit optOrigin, env)

[<AutoOpen>]
module ScriptSystemModule =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : Vmap<Guid, Script>
              Debugging : bool }