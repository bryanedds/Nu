// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Scripting

[<AutoOpen>]
module WorldScriptingUnary =

    module Scripting =

        type [<NoEquality; NoComparison>] UnaryFns =
            { Bool : bool -> SymbolOrigin option -> Expr
              Int : int -> SymbolOrigin option -> Expr
              Int64 : int64 -> SymbolOrigin option -> Expr
              Single : single -> SymbolOrigin option -> Expr
              Double : double -> SymbolOrigin option -> Expr
              Vector2 : Vector2 -> SymbolOrigin option -> Expr
              String : string -> SymbolOrigin option -> Expr
              Keyword : string -> SymbolOrigin option -> Expr
              Tuple : Expr array -> SymbolOrigin option -> Expr
              Keyphrase : string -> Expr array -> SymbolOrigin option -> Expr
              Codata : Codata -> SymbolOrigin option -> Expr
              List : Expr list -> SymbolOrigin option -> Expr
              Ring : Expr Set -> SymbolOrigin option -> Expr
              Table : Map<Expr, Expr> -> SymbolOrigin option -> Expr }

        let ToEmptyFns =
            { Bool = fun _ _ -> Bool false
              Int = fun _ _ -> Int 0
              Int64 = fun _ _ -> Int64 0L
              Single = fun _ _ -> Single 0.0f
              Double = fun _ _ -> Double 0.0
              Vector2 = fun _ _ -> Vector2 OpenTK.Vector2.Zero
              String = fun _ _ -> String String.Empty
              Keyword = fun _ _ -> Keyword String.Empty
              Tuple = fun _ _ -> Tuple Array.empty
              Keyphrase = fun _ _ _ -> Keyphrase (String.Empty, Array.empty)
              Codata = fun _ _ -> Codata Empty
              List = fun _ _ -> List []
              Ring = fun _ _ -> Ring Set.empty
              Table = fun _ _ -> Table Map.empty }

        let ToIdentityFns =
            { Bool = fun _ _ -> Bool true
              Int = fun _ _ -> Int 1
              Int64 = fun _ _ -> Int64 1L
              Single = fun _ _ -> Single 1.0f
              Double = fun _ _ -> Double 1.0
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 1.0f)
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a string to an identity representation.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a keyword to an identity representation.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a tuple to an identity representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a keyphrase to an identity representation.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a codata to an identity representation.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a list to an identity representation.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a ring to an identity representation.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToIdentity"], "Cannot convert a table to an identity representation.", originOpt) }

        let ToMinFns =
            { Bool = fun _ _ -> Bool false
              Int = fun _ _ -> Int Int32.MinValue
              Int64 = fun _ _ -> Int64 Int64.MinValue
              Single = fun _ _ -> Single Single.MinValue
              Double = fun _ _ -> Double Double.MinValue
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 Single.MinValue)
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a string to a minimum representation.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a keyword to a minimum representation.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a tuple to a minimum representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a keyphrase to a minimum representation.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert codata to a minimum representation.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a list to a minimum representation.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a ring to a minimum representation.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMin"], "Cannot convert a table to a minimum representation.", originOpt) }

        let ToMaxFns =
            { Bool = fun _ _ -> Bool true
              Int = fun _ _ -> Int Int32.MaxValue
              Int64 = fun _ _ -> Int64 Int64.MaxValue
              Single = fun _ _ -> Single Single.MaxValue
              Double = fun _ _ -> Double Double.MaxValue
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 Single.MaxValue)
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a string to a maximum representation.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a keyword to a maximum representation.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a tuple to a maximum representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a keyphrase to a maximum representation.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert codata to a maximum representation.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a list to a maximum representation.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a ring to a maximum representation.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "ToMax"], "Cannot convert a table to a maximum representation.", originOpt) }

        let IncFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (inc value)
              Int64 = fun value _ -> Int64 (inc value)
              Single = fun value _ -> Single (inc value)
              Double = fun value _ -> Double (inc value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (inc value.X, inc value.Y))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Inc"], "Cannot increment a table.", originOpt) }

        let DecFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (dec value)
              Int64 = fun value _ -> Int64 (dec value)
              Single = fun value _ -> Single (dec value)
              Double = fun value _ -> Double (dec value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (dec value.X, dec value.Y))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Dec"], "Cannot decrement a table.", originOpt) }

        let NegateFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (0 - value)
              Int64 = fun value _ -> Int64 (0L - value)
              Single = fun value _ -> Single (0.0f - value)
              Double = fun value _ -> Double (0.0 - value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2.Zero - value)
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Negate"], "Cannot negate a table.", originOpt) }

        let HashFns =
            { Bool = fun value _ -> Int (hash value)
              Int = fun value _ -> Int (hash value)
              Int64 = fun value _ -> Int (hash value)
              Single = fun value _ -> Int (hash value)
              Double = fun value _ -> Int (hash value)
              Vector2 = fun value _ -> Int (hash value)
              String = fun value _ -> Int (hash value)
              Keyword = fun value _ -> Int (hash value)
              Tuple = fun value _ -> Int (hash value)
              Keyphrase = fun word phrase _ -> Int (hash (word, phrase))
              Codata = fun value _ -> Int (hash value)
              List = fun value _ -> Int (hash value)
              Ring = fun value _ -> Int (hash value)
              Table = fun value _ -> Int (hash value) }

        let SqrFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a bool.", originOpt)
              Int = fun value _ -> Int (value * value)
              Int64 = fun value _ -> Int64 (value * value)
              Single = fun value _ -> Single (value * value)
              Double = fun value _ -> Double (value * value)
              Vector2 = fun value _ -> Vector2 (Vector2.Multiply (value, value))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqr"], "Cannot square a table.", originOpt) }

        let SqrtFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Sqrt (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Sqrt (double value))
              Single = fun value _ -> Single (single ^ Math.Sqrt (double value))
              Double = fun value _ -> Double (Math.Sqrt value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqrt"], "Cannot square root a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sqtr"], "Cannot square root a table.", originOpt) }

        let FloorFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Floor (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Floor (double value))
              Single = fun value _ -> Single (single ^ Math.Floor (double value))
              Double = fun value _ -> Double (Math.Floor value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Floor"], "Cannot floor a table.", originOpt) }

        let CeilingFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Ceiling (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Ceiling (double value))
              Single = fun value _ -> Single (single ^ Math.Ceiling (double value))
              Double = fun value _ -> Double (Math.Ceiling value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Ceiling"], "Cannot ceiling a table.", originOpt) }

        let TruncateFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Truncate (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Truncate (double value))
              Single = fun value _ -> Single (single ^ Math.Truncate (double value))
              Double = fun value _ -> Double (Math.Truncate value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Truncate"], "Cannot truncate a table.", originOpt) }

        let ExpFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Exp (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Exp (double value))
              Single = fun value _ -> Single (single ^ Math.Exp (double value))
              Double = fun value _ -> Double (Math.Exp value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Exp"], "Cannot exponentiate a table.", originOpt) }

        let RoundFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Round (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Round (double value))
              Single = fun value _ -> Single (single ^ Math.Round (double value))
              Double = fun value _ -> Double (Math.Round value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Round"], "Cannot round a table.", originOpt) }

        let LogFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation (["OutOfRangeArgument"; "Unary"; "Log"], "Cannot log a zero int.", originOpt) else Int (int ^ Math.Log (double value))
              Int64 = fun value originOpt -> if value = 0L then Violation (["OutOfRangeArgument"; "Unary"; "Log"], "Cannot log a zero 64-bit int.", originOpt) else Int64 (int64 ^ Math.Log (double value))
              Single = fun value originOpt -> if value = 0.0f then Violation (["OutOfRangeArgument"; "Unary"; "Log"], "Cannot log a zero single.", originOpt) else Single (single ^ Math.Log (double value))
              Double = fun value originOpt -> if value = 0.0 then Violation (["OutOfRangeArgument"; "Unary"; "Log"], "Cannot log a zero double.", originOpt) else Double (Math.Log value)
              Vector2 = fun value originOpt ->
                if value.X = 0.0f || value.Y = 0.0f
                then Violation (["OutOfRangeArgument"; "Unary"; "Log"], "Cannot log a vector containing a zero member.", originOpt)
                else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Log"], "Cannot log a table.", originOpt) }

        let SinFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Sin (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Sin (double value))
              Single = fun value _ -> Single (single ^ Math.Sin (double value))
              Double = fun value _ -> Double (Math.Sin value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Sin"], "Cannot sin a table.", originOpt) }

        let CosFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Cos (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Cos (double value))
              Single = fun value _ -> Single (single ^ Math.Cos (double value))
              Double = fun value _ -> Double (Math.Cos value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Cos"], "Cannot cos a table.", originOpt) }

        let TanFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Tan (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Tan (double value))
              Single = fun value _ -> Single (single ^ Math.Tan (double value))
              Double = fun value _ -> Double (Math.Tan value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Tan"], "Cannot tan a table.", originOpt) }

        let AsinFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Asin (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Asin (double value))
              Single = fun value _ -> Single (single ^ Math.Asin (double value))
              Double = fun value _ -> Double (Math.Asin value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Asin"], "Cannot asin a table.", originOpt) }

        let AcosFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Acos (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Acos (double value))
              Single = fun value _ -> Single (single ^ Math.Acos (double value))
              Double = fun value _ -> Double (Math.Acos value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Acos"], "Cannot acos a table.", originOpt) }

        let AtanFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Atan (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Atan (double value))
              Single = fun value _ -> Single (single ^ Math.Atan (double value))
              Double = fun value _ -> Double (Math.Atan value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)))
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Atan"], "Cannot atan a table.", originOpt) }

        let LengthFns =
            { Bool = fun value _ -> Int (if value then 1 else 0)
              Int = fun value _ -> Int (Math.Abs value)
              Int64 = fun value _ -> Int64 (Math.Abs value)
              Single = fun value _ -> Single (Math.Abs value)
              Double = fun value _ -> Double (Math.Abs value)
              Vector2 = fun value _ -> Single (value.Length)
              String = fun value _ -> Int (value.Length)
              Keyword = fun value _ -> Int (value.Length)
              Tuple = fun value _ -> Int (Array.length value)
              Keyphrase = fun _ phrase _ -> Int (Array.length phrase)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Length"], "Cannot get length of codata.", originOpt)
              List = fun value _ -> Int (List.length value)
              Ring = fun value _ -> Int (value.Count)
              Table = fun value _ -> Int (value.Count) }

        let NormalFns =
            { Bool = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation (["OutOfRangeArgument"; "Unary"; "Normal"], "Cannot get the normal of a zero int.", originOpt) elif value < 0 then Int -1 else Int 1
              Int64 = fun value originOpt -> if value = 0L then Violation (["OutOfRangeArgument"; "Unary"; "Normal"], "Cannot get the normal of a zero 64-bit int.", originOpt) elif value < 0L then Int64 -1L else Int64 1L
              Single = fun value originOpt -> if value = 0.0f then Violation (["OutOfRangeArgument"; "Unary"; "Normal"], "Cannot get the normal of a zero single.", originOpt) elif value < 0.0f then Single -1.0f else Single 1.0f
              Double = fun value originOpt -> if value = 0.0 then Violation (["OutOfRangeArgument"; "Unary"; "Normal"], "Cannot get the normal of a zero double.", originOpt) elif value < 0.0 then Double -1.0 else Double 1.0
              Vector2 = fun value originOpt -> if value = Vector2.Zero then Violation (["OutOfRangeArgument"; "Unary"; "Normal"], "Cannot get the normal of a zero vector.", originOpt) else Vector2 (Vector2.Normalize value)
              String = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a string.", originOpt)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a keyphrase.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize codata.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a list.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a ring.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Normal"], "Cannot normalize a table.", originOpt) }

        let BoolFns =
            { Bool = fun value _ -> Bool (value)
              Int = fun value _ -> Bool (value = 0)
              Int64 = fun value _ -> Bool (value = 0L)
              Single = fun value _ -> Bool (value = 0.0f)
              Double = fun value _ -> Bool (value = 0.0)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a vector to a bool.", originOpt)
              String = fun value _ -> Bool (scvalue value)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a keyword to a bool.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a tuple to a bool.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a keyphrase to a bool.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert codata to a bool.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a list to a bool.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a ring to a bool.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Bool"], "Cannot convert a table to a bool.", originOpt) }

        let IntFns =
            { Bool = fun value _ -> Int (if value then 1 else 0)
              Int = fun value _ -> Int (value)
              Int64 = fun value _ -> Int (int value)
              Single = fun value _ -> Int (int value)
              Double = fun value _ -> Int (int value)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a vector to an int.", originOpt)
              String = fun value _ -> Int (scvalue value)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a keyword to an int.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a tuple to an int.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a keyphrase to an int.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert codata to an int.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a list to an int.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a ring to an int.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int"], "Cannot convert a table to an int.", originOpt) }

        let Int64Fns =
            { Bool = fun value _ -> Int64 (if value then 1L else 0L)
              Int = fun value _ -> Int64 (int64 value)
              Int64 = fun value _ -> Int64 (value)
              Single = fun value _ -> Int64 (int64 value)
              Double = fun value _ -> Int64 (int64 value)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a vector to a 64-bit int.", originOpt)
              String = fun value _ -> Int64 (scvalue value)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a keyword to a 64-bit int.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a tuple to a 64-bit int.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a keyphrase to a 64-bit int.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert codata to a 64-bit int.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a list to a 64-bit int.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a ring to a 64-bit int.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Int64"], "Cannot convert a table to a 64-bit int.", originOpt) }

        let SingleFns =
            { Bool = fun value _ -> Single (if value then 1.0f else 0.0f)
              Int = fun value _ -> Single (single value)
              Int64 = fun value _ -> Single (single value)
              Single = fun value _ -> Single (value)
              Double = fun value _ -> Single (single value)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a vector to a single.", originOpt)
              String = fun value _ -> Single (scvalue value)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a keyword to a single.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a tuple to a single.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a keyphrase to a single.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert codata to a single.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a list to a single.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a ring to a single.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Single"], "Cannot convert a table to a single.", originOpt) }

        let DoubleFns =
            { Bool = fun value _ -> Double (if value then 1.0 else 0.0)
              Int = fun value _ -> Double (double value)
              Int64 = fun value _ -> Double (double value)
              Single = fun value _ -> Double (double value)
              Double = fun value _ -> Double (value)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a vector to a double.", originOpt)
              String = fun value _ -> Double (scvalue value)
              Keyword = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a keyword to a double.", originOpt)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a tuple to a double.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a keyphrase to a double.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert codata to a double.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a list to a double.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a ring to a double.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "Double"], "Cannot convert a table to a double.", originOpt) }

        let StringFns =
            { Bool = fun value _ -> String (scstring value)
              Int = fun value _ -> String (scstring value)
              Int64 = fun value _ -> String (scstring value)
              Single = fun value _ -> String (scstring value)
              Double = fun value _ -> String (scstring value)
              Vector2 = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a v2 to a string.", originOpt)
              String = fun value _ -> String (value)
              Keyword = fun value _ -> String (value)
              Tuple = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a tuple to a string.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a keyphrase to a string.", originOpt)
              Codata = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert codata to a string.", originOpt)
              List = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a list to a string.", originOpt)
              Ring = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a ring to a string.", originOpt)
              Table = fun _ originOpt -> Violation (["InvalidArgumentType"; "Unary"; "Conversion"; "String"], "Cannot convert a table to a string.", originOpt) }

        let evalBoolUnary fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] ->
                match evaledArg with
                | Bool bool -> (Bool (fn bool), world)
                | Violation _ as violation -> (violation, world)
                | _ -> (Violation (["InvalidArgumentType"; "Unary"; (String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", fnOriginOpt), world)
            | _ -> (Violation (["InvalidArgumentCount"; "Unary"; (String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalUnaryInner (fns : UnaryFns) fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Bool boolValue -> (fns.Bool boolValue fnOriginOpt, world)
            | Int intValue -> (fns.Int intValue fnOriginOpt, world)
            | Int64 int64Value -> (fns.Int64 int64Value fnOriginOpt, world)
            | Single singleValue -> (fns.Single singleValue fnOriginOpt, world)
            | Double doubleValue -> (fns.Double doubleValue fnOriginOpt, world)
            | Vector2 vector2Value -> (fns.Vector2 vector2Value fnOriginOpt, world)
            | String stringValue -> (fns.String stringValue fnOriginOpt, world)
            | Keyword keywordValue -> (fns.Keyword keywordValue fnOriginOpt, world)
            | Tuple tupleValue -> (fns.Tuple tupleValue fnOriginOpt, world)
            | Keyphrase (nameValue, phraseValue) -> (fns.Keyphrase nameValue phraseValue fnOriginOpt, world)
            | Codata codataValue -> (fns.Codata codataValue fnOriginOpt, world)
            | List listValue -> (fns.List listValue fnOriginOpt, world)
            | Ring ringValue -> (fns.Ring ringValue fnOriginOpt, world)
            | Table tableValue -> (fns.Table tableValue fnOriginOpt, world)
            | Violation _ as violation -> (violation, world)
            | _ -> (Violation (["InvalidArgumentType"; "Unary"; (String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", fnOriginOpt), world)

        let evalUnary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> evalUnaryInner fns fnOriginOpt fnName evaledArg world
            | _ -> (Violation (["InvalidArgumentCount"; "Unary"; (String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)