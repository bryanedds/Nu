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
#nowarn "21"
#nowarn "40"

[<AutoOpen>]
module WorldScriptSystem =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : Vmap<Guid, Script>
              Debugging : bool }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module ScriptSystem =

        let rec private tryImportList (value : obj) (ty : Type) optOrigin =
            try let garg = (ty.GetGenericArguments ()).[0]
                let objList = Reflection.objToObjList value
                let optEvaledList = List.map (fun item -> tryImport item garg optOrigin) objList
                match List.definitizePlus optEvaledList with
                | (true, evaledList) -> Some (List (evaledList, optOrigin))
                | (false, _) -> None
            with _ -> None

        and private tryImport value (ty : Type) optOrigin =
            let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
            match Importers.TryGetValue ty.Name with
            | (true, tryImportFn) -> tryImportFn value ty optOrigin
            | (false, _) -> None

        and private Importers : Dictionary<string, obj -> Type -> Origin option -> Expr option> =
            [(typeof<Expr>.Name, (fun (value : obj) _ _ -> Some (value :?> Expr)))
             (typeof<bool>.Name, (fun (value : obj) _ optOrigin -> match value with :? bool as bool -> Some (Bool (bool, optOrigin)) | _ -> None))
             (typeof<int>.Name, (fun (value : obj) _ optOrigin -> match value with :? int as int -> Some (Int (int, optOrigin)) | _ -> None))
             (typeof<int64>.Name, (fun (value : obj) _ optOrigin -> match value with :? int64 as int64 -> Some (Int64 (int64, optOrigin)) | _ -> None))
             (typeof<single>.Name, (fun (value : obj) _ optOrigin -> match value with :? single as single -> Some (Single (single, optOrigin)) | _ -> None))
             (typeof<double>.Name, (fun (value : obj) _ optOrigin -> match value with :? double as double -> Some (Double (double, optOrigin)) | _ -> None))
             (typeof<Vector2>.Name, (fun (value : obj) _ optOrigin -> match value with :? Vector2 as vector2 -> Some (Vector2 (vector2, optOrigin)) | _ -> None))
             (typedefof<_ list>.Name, (fun value ty optOrigin -> tryImportList value ty optOrigin))] |>
            dictC

        and private tryImportEventData event optOrigin =
            match tryImport event.Data event.DataType optOrigin with
            | Some data -> data
            | None -> Violation ([!!"InvalidStreamValue"], "Stream value could not be imported into scripting environment.", optOrigin)

        let rec private tryExportList (evaled : Expr) (ty : Type) =
            match evaled with
            | List (evaleds, _) ->
                let garg = ty.GetGenericArguments () |> Array.item 0
                let itemType = if garg.IsGenericTypeDefinition then garg.GetGenericTypeDefinition () else garg
                let optItems =
                    List.map (fun evaledItem ->
                        match Exporters.TryGetValue itemType.Name with
                        | (true, tryExport) -> tryExport evaledItem itemType
                        | (false, _) -> None)
                        evaleds
                match List.definitizePlus optItems with
                | (true, items) -> Some (Reflection.objsToList ty items)
                | (false, _) -> None
            | _ -> None

        and private Exporters : Dictionary<string, Expr -> Type -> obj option> =
            [(typeof<bool>.Name, (fun evaled _ -> match evaled with Bool (value, _) -> value :> obj |> Some | _ -> None))
             (typeof<int>.Name, (fun evaled _ -> match evaled with Int (value, _) -> value :> obj |> Some | _ -> None))
             (typeof<int64>.Name, (fun evaled _ -> match evaled with Int64 (value, _) -> value :> obj |> Some | _ -> None))
             (typeof<single>.Name, (fun evaled _ -> match evaled with Single (value, _) -> value :> obj |> Some | _ -> None))
             (typeof<double>.Name, (fun evaled _ -> match evaled with Double (value, _) -> value :> obj |> Some | _ -> None))
             (typeof<Vector2>.Name, (fun evaled _ -> match evaled with Vector2 (value, _) -> value :> obj |> Some | _ -> None))
             (typedefof<_ list>.Name, tryExportList)] |>
            dictC

        type [<NoEquality; NoComparison>] UnaryFns =
            { Bool : bool -> Origin option -> Expr
              Int : int -> Origin option -> Expr
              Int64 : int64 -> Origin option -> Expr
              Single : single -> Origin option -> Expr
              Double : double -> Origin option -> Expr
              Vector2 : Vector2 -> Origin option -> Expr
              String : string -> Origin option -> Expr
              List : Expr list -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Origin option -> Expr
              Phrase : Map<int, Expr> -> Origin option -> Expr }

        let SqrFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a bool.", optOrigin)
              Int = fun value optOrigin -> Int (value * value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (value * value, optOrigin)
              Single = fun value optOrigin -> Single (value * value, optOrigin)
              Double = fun value optOrigin -> Double (value * value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (Vector2.Multiply (value, value), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a phrase.", optOrigin) }

        let SqrtFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Sqrt (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Sqrt (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Sqrt (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Sqrt value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a phrase.", optOrigin) }

        let FloorFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Floor (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Floor (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Floor (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Floor value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a phrase.", optOrigin) }

        let CeilingFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Ceiling (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Ceiling (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Ceiling (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Ceiling value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a phrase.", optOrigin) }

        let TruncateFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Truncate (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Truncate (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Truncate (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Truncate value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a phrase.", optOrigin) }

        let ExpFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Exp (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Exp (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Exp (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Exp value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a phrase.", optOrigin) }

        let RoundFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Round (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Round (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Round (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Round value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a phrase.", optOrigin) }

        let LogFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a bool.", optOrigin)
              Int = fun value optOrigin -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero int.", optOrigin) else Int (int ^ Math.Log (double value), optOrigin)
              Int64 = fun value optOrigin -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero 64-bit int.", optOrigin) else Int64 (int64 ^ Math.Log (double value), optOrigin)
              Single = fun value optOrigin -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero single.", optOrigin) else Single (single ^ Math.Log (double value), optOrigin)
              Double = fun value optOrigin -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero double.", optOrigin) else Double (Math.Log value, optOrigin)
              Vector2 = fun value optOrigin -> if value.X = 0.0f || value.Y == 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a vector containing a zero member.", optOrigin) else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a phrase.", optOrigin) }

        let SinFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Sin (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Sin (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Sin (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Sin value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a phrase.", optOrigin) }

        let CosFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Cos (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Cos (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Cos (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Cos value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a phrase.", optOrigin) }

        let TanFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Tan (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Tan (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Tan (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Tan value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a phrase.", optOrigin) }

        let AsinFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Asin (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Asin (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Asin (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Asin value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a phrase.", optOrigin) }

        let AcosFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Acos (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Acos (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Acos (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Acos value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a phrase.", optOrigin) }

        let AtanFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a bool.", optOrigin)
              Int = fun value optOrigin -> Int (int ^ Math.Atan (double value), optOrigin)
              Int64 = fun value optOrigin -> Int64 (int64 ^ Math.Atan (double value), optOrigin)
              Single = fun value optOrigin -> Single (single ^ Math.Atan (double value), optOrigin)
              Double = fun value optOrigin -> Double (Math.Atan value, optOrigin)
              Vector2 = fun value optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)), optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a phrase.", optOrigin) }

        let LengthFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"length"], "Cannot get length of a bool.", optOrigin)
              Int = fun value optOrigin -> Int (Math.Abs value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (Math.Abs value, optOrigin)
              Single = fun value optOrigin -> Single (Math.Abs value, optOrigin)
              Double = fun value optOrigin -> Double (Math.Abs value, optOrigin)
              Vector2 = fun value optOrigin -> Single (value.Length, optOrigin)
              String = fun value optOrigin -> Int (value.Length, optOrigin)
              List = fun value optOrigin -> Int (List.length value, optOrigin)
              Tuple = fun value optOrigin -> Int (Array.length ^ Map.toArray value, optOrigin)
              Phrase = fun value optOrigin -> Int (Array.length ^ Map.toArray value, optOrigin) }

        let NormalFns =
            { Bool = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a bool.", optOrigin)
              Int = fun value optOrigin -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero int.", optOrigin) elif value < 0 then Int (-1, optOrigin) else Int (1, optOrigin)
              Int64 = fun value optOrigin -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero 64-bit int.", optOrigin) elif value < 0L then Int64 (-1L, optOrigin) else Int64 (1L, optOrigin)
              Single = fun value optOrigin -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero single.", optOrigin) elif value < 0.0f then Single (-1.0f, optOrigin) else Single (1.0f, optOrigin)
              Double = fun value optOrigin -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero double.", optOrigin) elif value < 0.0 then Double (-1.0, optOrigin) else Double (1.0, optOrigin)
              Vector2 = fun value optOrigin -> if value = Vector2.Zero then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero vector.", optOrigin) else Vector2 (Vector2.Normalize value, optOrigin)
              String = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a string.", optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a list.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a phrase.", optOrigin) }

        let BoolFns =
            { Bool = fun value optOrigin -> Bool (value, optOrigin)
              Int = fun value optOrigin -> Bool ((value = 0), optOrigin)
              Int64 = fun value optOrigin -> Bool ((value = 0L), optOrigin)
              Single = fun value optOrigin -> Bool ((value = 0.0f), optOrigin)
              Double = fun value optOrigin -> Bool ((value = 0.0), optOrigin)
              Vector2 = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a vector to a bool.", optOrigin)
              String = fun value optOrigin -> Bool (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a list to a bool.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a bool to a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a bool to a phrase.", optOrigin) }

        let IntFns =
            { Bool = fun value optOrigin -> Int ((if value then 1 else 0), optOrigin)
              Int = fun value optOrigin -> Int (value, optOrigin)
              Int64 = fun value optOrigin -> Int (int value, optOrigin)
              Single = fun value optOrigin -> Int (int value, optOrigin)
              Double = fun value optOrigin -> Int (int value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a vector to an int.", optOrigin)
              String = fun value optOrigin -> Int (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a list to an int.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert an int to a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert an int to a phrase.", optOrigin) }

        let Int64Fns =
            { Bool = fun value optOrigin -> Int64 ((if value then 1L else 0L), optOrigin)
              Int = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Int64 = fun value optOrigin -> Int64 (value, optOrigin)
              Single = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Double = fun value optOrigin -> Int64 (int64 value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a vector to a 64-bit int.", optOrigin)
              String = fun value optOrigin -> Int64 (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a list to a 64-bit int.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert an int64 to a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert an int64 to a phrase.", optOrigin) }

        let SingleFns =
            { Bool = fun value optOrigin -> Single ((if value then 1.0f else 0.0f), optOrigin)
              Int = fun value optOrigin -> Single (single value, optOrigin)
              Int64 = fun value optOrigin -> Single (single value, optOrigin)
              Single = fun value optOrigin -> Single (value, optOrigin)
              Double = fun value optOrigin -> Single (single value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a vector to a single.", optOrigin)
              String = fun value optOrigin -> Single (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a list to a single.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a single to a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a single to a phrase.", optOrigin) }

        let DoubleFns =
            { Bool = fun value optOrigin -> Double ((if value then 1.0 else 0.0), optOrigin)
              Int = fun value optOrigin -> Double (double value, optOrigin)
              Int64 = fun value optOrigin -> Double (double value, optOrigin)
              Single = fun value optOrigin -> Double (double value, optOrigin)
              Double = fun value optOrigin -> Double (value, optOrigin)
              Vector2 = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a vector to a double.", optOrigin)
              String = fun value optOrigin -> Double (scvalue value, optOrigin)
              List = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a list to a double.", optOrigin)
              Tuple = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a double to a tuple.", optOrigin)
              Phrase = fun _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a double to a phrase.", optOrigin) }

        let StringFns =
            { Bool = fun value optOrigin -> String (scstring value, optOrigin)
              Int = fun value optOrigin -> String (scstring value, optOrigin)
              Int64 = fun value optOrigin -> String (scstring value, optOrigin)
              Single = fun value optOrigin -> String (scstring value, optOrigin)
              Double = fun value optOrigin -> String (scstring value, optOrigin)
              Vector2 = fun value optOrigin -> String (scstring value, optOrigin)
              String = fun value optOrigin -> String (value, optOrigin)
              List = fun value optOrigin -> String (scstring value, optOrigin)
              Tuple = fun value optOrigin -> String (scstring value, optOrigin)
              Phrase = fun value optOrigin -> String (scstring value, optOrigin) }

        type [<NoEquality; NoComparison>] BinaryFns =
            { Bool : bool -> bool -> Origin option -> Expr
              Int : int -> int -> Origin option -> Expr
              Int64 : int64 -> int64 -> Origin option -> Expr
              Single : single -> single -> Origin option -> Expr
              Double : double -> double -> Origin option -> Expr
              Vector2 : Vector2 -> Vector2 -> Origin option -> Expr
              String : string -> string -> Origin option -> Expr
              List : Expr list -> Expr list -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr
              Phrase : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr }

        let EqFns =
            { Bool = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left = right), optOrigin)
              String = fun left right optOrigin -> Bool ((left = right), optOrigin)
              List = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Tuple = fun left right optOrigin -> Bool ((left = right), optOrigin)
              Phrase = fun left right optOrigin -> Bool ((left = right), optOrigin) }

        let NotEqFns =
            { Bool = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              String = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              List = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Tuple = fun left right optOrigin -> Bool ((left <> right), optOrigin)
              Phrase = fun left right optOrigin -> Bool ((left <> right), optOrigin) }

        let LtFns =
            { Bool = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left < right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared < right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left < right), optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Lt"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Lt"], "Cannot compare tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Lt"], "Cannot compare phrases.", optOrigin) }

        let GtFns =
            { Bool = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left > right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared > right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left > right), optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Gt"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Gt"], "Cannot compare tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"Gt"], "Cannot compare phrases.", optOrigin) }

        let LtEqFns =
            { Bool = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared <= right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left <= right), optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"LtEq"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"LtEq"], "Cannot compare tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"LtEq"], "Cannot compare phrases.", optOrigin) }

        let GtEqFns =
            { Bool = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Int = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Int64 = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Single = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Double = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              Vector2 = fun left right optOrigin -> Bool ((left.LengthSquared >= right.LengthSquared), optOrigin)
              String = fun left right optOrigin -> Bool ((left >= right), optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"GtEq"], "Cannot compare lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"GtEq"], "Cannot compare tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Comparison"; !!"GtEq"], "Cannot compare phrases.", optOrigin) }

        let AddFns =
            { Bool = fun left right optOrigin -> Bool ((if left && right then false elif left then true elif right then true else false), optOrigin)
              Int = fun left right optOrigin -> Int ((left + right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left + right), optOrigin)
              Single = fun left right optOrigin -> Single ((left + right), optOrigin)
              Double = fun left right optOrigin -> Double ((left + right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 ((left + right), optOrigin)
              String = fun left right optOrigin -> String ((left + right), optOrigin)
              List = fun left right optOrigin -> List ((left @ right), optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add phrases.", optOrigin) }

        let SubFns =
            { Bool = fun left right optOrigin -> Bool ((if left && right then false elif left then true elif right then true else false), optOrigin)
              Int = fun left right optOrigin -> Int ((left - right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left - right), optOrigin)
              Single = fun left right optOrigin -> Single ((left - right), optOrigin)
              Double = fun left right optOrigin -> Double ((left - right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 ((left - right), optOrigin)
              String = fun left right optOrigin -> String (left.Replace (right, String.Empty), optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract phrases.", optOrigin) }

        let MulFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (Vector2.Multiply (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply phrases.", optOrigin) }

        let DivFns =
            { Bool = fun left right optOrigin -> if right = false then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a false bool.", optOrigin) else Bool ((if left && right then true else false), optOrigin)
              Int = fun left right optOrigin -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero int.", optOrigin) else Int ((left / right), optOrigin)
              Int64 = fun left right optOrigin -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero 64-bit int.", optOrigin) else Int64 ((left / right), optOrigin)
              Single = fun left right optOrigin -> Single ((left / right), optOrigin)
              Double = fun left right optOrigin -> Double ((left / right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (Vector2.Divide (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide phrases.", optOrigin) }

        let ModFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate bools.", optOrigin)
              Int = fun left right optOrigin -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot modulate by a zero int.", optOrigin) else Int ((left % right), optOrigin)
              Int64 = fun left right optOrigin -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot divide by a zero 64-bit int.", optOrigin) else Int64 ((left % right), optOrigin)
              Single = fun left right optOrigin -> Single ((left % right), optOrigin)
              Double = fun left right optOrigin -> Double ((left % right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate phrases.", optOrigin) }

        let PowFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power bools.", optOrigin)
              Int = fun left right optOrigin -> Int (int ^ Math.Pow (double left, double right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 (int64 ^ Math.Pow (double left, double right), optOrigin)
              Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, double right), optOrigin)
              Double = fun left right optOrigin -> Double (Math.Pow (double left, double right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power phrases.", optOrigin) }

        let RootFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root bools.", optOrigin)
              Int = fun left right optOrigin -> Int (int ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Single = fun left right optOrigin -> Single (single ^ Math.Pow (double left, 1.0 / double right), optOrigin)
              Double = fun left right optOrigin -> Double (Math.Pow (double left, 1.0 / double right), optOrigin)
              Vector2 = fun left right optOrigin -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root phrases.", optOrigin) }

        let CrossFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply 2-dimensional vectors.", optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiple phrases.", optOrigin) }

        let DotFns =
            { Bool = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply bools.", optOrigin)
              Int = fun left right optOrigin -> Int ((left * right), optOrigin)
              Int64 = fun left right optOrigin -> Int64 ((left * right), optOrigin)
              Single = fun left right optOrigin -> Single ((left * right), optOrigin)
              Double = fun left right optOrigin -> Double ((left * right), optOrigin)
              Vector2 = fun left right optOrigin -> Single (Vector2.Dot (left, right), optOrigin)
              String = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply strings.", optOrigin)
              List = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply lists.", optOrigin)
              Tuple = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply tuples.", optOrigin)
              Phrase = fun _ _ optOrigin -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply phrases.", optOrigin) }

        let combine optOriginLeft optOriginRight =
            match (optOriginLeft, optOriginRight) with
            | (Some originLeft, Some originRight) -> Some { Start = originLeft.Start; Stop = originRight.Stop }
            | (_, _) -> None

        let addStream name stream optOrigin env =
            let context = Env.getContext env
            let streamAddress = Address.makeFromNames [!!"Stream"; !!name] ->>- context.ParticipantAddress
            let stream = stream |> Stream.map (fun event _ -> tryImportEventData event optOrigin :> obj) |> Stream.lifetime context
            let (unsubscribe, env) =
                let world = Env.getWorld env
                let world = match Env.tryGetStream streamAddress env with Some (_, unsubscribe) -> unsubscribe world | None -> world
                let (unsubscribe, world) = Stream.subscribePlus (fun event world -> (Cascade, World.publish event.Data streamAddress EventTrace.empty context world)) stream world
                let env = Env.setWorld World.choose world env
                (unsubscribe, env)
            Env.addStream streamAddress (stream, unsubscribe) env

        let evalBoolUnary fnOptOrigin fnName fn evaledArgs env =
            match evaledArgs with
            | [evaled] ->
                match evaled with
                | Bool (bool, optOrigin) -> (Bool (fn bool, optOrigin), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", Expr.getOptOrigin evaled), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

        let evalBoolBinary fnOptOrigin fnName fn evaledArgs env =
            match evaledArgs with
            | [evaledLeft; evaledRight] ->
                match (evaledLeft, evaledRight) with
                | (Bool (boolLeft, optOriginLeft), Bool (boolRight, optOriginRight)) -> (Bool (fn boolLeft boolRight, combine optOriginLeft optOriginRight), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

        let evalUnary fnOptOrigin fnName (fns : UnaryFns) evaledArgs env =
            match evaledArgs with
            | [evaled] ->
                match evaled with
                | Bool (boolValue, optOrigin) -> ((fns.Bool boolValue optOrigin), env)
                | Int (intValue, optOrigin) -> ((fns.Int intValue optOrigin), env)
                | Int64 (int64Value, optOrigin) -> ((fns.Int64 int64Value optOrigin), env)
                | Single (singleValue, optOrigin) -> ((fns.Single singleValue optOrigin), env)
                | Double (doubleValue, optOrigin) -> ((fns.Double doubleValue optOrigin), env)
                | Vector2 (vector2Value, optOrigin) -> ((fns.Vector2 vector2Value optOrigin), env)
                | String (stringValue, optOrigin) -> ((fns.String stringValue optOrigin), env)
                | Tuple (tupleValue, optOrigin) -> ((fns.Tuple tupleValue optOrigin), env)
                | List (listValue, optOrigin) -> ((fns.List listValue optOrigin), env)
                | Phrase (phraseValue, optOrigin) -> ((fns.Phrase phraseValue optOrigin), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", Expr.getOptOrigin evaled), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)

        let evalBinary fnOptOrigin fnName (fns : BinaryFns) evaledArgs env =
            match evaledArgs with
            | [evaledLeft; evaledRight] ->
                match (evaledLeft, evaledRight) with
                | (Bool (boolLeft, optOriginLeft), Bool (boolRight, optOriginRight)) -> ((fns.Bool boolLeft boolRight (combine optOriginLeft optOriginRight)), env)
                | (Int (intLeft, optOriginLeft), Int (intRight, optOriginRight)) -> ((fns.Int intLeft intRight (combine optOriginLeft optOriginRight)), env)
                | (Int64 (int64Left, optOriginLeft), Int64 (int64Right, optOriginRight)) -> ((fns.Int64 int64Left int64Right (combine optOriginLeft optOriginRight)), env)
                | (Single (singleLeft, optOriginLeft), Single (singleRight, optOriginRight)) -> ((fns.Single singleLeft singleRight (combine optOriginLeft optOriginRight)), env)
                | (Double (doubleLeft, optOriginLeft), Double (doubleRight, optOriginRight)) -> ((fns.Double doubleLeft doubleRight (combine optOriginLeft optOriginRight)), env)
                | (Vector2 (vector2Left, optOriginLeft), Vector2 (vector2Right, optOriginRight)) -> ((fns.Vector2 vector2Left vector2Right (combine optOriginLeft optOriginRight)), env)
                | (String (stringLeft, optOriginLeft), String (stringRight, optOriginRight)) -> ((fns.String stringLeft stringRight (combine optOriginLeft optOriginRight)), env)
                | (Tuple (tupleLeft, optOriginLeft), Tuple (tupleRight, optOriginRight)) -> ((fns.Tuple tupleLeft tupleRight (combine optOriginLeft optOriginRight)), env)
                | (List (listLeft, optOriginLeft), List (listRight, optOriginRight)) -> ((fns.List listLeft listRight (combine optOriginLeft optOriginRight)), env)
                | (Phrase (phraseLeft, optOriginLeft), Phrase (phraseRight, optOriginRight)) -> ((fns.Phrase phraseLeft phraseRight (combine optOriginLeft optOriginRight)), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", combine (Expr.getOptOrigin evaledLeft) (Expr.getOptOrigin evaledRight)), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)
        
        let evalSome fnOptOrigin fnName args env =
            match args with
            | [evaled] -> (Option (Some evaled, fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalIsSome fnOptOrigin fnName args env =
            match args with
            | [Option (evaled, optOrigin)] -> (Bool (Option.isSome evaled, optOrigin), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalHead fnOptOrigin fnName args env =
            match args with
            | [List (evaleds, _)] ->
                match evaleds with
                | evaledHead :: _ -> (evaledHead, env)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOptOrigin), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalTail fnOptOrigin fnName args env =
            match args with
            | [List (evaleds, optOrigin)] ->
                match evaleds with
                | _ :: evaledTail -> (List (evaledTail, optOrigin), env)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOptOrigin), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalCons fnOptOrigin fnName args env =
            match args with
            | [evaled; List (evaleds, optOrigin)] -> (List (evaled :: evaleds, optOrigin), env)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalIsEmpty fnOptOrigin fnName args env =
            match args with
            | [List (evaleds, optOrigin)] -> (Bool (List.isEmpty evaleds, optOrigin), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
    
        let evalNth5 fnOptOrigin fnName index args env =
            match args with
            | [Tuple (evaleds, optOrigin)] | [Phrase (evaleds, optOrigin)] ->
                match Map.tryFind index evaleds with
                | Some _ as optEvaled -> (Option (optEvaled, optOrigin), env)
                | None -> (Option (None, optOrigin), env)
            | [List (evaleds, optOrigin)] ->
                match List.tryFindAt index evaleds with
                | Some _ as optEvaled -> (Option (optEvaled, optOrigin), env)
                | None -> (Option (None, optOrigin), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-sequence.", fnOptOrigin), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOptOrigin), env)
        
        let evalNth fnOptOrigin fnName args env =
            match args with
            | [head; foot] ->
                match head with
                | Int (int, _) -> evalNth5 fnOptOrigin fnName int [foot] env
                | _ -> (Violation ([!!"InvalidNthArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Application of " + fnName + " requires an int for the first argument.", fnOptOrigin), env)
            | _ ->  (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOptOrigin), env)

        let rec evalIntrinsic optOrigin name args env =
            match name with
            | "!"  -> evalBoolUnary optOrigin name not args env
            | "&"  -> evalBoolBinary optOrigin name (&&) args env
            | "|"  -> evalBoolBinary optOrigin name (||) args env
            | "="  -> evalBinary optOrigin name EqFns args env
            | "<>" -> evalBinary optOrigin name NotEqFns args env
            | "<"  -> evalBinary optOrigin name LtFns args env
            | ">"  -> evalBinary optOrigin name GtFns args env
            | "<=" -> evalBinary optOrigin name LtEqFns args env
            | ">=" -> evalBinary optOrigin name GtEqFns args env
            | "+"  -> evalBinary optOrigin name AddFns args env
            | "-"  -> evalBinary optOrigin name SubFns args env
            | "*"  -> evalBinary optOrigin name MulFns args env
            | "/"  -> evalBinary optOrigin name DivFns args env
            | "%"  -> evalBinary optOrigin name ModFns args env
            | "pow" -> evalBinary optOrigin name PowFns args env
            | "root" -> evalBinary optOrigin name RootFns args env
            | "sqr" -> evalUnary optOrigin name SqrFns args env
            | "sqrt" -> evalUnary optOrigin name SqrtFns args env
            | "floor" -> evalUnary optOrigin name FloorFns args env
            | "ceiling" -> evalUnary optOrigin name CeilingFns args env
            | "truncate" -> evalUnary optOrigin name TruncateFns args env
            | "round" -> evalUnary optOrigin name RoundFns args env
            | "exp" -> evalUnary optOrigin name ExpFns args env
            | "log" -> evalUnary optOrigin name LogFns args env
            | "sin" -> evalUnary optOrigin name SinFns args env
            | "cos" -> evalUnary optOrigin name CosFns args env
            | "tan" -> evalUnary optOrigin name TanFns args env
            | "asin" -> evalUnary optOrigin name AsinFns args env
            | "acos" -> evalUnary optOrigin name AcosFns args env
            | "atan" -> evalUnary optOrigin name AtanFns args env
            | "length" -> evalUnary optOrigin name LengthFns args env
            | "normal" -> evalUnary optOrigin name NormalFns args env
            | "cross" -> evalBinary optOrigin name CrossFns args env
            | "dot" -> evalBinary optOrigin name DotFns args env
            | "bool" -> evalUnary optOrigin name BoolFns args env
            | "int" -> evalUnary optOrigin name IntFns args env
            | "int64" -> evalUnary optOrigin name Int64Fns args env
            | "single" -> evalUnary optOrigin name SingleFns args env
            | "double" -> evalUnary optOrigin name DoubleFns args env
            | "string" -> evalUnary optOrigin name StringFns args env
            | "some" -> evalSome optOrigin name args env
            | "isSome" -> evalIsSome optOrigin name args env
            | "head" -> evalHead optOrigin name args env
            | "tail" -> evalTail optOrigin name args env
            | "cons" -> evalCons optOrigin name args env
            | "isEmpty" -> evalIsEmpty optOrigin name args env
            | "fst" -> evalNth5 optOrigin name 0 args env
            | "snd" -> evalNth5 optOrigin name 1 args env
            | "thd" -> evalNth5 optOrigin name 2 args env
            | "fth" -> evalNth5 optOrigin name 3 args env
            | "fif" -> evalNth5 optOrigin name 4 args env
            | "nth" -> evalNth optOrigin name args env
            | "product" -> evalProduct optOrigin name args env
            | _ -> (Violation ([!!"InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", optOrigin), env)

        and evalProduct optOrigin name args env =
            match args with
            | [Stream (streamLeft, _); Stream (streamRight, optOriginRight)] ->
                match evalStreamToAddress name streamLeft optOrigin env with
                | Right (streamAddressLeft, env) ->
                    match evalStreamToStream name streamRight optOriginRight env with
                    | Right (streamRight, env) ->
                        let computedStream = Stream.product streamAddressLeft streamRight
                        (Stream (ComputedStream computedStream, optOrigin), env)
                    | Left violation -> (violation, env)
                | Left violation -> (violation, env)
            | _ -> (Violation ([!!"InvalidArgumentTypes"; !!(String.capitalize name)], "Incorrect types of arguments for application of '" + name + "'; 1 relation and 1 stream required.", optOrigin), env)

        and evalExprs exprs env =
            List.foldBack
                (fun expr (violations, evaleds, env) ->
                    let (evaled, env) = eval expr env
                    match evaled with
                    | Violation _ as violation -> (violation :: violations, evaleds, env)
                    | evaled -> (violations, evaled :: evaleds, env))
                exprs
                ([], [], env)

        and evalApply exprs optOrigin env =
            let oldEnv = env
            let (violations, evaleds, env) = evalExprs exprs env
            match violations with
            | Violation _ as violation :: _ -> (violation, oldEnv)
            | _ ->
                match evaleds with
                | fn :: args ->
                    match fn with
                    | Keyword (name, optOrigin) ->
                        let map = String (name, optOrigin) :: args |> List.indexed |> Map.ofList
                        (Phrase (map, optOrigin), env)
                    | Binding (name, optOrigin) ->
                        match Env.tryGetBinding name env with
                        | Some binding -> evalFn binding args env
                        | None -> evalIntrinsic optOrigin name args env
                    | _ -> (Violation ([!!"TODO: proper violation category."], "Cannot apply a non-binding.", optOrigin), env)
                | [] -> (Unit optOrigin, env)

        and evalIf condition consequent alternative optOrigin env =
            let oldEnv = env
            let (evaled, env) = eval condition env
            match evaled with
            | Violation _ -> (evaled, oldEnv)
            | Bool (bool, _) -> if bool then eval consequent env else eval alternative env
            | _ -> (Violation ([!!"InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", optOrigin), env)

        and evalCond exprPairs optOrigin env =
            List.foldUntil
                (fun (_, env) (condition, consequent) ->
                    let (evaled, env) = eval condition env
                    match evaled with
                    | Violation _ -> Some (evaled, env)
                    | Bool (bool, _) -> if bool then Some (eval consequent env) else None
                    | _ -> Some ((Violation ([!!"InvalidCondCondition"], "Must provide an expression that evaluates to a bool in a case condition.", optOrigin), env)))
                ((Unit optOrigin, env))
                exprPairs

        and evalTry body handlers _ env =
            let oldEnv = env
            let (evaled, env) = eval body env
            match evaled with
            | Violation (categories, _, optOrigin) ->
                List.foldUntil (fun (_, env) (handlerCategories, handlerBody) ->
                    let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                    if categoriesTrunc = handlerCategories then Some (eval handlerBody env) else None)
                    ((Unit optOrigin, oldEnv))
                    handlers
            | _ -> (evaled, env)

        and evalGet propertyName optRelationExpr optOrigin env =
            let context = Env.getContext env
            let eirAddressAndEnv =
                match optRelationExpr with
                | Some relationExpr ->
                    let (evaledExpr, env) = eval relationExpr env
                    match evaledExpr with
                    | String (str, optOrigin)
                    | Keyword (str, optOrigin) ->
                        ignore optOrigin
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        Right (address, env)
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", optOrigin))
                | None -> Right (context.SimulantAddress, env)
            match eirAddressAndEnv with
            | Right (address, env) ->
                let world = Env.getWorld env
                let eirPropertyValueAndType =
                    match Address.getNames address with
                    | [] -> Right (Simulants.Game.GetPropertyValueAndType propertyName world)
                    | [_] -> let screen = Screen.proxy (Address.changeType<Simulant, Screen> address) in Right (screen.GetPropertyValueAndType propertyName world)
                    | [_; _] -> let group = Group.proxy (Address.changeType<Simulant, Group> address) in Right (group.GetPropertyValueAndType propertyName world)
                    | [_; _; _] -> let entity = Entity.proxy (Address.changeType<Simulant, Entity> address) in Right (entity.GetPropertyValueAndType propertyName world)
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation can have no more than 3 parts.", optOrigin))
                match eirPropertyValueAndType with
                | Right (propertyValue, propertyType) ->
                    match Importers.TryGetValue propertyType.Name with
                    | (true, tryImport) ->
                        match tryImport propertyValue propertyType optOrigin with
                        | Some propertyValue -> (propertyValue, env)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting environment.", optOrigin), env)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting environment.", optOrigin), env)
                | Left violation -> (violation, env)
            | Left violation -> (violation, env)

        and evalSet propertyName propertyValueExpr optRelationExpr optOrigin env =
            let context = Env.getContext env
            let eirAddressAndEnv =
                match optRelationExpr with
                | Some relationExpr ->
                    let (evaledExpr, env) = eval relationExpr env
                    match evaledExpr with
                    | String (str, optOrigin)
                    | Keyword (str, optOrigin) ->
                        ignore optOrigin
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        Right (address, env)
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", optOrigin))
                | None -> Right (context.SimulantAddress, env)
            let (propertyValue, env) = eval propertyValueExpr env
            match eirAddressAndEnv with
            | Right (address, env) ->
                let world = Env.getWorld env
                let eirPropertyType =
                    match Address.getNames address with
                    | [] -> Right (Simulants.Game.GetPropertyType propertyName world)
                    | [_] -> let screen = Screen.proxy (Address.changeType<Simulant, Screen> address) in Right (screen.GetPropertyType propertyName world)
                    | [_; _] -> let group = Group.proxy (Address.changeType<Simulant, Group> address) in Right (group.GetPropertyType propertyName world)
                    | [_; _; _] -> let entity = Entity.proxy (Address.changeType<Simulant, Entity> address) in Right (entity.GetPropertyType propertyName world)
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation can have no more than 3 parts.", optOrigin))
                match eirPropertyType with
                | Right propertyType ->
                    match Exporters.TryGetValue propertyType.Name with
                    | (true, tryExport) ->
                        match tryExport propertyValue propertyType with
                        | Some propertyValue ->
                            let eirWorld =
                                match Address.getNames address with
                                | [] -> Right (Simulants.Game.Set propertyName propertyValue world)
                                | [_] -> let screen = Screen.proxy (Address.changeType<Simulant, Screen> address) in Right (screen.Set propertyName propertyValue world)
                                | [_; _] -> let group = Group.proxy (Address.changeType<Simulant, Group> address) in Right (group.Set propertyName propertyValue world)
                                | [_; _; _] -> let entity = Entity.proxy (Address.changeType<Simulant, Entity> address) in Right (entity.Set propertyName propertyValue world)
                                | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation can have no more than 3 parts.", optOrigin))
                            match eirWorld with
                            | Right world ->
                                let env = Env.setWorld World.choose world env
                                (Unit optOrigin, env)
                            | Left violation -> (violation, env)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", optOrigin), env)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", optOrigin), env)
                | Left violation -> (violation, env)
            | Left violation -> (violation, env)

        and evalDefine name expr optOrigin env =
            let (evaled, env) = eval expr env
            match Env.tryAddBinding true name evaled env with
            | Some env -> (Unit optOrigin, env)
            | None -> (Violation ([!!"InvalidDefinition"], "Definition '" + name + "' could not be created due to having the same name as another top-level binding.", optOrigin), env)

        and evalStreamToAddress name stream optOrigin env =
            match stream with
            | VariableStream variableName ->
                let context = Env.getContext env
                let variableAddress = Address.makeFromNames [!!"Stream"; !!variableName] ->>- context.ParticipantAddress
                Right (variableAddress, env)
            | EventStream eventAddress ->
                let (eventAddressEvaled, env) = eval eventAddress env
                match eventAddressEvaled with
                | String (eventAddressStr, optOrigin)
                | Keyword (eventAddressStr, optOrigin) ->
                    try let eventAddress = Address.makeFromString eventAddressStr in Right (eventAddress, env)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address '" + eventAddressStr + "'.", optOrigin))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address. Address must be either a string or a keyword", optOrigin))
            | PropertyStream (propertyName, propertyRelation) ->
                let (propertyRelationEvaled, env) = eval propertyRelation env
                match propertyRelationEvaled with
                | String (propertyRelationStr, optOrigin)
                | Keyword (propertyRelationStr, optOrigin) ->
                    try let context = Env.getContext env
                        let propertyRelation = Relation.makeFromString propertyRelationStr
                        let propertyAddress = Relation.resolve context.SimulantAddress propertyRelation -<<- Address.makeFromName !!propertyName
                        Right (propertyAddress, env)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation '" + propertyRelationStr + "'.", optOrigin))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation. Relation must be either a string or a keyword", optOrigin))
            | PropertyStreamMany _ -> Left (Violation ([!!"Unimplemented"], "Unimplemented feature.", optOrigin))
            | ComputedStream _ -> Left (Violation ([!!"InvalidStream"], "Cannot have a computed stream on the left-hand side due to the non-monoidal nature of the Stream functor.", optOrigin))

        and evalStreamToStream name stream optOrigin env =
            match stream with
            | ComputedStream computedStream -> Right (computedStream :?> Prime.Stream<obj, World>, env)
            | _ ->
                match evalStreamToAddress name stream optOrigin env with
                | Right (streamAddress, env) ->
                    let stream = Stream.stream streamAddress
                    Right (stream, env)
                | Left violation -> Left violation

        and evalVariable name stream optOrigin env =
            match evalStreamToStream name stream optOrigin env with
            | Right (stream', env) -> (Unit optOrigin, addStream name stream' optOrigin env)
            | Left violation -> (violation, env)

        and evalFn _ _ env =
            // TODO: implement
            (Unit None, env)

        and eval expr env : Expr * Env<Simulant, World> =
            match expr with
            | Violation _ -> (expr, env)
            | Unit _ -> (expr, env)
            | Bool _ -> (expr, env)
            | Int _ -> (expr, env)
            | Int64 _ -> (expr, env)
            | Single _ -> (expr, env)
            | Double _ -> (expr, env)
            | Vector2 _ -> (expr, env)
            | String _ -> (expr, env)
            | Keyword _ -> (expr, env)
            | Option _ -> (expr, env)
            | Tuple _ -> (expr, env)
            | List _ -> (expr, env)
            | Phrase _ -> (expr, env)
            | Stream _ -> (expr, env)
            | Binding _ -> (expr, env)
            | Apply (exprs, optOrigin) -> evalApply exprs optOrigin env
            | Quote _  -> (expr, env)
            | Let _ -> (expr, env)
            | LetMany _ -> (expr, env)
            | Fun _ -> (expr, env)
            | If (condition, consequent, alternative, optOrigin) -> evalIf condition consequent alternative optOrigin env
            | Cond (exprPairs, optOrigin) -> evalCond exprPairs optOrigin env
            | Try (body, handlers, optOrigin) -> evalTry body handlers optOrigin env
            | Break (expr, _) -> (expr, env)
            | Get (name, optOrigin) -> evalGet name None optOrigin env
            | GetFrom (name, expr, optOrigin) -> evalGet name (Some expr) optOrigin env
            | Set (name, expr, optOrigin) -> evalSet name expr None optOrigin env
            | SetTo (name, expr, expr2, optOrigin) -> evalSet name expr2 (Some expr) optOrigin env
            | Define (name, expr, optOrigin) -> evalDefine name expr optOrigin env
            | Variable (name, stream, optOrigin) -> evalVariable name stream optOrigin env
            | Equate (_, _, _, _, optOrigin) -> (Unit optOrigin, env)
            | EquateMany (_, _, _, _, _, optOrigin) -> (Unit optOrigin, env)
            | Handle (_, _, optOrigin) -> (Unit optOrigin, env)

/// An abstract data type for executing scripts.
type ScriptSystem = WorldScriptSystem.ScriptSystem