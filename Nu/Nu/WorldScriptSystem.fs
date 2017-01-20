// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open OpenTK
open Prime
open Nu
open Nu.Scripting
#nowarn "21"
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScriptSystem =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { Scripts : UMap<Guid, Script>
              Debugging : bool }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module ScriptSystem =

        let rec private tryImportList (value : obj) (ty : Type) originOpt =
            try let garg = (ty.GetGenericArguments ()).[0]
                let objList = Reflection.objToObjList value
                let evaledListOpts = List.map (fun item -> tryImport item garg originOpt) objList
                match List.definitizePlus evaledListOpts with
                | (true, evaledList) -> Some (List (evaledList, originOpt))
                | (false, _) -> None
            with _ -> None

        and private tryImport value (ty : Type) originOpt =
            let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
            match Importers.TryGetValue ty.Name with
            | (true, tryImportFn) -> tryImportFn value ty originOpt
            | (false, _) -> None

        and private Importers : Dictionary<string, obj -> Type -> Origin option -> Expr option> =
            [(typeof<Expr>.Name, (fun (value : obj) _ _ -> Some (value :?> Expr)))
             (typeof<bool>.Name, (fun (value : obj) _ originOpt -> match value with :? bool as bool -> Some (Bool (bool, originOpt)) | _ -> None))
             (typeof<int>.Name, (fun (value : obj) _ originOpt -> match value with :? int as int -> Some (Int (int, originOpt)) | _ -> None))
             (typeof<int64>.Name, (fun (value : obj) _ originOpt -> match value with :? int64 as int64 -> Some (Int64 (int64, originOpt)) | _ -> None))
             (typeof<single>.Name, (fun (value : obj) _ originOpt -> match value with :? single as single -> Some (Single (single, originOpt)) | _ -> None))
             (typeof<double>.Name, (fun (value : obj) _ originOpt -> match value with :? double as double -> Some (Double (double, originOpt)) | _ -> None))
             (typeof<Vector2>.Name, (fun (value : obj) _ originOpt -> match value with :? Vector2 as vector2 -> Some (Vector2 (vector2, originOpt)) | _ -> None))
             (typedefof<_ list>.Name, (fun value ty originOpt -> tryImportList value ty originOpt))] |>
             // TODO: remaining importers
            dictPlus

        and private tryImportEventData evt originOpt =
            match tryImport evt.Data evt.DataType originOpt with
            | Some data -> data
            | None -> Violation ([!!"InvalidStreamValue"], "Stream value could not be imported into scripting environment.", originOpt)

        let rec private tryExportList (evaled : Expr) (ty : Type) =
            match evaled with
            | List (evaleds, _) ->
                let garg = ty.GetGenericArguments () |> Array.item 0
                let itemType = if garg.IsGenericTypeDefinition then garg.GetGenericTypeDefinition () else garg
                let itemOpts =
                    List.map (fun evaledItem ->
                        match Exporters.TryGetValue itemType.Name with
                        | (true, tryExport) -> tryExport evaledItem itemType
                        | (false, _) -> None)
                        evaleds
                match List.definitizePlus itemOpts with
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
             // TODO: remaining exporters
            dictPlus

        type [<NoEquality; NoComparison>] UnaryFns =
            { Bool : bool -> Origin option -> Expr
              Int : int -> Origin option -> Expr
              Int64 : int64 -> Origin option -> Expr
              Single : single -> Origin option -> Expr
              Double : double -> Origin option -> Expr
              Vector2 : Vector2 -> Origin option -> Expr
              String : string -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Origin option -> Expr
              Keyphrase : Map<int, Expr> -> Origin option -> Expr
              List : Expr list -> Origin option -> Expr
              Ring : Expr Set -> Origin option -> Expr
              Table : Map<Expr, Expr> -> Origin option -> Expr }

        let SqrFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a bool.", originOpt)
              Int = fun value originOpt -> Int (value * value, originOpt)
              Int64 = fun value originOpt -> Int64 (value * value, originOpt)
              Single = fun value originOpt -> Single (value * value, originOpt)
              Double = fun value originOpt -> Double (value * value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (Vector2.Multiply (value, value), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a table.", originOpt) }

        let SqrtFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Sqrt (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Sqrt (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Sqrt (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Sqrt value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqtr"], "Cannot square root a table.", originOpt) }

        let FloorFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Floor (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Floor (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Floor (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Floor value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a table.", originOpt) }

        let CeilingFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Ceiling (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Ceiling (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Ceiling (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Ceiling value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a table.", originOpt) }

        let TruncateFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Truncate (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Truncate (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Truncate (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Truncate value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a table.", originOpt) }

        let ExpFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Exp (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Exp (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Exp (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Exp value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a table.", originOpt) }

        let RoundFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Round (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Round (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Round (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Round value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a table.", originOpt) }

        let LogFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero int.", originOpt) else Int (int ^ Math.Log (double value), originOpt)
              Int64 = fun value originOpt -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero 64-bit int.", originOpt) else Int64 (int64 ^ Math.Log (double value), originOpt)
              Single = fun value originOpt -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero single.", originOpt) else Single (single ^ Math.Log (double value), originOpt)
              Double = fun value originOpt -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero double.", originOpt) else Double (Math.Log value, originOpt)
              Vector2 = fun value originOpt -> if value.X = 0.0f || value.Y = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a vector containing a zero member.", originOpt) else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a table.", originOpt) }

        let SinFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Sin (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Sin (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Sin (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Sin value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a table.", originOpt) }

        let CosFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Cos (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Cos (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Cos (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Cos value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a table.", originOpt) }

        let TanFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Tan (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Tan (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Tan (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Tan value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a table.", originOpt) }

        let AsinFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Asin (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Asin (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Asin (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Asin value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a table.", originOpt) }

        let AcosFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Acos (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Acos (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Acos (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Acos value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a table.", originOpt) }

        let AtanFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a bool.", originOpt)
              Int = fun value originOpt -> Int (int ^ Math.Atan (double value), originOpt)
              Int64 = fun value originOpt -> Int64 (int64 ^ Math.Atan (double value), originOpt)
              Single = fun value originOpt -> Single (single ^ Math.Atan (double value), originOpt)
              Double = fun value originOpt -> Double (Math.Atan value, originOpt)
              Vector2 = fun value originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)), originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a table.", originOpt) }

        let LengthFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"length"], "Cannot get length of a bool.", originOpt)
              Int = fun value originOpt -> Int (Math.Abs value, originOpt)
              Int64 = fun value originOpt -> Int64 (Math.Abs value, originOpt)
              Single = fun value originOpt -> Single (Math.Abs value, originOpt)
              Double = fun value originOpt -> Double (Math.Abs value, originOpt)
              Vector2 = fun value originOpt -> Single (value.Length, originOpt)
              String = fun value originOpt -> Int (value.Length, originOpt)
              Tuple = fun value originOpt -> Int (Array.length ^ Map.toArray value, originOpt)
              Keyphrase = fun value originOpt -> Int (Array.length ^ Map.toArray value, originOpt)
              List = fun value originOpt -> Int (List.length value, originOpt)
              Ring = fun value originOpt -> Int (value.Count, originOpt)
              Table = fun value originOpt -> Int (value.Count, originOpt) }

        let NormalFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero int.", originOpt) elif value < 0 then Int (-1, originOpt) else Int (1, originOpt)
              Int64 = fun value originOpt -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero 64-bit int.", originOpt) elif value < 0L then Int64 (-1L, originOpt) else Int64 (1L, originOpt)
              Single = fun value originOpt -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero single.", originOpt) elif value < 0.0f then Single (-1.0f, originOpt) else Single (1.0f, originOpt)
              Double = fun value originOpt -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero double.", originOpt) elif value < 0.0 then Double (-1.0, originOpt) else Double (1.0, originOpt)
              Vector2 = fun value originOpt -> if value = Vector2.Zero then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero vector.", originOpt) else Vector2 (Vector2.Normalize value, originOpt)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a string.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a table.", originOpt) }

        let BoolFns =
            { Bool = fun value originOpt -> Bool (value, originOpt)
              Int = fun value originOpt -> Bool ((value = 0), originOpt)
              Int64 = fun value originOpt -> Bool ((value = 0L), originOpt)
              Single = fun value originOpt -> Bool ((value = 0.0f), originOpt)
              Double = fun value originOpt -> Bool ((value = 0.0), originOpt)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a vector to a bool.", originOpt)
              String = fun value originOpt -> Bool (scvalue value, originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a bool to a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a bool to a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a list to a bool.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a ring to a bool.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a table to a bool.", originOpt) }

        let IntFns =
            { Bool = fun value originOpt -> Int ((if value then 1 else 0), originOpt)
              Int = fun value originOpt -> Int (value, originOpt)
              Int64 = fun value originOpt -> Int (int value, originOpt)
              Single = fun value originOpt -> Int (int value, originOpt)
              Double = fun value originOpt -> Int (int value, originOpt)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a vector to an int.", originOpt)
              String = fun value originOpt -> Int (scvalue value, originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert an int to a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert an int to a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a list to an int.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a ring to an int.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a table to an int.", originOpt) }

        let Int64Fns =
            { Bool = fun value originOpt -> Int64 ((if value then 1L else 0L), originOpt)
              Int = fun value originOpt -> Int64 (int64 value, originOpt)
              Int64 = fun value originOpt -> Int64 (value, originOpt)
              Single = fun value originOpt -> Int64 (int64 value, originOpt)
              Double = fun value originOpt -> Int64 (int64 value, originOpt)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a vector to a 64-bit int.", originOpt)
              String = fun value originOpt -> Int64 (scvalue value, originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert an int64 to a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert an int64 to a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a list to a 64-bit int.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a ring to a 64-bit int.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a table to a 64-bit int.", originOpt) }

        let SingleFns =
            { Bool = fun value originOpt -> Single ((if value then 1.0f else 0.0f), originOpt)
              Int = fun value originOpt -> Single (single value, originOpt)
              Int64 = fun value originOpt -> Single (single value, originOpt)
              Single = fun value originOpt -> Single (value, originOpt)
              Double = fun value originOpt -> Single (single value, originOpt)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a vector to a single.", originOpt)
              String = fun value originOpt -> Single (scvalue value, originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a single to a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a single to a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a list to a single.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a ring to a single.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a table to a single.", originOpt) }

        let DoubleFns =
            { Bool = fun value originOpt -> Double ((if value then 1.0 else 0.0), originOpt)
              Int = fun value originOpt -> Double (double value, originOpt)
              Int64 = fun value originOpt -> Double (double value, originOpt)
              Single = fun value originOpt -> Double (double value, originOpt)
              Double = fun value originOpt -> Double (value, originOpt)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a vector to a double.", originOpt)
              String = fun value originOpt -> Double (scvalue value, originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a double to a tuple.", originOpt)
              Keyphrase = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a double to a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a list to a double.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a ring to a double.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a table to a double.", originOpt) }

        let StringFns =
            { Bool = fun value originOpt -> String (scstring value, originOpt)
              Int = fun value originOpt -> String (scstring value, originOpt)
              Int64 = fun value originOpt -> String (scstring value, originOpt)
              Single = fun value originOpt -> String (scstring value, originOpt)
              Double = fun value originOpt -> String (scstring value, originOpt)
              Vector2 = fun value originOpt -> String (scstring value, originOpt)
              String = fun value originOpt -> String (value, originOpt)
              Tuple = fun value originOpt -> String (scstring value, originOpt)
              Keyphrase = fun value originOpt -> String (scstring value, originOpt)
              List = fun value originOpt -> String (scstring value, originOpt)
              Ring = fun value originOpt -> String (scstring value, originOpt)
              Table = fun value originOpt -> String (scstring value, originOpt) }

        type [<NoEquality; NoComparison>] BinaryFns =
            { Bool : bool -> bool -> Origin option -> Expr
              Int : int -> int -> Origin option -> Expr
              Int64 : int64 -> int64 -> Origin option -> Expr
              Single : single -> single -> Origin option -> Expr
              Double : double -> double -> Origin option -> Expr
              Vector2 : Vector2 -> Vector2 -> Origin option -> Expr
              String : string -> string -> Origin option -> Expr
              Tuple : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr
              Keyphrase : Map<int, Expr> -> Map<int, Expr> -> Origin option -> Expr
              List : Expr list -> Expr list -> Origin option -> Expr
              Ring : Expr Set -> Expr Set -> Origin option -> Expr
              Table : Map<Expr, Expr> -> Map<Expr, Expr> -> Origin option -> Expr }

        let EqFns =
            { Bool = fun left right originOpt -> Bool ((left = right), originOpt)
              Int = fun left right originOpt -> Bool ((left = right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left = right), originOpt)
              Single = fun left right originOpt -> Bool ((left = right), originOpt)
              Double = fun left right originOpt -> Bool ((left = right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left = right), originOpt)
              String = fun left right originOpt -> Bool ((left = right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left = right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left = right), originOpt)
              List = fun left right originOpt -> Bool ((left = right), originOpt)
              Ring = fun left right originOpt -> Bool ((left = right), originOpt)
              Table = fun left right originOpt -> Bool ((left = right), originOpt) }

        let NotEqFns =
            { Bool = fun left right originOpt -> Bool ((left <> right), originOpt)
              Int = fun left right originOpt -> Bool ((left <> right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left <> right), originOpt)
              Single = fun left right originOpt -> Bool ((left <> right), originOpt)
              Double = fun left right originOpt -> Bool ((left <> right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left <> right), originOpt)
              String = fun left right originOpt -> Bool ((left <> right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left <> right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left <> right), originOpt)
              List = fun left right originOpt -> Bool ((left <> right), originOpt)
              Ring = fun left right originOpt -> Bool ((left <> right), originOpt)
              Table = fun left right originOpt -> Bool ((left <> right), originOpt) }

        let LtFns =
            { Bool = fun left right originOpt -> Bool ((left < right), originOpt)
              Int = fun left right originOpt -> Bool ((left < right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left < right), originOpt)
              Single = fun left right originOpt -> Bool ((left < right), originOpt)
              Double = fun left right originOpt -> Bool ((left < right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left.LengthSquared < right.LengthSquared), originOpt)
              String = fun left right originOpt -> Bool ((left < right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left < right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left < right), originOpt)
              List = fun left right originOpt -> Bool ((left < right), originOpt)
              Ring = fun left right originOpt -> Bool ((left < right), originOpt)
              Table = fun left right originOpt -> Bool ((left < right), originOpt) }

        let GtFns =
            { Bool = fun left right originOpt -> Bool ((left > right), originOpt)
              Int = fun left right originOpt -> Bool ((left > right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left > right), originOpt)
              Single = fun left right originOpt -> Bool ((left > right), originOpt)
              Double = fun left right originOpt -> Bool ((left > right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left.LengthSquared > right.LengthSquared), originOpt)
              String = fun left right originOpt -> Bool ((left > right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left > right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left > right), originOpt)
              List = fun left right originOpt -> Bool ((left > right), originOpt)
              Ring = fun left right originOpt -> Bool ((left > right), originOpt)
              Table = fun left right originOpt -> Bool ((left > right), originOpt) }

        let LtEqFns =
            { Bool = fun left right originOpt -> Bool ((left <= right), originOpt)
              Int = fun left right originOpt -> Bool ((left <= right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left <= right), originOpt)
              Single = fun left right originOpt -> Bool ((left <= right), originOpt)
              Double = fun left right originOpt -> Bool ((left <= right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left.LengthSquared <= right.LengthSquared), originOpt)
              String = fun left right originOpt -> Bool ((left <= right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left <= right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left <= right), originOpt)
              List = fun left right originOpt -> Bool ((left <= right), originOpt)
              Ring = fun left right originOpt -> Bool ((left <= right), originOpt)
              Table = fun left right originOpt -> Bool ((left <= right), originOpt) }

        let GtEqFns =
            { Bool = fun left right originOpt -> Bool ((left >= right), originOpt)
              Int = fun left right originOpt -> Bool ((left >= right), originOpt)
              Int64 = fun left right originOpt -> Bool ((left >= right), originOpt)
              Single = fun left right originOpt -> Bool ((left >= right), originOpt)
              Double = fun left right originOpt -> Bool ((left >= right), originOpt)
              Vector2 = fun left right originOpt -> Bool ((left.LengthSquared >= right.LengthSquared), originOpt)
              String = fun left right originOpt -> Bool ((left >= right), originOpt)
              Tuple = fun left right originOpt -> Bool ((left >= right), originOpt)
              Keyphrase = fun left right originOpt -> Bool ((left >= right), originOpt)
              List = fun left right originOpt -> Bool ((left >= right), originOpt)
              Ring = fun left right originOpt -> Bool ((left >= right), originOpt)
              Table = fun left right originOpt -> Bool ((left >= right), originOpt) }

        let AddFns =
            { Bool = fun left right originOpt -> Bool ((if left && right then false elif left then true elif right then true else false), originOpt)
              Int = fun left right originOpt -> Int ((left + right), originOpt)
              Int64 = fun left right originOpt -> Int64 ((left + right), originOpt)
              Single = fun left right originOpt -> Single ((left + right), originOpt)
              Double = fun left right originOpt -> Double ((left + right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 ((left + right), originOpt)
              String = fun left right originOpt -> String ((left + right), originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add phrases.", originOpt)
              List = fun left right originOpt -> List ((left @ right), originOpt)
              Ring = fun left right originOpt -> Ring ((Set.union left right), originOpt)
              Table = fun left right originOpt -> Table ((left @@ right), originOpt) }

        let SubFns =
            { Bool = fun left right originOpt -> Bool ((if left && right then false elif left then true elif right then true else false), originOpt)
              Int = fun left right originOpt -> Int ((left - right), originOpt)
              Int64 = fun left right originOpt -> Int64 ((left - right), originOpt)
              Single = fun left right originOpt -> Single ((left - right), originOpt)
              Double = fun left right originOpt -> Double ((left - right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 ((left - right), originOpt)
              String = fun left right originOpt -> String (left.Replace (right, String.Empty), originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract lists.", originOpt)
              Ring = fun left right originOpt -> Ring (Set.difference left right, originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract tables.", originOpt) }

        let MulFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply bools.", originOpt)
              Int = fun left right originOpt -> Int ((left * right), originOpt)
              Int64 = fun left right originOpt -> Int64 ((left * right), originOpt)
              Single = fun left right originOpt -> Single ((left * right), originOpt)
              Double = fun left right originOpt -> Double ((left * right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 (Vector2.Multiply (left, right), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply tables.", originOpt) }

        let DivFns =
            { Bool = fun left right originOpt -> if right = false then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a false bool.", originOpt) else Bool ((if left && right then true else false), originOpt)
              Int = fun left right originOpt -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero int.", originOpt) else Int ((left / right), originOpt)
              Int64 = fun left right originOpt -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 ((left / right), originOpt)
              Single = fun left right originOpt -> Single ((left / right), originOpt)
              Double = fun left right originOpt -> Double ((left / right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 (Vector2.Divide (left, right), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide tables.", originOpt) }

        let ModFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate bools.", originOpt)
              Int = fun left right originOpt -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot modulate by a zero int.", originOpt) else Int ((left % right), originOpt)
              Int64 = fun left right originOpt -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 ((left % right), originOpt)
              Single = fun left right originOpt -> Single ((left % right), originOpt)
              Double = fun left right originOpt -> Double ((left % right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate tables.", originOpt) }

        let PowFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power bools.", originOpt)
              Int = fun left right originOpt -> Int (int ^ Math.Pow (double left, double right), originOpt)
              Int64 = fun left right originOpt -> Int64 (int64 ^ Math.Pow (double left, double right), originOpt)
              Single = fun left right originOpt -> Single (single ^ Math.Pow (double left, double right), originOpt)
              Double = fun left right originOpt -> Double (Math.Pow (double left, double right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power tables.", originOpt) }

        let RootFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root bools.", originOpt)
              Int = fun left right originOpt -> Int (int ^ Math.Pow (double left, 1.0 / double right), originOpt)
              Int64 = fun left right originOpt -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right), originOpt)
              Single = fun left right originOpt -> Single (single ^ Math.Pow (double left, 1.0 / double right), originOpt)
              Double = fun left right originOpt -> Double (Math.Pow (double left, 1.0 / double right), originOpt)
              Vector2 = fun left right originOpt -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root tables.", originOpt) }

        let CrossFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply bools.", originOpt)
              Int = fun left right originOpt -> Int ((left * right), originOpt)
              Int64 = fun left right originOpt -> Int64 ((left * right), originOpt)
              Single = fun left right originOpt -> Single ((left * right), originOpt)
              Double = fun left right originOpt -> Double ((left * right), originOpt)
              Vector2 = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply 2-dimensional vectors.", originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiple phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply tables.", originOpt) }

        let DotFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply bools.", originOpt)
              Int = fun left right originOpt -> Int ((left * right), originOpt)
              Int64 = fun left right originOpt -> Int64 ((left * right), originOpt)
              Single = fun left right originOpt -> Single ((left * right), originOpt)
              Double = fun left right originOpt -> Double ((left * right), originOpt)
              Vector2 = fun left right originOpt -> Single (Vector2.Dot (left, right), originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply strings.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply tuples.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply phrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply tables.", originOpt) }

        let combine originLeftOpt originRightOpt =
            match (originLeftOpt, originRightOpt) with
            | (Some originLeft, Some originRight) -> Some { Start = originLeft.Start; Stop = originRight.Stop }
            | (_, _) -> None

        let addStream name stream originOpt env =
            let context = Env.getContext env
            let streamAddress = ltoa [!!"Stream"; !!name; !!"Event"] ->>- context.ParticipantAddress
            let stream = stream |> Stream.mapEvent (fun evt _ -> tryImportEventData evt originOpt :> obj) |> Stream.lifetime context
            let (unsubscribe, env) =
                let world = Env.getWorld env
                let world = match Env.tryGetStream streamAddress env with Some (_, unsubscribe) -> unsubscribe world | None -> world
                let eventTrace = EventTrace.empty // TODO: implement event trace!
                let (unsubscribe, world) = Stream.subscribePlus (fun evt world -> (Cascade, World.publish6 evt.Data streamAddress eventTrace context false world)) context stream world
                let env = Env.setWorld World.choose world env
                (unsubscribe, env)
            Env.addStream streamAddress (stream, unsubscribe) env

        let evalBoolUnary fn fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaled] ->
                match evaled with
                | Bool (bool, originOpt) -> (Bool (fn bool, originOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", Expr.getOriginOpt evaled), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalBoolBinary fn fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaledLeft; evaledRight] ->
                match (evaledLeft, evaledRight) with
                | (Bool (boolLeft, originLeftOpt), Bool (boolRight, originRightOpt)) -> (Bool (fn boolLeft boolRight, combine originLeftOpt originRightOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", combine (Expr.getOriginOpt evaledLeft) (Expr.getOriginOpt evaledRight)), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), env)

        let evalUnaryInner (fns : UnaryFns) fnName evaledArg env =
            match evaledArg with
            | Bool (boolValue, originOpt) -> ((fns.Bool boolValue originOpt), env)
            | Int (intValue, originOpt) -> ((fns.Int intValue originOpt), env)
            | Int64 (int64Value, originOpt) -> ((fns.Int64 int64Value originOpt), env)
            | Single (singleValue, originOpt) -> ((fns.Single singleValue originOpt), env)
            | Double (doubleValue, originOpt) -> ((fns.Double doubleValue originOpt), env)
            | Vector2 (vector2Value, originOpt) -> ((fns.Vector2 vector2Value originOpt), env)
            | String (stringValue, originOpt) -> ((fns.String stringValue originOpt), env)
            | Tuple (tupleValue, originOpt) -> ((fns.Tuple tupleValue originOpt), env)
            | Keyphrase (_, phraseValue, originOpt) -> ((fns.Keyphrase phraseValue originOpt), env)
            | List (listValue, originOpt) -> ((fns.List listValue originOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", Expr.getOriginOpt evaledArg), env)

        let evalUnary fns fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaledArg] -> evalUnaryInner fns fnName evaledArg env
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalBinaryInner (fns : BinaryFns) fnName evaledLeft evaledRight env =
            match (evaledLeft, evaledRight) with
            | (Bool (boolLeft, originLeftOpt), Bool (boolRight, originRightOpt)) -> ((fns.Bool boolLeft boolRight (combine originLeftOpt originRightOpt)), env)
            | (Int (intLeft, originLeftOpt), Int (intRight, originRightOpt)) -> ((fns.Int intLeft intRight (combine originLeftOpt originRightOpt)), env)
            | (Int64 (int64Left, originLeftOpt), Int64 (int64Right, originRightOpt)) -> ((fns.Int64 int64Left int64Right (combine originLeftOpt originRightOpt)), env)
            | (Single (singleLeft, originLeftOpt), Single (singleRight, originRightOpt)) -> ((fns.Single singleLeft singleRight (combine originLeftOpt originRightOpt)), env)
            | (Double (doubleLeft, originLeftOpt), Double (doubleRight, originRightOpt)) -> ((fns.Double doubleLeft doubleRight (combine originLeftOpt originRightOpt)), env)
            | (Vector2 (vector2Left, originLeftOpt), Vector2 (vector2Right, originRightOpt)) -> ((fns.Vector2 vector2Left vector2Right (combine originLeftOpt originRightOpt)), env)
            | (String (stringLeft, originLeftOpt), String (stringRight, originRightOpt)) -> ((fns.String stringLeft stringRight (combine originLeftOpt originRightOpt)), env)
            | (Tuple (tupleLeft, originLeftOpt), Tuple (tupleRight, originRightOpt)) -> ((fns.Tuple tupleLeft tupleRight (combine originLeftOpt originRightOpt)), env)
            | (Keyphrase (_, phraseLeft, originLeftOpt), Keyphrase (_, phraseRight, originRightOpt)) -> ((fns.Keyphrase phraseLeft phraseRight (combine originLeftOpt originRightOpt)), env)
            | (List (listLeft, originLeftOpt), List (listRight, originRightOpt)) -> ((fns.List listLeft listRight (combine originLeftOpt originRightOpt)), env)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", combine (Expr.getOriginOpt evaledLeft) (Expr.getOriginOpt evaledRight)), env)

        let evalBinary fns fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaledLeft; evaledRight] -> evalBinaryInner fns fnName evaledLeft evaledRight env                
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), env)
        
        let evalV2 fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [Single (x, _); Single (y, _)] -> (Vector2 (OpenTK.Vector2 (x, y), fnOriginOpt), env)
            | [Violation _ as violation; _] -> (violation, env)
            | [_; Violation _ as violation] -> (violation, env)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"V2"; !!(String.capitalize fnName)], "Application of " + fnName + " requires a single for the both arguments.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"V2"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), env)

        let evalPair fnOriginOpt (_ : string) evaledArgs env =
            match evaledArgs with
            | [_; _] -> (Tuple (evaledArgs |> List.indexed |> Map.ofList, fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Pair"], "Incorrect number of arguments for creation of a pair; 2 arguments required.", fnOriginOpt), env)

        let evalTuple fnOriginOpt _ evaledArgs env =
            (Tuple (evaledArgs |> List.indexed |> Map.ofList, fnOriginOpt), env)
    
        let evalNth5 index fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [Tuple (evaleds, originOpt)] ->
                match Map.tryFind index evaleds with
                | Some evaled -> (evaled, env)
                | None -> (Violation ([!!"OutOfRange"], "Tuple does not contain element at index " + string index + ".", originOpt), env)
            | [Keyphrase (_, evaleds, originOpt)] ->
                match Map.tryFind index evaleds with
                | Some evaled -> (evaled, env)
                | None -> (Violation ([!!"OutOfRange"], "Keyphrase does not contain element at index " + string index + ".", originOpt), env)
            | [List (evaleds, originOpt)] ->
                match List.tryFindAt index evaleds with
                | Some evaled -> (evaled, env)
                | None -> (Violation ([!!"OutOfRange"], "List does not contain element at index " + string index + ".", originOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-sequence.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
        
        let evalNth fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [head; foot] ->
                match head with
                | Int (int, _) -> evalNth5 int fnOriginOpt fnName [foot] env
                | _ -> (Violation ([!!"InvalidNthArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Application of " + fnName + " requires an int for the first argument.", fnOriginOpt), env)
            | _ ->  (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), env)
            
        let evalSome fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaledArg] -> (Option (Some evaledArg, fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalIsNone fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [Option (evaled, originOpt)] -> (Bool (Option.isNone evaled, originOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalIsSome fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [Option (evaled, originOpt)] -> (Bool (Option.isSome evaled, originOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalMap evalApply fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [_; Option (opt, _) as option] ->
                match opt with
                | Some value -> evalApply [value] fnOriginOpt env
                | None -> (option, env)
            | [_; List (list, _)] ->
                let (list, env) =
                    List.fold (fun (elems, env) current ->
                        let (elem, env) = evalApply [current] fnOriginOpt env
                        (elem :: elems, env))
                        ([], env)
                        list
                (List (List.rev list, fnOriginOpt), env)
            | [_; Ring (set, _)] ->
                let (set, env) =
                    Set.fold (fun (elems, env) current ->
                        let (elem, env) = evalApply [current] fnOriginOpt env
                        (Set.add elem elems, env))
                        (Set.empty, env)
                        set
                (Ring (set, fnOriginOpt), env)
            | [_; Table (map, _)] ->
                let (map, env) =
                    Map.fold (fun (elems, env) currentKey currentValue ->
                        let current = Tuple ([currentKey; currentValue] |> List.indexed |> Map.ofList, fnOriginOpt)
                        let (elem, env) = evalApply [current] fnOriginOpt env
                        match elem with
                        | Tuple (elems', _) when elems'.Count = 2 -> ((Map.add (Map.find 0 elems') (Map.find 1 elems') elems), env)
                        | _ -> (elems, env))
                        (Map.empty, env)
                        map
                (Table (map, fnOriginOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Functor"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-functor.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Functor"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
            
        let evalList fnOriginOpt _ evaledArgs env =
            (List (evaledArgs, fnOriginOpt), env)
    
        let evalHead fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [List (evaledArgs, _)] ->
                match evaledArgs with
                | evaledHead :: _ -> (evaledHead, env)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalTail fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [List (evaleds, originOpt)] ->
                match evaleds with
                | _ :: evaledTail -> (List (evaledTail, originOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalCons fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [evaled; List (evaleds, originOpt)] -> (List (evaled :: evaleds, originOpt), env)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalIsEmpty fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [List (list, originOpt)] -> (Bool (List.isEmpty list, originOpt), env)
            | [Ring (set, originOpt)] -> (Bool (Set.isEmpty set, originOpt), env)
            | [Table (map, originOpt)] -> (Bool (Map.isEmpty map, originOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalNotEmpty fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [List (evaleds, originOpt)] -> (Bool (not ^ List.isEmpty evaleds, originOpt), env)
            | [Ring (set, originOpt)] -> (Bool (not ^ Set.isEmpty set, originOpt), env)
            | [Table (map, originOpt)] -> (Bool (not ^ Map.isEmpty map, originOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalFold evalApply fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [_; state; List (list, _)] -> List.fold (fun (acc, env) current -> evalApply [acc; current] fnOriginOpt env) (state, env) list
            | [_; state; Ring (set, _)] -> Set.fold (fun (acc, env) current -> evalApply [acc; current] fnOriginOpt env) (state, env) set
            | [_; state; Table (map, _)] ->
                Map.fold (fun (acc, env) currentKey currentValue ->
                    let current = Tuple ([currentKey; currentValue] |> List.indexed |> Map.ofList, fnOriginOpt)
                    evalApply [acc; current] fnOriginOpt env)
                    (state, env)
                    map
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
    
        let evalReduce evalApply fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [_; List (list, _)] ->
                match list with
                | head :: tail -> List.fold (fun (acc, env) current -> evalApply [acc; current] fnOriginOpt env) (head, env) tail
                | _ -> (Violation ([!!"InvalidArgument"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to an empty list.", fnOriginOpt), env)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)
            
        let evalRing fnOriginOpt (_ : string) evaledArgs env =
            (Ring (Set.ofList evaledArgs, fnOriginOpt), env)

        let evalAdd fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring (set, originOpt) -> (Ring (Set.add value set, originOpt), env)
                | Table (map, originOpt) ->
                    match value with
                    | Tuple (map', _) when map'.Count = 2 -> (Table (Map.add (Map.find 0 map') (Map.find 1 map') map, originOpt), env)
                    | _ -> (Violation ([!!"InvalidEntry"; !!"Table"; !!(String.capitalize fnName)], "Table entry must consist of a pair.", fnOriginOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalRemove fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring (set, originOpt) -> (Ring (Set.remove value set, originOpt), env)
                | Table (map, originOpt) -> (Table (Map.remove value map, originOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalTryFind fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table (map, _) -> (Option (Map.tryFind key map, fnOriginOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalFind fnOriginOpt fnName evaledArgs env =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table (map, _) ->
                    match Map.tryFind key map with
                    | Some value -> (value, env)
                    | None -> (Violation ([!!"InvalidKey"; !!"Table"; !!(String.capitalize fnName)], "Key not found in table.", fnOriginOpt), env)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), env)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), env)

        let evalTable fnOriginOpt fnName evaledArgs env =
            if List.forall (function Tuple (map, _) when map.Count = 2 -> true | _ -> false) evaledArgs then
                let evaledPairs = List.map (function List ([evaledFst; evaledSnd], _) -> (evaledFst, evaledSnd) | _ -> failwithumf ()) evaledArgs
                let evaledMap = Map.ofList evaledPairs
                (Table (evaledMap, fnOriginOpt), env)
            else (Violation ([!!"InvalidEntries"; !!"Table"; !!(String.capitalize fnName)], "Table entries must consist of 1 or more pairs.", fnOriginOpt), env)

        let rec Intrinsics =
            dictPlus
                [("!", evalBoolUnary not)
                 ("&", evalBoolBinary (&&))
                 ("|", evalBoolBinary (||))
                 ("=", evalBinary EqFns)
                 ("<>", evalBinary NotEqFns)
                 ("<", evalBinary LtFns)
                 (">", evalBinary GtFns)
                 ("<=", evalBinary LtEqFns)
                 (">=", evalBinary GtEqFns)
                 ("+", evalBinary AddFns)
                 ("-", evalBinary SubFns)
                 ("*", evalBinary MulFns)
                 ("/", evalBinary DivFns)
                 ("%", evalBinary ModFns)
                 ("pow", evalBinary PowFns)
                 ("root", evalBinary RootFns)
                 ("sqr", evalUnary SqrFns)
                 ("sqrt", evalUnary SqrtFns)
                 ("floor", evalUnary FloorFns)
                 ("ceiling", evalUnary CeilingFns)
                 ("truncate", evalUnary TruncateFns)
                 ("round", evalUnary RoundFns)
                 ("exp", evalUnary ExpFns)
                 ("log", evalUnary LogFns)
                 ("sin", evalUnary SinFns)
                 ("cos", evalUnary CosFns)
                 ("tan", evalUnary TanFns)
                 ("asin", evalUnary AsinFns)
                 ("acos", evalUnary AcosFns)
                 ("atan", evalUnary AtanFns)
                 ("length", evalUnary LengthFns)
                 ("normal", evalUnary NormalFns)
                 ("cross", evalBinary CrossFns)
                 ("dot", evalBinary DotFns)
                 ("bool", evalUnary BoolFns)
                 ("int", evalUnary IntFns)
                 ("int64", evalUnary Int64Fns)
                 ("single", evalUnary SingleFns)
                 ("double", evalUnary DoubleFns)
                 ("string", evalUnary StringFns)
                 ("v2", evalV2)
                 //("xOf", evalNOf 0) TODO
                 //("yOf", evalNOf 1) TODO
                 //("xAs", evalNAs 0) TODO
                 //("yAs", evalNas 1) TODO
                 ("pair", evalTuple)
                 ("tuple", evalTuple)
                 ("fst", evalNth5 0)
                 ("snd", evalNth5 1)
                 ("thd", evalNth5 2)
                 ("fth", evalNth5 3)
                 ("fif", evalNth5 4)
                 ("nth", evalNth)
                 ("some", evalSome)
                 ("isNone", evalIsNone)
                 ("isSome", evalIsSome)
                 //("contains", evalContains)
                 ("map", evalMap evalApply)
                 ("list", evalList)
                 ("head", evalHead)
                 ("tail", evalTail)
                 ("cons", evalCons)
                 ("isEmpty", evalIsEmpty)
                 ("notEmpty", evalNotEmpty)
                 //("filter", evalFilter evalApply)
                 ("fold", evalFold evalApply)
                 ("reduce", evalReduce evalApply)
                 ("ring", evalRing)
                 ("add", evalAdd)
                 ("remove", evalRemove)
                 ("table", evalTable)
                 ("tryFind", evalTryFind)
                 ("find", evalFind)
                 ("product", evalProduct)]

        and isIntrinsic name =
            Intrinsics.ContainsKey name

        and evalIntrinsic originOpt name evaledArgs env =
            match Intrinsics.TryGetValue name with
            | (true, intrinsic) -> intrinsic originOpt name evaledArgs env
            | (false, _) -> (Violation ([!!"InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", originOpt), env)

        and evalProduct originOpt name evaledArgs env =
            match evaledArgs with
            | [Stream (streamLeft, originLeftOpt); Stream (streamRight, originRightOpt)] ->
                match evalStream name streamLeft originLeftOpt env with
                | Right (streamLeft, env) ->
                    match evalStream name streamRight originRightOpt env with
                    | Right (streamRight, env) ->
                        let computedStream = Stream.product streamLeft streamRight
                        (Stream (ComputedStream computedStream, originOpt), env)
                    | Left violation -> (violation, env)
                | Left violation -> (violation, env)
            | _ -> (Violation ([!!"InvalidArgumentTypes"; !!(String.capitalize name)], "Incorrect types of arguments for application of '" + name + "'; 1 relation and 1 stream required.", originOpt), env)

        and evalStream name stream originOpt env =
            match stream with
            | VariableStream variableName ->
                let context = Env.getContext env
                let variableAddress = ltoa [!!"Stream"; !!variableName; !!"Event"] ->>- context.ParticipantAddress
                let variableStream = Stream.stream variableAddress
                Right (variableStream, env)
            | EventStream eventAddress ->
                let (eventAddressEvaled, env) = eval eventAddress env
                match eventAddressEvaled with
                | String (eventAddressStr, originOpt)
                | Keyword (eventAddressStr, originOpt) ->
                    try let eventAddress = Address.makeFromString eventAddressStr
                        let eventStream = Stream.stream eventAddress
                        Right (eventStream, env)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address '" + eventAddressStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address. Address must be either a string or a keyword", originOpt))
            | PropertyStream (propertyName, propertyRelation) ->
                let (propertyRelationEvaled, env) = eval propertyRelation env
                match propertyRelationEvaled with
                | String (propertyRelationStr, originOpt)
                | Keyword (propertyRelationStr, originOpt) ->
                    try let context = Env.getContext env
                        let propertyRelation = Relation.makeFromString propertyRelationStr
                        let propertyAddress = Relation.resolve context.SimulantAddress propertyRelation -<<- Address.makeFromName !!propertyName
                        let propertyStream = Stream.stream propertyAddress
                        Right (propertyStream, env)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation '" + propertyRelationStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation. Relation must be either a string or a keyword", originOpt))
            | PropertyStreamMany _ ->
                // TODO: implement
                Left (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt))
            | ComputedStream computedStream ->
                Right (computedStream :?> Prime.Stream<obj, Game, World>, env)

        and evalBinding expr name cachedBinding originOpt env =
            match Env.tryGetBinding name cachedBinding env with
            | None ->
                if isIntrinsic name then (expr, env)
                else (Violation ([!!"NonexistentBinding"], "Non-existent binding '" + name + "' ", originOpt), env)
            | Some binding -> (binding, env)

        and evalApply exprs originOpt env =
            let oldEnv = env
            match evalMany exprs env with
            | (evaledHead :: evaledTail, env) ->
                match evaledHead with
                | Keyword (_, originOpt) as keyword ->
                    let map = evaledTail |> List.indexed |> Map.ofList
                    (Keyphrase (keyword, map, originOpt), oldEnv)
                | Binding (name, _, originOpt) ->
                    let (evaled, _) = evalIntrinsic originOpt name evaledTail env
                    (evaled, oldEnv)
                | Fun (pars, parsCount, body, _, envOpt, originOpt) ->
                    let env =
                        match envOpt with
                        | Some env -> env :?> Env<_, _, _>
                        | None -> env
                    let evaledArgs = evaledTail
                    if List.hasExactly parsCount evaledArgs then
                        let bindings = List.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                        let env = Env.addProceduralBindings (AddToNewFrame parsCount) bindings env
                        let (evaled, _) = eval body env
                        (evaled, oldEnv)
                    else (Violation ([!!"MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), oldEnv)                        
                | _ -> (Violation ([!!"TODO: proper violation category."], "Cannot apply a non-binding.", originOpt), oldEnv)
            | ([], _) -> (Unit originOpt, oldEnv)

        and evalLet4 binding body originOpt env =
            let env =
                match binding with
                | LetVariable (name, body) ->
                    let evaled = evalDropEnv body env
                    Env.addProceduralBinding (AddToNewFrame 1) name evaled env
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (env :> obj), originOpt)
                    Env.addProceduralBinding (AddToNewFrame 1) name fn env
            eval body env

        and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt env =
            let env =
                match bindingsHead with
                | LetVariable (name, body) ->
                    let bodyValue = evalDropEnv body env
                    Env.addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue env
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (env :> obj), originOpt)
                    Env.addProceduralBinding (AddToNewFrame bindingsCount) name fn env
            let mutable start = 1
            for binding in bindingsTail do
                match binding with
                | LetVariable (name, body) ->
                    let bodyValue = evalDropEnv body env
                    Env.addProceduralBinding (AddToHeadFrame start) name bodyValue env |> ignore
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (env :> obj), originOpt)
                    Env.addProceduralBinding (AddToHeadFrame start) name fn env |> ignore
                start <- start + 1
            eval body env
        
        and evalLet binding body originOpt env =
            let (evaled, _) = evalLet4 binding body originOpt env
            (evaled, env)
        
        and evalLetMany bindings body originOpt env =
            match bindings with
            | [] -> (Violation ([!!"MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), env)
            | bindingsHead :: bindingsTail ->
                let bindingsCount = List.length bindingsTail + 1
                let (evaled, _) = evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt env
                (evaled, env)

        and evalFun fn pars parsCount body envPushed envOpt originOpt env =
            if not envPushed then
                match envOpt with
                | Some env as envOpt -> (Fun (pars, parsCount, body, true, envOpt, originOpt), env :?> Env<_, _, _>)
                | None -> (Fun (pars, parsCount, body, true, Some (env :> obj), originOpt), env)
            else (fn, env)

        and evalIf condition consequent alternative originOpt env =
            let oldEnv = env
            let (evaled, env) = eval condition env
            match evaled with
            | Violation _ -> (evaled, oldEnv)
            | Bool (bool, _) -> if bool then eval consequent env else eval alternative env
            | _ -> (Violation ([!!"InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", originOpt), env)

        and evalMatch input (cases : (Expr * Expr) list) originOpt env =
            let (input, env) = eval input env
            let eir =
                List.foldUntilRight (fun env (condition, consequent) ->
                    let (evaledInput, env) = eval condition env
                    match evalBinaryInner EqFns "match" input evaledInput env with
                    | (Violation _, _) -> Right (evaledInput, env)
                    | (Bool (true, _), env) -> Right (eval consequent env)
                    | (Bool (false, _), _) -> Left env
                    | _ -> failwithumf ())
                    (Left env)
                    cases
            match eir with
            | Right (evaled, env) -> (evaled, env)
            | Left env -> (Violation ([!!"InexhaustiveMatch"], "A match expression failed to meet any of its cases.", originOpt), env)

        and evalSelect exprPairs originOpt env =
            let eir =
                List.foldUntilRight (fun env (condition, consequent) ->
                    let (evaled, env) = eval condition env
                    match evaled with
                    | Violation _ -> Right (evaled, env)
                    | Bool (bool, _) -> if bool then Right (eval consequent env) else Left env
                    | _ -> Right ((Violation ([!!"InvalidSelectCondition"], "Must provide an expression that evaluates to a bool in a case condition.", originOpt), env)))
                    (Left env)
                    exprPairs
            match eir with
            | Right (evaled, env) -> (evaled, env)
            | Left env -> (Violation ([!!"InexhaustiveSelect"], "A select expression failed to meet any of its cases.", originOpt), env)

        and evalTry body handlers _ env =
            let oldEnv = env
            let (evaled, env) = eval body env
            match evaled with
            | Violation (categories, _, _) ->
                let eir =
                    List.foldUntilRight (fun env (handlerCategories, handlerBody) ->
                        let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                        if categoriesTrunc = handlerCategories then Right (eval handlerBody env) else Left env)
                        (Left oldEnv)
                        handlers
                match eir with
                | Right (evaled, env) -> (evaled, env)
                | Left env -> (evaled, env)
            | _ -> (evaled, env)

        and evalDo exprs originOpt env =
            let evaledEir =
                List.foldWhileRight (fun (_, env) expr ->
                    let oldEnv = env
                    let (evaled, env) = eval expr env
                    match evaled with
                    | Violation _ as violation -> Left (violation, oldEnv)
                    | _ -> Right (evaled, env))
                    (Right (Unit originOpt, env))
                    exprs
            match evaledEir with
            | Right evaled -> evaled
            | Left error -> error

        and evalBreak expr env =
            // TODO: write all env bindings to console
            Debugger.Break ()
            eval expr env

        and evalGet propertyName relationExprOpt originOpt env =
            let context = Env.getContext env
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    let (evaledExpr, env) = eval relationExpr env
                    match evaledExpr with
                    | String (str, _)
                    | Keyword (str, _) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, env)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt))
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt))
                | None -> Right (context, env)
            match simulantAndEnvEir with
            | Right (simulant, env) ->
                let world = Env.getWorld env
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match Importers.TryGetValue propertyType.Name with
                    | (true, tryImport) ->
                        match tryImport propertyValue propertyType originOpt with
                        | Some propertyValue -> (propertyValue, env)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), env)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), env)
                | None -> (Violation ([!!"InvalidProperty"], "Simulant or property value could not be found.", originOpt), env)
            | Left violation -> (violation, env)

        and evalSet propertyName propertyValueExpr relationExprOpt originOpt env =
            let context = Env.getContext env
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    let (evaledExpr, env) = eval relationExpr env
                    match evaledExpr with
                    | String (str, _)
                    | Keyword (str, _) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, env)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt))
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt))
                | None -> Right (context, env)
            let (propertyValue, env) = eval propertyValueExpr env
            match simulantAndEnvEir with
            | Right (simulant, env) ->
                let world = Env.getWorld env
                // NOTE: this sucks, having to get the property before setting it just to find out its type...
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    match Exporters.TryGetValue propertyType.Name with
                    | (true, tryExport) ->
                        match tryExport propertyValue propertyType with
                        | Some propertyValue ->
                            match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                            | (true, world) ->
                                let env = Env.setWorld World.choose world env
                                (Unit originOpt, env)
                            | (false, _) -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), env)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), env)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), env)
                | None -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), env)
            | Left violation -> (violation, env)

        and evalDefine name expr originOpt env =
            let (evaled, env) = eval expr env
            match Env.tryAddDeclarationBinding name evaled env with
            | Some env -> (Unit originOpt, env)
            | None -> (Violation ([!!"InvalidDefinition"], "Definition '" + name + "' could not be created due to having the same name as another top-level binding.", originOpt), env)

        and evalVariable name stream originOpt env =
            match evalStream name stream originOpt env with
            | Right (stream, env) -> (Unit originOpt, addStream name stream originOpt env)
            | Left violation -> (violation, env)

        and eval expr env : Expr * Env<Simulant, Game, World> =
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
            | Tuple _ -> (expr, env)
            | Keyphrase _ -> (expr, env)
            | Option _ -> (expr, env)
            | List _ -> (expr, env)
            | Ring _ -> (expr, env)
            | Table _ -> (expr, env)
            | Stream _ -> (expr, env)
            | Binding (name, cachedBinding, originOpt) as expr -> evalBinding expr name cachedBinding originOpt env
            | Apply (exprs, originOpt) -> evalApply exprs originOpt env
            | Let (binding, body, originOpt) -> evalLet binding body originOpt env
            | LetMany (bindings, body, originOpt) -> evalLetMany bindings body originOpt env
            | Fun (pars, parsCount, body, envPushed, envOpt, originOpt) as fn -> evalFun fn pars parsCount body envPushed envOpt originOpt env
            | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt env
            | Match (input, cases, originOpt) -> evalMatch input cases originOpt env
            | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt env
            | Try (body, handlers, originOpt) -> evalTry body handlers originOpt env
            | Do (exprs, originOpt) -> evalDo exprs originOpt env
            | Run (_, originOpt) -> (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt), env) // TODO
            | Break (expr, _) -> evalBreak expr env
            | Get (name, originOpt) -> evalGet name None originOpt env
            | GetFrom (name, expr, originOpt) -> evalGet name (Some expr) originOpt env
            | Set (name, expr, originOpt) -> evalSet name expr None originOpt env
            | SetTo (name, expr, expr2, originOpt) -> evalSet name expr2 (Some expr) originOpt env
            | Quote _  -> (expr, env)
            | Define (name, expr, originOpt) -> evalDefine name expr originOpt env
            | Variable (name, stream, originOpt) -> evalVariable name stream originOpt env
            | Equate (_, _, _, _, originOpt) -> (Unit originOpt, env)
            | EquateMany (_, _, _, _, _, originOpt) -> (Unit originOpt, env)
            | Handle (_, _, originOpt) -> (Unit originOpt, env)

        and evalMany exprs env =
            List.fold
                (fun (evaleds, env) expr ->
                    let (evaled, env) = eval expr env
                    (evaled :: evaleds, env))
                ([], env)
                exprs

        and evalDropEnv expr env =
            eval expr env |> fst

        let runExpr exprName expr env =
            match eval expr env with
            | (Violation (names, error, optOrigin) as evaled, env) ->
                Log.debug ^
                    "Unexpected violation:" + (names |> Name.join "" |> Name.getNameStr) +
                    "\nin script " + exprName + " due to:" + error +
                    "\nat: " + scstring optOrigin + "'."
                (evaled, env)
            | (evaled, env) -> (evaled, env)

        let run script env =
            match runExpr "OnUnregister" script.OnUnregister env with
            | (Violation _ as evaled, env) -> ([evaled], env)
            | (evaled, env) ->
                match runExpr "OnRegister" script.OnRegister env with
                | (Violation _ as evaled', env) -> ([evaled; evaled'], env)
                | (evaled', env) -> ([evaled; evaled'], env)

/// An abstract data type for executing scripts.
type ScriptSystem = WorldScriptSystem.ScriptSystem