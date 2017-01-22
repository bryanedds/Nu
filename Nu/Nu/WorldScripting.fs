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
module WorldScripting =

    type World with

        static member tryGetDeclarationBinding name world =
            World.getScriptEnvBy (Scripting.EnvModule.Env.tryGetDeclarationBinding name) world

        static member tryGetProceduralBinding name world =
            World.getScriptEnvBy (Scripting.EnvModule.Env.tryGetProceduralBinding name) world

        static member tryGetBinding name cachedBinding world =
            World.getScriptEnvBy (Scripting.EnvModule.Env.tryGetBinding name cachedBinding) world

        static member tryAddDeclarationBinding name value world =
            World.tryUpdateScriptEnv (Scripting.EnvModule.Env.tryAddDeclarationBinding name value) world

        static member tryAddDeclarationBindings bindings world =
            World.tryUpdateScriptEnv (EnvModule.Env.tryAddDeclarationBindings bindings) world

        static member addProceduralBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBinding appendType name value) world

        static member addProceduralBindings appendType bindings world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBindings appendType bindings) world

    module Scripting =

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

        and private Importers : Dictionary<string, obj -> Type -> SymbolOrigin option -> Expr option> =
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
            { Bool : bool -> SymbolOrigin option -> Expr
              Int : int -> SymbolOrigin option -> Expr
              Int64 : int64 -> SymbolOrigin option -> Expr
              Single : single -> SymbolOrigin option -> Expr
              Double : double -> SymbolOrigin option -> Expr
              Vector2 : Vector2 -> SymbolOrigin option -> Expr
              String : string -> SymbolOrigin option -> Expr
              Tuple : Map<int, Expr> -> SymbolOrigin option -> Expr
              Keyphrase : Map<int, Expr> -> SymbolOrigin option -> Expr
              List : Expr list -> SymbolOrigin option -> Expr
              Ring : Expr Set -> SymbolOrigin option -> Expr
              Table : Map<Expr, Expr> -> SymbolOrigin option -> Expr }

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
            { Bool : bool -> bool -> SymbolOrigin option -> Expr
              Int : int -> int -> SymbolOrigin option -> Expr
              Int64 : int64 -> int64 -> SymbolOrigin option -> Expr
              Single : single -> single -> SymbolOrigin option -> Expr
              Double : double -> double -> SymbolOrigin option -> Expr
              Vector2 : Vector2 -> Vector2 -> SymbolOrigin option -> Expr
              String : string -> string -> SymbolOrigin option -> Expr
              Tuple : Map<int, Expr> -> Map<int, Expr> -> SymbolOrigin option -> Expr
              Keyphrase : Map<int, Expr> -> Map<int, Expr> -> SymbolOrigin option -> Expr
              List : Expr list -> Expr list -> SymbolOrigin option -> Expr
              Ring : Expr Set -> Expr Set -> SymbolOrigin option -> Expr
              Table : Map<Expr, Expr> -> Map<Expr, Expr> -> SymbolOrigin option -> Expr }

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
            | (Some originLeft, Some originRight) -> Some { Source = originLeft.Source; Start = originLeft.Start; Stop = originRight.Stop }
            | (_, _) -> None

        let evalBoolUnary fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled] ->
                match evaled with
                | Bool (bool, originOpt) -> (Bool (fn bool, originOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", Expr.getOriginOpt evaled), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalBoolBinary fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledLeft; evaledRight] ->
                match (evaledLeft, evaledRight) with
                | (Bool (boolLeft, originLeftOpt), Bool (boolRight, originRightOpt)) -> (Bool (fn boolLeft boolRight, combine originLeftOpt originRightOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", combine (Expr.getOriginOpt evaledLeft) (Expr.getOriginOpt evaledRight)), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalUnaryInner (fns : UnaryFns) fnName evaledArg world =
            match evaledArg with
            | Bool (boolValue, originOpt) -> ((fns.Bool boolValue originOpt), world)
            | Int (intValue, originOpt) -> ((fns.Int intValue originOpt), world)
            | Int64 (int64Value, originOpt) -> ((fns.Int64 int64Value originOpt), world)
            | Single (singleValue, originOpt) -> ((fns.Single singleValue originOpt), world)
            | Double (doubleValue, originOpt) -> ((fns.Double doubleValue originOpt), world)
            | Vector2 (vector2Value, originOpt) -> ((fns.Vector2 vector2Value originOpt), world)
            | String (stringValue, originOpt) -> ((fns.String stringValue originOpt), world)
            | Tuple (tupleValue, originOpt) -> ((fns.Tuple tupleValue originOpt), world)
            | Keyphrase (_, phraseValue, originOpt) -> ((fns.Keyphrase phraseValue originOpt), world)
            | List (listValue, originOpt) -> ((fns.List listValue originOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", Expr.getOriginOpt evaledArg), world)

        let evalUnary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> evalUnaryInner fns fnName evaledArg world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalBinaryInner (fns : BinaryFns) fnName evaledLeft evaledRight world =
            match (evaledLeft, evaledRight) with
            | (Bool (boolLeft, originLeftOpt), Bool (boolRight, originRightOpt)) -> ((fns.Bool boolLeft boolRight (combine originLeftOpt originRightOpt)), world)
            | (Int (intLeft, originLeftOpt), Int (intRight, originRightOpt)) -> ((fns.Int intLeft intRight (combine originLeftOpt originRightOpt)), world)
            | (Int64 (int64Left, originLeftOpt), Int64 (int64Right, originRightOpt)) -> ((fns.Int64 int64Left int64Right (combine originLeftOpt originRightOpt)), world)
            | (Single (singleLeft, originLeftOpt), Single (singleRight, originRightOpt)) -> ((fns.Single singleLeft singleRight (combine originLeftOpt originRightOpt)), world)
            | (Double (doubleLeft, originLeftOpt), Double (doubleRight, originRightOpt)) -> ((fns.Double doubleLeft doubleRight (combine originLeftOpt originRightOpt)), world)
            | (Vector2 (vector2Left, originLeftOpt), Vector2 (vector2Right, originRightOpt)) -> ((fns.Vector2 vector2Left vector2Right (combine originLeftOpt originRightOpt)), world)
            | (String (stringLeft, originLeftOpt), String (stringRight, originRightOpt)) -> ((fns.String stringLeft stringRight (combine originLeftOpt originRightOpt)), world)
            | (Tuple (tupleLeft, originLeftOpt), Tuple (tupleRight, originRightOpt)) -> ((fns.Tuple tupleLeft tupleRight (combine originLeftOpt originRightOpt)), world)
            | (Keyphrase (_, phraseLeft, originLeftOpt), Keyphrase (_, phraseRight, originRightOpt)) -> ((fns.Keyphrase phraseLeft phraseRight (combine originLeftOpt originRightOpt)), world)
            | (List (listLeft, originLeftOpt), List (listRight, originRightOpt)) -> ((fns.List listLeft listRight (combine originLeftOpt originRightOpt)), world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", combine (Expr.getOriginOpt evaledLeft) (Expr.getOriginOpt evaledRight)), world)

        let evalBinary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledLeft; evaledRight] -> evalBinaryInner fns fnName evaledLeft evaledRight world                
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)
        
        let evalV2 fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Single (x, _); Single (y, _)] -> (Vector2 (OpenTK.Vector2 (x, y), fnOriginOpt), world)
            | [Violation _ as violation; _] -> (violation, world)
            | [_; Violation _ as violation] -> (violation, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"V2"; !!(String.capitalize fnName)], "Application of " + fnName + " requires a single for the both arguments.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"V2"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalPair fnOriginOpt (_ : string) evaledArgs world =
            match evaledArgs with
            | [_; _] -> (Tuple (evaledArgs |> List.indexed |> Map.ofList, fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Pair"], "Incorrect number of arguments for creation of a pair; 2 arguments required.", fnOriginOpt), world)

        let evalTuple fnOriginOpt _ evaledArgs world =
            (Tuple (evaledArgs |> List.indexed |> Map.ofList, fnOriginOpt), world)
    
        let evalNth5 index fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Tuple (evaleds, originOpt)] ->
                match Map.tryFind index evaleds with
                | Some evaled -> (evaled, world)
                | None -> (Violation ([!!"OutOfRange"], "Tuple does not contain element at index " + string index + ".", originOpt), world)
            | [Keyphrase (_, evaleds, originOpt)] ->
                match Map.tryFind index evaleds with
                | Some evaled -> (evaled, world)
                | None -> (Violation ([!!"OutOfRange"], "Keyphrase does not contain element at index " + string index + ".", originOpt), world)
            | [List (evaleds, originOpt)] ->
                match List.tryFindAt index evaleds with
                | Some evaled -> (evaled, world)
                | None -> (Violation ([!!"OutOfRange"], "List does not contain element at index " + string index + ".", originOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-sequence.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
        
        let evalNth fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [head; foot] ->
                match head with
                | Int (int, _) -> evalNth5 int fnOriginOpt fnName [foot] world
                | _ -> (Violation ([!!"InvalidNthArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Application of " + fnName + " requires an int for the first argument.", fnOriginOpt), world)
            | _ ->  (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)
            
        let evalSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> (Option (Some evaledArg, fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsNone fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option (evaled, originOpt)] -> (Bool (Option.isNone evaled, originOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option (evaled, originOpt)] -> (Bool (Option.isSome evaled, originOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalMap evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; Option (opt, _) as option] ->
                match opt with
                | Some value -> evalApply [value] fnOriginOpt world
                | None -> (option, world)
            | [_; List (list, _)] ->
                let (list, world) =
                    List.fold (fun (elems, world) current ->
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list, fnOriginOpt), world)
            | [_; Ring (set, _)] ->
                let (set, world) =
                    Set.fold (fun (elems, world) current ->
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring (set, fnOriginOpt), world)
            | [_; Table (map, _)] ->
                let (map, world) =
                    Map.fold (fun (elems, world) currentKey currentValue ->
                        let current = Tuple ([currentKey; currentValue] |> List.indexed |> Map.ofList, fnOriginOpt)
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        match elem with
                        | Tuple (elems', _) when elems'.Count = 2 -> ((Map.add (Map.find 0 elems') (Map.find 1 elems') elems), world)
                        | _ -> (elems, world))
                        (Map.empty, world)
                        map
                (Table (map, fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Functor"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-functor.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Functor"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
            
        let evalList fnOriginOpt _ evaledArgs world =
            (List (evaledArgs, fnOriginOpt), world)
    
        let evalHead fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List (evaledArgs, _)] ->
                match evaledArgs with
                | evaledHead :: _ -> (evaledHead, world)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalTail fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List (evaleds, originOpt)] ->
                match evaleds with
                | _ :: evaledTail -> (List (evaledTail, originOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalCons fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; List (evaleds, originOpt)] -> (List (evaled :: evaleds, originOpt), world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsEmpty fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List (list, originOpt)] -> (Bool (List.isEmpty list, originOpt), world)
            | [Ring (set, originOpt)] -> (Bool (Set.isEmpty set, originOpt), world)
            | [Table (map, originOpt)] -> (Bool (Map.isEmpty map, originOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalNotEmpty fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List (evaleds, originOpt)] -> (Bool (not ^ List.isEmpty evaleds, originOpt), world)
            | [Ring (set, originOpt)] -> (Bool (not ^ Set.isEmpty set, originOpt), world)
            | [Table (map, originOpt)] -> (Bool (not ^ Map.isEmpty map, originOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalFold evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; state; List (list, _)] -> List.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (state, world) list
            | [_; state; Ring (set, _)] -> Set.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (state, world) set
            | [_; state; Table (map, _)] ->
                Map.fold (fun (acc, world) currentKey currentValue ->
                    let current = Tuple ([currentKey; currentValue] |> List.indexed |> Map.ofList, fnOriginOpt)
                    evalApply [acc; current] fnOriginOpt world)
                    (state, world)
                    map
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalReduce evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; List (list, _)] ->
                match list with
                | head :: tail -> List.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (head, world) tail
                | _ -> (Violation ([!!"InvalidArgument"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to an empty list.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
            
        let evalRing fnOriginOpt (_ : string) evaledArgs world =
            (Ring (Set.ofList evaledArgs, fnOriginOpt), world)

        let evalAdd fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring (set, originOpt) -> (Ring (Set.add value set, originOpt), world)
                | Table (map, originOpt) ->
                    match value with
                    | Tuple (map', _) when map'.Count = 2 -> (Table (Map.add (Map.find 0 map') (Map.find 1 map') map, originOpt), world)
                    | _ -> (Violation ([!!"InvalidEntry"; !!"Table"; !!(String.capitalize fnName)], "Table entry must consist of a pair.", fnOriginOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalRemove fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring (set, originOpt) -> (Ring (Set.remove value set, originOpt), world)
                | Table (map, originOpt) -> (Table (Map.remove value map, originOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTryFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table (map, _) -> (Option (Map.tryFind key map, fnOriginOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table (map, _) ->
                    match Map.tryFind key map with
                    | Some value -> (value, world)
                    | None -> (Violation ([!!"InvalidKey"; !!"Table"; !!(String.capitalize fnName)], "Key not found in table.", fnOriginOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTable fnOriginOpt fnName evaledArgs world =
            if List.forall (function Tuple (map, _) when map.Count = 2 -> true | _ -> false) evaledArgs then
                let evaledPairs = List.map (function List ([evaledFst; evaledSnd], _) -> (evaledFst, evaledSnd) | _ -> failwithumf ()) evaledArgs
                let evaledMap = Map.ofList evaledPairs
                (Table (evaledMap, fnOriginOpt), world)
            else (Violation ([!!"InvalidEntries"; !!"Table"; !!(String.capitalize fnName)], "Table entries must consist of 1 or more pairs.", fnOriginOpt), world)

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
                 //("contains", evalContains) TODO
                 ("map", evalMap evalApply)
                 ("list", evalList)
                 ("head", evalHead)
                 ("tail", evalTail)
                 ("cons", evalCons)
                 ("isEmpty", evalIsEmpty)
                 ("notEmpty", evalNotEmpty)
                 //("filter", evalFilter evalApply) TODO
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

        and evalIntrinsic originOpt name evaledArgs world =
            match Intrinsics.TryGetValue name with
            | (true, intrinsic) -> intrinsic originOpt name evaledArgs world
            | (false, _) -> (Violation ([!!"InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", originOpt), world)

        and evalProduct originOpt name evaledArgs world =
            match evaledArgs with
            | [Stream (streamLeft, originLeftOpt); Stream (streamRight, originRightOpt)] ->
                match evalStream name streamLeft originLeftOpt world with
                | Right (streamLeft, world) ->
                    match evalStream name streamRight originRightOpt world with
                    | Right (streamRight, world) ->
                        let computedStream = Stream.product streamLeft streamRight
                        (Stream (ComputedStream computedStream, originOpt), world)
                    | Left violation -> (violation, world)
                | Left violation -> (violation, world)
            | _ -> (Violation ([!!"InvalidArgumentTypes"; !!(String.capitalize name)], "Incorrect types of arguments for application of '" + name + "'; 1 relation and 1 stream required.", originOpt), world)

        and evalStream name stream originOpt world =
            match stream with
            | VariableStream variableName ->
                let context = World.getScriptContext world
                let variableAddress = ltoa [!!"Stream"; !!variableName; !!"Event"] ->>- context.SimulantAddress
                let variableStream = Stream.stream variableAddress
                Right (variableStream, world)
            | EventStream eventAddress ->
                let (eventAddressEvaled, world) = eval eventAddress world
                match eventAddressEvaled with
                | String (eventAddressStr, originOpt)
                | Keyword (eventAddressStr, originOpt) ->
                    try let eventAddress = Address.makeFromString eventAddressStr
                        let eventStream = Stream.stream eventAddress
                        Right (eventStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address '" + eventAddressStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address. Address must be either a string or a keyword", originOpt))
            | PropertyStream (propertyName, propertyRelation) ->
                let (propertyRelationEvaled, world) = eval propertyRelation world
                match propertyRelationEvaled with
                | String (propertyRelationStr, originOpt)
                | Keyword (propertyRelationStr, originOpt) ->
                    try let context = World.getScriptContext world
                        let propertyRelation = Relation.makeFromString propertyRelationStr
                        let propertyAddress = Relation.resolve context.SimulantAddress propertyRelation -<<- Address.makeFromName !!propertyName
                        let propertyStream = Stream.stream propertyAddress
                        Right (propertyStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation '" + propertyRelationStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation. Relation must be either a string or a keyword", originOpt))
            | PropertyStreamMany _ ->
                // TODO: implement
                Left (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt))
            | ComputedStream computedStream ->
                Right (computedStream :?> Prime.Stream<obj, Game, World>, world)

        and evalBinding expr name cachedBinding originOpt world =
            match World.tryGetBinding name cachedBinding world with
            | None ->
                if isIntrinsic name then (expr, world)
                else (Violation ([!!"NonexistentBinding"], "Non-existent binding '" + name + "' ", originOpt), world)
            | Some binding -> (binding, world)

        and evalApply exprs originOpt world =
            let oldWorld = world
            match evalMany exprs world with
            | (evaledHead :: evaledTail, world) ->
                match evaledHead with
                | Keyword (_, originOpt) as keyword ->
                    let map = evaledTail |> List.indexed |> Map.ofList
                    (Keyphrase (keyword, map, originOpt), oldWorld)
                | Binding (name, _, originOpt) ->
                    let (evaled, _) = evalIntrinsic originOpt name evaledTail world
                    (evaled, oldWorld)
                | Fun (pars, parsCount, body, _, worldOpt, originOpt) ->
                    let world =
                        match worldOpt with
                        | Some world -> world :?> World
                        | None -> world
                    let evaledArgs = evaledTail
                    if List.hasExactly parsCount evaledArgs then
                        let bindings = List.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                        let world = World.addProceduralBindings (AddToNewFrame parsCount) bindings world
                        let (evaled, _) = eval body world
                        (evaled, oldWorld)
                    else (Violation ([!!"MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), oldWorld)                        
                | _ -> (Violation ([!!"TODO: proper violation category."], "Cannot apply a non-binding.", originOpt), oldWorld)
            | ([], _) -> (Unit originOpt, oldWorld)

        and evalLet4 binding body originOpt world =
            let world =
                match binding with
                | LetVariable (name, body) ->
                    let evaled = evalDropEnv body world
                    World.addProceduralBinding (AddToNewFrame 1) name evaled world
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (world :> obj), originOpt)
                    World.addProceduralBinding (AddToNewFrame 1) name fn world
            eval body world

        and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
            let world =
                match bindingsHead with
                | LetVariable (name, body) ->
                    let bodyValue = evalDropEnv body world
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (world :> obj), originOpt)
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name fn world
            let mutable start = 1
            for binding in bindingsTail do
                match binding with
                | LetVariable (name, body) ->
                    let bodyValue = evalDropEnv body world
                    World.addProceduralBinding (AddToHeadFrame start) name bodyValue world |> ignore
                | LetFunction (name, args, body) ->
                    let fn = Fun (args, List.length args, body, true, Some (world :> obj), originOpt)
                    World.addProceduralBinding (AddToHeadFrame start) name fn world |> ignore
                start <- start + 1
            eval body world
        
        and evalLet binding body originOpt world =
            let (evaled, _) = evalLet4 binding body originOpt world
            (evaled, world)
        
        and evalLetMany bindings body originOpt world =
            match bindings with
            | [] -> (Violation ([!!"MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)
            | bindingsHead :: bindingsTail ->
                let bindingsCount = List.length bindingsTail + 1
                let (evaled, _) = evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world
                (evaled, world)

        and evalFun fn pars parsCount body worldPushed worldOpt originOpt world =
            if not worldPushed then
                match worldOpt with
                | Some world as worldOpt -> (Fun (pars, parsCount, body, true, worldOpt, originOpt), world :?> World)
                | None -> (Fun (pars, parsCount, body, true, Some (world :> obj), originOpt), world)
            else (fn, world)

        and evalIf condition consequent alternative originOpt world =
            let oldWorld = world
            let (evaled, world) = eval condition world
            match evaled with
            | Violation _ -> (evaled, oldWorld)
            | Bool (bool, _) -> if bool then eval consequent world else eval alternative world
            | _ -> (Violation ([!!"InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", originOpt), world)

        and evalMatch input (cases : (Expr * Expr) list) originOpt world =
            let (input, world) = eval input world
            let eir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    let (evaledInput, world) = eval condition world
                    match evalBinaryInner EqFns "match" input evaledInput world with
                    | (Violation _, _) -> Right (evaledInput, world)
                    | (Bool (true, _), world) -> Right (eval consequent world)
                    | (Bool (false, _), _) -> Left world
                    | _ -> failwithumf ())
                    (Left world)
                    cases
            match eir with
            | Right (evaled, world) -> (evaled, world)
            | Left world -> (Violation ([!!"InexhaustiveMatch"], "A match expression failed to meet any of its cases.", originOpt), world)

        and evalSelect exprPairs originOpt world =
            let eir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    let (evaled, world) = eval condition world
                    match evaled with
                    | Violation _ -> Right (evaled, world)
                    | Bool (bool, _) -> if bool then Right (eval consequent world) else Left world
                    | _ -> Right ((Violation ([!!"InvalidSelectCondition"], "Must provide an expression that evaluates to a bool in a case condition.", originOpt), world)))
                    (Left world)
                    exprPairs
            match eir with
            | Right (evaled, world) -> (evaled, world)
            | Left world -> (Violation ([!!"InexhaustiveSelect"], "A select expression failed to meet any of its cases.", originOpt), world)

        and evalTry body handlers _ world =
            let oldWorld = world
            let (evaled, world) = eval body world
            match evaled with
            | Violation (categories, _, _) ->
                let eir =
                    List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                        let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                        if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                        (Left oldWorld)
                        handlers
                match eir with
                | Right (evaled, world) -> (evaled, world)
                | Left world -> (evaled, world)
            | _ -> (evaled, world)

        and evalDo exprs originOpt world =
            let evaledEir =
                List.foldWhileRight (fun (_, world) expr ->
                    let oldWorld = world
                    let (evaled, world) = eval expr world
                    match evaled with
                    | Violation _ as violation -> Left (violation, oldWorld)
                    | _ -> Right (evaled, world))
                    (Right (Unit originOpt, world))
                    exprs
            match evaledEir with
            | Right evaled -> evaled
            | Left error -> error

        and evalBreak expr world =
            // TODO: write all world bindings to console
            Debugger.Break ()
            eval expr world

        and evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    let (evaledExpr, world) = eval relationExpr world
                    match evaledExpr with
                    | String (str, _)
                    | Keyword (str, _) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt))
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt))
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match Importers.TryGetValue propertyType.Name with
                    | (true, tryImport) ->
                        match tryImport propertyValue propertyType originOpt with
                        | Some propertyValue -> (propertyValue, world)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting worldironment.", originOpt), world)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting worldironment.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
            | Left violation -> (violation, world)

        and evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    let (evaledExpr, world) = eval relationExpr world
                    match evaledExpr with
                    | String (str, _)
                    | Keyword (str, _) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt))
                    | _ -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt))
                | None -> Right (context, world)
            let (propertyValue, world) = eval propertyValueExpr world
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                // NOTE: this sucks, having to get the property before setting it just to find out its type...
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    match Exporters.TryGetValue propertyType.Name with
                    | (true, tryExport) ->
                        match tryExport propertyValue propertyType with
                        | Some propertyValue ->
                            match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                            | (true, world) -> (Unit originOpt, world)
                            | (false, _) -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left violation -> (violation, world)

        and evalDefine name expr originOpt world =
            let (evaled, world) = eval expr world
            match World.tryAddDeclarationBinding name evaled world with
            | Some world -> (Unit originOpt, world)
            | None -> (Violation ([!!"InvalidDefinition"], "Definition '" + name + "' could not be created due to having the same name as another top-level binding.", originOpt), world)

        and eval expr world : Expr * World =
            match expr with
            | Violation _ -> (expr, world)
            | Unit _ -> (expr, world)
            | Bool _ -> (expr, world)
            | Int _ -> (expr, world)
            | Int64 _ -> (expr, world)
            | Single _ -> (expr, world)
            | Double _ -> (expr, world)
            | Vector2 _ -> (expr, world)
            | String _ -> (expr, world)
            | Keyword _ -> (expr, world)
            | Tuple _ -> (expr, world)
            | Keyphrase _ -> (expr, world)
            | Option _ -> (expr, world)
            | List _ -> (expr, world)
            | Ring _ -> (expr, world)
            | Table _ -> (expr, world)
            | Stream _ -> (expr, world)
            | Binding (name, cachedBinding, originOpt) as expr -> evalBinding expr name cachedBinding originOpt world
            | Apply (exprs, originOpt) -> evalApply exprs originOpt world
            | Let (binding, body, originOpt) -> evalLet binding body originOpt world
            | LetMany (bindings, body, originOpt) -> evalLetMany bindings body originOpt world
            | Fun (pars, parsCount, body, worldPushed, worldOpt, originOpt) as fn -> evalFun fn pars parsCount body worldPushed worldOpt originOpt world
            | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt world
            | Match (input, cases, originOpt) -> evalMatch input cases originOpt world
            | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt world
            | Try (body, handlers, originOpt) -> evalTry body handlers originOpt world
            | Do (exprs, originOpt) -> evalDo exprs originOpt world
            | Run (_, originOpt) -> (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt), world) // TODO
            | Break (expr, _) -> evalBreak expr world
            | Get (name, originOpt) -> evalGet name None originOpt world
            | GetFrom (name, expr, originOpt) -> evalGet name (Some expr) originOpt world
            | Set (name, expr, originOpt) -> evalSet name None expr originOpt world
            | SetTo (name, expr, expr2, originOpt) -> evalSet name (Some expr) expr2 originOpt world
            | Quote _  -> (expr, world)
            | Define (name, expr, originOpt) -> evalDefine name expr originOpt world

        and evalMany exprs world =
            let (evaledsRev, world) =
                List.fold
                    (fun (evaleds, world) expr ->
                        let (evaled, world) = eval expr world
                        (evaled :: evaleds, world))
                    ([], world)
                    exprs
            (List.rev evaledsRev, world)

        and evalDropEnv expr world =
            eval expr world |> fst

        let evalLogging expr world =
            match eval expr world with
            | (Violation (names, error, optOrigin) as evaled, world) ->
                Log.debug ^
                    "Unexpected violation:" + (names |> Name.join "" |> Name.getNameStr) +
                    "\ndue to:" + error +
                    "\nat: " + scstring optOrigin + "'."
                (evaled, world)
            | (evaled, world) -> (evaled, world)