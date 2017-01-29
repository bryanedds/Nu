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

        static member private getLocalDeclaration world =
            World.getScriptEnvBy Scripting.EnvModule.Env.getLocalDeclaration world

        static member private setLocalDeclaration localDeclaration world =
            World.updateScriptEnv (Scripting.EnvModule.Env.setLocalDeclaration localDeclaration) world

        static member private tryGetBinding name cachedBinding world =
            World.getScriptEnvBy (Scripting.EnvModule.Env.tryGetBinding name cachedBinding) world

        static member private addDeclarationBinding name value world =
            World.updateScriptEnv (Scripting.EnvModule.Env.addDeclarationBinding name value) world

        static member private addProceduralBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBinding appendType name value) world

        static member private addProceduralBindings appendType bindings world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBindings appendType bindings) world

        static member private addBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addBinding appendType name value) world

        static member private removeProceduralBindings world =
            World.updateScriptEnv EnvModule.Env.removeProceduralBindings world

        static member private getProceduralFrames world =
            World.getScriptEnvBy Scripting.EnvModule.Env.getProceduralFrames world

        static member private setProceduralFrames proceduralFrames world =
            World.updateScriptEnv (Scripting.EnvModule.Env.setProceduralFrames proceduralFrames) world

        static member internal getLocalFrame world =
            World.getScriptEnvBy Scripting.EnvModule.Env.getLocalFrame world

        static member internal setLocalFrame localFrame world =
            World.updateScriptEnv (Scripting.EnvModule.Env.setLocalFrame localFrame) world

    module Scripting =

        let rec private tryImport value (ty : Type) =
            let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
            match Importers.TryGetValue ty.Name with
            | (true, tryImportFn) -> tryImportFn value ty
            | (false, _) -> None

        and private tryImportList (value : obj) (ty : Type) =
            try let garg = (ty.GetGenericArguments ()).[0]
                let objList = Reflection.objToObjList value
                let evaledListOpts = List.map (fun item -> tryImport item garg) objList
                match List.definitizePlus evaledListOpts with
                | (true, evaledList) -> Some (List evaledList)
                | (false, _) -> None
            with _ -> None

        and private Importers : Dictionary<string, obj -> Type -> Expr option> =
            [(typeof<Expr>.Name, (fun (value : obj) _ -> Some (value :?> Expr)))
             (typeof<bool>.Name, (fun (value : obj) _ -> match value with :? bool as bool -> Some (Bool bool) | _ -> None))
             (typeof<int>.Name, (fun (value : obj) _ -> match value with :? int as int -> Some (Int int) | _ -> None))
             (typeof<int64>.Name, (fun (value : obj) _ -> match value with :? int64 as int64 -> Some (Int64 int64) | _ -> None))
             (typeof<single>.Name, (fun (value : obj) _ -> match value with :? single as single -> Some (Single single) | _ -> None))
             (typeof<double>.Name, (fun (value : obj) _ -> match value with :? double as double -> Some (Double double) | _ -> None))
             (typeof<Vector2>.Name, (fun (value : obj) _ -> match value with :? Vector2 as vector2 -> Some (Vector2 vector2) | _ -> None))
             (typedefof<_ list>.Name, (fun value ty -> tryImportList value ty))] |>
             // TODO: remaining importers
            dictPlus

        and private tryImportEventData evt originOpt =
            match tryImport evt.Data evt.DataType with
            | Some data -> data
            | None -> Violation ([!!"InvalidStreamValue"], "Stream value could not be imported into scripting environment.", originOpt)

        let rec private tryExportList (evaled : Expr) (ty : Type) =
            match evaled with
            | List evaleds ->
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
            [(typeof<bool>.Name, (fun evaled _ -> match evaled with Bool value -> value :> obj |> Some | _ -> None))
             (typeof<int>.Name, (fun evaled _ -> match evaled with Int value -> value :> obj |> Some | _ -> None))
             (typeof<int64>.Name, (fun evaled _ -> match evaled with Int64 value -> value :> obj |> Some | _ -> None))
             (typeof<single>.Name, (fun evaled _ -> match evaled with Single value -> value :> obj |> Some | _ -> None))
             (typeof<double>.Name, (fun evaled _ -> match evaled with Double value -> value :> obj |> Some | _ -> None))
             (typeof<Vector2>.Name, (fun evaled _ -> match evaled with Vector2 value -> value :> obj |> Some | _ -> None))
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
              Keyword : string -> SymbolOrigin option -> Expr
              Tuple : Expr array -> SymbolOrigin option -> Expr
              Keyphrase : Expr -> Expr array -> SymbolOrigin option -> Expr
              List : Expr list -> SymbolOrigin option -> Expr
              Ring : Expr Set -> SymbolOrigin option -> Expr
              Table : Map<Expr, Expr> -> SymbolOrigin option -> Expr }

        let ToZeroFns =
            { Bool = fun _ _ -> Bool false
              Int = fun _ _ -> Int 0
              Int64 = fun _ _ -> Int64 0L
              Single = fun _ _ -> Single 0.0f
              Double = fun _ _ -> Double 0.0
              Vector2 = fun _ _ -> Vector2 OpenTK.Vector2.Zero
              String = fun _ _ -> String String.Empty
              Keyword = fun _ _ -> Keyword String.Empty
              Tuple = fun _ _ -> Tuple Array.empty
              Keyphrase = fun _ _ _ -> Keyphrase (Keyword String.Empty, Array.empty)
              List = fun _ _ -> List []
              Ring = fun _ _ -> Ring Set.empty
              Table = fun _ _ -> Table Map.empty }

        let ToIdFns =
            { Bool = fun _ _ -> Bool true
              Int = fun _ _ -> Int 1
              Int64 = fun _ _ -> Int64 1L
              Single = fun _ _ -> Single 1.0f
              Double = fun _ _ -> Double 1.0
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 1.0f)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a string to an identity representation.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a keyword to an identity representation.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a tuple to an identity representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a keyphrase to an identity representation.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a list to an identity representation.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a ring to an identity representation.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToIdentity"], "Cannot convert a table to an identity representation.", originOpt) }

        let ToMinFns =
            { Bool = fun _ _ -> Bool false
              Int = fun _ _ -> Int Int32.MinValue
              Int64 = fun _ _ -> Int64 Int64.MinValue
              Single = fun _ _ -> Single Single.MinValue
              Double = fun _ _ -> Double Double.MinValue
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 Single.MinValue)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a string to a minimum representation.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a keyword to a minimum representation.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a tuple to a minimum representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a keyphrase to a minimum representation.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a list to a minimum representation.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a ring to a minimum representation.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMin"], "Cannot convert a table to a minimum representation.", originOpt) }

        let ToMaxFns =
            { Bool = fun _ _ -> Bool true
              Int = fun _ _ -> Int Int32.MaxValue
              Int64 = fun _ _ -> Int64 Int64.MaxValue
              Single = fun _ _ -> Single Single.MaxValue
              Double = fun _ _ -> Double Double.MaxValue
              Vector2 = fun _ _ -> Vector2 (OpenTK.Vector2 Single.MaxValue)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a string to a maximum representation.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a keyword to a maximum representation.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a tuple to a maximum representation.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a keyphrase to a maximum representation.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a list to a maximum representation.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a ring to a maximum representation.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"ToMax"], "Cannot convert a table to a maximum representation.", originOpt) }

        let IncFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (inc value)
              Int64 = fun value _ -> Int64 (inc value)
              Single = fun value _ -> Single (inc value)
              Double = fun value _ -> Double (inc value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (inc value.X, inc value.Y))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Inc"], "Cannot increment a table.", originOpt) }

        let DecFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (dec value)
              Int64 = fun value _ -> Int64 (dec value)
              Single = fun value _ -> Single (dec value)
              Double = fun value _ -> Double (dec value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (dec value.X, dec value.Y))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Dec"], "Cannot decrement a table.", originOpt) }

        let NegFns =
            { Bool = fun value _ -> Bool (if value then false else true)
              Int = fun value _ -> Int (0 - value)
              Int64 = fun value _ -> Int64 (0L - value)
              Single = fun value _ -> Single (0.0f - value)
              Double = fun value _ -> Double (0.0 - value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2.Zero - value)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Negate"], "Cannot negate a table.", originOpt) }

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
              List = fun value _ -> Int (hash value)
              Ring = fun value _ -> Int (hash value)
              Table = fun value _ -> Int (hash value) }

        let SqrFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a bool.", originOpt)
              Int = fun value _ -> Int (value * value)
              Int64 = fun value _ -> Int64 (value * value)
              Single = fun value _ -> Single (value * value)
              Double = fun value _ -> Double (value * value)
              Vector2 = fun value _ -> Vector2 (Vector2.Multiply (value, value))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqr"], "Cannot square a table.", originOpt) }

        let SqrtFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Sqrt (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Sqrt (double value))
              Single = fun value _ -> Single (single ^ Math.Sqrt (double value))
              Double = fun value _ -> Double (Math.Sqrt value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Sqrt (double value.X), single ^ Math.Sqrt (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqrt"], "Cannot square root a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sqtr"], "Cannot square root a table.", originOpt) }

        let FloorFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Floor (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Floor (double value))
              Single = fun value _ -> Single (single ^ Math.Floor (double value))
              Double = fun value _ -> Double (Math.Floor value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Floor (double value.X), single ^ Math.Floor (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Floor"], "Cannot floor a table.", originOpt) }

        let CeilingFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Ceiling (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Ceiling (double value))
              Single = fun value _ -> Single (single ^ Math.Ceiling (double value))
              Double = fun value _ -> Double (Math.Ceiling value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Ceiling (double value.X), single ^ Math.Ceiling (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Ceiling"], "Cannot ceiling a table.", originOpt) }

        let TruncateFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Truncate (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Truncate (double value))
              Single = fun value _ -> Single (single ^ Math.Truncate (double value))
              Double = fun value _ -> Double (Math.Truncate value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Truncate (double value.X), single ^ Math.Truncate (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Truncate"], "Cannot truncate a table.", originOpt) }

        let ExpFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Exp (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Exp (double value))
              Single = fun value _ -> Single (single ^ Math.Exp (double value))
              Double = fun value _ -> Double (Math.Exp value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Exp (double value.X), single ^ Math.Exp (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Exp"], "Cannot exponentiate a table.", originOpt) }

        let RoundFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Round (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Round (double value))
              Single = fun value _ -> Single (single ^ Math.Round (double value))
              Double = fun value _ -> Double (Math.Round value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Round (double value.X), single ^ Math.Round (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Round"], "Cannot round a table.", originOpt) }

        let LogFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero int.", originOpt) else Int (int ^ Math.Log (double value))
              Int64 = fun value originOpt -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero 64-bit int.", originOpt) else Int64 (int64 ^ Math.Log (double value))
              Single = fun value originOpt -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero single.", originOpt) else Single (single ^ Math.Log (double value))
              Double = fun value originOpt -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a zero double.", originOpt) else Double (Math.Log value)
              Vector2 = fun value originOpt -> if value.X = 0.0f || value.Y = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Log"], "Cannot log a vector containing a zero member.", originOpt) else Vector2 (OpenTK.Vector2 (single ^ Math.Log (double value.X), single ^ Math.Log (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Log"], "Cannot log a table.", originOpt) }

        let SinFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Sin (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Sin (double value))
              Single = fun value _ -> Single (single ^ Math.Sin (double value))
              Double = fun value _ -> Double (Math.Sin value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Sin (double value.X), single ^ Math.Sin (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Sin"], "Cannot sin a table.", originOpt) }

        let CosFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Cos (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Cos (double value))
              Single = fun value _ -> Single (single ^ Math.Cos (double value))
              Double = fun value _ -> Double (Math.Cos value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Cos (double value.X), single ^ Math.Cos (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Cos"], "Cannot cos a table.", originOpt) }

        let TanFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Tan (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Tan (double value))
              Single = fun value _ -> Single (single ^ Math.Tan (double value))
              Double = fun value _ -> Double (Math.Tan value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Tan (double value.X), single ^ Math.Tan (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Tan"], "Cannot tan a table.", originOpt) }

        let AsinFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Asin (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Asin (double value))
              Single = fun value _ -> Single (single ^ Math.Asin (double value))
              Double = fun value _ -> Double (Math.Asin value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Asin (double value.X), single ^ Math.Asin (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Asin"], "Cannot asin a table.", originOpt) }

        let AcosFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Acos (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Acos (double value))
              Single = fun value _ -> Single (single ^ Math.Acos (double value))
              Double = fun value _ -> Double (Math.Acos value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Acos (double value.X), single ^ Math.Acos (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Acos"], "Cannot acos a table.", originOpt) }

        let AtanFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a bool.", originOpt)
              Int = fun value _ -> Int (int ^ Math.Atan (double value))
              Int64 = fun value _ -> Int64 (int64 ^ Math.Atan (double value))
              Single = fun value _ -> Single (single ^ Math.Atan (double value))
              Double = fun value _ -> Double (Math.Atan value)
              Vector2 = fun value _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Atan (double value.X), single ^ Math.Atan (double value.Y)))
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Atan"], "Cannot atan a table.", originOpt) }

        let LengthFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"length"], "Cannot get length of a bool.", originOpt)
              Int = fun value _ -> Int (Math.Abs value)
              Int64 = fun value _ -> Int64 (Math.Abs value)
              Single = fun value _ -> Single (Math.Abs value)
              Double = fun value _ -> Double (Math.Abs value)
              Vector2 = fun value _ -> Single (value.Length)
              String = fun value _ -> Int (value.Length)
              Keyword = fun value _ -> Int (value.Length)
              Tuple = fun value _ -> Int (Array.length value)
              Keyphrase = fun _ phrase _ -> Int (Array.length phrase)
              List = fun value _ -> Int (List.length value)
              Ring = fun value _ -> Int (value.Count)
              Table = fun value _ -> Int (value.Count) }

        let NormalFns =
            { Bool = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a bool.", originOpt)
              Int = fun value originOpt -> if value = 0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero int.", originOpt) elif value < 0 then Int -1 else Int 1
              Int64 = fun value originOpt -> if value = 0L then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero 64-bit int.", originOpt) elif value < 0L then Int64 -1L else Int64 1L
              Single = fun value originOpt -> if value = 0.0f then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero single.", originOpt) elif value < 0.0f then Single -1.0f else Single 1.0f
              Double = fun value originOpt -> if value = 0.0 then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero double.", originOpt) elif value < 0.0 then Double -1.0 else Double 1.0
              Vector2 = fun value originOpt -> if value = Vector2.Zero then Violation ([!!"InvalidArgumentValue"; !!"Unary"; !!"Normal"], "Cannot get the normal of a zero vector.", originOpt) else Vector2 (Vector2.Normalize value)
              String = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a string.", originOpt)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a keyword.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a tuple.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a keyphrase.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a list.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a ring.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Normal"], "Cannot normalize a table.", originOpt) }

        let BoolFns =
            { Bool = fun value _ -> Bool (value)
              Int = fun value _ -> Bool (value = 0)
              Int64 = fun value _ -> Bool (value = 0L)
              Single = fun value _ -> Bool (value = 0.0f)
              Double = fun value _ -> Bool (value = 0.0)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a vector to a bool.", originOpt)
              String = fun value _ -> Bool (scvalue value)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a keyword to a bool.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a tuple to a bool.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a keyphrase to a bool.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a list to a bool.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a ring to a bool.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Bool"], "Cannot convert a table to a bool.", originOpt) }

        let IntFns =
            { Bool = fun value _ -> Int (if value then 1 else 0)
              Int = fun value _ -> Int (value)
              Int64 = fun value _ -> Int (int value)
              Single = fun value _ -> Int (int value)
              Double = fun value _ -> Int (int value)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a vector to an int.", originOpt)
              String = fun value _ -> Int (scvalue value)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a keyword to an int.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a tuple to an int.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a keyphrase to an int.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a list to an int.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a ring to an int.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int"], "Cannot convert a table to an int.", originOpt) }

        let Int64Fns =
            { Bool = fun value _ -> Int64 (if value then 1L else 0L)
              Int = fun value _ -> Int64 (int64 value)
              Int64 = fun value _ -> Int64 (value)
              Single = fun value _ -> Int64 (int64 value)
              Double = fun value _ -> Int64 (int64 value)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a vector to a 64-bit int.", originOpt)
              String = fun value _ -> Int64 (scvalue value)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a keyword to a 64-bit int.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a tuple to a 64-bit int.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a keyphrase to a 64-bit int.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a list to a 64-bit int.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a ring to a 64-bit int.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Int64"], "Cannot convert a table to a 64-bit int.", originOpt) }

        let SingleFns =
            { Bool = fun value _ -> Single (if value then 1.0f else 0.0f)
              Int = fun value _ -> Single (single value)
              Int64 = fun value _ -> Single (single value)
              Single = fun value _ -> Single (value)
              Double = fun value _ -> Single (single value)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a vector to a single.", originOpt)
              String = fun value _ -> Single (scvalue value)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a keyword to a single.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a tuple to a single.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a keyphrase to a single.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a list to a single.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a ring to a single.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Single"], "Cannot convert a table to a single.", originOpt) }

        let DoubleFns =
            { Bool = fun value _ -> Double (if value then 1.0 else 0.0)
              Int = fun value _ -> Double (double value)
              Int64 = fun value _ -> Double (double value)
              Single = fun value _ -> Double (double value)
              Double = fun value _ -> Double (value)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a vector to a double.", originOpt)
              String = fun value _ -> Double (scvalue value)
              Keyword = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a keyword to a double.", originOpt)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a tuple to a double.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a keyphrase to a double.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a list to a double.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a ring to a double.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"Double"], "Cannot convert a table to a double.", originOpt) }

        let StringFns =
            { Bool = fun value _ -> String (scstring value)
              Int = fun value _ -> String (scstring value)
              Int64 = fun value _ -> String (scstring value)
              Single = fun value _ -> String (scstring value)
              Double = fun value _ -> String (scstring value)
              Vector2 = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a v2 to a string.", originOpt)
              String = fun value _ -> String (value)
              Keyword = fun value _ -> String (value)
              Tuple = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a tuple to a string.", originOpt)
              Keyphrase = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a keyphrase to a string.", originOpt)
              List = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a list to a string.", originOpt)
              Ring = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a ring to a string.", originOpt)
              Table = fun _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Unary"; !!"Conversion"; !!"String"], "Cannot convert a table to a string.", originOpt) }

        type [<NoEquality; NoComparison>] BinaryFns =
            { Bool : bool -> bool -> SymbolOrigin option -> Expr
              Int : int -> int -> SymbolOrigin option -> Expr
              Int64 : int64 -> int64 -> SymbolOrigin option -> Expr
              Single : single -> single -> SymbolOrigin option -> Expr
              Double : double -> double -> SymbolOrigin option -> Expr
              Vector2 : Vector2 -> Vector2 -> SymbolOrigin option -> Expr
              String : string -> string -> SymbolOrigin option -> Expr
              Keyword : string -> string -> SymbolOrigin option -> Expr
              Tuple : Expr array -> Expr array -> SymbolOrigin option -> Expr
              Keyphrase : Expr -> Expr array -> Expr -> Expr array -> SymbolOrigin option -> Expr
              List : Expr list -> Expr list -> SymbolOrigin option -> Expr
              Ring : Expr Set -> Expr Set -> SymbolOrigin option -> Expr
              Table : Map<Expr, Expr> -> Map<Expr, Expr> -> SymbolOrigin option -> Expr }

        let EqFns =
            { Bool = fun left right _ -> Bool (left = right)
              Int = fun left right _ -> Bool (left = right)
              Int64 = fun left right _ -> Bool (left = right)
              Single = fun left right _ -> Bool (left = right)
              Double = fun left right _ -> Bool (left = right)
              Vector2 = fun left right _ -> Bool (left = right)
              String = fun left right _ -> Bool (left = right)
              Keyword = fun left right _ -> Bool (left = right)
              Tuple = fun left right _ -> Bool (left = right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) = (wordRight, phraseRight))
              List = fun left right _ -> Bool (left = right)
              Ring = fun left right _ -> Bool (left = right)
              Table = fun left right _ -> Bool (left = right) }

        let NotEqFns =
            { Bool = fun left right _ -> Bool (left <> right)
              Int = fun left right _ -> Bool (left <> right)
              Int64 = fun left right _ -> Bool (left <> right)
              Single = fun left right _ -> Bool (left <> right)
              Double = fun left right _ -> Bool (left <> right)
              Vector2 = fun left right _ -> Bool (left <> right)
              String = fun left right _ -> Bool (left <> right)
              Keyword = fun left right _ -> Bool (left <> right)
              Tuple = fun left right _ -> Bool (left <> right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) <> (wordRight, phraseRight))
              List = fun left right _ -> Bool (left <> right)
              Ring = fun left right _ -> Bool (left <> right)
              Table = fun left right _ -> Bool (left <> right) }

        let LtFns =
            { Bool = fun left right _ -> Bool (left < right)
              Int = fun left right _ -> Bool (left < right)
              Int64 = fun left right _ -> Bool (left < right)
              Single = fun left right _ -> Bool (left < right)
              Double = fun left right _ -> Bool (left < right)
              Vector2 = fun left right _ -> Bool (left.LengthSquared < right.LengthSquared)
              String = fun left right _ -> Bool (left < right)
              Keyword = fun left right _ -> Bool (left < right)
              Tuple = fun left right _ -> Bool (left < right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) < (wordRight, phraseRight))
              List = fun left right _ -> Bool (left < right)
              Ring = fun left right _ -> Bool (left < right)
              Table = fun left right _ -> Bool (left < right) }

        let GtFns =
            { Bool = fun left right _ -> Bool (left > right)
              Int = fun left right _ -> Bool (left > right)
              Int64 = fun left right _ -> Bool (left > right)
              Single = fun left right _ -> Bool (left > right)
              Double = fun left right _ -> Bool (left > right)
              Vector2 = fun left right _ -> Bool (left.LengthSquared > right.LengthSquared)
              String = fun left right _ -> Bool (left > right)
              Keyword = fun left right _ -> Bool (left > right)
              Tuple = fun left right _ -> Bool (left > right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) > (wordRight, phraseRight))
              List = fun left right _ -> Bool (left > right)
              Ring = fun left right _ -> Bool (left > right)
              Table = fun left right _ -> Bool (left > right) }

        let LtEqFns =
            { Bool = fun left right _ -> Bool (left <= right)
              Int = fun left right _ -> Bool (left <= right)
              Int64 = fun left right _ -> Bool (left <= right)
              Single = fun left right _ -> Bool (left <= right)
              Double = fun left right _ -> Bool (left <= right)
              Vector2 = fun left right _ -> Bool (left.LengthSquared <= right.LengthSquared)
              String = fun left right _ -> Bool (left <= right)
              Keyword = fun left right _ -> Bool (left <= right)
              Tuple = fun left right _ -> Bool (left <= right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) <= (wordRight, phraseRight))
              List = fun left right _ -> Bool (left <= right)
              Ring = fun left right _ -> Bool (left <= right)
              Table = fun left right _ -> Bool (left <= right) }

        let GtEqFns =
            { Bool = fun left right _ -> Bool (left >= right)
              Int = fun left right _ -> Bool (left >= right)
              Int64 = fun left right _ -> Bool (left >= right)
              Single = fun left right _ -> Bool (left >= right)
              Double = fun left right _ -> Bool (left >= right)
              Vector2 = fun left right _ -> Bool (left.LengthSquared >= right.LengthSquared)
              String = fun left right _ -> Bool (left >= right)
              Keyword = fun left right _ -> Bool (left >= right)
              Tuple = fun left right _ -> Bool (left >= right)
              Keyphrase = fun wordLeft phraseLeft wordRight phraseRight _ -> Bool ((wordLeft, phraseLeft) >= (wordRight, phraseRight))
              List = fun left right _ -> Bool (left >= right)
              Ring = fun left right _ -> Bool (left >= right)
              Table = fun left right _ -> Bool (left >= right) }

        let AddFns =
            { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
              Int = fun left right _ -> Int (left + right)
              Int64 = fun left right _ -> Int64 (left + right)
              Single = fun left right _ -> Single (left + right)
              Double = fun left right _ -> Double (left + right)
              Vector2 = fun left right _ -> Vector2 (left + right)
              String = fun left right _ -> String (left + right)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add tuples.", originOpt)
              Keyphrase =
                fun wordLeft phraseLeft wordRight phraseRight originOpt ->
                    if wordLeft = wordRight
                    then Keyphrase (wordLeft, Array.append phraseLeft phraseRight)
                    else Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Add"], "Cannot add keywords.", originOpt)
              List = fun left right _ -> List (left @ right)
              Ring = fun left right _ -> Ring (Set.union left right)
              Table = fun left right _ -> Table (left @@ right) }

        let SubFns =
            { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
              Int = fun left right _ -> Int (left - right)
              Int64 = fun left right _ -> Int64 (left - right)
              Single = fun left right _ -> Single (left - right)
              Double = fun left right _ -> Double (left - right)
              Vector2 = fun left right _ -> Vector2 (left - right)
              String = fun left right _ -> String (left.Replace (right, String.Empty))
              Keyword = fun left right _ -> String (left.Replace (right, String.Empty))
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract lists.", originOpt)
              Ring = fun left right _ -> Ring (Set.difference left right)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Sub"], "Cannot subtract tables.", originOpt) }

        let MulFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply bools.", originOpt)
              Int = fun left right _ -> Int (left * right)
              Int64 = fun left right _ -> Int64 (left * right)
              Single = fun left right _ -> Single (left * right)
              Double = fun left right _ -> Double (left * right)
              Vector2 = fun left right _ -> Vector2 (Vector2.Multiply (left, right))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply keyword.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mul"], "Cannot multiply tables.", originOpt) }

        let DivFns =
            { Bool = fun left right originOpt -> if right = false then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a false bool.", originOpt) else Bool (if left && right then true else false)
              Int = fun left right originOpt -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero int.", originOpt) else Int (left / right)
              Int64 = fun left right originOpt -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Div"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 (left / right)
              Single = fun left right _ -> Single (left / right)
              Double = fun left right _ -> Double (left / right)
              Vector2 = fun left right _ -> Vector2 (Vector2.Divide (left, right))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Div"], "Cannot divide tables.", originOpt) }

        let ModFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate bools.", originOpt)
              Int = fun left right originOpt -> if right = 0 then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot modulate by a zero int.", originOpt) else Int (left % right)
              Int64 = fun left right originOpt -> if right = 0L then Violation ([!!"InvalidArgumentValue"; !!"Binary"; !!"Mod"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 (left % right)
              Single = fun left right _ -> Single (left % right)
              Double = fun left right _ -> Double (left % right)
              Vector2 = fun left right _ -> Vector2 (OpenTK.Vector2 (left.X % right.X, left.Y % right.Y))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Mod"], "Cannot modulate tables.", originOpt) }

        let PowFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power bools.", originOpt)
              Int = fun left right _ -> Int (int ^ Math.Pow (double left, double right))
              Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, double right))
              Single = fun left right _ -> Single (single ^ Math.Pow (double left, double right))
              Double = fun left right _ -> Double (Math.Pow (double left, double right))
              Vector2 = fun left right _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, double right.X), single ^ Math.Pow (double left.Y, double right.Y)))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Pow"], "Cannot power tables.", originOpt) }

        let RootFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root bools.", originOpt)
              Int = fun left right _ -> Int (int ^ Math.Pow (double left, 1.0 / double right))
              Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right))
              Single = fun left right _ -> Single (single ^ Math.Pow (double left, 1.0 / double right))
              Double = fun left right _ -> Double (Math.Pow (double left, 1.0 / double right))
              Vector2 = fun left right _ -> Vector2 (OpenTK.Vector2 (single ^ Math.Pow (double left.X, 1.0 / double right.X), single ^ Math.Pow (double left.Y, 1.0 / double right.Y)))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Root"], "Cannot root tables.", originOpt) }

        let CrossFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply bools.", originOpt)
              Int = fun left right _ -> Int (left * right)
              Int64 = fun left right _ -> Int64 (left * right)
              Single = fun left right _ -> Single (left * right)
              Double = fun left right _ -> Double (left * right)
              Vector2 = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply 2-dimensional vectors.", originOpt)
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiple keyphrases.", originOpt)
              List = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply lists.", originOpt)
              Ring = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply rings.", originOpt)
              Table = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Cross"], "Cannot cross multiply tables.", originOpt) }

        let DotFns =
            { Bool = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply bools.", originOpt)
              Int = fun left right _ -> Int (left * right)
              Int64 = fun left right _ -> Int64 (left * right)
              Single = fun left right _ -> Single (left * right)
              Double = fun left right _ -> Double (left * right)
              Vector2 = fun left right _ -> Single (Vector2.Dot (left, right))
              String = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply strings.", originOpt)
              Keyword = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply keywords.", originOpt)
              Tuple = fun _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply tuples.", originOpt)
              Keyphrase = fun _ _ _ _ originOpt -> Violation ([!!"InvalidArgumentType"; !!"Binary"; !!"Dot"], "Cannot dot multiply keyphrases.", originOpt)
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
                | Bool bool -> (Bool (fn bool), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalBoolBinary fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledLeft; evaledRight] ->
                match (evaledLeft, evaledRight) with
                | (Bool boolLeft, Bool boolRight) -> (Bool (fn boolLeft boolRight), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a bool function to a non-bool value.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalUnaryInner (fns : UnaryFns) fnOriginOpt fnName evaledArg world =
            match evaledArg with
            | Bool boolValue -> ((fns.Bool boolValue fnOriginOpt), world)
            | Int intValue -> ((fns.Int intValue fnOriginOpt), world)
            | Int64 int64Value -> ((fns.Int64 int64Value fnOriginOpt), world)
            | Single singleValue -> ((fns.Single singleValue fnOriginOpt), world)
            | Double doubleValue -> ((fns.Double doubleValue fnOriginOpt), world)
            | Vector2 vector2Value -> ((fns.Vector2 vector2Value fnOriginOpt), world)
            | String stringValue -> ((fns.String stringValue fnOriginOpt), world)
            | Tuple tupleValue -> ((fns.Tuple tupleValue fnOriginOpt), world)
            | Keyphrase (wordValue, phraseValue) -> ((fns.Keyphrase wordValue phraseValue fnOriginOpt), world)
            | List listValue -> ((fns.List listValue fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Unary"; !!(String.capitalize fnName)], "Cannot apply an unary function on an incompatible value.", fnOriginOpt), world)

        let evalUnary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> evalUnaryInner fns fnOriginOpt fnName evaledArg world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Unary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalBinaryInner (fns : BinaryFns) fnOriginOpt fnName evaledLeft evaledRight world =
            match (evaledLeft, evaledRight) with
            | (Bool boolLeft, Bool boolRight) -> ((fns.Bool boolLeft boolRight fnOriginOpt), world)
            | (Int intLeft, Int intRight) -> ((fns.Int intLeft intRight fnOriginOpt), world)
            | (Int64 int64Left, Int64 int64Right) -> ((fns.Int64 int64Left int64Right fnOriginOpt), world)
            | (Single singleLeft, Single singleRight) -> ((fns.Single singleLeft singleRight fnOriginOpt), world)
            | (Double doubleLeft, Double doubleRight) -> ((fns.Double doubleLeft doubleRight fnOriginOpt), world)
            | (Vector2 vector2Left, Vector2 vector2Right) -> ((fns.Vector2 vector2Left vector2Right fnOriginOpt), world)
            | (String stringLeft, String stringRight) -> ((fns.String stringLeft stringRight fnOriginOpt), world)
            | (Tuple tupleLeft, Tuple tupleRight) -> ((fns.Tuple tupleLeft tupleRight fnOriginOpt), world)
            | (Keyphrase (wordLeft, phraseLeft), Keyphrase (wordRight, phraseRight)) -> ((fns.Keyphrase wordLeft phraseLeft wordRight phraseRight fnOriginOpt), world)
            | (List listLeft, List listRight) -> ((fns.List listLeft listRight fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentType"; !!"Binary"; !!(String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", fnOriginOpt), world)

        let evalBinary fns fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledLeft; evaledRight] -> evalBinaryInner fns fnOriginOpt fnName evaledLeft evaledRight world                
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Binary"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalSinglet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> fn fnOriginOpt fnName evaledArg world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 1 argument.", fnOriginOpt), world)

        let evalDoublet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; evaled2] -> fn fnOriginOpt fnName evaled evaled2 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 2 arguments.", fnOriginOpt), world)

        let evalTriplet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; evaled2; evaled3] -> fn fnOriginOpt fnName evaled evaled2 evaled3 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 3 arguments.", fnOriginOpt), world)

        let evalQuadlet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; evaled2; evaled3; evaled4] -> fn fnOriginOpt fnName evaled evaled2 evaled3 evaled4 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 4 arguments.", fnOriginOpt), world)

        let evalQuintet fn fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; evaled2; evaled3; evaled4; evaled5] -> fn fnOriginOpt fnName evaled evaled2 evaled3 evaled4 evaled5 world
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!fnName], "Function '" + fnName + "' requires 5 arguments.", fnOriginOpt), world)
        
        let evalV2 fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Single x; Single y] -> (Vector2 (OpenTK.Vector2 (x, y)), world)
            | [Violation _ as violation; _] -> (violation, world)
            | [_; Violation _ as violation] -> (violation, world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"V2"; !!(String.capitalize fnName)], "Application of " + fnName + " requires a single for the both arguments.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"V2"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)

        let evalPair fnOriginOpt (_ : string) evaledArgs world =
            match evaledArgs with
            | [_; _] -> (Tuple (List.toArray evaledArgs), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Pair"], "Incorrect number of arguments for creation of a pair; 2 arguments required.", fnOriginOpt), world)

        let evalTuple _ _ evaledArgs world =
            (Tuple (List.toArray evaledArgs), world)
    
        let evalNth5 index fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [String str] ->
                if index >= 0 && index < String.length str
                then (String (string str.[index]), world)
                else (Violation ([!!"OutOfRange"], "String does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [Tuple evaleds] ->
                if index >= 0 && index < Array.length evaleds
                then (evaleds.[index], world)
                else (Violation ([!!"OutOfRange"], "Tuple does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [Keyphrase (_, evaleds)] ->
                if index >= 0 && index < Array.length evaleds
                then (evaleds.[index], world)
                else (Violation ([!!"OutOfRange"], "Keyphrase does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [List evaleds] ->
                match List.tryFindAt index evaleds with
                | Some evaled -> (evaled, world)
                | None -> (Violation ([!!"OutOfRange"], "List does not contain element at index " + string index + ".", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-sequence.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
        
        let evalNth fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [head; foot] ->
                match head with
                | Int int -> evalNth5 int fnOriginOpt fnName [foot] world
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Sequence"; !!(String.capitalize fnName)], "Application of " + fnName + " requires an int for the first argument.", fnOriginOpt), world)
            | _ ->  (Violation ([!!"InvalidArgumentCount"; !!"Sequence"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", fnOriginOpt), world)
            
        let evalSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaledArg] -> (Option (Some evaledArg), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsNone fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option evaled] -> (Bool (Option.isNone evaled), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsSome fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [Option evaled] -> (Bool (Option.isSome evaled), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Option"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-option.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Option"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalMap evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; Option opt as option] ->
                match opt with
                | Some value -> evalApply [value] fnOriginOpt world
                | None -> (option, world)
            | [_; List list] ->
                let (list, world) =
                    List.fold (fun (elems, world) current ->
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        (elem :: elems, world))
                        ([], world)
                        list
                (List (List.rev list), world)
            | [_; Ring set] ->
                let (set, world) =
                    Set.fold (fun (elems, world) current ->
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        (Set.add elem elems, world))
                        (Set.empty, world)
                        set
                (Ring set, world)
            | [_; Table map] ->
                let (map, world) =
                    Map.fold (fun (elems, world) currentKey currentValue ->
                        let current = Tuple [|currentKey; currentValue|]
                        let (elem, world) = evalApply [current] fnOriginOpt world
                        match elem with
                        | Tuple elems' when Array.length elems' = 2 -> ((Map.add elems'.[0] elems'.[1] elems), world)
                        | _ -> (elems, world))
                        (Map.empty, world)
                        map
                (Table map, world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Functor"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-functor.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Functor"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
            
        let evalList _ _ evaledArgs world =
            (List evaledArgs, world)
    
        let evalHead fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List evaledArgs] ->
                match evaledArgs with
                | evaledHead :: _ -> (evaledHead, world)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalTail fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List evaleds] ->
                match evaleds with
                | _ :: evaledTail -> (List evaledTail, world)
                | _ -> (Violation ([!!"InvalidArgumentValue"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a list with no members.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalCons fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [evaled; List evaleds] -> (List (evaled :: evaleds), world)
            | [_; _] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-list.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalIsEmpty fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List list] -> (Bool (List.isEmpty list), world)
            | [Ring set] -> (Bool (Set.isEmpty set), world)
            | [Table map] -> (Bool (Map.isEmpty map), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalNotEmpty fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [List evaleds] -> (Bool (not ^ List.isEmpty evaleds), world)
            | [Ring set] -> (Bool (not ^ Set.isEmpty set), world)
            | [Table map] -> (Bool (not ^ Map.isEmpty map), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalFold evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; state; List list] -> List.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (state, world) list
            | [_; state; Ring set] -> Set.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (state, world) set
            | [_; state; Table map] ->
                Map.fold (fun (acc, world) currentKey currentValue ->
                    let current = Tuple [|currentKey; currentValue|]
                    evalApply [acc; current] fnOriginOpt world)
                    (state, world)
                    map
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
    
        let evalReduce evalApply fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [_; List list] ->
                match list with
                | head :: tail -> List.fold (fun (acc, world) current -> evalApply [acc; current] fnOriginOpt world) (head, world) tail
                | _ -> (Violation ([!!"InvalidArgument"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to an empty list.", fnOriginOpt), world)
            | [_] -> (Violation ([!!"InvalidArgumentType"; !!"List"; !!(String.capitalize fnName)], "Cannot apply " + fnName + " to a non-container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"List"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)
            
        let evalRing _ (_ : string) evaledArgs world =
            (Ring (Set.ofList evaledArgs), world)

        let evalAdd fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring set -> (Ring (Set.add value set), world)
                | Table map ->
                    match value with
                    | Tuple arr when Array.length arr = 2 -> (Table (Map.add arr.[0] arr.[1] map), world)
                    | _ -> (Violation ([!!"InvalidEntry"; !!"Table"; !!(String.capitalize fnName)], "Table entry must consist of a pair.", fnOriginOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalRemove fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [value; container] ->
                match container with
                | Ring set -> (Ring (Set.remove value set), world)
                | Table map -> (Table (Map.remove value map), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTryFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table map -> (Option (Map.tryFind key map), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalFind fnOriginOpt fnName evaledArgs world =
            match evaledArgs with
            | [key; container] ->
                match container with
                | Table map ->
                    match Map.tryFind key map with
                    | Some value -> (value, world)
                    | None -> (Violation ([!!"InvalidKey"; !!"Table"; !!(String.capitalize fnName)], "Key not found in table.", fnOriginOpt), world)
                | _ -> (Violation ([!!"InvalidArgumentType"; !!"Container"; !!(String.capitalize fnName)], "Incorrect type of argument for application of '" + fnName + "'; target must be a container.", fnOriginOpt), world)
            | _ -> (Violation ([!!"InvalidArgumentCount"; !!"Container"; !!(String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", fnOriginOpt), world)

        let evalTable fnOriginOpt fnName evaledArgs world =
            if List.forall (function Tuple arr when Array.length arr = 2 -> true | _ -> false) evaledArgs then
                let evaledPairs = List.map (function List [evaledFst; evaledSnd] -> (evaledFst, evaledSnd) | _ -> failwithumf ()) evaledArgs
                let evaledMap = Map.ofList evaledPairs
                (Table evaledMap, world)
            else (Violation ([!!"InvalidEntries"; !!"Table"; !!(String.capitalize fnName)], "Table entries must consist of 1 or more pairs.", fnOriginOpt), world)

        let rec Intrinsics =
            dictPlus
                [("&&", evalBoolBinary (&&))
                 ("||", evalBoolBinary (||))
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
                 ("toZero", evalUnary ToZeroFns)
                 ("toId", evalUnary ToIdFns)
                 ("toMin", evalUnary ToMinFns)
                 ("toMax", evalUnary ToMaxFns)
                 ("not", evalBoolUnary not)
                 ("inc", evalUnary IncFns)
                 ("dec", evalUnary DecFns)
                 ("neg", evalUnary NegFns)
                 ("hash", evalUnary HashFns)
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
                 //("yAs", evalNAs 1) TODO
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
                 //("rev", evalRev) TODO
                 //("toList", evalToList) TODO
                 ("ring", evalRing)
                 ("add", evalAdd)
                 ("remove", evalRemove)
                 //("toRing", evalToRing) TODO
                 ("table", evalTable)
                 ("tryFind", evalTryFind)
                 ("find", evalFind)
                 //("toTable", evalToTable) TODO
                 ("product", evalProduct)
                 ("entityExists", evalSinglet evalSimulantExists)
                 ("layerExists", evalSinglet evalSimulantExists)
                 ("screenExists", evalSinglet evalSimulantExists)
                 ("simulantExists", evalSinglet evalSimulantExists)]

        and isIntrinsic name =
            Intrinsics.ContainsKey name

        and evalIntrinsic originOpt name evaledArgs world =
            match Intrinsics.TryGetValue name with
            | (true, intrinsic) -> intrinsic originOpt name evaledArgs world
            | (false, _) -> (Violation ([!!"InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", originOpt), world)

        // TODO: ensure these origins are sensible
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

        and evalSimulantExists fnOriginOpt name evaledArg world =
            match evaledArg with
            | String str
            | Keyword str ->
                let relation = Relation.makeFromString str
                let context = World.getScriptContext world
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryProxySimulant address with
                | Some simulant -> (Bool (World.simulantExists simulant world), world)
                | None -> (Bool false, world)
            | _ -> (Violation ([!!"InvalidArgumentType"], "Function '" + name + "' requires 1 relation argument.", fnOriginOpt), world)

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
                | String eventAddressStr
                | Keyword eventAddressStr ->
                    try let eventAddress = Address.makeFromString eventAddressStr
                        let eventStream = Stream.stream eventAddress
                        Right (eventStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address '" + eventAddressStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidEventStreamAddress"], "Variable '" + name + "' could not be created due to invalid event stream address. Address must be either a string or a keyword", originOpt))
            | PropertyStream (propertyName, propertyRelation) ->
                let (propertyRelationEvaled, world) = eval propertyRelation world
                match propertyRelationEvaled with
                | String propertyRelationStr
                | Keyword propertyRelationStr ->
                    try let context = World.getScriptContext world
                        let propertyRelation = Relation.makeFromString propertyRelationStr
                        let propertyAddress = Relation.resolve context.SimulantAddress propertyRelation -<<- Address.makeFromName !!propertyName
                        let propertyStream = Stream.stream propertyAddress
                        Right (propertyStream, world)
                    with exn -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation '" + propertyRelationStr + "'.", originOpt))
                | _ -> Left (Violation ([!!"InvalidVariableDeclaration"; !!"InvalidPropertyStreamRelation"], "Variable '" + name + "' could not be created due to invalid property stream relation. Relation must be either a string or a keyword", originOpt))
            | PropertyStreamMany _ ->
                Left (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt)) // TODO: implement
            | ComputedStream computedStream ->
                Right (computedStream :?> Prime.Stream<obj, Game, World>, world)

        and evalBinding expr name cachedBinding originOpt world =
            match World.tryGetBinding name cachedBinding world with
            | None ->
                if isIntrinsic name then (expr, world)
                else (Violation ([!!"NonexistentBinding"], "Non-existent binding '" + name + "' ", originOpt), world)
            | Some binding -> (binding, world)

        and evalApply exprs originOpt world =
            match evalMany exprs world with
            | (evaledHead :: evaledTail, world) ->
                match evaledHead with
                | Keyword _ as keyword ->
                    let keyphrase = Keyphrase (keyword, List.toArray evaledTail)
                    (keyphrase, world)
                | Binding (name, _, originOpt) ->
                    // NOTE: we can infer we have an intrinsic when evaluation leads here
                    evalIntrinsic originOpt name evaledTail world
                | Fun (pars, parsCount, body, _, framesOpt, originOpt) ->
                    let (framesCurrentOpt, world) =
                        match framesOpt with
                        | Some frames ->
                            let framesCurrent =  World.getProceduralFrames world
                            let world = World.setProceduralFrames (frames :?> ProceduralFrame list) world
                            (Some framesCurrent, world)
                        | None -> (None, world)
                    let (evaled, world) =
                        let evaledArgs = evaledTail
                        if List.hasExactly parsCount evaledArgs then
                            let bindings = List.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                            let world = World.addProceduralBindings (AddToNewFrame parsCount) bindings world
                            let (evaled, world) = eval body world
                            (evaled, World.removeProceduralBindings world)
                        else (Violation ([!!"MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), world)
                    match framesCurrentOpt with
                    | Some framesCurrent ->
                        let world = World.setProceduralFrames framesCurrent world
                        (evaled, world)
                    | None -> (evaled, world)
                | _ -> (Violation ([!!"TODO: proper violation category."], "Cannot apply a non-binding.", originOpt), world)
            | ([], _) -> (Unit, world)

        and evalLet4 binding body originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let evaled = evalDropEnv body world
                    World.addProceduralBinding (AddToNewFrame 1) name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame 1) name fn world
            let (evaled, world) = eval body world
            (evaled, World.removeProceduralBindings world)

        and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
            let world =
                match bindingsHead with
                | VariableBinding (name, body) ->
                    let bodyValue = evalDropEnv body world
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name fn world
            let world =
                List.foldi (fun i world binding ->
                    match binding with
                    | VariableBinding (name, body) ->
                        let bodyValue = evalDropEnv body world
                        World.addProceduralBinding (AddToHeadFrame ^ inc i) name bodyValue world
                    | FunctionBinding (name, args, body) ->
                        let frames = World.getProceduralFrames world :> obj
                        let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                        World.addProceduralBinding (AddToHeadFrame ^ inc i) name fn world)
                    world
                    bindingsTail
            let (evaled, world) = eval body world
            (evaled, World.removeProceduralBindings world)
        
        and evalLet binding body originOpt world =
            evalLet4 binding body originOpt world
        
        and evalLetMany bindings body originOpt world =
            match bindings with
            | bindingsHead :: bindingsTail ->
                let bindingsCount = List.length bindingsTail + 1
                evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world
            | [] -> (Violation ([!!"MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)

        and evalFun fn pars parsCount body framesPushed framesOpt originOpt world =
            if not framesPushed then
                if Option.isNone framesOpt then
                    let frames = World.getProceduralFrames world :> obj
                    (Fun (pars, parsCount, body, true, Some frames, originOpt), world)
                else (Fun (pars, parsCount, body, true, framesOpt, originOpt), world)
            else (fn, world)

        and evalIf condition consequent alternative originOpt world =
            match eval condition world with
            | (Violation _ as evaled, world) -> (evaled, world)
            | (Bool bool, world) -> if bool then eval consequent world else eval alternative world
            | (_, world) -> (Violation ([!!"InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", originOpt), world)

        and evalMatch input (cases : (Expr * Expr) list) originOpt world =
            let (input, world) = eval input world
            let resultEir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    let (evaledInput, world) = eval condition world
                    match evalBinaryInner EqFns originOpt "=" input evaledInput world with
                    | (Violation _, world) -> Right (evaledInput, world)
                    | (Bool true, world) -> Right (eval consequent world)
                    | (Bool false, world) -> Left world
                    | _ -> failwithumf ())
                    (Left world)
                    cases
            match resultEir with
            | Right success -> success
            | Left world -> (Violation ([!!"InexhaustiveMatch"], "A match expression failed to meet any of its cases.", originOpt), world)

        and evalSelect exprPairs originOpt world =
            let resultEir =
                List.foldUntilRight (fun world (condition, consequent) ->
                    match eval condition world with
                    | (Violation _ as evaled, world) -> Right (evaled, world)
                    | (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                    | (_, world) -> Right ((Violation ([!!"InvalidSelectCondition"], "Must provide an expression that evaluates to a bool in a case condition.", originOpt), world)))
                    (Left world)
                    exprPairs
            match resultEir with
            | Right success -> success
            | Left world -> (Violation ([!!"InexhaustiveSelect"], "A select expression failed to meet any of its cases.", originOpt), world)

        and evalTry body handlers _ world =
            match eval body world with
            | (Violation (categories, _, _) as evaled, world) ->
                let resultEir =
                    List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                        let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                        if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                        (Left world)
                        handlers
                match resultEir with
                | Right success -> success
                | Left world -> (evaled, world)
            | success -> success

        and evalDo exprs _ world =
            let evaledEir =
                List.foldWhileRight (fun (_, world) expr ->
                    match eval expr world with
                    | (Violation _, _) as error -> Left error
                    | success -> Right success)
                    (Right (Unit, world))
                    exprs
            Either.amb evaledEir

        and evalBreak expr world =
            // TODO: write all procedural bindings to console
            Debugger.Break ()
            eval expr world

        and evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match eval relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt), world)
                    | (_, world) -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match Importers.TryGetValue propertyType.Name with
                    | (true, tryImport) ->
                        match tryImport propertyValue propertyType with
                        | Some propertyValue -> (propertyValue, world)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting worldironment.", originOpt), world)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be imported into scripting worldironment.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        and evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match eval relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryProxySimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt), world)
                    | (_, world) -> Left (Violation ([!!"InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    match Exporters.TryGetValue propertyType.Name with
                    | (true, tryExport) ->
                        let (propertyValue, world) = eval propertyValueExpr world
                        match tryExport propertyValue propertyType with
                        | Some propertyValue ->
                            match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                            | (true, world) -> (Unit, world)
                            | (false, world) -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
                        | None -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                    | (false, _) -> (Violation ([!!"InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Violation ([!!"InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        and evalDefine binding originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let evaled = evalDropEnv body world
                    World.addDeclarationBinding name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, List.length args, body, true, Some frames, originOpt)
                    World.addDeclarationBinding name fn world
            (Unit, world)

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
            | Fun (pars, parsCount, body, framesPushed, framesOpt, originOpt) as fn -> evalFun fn pars parsCount body framesPushed framesOpt originOpt world
            | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt world
            | Match (input, cases, originOpt) -> evalMatch input cases originOpt world
            | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt world
            | Try (body, handlers, originOpt) -> evalTry body handlers originOpt world
            | Do (exprs, originOpt) -> evalDo exprs originOpt world
            | Break (expr, _) -> evalBreak expr world
            | Get (name, originOpt) -> evalGet name None originOpt world
            | GetFrom (name, expr, originOpt) -> evalGet name (Some expr) originOpt world
            | Set (name, expr, originOpt) -> evalSet name None expr originOpt world
            | SetTo (name, expr, expr2, originOpt) -> evalSet name (Some expr) expr2 originOpt world
            | Quote (_, originOpt) -> (Violation ([!!"Unimplemented"], "Unimplemented feature.", originOpt), world) // TODO
            | Define (binding, originOpt) -> evalDefine binding originOpt world

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