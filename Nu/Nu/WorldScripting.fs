// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open FSharp.Reflection
open OpenTK
open Prime
open Prime.Scripting
open Nu
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScripting =

    let mutable Extrinsics =
        Unchecked.defaultof<Dictionary<string, string -> Expr array -> SymbolOrigin option -> World -> struct (Expr * World)>>

    let mutable Bindings =
        Unchecked.defaultof<Dictionary<string, string -> Expr array -> SymbolOrigin option -> World -> struct (Expr * World)>>

    type World with

        static member internal evalInternal expr world : struct (Expr * World) =
            ScriptingWorld.eval expr world

        static member internal evalManyInternal exprs world : struct (Expr array * World) =
            ScriptingWorld.evalMany exprs world

        static member internal evalWithLoggingInternal expr world : struct (Expr * World) =
            ScriptingWorld.evalWithLogging expr world

        static member internal evalManyWithLoggingInternal exprs world : struct (Expr array * World) =
            ScriptingWorld.evalManyWithLogging exprs world

        static member internal evalSimulantExists fnName evaledArg originOpt world =
            match evaledArg with
            | String str
            | Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDeriveSimulant address with
                | Some simulant -> struct (Bool (World.getSimulantExists simulant world), world)
                | None -> struct (Bool false, world)
            | Violation _ as error -> struct (error, world)
            | _ -> struct (Violation (["InvalidArgumentType"; "SimulantExists"], "Function '" + fnName + "' requires 1 Relation argument.", originOpt), world)

        static member internal evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | struct (String str, world)
                    | struct (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right struct (simulant, world)
                        | None -> Left struct (Violation (["InvalidPropertyRelation"; "Get"], "Relation must have 0 to 3 names.", originOpt), world)
                    | struct (Violation _, _) as error -> Left error
                    | struct (_, world) -> Left struct (Violation (["InvalidPropertyRelation"; "Get"], "Relation must be either a String or Keyword.", originOpt), world)
                | None -> Right struct (context, world)
            match simulantAndEnvEir with
            | Right struct (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some property ->
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        match ScriptingWorld.tryImport dp.DesignerType dp.DesignerValue world with
                        | Some propertyValue -> struct (propertyValue, world)
                        | None -> struct (Violation (["InvalidPropertyValue"; "Get"], "Property value could not be imported into scripting environment.", originOpt), world)
                    | _ ->
                        match ScriptingWorld.tryImport property.PropertyType property.PropertyValue world with
                        | Some propertyValue -> struct (propertyValue, world)
                        | None -> struct (Violation (["InvalidPropertyValue"; "Get"], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> struct (Violation (["InvalidProperty"; "Get"], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        static member internal evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | struct (String str, world)
                    | struct (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right struct (simulant, world)
                        | None -> Left struct (Violation (["InvalidPropertyRelation"; "Set"], "Relation must have 0 to 3 parts.", originOpt), world)
                    | struct (Violation _, _) as error -> Left error
                    | struct (_, world) -> Left struct (Violation (["InvalidPropertyRelation"; "Set"], "Relation must be either a String or Keyword.", originOpt), world)
                | None -> Right struct (context, world)
            match simulantAndEnvEir with
            | Right struct (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some property ->
                    let struct (propertyValue, world) = World.evalInternal propertyValueExpr world
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        match ScriptingWorld.tryExport property.PropertyType propertyValue world with
                        | Some propertyValue ->
                            let propertyValue = { dp with DesignerValue = propertyValue }
                            let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                            match World.trySetSimulantProperty propertyName property simulant world with
                            | (true, world) -> struct (Unit, world)
                            | (false, world) -> struct (Violation (["InvalidProperty"; "Set"], "Property value could not be set.", originOpt), world)
                        | None -> struct (Violation (["InvalidPropertyValue"; "Set"], "Property value could not be exported into Simulant property.", originOpt), world)
                    | _ ->
                        match ScriptingWorld.tryExport property.PropertyType propertyValue world with
                        | Some propertyValue ->
                            let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                            match World.trySetSimulantProperty propertyName property simulant world with
                            | (true, world) -> struct (Unit, world)
                            | (false, world) -> struct (Violation (["InvalidProperty"; "Set"], "Property value could not be set.", originOpt), world)
                        | None -> struct (Violation (["InvalidPropertyValue"; "Set"], "Property value could not be exported into Simulant property.", originOpt), world)
                | None -> struct (Violation (["InvalidProperty"; "Set"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        static member private evalMonitor5 subscription (eventAddress : obj Address) subscriber world =
            EventWorld.subscribe (fun evt world ->
                match World.tryGetSimulantScriptFrame subscriber world with
                | Some scriptFrame ->
                    match ScriptingWorld.tryImport evt.DataType evt.Data world with
                    | Some dataImported ->
                        let evtRecord =
                            Record
                                ("Event",
                                 [|"Data", 0; "Subscriber", 1; "Publisher", 2; "Address", 3|] |> Map.ofArray,
                                 [|dataImported
                                   String (scstring evt.Subscriber)
                                   String (scstring evt.Publisher)
                                   String (scstring evt.Address)|])
                        let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                        let application = Apply ([|subscription; evtRecord|], breakpoint, None)
                        World.evalWithLogging application scriptFrame subscriber world |> snd
                    | None -> Log.info "Property value could not be imported into scripting environment."; world
                | None -> world)
                eventAddress
                (subscriber :> Participant)
                world

        static member private evalMonitor fnName evaledArg evaledArg2 originOpt world =
            match evaledArg with
            | Binding _
            | Fun _ ->
                match evaledArg2 with
                | String str
                | Keyword str ->
                    let world = World.evalMonitor5 evaledArg (Address.makeFromString str) (World.getScriptContext world) world
                    struct (Unit, world)
                | Violation _ as error -> struct (error, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Relation for its 2nd argument.", originOpt), world)
            | Violation _ as error -> struct (error, world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Function for its 1st argument.", originOpt), world)

        /// Attempt to get a static type that matches the given expression.
        /// TODO: P1: move the major portion of this to Prime.Scripting.
        static member tryGetType expr =
            match expr with
            | Scripting.Violation _ -> None
            | Scripting.Unit -> Some typeof<unit>
            | Scripting.Bool _ -> Some typeof<bool>
            | Scripting.Int _ -> Some typeof<int>
            | Scripting.Int64 _ -> Some typeof<int64>
            | Scripting.Single _ -> Some typeof<single>
            | Scripting.Double _ -> Some typeof<double>
            | Scripting.String _ -> Some typeof<string>
            | Scripting.Keyword keyword ->
                let types =
                    AppDomain.CurrentDomain.GetAssemblies () |>
                    Array.map (fun asm -> asm.GetTypes ()) |>
                    Array.concat
                let candidateUnions =
                    types |>
                    Array.map (fun ty -> FSharpType.GetUnionCases ty) |>
                    Array.concat |>
                    Array.filter (fun uc -> uc.Name = keyword) |>
                    Array.map (fun uc -> uc.DeclaringType)
                let candidateTypes =
                    types |>
                    Array.filter (fun ty -> ty.Name = keyword) |>
                    Array.filter (fun ty -> ty.GenericTypeArguments.Length = 0) |> // generic algebraic data types not yet supported...
                    Array.append candidateUnions
                // just take the first found type for now...
                Array.tryHead candidateTypes
            | Scripting.Pluggable value ->
                // TODO: P1: use a virtual dispatching?
                match value with
                | :? Vector2Pluggable -> Some typeof<Vector2>
                | :? Vector4Pluggable -> Some typeof<Vector4>
                | :? Vector2iPluggable -> Some typeof<Vector2i>
                | _ -> None
            | Scripting.Tuple value ->
                let typeOpts = Array.map World.tryGetType value
                match Array.definitizePlus typeOpts with
                | (true, types) -> Some (FSharpType.MakeTupleType types)
                | (false, _) -> None
            | Scripting.Option opt ->
                match opt with
                | Some value -> World.tryGetType value
                | None -> None
            | Scripting.List values ->
                let typeOpts = List.map World.tryGetType values
                match List.definitizePlus typeOpts with
                | (true, types) ->
                    match types with
                    | head :: tail ->
                        if List.notExists (fun item -> item <> head) tail
                        then Some (typedefof<_ list>.MakeGenericType [|head|])
                        else None
                    | [] -> None
                | (false, _) -> None
            | Scripting.Ring values ->
                let typeOpts = List.map World.tryGetType (List.ofSeq values)
                match List.definitizePlus typeOpts with
                | (true, types) ->
                    match types with
                    | head :: tail ->
                        if List.notExists (fun item -> item <> head) tail
                        then Some (typedefof<_ Set>.MakeGenericType [|head|])
                        else None
                    | [] -> None
                | (false, _) -> None
            | Scripting.Table values ->
                let keyOpts = List.map World.tryGetType (Map.toKeyList values)
                match List.definitizePlus keyOpts with
                | (true, keyTypes) ->
                    match keyTypes with
                    | keyHead :: keyTail ->
                        if List.notExists (fun item -> item <> keyHead) keyTail then
                            let valOpts = List.map World.tryGetType (Map.toValueList values)
                            match List.definitizePlus valOpts with
                            | (true, valTypes) ->
                                match valTypes with
                                | valHead :: valTail ->
                                    if List.notExists (fun item -> item <> valHead) valTail
                                    then Some (typedefof<Map<_, _>>.MakeGenericType [|keyHead; valHead|])
                                    else None
                                | [] -> None
                            | (false, _) -> None
                        else None
                    | [] -> None
                | (false, _) -> None
            | _ -> None

        /// Attempt to evaluate the scripting prelude.
        static member tryEvalPrelude world =
            let oldLocalFrame = World.getLocalFrame world
            let oldScriptContext = World.getScriptContext world
            let globalFrame = World.getGlobalFrame world
            World.setLocalFrame globalFrame world
            match World.tryEvalScript Assets.PreludeFilePath world with
            | Right struct (scriptStr, _, world) ->
                World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Right struct (scriptStr, world)
            | Left struct (error, world) ->
                World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Left struct (error, world)

        static member internal evalV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Single x; Single y|], world) -> struct (Pluggable { Vector2 = Vector2 (x, y) }, world)
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the both arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalIndexV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2Pluggable as v2 ->
                    match indexer with
                    | "X" -> struct (Single v2.Vector2.X, world)
                    | "Y" -> struct (Single v2.Vector2.Y, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2 index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Single s; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2Pluggable as v2 ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector2 = Vector2 (s, v2.Vector2.Y) }, world)
                    | "Y" -> struct (Pluggable { Vector2 = Vector2 (v2.Vector2.X, s) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2 target, a keyword index of X or Y, and a Single value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Single x; Single y; Single z; Single w|], world) -> struct (Pluggable { Vector4 = Vector4 (x, y, z, w) }, world)
            | struct ([|_; _; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the all arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 4 arguments required.", originOpt), world)

        static member internal evalIndexV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector4Pluggable as v4 ->
                    match indexer with
                    | "X" -> struct (Single v4.Vector4.X, world)
                    | "Y" -> struct (Single v4.Vector4.Y, world)
                    | "Z" -> struct (Single v4.Vector4.Z, world)
                    | "W" -> struct (Single v4.Vector4.W, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V4.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V4 index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Single s; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector4Pluggable as v4 ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector4 = Vector4 (s, v4.Vector4.Y, v4.Vector4.Z, v4.Vector4.W) }, world)
                    | "Y" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, s, v4.Vector4.Z, v4.Vector4.W) }, world)
                    | "Z" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, v4.Vector4.Y, s, v4.Vector4.W) }, world)
                    | "W" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, v4.Vector4.Y, v4.Vector4.Z, s) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V4.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V4 target, a keyword index of X or Y, and an Int value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Int x; Int y|], world) -> struct (Pluggable { Vector2i = Vector2i (x, y) }, world)
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an Int for the both arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalIndexV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2iPluggable as v2i ->
                    match indexer with
                    | "X" -> struct (Int v2i.Vector2i.X, world)
                    | "Y" -> struct (Int v2i.Vector2i.Y, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2i.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2i index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Int i; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2iPluggable as v2i ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector2i = Vector2i (i, v2i.Vector2i.Y) }, world)
                    | "Y" -> struct (Pluggable { Vector2i = Vector2i (v2i.Vector2i.X, i) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2i.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2i target, a keyword index of X or Y, and an Int value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalGetExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation|], world)
            | struct ([|Keyword propertyName; relation|], world) -> World.evalGet propertyName (Some relation) originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName|], world)
            | struct ([|Keyword propertyName|], world) -> World.evalGet propertyName None originOpt world
            | struct ([|_|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 or 2 arguments required.", originOpt), world)

        static member internal evalSetExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation; value|], world)
            | struct ([|Keyword propertyName; relation; value|], world) -> World.evalSet propertyName (Some relation) value originOpt world
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; value|], world)
            | struct ([|Keyword propertyName; value|], world) -> World.evalSet propertyName None value originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, a value for the second, and an optional Relation for the third.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 or 3 arguments required.", originOpt), world)

        static member internal evalMonitorExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2|], world) -> World.evalMonitor fnName evaled evaled2 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            
        static member internal tryGetExtrinsic fnName =
            match Extrinsics.TryGetValue fnName with
            | (true, extrinsic) -> FOption.some extrinsic
            | (false, _) ->
                match Bindings.TryGetValue fnName with
                | (true, binding) -> FOption.some binding
                | (false, _) -> FOption.none ()

        static member internal initScripting () =
            let extrinsics =
                [("v2", World.evalV2Extrinsic)
                 ("index_Vector2", World.evalIndexV2Extrinsic)
                 ("update_Vector2", World.evalUpdateV2Extrinsic)
                 ("v4", World.evalV4Extrinsic)
                 ("index_Vector4", World.evalIndexV4Extrinsic)
                 ("update_Vector4", World.evalUpdateV4Extrinsic)
                 ("v2i", World.evalV2iExtrinsic)
                 ("index_Vector2i", World.evalIndexV2iExtrinsic)
                 ("update_Vector2i", World.evalUpdateV2iExtrinsic)
                 ("get", World.evalGetExtrinsic)
                 ("set", World.evalSetExtrinsic)
                 ("monitor", World.evalMonitorExtrinsic)] |>
                dictPlus
            Extrinsics <- extrinsics