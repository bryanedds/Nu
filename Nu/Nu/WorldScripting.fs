// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open OpenTK
open Prime
open Prime.Scripting
open Nu
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScripting =

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
                    match ScriptingWorld.tryExport property.PropertyType propertyValue world with
                    | Some propertyValue ->
                        let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                        match World.trySetSimulantProperty propertyName property simulant world with
                        | (true, world) -> struct (Unit, world)
                        | (false, world) -> struct (Violation (["InvalidProperty"; "Set"], "Property value could not be set.", originOpt), world)
                    | None -> struct (Violation (["InvalidPropertyValue"; "Set"], "Property value could not be exported into Simulant property.", originOpt), world)
                | None -> struct (Violation (["InvalidProperty"; "Set"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        static member evalMonitor5 subscription (eventAddress : obj Address) subscriber world =
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

        static member evalMonitor6 fnName evaledArg evaledArg2 originOpt world =
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

        static member evalMonitor fnName evaledArg evaledArg2 originOpt world =
            World.evalMonitor6 fnName evaledArg evaledArg2 originOpt world

        /// Attempt to evaluate the scripting prelude.
        static member tryEvalPrelude world =
            let oldLocalFrame = World.getLocalFrame world
            let oldScriptContext = World.getScriptContext world
            let globalFrame = World.getGlobalFrame world
            let world = World.setLocalFrame globalFrame world
            match World.tryEvalScript Assets.PreludeFilePath world with
            | Right struct (scriptStr, _, world) ->
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Right struct (scriptStr, world)
            | Left struct (error, world) ->
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Left struct (error, world)

        static member internal isExtrinsic fnName =
            match fnName with
            // TODO: | "tickRate" | "tickTime" | "getSimulantSelected" | "simulantExists"
            | "v2" | "index_Vector2" | "update_Vector2"
            | "get" | "set"
            | "monitor" -> true
            | _ -> false

        static member internal evalExtrinsic fnName =
            match fnName with
            | "v2" ->
                fun exprs originOpt world ->
                    match World.evalManyInternal exprs world with
                    | struct ([|Violation _ as v; _|], world) -> struct (v, world)
                    | struct ([|_; Violation _ as v|], world) -> struct (v, world)
                    | struct ([|Single x; Single y|], world) -> struct (Pluggable { Vector2 = Vector2 (x, y) }, world)
                    | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the both arguments.", originOpt), world)
                    | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "index_Vector2" ->
                fun exprs originOpt world ->
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
            | "update_Vector2" ->
                fun exprs originOpt world ->
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
            | "get" ->
                fun exprs originOpt world ->
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
            | "set" ->
                fun exprs originOpt world ->
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
            | "monitor" ->
                fun exprs originOpt world ->
                    match World.evalManyInternal exprs world with
                    | struct ([|Violation _ as v; _|], world) -> struct (v, world)
                    | struct ([|_; Violation _ as v|], world) -> struct (v, world)
                    | struct ([|evaled; evaled2|], world) -> World.evalMonitor fnName evaled evaled2 originOpt world
                    | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | _ -> fun _ _ _ -> failwithumf ()