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

        static member internal evalInternal expr world : (Expr * World) =
            ScriptingWorld.eval expr world

        static member internal evalManyInternal exprs world : (Expr array * World) =
            ScriptingWorld.evalMany exprs world

        static member internal evalWithLoggingInternal expr world : (Expr * World) =
            ScriptingWorld.evalWithLogging expr world

        static member internal evalManyWithLoggingInternal exprs world : (Expr array * World) =
            ScriptingWorld.evalManyWithLogging exprs world

        static member internal evalSimulantExists fnName originOpt evaledArg world =
            match evaledArg with
            | String str
            | Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDeriveSimulant address with
                | Some simulant -> (Bool (World.getSimulantExists simulant world), world)
                | None -> (Bool false, world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"], "Function '" + fnName + "' requires 1 Relation argument.", originOpt), world)

        static member internal evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation (["InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt), world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation (["InvalidPropertyRelation"], "Relation must be either a String or Keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match ScriptingWorld.tryImport propertyValue propertyType world with
                    | Some propertyValue -> (propertyValue, world)
                    | None -> (Violation (["InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> (Violation (["InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        static member internal evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | (String str, world)
                    | (Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Violation (["InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt), world)
                    | (Violation _, _) as error -> Left error
                    | (_, world) -> Left (Violation (["InvalidPropertyRelation"], "Relation must be either a String or Keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    let (propertyValue, world) = World.evalInternal propertyValueExpr world
                    match ScriptingWorld.tryExport propertyValue propertyType world with
                    | Some propertyValue ->
                        match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                        | (true, world) -> (Unit, world)
                        | (false, world) -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
                    | None -> (Violation (["InvalidPropertyValue"], "Property value could not be exported into Simulant property.", originOpt), world)
                | None -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        static member evalMonitor5 subscription (eventAddress : obj Address) subscriber world =
            EventWorld.subscribe (fun evt world ->
                match World.tryGetSimulantScriptFrame subscriber world with
                | Some scriptFrame ->
                    match ScriptingWorld.tryImport evt.Data evt.DataType world with
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

        static member evalMonitor6 fnName originOpt evaledArg evaledArg2 world =
            match evaledArg with
            | Binding _
            | Fun _ ->
                match evaledArg2 with
                | String str
                | Keyword str ->
                    let world = World.evalMonitor5 evaledArg (Address.makeFromString str) (World.getScriptContext world) world
                    (Unit, world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Relation for its 2nd argument.", originOpt), world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Function for its 1st argument.", originOpt), world)

        static member evalMonitor fnName originOpt evaledArg evaledArg2 world =
            World.evalMonitor6 fnName originOpt evaledArg evaledArg2 world

        /// Attempt to evaluate the scripting prelude.
        static member tryEvalPrelude world =
            let oldLocalFrame = World.getLocalFrame world
            let oldScriptContext = World.getScriptContext world
            let globalFrame = World.getGlobalFrame world
            let world = World.setLocalFrame globalFrame world
            match World.tryEvalScript Assets.PreludeFilePath world with
            | Right (scriptStr, _, world) ->
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Right (scriptStr, world)
            | Left (error, world) ->
                let world = World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Left (error, world)

        static member internal isExtrinsic fnName =
            match fnName with
            // TODO: | "tickRate" | "tickTime" | "getSimulantSelected" | "simulantExists"
            | "v2" | "of_Vector2"
            | "get" | "set"
            | "monitor" -> true
            | _ -> false

        static member internal evalExtrinsic fnName originOpt exprs world =
            match fnName with
            | "v2" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|Single x; Single y|], world) -> (Pluggable { Vector2 = Vector2 (x, y) }, world)
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the both arguments.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "of_Vector2" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|Keyword indexer; Pluggable pluggable|], world) ->
                    match pluggable with
                    | :? Vector2Pluggable as v2 ->
                        match indexer with
                        | "X" -> (Single v2.Vector2.X, world)
                        | "Y" -> (Single v2.Vector2.Y, world)
                        | _ -> (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for Vector2.", originOpt), world)
                    | _ -> failwithumf ()
                | ([|_|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the both arguments.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "get" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|String propertyName; relation|], world)
                | ([|Keyword propertyName; relation|], world) -> World.evalGet propertyName (Some relation) originOpt world
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
                | ([|Violation _ as v|], world) -> (v, world)
                | ([|String propertyName|], world)
                | ([|Keyword propertyName|], world) -> World.evalGet propertyName None originOpt world
                | ([|_|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 or 2 arguments required.", originOpt), world)
            | "set" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _; _|], world) -> (v, world)
                | ([|_; Violation _ as v; _|], world) -> (v, world)
                | ([|_; _; Violation _ as v|], world) -> (v, world)
                | ([|String propertyName; relation; value|], world)
                | ([|Keyword propertyName; relation; value|], world) -> World.evalSet propertyName (Some relation) value originOpt world
                | ([|_; _; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|String propertyName; value|], world)
                | ([|Keyword propertyName; value|], world) -> World.evalSet propertyName None value originOpt world
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, a value for the second, and an optional Relation for the third.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 or 3 arguments required.", originOpt), world)
            | "monitor" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|evaled; evaled2|], world) -> World.evalMonitor fnName originOpt evaled evaled2 world
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | _ -> failwithumf ()