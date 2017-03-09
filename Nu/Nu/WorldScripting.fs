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
            | _ -> (Violation (["InvalidArgumentType"], "Function '" + fnName + "' requires 1 relation argument.", originOpt), world)

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
                    | (_, world) -> Left (Violation (["InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match ScriptingWorld.tryImport propertyValue propertyType with
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
                    | (_, world) -> Left (Violation (["InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    let (propertyValue, world) = World.evalInternal propertyValueExpr world
                    match ScriptingWorld.tryExport propertyValue propertyType with
                    | Some propertyValue ->
                        match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                        | (true, world) -> (Unit, world)
                        | (false, world) -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
                    | None -> (Violation (["InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left error -> error

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
            | "v2" | "xOf" | "yOf" | "xAs" | "yAs"
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
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a single for the both arguments.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "xOf" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v|], world) -> (v, world)
                | ([|Pluggable pluggable|], world) ->
                    match pluggable with
                    | :? Vector2Pluggable as v2 -> (Single v2.Vector2.X, world)
                    | _ -> failwithumf ()
                | ([|_|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a single for the both arguments.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", originOpt), world)
            | "yOf" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v|], world) -> (v, world)
                | ([|Pluggable pluggable|], world) ->
                    match pluggable with
                    | :? Vector2Pluggable as v2 -> (Single v2.Vector2.Y, world)
                    | _ -> failwithumf ()
                | ([|_|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a single for the both arguments.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 1 argument required.", originOpt), world)
            | "xAs" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|Single x; Pluggable pluggable|], world) ->
                    match pluggable with
                    | :? Vector2Pluggable as v2 -> (Pluggable { Vector2 = Vector2 (x, v2.Vector2.Y) }, world)
                    | _ -> failwithumf ()
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a single for the first argument, and a v2 for the second.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "yAs" ->
                match World.evalManyInternal exprs world with
                | ([|Violation _ as v; _|], world) -> (v, world)
                | ([|_; Violation _ as v|], world) -> (v, world)
                | ([|Single y; Pluggable pluggable|], world) ->
                    match pluggable with
                    | :? Vector2Pluggable as v2 -> (Pluggable { Vector2 = Vector2 (v2.Vector2.X, y) }, world)
                    | _ -> failwithumf ()
                | ([|_; _|], world) -> (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a single for the first argument, and a v2 for the second.", originOpt), world)
                | (_, world) -> (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)
            | "get" -> failwithnie ()
            | "set" -> failwithnie ()
            | "monitor" -> failwithnie ()
            | _ -> failwithumf ()