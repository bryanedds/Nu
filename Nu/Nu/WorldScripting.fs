// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open OpenTK
open Prime
open Nu
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScripting =

    type World with

        static member internal evalInternal expr world : (Scripting.Expr * World) =
            ScriptingWorld.eval expr world

        static member internal evalManyInternal exprs world : (Scripting.Expr array * World) =
            ScriptingWorld.evalMany exprs world

        static member internal evalWithLoggingInternal expr world : (Scripting.Expr * World) =
            ScriptingWorld.evalWithLogging expr world

        static member internal evalManyWithLoggingInternal exprs world : (Scripting.Expr array * World) =
            ScriptingWorld.evalManyWithLogging exprs world

        static member internal evalSimulantExists fnName originOpt evaledArg world =
            match evaledArg with
            | Scripting.String str
            | Scripting.Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDeriveSimulant address with
                | Some simulant -> (Scripting.Bool (World.getSimulantExists simulant world), world)
                | None -> (Scripting.Bool false, world)
            | Scripting.Violation _ as error -> (error, world)
            | _ -> (Scripting.Violation (["InvalidArgumentType"], "Function '" + fnName + "' requires 1 relation argument.", originOpt), world)

        static member internal evalGet propertyName relationExprOpt originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | (Scripting.String str, world)
                    | (Scripting.Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Scripting.Violation (["InvalidPropertyRelation"], "Relation must have 0 to 3 names.", originOpt), world)
                    | (Scripting.Violation _, _) as error -> Left error
                    | (_, world) -> Left (Scripting.Violation (["InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (propertyValue, propertyType) ->
                    match ScriptingWorld.tryImport propertyValue propertyType with
                    | Some propertyValue -> (propertyValue, world)
                    | None -> (Scripting.Violation (["InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> (Scripting.Violation (["InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        static member internal evalSet propertyName relationExprOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            let simulantAndEnvEir =
                match relationExprOpt with
                | Some relationExpr ->
                    match World.evalInternal relationExpr world with
                    | (Scripting.String str, world)
                    | (Scripting.Keyword str, world) ->
                        let relation = Relation.makeFromString str
                        let address = Relation.resolve context.SimulantAddress relation
                        match World.tryDeriveSimulant address with
                        | Some simulant -> Right (simulant, world)
                        | None -> Left (Scripting.Violation (["InvalidPropertyRelation"], "Relation must have 0 to 3 parts.", originOpt), world)
                    | (Scripting.Violation _, _) as error -> Left error
                    | (_, world) -> Left (Scripting.Violation (["InvalidPropertyRelation"], "Relation must be either a string or a keyword.", originOpt), world)
                | None -> Right (context, world)
            match simulantAndEnvEir with
            | Right (simulant, world) ->
                match World.tryGetSimulantProperty propertyName simulant world with
                | Some (_, propertyType) ->
                    let (propertyValue, world) = World.evalInternal propertyValueExpr world
                    match ScriptingWorld.tryExport propertyValue propertyType with
                    | Some propertyValue ->
                        match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                        | (true, world) -> (Scripting.Unit, world)
                        | (false, world) -> (Scripting.Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
                    | None -> (Scripting.Violation (["InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Scripting.Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
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