// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open OpenTK
open Prime
open Nu
open Nu.Scripting
open Nu.WorldScriptingUnary.Scripting
open Nu.WorldScriptingBinary.Scripting
open Nu.WorldScriptingMarshalling.Scripting
open Nu.WorldScriptingPrimitives.Scripting
#nowarn "22"
#nowarn "40"

[<AutoOpen>]
module WorldScripting =

    type World with

        static member private getLocalDeclaration world =
            World.getScriptEnvBy EnvModule.Env.getLocalDeclaration world

        static member private setLocalDeclaration localDeclaration world =
            World.updateScriptEnv (EnvModule.Env.setLocalDeclaration localDeclaration) world

        static member private tryGetBinding name cachedBinding world =
            World.getScriptEnvBy (EnvModule.Env.tryGetBinding name cachedBinding) world

        static member private addDeclarationBinding name value world =
            World.updateScriptEnv (EnvModule.Env.addDeclarationBinding name value) world

        static member private addProceduralBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBinding appendType name value) world

        static member private addProceduralBindings appendType bindings world =
            World.updateScriptEnv (EnvModule.Env.addProceduralBindings appendType bindings) world

        static member private addBinding appendType name value world =
            World.updateScriptEnv (EnvModule.Env.addBinding appendType name value) world

        static member private removeProceduralBindings world =
            World.updateScriptEnv EnvModule.Env.removeProceduralBindings world

        static member private getProceduralFrames world =
            World.getScriptEnvBy EnvModule.Env.getProceduralFrames world

        static member private setProceduralFrames proceduralFrames world =
            World.updateScriptEnv (EnvModule.Env.setProceduralFrames proceduralFrames) world

        static member private getGlobalFrame world =
            World.getScriptEnvBy EnvModule.Env.getGlobalFrame world

        static member internal getLocalFrame world =
            World.getScriptEnvBy EnvModule.Env.getLocalFrame world

        static member internal setLocalFrame localFrame world =
            World.updateScriptEnv (EnvModule.Env.setLocalFrame localFrame) world

        /// Attempt to evaluate the scripting prelude.
        static member tryEvalPrelude world =
            try let prelude =
                    File.ReadAllText Assets.PreludeFilePath |>
                    String.unescape |>
                    (fun str -> Symbol.OpenSymbolsStr + str + Symbol.CloseSymbolsStr) |>
                    scvalue<Scripting.Expr list>
                let globalFrame = World.getGlobalFrame world
                let (_, world) = World.evalManyWithLogging prelude globalFrame Simulants.Game world
                Right world
            with exn ->
                Left ("Could not evaluate due to: " + scstring exn, World.choose world)

    module Scripting =

        let rec Intrinsics =
            dictPlus
                [("=", evalBinary EqFns)
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
                 ("!", evalSinglet evalDereference)
                 ("not", evalBoolUnary not)
                 ("toEmpty", evalUnary ToEmptyFns)
                 ("toIdentity", evalUnary ToIdentityFns)
                 ("toMin", evalUnary ToMinFns)
                 ("toMax", evalUnary ToMaxFns)
                 ("inc", evalUnary IncFns)
                 ("dec", evalUnary DecFns)
                 ("negate", evalUnary NegateFns)
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
                 ("keyname", evalSinglet evalKeyname)
                 ("keyfields", evalSinglet evalKeyfields)
                 ("v2", evalDoublet evalV2)
                 ("xOf", evalSinglet (evalNth5 0))
                 ("yOf", evalSinglet (evalNth5 1))
                 ("xAs", evalDoublet (evalNthAs5 0))
                 ("yAs", evalDoublet (evalNthAs5 1))
                 ("tuple", evalTuple)
                 ("pair", evalTuple)
                 ("fst", evalSinglet (evalNth5 0))
                 ("snd", evalSinglet (evalNth5 1))
                 ("thd", evalSinglet (evalNth5 2))
                 ("fth", evalSinglet (evalNth5 3))
                 ("fif", evalSinglet (evalNth5 4))
                 ("nth", evalDoublet evalNth)
                 ("fstAs", evalDoublet (evalNthAs5 0))
                 ("sndAs", evalDoublet (evalNthAs5 1))
                 ("thdAs", evalDoublet (evalNthAs5 2))
                 ("fthAs", evalDoublet (evalNthAs5 3))
                 ("fifAs", evalDoublet (evalNthAs5 4))
                 ("nthAs", evalTriplet evalNthAs)
                 ("some", evalSinglet evalSome)
                 ("Some", evalSinglet evalSome)
                 ("isNone", evalSinglet evalIsNone)
                 ("isSome", evalSinglet evalIsSome)
                 ("isEmpty", evalSinglet (evalIsEmpty evalApply))
                 ("notEmpty", evalSinglet (evalNotEmpty evalApply))
                 ("tryUncons", evalSinglet (evalTryUncons evalApply))
                 ("uncons", evalSinglet (evalUncons evalApply))
                 ("cons", evalDoublet evalCons)
                 ("commit", evalSinglet evalCommit)
                 ("tryHead", evalSinglet (evalTryHead evalApply))
                 ("head", evalSinglet (evalHead evalApply))
                 ("tryTail", evalSinglet (evalTryTail evalApply))
                 ("tail", evalSinglet (evalTail evalApply))
                 ("scanWhile", evalTriplet (evalScanWhile evalApply))
                 ("scani", evalTriplet (evalScani evalApply))
                 ("scan", evalTriplet (evalScan evalApply))
                 ("foldWhile", evalTriplet (evalFoldWhile evalApply))
                 ("foldi", evalTriplet (evalFoldi evalApply))
                 ("fold", evalTriplet (evalFold evalApply))
                 ("mapi", evalDoublet (evalMapi evalApply))
                 ("map", evalDoublet (evalMap evalApply))
                 ("contains", evalDoublet (evalContains evalApply))
                 ("codata", evalDoublet evalCodata)
                 ("toCodata", evalSinglet evalToCodata)
                 ("list", evalList)
                 //("toList", evalSinglet evalToList) TODO
                 ("ring", evalRing)
                 //("toRing", evalSinglet evalToRing) TODO
                 ("add", evalDoublet evalCons)
                 ("remove", evalDoublet evalRemove)
                 ("table", evalTable)
                 //("toTable", evalSinglet evalToTable) TODO
                 ("tryFind", evalDoublet evalTryFind)
                 ("find", evalDoublet evalFind)
                 ("monitor", evalDoublet evalMonitor)
                 ("simulantExists", evalSinglet evalSimulantExists)]

        and isIntrinsic name =
            Intrinsics.ContainsKey name

        and evalIntrinsic fnName originOpt evaledArgs world =
            match Intrinsics.TryGetValue fnName with
            | (true, intrinsic) -> intrinsic fnName originOpt evaledArgs world
            | (false, _) -> (Violation (["InvalidFunctionTargetBinding"], "Cannot apply a non-existent binding.", originOpt), world)

        and evalSimulantExists fnName originOpt evaledArg world =
            match evaledArg with
            | String str
            | Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDeriveSimulant address with
                | Some simulant -> (Bool (World.simulantExists simulant world), world)
                | None -> (Bool false, world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidArgumentType"], "Function '" + fnName + "' requires 1 relation argument.", originOpt), world)

        and evalBinding expr name cachedBinding originOpt world =
            match World.tryGetBinding name cachedBinding world with
            | None ->
                if isIntrinsic name then (expr, world)
                else (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "' ", originOpt), world)
            | Some binding -> (binding, world)

        and evalApply (exprs : Expr array) originOpt world =
            match evalMany exprs world with
            | (evaledHead :: evaledTail, world) ->
                match evaledHead with
                | Keyword keyword ->
                    let keyphrase = Keyphrase (keyword, List.toArray evaledTail)
                    (keyphrase, world)
                | Binding (fnName, _, originOpt) ->
                    // NOTE: we can infer we have an intrinsic when evaluation leads here
                    evalIntrinsic fnName originOpt evaledTail world
                | Fun (pars, parsCount, body, _, framesOpt, originOpt) ->
                    let (framesCurrentOpt, world) =
                        match framesOpt with
                        | Some frames ->
                            let framesCurrent =  World.getProceduralFrames world
                            let world = World.setProceduralFrames (frames :?> ProceduralFrame list) world
                            (Some framesCurrent, world)
                        | None -> (None, world)
                    let (evaled, world) =
                        let evaledArgs = Array.ofList evaledTail
                        if evaledArgs.Length = parsCount then
                            let bindings = Array.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                            let world = World.addProceduralBindings (AddToNewFrame parsCount) bindings world
                            let (evaled, world) = eval body world
                            (evaled, World.removeProceduralBindings world)
                        else (Violation (["MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), world)
                    match framesCurrentOpt with
                    | Some framesCurrent ->
                        let world = World.setProceduralFrames framesCurrent world
                        (evaled, world)
                    | None -> (evaled, world)
                | Violation _ as error -> (error, world)
                | _ -> (Violation (["MalformedApplication"], "Cannot apply a non-binding.", originOpt), world)
            | ([], world) -> (Unit, world)

        and evalApplyAnd exprs originOpt world =
            match exprs with
            | [|left; right|] ->
                match eval left world with
                | (Bool false, world) -> (Bool false, world)
                | (Bool true, world) ->
                    match eval right world with
                    | (Bool _, _) as result -> result
                    | (Violation _, _) as error -> error
                    | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
                | (Violation _, _) as error -> error
                | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
            | _ -> (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

        and evalApplyOr exprs originOpt world =
            match exprs with
            | [|left; right|] ->
                match eval left world with
                | (Bool true, world) -> (Bool true, world)
                | (Bool false, world) ->
                    match eval right world with
                    | (Bool _, _) as result -> result
                    | (Violation _, _) as error -> error
                    | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
                | (Violation _, _) as error -> error
                | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-bool values.", originOpt), world)
            | _ -> (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

        and evalLet4 binding body originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let (evaled, world) = eval body world
                    World.addProceduralBinding (AddToNewFrame 1) name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame 1) name fn world
            let (evaled, world) = eval body world
            (evaled, World.removeProceduralBindings world)

        and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
            let world =
                match bindingsHead with
                | VariableBinding (name, body) ->
                    let (bodyValue, world) = eval body world
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                    World.addProceduralBinding (AddToNewFrame bindingsCount) name fn world
            let world =
                List.foldi (fun i world binding ->
                    match binding with
                    | VariableBinding (name, body) ->
                        let (bodyValue, world) = eval body world
                        World.addProceduralBinding (AddToHeadFrame ^ inc i) name bodyValue world
                    | FunctionBinding (name, args, body) ->
                        let frames = World.getProceduralFrames world :> obj
                        let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
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
            | [] -> (Violation (["MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)

        and evalFun fn pars parsCount body framesPushed framesOpt originOpt world =
            if not framesPushed then
                if Option.isNone framesOpt then
                    let frames = World.getProceduralFrames world :> obj
                    (Fun (pars, parsCount, body, true, Some frames, originOpt), world)
                else (Fun (pars, parsCount, body, true, framesOpt, originOpt), world)
            else (fn, world)

        and evalIf condition consequent alternative originOpt world =
            match eval condition world with
            | (Bool bool, world) -> if bool then eval consequent world else eval alternative world
            | (Violation _ as evaled, world) -> (evaled, world)
            | (_, world) -> (Violation (["InvalidIfCondition"], "Must provide an expression that evaluates to a bool in an if condition.", originOpt), world)

        and evalMatch input (cases : (Expr * Expr) array) originOpt world =
            let (input, world) = eval input world
            let resultEir =
                Seq.foldUntilRight (fun world (condition, consequent) ->
                    let (evaledInput, world) = eval condition world
                    match evalBinaryInner EqFns "=" originOpt input evaledInput world with
                    | (Bool true, world) -> Right (eval consequent world)
                    | (Bool false, world) -> Left world
                    | (Violation _, world) -> Right (evaledInput, world)
                    | _ -> failwithumf ())
                    (Left world)
                    cases
            match resultEir with
            | Right success -> success
            | Left world -> (Violation (["InexhaustiveMatch"], "A match expression failed to meet any of its cases.", originOpt), world)

        and evalSelect exprPairs originOpt world =
            let resultEir =
                Seq.foldUntilRight (fun world (condition, consequent) ->
                    match eval condition world with
                    | (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                    | (Violation _ as evaled, world) -> Right (evaled, world)
                    | (_, world) -> Right ((Violation (["InvalidSelectCondition"], "Must provide an expression that evaluates to a bool in a case condition.", originOpt), world)))
                    (Left world)
                    exprPairs
            match resultEir with
            | Right success -> success
            | Left world -> (Violation (["InexhaustiveSelect"], "A select expression failed to meet any of its cases.", originOpt), world)

        and evalTry body handlers _ world =
            match eval body world with
            | (Violation (categories, _, _) as evaled, world) ->
                match
                    List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                        let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                        if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                        (Left world)
                        handlers with
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
                    match tryImport propertyValue propertyType with
                    | Some propertyValue -> (propertyValue, world)
                    | None -> (Violation (["InvalidPropertyValue"], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> (Violation (["InvalidProperty"], "Simulant or property value could not be found.", originOpt), world)
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
                    let (propertyValue, world) = eval propertyValueExpr world
                    match tryExport propertyValue propertyType with
                    | Some propertyValue ->
                        match World.trySetSimulantProperty propertyName (propertyValue, propertyType) simulant world with
                        | (true, world) -> (Unit, world)
                        | (false, world) -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
                    | None -> (Violation (["InvalidPropertyValue"], "Property value could not be exported into simulant property.", originOpt), world)
                | None -> (Violation (["InvalidProperty"], "Property value could not be set.", originOpt), world)
            | Left error -> error

        and evalDefine binding originOpt world =
            let world =
                match binding with
                | VariableBinding (name, body) ->
                    let (evaled, world) = eval body world
                    World.addDeclarationBinding name evaled world
                | FunctionBinding (name, args, body) ->
                    let frames = World.getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
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
            | Codata _ -> (expr, world)
            | List _ -> (expr, world)
            | Ring _ -> (expr, world)
            | Table _ -> (expr, world)
            | Binding (name, cachedBinding, originOpt) as expr -> evalBinding expr name cachedBinding originOpt world
            | Apply (exprs, originOpt) -> evalApply exprs originOpt world
            | ApplyAnd (exprs, originOpt) -> evalApplyAnd exprs originOpt world
            | ApplyOr (exprs, originOpt) -> evalApplyOr exprs originOpt world
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
            | Quote _ as quote -> (quote, world)
            | Define (binding, originOpt) -> evalDefine binding originOpt world

        and evalMany (exprs : Expr seq) world =
            let (evaledsRev, world) =
                Seq.fold
                    (fun (evaleds, world) expr ->
                        let (evaled, world) = eval expr world
                        (evaled :: evaleds, world))
                    ([], world)
                    exprs
            (List.rev evaledsRev, world)