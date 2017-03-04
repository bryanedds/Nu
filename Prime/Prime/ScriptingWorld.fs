// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Prime
open Prime.Scripting
open Prime.ScriptingUnary
open Prime.ScriptingBinary
open Prime.ScriptingMarshalling
open Prime.ScriptingPrimitives

/// The context in which scripting takes place. Effectively a mix-in for the 'w type, where 'w is a type that
/// represents the client program.
type 'w ScriptingWorld =
    interface
        abstract member GetEnv : unit -> Env
        abstract member UpdateEnv : (Env -> Env) -> 'w
        abstract member UpdateEnvPlus : (Env -> 'a * Env) -> 'a * 'w
        abstract member EvalExtrinsic : string -> SymbolOrigin option -> Expr array -> 'w ScriptingWorld -> Expr option * 'w ScriptingWorld
        abstract member TryImport : obj -> Type -> Expr option
        abstract member TryExport : Expr -> Type -> obj option
        end

[<RequireQualifiedAccess>]
module ScriptingWorld =

    let tryGetBinding<'w when 'w :> 'w ScriptingWorld> name cachedBinding (world : 'w) =
        EnvModule.Env.tryGetBinding name cachedBinding (world.GetEnv ())

    let tryAddDeclarationBinding<'w when 'w :> 'w ScriptingWorld> name value (world : 'w) =
        world.UpdateEnvPlus (EnvModule.Env.tryAddDeclarationBinding name value)

    let addProceduralBinding<'w when 'w :> 'w ScriptingWorld> appendType name value (world : 'w) =
        world.UpdateEnv (EnvModule.Env.addProceduralBinding appendType name value)

    let addProceduralBindings<'w when 'w :> 'w ScriptingWorld> appendType bindings (world : 'w) =
        world.UpdateEnv (EnvModule.Env.addProceduralBindings appendType bindings)

    let removeProceduralBindings<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        world.UpdateEnv (EnvModule.Env.removeProceduralBindings)

    let getProceduralFrames<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        EnvModule.Env.getProceduralFrames (world.GetEnv ())

    let setProceduralFrames<'w when 'w :> 'w ScriptingWorld> proceduralFrames (world : 'w) =
        world.UpdateEnv (EnvModule.Env.setProceduralFrames proceduralFrames)

    let getGlobalFrame<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        EnvModule.Env.getGlobalFrame (world.GetEnv ())

    let getLocalFrame<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        EnvModule.Env.getLocalFrame (world.GetEnv ())

    let setLocalFrame<'w when 'w :> 'w ScriptingWorld> localFrame (world : 'w) =
        world.UpdateEnv (EnvModule.Env.setLocalFrame localFrame)

    let isIntrinsic fnName =
        match fnName with
        | "=" | "<>" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "/" | "%" | "!"
        | "not" | "toEmpty" | "toIdentity" | "toMin" | "toMax"
        | "inc" | "dec" | "negate" | "hash"
        | "pow" | "root" | "sqr" | "sqrt"
        | "floor" | "ceiling" | "truncate" | "round" | "exp" | "log"
        | "sin" | "cos" | "tan" | "asin" | "acos" | "atan"
        | "length" | "normal" | "cross" | "dot"
        | "bool" | "int" | "int64" | "single" | "double" | "string"
        | "keyname" | "keyfields"
        | "tuple" | "pair" | "fst" | "snd" | "thd" | "fth" | "fif" | "nth"
        | "fstAs" | "sndAs" | "thdAs" | "fthAs" | "fifAs" | "nthAs"
        | "some" | "isNone" | "isSome" | "isEmpty" | "notEmpty"
        | "tryUncons" | "uncons" | "cons" | "commit" | "tryHead" | "head" | "tryTail" | "tail"
        | "scanWhile" | "scani" | "scan" | "foldWhile" | "foldi" | "fold" | "mapi" | "map" | "contains"
        | "codata" | "toCodata"
        | "list" (*| "toList"*)
        | "ring" (*| "toRing"*) | "add" | "remove"
        | "table" (*| "toTable"*) | "tryFind" | "find" -> true
        | _ -> false

    let rec evalIntrinsic fnName originOpt evaledArgs world =
        match fnName with
        | "=" -> evalBinary EqFns fnName originOpt evaledArgs world
        | "<>" -> evalBinary NotEqFns fnName originOpt evaledArgs world
        | "<" -> evalBinary LtFns fnName originOpt evaledArgs world
        | ">" -> evalBinary GtFns fnName originOpt evaledArgs world
        | "<=" -> evalBinary LtEqFns fnName originOpt evaledArgs world
        | ">=" -> evalBinary GtEqFns fnName originOpt evaledArgs world
        | "+" -> evalBinary AddFns fnName originOpt evaledArgs world
        | "-" -> evalBinary SubFns fnName originOpt evaledArgs world
        | "*" -> evalBinary MulFns fnName originOpt evaledArgs world
        | "/" -> evalBinary DivFns fnName originOpt evaledArgs world
        | "%" -> evalBinary ModFns fnName originOpt evaledArgs world
        | "!" -> evalSinglet evalDereference fnName originOpt evaledArgs world
        | "not" -> evalBoolUnary not fnName originOpt evaledArgs world
        | "toEmpty" -> evalUnary ToEmptyFns fnName originOpt evaledArgs world
        | "toIdentity" -> evalUnary ToIdentityFns fnName originOpt evaledArgs world
        | "toMin" -> evalUnary ToMinFns fnName originOpt evaledArgs world
        | "toMax" -> evalUnary ToMaxFns fnName originOpt evaledArgs world
        | "inc" -> evalUnary IncFns fnName originOpt evaledArgs world
        | "dec" -> evalUnary DecFns fnName originOpt evaledArgs world
        | "negate" -> evalUnary NegateFns fnName originOpt evaledArgs world
        | "hash" -> evalUnary HashFns fnName originOpt evaledArgs world
        | "pow" -> evalBinary PowFns fnName originOpt evaledArgs world
        | "root" -> evalBinary RootFns fnName originOpt evaledArgs world
        | "sqr" -> evalUnary SqrFns fnName originOpt evaledArgs world
        | "sqrt" -> evalUnary SqrtFns fnName originOpt evaledArgs world
        | "floor" -> evalUnary FloorFns fnName originOpt evaledArgs world
        | "ceiling" -> evalUnary CeilingFns fnName originOpt evaledArgs world
        | "truncate" -> evalUnary TruncateFns fnName originOpt evaledArgs world
        | "round" -> evalUnary RoundFns fnName originOpt evaledArgs world
        | "exp" -> evalUnary ExpFns fnName originOpt evaledArgs world
        | "log" -> evalUnary LogFns fnName originOpt evaledArgs world
        | "sin" -> evalUnary SinFns fnName originOpt evaledArgs world
        | "cos" -> evalUnary CosFns fnName originOpt evaledArgs world
        | "tan" -> evalUnary TanFns fnName originOpt evaledArgs world
        | "asin" -> evalUnary AsinFns fnName originOpt evaledArgs world
        | "acos" -> evalUnary AcosFns fnName originOpt evaledArgs world
        | "atan" -> evalUnary AtanFns fnName originOpt evaledArgs world
        | "length" -> evalUnary LengthFns fnName originOpt evaledArgs world
        | "normal" -> evalUnary NormalFns fnName originOpt evaledArgs world
        | "cross" -> evalBinary CrossFns fnName originOpt evaledArgs world
        | "dot" -> evalBinary DotFns fnName originOpt evaledArgs world
        | "bool" -> evalUnary BoolFns fnName originOpt evaledArgs world
        | "int" -> evalUnary IntFns fnName originOpt evaledArgs world
        | "int64" -> evalUnary Int64Fns fnName originOpt evaledArgs world
        | "single" -> evalUnary SingleFns fnName originOpt evaledArgs world
        | "double" -> evalUnary DoubleFns fnName originOpt evaledArgs world
        | "string" -> evalUnary StringFns fnName originOpt evaledArgs world
        | "keyname" -> evalSinglet evalKeyname fnName originOpt evaledArgs world
        | "keyfields" -> evalSinglet evalKeyfields fnName originOpt evaledArgs world
        | "xOf" -> evalSinglet (evalNth5 0) fnName originOpt evaledArgs world
        | "yOf" -> evalSinglet (evalNth5 1) fnName originOpt evaledArgs world
        | "xAs" -> evalDoublet (evalNthAs5 0) fnName originOpt evaledArgs world
        | "yAs" -> evalDoublet (evalNthAs5 1) fnName originOpt evaledArgs world
        | "tuple" -> evalTuple fnName originOpt evaledArgs world
        | "pair" -> evalTuple fnName originOpt evaledArgs world
        | "fst" -> evalSinglet (evalNth5 0) fnName originOpt evaledArgs world
        | "snd" -> evalSinglet (evalNth5 1) fnName originOpt evaledArgs world
        | "thd" -> evalSinglet (evalNth5 2) fnName originOpt evaledArgs world
        | "fth" -> evalSinglet (evalNth5 3) fnName originOpt evaledArgs world
        | "fif" -> evalSinglet (evalNth5 4) fnName originOpt evaledArgs world
        | "nth" -> evalDoublet evalNth fnName originOpt evaledArgs world
        | "fstAs" -> evalDoublet (evalNthAs5 0) fnName originOpt evaledArgs world
        | "sndAs" -> evalDoublet (evalNthAs5 1) fnName originOpt evaledArgs world
        | "thdAs" -> evalDoublet (evalNthAs5 2) fnName originOpt evaledArgs world
        | "fthAs" -> evalDoublet (evalNthAs5 3) fnName originOpt evaledArgs world
        | "fifAs" -> evalDoublet (evalNthAs5 4) fnName originOpt evaledArgs world
        | "nthAs" -> evalTriplet evalNthAs fnName originOpt evaledArgs world
        | "some" -> evalSinglet evalSome fnName originOpt evaledArgs world
        | "Some" -> evalSinglet evalSome fnName originOpt evaledArgs world
        | "isNone" -> evalSinglet evalIsNone fnName originOpt evaledArgs world
        | "isSome" -> evalSinglet evalIsSome fnName originOpt evaledArgs world
        | "isEmpty" -> evalSinglet (evalIsEmpty evalApply) fnName originOpt evaledArgs world
        | "notEmpty" -> evalSinglet (evalNotEmpty evalApply) fnName originOpt evaledArgs world
        | "tryUncons" -> evalSinglet (evalTryUncons evalApply) fnName originOpt evaledArgs world
        | "uncons" -> evalSinglet (evalUncons evalApply) fnName originOpt evaledArgs world
        | "cons" -> evalDoublet evalCons fnName originOpt evaledArgs world
        | "commit" -> evalSinglet evalCommit fnName originOpt evaledArgs world
        | "tryHead" -> evalSinglet (evalTryHead evalApply) fnName originOpt evaledArgs world
        | "head" -> evalSinglet (evalHead evalApply) fnName originOpt evaledArgs world
        | "tryTail" -> evalSinglet (evalTryTail evalApply) fnName originOpt evaledArgs world
        | "tail" -> evalSinglet (evalTail evalApply) fnName originOpt evaledArgs world
        | "scanWhile" -> evalTriplet (evalScanWhile evalApply) fnName originOpt evaledArgs world
        | "scani" -> evalTriplet (evalScani evalApply) fnName originOpt evaledArgs world
        | "scan" -> evalTriplet (evalScan evalApply) fnName originOpt evaledArgs world
        | "foldWhile" -> evalTriplet (evalFoldWhile evalApply) fnName originOpt evaledArgs world
        | "foldi" -> evalTriplet (evalFoldi evalApply) fnName originOpt evaledArgs world
        | "fold" -> evalTriplet (evalFold evalApply) fnName originOpt evaledArgs world
        | "mapi" -> evalDoublet (evalMapi evalApply) fnName originOpt evaledArgs world
        | "map" -> evalDoublet (evalMap evalApply) fnName originOpt evaledArgs world
        | "contains" -> evalDoublet (evalContains evalApply) fnName originOpt evaledArgs world
        | "codata" -> evalDoublet evalCodata fnName originOpt evaledArgs world
        | "toCodata" -> evalSinglet evalToCodata fnName originOpt evaledArgs world
        | "list" -> evalList fnName originOpt evaledArgs world
        //| "toList" -> evalSinglet evalToList fnName originOpt evaledArgs world TODO
        | "ring" -> evalRing fnName originOpt evaledArgs world
        //| "toRing" -> evalSinglet evalToRing fnName originOpt evaledArgs world TODO
        | "add" -> evalDoublet evalCons fnName originOpt evaledArgs world
        | "remove" -> evalDoublet evalRemove fnName originOpt evaledArgs world
        | "table" -> evalTable fnName originOpt evaledArgs world
        //| "toTable" -> evalSinglet evalToTable fnName originOpt evaledArgs world TODO
        | "tryFind" -> evalDoublet evalTryFind fnName originOpt evaledArgs world
        | "find" -> evalDoublet evalFind fnName originOpt evaledArgs world
        | _ -> (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalBinding expr name cachedBinding originOpt world =
        match tryGetBinding name cachedBinding world with
        | None ->
            if isIntrinsic name then (expr, world)
            else (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "'", originOpt), world)
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
                        let framesCurrent = getProceduralFrames world
                        let world = setProceduralFrames (frames :?> ProceduralFrame list) world
                        (Some framesCurrent, world)
                    | None -> (None, world)
                let (evaled, world) =
                    let evaledArgs = Array.ofList evaledTail
                    if evaledArgs.Length = parsCount then
                        let bindings = Array.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledArgs
                        let world = addProceduralBindings (AddToNewFrame parsCount) bindings world
                        let (evaled, world) = eval body world
                        (evaled, removeProceduralBindings world)
                    else (Violation (["MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), world)
                match framesCurrentOpt with
                | Some framesCurrent ->
                    let world = setProceduralFrames framesCurrent world
                    (evaled, world)
                | None -> (evaled, world)
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["MalformedApplication"], "Cannot apply the non-binding '" + scstring evaledHead + "'.", originOpt), world)
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
                addProceduralBinding (AddToNewFrame 1) name evaled world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                addProceduralBinding (AddToNewFrame 1) name fn world
        let (evaled, world) = eval body world
        (evaled, removeProceduralBindings world)

    and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
        let world =
            match bindingsHead with
            | VariableBinding (name, body) ->
                let (bodyValue, world) = eval body world
                addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                addProceduralBinding (AddToNewFrame bindingsCount) name fn world
        let world =
            List.foldi (fun i world binding ->
                match binding with
                | VariableBinding (name, body) ->
                    let (bodyValue, world) = eval body world
                    addProceduralBinding (AddToHeadFrame ^ inc i) name bodyValue world
                | FunctionBinding (name, args, body) ->
                    let frames = getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                    addProceduralBinding (AddToHeadFrame ^ inc i) name fn world)
                world
                bindingsTail
        let (evaled, world) = eval body world
        (evaled, removeProceduralBindings world)
        
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
                let frames = getProceduralFrames world :> obj
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

    and evalDefine binding originOpt world =
        let (bound, world) =
            match binding with
            | VariableBinding (name, body) ->
                let (evaled, world) = eval body world
                tryAddDeclarationBinding name evaled world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                tryAddDeclarationBinding name fn world
        if bound then (Unit, world)
        else (Violation (["InvalidDeclaration"], "Can make declarations only at the top-level.", None), world)

    /// Evaluate an expression.
    and eval expr world =
        match expr with
        | Violation _ -> (expr, world)
        | Unit _ -> (expr, world)
        | Bool _ -> (expr, world)
        | Int _ -> (expr, world)
        | Int64 _ -> (expr, world)
        | Single _ -> (expr, world)
        | Double _ -> (expr, world)
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
        | Quote _ as quote -> (quote, world)
        | Define (binding, originOpt) -> evalDefine binding originOpt world

    /// Evaluate a sequence of expressions.
    and evalMany (exprs : Expr seq) world =
        let (evaledsRev, world) =
            Seq.fold
                (fun (evaleds, world) expr ->
                    let (evaled, world) = eval expr world
                    (evaled :: evaleds, world))
                ([], world)
                exprs
        (List.rev evaledsRev, world)

    /// Attempt to evaluate a script.
    let tryEvalScript choose scriptFilePath world =
        try let scriptStr =
                scriptFilePath |>
                File.ReadAllText |>
                String.unescape
            let script =
                scriptStr |>
                (fun str -> Symbol.OpenSymbolsStr + str + Symbol.CloseSymbolsStr) |>
                scvalue<Scripting.Expr list>
            let (evaleds, world) = evalMany script world
            Right (scriptStr, evaleds, world)
        with exn ->
            Left ("Could not evaluate script due to: " + scstring exn, choose world)