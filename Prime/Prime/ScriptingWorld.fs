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
        abstract member IsExtrinsic : string -> bool
        abstract member EvalExtrinsic : string -> SymbolOrigin option -> Expr array -> Expr * 'w
        abstract member TryImport : obj -> Type -> Expr option
        abstract member TryExport : Expr -> Type -> obj option
        end

[<RequireQualifiedAccess>]
module ScriptingWorld =

    let inline annotateWorld<'w when 'w :> 'w ScriptingWorld> (_ : 'w) =
        () // NOTE: simply infers that a type is a world.

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

    let tryImport<'w when 'w :> 'w ScriptingWorld> value ty (world : 'w) =
        ScriptingMarshalling.tryImport world.TryImport value ty

    let tryExport<'w when 'w :> 'w ScriptingWorld> value ty (world : 'w) =
        ScriptingMarshalling.tryExport world.TryExport value ty

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
        | "getTypeName"
        | "tryIndex" | "hasIndex" | "index" | "tryUpdate" | "update" | "getName"
        | "tuple" | "pair" | "fst" | "snd" | "thd" | "fth" | "fif" | "nth"
        | "fstAs" | "sndAs" | "thdAs" | "fthAs" | "fifAs" | "nthAs"
        | "some" | "isNone" | "isSome" | "isEmpty" | "notEmpty"
        | "tryUncons" | "uncons" | "cons" | "commit" | "tryHead" | "head" | "tryTail" | "tail"
        | "scanWhile" | "scani" | "scan" | "foldWhile" | "foldi" | "fold" | "mapi" | "map" | "contains"
        | "toString"
        | "codata" | "toCodata"
        | "list" | "toList"
        | "ring" | "toRing" | "add" | "remove"
        | "table" | "toTable" -> true
        | _ -> false

    let rec internal evalIntrinsicInner<'w when 'w :> 'w ScriptingWorld> fnName evaledArgs originOpt (world : 'w) =
        match fnName with
        | "=" -> evalBinary EqFns fnName evaledArgs originOpt world
        | "<>" -> evalBinary NotEqFns fnName evaledArgs originOpt world
        | "<" -> evalBinary LtFns fnName evaledArgs originOpt world
        | ">" -> evalBinary GtFns fnName evaledArgs originOpt world
        | "<=" -> evalBinary LtEqFns fnName evaledArgs originOpt world
        | ">=" -> evalBinary GtEqFns fnName evaledArgs originOpt world
        | "+" -> evalBinary AddFns fnName evaledArgs originOpt world
        | "-" -> evalBinary SubFns fnName evaledArgs originOpt world
        | "*" -> evalBinary MulFns fnName evaledArgs originOpt world
        | "/" -> evalBinary DivFns fnName evaledArgs originOpt world
        | "%" -> evalBinary ModFns fnName evaledArgs originOpt world
        | "!" -> evalSinglet evalDereference fnName evaledArgs originOpt world
        | "not" -> evalBoolUnary not fnName evaledArgs originOpt world
        | "hash" -> evalUnary HashFns fnName evaledArgs originOpt world
        | "toEmpty" -> evalUnary ToEmptyFns fnName evaledArgs originOpt world
        | "toIdentity" -> evalUnary ToIdentityFns fnName evaledArgs originOpt world
        | "toMin" -> evalUnary ToMinFns fnName evaledArgs originOpt world
        | "toMax" -> evalUnary ToMaxFns fnName evaledArgs originOpt world
        | "inc" -> evalUnary IncFns fnName evaledArgs originOpt world
        | "dec" -> evalUnary DecFns fnName evaledArgs originOpt world
        | "negate" -> evalUnary NegateFns fnName evaledArgs originOpt world
        | "pow" -> evalBinary PowFns fnName evaledArgs originOpt world
        | "root" -> evalBinary RootFns fnName evaledArgs originOpt world
        | "sqr" -> evalUnary SqrFns fnName evaledArgs originOpt world
        | "sqrt" -> evalUnary SqrtFns fnName evaledArgs originOpt world
        | "floor" -> evalUnary FloorFns fnName evaledArgs originOpt world
        | "ceiling" -> evalUnary CeilingFns fnName evaledArgs originOpt world
        | "truncate" -> evalUnary TruncateFns fnName evaledArgs originOpt world
        | "round" -> evalUnary RoundFns fnName evaledArgs originOpt world
        | "exp" -> evalUnary ExpFns fnName evaledArgs originOpt world
        | "log" -> evalUnary LogFns fnName evaledArgs originOpt world
        | "sin" -> evalUnary SinFns fnName evaledArgs originOpt world
        | "cos" -> evalUnary CosFns fnName evaledArgs originOpt world
        | "tan" -> evalUnary TanFns fnName evaledArgs originOpt world
        | "asin" -> evalUnary AsinFns fnName evaledArgs originOpt world
        | "acos" -> evalUnary AcosFns fnName evaledArgs originOpt world
        | "atan" -> evalUnary AtanFns fnName evaledArgs originOpt world
        | "length" -> evalUnary LengthFns fnName evaledArgs originOpt world
        | "normal" -> evalUnary NormalFns fnName evaledArgs originOpt world
        | "cross" -> evalBinary CrossFns fnName evaledArgs originOpt world
        | "dot" -> evalBinary DotFns fnName evaledArgs originOpt world
        | "bool" -> evalUnary BoolFns fnName evaledArgs originOpt world
        | "int" -> evalUnary IntFns fnName evaledArgs originOpt world
        | "int64" -> evalUnary Int64Fns fnName evaledArgs originOpt world
        | "single" -> evalUnary SingleFns fnName evaledArgs originOpt world
        | "double" -> evalUnary DoubleFns fnName evaledArgs originOpt world
        | "string" -> evalUnary StringFns fnName evaledArgs originOpt world
        | "getTypeName" -> evalSinglet evalGetTypeName fnName evaledArgs originOpt world
        | "tryIndex" -> evalDoublet evalTryIndex fnName evaledArgs originOpt world
        | "hasIndex" -> evalDoublet evalHasIndex fnName evaledArgs originOpt world
        | "index" -> evalDoublet evalIndex fnName evaledArgs originOpt world
        | "getName" -> evalSinglet evalGetName fnName evaledArgs originOpt world
        | "tuple" -> evalTuple fnName evaledArgs originOpt world
        | "pair" -> evalTuple fnName evaledArgs originOpt world
        | "fst" -> evalSinglet (evalIndexInt 0) fnName evaledArgs originOpt world
        | "snd" -> evalSinglet (evalIndexInt 1) fnName evaledArgs originOpt world
        | "thd" -> evalSinglet (evalIndexInt 2) fnName evaledArgs originOpt world
        | "fth" -> evalSinglet (evalIndexInt 3) fnName evaledArgs originOpt world
        | "fif" -> evalSinglet (evalIndexInt 4) fnName evaledArgs originOpt world
        | "nth" -> evalDoublet evalNth fnName evaledArgs originOpt world
        | "some" -> evalSinglet evalSome fnName evaledArgs originOpt world
        | "isNone" -> evalSinglet evalIsNone fnName evaledArgs originOpt world
        | "isSome" -> evalSinglet evalIsSome fnName evaledArgs originOpt world
        | "isEmpty" -> evalSinglet (evalIsEmpty evalApply) fnName evaledArgs originOpt world
        | "notEmpty" -> evalSinglet (evalNotEmpty evalApply) fnName evaledArgs originOpt world
        | "tryUncons" -> evalSinglet (evalTryUncons evalApply) fnName evaledArgs originOpt world
        | "uncons" -> evalSinglet (evalUncons evalApply) fnName evaledArgs originOpt world
        | "cons" -> evalDoublet evalCons fnName evaledArgs originOpt world
        | "commit" -> evalSinglet evalCommit fnName evaledArgs originOpt world
        | "tryHead" -> evalSinglet (evalTryHead evalApply) fnName evaledArgs originOpt world
        | "head" -> evalSinglet (evalHead evalApply) fnName evaledArgs originOpt world
        | "tryTail" -> evalSinglet (evalTryTail evalApply) fnName evaledArgs originOpt world
        | "tail" -> evalSinglet (evalTail evalApply) fnName evaledArgs originOpt world
        | "scanWhile" -> evalTriplet (evalScanWhile evalApply) fnName evaledArgs originOpt world
        | "scani" -> evalTriplet (evalScani evalApply) fnName evaledArgs originOpt world
        | "scan" -> evalTriplet (evalScan evalApply) fnName evaledArgs originOpt world
        | "foldWhile" -> evalTriplet (evalFoldWhile evalApply) fnName evaledArgs originOpt world
        | "foldi" -> evalTriplet (evalFoldi evalApply) fnName evaledArgs originOpt world
        | "fold" -> evalTriplet (evalFold evalApply) fnName evaledArgs originOpt world
        | "mapi" -> evalDoublet (evalMapi evalApply) fnName evaledArgs originOpt world
        | "map" -> evalDoublet (evalMap evalApply) fnName evaledArgs originOpt world
        | "contains" -> evalDoublet (evalContains evalApply) fnName evaledArgs originOpt world
        | "toString" -> evalSinglet evalToString fnName evaledArgs originOpt world
        | "codata" -> evalDoublet evalCodata fnName evaledArgs originOpt world
        | "toCodata" -> evalSinglet evalToCodata fnName evaledArgs originOpt world
        | "list" -> evalList fnName evaledArgs originOpt world
        | "toList" -> evalSinglet evalToList fnName evaledArgs originOpt world
        | "ring" -> evalRing fnName evaledArgs originOpt world
        | "toRing" -> evalSinglet evalToRing fnName evaledArgs originOpt world
        | "add" -> evalDoublet evalCons fnName evaledArgs originOpt world
        | "remove" -> evalDoublet evalRemove fnName evaledArgs originOpt world
        | "toTable" -> evalSinglet evalToTable fnName evaledArgs originOpt world
        | _ -> (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalIntrinsic fnName evaledArgs originOpt world =
        match evalIntrinsicInner fnName evaledArgs originOpt world with
        | (Violation _, world) -> evalOverload fnName evaledArgs originOpt world
        | success -> success

    and evalOverload fnName evaledArgs originOpt world =
        if Array.notEmpty evaledArgs then
            match Array.last evaledArgs with
            | Pluggable pluggable ->
                let pluggableTypeName = pluggable.TypeName
                let xfnName = fnName + "_" + pluggableTypeName
                let xfnBinding = Binding (xfnName, ref UncachedBinding, None)
                let evaleds = Array.cons xfnBinding evaledArgs
                evalApply evaleds originOpt world
            | Union (name, _)
            | Record (name, _, _) ->
                let xfnName = fnName + "_" + name
                let xfnBinding = Binding (xfnName, ref UncachedBinding, None)
                let evaleds = Array.cons xfnBinding evaledArgs
                evalApply evaleds originOpt world
            | Violation _ as error -> (error, world)
            | _ -> (Violation (["InvalidOverload"], "Could not find overload for '" + fnName + "' for target.", originOpt), world)
        else (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalUnionUnevaled name exprs world =
        let (evaleds, world) = evalMany exprs world
        (Union (name, evaleds), world)

    and evalTableUnevaled exprPairs world =
        let (evaledPairs, world) =
            List.fold (fun (evaledPairs, world) (exprKey, exprValue) ->
                let (evaledKey, world) = eval exprKey world
                let (evaledValue, world) = eval exprValue world
                ((evaledKey, evaledValue) :: evaledPairs, world))
                ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        (Table (Map.ofList evaledPairs), world)

    and evalRecordUnevaled name exprPairs world =
        let (evaledPairs, world) =
            List.fold (fun (evaledPairs, world) (fieldName, expr) ->
                let (evaledValue, world) = eval expr world
                ((fieldName, evaledValue) :: evaledPairs, world))
                ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        let map = evaledPairs |> List.mapi (fun i (fieldName, _) -> (fieldName, i)) |> Map.ofList
        let fields = evaledPairs |> List.map snd |> Array.ofList
        (Record (name, map, fields), world)

    and evalBinding expr name cachedBinding originOpt world =
        match tryGetBinding name cachedBinding world with
        | None ->
            if world.IsExtrinsic name then (expr, world)
            elif isIntrinsic name then (expr, world)
            else (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "'.", originOpt), world)
        | Some binding -> (binding, world)

    and evalUpdateIntInner fnName index target value originOpt world =
        match target with
        | String str ->
            if index >= 0 && index < String.length str then
                match value with
                | String str2 when str2.Length = 1 ->
                    let left = str.Substring (0, index)
                    let right = str.Substring (index, str.Length)
                    Right (String (left + str2 + right), world)
                | _ -> Left (Violation (["InvalidArgumentValue"; String.capitalize fnName], "String update value must be a String of length 1.", originOpt), world)
            else Left (Violation (["OutOfRangeArgument"; String.capitalize fnName], "String does not contain element at index " + string index + ".", originOpt), world)
        | Option opt ->
            match (index, opt) with
            | (0, Some value) -> Right (value, world)
            | (_, _) -> Left (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Could not update at index " + string index + ".", originOpt), world)
        | List _ -> Left (Violation (["NotImplemented"; String.capitalize fnName], "Updating lists by index is not yet implemented.", originOpt), world) // TODO: implement
        | Table map -> Right (Table (Map.add (Int index) value map), world)
        | Tuple elements
        | Union (_, elements)
        | Record (_, _, elements) ->
            if index < elements.Length then
                let elements' = Array.copy elements
                elements'.[index] <- value
                match target with
                | Tuple _ -> Right (Tuple elements', world)
                | Union (name, _) -> Right (Union (name, elements'), world)
                | Record (name, map, _) -> Right (Record (name, map, elements'), world)
                | _ -> failwithumf ()
            else Left (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Could not update structure at index " + string index + ".", originOpt), world)
        | _ ->
            match evalOverload fnName [|target; (Int index); value|] originOpt world with
            | (Violation _, _) as error -> Left error
            | (_, _) as success -> Right success

    and evalUpdateKeywordInner fnName keyword target value originOpt world =
        match target with
        | Table map ->
            Right (Table (Map.add (Keyword keyword) value map), world)
        | Record (name, map, fields) ->
            match Map.tryFind keyword map with
            | Some index ->
                if index < fields.Length then
                    let fields' = Array.copy fields
                    fields'.[index] <- value
                    Right (Record (name, map, fields'), world)
                else Left (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
            | None ->
                Left (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
        | Violation _ as violation ->
            Left (violation, world)
        | _ ->
            match evalOverload fnName [|target; Keyword keyword; value|] originOpt world with
            | (Violation _, _) as error -> Left error
            | (_, _) as success -> Right success

    and evalUpdateInner fnName indexerExpr targetExpr valueExpr originOpt world =
        let (indexer, world) = eval indexerExpr world
        let (target, world) = eval targetExpr world
        let (value, world) = eval valueExpr world
        match indexer with
        | Violation _ as v -> Left (v, world)
        | Int index -> evalUpdateIntInner fnName index target value originOpt world
        | Keyword keyword -> evalUpdateKeywordInner fnName keyword target value originOpt world
        | _ ->
            match target with
            | Table map -> Right (Table (Map.add indexer valueExpr map), world)
            | _ ->
                match evalOverload fnName [|target; indexer; value|] originOpt world with
                | (Violation _, _) as error -> Left error
                | (_, _) as success -> Right success

    and evalTryUpdate fnName indexerExpr targetExpr valueExpr originOpt world =
        match evalUpdateInner fnName indexerExpr targetExpr valueExpr originOpt world with
        | Right (evaled, world) -> (Option (Some evaled), world)
        | Left (_, world) -> (Option None, world)

    and evalUpdate fnName indexerExpr targetExpr valueExpr originOpt world =
        match evalUpdateInner fnName indexerExpr targetExpr valueExpr originOpt world with
        | Right success -> success
        | Left error -> error

    and evalApply exprs originOpt world =
        if Array.notEmpty exprs then
            let (exprsHead, exprsTail) = (Array.head exprs, Array.tail exprs)
            let (evaledHead, world) = eval exprsHead world in annotateWorld world // force the type checker to see the world as it is
            match evaledHead with
            | Keyword keyword ->
                let (evaledTail, world) = evalMany exprsTail world
                let union = Union (keyword, evaledTail)
                (union, world)
            | Binding (fnName, _, originOpt) ->
                // NOTE: when evaluation leads here, we can (actually must) infer that we have
                // either an extrinsic or intrinsic function.
                if world.IsExtrinsic fnName then
                    let exprsTail = Array.tail exprs
                    world.EvalExtrinsic fnName originOpt exprsTail
                else
                    let (evaledTail, world) = evalMany exprsTail world
                    evalIntrinsic fnName evaledTail originOpt world
            | Fun (pars, parsCount, body, _, framesOpt, originOpt) ->
                let (evaledTail, world) = evalMany exprsTail world
                let (framesCurrentOpt, world) =
                    match framesOpt with
                    | Some frames ->
                        let framesCurrent = getProceduralFrames world
                        let world = setProceduralFrames (frames :?> ProceduralFrame list) world
                        (Some framesCurrent, world)
                    | None -> (None, world)
                let (evaled, world) =
                    if evaledTail.Length = parsCount then
                        let bindings = Array.map2 (fun par evaledArg -> (par, evaledArg)) pars evaledTail
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
        else (Unit, world)

    and evalApplyAnd exprs originOpt world =
        match exprs with
        | [|left; right|] ->
            match eval left world with
            | (Bool false, world) -> (Bool false, world)
            | (Bool true, world) ->
                match eval right world with
                | (Bool _, _) as result -> result
                | (Violation _, _) as error -> error
                | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | (Violation _, _) as error -> error
            | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
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
                | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | (Violation _, _) as error -> error
            | _ -> (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
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
        | (_, world) -> (Violation (["InvalidIfCondition"], "Must provide an expression that evaluates to a Bool in an if condition.", originOpt), world)

    and evalMatch input (cases : (Expr * Expr) array) originOpt world =
        let (input, world) = eval input world
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                let (evaledInput, world) = eval condition world
                match evalBinaryInner EqFns "=" input evaledInput originOpt world with
                | (Bool true, world) -> Right (eval consequent world)
                | (Bool false, world) -> Left world
                | (Violation _, world) -> Right (evaledInput, world)
                | _ -> failwithumf ())
                (Left world)
                cases
        match resultEir with
        | Right success -> success
        | Left world -> (Violation (["InexhaustiveMatch"], "A match expression failed to satisfy any of its cases.", originOpt), world)

    and evalSelect exprPairs originOpt world =
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                match eval condition world with
                | (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                | (Violation _ as evaled, world) -> Right (evaled, world)
                | (_, world) -> Right ((Violation (["InvalidSelectCondition"], "Must provide an expression that evaluates to a Bool in a case condition.", originOpt), world)))
                (Left world)
                exprPairs
        match resultEir with
        | Right success -> success
        | Left world -> (Violation (["InexhaustiveSelect"], "A select expression failed to satisfy any of its cases.", originOpt), world)

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
        | Violation _
        | Unit _
        | Bool _
        | Int _
        | Int64 _
        | Single _
        | Double _
        | String _
        | Keyword _
        | Tuple _
        | Union _
        | Pluggable _
        | Option _
        | Codata _
        | List _
        | Ring _
        | Table _
        | Record _ -> (expr, world)
        | UnionUnevaled (name, exprs) -> evalUnionUnevaled name exprs world
        | TableUnevaled exprPairs -> evalTableUnevaled exprPairs world
        | RecordUnevaled (name, exprPairs) -> evalRecordUnevaled name exprPairs world
        | Binding (name, cachedBinding, originOpt) as expr -> evalBinding expr name cachedBinding originOpt world
        | TryUpdate (expr, expr2, expr3, _, originOpt) -> evalTryUpdate "tryUpdate" expr expr2 expr3 originOpt world
        | Update (expr, expr2, expr3, _, originOpt) -> evalUpdate "update" expr expr2 expr3 originOpt world
        | Apply (exprs, _, originOpt) -> evalApply exprs originOpt world
        | ApplyAnd (exprs, _, originOpt) -> evalApplyAnd exprs originOpt world
        | ApplyOr (exprs, _, originOpt) -> evalApplyOr exprs originOpt world
        | Let (binding, body, originOpt) -> evalLet binding body originOpt world
        | LetMany (bindings, body, originOpt) -> evalLetMany bindings body originOpt world
        | Fun (pars, parsCount, body, framesPushed, framesOpt, originOpt) as fn -> evalFun fn pars parsCount body framesPushed framesOpt originOpt world
        | If (condition, consequent, alternative, originOpt) -> evalIf condition consequent alternative originOpt world
        | Match (input, cases, originOpt) -> evalMatch input cases originOpt world
        | Select (exprPairs, originOpt) -> evalSelect exprPairs originOpt world
        | Try (body, handlers, originOpt) -> evalTry body handlers originOpt world
        | Do (exprs, originOpt) -> evalDo exprs originOpt world
        | Quote _ as quote -> (quote, world)
        | Define (binding, originOpt) -> evalDefine binding originOpt world

    /// Evaluate a sequence of expressions.
    and evalMany (exprs : Expr array) world =
        let evaleds = Array.zeroCreate exprs.Length
        let world =
            Seq.foldi
                (fun i world expr ->
                    let (evaled, world) = eval expr world
                    evaleds.[i] <- evaled
                    world)
                world
                exprs
        (evaleds, world)

    /// Evaluate an expression, with logging on violation result.
    let evalWithLogging expr world =
        let (evaled, world) = eval expr world
        Scripting.log evaled
        (evaled, world)

    /// Evaluate a series of expressions, with logging on violation result.
    let evalManyWithLogging exprs world =
        let (evaleds, world) = evalMany exprs world
        Array.iter Scripting.log evaleds
        (evaleds, world)

    /// Attempt to evaluate a script.
    let tryEvalScript choose scriptFilePath world =
        Log.info ("Evaluating script '" + scriptFilePath + "...")
        try let scriptStr =
                scriptFilePath |>
                File.ReadAllText |>
                String.unescape
            let script =
                scriptStr |>
                (fun str -> Symbol.OpenSymbolsStr + str + Symbol.CloseSymbolsStr) |>
                scvalue<Scripting.Expr array>
            let (evaleds, world) = evalMany script world
            Log.info ("Successfully evaluated script '" + scriptFilePath + ".")
            Right (scriptStr, evaleds, world)
        with exn ->
            let error = "Failed to evaluate script '" + scriptFilePath + "' due to: " + scstring exn
            Log.info error
            Left (error, choose world)