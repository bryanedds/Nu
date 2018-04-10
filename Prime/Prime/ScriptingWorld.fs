// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

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
        abstract member TryGetExtrinsic : string -> (string -> Expr array -> SymbolOrigin option -> 'w -> struct (Expr * 'w)) FOption
        abstract member TryImport : Type -> obj -> Expr option
        abstract member TryExport : Type -> Expr -> obj option
        end

[<RequireQualifiedAccess>]
module ScriptingWorld =

    let mutable private Intrinsics =
        Unchecked.defaultof<obj>

    let inline annotateWorld<'w when 'w :> 'w ScriptingWorld> (_ : 'w) =
        () // NOTE: simply infers that a type is a world.

    let tryGetBinding<'w when 'w :> 'w ScriptingWorld> name cachedBinding bindingType (world : 'w) =
        Env.tryGetBinding name cachedBinding bindingType (world.GetEnv ())

    let tryAddDeclarationBinding<'w when 'w :> 'w ScriptingWorld> name value (world : 'w) =
        Env.tryAddDeclarationBinding name value (world.GetEnv ())

    let addProceduralBinding<'w when 'w :> 'w ScriptingWorld> appendType name value (world : 'w) =
        Env.addProceduralBinding appendType name value (world.GetEnv ())

    let addProceduralBindings<'w when 'w :> 'w ScriptingWorld> appendType bindings (world : 'w) =
        Env.addProceduralBindings appendType bindings (world.GetEnv ())

    let removeProceduralBindings<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        Env.removeProceduralBindings (world.GetEnv ())

    let getProceduralFrames<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        Env.getProceduralFrames (world.GetEnv ())

    let setProceduralFrames<'w when 'w :> 'w ScriptingWorld> proceduralFrames (world : 'w) =
        Env.setProceduralFrames proceduralFrames (world.GetEnv ())

    let getGlobalFrame<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        Env.getGlobalFrame (world.GetEnv ())

    let getLocalFrame<'w when 'w :> 'w ScriptingWorld> (world : 'w) =
        Env.getLocalFrame (world.GetEnv ())

    let setLocalFrame<'w when 'w :> 'w ScriptingWorld> localFrame (world : 'w) =
        Env.setLocalFrame localFrame (world.GetEnv ())

    let tryImport<'w when 'w :> 'w ScriptingWorld> ty value (world : 'w) =
        tryImport world.TryImport ty value

    let tryExport<'w when 'w :> 'w ScriptingWorld> ty value (world : 'w) =
        tryExport world.TryExport ty value

    let log expr =
        match expr with
        | Violation (names, error, originOpt) ->
            Log.info ^
                "Unexpected Violation: " + String.concat Constants.Scripting.ViolationSeparatorStr names + "\n" +
                "Due to: " + error + "\n" +
                SymbolOrigin.tryPrint originOpt + "\n"
        | _ -> ()

    let rec getIntrinsics<'w when 'w :> 'w ScriptingWorld> () =
        if isNull Intrinsics then
            let intrinsics =
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
                 ("hash", evalUnary HashFns)
                 ("toEmpty", evalUnary ToEmptyFns)
                 ("toIdentity", evalUnary ToIdentityFns)
                 ("toMin", evalUnary ToMinFns)
                 ("toMax", evalUnary ToMaxFns)
                 ("inc", evalUnary IncFns)
                 ("dec", evalUnary DecFns)
                 ("negate", evalUnary NegateFns)
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
                 ("getTypeName", evalSinglet evalGetTypeName)
                 ("tryIndex", evalDoublet evalTryIndex)
                 ("hasIndex", evalDoublet evalHasIndex)
                 ("index", evalDoublet evalIndex)
                 ("getName", evalSinglet evalGetName)
                 ("tuple", evalTuple)
                 ("pair", evalTuple)
                 ("fst", evalSinglet (evalIndexInt 0))
                 ("snd", evalSinglet (evalIndexInt 1))
                 ("thd", evalSinglet (evalIndexInt 2))
                 ("fth", evalSinglet (evalIndexInt 3))
                 ("fif", evalSinglet (evalIndexInt 4))
                 ("nth", evalDoublet evalNth)
                 ("some", evalSinglet evalSome)
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
                 ("toString", evalSinglet evalToString)
                 ("codata", evalDoublet evalCodata)
                 ("toCodata", evalSinglet evalToCodata)
                 ("list", evalList)
                 ("toList", evalSinglet (evalToList evalApply))
                 ("ring", evalRing)
                 ("toRing", evalSinglet (evalToRing evalApply))
                 ("add", evalDoublet evalCons)
                 ("remove", evalDoublet evalRemove)
                 ("toTable", evalSinglet evalToTable)] |>
                dictPlus
            Intrinsics <- intrinsics
            intrinsics
        else Intrinsics :?> Dictionary<string, string -> Expr array -> SymbolOrigin option -> 'w -> struct (Expr * 'w)>

    and internal evalIntrinsicInner<'w when 'w :> 'w ScriptingWorld> fnName argsEvaled originOpt (world : 'w) =
        let intrinsics = getIntrinsics ()
        match intrinsics.TryGetValue fnName with
        | (true, intrinsic) -> intrinsic fnName argsEvaled originOpt world
        | (false, _) -> struct (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalOverload fnName argsEvaled originOpt world =
        if Array.notEmpty argsEvaled then
            match Array.last argsEvaled with
            | Pluggable pluggable ->
                let pluggableTypeName = pluggable.TypeName
                let xfnName = fnName + "_" + pluggableTypeName
                let xfnBinding = Binding (xfnName, ref UncachedBinding, ref UnknownBindingType, None)
                let evaleds = Array.cons xfnBinding argsEvaled
                evalApply evaleds originOpt world
            | Union (name, _)
            | Record (name, _, _) ->
                let xfnName = fnName + "_" + name
                let xfnBinding = Binding (xfnName, ref UncachedBinding, ref UnknownBindingType, None)
                let evaleds = Array.cons xfnBinding argsEvaled
                evalApply evaleds originOpt world
            | Violation _ as error -> struct (error, world)
            | _ -> struct (Violation (["InvalidOverload"], "Could not find overload for '" + fnName + "' for target.", originOpt), world)
        else struct (Violation (["InvalidFunctionTargetBinding"], "Cannot apply the non-existent binding '" + fnName + "'.", originOpt), world)

    and evalUnionUnevaled name exprs world =
        let struct (evaleds, world) = evalMany exprs world
        struct (Union (name, evaleds), world)

    and evalTableUnevaled exprPairs world =
        let struct (evaledPairs, world) =
            List.fold (fun struct (evaledPairs, world) (exprKey, exprValue) ->
                let struct (evaledKey, world) = eval exprKey world
                let struct (evaledValue, world) = eval exprValue world
                struct ((evaledKey, evaledValue) :: evaledPairs, world))
                struct ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        struct (Table (Map.ofList evaledPairs), world)

    and evalRecordUnevaled name exprPairs world =
        let struct (evaledPairs, world) =
            List.fold (fun struct (evaledPairs, world) (fieldName, expr) ->
                let struct (evaledValue, world) = eval expr world
                struct ((fieldName, evaledValue) :: evaledPairs, world))
                struct ([], world)
                exprPairs
        let evaledPairs = List.rev evaledPairs
        let map = evaledPairs |> List.mapi (fun i (fieldName, _) -> (fieldName, i)) |> Map.ofList
        let fields = evaledPairs |> List.map snd |> Array.ofList
        struct (Record (name, map, fields), world)

    and evalBinding<'w when 'w :> 'w ScriptingWorld> expr name cachedBinding bindingType originOpt (world : 'w) =
        match tryGetBinding name cachedBinding bindingType world with
        | None ->
            match !bindingType with
            | UnknownBindingType ->
                if (getIntrinsics<'w> ()).ContainsKey name then bindingType := Intrinsic; struct (expr, world)
                elif FOption.isSome (world.TryGetExtrinsic name) then bindingType := Extrinsic; struct (expr, world)
                else bindingType := Environmental; struct (expr, world)
            | Intrinsic -> struct (expr, world)
            | Extrinsic -> struct (expr, world)
            | Environmental -> struct (Violation (["NonexistentBinding"], "Non-existent binding '" + name + "'.", originOpt), world)
        | Some binding -> struct (binding, world)

    and evalUpdateIntInner fnName index target value originOpt world =
        match target with
        | String str ->
            if index >= 0 && index < String.length str then
                match value with
                | String str2 when str2.Length = 1 ->
                    let left = str.Substring (0, index)
                    let right = str.Substring (index, str.Length)
                    Right struct (String (left + str2 + right), world)
                | _ -> Left struct (Violation (["InvalidArgumentValue"; String.capitalize fnName], "String update value must be a String of length 1.", originOpt), world)
            else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "String does not contain element at index " + string index + ".", originOpt), world)
        | Option opt ->
            match (index, opt) with
            | (0, Some value) -> Right struct (value, world)
            | (_, _) -> Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Could not update at index " + string index + ".", originOpt), world)
        | List _ -> Left struct (Violation (["NotImplemented"; String.capitalize fnName], "Updating lists by index is not yet implemented.", originOpt), world) // TODO: implement
        | Table map -> Right struct (Table (Map.add (Int index) value map), world)
        | Tuple elements
        | Union (_, elements)
        | Record (_, _, elements) ->
            if index < elements.Length then
                let elements' = Array.copy elements
                elements'.[index] <- value
                match target with
                | Tuple _ -> Right struct (Tuple elements', world)
                | Union (name, _) -> Right struct (Union (name, elements'), world)
                | Record (name, map, _) -> Right struct (Record (name, map, elements'), world)
                | _ -> failwithumf ()
            else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Could not update structure at index " + string index + ".", originOpt), world)
        | _ ->
            match evalOverload fnName [|Int index; value; target|] originOpt world with
            | struct (Violation _, _) as error -> Left error
            | struct (_, _) as success -> Right success

    and evalUpdateKeywordInner fnName keyword target value originOpt world =
        match target with
        | Table map ->
            Right struct (Table (Map.add (Keyword keyword) value map), world)
        | Record (name, map, fields) ->
            match Map.tryFind keyword map with
            | Some index ->
                if index < fields.Length then
                    let fields' = Array.copy fields
                    fields'.[index] <- value
                    Right struct (Record (name, map, fields'), world)
                else Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
            | None ->
                Left struct (Violation (["OutOfRangeArgument"; String.capitalize fnName], "Record does not contain element with name '" + name + "'.", originOpt), world)
        | Violation _ as violation ->
            Left struct (violation, world)
        | _ ->
            match evalOverload fnName [|Keyword keyword; value; target|] originOpt world with
            | struct (Violation _, _) as error -> Left error
            | struct (_, _) as success -> Right success

    and evalUpdateInner fnName indexerExpr targetExpr valueExpr originOpt world =
        let struct (indexer, world) = eval indexerExpr world
        let struct (target, world) = eval targetExpr world
        let struct (value, world) = eval valueExpr world
        match indexer with
        | Violation _ as v -> Left struct (v, world)
        | Int index -> evalUpdateIntInner fnName index target value originOpt world
        | Keyword keyword -> evalUpdateKeywordInner fnName keyword target value originOpt world
        | _ ->
            match target with
            | Table map -> Right struct (Table (Map.add indexer valueExpr map), world)
            | _ ->
                match evalOverload fnName [|indexer; value; target|] originOpt world with
                | struct (Violation _, _) as error -> Left error
                | struct (_, _) as success -> Right success

    and evalTryUpdate indexerExpr targetExpr valueExpr originOpt world =
        match evalUpdateInner "tryUpdate" indexerExpr targetExpr valueExpr originOpt world with
        | Right struct (evaled, world) -> struct (Option (Some evaled), world)
        | Left struct (_, world) -> struct (Option None, world)

    and evalUpdate indexerExpr targetExpr valueExpr originOpt world =
        match evalUpdateInner "update" indexerExpr targetExpr valueExpr originOpt world with
        | Right success -> success
        | Left error -> error

    // TODO: decompose this function - it's too hard to read
    and evalApply<'w when 'w :> 'w ScriptingWorld> (exprs : Expr array) (originOpt : SymbolOrigin option) (world : 'w) : struct (Expr * 'w) =
        if Array.notEmpty exprs then
            let (exprsHead, exprsTail) = (Array.head exprs, Array.tail exprs)
            let struct (headEvaled, world) = eval exprsHead world in annotateWorld world // force the type checker to see the world as it is
            match headEvaled with
            | Keyword keyword ->
                let struct (tailEvaled, world) = evalMany exprsTail world
                let union = Union (keyword, tailEvaled)
                struct (union, world)
            | Binding (fnName, _, bindingType, originOpt) ->
                // NOTE: when evaluation leads here, we infer that we have either an extrinsic or intrinsic function,
                // otherwise it would have led to the Fun case... Also, binding type should be decided by this point.
                match bindingType.Value with
                | UnknownBindingType ->
                    failwithumf ()
                | Intrinsic ->
                    let struct (argsEvaled, world) = evalMany exprsTail world
                    match evalIntrinsicInner fnName argsEvaled originOpt world with
                    | struct (Violation _, world) -> evalOverload fnName argsEvaled originOpt world
                    | success -> success
                | Extrinsic ->
                    let args = Array.tail exprs
                    let extrinsicOpt = world.TryGetExtrinsic fnName
                    if FOption.isSome extrinsicOpt
                    then extrinsicOpt.Value fnName args originOpt world
                    else failwithumf ()
                | Environmental ->
                    failwithumf ()
            | Fun (pars, parsCount, body, _, framesOpt, originOpt) ->
                let struct (tailEvaled, world) = evalMany exprsTail world
                let struct (framesCurrentOpt, world) =
                    match framesOpt with
                    | Some frames ->
                        let framesCurrent = getProceduralFrames world
                        setProceduralFrames (frames :?> ProceduralFrame list) world
                        struct (Some framesCurrent, world)
                    | None -> struct (None, world)
                let struct (evaled, world) =
                    if tailEvaled.Length = parsCount then
                        let bindings = Array.map2 (fun par evaledArg -> struct (par, evaledArg)) pars tailEvaled
                        addProceduralBindings (AddToNewFrame parsCount) bindings world
                        let struct (evaled, world) = eval body world
                        removeProceduralBindings world
                        struct (evaled, world)
                    else struct (Violation (["MalformedLambdaInvocation"], "Wrong number of arguments.", originOpt), world)
                match framesCurrentOpt with
                | Some framesCurrent ->
                    setProceduralFrames framesCurrent world
                    struct (evaled, world)
                | None -> struct (evaled, world)
            | Violation _ as error -> struct (error, world)
            | _ -> struct (Violation (["MalformedApplication"], "Cannot apply the non-binding '" + scstring headEvaled + "'.", originOpt), world)
        else struct (Unit, world)

    and evalApplyAnd exprs originOpt world =
        match exprs with
        | [|left; right|] ->
            match eval left world with
            | struct (Bool false, _) as never -> never
            | struct (Bool true, world) ->
                match eval right world with
                | struct (Bool _, _) as result -> result
                | struct (Violation _, _) as error -> error
                | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | struct (Violation _, _) as error -> error
            | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

    and evalApplyOr exprs originOpt world =
        match exprs with
        | [|left; right|] ->
            match eval left world with
            | struct (Bool true, _) as always -> always
            | struct (Bool false, world) ->
                match eval right world with
                | struct (Bool _, _) as result -> result
                | struct (Violation _, _) as error -> error
                | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
            | struct (Violation _, _) as error -> error
            | _ -> struct (Violation (["InvalidArgumentType"; "&&"], "Cannot apply a logic function to non-Bool values.", originOpt), world)
        | _ -> struct (Violation (["InvalidArgumentCount"; "&&"], "Incorrect number of arguments for application of '&&'; 2 arguments required.", originOpt), world)

    and evalLet4 binding body originOpt world =
        let world =
            match binding with
            | VariableBinding (name, body) ->
                let struct (evaled, world) = eval body world
                addProceduralBinding (AddToNewFrame 1) name evaled world
                world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                addProceduralBinding (AddToNewFrame 1) name fn world
                world
        let struct (evaled, world) = eval body world
        removeProceduralBindings world
        struct (evaled, world)

    and evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world =
        let world =
            match bindingsHead with
            | VariableBinding (name, body) ->
                let struct (bodyValue, world) = eval body world
                addProceduralBinding (AddToNewFrame bindingsCount) name bodyValue world
                world
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                addProceduralBinding (AddToNewFrame bindingsCount) name fn world
                world
        let world =
            List.foldi (fun i world binding ->
                match binding with
                | VariableBinding (name, body) ->
                    let struct (bodyValue, world) = eval body world
                    addProceduralBinding (AddToHeadFrame ^ inc i) name bodyValue world
                    world
                | FunctionBinding (name, args, body) ->
                    let frames = getProceduralFrames world :> obj
                    let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                    addProceduralBinding (AddToHeadFrame ^ inc i) name fn world
                    world)
                world
                bindingsTail
        let struct (evaled, world) = eval body world
        removeProceduralBindings world
        struct (evaled, world)
        
    and evalLet binding body originOpt world =
        evalLet4 binding body originOpt world
        
    and evalLetMany bindings body originOpt world =
        match bindings with
        | bindingsHead :: bindingsTail ->
            let bindingsCount = List.length bindingsTail + 1
            evalLetMany4 bindingsHead bindingsTail bindingsCount body originOpt world
        | [] -> struct (Violation (["MalformedLetOperation"], "Let operation must have at least 1 binding.", originOpt), world)

    and evalFun fn pars parsCount body framesPushed framesOpt originOpt world =
        if not framesPushed then
            if Option.isNone framesOpt then
                let frames = getProceduralFrames world :> obj
                struct (Fun (pars, parsCount, body, true, Some frames, originOpt), world)
            else struct (Fun (pars, parsCount, body, true, framesOpt, originOpt), world)
        else struct (fn, world)

    and evalIf condition consequent alternative originOpt world =
        match eval condition world with
        | struct (Bool bool, world) -> if bool then eval consequent world else eval alternative world
        | struct (Violation _ as evaled, world) -> struct (evaled, world)
        | struct (_, world) -> struct (Violation (["InvalidIfCondition"], "Must provide an expression that evaluates to a Bool in an if condition.", originOpt), world)

    and evalMatch input (cases : (Expr * Expr) array) originOpt world =
        let struct (input, world) = eval input world
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                let struct (evaledInput, world) = eval condition world
                match evalBinaryInner EqFns "=" input evaledInput originOpt world with
                | struct (Bool true, world) -> Right (eval consequent world)
                | struct (Bool false, world) -> Left world
                | struct (Violation _, world) -> Right struct (evaledInput, world)
                | _ -> failwithumf ())
                (Left world)
                cases
        match resultEir with
        | Right success -> success
        | Left world -> struct (Violation (["InexhaustiveMatch"], "A match expression failed to satisfy any of its cases.", originOpt), world)

    and evalSelect exprPairs originOpt world =
        let resultEir =
            Seq.foldUntilRight (fun world (condition, consequent) ->
                match eval condition world with
                | struct (Bool bool, world) -> if bool then Right (eval consequent world) else Left world
                | struct (Violation _ as evaled, world) -> Right struct (evaled, world)
                | struct (_, world) -> Right struct (Violation (["InvalidSelectCondition"], "Must provide an expression that evaluates to a Bool in a case condition.", originOpt), world))
                (Left world)
                exprPairs
        match resultEir with
        | Right success -> success
        | Left world -> struct (Violation (["InexhaustiveSelect"], "A select expression failed to satisfy any of its cases.", originOpt), world)

    and evalTry body handlers _ world =
        match eval body world with
        | struct (Violation (categories, _, _) as evaled, world) ->
            match
                List.foldUntilRight (fun world (handlerCategories, handlerBody) ->
                    let categoriesTrunc = List.truncate (List.length handlerCategories) categories
                    if categoriesTrunc = handlerCategories then Right (eval handlerBody world) else Left world)
                    (Left world)
                    handlers with
            | Right success -> success
            | Left world -> struct (evaled, world)
        | success -> success

    and evalDo exprs _ world =
        let evaledEir =
            List.foldWhileRight (fun struct (_, world) expr ->
                match eval expr world with
                | struct (Violation _, _) as error -> Left error
                | success -> Right success)
                (Right struct (Unit, world))
                exprs
        Either.amb evaledEir

    and evalDefine binding originOpt world =
        let struct (bound, world) =
            match binding with
            | VariableBinding (name, body) ->
                let struct (evaled, world) = eval body world
                struct (tryAddDeclarationBinding name evaled world, world)
            | FunctionBinding (name, args, body) ->
                let frames = getProceduralFrames world :> obj
                let fn = Fun (args, args.Length, body, true, Some frames, originOpt)
                struct (tryAddDeclarationBinding name fn world, world)
        if bound
        then struct (Unit, world)
        else struct (Violation (["InvalidDeclaration"], "Can make declarations only at the top-level.", None), world)

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
        | Record _ -> struct (expr, world)
        | UnionUnevaled (name, exprs) -> evalUnionUnevaled name exprs world
        | TableUnevaled exprPairs -> evalTableUnevaled exprPairs world
        | RecordUnevaled (name, exprPairs) -> evalRecordUnevaled name exprPairs world
        | Binding (name, cachedBinding, bindingType, originOpt) as expr -> evalBinding expr name cachedBinding bindingType originOpt world
        | TryUpdate (expr, expr2, expr3, _, originOpt) -> evalTryUpdate expr expr2 expr3 originOpt world
        | Update (expr, expr2, expr3, _, originOpt) -> evalUpdate expr expr2 expr3 originOpt world
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
        | Quote _ as quote -> struct (quote, world)
        | Define (binding, originOpt) -> evalDefine binding originOpt world

    /// Evaluate a sequence of expressions.
    and evalMany (exprs : Expr array) world =
        let evaleds = Array.zeroCreate exprs.Length
        let world =
            Seq.foldi
                (fun i world expr ->
                    let struct (evaled, world) = eval expr world
                    evaleds.[i] <- evaled
                    world)
                world
                exprs
        struct (evaleds, world)

    /// Evaluate an expression, with logging on violation result.
    let evalWithLogging expr world =
        let struct (evaled, world) = eval expr world
        log evaled
        struct (evaled, world)

    /// Evaluate a series of expressions, with logging on violation result.
    let evalManyWithLogging exprs world =
        let struct (evaleds, world) = evalMany exprs world
        Array.iter log evaleds
        struct (evaleds, world)

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
                scvalue<Expr array>
            let struct (evaleds, world) = evalMany script world
            Log.info ("Successfully evaluated script '" + scriptFilePath + ".")
            Right struct (scriptStr, evaleds, world)
        with exn ->
            let error = "Failed to evaluate script '" + scriptFilePath + "' due to: " + scstring exn
            Log.info error
            Left struct (error, choose world)