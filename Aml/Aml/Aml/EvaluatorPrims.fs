// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Aml
open System
open System.Collections.Generic
open Prime
open Aml.Ast
open Aml.AstModule
open Aml.AmlConstants
open Aml.Primitives
open Aml.Initial
open Aml.Writer
open Aml.Conversions

// TODO: when module exportation is implemented in F# - http://fslang.uservoice.com/forums/245727-f-language/suggestions/5688199-allow-re-exporting-from-modules
// make this module exported from Evaluator and rename to EvaluatorPrimsModule.

module EvaluatorPrims =

    /// Make an evaluation result.
    let makeEvalResult value env = { Value = value; Env = env }

    /// Make an evaluation result with unit as the value.
    let makeEvalUnit env = makeEvalResult UnitValue env

    /// Make an evaluation result whose environment has had its repl line configured.
    let configureEvalResultFirstReplLine (result : EvalResult) line =
#if AML_OPTIMIZED
        result
#else
        let resultEnv = result.Env
        let configuredEnv = { resultEnv with EnvDebugInfo = { resultEnv.EnvDebugInfo with DIOptFirstReplLine = line }}
        makeEvalResult result.Value configuredEnv
#endif

    /// Make an evaluation result whose environment has had its expr trace popped.
    let popEvalResultExpr (result : EvalResult) =
        let poppedEnv = popExpr result.Env
        makeEvalResult result.Value poppedEnv

    /// Make an evaluation result whose environment has had its stack trace popped.
    let popEvalResultStackFrame (result : EvalResult) =
        let poppedEnv = popStackFrame result.Env
        makeEvalResult result.Value poppedEnv

    /// Get a composite's type.
    let getTypeOfComposite composite =
        let aType = composite.CompType
        if isUnit aType then CompositeType else aType

    /// Try to find a declaration entry.
    let tryFindDeclarationEntry name env =
        let entry = ref Unchecked.defaultof<EnvEntry>
        if env.EnvDeclarationFrame.TryGetValue (name, entry) then Some !entry
        else None

    /// Try to find a declaration entry by the given search method.
    let tryFindDeclarationEntryBy by name env =
        let typeEntry = tryFindDeclarationEntry name env
        by typeEntry

    /// Try to find a procedural entry plus directions on how to access it again.
    let tryFindProceduralEntry name env =
        let offset = ref -1
        let optIndex = ref None
        let optEntry =
            List.tryFindPlus
                (fun (frame : ProceduralFrame) ->
                    offset := !offset + 1
                    optIndex := Array.tryFindIndexRev (fun (entryName, _) -> name = entryName) frame
                    match !optIndex with
                    | None -> (false, None)
                    | Some index -> (true, Some frame.[index]))
                env.EnvProceduralFrames
        match optEntry with
        | None -> None
        | Some (Some (_, entry)) -> Some (entry, !offset, (!optIndex).Value)
        | Some None -> failwith "Unexpected match failure in 'Aml.Evaluator.Prims.tryFindProceduralEntry'."

    /// Try to find an entry.
    let tryFindEntry name env =
        let optEntry =
            List.tryFindPlus
                (fun (frame : ProceduralFrame) ->
                    let optEntry = Array.tryFind (fun (entryName, _) -> name = entryName) frame
                    match optEntry with None -> (false, None) | Some (_, entry) -> (true, Some entry))
                env.EnvProceduralFrames
        match optEntry with
        | None -> tryFindDeclarationEntry name env
        | Some (Some entry) -> Some entry
        | Some None -> failwith "Unexpected match failure in 'Aml.Evaluator.Prims.tryFindEntry'."

    /// Try to find a dynamic entry. 
    let tryFindDynamicEntry name env =
        let findBy = function | Some (DynamicEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy findBy name env

    /// Try to find a protocol entry. 
    let tryFindProtocolEntry name env =
        let findBy = function | Some (ProtocolEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy findBy name env

    /// Try to find a type entry.
    let tryFindTypeEntry typeName env =
        let findBy = function | Some (TypeEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy findBy typeName env

    /// Try to find a type.
    let tryFindType typeName env =
        let findBy = function | Some (TypeEntry (typeName, value, doc)) -> Some (typeName, value, doc) | _ -> None
        tryFindDeclarationEntryBy findBy typeName env

    /// Try to find a protocol.
    let tryFindProtocol protocolName env =
        let findBy =
            function
            | Some (ProtocolEntry (arg, optConstraints, doc, signatures)) -> Some (arg, optConstraints, doc, signatures)
            | _ -> None
        tryFindDeclarationEntryBy findBy protocolName env

    /// Try to find the type of a value.
    let tryFindTypeByValue arg =
        match arg with
        | Violation _ -> Some ViolationType
        | Boolean _ -> Some BooleanType
        | Character _ -> Some CharacterType
        | String _ -> Some StringType
        | Int _ -> Some IntType
        | Long _ -> Some LongType
        | Float _ -> Some FloatType
        | Double _ -> Some DoubleType
        | Keyword _ -> Some KeywordType
        | Symbol _ -> Some LambdaType
        | Package _ -> None
        | Prefixed _ -> None
        | Dispatch _ -> Some LambdaType
        | SpecialValue _ -> Some SpecialValueType
        | SpecialObject _ -> None
        | Series s -> if s.SerExprs.IsEmpty then Some UnitType else None
        | Lambda _ -> Some LambdaType
        | Attempt _ -> None
        | Let _ -> None
        | Extend _ -> None
        | Case _ -> None
        | Condition _ -> None
        | Intervene _ -> None
        | Ref _ -> Some RefType
        | Get _ -> None
        | Set _ -> None
        | List _ -> Some ListType
        | Array _ -> Some ArrayType
        | Composite composite -> Some (getTypeOfComposite composite)
        | Selector _ -> None
        | Variable _ -> None
        | Function _ -> None
        | Structure _ -> None
        | Protocol _ -> None
        | Instance _ -> None
        | Affirmation _ -> None
        | UsingFile _ -> None
        | UsingLanguage _ -> None
        | SpecialSeries _ -> None
    
    /// Query that a declaration entry exists.
    let hasDeclarationEntry name env =
        (tryFindDeclarationEntry name env).IsSome

    /// Query that a symbol refers to a built-in operator.
    let isBuiltin expr env =
        match expr with
        | Symbol symbol ->
            if InitialBuiltinNames.Contains symbol.SymName then true
            else
                match env.EnvOptLanguageModule with
                | None -> false
                | Some lm -> lm.IsSpecialBuiltin symbol.SymName env
        | _ -> false

    /// Expand an entry to a lambda record represenation.
    let expandLambda lambda =
        match lambda with
        | Lambda lambda -> lambda
        | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.Prims.expandLambda'."
    
    /// Get a member from some composite members.
    let getMember memberName (members : MemberDict) env =
        let mem = ref Unchecked.defaultof<Member>
        if members.TryGetValue (memberName, mem) then (!mem).MemExpr
        else makeViolationWithPositions ":v/eval/missingMember" ("Member '" + memberName + "' does not exist.") env

    /// Get a value's type (not from the environment).
    let getType value env =
        match value with
        | SpecialObject _ | Prefixed _ ->
            match env.EnvOptLanguageModule with
            | None -> makeViolationWithPositions ":v/languageModule/missingLanguageModule" "Cannot get type of special value without a language module." env
            | Some lm -> lm.GetSpecialType value env
        | _ ->
            let optType = tryFindTypeByValue value
            match optType with
            | None -> makeViolationWithPositions ":v/eval/invalidTypeQuery" "Invalid type query." env
            | Some aType -> aType

    /// Query that a value has the given type.
    let hasType typeName value env =
        let vtype = getType value env
        match vtype with
        | Composite composite -> composite.CompName = typeName
        | _ -> false

    /// Augment an environment with a protocol.
    /// OPTIMIZATION: implemented with Seq.
    /// TODO: clean up this function with extraction.
    let tryAppendProtocol name arg optConstraints doc sigs env =
        let sigsMatchingEntry = Seq.filter (fun signature -> (tryFindDeclarationEntry signature.SigName env).IsSome) sigs
        let optFirstSigMatchingEntry = Seq.tryHead sigsMatchingEntry
        match optFirstSigMatchingEntry with
        | Some firstSigMatchingEntry when not env.EnvAllowRedeclaration ->
            failwith ("The protocol signature '" + firstSigMatchingEntry.SigName + "' clashes names with an existing declaration.")
        | _ ->
            let protocolName = ProtocolPrefixStr + name
            let protocol = ProtocolEntry (arg, optConstraints, doc, sigs)
            let optEnv = tryAppendDeclarationEntry protocolName protocol env
            match optEnv with
            | None -> None
            | Some env ->
                let entries =
                    List.map
                        (fun signature ->
                            let contingentArg = List.findIndex (fun sigArg -> sigArg.ArgName = arg) signature.SigArgs
                            (signature.SigName, DynamicEntry (contingentArg, signature.SigDoc)))
                        sigs
                let optEnv = tryAppendDeclarationEntries entries env
                match optEnv with
                | None -> None
                | Some env -> Some env

    /// Augment an environment with an instance.
    let tryAppendInstance protocolName args constraints namedSigImpls env =
        let firstArg = List.head args
        let firstConstraint = List.find (fun (constr : Constraint) -> constr.ConstrArgs.Head = firstArg) constraints
        let optFirstConstraintTypeEntry = tryFindTypeEntry firstConstraint.ConstrTypeName env
        match optFirstConstraintTypeEntry with
        | Some (TypeEntry (_, Composite composite, _)) ->
            for (name, sigImpl) in namedSigImpls do ignore (composite.CompSigImpls.ForceAdd (name, sigImpl))
            ignore (composite.CompProtocols.ForceAdd protocolName)
            true
        | _ -> false

    /// Query that a value has an instance of the given protocol.
    let hasProtocol protocolName value env =
        let vtype = getType value env
        match vtype with
        | Composite ctype -> ctype.CompProtocols.Contains protocolName
        | _ -> false

    /// Make a constraint predicate.
    let makeConstraintPredicate args1 (constraints1 : Dictionary<string, Constraint>) env =
        fun argValues1 ->
            List.forall2Plus
                (fun arg argValue ->
                    let constraintRef = ref Unchecked.defaultof<Constraint>
                    if constraints1.TryGetValue (arg.ArgName, constraintRef) then
                        let constr = !constraintRef
                        if hasType constr.ConstrTypeName argValue env then true
                        else hasProtocol constr.ConstrProtocolName argValue env
                    else true)
                args1
                argValues1

    /// Make a constraint predicate from a constraint list.
    let makeConstraintPredicateFromList args optConstraints env =
        match optConstraints with
        | None -> tautology
        | Some constraints ->
            let constraintDict = List.toDictionaryBy (fun constr -> (constr.ConstrArgs.Head, constr)) constraints
            makeConstraintPredicate args constraintDict env

    /// Augment an environment with a declaration function.
    let tryAppendDeclarationFunction name args argCount body optConstraints doc pre post emptyUnification optPositions env =
        let cpre = makeConstraintPredicateFromList args optConstraints env
        let lambda = Lambda (makeLambdaRecord false name args argCount body cpre pre post emptyUnification optPositions (Some env))
        tryAppendDeclarationEntry name (ValueEntry (lambda, doc)) env

    /// Augment an environment with a procedural function.
    let appendProceduralFunction appendType name args argCount body optConstraints doc pre post emptyUnification optPositions env =
        let cpre = makeConstraintPredicateFromList args optConstraints env
        let lambda = Lambda (makeLambdaRecord false name args argCount body cpre pre post emptyUnification optPositions (Some env))
        appendProceduralEntry appendType name (ValueEntry (lambda, doc)) env

    /// Augment an environment with a structure.
    let tryAppendStructure name memberNames optConstraints doc req argNames symbols optPositions env =

        // append type
        let typeName = TypePrefixStr + name
        let typeValue = makeType typeName optPositions
        let optEnv = tryAppendType typeName typeValue doc env
        match optEnv with
        | None -> None
        | Some env ->

            // append type indicator
            let typeIndicatorName = String.surround name TypeIndicatorStr
            let typeIndicatorMembers = List.toDictionaryBy (fun memName -> memName, (makeMember memName (makeViolationWithoutBreakpoint ":v/eval/typeIndicatorMemberAccess" "Cannot access the members of a type indicator."))) memberNames
            let typeIndicatorDoc = makeDoc ("A type indicator for a(n) '" + name + "' type.")
            let typeIndicatorValue = Composite (makeCompositeRecord false name typeIndicatorMembers typeValue null null optPositions)
            let optEnv = tryAppendDeclarationVariable typeIndicatorName typeIndicatorDoc typeIndicatorValue env
            match optEnv with
            | None -> None
            | Some env ->

                // append type query
                let isStructureArgs = [makeArg XStr Concrete UnitValue]
                let isStructureName = IsStr + (String.capitalize name)
                let hasTypeSymbol = Symbol (makeSymbolRecord HasTypeStr (ref CEUncached) optPositions)
                let selfSymbol = Symbol (makeSymbolRecord XStr (ref CEUncached) optPositions)
                let keywordTypeName = Keyword (makeKeywordRecord typeName optPositions)
                let isStructureBody = Series (makeSeriesRecord [hasTypeSymbol; keywordTypeName; selfSymbol] 3 optPositions)
                let optEnv = tryAppendDeclarationFunction isStructureName isStructureArgs 1 isStructureBody None doc UnitValue UnitValue true optPositions env
                match optEnv with
                | None -> None
                | Some env ->

                    // append constructor
                    let concreteArgs = List.map (fun argName -> makeArg argName Concrete UnitValue) argNames
                    let memberList = List.zipBy (fun (name, expr) -> makeMember name expr) memberNames symbols
                    let members = List.toDictionaryBy (fun mem -> (mem.MemName, mem)) memberList
                    let body = Composite (makeCompositeRecord false name members typeValue null null optPositions)
                    tryAppendDeclarationFunction name concreteArgs concreteArgs.Length body optConstraints doc req UnitValue true optPositions env

    /// Augment an environment with an affirmation function.
    let tryAppendAffirmationFunction name doc expr optPositions env =
        tryAppendDeclarationFunction name [] 0 expr None doc UnitValue UnitValue true optPositions env

    [<AutoOpen>]
    module ContraintValidationModule =

        type ConstraintValidity =
            | ValidConstraint
            | WrongArgCount
            | NonexistentTarget

        type ConstraintsValidity =
            | ValidConstraints
            | TooManyConstraints
            | InvalidConstraints of ConstraintValidity

        let getConstraintArgs constraints =
            let cargss = List.map (fun constr -> constr.ConstrArgs) constraints
            let cargsDup = List.concat cargss
            List.distinct cargsDup

        let getConstraintValidity constr env =
            let typeName = TypePrefixStr + constr.ConstrName
            let typeTarget = tryFindTypeEntry typeName env
            match typeTarget with
            | None ->
                let protocolName = ProtocolPrefixStr + constr.ConstrName
                let protocolTarget = tryFindProtocolEntry protocolName env
                match protocolTarget with
                | Some (ProtocolEntry _) -> if List.hasExactly 1 constr.ConstrArgs then ValidConstraint else WrongArgCount
                | _ -> NonexistentTarget
            | Some _ ->
                match constr.ConstrArgs with
                | [_] -> ValidConstraint
                | _ -> WrongArgCount

        let getConstraintsValidity pargs (optConstraints : Constraint list option) env =
            match optConstraints with
            | None -> ValidConstraints
            | Some constraints ->
                let cargs = getConstraintArgs constraints
                if List.isSubset pargs cargs then
                    let constraintValidities = List.map (fun constr -> getConstraintValidity constr env) constraints
                    let optConstraintInvalidity = List.tryFind (fun validity -> validity <> ValidConstraint) constraintValidities
                    match optConstraintInvalidity with
                    | None -> ValidConstraints
                    | Some (_ as invalidity) -> InvalidConstraints invalidity
                else TooManyConstraints

        let getOptConstraintsViolation pargs (optConstraints : Constraint list option) env =
            let constraintsValidity = getConstraintsValidity pargs optConstraints env
            match constraintsValidity with
            | ValidConstraints -> None
            | TooManyConstraints -> Some (makeViolationWithPositions ":v/eval/tooManyConstraints" "Too many constraints for target." env)
            | InvalidConstraints constraintInvalidity ->
                match constraintInvalidity with
                | ValidConstraint -> failwith "Unexpected match failure in 'Aml.Evaluator.evalProtocol'."
                | WrongArgCount -> Some (makeViolationWithPositions ":v/eval/invalidConstraintArguments" "Wrong number of arguments for target's constraints." env)
                | NonexistentTarget -> Some (makeViolationWithPositions ":v/eval/invalidConstraintTarget" "Constraint target does not exist." env)

    [<AutoOpen>]
    module SignatureValidationModule =

        type SignatureValidity =
            | ValidSignature
            | NoSignatureArgs
            | InvalidSignatureArgs

        type SignaturesValidity =
            | ValidSignatures
            | InvalidSignatures of SignatureValidity

        let getSignatureValidity parg (signature : Signature) =
            if signature.SigArgs.IsEmpty then NoSignatureArgs
            else
                let sargNames = List.map (fun sarg -> sarg.ArgName) signature.SigArgs
                if not (List.contains parg sargNames) then InvalidSignatureArgs
                else ValidSignature

        let getSignaturesValidity parg (sigs : Signature list) =
            let sigValidities = List.map (getSignatureValidity parg) sigs
            let optSigInvalidity = List.tryFind (fun validity -> validity <> ValidSignature) sigValidities
            match optSigInvalidity with
            | None -> ValidSignatures
            | Some (_ as invalidity) -> InvalidSignatures invalidity

        let getSignaturesViolation parg (sigs : Signature list) env =
            let sigsValidity = getSignaturesValidity parg sigs
            match sigsValidity with
            | ValidSignatures -> None
            | InvalidSignatures sigInvalidity ->
                match sigInvalidity with
                | ValidSignature -> failwith "Unexpected match failure in 'Aml.Evaluator.evalProtocol'."
                | NoSignatureArgs -> Some (makeViolationWithPositions ":v/eval/missingProtocolSignatureArguments" "Protocol signature must have at least one argument." env)
                | InvalidSignatureArgs -> Some (makeViolationWithPositions ":v/eval/invalidProtocolSignatureArguments" "A signature must use the protocol's argument." env)

        (* Argument Unification *)

        type [<ReferenceEquality>] PartialUnifyResult =
            { OptUnifiedArg : (Expr * Arg) option
              RestArgs : Expr list
              LargAdvances : int
              LargShouldAdvance : bool }

        let makePartialUnifyResult optUnifiedArg restArgs largAdvances largShouldAdvance = {
            OptUnifiedArg = optUnifiedArg
            RestArgs = restArgs
            LargAdvances = largAdvances
            LargShouldAdvance = largShouldAdvance }

        // TODO: clean up this function with extraction
        let tryPartialUnifyArgs (argsEvaluated : bool) (args1 : Expr list) (largs1 : Arg list) larg =
            match larg.ArgType with
            | Concrete
            | Abstracting ->
                let unifiedArg = Some (args1.Head, larg)
                makePartialUnifyResult unifiedArg args1.Tail 0 true
            | Labeled ->
                match args1.Head with
                | Package package ->
                    if package.PkgName = larg.ArgName then
                        let unifiedArg = Some (package.PkgExpr, larg)
                        makePartialUnifyResult unifiedArg args1.Tail 0 true
                    else
                        let matchingLargIndex = List.findIndex (fun larg -> larg.ArgName = package.PkgName) largs1
                        let matchingLarg = largs1.[matchingLargIndex]
                        let unifiedArg = Some (package.PkgExpr, matchingLarg)
                        makePartialUnifyResult unifiedArg args1.Tail matchingLargIndex true
                | _ ->
                    let unifiedArg = Some (args1.Head, larg)
                    makePartialUnifyResult unifiedArg args1.Tail 0 true
            | Variadic ->
                let argList = List (makeListRecord argsEvaluated args1 None)
                let unifiedArg = Some (argList, larg)
                makePartialUnifyResult unifiedArg [] 0 false

        let partialUnifyArgs (argsEvaluated : bool) (args : Expr list) (largs : Arg list) =
            // OPTIMIZATION: choose a unification lazily to save time
            if args.IsEmpty then Some (makePartialUnifyResult None [] largs.Length false)
            else
                let chosen = Seq.map (tryPartialUnifyArgs argsEvaluated args largs) largs
                Seq.tryHead chosen

        let advanceArgs (largs : Arg list) count =
            // TODO: see if a sequence can be sliced without redundant evaluation
            let advancedLargs = List.take count largs
            let restLargs = List.skip count largs
            let unifiedArgs = List.map (fun larg -> (larg.ArgExpr, larg)) advancedLargs
            (unifiedArgs, restLargs)

        let rec unifyArgs (argsEvaluated : bool)(args : Expr list) (largs : Arg list) =
            if args.IsEmpty && List.fornone (fun larg -> larg.ArgType = Labeled) largs then []
            else
                let optPartialUnifyResult = partialUnifyArgs argsEvaluated args largs
                match optPartialUnifyResult with
                | None -> [(args.Head, makeArg MissingStr Concrete UnitValue)]
                | Some r ->
                    let (unifiedArgs, restLargs) = advanceArgs largs r.LargAdvances
                    let restLargs2 = if r.LargShouldAdvance then restLargs.Tail else restLargs
                    let unifiedRest = unifyArgs argsEvaluated r.RestArgs restLargs2
                    let unifiedRest2 = match r.OptUnifiedArg with None -> unifiedRest | Some unifiedArg -> unifiedArg :: unifiedRest
                    unifiedArgs @ unifiedRest2

        let tryUnifyArgs (argsEvaluated : bool) (args : Expr list) (largs : Arg list) =
            let unifiedZipped = unifyArgs argsEvaluated args largs
            if List.areSameLength largs unifiedZipped then Some (List.unzip unifiedZipped)
            else None

        (* Constraint Projection *)

        let writeConstraintFailure (_, (constr, parg)) =
            "(" + constr.ConstrArgs.Head + " is " + parg + ")"

        let writeConstraintFailures constraintProjections =
            let constraintFailures = List.filter (fun (result, _) -> not result) constraintProjections
            let constraintFailuresString = List.joinBy writeConstraintFailure SpaceStr constraintFailures
            "Could not satisfy constraint(s) '" + constraintFailuresString + "'."

        let projectConstraintToProtocolArg (instances, matches) ((constr, parg : string) as instance) =
            let optPrevConstraint = Map.tryFind parg matches
            match optPrevConstraint with
            | None -> ((true, instance) :: instances, Map.add parg constr matches)
            | Some prevConstraint ->
                if prevConstraint.ConstrName = constr.ConstrName then ((true, instance) :: instances, matches)
                else ((false, instance) :: instances, matches)

        let projectConstraintsToProtocolArg (constraints : Constraint list) parg =
            let paddedPargs = List.padWithLastToProportion [parg] constraints
            let zipped = List.zip constraints paddedPargs
            let (constraintInstances, _) = List.fold projectConstraintToProtocolArg (List.empty, Map.empty) zipped
            let allConstraintsSuccessful = List.fornone (fun (result, _) -> not result) constraintInstances
            (allConstraintsSuccessful, constraintInstances)