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
    let makeEvalResult env value = { Env = env; Value = value }

    /// Make an evaluation result with unit as the value.
    let makeEvalUnit env = makeEvalResult env UnitValue

    /// Make an evaluation result whose environment has had its repl line configured.
    let configureEvalResultFirstReplLine (result : EvalResult) line =
#if AML_OPTIMIZED
        result
#else
        let resultEnv = result.Env
        let configuredEnv = { resultEnv with EnvDebugInfo = { resultEnv.EnvDebugInfo with DIOptFirstReplLine = line }}
        makeEvalResult configuredEnv result.Value
#endif

    /// Make an evaluation result whose environment has had its expr trace popped.
    let popEvalResultExpr (result : EvalResult) =
        let poppedEnv = popExpr result.Env
        makeEvalResult poppedEnv result.Value

    /// Make an evaluation result whose environment has had its stack trace popped.
    let popEvalResultStackFrame (result : EvalResult) =
        let poppedEnv = popStackFrame result.Env
        makeEvalResult poppedEnv result.Value

    /// Get a composite's type.
    let getTypeOfComposite composite =
        let aType = composite.CompType
        if isUnit aType then CompositeType else aType

    /// Try to find a declaration entry.
    let tryFindDeclarationEntry env name =
        let entry = ref Unchecked.defaultof<EnvEntry>
        if env.EnvDeclarationFrame.TryGetValue (name, entry) then Some !entry
        else None

    /// Try to find a declaration entry by the given search method.
    let tryFindDeclarationEntryBy env by name =
        let typeEntry = tryFindDeclarationEntry env name
        by typeEntry

    /// Try to find a procedural entry plus directions on how to access it again.
    let tryFindProceduralEntry env name =
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
    let tryFindEntry env name =
        let optEntry =
            List.tryFindPlus
                (fun (frame : ProceduralFrame) ->
                    let optEntry = Array.tryFind (fun (entryName, _) -> name = entryName) frame
                    match optEntry with None -> (false, None) | Some (_, entry) -> (true, Some entry))
                env.EnvProceduralFrames
        match optEntry with
        | None -> tryFindDeclarationEntry env name
        | Some (Some entry) -> Some entry
        | Some None -> failwith "Unexpected match failure in 'Aml.Evaluator.Prims.tryFindEntry'."

    /// Try to find a dynamic entry. 
    let tryFindDynamicEntry env name =
        let findBy = function | Some (DynamicEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy env findBy name

    /// Try to find a protocol entry. 
    let tryFindProtocolEntry env name =
        let findBy = function | Some (ProtocolEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy env findBy name

    /// Try to find a type entry.
    let tryFindTypeEntry env typeName =
        let findBy = function | Some (TypeEntry _ as value) -> Some value | _ -> None
        tryFindDeclarationEntryBy env findBy typeName

    /// Try to find a type.
    let tryFindType env typeName =
        let findBy = function | Some (TypeEntry (typeName, value, doc)) -> Some (typeName, value, doc) | _ -> None
        tryFindDeclarationEntryBy env findBy typeName

    /// Try to find a protocol.
    let tryFindProtocol env protocolName =
        let findBy =
            function
            | Some (ProtocolEntry (arg, optConstraints, doc, signatures)) -> Some (arg, optConstraints, doc, signatures)
            | _ -> None
        tryFindDeclarationEntryBy env findBy protocolName

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
    let hasDeclarationEntry env name =
        (tryFindDeclarationEntry env name).IsSome

    /// Query that a symbol refers to a built-in operator.
    let isBuiltin env expr =
        match expr with
        | Symbol symbol ->
            if InitialBuiltinNames.Contains symbol.SymName then true
            else
                match env.EnvOptLanguageModule with
                | None -> false
                | Some lm -> lm.IsSpecialBuiltin env symbol.SymName
        | _ -> false

    /// Expand an entry to a lambda record represenation.
    let expandLambda lambda =
        match lambda with
        | Lambda lambda -> lambda
        | _ -> failwith "Unexpected match failure in 'Aml.Evaluator.Prims.expandLambda'."
    
    /// Get a member from some composite members.
    let getMember env memberName (members : MemberDict) =
        let mem = ref Unchecked.defaultof<Member>
        if members.TryGetValue (memberName, mem) then (!mem).MemExpr
        else makeViolationWithPositions env ":v/eval/missingMember" ("Member '" + memberName + "' does not exist.")

    /// Get a value's type (not from the environment).
    let getType env value =
        match value with
        | SpecialObject _ | Prefixed _ ->
            match env.EnvOptLanguageModule with
            | None -> makeViolationWithPositions env ":v/languageModule/missingLanguageModule" "Cannot get type of special value without a language module."
            | Some lm -> lm.GetSpecialType env value
        | _ ->
            let optType = tryFindTypeByValue value
            match optType with
            | None -> makeViolationWithPositions env ":v/eval/invalidTypeQuery" "Invalid type query."
            | Some aType -> aType

    /// Query that a value has the given type.
    let hasType env typeName value =
        let vtype = getType env value
        match vtype with
        | Composite composite -> composite.CompName = typeName
        | _ -> false

    /// Augment an environment with a protocol.
    /// OPTIMIZATION: implemented with Seq.
    /// TODO: clean up this function with extraction.
    let tryAppendProtocol env name arg optConstraints doc sigs =
        let sigsMatchingEntry = Seq.filter (fun signature -> (tryFindDeclarationEntry env signature.SigName).IsSome) sigs
        let optFirstSigMatchingEntry = Seq.tryHead sigsMatchingEntry
        match optFirstSigMatchingEntry with
        | Some firstSigMatchingEntry when not env.EnvAllowRedeclaration ->
            failwith ("The protocol signature '" + firstSigMatchingEntry.SigName + "' clashes names with an existing declaration.")
        | _ ->
            let protocolName = ProtocolPrefixStr + name
            let protocol = ProtocolEntry (arg, optConstraints, doc, sigs)
            let optEnv = tryAppendDeclarationEntry env protocolName protocol
            match optEnv with
            | None -> None
            | Some env ->
                let entries =
                    List.map
                        (fun signature ->
                            let contingentArg = List.findIndex (fun sigArg -> sigArg.ArgName = arg) signature.SigArgs
                            (signature.SigName, DynamicEntry (contingentArg, signature.SigDoc)))
                        sigs
                let optEnv = tryAppendDeclarationEntries env entries
                match optEnv with
                | None -> None
                | Some env -> Some env

    /// Augment an environment with an instance.
    let tryAppendInstance env protocolName args constraints namedSigImpls =
        let firstArg = List.head args
        let firstConstraint = List.find (fun (constr : Constraint) -> constr.ConstrArgs.Head = firstArg) constraints
        let optFirstConstraintTypeEntry = tryFindTypeEntry env firstConstraint.ConstrTypeName
        match optFirstConstraintTypeEntry with
        | Some (TypeEntry (_, Composite composite, _)) ->
            for (name, sigImpl) in namedSigImpls do ignore (composite.CompSigImpls.ForceAdd (name, sigImpl))
            ignore (composite.CompProtocols.ForceAdd protocolName)
            true
        | _ -> false

    /// Query that a value has an instance of the given protocol.
    let hasProtocol env protocolName value =
        let vtype = getType env value
        match vtype with
        | Composite ctype -> ctype.CompProtocols.Contains protocolName
        | _ -> false

    /// Make a constraint predicate.
    let makeConstraintPredicate env args1 (constraints1 : Dictionary<string, Constraint>) =
        fun argValues1 ->
            List.forall2Plus
                (fun arg argValue ->
                    let constraintRef = ref Unchecked.defaultof<Constraint>
                    if constraints1.TryGetValue (arg.ArgName, constraintRef) then
                        let constr = !constraintRef
                        if hasType env constr.ConstrTypeName argValue then true
                        else hasProtocol env constr.ConstrProtocolName argValue
                    else true)
                args1
                argValues1

    /// Make a constraint predicate from a constraint list.
    let makeConstraintPredicateFromList env args optConstraints =
        match optConstraints with
        | None -> tautology
        | Some constraints ->
            let constraintDict = List.toDictionaryBy (fun constr -> (constr.ConstrArgs.Head, constr)) constraints
            makeConstraintPredicate env args constraintDict

    /// Augment an environment with a declaration function.
    let tryAppendDeclarationFunction env name args argCount body optConstraints doc pre post emptyUnification optPositions =
        let cpre = makeConstraintPredicateFromList env args optConstraints
        let lambda = Lambda (makeLambdaRecord false name args argCount body cpre pre post emptyUnification optPositions (Some env))
        tryAppendDeclarationEntry env name (ValueEntry (lambda, doc))

    /// Augment an environment with a procedural function.
    let appendProceduralFunction env appendType name args argCount body optConstraints doc pre post emptyUnification optPositions =
        let cpre = makeConstraintPredicateFromList env args optConstraints
        let lambda = Lambda (makeLambdaRecord false name args argCount body cpre pre post emptyUnification optPositions (Some env))
        appendProceduralEntry env appendType name (ValueEntry (lambda, doc))

    /// Augment an environment with a structure.
    let tryAppendStructure env name memberNames optConstraints doc req argNames symbols optPositions =

        // append type
        let typeName = TypePrefixStr + name
        let typeValue = makeType typeName optPositions
        let optEnv = tryAppendType env typeName typeValue doc
        match optEnv with
        | None -> None
        | Some env ->

            // append type indicator
            let typeIndicatorName = String.surround name TypeIndicatorStr
            let typeIndicatorMembers = List.toDictionaryBy (fun memName -> memName, (makeMember memName (makeViolationWithoutBreakpoint ":v/eval/typeIndicatorMemberAccess" "Cannot access the members of a type indicator."))) memberNames
            let typeIndicatorDoc = makeDoc ("A type indicator for a(n) '" + name + "' type.")
            let typeIndicatorValue = Composite (makeCompositeRecord false name typeIndicatorMembers typeValue null null optPositions)
            let optEnv = tryAppendDeclarationVariable env typeIndicatorName typeIndicatorDoc typeIndicatorValue
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
                let optEnv = tryAppendDeclarationFunction env isStructureName isStructureArgs 1 isStructureBody None doc UnitValue UnitValue true optPositions
                match optEnv with
                | None -> None
                | Some env ->

                    // append constructor
                    let concreteArgs = List.map (fun argName -> makeArg argName Concrete UnitValue) argNames
                    let memberList = List.zipBy (fun (name, expr) -> makeMember name expr) memberNames symbols
                    let members = List.toDictionaryBy (fun mem -> (mem.MemName, mem)) memberList
                    let body = Composite (makeCompositeRecord false name members typeValue null null optPositions)
                    tryAppendDeclarationFunction env name concreteArgs concreteArgs.Length body optConstraints doc req UnitValue true optPositions

    /// Augment an environment with an affirmation function.
    let tryAppendAffirmationFunction env name doc expr optPositions =
        tryAppendDeclarationFunction env name [] 0 expr None doc UnitValue UnitValue true optPositions

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

        let getConstraintValidity env constr =
            let typeName = TypePrefixStr + constr.ConstrName
            let typeTarget = tryFindTypeEntry env typeName
            match typeTarget with
            | None ->
                let protocolName = ProtocolPrefixStr + constr.ConstrName
                let protocolTarget = tryFindProtocolEntry env protocolName
                match protocolTarget with
                | Some (ProtocolEntry _) -> if List.hasExactly 1 constr.ConstrArgs then ValidConstraint else WrongArgCount
                | _ -> NonexistentTarget
            | Some _ ->
                match constr.ConstrArgs with
                | [_] -> ValidConstraint
                | _ -> WrongArgCount

        let getConstraintsValidity env pargs (optConstraints : Constraint list option) =
            match optConstraints with
            | None -> ValidConstraints
            | Some constraints ->
                let cargs = getConstraintArgs constraints
                if List.isSubset pargs cargs then
                    let constraintValidities = List.map (getConstraintValidity env) constraints
                    let optConstraintInvalidity = List.tryFind (fun validity -> validity <> ValidConstraint) constraintValidities
                    match optConstraintInvalidity with
                    | None -> ValidConstraints
                    | Some (_ as invalidity) -> InvalidConstraints invalidity
                else TooManyConstraints

        let getOptConstraintsViolation env pargs (optConstraints : Constraint list option) =
            let constraintsValidity = getConstraintsValidity env pargs optConstraints
            match constraintsValidity with
            | ValidConstraints -> None
            | TooManyConstraints -> Some (makeViolationWithPositions env ":v/eval/tooManyConstraints" "Too many constraints for target.")
            | InvalidConstraints constraintInvalidity ->
                match constraintInvalidity with
                | ValidConstraint -> failwith "Unexpected match failure in 'Aml.Evaluator.evalProtocol'."
                | WrongArgCount -> Some (makeViolationWithPositions env ":v/eval/invalidConstraintArguments" "Wrong number of arguments for target's constraints.")
                | NonexistentTarget -> Some (makeViolationWithPositions env ":v/eval/invalidConstraintTarget" "Constraint target does not exist.")

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

        let getSignaturesViolation env parg (sigs : Signature list) =
            let sigsValidity = getSignaturesValidity parg sigs
            match sigsValidity with
            | ValidSignatures -> None
            | InvalidSignatures sigInvalidity ->
                match sigInvalidity with
                | ValidSignature -> failwith "Unexpected match failure in 'Aml.Evaluator.evalProtocol'."
                | NoSignatureArgs -> Some (makeViolationWithPositions env ":v/eval/missingProtocolSignatureArguments" "Protocol signature must have at least one argument.")
                | InvalidSignatureArgs -> Some (makeViolationWithPositions env ":v/eval/invalidProtocolSignatureArguments" "A signature must use the protocol's argument.")

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