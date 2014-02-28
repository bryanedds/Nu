// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

module Aml.Primitives
open System
open System.Collections.Generic
open System.IO
open Prime
open Aml.Ast
open Aml.AmlConstants

/// The manner in which entries are appended to a procedural frame.
type AppendType =
    | AppendToNewFrame of int (*size*)
    | AppendToHeadFrame of int (*offset*)

/// Get the procedural children from an expression.
let getProceduralChildren parent =
    match parent with
    | Package p -> [p.PkgExpr]
    | Prefixed p -> [p.PxdExpr]
    | Series s -> s.SerExprs
    | Lambda l -> [l.LamBody; l.LamPre; l.LamPost]
    | Attempt a -> a.AttemptBody :: List.map (fun ab -> ab.ABBody) a.AttemptBranches
    | Let l -> l.LetBody :: List.concat (List.map (fun lb -> match lb with LetVariable (_, expr) -> [expr] | LetFunction (_, _, _, body, _, pre, post, _) -> [body; pre; post]) l.LetBindings)
    | Extend e -> e.ExtTarget :: List.map (fun mem -> mem.MemExpr) (List.ofSeq e.ExtMembers.Values)
    | Case c -> c.CaseTarget :: List.concat (List.map (fun branch -> [branch.TBBody; branch.TBTest]) c.CaseBranches)
    | Condition c -> List.concat (List.map (fun branch -> [branch.TBBody; branch.TBTest]) c.CondBranches)
    | Intervene i -> List.map (fun branch -> branch.IBBody) i.ItvBranches
    | List l -> l.ListElements
    | Array a -> Array.toList a.ArrElements
    | Composite c -> List.map (fun mem -> mem.MemExpr) (List.ofSeq c.CompMembers.Values)
    | Selector s -> [s.SelTarget; s.SelKey]
    | SpecialSeries s -> s.SSExprs
    | _ -> []

/// Get the optional parser positions from an expression.
let getOptPositions expr =
    match expr with
    | Violation v -> v.VioOptPositions
    | Boolean b -> b.BROptPositions
    | Character c -> c.CROptPositions
    | String s -> s.SROptPositions
    | Int i -> i.IROptPositions
    | Long l -> l.GROptPositions
    | Float f -> f.FROptPositions
    | Double d -> d.DROptPositions
    | Keyword k -> k.KROptPositions
    | Symbol s -> s.SymOptPositions
    | Package p -> p.PkgOptPositions
    | Prefixed p -> p.PxdOptPositions
    | Dispatch d -> d.DispOptPositions
    | SpecialValue s -> s.SVOptPositions
    | SpecialObject s -> s.SOOptPositions
    | Series s -> s.SerOptPositions
    | Lambda l -> l.LamOptPositions
    | Attempt a -> a.AttemptOptPositions
    | Let l -> l.LetOptPositions
    | Extend e -> e.ExtOptPositions
    | Case c -> c.CaseOptPositions
    | Condition c -> c.CondOptPositions
    | Intervene i -> i.ItvOptPositions
    | Ref r -> r.RefOptPositions
    | Get g -> g.GetOptPositions
    | Set s -> s.SetOptPositions
    | List l -> l.ListOptPositions
    | Array a -> a.ArrOptPositions
    | Composite c -> c.CompOptPositions
    | Selector s -> s.SelOptPositions
    | Variable v -> v.VarOptPositions
    | Function f -> f.FnOptPositions
    | Structure s -> s.StructOptPositions
    | Protocol p -> p.ProtoOptPositions
    | Instance i -> i.InstOptPositions
    | Affirmation a -> a.AffOptPositions
    | UsingFile u -> u.UFOptPositions
    | UsingLanguage u -> u.ULOptPositions
    | SpecialSeries s -> s.SSOptPositions

let tryGetSpecialId expr =
    match expr with
    | Prefixed prefixed -> Some prefixed.PxdSpecialId
    | SpecialSeries specialSeries -> Some specialSeries.SSSpecialId
    | _ -> None

/// Query that a subcategory is in a category.
let isInCategory category value =
    let categoryStr = category.LunStr
    let valueStr = value.LunStr
    categoryStr.StartsWith valueStr &&
        let overlapIndex = valueStr.Length
        categoryStr.Length = overlapIndex || categoryStr.[overlapIndex] = NameSeparatorChar

/// An Aml unit value.
let UnitValue = Series { SerExprs = []; SerExprCount = 0; SerOptPositions = None }
    
/// A nil value for an entry in a procedural frame.
/// Should ever exist ONLY in an uninitialized procedural frame!
let NilValue =
    Violation {
        VioEvaluated = true
        VioCategory = Lun.make ":v/eval/nilAccess"
        VioMessage = { SVValue = "Accessed a nil value."; SVType = LiteralString }
        VioData = UnitValue
        VioOptPositions = None }

/// A nil entry for a procedural frame.
/// Should ever exist ONLY in an uninitialized procedural frame!
let NilEntry = (Lun.empty, ValueEntry (NilValue, None))

/// An Aml true value.
let TrueValue = Boolean { BRValue = true; BROptPositions = None }

/// An Aml false value.
let FalseValue = Boolean { BRValue = false; BROptPositions = None }

/// An Aml empty string value.
let EmptyStringValue = String { SRValue = { SVValue = String.Empty; SVType = LiteralString }; SROptPositions = None }

/// An Aml zero int value.
let ZeroIntValue = Int { IRValue = 0; IROptPositions = None }

/// Make an argument.
let makeArg name argType expr = {
    ArgName = name
    ArgType = argType
    ArgExpr = expr }

/// Make an argument from a read name.
let makeArgFromName name =
    let (isVariadic, nameStr) = String.withEnd name.LunStr EllipsisStr
    if isVariadic then makeArg (Lun.make nameStr) Variadic UnitValue
    else makeArg name Concrete UnitValue

/// Make an argument from a package.
let makeArgFromPackage package =
    makeArg package.PkgName Labeled package.PkgExpr

/// Make an argument from a violation.
let makeArgFromViolation violation =
    makeArg violation.VioCategory Abstracting violation.VioData

/// Make a member.
let makeMember name expr = {
    MemName = name
    MemExpr = expr }

/// Make a test branch.
let makeTestBranch test body = {
    TBTest = test
    TBBody = body }

/// Make a signature.
let makeSignature name args doc = {
    SigName = name
    SigArgs = args
    SigDoc = doc }

/// Make a constraint.
let makeConstraint name args = {
    ConstrName = name
    ConstrTypeName = Lun.make TypePrefixStr ++ name
    ConstrProtocolName = Lun.make ProtocolPrefixStr ++ name
    ConstrArgs = args }

/// Make an attempt branch.
let makeAttemptBranch category body = {
    ABCategory = category
    ABBody = body }

/// Make an intervention branch.
let makeInterventionBranch env category body hide = {
    IBEnv = env
    IBCategory = category
    IBBody = body
    IBHide = hide }

/// Make a ParserPositions value.
let makeParserPositions start stop = {
    ParStart = start
    ParStop = stop }

/// Make a StringValue.
let makeStringValue value stringType = {
    SVValue = value
    SVType = stringType }

/// Make a literal StringValue.
let makeLiteralStringValue value =
    makeStringValue value LiteralString

/// Make a verbatim StringValue.
let makeVerbatimStringValue value =
    makeStringValue value VerbatimString

/// Make a documentation value.
let makeDoc value =
    Some (makeLiteralStringValue value)

/// Make a ViolationRecord.
let makeViolationRecord evaluated category message data optPositions = {
    VioEvaluated = evaluated
    VioCategory = category
    VioMessage = message
    VioData = data
    VioOptPositions = optPositions }

/// Make a violation from a category and a message.
/// For internal use only.
let private makeViolationInternal (category : string) message =
    let messageValue = makeVerbatimStringValue message
    if category.StartsWith ViolationPrefixStr
    then Violation (makeViolationRecord true (Lun.make category) messageValue UnitValue None)
    else failwith ("Violation category must start with '" + ViolationPrefixStr + "'.")

/// Make a violation from a category and a message.
/// No breakpoint can be placed in here.
let makeViolationWithoutBreakpoint = makeViolationInternal

/// Make a violation from a category and a message.
/// Breakpoints can be set in here when debugging for unexpected violations.
let makeViolation category message =
    makeViolationInternal category message

/// Make a BooleanRecord.
let makeBooleanRecord value optPositions = {
    BRValue = value
    BROptPositions = optPositions }

/// Make a CharacterRecord.
let makeCharacterRecord value optPositions = {
    CRValue = value
    CROptPositions = optPositions }

/// Make a StringRecord.
let makeStringRecord value optPositions = {
    SRValue = value
    SROptPositions = optPositions }

/// Make a IntRecord.
let makeIntRecord value optPositions = {
    IRValue = value
    IROptPositions = optPositions }

/// Make a LongRecord.
let makeLongRecord value optPositions = {
    GRValue = value
    GROptPositions = optPositions }

/// Make a FloatRecord.
let makeFloatRecord value optPositions = {
    FRValue = value
    FROptPositions = optPositions }

/// Make a DoubleRecord.
let makeDoubleRecord value optPositions = {
    DRValue = value
    DROptPositions = optPositions }

/// Make a KeywordRecord.
let makeKeywordRecord value optPositions = {
    KRValue = value
    KROptPositions = optPositions }

/// Make a SymbolRecord.
let makeSymbolRecord name cachedEntry optPositions = {
    SymName = name
    SymCachedEntry = cachedEntry
    SymOptPositions = optPositions }

/// Make a PackageRecord.
let makePackageRecord name expr optPositions = {
    PkgName = name
    PkgExpr = expr
    PkgOptPositions = optPositions }

/// Make a PrefixedRecord.
let makePrefixedRecord prefixType value specialId optPositions = {
    PxdType = prefixType
    PxdExpr = value
    PxdSpecialId = specialId
    PxdOptPositions = optPositions }

/// Make a DispatchRecord.
let makeDispatchRecord name contingentArg optPositions = {
    DispName = name
    DispContingentArg = contingentArg
    DispOptPositions = optPositions }

/// Make a SpecialValueRecord.
let makeSpecialValueRecord evaluated languageName expr optPositions = {
    SVEvaluated = evaluated
    SVLanguageName = languageName
    SVExpr = expr
    SVOptPositions = optPositions }

/// Make a SpecialObjectRecord.
let makeSpecialObjectRecord evaluated languageGuid content optPositions = {
    SOEvaluated = evaluated
    SOLanguageGuid = languageGuid
    SOContent = content
    SOOptPositions = optPositions }

/// Make a SeriesRecord.
let makeSeriesRecord exprs exprCount optPositions = {
    SerExprs = exprs
    SerExprCount = exprCount
    SerOptPositions = optPositions }

/// Make a LambdaRecord.
let makeLambdaRecord evaluated name args argCount body cpre pre post emptyUnification optPositions env = {
    LamEvaluated = evaluated
    LamName = name
    LamArgs = args
    LamArgCount = argCount
    LamBody = body
    LamCpre = cpre
    LamPre = pre
    LamPost = post
    LamEmptyUnification = emptyUnification
    LamOptPositions = optPositions
    LamEnv = env }

/// Make an AttemptRecord.
let makeAttemptRecord body branches optPositions = {
    AttemptBody = body
    AttemptBranches = branches
    AttemptOptPositions = optPositions }

/// Make a LetRecord.
let makeLetRecord bindings bindingCount body optPositions = {
    LetBindings = bindings
    LetBindingCount = bindingCount
    LetBody = body
    LetOptPositions = optPositions }

/// Make an ExtendRecord.
let makeExtendRecord target members optPositions = {
    ExtTarget = target
    ExtMembers = members
    ExtOptPositions = optPositions }

/// Make a CaseRecord.
let makeCaseRecord target branches optPositions = {
    CaseTarget = target
    CaseBranches = branches
    CaseOptPositions = optPositions }

/// Make a ConditionRecord.
let makeConditionRecord branches optPositions = {
    CondBranches = branches
    CondOptPositions = optPositions }

/// Make an InterveneRecord.
let makeInterveneRecord body branches optPositions = {
    ItvBody = body
    ItvBranches = branches
    ItvOptPositions = optPositions }

/// Make a RefRecord.
let makeRefRecord evaluated expr optPositions = {
    RefEvaluated = evaluated
    RefExpr = expr
    RefOptPositions = optPositions }

/// Make a GetRecord.
let makeGetRecord target optPositions = {
    GetTarget = target
    GetOptPositions = optPositions }

/// Make a SetRecord.
let makeSetRecord target injection optPositions = {
    SetTarget = target
    SetInjection = injection
    SetOptPositions = optPositions }

/// Make a ListRecord.
let makeListRecord evaluated elements optPositions = {
    ListEvaluated = evaluated
    ListElements = elements
    ListOptPositions = optPositions }

/// Make an ArrayRecord.
let makeArrayRecord evaluated elements optPositions = {
    ArrEvaluated = evaluated
    ArrElements = elements
    ArrOptPositions = optPositions }

/// Make a CompositeRecord.
let makeCompositeRecord evaluated name members aType sigImpls protocols optPositions = {
    CompEvaluated = evaluated
    CompName = name
    CompMembers = members
    CompType = aType
    CompSigImpls = sigImpls
    CompProtocols = protocols
    CompOptPositions = optPositions }

/// Make an SelectorRecord.
let makeSelectorRecord key target accType optPositions = {
    SelKey = key
    SelTarget = target
    SelType = accType
    SelOptPositions = optPositions }
    
/// Make a VariableRecord.
let makeVariableRecord name body doc optPositions = {
    VarName = name
    VarBody = body
    VarDoc = doc
    VarOptPositions = optPositions }

/// Make a FunctionRecord.
let makeFunctionRecord name args argCount body optConstraints doc pre post emptyUnification optPositions = {
    FnName = name
    FnArgs = args
    FnArgCount = argCount
    FnBody = body
    FnOptConstraints = optConstraints
    FnDoc = doc
    FnPre = pre
    FnPost = post
    FnEmptyUnification = emptyUnification
    FnOptPositions = optPositions }

/// Make a StructureRecord.
let makeStructureRecord name memberNames optConstraints doc req optPositions = {
    StructName = name
    StructMemberNames = memberNames
    StructOptConstraints = optConstraints
    StructDoc = doc
    StructReq = req
    StructOptPositions = optPositions }

/// Make a ProtocolRecord.
let makeProtocolRecord name arg optConstraints doc sigs optPositions = {
    ProtoName = name
    ProtoArg = arg
    ProtoOptConstraints = optConstraints
    ProtoDoc = doc
    ProtoSignatures = sigs
    ProtoOptPositions = optPositions }

/// Make an InstanceRecord.
let makeInstanceRecord protocolName args constraints functions optPositions = {
    InstProtocolName = protocolName
    InstArgs = args
    InstConstraints = constraints
    InstFunctions = functions
    InstOptPositions = optPositions }

/// Make an AffirmationRecord.
let makeAffirmationRecord name doc body optPositions = {
    AffName = name
    AffDoc = doc
    AffBody = body
    AffOptPositions = optPositions }

/// Make a UsingFileRecord.
let makeUsingFileRecord path reload optPositions = {
    UFPath = path
    UFReload = reload
    UFOptPositions = optPositions }

/// Make a UsingLanguageRecord.
let makeUsingLanguageRecord path aType optPositions = {
    ULPath = path
    ULType = aType
    ULOptPositions = optPositions }

/// Make a SpecialSeriesRecord.
let makeSpecialSeriesRecord ssType exprs exprCount specialId optPositions = {
    SSType = ssType
    SSExprs = exprs
    SSExprCount = exprCount
    SSSpecialId = specialId
    SSOptPositions = optPositions }

/// Make a declaration frame.
let makeDeclarationFrame () =
    DeclarationFrame ()

/// Make a procedural frame.
let makeProceduralFrame size =
    Array.create size NilEntry

/// Make debug info.
let makeDebugInfo optFirstReplLine exprTrace stackTrace =
    { DIOptFirstReplLine = optFirstReplLine
      DIExprTrace = exprTrace
      DIStackTrace = stackTrace }

/// Make an environment.
let makeEnv
        declarationFrame
        proceduralFrames
        cachedDeclarationEntries
        allowRedeclaration
        debugInfo
        usingFiles
        interventionBranchLists
        optLanguageModule
        optWorkBench
        path
        recordingCount
        recordedEnvs =
        { EnvDeclarationFrame = declarationFrame
          EnvProceduralFrames = proceduralFrames
          EnvCachedDeclarationEntries = cachedDeclarationEntries
          EnvAllowRedeclaration = allowRedeclaration
          EnvDebugInfo = debugInfo
          EnvUsingFiles = usingFiles
          EnvInterventionBranchLists = interventionBranchLists
          EnvOptLanguageModule = optLanguageModule
          EnvOptWorkBench = optWorkBench
          EnvPath = path
          EnvRecordingCount = recordingCount
          EnvRecordedEnvs = recordedEnvs }

/// Make a variable entry.
let makeVariableEntry value doc =
    ValueEntry (value, doc)
    
/// Make multiple variable entries.
let makeVariableEntries values =
    List.map (fun value -> makeVariableEntry value) values

/// Make an operator entry.
let makeOperatorEntry opName doc =
    let symbol = Symbol (makeSymbolRecord opName (ref CEUncached) None)
    ValueEntry (symbol, doc)

/// Make multiple operator entries.
let makeOperatorEntries opNames =
    List.map (fun opName -> makeOperatorEntry opName) opNames

/// Make a type value's name member.
let makeTypeNameMember typeName optPositions =
    let mem = makeMember NameLun (Keyword (makeKeywordRecord typeName optPositions))
    (NameLun, mem)

/// Make a type value.
let makeType typeName optPositions =
    let (typeNameName, typeNameMember) = makeTypeNameMember typeName optPositions
    let members = Dictionary.singleton (typeNameName, typeNameMember)
    Composite (makeCompositeRecord true typeName members UnitValue (Dictionary<Lun, Expr> ()) (HashSet<Lun> ()) optPositions)

/// Make a type entry.
let makeTypeEntry typeName doc optPositions =
    TypeEntry (typeName, makeType typeName optPositions, doc)

/// Overlay an environment with the executing one.
/// TODO: Consider using lazy lists or difference lists so that appending here is constant-time
/// (would Seqs also work?)
let overlayEnv (optEnv : Env option) env =
    match optEnv with
    | Some someEnv ->
#if AML_OPTIMIZED
        { someEnv with EnvInterventionBranchLists = someEnv.EnvInterventionBranchLists @ env.EnvInterventionBranchLists }
#else
        let envInfo = env.EnvDebugInfo
        let someEnvInfo = someEnv.EnvDebugInfo
        let newDebugInfo =
            makeDebugInfo
                envInfo.DIOptFirstReplLine
                (someEnvInfo.DIExprTrace @ envInfo.DIExprTrace)
                (someEnvInfo.DIStackTrace @ envInfo.DIStackTrace)
        let debugEnv = { someEnv with EnvDebugInfo = newDebugInfo }
        { debugEnv with EnvInterventionBranchLists = debugEnv.EnvInterventionBranchLists @ env.EnvInterventionBranchLists }
#endif
    | _ -> env

/// Push an environment's expr trace.
let pushExpr env exprTrace =
#if AML_OPTIMIZED
    env
#else
    let envInfo = env.EnvDebugInfo
    { env with EnvDebugInfo = { envInfo with DIExprTrace = exprTrace :: envInfo.DIExprTrace }}
#endif

/// Pop an environment's expr trace.
let popExpr env =
#if AML_OPTIMIZED
    env
#else
    let envInfo = env.EnvDebugInfo
    { env with EnvDebugInfo = { envInfo with DIExprTrace = envInfo.DIExprTrace.Tail }}
#endif

/// Push a frame on an environment's stack trace.
let pushStackFrame env frame =
#if AML_OPTIMIZED
    env
#else
    let envInfo = env.EnvDebugInfo
    { env with EnvDebugInfo = { envInfo with DIStackTrace = frame :: envInfo.DIStackTrace }}
#endif

/// Pop an frame off an environment's stack trace.
let popStackFrame env =
#if AML_OPTIMIZED
    env
#else
    let envInfo = env.EnvDebugInfo
    { env with EnvDebugInfo = { envInfo with DIStackTrace = envInfo.DIStackTrace.Tail }}
#endif

/// Push a list of intervention branches on an environment.
let pushInterventionBranchList env interventionBranches =
    { env with EnvInterventionBranchLists = interventionBranches :: env.EnvInterventionBranchLists }

/// Get the directory relative to the file.
let getDirectoryRelativeToFile env path =
    if Path.IsPathRooted path then path
    else
        let currentPath = env.EnvPath
        let directory = Path.GetDirectoryName path
        let combinedPath = Path.Combine (currentPath, directory)
        Path.GetFullPath combinedPath

/// Query that an expr is a unit value.
let isUnit expr = match expr with Series s when s.SerExprs.IsEmpty -> true | _ -> false

/// Query that an expr is a violation value.
let isViolation expr = match expr with Violation _ -> true | _ -> false

/// Query that an expr is a boolean value.
let isBoolean expr = match expr with Boolean _ -> true | _ -> false

/// Query that an expr is a true value.
let isTrue expr = match expr with Boolean b when b.BRValue -> true | _ -> false

/// Query that an expr is a false value.
let isFalse expr = match expr with Boolean b when not b.BRValue -> true | _ -> false

/// Query that an expr is a character value.
let isCharacter expr = match expr with Character _ -> true | _ -> false

/// Query that an expr is a string value.
let isString expr = match expr with String _ -> true | _ -> false

/// Query that an expr is an int value.
let isInt expr = match expr with Int _ -> true | _ -> false

/// Query that an expr is a long value.
let isLong expr = match expr with Long _ -> true | _ -> false

/// Query that an expr is a float value.
let isFloat expr = match expr with Float _ -> true | _ -> false

/// Query that an expr is a double value.
let isDouble expr = match expr with Double _ -> true | _ -> false

/// Query that an expr is a float value.
let isKeyword expr = match expr with Keyword _ -> true | _ -> false

/// Query that an expr is a symbol.
let isSymbol expr = match expr with Symbol _ -> true | _ -> false

/// Query that an expr is a package value.
let isPackage expr = match expr with Package _ -> true | _ -> false

/// Query that an expr is a lambda value.
let isLambda expr = match expr with Dispatch _ -> true | Lambda _ -> true | _ -> false

/// Query that an expr is a reference value.
let isRef expr = match expr with Ref _ -> true | _ -> false

/// Query that an expr is a list value.
let isList expr = match expr with List _ -> true | _ -> false

/// Query that an expr is an array value.
let isArray expr = match expr with Array _ -> true | _ -> false

/// Query that an expr is a composite value.
let isComposite expr = match expr with Composite _ -> true | _ -> false

/// Query that any of the exprs is a violation.
let anyViolations exprs = List.exists isViolation exprs
    
/// Get the first violation.
let firstViolation exprs = List.find isViolation exprs

/// Query that all args are simple enough to elide unification.
let areSimpleArgs args =
    List.forall (fun arg -> match arg.ArgType with Concrete | Abstracting -> true | Labeled | Variadic -> false) args

/// Query that args are of a concrete type.
let areConcreteArgs args =
    List.forall (fun arg -> arg.ArgType = Concrete) args

/// Query that all args of sigs are of a concrete type.
let doSigsHaveAllConcreteArgs sigs =
    List.forall (fun signature -> areConcreteArgs signature.SigArgs) sigs

/// Augment an environment with a declaration entry.
let tryAppendDeclarationEntry env name entry =
    let declarationFrame = env.EnvDeclarationFrame
    if env.EnvAllowRedeclaration then ignore (declarationFrame.ForceAdd (name, entry)); Some env
    elif declarationFrame.ContainsKey name then None
    else declarationFrame.Add (name, entry); Some env

/// Augment an environment with a procedural entry.
let appendProceduralEntry env appendType name entry =
    match appendType with
    | AppendToNewFrame size ->
        let newProceduralFrame = makeProceduralFrame size
        newProceduralFrame.[0] <- (name, entry)
        let newProceduralFrames = newProceduralFrame :: env.EnvProceduralFrames
        { env with EnvProceduralFrames = newProceduralFrames }
    | AppendToHeadFrame offset ->
        match env.EnvProceduralFrames with
        | [] -> failwith "Unexpected match failure in 'Aml.Primitives.tryAppendEntry'."
        | headFrame :: _ -> (headFrame.[offset] <- (name, entry)); env

/// Augment an environment with multiple declaration entries.
let tryAppendDeclarationEntries env entries =
    let declarationFrame = env.EnvDeclarationFrame
    if env.EnvAllowRedeclaration then 
        for (name, value) in entries do ignore (declarationFrame.ForceAdd (name, value))
        Some env
    else
        let existences = Seq.map (fun (key, _) -> declarationFrame.ContainsKey key) entries
        if Seq.exists id existences then None
        else
            for entry in entries do declarationFrame.Add entry
            Some env

/// Augment an environment with multiple procedural entries.
let appendProceduralEntries env appendType entries =
    match appendType with
    | AppendToNewFrame size ->
        let newProceduralFrame = makeProceduralFrame size
        let mutable index = 0
        for entry in entries do
            newProceduralFrame.[index] <- entry
            index <- index + 1
        let newProceduralFrames = newProceduralFrame :: env.EnvProceduralFrames
        { env with EnvProceduralFrames = newProceduralFrames }
    | AppendToHeadFrame start ->
        match env.EnvProceduralFrames with
        | [] -> failwith "Unexpected match failure in 'Aml.Primitives.tryAppendEntry'."
        | headFrame :: _ ->
            let mutable index = start
            for entry in entries do
                headFrame.[index] <- entry
                index <- index + 1
            env

/// Augment an environment with a declaration variable.
let tryAppendDeclarationVariable env name doc value =
    let entry = ValueEntry (value, doc)
    tryAppendDeclarationEntry env name entry

/// Augment an environment with a procedural variable.
let appendProceduralVariable env appendType name doc value =
    let entry = ValueEntry (value, doc)
    appendProceduralEntry env appendType name entry

/// Augment an environment with a type.
let tryAppendType env typeName typeValue doc =
    let typeEntry = TypeEntry (typeName, typeValue, doc)
    tryAppendDeclarationEntry env typeName typeEntry

let intInc (x : int) = x + 1
let intDec (x : int) = x - 1

let longInc (x : int64) = x + 1L
let longDec (x : int64) = x - 1L
let longPow (x : int64) (y : int64) = pown x (int y)

let floatFloor (x : single) = single (Math.Floor (float x))
let floatCeiling (x : single) = single (Math.Ceiling (float x))
let floatTruncate (x : single) = single (Math.Truncate (float x))
let floatRound (x : single) = single (Math.Round (float x))
let floatExp (x : single) = single (Math.Exp (float x))
let floatLog (x : single) = single (Math.Log (float x))
let floatSqrt (x : single) = single (Math.Sqrt (float x))
let floatSin (x : single) = single (Math.Sin (float x))
let floatCos (x : single) = single (Math.Cos (float x))
let floatTan (x : single) = single (Math.Tan (float x))
let floatAsin (x : single) = single (Math.Asin (float x))
let floatAcos (x : single) = single (Math.Acos (float x))
let floatAtan (x : single) = single (Math.Atan (float x))
let floatLogN (x : single) (y : single) = single (Math.Log (float x, float y))
let floatRoot (x : single) (y : single) = let y' = 1.0f / y in single (Math.Pow (float x, float y'))

let doubleLogN x y = Math.Log (x, y)
let doubleRoot x y = let y' = 1.0 / y in Math.Pow (x, y')

let ViolationType = makeType (Lun.make ViolationTypeStr) None
let BooleanType = makeType (Lun.make BooleanTypeStr) None
let CharacterType = makeType (Lun.make CharacterTypeStr) None
let StringType = makeType (Lun.make StringTypeStr) None
let IntType = makeType (Lun.make IntTypeStr) None
let LongType = makeType (Lun.make LongTypeStr) None
let FloatType = makeType (Lun.make FloatTypeStr) None
let DoubleType = makeType (Lun.make DoubleTypeStr) None
let KeywordType = makeType (Lun.make KeywordTypeStr) None
let PackageType = makeType (Lun.make PackageTypeStr) None
let SpecialValueType = makeType (Lun.make SpecialValueTypeStr) None
let LambdaType = makeType (Lun.make LambdaTypeStr) None
let UnitType = makeType (Lun.make UnitTypeStr) None
let RefType = makeType (Lun.make RefTypeStr) None
let ListType = makeType (Lun.make ListTypeStr) None
let ArrayType = makeType (Lun.make ArrayTypeStr) None
let CompositeType = makeType (Lun.make CompositeTypeStr) None

/// Make a special content value.
/// TODO: consider if the function name is strange.
let makeValueContent name value optPositions =
    Composite (makeCompositeRecord true name (Dictionary.singleton (name, makeMember name value)) CompositeType null null optPositions)