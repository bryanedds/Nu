// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.
//
// TODO: increase this project's warning level to 5 and fix the corresponding issues.

module Aml.Ast
open System
open System.Collections.Generic
open FParsec // NOTE: I do not like this dependency here...
#nowarn "342" // suppress warning related to Member in Dictionary hack.

/// Creates Special Ids.
let getNextSpecialId = createGetNextId ()

/// The type of a string.
type StringType =
    | LiteralString
    | VerbatimString

/// The type of an argument.
type ArgType =
    | Concrete
    | Abstracting
    | Labeled
    | Variadic

/// The type of a prefixed expression.
type PrefixType =
    | DollarPrefix
    | PercentPrefix
    | AmpersandPrefix
    | AtPrefix

/// The type of a special series.
type SpecialSeriesType =
    | DeclarativeExpr
    | MetaExpr

/// The type of a selector.
type SelectorType =
    | FunctionalSelector
    | DotSelector
    | DoubleColonSelector

/// A string value.
type StringValue =
    { SVValue : string
      SVType : StringType }

/// A pair of parser positions describing the location of an expression.
type ParserPositions =
    { ParStart : Position
      ParStop : Position }

/// A function or lambda argument.
type Arg =
    { ArgName : Lun
      ArgType : ArgType
      ArgExpr : Expr }

/// A composite member.
/// Hacked with IComparable to make Dictionary equality work...
and Member =
    { MemName : Lun
      MemExpr : Expr }
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Member as mem -> if this = mem then 0 else -1
            | _ -> -1

/// A function or lambda signature.
and Signature =
    { SigName : Lun
      SigArgs : Arg list
      SigDoc : StringValue option }

/// A type constraint.
and Constraint =
    { ConstrName : Lun
      ConstrTypeName : Lun
      ConstrProtocolName : Lun
      ConstrArgs : Lun list }

/// An attempt branch.
and [<NoEquality; NoComparison>] AttemptBranch = {
    ABCategory : Lun
    ABBody : Expr }

/// An intervention branch.
and [<NoEquality; NoComparison>] InterveneBranch = {
    IBEnv : Env option
    IBCategory : Lun
    IBBody : Expr
    IBHide : bool }

/// A test branch.
and [<NoEquality; NoComparison>] TestBranch = {
    TBTest : Expr
    TBBody : Expr }

/// A let binding.
and [<NoEquality; NoComparison>] LetBinding =
    | LetVariable of Lun * Expr
    | LetFunction of Lun * Arg list * int * Expr * Constraint list option * Expr * Expr * bool
    
/// A dictionary of composite members by name.
and MemberDict = Dictionary<Lun, Member>

/// Record for a Violation expression.
and [<NoEquality; NoComparison>] ViolationRecord =
    { VioEvaluated : bool
      VioCategory : Lun
      VioMessage : StringValue
      VioData : Expr
      VioOptPositions : ParserPositions option }

/// Record for a Boolean expression.
and [<NoEquality; NoComparison>] BooleanRecord =
    { BRValue : bool
      BROptPositions : ParserPositions option }

/// Record for a Character expression.
and [<NoEquality; NoComparison>] CharacterRecord =
    { CRValue : char
      CROptPositions : ParserPositions option }

/// Record for a String expression.
and [<NoEquality; NoComparison>] StringRecord =
    { SRValue : StringValue
      SROptPositions : ParserPositions option }

/// Record for a Int expression.
and [<NoEquality; NoComparison>] IntRecord =
    { IRValue : int
      IROptPositions : ParserPositions option }

/// Record for a Long expression.
and [<NoEquality; NoComparison>] LongRecord =
    { GRValue : int64
      GROptPositions : ParserPositions option }

/// Record for a Float expression.
and [<NoEquality; NoComparison>] FloatRecord =
    { FRValue : single
      FROptPositions : ParserPositions option }

/// Record for a Double expression.
and [<NoEquality; NoComparison>] DoubleRecord =
    { DRValue : double
      DROptPositions : ParserPositions option }

/// Record for a Keyword expression.
and [<NoEquality; NoComparison>] KeywordRecord =
    { KRValue : Lun
      KROptPositions : ParserPositions option }

/// Record for a Symbol expression.
and [<NoEquality; NoComparison>] SymbolRecord =
    { SymName : Lun
      SymCachedEntry : CachedEntry ref
      SymOptPositions : ParserPositions option }

/// Record for a Package expression.
and [<NoEquality; NoComparison>] PackageRecord =
    { PkgName : Lun
      PkgExpr : Expr
      PkgOptPositions : ParserPositions option }

/// Record for a Prefixed expression.
and [<NoEquality; NoComparison>] PrefixedRecord =
    { PxdType : PrefixType
      PxdExpr : Expr
      PxdSpecialId : int64
      PxdOptPositions : ParserPositions option }

/// Record for a Dispatch expression.
and [<NoEquality; NoComparison>] DispatchRecord =
    { DispName : Lun
      DispContingentArg : int // NOTE: only one arg is currently contingent
      DispOptPositions : ParserPositions option }

/// Record for a SpecialValue expression.
and [<NoEquality; NoComparison>] SpecialValueRecord =
    { SVEvaluated : bool
      SVLanguageName : Lun
      SVExpr : Expr
      SVOptPositions : ParserPositions option }

/// Record for a SpecialObject expression.
and [<NoEquality; NoComparison>] SpecialObjectRecord =
    { SOEvaluated : bool
      SOLanguageGuid : Guid
      SOContent : ISpecialContent
      SOOptPositions : ParserPositions option }

/// Record for a Series expression.
and [<NoEquality; NoComparison>] SeriesRecord =
    { SerExprs : Expr list
      SerExprCount : int
      SerOptPositions : ParserPositions option }

/// Record for a Lambda expression.
and [<NoEquality; NoComparison>] LambdaRecord =
    { LamEvaluated : bool
      LamName : Lun
      LamArgs : Arg list
      LamArgCount : int
      LamBody : Expr
      LamCpre : Expr list -> bool
      LamPre : Expr
      LamPost : Expr
      LamEmptyUnification : bool
      LamOptPositions : ParserPositions option
      LamEnv : Env option }

/// Record for an Attempt expression.
and [<NoEquality; NoComparison>] AttemptRecord =
    { AttemptBody : Expr
      AttemptBranches : AttemptBranch list
      AttemptOptPositions : ParserPositions option }

/// Record for a Let expression.
and [<NoEquality; NoComparison>] LetRecord =
    { LetBindings : LetBinding list
      LetBindingCount : int
      LetBody : Expr
      LetOptPositions : ParserPositions option }

/// Record for an Extend expression.
and [<NoEquality; NoComparison>] ExtendRecord =
    { ExtTarget : Expr
      ExtMembers : MemberDict
      ExtOptPositions : ParserPositions option }

/// Record for a Case expression.
and [<NoEquality; NoComparison>] CaseRecord =
    { CaseTarget : Expr
      CaseBranches : TestBranch list
      CaseOptPositions : ParserPositions option }

/// Record for a Condition expression.
and [<NoEquality; NoComparison>] ConditionRecord =
    { CondBranches : TestBranch list
      CondOptPositions : ParserPositions option }

/// Record for an Intervene expression.
and [<NoEquality; NoComparison>] InterveneRecord =
    { ItvBody : Expr
      ItvBranches : InterveneBranch list
      ItvOptPositions : ParserPositions option }

/// Record for a Ref expression.
and [<NoEquality; NoComparison>] RefRecord =
    { mutable RefEvaluated : bool
      mutable RefExpr : Expr
      RefOptPositions : ParserPositions option }

/// Record for a Get expression.
and [<NoEquality; NoComparison>] GetRecord =
    { GetTarget : Expr
      GetOptPositions : ParserPositions option }

/// Record for a Set! expression.
and [<NoEquality; NoComparison>] SetRecord =
    { SetTarget : Expr
      SetInjection : Expr
      SetOptPositions : ParserPositions option }

/// Record for a List expression.
and [<NoEquality; NoComparison>] ListRecord =
    { ListEvaluated : bool
      ListElements : Expr list
      ListOptPositions : ParserPositions option }

/// Record for an Array expression.
and [<NoEquality; NoComparison>] ArrayRecord =
    { ArrEvaluated : bool
      ArrElements : Expr array
      ArrOptPositions : ParserPositions option }

/// Record for a Composite expression.
/// TODO: combine CompSigImpls and CompProtocols as CompTypeData or something.
and [<NoEquality; NoComparison>] CompositeRecord =
    { CompEvaluated : bool
      CompName : Lun
      CompMembers : MemberDict
      CompType : Expr
      CompSigImpls : Dictionary<Lun, Expr>
      CompProtocols : HashSet<Lun>
      CompOptPositions : ParserPositions option }

/// Record for a Selector expression.
and [<NoEquality; NoComparison>] SelectorRecord =
    { SelKey : Expr
      SelTarget : Expr
      SelType : SelectorType
      SelOptPositions : ParserPositions option }

/// Record for a Variable expression.
and [<NoEquality; NoComparison>] VariableRecord =
    { VarName : Lun
      VarBody : Expr
      VarDoc : StringValue option
      VarOptPositions : ParserPositions option }

/// Record for a Function expression.
and [<NoEquality; NoComparison>] FunctionRecord =
    { FnName : Lun
      FnArgs : Arg list
      FnArgCount : int
      FnBody : Expr
      FnOptConstraints : Constraint list option
      FnDoc : StringValue option
      FnPre : Expr
      FnPost : Expr
      FnEmptyUnification : bool
      FnOptPositions : ParserPositions option }

/// Record for a Structure expression.
and [<NoEquality; NoComparison>] StructureRecord =
    { StructName : Lun
      StructMemberNames : Lun list
      StructOptConstraints : Constraint list option
      StructDoc : StringValue option
      StructReq : Expr
      StructOptPositions : ParserPositions option }

/// Record for a Protocol expression.
and [<NoEquality; NoComparison>] ProtocolRecord =
    { ProtoName : Lun
      ProtoArg : Lun
      ProtoOptConstraints : Constraint list option
      ProtoDoc : StringValue option
      ProtoSignatures : Signature list
      ProtoOptPositions : ParserPositions option }

/// Record for an Instance expression.
and [<NoEquality; NoComparison>] InstanceRecord =
    { InstProtocolName : Lun
      InstArgs : Lun list
      InstConstraints : Constraint list
      InstFunctions : Expr list
      InstOptPositions : ParserPositions option }

/// Record for an Affirmation expression.
and [<NoEquality; NoComparison>] AffirmationRecord =
    { AffName : Lun
      AffDoc : StringValue option
      AffBody : Expr
      AffOptPositions : ParserPositions option }

/// Record for a UsingFile expression.
and [<NoEquality; NoComparison>] UsingFileRecord =
    { UFPath : string
      UFReload : bool
      UFOptPositions : ParserPositions option }

/// Record for a UsingLanguage expression.
and [<NoEquality; NoComparison>] UsingLanguageRecord =
    { ULPath : string
      ULType : string
      ULOptPositions : ParserPositions option }

/// Record for a SpecialSeries expression.
and [<NoEquality; NoComparison>] SpecialSeriesRecord =
    { SSType : SpecialSeriesType
      SSExprs : Expr list
      SSExprCount : int
      SSSpecialId : int64
      SSOptPositions : ParserPositions option }

/// A value looked up from the environment entries.
and [<NoEquality; NoComparison>] EnvEntry =
    | ValueEntry of Expr * StringValue option
    | DynamicEntry of int * StringValue option
    | TypeEntry of Lun * Expr * StringValue option
    | ProtocolEntry of Lun * Constraint list option * StringValue option * Signature list

/// Describes the caching of an environment entry.
and [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue); NoEquality; NoComparison>] CachedEntry =
    | CEUncached
    | CEDeclaration of EnvEntry
    | CEProcedural of int * int

/// An environment frame for declaration entries.
and DeclarationFrame = Dictionary<Lun, EnvEntry>

/// An environment frame for procedural entries.
and ProceduralFrame = (Lun * EnvEntry) array

/// The environment's debug information.
and [<NoEquality; NoComparison>] DebugInfo =
    { DIOptFirstReplLine : string option
      DIExprTrace : Expr list
      DIStackTrace : Expr list }

/// The environment.
and [<NoEquality; NoComparison>] Env =
    { EnvDeclarationFrame : DeclarationFrame
      EnvProceduralFrames : ProceduralFrame list
      EnvCachedDeclarationEntries : CachedEntry ref List
      EnvAllowRedeclaration : bool
      EnvDebugInfo : DebugInfo
      EnvUsingFiles : string Set
      EnvInterventionBranchLists : InterveneBranch list list
      EnvOptLanguageModule : ILanguageModule option
      EnvOptWorkBench : obj option
      EnvPath : string
      EnvRecordingCount : int
      EnvRecordedEnvs : Env option list }

/// Interface for special object content.
and ISpecialContent =
    abstract ToValue : unit -> Expr

/// Interface for a language module.
and ILanguageModule =
    /// Try to initialize the language module.
    abstract TryInitialize : Env -> Env option
    /// Convert a special value to a special object.
    abstract SpecialValueToSpecialObject : Env -> Expr -> Expr
    /// Query that a symbol name represents a special built-in operator.
    abstract IsSpecialBuiltin : Env -> Lun -> bool
    /// Get the type of a special object.
    abstract GetSpecialType : Env -> Expr -> Expr
    /// Apply a special built-in operator.
    abstract ApplySpecialBuiltin : Env -> Lun -> Expr list -> int -> EvalResult
    /// Apply a selector to a special object.
    abstract ApplySpecialSelector : Env -> Expr -> Expr -> EvalResult
    /// Evaluate a prefixed expression.
    abstract EvalPrefixed : Env -> Expr -> EvalResult
    /// Evaluate a special object.
    abstract EvalSpecialObject : Env -> Expr -> EvalResult
    /// Evaluate a special series expression.
    abstract EvalSpecialSeries : Env -> Expr -> EvalResult
    /// The name of the language.
    abstract Name : Lun
    /// The unique identifier for the language.
    abstract Guid : Guid

/// The expression structure.
and [<CustomEquality; NoComparison>] Expr =
    | Violation of ViolationRecord
    | Boolean of BooleanRecord
    | Character of CharacterRecord
    | String of StringRecord
    | Int of IntRecord
    | Long of LongRecord
    | Float of FloatRecord
    | Double of DoubleRecord
    | Keyword of KeywordRecord
    | Symbol of SymbolRecord
    | Package of PackageRecord
    | Prefixed of PrefixedRecord
    | Dispatch of DispatchRecord
    | SpecialValue of SpecialValueRecord
    | SpecialObject of SpecialObjectRecord
    | Series of SeriesRecord
    | Lambda of LambdaRecord
    | Attempt of AttemptRecord
    | Let of LetRecord
    | Extend of ExtendRecord
    | Case of CaseRecord
    | Condition of ConditionRecord
    | Intervene of InterveneRecord
    | Ref of RefRecord
    | Get of GetRecord
    | Set of SetRecord
    | List of ListRecord
    | Array of ArrayRecord
    | Composite of CompositeRecord
    | Selector of SelectorRecord
    | Variable of VariableRecord
    | Function of FunctionRecord
    | Structure of StructureRecord
    | Protocol of ProtocolRecord
    | Instance of InstanceRecord
    | Affirmation of AffirmationRecord
    | UsingFile of UsingFileRecord
    | UsingLanguage of UsingLanguageRecord
    | SpecialSeries of SpecialSeriesRecord
    override this.Equals other =
        match other with
        | :? Expr as expr ->
            match (this, expr) with // NOTE: this tupling should be optimized away by the compiler
            | (Boolean xb, Boolean yb) -> xb.BRValue = yb.BRValue
            | (Character xc, Character yc) -> xc.CRValue = yc.CRValue
            | (String xs, String ys) -> xs.SRValue = ys.SRValue
            | (Int xi, Int yi) -> xi.IRValue = yi.IRValue
            | (Long xl, Long yl) -> xl.GRValue = yl.GRValue
            | (Float xf, Float yf) -> xf.FRValue = yf.FRValue
            | (Double xd, Double yd) -> xd.DRValue = yd.DRValue
            | (Keyword xs, Keyword ys) -> xs.KRValue = ys.KRValue
            | (Symbol xs, Symbol ys) -> xs.SymName = ys.SymName
            | (Package xp, Package yp) -> xp.PkgName = yp.PkgName && xp.PkgExpr = yp.PkgExpr
            | (Prefixed xp, Prefixed yp) -> xp.PxdType = yp.PxdType && xp.PxdExpr = yp.PxdExpr
            | (SpecialValue xs, SpecialValue ys) -> xs.SVExpr = ys.SVExpr
            | (SpecialObject xs, SpecialObject ys) -> obj.Equals (xs.SOContent, ys.SOContent)
            | (Series xl, Series yl) when xl.SerExprs.IsEmpty && yl.SerExprs.IsEmpty -> true
            | (Ref xr, Ref yr) -> xr.RefExpr = yr.RefExpr
            | (List xl, List yl) -> xl.ListElements = yl.ListElements
            | (Array xa, Array ya) -> xa.ArrElements = ya.ArrElements
            | (Composite xc, Composite yc) -> xc.CompType = yc.CompType && xc.CompMembers.ValueEquals(yc.CompMembers)
            | (Variable xv, Variable yv) -> xv.VarBody = yv.VarBody
            | (SpecialSeries xs, SpecialSeries ys) -> xs.SSType = ys.SSType && xs.SSExprs = ys.SSExprs
            | _ -> false
        | _ -> false
    override this.GetHashCode () = 0

/// The resulting context of evaluating an expression.
and [<NoEquality; NoComparison>] EvalResult =
    { Env : Env
      Value : Expr }