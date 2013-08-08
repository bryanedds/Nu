// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

module Dal
open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Threading
open Aml.Ast
open Aml.Constants
open Aml.Primitives
open Aml.Conversions
open Aml.EvalPrims
open Aml.Evaluator
open Aml.Writer
open Aml.Environment

(*
[usingLanguage "Aml.exe" "Dal+Dlm"]
[usingComponent "Aml.exe" "Dal+ConsoleKeyComponent"]
*)

// development flag
let [<Literal>] Development =
#if DAL_DEVELOPMENT
    true
#else
    false
#endif

#if FALSE
// names
let [<Literal>] LanguageNameStr = "dal"
let [<Literal>] ConsoleKeyComponentNameStr = "ckc"

// attributes
let [<Literal>] AttributeStr = "attribute"
let [<Literal>] GroupStr = "group"
let [<Literal>] CollectionStr = "collection"
let [<Literal>] RepeaterStr = "repeater"
let [<Literal>] ItemStr = "item"

// new
let [<Literal>] NewAttributeStr = "newAttribute"
let [<Literal>] NewGroupStr = "newGroup"
let [<Literal>] NewCollectionStr = "newCollection"
let [<Literal>] NewRepeaterStr = "newRepeater"

// as
let [<Literal>] AsAttributeStr = "asAttribute"
let [<Literal>] AsGroupStr = "asGroup"
let [<Literal>] AsCollectionStr = "asCollection"
let [<Literal>] AsRepeaterStr = "asRepeater"

// sources
let [<Literal>] SourceStr = "source"
let [<Literal>] TransientStr = "transient"
let [<Literal>] SpawnletStr = "spawnlet"

// passive attribute calculation expressions
let [<Literal>] InfoStr = "info"
let [<Literal>] SourceStr = "source"
let [<Literal>] PreviousStr = "previous"
let [<Literal>] InstantStr = "instant"
let [<Literal>] InstantRandomStr = "instantRandom"
let [<Literal>] FirstStr = "first"
let [<Literal>] FirstRandomStr = "firstRandom"

// active attribute calculation expressions
let [<Literal>] OriginStr = "origin"
let [<Literal>] ValueStr = "value"
let [<Literal>] YieldStr = "yield"
let [<Literal>] TallyStr = "tally"
let [<Literal>] AggregateStr = "aggregate"

// other syntax
let [<Literal>] UsingComponentStr = "usingComponent"
let [<Literal>] ExecutionStr = "execution"

// addressing
let [<Literal>] MyStr = "my"
let [<Literal>] ParentStr = "parent"
let [<Literal>] AnyStr = "any"
let [<Literal>] AliasStr = "alias"

// globals
let [<Literal>] GlobalAttributeStr = "globalAttribute"
let [<Literal>] GlobalStr = "global"
let [<Literal>] TickStr = "tick"

// misc
let [<Literal>] ChangeStr = "Change"
let [<Literal>] WhenStr = "when"
let [<Literal>] WhileStr = "while"
let [<Literal>] UpdateLimitStr = "updateLimit"

let ValueTypeLun = Lun.make ValueTypeStr
let VersionLun = Lun.make VersionStr
let ClassNameLun = Lun.make ClassNameStr
let UsingComponentLun = Lun.make UsingComponentStr
let ExecutionLun = Lun.make ExecutionStr
let ChangeLun = Lun.make ChangeStr
let WhenLun = Lun.make WhenStr
let WhileLun = Lun.make WhileStr
let AttributesLun = Lun.make AttributesStr
(*let ChildrenLun = Lun.make ChildrenStr*)
let ClassLun = Lun.make ClassStr
let SourceLun = Lun.make SourceStr
let PreviousLun = Lun.make PreviousStr
let InstantLun = Lun.make InstantStr
let InstantRandomLun = Lun.make InstantRandomStr
let FirstLun = Lun.make FirstStr
let FirstRandomLun = Lun.make FirstRandomStr
let OriginLun = Lun.make OriginStr
let ValueLun = Lun.make ValueStr
let TallyLun = Lun.make TallyStr
let YieldLun = Lun.make YieldStr
let AggregateLun = Lun.make AggregateStr
let InstantSumLun = Lun.make InstantSumStr
let FirstSumLun = Lun.make FirstSumStr
let AddressLun = Lun.make AddressStr
let GlobalClassLun = Lun.make GlobalClassStr
let GlobalLun = Lun.make GlobalStr
let TickLun = Lun.make TickStr
let IsSpawnletLun = Lun.make IsSpawnletStr
let UpdateLimitLun = Lun.make UpdateLimitStr
let AnonymousLun = Lun.make AnonymousStr
let LanguageNameLun = Lun.make LanguageNameStr
let LanguageGuid = Guid.NewGuid ()
let ConsoleKeyComponentNameLun = Lun.make ConsoleKeyComponentNameStr
let ZetaLargs = List.map (Lun.make >> makeArgFromName) ["value"; "oldValue"]
let ZetaLargCount = ZetaLargs.Length
let AggregateLargs = List.map (Lun.make >> makeArgFromName) ["value"; "oldValue"; "state"]
let AggregateLargCount = AggregateLargs.Length
let SumLargs = List.map (Lun.make >> makeArgFromName) ["value"; "oldValue"; "sum"]
let SumLargCount = SumLargs.Length

let getNextRandom =
    let random = Random ()
    fun () -> random.Next ()

let getNextRandomFloat () = 1.0f / single (getNextRandom ())

let getNextClassInstanceId = createGetNextId ()

let getNextZetaId = createGetNextId ()

type Connection =
    { ConnDescriptor : Lun }

type EventInfo =
    { ESName : Lun }

type Event =
    { EvtInfo : EventInfo }

type AttributeInfoRecord =
    { AirBody : Expr
      mutable AirOrigin : Expr
      AirTransformer : Expr
      AirReaction : Expr
      AirName : Lun }

type GroupInfoRecord =
    { GirAttributes : Attribute list
      GirName : Lun }

type CollectionInfoRecord =
    { CirUmmm : unit }

and Shape =
    | AttributeInfo of AttributeInfoRecord
    | GroupInfo of GroupInfoRecord
    | CollectionInfo of CollectionInfoRecord

type SourceAttributeRecord =
    { SvrInfo : AttributeInfoRecord
      mutable SvrValue : Expr }

type SourceGroupRecord =
    { SgrInfo : GroupInfoRecord
      mutable SgrValue : Expr }

type SourceCollectionRecord =
    { ScrInfo : CollectionInfoRecord
      ScrUmmm : unit }

type SourceAttribute =
    | SourceAttribute of SourceAttributeRecord
    | SourceGroup of SourceGroupRecord
    | SourceCollection of SourceCollectionRecord

type SourceRecord =
    { SourceId : int64
      SourceShape : Shape }

type TransientRecord =
    { TransientId : int64
      TransientShape : Shape }

type SpawnletRecord =
    { SpawnletId : int64
      SpawnletShape : Shape }

type Source =
    | Source of SourceRecord
    | Transient of TransientRecord
    | Spawnlet of SpawnletRecord

type AttributeRecord =
    { ARInfo : AttributeInfoRecord
      mutable ARValue : Expr }

type GroupRecord =
    { GRInfo : GroupInfoRecord
      mutable GRValue : Expr }

type CollectionRecord =
    { CRInfo : CollectionInfoRecord
      CRUmmm : unit }

type Attribute =
    | Attribute of AttributeRecord
    | Group of GroupRecord
    | Collection of CollectionRecord

type DecomposedExpr =
    { DecProceduralExpr : Expr
      DecZetaExprs : ZetaExpr list }

and SourceRecord =
    { SourceExpr : Expr
      SourceAttribute : Expr }

and PreviousRecord =
    { PreviousExpr : Expr }

and InstantRecord =
    { InstantExpr : Expr
      InstantConnection : Address }

and InstantRandomRecord =
    { InstantRandomExpr : Expr }

and FirstRecord =
    { FirstExpr : Expr
      FirstAddress : Address }

and FirstRandomRecord =
    { FirstRandomExpr : Expr }

and OriginRecord =
    { OriginExpr : Expr }

and ValueRecord =
    { ValueExpr : Expr
      ValueAddress : Address }

and YieldRecord =
    { YieldExpr : Expr
      YieldAddress : Address
      YieldBody : DecomposedExpr }

and TallyRecord =
    { TallyExpr : Expr
      TallyAddress : Address
      TallyWhen : DecomposedExpr option }

and AggregateRecord =
    { AggregateExpr : Expr
      AggregateAddress : Address
      AggregateBody : DecomposedExpr
      AggregateSeed : Expr
      AggregateWhen : DecomposedExpr option }

and ZetaExpr =
    //| Info of InfoRecord // {info collisionNormal}
    | Source of SourceRecord // {source health}
    | Previous of PreviousRecord // {previous}
    | Instant of InstantRecord // {instant player.power}
    | InstantRandom of InstantRandomRecord // {instantRandom type: :t/float}; defaults to int. NOTE: type not yet implemented!
    | First of FirstRecord // {first player.power}
    | FirstRandom of FirstRandomRecord // {firstRandom type: :t/float}; defaults to int. NOTE: type not yet implemented!
    | Origin of OriginRecord // {origin}; NOTE: updates not yet implemented!
    | Value of ValueRecord // {value player.shoot}
    | Yield of YieldRecord // {yield player.health (* value 2)}
    | Tally of TallyRecord // {tally my.damage when: #t}
    | Aggregate of AggregateRecord // {aggregate tick (+ state value) speed when: #t}
    
and [<CustomEquality; NoComparison>] ZetaInstance =
    { ZIZetaExpr : ZetaExpr
      ZIAttributeInstance : AttributeInstanceRecord
      ZIZetaId : int64
      ZIOptAddress : Address option
      mutable ZICachedValue : Expr }
    override this.Equals other =
        match other with
        | :? ZetaInstance as zetaInstance -> this.ZIZetaId = zetaInstance.ZIZetaId
        | _ -> false
    override this.GetHashCode () =
        int this.ZIZetaId

let rec exprToOptZetaExpr classTemplateAddress expr =
    match expr with
    | SpecialSeries s when not s.SSExprs.IsEmpty ->
        match s.SSExprs.Head with
        | Symbol symbol ->
            match symbol.SymName.LunStr with
            | SourceStr when List.hasExactly 2 s.SSExprs ->
                Some (Source { SourceExpr = s.SSExprs.[0]; SourceAttribute = s.SSExprs.[1] })
            | PreviousStr when List.hasExactly 1 s.SSExprs ->
                Some (Previous { PreviousExpr = expr })
            | InstantStr when List.hasExactly 2 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address -> Some (Instant { InstantExpr = expr; InstantAddress = address })
            | InstantRandomStr when List.hasExactly 1 s.SSExprs ->
                Some (InstantRandom { InstantRandomExpr = expr })
            | FirstStr when List.hasExactly 2 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address -> Some (First { FirstExpr = expr; FirstAddress = address })
            | FirstRandomStr when List.hasExactly 1 s.SSExprs ->
                Some (FirstRandom { FirstRandomExpr = expr })
            | OriginStr when List.hasExactly 1 s.SSExprs ->
                Some (Origin { OriginExpr = expr })
            | OriginStr when List.hasExactly 1 s.SSExprs ->
                Some (Origin { OriginExpr = expr })
            | ValueStr when List.hasExactly 2 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address -> Some (Value { ValueExpr = expr; ValueAddress = address })
            | TallyStr when List.hasExactly 2 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address -> Some (Tally { TallyExpr = expr; TallyAddress = address; TallyWhen = None })
            | TallyStr when List.hasExactly 3 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optWhen =
                        match s.SSExprs.[2] with
                        | Package package when package.PkgName = WhenLun -> Some package.PkgExpr
                        | _ -> None
                    match optWhen with
                    | None -> None
                    | Some whenExpr ->
                        let optWhenDecomposed = tryDecomposeExpr classTemplateAddress whenExpr : DecomposedExpr option
                        if optWhenDecomposed.IsSome
                        then Some (Tally { TallyExpr = expr; TallyAddress = address; TallyWhen = optWhenDecomposed })
                        else None
            | YieldStr when List.hasExactly 3 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optBody = tryDecomposeExpr classTemplateAddress s.SSExprs.[2]
                    match optBody with
                    | None -> None
                    | Some body -> Some (Yield { YieldExpr = expr; YieldAddress = address; YieldBody = body })
            | AggregateStr when List.hasExactly 4 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optBody = tryDecomposeExpr classTemplateAddress s.SSExprs.[2]
                    let seed = s.SSExprs.[3]
                    match optBody with
                    | None -> None
                    | Some body -> Some (Aggregate { AggregateExpr = expr; AggregateAddress = address; AggregateBody = body; AggregateSeed = seed; AggregateWhen = None})
            | AggregateStr when List.hasExactly 5 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optBody = tryDecomposeExpr classTemplateAddress s.SSExprs.[2]
                    let seed = s.SSExprs.[3]
                    match optBody with
                    | None -> None
                    | Some body ->
                        let optWhen =
                            match s.SSExprs.[4] with
                            | Package package when package.PkgName = WhenLun -> Some package.PkgExpr
                            | _ -> None
                        match optWhen with
                        | None -> None
                        | Some whenExpr ->
                            let optWhenDecomposed = tryDecomposeExpr classTemplateAddress whenExpr
                            if optWhenDecomposed.IsSome
                            then Some (Aggregate { AggregateExpr = expr; AggregateAddress = address; AggregateBody = body; AggregateSeed = seed; AggregateWhen = optWhenDecomposed })
                            else None
            | InstantSumStr when List.hasExactly 3 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optBody = tryDecomposeExpr classTemplateAddress s.SSExprs.[2]
                    match optBody with
                    | None -> None
                    | Some body -> Some (InstantSum { InstantSumExpr = expr; InstantSumAddress = address; InstantSumBody = body })
            | FirstSumStr when List.hasExactly 3 s.SSExprs ->
                let optAddress = tryGetAddressFromExpr classTemplateAddress s.SSExprs.[1]
                match optAddress with
                | None -> None
                | Some address ->
                    let optBody = tryDecomposeExpr classTemplateAddress s.SSExprs.[2]
                    match optBody with
                    | None -> None
                    | Some body -> Some (FirstSum { FirstSumExpr = expr; FirstSumAddress = address; FirstSumBody = body })
            | _ -> None
        | _ -> None
    | _ -> None

and tryDecomposeExpr classTemplateAddress expr =
    let exprTree = RduTree.fromParent getProceduralChildren expr
    let optZetaExprTree = RduTree.map (exprToOptZetaExpr classTemplateAddress) exprTree
    let optZetaExprs = RduTree.toValueList optZetaExprTree
    let zetaExprs = List.definitize optZetaExprs
    Some (makeDecomposedExpr expr zetaExprs)
            
let tryGetZetaInstances workBench address =
    match address.AddrOptGroupType with
    | None -> failwith "Unexpected match failure in 'Dol.tryGetAttributeInstance'."
    | Some groupType ->
        match groupType with
        | OneInGroup
        | AllInGroup _ ->
            let result = ref Unchecked.defaultof<ZetaInstance HashSet>
            if workBench.WBZetaInstanceRegistry.TryGetValue (address, result)
            then Some !result
            else None
        | AnyInGroup _ -> None

let updateZetaInstance env workBench zetaInstance =
    match zetaInstance.ZIZetaExpr with
    | Source _ -> ()
    | Previous _ -> ()
    | Instant _ -> ()
    | InstantRandom _ -> ()
    | First _ -> ()
    | FirstRandom _ -> ()
    | Origin _ -> ()
    | Value _ -> ()
    | Yield _ -> ()
    | Tally tally ->
        let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
        match optGetResult with
        | None -> ()
        | Some getResult ->
            match getResult with
            | GAISingle attributeInstance
            | GAIAny attributeInstance ->
                let optWhenResult =
                    withUpdatingAttributeInstance
                        (fun () ->
                            Option.map
                                (fun whenExpr -> applyZetaLambda env whenExpr.DecProceduralExpr attributeInstance)
                                tally.TallyWhen)
                        workBench
                        zetaInstance.ZIAttributeInstance
                let shouldTally =
                    match optWhenResult with
                    | None -> true
                    | Some evalResult ->
                        match evalResult.Value with
                        | Boolean boolean -> boolean.BRValue
                        | _ -> false
                if shouldTally then
                    match zetaInstance.ZICachedValue with
                    | Int intRecord -> zetaInstance.ZICachedValue <- Int (makeIntRecord (intRecord.IRValue + 1) None)
                    | Series series when series.SerExprs.IsEmpty -> zetaInstance.ZICachedValue <- Int (makeIntRecord 1 None)
                    | _ -> ()
            | GAIAll attributeInstances -> ()
    | Aggregate aggregate ->
        let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
        match optGetResult with
        | None -> ()
        | Some getResult ->
            match getResult with
            | GAISingle attributeInstance
            | GAIAny attributeInstance ->
                let optWhenResult =
                    withUpdatingAttributeInstance
                        (fun () ->
                            Option.map
                                (fun whenExpr -> applyZetaLambda env whenExpr.DecProceduralExpr attributeInstance)
                                aggregate.AggregateWhen)
                        workBench
                        zetaInstance.ZIAttributeInstance
                let shouldAggregate =
                    match optWhenResult with
                    | None -> true
                    | Some evalResult ->
                        match evalResult.Value with
                        | Boolean boolean -> boolean.BRValue
                        | _ -> false
                if shouldAggregate then
                    let lambdaResult = applyAggregateLambda env aggregate.AggregateBody.DecProceduralExpr zetaInstance attributeInstance
                    zetaInstance.ZICachedValue <- lambdaResult.Value
            | GAIAll attributeInstances -> ()
    | InstantSum instantSum -> ()
    | FirstSum firstSum -> ()

let instantiateZetaExpr env workBench attributeInstance zetaExpr =
    let expr = getExpr zetaExpr
    let optSpecialId = tryGetSpecialId expr
    match optSpecialId with
    | None -> failwith "Unexpected match failure in 'Dol.instantiateZetaExpr'."
    | Some specialId ->
        let optContextAddress = tryGetAddress zetaExpr
        let initialCachedValue = getInitialCachedValue env zetaExpr
        let zetaInstance = {
            ZIZetaExpr = zetaExpr
            ZIAttributeInstance = attributeInstance
            ZIZetaId = getNextZetaId ()
            ZIOptAddress = optContextAddress
            ZICachedValue = initialCachedValue }
        attributeInstance.AIZetaInstances.Add (specialId, zetaInstance)
#endif