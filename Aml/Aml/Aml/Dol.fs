// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

module Dol
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

#if NOT
(*
[usingLanguage "Aml.exe" "Dol+Dlm"]
[usingComponent "Aml.exe" "Dol+ConsoleKeyComponent"]
[class c v: 1.1 [attributes [name "Jim"] [ticks 5]]]
[object o [class c v: 1.1] [attributes [ticks {yield global.tick (+ {value global.tick} {value global.tick})}]]]
[object o2 [class c v: 1.1] [attributes [ticks (+ {tally global.tick} {origin}) updateLimit: (+ 1 1)]]]
[object o3 [class c v: 1.1] [attributes [ticks {tally global.tick when: {yield global.tick (isZero (rem value 2))}}]]]
[object o4 [class c v: 1.1] [attributes [ticks {aggregate o.ticks (+ state value) (+ 5 5)}]]]
[object o5 [class c v: 1.1] [attributes [ticks {value global.tick}]]]
[object o6 [class c v: 1.1] [attributes [ticks {yield o.ticks ( * value 2)}]]]
[object o7 [class c v: 1.1] [while {yield global.tick (isZero (rem value 2))}] [attributes [name "Bob"]]]
[class c2 v: 1.1 [attributes [weight 105] [summary ()]]]
;[object o8 [class c2 v: 1.1] [attributes [summary {instantSum all.c.ticks (+ sum value)}]]]
;[object o9 [class c2 v: 1.1] [attributes [summary {aggregate any.c.ticks (+ state value) 0)}]]]
[execution]
;[object o10 [class c v: 1.1] [children [object l [class c v: 1.1]]]]
;[object o11 [class c v: 1.1] [while #t] [children [object l [class c v: 1.1]]] [attributes [name "Rob"]]]
;[execution]
*)

let [<Literal>] Development =
#if DOL_DEVELOPMENT
    true
#else
    false
#endif
let [<Literal>] ValueTypeStr = "valueType"
let [<Literal>] VersionStr = "v"
let [<Literal>] ClassNameStr = "className"
let [<Literal>] UsingComponentStr = "usingComponent"
let [<Literal>] ExecutionStr = "execution"
let [<Literal>] ChangeStr = "Change"
let [<Literal>] WhenStr = "when"
let [<Literal>] WhileStr = "while"
let [<Literal>] AttributesStr = "attributes"
(*let [<Literal>] ChildrenStr = "children"*)
let [<Literal>] LanguageNameStr = "dol"
let [<Literal>] ConsoleKeyComponentNameStr = "ckc"
let [<Literal>] EventStr = "event"
let [<Literal>] EventTemplateStr = "eventTemplate"
let [<Literal>] EventInstanceStr = "eventInstance"
let [<Literal>] AttributeStr = "attribute"
let [<Literal>] AttributeTemplateStr = "attributeTemplate"
let [<Literal>] AttributeInstanceStr = "attributeInstance"
let [<Literal>] ClassStr = "class"
let [<Literal>] ClassTemplateStr = "object"
let [<Literal>] ClassInstanceStr = "spirit"
let [<Literal>] SourceStr = "source"
let [<Literal>] PreviousStr = "previous"
let [<Literal>] InstantStr = "instant"
let [<Literal>] InstantRandomStr = "instantRandom"
let [<Literal>] FirstStr = "first"
let [<Literal>] FirstRandomStr = "firstRandom"
let [<Literal>] OriginStr = "origin"
let [<Literal>] ValueStr = "value"
let [<Literal>] YieldStr = "yield"
let [<Literal>] TallyStr = "tally"
let [<Literal>] AggregateStr = "aggregate"
let [<Literal>] InstantSumStr = "instantSum"
let [<Literal>] FirstSumStr = "firstSum"
let [<Literal>] AddressStr = "address"
let [<Literal>] GlobalClassStr = "globalClass"
let [<Literal>] GlobalStr = "global"
let [<Literal>] TickStr = "tick"
let [<Literal>] IsSpawnletStr = "isSpawnlet"
let [<Literal>] UpdateLimitStr = "updateLimit"
let [<Literal>] AnonymousStr = "anonymous"
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

type Version = single

type AddressLink =
    | ALMy
    | ALAny
    | ALAll
    | ALName of Lun

type GroupType =
    | OneInGroup
    | AnyInGroup
    | AllInGroup of Address * Lun

and [<CustomEquality; NoComparison>] Address =
    { AddrVersion : Version option
      AddrName : Lun
      AddrOptGroupType : GroupType option
      AddrHash : int }
    override this.ToString () =
        match this.AddrVersion with
        | None -> this.AddrName.LunStr
        | Some version -> (version.ToString "F1") + DotStr + this.AddrName.LunStr
    override this.Equals other =
        match other with
        | :? Address as otherSar -> this.AddrVersion = otherSar.AddrVersion && this.AddrName = otherSar.AddrName
        | _ -> false
    override this.GetHashCode () =
        this.AddrHash

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
      InstantAddress : Address }

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

and InstantSumRecord =
    { InstantSumExpr : Expr
      InstantSumAddress : Address
      InstantSumBody : DecomposedExpr }
      
and FirstSumRecord =
    { FirstSumExpr : Expr
      FirstSumAddress : Address
      FirstSumBody : DecomposedExpr }

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
    | InstantSum of InstantSumRecord // {instantSum all.enemy.health (+ sum value)}
    | FirstSum of FirstSumRecord // {firstSum all.enemy.magic (+ sum value)}

type GetAttributeInstanceResult =
    | GAISingle of AttributeInstanceRecord
    | GAIAny of AttributeInstanceRecord
    | GAIAll of AttributeInstanceRecord list
    
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

and WhileRecord =
    { WhileOptPredicate : AttributeTemplateRecord option }

and EventRecord =
    { EventName : Lun }

and EventInstanceRecord =
    { EIEvent : EventRecord
      EIAddress : Address }

and AttributeRecord =
    { AttrName : Lun
      AttrEvent : EventRecord
      AttrDefault : Expr }

and AttributeTemplateRecord =
    { ATAttribute : AttributeRecord
      ATUpdateLimit : int
      ATDecomposedExpr : DecomposedExpr
      ATIsWhile : bool
      ATIsSpawnlet : bool
      mutable ATOptClassAttributeInstance : AttributeInstanceRecord option
      mutable ATOrigin : Expr } // can be changed in an editor or from the repl

and AttributeInstanceRecord =
    { AIAttributeTemplate : AttributeTemplateRecord
      AIClassTemplate : ClassTemplateRecord
      AIEventInstance : EventInstanceRecord
      AIZetaInstances : Dictionary<int64, ZetaInstance> // OPTIMIZATION: dictionary for look-up speed
      AIAddress : Address
      mutable AIOptClassInstance : ClassInstanceRecord option
      mutable AIValue : Expr
      mutable AIOldValue : Expr
      mutable AIUpdateCount : int }
    override this.ToString () =
        this.DebugView
    member this.DebugView
        with get () =
            "{Name = \"" + this.AIAttributeTemplate.ATAttribute.AttrName.LunStr + "\"; " +
            "Value = \"" + writeExpr this.AIValue + "\"; " +
            "OldValue = \"" + writeExpr this.AIOldValue + "\"; " +
            "Address = \"" + this.AIAddress.ToString () + "\"}"

and ClassRecord =
    { ClassName : Lun
      ClassVersion : Version
      ClassAttributes : AttributeRecord LunTrie }

and ClassTemplateRecord =
    { CTName : Lun
      CTClass : ClassRecord
      CTWhile : WhileRecord
      (*CTChildren : ClassTemplateRecord LunTrie*)
      CTAttributeTemplates : AttributeTemplateRecord LunTrie }
    override this.ToString () =
        this.DebugView
    member this.DebugView
        with get () =
            "{Name = \"" + this.CTName.LunStr + "\"; " +
            "Version = \"" + this.CTClass.ClassVersion.ToString "F1" + "\"; " +
            "Class = \"" + this.CTClass.ClassName.LunStr + "\"}"

and ClassInstanceRecord =
    { CIId : int64
      CIAddress : Address
      CIClassTemplate : ClassTemplateRecord
      CIAttributeInstances : AttributeInstanceRecord LunTrie }

and ClassInstanceGroupRecord =
    { CIGClassInstances : Dictionary<Address, ClassInstanceRecord> }

and ClassInstanceGroupGroupRecord =
    { CIGGClassInstanceGroups : Dictionary<Address, ClassInstanceGroupRecord> }

let makeClassInstanceGroupRecord classInstances =
    { CIGClassInstances = classInstances }

let makeClassInstanceGroupGroupRecord classInstanceGroups =
    { CIGGClassInstanceGroups = classInstanceGroups }

type SpecialContent =
    | Event of EventRecord
    | EventInstance of EventInstanceRecord
    | Attribute of AttributeRecord
    | AttributeTemplate of AttributeTemplateRecord
    | AttributeInstance of AttributeInstanceRecord
    | Class of ClassRecord
    | ClassTemplate of ClassTemplateRecord
    | ClassInstance of ClassInstanceRecord
    | ClassInstanceGroup of ClassInstanceGroupRecord
    | ClassInstanceGroupGroup of ClassInstanceGroupGroupRecord
    interface ISpecialContent with
        member this.ToValue () =
            match this with
            | Event e -> UnitValue // TODO: implement all these
            | EventInstance e -> UnitValue
            | Attribute a -> UnitValue
            | AttributeTemplate a -> UnitValue
            | AttributeInstance a -> UnitValue
            | Class c -> UnitValue
            | ClassTemplate c -> UnitValue
            | ClassInstance c -> UnitValue
            | ClassInstanceGroup c -> UnitValue
            | ClassInstanceGroupGroup c -> UnitValue

type WorkBench =
    { WBRunning : bool ref
      WBComponents : Dictionary<Lun, IComponent>
      WBClasses : Dictionary<Address, ClassRecord>
      WBClassTemplates : Dictionary<Address, ClassTemplateRecord>
      WBClassInstances : Dictionary<Address, ClassInstanceRecord>
      WBClassInstanceGroups : ClassInstanceGroupGroupRecord
      WBUpdatingAttributeInstance : AttributeInstanceRecord option ref
      WBUninstantiatedClassTemplates : ClassTemplateRecord List
      WBAttributeInstanceRegistry : Dictionary<Address, AttributeInstanceRecord>
      WBZetaInstanceRegistry : Dictionary<Address, ZetaInstance HashSet>
      WBUpdatedAttributeInstances : AttributeInstanceRecord List }

and IComponent =
    abstract TryInitialize : Env -> WorkBench -> Env option
    abstract Advance : Env -> WorkBench -> Unit
    abstract Name : Lun

let getClassInstanceName id classTemplate =
    let isSpawnlet = match classTemplate.CTWhile.WhileOptPredicate with None -> false | Some predicate -> predicate.ATIsSpawnlet
    let classTemplateName = classTemplate.CTName
    if isSpawnlet || classTemplateName = AnonymousLun
    then Lun.make (classTemplateName.LunStr + DotStr + id.ToString ())
    else classTemplateName
    
let getWorkBench env =
    match env.EnvOptWorkBench with
    | None -> failwith "Unexpected match failure in 'Dol.getWorkBench'."
    | Some wb ->
        match wb with
        | :? WorkBench as dwb -> dwb
        | _ -> failwith "Unexpected match failure in 'Dol.getWorkBench'."

let getExpr zetaExpr =
    match zetaExpr with
    | Source source -> source.SourceExpr
    | Previous previous -> previous.PreviousExpr
    | Instant instant -> instant.InstantExpr
    | InstantRandom instantRandom -> instantRandom.InstantRandomExpr
    | First first -> first.FirstExpr
    | FirstRandom firstRandom -> firstRandom.FirstRandomExpr
    | Origin origin -> origin.OriginExpr
    | Value value -> value.ValueExpr
    | Yield yieldRecord -> yieldRecord.YieldExpr
    | Tally tally -> tally.TallyExpr
    | Aggregate aggregate -> aggregate.AggregateExpr
    | InstantSum instantSum -> instantSum.InstantSumExpr
    | FirstSum firstSum -> firstSum.FirstSumExpr

let tryGetAddress zetaExpr =
    match zetaExpr with
    | Source source -> None
    | Previous previous -> None
    | Instant instant -> None
    | InstantRandom instantRandom -> None
    | First first -> None
    | FirstRandom firstRandom -> None
    | Origin _ -> None
    | Value value -> Some value.ValueAddress
    | Yield yieldRecord -> Some yieldRecord.YieldAddress
    | Tally tally -> Some tally.TallyAddress
    | Aggregate aggregate -> Some aggregate.AggregateAddress
    | InstantSum instantSum -> Some instantSum.InstantSumAddress
    | FirstSum firstSum -> Some firstSum.FirstSumAddress

let getInitialCachedValue env zetaExpr =
    match zetaExpr with
    | Source source -> UnitValue
    | Previous previous -> UnitValue
    | Instant instant -> UnitValue
    | InstantRandom instantRandom -> UnitValue
    | First first -> UnitValue
    | FirstRandom firstRandom -> UnitValue
    | Origin _ -> UnitValue
    | Value _ -> UnitValue
    | Yield _ -> UnitValue
    | Tally _ -> Int (makeIntRecord 0 None)
    | Aggregate aggregate -> evalExprDropEnv env aggregate.AggregateSeed
    | InstantSum instantSum -> UnitValue
    | FirstSum firstSum -> UnitValue

let makeAddressRecord version name optGroupType =
    { AddrVersion = version
      AddrName = name
      AddrOptGroupType = optGroupType
      AddrHash = (version, name).GetHashCode () }

let makeAddressRecord2 address optGroupType =
    { AddrVersion = address.AddrVersion
      AddrName = address.AddrName
      AddrOptGroupType = optGroupType
      AddrHash = (address.AddrVersion, address.AddrName).GetHashCode () }

let rec tryGetAddressFromLinksInternal addressLinks addressSoFar =
    match addressLinks with
    | [] -> Some addressSoFar
    | head :: tail ->
        match head with
        | ALName name ->
            let newName = addressSoFar.AddrName ++ (Lun.make DotStr) ++ name
            let newAddressSoFar = makeAddressRecord None newName addressSoFar.AddrOptGroupType
            tryGetAddressFromLinksInternal tail newAddressSoFar
        | _ -> None

let tryGetAddressFromLinks classTemplateAddress addressLinks =
    match addressLinks with
    | [] -> None
    | head :: tail ->
        match head with
        | ALMy ->
            let addressSoFar = makeAddressRecord2 classTemplateAddress (Some OneInGroup)
            tryGetAddressFromLinksInternal tail addressSoFar
        | ALAny ->
            match tail with
            | [] -> None
            | head :: tail ->
                match head with
                | ALName name ->
                    match tail with
                    | [] -> None
                    | [ALName _] ->
                        let groupAddress = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ name) None
                        let addressSoFar = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ name) (Some AnyInGroup)
                        tryGetAddressFromLinksInternal tail addressSoFar
                    | _ -> None
                | _ -> None
        | ALAll ->
            match tail with
            | [] -> None
            | head :: tail ->
                match head with
                | ALName name ->
                    match tail with
                    | [] -> None
                    | [ALName attributeName] ->
                        let groupAddress = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ name) None
                        let addressSoFar = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ name) (Some (AllInGroup (groupAddress, attributeName)))
                        tryGetAddressFromLinksInternal tail addressSoFar
                    | _ -> None
                | _ -> None
        | ALName name ->
            let addressSoFar = makeAddressRecord None name (Some OneInGroup)
            tryGetAddressFromLinksInternal tail addressSoFar

let symbolToAddressLink symbol =
    match symbol.SymName.LunStr with
    | "my" -> ALMy // TODO: make these literal variables
    | "any" -> ALAny
    | "all" -> ALAll
    | name -> ALName (Lun.make name)

let rec tryGetAddressLinksFromExpr event =
    match event with
    | Symbol symbol -> Some [symbolToAddressLink symbol]
    | Selector selector ->
        let optAddressLinks = tryGetAddressLinksFromExpr selector.SelTarget
        let optAddressLinks2 = tryGetAddressLinksFromExpr selector.SelKey
        match (optAddressLinks, optAddressLinks2) with
        | (Some addressLinks, Some addressLinks2) ->
            Some (addressLinks @ addressLinks2)
        | _ -> None
    | _ -> None

let tryGetAddressFromExpr classTemplateAddress event =
    let optLinks = tryGetAddressLinksFromExpr event
    match optLinks with
    | None -> None
    | Some links -> tryGetAddressFromLinks classTemplateAddress links

let makeSpecialBuiltinEntry (name, doc) =
    let symbol = Symbol (makeSymbolRecord name (ref CEUncached) None)
    (name, makeVariableEntry symbol doc)

let makeZetaArgs env context =
    let value = context.AIValue
    let oldValue = context.AIOldValue
    [value; oldValue]

let makeZetaLambda env body =
    makeLambdaRecord true Lun.empty ZetaLargs ZetaLargCount body tautology UnitValue UnitValue true None (Some env)

let makeAggregateArgs env zetaInstance context =
    let value = context.AIValue
    let oldValue = context.AIOldValue
    let state = zetaInstance.ZICachedValue
    [value; oldValue; state]

let makeAggregateLambda env body =
    makeLambdaRecord true Lun.empty AggregateLargs AggregateLargCount body tautology UnitValue UnitValue true None (Some env)

let makeSumArgs env summand context =
    let value = context.AIValue
    let oldValue = context.AIOldValue
    let sum = summand
    [value; oldValue; sum]

let makeSumLambda env body =
    makeLambdaRecord true Lun.empty SumLargs SumLargCount body tautology UnitValue UnitValue true None (Some env)

let makeDecomposedExpr proceduralExpr zetaExprs = {
    DecProceduralExpr = proceduralExpr
    DecZetaExprs = zetaExprs }

let makeWhileRecord optPredicate = {
    WhileOptPredicate = optPredicate }

let makeEventRecord evtName = {
    EventName = evtName }

let makeEventInstanceRecord eiEvent eiAddress = {
    EIEvent = eiEvent
    EIAddress = eiAddress }

let makeAttributeRecord attrName attrEvent attrDefault = {
    AttrName = attrName
    AttrEvent = attrEvent
    AttrDefault = attrDefault }
    
let makeAttributeTemplateRecord atAttribute atUpdateLimit atExpr atIsWhile atIsSpawnlet atOptClassAttributeInstance atOrigin = {
    ATAttribute = atAttribute
    ATUpdateLimit = atUpdateLimit
    ATDecomposedExpr = atExpr
    ATIsWhile = atIsWhile
    ATIsSpawnlet = atIsSpawnlet
    ATOptClassAttributeInstance = atOptClassAttributeInstance
    ATOrigin = atOrigin }

let makeAttributeInstanceRecord
        aiAttributeTemplate
        aiClassTemplate
        aiEventInstance
        aiZetaInstances
        aiAddress
        aiOptClassInstance
        aiValue
        aiOldValue
        aiUpdateCount =
        { AIAttributeTemplate = aiAttributeTemplate
          AIClassTemplate = aiClassTemplate
          AIEventInstance = aiEventInstance
          AIZetaInstances = aiZetaInstances
          AIAddress = aiAddress
          AIOptClassInstance = aiOptClassInstance
          AIValue = aiValue
          AIOldValue = aiOldValue
          AIUpdateCount = aiUpdateCount }

let makeClassRecord className classVersion classAttributes = {
    ClassName = className
    ClassVersion = classVersion
    ClassAttributes = classAttributes }

let makeClassTemplateRecord ctName ctClass ctWhile (*ctChildren*) ctAttributeTemplates = {
    CTName = ctName
    CTClass = ctClass
    CTWhile = ctWhile
    (*CTChildren = ctChildren*)
    CTAttributeTemplates = ctAttributeTemplates }

let makeClassInstanceRecord ciId ciAddress ciClassTemplate ciAttributeInstances = {
    CIId = ciId
    CIAddress = ciAddress
    CIClassTemplate = ciClassTemplate
    CIAttributeInstances = ciAttributeInstances }

let makeSimpleAttribute name defaultValue =
    let simpleEvent = makeEventRecord name
    makeAttributeRecord name simpleEvent defaultValue

let makeSimpleAttributeTemplate simpleAttribute originValue =
    let simpleAttributeExpr = { DecProceduralExpr = originValue; DecZetaExprs = [] }
    makeAttributeTemplateRecord simpleAttribute 0 simpleAttributeExpr false false None originValue

let makeAddressAttribute () =
    makeSimpleAttribute AddressLun EmptyStringValue

let makeAddressAttributeTemplate () =
    makeSimpleAttributeTemplate (makeAddressAttribute ()) EmptyStringValue

let makeTickAttribute () =
    makeSimpleAttribute TickLun ZeroIntValue

let makeTickAttributeTemplate () =
    makeSimpleAttributeTemplate (makeTickAttribute ()) ZeroIntValue

let makeGlobalClassTemplate () =
    let tickAttribute = makeTickAttribute ()
    let addressAttribute = makeAddressAttribute ()
    let attributes = LunTrie.ofSeq [(TickLun, tickAttribute); (AddressLun, addressAttribute)]
    let classRecord = makeClassRecord GlobalClassLun 1.0f attributes
    let tickAttributeTemplate = makeTickAttributeTemplate ()
    let addressAttributeTemplate = makeAddressAttributeTemplate ()
    let attributeTemplates = LunTrie.ofSeq [(TickLun, tickAttributeTemplate); (AddressLun, addressAttributeTemplate)]
    makeClassTemplateRecord GlobalLun classRecord { WhileOptPredicate = None } (*LunTrie.empty*) attributeTemplates

let makeWorkBench
    wbRunning
    wbComponents
    wbClasses
    wbClassTemplates
    wbClassInstances
    wbClassInstanceGroups
    wbUpdatingAttributeInstance
    wbUninstantiatedClassTemplates
    wbInstantiatingClassTemplates
    wbAttributeInstanceRegistry
    wbZetaInstanceRegistry
    wbUpdatedAttributeInstances =
    { WBRunning = wbRunning
      WBComponents = wbComponents
      WBClasses = wbClasses
      WBClassInstances = wbClassInstances
      WBClassInstanceGroups = wbClassInstanceGroups
      WBClassTemplates = wbClassTemplates
      WBUpdatingAttributeInstance = wbUpdatingAttributeInstance
      WBUninstantiatedClassTemplates = wbUninstantiatedClassTemplates
      WBAttributeInstanceRegistry = wbAttributeInstanceRegistry
      WBZetaInstanceRegistry = wbZetaInstanceRegistry
      WBUpdatedAttributeInstances = wbUpdatedAttributeInstances }

let makeEmptyWorkBench () =
    makeWorkBench
        (ref false)
        (Dictionary<Lun, IComponent> ())
        (Dictionary<Address, ClassRecord> ())
        (Dictionary<Address, ClassTemplateRecord> ())
        (Dictionary<Address, ClassInstanceRecord> ())
        (makeClassInstanceGroupGroupRecord (Dictionary<Address, ClassInstanceGroupRecord> ()))
        (ref None)
        (Generic.List<ClassTemplateRecord> ())
        (Generic.List<ClassTemplateRecord> ())
        (Dictionary<Address, AttributeInstanceRecord> ())
        (Dictionary<Address, ZetaInstance HashSet> ())
        (Generic.List<AttributeInstanceRecord> ())

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

let tryBuildWhile env classTemplateAddress exprs : (WhileRecord * Expr list) option =
    match exprs with
    | SpecialSeries specialSeries :: _ when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | Symbol symbol :: tail when symbol.SymName = WhileLun ->
            match tail with
            | [expr] ->
                let optDecomposedPredicate = tryDecomposeExpr classTemplateAddress expr
                match optDecomposedPredicate with
                | None -> None
                | Some decomposedPredicate ->
                    let name = symbol.SymName
                    let event = makeEventRecord name
                    let attribute = makeAttributeRecord name event expr
                    let attributeTemplate = makeAttributeTemplateRecord attribute 0 decomposedPredicate true false None TrueValue
                    Some (makeWhileRecord (Some attributeTemplate), exprs.Tail)
            | [expr; expr2] ->
                let optDecomposedPredicate = tryDecomposeExpr classTemplateAddress expr
                match optDecomposedPredicate with
                | None -> None
                | Some decomposedPredicate ->
                    let optIsSpawnlet =
                        match expr2 with
                        | Package package when package.PkgName = IsSpawnletLun ->
                            match package.PkgExpr with
                            | Boolean boolean -> Some boolean
                            | _ -> None
                        | _ -> None
                    match optIsSpawnlet with
                    | None -> None
                    | Some isSpawnlet ->
                        let name = symbol.SymName
                        let event = makeEventRecord name
                        let attribute = makeAttributeRecord name event expr
                        let attributeTemplate = makeAttributeTemplateRecord attribute 0 decomposedPredicate true isSpawnlet.BRValue None TrueValue
                        Some (makeWhileRecord (Some attributeTemplate), exprs.Tail)
            | _ -> None
        | _ -> Some (makeWhileRecord None, exprs)
    | _ -> Some (makeWhileRecord None, exprs)

let tryBuildAttribute env expr =
    match expr with
    | SpecialSeries specialSeries when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | head :: neck :: _ ->
            match head with
            | Symbol attributeName ->
                let eventRecord = makeEventRecord (attributeName.SymName ++ ChangeLun)
                Some (makeAttributeRecord attributeName.SymName eventRecord neck)
            | _ -> None
        | _ -> None
    | SpecialSeries specialSeries -> None
    | _ -> None

let tryBuildAttributes env exprs =
    match exprs with
    | [] -> Some ([], exprs)
    | head :: _ ->
        let optAttributeSeries = exprToOptSpecialSeries head
        match optAttributeSeries with
        | None -> None
        | Some specialSeries when specialSeries.SSType = DeclarativeExpr ->
            match specialSeries.SSExprs with
            | [] -> Some ([], exprs.Tail)
            | head :: tail ->
                match head with
                | Symbol symbol when symbol.SymName = AttributesLun ->
                    let optAttributes = List.map (tryBuildAttribute env) tail
                    if List.fornone (fun optAttribute -> Option.isNone optAttribute) optAttributes
                    then
                        let attributes = List.definitize optAttributes
                        let attributesPlusAddress = makeAddressAttribute () :: attributes
                        Some (attributesPlusAddress, exprs.Tail)
                    else None
                | _ -> None
        | Some _ -> None

let tryBuildVersion env exprs =
    match exprs with
    | [] -> Some (0.0f, exprs)
    | Package package :: _ when package.PkgName = VersionLun ->
        match package.PkgExpr with
        | Float float -> Some (float.FRValue, exprs.Tail)
        | _ -> None
    | Package _ :: _ -> None
    | _ :: _ -> Some (0.0f, exprs)

let tryBuildClass env exprs =
    match exprs with
    | SpecialSeries specialSeries :: tail when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | Symbol symbol :: tail ->
            match symbol.SymName.LunStr with
            | ClassStr ->
                match tail with
                | head :: tail ->
                    match head with
                    | Symbol classSymbol ->
                        let optVersionAndTail = tryBuildVersion env tail
                        match optVersionAndTail with
                        | None -> None
                        | Some (version, tail) ->
                            let optAttributes = tryBuildAttributes env tail
                            match optAttributes with
                            | Some (attributes, []) ->
                                let attributeTrie = LunTrie.ofSeqBy (fun attribute -> (attribute.AttrName, attribute)) attributes
                                let classRecord = makeClassRecord classSymbol.SymName version attributeTrie
                                Some (classRecord, exprs.Tail)
                            | _ -> None
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let tryBuildTemplateClassName env exprs =
    match exprs with
    | head :: _ ->
        let optClassSeries = exprToOptSpecialSeries head
        match optClassSeries with
        | Some specialSeries when specialSeries.SSType = DeclarativeExpr ->
            match specialSeries.SSExprs with
            | [Symbol symbol; Symbol symbol2] when symbol.SymName = ClassLun -> Some ((symbol2.SymName, 0.0f), exprs.Tail)
            | [Symbol symbol; Symbol symbol2; Package pkg] when symbol.SymName = ClassLun ->
                match pkg.PkgExpr with
                | Float float -> Some ((symbol2.SymName, float.FRValue), exprs.Tail)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let tryBuildAttributeTemplate env classTemplateAddress attributes exprs : (AttributeTemplateRecord * Expr list) option =
    match exprs with
    | SpecialSeries specialSeries :: _ when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | Symbol symbol :: tail when not tail.IsEmpty ->
            let attributeBody = tail.Head
            let attributeName = symbol.SymName
            let optAttribute = LunTrie.tryFind attributeName attributes
            match optAttribute with
            | None -> None
            | Some attribute ->
                let origin = attribute.AttrDefault
                let optDecomposedExpr = tryDecomposeExpr classTemplateAddress attributeBody
                match optDecomposedExpr with
                | None -> None
                | Some decomposedExpr ->
                    match tail.Tail with
                    | [] ->
                        match optDecomposedExpr with
                        | None -> None
                        | Some decomposedExpr -> Some (makeAttributeTemplateRecord attribute 1 decomposedExpr false false None origin, exprs.Tail)
                    | [updateCountExpr] ->
                        match updateCountExpr with
                        | Package package when package.PkgName = UpdateLimitLun ->
                            let updateCountValue = evalExprDropEnv env package.PkgExpr
                            match updateCountValue with
                            | Int intRecord -> Some (makeAttributeTemplateRecord attribute intRecord.IRValue decomposedExpr false false None origin, exprs.Tail)
                            | _ -> None
                        | _ -> None
                    | _ -> None
        | _ -> None
    | _ -> None

let tryBuildAttributeTemplates env classTemplateAddress attributes exprs : (AttributeTemplateRecord list * Expr list) option =
    match exprs with
    | SpecialSeries specialSeries :: _ ->
        match specialSeries.SSExprs with
        | Symbol symbol :: tail when symbol.SymName.LunStr = AttributesStr ->
            // TODO: try to improve mutation here
            let mutable mutTail = tail
            let mutable mutAttributeTemplates = []
            while not (List.isEmpty mutTail) do
                let optAttributeTemplate = tryBuildAttributeTemplate env classTemplateAddress attributes mutTail
                match optAttributeTemplate with
                | None -> mutTail <- []
                | Some (attributeTemplate, newTail) -> 
                    mutAttributeTemplates <- attributeTemplate :: mutAttributeTemplates
                    mutTail <- newTail
            if not (List.areSameLength mutAttributeTemplates specialSeries.SSExprs.Tail) then None
            else let attributeTemplatesRev = List.rev mutAttributeTemplates in Some (attributeTemplatesRev, exprs.Tail)
        | _ -> Some ([], exprs)
    | _ -> Some ([], exprs)

(*let rec tryBuildChildren env workBench exprs =
    match exprs with
    | [] -> Some ([], exprs)
    | head :: _ ->
        let optChildSeries = exprToOptSpecialSeries head
        match optChildSeries with
        | None -> None
        | Some specialSeries ->
            match specialSeries.SSExprs with
            | Symbol symbol :: tail when symbol.SymName = ChildrenLun ->
                let optChildren =
                    let mutable mutTail = tail
                    let mutable mutClassTemplates = []
                    while not mutTail.IsEmpty do
                        let optClassTemplate = tryBuildClassTemplate env workBench mutTail
                        match optClassTemplate with
                        | None -> mutTail <- []
                        | Some (classTemplate, newTail) -> 
                            mutClassTemplates <- classTemplate :: mutClassTemplates
                            mutTail <- newTail
                    if not (List.areSameLength mutClassTemplates tail) then None
                    else let classTemplatesRev = List.rev mutClassTemplates in Some classTemplatesRev
                match optChildren with
                | None -> None
                | Some children -> Some (children, exprs.Tail)
            | _ -> Some ([], exprs)*)

let tryBuildClassTemplate env workBench exprs : (ClassTemplateRecord * Expr list) option =
    match exprs with
    | SpecialSeries specialSeries :: tail when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | Symbol symbol :: tail ->
            match symbol.SymName.LunStr with
            | ClassTemplateStr ->
                match tail with
                | Symbol symbol :: tail ->
                    let name = symbol.SymName
                    let address = makeAddressRecord None name None
                    let optTemplateClassName = tryBuildTemplateClassName env tail
                    match optTemplateClassName with
                    | Some ((className, classVersion), tail) ->
                        let classRecord = ref Unchecked.defaultof<ClassRecord>
                        let classAddress = makeAddressRecord (Some classVersion) className None
                        if workBench.WBClasses.TryGetValue (classAddress, classRecord) then
                            let optWhile = tryBuildWhile env address tail
                            match optWhile with
                            | None -> None
                            | Some (whileRecord, tail) ->
                                (*let optChildren = tryBuildChildren env workBench tail
                                match optChildren with
                                | None -> None
                                | Some (children, tail) ->
                                    let childTrie = LunTrie.ofSeqBy (fun attribute -> (attribute.CTName, attribute)) children*)
                                    let optAttributeTemplates = tryBuildAttributeTemplates env address (!classRecord).ClassAttributes tail
                                    match optAttributeTemplates with
                                    | Some (attributeTemplates, []) ->
                                        let attributeTemplateTrie = LunTrie.ofSeqBy (fun attributeTemplate -> (attributeTemplate.ATAttribute.AttrName, attributeTemplate)) attributeTemplates
                                        let classTemplate = makeClassTemplateRecord name !classRecord whileRecord (*childTrie*) attributeTemplateTrie
                                        for attributeTemplate in attributeTemplates do
                                            let attribute = attributeTemplate.ATAttribute
                                            let classAttributeAddress = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ classTemplate.CTClass.ClassName ++ (Lun.make DotStr) ++ attribute.AttrName) None
                                            let classAttributeInstance = ref Unchecked.defaultof<AttributeInstanceRecord>
                                            if not (workBench.WBAttributeInstanceRegistry.TryGetValue (classAttributeAddress, classAttributeInstance)) then
                                                let classAttributeEventInstance = makeEventInstanceRecord attribute.AttrEvent classAttributeAddress
                                                let classAttributeInstance =
                                                    makeAttributeInstanceRecord
                                                        attributeTemplate
                                                        classTemplate
                                                        classAttributeEventInstance
                                                        (Dictionary<int64, ZetaInstance> ())
                                                        classAttributeAddress
                                                        None
                                                        UnitValue
                                                        UnitValue
                                                        0
                                                attributeTemplate.ATOptClassAttributeInstance <- Some classAttributeInstance
                                                ignore (workBench.WBAttributeInstanceRegistry.Add (classAttributeAddress, classAttributeInstance))
                                        Some (classTemplate, exprs.Tail)
                                    | _ -> None
                        else None
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let tryInstallComponent env exprs =
    let workBench = getWorkBench env
    match exprs with
    | SpecialSeries specialSeries :: _ when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | [Symbol symbol; String string; String string2] when symbol.SymName = UsingComponentLun ->
            let componentPath = string.SRValue.SVValue
            let componentType = string2.SRValue.SVValue
            let componentTypeLun = Lun.make componentType
            let componentObj = ref Unchecked.defaultof<IComponent>
            if workBench.WBComponents.TryGetValue (componentTypeLun, componentObj) then
                // TODO: consider making a violation if a different component than a current one is loaded
                Some (makeEvalUnit env, exprs.Tail)
            else
                try let assembly = Reflection.Assembly.LoadFrom componentPath
                    let instance = assembly.CreateInstance componentType
                    if instance = null
                    then Some (makeEvalViolation env ":v/eval/dol/componentCreationFailure" ("Could not create Dol component '" + componentType + "'."), exprs.Tail)
                    else
                        let newComponent = instance :?> IComponent
                        workBench.WBComponents.Add (componentTypeLun, newComponent)
                        let optNewEnv = newComponent.TryInitialize env workBench
                        match optNewEnv with
                        | None -> Some (makeEvalViolation env ":v/eval/dol/componentCreationFailure" ("Could not create Dol component '" + componentType + "' due to duplicate declaration names."), exprs.Tail)
                        | Some newEnv -> Some (makeEvalUnit newEnv, exprs.Tail)
                with exn -> Some (makeEvalExceptionViolation env exn, exprs.Tail)
        | _ -> None
    | _ -> None

let tryRecognizeExecution env exprs =
    match exprs with
    | SpecialSeries specialSeries :: _ when specialSeries.SSType = DeclarativeExpr ->
        match specialSeries.SSExprs with
        | [Symbol symbol] when symbol.SymName = ExecutionLun -> Some exprs.Tail
        | _ -> None
    | _ -> None

let classValueToSpecialObject members venv =
    makeViolation ":v/eval/dol" "Unimplemented."

let classTemplateValueToSpecialObject members venv =
    makeViolation ":v/eval/dol" "Unimplemented."

let classInstanceValueToSpecialObject members venv =
    makeViolation ":v/eval/dol" "Unimplemented."

let applyZetaLambda env body context =
    let lambda = makeZetaLambda env body
    let args = makeZetaArgs env context
    applyLambda env args args.Length lambda.LamArgs lambda.LamArgCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost lambda.LamEmptyUnification true (Lambda lambda)

let applyAggregateLambda env body zetaInstance context =
    let lambda = makeAggregateLambda env body
    let args = makeAggregateArgs env zetaInstance context
    applyLambda env args args.Length lambda.LamArgs lambda.LamArgCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost lambda.LamEmptyUnification true (Lambda lambda)

let applySumLambda env body summand context =
    let lambda = makeSumLambda env body
    let args = makeSumArgs env summand context
    applyLambda env args args.Length lambda.LamArgs lambda.LamArgCount lambda.LamBody lambda.LamCpre lambda.LamPre lambda.LamPost lambda.LamEmptyUnification true (Lambda lambda)

let withUpdatingAttributeInstance fn workBench attributeInstance =
    let currentOptAttributeInstance = !workBench.WBUpdatingAttributeInstance
    workBench.WBUpdatingAttributeInstance := Some attributeInstance
    let result = fn ()
    workBench.WBUpdatingAttributeInstance := currentOptAttributeInstance
    result
    
let tryGetAttributeInstance workBench zetaInstance address =
    match address.AddrOptGroupType with
    | None -> failwith "Unexpected match failure in 'Dol.tryGetAttributeInstance'."
    | Some groupType ->
        match groupType with
        | OneInGroup ->
            let result = ref Unchecked.defaultof<AttributeInstanceRecord>
            if workBench.WBAttributeInstanceRegistry.TryGetValue (address, result)
            then Some (GAISingle !result)
            else None
        | AnyInGroup _ ->
            Some (GAIAny zetaInstance.ZIAttributeInstance)
        | AllInGroup (groupAddress, attributeName) ->
            let group = ref Unchecked.defaultof<ClassInstanceGroupRecord>
            if workBench.WBClassInstanceGroups.CIGGClassInstanceGroups.TryGetValue (groupAddress, group) then
                let classInstances = (!group).CIGClassInstances
                if classInstances.Count > 0 && not (LunTrie.containsKey attributeName (classInstances.Values.First ()).CIAttributeInstances)
                then None
                else
                    let attributesInstances =
                        Seq.map
                            (fun classInstance -> (LunTrie.tryFind attributeName classInstance.CIAttributeInstances).Value)
                            classInstances.Values
                    Some (GAIAll (List.ofSeq attributesInstances))
            else None
            
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

let instantiateAttributeTemplate env workBench classInstanceName classTemplate attributeTemplate =
    let attribute = attributeTemplate.ATAttribute
    let attributeAddress = makeAddressRecord None (classInstanceName ++ (Lun.make DotStr) ++ attribute.AttrName) None
    let event = attribute.AttrEvent
    let eventInstance = makeEventInstanceRecord event attributeAddress
    if attribute.AttrName = AddressLun then
        let addressValue = String (makeStringRecord (makeLiteralStringValue classInstanceName.LunStr) None)
        makeAttributeInstanceRecord
            attributeTemplate
            classTemplate
            eventInstance
            (Dictionary<int64, ZetaInstance> ())
            attributeAddress
            None
            addressValue
            UnitValue
            0
    else
        let attributeInstance =
            makeAttributeInstanceRecord
                attributeTemplate
                classTemplate
                eventInstance
                (Dictionary<int64, ZetaInstance> ())
                attributeAddress
                None
                UnitValue
                UnitValue
                0
        for zetaExpr in attributeTemplate.ATDecomposedExpr.DecZetaExprs do
            instantiateZetaExpr env workBench attributeInstance zetaExpr
        attributeInstance

let rec updateAttribute env workBench attributeInstance =
    let attributeTemplate = attributeInstance.AIAttributeTemplate
    if attributeTemplate.ATAttribute.AttrName <> AddressLun then
        let updateLimit = attributeTemplate.ATUpdateLimit
        let updateCount = attributeInstance.AIUpdateCount
        if updateLimit = 0 || updateCount < updateLimit then
            attributeInstance.AIUpdateCount <- updateCount + 1
            let value =
                withUpdatingAttributeInstance
                    (fun () -> evalExprDropEnv env attributeTemplate.ATDecomposedExpr.DecProceduralExpr)
                    workBench
                    attributeInstance
            if not (isViolation value) then
                attributeInstance.AIOldValue <- attributeInstance.AIValue
                attributeInstance.AIValue <- value
                workBench.WBUpdatedAttributeInstances.Add attributeInstance
                if attributeTemplate.ATIsWhile then
                    let classTemplate = attributeInstance.AIClassTemplate
                    let newValue = isTrue attributeInstance.AIValue
                    let oldValue = isTrue attributeInstance.AIOldValue
                    match attributeInstance.AIOptClassInstance with
                    | None ->
                        if attributeTemplate.ATIsSpawnlet then
                            if newValue then
                                let attributeInstanceAddress =
                                    let attributeAddress = attributeInstance.AIAddress
                                    let instanceVersion = attributeAddress.AddrVersion
                                    let instanceName = Lun.make (attributeAddress.AddrName.LunStr.Substring (WhileStr.Length + 1))
                                    makeAddressRecord instanceVersion instanceName None
                                let attributeInstanceCopy =
                                    makeAttributeInstanceRecord
                                        attributeInstance.AIAttributeTemplate
                                        attributeInstance.AIClassTemplate
                                        attributeInstance.AIEventInstance
                                        (Dictionary<int64, ZetaInstance> ())
                                        attributeInstanceAddress
                                        None
                                        attributeInstance.AIValue
                                        attributeInstance.AIOldValue
                                        attributeInstance.AIUpdateCount
                                for zetaInstance in attributeInstance.AIZetaInstances do
                                    let zetaInstanceCopy = { zetaInstance.Value with ZIAttributeInstance = attributeInstanceCopy }
                                    attributeInstanceCopy.AIZetaInstances.Add (zetaInstance.Key, zetaInstanceCopy)
                                instantiateClassTemplate env workBench (Some attributeInstance) classTemplate
                        else
                            if newValue && not oldValue then
                                instantiateClassTemplate env workBench (Some attributeInstance) classTemplate
                    | Some classInstance ->
                        if not newValue then
                            ignore (workBench.WBClassInstances.Remove classInstance.CIAddress)
                            let classInstanceGroupAddress = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ classTemplate.CTClass.ClassName) None // TODO: consider adding this address to ClassRecord for speed / convenience
                            ignore (workBench.WBClassInstanceGroups.CIGGClassInstanceGroups.[classInstanceGroupAddress].CIGClassInstances.Remove classInstance.CIAddress)
                            for attributeInstance in LunTrie.toValueList classInstance.CIAttributeInstances do
                                ignore (workBench.WBAttributeInstanceRegistry.Remove attributeInstance.AIAddress)
                                for zetaInstance in attributeInstance.AIZetaInstances.Values do
                                    ignore (workBench.WBZetaInstanceRegistry.Remove zetaInstance.ZIOptAddress.Value)

and setUpAttributeInstance env workBench optClassInstance attributeInstance =

    // designate class instance
    attributeInstance.AIOptClassInstance <- optClassInstance

    // update attribute
    updateAttribute env workBench attributeInstance

    // register attribute
    workBench.WBAttributeInstanceRegistry.Add (attributeInstance.AIAddress, attributeInstance)

    // register attribute's zeta instances
    for zetaInstance in attributeInstance.AIZetaInstances do
        match zetaInstance.Value.ZIOptAddress with
        | None -> ()
        | Some address ->
            let registeredZetaInstances = ref Unchecked.defaultof<ZetaInstance HashSet>
            if workBench.WBZetaInstanceRegistry.TryGetValue (address, registeredZetaInstances)
            then ignore ((!registeredZetaInstances).Add zetaInstance.Value)
            else workBench.WBZetaInstanceRegistry.Add (address, HashSet.singleton zetaInstance.Value)
    
and instantiateClassTemplate env workBench optWhileAttribute classTemplate =

    // make class instance
    let id = getNextClassInstanceId ()
    let name = getClassInstanceName id classTemplate
    let attributeInstances =
        LunTrie.map
            (fun _ attributeTemplate -> instantiateAttributeTemplate env workBench name classTemplate attributeTemplate)
            classTemplate.CTAttributeTemplates
    let classInstanceAddress = makeAddressRecord None name None
    let classInstance = makeClassInstanceRecord id classInstanceAddress classTemplate attributeInstances
    let someClassInstance = Some classInstance

    // set up while attribute if appropriate
    match optWhileAttribute with
    | Some whileAttribute ->
        if whileAttribute.AIAttributeTemplate.ATIsSpawnlet
        then setUpAttributeInstance env workBench someClassInstance whileAttribute
        else whileAttribute.AIOptClassInstance <- someClassInstance
    | _ -> ()

    // set up attribute instances
    for attributeInstance in LunTrie.toValueList attributeInstances do
        setUpAttributeInstance env workBench someClassInstance attributeInstance
        
    // register class instance in group
    let classInstanceGroupAddress = makeAddressRecord None (ClassLun ++ (Lun.make DotStr) ++ classTemplate.CTClass.ClassName) None // TODO: consider adding this address to ClassRecord for speed / convenience
    let classInstanceGroup = ref Unchecked.defaultof<ClassInstanceGroupRecord>
    if not (workBench.WBClassInstanceGroups.CIGGClassInstanceGroups.TryGetValue (classInstanceGroupAddress, classInstanceGroup)) then
        classInstanceGroup := makeClassInstanceGroupRecord (Dictionary<Address, ClassInstanceRecord> ())
        workBench.WBClassInstanceGroups.CIGGClassInstanceGroups.Add (classInstanceGroupAddress, !classInstanceGroup)
    (!classInstanceGroup).CIGClassInstances.Add (classInstanceAddress, classInstance)

    // register class instance
    workBench.WBClassInstances.Add (classInstanceAddress, classInstance)

let instantiateInstantiatingClassTemplate env workBench classTemplate =
    let id = getNextClassInstanceId ()
    let name = Lun.make (WhileStr + DotStr + (getClassInstanceName id classTemplate).LunStr)
    let attributeInstance = instantiateAttributeTemplate env workBench name classTemplate classTemplate.CTWhile.WhileOptPredicate.Value // optPredicate's existence rightfully presumed
    setUpAttributeInstance env workBench None attributeInstance

let instantiateUninstantiatedClassTemplates env workBench =
    for classTemplate in workBench.WBUninstantiatedClassTemplates do
        if classTemplate.CTWhile.WhileOptPredicate.IsSome then instantiateInstantiatingClassTemplate env workBench classTemplate
        else instantiateClassTemplate env workBench None classTemplate
    workBench.WBUninstantiatedClassTemplates.Clear ()

let rec propagateChange env workBench attributeInstance =

    // raise instance event
    let attributeAddress = attributeInstance.AIAddress
    let zetaInstances = ref Unchecked.defaultof<ZetaInstance HashSet>
    if workBench.WBZetaInstanceRegistry.TryGetValue (attributeAddress, zetaInstances) then
        raiseEvent env workBench !zetaInstances

    // raise class event
    match attributeInstance.AIAttributeTemplate.ATOptClassAttributeInstance with
    | None -> ()
    | Some classAttributeInstance ->
        let classAttributeAddress = classAttributeInstance.AIAddress
        let classZetaInstances = ref Unchecked.defaultof<ZetaInstance HashSet>
        if workBench.WBZetaInstanceRegistry.TryGetValue (classAttributeAddress, classZetaInstances) then
            raiseEvent env workBench !classZetaInstances

and raiseEvent env workBench zetaInstances =
    for zetaInstance in zetaInstances do
        updateZetaInstance env workBench zetaInstance
    for zetaInstance in zetaInstances do
        updateAttribute env workBench zetaInstance.ZIAttributeInstance
    for zetaInstance in zetaInstances do
        propagateChange env workBench zetaInstance.ZIAttributeInstance

let raiseTickEvent env workBench =
    let tickAddress = makeAddressRecord None (Lun.make (GlobalStr + DotStr + TickStr)) None
    let tickAttribute = workBench.WBAttributeInstanceRegistry.[tickAddress]
    match tickAttribute.AIValue with
    | Int intRecord ->
        tickAttribute.AIOldValue <- tickAttribute.AIValue
        tickAttribute.AIValue <- Int (makeIntRecord (intRecord.IRValue + 1) None)
    | _ -> failwith "Unexpected match failure in 'Dol.raiseTickEvent'."
    let tickEvent = tickAttribute.AIEventInstance
    let zetaInstances = ref Unchecked.defaultof<ZetaInstance HashSet> 
    if workBench.WBZetaInstanceRegistry.TryGetValue (tickAddress, zetaInstances) then
        withUpdatingAttributeInstance
            (fun () -> raiseEvent env workBench !zetaInstances)
            workBench
            tickAttribute

let clearUpdateFlags workBench =
    for attributeInstance in workBench.WBUpdatedAttributeInstances do
        attributeInstance.AIUpdateCount <- 0
    workBench.WBUpdatedAttributeInstances.Clear ()

/// Execute Dol.
let execute env =

    // grab work bench and get it running
    let mutable key = Unchecked.defaultof<ConsoleKeyInfo>
    let workBench = getWorkBench env
    workBench.WBRunning := true

    // run one frame without tick
    instantiateUninstantiatedClassTemplates env workBench
    clearUpdateFlags workBench
    key <- Console.ReadKey true

    // run rest of frames with ticking
    while !workBench.WBRunning && (not Development || key.KeyChar <> '`') do
        raiseTickEvent env workBench
        clearUpdateFlags workBench
        instantiateUninstantiatedClassTemplates env workBench
        clearUpdateFlags workBench
        key <- Console.ReadKey true

/// The special built-in operators currently available from Dol.
let specialBuiltins = []

/// The special built-in operators as environment entries.
let specialBuiltinEntries =
    List.map makeSpecialBuiltinEntry specialBuiltins

/// The special built-in operators names.
let specialBuiltinNames =
    let names = List.map fst specialBuiltins
    List.toHashSet names

/// The appliable special built-in lambdas.
let appliableSpecialBuiltins = []

/// The appliable special built-in lambdas in dictionary form.
let appliableSpecialBuiltinDict =
    List.toDictionary appliableSpecialBuiltins

/// The Dol language module.
type Dlm () =
    
    /// Initialize Dol.
    member this.TryInitialize env =
        let workBench = makeEmptyWorkBench ()
        let newEnv = { env with EnvOptWorkBench = Some (workBench :> obj) }
        let globalClassTemplate = makeGlobalClassTemplate ()
        instantiateClassTemplate newEnv workBench None globalClassTemplate
        tryAppendDeclarationEntries newEnv specialBuiltinEntries

    /// Convert a Dol special value to a Dol special object.
    member this.SpecialValueToSpecialObject env expr =
        match expr with
        | SpecialValue specialValue ->
            match specialValue.SVExpr with
            | Composite composite ->
                let members = composite.CompMembers
                let valueType = ref Unchecked.defaultof<Member>
                if members.TryGetValue (ValueTypeLun, valueType) then
                    match (!valueType).MemExpr with
                    | String valueType ->
                        match valueType.SRValue.SVValue with
                        | ClassStr -> classValueToSpecialObject members env
                        | ClassTemplateStr -> classTemplateValueToSpecialObject members env
                        | ClassInstanceStr -> classInstanceValueToSpecialObject members env
                        | _ -> failwith "Unexpected match failure in 'Dol.SpecialValueToSpecialObject'."
                    | _ -> makeViolation ":v/contract/dol/invalidSpecialValueTypeForConversion" "Special value type is not a string."
                else makeViolation ":v/contract/dol/missingSpecialValueTypeForConversion" "Special value type is missing."
            | _ -> failwith "Unexpected match failure in 'Dol.SpecialValueToSpecialObject'."
        | _ -> failwith "Unexpected match failure in 'Dol.SpecialValueToSpecialObject'."

    /// Query that a symbol name represents a special built-in Dol operator.
    member this.IsSpecialBuiltin env symbolName =
        specialBuiltinNames.Contains symbolName

    /// Get the type of a Dol special object.
    member this.GetSpecialType env expr = UnitValue

    /// Apply a special built-in Dol operation.
    member this.ApplySpecialBuiltin env name args =
        let appliableSpecialBuiltin = ref Unchecked.defaultof<Env -> Lun -> Expr list -> EvalResult>
        if appliableSpecialBuiltinDict.TryGetValue (name, appliableSpecialBuiltin) then !appliableSpecialBuiltin env name args
        else failwith "Unexpected match failure in 'Dol.ApplySpecialBuiltin'."

    /// Apply a Dol special selector.
    member this.ApplySpecialSelector env key target = makeEvalUnit env

    /// Evaluate a Dol prefixed expression.
    member this.EvalPrefixed env expr = makeEvalUnit env

    /// Evaluate a Dol special object.
    member this.EvalSpecialObject env expr = makeEvalUnit env
    
    /// Evaluate a Dol special series.
    member this.EvalSpecialSeries env expr =
        let workBench = getWorkBench env
        match expr with
        | SpecialSeries specialSeries ->
            if specialSeries.SSType = MetaExpr then
                if specialSeries.SSExprs.IsEmpty
                then makeEvalViolation env ":v/eval/dol/emptyMetaExpression" "Meta-expressions cannot be empty in Dol."
                else
                    let currentAttributeInstance = (!workBench.WBUpdatingAttributeInstance).Value // updating attribute is rightly assumed to exist
                    let zetaInstance = currentAttributeInstance.AIZetaInstances.[specialSeries.SSSpecialId]
                    let resultValue =
                        match zetaInstance.ZIZetaExpr with
                        | Source source ->
                            match currentAttributeInstance.AIOptClassInstance with
                            | None -> UnitValue
                            | Some classInstance ->
                                match source.SourceAttribute with
                                | Symbol symbol ->
                                    let optAttributeInstance = LunTrie.tryFind symbol.SymName classInstance.CIAttributeInstances
                                    match optAttributeInstance with
                                    | None -> UnitValue
                                    | Some attributeInstance -> attributeInstance.AIValue
                                | _ -> UnitValue
                        | Previous _ ->
                            let currentValue = currentAttributeInstance.AIValue
                            match currentValue with
                            | Series series when series.SerExprs.IsEmpty -> currentAttributeInstance.AIAttributeTemplate.ATOrigin
                            | _ -> currentValue
                        | Instant instant ->
                            let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                            match optGetResult with
                            | None -> UnitValue
                            | Some getResult ->
                                match getResult with
                                | GAISingle attributeInstance
                                | GAIAny attributeInstance -> attributeInstance.AIValue
                                | GAIAll _ -> UnitValue
                        | InstantRandom instantRandom ->
                            Float (makeFloatRecord (getNextRandomFloat ()) None)
                        | First first ->
                            let cachedValue = zetaInstance.ZICachedValue
                            if isUnit cachedValue then
                                let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                                match optGetResult with
                                | None -> UnitValue
                                | Some getResult ->
                                    match getResult with
                                    | GAISingle attributeInstance
                                    | GAIAny attributeInstance -> attributeInstance.AIValue
                                    | GAIAll _ -> UnitValue
                            else cachedValue
                        | FirstRandom firstRandom ->
                            let cachedValue = zetaInstance.ZICachedValue
                            if isUnit cachedValue
                            then Float (makeFloatRecord (getNextRandomFloat ()) None)
                            else cachedValue
                        | Origin _ ->
                            currentAttributeInstance.AIAttributeTemplate.ATOrigin
                        | Value value ->
                            let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                            match optGetResult with
                            | None -> UnitValue
                            | Some getResult ->
                                match getResult with
                                | GAISingle attributeInstance
                                | GAIAny attributeInstance -> attributeInstance.AIValue
                                | GAIAll _ -> UnitValue
                        | Yield yieldRecord ->
                            let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                            match optGetResult with
                            | None -> UnitValue
                            | Some getResult ->
                                match getResult with
                                | GAISingle attributeInstance
                                | GAIAny attributeInstance ->
                                    let lambdaResult = applyZetaLambda env yieldRecord.YieldBody.DecProceduralExpr attributeInstance
                                    lambdaResult.Value
                                | GAIAll _ -> UnitValue
                        | Tally _ ->
                            zetaInstance.ZICachedValue
                        | Aggregate _ ->
                            zetaInstance.ZICachedValue
                        | InstantSum instantSum ->
                            let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                            match optGetResult with
                            | None -> UnitValue
                            | Some getResult ->
                                match getResult with
                                | GAISingle _
                                | GAIAny _ -> UnitValue
                                | GAIAll attributeInstances ->
                                    match attributeInstances with
                                    | [] -> UnitValue
                                    | head :: tail ->
                                        let sumExpr = instantSum.InstantSumBody.DecProceduralExpr
                                        let sumValue =
                                            List.fold
                                                (fun sum attributeInstance ->
                                                    let lambdaResult = applySumLambda env sumExpr sum attributeInstance
                                                    lambdaResult.Value)
                                                head.AIValue
                                                tail
                                        sumValue
                        | FirstSum firstSum ->
                            let cachedValue = zetaInstance.ZICachedValue
                            if isUnit cachedValue then
                                let optGetResult = tryGetAttributeInstance workBench zetaInstance zetaInstance.ZIOptAddress.Value // address is rightly assumed to exist
                                match optGetResult with
                                | None -> UnitValue
                                | Some getResult ->
                                    match getResult with
                                    | GAISingle _
                                    | GAIAny _ -> UnitValue
                                    | GAIAll attributeInstances ->
                                        match attributeInstances with
                                        | [] -> UnitValue
                                        | head :: tail ->
                                            let sumExpr = firstSum.FirstSumBody.DecProceduralExpr
                                            let sumValue =
                                                List.fold
                                                    (fun sum attributeInstance ->
                                                        let lambdaResult = applySumLambda env sumExpr sum attributeInstance
                                                        lambdaResult.Value)
                                                    head.AIValue
                                                    tail
                                            sumValue
                            else cachedValue
                    makeEvalResult env resultValue
            else
                let exprs = [expr]
                let optClass = tryBuildClass env exprs
                match optClass with
                | Some (classValue, []) ->
                    let classContentAddress = makeAddressRecord (Some classValue.ClassVersion) classValue.ClassName None
                    workBench.WBClasses.Add (classContentAddress, classValue)
                    let specialObjectRecord = makeSpecialObjectRecord true LanguageGuid (Class classValue) None
                    makeEvalResult env (SpecialObject specialObjectRecord)
                | _ ->
                    let optClassTemplate = tryBuildClassTemplate env workBench exprs
                    match optClassTemplate with
                    | Some (classTemplate, []) ->
                        let classTemplateAddress = makeAddressRecord None classTemplate.CTName None
                        workBench.WBClassTemplates.Add (classTemplateAddress, classTemplate)
                        workBench.WBUninstantiatedClassTemplates.Add classTemplate
                        let specialObjectRecord = makeSpecialObjectRecord true LanguageGuid (ClassTemplate classTemplate) None
                        makeEvalResult env (SpecialObject specialObjectRecord)
                    | _ ->
                        match tryInstallComponent env exprs with
                        | Some (evalResult, []) -> evalResult
                        | _ ->
                            match tryRecognizeExecution env exprs with
                            | Some [] ->
                                execute env
                                makeEvalUnit env
                            | _ -> makeEvalViolation env ":v/eval/dol/execution" "Execution declaration takes no arguments."
        | _ -> failwith "Unexpected match failure in Dol.Dlm.EvalSpecialSeries."

    interface ILanguageModule with
        member this.TryInitialize env = this.TryInitialize env
        member this.SpecialValueToSpecialObject env expr = this.SpecialValueToSpecialObject env expr
        member this.IsSpecialBuiltin env symbolName = this.IsSpecialBuiltin env symbolName
        member this.GetSpecialType env expr = this.GetSpecialType env expr
        member this.ApplySpecialBuiltin env name args = this.ApplySpecialBuiltin env name args
        member this.ApplySpecialSelector env key target = this.ApplySpecialSelector env key target
        member this.EvalPrefixed env expr = this.EvalPrefixed env expr
        member this.EvalSpecialObject env expr = this.EvalSpecialObject env expr
        member this.EvalSpecialSeries env expr = this.EvalSpecialSeries env expr
        member this.Name = LanguageNameLun
        member this.Guid = LanguageGuid

type ConsoleKeyComponent () =
    interface IComponent with
        member this.TryInitialize env workBench = Some env
        member this.Advance env workBench = let key = Console.ReadKey in ()
        member this.Name = ConsoleKeyComponentNameLun
#endif