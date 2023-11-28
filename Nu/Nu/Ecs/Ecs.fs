// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Ecs
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.ComponentModel
open System.IO
open System.Threading.Tasks
open Prime
open Nu

/// An unscheduled Ecs event callback.
type private EcsCallbackUnscheduled<'d, 'w when 'w : not struct> =
    EcsEvent<'d, 'w> -> Ecs -> 'w -> 'w

/// A scheduled Ecs event callback.
and [<ReferenceEquality>] EcsCallbackScheduled<'d, 'w when 'w : not struct> =
    { EcsQuery : Query
      EcsDependencies : Query list
      EcsCallback : EcsEvent<'d, 'w> -> Ecs -> 'w -> unit }

/// A scheduled Ecs event callback.
and [<ReferenceEquality>] private EcsCallbackScheduledObj =
    { EcsQuery : Query
      EcsDependencies : Query list
      EcsCallbackObj : obj }

/// The type of Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEventType =
    | GlobalEvent
    | EntityEvent of Entity : EcsEntity
    | ComponentEvent of Entity2 : EcsEntity * ComponentEvent : string

/// An Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEvent =
    { EcsEventName : string
      EcsEventType : EcsEventType }

/// An Ecs event.
and EcsEvent<'d, 'w when 'w : not struct> =
    { EcsEventData : 'd }

/// Data for an Ecs registration event.
and [<StructuralEquality; NoComparison; Struct>] EcsChangeData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// Data for an Ecs registration event.
and [<StructuralEquality; NoComparison; Struct>] EcsRegistrationData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// The out-of-box events for the Ecs construct.
and [<AbstractClass; Sealed>] EcsEvents =
    static member PreUpdate = { EcsEventName = "PreUpdate"; EcsEventType = GlobalEvent }
    static member Update = { EcsEventName = "Update"; EcsEventType = GlobalEvent }
    static member PostUpdate = { EcsEventName = "PostUpdate"; EcsEventType = GlobalEvent }
    static member Render = { EcsEventName = "Render"; EcsEventType = GlobalEvent }
    static member Register entity compName = { EcsEventName = "Register"; EcsEventType = ComponentEvent (entity, compName) }
    static member Unregistering entity compName = { EcsEventName = "Unregistering"; EcsEventType = ComponentEvent (entity, compName) }
    static member Change entity = { EcsEventName = "Change"; EcsEventType = EntityEvent entity }

/// Identifies an archetype.
and ArchetypeId (terms : Map<string, Term>) =

    let terms = Map.add (nameof EntityId) (Intra (nameof EntityId, typeof<EntityId>)) terms

    let hashCode = hash terms // OPTIMIZATION: hash is cached for speed

    new (intraComponents, subterms : Map<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponents |> Seq.map (fun (compName, compTy) -> (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    new (intraComponentTypes : Type seq, subterms : Map<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponentTypes |> Seq.map (fun compTy -> let compName = compTy.Name in (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    member this.Terms : Map<string, Term> =
        terms

    member this.HashCode =
        hashCode

    member this.AddTerm termName term =
        if strEq termName (nameof EntityId) then
            failwith "Cannot update an archetype's EntityId term."
        ArchetypeId (Map.add termName term terms)

    member this.RemoveTerm termName =
        if strEq termName (nameof EntityId) then
            failwith "All archetypes require an EntityId component."
        ArchetypeId (Map.remove termName terms)

    static member equals (left : ArchetypeId) (right : ArchetypeId) =
        left.HashCode = right.HashCode &&
        Term.equalsMany left.Terms right.Terms

    override this.GetHashCode () =
        hashCode

    override this.Equals that =
        match that with
        | :? ArchetypeId as that -> ArchetypeId.equals this that
        | _ -> failwithumf ()

    static member make (intraComponents : obj seq, subterms) =
        let intraComponentTypes = Seq.map getType intraComponents
        let intraComponentNames = Seq.map (fun (ty : Type) -> ty.Name) intraComponentTypes
        let intraComponents = Seq.zip intraComponentNames intraComponentTypes
        ArchetypeId (intraComponents, subterms)

    static member make (intraComponents : (string * obj) seq, subterms) =
        let intraComponents = Seq.map (fun (compName, compValue) -> (compName, getType compValue)) intraComponents
        ArchetypeId (intraComponents, subterms)

    static member make ?subterms =
        ArchetypeId (([] : (string * Type) list), Option.defaultValue Map.empty subterms)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        (?compName, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (?compName, ?comp2Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (?compName, ?comp2Name, ?comp3Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.defaultValue typeof<'c5>.Name comp5Name, typeof<'c5>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.defaultValue typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.defaultValue typeof<'c6>.Name comp6Name, typeof<'c6>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.defaultValue typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.defaultValue typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.defaultValue typeof<'c7>.Name comp7Name, typeof<'c7>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.defaultValue typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.defaultValue typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.defaultValue typeof<'c7>.Name comp7Name, typeof<'c7>)
              (Option.defaultValue typeof<'c8>.Name comp8Name, typeof<'c8>)],
             Option.defaultValue Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?subterms) =
        ArchetypeId
            ([(Option.defaultValue typeof<'c>.Name compName, typeof<'c>)
              (Option.defaultValue typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.defaultValue typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.defaultValue typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.defaultValue typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.defaultValue typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.defaultValue typeof<'c7>.Name comp7Name, typeof<'c7>)
              (Option.defaultValue typeof<'c8>.Name comp8Name, typeof<'c8>)
              (Option.defaultValue typeof<'c9>.Name comp9Name, typeof<'c9>)],
             Option.defaultValue Map.empty subterms)

/// A collection of component stores.
and Archetype (archetypeId : ArchetypeId) =

    let mutable freeIndex = 0
    let freeList = hashSetPlus<int> HashIdentity.Structural []
    let entityIdStore = Store<EntityId> "EntityId"
    let stores = Dictionary.singleton StringComparer.Ordinal "EntityId" (entityIdStore :> Store)

    do
        let storeTypeGeneric = typedefof<EntityId Store>
        for termEntry in archetypeId.Terms do
            if termEntry.Key <> nameof EntityId then
                match termEntry.Value with
                | Intra (name, ty)
                | Extra (name, ty, _) ->
                    let storeType = storeTypeGeneric.MakeGenericType [|ty|]
                    let store = Activator.CreateInstance (storeType, name) :?> Store
                    stores.[name] <- store
                | _ -> ()

    member this.Id = archetypeId
    member this.Length = freeIndex
    member this.EntityIdStore = entityIdStore
    member this.Stores = stores

    member private this.Grow () =
        for storeEntry in stores do
            storeEntry.Value.Grow ()

    member private this.AllocIndex entityId =
        let index =
            if freeList.Count > 0 then
                let index = Seq.head freeList
                freeList.Remove index |> ignore<bool>
                index
            else
                match Seq.tryHead stores with
                | Some headStoreEntry ->
                    let index = freeIndex
                    if index = headStoreEntry.Value.Length then this.Grow ()
                    freeIndex <- inc freeIndex
                    index
                | None ->
                    let index = freeIndex
                    freeIndex <- inc freeIndex
                    index
        entityIdStore.[index] <- { Active = true; EntityId = entityId }
        index

    member private this.FreeIndex index =
        entityIdStore.[index] <- { Active = false; EntityId = 0UL }
        if index = dec freeIndex
        then freeIndex <- dec freeIndex
        else freeList.Add index |> ignore<bool>

    member this.Register (comps : Dictionary<string, obj>) entityId =
        let index = this.AllocIndex entityId
        for compEntry in comps do
            stores.[compEntry.Key].SetItem index compEntry.Value
        index

    member this.Unregister (index : int) =
        for storeEntry in stores do
            storeEntry.Value.ZeroItem index
        this.FreeIndex index

    member this.GetComponents index =
        let comps = dictPlus<string, obj> StringComparer.Ordinal []
        for storeEntry in stores do
            comps.Add (storeEntry.Key, storeEntry.Value.[index])
        comps

    member this.Read count (stream : FileStream) =
        let firstIndex = freeIndex
        let lastIndex = freeIndex + count
        match Seq.tryHead stores with
        | Some headStoreEntry ->
            while headStoreEntry.Value.Length <= lastIndex do
                this.Grow ()
        | None -> ()
        for storeEntry in stores do
            let store = storeEntry.Value
            store.Read count freeIndex stream
        freeIndex <- inc lastIndex
        (firstIndex, lastIndex)

/// Type converter for Ecs (just serilizes to and from unit).
and EcsConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<Ecs>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then Symbols ([], ValueNone) :> obj
        elif destType = typeof<Ecs> then source
        else failconv "Invalid EcsConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Ecs>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols ([], _) -> Ecs () :> obj
            | _ -> failconv "Invalid EcsConverter conversion from source." (Some symbol)
        | :? Ecs -> source
        | _ -> failconv "Invalid EcsConverter conversion from source." None

/// An archetype-based Ecs construct,
and [<TypeConverter (typeof<EcsConverter>)>] Ecs () =

    let subscriptionIdLock = obj ()
    let mutable subscriptionIdCurrent = 0u
    let entityIdLock = obj ()
    let mutable entityIdCurrent = 0UL
    let archetypes = ConcurrentDictionary<ArchetypeId, Archetype> HashIdentity.Structural
    let entitySlots = ConcurrentDictionary<uint64, EcsEntitySlot> HashIdentity.Structural
    let componentTypes = ConcurrentDictionary<string, Type> StringComparer.Ordinal
    let subscriptions = dictPlus<EcsEvent, Dictionary<uint32, obj>> HashIdentity.Structural []
    let subscribedEntities = dictPlus<EcsEntity, int> HashIdentity.Structural []
    let postEventOperations = ConcurrentQueue<obj> ()
    let queries = List<Query> ()

    let createArchetype (archetypeId : ArchetypeId) =
        let archetype = Archetype archetypeId
        archetypes.TryAdd (archetypeId, archetype) |> ignore<bool>
        for query in queries do
            query.TryRegisterArchetype archetype
        archetype

    /// Thread-safe.
    member private this.AllocSubscriptionId () =
        lock subscriptionIdLock $ fun () ->
            subscriptionIdCurrent <- inc subscriptionIdCurrent
            if subscriptionIdCurrent = UInt32.MaxValue then failwith "Unbounded use of Ecs subscription ids not supported."
            subscriptionIdCurrent

    /// Thread-safe.
    member private this.BoxCallback<'d, 'w when 'w : not struct> (callback : EcsCallbackUnscheduled<'d, 'w>) =
        let boxableCallback = fun (evt : EcsEvent<obj, 'w>) store ->
            let evt = { EcsEventData = evt.EcsEventData :?> 'd }
            callback evt store
        boxableCallback :> obj

    member private this.RegisterEntityInternal comps archetypeId entity =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let archetypeIndex =
            lock archetype $ fun () ->
                archetype.Register comps entity.EntityId
        entitySlots.TryAdd (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype }) |> ignore<bool>

    member private this.UnregisterEntityInternal entitySlot entity =
        let archetype = entitySlot.Archetype
        let comps = archetype.GetComponents entitySlot.ArchetypeIndex
        entitySlots.TryRemove entity.EntityId |> ignore<bool * EcsEntitySlot>
        lock archetype $ fun () ->
            archetype.Unregister entitySlot.ArchetypeIndex
        comps

    /// Thread-safe.
    member this.IndexEntitySlot (entity : EcsEntity) =
        entitySlots.[entity.EntityId]

    /// Thread-safe.
    member this.MakeEntity () =
        lock entityIdLock $ fun () ->
            entityIdCurrent <- inc entityIdCurrent
            if entityIdCurrent = UInt64.MaxValue then failwith "Unbounded use of Ecs entity ids not supported."
            { EntityId = entityIdCurrent; Ecs = this }

    /// Thread-safe.
    member this.RegisterPostEventOperation (operation : 'w -> 'w) =
        postEventOperations.Enqueue (operation :> obj)

    member this.Publish<'d, 'w when 'w : not struct> event (eventData : 'd) (world : 'w) : 'w =
        // NOTE: we allow some special munging with world here. The callback may choose to ignore the world and return
        // the default of 'w. This works fine so long as 'w is a reference type where null is not a proper value
        // because we restore the original world when a null result is detected. However, this does not work
        // when 'w is a reference type that has null as a proper value because we have no way to efficiently
        // detect that case. Option would be an example of a reference type with null as a proper value.
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.UnscheduledEventSuffix }
        let worldOld = world
        let mutable world = world
        match subscriptions.TryGetValue event with
        | (true, callbacks) ->
            for entry in callbacks do
                match entry.Value with
                | :? EcsCallbackUnscheduled<obj, 'w> as objCallback ->
                    let evt = { EcsEventData = eventData :> obj }
                    world <- match objCallback evt this world :> obj with null -> worldOld | world -> world :?> 'w
                | :? EcsCallbackUnscheduled<obj, obj> as objCallback ->
                    let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                    world <- match objCallback evt this world with null -> worldOld | world -> world :?> 'w
                | _ -> ()
            let mutable operation = Unchecked.defaultof<_>
            while postEventOperations.TryDequeue &operation do
                match operation with
                | :? ('w -> 'w) as operation -> world <- operation world
                | _ -> failwith "PostEventOperation does not match 'w type of publish call."
        | (false, _) -> ()
        world

    member this.SubscribePlus<'d, 'w when 'w : not struct> subscriptionId event (callback : EcsEvent<'d, 'w> -> Ecs -> 'w -> 'w) =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.UnscheduledEventSuffix }
        let subscriptionId =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks.Add (subscriptionId, this.BoxCallback<'d, 'w> callback)
                subscriptionId
            | (false, _) ->
                let callbacks = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d, 'w> callback)]
                subscriptions.Add (event, callbacks)
                subscriptionId
        match event.EcsEventType with
        | ComponentEvent (entity, _) ->
            match subscribedEntities.TryGetValue entity with
            | (true, count) -> subscribedEntities.[entity] <- inc count
            | (false, _) -> subscribedEntities.Add (entity, 1)
        | _ -> ()
        subscriptionId

    member this.Subscribe<'d, 'w when 'w : not struct> event callback =
        this.SubscribePlus<'d, 'w> (this.AllocSubscriptionId ()) event callback |> ignore

    member this.Unsubscribe event subscriptionId =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.UnscheduledEventSuffix }
        let result =
            match subscriptions.TryGetValue event with
            | (true, callbacks) -> callbacks.Remove subscriptionId
            | (false, _) -> false
        if result then
            match event.EcsEventType with
            | ComponentEvent (entity, _) ->
                match subscribedEntities.TryGetValue entity with
                | (true, count) ->
                    if count = 1
                    then subscribedEntities.Remove entity |> ignore<bool>
                    else subscribedEntities.[entity] <- inc count
                | (false, _) -> failwith "Subscribed entities count mismatch."
            | _ -> failwith "Subscribed entities count mismatch."
        result

    member this.Notify<'d, 'w when 'w : not struct> event (eventData : 'd) (world : 'w) =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.ScheduledEventSuffix }
        let mutable world = world
        match subscriptions.TryGetValue event with
        | (true, callbacks) ->
            let dependentCallbacks = List ()
            for entry in callbacks do
                match entry.Value with
                | :? EcsCallbackScheduled<obj, 'w> as objCallback ->
                    dependentCallbacks.Add { EcsQuery = objCallback.EcsQuery; EcsDependencies = objCallback.EcsDependencies; EcsCallbackObj = objCallback.EcsCallback }
                | :? EcsCallbackScheduled<obj, obj> as objCallback ->
                    dependentCallbacks.Add { EcsQuery = objCallback.EcsQuery; EcsDependencies = objCallback.EcsDependencies; EcsCallbackObj = objCallback.EcsCallback }
                | _ -> ()
            let getDependenciess = fun (callback : EcsCallbackScheduledObj) -> seq callback.EcsDependencies
            let getKey = fun (callback : EcsCallbackScheduledObj) -> callback.EcsQuery
            match dependentCallbacks.Group (getDependenciess, getKey) with
            | (true, groups) ->
                Log.debug "Cycle found in dependencies. Executing at arbitrary starting point."
                for group in groups do
                    for callback in group do
                        match callback.EcsCallbackObj with
                        | :? EcsCallbackUnscheduled<obj, 'w> as objCallback ->
                            let evt = { EcsEventData = eventData :> obj }
                            objCallback evt this world |> ignore<'w>
                        | :? EcsCallbackUnscheduled<obj, obj> as objCallback ->
                            let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                            objCallback evt this world |> ignore<obj>
                        | _ -> ()
            | (false, groups) ->
                for group in groups do
                    let result =
                        Parallel.ForEach (group, fun callback ->
                            match callback.EcsCallbackObj with
                            | :? EcsCallbackUnscheduled<obj, 'w> as objCallback ->
                                let evt = { EcsEventData = eventData :> obj }
                                objCallback evt this world |> ignore<'w>
                            | :? EcsCallbackUnscheduled<obj, obj> as objCallback ->
                                let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                                objCallback evt this world |> ignore<obj>
                            | _ -> ())
                    ignore result
            let mutable operation = Unchecked.defaultof<_>
            while postEventOperations.TryDequeue &operation do
                match operation with
                | :? ('w -> 'w) as operation -> world <- operation world
                | _ -> failwith "PostEventOperation does not match 'w type of publish call."
        | (false, _) -> ()
        world

    member this.SchedulePlus<'d, 'w when 'w : not struct> subscriptionId event (callback : EcsCallbackScheduled<'d, 'w>) =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.ScheduledEventSuffix }
        let subscriptionId =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks.Add (subscriptionId, callback)
                subscriptionId
            | (false, _) ->
                let callbacks = dictPlus HashIdentity.Structural [(subscriptionId, callback :> obj)]
                subscriptions.Add (event, callbacks)
                subscriptionId
        match event.EcsEventType with
        | ComponentEvent (entity, _) ->
            match subscribedEntities.TryGetValue entity with
            | (true, count) -> subscribedEntities.[entity] <- inc count
            | (false, _) -> subscribedEntities.Add (entity, 1)
        | _ -> ()
        subscriptionId

    member this.Schedule<'d, 'w when 'w : not struct> event callback =
        this.SchedulePlus<'d, 'w> (this.AllocSubscriptionId ()) event callback |> ignore<uint>

    member this.Unschedule event subscriptionId =
        this.Unsubscribe
            { event with EcsEventName = event.EcsEventName + Constants.Ecs.ScheduledEventSuffix }
            subscriptionId

    /// Thread-safe.
    member this.RegisterComponentName<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.TryAdd (componentName, typeof<'c>) |> ignore<bool>

    member this.RegisterTerm (termName : string) term (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        if (match term with Intra _ -> true | _ -> false) then failwith "Intra components are for internal use only."
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            let archetypeId = entitySlot.Archetype.Id.AddTerm termName term
            this.RegisterEntityInternal comps archetypeId entity
        | (false, _) ->
            let archetypeId = ArchetypeId.make (Map.singleton termName term)
            let comps = dictPlus StringComparer.Ordinal []
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            this.RegisterEntityInternal comps archetypeId entity

    member this.UnregisterTerm (termName : string) (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            let archetypeId = entitySlot.Archetype.Id.RemoveTerm termName
            if archetypeId.Terms.Count > 0 then this.RegisterEntityInternal comps archetypeId entity
        | (false, _) -> ()

    member this.RegisterComponentPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        compName (comp : 'c) (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            comps.Add (compName, comp)
            let archetypeId = entitySlot.Archetype.Id.AddTerm (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (world :> obj) :?> 'w
        | (false, _) ->
            let archetypeId = ArchetypeId (Map.singleton (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>)))
            let comps = Dictionary.singleton StringComparer.Ordinal compName (comp :> obj)
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (world :> obj) :?> 'w

    member this.RegisterComponent<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        (comp : 'c) (entity : EcsEntity) (world : 'w) =
        this.RegisterComponentPlus<'c, 'w> (typeof<'c>.Name) comp (entity : EcsEntity) world

    member this.UnregisterComponentPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        compName (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let eventData = { EcsEntity = entity; ComponentName = compName }
            let world = this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
            let comps = this.UnregisterEntityInternal entitySlot entity
            let archetypeId = entitySlot.Archetype.Id.RemoveTerm (Constants.Ecs.IntraComponentPrefix + compName)
            if archetypeId.Terms.Count > 0 then
                comps.Remove compName |> ignore<bool>
                this.RegisterEntityInternal comps archetypeId entity
                world
            else world
        | (false, _) -> world

    member this.UnregisterComponent<'c, 'w when
        'c : struct and 'c :> 'c Component and 'w : not struct> (entity : EcsEntity) (world : 'w) =
        this.UnregisterComponentPlus<'c, 'w> typeof<'c>.Name entity world

    member this.RegisterEntity elideEvents comps archetypeId world =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let entity = this.MakeEntity ()
        let archetypeIndex =
            lock archetype $ fun () ->
                archetype.Register comps entity.EntityId
        entitySlots.TryAdd (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype }) |> ignore<bool>
        let mutable world = world
        if not elideEvents then
            for compName in archetype.Stores.Keys do
                let eventData = { EcsEntity = entity; ComponentName = compName }
                world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
        (entity, world)

    member this.UnregisterEntity (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let archetype = entitySlot.Archetype
            let mutable world = world
            if subscribedEntities.ContainsKey entity then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
            lock archetype $ fun () ->
                archetype.Unregister entitySlot.ArchetypeIndex
            world
        | (false, _) -> world

    member internal this.RegisterQuery (query : Query) =
        for archetypeEntry in archetypes do
            query.TryRegisterArchetype archetypeEntry.Value
        queries.Add query

    member this.RegisterEntitiesPlus elideEvents count comps archetypeId (world : 'w) =

        // get archetype
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId

        // register entities to archetype
        let mutable world = world
        let entities = SArray.zeroCreate count
        for i in 0 .. dec count do
            let entity = this.MakeEntity ()
            let archetypeIndex =
                lock archetype $ fun () ->
                    archetype.Register comps entity.EntityId
            entitySlots.TryAdd (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype }) |> ignore<bool>
            entities.[i] <- entity
            if not elideEvents then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w

        // fin
        (entities, world)

    member this.RegisterEntities elideEvents count comps archetypeId world =
        let comps = dictPlus StringComparer.Ordinal (Seq.map (fun comp -> (getTypeName comp, comp)) comps)
        this.RegisterEntitiesPlus elideEvents count comps archetypeId world

    member this.ReadEntities count archetypeId stream =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let (firstIndex, lastIndex) = archetype.Read count stream
        let entities = SArray.zeroCreate count
        for i in firstIndex .. lastIndex do
            let entity = this.MakeEntity ()
            entitySlots.TryAdd (entity.EntityId, { ArchetypeIndex = i; Archetype = archetype }) |> ignore<bool>
            entities.[i - firstIndex] <- entity
        entities

/// An entity's slot in an archetype.
and [<StructuralEquality; NoComparison; Struct>] EcsEntitySlot =
    { ArchetypeIndex : int
      Archetype : Archetype }

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.ToEntityId ecs =
        { EntityId = this.Archetype.EntityIdStore.[this.ArchetypeIndex].EntityId; Ecs = ecs }

    member this.ValidatePlus compName =
        let stores = this.Archetype.Stores
        stores.ContainsKey compName

    member this.Validate<'c when 'c : struct and 'c :> 'c Component> () =
        this.ValidatePlus typeof<'c>.Name

    member this.ValidateTerm termName =
        let terms = this.Archetype.Id.Terms
        terms.ContainsKey termName

    member this.IndexPlus<'c when 'c : struct and 'c :> 'c Component> compName =
        let stores = this.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = this.ArchetypeIndex
        &store.[i]

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        this.IndexPlus<'c> typeof<'c>.Name

    member this.IndexTerm termName =
        let terms = this.Archetype.Id.Terms
        terms.[termName]

    member this.MutatePlus<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) =
        let stores = this.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = this.ArchetypeIndex
        store.[i] <- comp

    member this.Mutate<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) =
        this.MutatePlus<'c> typeof<'c>.Name comp

    member this.Frame (statement : Statement<'c, 's>, ?compName, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke (&store.[i], Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
        let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
        let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
        let store9 = this.IndexStore<'c9> (Option.defaultValue typeof<'c9>.Name comp9Name) archetypeId stores
        let i = this.ArchetypeIndex
        if not archetype.EntityIdStore.[i].Active then failwith "Invalid component access."
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i],
             Option.defaultValue Unchecked.defaultof<'s> state)

and [<StructuralEquality; NoComparison; Struct>] EcsEntity =
    { EntityId : uint64
      Ecs : Ecs }

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.ToEntitySlot () =
        this.Ecs.IndexEntitySlot this

    member this.RegisterPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (comp : 'c) (world : 'w) =
        this.Ecs.RegisterComponentPlus<'c, 'w> compName comp this world

    member this.Register<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (comp : 'c) (world : 'w) =
        this.Ecs.RegisterComponent<'c, 'w> comp this world

    member this.UnregisterPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (world : 'w) =
        this.Ecs.UnregisterComponentPlus<'c, 'w> compName this world

    member this.Unregister<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (world : 'w) =
        this.Ecs.UnregisterComponent<'c, 'w> this world

    member this.RegisterTerm termName term =
        this.Ecs.RegisterTerm termName term this

    member this.UnregisterTerm termName =
        this.Ecs.UnregisterTerm termName this

    member this.ValidatePlus compName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.ValidatePlus compName

    member this.Validate<'c when 'c : struct and 'c :> 'c Component> () =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Validate<'c> ()

    member this.ValidateTerm termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.ValidateTerm termName

    member this.IndexPlus<'c when 'c : struct and 'c :> 'c Component> compName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.IndexPlus<'c> compName

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Index<'c> ()

    member this.IndexTerm termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.IndexTerm termName

    member this.UpdateTerm updater termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        let terms = entitySlot.Archetype.Id.Terms
        let term = updater terms.[termName]
        this.UnregisterTerm termName
        this.RegisterTerm termName term

    member this.MutatePlus<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.MutatePlus<'c> compName comp

    member this.Mutate<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Mutate<'c> comp

    member this.ChangePlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (comp : 'c) (world : 'w) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        let stores = entitySlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = entitySlot.ArchetypeIndex
        store.[i] <- comp
        this.Ecs.Publish (EcsEvents.Change this) { EcsEntity = this; ComponentName = compName } world

    member this.Change<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (comp : 'c) (world : 'w) =
        this.ChangePlus<'c, 'w> typeof<'c>.Name comp world

    member this.Frame
        (statement : Statement<'c, 's>,
         ?compName, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null comp5Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null comp5Name,
             Option.defaultValue null comp6Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null comp5Name,
             Option.defaultValue null comp6Name,
             Option.defaultValue null comp7Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null comp5Name,
             Option.defaultValue null comp6Name,
             Option.defaultValue null comp7Name,
             Option.defaultValue null comp8Name,
             Option.defaultValue null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.defaultValue null compName,
             Option.defaultValue null comp2Name,
             Option.defaultValue null comp3Name,
             Option.defaultValue null comp4Name,
             Option.defaultValue null comp5Name,
             Option.defaultValue null comp6Name,
             Option.defaultValue null comp7Name,
             Option.defaultValue null comp8Name,
             Option.defaultValue null comp9Name,
             Option.defaultValue null state)

and Query (compNames : string HashSet, subqueries : Subquery seq, ecs : Ecs) as this =

    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let subqueries = List subqueries

    do
        for compName in compNames do
            subqueries.Add (Tagged (Var (Constants.Ecs.IntraComponentPrefix + compName)))
        ecs.RegisterQuery this

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member private this.ThreadTasks (tasks : (int * (unit -> unit)) List) =
        let largeTasks = Seq.filter (fst >> (>) Constants.Ecs.ParallelTaskSizeMinimum) tasks
        if Seq.length largeTasks > 2 then
            let result = Parallel.ForEach (tasks, fun (_, task) -> task ())
            ignore<ParallelLoopResult> result
        else for (_, task) in tasks do task ()

    member this.Archetypes : IReadOnlyDictionary<ArchetypeId, Archetype> =
        archetypes :> _

    member this.Subqueries =
        seq subqueries

    member internal this.TryRegisterArchetype (archetype : Archetype) =
        if  not (archetypes.ContainsKey archetype.Id) &&
            Subquery.evalMany archetype.Id.Terms subqueries then
            archetypes.Add (archetype.Id, archetype)

    member this.IndexEntitySlots () =
        let slots = SList.make ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            for i in 0 .. dec archetype.Length do
                let entityId = archetype.EntityIdStore.[i]
                if entityId.Active then
                    slots.Add { ArchetypeIndex = i; Archetype = archetype }
        slots

    member this.IndexEntities () =
        let entities = SList.make ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            for i in 0 .. dec archetype.Length do
                let entityId = archetype.EntityIdStore.[i]
                if entityId.Active then
                    entities.Add { EntityId = entityId.EntityId; Ecs = ecs }
        entities

    member this.Iterate (statement : Statement<'c, 's>, ?compName, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)
                    i <- inc i
        state

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) : 's =
        let mutable state = Option.defaultValue Unchecked.defaultof<'s> state
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
            let store9 = this.IndexStore<'c9> (Option.defaultValue typeof<'c9>.Name comp9Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                if entityIdStore.[i].Active then
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)
                    i <- inc i
        state

    member this.IterateParallel
        (statement : StatementPlus<'c, 's>,
         ?compName, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.IterateParallel
        (statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) =
        let state = Option.defaultValue Unchecked.defaultof<'s> state
        let tasks = List ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let entityIdStore = archetype.EntityIdStore
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.defaultValue typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.defaultValue typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.defaultValue typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.defaultValue typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.defaultValue typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.defaultValue typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.defaultValue typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.defaultValue typeof<'c8>.Name comp8Name) archetypeId stores
            let store9 = this.IndexStore<'c9> (Option.defaultValue typeof<'c9>.Name comp9Name) archetypeId stores
            tasks.Add (Pair.make entityIdStore.Length (fun () ->
                lock archetype $ fun () ->
                    let mutable i = 0
                    while i < store.Length && i < length do
                        if entityIdStore.[i].Active then
                            statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)
                            i <- inc i))
        this.ThreadTasks tasks

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'w>,
         ?compName) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'w>,
         ?compName, ?comp2Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'w>,
         ?compName, ?comp2Name, ?comp3Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     Option.defaultValue typeof<'c8>.Name comp8Name,
                     world)
        ecs.Subscribe event callback

    member this.SubscribeIteration
        (event : EcsEvent,
         statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name) =
        let callback =
            fun _ _ world ->
                this.Iterate
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     Option.defaultValue typeof<'c8>.Name comp8Name,
                     Option.defaultValue typeof<'c9>.Name comp9Name,
                     world)
        ecs.Subscribe event callback

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'w>,
         ?compName) =
        let callback =
            fun _ _ (world : 'w) ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'w>,
         ?compName, ?comp2Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'w>,
         ?compName, ?comp2Name, ?comp3Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     Option.defaultValue typeof<'c8>.Name comp8Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    member this.ScheduleIteration
        (event : EcsEvent,
         dependencies : Query list,
         statement : StatementPlus<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name) =
        let callback =
            fun _ _ world ->
                this.IterateParallel
                    (statement,
                     Option.defaultValue typeof<'c>.Name compName,
                     Option.defaultValue typeof<'c2>.Name comp2Name,
                     Option.defaultValue typeof<'c3>.Name comp3Name,
                     Option.defaultValue typeof<'c4>.Name comp4Name,
                     Option.defaultValue typeof<'c5>.Name comp5Name,
                     Option.defaultValue typeof<'c6>.Name comp6Name,
                     Option.defaultValue typeof<'c7>.Name comp7Name,
                     Option.defaultValue typeof<'c8>.Name comp8Name,
                     Option.defaultValue typeof<'c9>.Name comp9Name,
                     world)
        ecs.Schedule event { EcsQuery = this; EcsDependencies = dependencies; EcsCallback = callback }

    static member make
        (ecs, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural [],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        (ecs, ?compName, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (ecs, ?compName, ?comp2Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name
                 Option.defaultValue typeof<'c5>.Name comp5Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name
                 Option.defaultValue typeof<'c5>.Name comp5Name
                 Option.defaultValue typeof<'c6>.Name comp6Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name
                 Option.defaultValue typeof<'c5>.Name comp5Name
                 Option.defaultValue typeof<'c6>.Name comp6Name
                 Option.defaultValue typeof<'c7>.Name comp7Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name
                 Option.defaultValue typeof<'c5>.Name comp5Name
                 Option.defaultValue typeof<'c6>.Name comp6Name
                 Option.defaultValue typeof<'c7>.Name comp7Name
                 Option.defaultValue typeof<'c8>.Name comp8Name],
             Option.defaultValue [] subqueries,
             ecs)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (ecs, ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.defaultValue typeof<'c>.Name compName
                 Option.defaultValue typeof<'c2>.Name comp2Name
                 Option.defaultValue typeof<'c3>.Name comp3Name
                 Option.defaultValue typeof<'c4>.Name comp4Name
                 Option.defaultValue typeof<'c5>.Name comp5Name
                 Option.defaultValue typeof<'c6>.Name comp6Name
                 Option.defaultValue typeof<'c7>.Name comp7Name
                 Option.defaultValue typeof<'c8>.Name comp8Name
                 Option.defaultValue typeof<'c9>.Name comp9Name],
             Option.defaultValue [] subqueries,
             ecs)

[<AutoOpen>]
module Query =

    [<AutoOpen>]
    module Ops =

        let Independent = [] : Query list