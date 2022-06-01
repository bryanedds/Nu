namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading.Tasks
open Prime

// TODO: 3D: cache empty sets and dicts.
// TODO: 3D: remove any incidental allocation.
// TODO: 3D: make sure to use proper collection comparer for string keys.
// TODO: 3D: make sure to use TryAdd in the appropriate places.

type [<StructuralEquality; NoComparison>] Term =
    | Z of int
    | R of single
    | C of IComparable
    | Obj of obj
    | Tag
    | Label of string
    | Labels of string HashSet
    | Entity of uint64
    | Terms of Term list
    static member equals (this : Term) (that : Term) = this.Equals that
    static member equalsMany (lefts : Dictionary<string, Term>) (rights : Dictionary<string, Term>) =
        if lefts.Count = rights.Count then
            let mutable result = true
            let mutable enr = lefts.GetEnumerator ()
            while result && enr.MoveNext () do
                let current = enr.Current
                let termName = current.Key
                match rights.TryGetValue termName with
                | (true, term) -> result <- Term.equals current.Value term
                | (false, _) -> result <- false
            result
        else false
    static member dict entries = dictPlus<string, Term> HashIdentity.Structural entries
    static member singleton termName term = Term.dict [(termName, term)]

type [<StructuralEquality; NoComparison>] Subquery =
    | Is
    | Has of string
    | Eq of Term
    | Gt of Term
    | Ge of Term
    | Lt of Term
    | Le of Term
    | Not of Subquery
    | And of Subquery list
    | Or of Subquery list

    static member private equalTo term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z = z2
        | (R r, R r2) -> r = r2
        | (C c, C c2) -> c = c2
        | (Obj o, Obj o2) -> objEq o o2
        | (Label label, Label label2) -> strEq label label2
        | (Labels labels, Labels labels2) -> labels.SetEquals labels2
        | (Entity entityId, Entity entityId2) -> entityId = entityId2
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.equalTo terms terms2
            else false
        | _ -> false

    static member eval term subquery =
        match subquery with
        | Is ->
            true
        | Has label ->
            match term with
            | Labels labels -> labels.Contains label
            | _ -> false
        | Eq term2  ->
            Subquery.equalTo term term2
        | Gt term2 ->
            match (term, term2) with
            | (Z z, Z z2) -> z > z2
            | (R r, R r2) -> r > r2
            | (C c, C c2) -> c.CompareTo c2 > 0
            | (_, _) -> false
        | Ge term2 ->
            match (term, term2) with
            | (Z z, Z z2) -> z >= z2
            | (R r, R r2) -> r >= r2
            | (C c, C c2) -> c.CompareTo c2 >= 0
            | (_, _) -> false
        | Lt term2 ->
            match (term, term2) with
            | (Z z, Z z2) -> z < z2
            | (R r, R r2) -> r < r2
            | (C c, C c2) -> c.CompareTo c2 < 0
            | (_, _) -> false
        | Le term2 ->
            match (term, term2) with
            | (Z z, Z z2) -> z <= z2
            | (R r, R r2) -> r <= r2
            | (C c, C c2) -> c.CompareTo c2 <= 0
            | (_, _) -> false
        | Not subquery ->
            not (Subquery.eval term subquery)
        | And subqueries ->
            match term with
            | Terms terms -> if terms.Length = subqueries.Length then List.forall2 Subquery.eval terms subqueries else false
            | _ -> false
        | Or subqueries ->
            match term with
            | Terms terms -> if terms.Length = subqueries.Length then List.exists2 Subquery.eval terms subqueries else false
            | _ -> false

    static member evalMany (terms : Dictionary<string, Term>) (subqueries : Dictionary<string, Subquery>) =
        let mutable result = true
        let mutable termEnr = terms.GetEnumerator ()
        while result && termEnr.MoveNext () do
            let termEntry = termEnr.Current
            match subqueries.TryGetValue termEntry.Key with
            | (true, subquery) -> result <- Subquery.eval termEntry.Value subquery
            | (false, _) -> result <- false
        result

    static member dict entries = dictPlus<string, Subquery> HashIdentity.Structural entries
    static member singleton subqueryName subquery = Term.dict [(subqueryName, subquery)]

/// Identifies an archetype.
/// TODO: consider embedding hash code to make look-ups faster.
type [<CustomEquality; NoComparison>] ArchetypeId =
    { ComponentNames : string HashSet
      Terms : Dictionary<string, Term> }

    static member inline Hash (hashSet : _ HashSet) =
        let mutable h = 0
        for item in hashSet do
            h <- h ^^^ item.GetHashCode ()
        h

    static member inline Hash (dict : Dictionary<_, _>) =
        let mutable h = 0
        for item in dict do
            h <- h ^^^ item.Key.GetHashCode () ^^^ item.Value.GetHashCode ()
        h

    static member equals left right =
        left.ComponentNames.SetEquals right.ComponentNames &&
        Term.equalsMany left.Terms right.Terms

    override this.GetHashCode () =
        let h = ArchetypeId.Hash this.ComponentNames
        h ^^^ ArchetypeId.Hash this.Terms

    override this.Equals that =
        match that with
        | :? ArchetypeId as that -> ArchetypeId.equals this that
        | _ -> failwithumf ()

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract Active : bool with get, set
        end

/// The component that holds an entity's id.
type [<NoEquality; NoComparison; Struct>] EntityId =
    { mutable Active : bool
      mutable EntityId : uint64 }
    interface EntityId Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

/// Data for an Ecs registration event.
type [<NoEquality; NoComparison; Struct>] RegistrationData =
    { EntityId : uint64
      ContextName : string }

/// Describes a means to store components.
type Store =
    interface
        abstract Length : int
        abstract Name : string
        abstract Item : int -> obj
        abstract SetItem : int -> obj -> unit
        abstract ZeroItem : int -> unit
        abstract Grow : unit -> unit
        abstract Read : int -> int -> FileStream -> unit
        end

/// Stores components.
type Store<'c when 'c : struct and 'c :> 'c Component> (name) =
    let mutable arr = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve
    member this.Length = arr.Length
    member this.Name = name
    member this.Item i = &arr.[i]
    member this.SetItem index comp = arr.[index] <- comp
    member this.ZeroItem index = arr.[index] <- Unchecked.defaultof<'c>
    member this.Grow () =
        let length = int (single (max arr.Length 2) * 1.5f)
        let arr' = Array.zeroCreate<'c> length
        Array.Copy (arr, arr', arr.Length)
        arr <- arr'
    member this.Read index count (stream : FileStream) =
        let compSize = sizeof<'c>
        let comp = Unchecked.defaultof<'c> :> obj
        let buffer = Array.zeroCreate<byte> compSize
        let gch = GCHandle.Alloc (comp, GCHandleType.Pinned)
        try 
            let mutable index = index
            for _ in 0 .. dec count do
                stream.Read (buffer, 0, compSize) |> ignore<int>
                Marshal.Copy (buffer, 0, gch.AddrOfPinnedObject (), compSize)
                if index = arr.Length then this.Grow ()
                arr.[index] <- comp :?> 'c
                index <- inc index
        finally gch.Free ()
    interface Store with
        member this.Length = this.Length
        member this.Name = this.Name
        member this.Item i = this.Item i :> obj
        member this.SetItem index compObj = this.SetItem index (compObj :?> 'c)
        member this.ZeroItem index = this.ZeroItem index
        member this.Grow () = this.Grow ()
        member this.Read index count stream = this.Read index count stream

/// Describes a means to query components.
type 'w Query =
    interface
        abstract Subqueries : Dictionary<string, Subquery>
        abstract CheckCompatibility : 'w Archetype -> bool
        abstract RegisterArchetype : 'w Archetype -> unit
        end

/// A collection of component stores.
and 'w Archetype (storeTypes : Dictionary<string, Type>, terms : Dictionary<string, Term>) =

    let mutable freeIndex = 0
    let freeList = hashSetPlus<int> HashIdentity.Structural []
    let stores = dictPlus<string, Store> HashIdentity.Structural []
    let id = { ComponentNames = hashSetPlus HashIdentity.Structural storeTypes.Keys; Terms = terms }

    do
        let storeTypeGeneric = typedefof<EntityId Store>
        for storeTypeEntry in storeTypes do
            let storeType = storeTypeGeneric.MakeGenericType [|storeTypeEntry.Value|]
            let store = Activator.CreateInstance (storeType, storeTypeEntry.Key) :?> Store
            stores.Add (storeTypeEntry.Key, store)

    member this.Id = id
    member this.Stores = stores
    member this.ComponentNames = hashSetPlus HashIdentity.Structural stores.Keys
    member this.Terms = terms

    member this.Register (comps : Dictionary<string, obj>) =
        if freeList.Count > 0 then
            let index = Seq.head freeList
            freeList.Remove index |> ignore<bool>
            for compEntry in comps do
                stores.[compEntry.Key].SetItem index compEntry.Value
            index
        else
            match Seq.tryHead stores with
            | Some headStoreEntry ->
                let index = freeIndex
                if index = headStoreEntry.Value.Length then
                    for storeEntry in stores do
                        storeEntry.Value.Grow ()
                for compEntry in comps do
                    stores.[compEntry.Key].SetItem index compEntry.Value
                freeIndex <- inc freeIndex
                index
            | None ->
                let index = freeIndex
                freeIndex <- inc freeIndex
                index

    member this.Unregister (index : int) =
        for storeEntry in stores do
            storeEntry.Value.ZeroItem index
        if index = dec freeIndex then
            freeIndex <- dec freeIndex
        else
            freeList.Add index |> ignore<bool>

    member this.GetComponents index =
        let comps = dictPlus<string, obj> HashIdentity.Structural []
        for storeEntry in stores do
            comps.Add (storeEntry.Key, storeEntry.Value.[index])
        comps

    member this.Read count (stream : FileStream) =
        for storeEntry in stores do
            let store = storeEntry.Value
            store.Read count freeIndex stream
        freeIndex <- freeIndex + count

/// An entity's place in an archetype.
type [<StructuralEquality; NoComparison; Struct>] 'w ArchetypeSlot =
    { ArchetypeIndex : int
      Archetype : 'w Archetype }

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"
    let [<Literal>] Register = "Register"
    let [<Literal>] Unregistering = "Unregistering"

/// An ECS event.
type [<NoEquality; NoComparison>] EcsEvent<'d, 'w> =
    { EcsEventData : 'd }

/// An ECS event callback.
type EcsCallback<'d, 'w> =
    EcsEvent<'d, 'w> -> 'w Ecs -> 'w -> 'w

/// A boxed ECS event callback.
and EcsCallbackBoxed<'w> =
    EcsEvent<obj, 'w> -> 'w Ecs -> 'w -> 'w

/// An archetype-based ECS construct.
and 'w Ecs () =

    let mutable subscriptionIdCurrent = 0u
    let mutable entityIdCurrent = 0UL
    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []
    let archetypeSlots = dictPlus<uint64, 'w ArchetypeSlot> HashIdentity.Structural []
    let componentTypes = dictPlus<string, Type> HashIdentity.Structural []
    let subscriptions = dictPlus<string, Dictionary<uint32, obj>> StringComparer.Ordinal []
    let queries = List<'w Query> ()

    let createArchetype (inferredType : Type) (archetypeId : ArchetypeId) =
        let storeTypes =
            archetypeId.ComponentNames |>
            Seq.map (fun componentName ->
                match componentTypes.TryGetValue componentName with
                | (true, componentType) -> (componentName, componentType)
                | (false, _) ->
                    if inferredType.Name = componentName
                    then componentTypes.Add (componentName, inferredType)
                    else failwith ("Could not infer component type of '" + componentName + "'.")
                    (componentName, inferredType)) |>
            dictPlus HashIdentity.Structural
        let archetype = Archetype<'w> (storeTypes, archetypeId.Terms)
        archetypes.Add (archetypeId, archetype)
        for query in queries do
            if query.CheckCompatibility archetype then
                query.RegisterArchetype archetype
        archetype

    member private this.SubscriptionId =
        subscriptionIdCurrent <- inc subscriptionIdCurrent
        if subscriptionIdCurrent = UInt32.MaxValue then failwith "Unbounded use of ECS subscription ids not supported."
        subscriptionIdCurrent

    member private this.BoxCallback<'d> (callback : EcsCallback<'d, 'w>) =
        let boxableCallback = fun (evt : EcsEvent<obj, 'w>) store ->
            let evt = { EcsEventData = evt.EcsEventData :?> 'd }
            callback evt store
        boxableCallback :> obj

    member this.EntityId =
        entityIdCurrent <- inc entityIdCurrent
        if entityIdCurrent = UInt64.MaxValue then failwith "Unbounded use of ECS entity ids not supported."
        entityIdCurrent

    member this.Publish<'d> eventName (eventData : 'd) world =
        match subscriptions.TryGetValue eventName with
        | (false, _) -> world
        | (true, callbacks) ->
            Seq.fold (fun world (callback : obj) ->
                match callback with
                | :? EcsCallback<obj, 'w> as objCallback ->
                    let evt = { EcsEventData = eventData :> obj }
                    objCallback evt this world
                | _ -> failwithumf ())
                world callbacks.Values

    member this.PublishAsync<'d> eventName (eventData : 'd) =
        let vsync =
            match subscriptions.TryGetValue eventName with
            | (true, callbacks) ->
                callbacks |>
                Seq.map (fun subscription ->
                    Task.Run (fun () ->
                        match subscription.Value with
                        | :? EcsCallback<obj, 'w> as objCallback ->
                            let evt = { EcsEventData = eventData :> obj }
                            objCallback evt this Unchecked.defaultof<'w> |> ignore<'w>
                        | _ -> failwithumf ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel
            | (false, _) -> Vsync.Parallel []
        Vsync.StartAsTask vsync

    member this.RegisterComponentType<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.Add (componentName, typeof<'c>)

    member this.RegisterTerm termName term entityId world =
        match archetypeSlots.TryGetValue entityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
            archetype.Unregister archetypeSlot.ArchetypeIndex
            archetypeSlots.Remove entityId |> ignore<bool>
            let archetypeId =
                { ComponentNames = archetype.Id.ComponentNames
                  Terms = Dictionary<_, _> archetype.Id.Terms }
            archetypeId.Terms.Add (termName, term)
            let world =
                match archetypes.TryGetValue archetypeId with
                | (true, archetype) ->
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
                | (false, _) ->
                    let archetype = createArchetype typeof<unit> archetypeId
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
            let eventData = { EntityId = entityId; ContextName = termName }
            this.Publish EcsEvents.Register eventData world
        | (false, _) ->
            let archetypeId =
                { ComponentNames = hashSetPlus HashIdentity.Structural []
                  Terms = Dictionary.singleton HashIdentity.Structural termName term }
            let comps = dictPlus HashIdentity.Structural [] // TODO: use cached empty dictionary.
            let world =
                match archetypes.TryGetValue archetypeId with
                | (true, archetype) ->
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
                | (false, _) ->
                    let archetype = createArchetype typeof<unit> archetypeId
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
            let eventData = { EntityId = entityId; ContextName = termName }
            this.Publish EcsEvents.Register eventData world

    member this.UnregisterTag termName entityId world =
        match archetypeSlots.TryGetValue entityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
            let eventData = { EntityId = entityId; ContextName = termName }
            let world = this.Publish EcsEvents.Unregistering eventData world
            archetype.Unregister archetypeSlot.ArchetypeIndex
            archetypeSlots.Remove entityId |> ignore<bool>
            let archetypeId =
                { ComponentNames = archetype.Id.ComponentNames
                  Terms = Dictionary<_, _> archetype.Id.Terms }
            archetypeId.Terms.Remove termName |> ignore<bool>
            if archetypeId.ComponentNames.Count > 0 || archetypeId.Terms.Count > 0 then
                let archetypeIndex = archetype.Register comps
                archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                world
            else world
        | (false, _) -> world

    member this.RegisterNamedComponent<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) entityId world =
        match archetypeSlots.TryGetValue entityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
            archetype.Unregister archetypeSlot.ArchetypeIndex
            archetypeSlots.Remove entityId |> ignore<bool>
            comps.Add (compName, comp)
            let archetypeId =
                { ComponentNames = hashSetPlus HashIdentity.Structural archetype.Id.ComponentNames
                  Terms = archetype.Id.Terms }
            archetypeId.ComponentNames.Add compName |> ignore<bool>
            let world =
                match archetypes.TryGetValue archetypeId with
                | (true, archetype) ->
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
                | (false, _) ->
                    let archetype = createArchetype typeof<'c> archetypeId
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
            let eventData = { EntityId = entityId; ContextName = compName }
            this.Publish EcsEvents.Register eventData world
        | (false, _) ->
            let archetypeId =
                { ComponentNames = HashSet.singleton HashIdentity.Structural compName
                  Terms = dictPlus HashIdentity.Structural [] }
            let comps = Dictionary.singleton HashIdentity.Structural compName (comp :> obj)
            let world =
                match archetypes.TryGetValue archetypeId with
                | (true, archetype) ->
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
                | (false, _) ->
                    let archetype = createArchetype typeof<'c> archetypeId
                    let archetypeIndex = archetype.Register comps
                    archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                    world
            let eventData = { EntityId = entityId; ContextName = compName }
            this.Publish EcsEvents.Register eventData world

    member this.RegisterComponent<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) entityId world =
        let compName = typeof<'c>.Name
        this.RegisterNamedComponent<'c> compName comp entityId world

    member this.UnregisterNamedComponent compName entityId world =
        match archetypeSlots.TryGetValue entityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
            let eventData = { EntityId = entityId; ContextName = compName }
            let world = this.Publish EcsEvents.Unregistering eventData world
            archetype.Unregister archetypeSlot.ArchetypeIndex
            archetypeSlots.Remove entityId |> ignore<bool>
            comps.Remove compName |> ignore<bool>
            let archetypeId =
                { ComponentNames = hashSetPlus HashIdentity.Structural archetype.Id.ComponentNames
                  Terms = archetype.Id.Terms }
            archetypeId.ComponentNames.Remove compName |> ignore<bool>
            if archetypeId.ComponentNames.Count > 0 || archetypeId.Terms.Count > 0 then
                let archetypeIndex = archetype.Register comps
                archetypeSlots.Add (entityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
                world
            else world
        | (false, _) -> world

    member this.UnregisterComponent<'c> entityId world =
        this.UnregisterNamedComponent typeof<'c>.Name entityId world

    member this.UnregisterEntity entityId world =
        match archetypeSlots.TryGetValue entityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let mutable world = world
            for compName in archetype.Stores.Keys do
                let eventData = { EntityId = entityId; ContextName = compName }
                world <- this.Publish EcsEvents.Unregistering eventData world
            for termEntry in archetype.Terms do
                let eventData = { EntityId = entityId; ContextName = termEntry.Key }
                world <- this.Publish EcsEvents.Unregistering eventData world
            archetype.Unregister archetypeSlot.ArchetypeIndex
            world
        | (false, _) -> world

    member this.RegisterQuery<'q when 'q :> 'w Query> (query : 'q) =
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            if query.CheckCompatibility archetype then
                query.RegisterArchetype archetype
        queries.Add query
        query

    member this.SubscribePlus<'d> subscriptionId eventName (callback : EcsCallback<'d, 'w>) =
        match subscriptions.TryGetValue eventName with
        | (true, callbacks) ->
            callbacks.Add (subscriptionId, this.BoxCallback<'d> callback)
            subscriptionId
        | (false, _) ->
            let callbacks = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d> callback)]
            subscriptions.Add (eventName, callbacks)
            subscriptionId

    member this.Subscribe<'d> eventName callback =
        this.SubscribePlus<'d> this.SubscriptionId eventName callback |> ignore

    member this.Unsubscribe eventName subscriptionId =
        match subscriptions.TryGetValue eventName with
        | (true, callbacks) -> callbacks.Remove subscriptionId
        | (false, _) -> false

    member this.IndexArchetypeSlot entityId =
        archetypeSlots.[entityId]

    member this.ReadComponents count archetypeId stream =
        match archetypes.TryGetValue archetypeId with
        | (true, archetype) -> archetype.Read count stream
        | (false, _) -> failwith "Could not find archetype."