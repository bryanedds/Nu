// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Prime

/// An array with additional indirection.
/// NOTE: Inlined everything for speed.
/// TODO: move to Prime?
type 'c ArrayRef =
    { mutable Array : 'c array }
    member inline this.Length = this.Array.Length
    member inline this.Item i = &this.Array.[i]
    static member inline make n = { Array = Array.zeroCreate n }

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract TypeName : string
        abstract Active : bool with get, set
        end

/// The component that holds an entity's id.
type [<NoEquality; NoComparison; Struct>] EntityId =
    { mutable Active : bool
      mutable EntityId : uint64 }
    interface EntityId Component with
        member this.TypeName = nameof EntityId
        member this.Active with get () = this.Active and set value = this.Active <- value

/// A storable reference to a component in its containing array.
/// DO NOT access the elements of ComponentArefBuffered without locking the field itself!
/// OPTIMIZATION: Inlined everything for speed.
/// TODO: test threaded / buffered ECS functionality.
and [<NoEquality; NoComparison; Struct>] ComponentRef<'c when 'c : struct and 'c :> 'c Component> =
    {
      /// The associated component's index.
      ComponentIndex : int
      /// The associated component array reference.
      ComponentAref : 'c ArrayRef
      /// DO NOT access the elements of ComponentArefBuffered without locking the field itself!
#if ECS_BUFFERED
      ComponentArefBuffered : 'c ArrayRef
#endif
    }

    member inline this.Index =
        &this.ComponentAref.[this.ComponentIndex]

    member inline this.IndexBuffered =
#if ECS_BUFFERED
        let index = this.ComponentIndex
        let arefBuffered = this.ComponentArefBuffered
        lock arefBuffered (fun () -> arefBuffered.[index])
#else
        this.Index
#endif

    member inline this.Assign value =
        this.ComponentAref.[this.ComponentIndex] <- value

    member inline this.Assign (value : 'c inref) =
        this.ComponentAref.[this.ComponentIndex] <- value

    static member inline (<!) (componentRef, value) =
        componentRef.ComponentAref.Array.[componentRef.ComponentIndex] <- value
        
    static member inline (<!) (componentRef, value : 'c inref) =
        componentRef.ComponentAref.Array.[componentRef.ComponentIndex] <- value

    static member inline make index aref arefBuffered : 'c ComponentRef =
#if !ECS_BUFFERED
        ignore<'c ArrayRef> arefBuffered
#endif
        {
          ComponentIndex = index
          ComponentAref = aref
#if ECS_BUFFERED          
          ComponentArefBuffered = arefBuffered
#endif
        }

/// An ECS event.
and [<NoEquality; NoComparison>] StoreEvent<'d, 'w> =
    { StoreEventData : 'd
      StorePublisher : 'w Store }

/// An ECS event callback.
and StoreCallback<'d, 'w> =
    StoreEvent<'d, 'w> -> 'w Store -> 'w Ecs -> 'w -> 'w

/// A boxed ECS event callback.
and StoreCallbackBoxed<'w> =
    StoreEvent<obj, 'w> -> 'w Store -> 'w Ecs -> 'w -> 'w

/// A base type that represents a means of storing components in an ECS.
and [<AbstractClass>] 'w Store (name) =
    member this.Name : string = name
    member this.Id : uint = Gen.id32
    abstract UnregisterComponent : uint64 -> 'w -> struct (bool * 'w)

/// Potentially-buffered array objects.
/// ArrayObjsUnbuffered will be refEq to ArrayObjsBuffered if buffering is disabled.
and [<NoEquality; NoComparison>] internal ArrayObjs =
    { ArrayObjsUnbuffered : obj List
      mutable ArrayObjsBuffered : obj List }
    member this.Buffered =
        refEq
            this.ArrayObjsBuffered
            this.ArrayObjsUnbuffered
            
type SystemId = Type HashSet

type 'w Store' =
    interface
        abstract RegisterComponent : obj -> uint64 -> unit
        abstract UnregisterComponent : uint64 -> struct (Type * obj)
        abstract Components : obj
        end

/// An ECS system.
and 'w System =
    abstract Id : SystemId
    abstract Stores : Dictionary<Type, 'w Store'>
    abstract Correlation : string HashSet
    abstract Filter : uint64 -> unit

/// A collection of ECS systems.
and 'w Systems () =

    let systems = dictPlus<string, 'w System HashSet> StringComparer.Ordinal []

    member this.RegisterSystem (system : 'w System) =
        for storeName in system.Correlation do
            match systems.TryGetValue storeName with
            | (true, systemSet) -> systemSet.Add system |> ignore<bool>
            | (false, _) -> systems.Add (storeName, hashSetPlus<'w System> HashIdentity.Reference [system])

    member this.UnregisterSystem (system : 'w System) =
        for storeName in system.Correlation do
            match systems.TryGetValue storeName with
            | (true, systemSet) ->
                systemSet.Remove system |> ignore<bool>
                if systemSet.Count = 0 then
                    systems.Remove storeName |> ignore<bool>
            | (false, _) -> ()

    member this.Filter storeName entityId =
        match systems.TryGetValue storeName with
        | (true, systemSet) ->
            for system in systemSet do
                system.Filter entityId
        | (false, _) -> ()

/// Nu's custom Entity-Component-System implementation. It presents a dynamic, striped, archetype-based ECS. Users can
/// add new component storage formats by implementing the 'w Store interface and providing related extension methods
/// on this 'w Ecs type.
and 'w Ecs () as this =

    let mutable cachedStore = { new Store<'w> (typeof<unit>.Name) with member this.UnregisterComponent _ world = struct (false, world) }
    let globalStore = cachedStore
    let arrayObjss = dictPlus<string, ArrayObjs> StringComparer.Ordinal []
    let storeSubscriptions = dictPlus<string, Dictionary<uint32, obj>> StringComparer.Ordinal []
    let storesUnordered = dictPlus<string, 'w Store> StringComparer.Ordinal []
    let storesOrdered = List<string * 'w Store> ()
    let storesById = dictPlus<uint, 'w Store> HashIdentity.Structural [] // TODO: see if a quadratic searching dictionary could improve perf here.
    let correlations = dictPlus<uint64, uint HashSet> HashIdentity.Structural [] // NOTE: correlations are keyed by Store.Ids to keep their dictionary entry types unmanaged.
    let emptyStoreIds = hashSetPlus<uint> HashIdentity.Structural []
    let systems = Systems<'w> ()

    do this.RegisterStoreGeneralized globalStore |> ignore<'w Store>

    member private this.BoxCallback<'a> (callback : StoreCallback<'a, 'w>) =
        let boxableCallback = fun (evt : StoreEvent<obj, 'w>) store ->
            let evt =
                { StoreEventData = evt.StoreEventData :?> 'a
                  StorePublisher = evt.StorePublisher }
            callback evt store
        boxableCallback :> obj

    member internal this.Correlations = correlations
    member this.StoreGlobal = globalStore

    member this.RegisterSystem system =
        systems.RegisterSystem system
        system

    member this.UnregisterSystem system =
        systems.UnregisterSystem system
        system

    member this.FilterForSystems storeName entityId =
        systems.Filter storeName entityId

    member this.RegisterStoreGeneralized (store : 'w Store) : 'w Store =
        storesUnordered.Add (store.Name, store)
        storesOrdered.Add (store.Name, store)
        storesById.Add (store.Id, store)
        store

    member this.TryIndexStore storeName =
        if cachedStore.Name <> storeName then
            match storesUnordered.TryGetValue storeName with
            | (true, store) -> 
                cachedStore <- store
                Some store
            | (false, _) -> None
        else Some cachedStore

    member this.IndexStore storeName =
        if cachedStore.Name <> storeName then
            match storesUnordered.TryGetValue storeName with
            | (true, store) -> 
                cachedStore <- store
                store
            | (false, _) -> failwith ("Could not index store '" + storeName + "'.")
        else cachedStore

    member this.TryIndexStore<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w Store> () =
        let storeName = Unchecked.defaultof<'c>.TypeName
        match this.TryIndexStore storeName with
        | Some store ->
            match store with
            | :? 's as storeAsS -> Some storeAsS
            | _ -> None
        | None -> None

    member this.IndexStore<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w Store> () =
        let storeName = Unchecked.defaultof<'c>.TypeName
        this.IndexStore storeName :?> 's

    member this.IndexStoreIds entityId =
        match correlations.TryGetValue entityId with
        | (true, correlation) -> correlation
        | (false, _) -> emptyStoreIds

    member this.IndexStores entityId =
        this.IndexStoreIds entityId |>
        Seq.map (fun id -> storesById.[id])

    member this.IndexEntities correlation =
        correlations |>
        Seq.filter (fun kvp -> kvp.Value.IsSubsetOf correlation) |>
        Seq.map (fun kvp -> kvp.Key)

    member this.UnregisterComponents entityId world =
        let mutable unregistered = false
        let mutable world = world
        for store in this.IndexStores entityId do
            let struct (unregistered', world') = store.UnregisterComponent entityId world
            if unregistered' then unregistered <- true
            world <- world'
        struct (unregistered, world)

    member this.SubscribePlus<'d> subscriptionId eventName (callback : StoreCallback<'d, 'w>) =
        match storeSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            subscriptions.Add (subscriptionId, this.BoxCallback<'d> callback)
            subscriptionId
        | (false, _) ->
            let subscriptions = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d> callback)]
            storeSubscriptions.Add (eventName, subscriptions)
            subscriptionId

    member this.Subscribe<'d> eventName callback =
        this.SubscribePlus<'d> Gen.id32 eventName callback |> ignore

    member this.Unsubscribe eventName subscriptionId =
        match storeSubscriptions.TryGetValue eventName with
        | (true, subscriptions) -> subscriptions.Remove subscriptionId
        | (false, _) -> false

    member this.Publish<'d> eventName (eventData : 'd) publisher world =
        match storeSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            Seq.fold (fun world (callback : obj) ->
                match callback with
                | :? StoreCallback<obj, 'w> as objCallback ->
                    let evt = { StoreEventData = eventData :> obj; StorePublisher = publisher }
                    objCallback evt publisher this world
                | _ -> failwithumf ())
                world subscriptions.Values
        | (false, _) -> world

    member this.PublishAsync<'d> eventName (eventData : 'd) publisher =
        let vsync =
            match storeSubscriptions.TryGetValue eventName with
            | (true, subscriptions) ->
                subscriptions |>
                Seq.map (fun subscription ->
                    Task.Run (fun () ->
                        match subscription.Value with
                        | :? StoreCallback<obj, 'w> as objCallback ->
                            let evt = { StoreEventData = eventData :> obj; StorePublisher = publisher }
                            objCallback evt publisher this Unchecked.defaultof<'w> |> ignore<'w>
                        | _ -> failwithumf ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel
            | (false, _) -> Vsync.Parallel []
        Vsync.StartAsTask vsync

    member this.AllocateComponents<'c when 'c : struct and 'c :> 'c Component> buffered =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, _) -> failwith ("Array already initally allocated for '" + componentName + "'. Do you have multiple stores with the same component type? (not allowed)")
        | (false, _) ->
            if not buffered then
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                let arefs = List [box aref]
                arrayObjss.Add (componentName, { ArrayObjsUnbuffered = arefs; ArrayObjsBuffered = arefs })
                (aref, aref)
            else
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                let arefBuffered = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                arrayObjss.Add (componentName, { ArrayObjsUnbuffered = List [box aref]; ArrayObjsBuffered = List [box arefBuffered] })
                (aref, arefBuffered)

    member this.GetComponentArrays<'c when 'c : struct and 'c :> 'c Component> () =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) -> arrayObjs.ArrayObjsUnbuffered |> Seq.map (fun arefObj -> (arefObj :?> 'c ArrayRef).Array) |> Seq.toArray
        | (false, _) -> [||]

    member this.WithComponentArraysBuffered<'c when 'c : struct and 'c :> 'c Component> fn (world : 'w) =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            let arefsBuffered = arrayObjs.ArrayObjsBuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
            fn arefsBuffered world
        | (false, _) -> world

    member this.BufferComponentArrays<'c when 'c : struct and 'c :> 'c Component> () =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            lock arrayObjs $ fun () ->
                for i in 0 .. arrayObjs.ArrayObjsUnbuffered.Count - 1 do
                    let aref = arrayObjs.ArrayObjsUnbuffered.[i] :?> 'c ArrayRef
                    let arefBuffered = arrayObjs.ArrayObjsBuffered.[i] :?> 'c ArrayRef
                    lock arefBuffered (fun () ->
                        if arefBuffered.Array.Length = aref.Array.Length
                        then aref.Array.CopyTo (arefBuffered.Array, 0)
                        else arefBuffered.Array <- Array.copy aref.Array)
        | (false, _) -> ()

[<Extension>]
type EcsExtensions =

    [<Extension>]
    static member RegisterStore<'s, 'w when 's :> 'w Store> (this : 'w Ecs, store : 's) : 's =
        this.RegisterStoreGeneralized store :?> 's

/// An ECS store with just a single component.
type SingletonStore<'c, 'w when 'c : struct and 'c :> 'c Component> (comp : 'c) =
    inherit Store<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable comp = comp

    member this.Component = &comp

    override this.UnregisterComponent _ world = struct (false, world)

    type 'w Ecs with

        member this.IndexSingletonStore<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexStore<'c, SingletonStore<'c, 'w>> ()

        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> () =
            let storeOpt = this.TryIndexStore<'c, SingletonStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            &store.Component

/// An ECS store with components stored by an integer index.
type UncorrelatedStore<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) as this =
    inherit Store<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let registerUncorrelatedEvent = getTypeName this + "RegisterUncorrelated"
    let unregisteringUncorellatedEvent = getTypeName this + "UnregisteringUncorellated"

    member this.RegisterUncorellatedEvent = registerUncorrelatedEvent
    member this.UnregisteringUncorellatedEvent = unregisteringUncorellatedEvent

    new (ecs) = UncorrelatedStore (false, ecs)

    member this.FreeListCount = freeList.Count

    member this.Components = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w -> 'w) world = lock componentsBuffered (fun () -> fn componentsBuffered world)

    member this.RewindFreeIndex () =
        while freeList.Remove (dec freeIndex) do
            freeIndex <- dec freeIndex

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterUncorrelated ordered (comp : 'c) (world : 'w) =

        // rewind the freeIndex when requesting ordered registration
        if ordered then this.RewindFreeIndex ()

        // assign component
        let index =
            if  not ordered &&
                freeList.Count > 0 then
                let index = Seq.head freeList
                freeList.Remove index |> ignore<bool>
                components.[index] <- comp
                index
            elif freeIndex < components.Length then
                let index = freeIndex
                components.[index] <- comp
                freeIndex <- inc index
                index
            else
                let index = freeIndex
                let arr = Array.zeroCreate (components.Length * Constants.Ecs.ArrayGrowth)
                components.Array.CopyTo (arr, 0)
                components.Array <- arr
                components.[index] <- comp
                freeIndex <- inc index
                index

        // raise event
        let world = ecs.Publish this.RegisterUncorellatedEvent index this world
        struct (index, world)

    member this.UnregisterUncorrelated index (world : 'w) =
        let world = ecs.Publish this.UnregisteringUncorellatedEvent index this world
        if index <> freeIndex then
            components.[index].Active <- false
            freeList.Add index |> ignore<bool>
        else freeIndex <- dec freeIndex
        world
        
    override this.UnregisterComponent _ world = struct (false, world)

    type 'w Ecs with

        member this.IndexUncorrelatedStore<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexStore<'c, UncorrelatedStore<'c, 'w>> ()

        member this.IndexUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            let storeOpt = this.TryIndexStore<'c, UncorrelatedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            store.IndexUncorrelated index

        member this.RegisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> ordered comp (world : 'w) =
            match this.TryIndexStore<'c, UncorrelatedStore<'c, 'w>> () with
            | Some store -> store.RegisterUncorrelated ordered comp world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> index (world : 'w) =
            match this.TryIndexStore<'c, UncorrelatedStore<'c, 'w>> () with
            | Some store -> store.UnregisterUncorrelated index world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// An ECS store with components correlated by entity id (uint64).
type CorrelatedStore<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) as this =
    inherit Store<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<uint64, int> HashIdentity.Structural []
    let registerCorellatedEvent = getTypeName this + "RegisterCorellated"
    let unregisteringCorellatedEvent = getTypeName this + "UnregisteringCorellated"

    member this.RegisterCorellatedEvent = registerCorellatedEvent
    member this.UnregisteringCorellatedEvent = unregisteringCorellatedEvent

    new (ecs) = CorrelatedStore (false, ecs)

    member this.FreeListCount = freeList.Count
    member this.EntitiesCorrelated = correlations |> Seq.map (fun kvp -> kvp.Key)

    member this.Correlateds = correlateds
    member this.WithCorrelatedsBuffered (fn : 'c ArrayRef -> 'w -> 'w) world = lock correlatedsBuffered (fun () -> fn correlatedsBuffered world)

    member this.RewindFreeIndex () =
        while freeList.Remove (dec freeIndex) do
            freeIndex <- dec freeIndex

    member this.IndexCorrelatedUnbuffered index = &correlateds.Array.[index]
    member this.IndexCorrelatedBuffered index = correlatedsBuffered.Array.[index]

    /// OPTIMIZATION: returns -1 for failure to find rather than for speed.
    member this.TryIndexCorrelatedToI entityId =
        let (found, index) = correlations.TryGetValue entityId
        if found then index else -1

    member this.IndexCorrelatedToI entityId =
        match this.TryIndexCorrelatedToI entityId with
        | -1 -> raise (ArgumentOutOfRangeException "entityId")
        | index -> index

    member this.QualifyCorrelated entityId =
        correlations.ContainsKey entityId

    member this.IndexCorrelated entityId =
        let index = this.IndexCorrelatedToI entityId
        ComponentRef<'c>.make index correlateds correlatedsBuffered

    member this.RegisterCorrelated ordered (comp : 'c) entityId (world : 'w) =

        // check if component is already registered
        if not (correlations.ContainsKey entityId) then
        
            // rewind the freeIndex when requesting ordered registration
            if ordered then this.RewindFreeIndex ()

            // allocate index for component, enlarging arrays if necessary
            let index =
                if freeIndex >= correlateds.Length then
                    if  not ordered &&
                        freeList.Count <> 0 then
                        let index = Seq.head freeList
                        freeList.Remove index |> ignore<bool>
                        index
                    else
                        let length = correlateds.Length * Constants.Ecs.ArrayGrowth
                        let arr = Array.zeroCreate length in correlateds.Array.CopyTo (arr, 0); correlateds.Array <- arr
                        lock correlatedsBuffered (fun () -> let arr = Array.zeroCreate length in correlatedsBuffered.Array.CopyTo (arr, 0); correlatedsBuffered.Array <- arr)
                        let index = freeIndex
                        freeIndex <- inc freeIndex
                        index
                else
                    let index = freeIndex
                    freeIndex <- inc freeIndex
                    index

            // allocate component
            correlations.Add (entityId, index)
            correlateds.Array.[index] <- comp

            // register correlation with ecs
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Add this.Id |> ignore<bool>
                ecs.FilterForSystems this.Name entityId
            | (false, _) ->
                let correlation = HashSet.singleton HashIdentity.Structural this.Id
                ecs.Correlations.Add (entityId, correlation)
                ecs.FilterForSystems this.Name entityId

            // raise event
            ecs.Publish this.RegisterCorellatedEvent entityId this world

        // component is already registered
        else world

    member this.UnregisterCorrelated entityId (world : 'w) =

        // attempt to unregister component
        let struct (unregistered, world) =
            match correlations.TryGetValue entityId with
            | (true, index) ->

                // raise removing event
                let world = ecs.Publish this.UnregisteringCorellatedEvent entityId this world

                // deallocate component
                correlations.Remove entityId |> ignore<bool>

                // deallocate index for component
                if index <> freeIndex
                then freeList.Add index |> ignore<bool>
                else freeIndex <- dec freeIndex

                // deactive component
                correlateds.[index].Active <- false

                // clear free list and reset free index when there are no registered components remaining
                if  freeList.Count = correlateds.Length then
                    freeList.Clear ()
                    freeIndex <- 0
                struct (true, world)
            | (false, _) -> struct (false, world)

        // unregister correlation from ecs
        if unregistered then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Id |> ignore<bool>
                ecs.FilterForSystems this.Name entityId
            | (false, _) ->
                ecs.FilterForSystems this.Name entityId

        // fin
        struct (unregistered, world)

        override this.UnregisterComponent entityId world =
            this.UnregisterCorrelated entityId world

    type 'w Ecs with

        member this.IndexCorrelatedStore<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexStore<'c, CorrelatedStore<'c, 'w>> ()

        member this.QualifyCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            let storeOpt = this.TryIndexStore<'c, CorrelatedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            store.QualifyCorrelated entityId

        member inline this.IndexCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId : 'c ComponentRef =
            let storeOpt = this.TryIndexStore<'c, CorrelatedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            store.IndexCorrelated entityId

        member this.RegisterCorrelated<'c when 'c : struct and 'c :> 'c Component> ordered comp entityId (world : 'w) =
            match this.TryIndexStore<'c, CorrelatedStore<'c, 'w>> () with
            | Some store -> store.RegisterCorrelated ordered comp entityId world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId (world : 'w) =
            match this.TryIndexStore<'c, CorrelatedStore<'c, 'w>> () with
            | Some store -> store.UnregisterCorrelated entityId world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// A HierarchicalStore node.
type [<NoEquality; NoComparison>] HierarchicalNode<'c when 'c : struct and 'c :> 'c Component> =
    { EntityId : uint64
      ComponentRef : 'c ComponentRef}

/// Tracks changes in a hierarchical store.
type [<NoEquality; NoComparison; Struct>] HierarchicalChange =
    { /// Whether change involves registeration (true) or unregistration (false).
      Registered : bool
      /// The node entity's id.
      NodeId : uint64
      /// The parent's entity id or 0UL if no parent.
      ParentIdOpt : uint64 }

/// An ECS store that stores components in a hierarchy.
/// TODO: make hierarchy changes into ECS events.
type HierarchicalStore<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit CorrelatedStore<'c, 'w> (buffered, ecs)

    let hierarchy = ListTree.makeEmpty<'c HierarchicalNode> ()
    let mutable hierarchyChanges = List<HierarchicalChange> ()
    
    new (ecs) = HierarchicalStore (false, ecs)
    
    member this.Hierarchy = hierarchy
    member this.HierarchyFlattened = this.Correlateds
    member this.WithHierarchyFlattenedBuffered fn world = this.WithCorrelatedsBuffered fn world

    member this.PopHierarchyChanges () =
        let popped = hierarchyChanges
        hierarchyChanges <- List<HierarchicalChange> ()
        popped

    member this.IndexHierarchical entityId =
        this.IndexCorrelated entityId

    member this.RegisterHierarchical ordered (parentIdOpt : uint64 option) comp entityId world =
        let this' = this :> CorrelatedStore<'c, 'w>
        let world = this'.RegisterCorrelated ordered comp entityId world
        let cref = this'.IndexCorrelated entityId
        let node = { EntityId = entityId; ComponentRef = cref }
        let addedOpt =
            match parentIdOpt with
            | Some parentId -> ListTree.tryInsert (fun node -> node.EntityId = parentId) node hierarchy
            | None -> ListTree.tryAdd tautology node hierarchy
        if Option.isSome addedOpt then
            let hierarchyChange = { Registered = true; NodeId = entityId; ParentIdOpt = Option.getOrDefault 0UL parentIdOpt }
            hierarchyChanges.Add hierarchyChange
            world
        else world

    member this.UnregisterHierarchical entityId world =
        let result = ListTree.removeFirst (fun node -> node.EntityId = entityId) hierarchy
        if result then
            let hierarchyChage = { Registered = false; NodeId = entityId; ParentIdOpt = 0UL }
            hierarchyChanges.Add hierarchyChage
        struct (result, world)

    member this.QualifyHierarchical entityId =
        this.QualifyCorrelated entityId

    override this.UnregisterComponent entityId world =
        this.UnregisterHierarchical entityId world

    type 'w Ecs with

        member this.IndexHierarchicalStore<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexStore<'c, HierarchicalStore<'c, 'w>> ()

        member this.IndexHierarchy<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.Hierarchy
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexHierarchyFlattened<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.HierarchyFlattened
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.PopHierarchyChanges<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.PopHierarchyChanges ()
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> entityId =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.IndexHierarchical entityId
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> 'c Component> ordered parentIdOpt comp entityId (world : 'w) =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.RegisterHierarchical ordered parentIdOpt comp entityId world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> parentIdOpt (world : 'w) =
            match this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> () with
            | Some store -> store.UnregisterHierarchical parentIdOpt world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> 'c Component> entityId =
            let storeOpt = this.TryIndexStore<'c, HierarchicalStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            store.QualifyHierarchical entityId

/// Handle to one of an array of multiplexed components.
type Simplex<'c when 'c : struct> =
    { mutable Simplex : 'c }

/// Allows an entity to contain zero to many of the same component.
/// However, it uses a dictionary without a small-object optimization, so this functionality won't get the typical
/// perf benefits of data-orientation. Really, this functionality is here for flexibility and convenience more than
/// anything else (which is good enough in almost all cases where multiplexing is used).
type [<NoEquality; NoComparison; Struct>] MultiplexedComponent<'c when 'c : struct and 'c :> 'c Component> =
    { mutable Active : bool
      Simplexes : Dictionary<string, 'c Simplex>
      TypeName : string }
    interface Component<'c MultiplexedComponent> with
        member this.TypeName = getTypeName this
        member this.Active with get() = this.Active and set value = this.Active <- value
    member this.RegisterMultiplexed (simplexName, comp) =
        this.Simplexes.Add (simplexName, { Simplex = comp })
    member this.UnregisterMultiplexed simplexName =
        this.Simplexes.Remove simplexName

/// An ECS store that stores zero to many of the same component per entity id.
type MultiplexedStore<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit CorrelatedStore<'c MultiplexedComponent, 'w> (buffered, ecs)

    new (ecs) = MultiplexedStore (false, ecs)

    member this.QualifyMultiplexed simplexName entityId =
        if this.QualifyCorrelated entityId then
            let comp = this.IndexCorrelated entityId
            comp.Index.Simplexes.ContainsKey simplexName
        else false

    member this.IndexMultiplexed simplexName entityId =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.Simplexes.[simplexName]

    member this.IndexMultiplexedBuffered simplexName entityId =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.IndexBuffered.Simplexes.[simplexName]

    member this.RegisterMultiplexed ordered comp simplexName entityId world =

        // register simplex
        let world = this.RegisterCorrelated ordered Unchecked.defaultof<_> entityId world
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.RegisterMultiplexed (simplexName, comp)
        
        // register correlation with ecs
        match ecs.Correlations.TryGetValue entityId with
        | (true, correlation) ->
            correlation.Add this.Id |> ignore<bool>
            ecs.FilterForSystems this.Name entityId
        | (false, _) ->
            let correlation = HashSet.singleton HashIdentity.Structural this.Id
            ecs.Correlations.Add (entityId, correlation)
            ecs.FilterForSystems this.Name entityId

        // fin
        struct (componentMultiplexed, world)

    member this.UnregisterMultiplexed simplexName entityId world =

        // attempt to unregister simplex
        let struct (unregistered, world) =
            let componentMultiplexed = this.IndexCorrelated entityId
            componentMultiplexed.Index.UnregisterMultiplexed simplexName |> ignore<bool>
            this.UnregisterCorrelated entityId world
        
        // unregister correlation
        if unregistered then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Id |> ignore<bool>
                ecs.FilterForSystems this.Name entityId
            | (false, _) ->
                ecs.FilterForSystems this.Name entityId

        // fin
        struct (unregistered, world)

    type 'w Ecs with

        member this.IndexMultiplexedStore<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexStore<'c, MultiplexedStore<'c, 'w>> ()

        member this.QualifyMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let storeOpt = this.TryIndexStore<'c, MultiplexedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            store.QualifyMultiplexed simplexName entityId

        member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let storeOpt = this.TryIndexStore<'c, MultiplexedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            let simplex = store.IndexMultiplexed simplexName entityId
            &simplex.Simplex

        member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let storeOpt = this.TryIndexStore<'c, MultiplexedStore<'c, 'w>> ()
            if Option.isNone storeOpt then failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let store = Option.get storeOpt
            let simplex = store.IndexMultiplexedBuffered simplexName entityId
            simplex.Simplex

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> ordered comp simplexName entityId (world : 'w) =
            match this.TryIndexStore<'c, MultiplexedStore<'c, 'w>> () with
            | Some store -> store.RegisterMultiplexed ordered comp simplexName entityId world
            | None -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId (world : 'w) =
            match this.TryIndexStore<'c, MultiplexedStore<'c, 'w>> () with
            | Some store -> store.UnregisterMultiplexed simplexName entityId world
            | _ -> failwith ("Could not find expected store '" + Unchecked.defaultof<'c>.TypeName + "'.")

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"