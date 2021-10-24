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
and [<NoEquality; NoComparison>] SystemEvent<'d, 'w> =
    { SystemEventData : 'd
      SystemPublisher : 'w System }

/// An ECS event callback.
and SystemCallback<'d, 'w> =
    SystemEvent<'d, 'w> -> 'w System -> 'w Ecs -> 'w -> 'w

/// A boxed ECS event callback.
and SystemCallbackBoxed<'w> =
    SystemEvent<obj, 'w> -> 'w System -> 'w Ecs -> 'w -> 'w

/// The event type recommended for SynchronizeCorrelationChanges.
and 'w SynchronizeCorrelationEvent =
    SystemEvent<Dictionary<uint64, string HashSet>, 'w>

/// The type of synchronization used.
and [<StructuralEquality; NoComparison; Struct>] SynchronizeResult =
    | Registered
    | Unregistered
    | Unchanged of bool

/// A base system type of an ECS.
/// Systems are a bit different in this ECS; they're primarily storage for components and don't have any behavior
/// associated with them. Because this ECS is purely event-driven, behavior is encoded in event handlers rather than
/// systems. If anything, it might be better to be renamed to 'w Storage.
and [<AbstractClass>] 'w System (name) =
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

/// An ECS query.
and 'w Query =
    abstract Correlation : string HashSet
    abstract Filter : uint64 -> bool

/// A collection of ECS queries.
and 'w Queries () =

    let queries = dictPlus<string, 'w Query HashSet> StringComparer.Ordinal []

    member this.RegisterQuery (query : 'w Query) =
        for systemName in query.Correlation do
            match queries.TryGetValue systemName with
            | (true, querySet) -> querySet.Add query |> ignore<bool>
            | (false, _) -> queries.Add (systemName, hashSetPlus<'w Query> HashIdentity.Reference [query])

    member this.UnregisterQuery (query : 'w Query) =
        for systemName in query.Correlation do
            match queries.TryGetValue systemName with
            | (true, querySet) ->
                querySet.Remove query |> ignore<bool>
                if querySet.Count = 0 then
                    queries.Remove systemName |> ignore<bool>
            | (false, _) -> ()

    member this.Filter systemName entityId =
        match queries.TryGetValue systemName with
        | (true, querySet) ->
            for query in querySet do
                query.Filter entityId |> ignore<bool>
        | (false, _) -> ()

/// Nu's custom Entity-Component-System implementation.
/// Nu's conception of an ECS is primarily as an abstraction over user-definable component storage formats.
/// The default formats include SoA-style formats for non-correlated, correlated, multiplexed, and hierarchichal value
/// types. User can add formats of their own design by implementing the 'w System interface and providing related
/// extension methods on this 'w Ecs type.
and 'w Ecs () as this =

    let mutable systemCached = { new System<'w> (typeof<unit>.Name) with member this.UnregisterComponent _ world = struct (false, world) }
    let systemGlobal = systemCached
    let arrayObjss = dictPlus<string, ArrayObjs> StringComparer.Ordinal []
    let systemSubscriptions = dictPlus<string, Dictionary<uint32, obj>> StringComparer.Ordinal []
    let systemsUnordered = dictPlus<string, 'w System> StringComparer.Ordinal []
    let systemsOrdered = List<string * 'w System> ()
    let systemsById = dictPlus<uint, 'w System> HashIdentity.Structural [] // TODO: see if a quadratic searching dictionary could improve perf here.
    let correlations = dictPlus<uint64, uint HashSet> HashIdentity.Structural [] // NOTE: correlations are keyed by System.Ids to keep their dictionary entry types unmanaged.
    let emptySystemIds = hashSetPlus<uint> HashIdentity.Structural []
    let queries = Queries<'w> ()

    do this.RegisterSystemGeneralized systemGlobal |> ignore<'w System>

    member private this.BoxCallback<'a> (callback : SystemCallback<'a, 'w>) =
        let boxableCallback = fun (evt : SystemEvent<obj, 'w>) system ->
            let evt =
                { SystemEventData = evt.SystemEventData :?> 'a
                  SystemPublisher = evt.SystemPublisher }
            callback evt system
        boxableCallback :> obj

    member internal this.Correlations = correlations
    member this.SystemGlobal = systemGlobal

    member this.RegisterQuery query =
        queries.RegisterQuery query
        query

    member this.UnregisterQuery query =
        queries.UnregisterQuery query
        query

    member this.FilterForQueries systemName entityId =
        queries.Filter systemName entityId

    member this.RegisterSystemGeneralized (system : 'w System) : 'w System =
        systemsUnordered.Add (system.Name, system)
        systemsOrdered.Add (system.Name, system)
        systemsById.Add (system.Id, system)
        system

    member this.TryIndexSystem systemName =
        if systemCached.Name <> systemName then
            match systemsUnordered.TryGetValue systemName with
            | (true, system) -> 
                systemCached <- system
                Some system
            | (false, _) -> None
        else Some systemCached

    member this.IndexSystem systemName =
        if systemCached.Name <> systemName then
            match systemsUnordered.TryGetValue systemName with
            | (true, system) -> 
                systemCached <- system
                system
            | (false, _) -> failwith ("Could not index system '" + systemName + "'.")
        else systemCached

    member this.TryIndexSystem<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w System> () =
        let systemName = Unchecked.defaultof<'c>.TypeName
        match this.TryIndexSystem systemName with
        | Some system ->
            match system with
            | :? 's as systemAsS -> Some systemAsS
            | _ -> None
        | None -> None

    member this.IndexSystem<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w System> () =
        let systemName = Unchecked.defaultof<'c>.TypeName
        this.IndexSystem systemName :?> 's

    member this.IndexSystemIds entityId =
        match correlations.TryGetValue entityId with
        | (true, correlation) -> correlation
        | (false, _) -> emptySystemIds

    member this.IndexSystems entityId =
        this.IndexSystemIds entityId |>
        Seq.map (fun id -> systemsById.[id])

    member this.IndexEntities correlation =
        correlations |>
        Seq.filter (fun kvp -> kvp.Value.IsSubsetOf correlation) |>
        Seq.map (fun kvp -> kvp.Key)

    member this.UnregisterComponents entityId world =
        let mutable unregistered = false
        let mutable world = world
        for system in this.IndexSystems entityId do
            let struct (unregistered', world') = system.UnregisterComponent entityId world
            if unregistered' then unregistered <- true
            world <- world'
        struct (unregistered, world)

    member this.SubscribePlus<'d> subscriptionId eventName (callback : SystemCallback<'d, 'w>) =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            subscriptions.Add (subscriptionId, this.BoxCallback<'d> callback)
            subscriptionId
        | (false, _) ->
            let subscriptions = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d> callback)]
            systemSubscriptions.Add (eventName, subscriptions)
            subscriptionId

    member this.Subscribe<'d, 'w> eventName callback =
        this.SubscribePlus<'d> Gen.id32 eventName callback |> ignore

    member this.Unsubscribe eventName subscriptionId =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) -> subscriptions.Remove subscriptionId
        | (false, _) -> false

    member this.Publish<'d> eventName (eventData : 'd) publisher world =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            Seq.fold (fun world (callback : obj) ->
                match callback with
                | :? SystemCallback<obj, 'w> as objCallback ->
                    let evt = { SystemEventData = eventData :> obj; SystemPublisher = publisher }
                    objCallback evt publisher this world
                | _ -> failwithumf ())
                world subscriptions.Values
        | (false, _) -> world

    member this.PublishAsync<'d> eventName (eventData : 'd) publisher =
        let vsync =
            match systemSubscriptions.TryGetValue eventName with
            | (true, subscriptions) ->
                subscriptions |>
                Seq.map (fun subscription ->
                    Task.Run (fun () ->
                        match subscription.Value with
                        | :? SystemCallback<obj, 'w> as objCallback ->
                            let evt = { SystemEventData = eventData :> obj; SystemPublisher = publisher }
                            objCallback evt publisher this Unchecked.defaultof<'w> |> ignore<'w>
                        | _ -> failwithumf ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel
            | (false, _) -> Vsync.Parallel []
        Vsync.StartAsTask vsync

    member this.AllocateComponents<'c when 'c : struct and 'c :> 'c Component> buffered =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, _) -> failwith ("Array already initally allocated for '" + componentName + "'. Do you have multiple systems with the same component type? (not allowed)")
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
    static member RegisterSystem<'s, 'w when 's :> 'w System> (this : 'w Ecs, system : 's) : 's =
        this.RegisterSystemGeneralized system :?> 's

/// An ECS system with just a single component.
type SystemSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> (comp : 'c) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable comp = comp

    member this.Component = &comp

    override this.UnregisterComponent _ world = struct (false, world)

    type 'w Ecs with

        member this.IndexSystemSingleton<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemSingleton<'c, 'w>> ()

        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> () =
            let systemOpt = this.TryIndexSystem<'c, SystemSingleton<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// An ECS system with components stored by an integer index.
type SystemUncorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) as this =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let uncorellatedRegisterEvent = getTypeName this + "UncorellatedRegister"
    let uncorellatedUnregisteringEvent = getTypeName this + "UncorellatedUnregistering"

    member this.UncorellatedRegisterEvent = uncorellatedRegisterEvent
    member this.UncorellatedUnregisteringEvent = uncorellatedUnregisteringEvent

    new (ecs) = SystemUncorrelated (false, ecs)

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

        // ensure component is marked active
        let mutable comp = comp
        comp.Active <- true

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
        let world = ecs.Publish this.UncorellatedRegisterEvent index this world
        struct (index, world)

    member this.UnregisterUncorrelated index (world : 'w) =
        let world = ecs.Publish this.UncorellatedUnregisteringEvent index this world
        if index <> freeIndex then
            components.[index].Active <- false
            freeList.Add index |> ignore<bool>
        else freeIndex <- dec freeIndex
        world
        
    override this.UnregisterComponent _ world = struct (false, world)

    type 'w Ecs with

        member this.IndexSystemUncorrelated<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemUncorrelated<'c, 'w>> ()

        member this.IndexUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            let systemOpt = this.TryIndexSystem<'c, SystemUncorrelated<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexUncorrelated index

        member this.RegisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> ordered comp =
            match this.TryIndexSystem<'c, SystemUncorrelated<'c, 'w>> () with
            | Some system -> system.RegisterUncorrelated ordered comp
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            match this.TryIndexSystem<'c, SystemUncorrelated<'c, 'w>> () with
            | Some system -> system.UnregisterUncorrelated index
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// An ECS system with components correlated by entity id (uint64).
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) as this =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<uint64, int> HashIdentity.Structural []
    let corellatedRegisterEvent = getTypeName this + "CorellatedRegister"
    let corellatedUnregisteringEvent = getTypeName this + "CorellatedUnregistering"

    member this.CorellatedRegisterEvent = corellatedRegisterEvent
    member this.CorellatedUnregisteringEvent = corellatedUnregisteringEvent

    new (ecs) = SystemCorrelated (false, ecs)

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

        // activate component
        let mutable comp = comp
        comp.Active <- true

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
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                let correlation = HashSet.singleton HashIdentity.Structural this.Id
                ecs.Correlations.Add (entityId, correlation)
                ecs.FilterForQueries this.Name entityId

            // raise event
            ecs.Publish this.CorellatedRegisterEvent entityId this world

        // component is already registered
        else world

    member this.UnregisterCorrelated entityId (world : 'w) =

        // attempt to unregister component
        let struct (unregistered, world) =
            match correlations.TryGetValue entityId with
            | (true, index) ->

                // raise removing event
                let world = ecs.Publish this.CorellatedUnregisteringEvent entityId this world

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
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                ecs.FilterForQueries this.Name entityId

        // fin
        struct (unregistered, world)

        override this.UnregisterComponent entityId world =
            this.UnregisterCorrelated entityId world

    type 'w Ecs with

        member this.IndexSystemCorrelated<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()

        member this.QualifyCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyCorrelated entityId

        member inline this.IndexCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId : 'c ComponentRef =
            let systemOpt = this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexCorrelated entityId

        member this.RegisterCorrelated<'c when 'c : struct and 'c :> 'c Component> ordered comp entityId =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system -> system.RegisterCorrelated ordered comp entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system -> system.UnregisterCorrelated entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated (systemName, entityId) =
            match this.TryIndexSystem systemName with
            | Some system -> system.UnregisterComponent entityId
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

/// A SystemHierarchical node.
type [<NoEquality; NoComparison>] NodeHierarchical<'c when 'c : struct and 'c :> 'c Component> =
    { EntityId : uint64
      ComponentRef : 'c ComponentRef}

/// Tracks changes in a hierarchical system.
type [<NoEquality; NoComparison; Struct>] ChangeHierarchical =
    { /// Whether change involves registeration (true) or unregistration (false).
      Registered : bool
      /// The node entity's id.
      NodeId : uint64
      /// The parent's entity id or 0UL if no parent.
      ParentIdOpt : uint64 }

/// An Ecs system that stores components in a hierarchy.
type SystemHierarchical<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c, 'w> (buffered, ecs)

    let hierarchy = ListTree.makeEmpty<'c NodeHierarchical> ()
    let mutable hierarchyChanges = List<ChangeHierarchical> ()
    
    new (ecs) = SystemHierarchical (false, ecs)
    
    member this.Hierarchy = hierarchy
    member this.HierarchyFlattened = this.Correlateds
    member this.WithHierarchyFlattenedBuffered fn world = this.WithCorrelatedsBuffered fn world

    member this.PopHierarchyChanges () =
        let popped = hierarchyChanges
        hierarchyChanges <- List<ChangeHierarchical> ()
        popped

    member this.IndexHierarchical entityId =
        this.IndexCorrelated entityId

    member this.RegisterHierarchical ordered (parentIdOpt : uint64 option) comp entityId world =
        let this' = this :> SystemCorrelated<'c, 'w>
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

        member this.IndexSystemHierarchical<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()

        member this.IndexHierarchy<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.Hierarchy
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexHierarchyFlattened<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.HierarchyFlattened
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.PopHierarchyChanges<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.PopHierarchyChanges ()
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> entityId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.IndexHierarchical entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> 'c Component> ordered parentIdOpt comp entityId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.RegisterHierarchical ordered parentIdOpt comp entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> parentIdOpt (world : 'w) =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.UnregisterHierarchical parentIdOpt world
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> 'c Component> entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyHierarchical entityId

/// Handle to one of an array of multiplexed components.
type Simplex<'c when 'c : struct> =
    { mutable Simplex : 'c }

/// Allows an entity to contain zero to many of the same component.
/// However, it uses a dictionary without a small-object optimization, so this functionality won't get the typical
/// perf benefits of data-orientation. Really, this functionality is here for flexibility and convenience more than
/// anything else (which is good enough in almost all cases where multiplexing is used).
type [<NoEquality; NoComparison; Struct>] ComponentMultiplexed<'c when 'c : struct and 'c :> 'c Component> =
    { mutable Active : bool
      Simplexes : Dictionary<string, 'c Simplex>
      TypeName : string }
    interface Component<'c ComponentMultiplexed> with
        member this.TypeName = getTypeName this
        member this.Active with get() = this.Active and set value = this.Active <- value
    member this.RegisterMultiplexed (simplexName, comp) =
        this.Simplexes.Add (simplexName, { Simplex = comp })
    member this.UnregisterMultiplexed simplexName =
        this.Simplexes.Remove simplexName

/// An ECS system that stores zero to many of the same component per entity id.
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (buffered, ecs)

    new (ecs) = SystemMultiplexed (false, ecs)

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
            ecs.FilterForQueries this.Name entityId
        | (false, _) ->
            let correlation = HashSet.singleton HashIdentity.Structural this.Id
            ecs.Correlations.Add (entityId, correlation)
            ecs.FilterForQueries this.Name entityId

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
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                ecs.FilterForQueries this.Name entityId

        // fin
        struct (unregistered, world)

    type 'w Ecs with

        member this.IndexSystemMultiplexed<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()

        member this.QualifyMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyMultiplexed simplexName entityId

        member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexed simplexName entityId
            &simplex.Simplex

        member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexedBuffered simplexName entityId
            simplex.Simplex

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> ordered comp simplexName entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system -> system.RegisterMultiplexed ordered comp simplexName entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system -> system.UnregisterMultiplexed simplexName entityId
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] SynchronizeCorrelationChanges = "SynchronizeCorrelationChanges"
    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"