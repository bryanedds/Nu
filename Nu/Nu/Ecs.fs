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
    SystemEvent<'d, 'w> -> 'w System -> 'w Ecs -> 'w option -> 'w option

/// A boxed ECS event callback.
and SystemCallbackBoxed<'w> =
    SystemEvent<obj, 'w> -> 'w System -> 'w Ecs -> 'w option -> 'w option

/// The event type recommended for SynchronizeCorrelationChanges.
and 'w SynchronizeCorrelationEvent =
    SystemEvent<Dictionary<Guid, string HashSet>, 'w>

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
    abstract UnregisterComponent : Guid -> bool

/// Potentially-buffered array objects.
/// ArrayObjsUnbuffered will be refEq to ArrayObjsBuffered if buffering is disabled.
and [<NoEquality; NoComparison>] internal ArrayObjs =
    { ArrayObjsUnbuffered : obj List
      mutable ArrayObjsBuffered : obj List }
    member this.Buffered =
        refEq
            this.ArrayObjsBuffered
            this.ArrayObjsUnbuffered

/// A correlated entity reference.
/// Slow relative to normal ECS operations, but convenient for one-off uses.
and [<NoEquality; NoComparison; Struct>] 'w EntityRef =
    { EntityId : Guid
      EntityEcs : 'w Ecs }
    static member make<'w> entityId ecs =
        { EntityId = entityId; EntityEcs = ecs } : 'w EntityRef

/// An ECS query.
and 'w Query =
    abstract Correlation : string HashSet
    abstract Filter : Guid -> bool

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
/// To keep this ECS implementation a library rather than a framework, it features no inversion of control mechanisms.
/// Yes, there are ECS event callbacks, but they are not IoC because they are not invoked from within the ECS but
/// rather by the client code exclusively. The most interesting exclusion is a lack of component add / remove events.
/// The author concludes that this feature would only compromise the algebraic nature of this design as well as
/// performance.
and 'w Ecs () as this =

    let mutable systemCached = { new System<'w> (typeof<unit>.Name) with member this.UnregisterComponent _ = false }
    let systemGlobal = systemCached
    let arrayObjss = dictPlus<string, ArrayObjs> StringComparer.Ordinal []
    let systemSubscriptions = dictPlus<string, Dictionary<Guid, obj>> StringComparer.Ordinal []
    let systemsUnordered = dictPlus<string, 'w System> StringComparer.Ordinal []
    let systemsOrdered = List<string * 'w System> ()
    let correlations = dictPlus<Guid, string HashSet> HashIdentity.Structural []
    let emptySystemNames = hashSetPlus<string> StringComparer.Ordinal [] // the empty systems dict to elide allocation on IndexSystemNames
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

    member this.GetEntityRef entityId =
        EntityRef.make<'w> entityId this

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

    member this.IndexSystemNames entityId =
        match correlations.TryGetValue entityId with
        | (true, correlation) -> correlation
        | (false, _) -> emptySystemNames

    member this.IndexSystems entityId =
        this.IndexSystemNames entityId |>
        Seq.map this.IndexSystem

    member this.IndexEntities correlation =
        correlations |>
        Seq.filter (fun kvp -> kvp.Value.IsSubsetOf correlation) |>
        Seq.map (fun kvp -> kvp.Key)

    member this.UnregisterComponents entityId =
        let mutable result = false
        for system in this.IndexSystems entityId do
            if system.UnregisterComponent entityId then
                result <- true
        result

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
        this.SubscribePlus<'d> Gen.id eventName (fun evt system ecs worldOpt -> callback evt system ecs; worldOpt) |> ignore

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
                    objCallback evt publisher this (Some world) |> Option.get
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
                            objCallback evt publisher this None |> ignore<'w option>
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

    member this.WithComponentArraysBuffered<'c when 'c : struct and 'c :> 'c Component> fn (worldOpt : 'w option) =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            let arefsBuffered = arrayObjs.ArrayObjsBuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
            fn arefsBuffered worldOpt
        | (false, _) -> worldOpt

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

    override this.UnregisterComponent _ = false

    type 'w Ecs with

        member this.IndexSystemSingleton<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemSingleton<'c, 'w>> ()

        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> () =
            let systemOpt = this.TryIndexSystem<'c, SystemSingleton<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// An ECS system with components stored by an integer index.
type SystemUncorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural

    new (ecs) = SystemUncorrelated (false, ecs)

    member this.FreeListCount = freeList.Count

    member this.Components = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock componentsBuffered (fun () -> fn componentsBuffered worldOpt)

    member this.RewindFreeIndex () =
        while freeList.Remove (dec freeIndex) do
            freeIndex <- dec freeIndex

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterUncorrelated ordered (comp : 'c) =

        // ensure component is marked active
        let mutable comp = comp
        comp.Active <- true

        // rewind the freeIndex when requesting ordered registration
        if ordered then this.RewindFreeIndex ()

        // assign component
        if  not ordered &&
            freeList.Count > 0 then
            let index = Seq.head freeList
            freeList.Remove index |> ignore<bool>
            components.[index] <- comp
            ComponentRef.make index components componentsBuffered
        elif freeIndex < components.Length then
            let index = freeIndex
            components.[index] <- comp
            freeIndex <- inc index
            ComponentRef.make index components componentsBuffered
        else
            let index = freeIndex
            let arr = Array.zeroCreate (components.Length * Constants.Ecs.ArrayGrowth)
            components.Array.CopyTo (arr, 0)
            components.Array <- arr
            components.[index] <- comp
            freeIndex <- inc index
            ComponentRef.make index components componentsBuffered

    member this.UnregisterUncorrelated index =
        if index <> freeIndex then
            components.[index].Active <- false
            freeList.Add index |> ignore<bool>
        else freeIndex <- dec freeIndex
        
    override this.UnregisterComponent _ = false

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

/// An ECS system with components correlated by entity id (Guid).
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (isolated, buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<Guid, int> HashIdentity.Structural []
    let correlationsBack = dictPlus<int, Guid> HashIdentity.Structural []
    
    new (buffered, ecs) = SystemCorrelated (false, buffered, ecs)
    new (ecs) = SystemCorrelated (false, false, ecs)

    member this.FreeListCount = freeList.Count
    member this.EntitiesCorrelated = correlations |> Seq.map (fun kvp -> kvp.Key)

    member this.Correlateds = correlateds
    member this.WithCorrelatedsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock correlatedsBuffered (fun () -> fn correlatedsBuffered worldOpt)

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

    member this.RegisterCorrelated ordered (comp : 'c) entityId =

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
            correlationsBack.Add (index, entityId)
            correlateds.Array.[index] <- comp

            // register correlation with ecs if needed
            if not isolated then
                match ecs.Correlations.TryGetValue entityId with
                | (true, correlation) ->
                    correlation.Add this.Name |> ignore<bool>
                    ecs.FilterForQueries this.Name entityId
                | (false, _) ->
                    let correlation = HashSet.singleton StringComparer.Ordinal this.Name
                    ecs.Correlations.Add (entityId, correlation)
                    ecs.FilterForQueries this.Name entityId

            // make component ref
            ComponentRef.make index correlateds correlatedsBuffered

        // component is already registered
        else failwith ("Component registered multiple times for entity '" + scstring entityId + "'.")

    member this.UnregisterCorrelated entityId =

        // attempt to unregister component
        let unregistered =
            match correlations.TryGetValue entityId with
            | (true, index) ->

                // deallocate component
                correlations.Remove entityId |> ignore<bool>
                correlationsBack.Remove index |> ignore<bool>

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
                true
            | (false, _) -> false

        // unregister correlation from ecs if needed
        if unregistered && not isolated then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Name |> ignore<bool>
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                ecs.FilterForQueries this.Name entityId

        // fin
        unregistered

        override this.UnregisterComponent entityId =
            this.UnregisterCorrelated entityId

    type 'w EntityRef with
    
        member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
            let system = this.EntityEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
            &correlated.Index
    
        member this.IndexBuffered<'c when 'c : struct and 'c :> 'c Component> () =
            let system = this.EntityEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
            correlated.IndexBuffered

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

/// A delegate that enables passing of values byref.
type Iter<'c when
            'c : struct and 'c :> 'c Component> =
            delegate of 'c byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2 when
           'c : struct and 'c :> 'c Component and
           'c2 : struct and 'c2 :> 'c2 Component> =
            delegate of 'c byref * 'c2 byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2, 'c3 when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component> =
            delegate of 'c byref * 'c2 byref * 'c3 byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2, 'c3, 'c4 when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component> =
            delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2, 'c3, 'c4, 'c5 when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component> =
            delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component and
            'c6 : struct and 'c6 :> 'c6 Component> =
            delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref -> unit

/// A delegate that enables passing of values byref.
type Iter<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component and
            'c6 : struct and 'c6 :> 'c6 Component and
            'c7 : struct and 'c7 :> 'c7 Component> =
            delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref -> unit

/// An ECS query.
type Query<'c, 'w when
            'c : struct and 'c :> 'c Component> (ecs : 'w Ecs) =
            
    let cache = OrderedDictionary<Guid, int> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()

    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let array = system.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            iter.Invoke &array.[enr.Current]
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            if indexOpt > -1 then
                this.Cache.[entityId] <- indexOpt
                true
            else
                this.Cache.Remove entityId |> ignore<bool>
                false

/// An ECS query.
type Query<'c, 'c2, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<Guid, struct (int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()

    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 then
                cache.[entityId] <- struct (indexOpt, indexOpt2)
                true
            elif indexOpt > -1 || indexOpt2 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

/// An ECS query.
type Query<'c, 'c2, 'c3, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component> (ecs : 'w Ecs) =
            
    let cache = OrderedDictionary<Guid, struct (int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    
    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2, 'c3>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let system3 = this.Ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2], &array3.[index3])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 then
                this.Cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component> (ecs : 'w Ecs) =
            
    let cache = OrderedDictionary<Guid, struct (int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    
    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2, 'c3, 'c4>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let system3 = this.Ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
        let system4 = this.Ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 then
                this.Cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<Guid, struct (int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    
    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2, 'c3, 'c4, 'c5>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let system3 = this.Ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
        let system4 = this.Ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
        let system5 = this.Ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 then
                this.Cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component and
            'c6 : struct and 'c6 :> 'c6 Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<Guid, struct (int * int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    
    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2, 'c3, 'c4, 'c5, 'c6>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let system3 = this.Ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
        let system4 = this.Ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
        let system5 = this.Ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
        let system6 = this.Ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            let indexOpt6 = system6.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 then
                this.Cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
            'c : struct and 'c :> 'c Component and
            'c2 : struct and 'c2 :> 'c2 Component and
            'c3 : struct and 'c3 :> 'c3 Component and
            'c4 : struct and 'c4 :> 'c4 Component and
            'c5 : struct and 'c5 :> 'c5 Component and
            'c6 : struct and 'c6 :> 'c6 Component and
            'c7 : struct and 'c7 :> 'c7 Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<Guid, struct (int * int * int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    let system7 = ecs.IndexSystem<'c7, SystemCorrelated<'c7, 'w>> ()
    
    member this.Ecs = ecs
    member this.Cache = cache

    member inline this.Iter (iter : Iter<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7>) =
        let system = this.Ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let system2 = this.Ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
        let system3 = this.Ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
        let system4 = this.Ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
        let system5 = this.Ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
        let system6 = this.Ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
        let system7 = this.Ecs.IndexSystem<'c7, SystemCorrelated<'c7, 'w>> ()
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let array7 = system7.Correlateds.Array
        let mutable enr = this.Cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6, index7) = enr.Current
            iter.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7])
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            let indexOpt6 = system6.TryIndexCorrelatedToI entityId
            let indexOpt7 = system7.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 && indexOpt7 > -1 then
                this.Cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6, indexOpt7)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 || indexOpt7 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false

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
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> 'c Component> (isolated, buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (isolated, buffered, ecs)

    new (buffered, ecs) = SystemMultiplexed (false, buffered, ecs)
    new (ecs) = SystemMultiplexed (false, false, ecs)

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

    member this.RegisterMultiplexed ordered comp simplexName entityId =

        // register simplex
        this.RegisterCorrelated ordered Unchecked.defaultof<_> entityId |> ignore<'c ComponentMultiplexed ComponentRef>
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.RegisterMultiplexed (simplexName, comp)
        
        // register correlation with ecs
        match ecs.Correlations.TryGetValue entityId with
        | (true, correlation) ->
            correlation.Add this.Name |> ignore<bool>
            ecs.FilterForQueries this.Name entityId
        | (false, _) ->
            let correlation = HashSet.singleton StringComparer.Ordinal this.Name
            ecs.Correlations.Add (entityId, correlation)
            ecs.FilterForQueries this.Name entityId

        // fin
        componentMultiplexed

    member this.UnregisterMultiplexed simplexName entityId =

        // attempt to unregister simplex
        let unregistered =
            let componentMultiplexed = this.IndexCorrelated entityId
            componentMultiplexed.Index.UnregisterMultiplexed simplexName |> ignore<bool>
            this.UnregisterCorrelated entityId
        
        // unregister correlation
        if unregistered then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Name |> ignore<bool>
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                ecs.FilterForQueries this.Name entityId

        // fin
        unregistered

    type 'w EntityRef with
    
        member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName =
            let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            let multiplexed = system.IndexMultiplexed simplexName this.EntityId : 'c Simplex
            &multiplexed.Simplex
    
        member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> simplexName =
            let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            let multiplexed = system.IndexMultiplexedBuffered simplexName this.EntityId : 'c Simplex
            multiplexed.Simplex

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

/// A SystemHierarchical node.
type [<NoEquality; NoComparison>] NodeHierarchical<'c when 'c : struct and 'c :> 'c Component> =
    { EntityId : Guid
      ComponentRef : 'c ComponentRef}

/// Tracks changes in a hierarchical system.
type [<NoEquality; NoComparison; Struct>] ChangeHierarchical =
    { /// Whether change involves registeration (true) or unregistration (false).
      Registered : bool
      /// The node entity's id.
      NodeId : Guid
      /// The parent's entity id or Guid.Empty if no parent.
      ParentIdOpt : Guid }

/// An Ecs system that stores components in a hierarchy.
type SystemHierarchical<'c, 'w when 'c : struct and 'c :> 'c Component> (isolated, buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c, 'w> (isolated, buffered, ecs)

    let hierarchy = ListTree.makeEmpty<'c NodeHierarchical> ()
    let mutable hierarchyChanges = List<ChangeHierarchical> ()
    
    new (buffered, ecs) = SystemHierarchical (false, buffered, ecs)
    new (ecs) = SystemHierarchical (false, false, ecs)
    
    member this.Hierarchy = hierarchy
    member this.HierarchyFlattened = this.Correlateds
    member this.WithHierarchyFlattenedBuffered fn worldOpt = this.WithCorrelatedsBuffered fn worldOpt

    member this.PopHierarchyChanges () =
        let popped = hierarchyChanges
        hierarchyChanges <- List<ChangeHierarchical> ()
        popped

    member this.IndexHierarchical entityId =
        this.IndexCorrelated entityId

    member this.RegisterHierarchical ordered (parentIdOpt : Guid option) comp entityId =
        let this' = this :> SystemCorrelated<'c, 'w>
        let cref = this'.RegisterCorrelated ordered comp entityId
        let node = { EntityId = entityId; ComponentRef = cref }
        let addedOpt =
            match parentIdOpt with
            | Some parentId -> ListTree.tryInsert (fun node -> node.EntityId = parentId) node hierarchy
            | None -> ListTree.tryAdd tautology node hierarchy
        if Option.isSome addedOpt then
            let hierarchyChange = { Registered = true; NodeId = entityId; ParentIdOpt = Option.getOrDefault Guid.Empty parentIdOpt }
            hierarchyChanges.Add hierarchyChange
            Some cref
        else None

    member this.UnregisterHierarchical entityId =
        let result = ListTree.removeFirst (fun node -> node.EntityId = entityId) hierarchy
        if result then
            let hierarchyChage = { Registered = false; NodeId = entityId; ParentIdOpt = Guid.Empty }
            hierarchyChanges.Add hierarchyChage
        result

    member this.QualifyHierarchical entityId =
        this.QualifyCorrelated entityId

    override this.UnregisterComponent entityId =
        this.UnregisterHierarchical entityId

    type 'w EntityRef with
    
        member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> () =
            let system = this.EntityEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            let hierarchical = system.IndexHierarchical this.EntityId : 'c ComponentRef
            &hierarchical.Index
    
        member this.IndexHierarchicalBuffered<'c when 'c : struct and 'c :> 'c Component> () =
            let system = this.EntityEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            let hierarchical = system.IndexHierarchical this.EntityId : 'c ComponentRef
            hierarchical.IndexBuffered

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

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.RegisterHierarchical parentIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.UnregisterHierarchical parentIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> 'c Component> entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyHierarchical entityId

/// The type of a familial change.
type [<NoEquality; NoComparison; Struct>] FamilyChangeType =
    | MemberAdded
    | MemberRemoved
    | ComponentAddedToMember
    | ComponentRemovedFromMember

/// Tracks changes in a familial system.
type [<NoEquality; NoComparison; Struct>] FamilyChange =
    { FamilyChangeType : FamilyChangeType
      MemberId : Guid
      AncestorIdOpt : Guid }

/// An Ecs system that stores components in a family tree.
type SystemFamilial<'c, 'w when 'c : struct and 'c :> 'c Component> (isolated, buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let systemTree = ListTree.makeEmpty<SystemCorrelated<'c, 'w>> ()
    let systemDict = dictPlus<Guid, SystemCorrelated<'c, 'w>> HashIdentity.Structural []
    let mutable familyChanges = List<FamilyChange> ()
    
    new (buffered, ecs) = SystemFamilial (false, buffered, ecs)
    new (ecs) = SystemFamilial (false, false, ecs)

    member this.FreeListCount = systemDict.Values |> Seq.fold (fun count system -> count + system.FreeListCount) 0
    member this.EntitiesCorrelated = systemDict.Values |> Seq.map (fun system -> system.EntitiesCorrelated) |> Seq.concat

    member this.Family = systemTree |> ListTree.map (fun system -> system.Correlateds)
    member this.WithFamilyBuffered fn worldOpt = systemTree |> ListTree.map (fun system -> system.WithCorrelatedsBuffered fn worldOpt)
    
    member this.RewindFreeIndices () =
        systemDict.Values |> Seq.iter (fun system -> system.RewindFreeIndex ())

    member this.PopFamilyChanges () =
        let popped = familyChanges
        familyChanges <- List<FamilyChange> ()
        popped

    member this.IndexMember memberId =
        match systemDict.TryGetValue memberId with
        | (true, system) -> system.Correlateds
        | (false, _) -> failwith ("Member with id '" + scstring memberId + "' not found.")

    member this.AddMember (ancestorIdOpt : Guid option) =
        let memberId = Gen.id
        let system = SystemCorrelated<'c, 'w> (buffered, true, ecs)
        let addedOpt =
            match ancestorIdOpt with
            | Some ancestorId ->
                let ancestorIdStr = string ancestorId
                systemTree |> ListTree.tryInsert (fun system -> system.Name = ancestorIdStr) system
            | None ->
                systemTree |> ListTree.tryAdd tautology system
        if Option.isSome addedOpt then
            familyChanges.Add { FamilyChangeType = MemberAdded; MemberId = memberId; AncestorIdOpt = match ancestorIdOpt with Some pid -> pid | None -> Guid.Empty }
            systemDict.Add (memberId, system)
            Some memberId
        else None

    member this.RemoveMember memberId =

        // infer system name
        let systemName = string memberId

        // unregister all correlated components from member's system
        systemTree |>
        ListTree.findAll (fun system -> system.Name = systemName) |>
        Seq.iter (fun system ->
            system.EntitiesCorrelated |>
            Seq.iter (fun correlated ->
                familyChanges.Add { FamilyChangeType = ComponentRemovedFromMember; MemberId = memberId; AncestorIdOpt = correlated }
                system.UnregisterCorrelated correlated |> ignore<bool>))

        // remove member's system from tree
        systemTree |> ListTree.removeFirst (fun system -> system.Name = systemName) |> ignore<bool>

        // remove entry from dict
        let result = systemDict.Remove memberId

        // log change
        if result then familyChanges.Add { FamilyChangeType = MemberRemoved; MemberId = memberId; AncestorIdOpt = Guid.Empty }

        // fin
        result

    member this.QualifyFamilial memberId entityId =
        match systemDict.TryGetValue memberId with
        | (true, system) -> system.QualifyCorrelated entityId
        | (false, _) -> false

    member this.IndexFamilial memberId entityId =
        let (found, system) = systemDict.TryGetValue memberId
        if not found then raise (ArgumentOutOfRangeException "memberId")
        system.IndexCorrelated entityId

    member this.RegisterFamilial ordered comp memberId entityId =

        // attempt to register component
        let registered =
            match systemDict.TryGetValue memberId with
            | (true, system) ->
                system.RegisterCorrelated ordered comp entityId |> ignore<'c ComponentRef>
                true
            | (false, _) -> false

        // track registration
        if registered then
            familyChanges.Add { FamilyChangeType = ComponentAddedToMember; MemberId = memberId; AncestorIdOpt = entityId }

        // register correlation
        if registered && not isolated then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Add this.Name |> ignore<bool>
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                let correlation = HashSet.singleton StringComparer.Ordinal this.Name
                ecs.Correlations.Add (entityId, correlation)
                ecs.FilterForQueries this.Name entityId

        // fin
        registered

    member this.UnregisterFamilial memberId entityId =

        // attempt to unregister component
        let unregistered =
            match systemDict.TryGetValue memberId with
            | (true, system) -> system.UnregisterCorrelated entityId
            | (false, _) -> false

        // track registration
        if unregistered then
            familyChanges.Add { FamilyChangeType = ComponentRemovedFromMember; MemberId = memberId; AncestorIdOpt = entityId }

        // unregister correlation
        if unregistered && not isolated then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Name |> ignore<bool>
                ecs.FilterForQueries this.Name entityId
            | (false, _) ->
                ecs.FilterForQueries this.Name entityId

        // fin
        unregistered

    override this.UnregisterComponent entityId =
        systemTree |>
        ListTree.findAll (fun system -> system.Name = string entityId) |>
        Seq.map (fun system -> system.UnregisterCorrelated entityId) |>
        Seq.exists id

    type 'w EntityRef with
    
        member this.IndexFamilial<'c when 'c : struct and 'c :> 'c Component> memberId =
            let system = this.EntityEcs.IndexSystem<'c, SystemFamilial<'c, 'w>> ()
            let familial = system.IndexFamilial memberId this.EntityId : 'c ComponentRef
            &familial.Index
    
        member this.IndexFamilialBuffered<'c when 'c : struct and 'c :> 'c Component> memberId =
            let system = this.EntityEcs.IndexSystem<'c, SystemFamilial<'c, 'w>> ()
            let familial = system.IndexFamilial memberId this.EntityId : 'c ComponentRef
            familial.IndexBuffered

    type 'w Ecs with

        member this.IndexSystemFamilial<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemFamilial<'c, 'w>> ()

        member this.IndexFamily<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.Family
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.PopFamilyChanges<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.PopFamilyChanges ()
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexMember<'c when 'c : struct and 'c :> 'c Component> memberId =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.IndexMember memberId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.AddMember<'c when 'c : struct and 'c :> 'c Component> ancestorIdOpt =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.AddMember ancestorIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.RemoveMember<'c when 'c : struct and 'c :> 'c Component> ancestorIdOpt =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.RemoveMember ancestorIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.QualifyFamilial<'c when 'c : struct and 'c :> 'c Component> memberId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyFamilial memberId entityId

        member this.IndexFamilial<'c when 'c : struct and 'c :> 'c Component> memberId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexFamilial memberId entityId

        member this.RegisterFamilial<'c when 'c : struct and 'c :> 'c Component> comp memberId entityId =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.RegisterFamilial comp memberId entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterFamilial<'c when 'c : struct and 'c :> 'c Component> memberId entityId =
            match this.TryIndexSystem<'c, SystemFamilial<'c, 'w>> () with
            | Some system -> system.UnregisterFamilial memberId entityId
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] SynchronizeCorrelationChanges = "SynchronizeCorrelationChanges"
    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"