// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Prime

/// An array with additional indirection.
/// NOTE: Inlined everything for speed.
/// TODO: move to Prime?
type 'c ArrayRef =
    { mutable Array : 'c array }
    member inline this.Length with get () = this.Array.Length
    member inline this.Item i = &this.Array.[i]
    static member inline make n = { Array = Array.zeroCreate n }

/// The base component type of an Ecs.
type Component<'c when 'c : struct and 'c :> 'c Component> =
    interface
        abstract Active : bool with get, set
        abstract ShouldJunction : string HashSet -> bool
        abstract AllocateJunctions : 'w Ecs -> (string * obj * obj) array
        abstract ResizeJunctions : int -> obj array -> 'w Ecs -> unit
        abstract Junction : int -> obj array -> obj array -> Guid -> 'w Ecs -> 'c
        abstract Disjunction : int -> obj array -> Guid -> 'w Ecs -> unit
        abstract TypeName : string
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

    member inline this.Index
        with get () = &this.ComponentAref.[this.ComponentIndex]

    member inline this.IndexBuffered
        with get () =
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
/// systems. If anything, it might be better to renamed this type to Storage.
and [<AbstractClass>] 'w System (name) =
    member this.Name with get () : string = name
    abstract UnregisterComponent : Guid -> bool

/// Potentially-buffered array objects.
/// ArrayObjsUnbuffered will be refEq to ArrayObjsBuffered if buffering is disabled.
and [<NoEquality; NoComparison>] internal ArrayObjs =
    { ArrayObjsUnbuffered : obj List
      mutable ArrayObjsBuffered : obj List }

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
    let mutable correlationChanges = dictPlus<Guid, string HashSet> HashIdentity.Structural []
    let emptySystemNames = hashSetPlus<string> StringComparer.Ordinal [] // the empty systems dict to elide allocation on IndexSystemNames
    let emptyCorrelation = hashSetPlus<string> StringComparer.Ordinal [] // the empty correlation to elide allocation on IndexEntitiesChanged

    do this.RegisterSystemGeneralized systemGlobal |> ignore<'w System>

    member private this.BoxCallback<'a> (callback : SystemCallback<'a, 'w>) =
        let boxableCallback = fun (evt : SystemEvent<obj, 'w>) system ->
            let evt =
                { SystemEventData = evt.SystemEventData :?> 'a
                  SystemPublisher = evt.SystemPublisher }
            callback evt system
        boxableCallback :> obj

    member internal this.EmptyCorrelation
        with get () = emptyCorrelation

    member internal this.Correlations
        with get () = correlations

    member internal this.CorrelationChanges
        with get () = correlationChanges

    member this.SystemGlobal
        with get () = systemGlobal

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
        | (true, systemNames) -> systemNames
        | (false, _) -> emptySystemNames

    member this.IndexSystems entityId =
        this.IndexSystemNames entityId |>
        Seq.map this.IndexSystem

    member this.IndexEntities systemNames =
        correlations |>
        Seq.filter (fun kvp -> kvp.Value.IsSubsetOf systemNames) |>
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

    member this.PopCorrelationChanges () =
        let result = correlationChanges
        correlationChanges <- dictPlus<Guid, string HashSet> HashIdentity.Structural []
        result

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

    member this.AllocateJunction<'c when 'c : struct and 'c :> 'c Component> (fieldPath : string) =
        let componentName = Unchecked.defaultof<'c>.TypeName
        match arrayObjss.TryGetValue componentName with
        | (true, found) ->
            if refEq found.ArrayObjsUnbuffered found.ArrayObjsBuffered then
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                found.ArrayObjsUnbuffered.Add (box aref)
                (fieldPath, box aref, box aref)
            else
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                let arefBuffered = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                found.ArrayObjsUnbuffered.Add (box aref)
                found.ArrayObjsBuffered.Add (box arefBuffered)
                (fieldPath, box aref, box arefBuffered)
        | (false, _) -> failwith ("No array initially allocated for '" + componentName + "'. Did you neglect to register a system for this component?")

    member this.AllocateJunctions<'c when 'c : struct and 'c :> 'c Component> () =
        let comp = Unchecked.defaultof<'c>
        let junctionObjss = comp.AllocateJunctions this
        let (fieldPaths, junctions, buffereds) = Array.unzip3 junctionObjss
        (Array.zip fieldPaths junctions, Array.zip fieldPaths buffereds)

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

    member this.Component with get () = &comp

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

    member this.Components with get () = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock componentsBuffered (fun () -> fn componentsBuffered worldOpt)

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterUncorrelated (comp : 'c) =

        // ensure component is marked active
        let mutable comp = comp
        comp.Active <- true

        // assign component
        if freeList.Count > 0 then
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

        member this.RegisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> comp =
            match this.TryIndexSystem<'c, SystemUncorrelated<'c, 'w>> () with
            | Some system -> system.RegisterUncorrelated comp
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            match this.TryIndexSystem<'c, SystemUncorrelated<'c, 'w>> () with
            | Some system -> system.UnregisterUncorrelated index
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// An ECS system with components correlated by entity id.
/// Hashing and storing millions of entity ids is slow, so if you need to create that many components quickly, consider
/// manually junctioning unordered components instead. The trade-off to using uncorrelated components is that you
/// have to manually unregister their component refs and you have to allocate and deallocate them consecutively in
/// batches to ensure maximum throughput when processing the manually-junctioned components.
/// Also note that all junctions are guaranteed to keep the same size and order as the related components so that they
/// can be accessed by the same index.
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, isolated, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable (junctionsNamed, junctionsBufferedNamed) = ecs.AllocateJunctions<'c> ()
    let mutable (junctions, junctionsBuffered) = (Array.map snd junctionsNamed, Array.map snd junctionsBufferedNamed)
    let mutable (junctionsMapped, junctionsBufferedMapped) = (dictPlus HashIdentity.Structural junctionsNamed, dictPlus HashIdentity.Structural junctionsBufferedNamed)
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<Guid, int> HashIdentity.Structural []
    let correlationsBack = dictPlus<int, Guid> HashIdentity.Structural []

    new (ecs) = SystemCorrelated (false, false, ecs)

    member this.Correlateds with get () = correlateds
    member this.WithCorrelatedsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock correlatedsBuffered (fun () -> fn correlatedsBuffered worldOpt)

    member this.IndexJunction<'j when 'j : struct and 'j :> 'j Component> fieldPath = junctionsMapped.[fieldPath] :?> 'j ArrayRef
    member this.WithJunctionBuffered<'j when 'j : struct and 'j :> 'j Component> fn fieldPath (worldOpt : 'w option) = let junction = junctionsBufferedMapped.[fieldPath] :?> 'j ArrayRef in lock junction (fun () -> fn junction worldOpt)

    member this.IndexCorrelatedUnbuffered index = &correlateds.Array.[index]
    member this.IndexCorrelatedBuffered index = correlatedsBuffered.Array.[index]

    member this.IndexJunctionedUnbuffered fieldPath index = &(this.IndexJunction<'j> fieldPath).[index]
    member this.IndexJunctionedBuffered fieldPath index = (junctionsMapped.[fieldPath] :?> 'j ArrayRef).[index]

    member inline private this.IndexCorrelatedToI entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (ArgumentOutOfRangeException "entityId")
        index

    member this.GetEntitiesCorrelated () =
        correlations.Keys :> _ IEnumerable

    member this.QualifyCorrelated entityId =
        correlations.ContainsKey entityId

    member this.IndexCorrelated entityId =
        let index = this.IndexCorrelatedToI entityId
        ComponentRef<'c>.make index correlateds correlatedsBuffered

    member this.IndexJunctioned<'j when 'j : struct and 'j :> 'j Component> fieldPath entityId =
        let index = this.IndexCorrelatedToI entityId
        let junction = junctionsMapped.[fieldPath] :?> 'j ArrayRef
        let buffered = junctionsBufferedMapped.[fieldPath] :?> 'j ArrayRef
        ComponentRef<'j>.make index junction buffered

    member this.RegisterCorrelated (comp : 'c) entityId =

        // activate component
        let mutable comp = comp
        comp.Active <- true

        // check if component is already registered
        if not (correlations.ContainsKey entityId) then

            // allocate index for component, enlarging arrays if necessary
            let index =
                if freeIndex >= correlateds.Length then
                    if freeList.Count <> 0 then
                        let index = Seq.head freeList
                        freeList.Remove index |> ignore<bool>
                        index
                    else
                        let length = correlateds.Length * Constants.Ecs.ArrayGrowth
                        let arr = Array.zeroCreate length in correlateds.Array.CopyTo (arr, 0); correlateds.Array <- arr
                        comp.ResizeJunctions length junctions ecs
                        lock correlatedsBuffered (fun () -> let arr = Array.zeroCreate length in correlatedsBuffered.Array.CopyTo (arr, 0); correlatedsBuffered.Array <- arr)
                        lock junctionsBuffered (fun () -> comp.ResizeJunctions length junctionsBuffered ecs)
                        let index = freeIndex
                        freeIndex <- inc freeIndex
                        index
                else
                    let index = freeIndex
                    freeIndex <- inc freeIndex
                    index

            // allocate component
            let comp = comp.Junction index junctions junctionsBuffered entityId ecs
            correlations.Add (entityId, index)
            correlationsBack.Add (index, entityId)
            correlateds.Array.[index] <- comp

            // register correlation with ecs if needed
            if not isolated then
                match ecs.Correlations.TryGetValue entityId with
                | (true, correlation) ->
                    correlation.Add this.Name |> ignore<bool>
                    ecs.CorrelationChanges.[entityId] <- correlation
                | (false, _) ->
                    ecs.Correlations.Add (entityId, HashSet.singleton StringComparer.Ordinal this.Name)
                    ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

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
                Unchecked.defaultof<'c>.Disjunction index junctions entityId ecs

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
                ecs.CorrelationChanges.[entityId] <- correlation
            | (false, _) ->
                ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

        // fin
        unregistered

        override this.UnregisterComponent entityId =
            this.UnregisterCorrelated entityId

    type 'w Ecs with

        member this.IndexSystemCorrelated<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()

        member this.GetEntitiesCorrelated<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system -> system.GetEntitiesCorrelated ()
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

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

        member inline this.IndexJunctioned<'c, 'j when 'c : struct and 'c :> 'c Component and 'j : struct and 'j :> 'j Component> fieldPath entityId : 'j ComponentRef =
            let systemOpt = this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexJunctioned<'j> fieldPath entityId

        member this.RegisterCorrelated<'c when 'c : struct and 'c :> 'c Component> comp entityId =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system -> system.RegisterCorrelated comp entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system -> system.UnregisterCorrelated entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated (systemName, entityId) =
            match this.TryIndexSystem systemName with
            | Some system -> system.UnregisterComponent entityId
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.SynchronizeCorrelated<'c when 'c : struct and 'c :> 'c Component> (systemNames : string HashSet) entityId =
            if systemNames.Contains Unchecked.defaultof<'c>.TypeName then
                if not (Unchecked.defaultof<'c>.ShouldJunction systemNames) then
                    this.UnregisterCorrelated<'c> entityId |> ignore<bool>
                    Unregistered
                else Unchanged true
            else
                if Unchecked.defaultof<'c>.ShouldJunction systemNames then
                    this.RegisterCorrelated Unchecked.defaultof<'c> entityId |> ignore<'c ComponentRef>
                    Registered
                else Unchanged false

        member this.JunctionPlus<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) (index : int) (componentsObj : obj) (componentsBufferedObj : obj) =

            // ensure component is active
            let mutable comp = comp
            comp.Active <- true

            // set component
            let components = componentsObj :?> 'c ArrayRef
            let componentsBuffered = componentsBufferedObj :?> 'c ArrayRef
            components.[index] <- comp
            ComponentRef<'c>.make index components componentsBuffered
            
        member this.Junction<'c when 'c : struct and 'c :> 'c Component> index componentsObj componentsBufferedObj =
            this.JunctionPlus<'c> Unchecked.defaultof<'c> index componentsObj componentsBufferedObj

        member this.Disjunction<'c when 'c : struct and 'c :> 'c Component> (index : int) (componentsObj : obj) =
            let components = componentsObj :?> 'c ArrayRef
            components.[index].Active <- false

        member this.ResizeJunction<'c when 'c : struct and 'c :> 'c Component> (size : int) (componentsObj : obj) =
            let components = componentsObj :?> 'c ArrayRef
            let arr = Array.zeroCreate<'c> size
            Array.blit components.Array 0 arr 0 (min components.Array.Length arr.Length)
            components.Array <- arr

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
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.ShouldJunction _ = true
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.Junction _ _ _ _ _ = this
        member this.Disjunction _ _ _ _ = ()
        member this.TypeName = getTypeName this
    member this.RegisterMultiplexed (simplexName, comp) =
        this.Simplexes.Add (simplexName, { Simplex = comp })
    member this.UnregisterMultiplexed simplexName =
        this.Simplexes.Remove simplexName

/// An ECS system that stores zero to many of the same component per entity id.
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, isolated, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (buffered, isolated, ecs)

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

    member this.RegisterMultiplexed comp simplexName entityId =

        // register simplex
        this.RegisterCorrelated Unchecked.defaultof<_> entityId |> ignore<'c ComponentMultiplexed ComponentRef>
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.RegisterMultiplexed (simplexName, comp)
        
        // register correlation with ecs
        match ecs.Correlations.TryGetValue entityId with
        | (true, correlation) ->
            correlation.Add this.Name |> ignore<bool>
            ecs.CorrelationChanges.[entityId] <- correlation
        | (false, _) ->
            ecs.Correlations.Add (entityId, HashSet.singleton StringComparer.Ordinal this.Name)
            ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

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
                ecs.CorrelationChanges.[entityId] <- correlation
            | (false, _) ->
                ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

        // fin
        unregistered

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

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> comp simplexName entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system -> system.RegisterMultiplexed comp simplexName entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system -> system.UnregisterMultiplexed simplexName entityId
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// Tracks changes in a hierarchical system.
type HierarchyChange =
    | NodeAdded of Guid * Guid option
    | NodeRemoved of Guid // TODO: try to figure out how to add parentId option here.
    | ComponentAddedToNode of Guid * Guid
    | ComponentRemovedFromNode of Guid * Guid

/// An Ecs system that stores components in a tree hierarchy.
type SystemHierarchical<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, isolated, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let systemTree = ListTree.makeEmpty<SystemCorrelated<'c, 'w>> ()
    let systemDict = dictPlus<Guid, SystemCorrelated<'c, 'w>> HashIdentity.Structural []
    let mutable hierarchyChanges = List<HierarchyChange> ()

    new (ecs) = SystemHierarchical (false, false, ecs)

    member this.Hierarchy with get () =
        systemTree |> ListTree.map (fun system -> system.Correlateds)

    member this.WithHierarchyBuffered fn worldOpt =
        systemTree |> ListTree.map (fun system -> system.WithCorrelatedsBuffered fn worldOpt)

    member this.PopHierarchyChanges () =
        let popped = hierarchyChanges
        hierarchyChanges <- List<HierarchyChange> ()
        popped

    member this.IndexNode nodeId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.Correlateds
        | (false, _) -> failwith ("Node with id '" + scstring nodeId + "' not found.")

    member this.AddNode (parentIdOpt : Guid option) =
        let nodeId = Gen.id
        let system = SystemCorrelated<'c, 'w> (buffered, true, ecs)
        let addedOpt =
            match parentIdOpt with
            | Some parentId ->
                let parentIdStr = string parentId
                systemTree |> ListTree.tryInsert (fun system -> system.Name = parentIdStr) system
            | None ->
                systemTree |> ListTree.tryAdd tautology system
        if Option.isSome addedOpt then
            hierarchyChanges.Add (NodeAdded (nodeId, parentIdOpt))
            systemDict.Add (nodeId, system)
            Some nodeId
        else None

    member this.RemoveNode nodeId =

        // infer system name
        let systemName = string nodeId

        // unregister all correlated components from node's system
        systemTree |>
        ListTree.findAll (fun system -> system.Name = systemName) |>
        Seq.iter (fun system ->
            system.GetEntitiesCorrelated () |>
            Seq.iter (fun correlated ->
                hierarchyChanges.Add (ComponentRemovedFromNode (nodeId, correlated))
                system.UnregisterCorrelated correlated |> ignore<bool>))

        // remove node's system from tree
        systemTree |> ListTree.removeFirst (fun system -> system.Name = systemName) |> ignore<bool>

        // remove entry from dict
        let result = systemDict.Remove nodeId

        // log change
        if result then hierarchyChanges.Add (NodeRemoved nodeId)

        // fin
        result

    member this.QualifyHierarchical nodeId entityId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.QualifyCorrelated entityId
        | (false, _) -> false

    member this.IndexHierarchical nodeId entityId =
        let (found, system) = systemDict.TryGetValue nodeId
        if not found then raise (ArgumentOutOfRangeException "nodeId")
        system.IndexCorrelated entityId

    member this.RegisterHierarchical comp nodeId entityId =

        // attempt to register component
        let registered =
            match systemDict.TryGetValue nodeId with
            | (true, system) ->
                system.RegisterCorrelated comp entityId |> ignore<'c ComponentRef>
                true
            | (false, _) -> false

        // track registration
        if registered then
            hierarchyChanges.Add (ComponentAddedToNode (nodeId, entityId))

        // register correlation
        if registered && not isolated then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Add this.Name |> ignore<bool>
                ecs.CorrelationChanges.[entityId] <- correlation
            | (false, _) ->
                ecs.Correlations.Add (entityId, HashSet.singleton StringComparer.Ordinal this.Name)
                ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

        // fin
        registered

    member this.UnregisterHierarchical nodeId entityId =

        // attempt to unregister component
        let unregistered =
            match systemDict.TryGetValue nodeId with
            | (true, system) -> system.UnregisterCorrelated entityId
            | (false, _) -> false

        // track registration
        if unregistered then
            hierarchyChanges.Add (ComponentAddedToNode (nodeId, entityId))

        // unregister correlation
        if unregistered && not isolated then
            match ecs.Correlations.TryGetValue entityId with
            | (true, correlation) ->
                correlation.Remove this.Name |> ignore<bool>
                ecs.CorrelationChanges.[entityId] <- correlation
            | (false, _) ->
                ecs.CorrelationChanges.[entityId] <- ecs.EmptyCorrelation

        // fin
        unregistered

    override this.UnregisterComponent entityId =
        systemTree |>
        ListTree.findAll (fun system -> system.Name = string entityId) |>
        Seq.map (fun system -> system.UnregisterCorrelated entityId) |>
        Seq.exists id

    type 'w Ecs with

        member this.IndexSystemHierarchical<'c when 'c : struct and 'c :> 'c Component> () =
            this.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()

        member this.IndexHierarchy<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.Hierarchy
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.PopHierarchyChanges<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.PopHierarchyChanges ()
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.IndexNode<'c when 'c : struct and 'c :> 'c Component> nodeId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.IndexNode nodeId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.AddNode<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.AddNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.RemoveNode<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.RemoveNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyHierarchical nodeId entityId

        member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexHierarchical nodeId entityId

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> 'c Component> comp nodeId entityId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.RegisterHierarchical comp nodeId entityId
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.UnregisterHierarchical nodeId entityId
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// A correlated entity reference.
/// Slow relative to normal ECS operations, but convenient for one-off uses.
type [<NoEquality; NoComparison; Struct>] 'w EntityRef =
    { EntityId : Guid
      EntityEcs : 'w Ecs }

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        let system = this.EntityEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
        &correlated.Index

    member this.IndexBuffered<'c when 'c : struct and 'c :> 'c Component> () =
        let system = this.EntityEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
        let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
        correlated.IndexBuffered

    member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> simplexName =
        let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
        let multiplexed = system.IndexMultiplexed simplexName this.EntityId : 'c Simplex
        &multiplexed.Simplex

    member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> simplexName =
        let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
        let multiplexed = system.IndexMultiplexedBuffered simplexName this.EntityId : 'c Simplex
        multiplexed.Simplex

    member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
        let hierarchical = system.IndexHierarchical nodeId this.EntityId : 'c ComponentRef
        &hierarchical.Index

    member this.IndexHierarchicalBuffered<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
        let hierarchical = system.IndexHierarchical nodeId this.EntityId : 'c ComponentRef
        hierarchical.IndexBuffered

    type 'w Ecs with
        member this.GetEntityRef entityId =
            { EntityId = entityId; EntityEcs = this }

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] SynchronizeCorrelationChanges = "SynchronizeCorrelationChanges"
    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"