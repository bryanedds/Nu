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
        abstract AllocateJunctions : 'w Ecs -> (string * obj * obj) array
        abstract ResizeJunctions : int -> obj array -> 'w Ecs -> unit
        abstract MoveJunctions : int -> int -> obj array -> 'w Ecs -> unit
        abstract Junction : int -> obj array -> obj array -> 'w Ecs -> 'c
        abstract Disjunction : int -> obj array -> 'w Ecs -> unit
        abstract TypeName : string
        end

/// A storable reference to a component in its containing array.
/// DO NOT access the elements of ComponentArefBuffered without locking the field itself!
/// OPTIMIZATION: Inlined everything for speed.
/// TODO: test threaded / buffered ECS functionality.
and [<NoEquality; NoComparison; Struct>] ComponentRef<'c when 'c : struct and 'c :> 'c Component> =
    { /// The associated component's index.
      ComponentIndex : int
      /// The associated component array reference.
      ComponentAref : 'c ArrayRef
#if ECS_BUFFERED
      /// DO NOT access the elements of ComponentArefBuffered without locking the field itself!
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

/// A base system type of an ECS.
and 'w System (name : string) =
    let pipedKey = Gen.id
    member this.PipedKey with get () = pipedKey
    abstract PipedInit : obj
    default this.PipedInit with get () = () :> obj
    member this.Name with get () = name

/// Potentially-buffered array objects.
/// ArrayObjsUnbuffered will be refEq to ArrayObjsBuffered if buffering is disabled.
and [<NoEquality; NoComparison>] internal ArrayObjs =
    { ArrayObjsUnbuffered : obj List
      mutable ArrayObjsBuffered : obj List }

/// Nu's custom Entity-Component-System implementation.
/// Nu's conception of an ECS is primarily as an abstraction over user-definable storage formats.
/// The default formats include SoA-style formats for non-correlated, correlated, multiplexed, and hierarchichal value types.
/// User can add formats of their own design by implementing the 'w System interface and providing related extension methods
/// on this 'w Ecs type.
and 'w Ecs () as this =

    let mutable systemCached = System<'w> typeof<unit>.Name
    let systemGlobal = systemCached
    let arrayObjss = dictPlus<string, ArrayObjs> StringComparer.Ordinal []
    let systemSubscriptions = dictPlus<string, Dictionary<Guid, obj>> StringComparer.Ordinal []
    let systemsUnordered = dictPlus<string, 'w System> StringComparer.Ordinal []
    let systemsOrdered = List<string * 'w System> ()
    let correlations = dictPlus<Guid, string List> HashIdentity.Structural []
    let pipedValues = ConcurrentDictionary<Guid, obj> ()

    do this.RegisterSystemGeneralized systemGlobal |> ignore<'w System>

    member private this.BoxCallback<'a> (callback : SystemCallback<'a, 'w>) =
        let boxableCallback = fun (evt : SystemEvent<obj, 'w>) system ->
            let evt =
                { SystemEventData = evt.SystemEventData :?> 'a
                  SystemPublisher = evt.SystemPublisher }
            callback evt system
        boxableCallback :> obj

    member private this.Correlations 
        with get () = correlations

    member this.SystemGlobal
        with get () = systemGlobal

    member this.RegisterPipedValue<'a when 'a : not struct> key (value : 'a) =
        pipedValues.[key] <- value :> obj

    member this.WithPipedValue<'a when 'a : not struct> key fn (worldOpt : 'w option) : 'w option =
        let pipedValue = pipedValues.[key] :?> 'a
        lock pipedValue (fun () -> fn pipedValue worldOpt)

    member this.RegisterSystemGeneralized (system : 'w System) : 'w System =
        systemsUnordered.Add (system.Name, system)
        systemsOrdered.Add (system.Name, system)
        this.RegisterPipedValue<obj> system.PipedKey system.PipedInit
        system

    member this.TryIndexSystem<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w System> () =
        let systemName = Unchecked.defaultof<'c>.TypeName
        if systemCached.Name = systemName then
            match systemCached with
            | :? 's as systemAsS -> Some systemAsS
            | _ -> None
        else
            match systemsUnordered.TryGetValue systemName with
            | (true, system) ->
                match system with
                | :? 's as systemAsS ->
                    systemCached <- system
                    Some systemAsS
                | _ -> None
            | (false, _) -> None

    member this.IndexSystem<'c, 's when 'c : struct and 'c :> 'c Component and 's :> 'w System> () =
        let systemName = Unchecked.defaultof<'c>.TypeName
        if systemCached.Name = systemName
        then systemCached :?> 's
        else
            match systemsUnordered.TryGetValue systemName with
            | (true, system) ->
                let systemAsS = system :?> 's
                systemCached <- system
                systemAsS
            | (false, _) -> failwith ("Could not index system '" + systemName + "' of type '" + typeof<'s>.Name + "'.")

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

    type 'w System with
        member this.RegisterPipedValue (ecs : 'w Ecs) = ecs.RegisterPipedValue<obj> this.PipedKey this.PipedInit
        member this.WithPipedValue<'a when 'a : not struct> fn (ecs : 'w Ecs) worldOpt = ecs.WithPipedValue<'a> this.PipedKey fn worldOpt

[<Extension>]
type EcsExtensions =

    [<Extension>]
    static member RegisterSystem<'s, 'w when 's :> 'w System> (this : 'w Ecs, system : 's) : 's =
        this.RegisterSystemGeneralized system :?> 's

/// An Ecs system with just a single component.
type SystemSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> (comp : 'c) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)
    let mutable comp = comp
    member this.Component with get () = &comp
    type 'w Ecs with
        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> () =
            let systemOpt = this.TryIndexSystem<'c, SystemSingleton<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// An Ecs system with components stored by an integer index in an unordered fashion.
type SystemUnordered<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural

    new (ecs) = SystemUnordered (false, ecs)

    member this.Components with get () = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock componentsBuffered (fun () -> fn componentsBuffered worldOpt)

    member this.IndexUnordered index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterUnordered (comp : 'c) =

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

    member this.UnregisterUnordered index =
        if index <> freeIndex then
            components.[index].Active <- false
            freeList.Add index |> ignore<bool>
        else freeIndex <- dec freeIndex

    type 'w Ecs with

        member this.IndexUnordered<'c when 'c : struct and 'c :> 'c Component> index =
            let systemOpt = this.TryIndexSystem<'c, SystemUnordered<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.IndexUnordered index

        member this.RegisterUnordered<'c when 'c : struct and 'c :> 'c Component> comp =
            match this.TryIndexSystem<'c, SystemUnordered<'c, 'w>> () with
            | Some system -> system.RegisterUnordered comp
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterUnordered<'c when 'c : struct and 'c :> 'c Component> index =
            match this.TryIndexSystem<'c, SystemUnordered<'c, 'w>> () with
            | Some system -> system.UnregisterUnordered index
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// An Ecs system with components correlated by entity id.
/// Hashing and storing millions of entity ids is slow, so if need to create that many components quickly, consider
/// manually junctioning unordered components instead. The trade-off to using uncorrelated components is that you
/// have to manually unregister their component refs and you have to allocate and deallocate them consecutively in
/// batches to ensure maximum throughput when processing the manually-junctioned components.
/// Also note that all junctions are guaranteed to keep the same size and order as the related components so that they
/// can be accessed by the same index.
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable (junctionsNamed, junctionsBufferedNamed) = ecs.AllocateJunctions<'c> ()
    let mutable (junctions, junctionsBuffered) = (Array.map snd junctionsNamed, Array.map snd junctionsBufferedNamed)
    let mutable junctionsMapped = dictPlus HashIdentity.Structural junctionsNamed
    let mutable junctionsBufferedMapped = dictPlus HashIdentity.Structural junctionsBufferedNamed
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<Guid, int> HashIdentity.Structural []
    let correlationsBack = dictPlus<int, Guid> HashIdentity.Structural []

    new (ecs) = SystemCorrelated (false, ecs)

    member this.Correlateds with get () = correlateds
    member this.WithCorrelatedsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock correlatedsBuffered (fun () -> fn correlatedsBuffered worldOpt)

    member this.IndexJunction<'j when 'j : struct and 'j :> 'j Component> fieldPath = junctionsMapped.[fieldPath] :?> 'j ArrayRef
    member this.WithJunctionBuffered<'j when 'j : struct and 'j :> 'j Component> fn fieldPath (worldOpt : 'w option) = let junction = junctionsBufferedMapped.[fieldPath] :?> 'j ArrayRef in lock junction (fun () -> fn junction worldOpt)

    member this.IndexCorrelatedUnbuffered index = &correlateds.Array.[index]
    member this.IndexCorrelatedBuffered index = correlatedsBuffered.Array.[index]

    member this.IndexJunctionedUnbuffered fieldPath index = &(this.IndexJunction<'j> fieldPath).[index]
    member this.IndexJunctionedBuffered fieldPath index = (junctionsMapped.[fieldPath] :?> 'j ArrayRef).[index]

    member private this.Compact ecs =

        // compact array
        // TODO: P1: step-debug this.
        let mutable i = 0
        let mutable j = 1
        while j < freeIndex do

            // check if slot is free
            if not correlateds.[i].Active && freeList.Contains i then

                // find next non-free component
                while
                    j < freeIndex &&
                    not correlateds.[j].Active &&
                    not (freeList.Contains j) do
                    j <- inc j

                // move components
                correlateds.[i] <- correlateds.[j]
                correlateds.[0].MoveJunctions j i junctions ecs

                // update book-keeping
                match correlationsBack.TryGetValue j with
                | (true, entityId) ->
                    correlations.[entityId] <- i
                    correlationsBack.Remove j |> ignore<bool>
                    correlationsBack.Add (i, entityId)
                | (false, _) -> failwithumf ()

                // loop
                j <- inc j
            i <- inc i

        // update book-keeping
        freeList.Clear ()
        freeIndex <- j

    member inline this.IndexCorrelatedInt entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (ArgumentOutOfRangeException "entityId")
        index

    member this.GetEntitiesCorrelated () =
        correlations.Keys :> _ IEnumerable

    member this.QualifyCorrelated entityId =
        correlations.ContainsKey entityId

    member this.IndexCorrelated entityId =
        let index = this.IndexCorrelatedInt entityId
        ComponentRef<'c>.make index correlateds correlatedsBuffered

    member this.IndexJunctioned<'j when 'j : struct and 'j :> 'j Component> fieldPath entityId =
        let index = this.IndexCorrelatedInt entityId
        let junction = junctionsMapped.[fieldPath] :?> 'j ArrayRef
        let buffered = junctionsBufferedMapped.[fieldPath] :?> 'j ArrayRef
        ComponentRef<'j>.make index junction buffered

    member this.RegisterCorrelated (comp : 'c) entityId ecs =

        // ensure component is marked active
        let mutable comp = comp
        comp.Active <- true

        // check if component is already registered
        if not (correlations.ContainsKey entityId) then

            // ensure there is space in the arrays
            if freeIndex >= correlateds.Length then
                let length = correlateds.Length * Constants.Ecs.ArrayGrowth
                let arr = Array.zeroCreate length in correlateds.Array.CopyTo (arr, 0); correlateds.Array <- arr
                comp.ResizeJunctions length junctions ecs
                lock correlatedsBuffered (fun () ->
                    let arr = Array.zeroCreate length in correlatedsBuffered.Array.CopyTo (arr, 0); correlatedsBuffered.Array <- arr)
                lock junctionsBuffered (fun () ->
                    comp.ResizeJunctions length junctionsBuffered ecs)

            // allocate component
            let index = freeIndex in freeIndex <- inc freeIndex
            let comp = comp.Junction index junctions junctionsBuffered ecs
            correlations.Add (entityId, index)
            correlationsBack.Add (index, entityId)
            correlateds.Array.[index] <- comp

            // make component ref
            ComponentRef.make index correlateds correlatedsBuffered

        // component is already registered
        else failwith ("Component registered multiple times for entity '" + string entityId + "'.")

    member this.UnregisterCorrelated entityId ecs =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            let comp = correlateds.[index]
            if  index <> freeIndex then
                correlateds.[index].Active <- false
                freeList.Add index |> ignore<bool>
            else freeIndex <- dec freeIndex
            correlations.Remove entityId |> ignore<bool>
            correlationsBack.Remove index |> ignore<bool>
            comp.Disjunction index junctions ecs
            if  correlateds.Length < freeList.Count * 2 && // freeList is always empty if unordered
                correlateds.Length > Constants.Ecs.ArrayReserve then
                this.Compact ecs
            true
        | (false, _) -> false

    type 'w Ecs with

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
            | Some system ->
                let componentRef = system.RegisterCorrelated comp entityId this
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add Unchecked.defaultof<'c>.TypeName
                | (false, _) -> this.Correlations.Add (entityId, List [Unchecked.defaultof<'c>.TypeName])
                componentRef
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            match this.TryIndexSystem<'c, SystemCorrelated<'c, 'w>> () with
            | Some system ->
                let result = system.UnregisterCorrelated entityId this
                if result then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Remove Unchecked.defaultof<'c>.TypeName |> ignore<bool>
                    | (false, _) -> ()
                result
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.JunctionPlus<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) (index : int) (componentsObj : obj) (componentsBufferedObj : obj) =

            // ensure component is marked active
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

        member this.MoveJunction<'c when 'c : struct and 'c :> 'c Component> (src : int) (dst : int) (componentsObj : obj) =
            let components = componentsObj :?> 'c ArrayRef
            components.[dst] <- components.[src]

/// Handle to one of an array of multiplexed components.
type Simplex<'c when 'c : struct> =
    { mutable Simplex : 'c }

/// Allows an entity to contain multiple of the same component.
/// However, it uses a dictionary without a small-object optimization, so this functionality won't get the typical
/// perf benefits of data-orientation. Really, this functionality is here for flexibility and convenience more than
/// anything else (which is good enough in almost all cases where multi-components are used). Just make sure to
/// interface with the involved systems directly rather than through the Ecs API which indexes them implcitly.
type [<NoEquality; NoComparison; Struct>] ComponentMultiplexed<'c when 'c : struct and 'c :> 'c Component> =
    { mutable Active : bool
      Simplexes : Dictionary<Guid, 'c Simplex>
      TypeName : string }
    interface Component<'c ComponentMultiplexed> with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.MoveJunctions _ _ _ _ = ()
        member this.Junction _ _ _ _ = this
        member this.Disjunction _ _ _ = ()
        member this.TypeName = getTypeName this
    member this.RegisterMultiplexed (multiId, comp) =
        this.Simplexes.Add (multiId, { Simplex = comp })
    member this.UnregisterMultiplexed multiId =
        this.Simplexes.Remove multiId

/// An Ecs system that stores multiple components per entity id.
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (buffered, ecs)

    new (ecs) = SystemMultiplexed (false, ecs)

    member this.QualifyMultiplexed multiId entityId =
        if this.QualifyCorrelated entityId then
            let comp = this.IndexCorrelated entityId
            comp.Index.Simplexes.ContainsKey multiId
        else false

    member this.IndexMultiplexed multiId entityId =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.Simplexes.[multiId]

    member this.IndexMultiplexedBuffered multiId entityId =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.IndexBuffered.Simplexes.[multiId]

    member this.RegisterMultiplexed comp multiId entityId ecs =
        let _ = this.RegisterCorrelated Unchecked.defaultof<_> entityId ecs
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.RegisterMultiplexed (multiId, comp)
        componentMultiplexed

    member this.UnregisterMultiplexed multiId entityId ecs =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.UnregisterMultiplexed multiId |> ignore<bool>
        this.UnregisterCorrelated entityId ecs

    type 'w Ecs with

        member this.QualifyMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            system.QualifyMultiplexed multiId entityId

        member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexed multiId entityId
            &simplex.Simplex

        member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemOpt = this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexedBuffered multiId entityId
            simplex.Simplex

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> comp multiId entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system ->
                let _ = system.RegisterMultiplexed comp multiId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add Unchecked.defaultof<'c>.TypeName
                | (false, _) -> this.Correlations.Add (entityId, List [Unchecked.defaultof<'c>.TypeName])
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            match this.TryIndexSystem<'c, SystemMultiplexed<'c, 'w>> () with
            | Some system ->
                if system.UnregisterMultiplexed multiId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add Unchecked.defaultof<'c>.TypeName
                    | (false, _) -> this.Correlations.Add (entityId, List [Unchecked.defaultof<'c>.TypeName])
            | _ -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

/// An Ecs system that stores components in a tree hierarchy.
type SystemHierarchical<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) =
    inherit System<'w> (Unchecked.defaultof<'c>.TypeName)

    let systemTree = ListTree.makeEmpty<SystemCorrelated<'c, 'w>> ()
    let systemDict = dictPlus<Guid, SystemCorrelated<'c, 'w>> HashIdentity.Structural []

    new (ecs) = SystemHierarchical (false, ecs)

    member this.Components with get () = systemTree |> ListTree.map (fun system -> system.Correlateds)
    member this.WithComponentsBuffered fn worldOpt = systemTree |> ListTree.map (fun system -> system.WithCorrelatedsBuffered fn worldOpt)

    member this.IndexNode nodeId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.Correlateds
        | (false, _) -> failwith ("Node with id '" + scstring nodeId + "' not found.")

    member this.AddNode (parentIdOpt : Guid option) =
        let nodeId = Gen.id
        let system = SystemCorrelated<'c, 'w> (buffered, ecs)
        let added =
            match parentIdOpt with
            | Some parentId ->
                let parentIdStr = scstring parentId
                systemTree |> ListTree.tryInsert (fun system -> system.Name = parentIdStr) system
            | None ->
                systemTree |> ListTree.tryAdd tautology system
        if Option.isSome added then
            systemDict.Add (nodeId, system)
            Some nodeId
        else None

    member this.RemoveNode nodeId =
        let nodeIdStr = scstring nodeId
        let _ = systemTree |> ListTree.removeFirst (fun system -> system.Name = nodeIdStr)
        systemDict.Remove nodeId

    member this.QualifyHierarchical nodeId entityId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.QualifyCorrelated entityId
        | (false, _) -> false

    member this.IndexHierarchical nodeId entityId =
        let (found, system) = systemDict.TryGetValue nodeId
        if not found then raise (ArgumentOutOfRangeException "nodeId")
        system.IndexCorrelated entityId

    member this.RegisterHierarchical comp nodeId entityId ecs =
        match systemDict.TryGetValue nodeId with
        | (true, system) ->
            let _ = system.RegisterCorrelated comp entityId ecs
            true
        | (false, _) -> false

    member this.UnregisterHierarchical nodeId entityId ecs =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.UnregisterCorrelated entityId ecs
        | (false, _) -> false

    type 'w Ecs with

        member this.IndexTree<'c when 'c : struct and 'c :> 'c Component> () =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system -> system.Components
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
            | Some system ->
                let _ = system.RegisterHierarchical comp nodeId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add Unchecked.defaultof<'c>.TypeName
                | (false, _) -> this.Correlations.Add (entityId, List [Unchecked.defaultof<'c>.TypeName])
            | None -> failwith ("Could not find expected system '" + Unchecked.defaultof<'c>.TypeName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            match this.TryIndexSystem<'c, SystemHierarchical<'c, 'w>> () with
            | Some system ->
                if system.UnregisterHierarchical nodeId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add Unchecked.defaultof<'c>.TypeName
                    | (false, _) -> this.Correlations.Add (entityId, List [Unchecked.defaultof<'c>.TypeName])
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

    member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId =
        let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
        let multiplexed = system.IndexMultiplexed multiId this.EntityId : 'c Simplex
        &multiplexed.Simplex

    member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> multiId =
        let system = this.EntityEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
        let multiplexed = system.IndexMultiplexedBuffered multiId this.EntityId : 'c Simplex
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

/// A correlated entity reference that memoizes a portion of its component queries to eliminate intermediate look-ups.
type [<NoEquality; NoComparison; Struct>] 'w EntityMemo =
    { EntityMemoCache : Dictionary<Guid, struct ('w System * int)>
      EntityMemoId : Guid
      EntityMemoEcs : 'w Ecs }

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        match this.EntityMemoCache.TryGetValue this.EntityMemoId with
        | (true, struct (system, index)) ->
            let system = system :?> SystemCorrelated<'c, 'w>
            &system.IndexCorrelatedUnbuffered index
        | (false, _) ->
            let system = this.EntityMemoEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            let index = system.IndexCorrelatedInt this.EntityMemoId
            this.EntityMemoCache.Add (this.EntityMemoId, struct (system :> 'w System, index))
            &system.IndexCorrelatedUnbuffered index

    member this.IndexBuffered<'c when 'c : struct and 'c :> 'c Component> () =
        match this.EntityMemoCache.TryGetValue this.EntityMemoId with
        | (true, struct (system, index)) ->
            let system = system :?> SystemCorrelated<'c, 'w>
            system.IndexCorrelatedBuffered index
        | (false, _) ->
            let system = this.EntityMemoEcs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
            let index = system.IndexCorrelatedInt this.EntityMemoId
            this.EntityMemoCache.Add (this.EntityMemoId, struct (system :> 'w System, index))
            system.IndexCorrelatedBuffered index

    member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId =
        match this.EntityMemoCache.TryGetValue this.EntityMemoId with
        | (true, struct (system, index)) ->
            let system = system :?> SystemMultiplexed<'c, 'w>
            &(system.IndexCorrelatedUnbuffered index).Simplexes.[multiId].Simplex
        | (false, _) ->
            let system = this.EntityMemoEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            let index = system.IndexCorrelatedInt this.EntityMemoId
            this.EntityMemoCache.Add (this.EntityMemoId, struct (system :> 'w System, index))
            &(system.IndexCorrelatedUnbuffered index).Simplexes.[multiId].Simplex

    member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> multiId =
        match this.EntityMemoCache.TryGetValue this.EntityMemoId with
        | (true, struct (system, index)) ->
            let system = system :?> SystemMultiplexed<'c, 'w>
            (system.IndexCorrelatedBuffered index).Simplexes.[multiId].Simplex
        | (false, _) ->
            let system = this.EntityMemoEcs.IndexSystem<'c, SystemMultiplexed<'c, 'w>> ()
            let index = system.IndexCorrelatedInt this.EntityMemoId
            this.EntityMemoCache.Add (this.EntityMemoId, struct (system :> 'w System, index))
            (system.IndexCorrelatedBuffered index).Simplexes.[multiId].Simplex

    member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityMemoEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
        let hierarchical = system.IndexHierarchical nodeId this.EntityMemoId : 'c ComponentRef
        &hierarchical.Index

    member this.IndexHierarchicalBuffered<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityMemoEcs.IndexSystem<'c, SystemHierarchical<'c, 'w>> ()
        let hierarchical = system.IndexHierarchical nodeId this.EntityMemoId : 'c ComponentRef
        hierarchical.IndexBuffered

    type 'w Ecs with
        member this.GetEntityMemo entityId =
            { EntityMemoCache = dictPlus HashIdentity.Structural Seq.empty
              EntityMemoId = entityId
              EntityMemoEcs = this }

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"