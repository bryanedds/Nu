// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Prime

/// A freezable type.
/// TODO: move to Prime?
type Freezable =
    interface
        abstract Frozen : bool
        abstract Freeze : unit -> unit
        abstract Thaw : unit -> unit
        end

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
        abstract AllocateJunctions : 'w Ecs -> (obj * obj) array
        abstract ResizeJunctions : int -> obj array -> 'w Ecs -> unit
        abstract MoveJunctions : int -> int -> obj array -> 'w Ecs -> unit
        abstract Junction : int -> obj array -> obj array -> 'w Ecs -> 'c
        abstract Disjunction : int -> obj array -> 'w Ecs -> unit
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
      /// DO NOT access the elements of ComponentArefBuffered without locking the field itself!
      ComponentArefBuffered : 'c ArrayRef }

    member inline this.Index
        with get () = &this.ComponentAref.[this.ComponentIndex]

    member inline this.IndexBuffered
        with get () =
            let index = this.ComponentIndex
            let arefBuffered = this.ComponentArefBuffered
            lock arefBuffered (fun () -> arefBuffered.[index])

    member inline this.Assign value =
        this.ComponentAref.[this.ComponentIndex] <- value

    member inline this.Assign (value : 'c inref) =
        this.ComponentAref.[this.ComponentIndex] <- value

    static member inline (<!) (componentRef, value) =
        componentRef.ComponentAref.Array.[componentRef.ComponentIndex] <- value
        
    static member inline (<!) (componentRef, value : 'c inref) =
        componentRef.ComponentAref.Array.[componentRef.ComponentIndex] <- value

    static member inline make index aref arefBuffered : 'c ComponentRef =
        { ComponentIndex = index
          ComponentAref = aref
          ComponentArefBuffered = arefBuffered }

/// An ECS event.
and [<NoEquality; NoComparison>] SystemEvent<'d, 'w when 'w :> Freezable> =
    { SystemEventData : 'd
      SystemPublisher : 'w System }

/// An ECS event callback.
and SystemCallback<'d, 'w when 'w :> Freezable> =
    SystemEvent<'d, 'w> -> 'w System -> 'w Ecs -> 'w option -> 'w option

/// A boxed ECS event callback.
and SystemCallbackBoxed<'w when 'w :> Freezable> =
    SystemEvent<obj, 'w> -> 'w System -> 'w Ecs -> 'w option -> 'w option

/// A base system type of an Ecs.
and System<'w when 'w :> Freezable> (name : string) =
    let pipedKey = Gen.id
    member this.PipedKey with get () = pipedKey
    abstract PipedInit : obj
    default this.PipedInit with get () = () :> obj
    member this.Name with get () = name

/// Buffered array objects.
and [<NoEquality; NoComparison>] ArrayObjsBuffered =
    { ArrayObjsUnbuffered : obj List
      mutable ArrayObjsBuffered : obj List }

/// Array objects that may or may not be buffered.
and [<NoEquality; NoComparison>] ArrayObjs =
    | ArrayObjs of obj List
    | ArrayObjsBuffered of ArrayObjsBuffered

/// Nu's custom Entity-Component-System implementation.
/// Nu's conception of an ECS is primarily as an abstraction over user-definable storage formats.
/// The default formats include SoA-based formats for non-correlated, correlated, multiplexed, and hierarchichal value types.
/// User can add formats of their own design by implementing the 'w System interface and providing related extension methods
/// on this 'w Ecs type.
and Ecs<'w when 'w :> Freezable> () as this =

    let arrayObjss = dictPlus<string, ArrayObjs> StringComparer.Ordinal []
    let systemSubscriptions = dictPlus<string, Dictionary<Guid, obj>> StringComparer.Ordinal []
    let systemsUnordered = dictPlus<string, 'w System> StringComparer.Ordinal []
    let systemsOrdered = List<string * 'w System> ()
    let correlations = dictPlus<Guid, string List> HashIdentity.Structural []
    let pipedValues = ConcurrentDictionary<Guid, obj> ()
    let globalSystem = System<'w> "Global"
    
    do this.RegisterSystemGeneralized globalSystem

    member private this.BoxCallback<'a> (callback : SystemCallback<'a, 'w>) =
        let boxableCallback = fun (evt : SystemEvent<obj, 'w>) system ->
            let evt =
                { SystemEventData = evt.SystemEventData :?> 'a
                  SystemPublisher = evt.SystemPublisher }
            callback evt system
        boxableCallback :> obj

    member private this.Correlations 
        with get () = correlations

    member this.GlobalSystem
        with get () = globalSystem

    member this.RegisterPipedValue<'a when 'a : not struct> key (value : 'a) =
        pipedValues.[key] <- value :> obj

    member this.WithPipedValue<'a when 'a : not struct> key fn (worldOpt : 'w option) : 'w option =
        let pipedValue = pipedValues.[key] :?> 'a
        lock pipedValue (fun () -> fn pipedValue worldOpt)

    member this.RegisterSystemGeneralized (system : 'w System) =
        systemsUnordered.Add (system.Name, system)
        systemsOrdered.Add (system.Name, system)
        this.RegisterPipedValue<obj> system.PipedKey system.PipedInit

    member this.TryIndexSystem<'s when 's :> 'w System> systemName =
        match systemsUnordered.TryGetValue systemName with
        | (true, system) ->
            match system with
            | :? 's as systemAsS -> Some systemAsS
            | _ -> None
        | (false, _) -> None

    member this.IndexSystem<'s when 's :> 'w System> systemName =
        match systemsUnordered.TryGetValue systemName with
        | (true, system) -> system :?> 's
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

    member this.Subscribe<'d, 'w when 'w :> Freezable> eventName callback =
        this.SubscribePlus<'d> Gen.id eventName (fun evt system ecs worldOpt -> callback evt system ecs; worldOpt) |> ignore

    member this.Unsubscribe eventName subscriptionId =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) -> subscriptions.Remove subscriptionId
        | (false, _) -> false

    member this.Publish<'d> eventName (eventData : 'd) publisher world =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            (Some world, subscriptions.Values) ||>
            Seq.fold (fun worldOpt (callback : obj) ->
                match callback with
                | :? SystemCallback<obj, 'w> as objCallback ->
                    let evt = { SystemEventData = eventData :> obj; SystemPublisher = publisher }
                    objCallback evt publisher this worldOpt
                | _ -> failwithumf ()) |>
            Option.get
        | (false, _) -> world

    member this.PublishParallel<'d> eventName (eventData : 'd) publisher =
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

    member this.AllocateComponents<'c when 'c : struct and 'c :> 'c Component> buffered =
        let componentName = typeof<'c>.Name
        match arrayObjss.TryGetValue componentName with
        | (true, _) -> failwith ("Array already initally allocated for '" + componentName + "'.")
        | (false, _) ->
            if buffered then
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                arrayObjss.Add (componentName, ArrayObjs (List [box aref]))
                (aref, aref)
            else
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                let arefBuffered = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                arrayObjss.Add (componentName, ArrayObjsBuffered { ArrayObjsUnbuffered = List [box aref]; ArrayObjsBuffered = List [box arefBuffered] })
                (aref, arefBuffered)

    member this.AllocateJunction<'c when 'c : struct and 'c :> 'c Component> () =
        let componentName = typeof<'c>.Name
        match arrayObjss.TryGetValue componentName with
        | (true, found) ->
            match found with
            | ArrayObjs arrayObjs ->
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                arrayObjs.Add (box aref)
                (box aref, box aref)
            | ArrayObjsBuffered arrayObjs ->
                let aref = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                let arefBuffered = { Array = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve }
                arrayObjs.ArrayObjsUnbuffered.Add (box aref)
                arrayObjs.ArrayObjsBuffered.Add (box arefBuffered)
                (box aref, box arefBuffered)
        | (false, _) -> failwith ("No array initially allocated for '" + componentName + "'.")

    member this.GetComponentArrays<'c when 'c : struct and 'c :> 'c Component> () =
        let componentName = typeof<'c>.Name
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            match arrayObjs with
            | ArrayObjs unbuffered -> unbuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
            | ArrayObjsBuffered buffered -> buffered.ArrayObjsUnbuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
        | (false, _) -> [||]

    member this.WithComponentArraysBuffered<'c when 'c : struct and 'c :> 'c Component> fn (worldOpt : 'w option) =
        let componentName = typeof<'c>.Name
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            match arrayObjs with
            | ArrayObjs unbuffered ->
                let arefs = unbuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
                fn arefs worldOpt
            | ArrayObjsBuffered buffered ->
                lock arrayObjs $ fun () ->
                    let arefsBuffered = buffered.ArrayObjsBuffered |> Seq.cast<'c ArrayRef> |> Seq.toArray
                    fn arefsBuffered worldOpt
        | (false, _) -> worldOpt

    member this.BufferComponentArrays<'c when 'c : struct and 'c :> 'c Component> () =
        let componentName = typeof<'c>.Name
        match arrayObjss.TryGetValue componentName with
        | (true, arrayObjs) ->
            match arrayObjs with
            | ArrayObjs _ -> ()
            | ArrayObjsBuffered buffered ->
                lock arrayObjs $ fun () ->
                    for i in 0 .. buffered.ArrayObjsUnbuffered.Count - 1 do
                        let aref = buffered.ArrayObjsUnbuffered.[i] :?> 'c ArrayRef
                        let arefBuffered = buffered.ArrayObjsBuffered.[i] :?> 'c ArrayRef
                        lock arefBuffered (fun () ->
                            if arefBuffered.Array.Length = aref.Array.Length
                            then aref.Array.CopyTo (arefBuffered.Array, 0)
                            else arefBuffered.Array <- Array.copy aref.Array)
        | (false, _) -> ()

    type System<'w when 'w :> Freezable> with
        member this.RegisterPipedValue (ecs : 'w Ecs) = ecs.RegisterPipedValue<obj> this.PipedKey this.PipedInit
        member this.WithPipedValue<'a when 'a : not struct> fn (ecs : 'w Ecs) worldOpt = ecs.WithPipedValue<'a> this.PipedKey fn worldOpt

[<Extension>]
type EcsExtensions =

    [<Extension>]
    static member RegisterSystem<'s, 'w when 's :> 'w System and 'w :> Freezable> (this : 'w Ecs, system : 's) =
        this.RegisterSystemGeneralized system

/// An Ecs system with just a single component.
type SystemSingleton<'c, 'w when 'c : struct and 'c :> 'c Component and 'w :> Freezable> (name, comp : 'c) =
    inherit System<'w> (name)
    let mutable comp = comp
    new (comp) = SystemSingleton (typeof<'c>.Name, comp)
    member this.Component with get () = &comp
    type Ecs<'w when 'w :> Freezable> with
        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> 'c Component> () =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemSingleton<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// An Ecs system with components stored by a raw index.
/// Stores components in an unordered manner.
type SystemUncorrelated<'c, 'w when 'c : struct and 'c :> 'c Component and 'w :> Freezable> (name, buffered, ecs : 'w Ecs) =
    inherit System<'w> (name)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural

    new (ecs) = SystemUncorrelated (typeof<'c>.Name, false, ecs)
    new (buffered, ecs) = SystemUncorrelated (typeof<'c>.Name, buffered, ecs)

    member this.Components with get () = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock componentsBuffered (fun () -> fn componentsBuffered worldOpt)

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterUncorrelated comp =
        if freeList.Count > 0 then
            let index = Seq.head freeList
            freeList.Remove index |> ignore<bool>
            components.[index] <- comp
        elif freeIndex < components.Length then
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex
        else
            let arr = Array.zeroCreate (components.Length * Constants.Ecs.ArrayGrowth)
            components.Array.CopyTo (arr, 0)
            components.Array <- arr
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex

    member this.UnregisterUncorrelated index =
        if index <> freeIndex then
            components.[index].Active <- false
            freeList.Add index |> ignore<bool>
        else freeIndex <- dec freeIndex

    type Ecs<'w when 'w :> Freezable> with

        member this.IndexUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.IndexUncorrelated index

        member this.RegisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> comp =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName with
            | Some system -> system.RegisterUncorrelated comp
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterUncorrelated<'c when 'c : struct and 'c :> 'c Component> index =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName with
            | Some system -> system.UnregisterUncorrelated index
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

/// An Ecs system with components stored by entity id.
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> 'c Component and 'w :> Freezable> (name, buffered, ecs : 'w Ecs) =
    inherit System<'w> (name)

    let mutable (components, componentsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable (junctions, junctionsBuffered) = Unchecked.defaultof<'c>.AllocateJunctions ecs |> Array.unzip
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let correlations = dictPlus<Guid, int> HashIdentity.Structural []
    let correlationsBack = dictPlus<int, Guid> HashIdentity.Structural []
    static member private isJunction<'c> junction = junction.GetType().GetElementType().GetGenericArguments().[0] = typeof<'c>

    new (ecs) = SystemCorrelated (typeof<'c>.Name, false, ecs)
    new (buffered, ecs) = SystemCorrelated (typeof<'c>.Name, buffered, ecs)

    member this.Components with get () = components
    member this.WithComponentsBuffered (fn : 'c ArrayRef -> 'w option -> 'w option) worldOpt = lock componentsBuffered (fun () -> fn componentsBuffered worldOpt)

    member this.GetJunction<'c> () = junctions |> Array.find SystemCorrelated<'c, 'w>.isJunction<'c> :?> 'c ArrayRef
    member this.WithJunctionBuffered<'c> fn (worldOpt : 'w option) = let junction = this.GetJunction<'c> () in  lock junction (fun () -> fn junction worldOpt)

    member private this.Compact ecs =

        // compact array
        // TODO: P1: step-debug this.
        let mutable i = 0
        let mutable j = 1
        while j < freeIndex do

            // check if slot is free
            if not components.[i].Active && freeList.Contains i then

                // find next non-free component
                while
                    j < freeIndex &&
                    not components.[j].Active &&
                    not (freeList.Contains j) do
                    j <- inc j

                // move components
                components.[i] <- components.[j]
                components.[0].MoveJunctions j i junctions ecs

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

    member this.GetEntitiesCorrelated () =
        correlations.Keys :> _ IEnumerable

    member this.QualifyCorrelated entityId =
        correlations.ContainsKey entityId

    member this.IndexCorrelatedI entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (ArgumentOutOfRangeException "entityId")
        index

    member this.IndexCorrelated entityId =
        let index = this.IndexCorrelatedI entityId
        ComponentRef<'c>.make index components componentsBuffered

    member this.RegisterCorrelated (comp : 'c) entityId ecs =

        // check if component is already registered
        if not (correlations.ContainsKey entityId) then

            // ensure there is space in the arrays
            if freeIndex >= components.Length then
                let length = components.Length * Constants.Ecs.ArrayGrowth
                let arr = Array.zeroCreate length in components.Array.CopyTo (arr, 0); components.Array <- arr
                comp.ResizeJunctions length junctions ecs
                lock componentsBuffered (fun () ->
                    let arr = Array.zeroCreate length in componentsBuffered.Array.CopyTo (arr, 0); componentsBuffered.Array <- arr)
                lock junctionsBuffered (fun () ->
                    comp.ResizeJunctions length junctionsBuffered ecs)

            // allocate component
            let index = freeIndex in freeIndex <- inc freeIndex
            let mutable comp = comp.Junction index junctions junctionsBuffered ecs
            comp.Active <- true
            correlations.Add (entityId, index)
            correlationsBack.Add (index, entityId)
            components.Array.[index] <- comp

            // fin
            entityId

        // component is already registered
        else failwith ("Component registered multiple times for entity '" + string entityId + "'.")

    member this.UnregisterCorrelated entityId ecs =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            let comp = components.[index]
            if index <> freeIndex then
                components.[index].Active <- false
                freeList.Add index |> ignore<bool>
            else freeIndex <- dec freeIndex
            correlations.Remove entityId |> ignore<bool>
            correlationsBack.Remove index |> ignore<bool>
            comp.Disjunction index junctions ecs
            if  components.Length < freeList.Count * 2 && // freeList is always empty if unordered
                components.Length > Constants.Ecs.ArrayReserve then
                this.Compact ecs
            true
        | (false, _) -> false

    type Ecs<'w when 'w :> Freezable> with

        member this.GetSystemsCorrelated entityId =
            this.Correlations.[entityId] |>
            Seq.map (fun systemName -> (systemName, this.IndexSystem<'w System> systemName)) |>
            dictPlus StringComparer.Ordinal 

        member this.GetEntitiesCorrelated<'c when 'c : struct and 'c :> 'c Component> () =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system -> system.GetEntitiesCorrelated ()
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.QualifyCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyCorrelated entityId

        member inline this.IndexCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId : 'c ComponentRef =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.IndexCorrelated entityId

        member this.RegisterCorrelated<'c when 'c : struct and 'c :> 'c Component> comp entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system ->
                let entityId = system.RegisterCorrelated comp entityId this
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
                entityId
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> 'c Component> entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system ->
                let result = system.UnregisterCorrelated entityId this
                if result then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Remove systemName |> ignore<bool>
                    | (false, _) -> ()
                result
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.JunctionPlus<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) (index : int) (componentsObj : obj) (componentsBufferedObj : obj) =
            let components = componentsObj :?> 'c ArrayRef
            let componentsBuffered = componentsBufferedObj :?> 'c ArrayRef
            comp.Active <- true
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
/// anything else (which is good enough in almost all cases where multi-components are used).
type [<NoEquality; NoComparison; Struct>] ComponentMultiplexed<'c when 'c : struct and 'c :> 'c Component> =
    { mutable Active : bool
      Simplexes : Dictionary<Guid, 'c Simplex> }
    interface Component<'c ComponentMultiplexed> with
        member this.Active with get () = this.Active and set value = this.Active <- value
        member this.AllocateJunctions _ = [||]
        member this.ResizeJunctions _ _ _ = ()
        member this.MoveJunctions _ _ _ _ = ()
        member this.Junction _ _ _ _ = this
        member this.Disjunction _ _ _ = ()
    member this.RegisterMultiplexed (multiId, comp) =
        this.Simplexes.Add (multiId, { Simplex = comp })
    member this.UnregisterMultiplexed multiId =
        this.Simplexes.Remove multiId

/// An Ecs system that stores multiple components per entity id.
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> 'c Component and 'w :> Freezable> (name, buffered, ecs : 'w Ecs) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (name, buffered, ecs)

    new (ecs) = SystemMultiplexed (typeof<'c>.Name, false, ecs)
    new (buffered, ecs) = SystemMultiplexed (typeof<'c>.Name, buffered, ecs)

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
        let entityId = this.RegisterCorrelated Unchecked.defaultof<_> entityId ecs
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.RegisterMultiplexed (multiId, comp)
        multiId

    member this.UnregisterMultiplexed multiId entityId ecs =
        let componentMultiplexed = this.IndexCorrelated entityId
        componentMultiplexed.Index.UnregisterMultiplexed multiId |> ignore<bool>
        this.UnregisterCorrelated entityId ecs

    type Ecs<'w when 'w :> Freezable> with

        member this.QualifyMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyMultiplexed multiId entityId

        member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexed multiId entityId
            &simplex.Simplex

        member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexedBuffered multiId entityId
            simplex.Simplex

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> comp multiId entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterMultiplexed comp multiId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName with
            | Some system ->
                if system.UnregisterMultiplexed multiId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

/// An Ecs system that stores components in a tree hierarchy.
type SystemHierarchical<'c, 'w when 'c : struct and 'c :> 'c Component and 'w :> Freezable> (name, buffered, ecs : 'w Ecs) =
    inherit System<'w> (name)

    let systemTree = ListTree.makeEmpty<SystemCorrelated<'c, 'w>> ()
    let systemDict = dictPlus<Guid, SystemCorrelated<'c, 'w>> HashIdentity.Structural []

    new (ecs) = SystemHierarchical (typeof<'c>.Name, false, ecs)
    new (buffered, ecs) = SystemHierarchical (typeof<'c>.Name, buffered, ecs)

    member this.Components with get () =
        systemTree |> ListTree.map (fun system -> system.Components)

    member this.IndexNode nodeId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.Components
        | (false, _) -> failwith ("Node with id '" + scstring nodeId + "' not found.")

    member this.AddNode (parentIdOpt : Guid option) =
        let nodeId = Gen.id
        let system = SystemCorrelated<'c, 'w> (scstring nodeId, buffered, ecs)
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

    type Ecs<'w when 'w :> Freezable> with

        member this.IndexHierarchy<'c when 'c : struct and 'c :> 'c Component> () =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.Components
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.IndexNode<'c when 'c : struct and 'c :> 'c Component> nodeId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.IndexNode nodeId
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.AddNode<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.AddNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.RemoveNode<'c when 'c : struct and 'c :> 'c Component> parentIdOpt =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.RemoveNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyHierarchical nodeId entityId

        member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            let systemName = typeof<'c>.Name
            let systemOpt = this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.IndexHierarchical nodeId entityId

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> 'c Component> comp nodeId entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterHierarchical comp nodeId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId entityId =
            let systemName = typeof<'c>.Name
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system ->
                if system.UnregisterHierarchical nodeId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

/// A correlated entity reference.
/// Slow relative to normal ECS operations, but convenient for one-off uses.
type [<NoEquality; NoComparison; Struct>] EntityRef<'w when 'w :> Freezable> =
    { EntityId : Guid
      EntityEcs : 'w Ecs }

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        let system = this.EntityEcs.IndexSystem<SystemCorrelated<'c, 'w>> typeof<'c>.Name
        let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
        &correlated.Index

    member this.IndexBuffered<'c when 'c : struct and 'c :> 'c Component> () =
        let system = this.EntityEcs.IndexSystem<SystemCorrelated<'c, 'w>> typeof<'c>.Name
        let correlated = system.IndexCorrelated this.EntityId : 'c ComponentRef
        correlated.IndexBuffered

    member this.IndexMultiplexed<'c when 'c : struct and 'c :> 'c Component> multiId =
        let system = this.EntityEcs.IndexSystem<SystemMultiplexed<'c, 'w>> typeof<'c>.Name
        let multiplexed = system.IndexMultiplexed multiId this.EntityId : 'c Simplex
        &multiplexed.Simplex

    member this.IndexMultiplexedBuffered<'c when 'c : struct and 'c :> 'c Component> multiId =
        let system = this.EntityEcs.IndexSystem<SystemMultiplexed<'c, 'w>> typeof<'c>.Name
        let multiplexed = system.IndexMultiplexedBuffered multiId this.EntityId : 'c Simplex
        multiplexed.Simplex

    member this.IndexHierarchical<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityEcs.IndexSystem<SystemHierarchical<'c, 'w>> typeof<'c>.Name
        let hierarchical = system.IndexHierarchical nodeId this.EntityId : 'c ComponentRef
        &hierarchical.Index

    member this.IndexHierarchicalBuffered<'c when 'c : struct and 'c :> 'c Component> nodeId =
        let system = this.EntityEcs.IndexSystem<SystemHierarchical<'c, 'w>> typeof<'c>.Name
        let hierarchical = system.IndexHierarchical nodeId this.EntityId : 'c ComponentRef
        hierarchical.IndexBuffered

    type Ecs<'w when 'w :> Freezable> with
        member this.GetEntityRef entityId =
            { EntityId = entityId; EntityEcs = this }

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] Update = "Update"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] Actualize = "Actualize"