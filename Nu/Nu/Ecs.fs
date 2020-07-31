// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
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
/// TODO: move to Prime?
type 'c ArrayRef =
    { mutable Array : 'c array }
    member this.Length with get () = this.Array.Length
    member this.Item i = &this.Array.[i]
    static member make n = { Array = Array.zeroCreate n }

/// The base component type of an Ecs.
type Component =
    abstract RefCount : int with get, set

/// A storable reference to a component in its containing array.
type [<NoEquality; NoComparison; Struct>] ComponentRef<'c when 'c : struct and 'c :> Component> =
    { ComponentIndex : int
      ComponentArrRef : 'c ArrayRef }

    member this.Index
        with get () = &this.ComponentArrRef.[this.ComponentIndex]

    member this.Assign value =
        this.ComponentArrRef.[this.ComponentIndex] <- value

    static member (<!) (componentRef, value) =
        componentRef.ComponentArrRef.Array.[componentRef.ComponentIndex] <- value

    static member (!>) componentRef =
        &componentRef.ComponentArrRef.Array.[componentRef.ComponentIndex]

[<RequireQualifiedAccess>]
module ComponentRef =

    let make index arr =
        { ComponentIndex = index
          ComponentArrRef = arr }

type [<NoEquality; NoComparison>] SystemEvent<'d, 'w when 'w :> Freezable> =
    { SystemEventData : 'd
      SystemPublisher : 'w System }

and SystemCallback<'d, 'w when 'w :> Freezable> =
    SystemEvent<'d, 'w> -> 'w System -> 'w Ecs -> 'w -> 'w

and SystemBoxedCallback<'w when 'w :> Freezable> =
    SystemEvent<obj, 'w> -> 'w System -> 'w Ecs -> 'w -> 'w

/// A base system type of an Ecs.
and System<'w when 'w :> Freezable> (name : string) =
    let pipedKey = Gen.id
    member this.PipedKey with get () = pipedKey
    abstract PipedInit : obj
    default this.PipedInit with get () = () :> obj
    member this.Name with get () = name

/// Nu's custom Entity-Component-System implementation.
/// While this isn't the most efficient ECS, it isn't the least efficient either. Due to the set-associative nature of
/// modern caches, most cache hits will be of the L2 variety for junctioned components. Uncorrelated components will be
/// L1-bound as is typical. Degradation of cache-prediction would only occur when a significant number of junctioned
/// components are very chaotically unregistered in a scenario that the I, the library author, have trouble imagining
/// for the intended use cases.
and [<NoEquality; NoComparison>] Ecs<'w when 'w :> Freezable> () as this =

    let systemSubscriptions = dictPlus [] : Dictionary<string, Dictionary<Guid, obj>>
    let systemsUnordered = dictPlus [] : Dictionary<string, 'w System>
    let systemsOrdered = List () : (string * 'w System) List
    let correlations = dictPlus [] : Dictionary<Guid, string List>
    let pipedValues = ConcurrentDictionary<Guid, obj> ()
    let globalSystem = System<'w> "Global"

    do this.RegisterSystemGeneralized globalSystem

    member private this.BoxCallback<'a> (callback : SystemCallback<'a, 'w>) =
        let boxableCallback = fun (evt : SystemEvent<obj, 'w>) world ->
            let evt =
                { SystemEventData = evt.SystemEventData :?> 'a
                  SystemPublisher = evt.SystemPublisher }
            callback evt world
        boxableCallback :> obj

    member internal this.Correlations 
        with get () = correlations

    member this.GlobalSystem
        with get () = globalSystem

    /// Thread-safe.
    member this.RegisterPipedValue<'a> key (value : 'a) =
        pipedValues.[key] <- value :> obj

    /// Thread-safe.
    member this.UnregisterPipedValue key =
        pipedValues.TryRemove (key, ref (obj ()))

    /// Thread-safe.
    member this.TryIndexPipedValue<'a> key =
        match pipedValues.TryGetValue key with
        | (true, value) -> Some (value :?> 'a)
        | (false, _) -> None

    /// Thread-safe.
    member this.IndexPipedValue<'a> key =
        pipedValues.[key] :?> 'a

    member this.RegisterSystemGeneralized (system : 'w System) =
        systemsUnordered.Add (system.Name, system)
        systemsOrdered.Add (system.Name, system)
        //system.RegisterPipedValue this

    member this.UnregisterSystem (system : 'w System) =
        //system.UnregisterPipedValue this
        systemsOrdered.RemoveAll (fun (systemName', _) -> systemName' = system.Name) |> ignore
        systemsUnordered.Remove system.Name |> ignore

    member this.TryIndexSystem<'s when 's :> 'w System> systemName =
        match systemsUnordered.TryGetValue systemName with
        | (true, system) ->
            match system with
            | :? 's as systemAsS -> Some systemAsS
            | _ -> None
        | (false, _) -> None

    member this.IndexSystem<'s when 's :> 'w System> systemName =
        this.TryIndexSystem<'s> systemName |> Option.get

    member this.Subscribe<'d> eventName (callback : SystemCallback<'d, 'w>) =
        let subscriptionId = Gen.id
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            subscriptions.Add (subscriptionId, this.BoxCallback<'d> callback)
            subscriptionId
        | (false, _) ->
            let subscriptions = dictPlus [(subscriptionId, this.BoxCallback<'d> callback)]
            systemSubscriptions.Add (eventName, subscriptions)
            subscriptionId

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

    member this.PublishParallel<'d> eventName (eventData : 'd) publisher (world : 'w) =
        match systemSubscriptions.TryGetValue eventName with
        | (true, subscriptions) ->
            world.Freeze ()
            try subscriptions |>
                Seq.map (fun subscription ->
                    Task.Run (fun () ->
                        match subscription.Value with
                        | :? SystemCallback<obj, 'w> as objCallback ->
                            let evt = { SystemEventData = eventData :> obj; SystemPublisher = publisher }
                            let _ = objCallback evt publisher this world // ignore returned world
                            ()
                        | _ -> failwithumf ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel |>
                Vsync.RunSynchronously |>
                ignore
            finally world.Thaw ()
            world
        | (false, _) -> world

    type System<'w when 'w :> Freezable> with
        member this.RegisterPipedValue (ecs : 'w Ecs) = ecs.RegisterPipedValue<obj> this.PipedKey this.PipedInit
        member this.UnregisterPipedValue (ecs : 'w Ecs) = ecs.UnregisterPipedValue this.PipedKey
        member this.IndexPipedValue<'a> (ecs : 'w Ecs) = ecs.IndexPipedValue<'a> this.PipedKey

[<Extension>]
type EcsExtensions =

    [<Extension>]
    static member RegisterSystem<'s, 'w when 's :> 'w System and 'w :> Freezable> (this : 'w Ecs, system : 's) =
        this.RegisterSystemGeneralized system
        system

/// An Ecs system with just a single component.
type SystemSingleton<'c, 'w when 'c : struct and 'c :> Component and 'w :> Freezable> (name, comp : 'c) =
    inherit System<'w> (name)

    let mutable comp = comp
    
    new (comp) = SystemSingleton (typeof<'c>.Name, comp)

    member this.Component with get () = &comp

    type Ecs<'w when 'w :> Freezable> with

        member this.IndexSingleton<'c, 'w when 'c : struct and 'c :> Component> systemName =
            let systemOpt = this.TryIndexSystem<SystemSingleton<'c, 'w>> systemName 
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// A system with zero to many components.
type [<AbstractClass>] SystemMany<'w when 'w :> Freezable> (name) =
    inherit System<'w> (name)
    abstract SizeOfComponent : int
    abstract ComponentsLength : int
    abstract ComponentsToBytes : unit -> char array
    abstract BytesToComponents : char array -> unit
    abstract PadComponents : int -> unit

/// An Ecs system with components stored by a raw index.
type SystemUncorrelated<'c, 'w when 'c : struct and 'c :> Component and 'w :> Freezable> (name) =
    inherit SystemMany<'w> (name)

    let mutable components = ArrayRef<'c>.make 32
    let mutable freeIndex = 0
    let freeList = Queue<int> ()
    
    new () = SystemUncorrelated typeof<'c>.Name

    abstract ComponentToBytes : 'c -> char array
    default this.ComponentToBytes _ = failwithnie ()
    abstract BytesToComponent : char array -> 'c
    default this.BytesToComponent _ = failwithnie ()

    override this.SizeOfComponent with get () =
        sizeof<'c>

    override this.ComponentsLength with get () =
        components.Length

    override this.ComponentsToBytes () =
        let byteArrays = Array.map this.ComponentToBytes components.Array
        Array.concat byteArrays

    override this.BytesToComponents (bytes : char array) =
        let byteArrays = Array.chunkBySize this.SizeOfComponent bytes
        if bytes.Length <> components.Length then
            failwith "Incoming bytes array must have the same number of elements as target system has components."
        let arr = Array.map this.BytesToComponent byteArrays
        components.Array <- arr

    override this.PadComponents length =
        let arr = Array.zeroCreate (components.Length + length)
        components.Array.CopyTo (arr, 0)
        components.Array <- arr

    member this.Components with get () = components
    member this.FreeIndex with get () = freeIndex
    member this.Iter with get () = (components.Array, freeIndex - 1)

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        &components.[index]

    member this.RegisterUncorrelated comp =
        if freeIndex < components.Length - 1 then
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex
        else
            let arr = Array.zeroCreate (components.Length * 2)
            components.Array.CopyTo (arr, 0)
            components.Array <- arr
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex

    member this.UnregisterUncorrelated index =
        if index <> freeIndex then
            components.[index].RefCount <- dec components.[index].RefCount
            if components.[index].RefCount = 0 then freeList.Enqueue index
        else freeIndex <- dec freeIndex

    type Ecs<'w when 'w :> Freezable> with

        member this.IndexUncorrelated<'c when 'c : struct and 'c :> Component> systemName index =
            let systemOpt = this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.IndexUncorrelated index

        member this.RegisterUncorrelated<'c when 'c : struct and 'c :> Component> comp systemName =
            match this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName with
            | Some system -> system.RegisterUncorrelated comp
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterUncorrelated<'c when 'c : struct and 'c :> Component> systemName index =
            match this.TryIndexSystem<SystemUncorrelated<'c, 'w>> systemName with
            | Some system -> system.UnregisterUncorrelated index
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.ReadUncorrelated (readers : Dictionary<string, StreamReader>) (count : int) =
            for readerEntry in readers do
                let (systemName, reader) = (readerEntry.Key, readerEntry.Value)
                match this.TryIndexSystem<'w SystemMany> systemName with
                | Some system ->
                    let length = system.ComponentsLength
                    system.PadComponents count
                    let bytes = system.ComponentsToBytes ()
                    let _ = reader.ReadBlock (bytes, system.SizeOfComponent * length, system.SizeOfComponent * count)
                    system.BytesToComponents bytes
                | None -> failwith ("Could not find expected system '" + systemName + "'.")

/// An Ecs system with components stored by entity id.
type SystemCorrelated<'c, 'w when 'c : struct and 'c :> Component and 'w :> Freezable> (name) =
    inherit SystemMany<'w> (name)

    let mutable components = ArrayRef<'c>.make 32
    let mutable freeIndex = 0
    let freeList = Queue<int> ()
    let correlations = dictPlus [] : Dictionary<Guid, int>

    new () = SystemCorrelated typeof<'c>.Name

    member this.Components with get () = components
    member this.FreeIndex with get () = freeIndex and internal set value = freeIndex <- value
    member this.Iter with get () = (components.Array, freeIndex - 1)
    member internal this.FreeList with get () = freeList
    member internal this.Correlations with get () = correlations

    abstract ComponentToBytes : 'c -> char array
    default this.ComponentToBytes _ = failwithnie ()
    abstract BytesToComponent : char array -> 'c
    default this.BytesToComponent _ = failwithnie ()

    override this.SizeOfComponent with get () =
        sizeof<'c>

    override this.ComponentsLength with get () =
        components.Length

    override this.ComponentsToBytes () =
        let byteArrays = Array.map this.ComponentToBytes components.Array
        Array.concat byteArrays

    override this.BytesToComponents (bytes : char array) =
        let byteArrays = Array.chunkBySize this.SizeOfComponent bytes
        if bytes.Length <> components.Length then
            failwith "Incoming bytes array must have the same number of elements as target system has components."
        let arr = Array.map this.BytesToComponent byteArrays
        components.Array <- arr

    override this.PadComponents length =
        let arr = Array.zeroCreate (components.Length + length)
        components.Array.CopyTo (arr, 0)
        components.Array <- arr

    member internal this.Compact () =
        // TODO: P1: step-debug this.
        let mutable i = 0
        let mutable j = i
        let liveCount = freeIndex - freeList.Count
        let liveArray = Array.zeroCreate liveCount
        while j < freeIndex do
            if components.[i].RefCount = 0 then
                while components.[j].RefCount = 0 &&
                      j < freeIndex do
                      j <- inc j
                liveArray.[i] <- components.[j]
                j <- inc j
            i <- inc i
        freeList.Clear ()
        freeIndex <- j
        components.Array <- liveArray

    member this.GetEntitiesCorrelated () =
        correlations.Keys :> _ IEnumerable

    member this.QualifyCorrelated entityId =
        correlations.ContainsKey entityId

    member this.IndexCorrelatedI entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (InvalidOperationException "entityId")
        index

    member this.IndexCorrelated entityId =
        let index = this.IndexCorrelatedI entityId
        &components.[index]

    abstract member RegisterCorrelated : 'c -> Guid -> 'w Ecs -> int
    default this.RegisterCorrelated comp entityId _ =
        if components.Length < freeList.Count * 2 then this.Compact ()
        match Dictionary.tryGetValue entityId correlations with
        | (false, _) ->
            comp.RefCount <- inc comp.RefCount
            if freeIndex < components.Length - 1 then
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
            else
                let arr = Array.zeroCreate (components.Length * 2)
                components.Array.CopyTo (arr, 0)
                components.Array <- arr
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
            let index = dec freeIndex
            correlations.Add (entityId, index)
            index
        | (true, index) ->
            let mutable comp = components.[index]
            comp.RefCount <- inc comp.RefCount
            index

    abstract member UnregisterCorrelated : Guid -> 'w Ecs -> bool
    default this.UnregisterCorrelated entityId _ =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            if index <> freeIndex then
                components.[index].RefCount <- dec components.[index].RefCount
                if components.[index].RefCount = 0 then freeList.Enqueue index
            else freeIndex <- dec freeIndex
            true
        | (false, _) -> false

    type Ecs<'w when 'w :> Freezable> with

        member this.GetSystemsCorrelated entityId =
            this.Correlations.[entityId] |>
            Seq.map (fun systemName -> (systemName, this.IndexSystem<'w System> systemName)) |>
            dictPlus

        member this.GetEntitiesCorrelated<'c when 'c : struct and 'c :> Component> systemName =
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system -> system.GetEntitiesCorrelated ()
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.GetEntityRef entityId =
            { EntityId = entityId; EntityEcs = this }

        member this.QualifyCorrelated<'c when 'c : struct and 'c :> Component> systemName entityId =
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyCorrelated entityId

        member this.IndexCorrelated<'c when 'c : struct and 'c :> Component> systemName entityId =
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.IndexCorrelated entityId

        member this.RegisterCorrelated<'c when 'c : struct and 'c :> Component> comp systemName entityId =
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterCorrelated comp entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterCorrelated<'c when 'c : struct and 'c :> Component> systemName entityId =
            match this.TryIndexSystem<SystemCorrelated<'c, 'w>> systemName with
            | Some system ->
                let result = system.UnregisterCorrelated entityId this
                if result then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
                result
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

/// A correlated entity reference.
/// Very slow, but convenient for one-off operations.
and [<NoEquality; NoComparison; Struct>] EntityRef<'w when 'w :> Freezable> =
    { EntityId : Guid
      EntityEcs : 'w Ecs }
    member this.IndexPlus<'c when 'c : struct and 'c :> Component> systemName =
        let system = this.EntityEcs.IndexSystem<SystemCorrelated<'c, 'w>> systemName
        &system.IndexCorrelated this.EntityId
    member this.Index<'c when 'c : struct and 'c :> Component> () =
        let system = this.EntityEcs.IndexSystem<SystemCorrelated<'c, 'w>> typeof<'c>.Name
        &system.IndexCorrelated this.EntityId

/// A junctioned component.
type Junction<'c when 'c : struct and 'c :> 'c Junction> =
    interface
        inherit Component
        abstract SystemNames : string array
        abstract Junction : 'w System array -> Guid -> 'w Ecs -> 'c
        abstract Disjunction : 'w System array -> Guid -> 'w Ecs -> unit
        end

/// An Ecs system that explicitly associates components by entity id.
type SystemJunctioned<'c, 'w when 'c : struct and 'c :> 'c Junction and 'w :> Freezable> (name) =
    inherit SystemCorrelated<'c, 'w> (name)

    let junctionedSystemNames = Unchecked.defaultof<'c>.SystemNames
    let mutable junctionsOpt = None : 'w System array option

    new () = SystemJunctioned (typeof<'c>.Name)

    member this.GetJunctions (ecs : 'w Ecs) =
        match junctionsOpt with
        | Some junctions -> junctions
        | None ->
            let junctions = Array.map (fun sourceName -> ecs.IndexSystem<'w System> sourceName) junctionedSystemNames
            junctionsOpt <- Some junctions
            junctions

    override this.RegisterCorrelated comp entityId ecs =
        if this.Components.Length < this.FreeList.Count * 2 then this.Compact ()
        match Dictionary.tryGetValue entityId this.Correlations with
        | (false, _) ->
            let junctions = this.GetJunctions ecs
            let comp = comp.Junction junctions entityId ecs
            comp.RefCount <- inc comp.RefCount
            if this.FreeIndex < this.Components.Length - 1 then
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
            else
                let arr = Array.zeroCreate (this.Components.Length * 2)
                this.Components.Array.CopyTo (arr, 0)
                this.Components.Array <- arr
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
            let index = dec this.FreeIndex
            this.Correlations.Add (entityId, index)
            index
        | (true, index) ->
            let mutable comp = this.Components.[index]
            comp.RefCount <- inc comp.RefCount
            index

    override this.UnregisterCorrelated entityId ecs =
        match this.Correlations.TryGetValue entityId with
        | (true, index) ->
            let comp = this.Components.[index]
            let junctions = this.GetJunctions ecs
            if index <> this.FreeIndex then
                this.Components.[index].RefCount <- dec this.Components.[index].RefCount
                if this.Components.[index].RefCount = 0 then this.FreeList.Enqueue index
            else this.FreeIndex <- dec this.FreeIndex
            comp.Disjunction junctions entityId ecs
            true
        | (false, _) -> false

    type Ecs<'w when 'w :> Freezable> with

        member this.QualifyJunctioned<'c when 'c : struct and 'c :> 'c Junction> systemName entityId =
            this.QualifyCorrelated<'c> systemName entityId

        member this.IndexJunctioned<'c when 'c : struct and 'c :> 'c Junction> systemName entityId =
            this.IndexCorrelated<'c> systemName entityId

        member this.RegisterJunctioned<'c when 'c : struct and 'c :> 'c Junction> comp systemName entityId =
            this.RegisterCorrelated<'c> comp systemName entityId
            entityId

        member this.UnregisterJunctioned<'c when 'c : struct and 'c :> 'c Junction> systemName entityId =
            this.UnregisterCorrelated<'c> systemName entityId

        member this.JunctionPlus<'c when 'c : struct and 'c :> Component>
            (comp : 'c) (entityId : Guid) (system : 'w System) =
            let system = system :?> SystemCorrelated<'c, 'w>
            let index = system.RegisterCorrelated comp entityId this
            ComponentRef.make index system.Components

        member this.Junction<'c when 'c : struct and 'c :> Component>
            (entityId : Guid) (system : 'w System) =
            this.JunctionPlus<'c> Unchecked.defaultof<'c> entityId system

        member this.DisjunctionPlus<'c when 'c : struct and 'c :> Component>
            (entityId : Guid) (system : 'w System) =
            let system = system :?> SystemCorrelated<'c, 'w>
            system.UnregisterCorrelated entityId this |> ignore

        member this.Disjunction<'c when 'c : struct and 'c :> Component>
            (entityId : Guid) (system : 'w System) =
            this.DisjunctionPlus<'c> entityId system

/// Handle to one of an array of multiplexed components.
type Simplex<'c when 'c : struct> =
    { mutable Simplex : 'c }

/// Allows an entity to contain multiple of the same component.
/// However, it uses a dictionary without a small-object optimization, so this functionality won't get the typical
/// perf benefits of data-orientation. Really, this functionality is here for flexibility and convenience more than
/// anything else (which is good enough in almost all cases where multi-components are used).
type [<NoEquality; NoComparison; Struct>] ComponentMultiplexed<'c when 'c : struct> =
    { mutable RefCount : int
      Simplexes : Dictionary<Guid, 'c Simplex> }
    interface Component with
        member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
    member this.RegisterMultiplexed (multiId, comp) =
        this.Simplexes.Add (multiId, { Simplex = comp })
    member this.UnregisterMultiplexed multiId =
        this.Simplexes.Remove multiId

/// An Ecs system that stores multiple components per entity id.
type SystemMultiplexed<'c, 'w when 'c : struct and 'c :> Component and 'w :> Freezable> (name) =
    inherit SystemCorrelated<'c ComponentMultiplexed, 'w> (name)
    
    new () = SystemMultiplexed typeof<'c>.Name

    member this.QualifyMultiplexed multiId entityId =
        if this.QualifyCorrelated entityId then
            let comp = this.IndexCorrelated entityId
            comp.Simplexes.ContainsKey multiId
        else false

    member this.IndexMultiplexed multiId entityId =
        let componentMultiplexed = &this.IndexCorrelated entityId
        componentMultiplexed.Simplexes.[multiId]

    member this.RegisterMultiplexed comp multiId entityId ecs =
        let _ = this.RegisterCorrelated Unchecked.defaultof<_> entityId ecs
        let componentMultiplexed = &this.IndexCorrelated entityId
        componentMultiplexed.RegisterMultiplexed (multiId, comp)
        multiId

    member this.UnregisterMultiplexed multiId entityId ecs =
        let componentMultiplexed = &this.IndexCorrelated entityId
        let _ = componentMultiplexed.UnregisterMultiplexed multiId
        this.UnregisterCorrelated entityId ecs

    type Ecs<'w when 'w :> Freezable> with

        member this.QualifyMultiplexed<'c when 'c : struct and 'c :> Component> systemName multiId entityId =
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyMultiplexed multiId entityId

        member this.IndexMultiplexed<'c when 'c : struct and 'c :> Component> systemName multiId entityId =
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexed multiId entityId
            &simplex.Simplex

        member this.RegisterMultiplexed<'c when 'c : struct and 'c :> Component> comp systemName multiId entityId =
            match this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterMultiplexed comp multiId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterMultiplexed<'c when 'c : struct and 'c :> Component> systemName multiId entityId =
            match this.TryIndexSystem<SystemMultiplexed<'c, 'w>> systemName with
            | Some system ->
                if system.UnregisterMultiplexed multiId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

type SystemHierarchical<'c, 'w when 'c : struct and 'c :> Component and 'w :> Freezable> (name) =
    inherit System<'w> (name)

    let systemTree = ListTree.makeEmpty<SystemCorrelated<'c, 'w>> ()
    let systemDict = dictPlus [] : Dictionary<Guid, SystemCorrelated<'c, 'w>>

    member this.Components with get () =
        systemTree |> ListTree.map (fun system -> system.Components)

    member this.IndexNode nodeId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.Components
        | (false, _) -> failwith ("Node with id '" + scstring nodeId + "'not found.")

    member this.AddNode parentIdOpt =
        let nodeId = Gen.id
        let system = SystemCorrelated<'c, 'w> (scstring nodeId)
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
        let _ = systemTree |> ListTree.remove (fun system -> system.Name = nodeIdStr)
        systemDict.Remove nodeId

    member this.QualifyHierarchical nodeId entityId =
        match systemDict.TryGetValue nodeId with
        | (true, system) -> system.QualifyCorrelated entityId
        | (false, _) -> false

    member this.IndexHierarchical nodeId entityId =
        let (found, system) = systemDict.TryGetValue nodeId
        if not found then raise (InvalidOperationException "nodeId")
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

        member this.IndexHierarchy<'c when 'c : struct and 'c :> Component> systemName =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.Components
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.IndexNode<'c when 'c : struct and 'c :> Component> systemName nodeId =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.IndexNode nodeId
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.AddNode<'c when 'c : struct and 'c :> Component> systemName parentIdOpt =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.AddNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.RemoveNode<'c when 'c : struct and 'c :> Component> systemName parentIdOpt =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system -> system.RemoveNode parentIdOpt
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.QualifyHierarchical<'c when 'c : struct and 'c :> Component> systemName nodeId entityId =
            let systemOpt = this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyHierarchical nodeId entityId

        member this.IndexHierarchical<'c when 'c : struct and 'c :> Component> systemName nodeId entityId =
            let systemOpt = this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.IndexHierarchical nodeId entityId

        member this.RegisterHierarchical<'c when 'c : struct and 'c :> Component> comp systemName nodeId entityId =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterHierarchical comp nodeId entityId
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterHierarchical<'c when 'c : struct and 'c :> Component> systemName nodeId entityId =
            match this.TryIndexSystem<SystemHierarchical<'c, 'w>> systemName with
            | Some system ->
                if system.UnregisterHierarchical nodeId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] Update = "Update"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] Actualize = "Actualize"