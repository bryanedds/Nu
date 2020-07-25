// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Prime

/// The base component type of an Ecs.
type Component =
    abstract RefCount : int with get, set

/// A storable reference to a component in its containing array.
type [<NoEquality; NoComparison; Struct>] ComponentRef<'t when 't : struct and 't :> Component> =
    { ComponentIndex : int
      ComponentArr : 't array }

    member this.Value
        with get () = &this.ComponentArr.[this.ComponentIndex]

    member this.Assign value =
        this.ComponentArr.[this.ComponentIndex] <- value

    static member (<!) (componentRef, value) =
        componentRef.ComponentArr.[componentRef.ComponentIndex] <- value

    static member (!>) componentRef =
        &componentRef.ComponentArr.[componentRef.ComponentIndex]

[<RequireQualifiedAccess>]
module ComponentRef =

    let make index arr =
        { ComponentIndex = index
          ComponentArr = arr }

/// A base system type of an Ecs.
type [<AbstractClass>] 'w System () =
    let pipedKey = Gen.id
    member this.PipedKey with get () = pipedKey
    abstract PipedInit : obj
    default this.PipedInit with get () = () :> obj
    abstract ProcessUpdate : 'w Ecs -> 'w -> 'w
    default this.ProcessUpdate _ world = world
    abstract ProcessPostUpdate : 'w Ecs -> 'w -> 'w
    default this.ProcessPostUpdate _ world = world
    abstract ProcessActualize : 'w Ecs -> 'w -> 'w
    default this.ProcessActualize _ world = world

/// A system with zero to many components.
and [<AbstractClass>] 'w SystemMany () =
    inherit System<'w> ()
    abstract SizeOfComponent : int
    abstract ComponentsLength : int
    abstract ComponentsToBytes : unit -> char array
    abstract BytesToComponents : char array -> unit
    abstract Pad : int -> unit

/// Nu's custom Entity-Component-System implementation.
/// While this isn't the most efficient ECS, it isn't the least efficient either. Due to the set-associative nature of
/// modern caches, most cache hits will be of the L2 variety for junctioned components. Uncorrelated components will be
/// L1-bound as is typical. Degradation of cache-prediction would only occur when a significant number of junctioned
/// components are very chaotically unregistered in a use-case scenario that the I, the library author, have trouble
/// even imagining.
and [<NoEquality; NoComparison>] 'w Ecs () =

    let systemsUnordered = dictPlus [] : Dictionary<string, 'w System>
    let systemsOrdered = List () : (string * 'w System) List
    let correlations = dictPlus [] : Dictionary<Guid, string List>
    let pipedValues = ConcurrentDictionary<Guid, obj> ()

    member internal this.Correlations 
        with get () = correlations

    /// Thread-safe.
    member this.RegisterPipedValue<'a> key (value : 'a) =
        pipedValues.[key] <- value :> obj

    /// Thread-safe.
    member this.UnregisterPipedValue key =
        pipedValues.TryRemove key

    /// Thread-safe.
    member this.TryIndexPipedValue<'a> key =
        match pipedValues.TryGetValue key with
        | (true, value) -> Some (value :?> 'a)
        | (false, _) -> None

    /// Thread-safe.
    member this.IndexPipedValue<'a> key =
        pipedValues.[key] :?> 'a

    member this.RegisterSystem systemName system =
        systemsUnordered.Add (systemName, system)
        systemsOrdered.Add (systemName, system)

    member this.UnregisterSystem systemName =
        systemsOrdered.RemoveAll (fun (systemName', _) -> systemName' = systemName) |> ignore
        systemsUnordered.Remove systemName |> ignore

    member this.TryIndexSystem<'s when 's :> 'w System> systemName =
        match systemsUnordered.TryGetValue systemName with
        | (true, system) ->
            match system with
            | :? 's as systemAsS -> Some systemAsS
            | _ -> None
        | (false, _) -> None

    member this.IndexSystem<'s when 's :> 'w System> systemName =
        this.TryIndexSystem<'s> systemName |> Option.get

    member this.ProcessUpdate world =
        Seq.fold
            (fun world (_, system : 'w System) -> system.ProcessUpdate this world)
            world systemsOrdered

    member this.ProcessPostUpdate world =
        Seq.fold
            (fun world (_, system : 'w System) -> system.ProcessPostUpdate this world)
            world systemsOrdered

    member this.ProcessActualize world =
        Seq.fold
            (fun world (_, system : 'w System) -> system.ProcessActualize this world)
            world systemsOrdered

/// An Ecs system with just a single component.
and [<AbstractClass>] SystemSingleton<'t, 'w when 't : struct and 't :> Component> (comp : 't) =
    inherit System<'w> ()

    let mutable comp = comp

    member this.Component with get () = &comp

    type 'w Ecs with

        member this.IndexSingleton<'t, 'w when 't : struct and 't :> Component> systemName =
            let systemOpt = this.TryIndexSystem<SystemSingleton<'t, 'w>> systemName 
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.Component

/// An Ecs system with components stored by a raw index.
type [<AbstractClass>] SystemUncorrelated<'t, 'w when 't : struct and 't :> Component> () =
    inherit SystemMany<'w> ()

    let mutable components = Array.zeroCreate 32 : 't array
    let mutable freeIndex = 0
    let freeList = Queue<int> ()

    abstract ComponentToBytes : 't -> char array
    default this.ComponentToBytes _ = failwithnie ()
    abstract BytesToComponent : char array -> 't
    default this.BytesToComponent _ = failwithnie ()

    override this.SizeOfComponent with get () =
        sizeof<'t>

    override this.ComponentsLength with get () =
        components.Length

    override this.ComponentsToBytes () =
        let byteArrays = Array.map this.ComponentToBytes components
        Array.concat byteArrays

    override this.BytesToComponents (bytes : char array) =
        let byteArrays = Array.chunkBySize this.SizeOfComponent bytes
        if bytes.Length <> components.Length then
            failwith "Incoming bytes array must have the same number of elements as target system has components."
        let components' = Array.map this.BytesToComponent byteArrays
        components <- components'

    override this.Pad length =
        let arr = Array.zeroCreate (components.Length + length)
        components.CopyTo (arr, 0)

    member this.Components
        with get () = components

    member this.FreeIndex
        with get () = freeIndex

    member this.IndexUncorrelated index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        &components.[index]

    member this.RegisterUncorrelated comp =
        if freeList.Count = 0 then
            if freeIndex < components.Length - 1 then
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
            else
                let arr = Array.zeroCreate (components.Length * 2)
                components.CopyTo (arr, 0)
                components <- arr
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
        else components.[freeList.Dequeue ()] <- comp

    member this.UnregisterUncorrelated index =
        if index <> freeIndex then
            components.[index].RefCount <- dec components.[index].RefCount
            if components.[index].RefCount = 0 then freeList.Enqueue index
        else freeIndex <- dec freeIndex

    type 'w Ecs with

        member this.IndexUncorrelated<'t when 't : struct and 't :> Component> systemName index =
            let systemOpt = this.TryIndexSystem<SystemUncorrelated<'t, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.IndexUncorrelated index

        member this.RegisterUncorrelated<'t when 't : struct and 't :> Component> systemName (comp : 't) =
            match this.TryIndexSystem<SystemUncorrelated<'t, 'w>> systemName with
            | Some system -> system.RegisterUncorrelated comp
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterUncorrelated<'t when 't : struct and 't :> Component> systemName index =
            match this.TryIndexSystem<SystemUncorrelated<'t, 'w>> systemName with
            | Some system -> system.UnregisterUncorrelated index
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.ReadUncorrelated (readers : Dictionary<string, StreamReader>) (count : int) =
            for readerEntry in readers do
                let (systemName, reader) = (readerEntry.Key, readerEntry.Value)
                match this.TryIndexSystem<'w SystemMany> systemName with
                | Some system ->
                    let length = system.ComponentsLength
                    system.Pad count
                    let bytes = system.ComponentsToBytes ()
                    let _ = reader.ReadBlock (bytes, system.SizeOfComponent * length, system.SizeOfComponent * count)
                    system.BytesToComponents bytes
                | None -> failwith ("Could not find expected system '" + systemName + "'.")

/// An Ecs system with components stored by entity id.
type [<AbstractClass>] SystemCorrelated<'t, 'w when 't : struct and 't :> Component> () =
    inherit SystemMany<'w> ()

    let mutable components = Array.zeroCreate 32 : 't array
    let mutable freeIndex = 0
    let freeList = Queue<int> ()
    let correlations = dictPlus [] : Dictionary<Guid, int>

    member this.Components with get () = components and internal set value = components <- value
    member this.FreeIndex with get () = freeIndex and internal set value = freeIndex <- value
    member internal this.FreeList with get () = freeList
    member internal this.Correlations with get () = correlations

    abstract ComponentToBytes : 't -> char array
    default this.ComponentToBytes _ = failwithnie ()
    abstract BytesToComponent : char array -> 't
    default this.BytesToComponent _ = failwithnie ()

    override this.SizeOfComponent with get () =
        sizeof<'t>

    override this.ComponentsLength with get () =
        components.Length

    override this.ComponentsToBytes () =
        let byteArrays = Array.map this.ComponentToBytes components
        Array.concat byteArrays

    override this.BytesToComponents (bytes : char array) =
        let byteArrays = Array.chunkBySize this.SizeOfComponent bytes
        if bytes.Length <> components.Length then
            failwith "Incoming bytes array must have the same number of elements as target system has components."
        let components' = Array.map this.BytesToComponent byteArrays
        components <- components'

    override this.Pad length =
        let arr = Array.zeroCreate (components.Length + length)
        components.CopyTo (arr, 0)

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

    abstract member RegisterCorrelated : Guid -> 't -> 'w Ecs -> int
    default this.RegisterCorrelated entityId comp _ =
        match Dictionary.tryGetValue entityId correlations with
        | (false, _) ->
            if freeList.Count = 0 then
                if freeIndex < components.Length - 1 then
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                else
                    let arr = Array.zeroCreate (components.Length * 2)
                    components.CopyTo (arr, 0)
                    components <- arr
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                let index = dec freeIndex
                correlations.Add (entityId, index)
                index
            else
                let index = freeList.Dequeue ()
                components.[index] <- comp
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

    type 'w Ecs with

        member this.GetSystemsCorrelated entityId =
            this.Correlations.[entityId] |>
            Seq.map (fun systemName -> (systemName, this.IndexSystem<'w System> systemName)) |>
            dictPlus

        member this.GetEntitiesCorrelated<'t when 't : struct and 't :> Component> systemName =
            match this.TryIndexSystem<SystemCorrelated<'t, 'w>> systemName with
            | Some system -> system.GetEntitiesCorrelated ()
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.QualifyCorrelated<'t when 't : struct and 't :> Component> systemName entityId =
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'t, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyCorrelated entityId

        member this.IndexCorrelated<'t when 't : struct and 't :> Component> systemName entityId =
            let systemOpt = this.TryIndexSystem<SystemCorrelated<'t, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            &system.IndexCorrelated entityId

        member this.RegisterCorrelated<'t when 't : struct and 't :> Component> systemName entityId comp =
            match this.TryIndexSystem<SystemCorrelated<'t, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterCorrelated entityId comp
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterCorrelated<'t when 't : struct and 't :> Component> systemName entityId =
            match this.TryIndexSystem<SystemCorrelated<'t, 'w>> systemName with
            | Some system ->
                if system.UnregisterCorrelated entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")
                

/// An Ecs system that explicitly associates components by entity id.
type [<AbstractClass>] SystemJunctioned<'t, 'w when 't : struct and 't :> Component> (junctionedSystemNames : string array) =
    inherit SystemCorrelated<'t, 'w> ()

    let mutable junctionsOpt = None : Dictionary<string, 'w System> option

    abstract Junction : Dictionary<string, 'w System> -> Guid -> 't -> 'w Ecs -> 't

    abstract Disjunction : Dictionary<string, 'w System> -> Guid -> 't -> 'w Ecs -> unit

    member this.GetJunctions (ecs : 'w Ecs) =
        match junctionsOpt with
        | Some junctions -> junctions
        | None ->
            let junctions =
                junctionedSystemNames |>
                Array.map (fun sourceName -> (sourceName, ecs.IndexSystem<'w System> sourceName)) |>
                dictPlus
            junctionsOpt <- Some junctions
            junctions

    override this.RegisterCorrelated entityId comp ecs =
        match Dictionary.tryGetValue entityId this.Correlations with
        | (false, _) ->
            let junctions = this.GetJunctions ecs
            let comp = this.Junction junctions entityId comp ecs
            if this.FreeList.Count = 0 then
                if this.FreeIndex < this.Components.Length - 1 then
                    this.Components.[this.FreeIndex] <- comp
                    this.FreeIndex <- inc this.FreeIndex
                else
                    let arr = Array.zeroCreate (this.Components.Length * 2)
                    this.Components.CopyTo (arr, 0)
                    this.Components <- arr
                    this.Components.[this.FreeIndex] <- comp
                    this.FreeIndex <- inc this.FreeIndex
                let index = dec this.FreeIndex
                this.Correlations.Add (entityId, index)
                index
            else
                let index = this.FreeList.Dequeue ()
                this.Components.[index] <- comp
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
            this.Disjunction junctions entityId comp ecs
            true
        | (false, _) -> false

    type 'w Ecs with

        member this.JunctionPlus<'t, 'w when 't : struct and 't :> Component>
            (systemName : string) (junctions : Dictionary<string, 'w System>) (entityId : Guid) (comp : 't) =
            let system = junctions.[systemName] :?> SystemCorrelated<'t, 'w>
            let index = system.RegisterCorrelated entityId comp this
            ComponentRef.make index system.Components

        member this.Junction<'t, 'w when 't : struct and 't :> Component>
            (junctions : Dictionary<string, 'w System>) (entityId : Guid) =
            this.JunctionPlus<'t, 'w> typeof<'t>.Name junctions entityId Unchecked.defaultof<'t>

        member this.DisjunctionPlus<'t, 'w when 't : struct and 't :> Component>
            (systemName : string) (junctions : Dictionary<string, 'w System>) (entityId : Guid) =
            let system = junctions.[systemName] :?> SystemCorrelated<'t, 'w>
            system.UnregisterCorrelated entityId this |> ignore

        member this.Disjunction<'t, 'w when 't : struct and 't :> Component>
            (junctions : Dictionary<string, 'w System>) (entityId : Guid) =
            this.DisjunctionPlus<'t, 'w> typeof<'t>.Name junctions entityId

/// Handle to one of an array of multiplexed components.
type Simplex<'t when 't : struct> =
    { mutable Simplex : 't }

/// Allows an entity to contain multiple of the same component.
/// However, it uses a dictionary without a small-object optimization, so this functionality won't get the typical
/// perf benefits of data-orientation. Really, this functionality is here for flexibility and convenience more than
/// anything else (which is good enough in almost all cases where multi-components are used).
type [<NoEquality; NoComparison; Struct>] ComponentMultiplexed<'t when 't : struct> =
    { mutable RefCount : int
      Simplexes : Dictionary<Guid, 't Simplex> }
    interface Component with
        member this.RefCount
            with get () = this.RefCount
            and set value = this.RefCount <- value
    member this.RegisterMultiplexed (multiId, comp) =
        this.Simplexes.Add (multiId, { Simplex = comp })
    member this.UnregisterMultiplexed multiId =
        this.Simplexes.Remove multiId

/// An Ecs system that stores multiple components per entity id.
type [<AbstractClass>] SystemMultiplexed<'t, 'w when 't : struct and 't :> Component> () =
    inherit SystemCorrelated<'t ComponentMultiplexed, 'w> ()

    member this.QualifyMultiplexed multiId entityId =
        if this.QualifyCorrelated entityId then
            let comp = this.IndexCorrelated entityId
            comp.Simplexes.ContainsKey multiId
        else false

    member this.IndexMultiplexed multiId entityId =
        let componentMultiplexed = &this.IndexCorrelated entityId
        componentMultiplexed.Simplexes.[multiId]

    member this.RegisterMultiplexed multiId entityId comp ecs =
        let _ = this.RegisterCorrelated entityId ecs
        let componentMultiplexed = &this.IndexCorrelated entityId
        do componentMultiplexed.RegisterMultiplexed (multiId, comp)
        multiId

    member this.UnregisterMultiplexed multiId entityId ecs =
        let componentMultiplexed = &this.IndexCorrelated entityId
        let _ = componentMultiplexed.UnregisterMultiplexed multiId
        this.UnregisterCorrelated entityId ecs

    type 'w Ecs with

        member this.QualifyMultiplexed<'t when 't : struct and 't :> Component> systemName multiId entityId =
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'t, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            system.QualifyMultiplexed multiId entityId

        member this.IndexMultiplexed<'t when 't : struct and 't :> Component> systemName multiId entityId =
            let systemOpt = this.TryIndexSystem<SystemMultiplexed<'t, 'w>> systemName
            if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
            let system = Option.get systemOpt
            let simplex = system.IndexMultiplexed multiId entityId
            &simplex.Simplex

        member this.RegisterMultiplexed<'t when 't : struct and 't :> Component> systemName multiId entityId comp =
            match this.TryIndexSystem<SystemMultiplexed<'t, 'w>> systemName with
            | Some system ->
                let _ = system.RegisterMultiplexed multiId entityId comp
                match this.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterMultiplexed<'t when 't : struct and 't :> Component> systemName multiId entityId =
            match this.TryIndexSystem<SystemMultiplexed<'t, 'w>> systemName with
            | Some system ->
                if system.UnregisterMultiplexed multiId entityId this then
                    match this.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> this.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "'.")

/// A collection of systems that can be run in parallel.
/// Because 'w is not a monoid (no mappend), null is passed in to all subsystems. Any information that affects the
/// world has to use a piped value and the related process lambda.
type 'w SystemParallel (processUpdateResults, processPostUpdateResults, processActualizeResults) =
    inherit System<'w> ()

    let subsystems =
        dictPlus [] : Dictionary<string, 'w System>

    member this.RegisterSubsystem subsystemName subsystem =
        subsystems.[subsystemName] <- subsystem

    member this.UnregisterSubsystem subsystemName =
        subsystems.Remove subsystemName

    override this.ProcessUpdate ecs world =
        let results =
            subsystems |>
            Seq.map (fun entry -> Task.Run (fun () ->
                let system = entry.Value
                do ecs.RegisterPipedValue system.PipedKey system.PipedInit
                let _ = system.ProcessUpdate ecs Unchecked.defaultof<'w>
                ecs.IndexPipedValue<obj> system.PipedKey) |> Vsync.AwaitTaskT) |>
            Vsync.Parallel
        processUpdateResults results world

    override this.ProcessPostUpdate ecs world =
        let results =
            subsystems |>
            Seq.map (fun entry -> Task.Run (fun () ->
                let system = entry.Value
                do ecs.RegisterPipedValue system.PipedKey system.PipedInit
                let _ = system.ProcessPostUpdate ecs Unchecked.defaultof<'w>
                ecs.IndexPipedValue<obj> system.PipedKey) |> Vsync.AwaitTaskT) |>
            Vsync.Parallel
        processPostUpdateResults results world

    override this.ProcessActualize ecs world =
        let results =
            subsystems |>
            Seq.map (fun entry -> Task.Run (fun () ->
                let system = entry.Value
                do ecs.RegisterPipedValue system.PipedKey system.PipedInit
                let _ = system.ProcessActualize ecs Unchecked.defaultof<'w>
                ecs.IndexPipedValue<obj> system.PipedKey) |> Vsync.AwaitTaskT) |>
            Vsync.Parallel
        processActualizeResults results world

    type 'w Ecs with

        member this.RegisterSubsystem systemName subsystemName subsystem =
            match this.TryIndexSystem<'w SystemParallel> systemName with
            | Some system -> system.RegisterSubsystem subsystemName subsystem
            | None -> failwith ("Could not find expected system '" + systemName + "'.")

        member this.UnregisterSubsystem systemName subsystemName =
            match this.TryIndexSystem<'w SystemParallel> systemName with
            | Some system -> system.UnregisterSubsystem subsystemName
            | None -> failwith ("Could not find expected system '" + systemName + "'.")