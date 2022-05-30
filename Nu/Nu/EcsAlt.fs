namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

type EntityId' = uint64

type 'w SystemKey =
    { SystemIndex : uint64
      System : 'w System }

type Query<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> () =
    
    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'w>) world =
        for systemEntry in systems do
            let system = systemEntry.Value
            let stores = system.Stores
            let arr = stores.[typeof<'c>].Components :?> 'c array
            let arr2 = stores.[typeof<'c2>].Components :?> 'c2 array
            let arr3 = stores.[typeof<'c3>].Components :?> 'c3 array
            let mutable i = 0
            let mutable world = world
            while i < arr.Length do
                world <- statement.Invoke (&arr.[i], &arr2.[i], &arr3.[i], world)

type CorrelatedStore'<'c, 'w when 'c : struct and 'c :> 'c Component> (buffered, ecs : 'w Ecs) as this =
    inherit Store<'w> (Unchecked.defaultof<'c>.TypeName)

    let mutable (correlateds, correlatedsBuffered) = ecs.AllocateComponents<'c> buffered
    let mutable freeIndex = 0
    let freeList = HashSet<int> HashIdentity.Structural
    let registerCorellatedEvent = getTypeName this + "RegisterCorellated"
    let unregisteringCorellatedEvent = getTypeName this + "UnregisteringCorellated"

    member this.RegisterCorellatedEvent = registerCorellatedEvent
    member this.UnregisteringCorellatedEvent = unregisteringCorellatedEvent

    new (ecs) = CorrelatedStore' (false, ecs)

    member this.FreeListCount = freeList.Count

    member this.Correlateds = correlateds
    member this.WithCorrelatedsBuffered (fn : 'c ArrayRef -> 'w -> 'w) world = lock correlatedsBuffered (fun () -> fn correlatedsBuffered world)

    member this.RewindFreeIndex () =
        while freeList.Remove (dec freeIndex) do
            freeIndex <- dec freeIndex

    member this.IndexCorrelatedUnbuffered storeIndex = &correlateds.Array.[storeIndex]
    member this.IndexCorrelatedBuffered storeIndex = correlatedsBuffered.Array.[storeIndex]

    member this.RegisterCorrelated ordered (comp : 'c) entityId (world : 'w) =

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
        correlateds.Array.[index] <- comp

        // raise event
        let world = ecs.Publish this.RegisterCorellatedEvent entityId this world

        // fin
        struct (index, world)

    member this.UnregisterCorrelated storeIndex entityId (world : 'w) =

        // attempt to unregister component
        let struct (unregistered, world) =

            // raise removing event
            let world = ecs.Publish this.UnregisteringCorellatedEvent entityId this world

            // deallocate index for component
            if storeIndex <> freeIndex
            then freeList.Add storeIndex |> ignore<bool>
            else freeIndex <- dec freeIndex

            // deactive component
            correlateds.[storeIndex].Active <- false

            // clear free list and reset free index when there are no registered components remaining
            if  freeList.Count = correlateds.Length then
                freeList.Clear ()
                freeIndex <- 0
            struct (true, world)

        // fin
        struct (unregistered, world)

type 'w Ecs' () =

    let mutable entityIdCurrent = 0UL

    let stores = dictPlus<Type, 'w Store List> HashIdentity.Structural []
    
    let systemKeys = dictPlus<EntityId', 'w SystemKey> HashIdentity.Structural []

    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []

    member this.RegisterCorrelatedComponent<'a when 'a : struct and 'a :> 'a Component> comp (entityIdOpt : EntityId' voption) world =
        match entityIdOpt with
        | ValueSome entityId ->
            match systemKeys.TryGetValue entityId with
            | (true, systemKey) ->
                let comps = List () // allocation
                let world =
                    if systemKey.System.Id.Contains typeof<'a> then
                        // already has component
                        failwithumf ()
                    else
                        let mutable world = world
                        for storeEntry in systemKey.System.Stores do
                            comps.Add (storeEntry.Value.UnregisterComponent systemKey.SystemIndex)
                        world
                let systemId' = HashSet systemKey.System.Id
                systemId'.Add typeof<'a> |> ignore<bool>
                let world =
                    let mutable world = world
                    match systems.TryGetValue systemId' with
                    | (true, system) ->
                        for struct (compType, compObj) in comps do
                            system.Stores.[compType].RegisterComponent compObj ()
                        system.Stores.[typeof<'a>].RegisterComponent comp ()
                        world
                    | (false, _) ->
                        // create system and add entity
                        world
                world
        | ValueNone ->
            entityIdCurrent <- inc entityIdCurrent
            let entityId = entityIdCurrent
            let storeId = [|typeof<'a>|]
            match storesUnordered.TryGetValue storeId with
            | (true, (:? CorrelatedStore'<'a, 'w> as store)) ->
                store.RegisterCorrelated false comp entityId world
            | (true, _) | (false, _) ->
                failwithumf ()
