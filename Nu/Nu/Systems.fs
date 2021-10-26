// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

/// A delegate for interfacing with correlated components.
type Statement<'c, 'w when
    'c : struct and 'c :> 'c Component> =
    delegate of 'c byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> =
    delegate of 'c byref * 'c2 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'w -> 'w

/// A delegate for interfacing with correlated components.
type Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> =
    delegate of 'c byref * 'c2 byref * 'c3 byref * 'c4 byref * 'c5 byref * 'c6 byref * 'c7 byref * 'w -> 'w

/// An ECS system.
type System<'c, 'w when
    'c : struct and 'c :> 'c Component> (excluding : Type array, ecs : 'w Ecs) =
    
    let cache = OrderedDictionary<uint64, int> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId

    member this.Iterate (fn : Statement<'c, 's>) state =
        if excluding.Count = 0 then
            let array = store.Correlateds.Array
            let mutable state = state
            for i in 0 .. array.Length - 1 do
                state <- fn.Invoke (&array.[i], state)
            state
        else
            let array = store.Correlateds.Array
            let mutable state = state
            let mutable enr = cache.ValuesEnumerator
            while enr.MoveNext () do
                state <- fn.Invoke (&array.[enr.Current], state)
            state

    member this.IterateBuffered (fn : Statement<'c, 'w>) world =
        if excluding.Count = 0 then
            store.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let mutable world = world
                for i in 0 .. array.Length - 1 do
                    world <- fn.Invoke (&array.[i], world)
                world)
                world
        else
            store.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let mutable world = world
                let mutable enr = cache.ValuesEnumerator
                while enr.MoveNext () do
                    let index = enr.Current
                    world <- fn.Invoke (&array.[index], world)
                world)
                world

    member this.Index (entityId : uint64) (fn : Statement<'c, 's>) state =
        if excluding.Count = 0 then
            let cref = store.IndexCorrelated entityId
            fn.Invoke (&cref.Index, state)
        else
            let array = store.Correlateds.Array
            let index = cache.[entityId]
            fn.Invoke (&array.[index], state)

    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'w>) world =
        if excluding.Count = 0 then
            store.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let index = store.IndexCorrelatedToI entityId
                fn.Invoke (&array.[index], world))
                world
        else
            store.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let index = cache.[entityId]
                fn.Invoke (&array.[index], world))
                world

    member this.RegisterCorrelated ordered comp entityId world =
        ecs.RegisterCorrelated<'c> ordered comp entityId world

    member this.UnregisterCorrelated (entityId : uint64) world =
        ecs.UnregisterCorrelated<'c> entityId world

    member this.RegisterHierarchical ordered parentIdOpt comp entityId world =
        ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world

    member this.UnregisterHierarchical parentIdOpt world =
        ecs.UnregisterHierarchical<'c> parentIdOpt world

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world = this.Index entityId (new Statement<_, _> (fun comp' world -> comp' <- comp; world)) world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world = this.Index entityId (new Statement<_, _> (fun comp world -> comp <- Unchecked.defaultof<'c>; world)) world
            struct (true, world)
        else struct (false, world)

    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            if excluding.Count = 0 then
                () // just pulls from lone associated store
            else
                let indexOpt = store.TryIndexCorrelatedToI entityId
                if indexOpt > -1 then
                    let storeIds = ecs.IndexStoreIds entityId
                    if not (storeIds.IsSupersetOf excluding)
                    then cache.[entityId] <- indexOpt
                    else cache.Remove entityId |> ignore<bool>
                else cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> (excluding : Type array, ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, struct (int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId

    member this.Iterate (fn : Statement<'c, 'c2, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                let array = aref.Array
                let array2 = aref2.Array
                let mutable world = world
                let mutable enr = cache.ValuesEnumerator
                while enr.MoveNext () do
                    let struct (index, index2) = enr.Current
                    world <- fn.Invoke (&array.[index], &array2.[index2], world)
                world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let struct (index, index2) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                let array = aref.Array
                let array2 = aref2.Array
                let struct (index, index2) = cache.[entityId]
                fn.Invoke (&array.[index], &array2.[index2], world))
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        struct (unregistered || unregistered2, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        struct (unregistered || unregistered2, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _> (fun comp' comp2' world ->
                comp' <- comp
                comp2' <- comp2
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _> (fun comp comp2 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    world))
                    world
            struct (true, world)
        else struct (false, world)
    
    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> (excluding : Type array, ecs : 'w Ecs) =
            
    let cache = OrderedDictionary<uint64, struct (int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let store3 = ecs.IndexStore<'c3, CorrelatedStore<'c3, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'c3, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    let array = aref.Array
                    let array2 = aref2.Array
                    let array3 = aref3.Array
                    let mutable world = world
                    let mutable enr = cache.ValuesEnumerator
                    while enr.MoveNext () do
                        let struct (index, index2, index3) = enr.Current
                        world <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], world)
                    world)
                    world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let struct (index, index2, index3) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    let array = aref.Array
                    let array2 = aref2.Array
                    let array3 = aref3.Array
                    let struct (index, index2, index3) = cache.[entityId]
                    fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], world))
                    world)
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 comp3 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        struct (unregistered || unregistered2 || unregistered3, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        struct (unregistered || unregistered2 || unregistered3, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 comp3 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _, _> (fun comp' comp2' comp3' world ->
                comp' <- comp
                comp2' <- comp2
                comp3' <- comp3
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _, _> (fun comp comp2 comp3 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    comp3 <- Unchecked.defaultof<'c3>
                    world))
                    world
            struct (true, world)
        else struct (false, world)
    
    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            let indexOpt3 = store3.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'c3, 'c4, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> (excluding : Type array, ecs : 'w Ecs) =
            
    let cache = OrderedDictionary<uint64, struct (int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let store3 = ecs.IndexStore<'c3, CorrelatedStore<'c3, 'w>> ()
    let store4 = ecs.IndexStore<'c4, CorrelatedStore<'c4, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'c3, 'c4, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)
    
    new (ecs) = System<_, _, _, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        let array = aref.Array
                        let array2 = aref2.Array
                        let array3 = aref3.Array
                        let array4 = aref4.Array
                        let mutable world = world
                        let mutable enr = cache.ValuesEnumerator
                        while enr.MoveNext () do
                            let struct (index, index2, index3, index4) = enr.Current
                            world <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], world)
                        world)
                        world)
                    world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let struct (index, index2, index3, index4) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        let array = aref.Array
                        let array2 = aref2.Array
                        let array3 = aref3.Array
                        let array4 = aref4.Array
                        let struct (index, index2, index3, index4) = cache.[entityId]
                        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], world))
                        world)
                    world)
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 comp3 comp4 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _, _, _> (fun comp' comp2' comp3' comp4' world ->
                comp' <- comp
                comp2' <- comp2
                comp3' <- comp3
                comp4' <- comp4
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _, _, _> (fun comp comp2 comp3 comp4 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    comp3 <- Unchecked.defaultof<'c3>
                    comp4 <- Unchecked.defaultof<'c4>
                    world))
                    world
            struct (true, world)
        else struct (false, world)
    
    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            let indexOpt3 = store3.TryIndexCorrelatedToI entityId
            let indexOpt4 = store4.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'c3, 'c4, 'c5, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> (excluding : Type array, ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let store3 = ecs.IndexStore<'c3, CorrelatedStore<'c3, 'w>> ()
    let store4 = ecs.IndexStore<'c4, CorrelatedStore<'c4, 'w>> ()
    let store5 = ecs.IndexStore<'c5, CorrelatedStore<'c5, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _, _, _, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            let array = aref.Array
                            let array2 = aref2.Array
                            let array3 = aref3.Array
                            let array4 = aref4.Array
                            let array5 = aref5.Array
                            let mutable world = world
                            let mutable enr = cache.ValuesEnumerator
                            while enr.MoveNext () do
                                let struct (index, index2, index3, index4, index5) = enr.Current
                                world <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], world)
                            world)
                            world)
                        world)
                    world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let struct (index, index2, index3, index4, index5) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            let array = aref.Array
                            let array2 = aref2.Array
                            let array3 = aref3.Array
                            let array4 = aref4.Array
                            let array5 = aref5.Array
                            let struct (index, index2, index3, index4, index5) = cache.[entityId]
                            fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], world))
                            world)
                        world)
                    world)
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _, _, _, _> (fun comp' comp2' comp3' comp4' comp5' world ->
                comp' <- comp
                comp2' <- comp2
                comp3' <- comp3
                comp4' <- comp4
                comp5' <- comp5
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _, _, _, _> (fun comp comp2 comp3 comp4 comp5 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    comp3 <- Unchecked.defaultof<'c3>
                    comp4 <- Unchecked.defaultof<'c4>
                    comp5 <- Unchecked.defaultof<'c5>
                    world))
                    world
            struct (true, world)
        else struct (false, world)
    
    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            let indexOpt3 = store3.TryIndexCorrelatedToI entityId
            let indexOpt4 = store4.TryIndexCorrelatedToI entityId
            let indexOpt5 = store5.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> (excluding : Type array, ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let store3 = ecs.IndexStore<'c3, CorrelatedStore<'c3, 'w>> ()
    let store4 = ecs.IndexStore<'c4, CorrelatedStore<'c4, 'w>> ()
    let store5 = ecs.IndexStore<'c5, CorrelatedStore<'c5, 'w>> ()
    let store6 = ecs.IndexStore<'c6, CorrelatedStore<'c6, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _, _, _, _, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let array6 = store6.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            store6.WithCorrelatedsBuffered (fun aref6 world ->
                                let array = aref.Array
                                let array2 = aref2.Array
                                let array3 = aref3.Array
                                let array4 = aref4.Array
                                let array5 = aref5.Array
                                let array6 = aref6.Array
                                let mutable world = world
                                let mutable enr = cache.ValuesEnumerator
                                while enr.MoveNext () do
                                    let struct (index, index2, index3, index4, index5, index6) = enr.Current
                                    world <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], world)
                                world)
                                world)
                            world)
                        world)
                    world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let array6 = store6.Correlateds.Array
        let struct (index, index2, index3, index4, index5, index6) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            store6.WithCorrelatedsBuffered (fun aref6 world ->
                                let array = aref.Array
                                let array2 = aref2.Array
                                let array3 = aref3.Array
                                let array4 = aref4.Array
                                let array5 = aref5.Array
                                let array6 = aref6.Array
                                let struct (index, index2, index3, index4, index5, index6) = cache.[entityId]
                                fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], world))
                                world)
                            world)
                        world)
                    world)
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 comp6 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        let world = ecs.RegisterCorrelated<'c6> ordered comp6 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        let struct (unregistered6, world) = ecs.UnregisterCorrelated<'c6> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5 || unregistered6, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 comp6 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        let world = ecs.RegisterCorrelated<'c6> ordered comp6 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        let struct (unregistered6, world) = ecs.UnregisterCorrelated<'c6> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5 || unregistered6, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> Unchecked.defaultof<'c6> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 comp6 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _, _, _, _, _> (fun comp' comp2' comp3' comp4' comp5' comp6' world ->
                comp' <- comp
                comp2' <- comp2
                comp3' <- comp3
                comp4' <- comp4
                comp5' <- comp5
                comp6' <- comp6
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _, _, _, _, _> (fun comp comp2 comp3 comp4 comp5 comp6 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    comp3 <- Unchecked.defaultof<'c3>
                    comp4 <- Unchecked.defaultof<'c4>
                    comp5 <- Unchecked.defaultof<'c5>
                    comp6 <- Unchecked.defaultof<'c6>
                    world))
                    world
            struct (true, world)
        else struct (false, world)

    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            let indexOpt3 = store3.TryIndexCorrelatedToI entityId
            let indexOpt4 = store4.TryIndexCorrelatedToI entityId
            let indexOpt5 = store5.TryIndexCorrelatedToI entityId
            let indexOpt6 = store6.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS system.
type System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> (excluding : Type array, ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun (ty : Type) -> (ecs.IndexStore ty.Name).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name]
    let store = ecs.IndexStore<'c, CorrelatedStore<'c, 'w>> ()
    let store2 = ecs.IndexStore<'c2, CorrelatedStore<'c2, 'w>> ()
    let store3 = ecs.IndexStore<'c3, CorrelatedStore<'c3, 'w>> ()
    let store4 = ecs.IndexStore<'c4, CorrelatedStore<'c4, 'w>> ()
    let store5 = ecs.IndexStore<'c5, CorrelatedStore<'c5, 'w>> ()
    let store6 = ecs.IndexStore<'c6, CorrelatedStore<'c6, 'w>> ()
    let store7 = ecs.IndexStore<'c7, CorrelatedStore<'c7, 'w>> ()
    let allocated = HashSet<uint64> HashIdentity.Structural
    let unallocated = HashSet<uint64> HashIdentity.Structural

    static member Excluding<'x> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>|], ecs)
    static member Excluding<'x, 'x2> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>|], ecs)
    static member Excluding<'x, 'x2, 'x3> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>|], ecs)
    static member Excluding<'x, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7> ecs = new System<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w> ([|typeof<'x>; typeof<'x2>; typeof<'x3>; typeof<'x4>; typeof<'x5>; typeof<'x6>; typeof<'x7>|], ecs)

    new (ecs) = System<_, _, _, _, _, _, _, _> ([||], ecs)

    member this.Cached = cache.Keys
    member this.Allocated = allocated.AsReadOnly ()

    member this.Contains entityId =
        cache.ContainsKey entityId
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let array6 = store6.Correlateds.Array
        let array7 = store7.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6, index7) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            store6.WithCorrelatedsBuffered (fun aref6 world ->
                                store7.WithCorrelatedsBuffered (fun aref7 world ->
                                    let array = aref.Array
                                    let array2 = aref2.Array
                                    let array3 = aref3.Array
                                    let array4 = aref4.Array
                                    let array5 = aref5.Array
                                    let array6 = aref6.Array
                                    let array7 = aref7.Array
                                    let mutable world = world
                                    let mutable enr = cache.ValuesEnumerator
                                    while enr.MoveNext () do
                                        let struct (index, index2, index3, index4, index5, index6, index7) = enr.Current
                                        world <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], world)
                                    world)
                                    world)
                                world)
                            world)
                        world)
                    world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) state =
        let array = store.Correlateds.Array
        let array2 = store2.Correlateds.Array
        let array3 = store3.Correlateds.Array
        let array4 = store4.Correlateds.Array
        let array5 = store5.Correlateds.Array
        let array6 = store6.Correlateds.Array
        let array7 = store7.Correlateds.Array
        let struct (index, index2, index3, index4, index5, index6, index7) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>) world =
        store.WithCorrelatedsBuffered (fun aref world ->
            store2.WithCorrelatedsBuffered (fun aref2 world ->
                store3.WithCorrelatedsBuffered (fun aref3 world ->
                    store4.WithCorrelatedsBuffered (fun aref4 world ->
                        store5.WithCorrelatedsBuffered (fun aref5 world ->
                            store6.WithCorrelatedsBuffered (fun aref6 world ->
                                store7.WithCorrelatedsBuffered (fun aref7 world ->
                                    let array = aref.Array
                                    let array2 = aref2.Array
                                    let array3 = aref3.Array
                                    let array4 = aref4.Array
                                    let array5 = aref5.Array
                                    let array6 = aref6.Array
                                    let array7 = aref7.Array
                                    let struct (index, index2, index3, index4, index5, index6, index7) = cache.[entityId]
                                    fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], world))
                                    world)
                                world)
                            world)
                        world)
                    world)
                world)
            world

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 comp6 comp7 entityId world =
        let world = ecs.RegisterCorrelated<'c> ordered comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        let world = ecs.RegisterCorrelated<'c6> ordered comp6 entityId world
        let world = ecs.RegisterCorrelated<'c7> ordered comp7 entityId world
        world

    member this.UnregisterCorrelated (entityId : uint64) world =
        let struct (unregistered, world) = ecs.UnregisterCorrelated<'c> entityId world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        let struct (unregistered6, world) = ecs.UnregisterCorrelated<'c6> entityId world
        let struct (unregistered7, world) = ecs.UnregisterCorrelated<'c7> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5 || unregistered6 || unregistered7, world)

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 comp6 comp7 entityId world =
        let world = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId world
        let world = ecs.RegisterCorrelated<'c2> ordered comp2 entityId world
        let world = ecs.RegisterCorrelated<'c3> ordered comp3 entityId world
        let world = ecs.RegisterCorrelated<'c4> ordered comp4 entityId world
        let world = ecs.RegisterCorrelated<'c5> ordered comp5 entityId world
        let world = ecs.RegisterCorrelated<'c6> ordered comp6 entityId world
        let world = ecs.RegisterCorrelated<'c7> ordered comp7 entityId world
        world

    member this.UnregisterHierarchical parentIdOpt entityId world =
        let struct (unregistered, world) = ecs.UnregisterHierarchical<'c> parentIdOpt world
        let struct (unregistered2, world) = ecs.UnregisterCorrelated<'c2> entityId world
        let struct (unregistered3, world) = ecs.UnregisterCorrelated<'c3> entityId world
        let struct (unregistered4, world) = ecs.UnregisterCorrelated<'c4> entityId world
        let struct (unregistered5, world) = ecs.UnregisterCorrelated<'c5> entityId world
        let struct (unregistered6, world) = ecs.UnregisterCorrelated<'c6> entityId world
        let struct (unregistered7, world) = ecs.UnregisterCorrelated<'c7> entityId world
        struct (unregistered || unregistered2 || unregistered3 || unregistered4 || unregistered5 || unregistered6 || unregistered7, world)

    member this.Expand count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> Unchecked.defaultof<'c6> Unchecked.defaultof<'c7> entityId world
        world

    member this.Shrink world =
        let mutable world = world
        for entityId in Seq.map (fun inv -> UInt64.MaxValue - inv) unallocated do
            world <- this.UnregisterCorrelated entityId world |> snd'
        unallocated.Clear ()
        world

    member this.NextEntityId world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        let entityId = UInt64.MaxValue - entityIdInv
        struct (entityId, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 comp6 comp7 world =
        let world =
            if unallocated.Count = 0
            then this.Expand Constants.Ecs.PreallocateAmount world
            else world
        let entityIdInv = Seq.head unallocated
        unallocated.Remove entityIdInv |> ignore
        let entityId = UInt64.MaxValue - entityIdInv
        let world =
            this.Index entityId (new Statement<_, _, _, _, _, _, _, _> (fun comp' comp2' comp3' comp4' comp5' comp6' comp7' world ->
                comp' <- comp
                comp2' <- comp2
                comp3' <- comp3
                comp4' <- comp4
                comp5' <- comp5
                comp6' <- comp6
                comp7' <- comp7
                world))
                world
        struct (entityId, world)

    member this.Deallocate entityId world =
        if allocated.Remove entityId then
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            let world =
                this.Index entityId (new Statement<_, _, _, _, _, _, _, _> (fun comp comp2 comp3 comp4 comp5 comp6 comp7 world ->
                    comp <- Unchecked.defaultof<'c>
                    comp2 <- Unchecked.defaultof<'c2>
                    comp3 <- Unchecked.defaultof<'c3>
                    comp4 <- Unchecked.defaultof<'c4>
                    comp5 <- Unchecked.defaultof<'c5>
                    comp6 <- Unchecked.defaultof<'c6>
                    comp7 <- Unchecked.defaultof<'c7>
                    world))
                    world
            struct (true, world)
        else struct (false, world)
    
    interface System<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = store.TryIndexCorrelatedToI entityId
            let indexOpt2 = store2.TryIndexCorrelatedToI entityId
            let indexOpt3 = store3.TryIndexCorrelatedToI entityId
            let indexOpt4 = store4.TryIndexCorrelatedToI entityId
            let indexOpt5 = store5.TryIndexCorrelatedToI entityId
            let indexOpt6 = store6.TryIndexCorrelatedToI entityId
            let indexOpt7 = store7.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 && indexOpt7 > -1 then
                let storeIds = ecs.IndexStoreIds entityId
                if excluding.Count = 0 || not (storeIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6, indexOpt7)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 || indexOpt7 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>