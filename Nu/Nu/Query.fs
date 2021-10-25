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

/// An ECS query.
type Query<'c, 'w when
    'c : struct and 'c :> 'c Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =
    
    let cache = OrderedDictionary<uint64, int> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system

    member this.Iterate (fn : Statement<'c, 's>) state =
        if Seq.isEmpty excluding then
            let array = system.Correlateds.Array
            let mutable state = state
            for i in 0 .. array.Length - 1 do
                state <- fn.Invoke (&array.[i], state)
            state
        else
            let array = system.Correlateds.Array
            let mutable state = state
            let mutable enr = cache.ValuesEnumerator
            while enr.MoveNext () do
                state <- fn.Invoke (&array.[enr.Current], state)
            state

    member this.IterateBuffered (fn : Statement<'c, 'w>) world =
        if Seq.isEmpty excluding then
            system.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let mutable world = world
                for i in 0 .. array.Length - 1 do
                    world <- fn.Invoke (&array.[i], world)
                world)
                world
        else
            system.WithCorrelatedsBuffered (fun aref world ->
                let array = aref.Array
                let mutable world = world
                let mutable enr = cache.ValuesEnumerator
                while enr.MoveNext () do
                    let index = enr.Current
                    world <- fn.Invoke (&array.[index], world)
                world)
                world

    member this.Index (entityId : uint64) (fn : Statement<'c, 's>) state =
        if Seq.isEmpty excluding then
            let cref = system.IndexCorrelated entityId
            fn.Invoke (&cref.Index, state)
        else
            let array = system.Correlateds.Array
            let index = cache.[entityId]
            fn.Invoke (&array.[index], state)

    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'w>) world =
        if Seq.isEmpty excluding then
            system.WithCorrelatedsBuffered (fun array world ->
                let index = system.IndexCorrelatedToI entityId
                fn.Invoke (&array.[index], world))
                world
        else
            system.WithCorrelatedsBuffered (fun array world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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

    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            if Seq.isEmpty excluding then
                () // just pulls from lone associated system
            else
                let indexOpt = system.TryIndexCorrelatedToI entityId
                if indexOpt > -1 then
                    let systemIds = ecs.IndexSystemIds entityId
                    if not (systemIds.IsSupersetOf excluding)
                    then cache.[entityId] <- indexOpt
                    else cache.Remove entityId |> ignore<bool>
                else cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =

    let cache = OrderedDictionary<uint64, struct (int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2

    member this.Iterate (fn : Statement<'c, 'c2, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                let mutable world = world
                let mutable enr = cache.ValuesEnumerator
                while enr.MoveNext () do
                    let struct (index, index2) = enr.Current
                    world <- fn.Invoke (&array.[index], &array2.[index2], world)
                world)
                world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 'c2, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let struct (index, index2) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =
            
    let cache = OrderedDictionary<uint64, struct (int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2
    member this.System3 = system3
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
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
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let struct (index, index2, index3) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 comp3 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =
            
    let cache = OrderedDictionary<uint64, struct (int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2
    member this.System3 = system3
    member this.System4 = system4
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
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
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let struct (index, index2, index3, index4) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 comp3 comp4 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2
    member this.System3 = system3
    member this.System4 = system4
    member this.System5 = system5
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
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
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let struct (index, index2, index3, index4, index5) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2
    member this.System3 = system3
    member this.System4 = system4
    member this.System5 = system5
    member this.System6 = system6
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
                            system6.WithCorrelatedsBuffered (fun array6 world ->
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
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let struct (index, index2, index3, index4, index5, index6) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
                            system6.WithCorrelatedsBuffered (fun array6 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> Unchecked.defaultof<'c6> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 comp6 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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

    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            let indexOpt6 = system6.TryIndexCorrelatedToI entityId
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>

/// An ECS query.
type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component> (ecs : 'w Ecs, [<ParamArray>] excluding : Type array) =

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int * int)> HashIdentity.Structural
    let excluding = hashSetPlus<uint> HashIdentity.Structural (Seq.map (fun ty -> (ecs.IndexSystem (getTypeName ty)).Id) excluding)
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    let system7 = ecs.IndexSystem<'c7, SystemCorrelated<'c7, 'w>> ()
    let unallocated = HashSet<uint64> HashIdentity.Structural
    let allocated = HashSet<uint64> HashIdentity.Structural

    member this.System = system
    member this.System2 = system2
    member this.System3 = system3
    member this.System4 = system4
    member this.System5 = system5
    member this.System6 = system6
    member this.System7 = system7
    
    member this.Iterate (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) state =
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let array7 = system7.Correlateds.Array
        let mutable state = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            let struct (index, index2, index3, index4, index5, index6, index7) = enr.Current
            state <- fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], state)
        state

    member this.IterateBuffered (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
                            system6.WithCorrelatedsBuffered (fun array6 world ->
                                system7.WithCorrelatedsBuffered (fun array7 world ->
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
        let array = system.Correlateds.Array
        let array2 = system2.Correlateds.Array
        let array3 = system3.Correlateds.Array
        let array4 = system4.Correlateds.Array
        let array5 = system5.Correlateds.Array
        let array6 = system6.Correlateds.Array
        let array7 = system7.Correlateds.Array
        let struct (index, index2, index3, index4, index5, index6, index7) = cache.[entityId]
        fn.Invoke (&array.[index], &array2.[index2], &array3.[index3], &array4.[index4], &array5.[index5], &array6.[index6], &array7.[index7], state)
        
    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            system2.WithCorrelatedsBuffered (fun array2 world ->
                system3.WithCorrelatedsBuffered (fun array3 world ->
                    system4.WithCorrelatedsBuffered (fun array4 world ->
                        system5.WithCorrelatedsBuffered (fun array5 world ->
                            system6.WithCorrelatedsBuffered (fun array6 world ->
                                system7.WithCorrelatedsBuffered (fun array7 world ->
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

    member this.Preallocate count world =
        let mutable world = world
        for _ in 0 .. count - 1 do
            let entityId = Gen.id64
            let entityIdInv = UInt64.MaxValue - entityId
            unallocated.Add entityIdInv |> ignore
            world <- this.RegisterCorrelated true Unchecked.defaultof<'c> Unchecked.defaultof<'c2> Unchecked.defaultof<'c3> Unchecked.defaultof<'c4> Unchecked.defaultof<'c5> Unchecked.defaultof<'c6> Unchecked.defaultof<'c7> entityId world
        world

    member this.TryClear world =
        if Seq.isEmpty allocated then
            let mutable world = world
            for entityIdInv in unallocated do
                let entityId = UInt64.MaxValue - entityIdInv
                world <- this.UnregisterCorrelated entityId world |> snd'
            unallocated.Clear ()
            struct (true, world)
        else struct (false, world)

    member this.Allocate comp comp2 comp3 comp4 comp5 comp6 comp7 world =
        let world =
            if Seq.isEmpty unallocated
            then this.Preallocate Constants.Ecs.PreallocateAmount world
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
            if indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 && indexOpt6 > -1 && indexOpt7 > -1 then
                let systemIds = ecs.IndexSystemIds entityId
                if not (systemIds.IsSupersetOf excluding)
                then cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6, indexOpt7)
                else cache.Remove entityId |> ignore<bool>
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 || indexOpt7 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>