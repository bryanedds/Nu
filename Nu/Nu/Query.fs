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
    'c : struct and 'c :> 'c Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, int> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()

    member this.Iterate (fn : Statement<'c, 's>) state =
        let array = system.Correlateds.Array
        let mutable world = state
        let mutable enr = cache.ValuesEnumerator
        while enr.MoveNext () do
            world <- fn.Invoke (&array.[enr.Current], world)
        world

    member this.IterateBuffered (fn : Statement<'c, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            let mutable world = world
            let mutable enr = cache.ValuesEnumerator
            while enr.MoveNext () do
                let index = enr.Current
                world <- fn.Invoke (&array.[index], world)
            world)
            world

    member this.Index (entityId : uint64) (fn : Statement<'c, 's>) state =
        let array = system.Correlateds.Array
        let index = cache.[entityId]
        fn.Invoke (&array.[index], state)

    member this.IndexBuffered (entityId : uint64) (fn : Statement<'c, 'w>) world =
        system.WithCorrelatedsBuffered (fun array world ->
            let index = cache.[entityId]
            fn.Invoke (&array.[index], world))
            world

    member this.RegisterCorrelated ordered comp entityId =
        ecs.RegisterCorrelated<'c> ordered comp entityId

    member this.UnregisterCorrelated (entityId : uint64) =
        ecs.UnregisterCorrelated<'c> entityId

    member this.RegisterHierarchical ordered parentIdOpt comp entityId =
        ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId

    member this.UnregisterHierarchical parentIdOpt =
        ecs.UnregisterHierarchical<'c> parentIdOpt
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            if indexOpt > -1 then
                cache.[entityId] <- indexOpt
                true
            else
                cache.Remove entityId |> ignore<bool>
                false

/// An ECS query.
type Query<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> (ecs : 'w Ecs) =

    let cache = OrderedDictionary<uint64, struct (int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()

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

    member this.RegisterCorrelated ordered comp comp2 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        struct (comp, comp2)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        result || result2

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        struct (comp, comp2)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        result || result2
    
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
            
    let cache = OrderedDictionary<uint64, struct (int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    
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

    member this.RegisterCorrelated ordered comp comp2 comp3 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        struct (comp, comp2, comp3)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        result || result2 || result3

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        struct (comp, comp2, comp3)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        result || result2 || result3
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 then
                cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3)
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
            
    let cache = OrderedDictionary<uint64, struct (int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    
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

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        struct (comp, comp2, comp3, comp4)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        result || result2 || result3 || result4

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        struct (comp, comp2, comp3, comp4)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        result || result2 || result3 || result4
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 then
                cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4)
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

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    
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

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        struct (comp, comp2, comp3, comp4, comp5)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        result || result2 || result3 || result4 || result5

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        struct (comp, comp2, comp3, comp4, comp5)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        result || result2 || result3 || result4 || result5
    
    interface Query<'w> with

        override this.Correlation = correlation

        override this.Filter entityId =
            let indexOpt = system.TryIndexCorrelatedToI entityId
            let indexOpt2 = system2.TryIndexCorrelatedToI entityId
            let indexOpt3 = system3.TryIndexCorrelatedToI entityId
            let indexOpt4 = system4.TryIndexCorrelatedToI entityId
            let indexOpt5 = system5.TryIndexCorrelatedToI entityId
            if  indexOpt > -1 && indexOpt2 > -1 && indexOpt3 > -1 && indexOpt4 > -1 && indexOpt5 > -1 then
                cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5)
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

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    
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

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 comp6 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        let comp6 = ecs.RegisterCorrelated<'c6> ordered comp6 entityId
        struct (comp, comp2, comp3, comp4, comp5, comp6)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        let result6 = ecs.UnregisterCorrelated<'c6> entityId
        result || result2 || result3 || result4 || result5 || result6

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 comp6 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        let comp6 = ecs.RegisterCorrelated<'c6> ordered comp6 entityId
        struct (comp, comp2, comp3, comp4, comp5, comp6)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        let result6 = ecs.UnregisterCorrelated<'c6> entityId
        result || result2 || result3 || result4 || result5 || result6
    
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
                cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6)
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

    let cache = OrderedDictionary<uint64, struct (int * int * int * int * int * int * int)> HashIdentity.Structural
    let correlation = hashSetPlus<string> StringComparer.Ordinal [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name]
    let system = ecs.IndexSystem<'c, SystemCorrelated<'c, 'w>> ()
    let system2 = ecs.IndexSystem<'c2, SystemCorrelated<'c2, 'w>> ()
    let system3 = ecs.IndexSystem<'c3, SystemCorrelated<'c3, 'w>> ()
    let system4 = ecs.IndexSystem<'c4, SystemCorrelated<'c4, 'w>> ()
    let system5 = ecs.IndexSystem<'c5, SystemCorrelated<'c5, 'w>> ()
    let system6 = ecs.IndexSystem<'c6, SystemCorrelated<'c6, 'w>> ()
    let system7 = ecs.IndexSystem<'c7, SystemCorrelated<'c7, 'w>> ()
    
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

    member this.RegisterCorrelated ordered comp comp2 comp3 comp4 comp5 comp6 comp7 entityId =
        let comp = ecs.RegisterCorrelated<'c> ordered comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        let comp6 = ecs.RegisterCorrelated<'c6> ordered comp6 entityId
        let comp7 = ecs.RegisterCorrelated<'c7> ordered comp7 entityId
        struct (comp, comp2, comp3, comp4, comp5, comp6, comp7)

    member this.UnregisterCorrelated (entityId : uint64) =
        let result = ecs.UnregisterCorrelated<'c> entityId
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        let result6 = ecs.UnregisterCorrelated<'c6> entityId
        let result7 = ecs.UnregisterCorrelated<'c7> entityId
        result || result2 || result3 || result4 || result5 || result6 || result7

    member this.RegisterHierarchical ordered parentIdOpt comp comp2 comp3 comp4 comp5 comp6 comp7 entityId =
        let comp = ecs.RegisterHierarchical<'c> ordered parentIdOpt comp entityId
        let comp2 = ecs.RegisterCorrelated<'c2> ordered comp2 entityId
        let comp3 = ecs.RegisterCorrelated<'c3> ordered comp3 entityId
        let comp4 = ecs.RegisterCorrelated<'c4> ordered comp4 entityId
        let comp5 = ecs.RegisterCorrelated<'c5> ordered comp5 entityId
        let comp6 = ecs.RegisterCorrelated<'c6> ordered comp6 entityId
        let comp7 = ecs.RegisterCorrelated<'c7> ordered comp7 entityId
        struct (comp, comp2, comp3, comp4, comp5, comp6, comp7)

    member this.UnregisterHierarchical parentIdOpt entityId =
        let result = ecs.UnregisterHierarchical<'c> parentIdOpt
        let result2 = ecs.UnregisterCorrelated<'c2> entityId
        let result3 = ecs.UnregisterCorrelated<'c3> entityId
        let result4 = ecs.UnregisterCorrelated<'c4> entityId
        let result5 = ecs.UnregisterCorrelated<'c5> entityId
        let result6 = ecs.UnregisterCorrelated<'c6> entityId
        let result7 = ecs.UnregisterCorrelated<'c7> entityId
        result || result2 || result3 || result4 || result5 || result6 || result7
    
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
                cache.[entityId] <- struct (indexOpt, indexOpt2, indexOpt3, indexOpt4, indexOpt5, indexOpt6, indexOpt7)
                true
            elif indexOpt > -1 || indexOpt2 > -1 || indexOpt3 > -1 || indexOpt4 > -1 || indexOpt5 > -1 || indexOpt6 > -1 || indexOpt7 > -1 then // OPTIMIZATION: make sure it exists and needs removing
                cache.Remove entityId |> ignore<bool>
                false
            else false