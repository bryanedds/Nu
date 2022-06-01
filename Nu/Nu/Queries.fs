// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type [<AbstractClass; Sealed>] Exclude =

    static member zero =
        fun (_ : string HashSet) -> false

    static member byName (compName : string, componentNames : string HashSet) =
        componentNames.Contains compName

    static member byName (compName : string, comp2Name : string, componentNames : string HashSet) =
        componentNames.Contains compName &&
        componentNames.Contains comp2Name

    static member byName (compName : string, comp2Name : string, comp3Name : string, componentNames : string HashSet) =
        componentNames.Contains compName &&
        componentNames.Contains comp2Name &&
        componentNames.Contains comp3Name

    static member byType<'c when
       'c : struct and 'c :> 'c Component>
       (componentNames : string HashSet) =
       Exclude.byName (typeof<'c>.Name, componentNames)

    static member byType<'c, 'c2 when
       'c : struct and 'c :> 'c Component and
       'c2 : struct and 'c2 :> 'c2 Component>
       (componentNames : string HashSet) =
       Exclude.byName (typeof<'c>.Name, typeof<'c2>.Name, componentNames)

    static member byType<'c, 'c2, 'c3 when
       'c : struct and 'c :> 'c Component and
       'c2 : struct and 'c2 :> 'c2 Component and
       'c3 : struct and 'c3 :> 'c3 Component>
       (componentNames : string HashSet) =
       Exclude.byName (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, componentNames)

type 'w Query
    (compNames : string HashSet, exclude : string HashSet -> bool, subqueries : Dictionary<string, Subquery>, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    static member create
        (compName, exclude, subqueries, ecs) =
        Query<'w> (HashSet.singleton HashIdentity.Structural compName, exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name], exclude, subqueries, ecs)

    static member create
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name; comp9Name], exclude, subqueries, ecs)

    static member create<'c when
        'c : struct and 'c :> 'c Component>
        (exclude, subqueries, ecs) =
        Query<'w> (HashSet.singleton HashIdentity.Structural typeof<'c>.Name, exclude, subqueries, ecs)

    static member create<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name], exclude, subqueries, ecs)

    static member create<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (exclude, subqueries, ecs) =
        Query<'w> (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name; typeof<'c9>.Name], exclude, subqueries, ecs)

    member this.Subqueries = subqueries

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        Seq.forall stores.ContainsKey compNames &&
        not (exclude archetype.ComponentNames) &&
        Subquery.evalMany archetype.Terms subqueries

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.Iterate (compName, statement : Statement<'c, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke (&store.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, statement : Statement<'c, 'c2, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke (&store.[i], &store2.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, statement : Statement<'c, 'c2, 'c3, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, statement : Statement<'c, 'c2, 'c3, 'c4, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
                let store7 = stores.[comp7Name] :?> 'c7 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
                let store7 = stores.[comp7Name] :?> 'c7 Store
                let store8 = stores.[comp8Name] :?> 'c8 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)
                    i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
                let store7 = stores.[comp7Name] :?> 'c7 Store
                let store8 = stores.[comp8Name] :?> 'c8 Store
                let store9 = stores.[comp9Name] :?> 'c9 Store
                let mutable i = 0
                while i < store.Length do
                    state <- statement.Invoke
                        (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)
                    i <- inc i
            state

    member this.Iterate<'c, 's when
        'c : struct and 'c :> 'c Component>
        (statement : Statement<'c, 's>) =
        this.Iterate (typeof<'c>.Name, statement)

    member this.Iterate<'c, 'c2, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (statement : Statement<'c, 'c2, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (statement : Statement<'c, 'c2, 'c3, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, typeof<'c9>.Name, statement)

    member this.Index (compName, statement : Statement<'c, 's>, entityId, state) =
        let archetypeSlot = ecs.IndexArchetypeSlot entityId
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], state)

    member this.Index (compName, comp2Name, statement : Statement<'c, 'c2, 's>, entityId, state) =
        let archetypeSlot = ecs.IndexArchetypeSlot entityId
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let store2 = stores.[comp2Name] :?> 'c2 Store
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], state)

    member this.Index (compName, comp2Name, comp3Name, statement : Statement<'c, 'c2, 'c3, 's>, entityId, state) =
        let archetypeSlot = ecs.IndexArchetypeSlot entityId
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let store2 = stores.[comp2Name] :?> 'c2 Store
        let store3 = stores.[comp3Name] :?> 'c3 Store
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], state)

    interface 'w IQuery with
        member this.Subqueries = this.Subqueries
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype