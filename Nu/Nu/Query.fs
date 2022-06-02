// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type Query
    (compNames : string HashSet, subqueries : Dictionary<string, Subquery>) =

    let subqueries = Dictionary (subqueries, StringComparer.Ordinal)
    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []

    do
        for compName in compNames do
            subqueries.Add (Constants.Ecs.IntraComponentPrefix + compName, Is)

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")
        
    member this.Subqueries =
        subqueries :> IReadOnlyDictionary<_, _>

    member this.TryRegisterArchetype (archetype : Archetype) =
        if  not (archetypes.ContainsKey archetype.Id) &&
            Subquery.evalMany archetype.Id.Terms subqueries then
            archetypes.Add (archetype.Id, archetype)

    member this.Iterate (compName, statement : Statement<'c, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, statement : Statement<'c, 'c2, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, statement : Statement<'c, 'c2, 'c3, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, statement : Statement<'c, 'c2, 'c3, 'c4, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
                let store9 = this.IndexStore<'c9> comp9Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)
                        i <- inc i
            state

    member this.Iterate<'c, 's when
        'c : struct and 'c :> 'c Component>
        (statement : Statement<'c, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, statement)

    member this.Iterate<'c, 'c2, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (statement : Statement<'c, 'c2, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (statement : Statement<'c, 'c2, 'c3, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) : 's -> 's =
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
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) : 's -> 's =
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
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, typeof<'c9>.Name, statement)

    interface IQuery with
        member this.Subqueries = this.Subqueries
        member this.TryRegisterArchetype archetype = this.TryRegisterArchetype archetype

    static member byName
        (compName, subqueries) =
        Query (HashSet.singleton HashIdentity.Structural compName, subqueries)

    static member byName
        (compName, comp2Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name; comp9Name], subqueries)

    static member byType<'c when
        'c : struct and 'c :> 'c Component>
        (subqueries) =
        Query (HashSet.singleton HashIdentity.Structural typeof<'c>.Name, subqueries)

    static member byType<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name], subqueries)

    static member byType<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name; typeof<'c9>.Name], subqueries)