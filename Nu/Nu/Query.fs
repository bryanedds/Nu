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

type Query
    (compNames : string HashSet, exclude : string HashSet -> bool, subqueries : Dictionary<string, Subquery>) =

    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []

    static member byName
        (compName, exclude, subqueries) =
        Query (HashSet.singleton HashIdentity.Structural compName, exclude, subqueries)

    static member byName
        (compName, comp2Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name], exclude, subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name; comp9Name], exclude, subqueries)

    static member byType<'c when
        'c : struct and 'c :> 'c Component>
        (exclude, subqueries) =
        Query (HashSet.singleton HashIdentity.Structural typeof<'c>.Name, exclude, subqueries)

    static member byType<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name], exclude, subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name], exclude, subqueries)

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
        (exclude, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name; typeof<'c9>.Name], exclude, subqueries)

    member this.Subqueries = subqueries

    member this.CheckCompatibility (archetype : Archetype) =
        let stores = archetype.Stores
        Seq.forall stores.ContainsKey compNames &&
        not (exclude archetype.ComponentNames) &&
        Subquery.evalMany archetype.Id.Terms subqueries

    member this.RegisterArchetype (archetype : Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.Iterate (compName, statement : Statement<'c, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
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
                let length = archetype.Length
                let stores = archetype.Stores
                let store = stores.[compName] :?> 'c Store
                let store2 = stores.[comp2Name] :?> 'c2 Store
                let store3 = stores.[comp3Name] :?> 'c3 Store
                let store4 = stores.[comp4Name] :?> 'c4 Store
                let store5 = stores.[comp5Name] :?> 'c5 Store
                let store6 = stores.[comp6Name] :?> 'c6 Store
                let store7 = stores.[comp7Name] :?> 'c7 Store
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
                let length = archetype.Length
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
                let length = archetype.Length
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
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype