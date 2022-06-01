// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type [<AbstractClass; Sealed>] Exclude =

    static member byName (compName : string, componentNames : string HashSet) =
        not (componentNames.Contains compName)

    static member byName (compName : string, comp2Name : string, componentNames : string HashSet) =
       not (componentNames.Contains compName) &&
       not (componentNames.Contains comp2Name)

    static member byName (compName : string, comp2Name : string, comp3Name : string, componentNames : string HashSet) =
       not (componentNames.Contains compName) &&
       not (componentNames.Contains comp2Name) &&
       not (componentNames.Contains comp3Name)

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

type Query<'c, 'w when
    'c : struct and 'c :> 'c Component>
    (compName : string, excluding : string HashSet -> bool, subquery : Subquery, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) = Query<_, _> (typeof<'c>.Name, excluding, subquery, ecs)
    new (subquery, ecs) = Query<_, _> (typeof<'c>.Name, tautology, subquery, ecs)
    new (ecs) = Query<_, _> (typeof<'c>.Name, tautology, Wildcard, ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponent compName (comp : 'c) entityId world =
        ecs.RegisterNamedComponent compName comp entityId world

    member this.RegisterComponent (comp : 'c) entityId world =
        this.RegisterNamedComponent typeof<'c>.Name comp entityId world

    member this.UnregisterNamedComponent compName entityId world =
        ecs.UnregisterNamedComponent compName entityId world

    member this.UnregisterComponent<'c> entityId world =
        ecs.UnregisterComponent<'c> entityId world

    member this.Iterate (statement : Statement<'c, 's>) state =
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

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component>
    (compName : string, comp2Name : string, excluding : string HashSet -> bool, subquery : Subquery, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) = Query<_, _, _> (typeof<'c>.Name, typeof<'c2>.Name, excluding, subquery, ecs)
    new (subquery, ecs) = Query<_, _, _> (typeof<'c>.Name, typeof<'c2>.Name, tautology, subquery, ecs)
    new (ecs) = Query<_, _, _> (typeof<'c>.Name, typeof<'c2>.Name, tautology, Wildcard, ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents compName (comp : 'c) comp2Name (comp2 : 'c2) entityId world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        world

    member this.RegisterComponents (comp : 'c) (comp2 : 'c2) entityId world =
        this.RegisterNamedComponents typeof<'c>.Name comp typeof<'c2>.Name comp2 entityId world

    member this.UnregisterNamedComponents compName comp2Name entityId world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents typeof<'c>.Name typeof<'c2>.Name entityId world

    member this.Iterate (statement : Statement<'c, 'c2, 's>) state =
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

    member this.Index (statement : Statement<'c, 'c2, 's>) entityId state =
        let archetypeSlot = ecs.IndexArchetypeSlot entityId
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let store2 = stores.[comp2Name] :?> 'c2 Store
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], state)

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component>
    (compName : string, comp2Name : string, comp3Name : string, excluding : string HashSet -> bool, subquery : Subquery, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) = Query<_, _, _, _> (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, excluding, subquery, ecs)
    new (subquery, ecs) = Query<_, _, _, _> (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, tautology, subquery, ecs)
    new (ecs) = Query<_, _, _, _> (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, tautology, Wildcard, ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents compName (comp : 'c) comp2Name (comp2 : 'c2) comp3Name (comp3 : 'c3) entityId world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        world

    member this.RegisterComponents (comp : 'c) (comp2 : 'c2) (comp3 : 'c3) entityId world =
        this.RegisterNamedComponents typeof<'c>.Name comp typeof<'c2>.Name comp2 typeof<'c3>.Name comp3 entityId world

    member this.UnregisterNamedComponents compName comp2Name comp3Name entityId world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents typeof<'c>.Name typeof<'c2>.Name typeof<'c3>.Name entityId world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 's>) state =
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

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'c5, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     comp5Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        stores.ContainsKey comp5Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        comp5Name (comp5 : 'c5)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        let world = ecs.RegisterNamedComponent comp5Name comp5 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        (comp5 : 'c5)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            typeof<'c5>.Name comp5
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        comp5Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        let world = ecs.UnregisterNamedComponent comp5Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            typeof<'c5>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     &store5.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     comp5Name : string,
     comp6Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        stores.ContainsKey comp5Name &&
        stores.ContainsKey comp6Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        comp5Name (comp5 : 'c5)
        comp6Name (comp6 : 'c6)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        let world = ecs.RegisterNamedComponent comp5Name comp5 entityId world
        let world = ecs.RegisterNamedComponent comp6Name comp6 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        (comp5 : 'c5)
        (comp6 : 'c6)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            typeof<'c5>.Name comp5
            typeof<'c6>.Name comp6
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        comp5Name
        comp6Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        let world = ecs.UnregisterNamedComponent comp5Name entityId world
        let world = ecs.UnregisterNamedComponent comp6Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            typeof<'c5>.Name
            typeof<'c6>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     &store5.[i],
                     &store6.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     comp5Name : string,
     comp6Name : string,
     comp7Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        stores.ContainsKey comp5Name &&
        stores.ContainsKey comp6Name &&
        stores.ContainsKey comp7Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        comp5Name (comp5 : 'c5)
        comp6Name (comp6 : 'c6)
        comp7Name (comp7 : 'c7)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        let world = ecs.RegisterNamedComponent comp5Name comp5 entityId world
        let world = ecs.RegisterNamedComponent comp6Name comp6 entityId world
        let world = ecs.RegisterNamedComponent comp7Name comp7 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        (comp5 : 'c5)
        (comp6 : 'c6)
        (comp7 : 'c7)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            typeof<'c5>.Name comp5
            typeof<'c6>.Name comp6
            typeof<'c7>.Name comp7
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        comp5Name
        comp6Name
        comp7Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        let world = ecs.UnregisterNamedComponent comp5Name entityId world
        let world = ecs.UnregisterNamedComponent comp6Name entityId world
        let world = ecs.UnregisterNamedComponent comp7Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            typeof<'c5>.Name
            typeof<'c6>.Name
            typeof<'c7>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     &store5.[i],
                     &store6.[i],
                     &store7.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     comp5Name : string,
     comp6Name : string,
     comp7Name : string,
     comp8Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        stores.ContainsKey comp5Name &&
        stores.ContainsKey comp6Name &&
        stores.ContainsKey comp7Name &&
        stores.ContainsKey comp8Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        comp5Name (comp5 : 'c5)
        comp6Name (comp6 : 'c6)
        comp7Name (comp7 : 'c7)
        comp8Name (comp8 : 'c8)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        let world = ecs.RegisterNamedComponent comp5Name comp5 entityId world
        let world = ecs.RegisterNamedComponent comp6Name comp6 entityId world
        let world = ecs.RegisterNamedComponent comp7Name comp7 entityId world
        let world = ecs.RegisterNamedComponent comp8Name comp8 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        (comp5 : 'c5)
        (comp6 : 'c6)
        (comp7 : 'c7)
        (comp8 : 'c8)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            typeof<'c5>.Name comp5
            typeof<'c6>.Name comp6
            typeof<'c7>.Name comp7
            typeof<'c8>.Name comp8
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        comp5Name
        comp6Name
        comp7Name
        comp8Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        let world = ecs.UnregisterNamedComponent comp5Name entityId world
        let world = ecs.UnregisterNamedComponent comp6Name entityId world
        let world = ecs.UnregisterNamedComponent comp7Name entityId world
        let world = ecs.UnregisterNamedComponent comp8Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            typeof<'c5>.Name
            typeof<'c6>.Name
            typeof<'c7>.Name
            typeof<'c8>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     &store5.[i],
                     &store6.[i],
                     &store7.[i],
                     &store8.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component and
    'c4 : struct and 'c4 :> 'c4 Component and
    'c5 : struct and 'c5 :> 'c5 Component and
    'c6 : struct and 'c6 :> 'c6 Component and
    'c7 : struct and 'c7 :> 'c7 Component and
    'c8 : struct and 'c8 :> 'c8 Component and
    'c9 : struct and 'c9 :> 'c9 Component>
    (compName : string,
     comp2Name : string,
     comp3Name : string,
     comp4Name : string,
     comp5Name : string,
     comp6Name : string,
     comp7Name : string,
     comp8Name : string,
     comp9Name : string,
     excluding : string HashSet -> bool, 
     subquery : Subquery,
     ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (excluding, subquery, ecs) =
        Query<_, _, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             typeof<'c9>.Name,
             excluding,
             subquery,
             ecs)

    new (subquery, ecs) =
        Query<_, _, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             typeof<'c9>.Name,
             tautology,
             subquery,
             ecs)

    new (ecs) =
        Query<_, _, _, _, _, _, _, _, _, _>
            (typeof<'c>.Name,
             typeof<'c2>.Name,
             typeof<'c3>.Name,
             typeof<'c4>.Name,
             typeof<'c5>.Name,
             typeof<'c6>.Name,
             typeof<'c7>.Name,
             typeof<'c8>.Name,
             typeof<'c9>.Name,
             tautology,
             Wildcard,
             ecs)

    member this.Subquery = subquery

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name &&
        stores.ContainsKey comp4Name &&
        stores.ContainsKey comp5Name &&
        stores.ContainsKey comp6Name &&
        stores.ContainsKey comp7Name &&
        stores.ContainsKey comp8Name &&
        stores.ContainsKey comp9Name &&
        excluding archetype.ComponentNames &&
        Subquery.eval archetype.TermsValueCollection subquery

    member this.RegisterArchetype (archetype : 'w Archetype) =
        archetypes.Add (archetype.Id, archetype)

    member this.RegisterNamedComponents
        compName (comp : 'c)
        comp2Name (comp2 : 'c2)
        comp3Name (comp3 : 'c3)
        comp4Name (comp4 : 'c4)
        comp5Name (comp5 : 'c5)
        comp6Name (comp6 : 'c6)
        comp7Name (comp7 : 'c7)
        comp8Name (comp8 : 'c8)
        comp9Name (comp9 : 'c9)
        entityId
        world =
        let world = ecs.RegisterNamedComponent compName comp entityId world
        let world = ecs.RegisterNamedComponent comp2Name comp2 entityId world
        let world = ecs.RegisterNamedComponent comp3Name comp3 entityId world
        let world = ecs.RegisterNamedComponent comp4Name comp4 entityId world
        let world = ecs.RegisterNamedComponent comp5Name comp5 entityId world
        let world = ecs.RegisterNamedComponent comp6Name comp6 entityId world
        let world = ecs.RegisterNamedComponent comp7Name comp7 entityId world
        let world = ecs.RegisterNamedComponent comp8Name comp8 entityId world
        let world = ecs.RegisterNamedComponent comp9Name comp9 entityId world
        world

    member this.RegisterComponents
        (comp : 'c)
        (comp2 : 'c2)
        (comp3 : 'c3)
        (comp4 : 'c4)
        (comp5 : 'c5)
        (comp6 : 'c6)
        (comp7 : 'c7)
        (comp8 : 'c8)
        (comp9 : 'c9)
        entityId
        world =
        this.RegisterNamedComponents
            typeof<'c>.Name comp
            typeof<'c2>.Name comp2
            typeof<'c3>.Name comp3
            typeof<'c4>.Name comp4
            typeof<'c5>.Name comp5
            typeof<'c6>.Name comp6
            typeof<'c7>.Name comp7
            typeof<'c8>.Name comp8
            typeof<'c9>.Name comp9
            entityId
            world

    member this.UnregisterNamedComponents
        compName
        comp2Name
        comp3Name
        comp4Name
        comp5Name
        comp6Name
        comp7Name
        comp8Name
        comp9Name
        entityId
        world =
        let world = ecs.UnregisterNamedComponent compName entityId world
        let world = ecs.UnregisterNamedComponent comp2Name entityId world
        let world = ecs.UnregisterNamedComponent comp3Name entityId world
        let world = ecs.UnregisterNamedComponent comp4Name entityId world
        let world = ecs.UnregisterNamedComponent comp5Name entityId world
        let world = ecs.UnregisterNamedComponent comp6Name entityId world
        let world = ecs.UnregisterNamedComponent comp7Name entityId world
        let world = ecs.UnregisterNamedComponent comp8Name entityId world
        let world = ecs.UnregisterNamedComponent comp9Name entityId world
        world

    member this.UnregisterComponents entityId world =
        this.UnregisterNamedComponents
            typeof<'c>.Name
            typeof<'c2>.Name
            typeof<'c3>.Name
            typeof<'c4>.Name
            typeof<'c5>.Name
            typeof<'c6>.Name
            typeof<'c7>.Name
            typeof<'c8>.Name
            typeof<'c9>.Name
            entityId
            world

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) state =
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
                    (&store.[i],
                     &store2.[i],
                     &store3.[i],
                     &store4.[i],
                     &store5.[i],
                     &store6.[i],
                     &store7.[i],
                     &store8.[i],
                     &store9.[i],
                     state)
                i <- inc i
        state

    interface 'w Query with
        member this.Subquery = this.Subquery
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype