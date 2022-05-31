// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime

type Query<'c, 'w when
    'c : struct and 'c :> 'c Component> (compName : string, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (ecs) = Query<_, _> (typeof<'c>.Name, ecs)

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName

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
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> (compName : string, comp2Name : string, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (ecs) = Query<_, _, _> (typeof<'c>.Name, typeof<'c2>.Name, ecs)

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name

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
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype

type Query<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> (compName : string, comp2Name : string, comp3Name : string, ecs : 'w Ecs) =

    let archetypes = dictPlus<ArchetypeId, 'w Archetype> HashIdentity.Structural []

    new (ecs) = Query<_, _, _, _> (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, ecs)

    member this.CheckCompatibility (archetype : 'w Archetype) =
        let stores = archetype.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name

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
        member this.CheckCompatibility archetype = this.CheckCompatibility archetype
        member this.RegisterArchetype archetype = this.RegisterArchetype archetype