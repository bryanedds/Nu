// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime

type Query<'c, 'w when
    'c : struct and 'c :> 'c Component> (compName : string) =

    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []

    member this.CheckCompatibility (system : 'w System) =
        let stores = system.Stores
        stores.ContainsKey compName

    member this.RegisterSystem (system : 'w System) =
        systems.Add (system.Id, system)

    member this.Iterate (statement : Statement<'c, 'w>) world =
        let mutable world = world
        for systemEntry in systems do
            let system = systemEntry.Value
            let stores = system.Stores
            let store = stores.[compName] :?> 'c Store
            let mutable i = 0
            while i < store.Length do
                world <- statement.Invoke (&store.[i], world)
                i <- inc i
        world

    interface 'w Query with
        member this.CheckCompatibility system = this.CheckCompatibility system
        member this.RegisterSystem system = this.RegisterSystem system

type Query<'c, 'c2, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component> (compName : string, comp2Name : string) =

    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []

    member this.CheckCompatibility (system : 'w System) =
        let stores = system.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name

    member this.RegisterSystem (system : 'w System) =
        systems.Add (system.Id, system)

    member this.Iterate (statement : Statement<'c, 'c2, 'w>) world =
        let mutable world = world
        for systemEntry in systems do
            let system = systemEntry.Value
            let stores = system.Stores
            let store = stores.[compName] :?> 'c Store
            let store2 = stores.[comp2Name] :?> 'c2 Store
            let mutable i = 0
            while i < store.Length do
                world <- statement.Invoke (&store.[i], &store2.[i], world)
                i <- inc i
        world

    interface 'w Query with
        member this.CheckCompatibility system = this.CheckCompatibility system
        member this.RegisterSystem system = this.RegisterSystem system

type Query<'c, 'c2, 'c3, 'w when
    'c : struct and 'c :> 'c Component and
    'c2 : struct and 'c2 :> 'c2 Component and
    'c3 : struct and 'c3 :> 'c3 Component> (compName : string, comp2Name : string, comp3Name : string) =

    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []

    member this.CheckCompatibility (system : 'w System) =
        let stores = system.Stores
        stores.ContainsKey compName &&
        stores.ContainsKey comp2Name &&
        stores.ContainsKey comp3Name

    member this.RegisterSystem (system : 'w System) =
        systems.Add (system.Id, system)

    member this.Iterate (statement : Statement<'c, 'c2, 'c3, 'w>) world =
        let mutable world = world
        for systemEntry in systems do
            let system = systemEntry.Value
            let stores = system.Stores
            let store = stores.[compName] :?> 'c Store
            let store2 = stores.[comp2Name] :?> 'c2 Store
            let store3 = stores.[comp3Name] :?> 'c3 Store
            let mutable i = 0
            while i < store.Length do
                world <- statement.Invoke (&store.[i], &store2.[i], &store3.[i], world)
                i <- inc i
        world

    interface 'w Query with
        member this.CheckCompatibility system = this.CheckCompatibility system
        member this.RegisterSystem system = this.RegisterSystem system