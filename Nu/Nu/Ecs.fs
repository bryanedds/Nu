namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

type SystemId = string HashSet

/// The component that holds an entity's id.
type [<NoEquality; NoComparison; Struct>] EntityId =
    { mutable Active : bool
      mutable EntityId : uint64 }
    interface EntityId Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

type Store =
    interface
        abstract Length : int
        abstract Name : string
        abstract Item : int -> obj
        abstract SetItem : int -> obj -> unit
        abstract ZeroItem : int -> unit
        abstract Grow : unit -> unit
        end

type Store<'c when 'c : struct and 'c :> 'c Component> (name) =
    let mutable arr = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve
    member this.Item i = &arr.[i]
    member this.Length = arr.Length
    interface Store with
        member this.Length = arr.Length
        member this.Name = name
        member this.Item i = arr.[i] :> obj
        member this.SetItem index compObj = arr.[index] <- compObj :?> 'c
        member this.ZeroItem index = arr.[index] <- Unchecked.defaultof<'c>
        member this.Grow () =
            let arr' = Array.zeroCreate<'c> (arr.Length * 2)
            Array.Copy (arr, arr', arr.Length)
            arr <- arr'

type 'w Query =
    interface
        abstract CheckCompatibility : 'w System -> bool
        abstract RegisterSystem : 'w System -> unit
        end

and 'w System (storeTypes : Dictionary<string, Type>) =

    let mutable freeIndex = 0
    let freeList = hashSetPlus<int> HashIdentity.Structural []
    let (id : SystemId) = hashSetPlus HashIdentity.Structural []
    let stores = dictPlus<string, Store> HashIdentity.Structural []

    do
        let storeTypeGeneric = typedefof<EntityId Store>
        for storeTypeEntry in storeTypes do
            let storeType = storeTypeGeneric.MakeGenericType [|storeTypeEntry.Value|]
            let store = Activator.CreateInstance (storeType, [|storeTypeEntry.Key|]) :?> Store
            stores.Add (storeTypeEntry.Key, store)

    let store0 = stores.Values |> Seq.head

    member this.Id = id

    member this.Stores = stores

    member this.Register (comps : Dictionary<string, obj>) =
        if freeList.Count > 0 then
            let index = Seq.head freeList
            freeList.Remove index |> ignore<bool>
            for compEntry in comps do
                stores.[compEntry.Key].SetItem index compEntry.Value
        else
            if freeIndex < store0.Length then
                for storeEntry in stores do
                    storeEntry.Value.Grow ()
            for compEntry in comps do
                stores.[compEntry.Key].SetItem freeIndex compEntry.Value
            freeIndex <- inc freeIndex

    member this.Unregister (index : int) =
        for storeEntry in stores do
            storeEntry.Value.ZeroItem index
        if index = dec freeIndex then
            freeIndex <- dec freeIndex
        else
            freeList.Add index |> ignore<bool>

    member this.GetComponents index =
        let comps = dictPlus<string, obj> HashIdentity.Structural []
        for storeEntry in stores do
            comps.Add (storeEntry.Key, storeEntry.Value.[index])
        comps

type [<StructuralEquality; NoComparison>] 'w SystemSlot =
    { SystemIndex : int
      System : 'w System }

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

type 'w Ecs () =

    let mutable entityIdCurrent = 0UL
    let systemSlots = dictPlus<uint64, 'w SystemSlot> HashIdentity.Structural []
    let systems = dictPlus<SystemId, 'w System> HashIdentity.Structural []
    let componentTypes = dictPlus<string, Type> HashIdentity.Structural []
    let queries = List<'w Query> ()

    let createSystem (inferredType : Type) (systemId : SystemId) =
        let storeTypes =
            systemId |>
            Seq.map (fun componentName ->
                match componentTypes.TryGetValue componentName with
                | (true, componentType) -> (componentName, componentType)
                | (false, _) ->
                    if inferredType.Name = componentName
                    then componentTypes.Add (componentName, inferredType)
                    else failwith ("Could not infer component type of '" + componentName + "'.")
                    (componentName, inferredType)) |>
            dictPlus HashIdentity.Structural
        let system = System<'w> storeTypes
        systems.Add (system.Id, system)
        for query in queries do
            if query.CheckCompatibility system then
                query.RegisterSystem system
        system

    member private this.RegisterNamedComponentInternal<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) compName entityId world =
        match systemSlots.TryGetValue entityId with
        | (true, systemSlot) ->
            let system = systemSlot.System
            let comps = system.GetComponents systemSlot.SystemIndex
            system.Unregister systemSlot.SystemIndex
            comps.Add (compName, comp)
            let systemId = HashSet system.Id
            systemId.Add compName |> ignore<bool>
            let world =
                let mutable world = world
                match systems.TryGetValue systemId with
                | (true, system) -> system.Register comps
                | (false, _) ->
                    let system = createSystem typeof<'c> systemId
                    system.Register comps
                    world
            world
        | (false, _) ->
            let systemId = HashSet.singleton HashIdentity.Structural compName
            let system = createSystem typeof<'c> systemId
            let comps = Dictionary.singleton HashIdentity.Structural compName (comp :> obj)
            system.Register comps
            world

    member this.RegisterComponentType<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.Add (componentName, typeof<'c>)

    member this.RegisterNamedComponent<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) compName (entityIdOpt : uint64 voption) world =
        match entityIdOpt with
        | ValueSome entityId ->
            this.RegisterNamedComponentInternal<'c> comp compName entityId world
        | ValueNone ->
            entityIdCurrent <- inc entityIdCurrent
            let entityId = entityIdCurrent
            this.RegisterNamedComponentInternal<'c> comp compName entityId world

    member this.RegisterComponent<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) (entityIdOpt : uint64 voption) world =
        let compName = typeof<'c>.Name
        this.RegisterNamedComponent<'c> comp compName entityIdOpt world

    member this.RegisterQuery (query : 'w Query) =
        for systemEntry in systems do
            let system = systemEntry.Value
            if query.CheckCompatibility system then
                query.RegisterSystem system
        queries.Add query

[<RequireQualifiedAccess>]
module EcsEvents =

    let [<Literal>] Update = "Update"
    let [<Literal>] UpdateParallel = "UpdateParallel"
    let [<Literal>] PostUpdate = "PostUpdate"
    let [<Literal>] PostUpdateParallel = "PostUpdateParallel"
    let [<Literal>] Actualize = "Actualize"