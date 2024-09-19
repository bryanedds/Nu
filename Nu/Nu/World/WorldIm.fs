// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

#nowarn "0052"

[<AutoOpen>]
module ImWorld =

    let mutable private context = Address.empty :> Address
    let mutable private contextRecent = context
    let private entities = Dictionary<Entity, (uint64 * obj) array> ()

    type World with

        static member private imUpdate world =
            Seq.fold (fun world (entry : KeyValuePair<Entity, _>) ->
                let entity = entry.Key
                if entity.GetExists world then
                    if not (entity.GetActive world)
                    then World.destroyEntityImmediate entity world
                    else entity.SetActive false world
                else world)
                world
                entities

        static member imBeginGame (world : World, [<ParamArray>] args : DefinitionContent array) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName context.Names)
            let world = World.setImCurrent gameAddress world
            let game = Nu.Game gameAddress
            Array.fold
                (fun world arg ->
                    match arg with
                    | PropertyContent pc -> game.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                    | _ -> world)
                world args

        static member imEndGame (world : World) =
            match world.ImCurrent with
            | :? (Game Address) as gameAddress ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndGame mismatch.")

        static member imBeginScreen (screenName, world : World, [<ParamArray>] args : DefinitionContent array) =
            let screenAddress = Address.makeFromArray (Array.add screenName context.Names)
            let world = World.setImCurrent screenAddress world
            let screen = Nu.Screen screenAddress
            Array.fold
                (fun world arg ->
                    match arg with
                    | PropertyContent pc when screen.GetExists world -> screen.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                    | _ -> world)
                world args

        static member imEndScreen (world : World) =
            match world.ImCurrent with
            | :? (Screen Address) as screenAddress ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndScreen mismatch.")

        static member imBeginGroup<'d when 'd :> GroupDispatcher> (groupName : string, world : World, [<ParamArray>] args : DefinitionContent array) : World =
            let groupAddress = Address.makeFromArray (Array.add groupName context.Names)
            let world = World.setImCurrent groupAddress world
            let group = Nu.Group groupAddress
            let world =
                if not (group.GetExists world)
                then World.createGroup<'d> (Some groupName) group.Screen world |> snd
                else world
            let world =
                Array.fold
                    (fun world arg ->
                        match arg with
                        | PropertyContent pc when group.GetExists world -> group.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                        | _ -> world)
                    world args
            if group.GetExists world
            then group.SetActive true world
            else world

        static member imEndGroup (world : World) =
            match world.ImCurrent with
            | :? (Group Address) as groupAddress ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndGroup mismatch.")

        static member imBeginEntity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName : string, world : World, [<ParamArray>] args : DefinitionContent array) : 'r * World =
            let entityAddress = Address.makeFromArray (Array.add entityName context.Names)
            let world = World.setImCurrent entityAddress world
            let entity = Nu.Entity entityAddress
            let (subs, world) =
                if not (entity.GetExists world) then
                    let world = World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                    let (subs, world) = init entity world
                    entities.[entity] <- subs
                    (subs, world)
                else ([||], world)
            let world =
                Array.fold
                    (fun world arg ->
                        match arg with
                        | PropertyContent pc when entity.GetExists world -> entity.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                        | _ -> world)
                    world args
            if entity.GetExists world then
                let result = inspect subs
                let world = entity.SetActive true world
                (result, world)
            else (Activator.CreateInstance<'r> (), world)

        static member imEndEntity (world : World) =
            match world.ImCurrent with
            | :? (Entity Address) as entityAddress ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndEntity mismatch.")

        static member imEntity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName, world, [<ParamArray>] args : DefinitionContent array) =
            let (result, world) = World.imBeginEntity<'d, 'r> (init, inspect, entityName, world, args)
            let world = World.imEndEntity world
            (result, world)

        static member imButton (buttonName : string, world : World, [<ParamArray>] args : DefinitionContent array) =
            let init (button : Entity) world =
                let result = ref false
                let world = World.createEntity<ButtonDispatcher> OverlayNameDescriptor.DefaultOverlay (Some button.Surnames) button.Group world |> snd
                let subKey = Gen.id64
                let world = World.subscribePlus subKey (fun evt world -> result.Value <- true; (Cascade, world)) button.ClickEvent Game world |> snd
                ([|(subKey, result :> obj)|], world)
            let inspect button (subs : (uint64 * obj) array) =
                let resultRef = snd subs.[0] :?> bool ref
                let result = resultRef.Value
                resultRef.Value <- false
            World.imEntity<ButtonDispatcher, _>
                ((fun button world ->
                    let result = ref false
                    let subKey = Gen.id64
                    let world = World.subscribePlus subKey (fun evt world -> result.Value <- true; (Cascade, world)) button.ClickEvent Game world |> snd
                    ([|(subKey, result :> obj)|], world)),
                 (fun subs ->
                    let resultRef = snd subs.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result),
                 buttonName,
                 world,
                 args)