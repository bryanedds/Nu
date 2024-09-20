// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldIm =

    type World with

        static member imUpdate world =
            OMap.fold (fun (world : World) simulant imSimulant ->
                if not imSimulant.Utilized then
                    let world = World.destroyImmediate simulant world
                    let imSimulants = OMap.remove simulant world.ImSimulants
                    World.setImSimulants imSimulants world
                else
                    if world.Imperative then
                        imSimulant.Utilized <- false
                        world
                    else
                        let imSimulants = OMap.add simulant { imSimulant with Utilized = false } world.ImSimulants
                        World.setImSimulants imSimulants world)
                world world.ImSimulants

        static member imBeginGame (world : World, [<ParamArray>] args : DefinitionContent array) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ImCurrent.Names)
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
            | :? (Game Address) ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndGame mismatch.")

        static member imBeginScreen (screenName, world : World, [<ParamArray>] args : DefinitionContent array) =
            let screenAddress = Address.makeFromArray (Array.add screenName world.ImCurrent.Names)
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
            | :? (Screen Address) ->
                World.setImCurrent Game.GameAddress world
            | _ -> raise (new InvalidOperationException "ImEndScreen mismatch.")

        static member imBeginGroup<'d when 'd :> GroupDispatcher> (groupName : string, world : World, [<ParamArray>] args : DefinitionContent array) : World =
            let groupAddress = Address.makeFromArray (Array.add groupName world.ImCurrent.Names)
            let world = World.setImCurrent groupAddress world
            let group = Nu.Group groupAddress
            let world =
                let imSimulants = world.ImSimulants
                match imSimulants.TryGetValue group with
                | (true, imGroup) ->
                    if world.Imperative then
                        imGroup.Utilized <- true
                        world
                    else
                        let imSimulants = OMap.add (group :> Simulant) { imGroup with Utilized = true } imSimulants
                        World.setImSimulants imSimulants world
                | (false, _) ->
                    let world = World.createGroup<'d> (Some groupName) group.Screen world |> snd
                    let imGroup = { Utilized = true; Subs = [||] }
                    let imSimulants = OMap.add (group :> Simulant) imGroup imSimulants
                    World.setImSimulants imSimulants world
            Array.fold
                (fun world arg ->
                    match arg with
                    | PropertyContent pc when group.GetExists world -> group.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                    | _ -> world)
                world args

        static member imEndGroup (world : World) =
            match world.ImCurrent with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setImCurrent currentAddress world
            | _ -> raise (new InvalidOperationException "ImEndGroup mismatch.")

        static member imBeginEntity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName : string, world : World, [<ParamArray>] args : DefinitionContent array) : 'r * World =
            let entityAddress = Address.makeFromArray (Array.add entityName world.ImCurrent.Names)
            let world = World.setImCurrent entityAddress world
            let entity = Nu.Entity entityAddress
            let (subs, world) =
                let imSimulants = world.ImSimulants
                match imSimulants.TryGetValue entity with
                | (true, imEntity) ->
                    if world.Imperative then
                        imEntity.Utilized <- true
                        (imEntity.Subs, world)
                    else
                        let imSimulants = OMap.add (entity :> Simulant) { imEntity with Utilized = true } imSimulants
                        let world = World.setImSimulants imSimulants world
                        (imEntity.Subs, world)
                | (false, _) ->
                    let world = World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                    let (subs, world) = init entity world
                    let imEntity = { Utilized = true; Subs = subs }
                    let imSimulants = OMap.add (entity :> Simulant) imEntity imSimulants
                    let world = World.setImSimulants imSimulants world
                    (subs, world)
            let world =
                Array.fold
                    (fun world arg ->
                        match arg with
                        | PropertyContent pc when entity.GetExists world -> entity.TrySet pc.PropertyLens.Name pc.PropertyValue world |> __c'
                        | _ -> world)
                    world args
            (inspect subs, world)

        static member imEndEntity (world : World) =
            match world.ImCurrent with
            | :? (Entity Address) as entityAddress when entityAddress.Length >= 4 ->
                let currentNames = Array.take (dec entityAddress.Length) entityAddress.Names
                let currentAddress =
                    if currentNames.Length = 3
                    then Address.makeFromArray<Group> currentNames :> Address
                    else Address.makeFromArray<Entity> currentNames
                World.setImCurrent currentAddress world
            | _ -> raise (new InvalidOperationException "ImEndEntity mismatch.")

        static member imEntity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName, world, [<ParamArray>] args : DefinitionContent array) =
            let (result, world) = World.imBeginEntity<'d, 'r> (init, inspect, entityName, world, args)
            let world = World.imEndEntity world
            (result, world)

        static member imButton (buttonName : string, world : World, [<ParamArray>] args : DefinitionContent array) =
            World.imEntity<ButtonDispatcher, _>
                ((fun button world ->
                    let result = ref false
                    let subKey = Gen.id64
                    let world = World.subscribePlus subKey (fun _ world -> result.Value <- true; (Cascade, world)) button.ClickEvent Game world |> snd
                    ([|(subKey, result :> obj)|], world)),
                 (fun subs ->
                    let resultRef = snd subs.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result),
                 buttonName,
                 world,
                 args)