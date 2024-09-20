// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldIm =

    /// Define an im property definition.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ImPropertyName = lens.Name; ImPropertyValue = value }

    type World with

        static member internal imUpdate world =
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

        static member scope (game : Game, world : World, [<ParamArray>] args : ImProperty array) =
            let world = World.setImCurrent game.GameAddress world
            Array.fold
                (fun world arg -> game.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c')
                world args

        static member scope (screen : Screen, world : World, [<ParamArray>] args : ImProperty array) =
            let world = World.setImCurrent screen.ScreenAddress world
            Array.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                    else world)
                world args

        static member scope (group : Group, world : World, [<ParamArray>] args : ImProperty array) =
            let world = World.setImCurrent group.GroupAddress world
            Array.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                    else world)
                world args

        static member scope (entity : Entity, world : World, [<ParamArray>] args : ImProperty array) =
            let world = World.setImCurrent entity.EntityAddress world
            Array.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                    else world)
                world args

        static member unscope (world : World) =
            // be nice if we could assert here that this is paired properly with scope
            World.setImCurrent Address.empty world

        static member beginGame (world : World, [<ParamArray>] args : ImProperty array) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ImCurrent.Names)
            let world = World.setImCurrent gameAddress world
            let game = Nu.Game gameAddress
            Array.fold
                (fun world arg -> game.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c')
                world args

        static member endGame (world : World) =
            match world.ImCurrent with
            | :? (Game Address) ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndGame mismatch.")

        static member internal beginScreenInternal<'d when 'd :> ScreenDispatcher> (transitionScreen, setScreenSlide, screenName, behavior, select, world : World, [<ParamArray>] args : ImProperty array) =
            let screenAddress = Address.makeFromArray (Array.add screenName world.ImCurrent.Names)
            let world = World.setImCurrent screenAddress world
            let screen = Nu.Screen screenAddress
            let world =
                let imSimulants = world.ImSimulants
                match imSimulants.TryGetValue screen with
                | (true, imScreen) ->
                    if world.Imperative then
                        imScreen.Utilized <- true
                        world
                    else
                        let imSimulants = OMap.add (screen :> Simulant) { imScreen with Utilized = true } imSimulants
                        World.setImSimulants imSimulants world
                | (false, _) ->
                    let world = World.createScreen<'d> (Some screenName) world |> snd
                    let imGroup = { Utilized = true; Subs = [||] }
                    let imSimulants = OMap.add (screen :> Simulant) imGroup imSimulants
                    World.setImSimulants imSimulants world
            let world =
                Array.fold
                    (fun world arg ->
                        if screen.GetExists world
                        then screen.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                        else world)
                    world args
            let world = if screen.GetExists world then World.applyScreenBehavior setScreenSlide behavior screen world else world
            let world = if screen.GetExists world && select then transitionScreen screen world else world
            world

        static member endScreen (world : World) =
            match world.ImCurrent with
            | :? (Screen Address) ->
                World.setImCurrent Game.GameAddress world
            | _ -> raise (new InvalidOperationException "ImEndScreen mismatch.")

        static member beginGroup<'d when 'd :> GroupDispatcher> (groupName : string, world : World, [<ParamArray>] args : ImProperty array) : World =
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
                    if group.GetExists world
                    then group.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                    else world)
                world args

        static member endGroup (world : World) =
            match world.ImCurrent with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setImCurrent currentAddress world
            | _ -> raise (new InvalidOperationException "ImEndGroup mismatch.")

        static member beginEntity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName : string, world : World, [<ParamArray>] args : ImProperty array) : 'r * World =
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
                        if entity.GetExists world
                        then entity.TrySet arg.ImPropertyName arg.ImPropertyValue world |> __c'
                        else world)
                    world args
            (inspect subs, world)

        static member endEntity (world : World) =
            match world.ImCurrent with
            | :? (Entity Address) as entityAddress when entityAddress.Length >= 4 ->
                let currentNames = Array.take (dec entityAddress.Length) entityAddress.Names
                let currentAddress =
                    if currentNames.Length = 3
                    then Address.makeFromArray<Group> currentNames :> Address
                    else Address.makeFromArray<Entity> currentNames
                World.setImCurrent currentAddress world
            | _ -> raise (new InvalidOperationException "ImEndEntity mismatch.")

        static member entity<'d, 'r when 'd :> EntityDispatcher> (init, inspect, entityName, world, [<ParamArray>] args : ImProperty array) =
            let (result, world) = World.beginEntity<'d, 'r> (init, inspect, entityName, world, args)
            let world = World.endEntity world
            (result, world)

        static member button (buttonName : string, world : World, [<ParamArray>] args : ImProperty array) =
            World.entity<ButtonDispatcher, _>
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