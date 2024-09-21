// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldIm =

    /// Define an immediate-mode property definition.
    let
#if !DEBUG
        inline
#endif
        (.=) (lens : Lens<'a, 's>) (value : 'a) =
        { ImPropertyLens = lens; ImPropertyValue = value } : 's ImProperty

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

        static member scopeGame (game : Game) world (args : Game ImProperty seq) =
            let world = World.setImCurrent game.GameAddress world
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c')
                world args

        static member scopeScreen (screen : Screen) world (args : Screen ImProperty seq) =
            let world = World.setImCurrent screen.ScreenAddress world
            Seq.fold
                (fun world arg ->
                    if screen.GetExists world
                    then screen.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                    else world)
                world args

        static member scopeGroup (group : Group) world (args : Group ImProperty seq) =
            let world = World.setImCurrent group.GroupAddress world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                    else world)
                world args

        static member scopeEntity (entity : Entity) world (args : Entity ImProperty seq) =
            let world = World.setImCurrent entity.EntityAddress world
            Seq.fold
                (fun world arg ->
                    if entity.GetExists world
                    then entity.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                    else world)
                world args

        static member scopeWorld world =
            World.setImCurrent Address.empty world

        static member beginGame (world : World) (args : Game ImProperty seq) =
            let gameAddress = Address.makeFromArray (Array.add Constants.Engine.GameName world.ImCurrent.Names)
            let world = World.setImCurrent gameAddress world
            let game = Nu.Game gameAddress
            Seq.fold
                (fun world arg -> game.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c')
                world args

        static member endGame (world : World) =
            match world.ImCurrent with
            | :? (Game Address) ->
                World.setImCurrent Address.empty world
            | _ -> raise (new InvalidOperationException "ImEndGame mismatch.")

        static member doGame world args =
            let world = World.beginGame world args
            World.endGame world

        static member internal beginScreenInternal<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name behavior select (world : World) (args : Screen ImProperty seq) =
            let screenAddress = Address.makeFromArray (Array.add name world.ImCurrent.Names)
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
                    let world = World.createScreen<'d> (Some name) world |> snd
                    let imGroup = { Utilized = true; Results = [||] }
                    let imSimulants = OMap.add (screen :> Simulant) imGroup imSimulants
                    World.setImSimulants imSimulants world
            let world =
                Seq.fold
                    (fun world arg ->
                        if screen.GetExists world
                        then screen.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                        else world)
                    world args
            let world = if screen.GetExists world then World.applyScreenBehavior setScreenSlide behavior screen world else world
            if screen.GetExists world && select then transitionScreen screen world else world

        static member endScreen (world : World) =
            match world.ImCurrent with
            | :? (Screen Address) ->
                World.setImCurrent Game.GameAddress world
            | _ -> raise (new InvalidOperationException "ImEndScreen mismatch.")

        static member doScreenInternal<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name behavior select world args =
            let world = World.beginScreenInternal<'d> transitionScreen setScreenSlide name behavior select world args
            World.endScreen world

        static member beginGroup<'d when 'd :> GroupDispatcher> name (world : World) (args : Group ImProperty seq) =
            let groupAddress = Address.makeFromArray (Array.add name world.ImCurrent.Names)
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
                    let world = World.createGroup<'d> (Some name) group.Screen world |> snd
                    let imGroup = { Utilized = true; Results = [||] }
                    let imSimulants = OMap.add (group :> Simulant) imGroup imSimulants
                    World.setImSimulants imSimulants world
            Seq.fold
                (fun world arg ->
                    if group.GetExists world
                    then group.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                    else world)
                world args
            
        static member endGroup (world : World) =
            match world.ImCurrent with
            | :? (Group Address) as groupAddress ->
                let currentAddress = Address.take<Group, Screen> 2 groupAddress
                World.setImCurrent currentAddress world
            | _ -> raise (new InvalidOperationException "ImEndGroup mismatch.")

        static member doGroup<'d when 'd :> GroupDispatcher> name world args =
            let world = World.beginGroup<'d> name world args
            World.endGroup world

        static member beginEntityPlus<'d, 'r when 'd :> EntityDispatcher> init inspect name (world : World) (args : Entity ImProperty seq) : 'r * World =
            let entityAddress = Address.makeFromArray (Array.add name world.ImCurrent.Names)
            let world = World.setImCurrent entityAddress world
            let entity = Nu.Entity entityAddress
            let (results, world) =
                let imSimulants = world.ImSimulants
                match imSimulants.TryGetValue entity with
                | (true, imEntity) ->
                    if world.Imperative then
                        imEntity.Utilized <- true
                        (imEntity.Results, world)
                    else
                        let imSimulants = OMap.add (entity :> Simulant) { imEntity with Utilized = true } imSimulants
                        let world = World.setImSimulants imSimulants world
                        (imEntity.Results, world)
                | (false, _) ->
                    let world = World.createEntity<'d> OverlayNameDescriptor.DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                    let world = if entity.Surnames.Length > 1 then entity.SetMountOpt (Some (Relation.makeParent ())) world else world
                    let (results, world) = init entity world
                    let imEntity = { Utilized = true; Results = results }
                    let imSimulants = OMap.add (entity :> Simulant) imEntity imSimulants
                    let world = World.setImSimulants imSimulants world
                    (results, world)
            let world =
                Seq.fold
                    (fun world arg ->
                        if entity.GetExists world
                        then entity.TrySetProperty arg.ImPropertyLens.Name { PropertyType = arg.ImPropertyLens.Type; PropertyValue = arg.ImPropertyValue } world |> __c'
                        else world)
                    world args
            (inspect results, world)

        static member beginEntity<'d when 'd :> EntityDispatcher> name world args =
            World.beginEntityPlus<'d, unit> (fun _ _ -> ([||], world)) (fun _ -> ()) name world args |> snd

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

        static member doEntityPlus<'d, 'r when 'd :> EntityDispatcher> init inspect name world args =
            let (result, world) = World.beginEntityPlus<'d, 'r> init inspect name world args
            let world = World.endEntity world
            (result, world)

        static member doEntity<'d when 'd :> EntityDispatcher> name world args =
            let world = World.beginEntity<'d> name world args
            World.endEntity world

        /// Declare a text entity with the given arguments.
        static member doText name world args = World.doEntity<TextDispatcher> name world args

        /// Declare a label with the given arguments.
        static member doLabel name world args = World.doEntity<LabelDispatcher> name world args

        /// Declare a button with the given arguments.
        static member doButton name world args =
            World.doEntityPlus<ButtonDispatcher, bool>
                (fun button world ->
                    let result = ref false
                    let world = World.monitor (fun _ world -> result.Value <- true; (Cascade, world)) button.ClickEvent button world
                    ([|result :> obj|], world))
                (fun results ->
                    let resultRef = results.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result)
                name world args

        /// Declare a toggle button with the given arguments.
        static member doToggleButton name world args =
            World.doEntityPlus<ToggleButtonDispatcher, bool>
                (fun button world ->
                    let result = ref false
                    let world = World.monitor (fun _ world -> result.Value <- true; (Cascade, world)) button.ClickEvent button world
                    ([|result :> obj|], world))
                (fun results ->
                    let resultRef = results.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result)
                name world args

        /// Declare a radio button with the given arguments.
        static member doRadioButton name world args =
            World.doEntityPlus<ToggleButtonDispatcher, bool>
                (fun button world ->
                    let result = ref false
                    let world = World.monitor (fun _ world -> result.Value <- true; (Cascade, world)) button.ClickEvent button world
                    ([|result :> obj|], world))
                (fun results ->
                    let resultRef = results.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result)
                name world args

        /// Declare a fill bar with the given arguments.
        static member doFillBar name world args = World.doEntity<FillBarDispatcher> name world args

        /// Declare a feeler with the given arguments.
        static member doFeelerButton name world args =
            World.doEntityPlus<FeelerDispatcher, bool>
                (fun feeler world ->
                    let result = ref false
                    let world = World.monitor (fun _ world -> result.Value <- true; (Cascade, world)) feeler.TouchEvent feeler world
                    ([|result :> obj|], world))
                (fun results ->
                    let resultRef = results.[0] :?> bool ref
                    let result = resultRef.Value
                    resultRef.Value <- false
                    result)
                name world args

        /// Declare an fps entity with the given arguments.
        static member doFps name world args = World.doEntity<FpsDispatcher> name world args

        /// Declare the beginning of a panel with the given arguments.
        static member beginPanel name world args = World.beginEntity<PanelDispatcher> name world args

        /// Declare the end of a panel.
        static member endPanel world = World.endEntity world

        /// Declare a panel with the given arguments.
        static member doPanel name world args = World.doEntity<PanelDispatcher> name world args