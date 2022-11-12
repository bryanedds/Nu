// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module Forge =

    let private synchronizeEventSignals (forgeOld : SimulantForge) (forge : SimulantForge) (origin : Simulant) (simulant : Simulant) world =
        let eventSignalForgesOld =
            forgeOld.EventSignalForges |>
            Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> simulant.SimulantAddress, snd entry.Key) else entry.Key), entry.Value)) |>
            dictPlus HashIdentity.Structural
        let eventSignalForges =
            forge.EventSignalForges |>
            Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> simulant.SimulantAddress, snd entry.Key) else entry.Key), entry.Value)) |>
            dictPlus HashIdentity.Structural
        let eventHandlersAdded = List ()
        for eventHandlerEntry in eventSignalForges do
            match eventSignalForgesOld.TryGetValue eventHandlerEntry.Key with
            | (true, _) -> ()
            | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
        let eventHandlersRemoved = List ()
        for eventHandlerEntry in eventSignalForgesOld do
            match eventSignalForges.TryGetValue eventHandlerEntry.Key with
            | (true, _) -> ()
            | (false, _) -> eventHandlersRemoved.Add eventHandlerEntry.Value
        let world =
            Seq.fold
                (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                world eventHandlersRemoved
        let world =
            Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                let (unsubscribe, world) =
                    World.subscribePlus subscriptionId (fun (_ : Event) world ->
                        let world = WorldModule.trySignal signalObj origin world
                        (Cascade, world))
                        eventAddress origin world
                let world =
                    World.monitor
                        (fun _ world -> (Cascade, unsubscribe world))
                        (Events.Unregistering --> simulant.SimulantAddress)
                        simulant
                        world
                world)
                world eventHandlersAdded
        world

    let private synchronizeProperties (forgeOld : SimulantForge) (forge : SimulantForge) (simulant : Simulant) world =
        let propertyForgesOld =
            forgeOld.PropertyForges |>
            Seq.map (fun (simulantOpt, propertyName, propertyType, propertyValue) ->
                match simulantOpt with
                | ValueSome simulant -> (simulant, propertyName, propertyType, propertyValue)
                | ValueNone -> (simulant, propertyName, propertyType, propertyValue)) |>
            hashSetPlus HashIdentity.Structural
        let propertyForges =
            forge.PropertyForges |>
            Seq.map (fun (simulantAddressOpt, propertyName, propertyType, propertyValue) ->
                match simulantAddressOpt with
                | ValueSome simulant -> (simulant, propertyName, propertyType, propertyValue)
                | ValueNone -> (simulant, propertyName, propertyType, propertyValue)) |>
            hashSetPlus HashIdentity.Structural
        let propertiesAdded = HashSet (propertyForges, propertyForges.Comparer)
        propertiesAdded.ExceptWith propertyForgesOld
        let world =
            Seq.fold (fun world (simulant, propertyName, propertyType, propertyValue) ->
                let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                World.setProperty propertyName property simulant world |> snd')
                world
                propertiesAdded
        world

    let private differentiateChildren<'child, 'childForge when 'child : equality and 'child :> Simulant and 'childForge :> SimulantForge>
        (forgeOld : SimulantForge) (forge : SimulantForge) (simulant : Simulant) =
        let childrenPotentiallyAltered = Dictionary ()
        let childrenAdded = List ()
        let childForgesOld = forgeOld.GetChildForges<'childForge> ()
        let childForges = forge.GetChildForges<'childForge> ()
        for childEntry in childForges do
            let childSimulant = World.derive (Address.makeFromArray (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
            match childForgesOld.TryGetValue childEntry.Key with
            | (true, _) -> childrenPotentiallyAltered.Add (childSimulant, childEntry.Value)
            | (false, _) -> childrenAdded.Add (childSimulant, childEntry.Value)
        let childrenRemoved = List ()
        for childEntry in childForgesOld do
            match childForges.TryGetValue childEntry.Key with
            | (true, _) -> ()
            | (false, _) ->
                let childSimulant = World.derive (Address.makeFromArray (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
                childrenRemoved.Add childSimulant
                childrenPotentiallyAltered.Remove childSimulant |> ignore
        (childrenAdded, childrenRemoved, childrenPotentiallyAltered)

    let rec synchronizeEntity (forgeOld : EntityForge) (forge : EntityForge) (origin : Simulant) (entity : Entity) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin entity world
            let world = synchronizeProperties forgeOld forge entity world
            let (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) = differentiateChildren<Entity, EntityForge> forgeOld forge entity
            let world = Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                    let (entity, entityForge) = (kvp.Key, kvp.Value)
                    let entityForgeOld = forgeOld.EntityForges.[entity.Name]
                    synchronizeEntity entityForgeOld entityForge origin entity world)
                    world entitiesPotentiallyAltered
            let world =
                Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                    let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                    synchronizeEntity EntityForge.empty entityForge origin entity world)
                    world entitiesAdded
            world
        else world

    let synchronizeGroup (forgeOld : GroupForge) (forge : GroupForge) (origin : Simulant) (group : Group) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin group world
            let world = synchronizeProperties forgeOld forge group world
            let (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) = differentiateChildren<Entity, EntityForge> forgeOld forge group
            let world = Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                    let (entity, entityForge) = (kvp.Key, kvp.Value)
                    let entityForgeOld = forgeOld.EntityForges.[entity.Name]
                    synchronizeEntity entityForgeOld entityForge origin entity world)
                    world entitiesPotentiallyAltered
            let world =
                Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                    let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                    synchronizeEntity EntityForge.empty entityForge origin entity world)
                    world entitiesAdded
            world
        else world

    let synchronizeScreen (forgeOld : ScreenForge) (forge : ScreenForge) (origin : Simulant) (screen : Screen) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin screen world
            let world = synchronizeProperties forgeOld forge screen world
            let (groupsAdded, groupsRemoved, groupsPotentiallyAltered) = differentiateChildren<Group, GroupForge> forgeOld forge screen
            let world = Seq.fold (fun world group -> World.destroyGroup group world) world groupsRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<Group, _>) ->
                    let (group, groupForge) = (kvp.Key, kvp.Value)
                    let groupForgeOld = forgeOld.GroupForges.[group.Name]
                    synchronizeGroup groupForgeOld groupForge origin group world)
                    world groupsPotentiallyAltered
            let world =
                Seq.fold (fun world (group : Group, groupForge : GroupForge) ->
                    let (group, world) = World.createGroup4 groupForge.GroupDispatcherName (Some group.Name) group.Screen world
                    synchronizeGroup GroupForge.empty groupForge origin group world)
                    world groupsAdded
            world
        else world

    let synchronizeGame setScreenSplash (forgeOld : GameForge) (forge : GameForge) (origin : Simulant) world =
        if forgeOld <> forge then
            let game = Simulants.Game
            let world = synchronizeEventSignals forgeOld forge origin game world
            let world = synchronizeProperties forgeOld forge game world
            let (screensAdded, screensRemoved, screensPotentiallyAltered) = differentiateChildren<Screen, ScreenForge> forgeOld forge game
            let world = Seq.fold (fun world screen -> World.destroyScreen screen world) world screensRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<Screen, _>) ->
                    let (screen, screenForge) = (kvp.Key, kvp.Value)
                    let screenForgeOld = forgeOld.ScreenForges.[screen.Name]
                    synchronizeScreen screenForgeOld screenForge origin screen world)
                    world screensPotentiallyAltered
            let world =
                Seq.fold (fun world (screen : Screen, screenForge : ScreenForge) ->
                    let (screen, world) = World.createScreen3 screenForge.ScreenDispatcherName (Some screen.Name) world
                    let world = World.applyScreenBehavior setScreenSplash screenForge.ScreenBehavior screen world
                    synchronizeScreen ScreenForge.empty screenForge origin screen world)
                    world screensAdded
            (forge.InitialScreenNameOpt |> Option.map Screen, world)
        else (forge.InitialScreenNameOpt |> Option.map Screen, world)

    let composite<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName properties entities =
        { EntityDispatcherName = typeof<'entityDispatcher>.Name
          EntityName = entityName
          PropertyForges = properties |> List.choose (function PropertyForge (simOpt, name, ty, value) -> Some (simOpt, name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventSignalForges = properties |> List.choose (function EventSignalForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          EventHandlerForges = dictPlus HashIdentity.Structural [] // TODO: populate.
          EntityForges = entities |> List.map (fun entityForge -> (entityForge.EntityName, entityForge)) |> dictPlus HashIdentity.Structural }

    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName properties =
        composite<'entityDispatcher> entityName properties []

    /// Describe a 2d basic emitter with the given initializers.
    let basicEmitter2d entityName initializers = entity<BasicEmitterDispatcher2d> entityName initializers

    /// Describe a 2d effect with the given initializers.
    let effect2d entityName initializers = entity<EffectDispatcher2d> entityName initializers

    /// Describe a static sprite with the given initializers.
    let staticSprite entityName initializers = entity<StaticSpriteDispatcher> entityName initializers

    /// Describe an animated sprite with the given initializers.
    let animatedSprite entityName initializers = entity<AnimatedSpriteDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

    /// Describe a text with the given initializers.
    let text entityName initializers = entity<TextDispatcher> entityName initializers

    /// Describe a toggle button with the given initializers.
    let toggleButton entityName initializers = entity<ToggleButtonDispatcher> entityName initializers

    /// Describe a radio button with the given initializers.
    let radioButton entityName initializers = entity<RadioButtonDispatcher> entityName initializers

    /// Describe an fps gui with the given initializers.
    let fps entityName initializers = entity<FpsDispatcher> entityName initializers

    /// Describe a feeler with the given initializers.
    let feeler entityName initializers = entity<FeelerDispatcher> entityName initializers

    /// Describe a fill bar with the given initializers.
    let fillBar entityName initializers = entity<FillBarDispatcher> entityName initializers

    /// Describe a 2d block with the given initializers.
    let block2d entityName initializers = entity<BlockDispatcher2d> entityName initializers

    /// Describe a 2d box with the given initializers.
    let box2d entityName initializers = entity<BoxDispatcher2d> entityName initializers

    /// Describe a side-view character with the given initializers.
    let sideViewCharacter entityName initializers = entity<SideViewCharacterDispatcher> entityName initializers

    /// Describe a tile map with the given initializers.
    let tileMap entityName initializers = entity<TileMapDispatcher> entityName initializers

    /// Describe a tmx map with the given initializers.
    let tmxMap entityName initializers = entity<TmxMapDispatcher> entityName initializers

    /// Describe a 3d light with the given initializers.
    let light3d entityName initializers = entity<LightDispatcher3d> entityName initializers

    /// Describe a sky box with the given initializers.
    let skyBox entityName initializers = entity<SkyBoxDispatcher> entityName initializers

    /// Describe a static billboard with the given initializers.
    let staticBillboard entityName initializers = entity<StaticBillboardDispatcher> entityName initializers

    /// Describe a static model with the given initializers.
    let staticModel entityName initializers = entity<StaticModelDispatcher> entityName initializers

    /// Describe a static model surface with the given initializers.
    let staticModelSurface entityName initializers = entity<StaticModelSurfaceDispatcher> entityName initializers

    /// Describe a static model expanded into an entity hierarchy with the given initializers.
    let staticModelHierarchy entityName initializers = entity<StaticModelHierarchyDispatcher> entityName initializers

    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName properties entities =
        { GroupDispatcherName = typeof<'groupDispatcher>.Name
          GroupName = groupName
          PropertyForges = properties |> List.choose (function PropertyForge (simOpt, name, ty, value) -> Some (simOpt, name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventSignalForges = properties |> List.choose (function EventSignalForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          EntityForges = entities |> List.map (fun entityForge -> (entityForge.EntityName, entityForge)) |> dictPlus HashIdentity.Structural }

    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior properties groups =
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name
          ScreenName = screenName
          ScreenBehavior = screenBehavior
          PropertyForges = properties |> List.choose (function PropertyForge (simOpt, name, ty, value) -> Some (simOpt, name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventSignalForges = properties |> List.choose (function EventSignalForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          GroupForges = groups |> List.map (fun groupForge -> (groupForge.GroupName, groupForge)) |> dictPlus HashIdentity.Structural }

    let game properties screens =
        { PropertyForges = properties |> List.choose (function PropertyForge (simOpt, name, ty, value) -> Some (simOpt, name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventSignalForges = properties |> List.choose (function EventSignalForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          ScreenForges = screens |> List.map (fun screenForge -> (screenForge.ScreenName, screenForge)) |> dictPlus HashIdentity.Structural
          InitialScreenNameOpt = match screens with [] -> None | screen :: _ -> Some screen.ScreenName }

module ForgeOperators =

    /// Initialize a forge property.
    let inline (==) (lens : Lens<'a, World>) (value : 'a) : PropertyForge =
        let simulantOpt = match lens.This :> obj with null -> ValueNone | _ -> ValueSome lens.This
        PropertyForge (simulantOpt, lens.Name, lens.Type, value)

    /// Bind an event to a signal.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : PropertyForge =
        EventSignalForge (Address.generalize eventAddress, signal)

    /// Bind an event to a handler.
    let inline (==|>) (eventAddress : 'a Address) (callback : Event<'a, 's> -> Signal<'message, 'command>) : PropertyForge =
        EventHandlerForge (PartialEquatable.make (Address.generalize eventAddress) (fun (evt : Event) -> callback (Event.specialize evt) :> obj))