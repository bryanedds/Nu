// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module Forge =

    let rec internal synchronizeEntity (forgeOld : EntityForge) (forge : EntityForge) (origin : Simulant) (entity : Entity) world =
        
        if forgeOld <> forge then

            let eventHandlerForgesOld =
                forgeOld.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> entity, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlerForges =
                forge.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> entity, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlersAdded = List ()
            for eventHandlerEntry in eventHandlerForges do
                match eventHandlerForgesOld.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
            let eventHandlersRemoved = List ()
            for eventHandlerEntry in eventHandlerForgesOld do
                match eventHandlerForges.TryGetValue eventHandlerEntry.Key with
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
                            (Events.Unregistering --> entity)
                            entity
                            world
                    world)
                    world eventHandlersAdded

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges
            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setEntityProperty propertyName property entity world |> snd')
                    world
                    propertiesAdded

            let entitiesPotentiallyAltered = Dictionary ()
            let entitiesAdded = List ()
            for entityEntry in forge.EntityForges do
                match forgeOld.EntityForges.TryGetValue entityEntry.Key with
                | (true, _) -> entitiesPotentiallyAltered.Add (entity / entityEntry.Key, entityEntry.Value)
                | (false, _) -> entitiesAdded.Add (entity / entityEntry.Key, entityEntry.Value)
            let entitiesRemoved = List ()
            for entityEntry in forgeOld.EntityForges do
                match forge.EntityForges.TryGetValue entityEntry.Key with
                | (true, _) -> ()
                | (false, _) ->
                    entitiesRemoved.Add (entity / entityEntry.Key)
                    entitiesPotentiallyAltered.Remove (entity / entityEntry.Key) |> ignore
            let world =
                Seq.fold
                    (fun world entity -> World.destroyEntity entity world)
                    world entitiesRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (entity : Entity, entityForge) = (kvp.Key, kvp.Value)
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

    let internal synchronizeGroup (forgeOld : GroupForge) (forge : GroupForge) (origin : Simulant) (group : Group) world =
        
        if forgeOld <> forge then

            let eventHandlerForgesOld =
                forgeOld.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> group, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlerForges =
                forge.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> group, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlersAdded = List ()
            for eventHandlerEntry in eventHandlerForges do
                match eventHandlerForgesOld.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
            let eventHandlersRemoved = List ()
            for eventHandlerEntry in eventHandlerForgesOld do
                match eventHandlerForges.TryGetValue eventHandlerEntry.Key with
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
                            (Events.Unregistering --> group)
                            group
                            world
                    world)
                    world eventHandlersAdded

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges
            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setGroupProperty propertyName property group world |> snd')
                    world
                    propertiesAdded

            let entitiesPotentiallyAltered = Dictionary ()
            let entitiesAdded = List ()
            for entityEntry in forge.EntityForges do
                match forgeOld.EntityForges.TryGetValue entityEntry.Key with
                | (true, _) -> entitiesPotentiallyAltered.Add (group / entityEntry.Key, entityEntry.Value)
                | (false, _) -> entitiesAdded.Add (group / entityEntry.Key, entityEntry.Value)
            let entitiesRemoved = List ()
            for entityEntry in forgeOld.EntityForges do
                match forge.EntityForges.TryGetValue entityEntry.Key with
                | (true, _) -> ()
                | (false, _) ->
                    entitiesRemoved.Add (group / entityEntry.Key)
                    entitiesPotentiallyAltered.Remove (group / entityEntry.Key) |> ignore
            let world =
                Seq.fold
                    (fun world entity -> World.destroyEntity entity world)
                    world entitiesRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (entity : Entity, entityForge) = (kvp.Key, kvp.Value)
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

    let internal synchronizeScreen (forgeOld : ScreenForge) (forge : ScreenForge) (origin : Simulant) (screen : Screen) world =
        
        if forgeOld <> forge then

            let eventHandlerForgesOld =
                forgeOld.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> screen, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlerForges =
                forge.EventHandlerForges |>
                Seq.map (fun entry -> ((if Address.last (fst entry.Key) = "Event" then (fst entry.Key --> screen, snd entry.Key) else entry.Key), entry.Value)) |>
                dictPlus HashIdentity.Structural
            let eventHandlersAdded = List ()
            for eventHandlerEntry in eventHandlerForges do
                match eventHandlerForgesOld.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
            let eventHandlersRemoved = List ()
            for eventHandlerEntry in eventHandlerForgesOld do
                match eventHandlerForges.TryGetValue eventHandlerEntry.Key with
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
                            (Events.Unregistering --> screen)
                            screen
                            world
                    world)
                    world eventHandlersAdded

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges
            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setScreenProperty propertyName property screen world |> snd')
                    world
                    propertiesAdded

            let groupsPotentiallyAltered = Dictionary ()
            let groupsAdded = List ()
            for groupEntry in forge.GroupForges do
                match forgeOld.GroupForges.TryGetValue groupEntry.Key with
                | (true, _) -> groupsPotentiallyAltered.Add (screen / groupEntry.Key, groupEntry.Value)
                | (false, _) -> groupsAdded.Add (screen / groupEntry.Key, groupEntry.Value)
            let groupsRemoved = List ()
            for groupEntry in forgeOld.GroupForges do
                match forge.GroupForges.TryGetValue groupEntry.Key with
                | (true, _) -> ()
                | (false, _) ->
                    groupsRemoved.Add (screen / groupEntry.Key)
                    groupsPotentiallyAltered.Remove (screen / groupEntry.Key) |> ignore
            let world =
                Seq.fold
                    (fun world group -> World.destroyGroup group world)
                    world groupsRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (group : Group, groupForge) = (kvp.Key, kvp.Value)
                    let groupForgeOld = forgeOld.GroupForges.[group.Name]
                    synchronizeGroup groupForgeOld groupForge origin group world)
                    world groupsPotentiallyAltered
            let world =
                Seq.fold (fun world (group : Group, groupForge : GroupForge) ->
                    let (group, world) = World.createGroup4 groupForge.GroupDispatcherName (Some group.Name) screen world
                    synchronizeGroup GroupForge.empty groupForge origin group world)
                    world groupsAdded
            world
        else world

    let internal synchronizeGame setScreenSplash (forgeOld : GameForge) (forge : GameForge) (origin : Simulant) (game : Game) world =

        if forgeOld <> forge then

            let eventHandlerForgesOld = forgeOld.EventHandlerForges
            let eventHandlerForges = forge.EventHandlerForges
            let eventHandlersAdded = List ()
            for eventHandlerEntry in eventHandlerForges do
                match eventHandlerForgesOld.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
            let eventHandlersRemoved = List ()
            for eventHandlerEntry in eventHandlerForgesOld do
                match eventHandlerForges.TryGetValue eventHandlerEntry.Key with
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
                            (Events.Unregistering --> game)
                            game
                            world
                    world)
                    world eventHandlersAdded

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges
            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setGameProperty propertyName property world |> snd')
                    world
                    propertiesAdded

            let screensPotentiallyAltered = Dictionary ()
            let screensAdded = List ()
            for screenEntry in forge.ScreenForges do
                match forgeOld.ScreenForges.TryGetValue screenEntry.Key with
                | (true, _) -> screensPotentiallyAltered.Add (Screen screenEntry.Key, screenEntry.Value)
                | (false, _) -> screensAdded.Add (Screen screenEntry.Key, screenEntry.Value)
            let screensRemoved = List ()
            for screenEntry in forgeOld.ScreenForges do
                match forge.ScreenForges.TryGetValue screenEntry.Key with
                | (true, _) -> ()
                | (false, _) ->
                    screensRemoved.Add (Screen screenEntry.Key)
                    screensPotentiallyAltered.Remove (Screen screenEntry.Key) |> ignore
            let world =
                Seq.fold
                    (fun world screen -> World.destroyScreen screen world)
                    world screensRemoved
            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (screen : Screen, screenForge) = (kvp.Key, kvp.Value)
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
          PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
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
          PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          EntityForges = entities |> List.map (fun entityForge -> (entityForge.EntityName, entityForge)) |> dictPlus HashIdentity.Structural }

    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior properties groups =
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name
          ScreenName = screenName
          ScreenBehavior = screenBehavior
          PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          GroupForges = groups |> List.map (fun groupForge -> (groupForge.GroupName, groupForge)) |> dictPlus HashIdentity.Structural }

    let game properties screens =
        { PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          ScreenForges = screens |> List.map (fun screenForge -> (screenForge.ScreenName, screenForge)) |> dictPlus HashIdentity.Structural
          InitialScreenNameOpt = match screens with [] -> None | screen :: _ -> Some screen.ScreenName }

module ForgeOperators =

    /// Initialize a forge property.
    let inline (==) (lens : Lens<'a, World>) (value : 'a) : PropertyForge =
        PropertyForge (lens.Name, lens.Type, value)

    /// Bind an event to a signal.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : PropertyForge =
        EventHandlerForge (Address.generalize eventAddress, signal)