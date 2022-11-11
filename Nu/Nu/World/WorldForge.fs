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

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setEntityProperty propertyName property entity world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for eventHandlerEntry in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)

            let eventHandlersRemoved = List ()
            for eventHandlerEntry in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add eventHandlerEntry.Value

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj origin world
                            (Cascade, world))
                            (eventAddress --> entity) origin world
                    let world =
                        World.monitor
                            (fun _ world -> (Cascade, unsubscribe world))
                            (Events.Unregistering --> entity)
                            entity
                            world
                    world)
                    world eventHandlersAdded

            let world =
                Seq.fold
                    (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                    world eventHandlersRemoved

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

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setGroupProperty propertyName property group world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for eventHandlerEntry in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)

            let eventHandlersRemoved = List ()
            for eventHandlerEntry in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add eventHandlerEntry.Value

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj origin world
                            (Cascade, world))
                            (eventAddress --> group) origin world
                    let world =
                        World.monitor
                            (fun _ world -> (Cascade, unsubscribe world))
                            (Events.Unregistering --> group)
                            group
                            world
                    world)
                    world eventHandlersAdded

            let world =
                Seq.fold
                    (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                    world eventHandlersRemoved

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

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setScreenProperty propertyName property screen world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for eventHandlerEntry in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)

            let eventHandlersRemoved = List ()
            for eventHandlerEntry in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add eventHandlerEntry.Value

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj origin world
                            (Cascade, world))
                            (eventAddress --> screen) origin world
                    let world =
                        World.monitor
                            (fun _ world -> (Cascade, unsubscribe world))
                            (Events.Unregistering --> screen)
                            screen
                            world
                    world)
                    world eventHandlersAdded

            let world =
                Seq.fold
                    (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                    world eventHandlersRemoved

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

    let internal synchronizeGame (forgeOld : GameForge) (forge : GameForge) (origin : Simulant) (game : Game) world =

        if forgeOld <> forge then

            let propertiesAdded = HashSet (forge.PropertyForges, forge.PropertyForges.Comparer)
            propertiesAdded.ExceptWith forgeOld.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setGameProperty propertyName property world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for eventHandlerEntry in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)

            let eventHandlersRemoved = List ()
            for eventHandlerEntry in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerEntry.Key with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add eventHandlerEntry.Value

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj origin world
                            (Cascade, world))
                            (eventAddress --> game) origin world
                    let world =
                        World.monitor
                            (fun _ world -> (Cascade, unsubscribe world))
                            (Events.Unregistering --> game)
                            game
                            world
                    world)
                    world eventHandlersAdded

            let world =
                Seq.fold
                    (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                    world eventHandlersRemoved

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
                    synchronizeScreen ScreenForge.empty screenForge origin screen world)
                    world screensAdded

            (forge.InitialScreenNameOpt |> Option.map Screen, world)
        else (forge.InitialScreenNameOpt |> Option.map Screen, world)

    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName properties entities =
        { EntityDispatcherName = typeof<'entityDispatcher>.Name
          EntityName = entityName
          PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          EntityForges = entities |> List.map (fun entityForge -> (entityForge.EntityName, entityForge)) |> dictPlus HashIdentity.Structural }

    let button entityName properties = entity<ButtonDispatcher> entityName properties []
    let text entityName properties = entity<TextDispatcher> entityName properties []

    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName properties entities =
        { GroupDispatcherName = typeof<'groupDispatcher>.Name
          GroupName = groupName
          PropertyForges = properties |> List.choose (function PropertyForge (name, ty, value) -> Some (name, ty, value) | _ -> None) |> hashSetPlus HashIdentity.Structural
          EventHandlerForges = properties |> List.choose (function EventHandlerForge (addr, value) -> Some ((addr, value), makeGuid ()) | _ -> None) |> dictPlus HashIdentity.Structural
          EntityForges = entities |> List.map (fun entityForge -> (entityForge.EntityName, entityForge)) |> dictPlus HashIdentity.Structural }

    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName properties groups =
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name
          ScreenName = screenName
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