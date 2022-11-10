// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module Forge =

    let rec synchronizeEntity (forgeOld : EntityForge) (forge : EntityForge) (entity : Entity) world =
        
        if forgeOld <> forge then

            let propertiesAdded =
                USet.differenceFast forgeOld.PropertyForges forge.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setProperty propertyName property entity world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for (eventHandlerKey, subscriptionId) in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerKey, subscriptionId)

            let eventHandlersRemoved = List ()
            for (eventHandlerKey, subscriptionId) in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add subscriptionId

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj entity world
                            (Cascade, world))
                            eventAddress (entity :> Simulant) world
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
            for (entityName, entityForge) in forgeOld.EntityForges do
                match forge.EntityForges.TryGetValue entityName with
                | (true, _) -> entitiesPotentiallyAltered.Add (entity / entityName, entityForge) |> ignore
                | (false, _) -> entitiesAdded.Add (entity / entityName, entityForge)

            let entitiesRemoved = List ()
            for (entityName, entityForge) in forge.EntityForges do
                match forgeOld.EntityForges.TryGetValue entityName with
                | (true, _) ->
                    entitiesPotentiallyAltered.Add (entity / entityName, entityForge) |> ignore
                    entitiesPotentiallyAltered.Remove (entity / entityName) |> ignore
                | (false, _) -> entitiesRemoved.Add (entity / entityName)

            let world =
                Seq.fold
                    (fun world entity -> World.destroyEntityImmediate entity world)
                    world entitiesRemoved

            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (entity, entityForge) = (kvp.Key, kvp.Value)
                    let entityForgeOld = World.getEntityForge entity world
                    synchronizeEntity entityForgeOld entityForge entity world)
                    world entitiesPotentiallyAltered

            let world =
                Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                    let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                    synchronizeEntity EntityForge.empty entityForge entity world)
                    world entitiesAdded

            world
        else world

    let synchronizeGroup (forgeOld : GroupForge) (forge : GroupForge) (group : Group) world =
        
        if forgeOld <> forge then

            let propertiesAdded =
                USet.differenceFast forgeOld.PropertyForges forge.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setProperty propertyName property group world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for (eventHandlerKey, subscriptionId) in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerKey, subscriptionId)

            let eventHandlersRemoved = List ()
            for (eventHandlerKey, subscriptionId) in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add subscriptionId

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj group world
                            (Cascade, world))
                            eventAddress (group :> Simulant) world
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
            for (entityName, entityForge) in forgeOld.EntityForges do
                match forge.EntityForges.TryGetValue entityName with
                | (true, _) -> entitiesPotentiallyAltered.Add (group / entityName, entityForge) |> ignore
                | (false, _) -> entitiesAdded.Add (group / entityName, entityForge)

            let entitiesRemoved = List ()
            for (entityName, entityForge) in forge.EntityForges do
                match forgeOld.EntityForges.TryGetValue entityName with
                | (true, _) ->
                    entitiesPotentiallyAltered.Add (group / entityName, entityForge) |> ignore
                    entitiesPotentiallyAltered.Remove (group / entityName) |> ignore
                | (false, _) -> entitiesRemoved.Add (group / entityName)

            let world =
                Seq.fold
                    (fun world entity -> World.destroyEntityImmediate entity world)
                    world entitiesRemoved

            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (entity, entityForge) = (kvp.Key, kvp.Value)
                    let entityForgeOld = World.getEntityForge entity world
                    synchronizeEntity entityForgeOld entityForge entity world)
                    world entitiesPotentiallyAltered

            let world =
                Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                    let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                    synchronizeEntity EntityForge.empty entityForge entity world)
                    world entitiesAdded

            world
        else world

    let synchronizeScreen (forgeOld : ScreenForge) (forge : ScreenForge) (screen : Screen) world =
        
        if forgeOld <> forge then

            let propertiesAdded =
                USet.differenceFast forgeOld.PropertyForges forge.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setProperty propertyName property screen world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for (eventHandlerKey, subscriptionId) in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerKey, subscriptionId)

            let eventHandlersRemoved = List ()
            for (eventHandlerKey, subscriptionId) in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add subscriptionId

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj screen world
                            (Cascade, world))
                            eventAddress (screen :> Simulant) world
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
            for (groupName, groupForge) in forgeOld.GroupForges do
                match forge.GroupForges.TryGetValue groupName with
                | (true, _) -> groupsPotentiallyAltered.Add (screen / groupName, groupForge) |> ignore
                | (false, _) -> groupsAdded.Add (screen / groupName, groupForge)

            let groupsRemoved = List ()
            for (groupName, groupForge) in forge.GroupForges do
                match forgeOld.GroupForges.TryGetValue groupName with
                | (true, _) ->
                    groupsPotentiallyAltered.Add (screen / groupName, groupForge) |> ignore
                    groupsPotentiallyAltered.Remove (screen / groupName) |> ignore
                | (false, _) -> groupsRemoved.Add (screen / groupName)

            let world =
                Seq.fold
                    (fun world group -> World.destroyGroupImmediate group world)
                    world groupsRemoved

            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (group, groupForge) = (kvp.Key, kvp.Value)
                    let groupForgeOld = World.getGroupForge group world
                    synchronizeGroup groupForgeOld groupForge group world)
                    world groupsPotentiallyAltered

            let world =
                Seq.fold (fun world (group : Group, groupForge : GroupForge) ->
                    let (group, world) = World.createGroup4 groupForge.GroupDispatcherName (Some group.Name) screen world
                    synchronizeGroup GroupForge.empty groupForge group world)
                    world groupsAdded

            world
        else world

    let synchronizeGame (forgeOld : GameForge) (forge : GameForge) (game : Game) world =

        if forgeOld <> forge then

            let propertiesAdded =
                USet.differenceFast forgeOld.PropertyForges forge.PropertyForges

            let world =
                Seq.fold (fun world (propertyName, propertyType, propertyValue) ->
                    let property = { PropertyType = propertyType; PropertyValue = propertyValue }
                    World.setProperty propertyName property game world |> snd')
                    world
                    propertiesAdded

            let eventHandlersAdded = List ()
            for (eventHandlerKey, subscriptionId) in forgeOld.EventHandlerForges do
                match forge.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersAdded.Add (eventHandlerKey, subscriptionId)

            let eventHandlersRemoved = List ()
            for (eventHandlerKey, subscriptionId) in forge.EventHandlerForges do
                match forgeOld.EventHandlerForges.TryGetValue eventHandlerKey with
                | (true, _) -> ()
                | (false, _) -> eventHandlersRemoved.Add subscriptionId

            let world =
                Seq.fold (fun world ((eventAddress, signalObj), subscriptionId) ->
                    let (unsubscribe, world) =
                        World.subscribePlus subscriptionId (fun (_ : Event) world ->
                            let world = WorldModule.trySignal signalObj game world
                            (Cascade, world))
                            eventAddress (game :> Simulant) world
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
            for (screenName, screenForge) in forgeOld.ScreenForges do
                match forge.ScreenForges.TryGetValue screenName with
                | (true, _) -> screensPotentiallyAltered.Add (Screen screenName, screenForge) |> ignore
                | (false, _) -> screensAdded.Add (Screen screenName, screenForge)

            let screensRemoved = List ()
            for (screenName, screenForge) in forge.ScreenForges do
                match forgeOld.ScreenForges.TryGetValue screenName with
                | (true, _) ->
                    screensPotentiallyAltered.Add (Screen screenName, screenForge) |> ignore
                    screensPotentiallyAltered.Remove (Screen screenName) |> ignore
                | (false, _) -> screensRemoved.Add (Screen screenName)

            let world =
                Seq.fold
                    (fun world screen -> World.destroyScreenImmediate screen world)
                    world screensRemoved

            let world =
                Seq.fold (fun world (kvp : KeyValuePair<_, _>) ->
                    let (screen, screenForge) = (kvp.Key, kvp.Value)
                    let screenForgeOld = World.getScreenForge screen world
                    synchronizeScreen screenForgeOld screenForge screen world)
                    world screensPotentiallyAltered

            let world =
                Seq.fold (fun world (screen : Screen, screenForge : ScreenForge) ->
                    let (screen, world) = World.createScreen3 screenForge.ScreenDispatcherName (Some screen.Name) world
                    synchronizeScreen ScreenForge.empty screenForge screen world)
                    world screensAdded

            (forge.InitialScreenNameOpt |> Option.map Screen, world)
        else (forge.InitialScreenNameOpt |> Option.map Screen, world)
