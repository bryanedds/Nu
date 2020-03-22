// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleLayer =

    /// Dynamic property getters.
    let internal Getters = Dictionary<string, Layer -> World -> Property> HashIdentity.Structural

    /// Dynamic property setters.
    let internal Setters = Dictionary<string, Property -> Layer -> World -> bool * World> HashIdentity.Structural

    type World with
    
        static member private layerStateFinder (layer : Layer) world =
            UMap.tryFind layer.LayerAddress world.LayerStates

        static member private layerStateAdder layerState (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [|screenName; layerName|] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some layerDirectory ->
                        match UMap.tryFind layerName layerDirectory.Value with
                        | Some entityDirectory ->
                            let layerDirectory' = UMap.add layerName (KeyValuePair (entityDirectory.Key, entityDirectory.Value)) layerDirectory.Value
                            let entityDirectory' = KeyValuePair (layerDirectory.Key, layerDirectory')
                            UMap.add screenName entityDirectory' world.ScreenDirectory
                        | None ->
                            let entityDirectory' = (KeyValuePair (layer.LayerAddress, UMap.makeEmpty Constants.Engine.SimulantMapConfig))
                            let layerDirectory' = UMap.add layerName entityDirectory' layerDirectory.Value
                            UMap.add screenName (KeyValuePair (layerDirectory.Key, layerDirectory')) world.ScreenDirectory
                    | None -> failwith ("Cannot add layer '" + scstring layer.LayerAddress + "' to non-existent screen.")
                | _ -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateRemover (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [|screenName; layerName|] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some layerDirectory ->
                        let layerDirectory' = UMap.remove layerName layerDirectory.Value
                        UMap.add screenName (KeyValuePair (layerDirectory.Key, layerDirectory')) world.ScreenDirectory
                    | None -> failwith ("Cannot remove layer '" + scstring layer.LayerAddress + "' from non-existent screen.")
                | _ -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
            let layerStates = UMap.remove layer.LayerAddress world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateSetter layerState (layer : Layer) world =
#if DEBUG
            if not (UMap.containsKey layer.LayerAddress world.LayerStates) then
                failwith ("Cannot set the state of a non-existent layer '" + scstring layer.LayerAddress + "'")
            if not (World.qualifyEventContext (atooa layer.LayerAddress) world) then
                failwith "Cannot set the state of a layer in an unqualifed event context."
#endif
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with LayerStates = layerStates }

        static member private addLayerState layerState layer world =
            World.layerStateAdder layerState layer world

        static member private removeLayerState layer world =
            World.layerStateRemover layer world

        static member private publishLayerChange propertyName propertyValue (layer : Layer) world =
            let world =
                let layerNames = Address.getNames layer.LayerAddress
                let changeEventAddress = rtoa<ChangeData> [|"Change"; propertyName; "Event"; layerNames.[0]; layerNames.[1]|]
                let eventTrace = EventTrace.record "World" "publishLayerChange" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy { Name = propertyName; Value = propertyValue } changeEventAddress eventTrace layer false world
            world

        static member private getLayerStateOpt layer world =
            World.layerStateFinder layer world

        static member internal getLayerState layer world =
            match World.getLayerStateOpt layer world with
            | Some layerState -> layerState
            | None -> failwith ("Could not find layer with address '" + scstring layer.LayerAddress + "'.")

        static member internal getLayerXtensionProperties layer world =
            let layerState = World.getLayerState layer world
            layerState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private setLayerState layerState layer world =
            World.layerStateSetter layerState layer world

        static member private updateLayerStateWithoutEvent updater layer world =
            let layerState = World.getLayerState layer world
            let layerState = updater layerState
            World.setLayerState layerState layer world

        static member private updateLayerState updater propertyName propertyValue layer world =
            let world = World.updateLayerStateWithoutEvent updater layer world
            World.publishLayerChange propertyName propertyValue layer world

        /// Check that a layer exists in the world.
        static member internal getLayerExists layer world =
            Option.isSome (World.getLayerStateOpt layer world)

        static member internal getLayerDispatcher layer world = (World.getLayerState layer world).Dispatcher
        static member internal getLayerDepth layer world = (World.getLayerState layer world).Depth
        static member internal setLayerDepth value layer world = World.updateLayerState (fun layerState -> { layerState with Depth = value }) Property? Depth value layer world
        static member internal getLayerVisible layer world = (World.getLayerState layer world).Visible
        static member internal setLayerVisible value layer world = World.updateLayerState (fun layerState -> { layerState with Visible = value }) Property? Visible value layer world
        static member internal getLayerPersistent layer world = (World.getLayerState layer world).Persistent
        static member internal setLayerPersistent value layer world = World.updateLayerState (fun layerState -> { layerState with Persistent = value }) Property? Persistent value layer world
        static member internal getLayerCreationTimeStamp layer world = (World.getLayerState layer world).CreationTimeStamp
        static member internal getLayerScriptFrame layer world = (World.getLayerState layer world).ScriptFrame
        static member internal setLayerScriptFrame value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptFrame = value }) Property? ScriptFrame value layer world
        static member internal getLayerName layer world = (World.getLayerState layer world).Name
        static member internal getLayerId layer world = (World.getLayerState layer world).Id
        
        static member internal tryGetLayerProperty propertyName layer world =
            if World.getLayerExists layer world then
                match Getters.TryGetValue propertyName with
                | (false, _) -> LayerState.tryGetProperty propertyName (World.getLayerState layer world)
                | (true, getter) -> Some (getter layer world)
            else None

        static member internal getLayerProperty propertyName layer world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                | None -> failwithf "Could not find property '%s'." propertyName
                | Some property -> property
            | (true, getter) -> getter layer world

        static member internal trySetLayerProperty propertyName property layer world =
            if World.getLayerExists layer world then
                match Setters.TryGetValue propertyName with
                | (false, _) ->
                    let mutable success = false // bit of a hack to get additional state out of the lambda
                    let world =
                        World.updateLayerState (fun layerState ->
                            let (successInner, layerState) = LayerState.trySetProperty propertyName property layerState
                            success <- successInner
                            layerState)
                            propertyName property.PropertyValue layer world
                    (success, world)
                | (true, setter) -> setter property layer world
            else (false, world)

        static member internal setLayerProperty propertyName property layer world =
            if World.getLayerExists layer world then
                match Setters.TryGetValue propertyName with
                | (false, _) ->
                    World.updateLayerState
                        (LayerState.setProperty propertyName property)
                        propertyName property.PropertyValue layer world
                | (true, setter) ->
                    match setter property layer world with
                    | (true, world) -> world
                    | (false, _) -> failwith ("Cannot change layer property " + propertyName + ".")
            else world

        static member internal attachLayerProperty propertyName property layer world =
            if World.getLayerExists layer world
            then World.updateLayerState (LayerState.attachProperty propertyName property) propertyName property.PropertyValue layer world
            else failwith ("Cannot attach layer property '" + propertyName + "'; layer '" + layer.Name + "' is not found.")

        static member internal detachLayerProperty propertyName layer world =
            if World.getLayerExists layer world
            then World.updateLayerStateWithoutEvent (LayerState.detachProperty propertyName) layer world
            else failwith ("Cannot detach layer property '" + propertyName + "'; layer '" + layer.Name + "' is not found.")

        static member internal registerLayer layer world =
            World.withEventContext (fun world ->
                let dispatcher = World.getLayerDispatcher layer world
                let world = dispatcher.Register (layer, world)
                let eventTrace = EventTrace.record "World" "registerLayer" EventTrace.empty
                World.publish () (rtoa<unit> [|"Register"; "Event"|] --> layer) eventTrace layer world)
                layer
                world

        static member internal unregisterLayer layer world =
            World.withEventContext (fun world ->
                let dispatcher = World.getLayerDispatcher layer world
                let eventTrace = EventTrace.record "World" "unregisteringLayer" EventTrace.empty
                let world = World.publish () (rtoa<unit> [|"Unregistering"; "Event"|] --> layer) eventTrace layer world
                dispatcher.Unregister (layer, world))
                layer
                world

        static member internal addLayer mayReplace layerState layer world =
            let isNew = not (World.getLayerExists layer world)
            if isNew || mayReplace then
                let world = World.addLayerState layerState layer world
                if isNew then World.registerLayer layer world else world
            else failwith ("Adding a layer that the world already contains at address '" + scstring layer.LayerAddress + "'.")

        static member internal removeLayer3 removeEntities layer world =
            if World.getLayerExists layer world then
                let world = World.unregisterLayer layer world
                let world = removeEntities layer world
                World.removeLayerState layer world
            else world

        static member internal writeLayer4 writeEntities layer layerDescriptor world =
            let layerState = World.getLayerState layer world
            let layerDispatcherName = getTypeName layerState.Dispatcher
            let layerDescriptor = { layerDescriptor with LayerDispatcherName = layerDispatcherName }
            let getLayerProperties = Reflection.writePropertiesFromTarget tautology3 layerDescriptor.LayerProperties layerState
            let layerDescriptor = { layerDescriptor with LayerProperties = getLayerProperties }
            writeEntities layer layerDescriptor world

        static member internal readLayer5 readEntities layerDescriptor nameOpt (screen : Screen) world =

            // create the dispatcher
            let dispatcherName = layerDescriptor.LayerDispatcherName
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ("Could not find LayerDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
                    let dispatcherName = typeof<LayerDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the layer state and populate its properties
            let layerState = LayerState.make None dispatcher
            let layerState = Reflection.attachProperties LayerState.copy layerState.Dispatcher layerState world
            let layerState = Reflection.readPropertiesToTarget LayerState.copy layerDescriptor.LayerProperties layerState

            // apply the name if one is provided
            let layerState =
                match nameOpt with
                | Some name -> { layerState with Name = name }
                | None -> layerState

            // add the layer's state to the world
            let layer = Layer (screen.ScreenAddress <-- ntoa<Layer> layerState.Name)
            let world = World.addLayer true layerState layer world

            // read the layer's entities
            let world = readEntities layerDescriptor layer world |> snd
            (layer, world)

        /// View all of the properties of a layer.
        static member internal viewLayerProperties layer world =
            let state = World.getLayerState layer world
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c

    /// Initialize property getters.
    let private initGetters () =
        Getters.Add ("Dispatcher", fun layer world -> { PropertyType = typeof<LayerDispatcher>; PropertyValue = World.getLayerDispatcher layer world })
        Getters.Add ("Depth", fun layer world -> { PropertyType = typeof<single>; PropertyValue = World.getLayerDepth layer world })
        Getters.Add ("Visible", fun layer world -> { PropertyType = typeof<single>; PropertyValue = World.getLayerVisible layer world })
        Getters.Add ("Persistent", fun layer world -> { PropertyType = typeof<bool>; PropertyValue = World.getLayerPersistent layer world })
        Getters.Add ("ScriptFrame", fun layer world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getLayerScriptFrame layer world })
        Getters.Add ("CreationTimeStamp", fun layer world -> { PropertyType = typeof<int64>; PropertyValue = World.getLayerCreationTimeStamp layer world })
        Getters.Add ("Name", fun layer world -> { PropertyType = typeof<string>; PropertyValue = World.getLayerName layer world })
        Getters.Add ("Id", fun layer world -> { PropertyType = typeof<Guid>; PropertyValue = World.getLayerId layer world })
        
    /// Initialize property setters.
    let private initSetters () =
        Setters.Add ("Dispatcher", fun _ _ world -> (false, world))
        Setters.Add ("Depth", fun property layer world -> (true, World.setLayerDepth (property.PropertyValue :?> single) layer world))
        Setters.Add ("Visible", fun property layer world -> (true, World.setLayerVisible (property.PropertyValue :?> bool) layer world))
        Setters.Add ("Persistent", fun property layer world -> (true, World.setLayerPersistent (property.PropertyValue :?> bool) layer world))
        Setters.Add ("CreationTimeStamp", fun _ _ world -> (false, world))
        Setters.Add ("Name", fun _ _ world -> (false, world))
        Setters.Add ("Id", fun _ _ world -> (false, world))
        
    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()