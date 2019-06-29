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
    
        static member private layerStateKeyEquality
            (layerStateKey : KeyValuePair<Layer Address, UMap<Layer Address, LayerState>>)
            (layerStateKey2 : KeyValuePair<Layer Address, UMap<Layer Address, LayerState>>) =
            refEq layerStateKey.Key layerStateKey2.Key &&
            refEq layerStateKey.Value layerStateKey2.Value

        static member private layerGetFreshKeyAndValue (layer : Layer) world =
            let layerStateOpt = UMap.tryFindFast layer.LayerAddress world.LayerStates
            KeyValuePair (KeyValuePair (layer.LayerAddress, world.LayerStates), layerStateOpt)

        static member private layerStateFinder (layer : Layer) world =
            KeyedCache.getValue
                World.layerStateKeyEquality
                (fun () -> World.layerGetFreshKeyAndValue layer world)
                (KeyValuePair (layer.LayerAddress, world.LayerStates))
                (World.getLayerCachedOpt world)

        static member private layerStateAdder layerState (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    let layerDirectoryOpt = UMap.tryFindFast screenName world.ScreenDirectory
                    if FOption.isSome layerDirectoryOpt then
                        let layerDirectory = FOption.get layerDirectoryOpt
                        let entityDirectoryOpt = UMap.tryFindFast layerName layerDirectory.Value
                        if FOption.isSome entityDirectoryOpt then
                            let entityDirectory = FOption.get entityDirectoryOpt
                            let layerDirectory' = UMap.add layerName (KeyValuePair (entityDirectory.Key, entityDirectory.Value)) layerDirectory.Value
                            let entityDirectory' = KeyValuePair (layerDirectory.Key, layerDirectory')
                            UMap.add screenName entityDirectory' world.ScreenDirectory
                        else
                            let entityDirectory' = (KeyValuePair (layer.LayerAddress, UMap.makeEmpty Constants.Engine.SimulantMapConfig))
                            let layerDirectory' = UMap.add layerName entityDirectory' layerDirectory.Value
                            UMap.add screenName (KeyValuePair (layerDirectory.Key, layerDirectory')) world.ScreenDirectory
                    else failwith ("Cannot add layer '" + scstring layer.LayerAddress + "' to non-existent screen.")
                | _ -> failwith ("Invalid layer address '" + scstring layer.LayerAddress + "'.")
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateRemover (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    let layerDirectoryOpt = UMap.tryFindFast screenName world.ScreenDirectory
                    if FOption.isSome layerDirectoryOpt then
                        let layerDirectory = FOption.get layerDirectoryOpt
                        let layerDirectory' = UMap.remove layerName layerDirectory.Value
                        UMap.add screenName (KeyValuePair (layerDirectory.Key, layerDirectory')) world.ScreenDirectory
                    else failwith ("Cannot remove layer '" + scstring layer.LayerAddress + "' from non-existent screen.")
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

        static member private publishLayerChange (propertyName : string) (layer : Layer) oldWorld world =
            let world =
                let changeEventAddress = ltoa ["Change"; propertyName; "Event"] --> layer.LayerAddress
                let eventTrace = EventTrace.record "World" "publishLayerChange" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy { PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace layer false world
            world

        static member private getLayerStateOpt layer world =
            World.layerStateFinder layer world

        static member internal getLayerState layer world =
            let layerStateOpt = World.getLayerStateOpt layer world
            if FOption.isSome layerStateOpt then FOption.get layerStateOpt
            else failwith ("Could not find layer with address '" + scstring layer.LayerAddress + "'.")

        static member internal getLayerXtensionProperties layer world =
            let layerState = World.getLayerState layer world
            layerState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private setLayerState layerState layer world =
            World.layerStateSetter layerState layer world

        static member private updateLayerStateWithoutEvent updater layer world =
            let layerState = World.getLayerState layer world
            let layerState = updater layerState
            World.setLayerState layerState layer world

        static member private updateLayerState updater propertyName layer world =
            let oldWorld = world
            let world = World.updateLayerStateWithoutEvent updater layer world
            World.publishLayerChange propertyName layer oldWorld world

        /// Check that a layer exists in the world.
        static member internal getLayerExists layer world =
            FOption.isSome (World.getLayerStateOpt layer world)

        static member internal getLayerId layer world = (World.getLayerState layer world).Id
        static member internal getLayerName layer world = (World.getLayerState layer world).Name
        static member internal getLayerDispatcher layer world = (World.getLayerState layer world).Dispatcher
        static member internal getLayerPersistent layer world = (World.getLayerState layer world).Persistent
        static member internal setLayerPersistent value layer world = World.updateLayerState (fun layerState -> { layerState with Persistent = value }) Property? Persistent layer world
        static member internal getLayerCreationTimeStamp layer world = (World.getLayerState layer world).CreationTimeStamp
        static member internal getLayerImperative layer world = Xtension.getImperative (World.getLayerState layer world).Xtension
        static member internal getLayerScriptOpt layer world = (World.getLayerState layer world).ScriptOpt
        static member internal setLayerScriptOpt value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptOpt = value }) Property? ScriptOpt layer world
        static member internal getLayerScript layer world = (World.getLayerState layer world).Script
        static member internal setLayerScript value layer world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateLayerState (fun layerState -> { layerState with Script = value }) Property? Script layer world
            let world = World.setLayerScriptFrame scriptFrame layer world
            evalManyWithLogging value scriptFrame layer world |> snd'
        static member internal getLayerScriptFrame layer world = (World.getLayerState layer world).ScriptFrame
        static member internal setLayerScriptFrame value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptFrame = value }) Property? ScriptFrame layer world
        static member internal getLayerScriptUnsubscriptions layer world = (World.getLayerState layer world).ScriptUnsubscriptions
        static member internal setLayerScriptUnsubscriptions value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptUnsubscriptions = value }) Property? ScriptUnsubscriptions layer world
        static member internal getLayerOnRegister layer world = (World.getLayerState layer world).OnRegister
        static member internal setLayerOnRegister value layer world = World.updateLayerState (fun layerState -> { layerState with OnRegister = value }) Property? OnRegister layer world
        static member internal getLayerOnUnregister layer world = (World.getLayerState layer world).OnUnregister
        static member internal setLayerOnUnregister value layer world = World.updateLayerState (fun layerState -> { layerState with OnUnregister = value }) Property? OnUnregister layer world
        static member internal getLayerOnUpdate layer world = (World.getLayerState layer world).OnUpdate
        static member internal setLayerOnUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnUpdate = value }) Property? OnUpdate layer world
        static member internal getLayerOnPostUpdate layer world = (World.getLayerState layer world).OnPostUpdate
        static member internal setLayerOnPostUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnPostUpdate = value }) Property? OnPostUpdate layer world
        static member internal getLayerOnSignal layer world = (World.getLayerState layer world).OnSignal
        static member internal setLayerOnSignal value layer world = World.updateLayerState (fun layerState -> { layerState with OnSignal = value }) Property? OnSignal layer world
        static member internal getLayerDepth layer world = (World.getLayerState layer world).Depth
        static member internal setLayerDepth value layer world = World.updateLayerState (fun layerState -> { layerState with Depth = value }) Property? Depth layer world
        static member internal getLayerVisible layer world = (World.getLayerState layer world).Visible
        static member internal setLayerVisible value layer world = World.updateLayerState (fun layerState -> { layerState with Visible = value }) Property? Visible layer world

        static member internal tryGetLayerCalculatedProperty propertyName layer world =
            let dispatcher = World.getLayerDispatcher layer world
            dispatcher.TryGetCalculatedProperty (propertyName, layer, world)

        static member internal tryGetLayerProperty propertyName layer world =
            if World.getLayerExists layer world then
                match Getters.TryGetValue propertyName with
                | (false, _) ->
                    match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                    | None -> World.tryGetLayerCalculatedProperty propertyName layer world
                    | Some _ as propertyOpt -> propertyOpt
                | (true, getter) -> Some (getter layer world)
            else None

        static member internal getLayerProperty propertyName layer world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                | None ->
                    match World.tryGetLayerCalculatedProperty propertyName layer world with
                    | None -> failwithf "Could not find property '%s'." propertyName
                    | Some property -> property
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
                            propertyName layer world
                    (success, world)
                | (true, setter) -> setter property layer world
            else (false, world)

        static member internal setLayerProperty propertyName property layer world =
            if World.getLayerExists layer world then
                match Setters.TryGetValue propertyName with
                | (false, _) -> World.updateLayerState (LayerState.setProperty propertyName property) propertyName layer world
                | (true, setter) ->
                    match setter property layer world with
                    | (true, world) -> world
                    | (false, _) -> failwith ("Cannot change layer property " + propertyName + ".")
            else world

        static member internal attachLayerProperty propertyName property layer world =
            if World.getLayerExists layer world
            then World.updateLayerState (LayerState.attachProperty propertyName property) propertyName layer world
            else failwith ("Cannot attach layer property '" + propertyName + "'; layer '" + layer.LayerName + "' is not found.")

        static member internal detachLayerProperty propertyName layer world =
            if World.getLayerExists layer world
            then World.updateLayerState (LayerState.detachProperty propertyName) propertyName layer world
            else failwith ("Cannot detach layer property '" + propertyName + "'; layer '" + layer.LayerName + "' is not found.")

        static member private layerOnRegisterChanged evt world =
            let layer = evt.Subscriber : Layer
            let world = World.unregisterLayer layer world
            World.registerLayer layer world

        static member private layerScriptOptChanged evt world =
            let layer = evt.Subscriber : Layer
            match World.getLayerScriptOpt layer world with
            | Some script ->
                let symbolLoadMetadata = { ImplicitDelimiters = true; StripCsvHeader = false }
                match World.assetTagToValueOpt<Scripting.Expr array> symbolLoadMetadata script world with
                | (Some script, world) -> World.setLayerScript script layer world
                | (None, world) -> world
            | None -> world

        static member internal registerLayer layer world =
            let world = World.monitor World.layerOnRegisterChanged (ltoa<World ParticipantChangeData> ["Change"; (Property? OnRegister); "Event"] --> layer) layer world
            let world = World.monitor World.layerScriptOptChanged (ltoa<World ParticipantChangeData> ["Change"; (Property? ScriptOpt); "Event"] --> layer) layer world
            World.withEventContext (fun world ->
                let dispatcher = World.getLayerDispatcher layer world
                let world = dispatcher.Register (layer, world)
                let eventTrace = EventTrace.record "World" "registerLayer" EventTrace.empty
                let world = World.publish () (ltoa<unit> ["Register"; "Event"] --> layer) eventTrace layer world
                eval (World.getLayerOnRegister layer world) (World.getLayerScriptFrame layer world) layer world |> snd')
                layer
                world

        static member internal unregisterLayer layer world =
            World.withEventContext (fun world ->
                let world = eval (World.getLayerOnUnregister layer world) (World.getLayerScriptFrame layer world) layer world |> snd'
                let dispatcher = World.getLayerDispatcher layer world
                let eventTrace = EventTrace.record "World" "unregisteringLayer" EventTrace.empty
                let world = World.publish () (ltoa<unit> ["Unregistering"; "Event"] --> layer) eventTrace layer world
                dispatcher.Unregister (layer, world))
                layer
                world

        static member internal signalLayer signal layer world =
            World.withEventContext (fun world ->
                let world =
                    match ScriptingSystem.tryImport typeof<Symbol> signal world with
                    | Some signalExpr ->
                        ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("signal", signalExpr) }) world
                        let world = eval (World.getLayerOnSignal layer world) (World.getLayerScriptFrame layer world) layer world |> snd'
                        ScriptingSystem.removeProceduralBindings world
                        world
                    | None -> failwithumf ()
                let dispatcher = World.getLayerDispatcher layer world
                let eventTrace = EventTrace.record "World" "signalLayer" EventTrace.empty
                let world = World.publish signal (ltoa<Symbol> ["Signal"; "Event"] --> layer) eventTrace layer world
                dispatcher.Signal (signal, layer, world))
                layer
                world

        static member private addLayer mayReplace layerState layer world =
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

        /// Create a layer and add it to the world.
        [<FunctionBinding "createLayer">]
        static member createLayer4 dispatcherName nameOpt (screen : Screen) world =
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find a LayerDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
            let layerState = LayerState.make nameOpt dispatcher
            let layerState = Reflection.attachProperties LayerState.copy layerState.Dispatcher layerState world
            let layer = Layer (screen.ScreenAddress <-- ntoa<Layer> layerState.Name)
            let world =
                if World.getLayerExists layer world then
                    Log.debug "Scheduling layer creation assuming existing layer at the same address is being destroyed."
                    World.schedule2 (World.addLayer false layerState layer) world
                else World.addLayer false layerState layer world
            (layer, world)

        /// Create a layer and add it to the world.
        static member createLayer<'d when 'd :> LayerDispatcher> nameOpt screen world =
            World.createLayer4 typeof<'d>.Name nameOpt screen world

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
        Getters.Add ("Id", fun layer world -> { PropertyType = typeof<Guid>; PropertyValue = World.getLayerId layer world })
        Getters.Add ("Name", fun layer world -> { PropertyType = typeof<string>; PropertyValue = World.getLayerName layer world })
        Getters.Add ("Dispatcher", fun layer world -> { PropertyType = typeof<LayerDispatcher>; PropertyValue = World.getLayerDispatcher layer world })
        Getters.Add ("Persistent", fun layer world -> { PropertyType = typeof<bool>; PropertyValue = World.getLayerPersistent layer world })
        Getters.Add ("CreationTimeStamp", fun layer world -> { PropertyType = typeof<int64>; PropertyValue = World.getLayerCreationTimeStamp layer world })
        Getters.Add ("Imperative", fun layer world -> { PropertyType = typeof<bool>; PropertyValue = World.getLayerImperative layer world })
        Getters.Add ("ScriptOpt", fun layer world -> { PropertyType = typeof<Symbol AssetTag option>; PropertyValue = World.getLayerScriptOpt layer world })
        Getters.Add ("Script", fun layer world -> { PropertyType = typeof<Scripting.Expr array>; PropertyValue = World.getLayerScript layer world })
        Getters.Add ("ScriptFrame", fun layer world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getLayerScript layer world })
        Getters.Add ("ScriptUnsubscriptions", fun layer world -> { PropertyType = typeof<Unsubscription list>; PropertyValue = World.getLayerScriptUnsubscriptions layer world })
        Getters.Add ("OnRegister", fun layer world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnRegister layer world })
        Getters.Add ("OnUnregister", fun layer world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUnregister layer world })
        Getters.Add ("OnUpdate", fun layer world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUpdate layer world })
        Getters.Add ("OnPostUpdate", fun layer world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnPostUpdate layer world })
        Getters.Add ("OnSignal", fun layer world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnSignal layer world })
        Getters.Add ("Depth", fun layer world -> { PropertyType = typeof<single>; PropertyValue = World.getLayerDepth layer world })
        Getters.Add ("Visible", fun layer world -> { PropertyType = typeof<single>; PropertyValue = World.getLayerVisible layer world })

    /// Initialize property setters.
    let private initSetters () =
        Setters.Add ("Id", fun _ _ world -> (false, world))
        Setters.Add ("Name", fun _ _ world -> (false, world))
        Setters.Add ("Dispatcher", fun _ _ world -> (false, world))
        Setters.Add ("Persistent", fun property layer world -> (true, World.setLayerPersistent (property.PropertyValue :?> bool) layer world))
        Setters.Add ("CreationTimeStamp", fun _ _ world -> (false, world))
        Setters.Add ("Imperative", fun _ _ world -> (false, world))
        Setters.Add ("ScriptOpt", fun property layer world -> (true, World.setLayerScriptOpt (property.PropertyValue :?> Symbol AssetTag option) layer world))
        Setters.Add ("Script", fun property layer world -> (true, World.setLayerScript (property.PropertyValue :?> Scripting.Expr array) layer world))
        Setters.Add ("ScriptFrame", fun _ _ world -> (false, world))
        Setters.Add ("ScriptUnsubscriptions", fun property layer world -> (true, World.setLayerScriptUnsubscriptions (property.PropertyValue :?> Unsubscription list) layer world))
        Setters.Add ("OnRegister", fun property layer world -> (true, World.setLayerOnRegister (property.PropertyValue :?> Scripting.Expr) layer world))
        Setters.Add ("OnUnregister", fun property layer world -> (true, World.setLayerOnUnregister (property.PropertyValue :?> Scripting.Expr) layer world))
        Setters.Add ("OnUpdate", fun property layer world -> (true, World.setLayerOnUpdate (property.PropertyValue :?> Scripting.Expr) layer world))
        Setters.Add ("OnPostUpdate", fun property layer world -> (true, World.setLayerOnPostUpdate (property.PropertyValue :?> Scripting.Expr) layer world))
        Setters.Add ("OnSignal", fun property layer world -> (true, World.setLayerOnSignal (property.PropertyValue :?> Scripting.Expr) layer world))
        Setters.Add ("Depth", fun property layer world -> (true, World.setLayerDepth (property.PropertyValue :?> single) layer world))
        Setters.Add ("Visible", fun property layer world -> (true, World.setLayerVisible (property.PropertyValue :?> bool) layer world))

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()