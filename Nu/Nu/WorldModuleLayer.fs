// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldModuleLayer =

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
                    else failwith ^ "Cannot add layer '" + scstring layer.LayerAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
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
                    else failwith ^ "Cannot remove layer '" + scstring layer.LayerAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
            let layerStates = UMap.remove layer.LayerAddress world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateSetter layerState (layer : Layer) world =
#if DEBUG
            if not ^ UMap.containsKey layer.LayerAddress world.LayerStates then
                failwith ^ "Cannot set the state of a non-existent layer '" + scstring layer.LayerAddress + "'"
            if not ^ World.qualifyEventContext (atooa layer.LayerAddress) world then
                failwith ^ "Cannot set the state of a layer in an unqualifed event context."
#endif
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with LayerStates = layerStates }

        static member private addLayerState layerState layer world =
            World.layerStateAdder layerState layer world

        static member private removeLayerState layer world =
            World.layerStateRemover layer world

        static member private publishLayerChange (propertyName : string) (layer : Layer) oldWorld world =
            let changeEventAddress = ltoa ["Layer"; "Change"; propertyName; "Event"] ->>- layer.LayerAddress
            let eventTrace = EventTrace.record "World" "publishLayerChange" EventTrace.empty
            World.publishPlus World.sortSubscriptionsByHierarchy { Participant = layer; PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace layer false world

        static member private getLayerStateOpt layer world =
            World.layerStateFinder layer world

        static member private getLayerState layer world =
            let layerStateOpt = World.getLayerStateOpt layer world
            if FOption.isSome layerStateOpt then FOption.get layerStateOpt
            else failwith ^ "Could not find layer with address '" + scstring layer.LayerAddress + "'."

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
        static member internal layerExists layer world =
            FOption.isSome ^ World.getLayerStateOpt layer world

        static member internal getLayerId layer world = (World.getLayerState layer world).Id
        static member internal getLayerName layer world = (World.getLayerState layer world).Name
        static member internal getLayerDispatcherNp layer world = (World.getLayerState layer world).DispatcherNp
        static member internal getLayerSpecialization layer world = (World.getLayerState layer world).Specialization
        static member internal getLayerPersistent layer world = (World.getLayerState layer world).Persistent
        static member internal setLayerPersistent value layer world = World.updateLayerState (fun layerState -> { layerState with Persistent = value }) Property? Persistent layer world
        static member internal getLayerCreationTimeStampNp layer world = (World.getLayerState layer world).CreationTimeStampNp
        static member internal getLayerImperative layer world = Xtension.getImperative (World.getLayerState layer world).Xtension
        static member internal getLayerScriptOpt layer world = (World.getLayerState layer world).ScriptOpt
        static member internal setLayerScriptOpt value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptOpt = value }) Property? ScriptOpt layer world
        static member internal getLayerScript layer world = (World.getLayerState layer world).Script
        static member internal setLayerScript value layer world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateLayerState (fun layerState -> { layerState with Script = value }) Property? Script layer world
            let world = World.setLayerScriptFrameNp scriptFrame layer world
            evalManyWithLogging value scriptFrame layer world |> snd
        static member internal getLayerScriptFrameNp layer world = (World.getLayerState layer world).ScriptFrameNp
        static member internal setLayerScriptFrameNp value layer world = World.updateLayerState (fun layerState -> { layerState with ScriptFrameNp = value }) Property? ScriptFrameNp layer world
        static member internal getLayerOnRegister layer world = (World.getLayerState layer world).OnRegister
        static member internal setLayerOnRegister value layer world = World.updateLayerState (fun layerState -> { layerState with OnRegister = value }) Property? OnRegister layer world
        static member internal getLayerOnUnregister layer world = (World.getLayerState layer world).OnUnregister
        static member internal setLayerOnUnregister value layer world = World.updateLayerState (fun layerState -> { layerState with OnUnregister = value }) Property? OnUnregister layer world
        static member internal getLayerOnUpdate layer world = (World.getLayerState layer world).OnUpdate
        static member internal setLayerOnUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnUpdate = value }) Property? OnUpdate layer world
        static member internal getLayerOnPostUpdate layer world = (World.getLayerState layer world).OnPostUpdate
        static member internal setLayerOnPostUpdate value layer world = World.updateLayerState (fun layerState -> { layerState with OnPostUpdate = value }) Property? OnPostUpdate layer world
        static member internal getLayerDepth layer world = (World.getLayerState layer world).Depth
        static member internal setLayerDepth value layer world = World.updateLayerState (fun layerState -> { layerState with Depth = value }) Property? Depth layer world
        static member internal getLayerVisible layer world = (World.getLayerState layer world).Visible
        static member internal setLayerVisible value layer world = World.updateLayerState (fun layerState -> { layerState with Visible = value }) Property? Visible layer world

        static member internal tryGetLayerCalculatedProperty propertyName layer world =
            let dispatcher = World.getLayerDispatcherNp layer world
            dispatcher.TryGetCalculatedProperty (propertyName, layer, world)

        static member internal tryGetLayerProperty propertyName layer world =
            if World.layerExists layer world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> Some { PropertyType = typeof<Guid>; PropertyValue = World.getLayerId layer world }
                | "Name" -> Some { PropertyType = typeof<string>; PropertyValue = World.getLayerName layer world }
                | "DispatcherNp" -> Some { PropertyType = typeof<LayerDispatcher>; PropertyValue = World.getLayerDispatcherNp layer world }
                | "Specialization" -> Some { PropertyType = typeof<string>; PropertyValue = World.getLayerSpecialization layer world }
                | "Persistent" -> Some { PropertyType = typeof<bool>; PropertyValue = World.getLayerPersistent layer world }
                | "CreationTimeStampNp" -> Some { PropertyType = typeof<int64>; PropertyValue = World.getLayerCreationTimeStampNp layer world }
                | "Imperative" -> Some { PropertyType = typeof<bool>; PropertyValue = World.getLayerImperative layer world }
                | "ScriptOpt" -> Some { PropertyType = typeof<AssetTag option>; PropertyValue = World.getLayerScriptOpt layer world }
                | "Script" -> Some { PropertyType = typeof<Scripting.Expr array>; PropertyValue = World.getLayerScript layer world }
                | "ScriptFrameNp" -> Some { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getLayerScript layer world }
                | "OnRegister" -> Some { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnRegister layer world }
                | "OnUnregister" -> Some { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUnregister layer world }
                | "OnUpdate" -> Some { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUpdate layer world }
                | "OnPostUpdate" -> Some { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnPostUpdate layer world }
                | "Depth" -> Some { PropertyType = typeof<single>; PropertyValue = World.getLayerDepth layer world }
                | "Visible" -> Some { PropertyType = typeof<single>; PropertyValue = World.getLayerVisible layer world }
                | _ ->
                    match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                    | None -> World.tryGetLayerCalculatedProperty propertyName layer world
                    | Some _ as propertyOpt -> propertyOpt
            else None

        static member internal getLayerProperty propertyName layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> { PropertyType = typeof<Guid>; PropertyValue = World.getLayerId layer world }
            | "Name" -> { PropertyType = typeof<string>; PropertyValue = World.getLayerName layer world }
            | "DispatcherNp" -> { PropertyType = typeof<LayerDispatcher>; PropertyValue = World.getLayerDispatcherNp layer world }
            | "Specialization" -> { PropertyType = typeof<string>; PropertyValue = World.getLayerSpecialization layer world }
            | "Persistent" -> { PropertyType = typeof<bool>; PropertyValue = World.getLayerPersistent layer world }
            | "CreationTimeStampNp" -> { PropertyType = typeof<int64>; PropertyValue = World.getLayerCreationTimeStampNp layer world }
            | "Imperative" -> { PropertyType = typeof<bool>; PropertyValue = World.getLayerImperative layer world }
            | "ScriptOpt" -> { PropertyType = typeof<AssetTag option>; PropertyValue = World.getLayerScriptOpt layer world }
            | "Script" -> { PropertyType = typeof<Scripting.Expr array>; PropertyValue = World.getLayerScript layer world }
            | "ScriptFrameNp" -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getLayerScript layer world }
            | "OnRegister" -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnRegister layer world }
            | "OnUnregister" -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUnregister layer world }
            | "OnUpdate" -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnUpdate layer world }
            | "OnPostUpdate" -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getLayerOnPostUpdate layer world }
            | "Depth" -> { PropertyType = typeof<single>; PropertyValue = World.getLayerDepth layer world }
            | "Visible" -> { PropertyType = typeof<single>; PropertyValue = World.getLayerVisible layer world }
            | _ ->
                match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                | None ->
                    match World.tryGetLayerCalculatedProperty propertyName layer world with
                    | None -> failwithf "Could not find property '%s'." propertyName
                    | Some property -> property
                | Some property -> property

        static member internal trySetLayerProperty propertyName property layer world =
            if World.layerExists layer world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> (false, world)
                | "Name" -> (false, world)
                | "DispatcherNp" -> (false, world)
                | "Specialization" -> (false, world)
                | "Persistent" -> (true, World.setLayerPersistent (property.PropertyValue :?> bool) layer world)
                | "CreationTimeStampNp" -> (false, world)
                | "Imperative" -> (false, world)
                | "ScriptOpt" -> (false, world)
                | "Script" -> (false, world)
                | "ScriptFrameNp" -> (false, world)
                | "OnRegister" -> (false, world)
                | "OnUnregister" -> (false, world)
                | "OnUpdate" -> (false, world)
                | "OnPostUpdate" -> (false, world)
                | _ ->
                    // HACK: needed to mutate a flag to get the success state out of an updateLayerState callback...
                    let mutable success = false
                    let world =
                        World.updateLayerState (fun layerState ->
                            let (successInner, layerState) = LayerState.trySetProperty propertyName property layerState
                            success <- successInner; layerState)
                            propertyName layer world
                    (success, world)
            else (false, world)

        static member internal setLayerProperty propertyName property layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Name" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Persistent" -> World.setLayerPersistent (property.PropertyValue :?> bool) layer world
            | "CreationTimeStampNp" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Imperative" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "ScriptOpt" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "Script" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "ScriptFrameNp" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnRegister" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnUnregister" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnUpdate" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | "OnPostUpdate" -> failwith ^ "Cannot change layer " + propertyName + " dynamically."
            | _ -> World.updateLayerState (LayerState.setProperty propertyName property) propertyName layer world

        static member private layerOnRegisterChanged evt world =
            let layer = evt.Subscriber : Layer
            let world = World.unregisterLayer layer world
            World.registerLayer layer world

        static member private layerScriptOptChanged evt world =
            let layer = evt.Subscriber : Layer
            match World.getLayerScriptOpt layer world with
            | Some script ->
                match World.assetTagToValueOpt<Scripting.Expr array> true script world with
                | (Some script, world) -> World.setLayerScript script layer world
                | (None, world) -> world
            | None -> world

        static member internal registerLayer layer world =
            let world = World.monitor World.layerOnRegisterChanged (ltoa<ParticipantChangeData<Layer, World>> ["Layer"; "Change"; (Property? OnRegister); "Event"] ->- layer) layer world
            let world = World.monitor World.layerScriptOptChanged (ltoa<ParticipantChangeData<Layer, World>> ["Layer"; "Change"; (Property? ScriptOpt); "Event"] ->- layer) layer world
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = World.getLayerDispatcherNp layer world
                    let world = dispatcher.Register (layer, world)
                    let eventTrace = EventTrace.record "World" "registerLayer" EventTrace.empty
                    let world = World.publish () (ltoa<unit> ["Layer"; "Register"; "Event"] ->- layer) eventTrace layer world
                    eval (World.getLayerOnUnregister layer world) (World.getLayerScriptFrameNp layer world) layer world |> snd)
                    layer
                    world
            World.choose world

        static member internal unregisterLayer layer world =
            let world =
                World.withEventContext (fun world ->
                    let world = eval (World.getLayerOnRegister layer world) (World.getLayerScriptFrameNp layer world) layer world |> snd
                    let dispatcher = World.getLayerDispatcherNp layer world
                    let eventTrace = EventTrace.record "World" "unregisterLayer" EventTrace.empty
                    let world = World.publish () (ltoa<unit> ["Layer"; "Unregistering"; "Event"] ->- layer) eventTrace layer world
                    dispatcher.Unregister (layer, world))
                    layer
                    world
            World.choose world

        static member private addLayer mayReplace layerState layer world =
            let isNew = not ^ World.layerExists layer world
            if isNew || mayReplace then
                let world = World.addLayerState layerState layer world
                if isNew then World.registerLayer layer world else world
            else failwith ^ "Adding a layer that the world already contains at address '" + scstring layer.LayerAddress + "'."

        static member internal removeLayer3 removeEntities layer world =
            if World.layerExists layer world then
                let world = World.unregisterLayer layer world
                let world = removeEntities layer world
                World.removeLayerState layer world
            else world

        /// Create a layer and add it to the world.
        static member createLayer5 dispatcherName specializationOpt nameOpt (screen : Screen) world =
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ^ "Could not find a LayerDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
            let layerState = LayerState.make specializationOpt nameOpt dispatcher
            let layerState = Reflection.attachProperties LayerState.copy dispatcher layerState
            let layer = Layer (screen.ScreenAddress -<<- ntoa<Layer> layerState.Name)
            let world = World.addLayer false layerState layer world
            (layer, world)

        /// Create a layer and add it to the world.
        static member createLayer<'d when 'd :> LayerDispatcher> specializationOpt nameOpt screen world =
            World.createLayer5 typeof<'d>.Name specializationOpt nameOpt screen world

        static member internal writeLayer4 writeEntities layer layerDescriptor world =
            let layerState = World.getLayerState layer world
            let layerDispatcherName = getTypeName layerState.DispatcherNp
            let layerDescriptor = { layerDescriptor with LayerDispatcher = layerDispatcherName }
            let getLayerProperties = Reflection.writePropertiesFromTarget tautology3 layerDescriptor.LayerProperties layerState
            let layerDescriptor = { layerDescriptor with LayerProperties = getLayerProperties }
            writeEntities layer layerDescriptor world

        static member internal readLayer5 readEntities layerDescriptor nameOpt (screen : Screen) world =

            // create the dispatcher
            let dispatcherName = layerDescriptor.LayerDispatcher
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not find LayerDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?"
                    let dispatcherName = typeof<LayerDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the layer state and populate its properties
            let layerState = LayerState.make None None dispatcher
            let layerState = Reflection.attachProperties LayerState.copy layerState.DispatcherNp layerState
            let layerState = Reflection.readPropertiesToTarget LayerState.copy layerDescriptor.LayerProperties layerState

            // apply the name if one is provided
            let layerState =
                match nameOpt with
                | Some name -> { layerState with Name = name }
                | None -> layerState

            // add the layer's state to the world
            let layer = Layer (screen.ScreenAddress -<<- ntoa<Layer> layerState.Name)
            let world = World.addLayer true layerState layer world

            // read the layer's entities
            let world = readEntities layerDescriptor layer world |> snd
            (layer, world)

        /// View all of the properties of a layer.
        static member internal viewLayerProperties layer world =
            let state = World.getLayerState layer world
            let properties = World.getProperties state
            Array.ofList properties