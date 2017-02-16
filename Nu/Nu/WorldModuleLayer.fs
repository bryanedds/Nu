// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldModuleLayer =

    type World with

        static member private layerStateKeyEquality
            (layerAddress : Layer Address, layerStates : UMap<Layer Address, LayerState>)
            (layerAddress2 : Layer Address, layerStates2 : UMap<Layer Address, LayerState>) =
            refEq layerAddress layerAddress2 && refEq layerStates layerStates2

        static member private layerGetFreshKeyAndValue (layer : Layer) world =
            let layerStateOpt = UMap.tryFind layer.LayerAddress world.LayerStates
            ((layer.LayerAddress, world.LayerStates), layerStateOpt)

        static member private layerStateFinder (layer : Layer) world =
            KeyedCache.getValue
                World.layerStateKeyEquality
                (fun () -> World.layerGetFreshKeyAndValue layer world)
                (layer.LayerAddress, world.LayerStates)
                (World.getLayerCachedOpt world)

        static member private layerStateAdder layerState (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        match UMap.tryFind layerName layerDirectory with
                        | Some (layerAddress, entityDirectory) ->
                            let layerDirectory = UMap.add layerName (layerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                        | None ->
                            let entityDirectory = UMap.makeEmpty None
                            let layerDirectory = UMap.add layerName (layer.LayerAddress, entityDirectory) layerDirectory
                            UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add layer '" + scstring layer.LayerAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid layer address '" + scstring layer.LayerAddress + "'."
            let layerStates = UMap.add layer.LayerAddress layerState world.LayerStates
            World.choose { world with ScreenDirectory = screenDirectory; LayerStates = layerStates }

        static member private layerStateRemover (layer : Layer) world =
            let screenDirectory =
                match Address.getNames layer.LayerAddress with
                | [screenName; layerName] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, layerDirectory) ->
                        let layerDirectory = UMap.remove layerName layerDirectory
                        UMap.add screenName (screenAddress, layerDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove layer '" + scstring layer.LayerAddress + "' from non-existent screen."
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
            match World.getLayerStateOpt layer world with
            | Some layerState -> layerState
            | None -> failwith ^ "Could not find layer with address '" + scstring layer.LayerAddress + "'."

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
            Option.isSome ^ World.getLayerStateOpt layer world

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
            let scriptFrame = Scripting.DeclarationFrame () (* HashIdentity *)
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
                | "Id" -> Some (World.getLayerId layer world :> obj, typeof<Guid>)
                | "Name" -> Some (World.getLayerName layer world :> obj, typeof<string>)
                | "DispatcherNp" -> Some (World.getLayerDispatcherNp layer world :> obj, typeof<LayerDispatcher>)
                | "Specialization" -> Some (World.getLayerSpecialization layer world :> obj, typeof<string>)
                | "Persistent" -> Some (World.getLayerPersistent layer world :> obj, typeof<bool>)
                | "CreationTimeStampNp" -> Some (World.getLayerCreationTimeStampNp layer world :> obj, typeof<int64>)
                | "Imperative" -> Some (World.getLayerImperative layer world :> obj, typeof<bool>)
                | "ScriptOpt" -> Some (World.getLayerScriptOpt layer world :> obj, typeof<AssetTag option>)
                | "Script" -> Some (World.getLayerScript layer world :> obj, typeof<Scripting.Expr list>)
                | "ScriptFrameNp" -> Some (World.getLayerScript layer world :> obj, typeof<Scripting.ProceduralFrame list>)
                | "OnRegister" -> Some (World.getLayerOnRegister layer world :> obj, typeof<Scripting.Expr>)
                | "OnUnregister" -> Some (World.getLayerOnUnregister layer world :> obj, typeof<Scripting.Expr>)
                | "OnUpdate" -> Some (World.getLayerOnUpdate layer world :> obj, typeof<Scripting.Expr>)
                | "OnPostUpdate" -> Some (World.getLayerOnPostUpdate layer world :> obj, typeof<Scripting.Expr>)
                | "Depth" -> Some (World.getLayerDepth layer world :> obj, typeof<single>)
                | "Visible" -> Some (World.getLayerVisible layer world :> obj, typeof<single>)
                | _ ->
                    match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                    | None -> World.tryGetLayerCalculatedProperty propertyName layer world
                    | Some _ as propertyOpt -> propertyOpt
            else None

        static member internal getLayerProperty propertyName layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> (World.getLayerId layer world :> obj, typeof<Guid>)
            | "Name" -> (World.getLayerName layer world :> obj, typeof<string>)
            | "DispatcherNp" -> (World.getLayerDispatcherNp layer world :> obj, typeof<LayerDispatcher>)
            | "Specialization" -> (World.getLayerSpecialization layer world :> obj, typeof<string>)
            | "Persistent" -> (World.getLayerPersistent layer world :> obj, typeof<bool>)
            | "CreationTimeStampNp" -> (World.getLayerCreationTimeStampNp layer world :> obj, typeof<int64>)
            | "Imperative" -> (World.getLayerImperative layer world :> obj, typeof<bool>)
            | "ScriptOpt" -> (World.getLayerScriptOpt layer world :> obj, typeof<AssetTag option>)
            | "Script" -> (World.getLayerScript layer world :> obj, typeof<Scripting.Expr list>)
            | "ScriptFrameNp" -> (World.getLayerScript layer world :> obj, typeof<Scripting.ProceduralFrame list>)
            | "OnRegister" -> (World.getLayerOnRegister layer world :> obj, typeof<Scripting.Expr>)
            | "OnUnregister" -> (World.getLayerOnUnregister layer world :> obj, typeof<Scripting.Expr>)
            | "OnUpdate" -> (World.getLayerOnUpdate layer world :> obj, typeof<Scripting.Expr>)
            | "OnPostUpdate" -> (World.getLayerOnPostUpdate layer world :> obj, typeof<Scripting.Expr>)
            | "Depth" -> (World.getLayerDepth layer world :> obj, typeof<single>)
            | "Visible" -> (World.getLayerVisible layer world :> obj, typeof<single>)
            | _ ->
                match LayerState.tryGetProperty propertyName (World.getLayerState layer world) with
                | None ->
                    match World.tryGetLayerCalculatedProperty propertyName layer world with
                    | None -> failwithf "Could not find property '%s'." propertyName
                    | Some property -> property
                | Some property -> property

        static member internal trySetLayerProperty propertyName (property : obj * Type) layer world =
            if World.layerExists layer world then
                match propertyName with // OPTIMIZATION: string match for speed
                | "Id" -> (false, world)
                | "Name" -> (false, world)
                | "DispatcherNp" -> (false, world)
                | "Specialization" -> (false, world)
                | "Persistent" -> (true, World.setLayerPersistent (property |> fst :?> bool) layer world)
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

        static member internal setLayerProperty propertyName (property : obj * Type) layer world =
            match propertyName with // OPTIMIZATION: string match for speed
            | "Id" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Name" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "DispatcherNp" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Specialization" -> failwith ^ "Cannot change layer " + propertyName + "."
            | "Persistent" -> World.setLayerPersistent (property |> fst :?> bool) layer world
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
                match World.assetTagToValueOpt<Scripting.Expr list> true script world with
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