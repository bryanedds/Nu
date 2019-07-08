// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleScreen =

    /// Dynamic property getters.
    let internal Getters = Dictionary<string, Screen -> World -> Property> HashIdentity.Structural

    /// Dynamic property setters.
    let internal Setters = Dictionary<string, Property -> Screen -> World -> bool * World> HashIdentity.Structural

    type World with
    
        static member private screenStateFinder (screen : Screen) world =
            UMap.tryFindFast screen.ScreenAddress world.ScreenStates

        static member private screenStateAdder screenState (screen : Screen) world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] ->
                    let layerDirectoryOpt = UMap.tryFindFast screenName world.ScreenDirectory
                    if FOption.isSome layerDirectoryOpt then
                        let layerDirectory = FOption.get layerDirectoryOpt
                        // NOTE: this is logically a redundant operation...
                        let layerDirectory = KeyValuePair (screen.ScreenAddress, layerDirectory.Value)
                        UMap.add screenName layerDirectory world.ScreenDirectory
                    else
                        let layerDirectory = KeyValuePair (screen.ScreenAddress, UMap.makeEmpty Constants.Engine.SimulantMapConfig)
                        UMap.add screenName layerDirectory world.ScreenDirectory
                | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            let screenStates = UMap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover (screen : Screen) world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] -> UMap.remove screenName world.ScreenDirectory
                | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            let screenStates = UMap.remove screen.ScreenAddress world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateSetter screenState (screen : Screen) world =
#if DEBUG
            if not (UMap.containsKey screen.ScreenAddress world.ScreenStates) then
                failwith ("Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'")
            if not (World.qualifyEventContext (atooa screen.ScreenAddress) world) then
                failwith "Cannot set the state of a screen in an unqualifed event context."
#endif
            let screenStates = UMap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member private removeScreenState screen world =
            World.screenStateRemover screen world

        static member private publishScreenChange (propertyName : string) (screen : Screen) oldWorld world =
            let world =
                let changeEventAddress = ltoa ["Change"; propertyName; "Event"] --> screen.ScreenAddress
                let eventTrace = EventTrace.record "World" "publishScreenChange" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy { PropertyName = propertyName; OldWorld = oldWorld } changeEventAddress eventTrace screen false world
            world

        static member private getScreenStateOpt screen world =
             World.screenStateFinder screen world

        static member internal getScreenState screen world =
            let screenStateOpt = World.getScreenStateOpt screen world
            if FOption.isSome screenStateOpt then FOption.get screenStateOpt
            else failwith ("Could not find screen with address '" + scstring screen.ScreenAddress + "'.")

        static member internal setScreenState screenState screen world =
            World.screenStateSetter screenState screen world

        static member private updateScreenStateWithoutEvent updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        static member private updateScreenState updater propertyName screen world =
            let oldWorld = world
            let world = World.updateScreenStateWithoutEvent updater screen world
            World.publishScreenChange propertyName screen oldWorld world

        /// Check that a screen exists in the world.
        static member internal getScreenExists screen world =
            FOption.isSome (World.getScreenStateOpt screen world)

        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name
        static member internal getScreenDispatcher screen world = (World.getScreenState screen world).Dispatcher
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) Property? Persistent screen world
        static member internal getScreenCreationTimeStamp screen world = (World.getScreenState screen world).CreationTimeStamp
        static member internal getScreenScriptOpt screen world = (World.getScreenState screen world).ScriptOpt
        static member internal setScreenScriptOpt value screen world = World.updateScreenState (fun screenState -> { screenState with ScriptOpt = value }) Property? ScriptOpt screen world
        static member internal getScreenScript screen world = (World.getScreenState screen world).Script
        static member internal setScreenScript value screen world =
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = World.updateScreenState (fun screenState -> { screenState with Script = value }) Property? Script screen world
            let world = World.setScreenScriptFrame scriptFrame screen world
            evalManyWithLogging value scriptFrame screen world |> snd'
        static member internal getScreenScriptFrame screen world = (World.getScreenState screen world).ScriptFrame
        static member internal setScreenScriptFrame value screen world = World.updateScreenState (fun screenState -> { screenState with ScriptFrame = value }) Property? ScriptFrame screen world
        static member internal getScreenScriptUnsubscriptions screen world = (World.getScreenState screen world).ScriptUnsubscriptions
        static member internal setScreenScriptUnsubscriptions value screen world = World.updateScreenState (fun screenState -> { screenState with ScriptUnsubscriptions = value }) Property? ScriptUnsubscriptions screen world
        static member internal getScreenOnRegister screen world = (World.getScreenState screen world).OnRegister
        static member internal setScreenOnRegister value screen world = World.updateScreenState (fun screenState -> { screenState with OnRegister = value }) Property? OnRegister screen world
        static member internal getScreenOnUnregister screen world = (World.getScreenState screen world).OnUnregister
        static member internal setScreenOnUnregister value screen world = World.updateScreenState (fun screenState -> { screenState with OnUnregister = value }) Property? OnUnregister screen world
        static member internal getScreenOnUpdate screen world = (World.getScreenState screen world).OnUpdate
        static member internal setScreenOnUpdate value screen world = World.updateScreenState (fun screenState -> { screenState with OnUpdate = value }) Property? OnUpdate screen world
        static member internal getScreenOnPostUpdate screen world = (World.getScreenState screen world).OnPostUpdate
        static member internal setScreenOnPostUpdate value screen world = World.updateScreenState (fun screenState -> { screenState with OnPostUpdate = value }) Property? OnPostUpdate screen world
        static member internal getScreenOnSignal screen world = (World.getScreenState screen world).OnSignal
        static member internal setScreenOnSignal value screen world = World.updateScreenState (fun screenState -> { screenState with OnSignal = value }) Property? OnSignal screen world
        static member internal getScreenTransitionState screen world = (World.getScreenState screen world).TransitionState
        static member internal setScreenTransitionState value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionState = value }) Property? TransitionState screen world
        static member internal getScreenTransitionTicks screen world = (World.getScreenState screen world).TransitionTicks
        static member internal setScreenTransitionTicks value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionTicks = value }) Property? TransitionTicks screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) Property? Incoming screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) Property? Outgoing screen world

        static member internal tryGetScreenCalculatedProperty propertyName screen world =
            let dispatcher = World.getScreenDispatcher screen world
            dispatcher.TryGetCalculatedProperty (propertyName, screen, world)

        static member internal tryGetScreenProperty propertyName screen world =
            if World.getScreenExists screen world then
                match Getters.TryGetValue propertyName with
                | (false, _) ->
                    match ScreenState.tryGetProperty propertyName (World.getScreenState screen world) with
                    | None -> World.tryGetScreenCalculatedProperty propertyName screen world
                    | Some _ as propertyOpt -> propertyOpt
                | (true, getter) -> Some (getter screen world)
            else None

        static member internal getScreenProperty propertyName screen world =
            match Getters.TryGetValue propertyName with
            | (false, _) ->
                match ScreenState.tryGetProperty propertyName (World.getScreenState screen world) with
                | None ->
                    match World.tryGetScreenCalculatedProperty propertyName screen world with
                    | None -> failwithf "Could not find property '%s'." propertyName
                    | Some property -> property
                | Some property -> property
            | (true, getter) -> getter screen world

        static member internal trySetScreenProperty propertyName property screen world =
            if World.getScreenExists screen world then
                match Setters.TryGetValue propertyName with
                | (false, _) ->
                    let mutable success = false // bit of a hack to get additional state out of the lambda
                    let world =
                        World.updateScreenState (fun screenState ->
                            let (successInner, screenState) = ScreenState.trySetProperty propertyName property screenState
                            success <- successInner
                            screenState)
                            propertyName screen world
                    (success, world)
                | (true, setter) -> setter property screen world
            else (false, world)

        static member internal setScreenProperty propertyName property screen world =
            if World.getScreenExists screen world then
                match Setters.TryGetValue propertyName with
                | (false, _) -> World.updateScreenState (ScreenState.setProperty propertyName property) propertyName screen world
                | (true, setter) ->
                    match setter property screen world with
                    | (true, world) -> world
                    | (false, _) -> failwith ("Cannot change screen property " + propertyName + ".")
            else world

        static member internal attachScreenProperty propertyName property screen world =
            if World.getScreenExists screen world
            then World.updateScreenState (ScreenState.attachProperty propertyName property) propertyName screen world
            else failwith ("Cannot attach screen property '" + propertyName + "'; screen '" + screen.ScreenName + "' is not found.")

        static member internal detachScreenProperty propertyName screen world =
            if World.getScreenExists screen world
            then World.updateScreenState (ScreenState.detachProperty propertyName) propertyName screen world
            else failwith ("Cannot detach screen property '" + propertyName + "'; screen '" + screen.ScreenName + "' is not found.")

        static member private screenOnRegisterChanged evt world =
            let screen = evt.Subscriber : Screen
            let world = World.unregisterScreen screen world
            World.registerScreen screen world

        static member private screenScriptOptChanged evt world =
            let screen = evt.Subscriber : Screen
            match World.getScreenScriptOpt screen world with
            | Some script ->
                let symbolLoadMetadata = { ImplicitDelimiters = true; StripCsvHeader = false }
                match World.assetTagToValueOpt<Scripting.Expr array> script symbolLoadMetadata world with
                | Some script -> World.setScreenScript script screen world
                | None -> world
            | None -> world

        static member internal registerScreen screen world =
            let world = World.monitor World.screenOnRegisterChanged (ltoa<World ParticipantChangeData> ["Change"; (Property? OnRegister); "Event"] --> screen) screen world
            let world = World.monitor World.screenScriptOptChanged (ltoa<World ParticipantChangeData> ["Change"; (Property? ScriptOpt); "Event"] --> screen) screen world
            World.withEventContext (fun world ->
                let dispatcher = World.getScreenDispatcher screen world
                let world = dispatcher.Register (screen, world)
                let eventTrace = EventTrace.record "World" "registerScreen" EventTrace.empty
                let world = World.publish () (ltoa<unit> ["Register"; "Event"] --> screen) eventTrace screen world
                eval (World.getScreenOnRegister screen world) (World.getScreenScriptFrame screen world) screen world |> snd')
                screen
                world

        static member internal unregisterScreen screen world =
            World.withEventContext (fun world ->
                let world = eval (World.getScreenOnUnregister screen world) (World.getScreenScriptFrame screen world) screen world |> snd'
                let dispatcher = World.getScreenDispatcher screen world
                let eventTrace = EventTrace.record "World" "unregisteringScreen" EventTrace.empty
                let world = World.publish () (ltoa<unit> ["Unregistering"; "Event"] --> screen) eventTrace screen world
                dispatcher.Unregister (screen, world))
                screen
                world

        static member internal signalScreen signal screen world =
            World.withEventContext (fun world ->
                let world =
                    match ScriptingSystem.tryImport typeof<Symbol> signal world with
                    | Some signalExpr ->
                        ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("signal", signalExpr) }) world
                        let world = eval (World.getScreenOnSignal screen world) (World.getScreenScriptFrame screen world) screen world |> snd'
                        ScriptingSystem.removeProceduralBindings world
                        world
                    | None -> failwithumf ()
                let dispatcher = World.getScreenDispatcher screen world
                let eventTrace = EventTrace.record "World" "signalScreen" EventTrace.empty
                let world = World.publish signal (ltoa<Symbol> ["Signal"; "Event"] --> screen) eventTrace screen world
                dispatcher.Signal (signal, screen, world))
                screen
                world

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not (World.getScreenExists screen world)
            if isNew || mayReplace then
                let world = World.addScreenState screenState screen world
                if isNew then World.registerScreen screen world else world
            else failwith ("Adding a screen that the world already contains at address '" + scstring screen.ScreenAddress + "'.")

        static member internal removeScreen3 removeLayers screen world =
            if World.getScreenExists screen world then
                let world = World.unregisterScreen screen world
                let world = removeLayers screen world
                World.removeScreenState screen world
            else world

        static member internal writeScreen4 writeLayers screen screenDescriptor world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.Dispatcher
            let screenDescriptor = { screenDescriptor with ScreenDispatcherName = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            writeLayers screen screenDescriptor world

        static member internal readScreen4 readLayers screenDescriptor nameOpt world =
            
            // create the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcherName
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ("Could not find ScreenDispatcher '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the screen state and populate its properties
            let screenState = ScreenState.make None dispatcher
            let screenState = Reflection.attachProperties ScreenState.copy screenState.Dispatcher screenState world
            let screenState = Reflection.readPropertiesToTarget ScreenState.copy screenDescriptor.ScreenProperties screenState

            // apply the name if one is provided
            let screenState =
                match nameOpt with
                | Some name -> { screenState with Name = name }
                | None -> screenState

            // add the screen's state to the world
            let screen = Screen (ntoa screenState.Name)
            let world = World.addScreen true screenState screen world
            
            // read the screen's layers
            let world = readLayers screenDescriptor screen world |> snd
            (screen, world)

        /// View all of the properties of a screen.
        static member internal viewScreenProperties screen world =
            let state = World.getScreenState screen world
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c

    /// Initialize property getters.
    let private initGetters () =
        Getters.Add ("Id", fun screen world -> { PropertyType = typeof<Guid>; PropertyValue = World.getScreenId screen world })
        Getters.Add ("Name", fun screen world -> { PropertyType = typeof<string>; PropertyValue = World.getScreenName screen world })
        Getters.Add ("Dispatcher", fun screen world -> { PropertyType = typeof<ScreenDispatcher>; PropertyValue = World.getScreenDispatcher screen world })
        Getters.Add ("Persistent", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenPersistent screen world })
        Getters.Add ("CreationTimeStamp", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenCreationTimeStamp screen world })
        Getters.Add ("ScriptOpt", fun screen world -> { PropertyType = typeof<Symbol AssetTag option>; PropertyValue = World.getScreenScriptOpt screen world })
        Getters.Add ("Script", fun screen world -> { PropertyType = typeof<Scripting.Expr array>; PropertyValue = World.getScreenScript screen world })
        Getters.Add ("ScriptFrame", fun screen world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getScreenScriptFrame screen world })
        Getters.Add ("ScriptUnsubscriptions", fun screen world -> { PropertyType = typeof<Unsubscription list>; PropertyValue = World.getScreenScriptUnsubscriptions screen world })
        Getters.Add ("OnRegister", fun screen world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getScreenOnRegister screen world })
        Getters.Add ("OnUnregister", fun screen world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getScreenOnUnregister screen world })
        Getters.Add ("OnUpdate", fun screen world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getScreenOnUpdate screen world })
        Getters.Add ("OnPostUpdate", fun screen world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getScreenOnPostUpdate screen world })
        Getters.Add ("OnSignal", fun screen world -> { PropertyType = typeof<Scripting.Expr>; PropertyValue = World.getScreenOnSignal screen world })
        Getters.Add ("TransitionState", fun screen world -> { PropertyType = typeof<TransitionState>; PropertyValue = World.getScreenTransitionState screen world })
        Getters.Add ("TransitionTicks", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenTransitionTicks screen world })
        Getters.Add ("Incoming", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenIncoming screen world })
        Getters.Add ("Outgoing", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenOutgoing screen world })

    /// Initialize property setters.
    let private initSetters () =
        Setters.Add ("Id", fun _ _ world -> (false, world))
        Setters.Add ("Name", fun _ _ world -> (false, world))
        Setters.Add ("Dispatcher", fun _ _ world -> (false, world))
        Setters.Add ("Persistent", fun property screen world -> (true, World.setScreenPersistent (property.PropertyValue :?> bool) screen world))
        Setters.Add ("CreationTimeStamp", fun _ _ world -> (false, world))
        Setters.Add ("ScriptOpt", fun property screen world -> (true, World.setScreenScriptOpt (property.PropertyValue :?> Symbol AssetTag option) screen world))
        Setters.Add ("Script", fun property screen world -> (true, World.setScreenScript (property.PropertyValue :?> Scripting.Expr array) screen world))
        Setters.Add ("ScriptFrame", fun _ _ world -> (false, world))
        Setters.Add ("ScriptUnsubscriptions", fun property screen world -> (true, World.setScreenScriptUnsubscriptions (property.PropertyValue :?> Unsubscription list) screen world))
        Setters.Add ("OnRegister", fun property screen world -> (true, World.setScreenOnRegister (property.PropertyValue :?> Scripting.Expr) screen world))
        Setters.Add ("OnUnregister", fun property screen world -> (true, World.setScreenOnUnregister (property.PropertyValue :?> Scripting.Expr) screen world))
        Setters.Add ("OnUpdate", fun property screen world -> (true, World.setScreenOnUpdate (property.PropertyValue :?> Scripting.Expr) screen world))
        Setters.Add ("OnPostUpdate", fun property screen world -> (true, World.setScreenOnPostUpdate (property.PropertyValue :?> Scripting.Expr) screen world))
        Setters.Add ("OnSignal", fun property screen world -> (true, World.setScreenOnSignal (property.PropertyValue :?> Scripting.Expr) screen world))
        Setters.Add ("TransitionState", fun property screen world -> (true, World.setScreenTransitionState (property.PropertyValue :?> TransitionState) screen world))
        Setters.Add ("TransitionTicks", fun property screen world -> (true, World.setScreenTransitionTicks (property.PropertyValue :?> int64) screen world))
        Setters.Add ("Incoming", fun property screen world -> (true, World.setScreenIncoming (property.PropertyValue :?> Transition) screen world))
        Setters.Add ("Outgoing", fun property screen world -> (true, World.setScreenOutgoing (property.PropertyValue :?> Transition) screen world))

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()