// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleScreen =

    /// Dynamic property getters / setters.
    let internal ScreenGetters = Dictionary<string, Screen -> World -> Property> StringComparer.Ordinal
    let internal ScreenSetters = Dictionary<string, Property -> Screen -> World -> struct (bool * World)> StringComparer.Ordinal

    type World with
    
        static member private screenStateFinder (screen : Screen) world =
            UMap.tryFind screen world.ScreenStates

        static member private screenStateAdder screenState (screen : Screen) world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [|screenName|] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some groupDirectory ->
                        // NOTE: this is logically a redundant operation...
                        let groupDirectory = KeyValuePair (screen, groupDirectory.Value)
                        UMap.add screenName groupDirectory world.ScreenDirectory
                    | None ->
                        let config = World.getCollectionConfig world
                        let groupDirectory = KeyValuePair (screen, UMap.makeEmpty StringComparer.Ordinal config)
                        UMap.add screenName groupDirectory world.ScreenDirectory
                | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            let screenStates = UMap.add screen screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover (screen : Screen) world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [|screenName|] -> UMap.remove screenName world.ScreenDirectory
                | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            let screenStates = UMap.remove screen world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateSetter screenState (screen : Screen) world =
#if DEBUG
            if not (UMap.containsKey screen world.ScreenStates) then
                failwith ("Cannot set the state of a non-existent screen '" + scstring screen + "'")
#endif
            let screenStates = UMap.add screen screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member private removeScreenState screen world =
            World.screenStateRemover screen world

        static member private publishScreenChange (propertyName : string) (propertyValue : obj) (screen : Screen) world =

            // publish change binding
            let world =
                World.publishChangeBinding propertyName screen world

            // publish change event
            let world =
                let changeData = { Name = propertyName; Value = propertyValue }
                let changeEventAddress = rtoa<ChangeData> [|"Change"; propertyName; "Event"; screen.Name|]
                let eventTrace = EventTrace.debug "World" "publishScreenChange" "" EventTrace.empty
                World.publishPlus changeData changeEventAddress eventTrace screen false false world

            // fin
            world

        static member private getScreenStateOpt screen world =
             World.screenStateFinder screen world

        static member internal getScreenState screen world =
            match World.getScreenStateOpt screen world with
            | Some screenState -> screenState
            | None -> failwith ("Could not find screen with '" + scstring screen + "'.")

        static member internal setScreenState screenState screen world =
            World.screenStateSetter screenState screen world

        static member private updateScreenStateWithoutEvent updater screen world =
            let screenStateOpt = updater (World.getScreenState screen world)
            match screenStateOpt :> obj with
            | null -> struct (false, world)
            | _ -> struct (true, World.setScreenState screenStateOpt screen world)

        static member private updateScreenState updater propertyName propertyValue screen world =
            let struct (changed, world) = World.updateScreenStateWithoutEvent updater screen world
            let world =
                if changed
                then World.publishScreenChange propertyName propertyValue screen world
                else world
            struct (changed, world)

        /// Check that a screen exists in the world.
        static member internal getScreenExists screen world =
            Option.isSome (World.getScreenStateOpt screen world)

        static member internal getScreenDispatcher screen world = (World.getScreenState screen world).Dispatcher
        static member internal getScreenModelProperty screen world = (World.getScreenState screen world).Model
        static member internal getScreenModel<'a> screen world = (World.getScreenState screen world).Model.DesignerValue :?> 'a
        static member internal getScreenEcs screen world = (World.getScreenState screen world).Ecs
        static member internal getScreenTransitionState screen world = (World.getScreenState screen world).TransitionState
        static member internal setScreenTransitionState value screen world = World.updateScreenState (fun screenState -> if value <> screenState.TransitionState then { screenState with TransitionState = value } else Unchecked.defaultof<_>) Property? TransitionState value screen world
        static member internal getScreenTransitionUpdates screen world = (World.getScreenState screen world).TransitionUpdates
        static member internal setScreenTransitionUpdates value screen world = World.updateScreenState (fun screenState -> if value <> screenState.TransitionUpdates then { screenState with TransitionUpdates = value } else Unchecked.defaultof<_>) Property? TransitionUpdates value screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) Property? Incoming value screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) Property? Outgoing value screen world
        static member internal getScreenSplashOpt screen world = (World.getScreenState screen world).SplashOpt
        static member internal setScreenSplashOpt value screen world = World.updateScreenState (fun screenState -> { screenState with SplashOpt = value }) Property? SplashOpt value screen world
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> if value <> screenState.Persistent then { screenState with Persistent = value } else Unchecked.defaultof<_>) Property? Persistent value screen world
        static member internal getScreenDestroying (screen : Screen) world = List.exists ((=) (screen :> Simulant)) world.WorldExtension.DestructionListRev
        static member internal getScreenCreationTimeStamp screen world = (World.getScreenState screen world).CreationTimeStamp
        static member internal getScreenScriptFrame screen world = (World.getScreenState screen world).ScriptFrame
        static member internal setScreenScriptFrame value screen world = World.updateScreenState (fun screenState -> if value <> screenState.ScriptFrame then { screenState with ScriptFrame = value } else Unchecked.defaultof<_>) Property? ScriptFrame value screen world
        static member internal getScreenName screen world = (World.getScreenState screen world).Name
        static member internal getScreenId screen world = (World.getScreenState screen world).Id

        static member internal setScreenModelProperty (value : DesignerProperty) screen world =
            World.updateScreenState
                (fun screenState ->
                    if value.DesignerValue =/= screenState.Model.DesignerValue
                    then { screenState with Model = { screenState.Model with DesignerValue = value.DesignerValue }}
                    else Unchecked.defaultof<_>)
                Property? Model value.DesignerValue screen world

        static member internal setScreenModel<'a> (value : 'a) screen world =
            World.updateScreenState
                (fun screenState ->
                    let valueObj = value :> obj
                    if valueObj =/= screenState.Model.DesignerValue
                    then { screenState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    else Unchecked.defaultof<_>)
                Property? Model value screen world

        static member internal tryGetScreenXtensionProperty (propertyName, screen, world, property : _ outref) =
            if World.getScreenExists screen world
            then ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property)
            else false

        static member internal getScreenXtensionProperty propertyName screen world =
            let mutable property = Unchecked.defaultof<_>
            match ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal tryGetScreenProperty (propertyName, screen, world, property : _ outref) =
            match ScreenGetters.TryGetValue propertyName with
            | (true, getter) ->
                if World.getScreenExists screen world then
                    property <- getter screen world
                    true
                else false
            | (false, _) ->
                World.tryGetScreenXtensionProperty (propertyName, screen, world, &property)

        static member internal getScreenProperty propertyName screen world =
            match ScreenGetters.TryGetValue propertyName with
            | (true, getter) -> getter screen world
            | (false, _) -> World.getScreenXtensionProperty propertyName screen world

        static member internal trySetScreenXtensionPropertyFast propertyName property screen world =
            if World.getScreenExists screen world then
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let struct (_, world) =
                    World.updateScreenState
                        (fun screenState ->
                            let mutable propertyOld = Unchecked.defaultof<_>
                            match ScreenState.tryGetProperty (propertyName, screenState, &propertyOld) with
                            | true ->
                                if property.PropertyValue =/= propertyOld.PropertyValue then
                                    let struct (successInner, gameState) = ScreenState.trySetProperty propertyName property screenState
                                    success <- successInner
                                    gameState
                                else Unchecked.defaultof<_>
                            | false -> Unchecked.defaultof<_>)
                        propertyName property.PropertyValue screen world
                world
            else world

        static member internal trySetScreenXtensionProperty propertyName property screen world =
            if World.getScreenExists screen world then
                let mutable success = false // bit of a hack to get additional state out of the lambda
                let struct (changed, world) =
                    World.updateScreenState
                        (fun screenState ->
                            let mutable propertyOld = Unchecked.defaultof<_>
                            match ScreenState.tryGetProperty (propertyName, screenState, &propertyOld) with
                            | true ->
                                if property.PropertyValue =/= propertyOld.PropertyValue then
                                    let struct (successInner, gameState) = ScreenState.trySetProperty propertyName property screenState
                                    success <- successInner
                                    gameState
                                else Unchecked.defaultof<_>
                            | false -> Unchecked.defaultof<_>)
                        propertyName property.PropertyValue screen world
                struct (success, changed, world)
            else (false, false, world)

        static member internal setScreenXtensionProperty propertyName property screen world =
            if World.getScreenExists screen world then
                World.updateScreenState
                    (fun screenState ->
                        let propertyOld = ScreenState.getProperty propertyName screenState
                        if property.PropertyValue =/= propertyOld.PropertyValue
                        then ScreenState.setProperty propertyName property screenState
                        else Unchecked.defaultof<_>)
                    propertyName property.PropertyValue screen world
            else struct (false, world)

        static member internal trySetScreenPropertyFast propertyName property screen world =
            match ScreenSetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getScreenExists screen world
                then setter property screen world |> snd'
                else world
            | (false, _) ->
                World.trySetScreenXtensionPropertyFast propertyName property screen world

        static member internal trySetScreenProperty propertyName property screen world =
            match ScreenSetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getScreenExists screen world then
                    let struct (changed, world) = setter property screen world
                    struct (true, changed, world)
                else (false, false, world)
            | (false, _) ->
                World.trySetScreenXtensionProperty propertyName property screen world

        static member internal setScreenProperty propertyName property screen world =
            match ScreenSetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getScreenExists screen world
                then setter property screen world
                else struct (false, world)
            | (false, _) ->
                World.setScreenXtensionProperty propertyName property screen world

        static member internal attachScreenProperty propertyName property screen world =
            if World.getScreenExists screen world then
                let struct (_, world) =
                    World.updateScreenState
                        (fun screenState -> ScreenState.attachProperty propertyName property screenState)
                        propertyName property.PropertyValue screen world
                world
            else failwith ("Cannot attach screen property '" + propertyName + "'; screen '" + screen.Name + "' is not found.")

        static member internal detachScreenProperty propertyName screen world =
            if World.getScreenExists screen world then
                let struct (_, world) =
                    World.updateScreenStateWithoutEvent
                        (fun screenState -> ScreenState.detachProperty propertyName screenState)
                        screen world
                world
            else failwith ("Cannot detach screen property '" + propertyName + "'; screen '" + screen.Name + "' is not found.")

        static member internal registerScreen screen world =
            let dispatcher = World.getScreenDispatcher screen world
            let world = dispatcher.Register (screen, world)
            let eventTrace = EventTrace.debug "World" "registerScreen" "" EventTrace.empty
            World.publishPlus () (rtoa<unit> [|"Register"; "Event"; screen.Name|]) eventTrace screen true false world

        static member internal unregisterScreen screen world =
            let dispatcher = World.getScreenDispatcher screen world
            let eventTrace = EventTrace.debug "World" "unregisteringScreen" "" EventTrace.empty
            let world = World.publishPlus () (rtoa<unit> [|"Unregistering"; "Event"; screen.Name|]) eventTrace screen true false world
            dispatcher.Unregister (screen, world)

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not (World.getScreenExists screen world)
            if isNew || mayReplace then
                let world = World.addScreenState screenState screen world
                if isNew then World.registerScreen screen world else world
            else failwith ("Adding a screen that the world already contains '" + scstring screen + "'.")

        static member internal removeScreen3 removeGroups screen world =
            if World.getScreenExists screen world then
                let world = World.unregisterScreen screen world
                let world = removeGroups screen world
                World.removeScreenState screen world
            else world

        static member internal writeScreen4 writeGroups screen screenDescriptor world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.Dispatcher
            let screenDescriptor = { screenDescriptor with ScreenDispatcherName = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            writeGroups screen screenDescriptor world

        static member internal readScreen4 readGroups screenDescriptor nameOpt world =

            // make the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcherName
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ("Could not find ScreenDispatcher '" + dispatcherName + "'.")
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName dispatchers

            // make the ecs
            let ecs = world.WorldExtension.Plugin.MakeEcs ()

            // make the screen state and populate its properties
            let screenState = ScreenState.make None dispatcher ecs
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
            
            // read the screen's groups
            let world = readGroups screenDescriptor screen world |> snd
            (screen, world)

        /// View all of the properties of a screen.
        static member internal viewScreenProperties screen world =
            let state = World.getScreenState screen world
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c

    /// Initialize property getters.
    let private initGetters () =
        ScreenGetters.Add ("Dispatcher", fun screen world -> { PropertyType = typeof<ScreenDispatcher>; PropertyValue = World.getScreenDispatcher screen world })
        ScreenGetters.Add ("Model", fun screen world -> let designerProperty = World.getScreenModelProperty screen world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        ScreenGetters.Add ("Ecs", fun screen world -> { PropertyType = typeof<World Ecs>; PropertyValue = World.getScreenEcs screen world })
        ScreenGetters.Add ("TransitionState", fun screen world -> { PropertyType = typeof<TransitionState>; PropertyValue = World.getScreenTransitionState screen world })
        ScreenGetters.Add ("TransitionUpdates", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenTransitionUpdates screen world })
        ScreenGetters.Add ("Incoming", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenIncoming screen world })
        ScreenGetters.Add ("Outgoing", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenOutgoing screen world })
        ScreenGetters.Add ("SplashOpt", fun screen world -> { PropertyType = typeof<Splash option>; PropertyValue = World.getScreenSplashOpt screen world })
        ScreenGetters.Add ("Persistent", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenPersistent screen world })
        ScreenGetters.Add ("ScriptFrame", fun screen world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getScreenScriptFrame screen world })
        ScreenGetters.Add ("CreationTimeStamp", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenCreationTimeStamp screen world })
        ScreenGetters.Add ("Name", fun screen world -> { PropertyType = typeof<string>; PropertyValue = World.getScreenName screen world })
        ScreenGetters.Add ("Id", fun screen world -> { PropertyType = typeof<Guid>; PropertyValue = World.getScreenId screen world })

    /// Initialize property setters.
    let private initSetters () =
        ScreenSetters.Add ("Model", fun property screen world -> World.setScreenModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } screen world)
        ScreenSetters.Add ("TransitionState", fun property screen world -> World.setScreenTransitionState (property.PropertyValue :?> TransitionState) screen world)
        ScreenSetters.Add ("TransitionUpdates", fun property screen world -> World.setScreenTransitionUpdates (property.PropertyValue :?> int64) screen world)
        ScreenSetters.Add ("Incoming", fun property screen world -> World.setScreenIncoming (property.PropertyValue :?> Transition) screen world)
        ScreenSetters.Add ("Outgoing", fun property screen world -> World.setScreenOutgoing (property.PropertyValue :?> Transition) screen world)
        ScreenSetters.Add ("SplashOpt", fun property screen world -> World.setScreenSplashOpt (property.PropertyValue :?> Splash option) screen world)
        ScreenSetters.Add ("Persistent", fun property screen world -> World.setScreenPersistent (property.PropertyValue :?> bool) screen world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()