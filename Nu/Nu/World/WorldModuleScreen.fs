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
            let game = Simulants.Game
            let simulants =
                match world.Simulants.TryGetValue (game :> Simulant) with
                | (true, screensOpt) ->
                    match screensOpt with
                    | Some screens ->
                        let screens = USet.add (screen :> Simulant) screens
                        UMap.add (game :> Simulant) (Some screens) world.Simulants
                    | None ->
                        let screens = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) (screen :> Simulant)
                        UMap.add (game :> Simulant) (Some screens) world.Simulants
                | (false, _) -> failwith ("Cannot add screen '" + scstring screen + "' to non-existent screen.")
            let simulants =
                if not (UMap.containsKey (screen :> Simulant) simulants)
                then UMap.add (screen :> Simulant) None simulants
                else simulants
            let screenStates = UMap.add screen screenState world.ScreenStates
            World.choose { world with Simulants = simulants; ScreenStates = screenStates }
        
        static member private screenStateRemover (screen : Screen) world =
            let game = Simulants.Game
            let simulants =
                match world.Simulants.TryGetValue (game :> Simulant) with
                | (true, screensOpt) ->
                    match screensOpt with
                    | Some screens ->
                        let screens = USet.remove (game :> Simulant) screens
                        if USet.isEmpty screens
                        then UMap.add (screen :> Simulant) None world.Simulants
                        else UMap.add (screen :> Simulant) (Some screens) world.Simulants
                    | None -> world.Simulants
                | (false, _) -> world.Simulants
            let simulants = UMap.remove (game :> Simulant) simulants
            let screenStates = UMap.remove screen world.ScreenStates
            World.choose { world with Simulants = simulants; ScreenStates = screenStates }

        static member private screenStateSetter screenState (screen : Screen) world =
#if DEBUG
            if not (UMap.containsKey screen world.ScreenStates) then
                failwith ("Cannot set the state of a non-existent screen '" + scstring screen + "'")
#endif
            let screenStates = UMap.add screen screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member internal removeScreenState screen world =
            World.screenStateRemover screen world

        static member internal publishScreenChange (propertyName : string) (propertyValue : obj) (screen : Screen) world =

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
        static member internal setScreenTransitionState value screen world = World.updateScreenState (fun screenState -> if value <> screenState.TransitionState then { screenState with TransitionState = value } else Unchecked.defaultof<_>) "TransitionState" value screen world
        static member internal getScreenTransitionUpdates screen world = (World.getScreenState screen world).TransitionUpdates
        static member internal setScreenTransitionUpdates value screen world = World.updateScreenState (fun screenState -> if value <> screenState.TransitionUpdates then { screenState with TransitionUpdates = value } else Unchecked.defaultof<_>) "TransitionUpdates" value screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) "Incoming" value screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) "Outgoing" value screen world
        static member internal getScreenSplashOpt screen world = (World.getScreenState screen world).SplashOpt
        static member internal setScreenSplashOpt value screen world = World.updateScreenState (fun screenState -> { screenState with SplashOpt = value }) "SplashOpt" value screen world
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> if value <> screenState.Persistent then { screenState with Persistent = value } else Unchecked.defaultof<_>) "Persistent" value screen world
        static member internal getScreenDestroying (screen : Screen) world = List.exists ((=) (screen :> Simulant)) world.WorldExtension.DestructionListRev
        static member internal getScreenOrder screen world = (World.getScreenState screen world).Order
        static member internal getScreenScriptFrame screen world = (World.getScreenState screen world).ScriptFrame
        static member internal setScreenScriptFrame value screen world = World.updateScreenState (fun screenState -> if value <> screenState.ScriptFrame then { screenState with ScriptFrame = value } else Unchecked.defaultof<_>) "ScriptFrame" value screen world
        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name

        static member internal setScreenModelProperty (value : DesignerProperty) screen world =
            World.updateScreenState
                (fun screenState ->
                    if value.DesignerValue =/= screenState.Model.DesignerValue
                    then { screenState with Model = { screenState.Model with DesignerValue = value.DesignerValue }}
                    else Unchecked.defaultof<_>)
                "Model" value.DesignerValue screen world

        static member internal setScreenModel<'a> (value : 'a) screen world =
            World.updateScreenState
                (fun screenState ->
                    let valueObj = value :> obj
                    if valueObj =/= screenState.Model.DesignerValue
                    then { screenState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    else Unchecked.defaultof<_>)
                "Model" value screen world

        static member internal tryGetScreenXtensionProperty (propertyName, screen, world, property : _ outref) =
            if World.getScreenExists screen world
            then ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property)
            else false

        static member internal getScreenXtensionProperty propertyName screen world =
            let mutable property = Unchecked.defaultof<_>
            match ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal getScreenXtensionValue<'a> propertyName screen world =
            let screenState = World.getScreenState screen world
            let property = ScreenState.getProperty propertyName screenState
            property.PropertyValue :?> 'a

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
                                    let struct (successInner, screenState) = ScreenState.trySetProperty propertyName property screenState
                                    success <- successInner
                                    screenState
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
                                    let struct (successInner, screenState) = ScreenState.trySetProperty propertyName property screenState
                                    success <- successInner
                                    screenState
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
            let world = World.publishPlus () (Events.Register --> screen) eventTrace screen true false world
            let eventTrace = EventTrace.debug "World" "registerScreen" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData screen) (Events.LifeCycle (nameof Screen)) eventTrace screen true false world

        static member internal unregisterScreen screen world =
            let dispatcher = World.getScreenDispatcher screen world
            let eventTrace = EventTrace.debug "World" "registerScreen" "LifeCycle" EventTrace.empty
            let world = World.publishPlus (UnregisteringData screen) (Events.LifeCycle (nameof Screen)) eventTrace screen true false world
            let eventTrace = EventTrace.debug "World" "unregisteringScreen" "" EventTrace.empty
            let world = World.publishPlus () (Events.Unregistering --> screen) eventTrace screen true false world
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

        /// View all of the properties of a screen.
        static member viewScreenProperties screen world =
            let state = World.getScreenState screen world
            World.viewProperties state

    /// Initialize property getters.
    let private initGetters () =
        ScreenGetters.Add ("Dispatcher", fun screen world -> { PropertyType = typeof<ScreenDispatcher>; PropertyValue = World.getScreenDispatcher screen world })
        ScreenGetters.Add ("Model", fun screen world -> let designerProperty = World.getScreenModelProperty screen world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        ScreenGetters.Add ("Ecs", fun screen world -> { PropertyType = typeof<Ecs.Ecs>; PropertyValue = World.getScreenEcs screen world })
        ScreenGetters.Add ("TransitionState", fun screen world -> { PropertyType = typeof<TransitionState>; PropertyValue = World.getScreenTransitionState screen world })
        ScreenGetters.Add ("TransitionUpdates", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenTransitionUpdates screen world })
        ScreenGetters.Add ("Incoming", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenIncoming screen world })
        ScreenGetters.Add ("Outgoing", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenOutgoing screen world })
        ScreenGetters.Add ("SplashOpt", fun screen world -> { PropertyType = typeof<Splash option>; PropertyValue = World.getScreenSplashOpt screen world })
        ScreenGetters.Add ("Persistent", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenPersistent screen world })
        ScreenGetters.Add ("ScriptFrame", fun screen world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getScreenScriptFrame screen world })
        ScreenGetters.Add ("Order", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenOrder screen world })
        ScreenGetters.Add ("Id", fun screen world -> { PropertyType = typeof<Guid>; PropertyValue = World.getScreenId screen world })
        ScreenGetters.Add ("Name", fun screen world -> { PropertyType = typeof<string>; PropertyValue = World.getScreenName screen world })

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