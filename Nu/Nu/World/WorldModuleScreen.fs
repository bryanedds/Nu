// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<AutoOpen>]
module WorldModuleScreen =

    /// Dynamic property getters / setters.
    let private ScreenGetters = Dictionary<string, Screen -> World -> Property> StringComparer.Ordinal
    let private ScreenSetters = Dictionary<string, Property -> Screen -> World -> struct (bool * World)> StringComparer.Ordinal

    type World with

        static member private screenStateFinder (screen : Screen) world =
            UMap.tryFind screen world.ScreenStates

        static member private screenStateAdder screenState (screen : Screen) world =
            let simulants =
                match world.Simulants.TryGetValue (Game.Handle :> Simulant) with
                | (true, screensOpt) ->
                    match screensOpt with
                    | Some screens ->
                        let screens = USet.add (screen :> Simulant) screens
                        UMap.add (Game.Handle :> Simulant) (Some screens) world.Simulants
                    | None ->
                        let screens = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) (screen :> Simulant)
                        UMap.add (Game.Handle :> Simulant) (Some screens) world.Simulants
                | (false, _) -> failwith ("Cannot add screen '" + scstring screen + "' to non-existent game.")
            let simulants =
                if not (UMap.containsKey (screen :> Simulant) simulants)
                then UMap.add (screen :> Simulant) None simulants
                else simulants
            let screenStates = UMap.add screen screenState world.ScreenStates
            World.choose { world with Simulants = simulants; ScreenStates = screenStates }

        static member private screenStateRemover (screen : Screen) world =
            let simulants =
                match world.Simulants.TryGetValue (Game.Handle :> Simulant) with
                | (true, screensOpt) ->
                    match screensOpt with
                    | Some screens ->
                        let screens = USet.remove (screen :> Simulant) screens
                        if USet.isEmpty screens
                        then UMap.add (Game.Handle :> Simulant) None world.Simulants
                        else UMap.add (Game.Handle :> Simulant) (Some screens) world.Simulants
                    | None -> world.Simulants
                | (false, _) -> world.Simulants
            let simulants = UMap.remove (screen :> Simulant) simulants
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

        static member internal publishScreenChange (propertyName : string) (propertyPrevious : obj) (propertyValue : obj) (screen : Screen) world =
            let changeData = { Name = propertyName; Previous = propertyPrevious; Value = propertyValue }
            let changeEventAddress = rtoa<ChangeData> [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName; screen.Names.[0]; screen.Names.[1]|]
            let eventTrace = EventTrace.debug "World" "publishScreenChange" "" EventTrace.empty
            World.publishPlus changeData changeEventAddress eventTrace screen false false world

        static member internal getScreenStateOpt screen world =
             World.screenStateFinder screen world

        static member internal getScreenState screen world =
            match World.getScreenStateOpt screen world with
            | Some screenState -> screenState
            | None -> failwith ("Could not find screen with '" + scstring screen + "'.")

        static member internal setScreenState screenState screen world =
            World.screenStateSetter screenState screen world

        /// Check that a screen exists in the world.
        static member internal getScreenExists screen world =
            Option.isSome (World.getScreenStateOpt screen world)

        static member internal getScreenDispatcher screen world = (World.getScreenState screen world).Dispatcher
        static member internal getScreenModelProperty screen world = (World.getScreenState screen world).Model
        static member internal getScreenContent screen world = (World.getScreenState screen world).Content
        static member internal getScreenTransitionState screen world = (World.getScreenState screen world).TransitionState
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal getScreenSlideOpt screen world = (World.getScreenState screen world).SlideOpt
        static member internal getScreenNav3d screen world = (World.getScreenState screen world).Nav3d
        static member internal getScreenProtected screen world = (World.getScreenState screen world).Protected
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal getScreenDestroying (screen : Screen) world = List.exists ((=) (screen :> Simulant)) (World.getDestructionListRev world)
        static member internal getScreenOrder screen world = (World.getScreenState screen world).Order
        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name

        static member internal setScreenModelProperty initializing (value : DesignerProperty) screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                let struct (screenState, world) =
                    let screenState = { screenState with Model = { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }}
                    struct (screenState, World.setScreenState screenState screen world)
                let world = screenState.Dispatcher.TrySynchronize (initializing, screen, world)
                let world = World.publishScreenChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue screen world
                struct (true, world)
            else struct (false, world)

        static member internal getScreenModel<'a> screen world =
            let screenState = World.getScreenState screen world
            match screenState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                try let model = modelObj |> valueToSymbol |> symbolToValue
                    screenState.Model.DesignerValue <- model
                    model
                with _ ->
                    Log.debugOnce "Could not convert existing screen model to new type. Falling back on initial model value."
                    match screenState.Dispatcher.TryGetInitialModel<'a> world with
                    | None -> failwithnie ()
                    | Some value -> value

        static member internal setScreenModel<'a> initializing (value : 'a) screen world =
            let screenState = World.getScreenState screen world
            let valueObj = value :> obj
            let previous = screenState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                let struct (screenState, world) =
                    let screenState = { screenState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    struct (screenState, World.setScreenState screenState screen world)
                let world = screenState.Dispatcher.TrySynchronize (initializing, screen, world)
                let world = World.publishScreenChange Constants.Engine.ModelPropertyName previous.DesignerValue value screen world
                struct (true, world)
            else struct (false, world)

        static member internal setScreenContent (value : ScreenContent) screen world =
            let screenState = World.getScreenState screen world
            let screenState = { screenState with Content = value }
            World.setScreenState screenState screen world

        static member internal setScreenTransitionState value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.TransitionState
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with TransitionState = value } screen |> World.publishScreenChange (nameof screenState.TransitionState) previous value screen)
            else struct (false, world)

        static member internal setScreenIncoming value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Incoming
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with Incoming = value } screen |> World.publishScreenChange (nameof screenState.Incoming) previous value screen)
            else struct (false, world)

        static member internal setScreenOutgoing value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Outgoing
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with Outgoing = value } screen |> World.publishScreenChange (nameof screenState.Outgoing) previous value screen)
            else struct (false, world)

        static member internal setScreenSlideOpt value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.SlideOpt
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with SlideOpt = value } screen |> World.publishScreenChange (nameof screenState.SlideOpt) previous value screen)
            else struct (false, world)

        static member internal setScreenNav3d value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Nav3d
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with Nav3d = value } screen |> World.publishScreenChange (nameof screenState.Nav3d) previous value screen)
            else struct (false, world)

        static member internal setScreenProtected value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Protected
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with Protected = value } screen |> World.publishScreenChange (nameof screenState.Protected) previous value screen)
            else struct (false, world)

        static member internal setScreenPersistent value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.Persistent
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with Persistent = value } screen |> World.publishScreenChange (nameof screenState.Persistent) previous value screen)
            else struct (false, world)

        static member internal tryGetScreenXtensionProperty (propertyName, screen, world, property : _ outref) =
            if World.getScreenExists screen world
            then ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property)
            else false

        static member internal tryGetScreenXtensionValue<'a> propertyName screen world =
            let screenStateOpt = World.getScreenStateOpt screen world
            match screenStateOpt :> obj with
            | null -> failwithf "Could not find screen '%s'." (scstring screen)
            | _ ->
                let mutable property = Unchecked.defaultof<Property>
                if World.tryGetScreenProperty (propertyName, screen, world, &property) then
                    match property.PropertyValue with
                    | :? 'a as value -> value
                    | null -> null :> obj :?> 'a
                    | valueObj -> valueObj |> valueToSymbol |> symbolToValue
                else Unchecked.defaultof<'a>

        static member internal getScreenXtensionProperty propertyName screen world =
            let mutable property = Unchecked.defaultof<_>
            match ScreenState.tryGetProperty (propertyName, World.getScreenState screen world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal getScreenXtensionValue<'a> propertyName screen world =
            let screenState = World.getScreenState screen world
            let property = ScreenState.getProperty propertyName screenState
            match property.PropertyValue with
            | :? 'a as value -> value
            | null -> null :> obj :?> 'a
            | valueObj -> valueObj |> valueToSymbol |> symbolToValue

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

        static member internal trySetScreenXtensionPropertyFast propertyName (property : Property) screen world =
            let screenState = World.getScreenState screen world
            match ScreenState.tryGetProperty (propertyName, screenState) with
            | (true, propertyOld) ->
                if property.PropertyValue =/= propertyOld.PropertyValue then
                    let struct (success, screenState) = ScreenState.trySetProperty propertyName property screenState
                    let world = World.setScreenState screenState screen world
                    if success then World.publishScreenChange propertyName propertyOld.PropertyValue property.PropertyValue screen world else world
                else world
            | (false, _) -> world

        static member internal trySetScreenXtensionProperty propertyName (property : Property) screen world =
            let screenState = World.getScreenState screen world
            match ScreenState.tryGetProperty (propertyName, screenState) with
            | (true, propertyOld) ->
                if property.PropertyValue =/= propertyOld.PropertyValue then
                    let struct (success, screenState) = ScreenState.trySetProperty propertyName property screenState
                    let world = World.setScreenState screenState screen world
                    if success
                    then struct (success, true, World.publishScreenChange propertyName propertyOld.PropertyValue property.PropertyValue screen world)
                    else struct (false, true, world)
                else struct (false, false, world)
            | (false, _) -> struct (false, false, world)

        static member internal setScreenXtensionProperty propertyName (property : Property) screen world =
            let screenState = World.getScreenState screen world
            let propertyOld = ScreenState.getProperty propertyName screenState
            if property.PropertyValue =/= propertyOld.PropertyValue then
                let screenState = ScreenState.setProperty propertyName property screenState
                let world = World.setScreenState screenState screen world
                struct (true, World.publishScreenChange propertyName propertyOld.PropertyValue property.PropertyValue screen world)
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
                let screenState = World.getScreenState screen world
                let screenState = ScreenState.attachProperty propertyName property screenState
                let world = World.setScreenState screenState screen world
                World.publishScreenChange propertyName property.PropertyValue property.PropertyValue screen world
            else failwith ("Cannot attach screen property '" + propertyName + "'; screen '" + scstring screen + "' is not found.")

        static member internal detachScreenProperty propertyName screen world =
            if World.getScreenExists screen world then
                let screenState = World.getScreenState screen world
                let screenState = ScreenState.detachProperty propertyName screenState
                World.setScreenState screenState screen world
            else failwith ("Cannot detach screen property '" + propertyName + "'; screen '" + scstring screen + "' is not found.")

        static member internal registerScreen screen world =
            let dispatcher = World.getScreenDispatcher screen world
            let world = dispatcher.Register (screen, world)
            let eventTrace = EventTrace.debug "World" "registerScreen" "" EventTrace.empty
            let world = World.publishPlus () (Events.RegisterEvent --> screen) eventTrace screen true false world
            let eventTrace = EventTrace.debug "World" "registerScreen" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData screen) (Events.LifeCycleEvent (nameof Screen) --> Nu.Game.Handle) eventTrace screen true false world

        static member internal unregisterScreen screen world =
            let dispatcher = World.getScreenDispatcher screen world
            let eventTrace = EventTrace.debug "World" "registerScreen" "LifeCycle" EventTrace.empty
            let world = World.publishPlus (UnregisteringData screen) (Events.LifeCycleEvent (nameof Screen) --> Nu.Game.Handle) eventTrace screen true false world
            let eventTrace = EventTrace.debug "World" "unregisteringScreen" "" EventTrace.empty
            let world = World.publishPlus () (Events.UnregisteringEvent --> screen) eventTrace screen true false world
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

        static member internal viewScreenProperties screen world =
            let state = World.getScreenState screen world
            World.viewSimulantStateProperties state

    /// Initialize property getters.
    let private initGetters () =
        ScreenGetters.Add ("Dispatcher", fun screen world -> { PropertyType = typeof<ScreenDispatcher>; PropertyValue = World.getScreenDispatcher screen world })
        ScreenGetters.Add ("Model", fun screen world -> let designerProperty = World.getScreenModelProperty screen world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        ScreenGetters.Add ("TransitionState", fun screen world -> { PropertyType = typeof<TransitionState>; PropertyValue = World.getScreenTransitionState screen world })
        ScreenGetters.Add ("Incoming", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenIncoming screen world })
        ScreenGetters.Add ("Outgoing", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenOutgoing screen world })
        ScreenGetters.Add ("SlideOpt", fun screen world -> { PropertyType = typeof<Slide option>; PropertyValue = World.getScreenSlideOpt screen world })
        ScreenGetters.Add ("Nav3d", fun screen world -> { PropertyType = typeof<Nav3d>; PropertyValue = World.getScreenNav3d screen world })
        ScreenGetters.Add ("Protected", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenProtected screen world })
        ScreenGetters.Add ("Persistent", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenPersistent screen world })
        ScreenGetters.Add ("Order", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenOrder screen world })
        ScreenGetters.Add ("Id", fun screen world -> { PropertyType = typeof<Guid>; PropertyValue = World.getScreenId screen world })
        ScreenGetters.Add ("Name", fun screen world -> { PropertyType = typeof<string>; PropertyValue = World.getScreenName screen world })

    /// Initialize property setters.
    let private initSetters () =
        ScreenSetters.Add ("Model", fun property screen world -> World.setScreenModelProperty false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } screen world)
        ScreenSetters.Add ("TransitionState", fun property screen world -> World.setScreenTransitionState (property.PropertyValue :?> TransitionState) screen world)
        ScreenSetters.Add ("Incoming", fun property screen world -> World.setScreenIncoming (property.PropertyValue :?> Transition) screen world)
        ScreenSetters.Add ("Outgoing", fun property screen world -> World.setScreenOutgoing (property.PropertyValue :?> Transition) screen world)
        ScreenSetters.Add ("SlideOpt", fun property screen world -> World.setScreenSlideOpt (property.PropertyValue :?> Slide option) screen world)
        ScreenSetters.Add ("Persistent", fun property screen world -> World.setScreenPersistent (property.PropertyValue :?> bool) screen world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()