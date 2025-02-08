﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Frozen
open Prime

[<AutoOpen>]
module WorldModuleScreen =

    /// Dynamic property getter and setter.
    type private PropertyGetter = Screen -> World -> Property
    type private PropertySetter = Property -> Screen -> World -> struct (bool * World)

    /// Dynamic property getters / setters.
    let mutable private ScreenGetters = Unchecked.defaultof<FrozenDictionary<string, PropertyGetter>>
    let mutable private ScreenSetters = Unchecked.defaultof<FrozenDictionary<string, PropertySetter>>

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

        static member internal getScreenXtensionProperties screen world =
            let screenState = World.getScreenState screen world
            screenState.Xtension |> Xtension.toSeq |> Seq.toList

        static member internal getScreenExists screen world =
            Option.isSome (World.getScreenStateOpt screen world)

        static member internal getScreenSelected (screen : Screen) world =
            let gameState = World.getGameState Game.Handle world
            match gameState.SelectedScreenOpt with
            | Some selectedScreen when screen.Name = selectedScreen.Name -> true
            | _ -> false

        static member internal getScreenDispatcher screen world = (World.getScreenState screen world).Dispatcher
        static member internal getScreenModelProperty screen world = (World.getScreenState screen world).Model
        static member internal getScreenContent screen world = (World.getScreenState screen world).Content
        static member internal getScreenTransitionState screen world = (World.getScreenState screen world).TransitionState
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal getScreenRequestedSong screen world = (World.getScreenState screen world).RequestedSong
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

        static member internal getScreenModelGeneric<'a> screen world =
            let screenState = World.getScreenState screen world
            match screenState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                let modelSymbol = valueToSymbol modelObj
                try let model = symbolToValue modelSymbol
                    screenState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                    model
                with _ ->
                    Log.warn "Could not convert existing screen model value to new type; attempting to use fallback model value instead."
                    match screenState.Dispatcher.TryGetFallbackModel<'a> (modelSymbol, screen, world) with
                    | None -> typeof<'a>.GetDefaultValue () :?> 'a
                    | Some model ->
                        screenState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                        model

        static member internal setScreenModelGeneric<'a> initializing (value : 'a) screen world =
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

        static member internal setScreenRequestedSong value screen world =
            let screenState = World.getScreenState screen world
            let previous = screenState.RequestedSong
            if value <> previous
            then struct (true, world |> World.setScreenState { screenState with RequestedSong = value } screen |> World.publishScreenChange (nameof screenState.RequestedSong) previous value screen)
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
                let screenState = World.getScreenState screen world
                if ScreenState.tryGetProperty (propertyName, screenState, &property) then
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                    | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (screen :> obj) (world :> obj) }; true
                    | _ -> true
                else false

        static member internal getScreenXtensionValue<'a> propertyName screen world =
            let screenState = World.getScreenState screen world
            let mutable property = Unchecked.defaultof<_>
            if ScreenState.tryGetProperty (propertyName, screenState, &property) then
                let valueObj =
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue
                    | :? ComputedProperty as cp -> cp.ComputedGet screen world
                    | _ -> property.PropertyValue
                match valueObj with
                | :? 'a as value -> value
                | null -> null :> obj :?> 'a
                | value ->
                    let value' = value |> valueToSymbol |> symbolToValue
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerType <- typeof<'a>; dp.DesignerValue <- value'
                    | :? ComputedProperty -> () // nothing to do
                    | _ -> property.PropertyType <- typeof<'a>; property.PropertyValue <- value'
                    value'
            else
                let definitions = Reflection.getPropertyDefinitions (getType screenState.Dispatcher)
                let value =
                    match List.tryFind (fun (pd : PropertyDefinition) -> pd.PropertyName = propertyName) definitions with
                    | Some definition ->
                        match definition.PropertyExpr with
                        | DefineExpr value -> value :?> 'a
                        | VariableExpr eval -> eval world :?> 'a
                        | ComputedExpr property -> property.ComputedGet screen world :?> 'a
                    | None -> failwithumf ()
                let property = { PropertyType = typeof<'a>; PropertyValue = value }
                screenState.Xtension <- Xtension.attachProperty propertyName property screenState.Xtension
                value

        static member internal getScreenProperty propertyName screen world =
            match ScreenGetters.TryGetValue propertyName with
            | (true, getter) -> getter screen world
            | (false, _) -> World.getScreenXtensionProperty propertyName screen world

        static member internal trySetScreenXtensionPropertyWithoutEvent propertyName (property : Property) screenState screen world =
            let mutable propertyOld = Unchecked.defaultof<_>
            match ScreenState.tryGetProperty (propertyName, screenState, &propertyOld) with
            | true ->
                match propertyOld.PropertyValue with
                | :? DesignerProperty as dp ->
                    let previous = dp.DesignerValue
                    if property.PropertyValue =/= previous then
                        let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                        match ScreenState.trySetProperty propertyName property screenState with
                        | struct (true, screenState) -> struct (true, true, previous, World.setScreenState screenState screen world)
                        | struct (false, _) -> struct (false, false, previous, world)
                    else (true, false, previous, world)
                | :? ComputedProperty as cp ->
                    match cp.ComputedSetOpt with
                    | Some computedSet ->
                        let previous = cp.ComputedGet (box screen) (box world)
                        if property.PropertyValue =/= previous
                        then struct (true, true, previous, computedSet property.PropertyValue screen world :?> World)
                        else struct (true, false, previous, world)
                    | None -> struct (false, false, Unchecked.defaultof<_>, world)
                | _ ->
                    let previous = propertyOld.PropertyValue
                    if property.PropertyValue =/= previous then
                        match ScreenState.trySetProperty propertyName property screenState with
                        | struct (true, screenState) -> (true, true, previous, World.setScreenState screenState screen world)
                        | struct (false, _) -> struct (false, false, previous, world)
                    else struct (true, false, previous, world)
            | false -> struct (false, false, Unchecked.defaultof<_>, world)

        static member internal trySetScreenXtensionPropertyFast propertyName (property : Property) screen world =
            let screenState = World.getScreenState screen world
            match World.trySetScreenXtensionPropertyWithoutEvent propertyName property screenState screen world with
            | struct (true, changed, previous, world) ->
                if changed
                then World.publishScreenChange propertyName previous property.PropertyValue screen world
                else world
            | struct (false, _, _, world) -> world

        static member internal trySetScreenXtensionProperty propertyName (property : Property) screen world =
            let screenState = World.getScreenState screen world
            match World.trySetScreenXtensionPropertyWithoutEvent propertyName property screenState screen world with
            | struct (true, changed, previous, world) ->
                let world =
                    if changed
                    then World.publishScreenChange propertyName previous property.PropertyValue screen world
                    else world
                struct (true, changed, world)
            | struct (false, changed, _, world) -> struct (false, changed, world)

        static member internal setScreenXtensionValue<'a> propertyName (value : 'a) screen world =
            let screenState = World.getScreenState screen world
            let propertyOld = ScreenState.getProperty propertyName screenState
            let mutable previous = Unchecked.defaultof<obj> // OPTIMIZATION: avoid passing around structs.
            let mutable changed = false // OPTIMIZATION: avoid passing around structs.
            let world =
                match propertyOld.PropertyValue with
                | :? DesignerProperty as dp ->
                    previous <- dp.DesignerValue
                    if value =/= previous then
                        changed <- true
                        let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                        let screenState = ScreenState.setProperty propertyName property screenState
                        World.setScreenState screenState screen world
                    else world
                | :? ComputedProperty as cp ->
                    match cp.ComputedSetOpt with
                    | Some computedSet ->
                        previous <- cp.ComputedGet (box screen) (box world)
                        if value =/= previous then
                            changed <- true
                            computedSet propertyOld.PropertyValue screen world :?> World
                        else world
                    | None -> world
                | _ ->
                    previous <- propertyOld.PropertyValue
                    if value =/= previous then
                        changed <- true
                        let property = { propertyOld with PropertyValue = value }
                        let screenState = ScreenState.setProperty propertyName property screenState
                        World.setScreenState screenState screen world
                    else world
            if changed
            then World.publishScreenChange propertyName previous value screen world
            else world

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

        static member notifyScreenModelChange screen world =
            let screenState = World.getScreenState screen world
            let world = screenState.Dispatcher.TrySynchronize (false, screen, world)
            World.publishScreenChange Constants.Engine.ModelPropertyName screenState.Model.DesignerValue screenState.Model.DesignerValue screen world

    /// Initialize property getters.
    let private initGetters () =
        let screenGetters =
            dictPlus StringComparer.Ordinal
                [("Dispatcher", fun screen world -> { PropertyType = typeof<ScreenDispatcher>; PropertyValue = World.getScreenDispatcher screen world })
                 ("Model", fun screen world -> let designerProperty = World.getScreenModelProperty screen world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
                 ("TransitionState", fun screen world -> { PropertyType = typeof<TransitionState>; PropertyValue = World.getScreenTransitionState screen world })
                 ("Incoming", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenIncoming screen world })
                 ("Outgoing", fun screen world -> { PropertyType = typeof<Transition>; PropertyValue = World.getScreenOutgoing screen world })
                 ("RequestedSong", fun screen world -> { PropertyType = typeof<RequestedSong>; PropertyValue = World.getScreenRequestedSong screen world })
                 ("SlideOpt", fun screen world -> { PropertyType = typeof<Slide option>; PropertyValue = World.getScreenSlideOpt screen world })
                 ("Nav3d", fun screen world -> { PropertyType = typeof<Nav3d>; PropertyValue = World.getScreenNav3d screen world })
                 ("Protected", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenProtected screen world })
                 ("Persistent", fun screen world -> { PropertyType = typeof<bool>; PropertyValue = World.getScreenPersistent screen world })
                 ("Order", fun screen world -> { PropertyType = typeof<int64>; PropertyValue = World.getScreenOrder screen world })
                 ("Id", fun screen world -> { PropertyType = typeof<Guid>; PropertyValue = World.getScreenId screen world })
                 ("Name", fun screen world -> { PropertyType = typeof<string>; PropertyValue = World.getScreenName screen world })]
        ScreenGetters <- screenGetters.ToFrozenDictionary ()

    /// Initialize property setters.
    let private initSetters () =
        let screenSetters =
            dictPlus StringComparer.Ordinal
                [("Model", fun property screen world -> World.setScreenModelProperty false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } screen world)
                 ("TransitionState", fun property screen world -> World.setScreenTransitionState (property.PropertyValue :?> TransitionState) screen world)
                 ("Incoming", fun property screen world -> World.setScreenIncoming (property.PropertyValue :?> Transition) screen world)
                 ("Outgoing", fun property screen world -> World.setScreenOutgoing (property.PropertyValue :?> Transition) screen world)
                 ("RequestedSong", fun property screen world -> World.setScreenRequestedSong (property.PropertyValue :?> RequestedSong) screen world)
                 ("SlideOpt", fun property screen world -> World.setScreenSlideOpt (property.PropertyValue :?> Slide option) screen world)
                 ("Persistent", fun property screen world -> World.setScreenPersistent (property.PropertyValue :?> bool) screen world)]
        ScreenSetters <- screenSetters.ToFrozenDictionary ()

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()