// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Frozen
open Prime

/// Group functions for the world (1/2).
[<AutoOpen>]
module WorldModuleGroup =

    /// Dynamic property getter and setter.
    type private PropertyGetter = Group -> World -> Property
    type private PropertySetter = Property -> Group -> World -> bool

    /// Dynamic property getters / setters.
    let mutable private GroupGetters = Unchecked.defaultof<FrozenDictionary<string, PropertyGetter>>
    let mutable private GroupSetters = Unchecked.defaultof<FrozenDictionary<string, PropertySetter>>

    type World with
    
        static member private groupStateFinder (group : Group) (world : World) =
            UMap.tryFind group world.GroupStates

        static member private groupStateAdder groupState (group : Group) (world : World) =
            let screen = group.Screen
            let simulants =
                match world.Simulants.TryGetValue (screen :> Simulant) with
                | (true, groupsOpt) ->
                    match groupsOpt with
                    | Some groups ->
                        let groups = USet.add (group :> Simulant) groups
                        UMap.add (screen :> Simulant) (Some groups) world.Simulants
                    | None ->
                        let groups = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) (group :> Simulant)
                        UMap.add (screen :> Simulant) (Some groups) world.Simulants
                | (false, _) -> failwith ("Cannot add group '" + scstring group + "' to non-existent screen.")
            let simulants =
                if not (UMap.containsKey (group :> Simulant) simulants)
                then UMap.add (group :> Simulant) None simulants
                else simulants
            let groupStates = UMap.add group groupState world.GroupStates
            world.WorldState <- { world.WorldState with Simulants = simulants; GroupStates = groupStates }
        
        static member private groupStateRemover (group : Group) (world : World) =
            let screen = group.Screen
            let simulants =
                match world.Simulants.TryGetValue (screen :> Simulant) with
                | (true, groupsOpt) ->
                    match groupsOpt with
                    | Some groups ->
                        let groups = USet.remove (group :> Simulant) groups
                        if USet.isEmpty groups
                        then UMap.add (screen :> Simulant) None world.Simulants
                        else UMap.add (screen :> Simulant) (Some groups) world.Simulants
                    | None -> world.Simulants
                | (false, _) -> world.Simulants
            let simulants = UMap.remove (group :> Simulant) simulants
            let groupStates = UMap.remove group world.GroupStates
            world.WorldState <- { world.WorldState with Simulants = simulants; GroupStates = groupStates }

        static member private groupStateSetter groupState (group : Group) (world : World) =
#if DEBUG
            if not (UMap.containsKey group world.GroupStates) then
                failwith ("Cannot set the state of a non-existent group '" + scstring group + "'")
#endif
            let groupStates = UMap.add group groupState world.GroupStates
            world.WorldState <- { world.WorldState with GroupStates = groupStates }

        static member private addGroupState groupState group world =
            World.groupStateAdder groupState group world

        static member internal removeGroupState group world =
            World.groupStateRemover group world

        static member internal publishGroupChange propertyName (propertyPrevious : obj) (propertyValue : obj) (group : Group) world =
            let changeData = { Name = propertyName; Previous = propertyPrevious; Value = propertyValue }
            let groupNames = Address.getNames group.GroupAddress
            let changeEventAddress = rtoa<ChangeData> [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName; groupNames.[0]; groupNames.[1]; groupNames.[2]|]
            let eventTrace = EventTrace.debug "World" "publishGroupChange" "" EventTrace.empty
            World.publishPlus changeData changeEventAddress eventTrace group false false world

        static member internal getGroupStateOpt group world =
            World.groupStateFinder group world

        static member internal getGroupState group world =
            match World.getGroupStateOpt group world with
            | Some groupState -> groupState
            | None -> failwith ("Could not find group '" + scstring group + "'.")

        static member internal setGroupState groupState group world =
            World.groupStateSetter groupState group world

        static member internal getGroupXtension group world =
            let groupState = World.getGroupState group world
            groupState.Xtension

        static member internal getGroupExists group world =
            Option.isSome (World.getGroupStateOpt group world)

        static member internal getGroupSelected (group : Group) world =
            let gameState = World.getGameState Game.Handle world
            match gameState.SelectedScreenOpt with
            | Some selectedScreen when group.Screen.Name = selectedScreen.Name -> true
            | _ -> false

        static member internal getGroupModelProperty group world = (World.getGroupState group world).Model
        static member internal getGroupContent group world = (World.getGroupState group world).Content
        static member internal getGroupDispatcher group world = (World.getGroupState group world).Dispatcher
        static member internal getGroupVisible group world = (World.getGroupState group world).Visible
        static member internal getGroupProtected group world = (World.getGroupState group world).Protected
        static member internal getGroupPersistent group world = (World.getGroupState group world).Persistent
        static member internal getGroupDestroying (group : Group) world = List.exists ((=) (group :> Simulant)) (World.getDestructionListRev world)
        static member internal getGroupOrder group world = (World.getGroupState group world).Order
        static member internal getGroupId group world = (World.getGroupState group world).Id
        static member internal getGroupName group world = (World.getGroupState group world).Name

        static member internal setGroupModelProperty initializing reinitializing (value : DesignerProperty) group (world : World) =
            let groupState = World.getGroupState group world
            let previous = groupState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                let groupState = { groupState with Model = { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }}
                World.setGroupState groupState group world
                groupState.Dispatcher.TrySynchronize (initializing, reinitializing, group, world)
                World.publishGroupChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue group world
                true
            else false

        static member internal getGroupModelGeneric<'a> group world =
            let groupState = World.getGroupState group world
            match groupState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                let modelSymbol = valueToSymbol modelObj
                try let model = symbolToValue modelSymbol
                    groupState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                    model
                with _ ->
                    Log.warn "Could not convert existing group model value to new type; attempting to use fallback model value instead."
                    match groupState.Dispatcher.TryGetFallbackModel<'a> (modelSymbol, group, world) with
                    | None -> typeof<'a>.GetDefaultValue () :?> 'a
                    | Some model ->
                        groupState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                        model

        static member internal setGroupModelGeneric<'a> initializing reinitializing (value : 'a) group world =
            let groupState = World.getGroupState group world
            let valueObj = value :> obj
            let previous = groupState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                let groupState = { groupState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                World.setGroupState groupState group world
                groupState.Dispatcher.TrySynchronize (initializing, reinitializing, group, world)
                World.publishGroupChange Constants.Engine.ModelPropertyName previous.DesignerValue value group world
                true
            else false

        static member internal setGroupContent (value : GroupContent) group world =
            let groupState = World.getGroupState group world
            let groupState = { groupState with Content = value }
            World.setGroupState groupState group world

        static member internal setGroupVisible value group world =
            let groupState = World.getGroupState group world
            let previous = groupState.Visible
            if value <> previous then
                World.setGroupState { groupState with Visible = value } group world
                World.publishGroupChange (nameof groupState.Visible) previous value group world
                true
            else false

        static member internal setGroupProtected value group world =
            let groupState = World.getGroupState group world
            let previous = groupState.Protected
            if value <> previous then
                World.setGroupState { groupState with Protected = value } group world
                World.publishGroupChange (nameof groupState.Protected) previous value group world
                true
            else false

        static member internal setGroupPersistent value group world =
            let groupState = World.getGroupState group world
            let previous = groupState.Persistent
            if value <> previous then
                World.setGroupState { groupState with Persistent = value } group world
                World.publishGroupChange (nameof groupState.Persistent) previous value group world
                true
            else false

        static member internal tryGetGroupXtensionProperty (propertyName, group, world, property : _ outref) =
            if World.getGroupExists group world
            then GroupState.tryGetProperty (propertyName, World.getGroupState group world, &property)
            else false

        static member internal getGroupXtensionProperty propertyName group world =
            let mutable property = Unchecked.defaultof<_>
            match GroupState.tryGetProperty (propertyName, World.getGroupState group world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal tryGetGroupProperty (propertyName, group, world, property : _ outref) =
            match GroupGetters.TryGetValue propertyName with
            | (true, getter) ->
                if World.getGroupExists group world then
                    property <- getter group world
                    true
                else false
            | (false, _) ->
                let groupState = World.getGroupState group world
                if GroupState.tryGetProperty (propertyName, groupState, &property) then
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                    | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (group :> obj) (world :> obj) }; true
                    | _ -> true
                else false

        static member internal getGroupXtensionValue<'a> propertyName group world =
            let groupState = World.getGroupState group world
            let mutable property = Unchecked.defaultof<_>
            if GroupState.tryGetProperty (propertyName, groupState, &property) then
                let valueObj =
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue
                    | :? ComputedProperty as cp -> cp.ComputedGet group world
                    | _ -> property.PropertyValue
                match valueObj with
                | :? 'a as value -> value
                | null -> null :> obj :?> 'a
                | value ->
                    let value =
                        try value |> valueToSymbol |> symbolToValue
                        with _ ->
                            let value = typeof<'a>.GetDefaultValue ()
                            Log.warn "Could not gracefully promote value to the required type, so using a default value instead."
                            value :?> 'a
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerType <- typeof<'a>; dp.DesignerValue <- value
                    | :? ComputedProperty -> () // nothing to do
                    | _ -> property.PropertyType <- typeof<'a>; property.PropertyValue <- value
                    value
            else
                let definitions = Reflection.getPropertyDefinitions (getType groupState.Dispatcher)
                let value =
                    match List.tryFind (fun (pd : PropertyDefinition) -> pd.PropertyName = propertyName) definitions with
                    | Some definition ->
                        match definition.PropertyExpr with
                        | DefineExpr value -> value :?> 'a
                        | VariableExpr eval -> eval world :?> 'a
                        | ComputedExpr property -> property.ComputedGet group world :?> 'a
                    | None -> failwithumf ()
                let property = { PropertyType = typeof<'a>; PropertyValue = value }
                groupState.Xtension <- Xtension.attachProperty propertyName property groupState.Xtension
                value

        static member internal tryGetGroupXtensionValue<'a> propertyName group world : 'a voption =
            // NOTE: we're only using exceptions as flow control in order to avoid code duplication and perf costs.
            // TODO: P1: see if we can find a way to refactor this situation without incurring any additional overhead on the getGroupXtensionValue call.
            try World.getGroupXtensionValue<'a> propertyName group world |> ValueSome
            with _ -> ValueNone

        static member internal getGroupProperty propertyName group world =
            match GroupGetters.TryGetValue propertyName with
            | (true, getter) -> getter group world
            | (false, _) -> World.getGroupXtensionProperty propertyName group world

        static member internal trySetGroupXtensionPropertyWithoutEvent propertyName (property : Property) groupState group world =
            let mutable propertyOld = Unchecked.defaultof<_>
            match GroupState.tryGetProperty (propertyName, groupState, &propertyOld) with
            | true ->
                match propertyOld.PropertyValue with
                | :? DesignerProperty as dp ->
                    let previous = dp.DesignerValue
                    if property.PropertyValue =/= previous then
                        let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                        match GroupState.trySetProperty propertyName property groupState with
                        | struct (true, groupState) ->
                            World.setGroupState groupState group world
                            struct (true, true, previous)
                        | struct (false, _) -> struct (false, false, previous)
                    else (true, false, previous)
                | :? ComputedProperty as cp ->
                    match cp.ComputedSetOpt with
                    | Some computedSet ->
                        let previous = cp.ComputedGet (box group) (box world)
                        if property.PropertyValue =/= previous then
                            computedSet property.PropertyValue group world |> ignore<obj> // TODO: P0: move related type definitions into Nu from Prime and modify them to match mutable usage.
                            struct (true, true, previous)
                        else struct (true, false, previous)
                    | None -> struct (false, false, Unchecked.defaultof<_>)
                | _ ->
                    let previous = propertyOld.PropertyValue
                    if property.PropertyValue =/= previous then
                        match GroupState.trySetProperty propertyName property groupState with
                        | struct (true, groupState) ->
                            World.setGroupState groupState group world
                            struct (true, true, previous)
                        | struct (false, _) -> struct (false, false, previous)
                    else struct (true, false, previous)
            | false -> struct (false, false, Unchecked.defaultof<_>)

        static member internal trySetGroupXtensionPropertyFast propertyName (property : Property) group world =
            let groupState = World.getGroupState group world
            match World.trySetGroupXtensionPropertyWithoutEvent propertyName property groupState group world with
            | struct (true, changed, previous) ->
                if changed then
                    World.publishGroupChange propertyName previous property.PropertyValue group world
            | struct (false, _, _) -> ()

        static member internal trySetGroupXtensionProperty propertyName (property : Property) group world =
            let groupState = World.getGroupState group world
            match World.trySetGroupXtensionPropertyWithoutEvent propertyName property groupState group world with
            | struct (true, changed, previous) ->
                if changed then
                    World.publishGroupChange propertyName previous property.PropertyValue group world
                struct (true, changed)
            | struct (false, changed, _) -> struct (false, changed)

        static member internal trySetGroupXtensionValue<'a> propertyName (value : 'a) group world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetGroupXtensionProperty propertyName property group world

        static member internal setGroupXtensionValue<'a> propertyName (value : 'a) group world =
            let groupState = World.getGroupState group world
            let propertyOld = GroupState.getProperty propertyName groupState
            let mutable previous = Unchecked.defaultof<obj> // OPTIMIZATION: avoid passing around structs.
            let mutable changed = false // OPTIMIZATION: avoid passing around structs.
            match propertyOld.PropertyValue with
            | :? DesignerProperty as dp ->
                previous <- dp.DesignerValue
                if value =/= previous then
                    changed <- true
                    let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                    let groupState = GroupState.setProperty propertyName property groupState
                    World.setGroupState groupState group world
            | :? ComputedProperty as cp ->
                match cp.ComputedSetOpt with
                | Some computedSet ->
                    previous <- cp.ComputedGet (box group) (box world)
                    if value =/= previous then
                        changed <- true
                        computedSet propertyOld.PropertyValue group world |> ignore<obj> // TODO: P0: move related type definitions into Nu from Prime and modify them to match mutable usage.
                | None -> ()
            | _ ->
                previous <- propertyOld.PropertyValue
                if value =/= previous then
                    changed <- true
                    let property = { propertyOld with PropertyValue = value }
                    let groupState = GroupState.setProperty propertyName property groupState
                    World.setGroupState groupState group world
            if changed then
                World.publishGroupChange propertyName previous value group world

        static member internal setGroupXtensionProperty propertyName (property : Property) group world =
            let groupState = World.getGroupState group world
            let propertyOld = GroupState.getProperty propertyName groupState
            if property.PropertyValue =/= propertyOld.PropertyValue then
                let groupState = GroupState.setProperty propertyName property groupState
                World.setGroupState groupState group world
                World.publishGroupChange propertyName propertyOld.PropertyValue property.PropertyValue group world
                true
            else false

        static member internal trySetGroupPropertyFast propertyName property group world =
            match GroupSetters.TryGetValue propertyName with
            | (true, setter) -> setter property group world |> ignore<bool>
            | (false, _) -> World.trySetGroupXtensionPropertyFast propertyName property group world

        static member internal trySetGroupProperty propertyName property group world =
            match GroupSetters.TryGetValue propertyName with
            | (true, setter) ->
                let changed = setter property group world
                struct (true, changed)
            | (false, _) ->
                World.trySetGroupXtensionProperty propertyName property group world

        static member internal setGroupProperty propertyName property group world =
            match GroupSetters.TryGetValue propertyName with
            | (true, setter) -> setter property group world
            | (false, _) -> World.setGroupXtensionProperty propertyName property group world

        static member internal attachGroupMissingProperties group world =
            let groupState = World.getGroupState group world
            let definitions = Reflection.getReflectivePropertyDefinitions groupState
            let groupState =
                Map.fold (fun groupState propertyName (propertyDefinition : PropertyDefinition) ->
                    let mutable property = Unchecked.defaultof<_>
                    if not (World.tryGetGroupProperty (propertyName, group, world, &property)) then
                        let propertyValue = PropertyExpr.eval propertyDefinition.PropertyExpr world
                        let property = { PropertyType = propertyDefinition.PropertyType; PropertyValue = propertyValue }
                        GroupState.attachProperty propertyName property groupState
                    else groupState)
                    groupState definitions
            World.setGroupState groupState group world

        static member internal registerGroup group world =
            let dispatcher = World.getGroupDispatcher group world
            dispatcher.Register (group, world)
            let eventTrace = EventTrace.debug "World" "registerGroup" "" EventTrace.empty
            World.publishPlus () (Events.RegisterEvent --> group) eventTrace group true false world
            let eventTrace = EventTrace.debug "World" "registerGroup" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData group) (Events.LifeCycleEvent (nameof Group) --> Nu.Game.Handle) eventTrace group true false world

        static member internal unregisterGroup group world =
            let dispatcher = World.getGroupDispatcher group world
            let eventTrace = EventTrace.debug "World" "unregisterGroup" "LifeCycle" EventTrace.empty
            World.publishPlus (UnregisteringData group) (Events.LifeCycleEvent (nameof Group) --> Nu.Game.Handle) eventTrace group true false world
            let eventTrace = EventTrace.debug "World" "unregisteringGroup" "" EventTrace.empty
            World.publishPlus () (Events.UnregisteringEvent --> group) eventTrace group true false world
            dispatcher.Unregister (group, world)

        static member internal addGroup mayReplace groupState group world =
            let isNew = not (World.getGroupExists group world)
            if isNew || mayReplace then
                World.addGroupState groupState group world
                if isNew then World.registerGroup group world
            else failwith ("Adding a group that the world already contains '" + scstring group + "'.")

        static member internal removeGroup3 removeEntities group world =
            if World.getGroupExists group world then
                World.unregisterGroup group world
                removeEntities group world
                World.removeGroupState group world

        static member internal viewGroupProperties group world =
            let state = World.getGroupState group world
            World.viewSimulantStateProperties state

        static member notifyGroupModelChange group world =
            let groupState = World.getGroupState group world
            groupState.Dispatcher.TrySynchronize (false, false, group, world)
            World.publishGroupChange Constants.Engine.ModelPropertyName groupState.Model.DesignerValue groupState.Model.DesignerValue group world

    /// Initialize property getters.
    let private initGetters () =
        let groupGetters =
            dictPlus StringComparer.Ordinal
                [("Dispatcher", fun group world -> { PropertyType = typeof<GroupDispatcher>; PropertyValue = World.getGroupDispatcher group world })
                 ("Model", fun group world -> let designerProperty = World.getGroupModelProperty group world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
                 ("Visible", fun group world -> { PropertyType = typeof<bool>; PropertyValue = World.getGroupVisible group world })
                 ("Protected", fun group world -> { PropertyType = typeof<bool>; PropertyValue = World.getGroupProtected group world })
                 ("Persistent", fun group world -> { PropertyType = typeof<bool>; PropertyValue = World.getGroupPersistent group world })
                 ("Destroying", fun group world -> { PropertyType = typeof<bool>; PropertyValue = World.getGroupDestroying group world })
                 ("Order", fun group world -> { PropertyType = typeof<int64>; PropertyValue = World.getGroupOrder group world })
                 ("Id", fun group world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGroupId group world })
                 ("Name", fun group world -> { PropertyType = typeof<string>; PropertyValue = World.getGroupName group world })]
        GroupGetters <- groupGetters.ToFrozenDictionary ()

    /// Initialize property setters.
    let private initSetters () =
        let groupSetters =
            dictPlus StringComparer.Ordinal
                [("Model", fun property group world -> World.setGroupModelProperty false false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } group world)
                 ("Visible", fun property group world -> World.setGroupVisible (property.PropertyValue :?> bool) group world)
                 ("Persistent", fun property group world -> World.setGroupPersistent (property.PropertyValue :?> bool) group world)]
        GroupSetters <- groupSetters.ToFrozenDictionary ()

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()