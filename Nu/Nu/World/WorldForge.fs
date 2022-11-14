// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime

[<RequireQualifiedAccess>]
module Forge =

    let
#if !DEBUG
        inline
#endif
        private synchronizeEventSignals (forgeOld : SimulantForge) (forge : SimulantForge) (origin : Simulant) (simulant : Simulant) world =
        if notNull forgeOld.EventSignalForgesOpt || notNull forge.EventSignalForgesOpt then
            let eventSignalForgesOld = if isNull forgeOld.EventSignalForgesOpt then OrderedDictionary HashIdentity.Structural else forgeOld.EventSignalForgesOpt
            let eventSignalForges = if isNull forge.EventSignalForgesOpt then OrderedDictionary HashIdentity.Structural else forge.EventSignalForgesOpt
            if eventSignalForgesOld.Count > 0 || eventSignalForges.Count > 0 then
                let eventSignalsAdded = List ()
                for eventSignalEntry in eventSignalForges do
                    if not (eventSignalForgesOld.ContainsKey eventSignalEntry.Key) then
                        eventSignalsAdded.Add (eventSignalEntry.Key, eventSignalEntry.Value)
                let eventSignalsRemoved = List ()
                for eventSignalEntry in eventSignalForgesOld do
                    if not (eventSignalForges.ContainsKey eventSignalEntry.Key) then
                        eventSignalsRemoved.Add eventSignalEntry.Value
                let world =
                    Seq.fold
                        (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                        world eventSignalsRemoved
                let world =
                    Seq.fold (fun world ((eventAddress : obj Address, signalObj), subscriptionId) ->
                        let eventAddress = if eventAddress.Anonymous then eventAddress --> simulant.SimulantAddress else eventAddress
                        let (unsubscribe, world) =
                            World.subscribePlus subscriptionId (fun (_ : Event) world ->
                                let world = WorldModule.trySignal signalObj origin world
                                (Cascade, world))
                                eventAddress origin world
                        let world =
                            World.monitor
                                (fun _ world -> (Cascade, unsubscribe world))
                                (Events.Unregistering --> simulant.SimulantAddress)
                                simulant
                                world
                        world)
                        world eventSignalsAdded
                world
            else world
        else world

    let
#if !DEBUG
        inline
#endif
        private synchronizeEventHandlers (forgeOld : SimulantForge) (forge : SimulantForge) (origin : Simulant) (simulant : Simulant) world =
        if notNull forgeOld.EventHandlerForgesOpt || notNull forge.EventHandlerForgesOpt then
            let eventHandlerForgesOld = if isNull forgeOld.EventHandlerForgesOpt then OrderedDictionary HashIdentity.Structural else forgeOld.EventHandlerForgesOpt
            let eventHandlerForges = if isNull forge.EventHandlerForgesOpt then OrderedDictionary HashIdentity.Structural else forge.EventHandlerForgesOpt
            if eventHandlerForgesOld.Count > 0 || eventHandlerForges.Count > 0 then
                let eventHandlersAdded = List ()
                for eventHandlerEntry in eventHandlerForges do
                    if not (eventHandlerForgesOld.ContainsKey eventHandlerEntry.Key) then
                        eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
                let eventHandlersRemoved = List ()
                for eventHandlerEntry in eventHandlerForgesOld do
                    if not (eventHandlerForges.ContainsKey eventHandlerEntry.Key) then
                        eventHandlersRemoved.Add eventHandlerEntry.Value
                let world =
                    Seq.fold
                        (fun world (subscriptionId, _) -> World.unsubscribe subscriptionId world)
                        world eventHandlersRemoved
                let world =
                    Seq.fold (fun world ((_, eventAddress : obj Address), (subscriptionId, handler)) ->
                        let eventAddress = if eventAddress.Anonymous then eventAddress --> simulant.SimulantAddress else eventAddress
                        let (unsubscribe, world) =
                            World.subscribePlus subscriptionId (fun event world ->
                                let world = WorldModule.trySignal (handler event) origin world
                                (Cascade, world))
                                eventAddress origin world
                        let world =
                            World.monitor
                                (fun _ world -> (Cascade, unsubscribe world))
                                (Events.Unregistering --> simulant.SimulantAddress)
                                simulant
                                world
                        world)
                        world eventHandlersAdded
                world
            else world
        else world

    let
#if !DEBUG
        inline
#endif
        private synchronizeProperties (forgeOld : SimulantForge) (forge : SimulantForge) (simulant : Simulant) world =
        if notNull forge.PropertyForgesOpt && forge.PropertyForgesOpt.Count > 0 then
            let simulant = if notNull (forgeOld.SimulantCachedOpt :> obj) then forgeOld.SimulantCachedOpt else simulant
            forge.SimulantCachedOpt <- simulant
            Seq.fold (fun world (simulantOpt, propertyLens : World Lens, propertyValue) ->
                propertyLens.TrySet propertyValue (match simulantOpt with ValueSome simulant -> simulant | ValueNone -> simulant) world)
                world forge.PropertyForgesOpt
        else world

    let
#if !DEBUG
        inline
#endif
        private tryDifferentiateChildren<'child, 'childForge when 'child : equality and 'child :> Simulant and 'childForge :> SimulantForge>
        (forgeOld : SimulantForge) (forge : SimulantForge) (simulant : Simulant) =
        if notNull (forgeOld.GetChildForgesOpt<'childForge> ()) || notNull (forge.GetChildForgesOpt<'childForge> ()) then
            let childForgesOld = if isNull (forgeOld.GetChildForgesOpt<'childForge> ()) then OrderedDictionary HashIdentity.Structural else forgeOld.GetChildForgesOpt<'childForge> ()
            let childForges = if isNull (forge.GetChildForgesOpt<'childForge> ()) then OrderedDictionary HashIdentity.Structural else forge.GetChildForgesOpt<'childForge> ()
            if childForgesOld.Count > 0 || childForges.Count > 0 then
                let childrenPotentiallyAltered = OrderedDictionary HashIdentity.Structural
                let childrenAdded = List ()
                for childEntry in childForges do
                    match childForgesOld.TryGetValue childEntry.Key with
                    | (true, childForgeOld) ->
                        let childSimulant = // OPTIMIZATION: attempt to get child simulant from old forge rather than deriving it, and store it for future use.
                            if isNull (childForgeOld.SimulantCachedOpt :> obj) then
                                let derived = World.derive (rtoa (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
                                childEntry.Value.SimulantCachedOpt <- derived
                                derived
                            else
                                let found = childForgeOld.SimulantCachedOpt :?> 'child
                                childEntry.Value.SimulantCachedOpt <- found
                                found
                        childrenPotentiallyAltered.Add (childSimulant, childEntry.Value)
                    | (false, _) ->
                        let childSimulant = World.derive (rtoa (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
                        childEntry.Value.SimulantCachedOpt <- childSimulant
                        childrenAdded.Add (childSimulant, childEntry.Value)
                let childrenRemoved = List<'child> ()
                for childEntryOld in childForgesOld do
                    match childForges.TryGetValue childEntryOld.Key with
                    | (true, _) -> ()
                    | (false, _) ->
                        let childSimulant = childEntryOld.Value.SimulantCachedOpt :?> 'child // OPTIMIZATION: because of above optimization, should be guaranteed to exist.
                        childrenRemoved.Add childSimulant
                        childrenPotentiallyAltered.Remove childSimulant |> ignore
                Some (childrenAdded, childrenRemoved, childrenPotentiallyAltered)
            else None
        else None

    ///
    let rec synchronizeEntity (forgeOld : EntityForge) (forge : EntityForge) (origin : Simulant) (entity : Entity) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin entity world
            let world = synchronizeEventHandlers forgeOld forge origin entity world
            let world = synchronizeProperties forgeOld forge entity world
            match tryDifferentiateChildren<Entity, EntityForge> forgeOld forge entity with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull forgeOld.EntityForgesOpt then
                        Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                            let (entity, entityForge) = (kvp.Key, kvp.Value)
                            let entityForgeOld = forgeOld.EntityForgesOpt.[entity.Name]
                            synchronizeEntity entityForgeOld entityForge origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                        let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                        synchronizeEntity EntityForge.empty entityForge origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    ///
    let synchronizeGroup (forgeOld : GroupForge) (forge : GroupForge) (origin : Simulant) (group : Group) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin group world
            let world = synchronizeEventHandlers forgeOld forge origin group world
            let world = synchronizeProperties forgeOld forge group world
            match tryDifferentiateChildren<Entity, EntityForge> forgeOld forge group with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull forgeOld.EntityForgesOpt then
                        Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                            let (entity, entityForge) = (kvp.Key, kvp.Value)
                            let entityForgeOld = forgeOld.EntityForgesOpt.[entity.Name]
                            synchronizeEntity entityForgeOld entityForge origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    Seq.fold (fun world (entity : Entity, entityForge : EntityForge) ->
                        let (entity, world) = World.createEntity5 entityForge.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                        synchronizeEntity EntityForge.empty entityForge origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    ///
    let synchronizeScreen (forgeOld : ScreenForge) (forge : ScreenForge) (origin : Simulant) (screen : Screen) world =
        if forgeOld <> forge then
            let world = synchronizeEventSignals forgeOld forge origin screen world
            let world = synchronizeEventHandlers forgeOld forge origin screen world
            let world = synchronizeProperties forgeOld forge screen world
            let world =
                if forgeOld.GroupFilePathOpt <> forge.GroupFilePathOpt then
                    let world =
                        match forgeOld.GroupFilePathOpt with
                        | Some groupFilePath ->
                            // NOTE: have to load the group file just get the name of the group to destroy...
                            let groupDescriptorStr = File.ReadAllText groupFilePath
                            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                            let groupName =
                                Constants.Engine.NamePropertyName |>
                                groupDescriptor.GroupProperties.TryFind |>
                                Option.mapOrDefaultValue symbolToValue Simulants.Default.Group.Name
                            let group = screen / groupName
                            World.destroyGroup group world
                        | None -> world
                    let world =
                        match forge.GroupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> snd
                        | None -> world
                    world
                else world
            match tryDifferentiateChildren<Group, GroupForge> forgeOld forge screen with
            | Some (groupsAdded, groupsRemoved, groupsPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world group -> World.destroyGroup group world) world groupsRemoved
                let world =
                    Seq.fold (fun world (kvp : KeyValuePair<Group, _>) ->
                        let (group, groupForge) = (kvp.Key, kvp.Value)
                        let groupForgeOld = forgeOld.GroupForges.[group.Name]
                        synchronizeGroup groupForgeOld groupForge origin group world)
                        world groupsPotentiallyAltered
                let world =
                    Seq.fold (fun world (group : Group, groupForge : GroupForge) ->
                        let (group, world) =
                            match groupForge.GroupFilePathOpt with
                            | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world
                            | None -> World.createGroup4 groupForge.GroupDispatcherName (Some group.Name) group.Screen world
                        synchronizeGroup GroupForge.empty groupForge origin group world)
                        world groupsAdded
                world
            | None -> world
        else world

    ///
    let synchronizeGame setScreenSplash (forgeOld : GameForge) (forge : GameForge) (origin : Simulant) world =
        if forgeOld <> forge then
            let game = Simulants.Game
            let world = synchronizeEventSignals forgeOld forge origin game world
            let world = synchronizeEventHandlers forgeOld forge origin game world
            let world = synchronizeProperties forgeOld forge game world
            match tryDifferentiateChildren<Screen, ScreenForge> forgeOld forge game with
            | Some (screensAdded, screensRemoved, screensPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world screen -> World.destroyScreen screen world) world screensRemoved
                let world =
                    Seq.fold (fun world (kvp : KeyValuePair<Screen, _>) ->
                        let (screen, screenForge) = (kvp.Key, kvp.Value)
                        let screenForgeOld = forgeOld.ScreenForges.[screen.Name]
                        synchronizeScreen screenForgeOld screenForge origin screen world)
                        world screensPotentiallyAltered
                let world =
                    Seq.fold (fun world (screen : Screen, screenForge : ScreenForge) ->
                        let (screen, world) = World.createScreen3 screenForge.ScreenDispatcherName (Some screen.Name) world
                        let world = World.applyScreenBehavior setScreenSplash screenForge.ScreenBehavior screen world
                        synchronizeScreen ScreenForge.empty screenForge origin screen world)
                        world screensAdded
                (forge.InitialScreenNameOpt |> Option.map Screen, world)
            | None -> (forge.InitialScreenNameOpt |> Option.map Screen, world)
        else (forge.InitialScreenNameOpt |> Option.map Screen, world)

    ///
    let composite<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName initializers entities =
        let mutable eventSignalForgesOpt = null
        let mutable eventHandlerForgesOpt = null
        let mutable propertyForgesOpt = null
        let mutable entityForgesOpt = null
        let mutable i = 0
        for property in initializers do
            match property with
            | EventSignalForge (addr, value) -> (if isNull eventSignalForgesOpt then eventSignalForgesOpt <- OrderedDictionary HashIdentity.Structural); eventSignalForgesOpt.Add ((addr, value), makeGuid ())
            | EventHandlerForge pe -> (if isNull eventHandlerForgesOpt then eventHandlerForgesOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerForgesOpt.Add ((i, pe.Equatable), (makeGuid (), pe.Nonequatable))
            | PropertyForge (simulantOpt, lens, value) -> (if isNull propertyForgesOpt then propertyForgesOpt <- List ()); propertyForgesOpt.Add (simulantOpt, lens, value)
            i <- inc i
        for entity in entities do
            if isNull entityForgesOpt then entityForgesOpt <- OrderedDictionary StringComparer.Ordinal
            entityForgesOpt.Add (entity.EntityName, entity)
        { EntityDispatcherName = typeof<'entityDispatcher>.Name; EntityName = entityName; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalForgesOpt = eventSignalForgesOpt; EventHandlerForgesOpt = eventHandlerForgesOpt; PropertyForgesOpt = propertyForgesOpt; EntityForgesOpt = entityForgesOpt }

    ///
    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName initializers =
        composite<'entityDispatcher> entityName initializers []

    /// Describe a 2d basic emitter with the given initializers.
    let basicEmitter2d entityName initializers = entity<BasicEmitterDispatcher2d> entityName initializers

    /// Describe a 2d effect with the given initializers.
    let effect2d entityName initializers = entity<EffectDispatcher2d> entityName initializers

    /// Describe a static sprite with the given initializers.
    let staticSprite entityName initializers = entity<StaticSpriteDispatcher> entityName initializers

    /// Describe an animated sprite with the given initializers.
    let animatedSprite entityName initializers = entity<AnimatedSpriteDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

    /// Describe a text with the given initializers.
    let text entityName initializers = entity<TextDispatcher> entityName initializers

    /// Describe a toggle button with the given initializers.
    let toggleButton entityName initializers = entity<ToggleButtonDispatcher> entityName initializers

    /// Describe a radio button with the given initializers.
    let radioButton entityName initializers = entity<RadioButtonDispatcher> entityName initializers

    /// Describe an fps gui with the given initializers.
    let fps entityName initializers = entity<FpsDispatcher> entityName initializers

    /// Describe a feeler with the given initializers.
    let feeler entityName initializers = entity<FeelerDispatcher> entityName initializers

    /// Describe a fill bar with the given initializers.
    let fillBar entityName initializers = entity<FillBarDispatcher> entityName initializers

    /// Describe a 2d block with the given initializers.
    let block2d entityName initializers = entity<BlockDispatcher2d> entityName initializers

    /// Describe a 2d box with the given initializers.
    let box2d entityName initializers = entity<BoxDispatcher2d> entityName initializers

    /// Describe a side-view character with the given initializers.
    let sideViewCharacter entityName initializers = entity<SideViewCharacterDispatcher> entityName initializers

    /// Describe a tile map with the given initializers.
    let tileMap entityName initializers = entity<TileMapDispatcher> entityName initializers

    /// Describe a tmx map with the given initializers.
    let tmxMap entityName initializers = entity<TmxMapDispatcher> entityName initializers

    /// Describe a 3d light with the given initializers.
    let light3d entityName initializers = entity<LightDispatcher3d> entityName initializers

    /// Describe a sky box with the given initializers.
    let skyBox entityName initializers = entity<SkyBoxDispatcher> entityName initializers

    /// Describe a static billboard with the given initializers.
    let staticBillboard entityName initializers = entity<StaticBillboardDispatcher> entityName initializers

    /// Describe a static model with the given initializers.
    let staticModel entityName initializers = entity<StaticModelDispatcher> entityName initializers

    /// Describe a static model surface with the given initializers.
    let staticModelSurface entityName initializers = entity<StaticModelSurfaceDispatcher> entityName initializers

    /// Describe a static model expanded into an entity hierarchy with the given initializers.
    let staticModelHierarchy entityName initializers = entity<StaticModelHierarchyDispatcher> entityName initializers

    ///
    let group4<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName groupFilePathOpt initializers entities =
        let mutable eventSignalForgesOpt = null
        let mutable eventHandlerForgesOpt = null
        let mutable propertyForgesOpt = null
        let mutable entityForgesOpt = null
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalForge (addr, value) -> (if isNull eventSignalForgesOpt then eventSignalForgesOpt <- OrderedDictionary HashIdentity.Structural); eventSignalForgesOpt.Add ((addr, value), makeGuid ())
            | EventHandlerForge pe -> (if isNull eventHandlerForgesOpt then eventHandlerForgesOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerForgesOpt.Add ((i, pe.Equatable), (makeGuid (), pe.Nonequatable))
            | PropertyForge (simulantOpt, lens, value) -> (if isNull propertyForgesOpt then propertyForgesOpt <- List ()); propertyForgesOpt.Add (simulantOpt, lens, value)
            i <- inc i
        for entity in entities do
            if isNull entityForgesOpt then entityForgesOpt <- OrderedDictionary StringComparer.Ordinal
            entityForgesOpt.Add (entity.EntityName, entity)
        { GroupDispatcherName = typeof<'groupDispatcher>.Name; GroupName = groupName; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalForgesOpt = eventSignalForgesOpt; EventHandlerForgesOpt = eventHandlerForgesOpt; PropertyForgesOpt = propertyForgesOpt; EntityForgesOpt = entityForgesOpt }

    ///
    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName initializers entities =
        group4<'groupDispatcher> groupName None initializers entities

    ///
    let groupFromFile<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName filePath initializers entities =
        group4<'groupDispatcher> groupName (Some filePath) initializers entities

    ///
    let private screen5<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePathOpt initializers groups =
        let mutable eventSignalForgesOpt = null
        let mutable eventHandlerForgesOpt = null
        let mutable propertyForgesOpt = null
        let groupForges = OrderedDictionary StringComparer.Ordinal
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalForge (addr, value) -> (if isNull eventSignalForgesOpt then eventSignalForgesOpt <- OrderedDictionary HashIdentity.Structural); eventSignalForgesOpt.Add ((addr, value), makeGuid ())
            | EventHandlerForge pe -> (if isNull eventHandlerForgesOpt then eventHandlerForgesOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerForgesOpt.Add ((i, pe.Equatable), (makeGuid (), pe.Nonequatable))
            | PropertyForge (simulantOpt, lens, value) -> (if isNull propertyForgesOpt then propertyForgesOpt <- List ()); propertyForgesOpt.Add (simulantOpt, lens, value)
            i <- inc i
        for group in groups do
            groupForges.Add (group.GroupName, group)
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name; ScreenName = screenName; ScreenBehavior = screenBehavior; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalForgesOpt = eventSignalForgesOpt; EventHandlerForgesOpt = eventHandlerForgesOpt; PropertyForgesOpt = propertyForgesOpt; GroupForges = groupForges }

    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior initializers groups =
        screen5<'screenDispatcher> screenName screenBehavior None initializers groups

    let screenWithGroupFromFile<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePath initializers groups =
        screen5<'screenDispatcher> screenName screenBehavior (Some groupFilePath) initializers groups

    ///
    let game initializers screens =
        let initialScreenNameOpt = match Seq.tryHead screens with Some screen -> Some screen.ScreenName | None -> None
        let mutable eventSignalForgesOpt = null
        let mutable eventHandlerForgesOpt = null
        let mutable propertyForgesOpt = null
        let screenForges = OrderedDictionary StringComparer.Ordinal
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalForge (addr, value) -> (if isNull eventSignalForgesOpt then eventSignalForgesOpt <- OrderedDictionary HashIdentity.Structural); eventSignalForgesOpt.Add ((addr, value), makeGuid ())
            | EventHandlerForge pe -> (if isNull eventHandlerForgesOpt then eventHandlerForgesOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerForgesOpt.Add ((i, pe.Equatable), (makeGuid (), pe.Nonequatable))
            | PropertyForge (simulantOpt, lens, value) -> (if isNull propertyForgesOpt then propertyForgesOpt <- List ()); propertyForgesOpt.Add (simulantOpt, lens, value)
            i <- inc i
        for screen in screens do
            screenForges.Add (screen.ScreenName, screen)
        { InitialScreenNameOpt = initialScreenNameOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalForgesOpt = eventSignalForgesOpt; EventHandlerForgesOpt = eventHandlerForgesOpt; PropertyForgesOpt = propertyForgesOpt; ScreenForges = screenForges }

[<AutoOpen>]
module ForgeOperators =

    /// Initialize a property forge.
    let inline (==) (lens : Lens<'a, 's, World>) (value : 'a) : InitializerForge =
        PropertyForge (ValueNone, lens, value :> obj)

    /// Initialize a signal forge.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : InitializerForge =
        EventSignalForge (Address.generalize eventAddress, signal)

    /// Initialize a signal handler forge.
    let inline (==|>) (eventAddress : 'a Address) (callback : Event<'a, 's> -> Signal<'message, 'command>) : InitializerForge =
        EventHandlerForge (PartialEquatable.make (Address.generalize eventAddress) (fun (evt : Event) -> callback (Event.specialize evt) :> obj))