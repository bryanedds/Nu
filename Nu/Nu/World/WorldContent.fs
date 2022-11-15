// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime

[<RequireQualifiedAccess>]
module Content =

    let
#if !DEBUG
        inline
#endif
        private synchronizeEventSignals (contentOld : SimulantContent) (content : SimulantContent) (origin : Simulant) (simulant : Simulant) world =
        if notNull contentOld.EventSignalContentsOpt || notNull content.EventSignalContentsOpt then
            let eventSignalContentsOld = if isNull contentOld.EventSignalContentsOpt then OrderedDictionary HashIdentity.Structural else contentOld.EventSignalContentsOpt
            let eventSignalContents = if isNull content.EventSignalContentsOpt then OrderedDictionary HashIdentity.Structural else content.EventSignalContentsOpt
            if eventSignalContentsOld.Count > 0 || eventSignalContents.Count > 0 then
                let eventSignalsAdded = List ()
                for eventSignalEntry in eventSignalContents do
                    if not (eventSignalContentsOld.ContainsKey eventSignalEntry.Key) then
                        eventSignalsAdded.Add (eventSignalEntry.Key, eventSignalEntry.Value)
                let eventSignalsRemoved = List ()
                for eventSignalEntry in eventSignalContentsOld do
                    if not (eventSignalContents.ContainsKey eventSignalEntry.Key) then
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
        private synchronizeEventHandlers (contentOld : SimulantContent) (content : SimulantContent) (origin : Simulant) (simulant : Simulant) world =
        if notNull contentOld.EventHandlerContentsOpt || notNull content.EventHandlerContentsOpt then
            let eventHandlerContentsOld = if isNull contentOld.EventHandlerContentsOpt then OrderedDictionary HashIdentity.Structural else contentOld.EventHandlerContentsOpt
            let eventHandlerContents = if isNull content.EventHandlerContentsOpt then OrderedDictionary HashIdentity.Structural else content.EventHandlerContentsOpt
            if eventHandlerContentsOld.Count > 0 || eventHandlerContents.Count > 0 then
                let eventHandlersAdded = List ()
                for eventHandlerEntry in eventHandlerContents do
                    if not (eventHandlerContentsOld.ContainsKey eventHandlerEntry.Key) then
                        eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
                let eventHandlersRemoved = List ()
                for eventHandlerEntry in eventHandlerContentsOld do
                    if not (eventHandlerContents.ContainsKey eventHandlerEntry.Key) then
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
        private synchronizeProperties (contentOld : SimulantContent) (content : SimulantContent) (simulant : Simulant) world =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let simulant = if notNull (contentOld.SimulantCachedOpt :> obj) then contentOld.SimulantCachedOpt else simulant
            content.SimulantCachedOpt <- simulant
            Seq.fold (fun world propertyContent ->
                let world =
                    let lens = propertyContent.PropertyLens
                    let simulant = match propertyContent.PropertySimulantOpt with Some simulant -> simulant | None -> simulant
                    World.setProperty lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } simulant world |> snd'
                world)
                world content.PropertyContentsOpt
        else world

    let
#if !DEBUG
        inline
#endif
        private synchronizeEntityPropertiesFast (contentOld : EntityContent) (content : EntityContent) (entity : Entity) world =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let entity = if notNull (contentOld.EntityCachedOpt :> obj) then contentOld.EntityCachedOpt else entity
            content.EntityCachedOpt <- entity
            Seq.fold (fun world propertyContent ->
                let world =
                    let lens = propertyContent.PropertyLens
                    let entity = match propertyContent.PropertySimulantOpt with Some simulant -> simulant :?> Entity | None -> entity
                    World.setEntityPropertyFast lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } entity world
                world)
                world content.PropertyContentsOpt
        else world

    let
#if !DEBUG
        inline
#endif
        private tryDifferentiateChildren<'child, 'childContent when 'child : equality and 'child :> Simulant and 'childContent :> SimulantContent>
        (contentOld : SimulantContent) (content : SimulantContent) (simulant : Simulant) =
        if notNull (contentOld.GetChildContentsOpt<'childContent> ()) || notNull (content.GetChildContentsOpt<'childContent> ()) then
            let childContentsOld = if isNull (contentOld.GetChildContentsOpt<'childContent> ()) then OrderedDictionary HashIdentity.Structural else contentOld.GetChildContentsOpt<'childContent> ()
            let childContents = if isNull (content.GetChildContentsOpt<'childContent> ()) then OrderedDictionary HashIdentity.Structural else content.GetChildContentsOpt<'childContent> ()
            if childContentsOld.Count > 0 || childContents.Count > 0 then
                let childrenPotentiallyAltered = OrderedDictionary HashIdentity.Structural
                let childrenAdded = List ()
                for childEntry in childContents do
                    match childContentsOld.TryGetValue childEntry.Key with
                    | (true, childContentOld) ->
                        let childSimulant = // OPTIMIZATION: attempt to get child simulant from old content rather than deriving it, and store it for future use.
                            if isNull (childContentOld.SimulantCachedOpt :> obj) then
                                let derived = World.derive (rtoa (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
                                childEntry.Value.SimulantCachedOpt <- derived
                                derived
                            else
                                let found = childContentOld.SimulantCachedOpt :?> 'child
                                childEntry.Value.SimulantCachedOpt <- found
                                found
                        childrenPotentiallyAltered.Add (childSimulant, childEntry.Value)
                    | (false, _) ->
                        let childSimulant = World.derive (rtoa (Array.add childEntry.Key simulant.SimulantAddress.Names)) :?> 'child
                        childEntry.Value.SimulantCachedOpt <- childSimulant
                        childrenAdded.Add (childSimulant, childEntry.Value)
                let childrenRemoved = List<'child> ()
                for childEntryOld in childContentsOld do
                    match childContents.TryGetValue childEntryOld.Key with
                    | (true, _) -> ()
                    | (false, _) ->
                        let childSimulant = childEntryOld.Value.SimulantCachedOpt :?> 'child // OPTIMIZATION: because of above optimization, should be guaranteed to exist.
                        childrenRemoved.Add childSimulant
                        childrenPotentiallyAltered.Remove childSimulant |> ignore
                Some (childrenAdded, childrenRemoved, childrenPotentiallyAltered)
            else None
        else None

    ///
    let rec synchronizeEntity (contentOld : EntityContent) (content : EntityContent) (origin : Simulant) (entity : Entity) world =
        if contentOld <> content then
            let world = synchronizeEventSignals contentOld content origin entity world
            let world = synchronizeEventHandlers contentOld content origin entity world
            let world = synchronizeEntityPropertiesFast contentOld content entity world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content entity with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull contentOld.EntityContentsOpt then
                        Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                            let (entity, entityContent) = (kvp.Key, kvp.Value)
                            let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                            synchronizeEntity entityContentOld entityContent origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    Seq.fold (fun world (entity : Entity, entityContent : EntityContent) ->
                        let (entity, world) = World.createEntity5 entityContent.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                        synchronizeEntity EntityContent.empty entityContent origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    ///
    let synchronizeGroup (contentOld : GroupContent) (content : GroupContent) (origin : Simulant) (group : Group) world =
        if contentOld <> content then
            let world = synchronizeEventSignals contentOld content origin group world
            let world = synchronizeEventHandlers contentOld content origin group world
            let world = synchronizeProperties contentOld content group world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content group with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull contentOld.EntityContentsOpt then
                        Seq.fold (fun world (kvp : KeyValuePair<Entity, _>) ->
                            let (entity, entityContent) = (kvp.Key, kvp.Value)
                            let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                            synchronizeEntity entityContentOld entityContent origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    Seq.fold (fun world (entity : Entity, entityContent : EntityContent) ->
                        let (entity, world) = World.createEntity5 entityContent.EntityDispatcherName (Some entity.Surnames) DefaultOverlay entity.Group world
                        synchronizeEntity EntityContent.empty entityContent origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    ///
    let synchronizeScreen (contentOld : ScreenContent) (content : ScreenContent) (origin : Simulant) (screen : Screen) world =
        if contentOld <> content then
            let world = synchronizeEventSignals contentOld content origin screen world
            let world = synchronizeEventHandlers contentOld content origin screen world
            let world = synchronizeProperties contentOld content screen world
            let world =
                if contentOld.GroupFilePathOpt <> content.GroupFilePathOpt then
                    let world =
                        match contentOld.GroupFilePathOpt with
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
                        match content.GroupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> snd
                        | None -> world
                    world
                else world
            match tryDifferentiateChildren<Group, GroupContent> contentOld content screen with
            | Some (groupsAdded, groupsRemoved, groupsPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world group -> World.destroyGroup group world) world groupsRemoved
                let world =
                    Seq.fold (fun world (kvp : KeyValuePair<Group, _>) ->
                        let (group, groupContent) = (kvp.Key, kvp.Value)
                        let groupContentOld = contentOld.GroupContents.[group.Name]
                        synchronizeGroup groupContentOld groupContent origin group world)
                        world groupsPotentiallyAltered
                let world =
                    Seq.fold (fun world (group : Group, groupContent : GroupContent) ->
                        let (group, world) =
                            match groupContent.GroupFilePathOpt with
                            | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world
                            | None -> World.createGroup4 groupContent.GroupDispatcherName (Some group.Name) group.Screen world
                        synchronizeGroup GroupContent.empty groupContent origin group world)
                        world groupsAdded
                world
            | None -> world
        else world

    ///
    let synchronizeGame setScreenSplash (contentOld : GameContent) (content : GameContent) (origin : Simulant) world =
        if contentOld <> content then
            let game = Simulants.Game
            let world = synchronizeEventSignals contentOld content origin game world
            let world = synchronizeEventHandlers contentOld content origin game world
            let world = synchronizeProperties contentOld content game world
            match tryDifferentiateChildren<Screen, ScreenContent> contentOld content game with
            | Some (screensAdded, screensRemoved, screensPotentiallyAltered) ->
                let world =
                    Seq.fold (fun world screen -> World.destroyScreen screen world) world screensRemoved
                let world =
                    Seq.fold (fun world (kvp : KeyValuePair<Screen, _>) ->
                        let (screen, screenContent) = (kvp.Key, kvp.Value)
                        let screenContentOld = contentOld.ScreenContents.[screen.Name]
                        synchronizeScreen screenContentOld screenContent origin screen world)
                        world screensPotentiallyAltered
                let world =
                    Seq.fold (fun world (screen : Screen, screenContent : ScreenContent) ->
                        let (screen, world) = World.createScreen3 screenContent.ScreenDispatcherName (Some screen.Name) world
                        let world = World.applyScreenBehavior setScreenSplash screenContent.ScreenBehavior screen world
                        synchronizeScreen ScreenContent.empty screenContent origin screen world)
                        world screensAdded
                (content.InitialScreenNameOpt |> Option.map Screen, world)
            | None -> (content.InitialScreenNameOpt |> Option.map Screen, world)
        else (content.InitialScreenNameOpt |> Option.map Screen, world)

    ///
    let composite<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName initializers entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        let mutable i = 0
        for property in initializers do
            match property with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((i, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pf -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pf
            i <- inc i
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
        { EntityDispatcherName = typeof<'entityDispatcher>.Name; EntityName = entityName; EntityCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt; EntityContentsOpt = entityContentsOpt }

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
    let private group4<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName groupFilePathOpt initializers entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((i, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pf -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pf
            i <- inc i
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
        { GroupDispatcherName = typeof<'groupDispatcher>.Name; GroupName = groupName; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt; EntityContentsOpt = entityContentsOpt }

    ///
    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName initializers entities =
        group4<'groupDispatcher> groupName None initializers entities

    ///
    let groupFromFile<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName filePath initializers entities =
        group4<'groupDispatcher> groupName (Some filePath) initializers entities

    ///
    let private screen5<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePathOpt initializers groups =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let groupContents = OrderedDictionary StringComparer.Ordinal
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((i, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pf -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pf
            i <- inc i
        for group in groups do
            groupContents.Add (group.GroupName, group)
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name; ScreenName = screenName; ScreenBehavior = screenBehavior; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt; GroupContents = groupContents }

    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior initializers groups =
        screen5<'screenDispatcher> screenName screenBehavior None initializers groups

    let screenWithGroupFromFile<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePath initializers groups =
        screen5<'screenDispatcher> screenName screenBehavior (Some groupFilePath) initializers groups

    ///
    let game initializers screens =
        let initialScreenNameOpt = match Seq.tryHead screens with Some screen -> Some screen.ScreenName | None -> None
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let screenContents = OrderedDictionary StringComparer.Ordinal
        let mutable i = 0
        for initializer in initializers do
            match initializer with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((i, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pf -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pf
            i <- inc i
        for screen in screens do
            screenContents.Add (screen.ScreenName, screen)
        { InitialScreenNameOpt = initialScreenNameOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt; ScreenContents = screenContents }

[<AutoOpen>]
module ContentOperators =

    /// Define an implicit property content.
    let inline (<==) (lens : Lens<'a, 's, World>) (value : 'a) : ContentInitializer =
        PropertyContent (PropertyContent.make (if notNull (lens.This :> obj) then Some (lens.This :> Simulant) else None) lens value)

    /// Define a signal content.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : ContentInitializer =
        EventSignalContent (Address.generalize eventAddress, signal)

    /// Define a signal handler content.
    let inline (==|>) (eventAddress : 'a Address) (callback : Event<'a, 's> -> Signal<'message, 'command>) : ContentInitializer =
        EventHandlerContent (PartialEquatable.make (Address.generalize eventAddress) (fun (evt : Event) -> callback (Event.specialize evt) :> obj))