﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Prime

[<RequireQualifiedAccess>]
module Content =

    let private ContentsCached = Dictionary<string, struct (obj * obj)> StringComparer.Ordinal

    /// Helps to track when content bound to event handlers needs to be updated due to LateBindings changing, such as
    /// via code reloading.
    let mutable internal UpdateLateBindingsCount = 0

    // NOTE: extracted from Content.synchronizeEventHandlers to shorten stack trace.
    let [<DebuggerHidden>] private signalHandler signalObj origin =
        fun (_ : Event) world ->
            WorldModule.signal signalObj origin world
            Cascade

    // NOTE: extracted from Content.synchronizeEventHandlers to shorten stack trace.
    let [<DebuggerHidden>] private signalHandlerHandler handler origin =
        fun event world ->
            WorldModule.signal (handler event) origin world
            Cascade

    let
#if !DEBUG
        inline
#endif
        private synchronizeEventSignals (contentOld : SimulantContent) (content : SimulantContent) (origin : Simulant) (simulant : Simulant) world =
        if notNull contentOld.EventSignalContentsOpt || notNull content.EventSignalContentsOpt then
            let eventSignalContentsOld = if isNull contentOld.EventSignalContentsOpt then OrderedDictionary HashIdentity.Structural else contentOld.EventSignalContentsOpt
            let eventSignalContents = if isNull content.EventSignalContentsOpt then OrderedDictionary HashIdentity.Structural else content.EventSignalContentsOpt
            if eventSignalContentsOld.Count > 0 || eventSignalContents.Count > 0 then

                // compute deltas
                let eventSignalsAdded = List ()
                for eventSignalEntry in eventSignalContents do
                    if not (eventSignalContentsOld.ContainsKey eventSignalEntry.Key) then
                        eventSignalsAdded.Add (eventSignalEntry.Key, eventSignalEntry.Value)
                let eventSignalsRemoved = List ()
                for eventSignalEntry in eventSignalContentsOld do
                    if not (eventSignalContents.ContainsKey eventSignalEntry.Key) then
                        eventSignalsRemoved.Add eventSignalEntry.Value

                // unsubscribe to removed events
                for subscriptionId in eventSignalsRemoved do
                    World.unsubscribe subscriptionId world

                // subscribe to added events
                for ((eventAddress : obj Address, signalObj), subscriptionId) in eventSignalsAdded do
                    let eventAddress = if eventAddress.Anonymous then eventAddress --> itoa simulant.SimulantAddress else eventAddress
                    let unsubscribe = World.subscribePlus subscriptionId (signalHandler signalObj origin) eventAddress origin world
                    World.monitor
                        (fun _ world -> unsubscribe world; Cascade)
                        (Events.UnregisteringEvent --> itoa simulant.SimulantAddress)
                        simulant
                        world

                // drag event signals with existing subscription ids forward in time
                for eventSignalEntry in eventSignalContentsOld do
                    if eventSignalContents.ContainsKey eventSignalEntry.Key then
                        eventSignalContents.[eventSignalEntry.Key] <- eventSignalEntry.Value

    let
#if !DEBUG
        inline
#endif
        private synchronizeEventHandlers (contentOld : SimulantContent) (content : SimulantContent) (origin : Simulant) (simulant : Simulant) world =
        if notNull contentOld.EventHandlerContentsOpt || notNull content.EventHandlerContentsOpt then
            let eventHandlerContentsOld = if isNull contentOld.EventHandlerContentsOpt then OrderedDictionary HashIdentity.Structural else contentOld.EventHandlerContentsOpt
            let eventHandlerContents = if isNull content.EventHandlerContentsOpt then OrderedDictionary HashIdentity.Structural else content.EventHandlerContentsOpt
            if eventHandlerContentsOld.Count > 0 || eventHandlerContents.Count > 0 then

                // compute event handler deltas
                let eventHandlersAdded = List ()
                for eventHandlerEntry in eventHandlerContents do
                    if not (eventHandlerContentsOld.ContainsKey eventHandlerEntry.Key) then
                        eventHandlersAdded.Add (eventHandlerEntry.Key, eventHandlerEntry.Value)
                let eventHandlersRemoved = List ()
                for eventHandlerEntry in eventHandlerContentsOld do
                    if not (eventHandlerContents.ContainsKey eventHandlerEntry.Key) then
                        eventHandlersRemoved.Add eventHandlerEntry.Value

                // unsubscribe from removed handlers
                for (subscriptionId, _) in eventHandlersRemoved do
                    World.unsubscribe subscriptionId world

                // subscribe to added handlers
                for ((_, eventAddress : obj Address), (subscriptionId, handler)) in eventHandlersAdded do
                    let eventAddress = if eventAddress.Anonymous then eventAddress --> itoa simulant.SimulantAddress else eventAddress
                    let unsubscribe = World.subscribePlus subscriptionId (signalHandlerHandler handler origin) eventAddress origin world
                    World.monitor
                        (fun _ world -> unsubscribe world; Cascade)
                        (Events.UnregisteringEvent --> itoa simulant.SimulantAddress)
                        simulant
                        world

                // drag event signals with existing subscription ids forward in time
                for eventHandlerEntry in eventHandlerContentsOld do
                    if eventHandlerContents.ContainsKey eventHandlerEntry.Key then
                        eventHandlerContents.[eventHandlerEntry.Key] <- eventHandlerEntry.Value

    let
#if !DEBUG
        inline
#endif
        private synchronizeProperties initializing (contentOld : SimulantContent) (content : SimulantContent) (simulant : Simulant) world =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let simulant = if notNull (contentOld.SimulantCachedOpt :> obj) then contentOld.SimulantCachedOpt else simulant
            content.SimulantCachedOpt <- simulant
            for propertyContent in content.PropertyContentsOpt do
                if not propertyContent.PropertyStatic || initializing then
                    let lens = propertyContent.PropertyLens
                    match lens.This :> obj with
                    | null -> World.setProperty lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } simulant world |> ignore<bool>
                    | _ -> lens.TrySet propertyContent.PropertyValue world |> ignore<bool>

    let
#if !DEBUG
        inline
#endif
        private synchronizeEntityPropertiesFast (initializing, contentOld : EntityContent, content : EntityContent, entity : Entity, world, mountOptFound : bool outref) =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let entity = if notNull (contentOld.EntityCachedOpt :> obj) then contentOld.EntityCachedOpt else entity
            content.EntityCachedOpt <- entity
            let propertyContents = content.PropertyContentsOpt
            for i in 0 .. dec propertyContents.Count do
                let propertyContent = propertyContents.[i]
                if not propertyContent.PropertyStatic || initializing then
                    let lens = propertyContent.PropertyLens
                    if strEq lens.Name Constants.Engine.MountOptPropertyName then mountOptFound <- true
                    match lens.This :> obj with
                    | null -> World.setEntityPropertyFast lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } entity world
                    | _ -> lens.TrySet propertyContent.PropertyValue world |> ignore<bool>
            content.PropertyContentsOpt <- null // OPTIMIZATION: blank out property contents to avoid GC promotion.

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
                    | (true, childContentOld) when optEq childEntry.Value.DispatcherNameOpt childContentOld.DispatcherNameOpt ->
                        let childSimulant = // OPTIMIZATION: attempt to get child simulant from old content rather than deriving it, and store it for future use.
                            if isNull (childContentOld.SimulantCachedOpt :> obj) then
                                let derived = World.deriveFromNames (Array.add childEntry.Key simulant.SimulantAddress.Names) :?> 'child
                                childEntry.Value.SimulantCachedOpt <- derived
                                derived
                            else
                                let found = childContentOld.SimulantCachedOpt :?> 'child
                                childEntry.Value.SimulantCachedOpt <- found
                                found
                        childrenPotentiallyAltered.Add (childSimulant, childEntry.Value)
                    | (_, _) ->
                        let childSimulant = World.deriveFromNames (Array.add childEntry.Key simulant.SimulantAddress.Names) :?> 'child
                        childEntry.Value.SimulantCachedOpt <- childSimulant
                        childrenAdded.Add (childSimulant, childEntry.Value)
                let childrenRemoved = List<'child> ()
                for childEntryOld in childContentsOld do
                    match childContents.TryGetValue childEntryOld.Key with
                    | (true, childContentOld) when optEq childEntryOld.Value.DispatcherNameOpt childContentOld.DispatcherNameOpt -> ()
                    | (_, _) ->
                        let childSimulant = childEntryOld.Value.SimulantCachedOpt :?> 'child // OPTIMIZATION: because of above optimization, should be guaranteed to exist.
                        childrenRemoved.Add childSimulant
                        childrenPotentiallyAltered.Remove childSimulant |> ignore
                Some (childrenAdded, childrenRemoved, childrenPotentiallyAltered)
            else None
        else None

    /// Synchronize an entity and its contained simulants to the given content.
    let rec internal synchronizeEntity initializing (contentOld : EntityContent) (content : EntityContent) (origin : Simulant) (entity : Entity) world =
        if contentOld =/= content then
            let mutable mountOptFound = false
            synchronizeEventSignals contentOld content origin entity world
            synchronizeEventHandlers contentOld content origin entity world
            synchronizeEntityPropertiesFast (initializing, contentOld, content, entity, world, &mountOptFound)
            if initializing then
                if not mountOptFound && entity.Surnames.Length > 1 then
                    World.setEntityMountOpt (Some (Relation.makeParent ())) entity world |> ignore<bool>
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content entity with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                for entity in entitiesRemoved do
                    World.destroyEntity entity world
                if notNull contentOld.EntityContentsOpt then
                    for entry in entitiesPotentiallyAltered do
                        let entity = entry.Key
                        let entityContent = entry.Value
                        let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                        synchronizeEntity initializing entityContentOld entityContent origin entity world
                for (entity : Entity, entityContent : EntityContent) in entitiesAdded do
                    if not (entity.GetExists world) || entity.GetDestroying world then
                        World.createEntity6 false entityContent.EntityDispatcherName DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>
                    World.setEntityProtected true entity world |> ignore<bool>
                    synchronizeEntity true EntityContent.empty entityContent origin entity world
            | None -> ()

    /// Synchronize a group and its contained simulants to the given content.
    let internal synchronizeGroup initializing (contentOld : GroupContent) (content : GroupContent) (origin : Simulant) (group : Group) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin group world
            synchronizeEventHandlers contentOld content origin group world
            synchronizeProperties initializing contentOld content group world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content group with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                for entity in entitiesRemoved do
                    World.destroyEntity entity world
                if notNull contentOld.EntityContentsOpt then
                    for entry in entitiesPotentiallyAltered do
                        let entity = entry.Key
                        let entityContent = entry.Value
                        let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                        synchronizeEntity initializing entityContentOld entityContent origin entity world
                for (entity : Entity, entityContent : EntityContent) in entitiesAdded do
                    if not (entity.GetExists world) || entity.GetDestroying world then
                        match entityContent.EntityFilePathOpt with
                        | Some entityFilePath -> World.readEntityFromFile false true entityFilePath (Some entity.Name) entity.Parent world |> ignore<Entity>
                        | None -> World.createEntity6 false entityContent.EntityDispatcherName DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>
                    World.setEntityProtected true entity world |> ignore<bool>
                    synchronizeEntity true EntityContent.empty entityContent origin entity world
            | None -> ()

    /// Synchronize a screen and its contained simulants to the given content.
    let internal synchronizeScreen initializing (contentOld : ScreenContent) (content : ScreenContent) (origin : Simulant) (screen : Screen) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin screen world
            synchronizeEventHandlers contentOld content origin screen world
            synchronizeProperties initializing contentOld content screen world
            if contentOld.GroupFilePathOpt =/= content.GroupFilePathOpt then
                match contentOld.GroupFilePathOpt with
                | Some groupFilePath ->
                    // NOTE: have to load the group file just get the name of the group to destroy...
                    let groupDescriptorStr = File.ReadAllText groupFilePath
                    let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                    let groupName =
                        Constants.Engine.NamePropertyName
                        |> groupDescriptor.GroupProperties.TryFind
                        |> Option.mapOrDefaultValue symbolToValue "GroupFromFile" // TODO: make constant?
                    let group = screen / groupName
                    World.destroyGroup group world
                | None -> ()
                match content.GroupFilePathOpt with
                | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> ignore<Group>
                | None -> ()
            match tryDifferentiateChildren<Group, GroupContent> contentOld content screen with
            | Some (groupsAdded, groupsRemoved, groupsPotentiallyAltered) ->
                for group in groupsRemoved do
                    World.destroyGroup group world
                for entry in groupsPotentiallyAltered do
                    let group = entry.Key
                    let groupContent = entry.Value
                    let groupContentOld = contentOld.GroupContents.[group.Name]
                    synchronizeGroup initializing groupContentOld groupContent origin group world
                for (group : Group, groupContent : GroupContent) in groupsAdded do
                    if not (group.GetExists world) || group.GetDestroying world then
                        match groupContent.GroupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some group.Name) screen world |> ignore<Group>
                        | None -> World.createGroup5 false groupContent.GroupDispatcherName (Some group.Name) group.Screen world |> ignore<Group>
                    World.setGroupProtected true group world |> ignore<bool>
                    synchronizeGroup true GroupContent.empty groupContent origin group world
            | None -> ()

    /// Synchronize a screen and its contained simulants to the given content.
    let internal synchronizeGame setScreenSlide initializing (contentOld : GameContent) (content : GameContent) (origin : Simulant) (game : Game) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin game world
            synchronizeEventHandlers contentOld content origin game world
            synchronizeProperties initializing contentOld content game world
            match tryDifferentiateChildren<Screen, ScreenContent> contentOld content game with
            | Some (screensAdded, screensRemoved, screensPotentiallyAltered) ->
                for screen in screensRemoved do
                    World.destroyScreen screen world
                for entry in screensPotentiallyAltered do
                    let screen = entry.Key
                    let screenContent = entry.Value
                    let screenContentOld = contentOld.ScreenContents.[screen.Name]
                    synchronizeScreen initializing screenContentOld screenContent origin screen world
                for (screen : Screen, screenContent : ScreenContent) in screensAdded do
                    if not (screen.GetExists world) || screen.GetDestroying world then
                        World.createScreen4 screenContent.ScreenDispatcherName (Some screen.Name) world |> ignore<Screen>
                    World.setScreenProtected true screen world |> ignore<bool>
                    World.applyScreenBehavior setScreenSlide screenContent.ScreenBehavior screen world
                    synchronizeScreen true ScreenContent.empty screenContent origin screen world
                content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)
            | None -> content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)
        else content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)

    /// Describe an entity with the given dispatcher type and definitions as well as its contained entities.
    let private composite4<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName entityFilePathOpt (definitions : Entity DefinitionContent seq) entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        for definition in definitions do
            match definition with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), Gen.id64)
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (Gen.id64, ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
#if DEBUG
        if notNull entityContentsOpt && entityContentsOpt.Count > 2048 then // probably indicates a 4096 24-bit Dictionary.Entry array on the LOH
            Log.warnOnce "High MMCC entity content count: having a large number of MMCC entities (> 2048) in a single entity parent may thrash the LOH."
#endif
        { EntityDispatcherName = typeof<'entityDispatcher>.Name; EntityName = entityName; EntityFilePathOpt = entityFilePathOpt; EntityCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          EntityContentsOpt = entityContentsOpt }

    /// Describe an entity with the given dispatcher type and definitions as well as its contained entities.
    let composite<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName definitions entities =
        composite4<'entityDispatcher> entityName None definitions entities

    /// Describe an entity with the given dispatcher type and definitions as well as its contained entities.
    let compositeFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath definitions entities =
        composite4<'entityDispatcher> entityName (Some filePath) definitions entities

    /// Describe an entity with the given dispatcher type and definitions.
    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName definitions =
        composite<'entityDispatcher> entityName definitions []

    /// Describe an entity with the given dispatcher type and definitions.
    let entityFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath definitions =
        compositeFromFile<'entityDispatcher> entityName filePath definitions []

    /// Describe a 2d effect with the given definitions.
    let effect2d entityName definitions = entity<Effect2dDispatcher> entityName definitions

    /// Describe a static sprite with the given definitions.
    let staticSprite entityName definitions = entity<StaticSpriteDispatcher> entityName definitions

    /// Describe an animated sprite with the given definitions.
    let animatedSprite entityName definitions = entity<AnimatedSpriteDispatcher> entityName definitions

    /// Describe a basic static sprite emitter with the given definitions.
    let basicStaticSpriteEmitter entityName definitions = entity<BasicStaticSpriteEmitterDispatcher> entityName definitions

    /// Describe an association of gui entities with the given definitions and content.
    let association entityName definitions content = composite<GuiDispatcher> entityName definitions content

    /// Describe a text entity with the given definitions.
    let text entityName definitions = entity<TextDispatcher> entityName definitions

    /// Describe a label with the given definitions.
    let label entityName definitions = entity<LabelDispatcher> entityName definitions

    /// Describe a button with the given definitions.
    let button entityName definitions = entity<ButtonDispatcher> entityName definitions

    /// Describe a toggle button with the given definitions.
    let toggleButton entityName definitions = entity<ToggleButtonDispatcher> entityName definitions

    /// Describe a radio button with the given definitions.
    let radioButton entityName definitions = entity<RadioButtonDispatcher> entityName definitions

    /// Describe a fill bar with the given definitions.
    let fillBar entityName definitions = entity<FillBarDispatcher> entityName definitions

    /// Describe a feeler with the given definitions.
    let feeler entityName definitions = entity<FeelerDispatcher> entityName definitions

    /// Describe a text box entity with the given definitions.
    let textBox entityName definitions = entity<TextBoxDispatcher> entityName definitions

    /// Describe an fps entity with the given definitions.
    let fps entityName definitions = entity<FpsDispatcher> entityName definitions

    /// Describe a panel with the given definitions and content.
    let panel entityName definitions content = composite<PanelDispatcher> entityName definitions content

    /// Describe a 2d block with the given definitions.
    let block2d entityName definitions = entity<Block2dDispatcher> entityName definitions

    /// Describe a 2d box with the given definitions.
    let box2d entityName definitions = entity<Box2dDispatcher> entityName definitions

    /// Describe a 2d sphere with the given definitions.
    let sphere2d entityName definitions = entity<Sphere2dDispatcher> entityName definitions

    /// Describe a 2d ball with the given definitions.
    let ball2d entityName definitions = entity<Ball2dDispatcher> entityName definitions

    /// Describe a 2d character with the given definitions.
    let character2d entityName definitions = entity<Character2dDispatcher> entityName definitions

    /// Describe a 2d body joint with the given definitions.
    let bodyJoint2d entityName definitions = entity<BodyJoint2dDispatcher> entityName definitions

    /// Describe a tile map with the given definitions.
    let tileMap entityName definitions = entity<TileMapDispatcher> entityName definitions

    /// Describe a tmx map with the given definitions.
    let tmxMap entityName definitions = entity<TmxMapDispatcher> entityName definitions

    /// Describe a Spine skeleton with the given definitions.
    let spineSkeleton entityName definitions = entity<SpineSkeletonDispatcher> entityName definitions

    /// Describe a 3d light probe with the given definitions.
    let lightProbe3d entityName definitions = entity<LightProbe3dDispatcher> entityName definitions

    /// Describe a 3d light with the given definitions.
    let light3d entityName definitions = entity<Light3dDispatcher> entityName definitions

    /// Describe a sky box with the given definitions.
    let skyBox entityName definitions = entity<SkyBoxDispatcher> entityName definitions

    /// Describe a basic static billboard emitter with the given definitions.
    let basicStaticBillboardEmitter entityName definitions = entity<BasicStaticBillboardEmitterDispatcher> entityName definitions

    /// Describe a 3d effect with the given definitions.
    let effect3d entityName definitions = entity<Effect3dDispatcher> entityName definitions

    /// Describe a 3d block with the given definitions.
    let block3d entityName definitions = entity<Block3dDispatcher> entityName definitions

    /// Describe a 3d box with the given definitions.
    let box3d entityName definitions = entity<Box3dDispatcher> entityName definitions

    /// Describe a 3d sphere with the given definitions.
    let sphere3d entityName definitions = entity<Sphere3dDispatcher> entityName definitions

    /// Describe a 3d ball with the given definitions.
    let ball3d entityName definitions = entity<Ball3dDispatcher> entityName definitions

    /// Describe a static billboard with the given definitions.
    let staticBillboard entityName definitions = entity<StaticBillboardDispatcher> entityName definitions

    /// Describe an animated billboard with the given definitions.
    let animatedBillboard entityName definitions = entity<AnimatedBillboardDispatcher> entityName definitions

    /// Describe a static model with the given definitions.
    let staticModel entityName definitions = entity<StaticModelDispatcher> entityName definitions

    /// Describe an animated model with the given definitions.
    let animatedModel entityName definitions = entity<AnimatedModelDispatcher> entityName definitions

    /// Describe a sensor model with the given definitions.
    let sensorModel entityName definitions = entity<SensorModelDispatcher> entityName definitions

    /// Describe a rigid model with the given definitions.
    let rigidModel entityName definitions = entity<RigidModelDispatcher> entityName definitions

    /// Describe a static model surface with the given definitions.
    let staticModelSurface entityName definitions = entity<StaticModelSurfaceDispatcher> entityName definitions

    /// Describe a sensor model surface with the given definitions.
    let sensorModelSurface entityName definitions = entity<SensorModelSurfaceDispatcher> entityName definitions

    /// Describe a rigid model surface with the given definitions.
    let rigidModelSurface entityName definitions = entity<RigidModelSurfaceDispatcher> entityName definitions

    /// Describe a 3d character with the given definitions.
    let character3d entityName definitions = entity<Character3dDispatcher> entityName definitions

    /// Describe a 3d body joint with the given definitions.
    let bodyJoint3d entityName definitions = entity<BodyJoint3dDispatcher> entityName definitions

    /// Describe a terrain with the given definitions.
    let terrain entityName definitions = entity<TerrainDispatcher> entityName definitions

    /// Describe a 3d navigation configuration.
    let nav3dConfig entityName definitions = entity<Nav3dConfigDispatcher> entityName definitions

    /// Describe a 3d lighting configuration.
    let lighting3dConfig entityName definitions = entity<Lighting3dConfigDispatcher> entityName definitions

    /// Describe a static model expanded into an entity hierarchy with the given definitions.
    let staticModelHierarchy entityName definitions = entity<StaticModelHierarchyDispatcher> entityName definitions

    /// Describe a rigid model expanded into an entity hierarchy with the given definitions.
    let rigidModelHierarchy entityName definitions = entity<RigidModelHierarchyDispatcher> entityName definitions

    /// Describe a group with the given dispatcher type and definitions as well as its contained entities.
    let private group4<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName groupFilePathOpt (definitions : Group DefinitionContent seq)  entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        for definition in definitions do
            match definition with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), Gen.id64)
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (Gen.id64, ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
#if DEBUG
        if notNull entityContentsOpt && entityContentsOpt.Count > 2048 then // probably indicates a 4096 24-bit Dictionary.Entry array on the LOH
            Log.warnOnce "High MMCC entity content count: having a large number of MMCC entities (> 2048) in a single group may thrash the LOH."
#endif
        { GroupDispatcherName = typeof<'groupDispatcher>.Name; GroupName = groupName; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          EntityContentsOpt = entityContentsOpt }

    /// Describe a group with the given dispatcher type and definitions as well as its contained entities.
    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName definitions entities =
        group4<'groupDispatcher> groupName None definitions entities

    /// Describe a group and its contained entities loaded from the given file with the given definitions.
    let groupFromFile<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName filePath definitions entities =
        group4<'groupDispatcher> groupName (Some filePath) definitions entities

    /// Describe a screen with the given dispatcher type and definitions as well as its contained simulants.
    let private screen5<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePathOpt (definitions : Screen DefinitionContent seq)  groups =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let groupContents = OrderedDictionary StringComparer.Ordinal
        for definition in definitions do
            match definition with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), Gen.id64)
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (Gen.id64, ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for group in groups do
            groupContents.Add (group.GroupName, group)
#if DEBUG
        if groupContents.Count > 2048 then // probably indicates a 4096 24-bit Dictionary.Entry array on the LOH
            Log.warnOnce "High MMCC group content count: having a large number of MMCC groups (> 2048) in a single screen may thrash the LOH."
#endif
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name; ScreenName = screenName; ScreenBehavior = screenBehavior; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          GroupContents = groupContents }

    /// Describe a screen with the given dispatcher type and definitions as well as its contained simulants.
    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior definitions groups =
        screen5<'screenDispatcher> screenName screenBehavior None definitions groups

    /// Describe a screen with the given type and definitions with a group loaded from the given file.
    let screenWithGroupFromFile<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePath definitions groups =
        screen5<'screenDispatcher> screenName screenBehavior (Some groupFilePath) definitions groups

    /// Describe a game with the given definitions as well as its contained simulants.
    let game gameName definitions screens =
        ignore<string> gameName
        let initialScreenNameOpt = match Seq.tryHead screens with Some screen -> Some screen.ScreenName | None -> None
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let screenContents = OrderedDictionary StringComparer.Ordinal
        for definition in definitions do
            match definition with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), Gen.id64)
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (Gen.id64, ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for screen in screens do
            screenContents.Add (screen.ScreenName, screen)
#if DEBUG
        if screenContents.Count > 2048 then // probably indicates a 4096 24-bit Dictionary.Entry array on the LOH
            Log.warnOnce "High MMCC scrren content count: having a large number of MMCC screens (> 2048) in a single game may thrash the LOH."
#endif
        { InitialScreenNameOpt = initialScreenNameOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          ScreenContents = screenContents }

    /// Cache named content. Name must be globally unique!
    let cache<'v, 'c> name (value : 'v) (fn : 'v -> 'c) : 'c =
        match ContentsCached.TryGetValue name with
        | (true, struct (v, (:? 'c as content))) when v === value -> content
        | (_, _) ->
            let content = fn value
            ContentsCached.[name] <- struct (value, content)
            content

    /// Discard cached content.
    let wipe () =
        ContentsCached.Clear ()

[<AutoOpen>]
module ContentOperators =

    /// Define a static property equality.
    let
#if !DEBUG
        inline
#endif
        (==) (lens : Lens<'a, 's>) (value : 'a) : 's DefinitionContent =
        PropertyContent (PropertyContent.make true lens value)

    /// Define a dynamic property equality.
    let
#if !DEBUG
        inline
#endif
        (:=) (lens : Lens<'a, 's>) (value : 'a) : 's DefinitionContent =
        PropertyContent (PropertyContent.make false lens value)

    /// Define an event signal.
    let
#if !DEBUG
        inline
#endif
        (=>) (eventAddress : 'a Address) (signal : Signal) : 's DefinitionContent =
        EventSignalContent (Address.generalize eventAddress, signal)

    /// Define an event handler.
    let
#if !DEBUG
        inline
#endif
        (=|>) (eventAddress : 'a Address) (callback : Event<'a, #Simulant> -> Signal) : 's DefinitionContent =
        EventHandlerContent (PartialEquatable.make (Address.generalize eventAddress) (fun (evt : Event) -> callback (Event.specialize evt) :> obj))