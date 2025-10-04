// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Prime

/// MMCC content declaration API.
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
        private synchronizeNonEntityProperties initializing reinitializing (contentOld : SimulantContent) (content : SimulantContent) (simulant : Simulant) world =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let simulant = if notNull (contentOld.SimulantCachedOpt :> obj) then contentOld.SimulantCachedOpt else simulant
            content.SimulantCachedOpt <- simulant
            for propertyContent in content.PropertyContentsOpt do
                if (match propertyContent.PropertyType with
                    | InitializingProperty -> initializing
                    | ReinitializingProperty -> initializing || reinitializing
                    | DynamicProperty -> true) then
                    let lens = propertyContent.PropertyLens
                    match lens.This :> obj with
                    | null -> World.setProperty lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } simulant world |> ignore<bool>
                    | _ -> lens.TrySet propertyContent.PropertyValue world |> ignore<bool>

    let
#if !DEBUG
        inline
#endif
        private synchronizeEntityProperties (initializing, reinitializing, contentOld : EntityContent, content : EntityContent, entity : Entity, world, mountOptOpt : Entity Address option ValueOption outref) =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let entity = if notNull (contentOld.EntityCachedOpt :> obj) then contentOld.EntityCachedOpt else entity
            content.EntityCachedOpt <- entity
            let propertyContents = content.PropertyContentsOpt
            for i in 0 .. dec propertyContents.Count do
                let propertyContent = propertyContents.[i]
                let lens = propertyContent.PropertyLens
                if lens.Name = Constants.Engine.MountOptPropertyName then
                    mountOptOpt <- ValueSome (propertyContent.PropertyValue :?> Entity Address option)
                if (match propertyContent.PropertyType with
                    | InitializingProperty -> initializing
                    | ReinitializingProperty -> initializing || reinitializing
                    | DynamicProperty -> true) then
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
    let rec internal synchronizeEntity initializing reinitializing (contentOld : EntityContent) (content : EntityContent) (origin : Simulant) (entity : Entity) world =
        if contentOld =/= content then
            let mutable mountOptOpt = ValueNone
            synchronizeEventSignals contentOld content origin entity world
            synchronizeEventHandlers contentOld content origin entity world
            synchronizeEntityProperties (initializing, reinitializing, contentOld, content, entity, world, &mountOptOpt)
            if initializing then
                if mountOptOpt.IsNone && entity.Surnames.Length > 1 then
                    World.setEntityMountOpt (Some Address.parent) entity world |> ignore<bool>
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content entity with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                for entity in entitiesRemoved do
                    World.destroyEntity entity world
                if notNull contentOld.EntityContentsOpt then
                    for entry in entitiesPotentiallyAltered do
                        let entity = entry.Key
                        let entityContent = entry.Value
                        let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                        synchronizeEntity initializing reinitializing entityContentOld entityContent origin entity world
                for (entity : Entity, entityContent : EntityContent) in entitiesAdded do
                    if not (entity.GetExists world) || entity.GetDestroying world then
                        let mountOpt = match entityContent.MountOptOpt with ValueSome mountOpt -> mountOpt | ValueNone -> Some Address.parent
                        World.createEntity7 false entityContent.EntityDispatcherName mountOpt DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>
                    World.setEntityProtected true entity world |> ignore<bool>
                    synchronizeEntity true reinitializing EntityContent.empty entityContent origin entity world
            | None -> ()

    /// Synchronize a group and its contained simulants to the given content.
    let internal synchronizeGroup initializing reinitializing (contentOld : GroupContent) (content : GroupContent) (origin : Simulant) (group : Group) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin group world
            synchronizeEventHandlers contentOld content origin group world
            synchronizeNonEntityProperties initializing reinitializing contentOld content group world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content group with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                for entity in entitiesRemoved do
                    World.destroyEntity entity world
                if notNull contentOld.EntityContentsOpt then
                    for entry in entitiesPotentiallyAltered do
                        let entity = entry.Key
                        let entityContent = entry.Value
                        let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                        synchronizeEntity initializing reinitializing entityContentOld entityContent origin entity world
                for (entity : Entity, entityContent : EntityContent) in entitiesAdded do
                    if not (entity.GetExists world) || entity.GetDestroying world then
                        match entityContent.EntityFilePathOpt with
                        | Some entityFilePath -> World.readEntityFromFile false true entityFilePath (Some entity.Name) entity.Parent world |> ignore<Entity>
                        | None ->
                            let mountOpt = match entityContent.MountOptOpt with ValueSome mountOpt -> mountOpt | ValueNone -> Some Address.parent
                            World.createEntity7 false entityContent.EntityDispatcherName mountOpt DefaultOverlay (Some entity.Surnames) entity.Group world |> ignore<Entity>
                    World.setEntityProtected true entity world |> ignore<bool>
                    synchronizeEntity true reinitializing EntityContent.empty entityContent origin entity world
            | None -> ()

    /// Synchronize a screen and its contained simulants to the given content.
    let internal synchronizeScreen initializing reinitializing (contentOld : ScreenContent) (content : ScreenContent) (origin : Simulant) (screen : Screen) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin screen world
            synchronizeEventHandlers contentOld content origin screen world
            synchronizeNonEntityProperties initializing reinitializing contentOld content screen world
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
                    synchronizeGroup initializing reinitializing groupContentOld groupContent origin group world
                for (group : Group, groupContent : GroupContent) in groupsAdded do
                    if not (group.GetExists world) || group.GetDestroying world then
                        match groupContent.GroupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath (Some group.Name) screen world |> ignore<Group>
                        | None -> World.createGroup5 false groupContent.GroupDispatcherName (Some group.Name) group.Screen world |> ignore<Group>
                    World.setGroupProtected true group world |> ignore<bool>
                    synchronizeGroup true reinitializing GroupContent.empty groupContent origin group world
            | None -> ()

    /// Synchronize a screen and its contained simulants to the given content.
    let internal synchronizeGame setScreenSlide initializing reinitializing (contentOld : GameContent) (content : GameContent) (origin : Simulant) (game : Game) world =
        if contentOld =/= content then
            synchronizeEventSignals contentOld content origin game world
            synchronizeEventHandlers contentOld content origin game world
            synchronizeNonEntityProperties initializing reinitializing contentOld content game world
            match tryDifferentiateChildren<Screen, ScreenContent> contentOld content game with
            | Some (screensAdded, screensRemoved, screensPotentiallyAltered) ->
                for screen in screensRemoved do
                    World.destroyScreen screen world
                for entry in screensPotentiallyAltered do
                    let screen = entry.Key
                    let screenContent = entry.Value
                    let screenContentOld = contentOld.ScreenContents.[screen.Name]
                    synchronizeScreen initializing reinitializing screenContentOld screenContent origin screen world
                for (screen : Screen, screenContent : ScreenContent) in screensAdded do
                    if not (screen.GetExists world) || screen.GetDestroying world then
                        World.createScreen4 screenContent.ScreenDispatcherName (Some screen.Name) world |> ignore<Screen>
                    World.setScreenProtected true screen world |> ignore<bool>
                    World.applyScreenBehavior setScreenSlide screenContent.ScreenBehavior screen world
                    synchronizeScreen true reinitializing ScreenContent.empty screenContent origin screen world
                content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)
            | None -> content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)
        else content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name)

    /// Describe an entity with the given dispatcher type and definitions as well as its contained entities.
    let private composite4<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName entityFilePathOpt (definitions : Entity DefinitionContent seq) entities =
        Address.assertIdentifierName entityName
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

    /// Describe an entity loaded from the given file path with the given dispatcher type and definitions as well as its contained entities.
    let compositeFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath definitions entities =
        composite4<'entityDispatcher> entityName (Some filePath) definitions entities

    /// Describe an entity with the given dispatcher type and definitions.
    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName definitions =
        composite<'entityDispatcher> entityName definitions []

    /// Describe an entity loaded from the given file path with the given dispatcher type and definitions.
    let entityFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath definitions =
        compositeFromFile<'entityDispatcher> entityName filePath definitions []

    /// <summary>
    /// Describe a 2d effect with the given definitions.
    /// See <see cref="Effect2dDispatcher"/>.
    /// </summary>
    let effect2d entityName definitions = entity<Effect2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a static sprite with the given definitions.
    /// See <see cref="StaticSpriteDispatcher"/>.
    /// </summary>
    let staticSprite entityName definitions = entity<StaticSpriteDispatcher> entityName definitions

    /// <summary>
    /// Describe an animated sprite with the given definitions.
    /// See <see cref="AnimatedSpriteDispatcher"/>.
    /// </summary>
    let animatedSprite entityName definitions = entity<AnimatedSpriteDispatcher> entityName definitions

    /// <summary>
    /// Describe a basic static sprite emitter with the given definitions.
    /// See <see cref="BasicStaticSpriteEmitterDispatcher"/>.
    /// </summary>
    let basicStaticSpriteEmitter entityName definitions = entity<BasicStaticSpriteEmitterDispatcher> entityName definitions

    /// <summary>
    /// Describe an association of gui entities with the given definitions and content.
    /// See <see cref="GuiDispatcher"/>.
    /// </summary>
    let association entityName definitions content = composite<GuiDispatcher> entityName definitions content

    /// <summary>
    /// Describe a text entity with the given definitions.
    /// See <see cref="TextDispatcher"/>.
    /// </summary>
    let text entityName definitions = entity<TextDispatcher> entityName definitions

    /// <summary>
    /// Describe a label with the given definitions.
    /// See <see cref="LabelDispatcher"/>.
    /// </summary>
    let label entityName definitions = entity<LabelDispatcher> entityName definitions

    /// <summary>
    /// Describe a button with the given definitions.
    /// See <see cref="ButtonDispatcher"/>.
    /// </summary>
    let button entityName definitions = entity<ButtonDispatcher> entityName definitions

    /// <summary>
    /// Describe a toggle button with the given definitions.
    /// See <see cref="ToggleButtonDispatcher"/>.
    /// </summary>
    let toggleButton entityName definitions = entity<ToggleButtonDispatcher> entityName definitions

    /// <summary>
    /// Describe a radio button with the given definitions.
    /// See <see cref="RadioButtonDispatcher"/>.
    /// </summary>
    let radioButton entityName definitions = entity<RadioButtonDispatcher> entityName definitions

    /// <summary>
    /// Describe a fill bar with the given definitions.
    /// See <see cref="FillBarDispatcher"/>.
    /// </summary>
    let fillBar entityName definitions = entity<FillBarDispatcher> entityName definitions

    /// <summary>
    /// Describe a feeler with the given definitions.
    /// See <see cref="FeelerDispatcher"/>.
    /// </summary>
    let feeler entityName definitions = entity<FeelerDispatcher> entityName definitions

    /// <summary>
    /// Describe a text box entity with the given definitions.
    /// See <see cref="TextBoxDispatcher"/>.
    /// </summary>
    let textBox entityName definitions = entity<TextBoxDispatcher> entityName definitions

    /// <summary>
    /// Describe an fps entity with the given definitions.
    /// See <see cref="FpsDispatcher"/>.
    /// </summary>
    let fps entityName definitions = entity<FpsDispatcher> entityName definitions

    /// <summary>
    /// Describe a panel with the given definitions and content.
    /// See <see cref="PanelDispatcher"/>.
    /// </summary>
    let panel entityName definitions content = composite<PanelDispatcher> entityName definitions content

    /// <summary>
    /// Describe a cursor with the given definitions and content.
    /// See <see cref="CursorDispatcher"/>.
    /// </summary>
    let cursor entityName definitions content = composite<CursorDispatcher> entityName definitions content

    /// <summary>
    /// Describe a 2d block with the given definitions.
    /// See <see cref="Block2dDispatcher"/>.
    /// </summary>
    let block2d entityName definitions = entity<Block2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 2d box with the given definitions.
    /// See <see cref="Box2dDispatcher"/>.
    /// </summary>
    let box2d entityName definitions = entity<Box2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 2d sphere with the given definitions.
    /// See <see cref="Sphere2dDispatcher"/>.
    /// </summary>
    let sphere2d entityName definitions = entity<Sphere2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 2d ball with the given definitions.
    /// See <see cref="Ball2dDispatcher"/>.
    /// </summary>
    let ball2d entityName definitions = entity<Ball2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 2d character with the given definitions.
    /// See <see cref="Character2dDispatcher"/>.
    /// </summary>
    let character2d entityName definitions = entity<Character2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 2d body joint with the given definitions.
    /// See <see cref="BodyJoint2dDispatcher"/>.
    /// </summary>
    let bodyJoint2d entityName definitions = entity<BodyJoint2dDispatcher> entityName definitions

    /// <summary>
    /// Describe a tile map with the given definitions.
    /// See <see cref="TileMapDispatcher"/>.
    /// </summary>
    let tileMap entityName definitions = entity<TileMapDispatcher> entityName definitions

    /// <summary>
    /// Describe a tmx map with the given definitions.
    /// See <see cref="TmxMapDispatcher"/>.
    /// </summary>
    let tmxMap entityName definitions = entity<TmxMapDispatcher> entityName definitions

    /// <summary>
    /// Describe a Spine skeleton with the given definitions.
    /// See <see cref="SpineSkeletonDispatcher"/>.
    /// </summary>
    let spineSkeleton entityName definitions = entity<SpineSkeletonDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d light probe with the given definitions.
    /// See <see cref="LightProbe3dDispatcher"/>.
    /// </summary>
    let lightProbe3d entityName definitions = entity<LightProbe3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d light with the given definitions.
    /// See <see cref="Light3dDispatcher"/>.
    /// </summary>
    let light3d entityName definitions = entity<Light3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a sky box with the given definitions.
    /// See <see cref="SkyBoxDispatcher"/>.
    /// </summary>
    let skyBox entityName definitions = entity<SkyBoxDispatcher> entityName definitions

    /// <summary>
    /// Describe a basic static billboard emitter with the given definitions.
    /// See <see cref="BasicStaticBillboardEmitterDispatcher"/>.
    /// </summary>
    let basicStaticBillboardEmitter entityName definitions = entity<BasicStaticBillboardEmitterDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d effect with the given definitions.
    /// See <see cref="Effect3dDispatcher"/>.
    /// </summary>
    let effect3d entityName definitions = entity<Effect3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d block with the given definitions.
    /// See <see cref="Block3dDispatcher"/>.
    /// </summary>
    let block3d entityName definitions = entity<Block3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d box with the given definitions.
    /// See <see cref="Box3dDispatcher"/>.
    /// </summary>
    let box3d entityName definitions = entity<Box3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d sphere with the given definitions.
    /// See <see cref="Sphere3dDispatcher"/>.
    /// </summary>
    let sphere3d entityName definitions = entity<Sphere3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d ball with the given definitions.
    /// See <see cref="Ball3dDispatcher"/>.
    /// </summary>
    let ball3d entityName definitions = entity<Ball3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a static billboard with the given definitions.
    /// See <see cref="StaticBillboardDispatcher"/>.
    /// </summary>
    let staticBillboard entityName definitions = entity<StaticBillboardDispatcher> entityName definitions

    /// <summary>
    /// Describe an animated billboard with the given definitions.
    /// See <see cref="AnimatedBillboardDispatcher"/>.
    /// </summary>
    let animatedBillboard entityName definitions = entity<AnimatedBillboardDispatcher> entityName definitions

    /// <summary>
    /// Describe a static model with the given definitions.
    /// See <see cref="StaticModelDispatcher"/>.
    /// </summary>
    let staticModel entityName definitions = entity<StaticModelDispatcher> entityName definitions

    /// <summary>
    /// Describe an animated model with the given definitions.
    /// See <see cref="AnimatedModelDispatcher"/>.
    /// </summary>
    let animatedModel entityName definitions = entity<AnimatedModelDispatcher> entityName definitions

    /// <summary>
    /// Describe a sensor model with the given definitions.
    /// See <see cref="SensorModelDispatcher"/>.
    /// </summary>
    let sensorModel entityName definitions = entity<SensorModelDispatcher> entityName definitions

    /// <summary>
    /// Describe a rigid model with the given definitions.
    /// See <see cref="RigidModelDispatcher"/>.
    /// </summary>
    let rigidModel entityName definitions = entity<RigidModelDispatcher> entityName definitions

    /// <summary>
    /// Describe a static model surface with the given definitions.
    /// See <see cref="StaticModelSurfaceDispatcher"/>.
    /// </summary>
    let staticModelSurface entityName definitions = entity<StaticModelSurfaceDispatcher> entityName definitions

    /// <summary>
    /// Describe a sensor model surface with the given definitions.
    /// See <see cref="SensorModelSurfaceDispatcher"/>.
    /// </summary>
    let sensorModelSurface entityName definitions = entity<SensorModelSurfaceDispatcher> entityName definitions

    /// <summary>
    /// Describe a rigid model surface with the given definitions.
    /// See <see cref="RigidModelSurfaceDispatcher"/>.
    /// </summary>
    let rigidModelSurface entityName definitions = entity<RigidModelSurfaceDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d character with the given definitions.
    /// See <see cref="Character3dDispatcher"/>.
    /// </summary>
    let character3d entityName definitions = entity<Character3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d body joint with the given definitions.
    /// See <see cref="BodyJoint3dDispatcher"/>.
    /// </summary>
    let bodyJoint3d entityName definitions = entity<BodyJoint3dDispatcher> entityName definitions

    /// <summary>
    /// Describe a terrain with the given definitions.
    /// See <see cref="TerrainDispatcher"/>.
    /// </summary>
    let terrain entityName definitions = entity<TerrainDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d navigation configuration.
    /// See <see cref="Nav3dConfigDispatcher"/>.
    /// </summary>
    let nav3dConfig entityName definitions = entity<Nav3dConfigDispatcher> entityName definitions

    /// <summary>
    /// Describe a 3d lighting configuration.
    /// See <see cref="Lighting3dConfigDispatcher"/>.
    /// </summary>
    let lighting3dConfig entityName definitions = entity<Lighting3dConfigDispatcher> entityName definitions

    /// <summary>
    /// Describe a static model expanded into an entity hierarchy with the given definitions.
    /// See <see cref="StaticModelHierarchyDispatcher"/>.
    /// </summary>
    let staticModelHierarchy entityName definitions = entity<StaticModelHierarchyDispatcher> entityName definitions

    /// <summary>
    /// Describe a rigid model expanded into an entity hierarchy with the given definitions.
    /// See <see cref="RigidModelHierarchyDispatcher"/>.
    /// </summary>
    let rigidModelHierarchy entityName definitions = entity<RigidModelHierarchyDispatcher> entityName definitions

    /// Describe a group with the given dispatcher type and definitions as well as its contained entities.
    let private group4<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName groupFilePathOpt (definitions : Group DefinitionContent seq) entities =
        Address.assertIdentifierName groupName
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
    let private screen5<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePathOpt (definitions : Screen DefinitionContent seq) groups =
        Address.assertIdentifierName screenName
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
    let game definitions screens =
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

/// MMCC content operators.
[<AutoOpen>]
module ContentOperators =

    /// Define a static property equality that doesn't reinitialize on code reload.
    let
#if !DEBUG
        inline
#endif
        (!=) (lens : Lens<'a, 's>) (value : 'a) : 's DefinitionContent =
        PropertyContent (PropertyContent.make InitializingProperty lens value)

    /// Define a static property equality that reinitializes on code reload.
    let
#if !DEBUG
        inline
#endif
        (==) (lens : Lens<'a, 's>) (value : 'a) : 's DefinitionContent =
        PropertyContent (PropertyContent.make ReinitializingProperty lens value)

    /// Define a dynamic property equality.
    let
#if !DEBUG
        inline
#endif
        (:=) (lens : Lens<'a, 's>) (value : 'a) : 's DefinitionContent =
        PropertyContent (PropertyContent.make DynamicProperty lens value)

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