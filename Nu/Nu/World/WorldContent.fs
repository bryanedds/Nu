// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Prime

[<RequireQualifiedAccess>]
module Content =

    /// Helps to track when content bound to event handlers needs to be updated due to LateBindings changing, such as
    /// via code reloading.
    let mutable internal UpdateLateBindingsCount = 0

    // NOTE: extracted from Content.synchronizeEventHandlers to shorten stack trace.
    let [<DebuggerHidden>] private signalHandler signalObj origin =
        fun (_ : Event) world ->
            let world = WorldModule.signal signalObj origin world
            (Cascade, world)

    // NOTE: extracted from Content.synchronizeEventHandlers to shorten stack trace.
    let [<DebuggerHidden>] private signalHandlerHandler handler origin =
        fun event world ->
            let world = WorldModule.signal (handler event) origin world
            (Cascade, world)

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
                let world =
                    List.foldGeneric
                        (fun world subscriptionId -> World.unsubscribe subscriptionId world)
                        world eventSignalsRemoved

                // subscribe to added events
                let world =
                    List.foldGeneric (fun world ((eventAddress : obj Address, signalObj), subscriptionId) ->
                        let eventAddress = if eventAddress.Anonymous then eventAddress --> simulant.SimulantAddress else eventAddress
                        let (unsubscribe, world) = World.subscribePlus subscriptionId (signalHandler signalObj origin) eventAddress origin world
                        let world =
                            World.monitor
                                (fun _ world -> (Cascade, unsubscribe world))
                                (Events.UnregisteringEvent --> simulant.SimulantAddress)
                                simulant
                                world
                        world)
                        world eventSignalsAdded

                /// drag event signals with existing subscription ids forward in time
                for eventSignalEntry in eventSignalContentsOld do
                    if eventSignalContents.ContainsKey eventSignalEntry.Key then
                        eventSignalContents.[eventSignalEntry.Key] <- eventSignalEntry.Value

                // fin
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
                let world =
                    List.foldGeneric
                        (fun world (subscriptionId, _) -> World.unsubscribe subscriptionId world)
                        world eventHandlersRemoved

                // subscribe to added handlers
                let world =
                    List.foldGeneric (fun world ((_, eventAddress : obj Address), (subscriptionId, handler)) ->
                        let eventAddress = if eventAddress.Anonymous then eventAddress --> simulant.SimulantAddress else eventAddress
                        let (unsubscribe, world) = World.subscribePlus subscriptionId (signalHandlerHandler handler origin) eventAddress origin world
                        let world =
                            World.monitor
                                (fun _ world -> (Cascade, unsubscribe world))
                                (Events.UnregisteringEvent --> simulant.SimulantAddress)
                                simulant
                                world
                        world)
                        world eventHandlersAdded

                /// drag event signals with existing subscription ids forward in time
                for eventHandlerEntry in eventHandlerContentsOld do
                    if eventHandlerContents.ContainsKey eventHandlerEntry.Key then
                        eventHandlerContents.[eventHandlerEntry.Key] <- eventHandlerEntry.Value

                // fin
                world
            else world
        else world

    let
#if !DEBUG
        inline
#endif
        private synchronizeProperties initializing (contentOld : SimulantContent) (content : SimulantContent) (simulant : Simulant) world =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let simulant = if notNull (contentOld.SimulantCachedOpt :> obj) then contentOld.SimulantCachedOpt else simulant
            content.SimulantCachedOpt <- simulant
            List.foldGeneric (fun world propertyContent ->
                if not propertyContent.PropertyStatic || initializing then
                    let lens = propertyContent.PropertyLens
                    let simulant = match lens.This :> obj with null -> simulant | _ -> lens.This
                    World.setProperty lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } simulant world |> snd'
                else world)
                world content.PropertyContentsOpt
        else world

    let
#if !DEBUG
        inline
#endif
        private synchronizeEntityPropertiesFast (initializing, contentOld : EntityContent, content : EntityContent, entity : Entity, world, mountOptFound : bool outref) =
        if notNull content.PropertyContentsOpt && content.PropertyContentsOpt.Count > 0 then
            let entity = if notNull (contentOld.EntityCachedOpt :> obj) then contentOld.EntityCachedOpt else entity
            content.EntityCachedOpt <- entity
            let mutable world = world // OPTIMIZATION: manual fold for speed.
            let propertyContents = content.PropertyContentsOpt
            for i in 0 .. dec propertyContents.Count do
                let propertyContent = propertyContents.[i]
                if not propertyContent.PropertyStatic || initializing then
                    let lens = propertyContent.PropertyLens
                    if strEq lens.Name "MountOpt" then mountOptFound <- true
                    let entity = match lens.This :> obj with null -> entity | _ -> lens.This :?> Entity
                    world <- World.setEntityPropertyFast lens.Name { PropertyType = lens.Type; PropertyValue = propertyContent.PropertyValue } entity world
            content.PropertyContentsOpt <- null // OPTIMIZATION: blank out property contents to avoid GC promotion.
            world
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

    /// Synchronize an entity and its contained simulants to the given content.
    let rec synchronizeEntity initializing (contentOld : EntityContent) (content : EntityContent) (origin : Simulant) (entity : Entity) world =
        if contentOld =/= content then
            let mutable mountOptFound = false
            let world = synchronizeEventSignals contentOld content origin entity world
            let world = synchronizeEventHandlers contentOld content origin entity world
            let world = synchronizeEntityPropertiesFast (initializing, contentOld, content, entity, world, &mountOptFound)
            let world =
                if initializing then
                    if not mountOptFound && entity.Surnames.Length > 1
                    then World.setEntityMountOpt (Some (Relation.makeParent ())) entity world |> snd'
                    else world
                else world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content entity with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    List.foldGeneric (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull contentOld.EntityContentsOpt then
                        OrderedDictionary.fold (fun world (entity : Entity) entityContent ->
                            let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                            synchronizeEntity initializing entityContentOld entityContent origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    List.foldGeneric (fun world (entity : Entity, entityContent : EntityContent) ->
                        let world =
                            if not (entity.Exists world) || entity.GetDestroying world
                            then World.createEntity5 entityContent.EntityDispatcherName DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                            else world
                        let world = World.setEntityProtected true entity world |> snd'
                        synchronizeEntity true EntityContent.empty entityContent origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    /// Synchronize a group and its contained simulants to the given content.
    let synchronizeGroup initializing (contentOld : GroupContent) (content : GroupContent) (origin : Simulant) (group : Group) world =
        if contentOld =/= content then
            let world = synchronizeEventSignals contentOld content origin group world
            let world = synchronizeEventHandlers contentOld content origin group world
            let world = synchronizeProperties initializing contentOld content group world
            match tryDifferentiateChildren<Entity, EntityContent> contentOld content group with
            | Some (entitiesAdded, entitiesRemoved, entitiesPotentiallyAltered) ->
                let world =
                    List.foldGeneric (fun world entity -> World.destroyEntity entity world) world entitiesRemoved
                let world =
                    if notNull contentOld.EntityContentsOpt then
                        OrderedDictionary.fold (fun world (entity : Entity) entityContent ->
                            let entityContentOld = contentOld.EntityContentsOpt.[entity.Name]
                            synchronizeEntity initializing entityContentOld entityContent origin entity world)
                            world entitiesPotentiallyAltered
                    else world
                let world =
                    List.foldGeneric (fun world (entity : Entity, entityContent : EntityContent) ->
                        let world =
                            if not (entity.Exists world) || entity.GetDestroying world then
                                match entityContent.EntityFilePathOpt with
                                | Some entityFilePath -> World.readEntityFromFile entityFilePath (Some entity.Name) entity.Parent world |> snd
                                | None -> World.createEntity5 entityContent.EntityDispatcherName DefaultOverlay (Some entity.Surnames) entity.Group world |> snd
                            else world
                        let world = World.setEntityProtected true entity world |> snd'
                        synchronizeEntity true EntityContent.empty entityContent origin entity world)
                        world entitiesAdded
                world
            | None -> world
        else world

    /// Synchronize a screen and its contained simulants to the given content.
    let synchronizeScreen initializing (contentOld : ScreenContent) (content : ScreenContent) (origin : Simulant) (screen : Screen) world =
        if contentOld =/= content then
            let world = synchronizeEventSignals contentOld content origin screen world
            let world = synchronizeEventHandlers contentOld content origin screen world
            let world = synchronizeProperties initializing contentOld content screen world
            let world =
                if contentOld.GroupFilePathOpt =/= content.GroupFilePathOpt then
                    let world =
                        match contentOld.GroupFilePathOpt with
                        | Some groupFilePath ->
                            // NOTE: have to load the group file just get the name of the group to destroy...
                            let groupDescriptorStr = File.ReadAllText groupFilePath
                            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                            let groupName =
                                Constants.Engine.NamePropertyName |>
                                groupDescriptor.GroupProperties.TryFind |>
                                Option.mapOrDefaultValue symbolToValue "GroupFromFile" // TODO: make constant?
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
                    List.foldGeneric (fun world group -> World.destroyGroup group world) world groupsRemoved
                let world =
                    OrderedDictionary.fold (fun world (group : Group) groupContent ->
                        let groupContentOld = contentOld.GroupContents.[group.Name]
                        synchronizeGroup initializing groupContentOld groupContent origin group world)
                        world groupsPotentiallyAltered
                let world =
                    List.foldGeneric (fun world (group : Group, groupContent : GroupContent) ->
                        let world =
                            if not (group.Exists world) || group.GetDestroying world then
                                match groupContent.GroupFilePathOpt with
                                | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> snd
                                | None -> World.createGroup4 groupContent.GroupDispatcherName (Some group.Name) group.Screen world |> snd
                            else world
                        let world = World.setGroupProtected true group world |> snd'
                        synchronizeGroup true GroupContent.empty groupContent origin group world)
                        world groupsAdded
                world
            | None -> world
        else world

    /// Synchronize a screen and its contained simulants to the given content.
    let synchronizeGame setScreenSlide initializing (contentOld : GameContent) (content : GameContent) (origin : Simulant) (game : Game) world =
        if contentOld =/= content then
            let world = synchronizeEventSignals contentOld content origin game world
            let world = synchronizeEventHandlers contentOld content origin game world
            let world = synchronizeProperties initializing contentOld content game world
            match tryDifferentiateChildren<Screen, ScreenContent> contentOld content game with
            | Some (screensAdded, screensRemoved, screensPotentiallyAltered) ->
                let world =
                    List.foldGeneric (fun world screen -> World.destroyScreen screen world) world screensRemoved
                let world =
                    OrderedDictionary.fold (fun world (screen : Screen) screenContent ->
                        let screenContentOld = contentOld.ScreenContents.[screen.Name]
                        synchronizeScreen initializing screenContentOld screenContent origin screen world)
                        world screensPotentiallyAltered
                let world =
                    List.foldGeneric (fun world (screen : Screen, screenContent : ScreenContent) ->
                        let world =
                            if not (screen.Exists world) || screen.GetDestroying world
                            then World.createScreen3 screenContent.ScreenDispatcherName (Some screen.Name) world |> snd
                            else world
                        let world = World.setScreenProtected true screen world |> snd'
                        let world = World.applyScreenBehavior setScreenSlide screenContent.ScreenBehavior screen world
                        synchronizeScreen true ScreenContent.empty screenContent origin screen world)
                        world screensAdded
                (content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name), world)
            | None -> (content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name), world)
        else (content.InitialScreenNameOpt |> Option.map (fun name -> Nu.Game.Handle / name), world)

    /// Describe an entity with the given dispatcher type and initialization as well as its contained entities.
    let private composite4<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName entityFilePathOpt initializations entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        for initialization in initializations do
            match initialization with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
        { EntityDispatcherName = typeof<'entityDispatcher>.Name; EntityName = entityName; EntityFilePathOpt = entityFilePathOpt; EntityCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          EntityContentsOpt = entityContentsOpt }

    /// Describe an entity with the given dispatcher type and initialization as well as its contained entities.
    let composite<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName initializations entities =
        composite4<'entityDispatcher> entityName None initializations entities

    /// Describe an entity with the given dispatcher type and initializations as well as its contained entities.
    let compositeFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath initializations entities =
        composite4<'entityDispatcher> entityName (Some filePath) initializations entities

    /// Describe an entity with the given dispatcher type and initializations.
    let entity<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName initializations =
        composite<'entityDispatcher> entityName initializations []

    /// Describe an entity with the given dispatcher type and initializations.
    let entityFromFile<'entityDispatcher when 'entityDispatcher :> EntityDispatcher> entityName filePath initializations =
        compositeFromFile<'entityDispatcher> entityName filePath initializations []

    /// Describe a 2d effect with the given initializations.
    let effect2d entityName initializations = entity<Effect2dDispatcher> entityName initializations

    /// Describe a static sprite with the given initializations.
    let staticSprite entityName initializations = entity<StaticSpriteDispatcher> entityName initializations

    /// Describe an animated sprite with the given initializations.
    let animatedSprite entityName initializations = entity<AnimatedSpriteDispatcher> entityName initializations

    /// Describe a basic static sprite emitter with the given initializations.
    let basicStaticSpriteEmitter entityName initializations = entity<BasicStaticSpriteEmitterDispatcher> entityName initializations

    /// Describe an association of gui entities with the given initializations and content.
    let association entityName initializations content = composite<GuiDispatcher> entityName initializations content

    /// Describe a panel with the given initializations and content.
    let panel entityName initializations content = composite<LabelDispatcher> entityName initializations content

    /// Describe a text with the given initializations.
    let text entityName initializations = entity<TextDispatcher> entityName initializations

    /// Describe a label with the given initializations.
    let label entityName initializations = entity<LabelDispatcher> entityName initializations

    /// Describe a button with the given initializations.
    let button entityName initializations = entity<ButtonDispatcher> entityName initializations

    /// Describe a toggle button with the given initializations.
    let toggleButton entityName initializations = entity<ToggleButtonDispatcher> entityName initializations

    /// Describe a radio button with the given initializations.
    let radioButton entityName initializations = entity<RadioButtonDispatcher> entityName initializations

    /// Describe a fill bar with the given initializations.
    let fillBar entityName initializations = entity<FillBarDispatcher> entityName initializations

    /// Describe a feeler with the given initializations.
    let feeler entityName initializations = entity<FeelerDispatcher> entityName initializations

    /// Describe an fps gui with the given initializations.
    let fps entityName initializations = entity<FpsDispatcher> entityName initializations

    /// Describe a 2d block with the given initializations.
    let block2d entityName initializations = entity<Block2dDispatcher> entityName initializations

    /// Describe a 2d box with the given initializations.
    let box2d entityName initializations = entity<Box2dDispatcher> entityName initializations

    /// Describe a 2d character with the given initializations.
    let character2d entityName initializations = entity<Character2dDispatcher> entityName initializations

    /// Describe a tile map with the given initializations.
    let tileMap entityName initializations = entity<TileMapDispatcher> entityName initializations

    /// Describe a tmx map with the given initializations.
    let tmxMap entityName initializations = entity<TmxMapDispatcher> entityName initializations

    /// Describe a 3d light probe with the given initializations.
    let lightProbe3d entityName initializations = entity<LightProbe3dDispatcher> entityName initializations

    /// Describe a 3d light with the given initializations.
    let light3d entityName initializations = entity<Light3dDispatcher> entityName initializations

    /// Describe a sky box with the given initializations.
    let skyBox entityName initializations = entity<SkyBoxDispatcher> entityName initializations

    /// Describe a static billboard with the given initializations.
    let staticBillboard entityName initializations = entity<StaticBillboardDispatcher> entityName initializations

    /// Describe a basic static billboard emitter with the given initializations.
    let basicStaticBillboardEmitter entityName initializations = entity<BasicStaticSpriteEmitterDispatcher> entityName initializations

    /// Describe a static model with the given initializations.
    let staticModel entityName initializations = entity<StaticModelDispatcher> entityName initializations

    /// Describe a static model surface with the given initializations.
    let staticModelSurface entityName initializations = entity<StaticModelSurfaceDispatcher> entityName initializations

    /// Describe an animated model with the given initializations.
    let animatedModel entityName initializations = entity<AnimatedModelDispatcher> entityName initializations

    /// Describe a 3d character with the given initializations.
    let character3d entityName initializations = entity<Character3dDispatcher> entityName initializations

    /// Describe a terrain with the given initializations.
    let terrain entityName initializations = entity<TerrainDispatcher> entityName initializations

    /// Describe a static model expanded into an entity hierarchy with the given initializations.
    let staticModelHierarchy entityName initializations = entity<StaticModelHierarchyDispatcher> entityName initializations

    /// Describe a rigid model expanded into an entity hierarchy with the given initializations.
    let rigidModelHierarchy entityName initializations = entity<RigidModelHierarchyDispatcher> entityName initializations

    /// Describe a group with the given dispatcher type and initializations as well as its contained entities.
    let private group4<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName groupFilePathOpt initializations entities =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let mutable entityContentsOpt = null
        for initialization in initializations do
            match initialization with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for entity in entities do
            if isNull entityContentsOpt then entityContentsOpt <- OrderedDictionary StringComparer.Ordinal
            entityContentsOpt.Add (entity.EntityName, entity)
        { GroupDispatcherName = typeof<'groupDispatcher>.Name; GroupName = groupName; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          EntityContentsOpt = entityContentsOpt }

    /// Describe a group with the given dispatcher type and initializations as well as its contained entities.
    let group<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName initializations entities =
        group4<'groupDispatcher> groupName None initializations entities

    /// Describe a group and its contained entities loaded from the given file with the given initializations.
    let groupFromFile<'groupDispatcher when 'groupDispatcher :> GroupDispatcher> groupName filePath initializations entities =
        group4<'groupDispatcher> groupName (Some filePath) initializations entities

    /// Describe a screen with the given dispatcher type and initializations as well as its contained simulants.
    let private screen5<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePathOpt initializations groups =
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let groupContents = OrderedDictionary StringComparer.Ordinal
        for initialization in initializations do
            match initialization with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for group in groups do
            groupContents.Add (group.GroupName, group)
        { ScreenDispatcherName = typeof<'screenDispatcher>.Name; ScreenName = screenName; ScreenBehavior = screenBehavior; GroupFilePathOpt = groupFilePathOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          GroupContents = groupContents }

    /// Describe a screen with the given dispatcher type and initializations as well as its contained simulants.
    let screen<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior initializations groups =
        screen5<'screenDispatcher> screenName screenBehavior None initializations groups

    /// Describe a screen with the given type and initializations with a group loaded from the given file.
    let screenWithGroupFromFile<'screenDispatcher when 'screenDispatcher :> ScreenDispatcher> screenName screenBehavior groupFilePath initializations groups =
        screen5<'screenDispatcher> screenName screenBehavior (Some groupFilePath) initializations groups

    /// Describe a game with the given initializations as well as its contained simulants.
    let game gameName initializations screens =
        ignore<string> gameName
        let initialScreenNameOpt = match Seq.tryHead screens with Some screen -> Some screen.ScreenName | None -> None
        let mutable eventSignalContentsOpt = null
        let mutable eventHandlerContentsOpt = null
        let mutable propertyContentsOpt = null
        let screenContents = OrderedDictionary StringComparer.Ordinal
        for initialization in initializations do
            match initialization with
            | EventSignalContent (addr, value) -> (if isNull eventSignalContentsOpt then eventSignalContentsOpt <- OrderedDictionary HashIdentity.Structural); eventSignalContentsOpt.Add ((addr, value), makeGuid ())
            | EventHandlerContent ehf -> (if isNull eventHandlerContentsOpt then eventHandlerContentsOpt <- OrderedDictionary HashIdentity.Structural); eventHandlerContentsOpt.Add ((UpdateLateBindingsCount, ehf.Equatable), (makeGuid (), ehf.Nonequatable))
            | PropertyContent pc -> (if isNull propertyContentsOpt then propertyContentsOpt <- List ()); propertyContentsOpt.Add pc
        for screen in screens do
            screenContents.Add (screen.ScreenName, screen)
        { InitialScreenNameOpt = initialScreenNameOpt; SimulantCachedOpt = Unchecked.defaultof<_>
          EventSignalContentsOpt = eventSignalContentsOpt; EventHandlerContentsOpt = eventHandlerContentsOpt; PropertyContentsOpt = propertyContentsOpt
          ScreenContents = screenContents }

[<AutoOpen>]
module ContentOperators =

    /// Define a static property equality.
    let
#if !DEBUG
        inline
#endif
        (==) (lens : Lens<'a, 's>) (value : 'a) : InitializationContent =
        PropertyContent (PropertyContent.make true lens value)

    /// Define a dynamic property equality.
    let
#if !DEBUG
        inline
#endif
        (:=) (lens : Lens<'a, 's>) (value : 'a) : InitializationContent =
        PropertyContent (PropertyContent.make false lens value)

    /// Define an event signal.
    let
#if !DEBUG
        inline
#endif
        (=>) (eventAddress : 'a Address) (signal : Signal) : InitializationContent =
        EventSignalContent (Address.generalize eventAddress, signal)

    /// Define an event handler.
    let
#if !DEBUG
        inline
#endif
        (=|>) (eventAddress : 'a Address) (callback : Event<'a, 's> -> Signal) : InitializationContent =
        EventHandlerContent (PartialEquatable.make (Address.generalize eventAddress) (fun (evt : Event) -> callback (Event.specialize evt) :> obj))