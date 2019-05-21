// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModule =

    /// F# reach-around for evaluating a script expression.
    let mutable internal eval : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World)>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalMany : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World)>

    /// F# reach-around for evaluating a script expression.
    let mutable internal evalWithLogging : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World)>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalManyWithLogging : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World)>

    /// F# reach-around for adding script unsubscriptions to simulants.
    let mutable internal addSimulantScriptUnsubscription : Unsubscription -> Simulant -> World -> World =
        Unchecked.defaultof<Unsubscription -> Simulant -> World -> World>

    /// F# reach-around for unsubscribing script subscriptions of simulants.
    let mutable internal unsubscribeSimulantScripts : Simulant -> World -> World =
        Unchecked.defaultof<Simulant -> World -> World>

    type World with // Construction

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

        /// Make the world.
        static member internal make eventDelegate dispatchers subsystems scriptingEnv ambientState activeGameDispatcher =
            let gameState = GameState.make activeGameDispatcher
            let screenStates = UMap.makeEmpty Constants.Engine.SimulantMapConfig
            let layerStates = UMap.makeEmpty Constants.Engine.SimulantMapConfig
            let entityStates = UMap.makeEmpty Constants.Engine.SimulantMapConfig
            let world =
                { EventSystemDelegate = eventDelegate
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  ScriptingEnv = scriptingEnv
                  ScriptingContext = Game Address.empty
                  ScreenCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Screen>, screenStates)) (FOption.none ())
                  LayerCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Layer>, layerStates)) (FOption.none ())
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Entity>, entityStates)) (FOption.none ())
                  ScreenDirectory = UMap.makeEmpty Constants.Engine.SimulantMapConfig
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = screenStates
                  LayerStates = layerStates
                  EntityStates = entityStates }
            World.choose world

    type World with // EventSystem

        /// Get event subscriptions.
        static member getSubscriptions world =
            EventSystem.getSubscriptions<World> world

        /// Get event unsubscriptions.
        static member getUnsubscriptions world =
            EventSystem.getUnsubscriptions<World> world

        /// Get whether events are being traced.
        static member getEventTracing (world : World) =
            EventSystem.getEventTracing world

        /// Set whether events are being traced.
        static member setEventTracing tracing (world : World) =
            EventSystem.setEventTracing tracing world

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventSystem.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            EventSystem.setEventFilter filter world

        /// Get the context of the event system.
        static member getEventContext (world : World) =
            EventSystem.getEventContext world

        /// Set the context of the event system.
#if DEBUG
        static member internal withEventContext operation context (world : World) =
            let oldContext = World.getEventContext world
            EventSystem.setEventContext context world
            let world = operation world
            EventSystem.setEventContext oldContext world
            world
#else
        static member inline internal withEventContext operation _ (world : World) =
            // NOTE: inlined in debug to hopefully get rid of the lambda
            operation world
#endif

        /// Qualify the context of the event system.
        static member internal qualifyEventContext address (world : World) =
            EventSystem.qualifyEventContext address world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by subscriptions (world : World) =
            EventSystem.sortSubscriptionsBy by subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions (world : World) =
            // OPTIMIZATION: entity priority boxed up front to decrease GC pressure.
            let entityPriorityBoxed = Constants.Engine.EntitySortPriority :> IComparable
            World.sortSubscriptionsBy
                (fun (participant : Participant) _ ->
                    match participant with
                    | :? GlobalParticipantGeneralized
                    | :? Game -> Constants.Engine.GameSortPriority :> IComparable
                    | :? Screen -> Constants.Engine.ScreenSortPriority :> IComparable
                    | :? Layer -> Constants.Engine.LayerSortPriority :> IComparable
                    | :? Entity -> entityPriorityBoxed
                    | _ -> failwithumf ())
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone subscriptions (world : World) =
            EventSystem.sortSubscriptionsNone subscriptions world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publishPlus<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) allowWildcard world =
            EventSystem.publishPlus<'a, 'p, World> publishSorter eventData eventAddress eventTrace publisher allowWildcard world

        /// Publish an event.
        static member publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventSystem.publishPlus<'a, 'p, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher true world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            EventSystem.unsubscribe<World> subscriptionKey world

        /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Participant>
            subscriptionKey (subscription : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventSystem.subscribePlus<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Participant>
            (subscription : Event<'a, 's> -> World -> World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventSystem.subscribe<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
        static member monitorPlus<'a, 's when 's :> Participant>
            (subscription : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventSystem.monitorPlus<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Participant>
            (subscription : Event<'a, 's> -> World -> World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventSystem.monitor<'a, 's, World> subscription eventAddress subscriber world

    type World with // Dispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.Dispatchers.GameDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.Dispatchers.ScreenDispatchers

        /// Get the layer dispatchers of the world.
        static member getLayerDispatchers world =
            world.Dispatchers.LayerDispatchers

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.Dispatchers.EntityDispatchers

        /// Get the facets of the world.
        static member getFacets world =
            world.Dispatchers.Facets

    type World with // Subsystems

        static member internal getSubsystemMap world =
            Subsystems.getSubsystemMap world.Subsystems

        static member getSubsystem<'s when 's :> World Subsystem> name world : 's =
            Subsystems.getSubsystem name world.Subsystems

        static member getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
            Subsystems.getSubsystemBy by name world.Subsystems

        static member addSubsystem<'s when 's :> World Subsystem> name (subsystem : 's) world =
            { world with Subsystems = Subsystems.addSubsystem name subsystem world.Subsystems }

        static member removeSubsystem<'s when 's :> World Subsystem> name world =
            { world with Subsystems = Subsystems.removeSubsystem name world.Subsystems }

        static member internal updateSubsystem<'s when 's :> World Subsystem> (updater : 's -> World -> 's) name world =
            World.choose { world with Subsystems = Subsystems.updateSubsystem updater name world.Subsystems world }

        static member internal updateSubsystems (updater : World Subsystem -> World -> World Subsystem) world =
            World.choose { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

        static member internal clearSubsystemsMessages world =
            World.choose { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

    type World with // AmbientState

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }

        static member internal updateTickTime world =
            World.updateAmbientState AmbientState.updateTickTime world

        static member internal incrementUpdateCount world =
            World.updateAmbientState AmbientState.incrementUpdateCount world
    
        /// Get the tick rate.
        [<FunctionBinding>]
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        [<FunctionBinding>]
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate, starting at the end of the current frame.
        [<FunctionBinding>]
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState (AmbientState.setTickRateImmediate tickRate) world }}) world

        /// Reset the tick time to 0 at the end of the current frame.
        [<FunctionBinding>]
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState AmbientState.resetTickTimeImmediate world }}) world

        /// Increment the tick time at the end of the current frame.
        [<FunctionBinding>]
        static member incTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState AmbientState.incTickTimeImmediate world }}) world

        /// Increment the tick time at the end of the current frame.
        [<FunctionBinding>]
        static member decTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState AmbientState.decTickTimeImmediate world }}) world

        /// Get the world's tick time.
        [<FunctionBinding>]
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Check that the world is ticking.
        [<FunctionBinding>]
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        /// Get the world's update count.
        [<FunctionBinding>]
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        /// Get the the liveness state of the engine.
        [<FunctionBinding>]
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current phase.
        [<FunctionBinding>]
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientState (AmbientState.restoreTasklets tasklets) world

        static member internal getTaskletsProcessing world =
            World.getAmbientStateBy AmbientState.getTaskletsProcessing world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientState (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientState (AmbientState.addTasklets tasklets) world

        /// Schedule an operation to be executed by the engine at the given time.
        static member schedule fn time world =
            let tasklet = { ScheduledTime = time; Command = { Execute = fn }}
            World.addTasklet tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame.
        static member schedule2 fn world =
            let taskletsProcessing = World.getTaskletsProcessing world
            World.schedule fn (World.getTickTime world + if taskletsProcessing then 1L else 0L) world

        static member internal getMetadata world =
            AmbientState.getMetadata (World.getAmbientState world)

        static member internal setMetadata assetMetadataMap world =
            World.updateAmbientState (AmbientState.setMetadata assetMetadataMap) world

        /// Try to get the texture metadata of the given asset.
        [<FunctionBinding>]
        static member tryGetTextureSize assetTag world =
            Metadata.tryGetTextureSize assetTag (World.getMetadata world)

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        [<FunctionBinding>]
        static member getTextureSize assetTag world =
            Metadata.getTextureSize assetTag (World.getMetadata world)

        /// Try to get the texture size metadata of the given asset.
        [<FunctionBinding>]
        static member tryGetTextureSizeAsVector2 assetTag world =
            Metadata.tryGetTextureSizeAsVector2 assetTag (World.getMetadata world)

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        [<FunctionBinding>]
        static member getTextureSizeAsVector2 assetTag world =
            Metadata.getTextureSizeAsVector2 assetTag (World.getMetadata world)

        /// Try to get the tile map metadata of the given asset.
        static member tryGetTileMapMetadata assetTag world =
            Metadata.tryGetTileMapMetadata assetTag (World.getMetadata world)

        /// Forcibly get the tile map metadata of the given asset (throwing on failure).
        static member getTileMapMetadata assetTag world =
            Metadata.getTileMapMetadata assetTag (World.getMetadata world)

        /// Get a copy of the metadata map.
        static member getMetadataMap world =
            Metadata.getMetadataMap (World.getMetadata world)

        /// Get a map of all discovered assets.
        static member getAssetMap world =
            Metadata.getAssetMap (World.getMetadata world)

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get intrinsic overlays.
        static member getIntrinsicOverlays world =
            World.getOverlayerBy Overlayer.getIntrinsicOverlays world

        /// Get extrinsic overlays.
        static member getExtrinsicOverlays world =
            World.getOverlayerBy Overlayer.getExtrinsicOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getOverlayRouterBy by world =
            let overlayRouter = World.getOverlayRouter world
            by overlayRouter

        /// Try to get the routed optional overlay name.
        static member tryFindRoutedOverlayNameOpt dispatcherName state =
            World.getOverlayRouterBy (OverlayRouter.tryFindOverlayNameOpt dispatcherName) state

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage implicitDelimiters packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage implicitDelimiters packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol implicitDelimiters assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol implicitDelimiters assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols implicitDelimiters assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols implicitDelimiters assetTags symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Reload all the symbols in the symbol store.
        [<FunctionBinding>]
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user-defined state value, cast to 'a.
        static member getUserValue world : 'a =
            World.getAmbientStateBy AmbientState.getUserValue world

        /// Update the user-defined state value of the world.
        static member updateUserValue (updater : 'a -> 'b) world =
            World.updateAmbientState (AmbientState.updateUserValue updater) world

        /// Make vanilla overlay routes from dispatchers.
        static member internal dispatchersToOverlayRoutes entityDispatchers =
            entityDispatchers |>
            Map.toValueListBy getTypeName |>
            List.map (fun typeName -> (typeName, None))

    type World with // Caching

        /// Get the screen directory.
        static member internal getScreenDirectory world =
            world.ScreenDirectory

        /// Get the optional cached screen.
        static member internal getScreenCachedOpt world =
            world.ScreenCachedOpt

        /// Get the optional cached layer.
        static member internal getLayerCachedOpt world =
            world.LayerCachedOpt

        /// Get the optional cached entity.
        static member internal getEntityCachedOpt world =
            world.EntityCachedOpt

    type World with // Scripting

        static member internal getGlobalFrame (world : World) =
            ScriptingSystem.getGlobalFrame world

        static member internal getLocalFrame (world : World) =
            ScriptingSystem.getLocalFrame world

        static member internal setLocalFrame localFrame (world : World) =
            ScriptingSystem.setLocalFrame localFrame world

        /// Get the context of the script system.
        static member internal getScriptContext (world : World) =
            world.ScriptingContext

        /// Get the context of the script system.
        static member internal setScriptContext context (world : World) =
            { world with ScriptingContext = context }

        /// Evaluate a script expression.
        static member eval expr localFrame simulant world =
            eval expr localFrame simulant world

        /// Evaluate a script expression, with logging on violation result.
        static member evalWithLogging expr localFrame simulant world =
            evalWithLogging expr localFrame simulant world

        /// Evaluate a series of script expressions.
        static member evalMany exprs localFrame simulant world =
            evalMany exprs localFrame simulant world

        /// Evaluate a series of script expressions, with logging on violation results.
        static member evalManyWithLogging exprs localFrame simulant world =
            evalManyWithLogging exprs localFrame simulant world

        /// Attempt to evaluate a script.
        static member tryEvalScript scriptFilePath world =
            ScriptingSystem.tryEvalScript World.choose scriptFilePath world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getMemberProperties (state : SimulantState) =
            state |>
            getType |>
            getProperties |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.PropertyType, property.GetValue state)) |>
            Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getXtensionProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            List.ofSeq |>
            List.sortBy fst |>
            List.map (fun (name, property) -> (name, property.PropertyType, property.PropertyValue))

        /// Provides a full view of all the properties of some SimulantState.
        static member internal getProperties state =
            List.append
                (World.getMemberProperties state)
                (World.getXtensionProperties state)

    type World with // Handlers

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
            
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)

// TODO: consider putting this in its own WorldModuleDescriptors.fs file once we can sensibly use folders in F#.
module Descriptors =

    /// Describe a game with the given properties values and contained screens.
    let Game<'d when 'd :> GameDispatcher> properties screens =
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained layers.
    let Screen<'d when 'd :> ScreenDispatcher> properties layers =
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Layers = List.ofSeq layers }

    /// Describe a layer with the given properties values and contained entities.
    let Layer<'d when 'd :> LayerDispatcher> properties entities =
        { LayerDispatcher = typeof<'d>.Name
          LayerProperties = Map.ofSeq properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let Entity<'d when 'd :> EntityDispatcher> properties =
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }