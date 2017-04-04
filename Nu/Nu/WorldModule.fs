// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.Reflection
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldModule =

    /// F# reach-around for evaluating a script expression.
    let mutable internal eval : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World =
        Unchecked.defaultof<Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalMany : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr array * World =
        Unchecked.defaultof<Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr array * World>

    /// F# reach-around for evaluating a script expression.
    let mutable internal evalWithLogging : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World =
        Unchecked.defaultof<Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr * World>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalManyWithLogging : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr array * World =
        Unchecked.defaultof<Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> Scripting.Expr array * World>

    type World with // Construction

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

        /// Make the world.
        static member internal make eventSystem dispatchers subsystems scriptingEnv ambientState gameSpecializationOpt activeGameDispatcher =
            let gameState = GameState.make gameSpecializationOpt activeGameDispatcher
            let screenStates = UMap.makeEmpty None
            let layerStates = UMap.makeEmpty None
            let entityStates = UMap.makeEmpty None
            let world =
                { EventSystem = eventSystem
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  ScriptingEnv = scriptingEnv
                  ScriptingContext = Game Address.empty
                  ScreenCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Screen>, screenStates)) (FOption.none ())
                  LayerCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Layer>, layerStates)) (FOption.none ())
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Address.empty<Entity>, entityStates)) (FOption.none ())
                  ScreenDirectory = UMap.makeEmpty None
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = screenStates
                  LayerStates = layerStates
                  EntityStates = entityStates }
            World.choose world

    type World with // EventSystem

        /// Get event subscriptions.
        static member getSubscriptions world =
            EventWorld.getSubscriptions<Game, World> world

        /// Get event unsubscriptions.
        static member getUnsubscriptions world =
            EventWorld.getUnsubscriptions<Game, World> world

        /// Get whether events are being traced.
        static member getEventTracing (world : World) =
            EventWorld.getEventTracing world

        /// Set whether events are being traced.
        static member setEventTracing tracing (world : World) =
            EventWorld.setEventTracing tracing world

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventWorld.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            EventWorld.setEventFilter filter world

        /// Get the context of the event system.
        static member getEventContext (world : World) =
            EventWorld.getEventContext world

        /// Set the context of the event system.
#if DEBUG
        static member internal withEventContext operation context (world : World) =
            let oldContext = World.getEventContext world
            EventWorld.setEventContext context world
            let world = operation world
            EventWorld.setEventContext oldContext world
            world
#else
        static member inline internal withEventContext operation _ (world : World) =
            // NOTE: inlined in debug to hopefully get rid of the lambda
            operation world
#endif

        /// Qualify the context of the event system.
        static member internal qualifyEventContext address (world : World) =
            EventWorld.qualifyEventContext address world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by subscriptions (world : World) =
            EventWorld.sortSubscriptionsBy by subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions (world : World) =
            // OPTIMIZATION: entity priority boxed up front to decrease GC pressure.
            let entityPriorityBoxed = Constants.Engine.EntitySortPriority :> IComparable
            World.sortSubscriptionsBy
                (fun (participant : Participant) _ ->
                    match participant with
                    | :? Game -> Constants.Engine.GameSortPriority :> IComparable
                    | :? Screen -> Constants.Engine.ScreenSortPriority :> IComparable
                    | :? Layer -> Constants.Engine.LayerSortPriority :> IComparable
                    | :? Entity -> entityPriorityBoxed
                    | _ -> failwithumf ())
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone subscriptions (world : World) =
            EventWorld.sortSubscriptionsNone subscriptions world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publishPlus<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) allowWildcard world =
            EventWorld.publishPlus<'a, 'p, Game, World> publishSorter eventData eventAddress eventTrace publisher allowWildcard world

        /// Publish an event.
        static member publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publishPlus<'a, 'p, Game, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher true world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            EventWorld.unsubscribe<Game, World> subscriptionKey world

        /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus<'a, 's, Game, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (subscription : Event<'a, 's> -> World -> World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe<'a, 's, Game, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (subscription : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitorPlus<'a, 's, Game, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Event<'a, 's> -> World -> World) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitor<'a, 's, Game, World> subscription eventAddress subscriber world

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

        static member internal getSubsystem<'s when 's :> World Subsystem> name world : 's =
            Subsystems.getSubsystem name world.Subsystems

        static member internal getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
            Subsystems.getSubsystemBy by name world.Subsystems

        // NOTE: it'd be nice to get rid of this function to improve encapsulation, but I can't seem to do so in practice...
        static member internal setSubsystem<'s when 's :> World Subsystem> (subsystem : 's) name world =
            World.choose { world with Subsystems = Subsystems.setSubsystem subsystem name world.Subsystems }

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
    
        /// Get the tick rate.
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate, starting at the end of the current frame.
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState (AmbientState.setTickRateImmediate tickRate) world }}) world

        /// Reset the tick time to 0 at the end of the current frame.
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world
                      Command = { Execute = fun world -> World.updateAmbientState AmbientState.resetTickTimeImmediate world }}) world

        /// Get the world's tick time.
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Check that the world is ticking.
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        static member internal updateTickTime world =
            World.updateAmbientState AmbientState.updateTickTime world

        /// Get the world's update count.
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        static member internal incrementUpdateCount world =
            World.updateAmbientState AmbientState.incrementUpdateCount world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current phase.
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientState (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientState (AmbientState.addTasklets tasklets) world

        /// Get the asset metadata map.
        static member getMetadata world =
            AmbientState.getMetadata ^ World.getAmbientState world

        static member internal setMetadata assetMetadataMap world =
            World.updateAmbientState (AmbientState.setMetadata assetMetadataMap) world

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
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            World.getAmbientStateBy AmbientState.getUserState world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            World.updateAmbientState (AmbientState.updateUserState updater) world

        /// Make vanilla overlay routes from dispatchers.
        static member internal dispatchersToOverlayRoutes entityDispatchers =
            entityDispatchers |>
            Map.toValueListBy getTypeName |>
            List.map (fun typeName -> (typeName, OverlayDescriptor.makeVanilla (Some typeName)))

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

    type World with // Scripting - TODO: document.

        static member internal getGlobalFrame (world : World) =
            ScriptingWorld.getGlobalFrame world

        static member internal getLocalFrame (world : World) =
            ScriptingWorld.getLocalFrame world

        static member internal setLocalFrame localFrame (world : World) =
            ScriptingWorld.setLocalFrame localFrame world

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
            ScriptingWorld.tryEvalScript World.choose scriptFilePath world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getMemberProperties (state : SimulantState) =
            state |>
            getType |>
            getProperties |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state)) |>
            Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getXtensionProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            List.ofSeq |>
            List.sortBy fst |>
            List.map (fun (name, property) -> (name, property.PropertyValue))

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