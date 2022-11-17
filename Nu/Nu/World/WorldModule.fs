// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Linq
open System.Reflection
open FSharpx.Collections
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    /// The game. Always exists.
    let Game = Game ()
    
    [<RequireQualifiedAccess>]
    module Default =

        /// The default screen - may or may not exist.
        let Screen = Screen (nameof Screen)

        /// The default group - may or may not exist.
        let Group = Screen / nameof Group

        /// The default entity - may or may not exist.
        let Entity = Group / nameof Entity

[<AutoOpen>]
module WorldModuleOperators =

    /// Derive a screen from a name.
    let ntos (screenName : string) =
        Screen screenName

    /// Attempt to resolve a relationship from a simulant.
    let tryResolve<'t when 't :> Simulant> (simulant : Simulant) (relation : 't Relation) : 't option =
        let simulant2 = Relation.resolve<Simulant, 't> simulant.SimulantAddress relation
        if simulant2.Names.Length >= 3 && typeof<'t> = typeof<Entity> then Some (Entity (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 2 && typeof<'t> = typeof<Group> then Some (Group (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 1 && typeof<'t> = typeof<Screen> then Some (Screen (simulant2.Names.[0]) :> Simulant :?> 't)
        elif simulant2.Names.Length = 0 && typeof<'t> = typeof<Game> then Some (Simulants.Game :> Simulant :?> 't)
        else None

    /// Relate the second simulant to the first.
    let relate<'t when 't :> Simulant> (simulant : Simulant) (simulant2 : 't) : 't Relation =
        Relation.relate<Simulant, 't> simulant.SimulantAddress (atoa simulant2.SimulantAddress)

[<AutoOpen; ModuleBinding>]
module WorldModule =

    /// Track if we're in the portion of the frame before tasklet processing has started or after.
    let mutable internal TaskletProcessingStarted = false

    /// F# reach-around for evaluating a script expression.
    let mutable internal eval : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalMany : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating a script expression.
    let mutable internal evalWithLogging : Scripting.Expr -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for evaluating script expressions.
    let mutable internal evalManyWithLogging : Scripting.Expr array -> Scripting.DeclarationFrame -> Simulant -> World -> struct (Scripting.Expr array * World) =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is selected.
    let mutable internal isSelected : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is ignoring bindings.
    let mutable internal ignorePropertyBindings : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for getting a screen's Ecs.
    let mutable internal getScreenEcs : Screen -> World -> Ecs.Ecs =
        Unchecked.defaultof<_>

    /// F# reach-around for sorting subscriptions by elevation.
    let mutable internal sortSubscriptionsByElevation : (Guid * SubscriptionEntry) seq -> obj -> (Guid * SubscriptionEntry) seq =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal evictScreenElements : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal admitScreenElements : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal registerScreenPhysics : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal unregisterScreenPhysics : Screen -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for adding script unsubscriptions to simulants.
    let mutable internal addSimulantScriptUnsubscription : Unsubscription -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unsubscribing script subscriptions of simulants.
    let mutable internal unsubscribeSimulantScripts : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal register : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal unregister : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal destroyImmediate : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal destroy : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal trySignalFacet : obj -> string -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal trySignal : obj -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    type World with // Construction

        /// Choose a world to be used as the active world. Call this whenever the most recently constructed world value
        /// is to be discarded in favor of the given world value, such as in an exception handler.
        static member choose (world : World) =
            world.Choose ()

        /// Assert that the current world is the chosen world (used for debugging).
        static member assertChosen (world : World) =
            world.AssertChosen ()

        /// Make the world.
        static member internal make plugin eventDelegate dispatchers subsystems scriptingEnv ambientState quadtree octree activeGameDispatcher =
            let config = AmbientState.getConfig ambientState
            let entityStates = UMap.makeEmpty HashIdentity.Structural config
            let groupStates = UMap.makeEmpty HashIdentity.Structural config
            let screenStates = UMap.makeEmpty HashIdentity.Structural config
            let gameState = GameState.make activeGameDispatcher
            let simulants = UMap.singleton HashIdentity.Structural config (Simulants.Game :> Simulant) None
            let worldExtension = { DestructionListRev = []; Dispatchers = dispatchers; Plugin = plugin; ScriptingEnv = scriptingEnv; ScriptingContext = Game () }
            let world =
                { SequenceId = Gen.idForWorldSequence
                  DivergenceId = Gen.idForWorldDivergence
                  EventSystemDelegate = eventDelegate
                  EntityCachedOpt = KeyedCache.make (KeyValuePair (Unchecked.defaultof<Entity>, entityStates)) Unchecked.defaultof<EntityState>
                  EntityStates = entityStates
                  GroupStates = groupStates
                  ScreenStates = screenStates
                  GameState = gameState
                  EntityMounts = UMap.makeEmpty HashIdentity.Structural config
                  Quadtree = MutantCache.make id quadtree
                  Octree = MutantCache.make id octree
                  SelectedEcsOpt = None
                  AmbientState = ambientState
                  Subsystems = subsystems
                  Simulants = simulants
                  WorldExtension = worldExtension }
            let world = { world with GameState = Reflection.attachProperties GameState.copy gameState.Dispatcher gameState world }
            World.choose world

    type World with // Caching

        /// Get the optional cached entity.
        static member internal getEntityCachedOpt world =
            world.EntityCachedOpt

        /// Get the simulants.
        static member internal getSimulants world =
            world.Simulants

    type World with // Dispatchers

        static member internal getDestructionListRev world =
            world.WorldExtension.DestructionListRev

        static member internal addSimulantToDestruction simulant world =
            { world with
                WorldExtension =
                    { world.WorldExtension with
                        DestructionListRev = simulant :: world.WorldExtension.DestructionListRev }}

        static member internal tryRemoveSimulantFromDestruction simulant world =
            { world with
                WorldExtension =
                    { world.WorldExtension with
                        DestructionListRev = List.remove ((=) simulant) world.WorldExtension.DestructionListRev }}

        /// Get the facets of the world.
        static member getFacets world =
            world.WorldExtension.Dispatchers.Facets

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.WorldExtension.Dispatchers.EntityDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.WorldExtension.Dispatchers.GroupDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.WorldExtension.Dispatchers.ScreenDispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.WorldExtension.Dispatchers.GameDispatchers

    type World with // Metadata

        /// Try to get the texture metadata of the given asset via script.
        [<FunctionBinding "tryGetTextureSize">]
        static member internal tryGetTextureSizeViaScript assetTag (world : World) =
            ignore world
            Metadata.tryGetTextureSize assetTag

        /// Forcibly get the texture size metadata of the given asset via script (throwing on failure).
        [<FunctionBinding "getTextureSize">]
        static member internal getTextureSizeViaScript assetTag (world : World) =
            ignore world
            Metadata.getTextureSize assetTag

        /// Try to get the texture size metadata of the given asset via script.
        [<FunctionBinding "tryGetTextureSizeF">]
        static member internal tryGetTextureSizeFViaScript assetTag (world : World) =
            ignore world
            Metadata.tryGetTextureSizeF assetTag

        /// Forcibly get the texture size metadata of the given asset via script (throwing on failure).
        [<FunctionBinding "getTextureSizeF">]
        static member internal getTextureSizeFViaScript assetTag (world : World) =
            ignore world
            Metadata.getTextureSizeF assetTag

    type World with // AmbientState

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }

        /// Get whether the engine is running imperatively.
        [<FunctionBinding>]
        static member getImperative world =
            World.getAmbientStateBy AmbientState.getImperative world

        /// Get whether the engine is running stand-alone.
        [<FunctionBinding>]
        static member getStandAlone world =
            World.getAmbientStateBy AmbientState.getStandAlone world

        /// Get collection config value.
        [<FunctionBinding>]
        static member getCollectionConfig world =
            World.getAmbientStateBy AmbientState.getConfig world

        /// Get the the liveness state of the engine.
        [<FunctionBinding>]
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        static member internal updateTime world =
            World.updateAmbientState AmbientState.updateTime world

        /// Get the update rate.
        [<FunctionBinding>]
        static member getUpdateRate world =
            World.getAmbientStateBy AmbientState.getUpdateRate world

        /// Set the update rate, starting at the end of the current frame.
        [<FunctionBinding>]
        static member setUpdateRate updateRate world =
            World.frame (World.updateAmbientState (AmbientState.setUpdateRateImmediate updateRate)) Simulants.Game world

        /// Check that the update rate is non-zero.
        [<FunctionBinding>]
        static member getAdvancing world =
            World.getAmbientStateBy AmbientState.getAdvancing world

        /// Check that the update rate is zero.
        [<FunctionBinding>]
        static member getHalted world =
            World.getAmbientStateBy AmbientState.getHalted world

        /// Get the world's update time.
        [<FunctionBinding>]
        static member getUpdateTime world =
            World.getAmbientStateBy AmbientState.getUpdateTime world

        /// Get the world's clock time.
        /// No script function binding due to lack of a DateTimeOffset script conversion.
        static member getClockTime world =
            World.getAmbientStateBy AmbientState.getClockTime world

        /// Get the world's clock delta time in normalized floating point units.
        [<FunctionBinding>]
        static member getClockDelta world =
            World.getAmbientStateBy AmbientState.getClockDelta world

        /// Place the engine into a state such that the app will exit at the end of the current frame.
        [<FunctionBinding>]
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getKeyValueStoreBy by world =
            World.getAmbientStateBy (AmbientState.getKeyValueStoreBy by) world

        static member internal getKeyValueStore world =
            World.getAmbientStateBy AmbientState.getKeyValueStore world

        static member internal setKeyValueStore symbolics world =
            World.updateAmbientState (AmbientState.setKeyValueStore symbolics) world

        static member internal updateKeyValueStore updater world =
            World.updateAmbientState (AmbientState.updateKeyValueStore updater) world

        static member internal tryGetKeyedValueFast<'a> (key, world, value : 'a outref) =
            let ambientState = World.getAmbientState world
            let kvs = AmbientState.getKeyValueStore ambientState
            let mutable valueObj = Unchecked.defaultof<obj>
            if kvs.TryGetValue (key, &valueObj) then
                value <- valueObj :?> 'a
                true
            else false

        /// Attempt to look up a value from the world's key value store.
        static member tryGetKeyedValue<'a> key world =
            match World.getKeyValueStoreBy (UMap.tryFind key) world with
            | Some value -> Some (value :?> 'a)
            | None -> None

        /// Look up a value from the world's key value store, throwing an exception if it is not found.
        static member getKeyedValue<'a> key world =
            World.getKeyValueStoreBy (UMap.find key) world :?> 'a

        /// Add a value to the world's key value store.
        static member addKeyedValue<'a> key (value : 'a) world =
            World.updateKeyValueStore (UMap.add key (value :> obj)) world

        /// Remove a value from the world's key value store.
        static member removeKeyedValue key world =
            World.updateKeyValueStore (UMap.remove key) world

        /// Transform a value in the world's key value store if it exists.
        static member updateKeyedValue<'a> (updater : 'a -> 'a) key world =
            World.addKeyedValue key (updater (World.getKeyedValue<'a> key world)) world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal removeTasklets simulant world =
            World.updateAmbientState (AmbientState.removeTasklets simulant) world

        static member internal clearTasklets world =
            World.updateAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet simulant tasklet world =
            World.updateAmbientState (AmbientState.addTasklet simulant tasklet) world

        /// Schedule an operation to be executed by the engine with the given delay.
        static member schedule operation delay (simulant : Simulant) world =
            let tasklet = { ScheduledTime = World.getUpdateTime world + delay; ScheduledOp = operation }
            World.addTasklet simulant tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame or the next frame if we've already started processing tasklets.
        static member frame operation (simulant : Simulant) world =
            World.schedule operation (if TaskletProcessingStarted then World.getUpdateRate world else 0L) simulant world

        /// Attempt to get the window flags.
        [<FunctionBinding>]
        static member tryGetWindowFlags world =
            World.getAmbientStateBy AmbientState.tryGetWindowFlags world

        /// Attempt to check that the window is minimized.
        [<FunctionBinding>]
        static member tryGetWindowMinimized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMinimized world

        /// Attempt to check that the window is maximized.
        [<FunctionBinding>]
        static member tryGetWindowMaximized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMaximized world
            
        /// Attempt to check that the window is in a full screen state.
        [<FunctionBinding>]
        static member tryGetWindowFullScreen world =
            World.getAmbientStateBy AmbientState.tryGetWindowFullScreen world

        /// Attempt to set the window's full screen state.
        [<FunctionBinding>]
        static member trySetWindowFullScreen fullScreen world =
            World.updateAmbientState (AmbientState.trySetWindowFullScreen fullScreen) world

        /// Attempt to get the window size.
        [<FunctionBinding>]
        static member tryGetWindowSize world =
            World.getAmbientStateBy (AmbientState.tryGetWindowSize) world

        /// Get the window size, using resolution as default in case there is no window.
        [<FunctionBinding>]
        static member getWindowSize world =
            match World.tryGetWindowSize world with
            | Some windowsSize -> windowsSize
            | None -> Constants.Render.Resolution

        /// Get the viewport.
        [<FunctionBinding>]
        static member getViewport (world : World) =
            ignore world
            Constants.Render.Viewport

        /// Get the viewport offset by margin when applicable.
        [<FunctionBinding>]
        static member getViewportOffset world =
            let windowSize = World.getWindowSize world
            Constants.Render.ViewportOffset windowSize

        /// Check whether the world should sleep rather than run.
        [<FunctionBinding>]
        static member shouldSleep world =
            World.getAmbientStateBy AmbientState.shouldSleep world

        static member internal getSymbolicsBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolicsBy by) world

        static member internal getSymbolics world =
            World.getAmbientStateBy AmbientState.getSymbolics world

        static member internal setSymbolics symbolics world =
            World.updateAmbientState (AmbientState.setSymbolics symbolics) world

        static member internal updateSymbolics updater world =
            World.updateAmbientState (AmbientState.updateSymbolics updater) world

        /// Try to load a symbol package with the given name.
        static member tryLoadSymbolPackage implicitDelimiters packageName world =
            World.getSymbolicsBy (Symbolics.tryLoadSymbolPackage implicitDelimiters packageName) world

        /// Unload a symbol package with the given name.
        static member unloadSymbolPackage packageName world =
            World.getSymbolicsBy (Symbolics.unloadSymbolPackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryGetSymbol assetTag metadata world =
            let symbolics = World.getSymbolics world
            Symbolics.tryGetSymbol assetTag metadata symbolics

        /// Try to find symbols with the given asset tags.
        static member tryGetSymbols implicitDelimiters assetTags world =
            let symbolics = World.getSymbolics world
            Symbolics.tryGetSymbols implicitDelimiters assetTags symbolics

        /// Reload all the symbols in symbolics.
        [<FunctionBinding>]
        static member reloadSymbols world =
            World.getSymbolicsBy Symbolics.reloadSymbols world
            world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get overlays.
        static member getOverlays world =
            World.getOverlayerBy Overlayer.getOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getOverlayRouterBy by world =
            let overlayRouter = World.getOverlayRouter world
            by overlayRouter

        static member internal setOverlayRouter router world =
            World.updateAmbientState (AmbientState.setOverlayRouter router) world

        static member internal tryGetRoutedOverlayNameOpt dispatcherName state =
            World.getOverlayRouterBy (OverlayRouter.tryGetOverlayNameOpt dispatcherName) state

    type World with // Quadtree

        static member internal getQuadtree world =
            world.Quadtree

        static member internal setQuadtree quadtree world =
            if World.getImperative world then
                world.Quadtree <- quadtree
                world
            else World.choose { world with Quadtree = quadtree }

        static member internal updateQuadtree updater world =
            World.setQuadtree (updater (World.getQuadtree world))

    type World with // Octree

        static member internal getOctree world =
            world.Octree

        static member internal setOctree octree world =
            if World.getImperative world then
                world.Octree <- octree
                world
            else World.choose { world with Octree = octree }

        static member internal updateOctree updater world =
            World.setOctree (updater (World.getOctree world))

    type World with // SelectedEcsOpt

        /// Attempt to get the currently selected ECS.
        static member getSelectedEcsOpt world =
            world.SelectedEcsOpt

        /// Get the currently selected ECS or throw an exception.
        static member getSelectedEcs world =
            match  world.SelectedEcsOpt with
            | Some selectedEcsOpt -> selectedEcsOpt
            | None -> failwith "Cannot get Ecs when no screen is selected."

        static member internal setSelectedEcsOpt selectedEcsOpt world =
            if World.getImperative world then
                world.SelectedEcsOpt <- selectedEcsOpt
                world
            else World.choose { world with SelectedEcsOpt = selectedEcsOpt }

    type World with // Subsystems

        static member internal getSubsystems world =
            world.Subsystems

        static member internal setSubsystems subsystems world =
            World.choose { world with Subsystems = subsystems }

        static member internal updateSubsystems updater world =
            World.setSubsystems (updater world.Subsystems) world

        static member internal cleanUpSubsystems world =
            World.updateSubsystems (fun subsystems -> subsystems.RendererProcess.Terminate (); subsystems) world

    type World with // EventSystem

        /// Get event subscriptions.
        static member internal getSubscriptions world =
            EventSystem.getSubscriptions<World> world

        /// Get event unsubscriptions.
        static member internal getUnsubscriptions world =
            EventSystem.getUnsubscriptions<World> world

        /// Get how events are being traced.
        static member getEventTracerOpt (world : World) =
            EventSystem.getEventTracerOpt world

        /// Set how events are being traced.
        static member setEventTracerOpt tracerOpt (world : World) =
            World.choose (EventSystem.setEventTracerOpt tracerOpt world)

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventSystem.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            World.choose (EventSystem.setEventFilter filter world)

        /// Publish an event with no subscription sorting.
        /// OPTIMIZATION: unrolled publishPlus here for speed.
        static member publishPlus<'a, 'p when 'p :> Simulant>
            (eventData : 'a)
            (eventAddress : 'a Address)
            eventTrace
            (publisher : 'p)
            hierarchical
            selectedOnly
            (world : World) =

            // OPTIMIZATION: generalize only once
            let eventAddressObj = Address.generalize eventAddress

#if DEBUG
            // log event based on event filter
            EventSystemDelegate.logEvent<World> eventAddressObj eventTrace world.EventSystemDelegate
#endif

            // get subscriptions the fastest way possible
            // OPTIMIZATION: subscriptions nullable to elide allocation via Seq.empty.
            let subscriptionsOpt =
                if hierarchical then
                    EventSystemDelegate.getSubscriptionsSorted
                        sortSubscriptionsByElevation eventAddressObj world.EventSystemDelegate world
                else
                    let subscriptions = EventSystemDelegate.getSubscriptions world.EventSystemDelegate
                    match UMap.tryFind eventAddressObj subscriptions with
                    | Some subscriptions -> OMap.toSeq subscriptions
                    | None -> null

            // publish to each subscription
            // OPTIMIZATION: inlined foldWhile here in order to compact the call stack.
            // OPTIMIZATION: fused PublishEventHook for speed.
            if notNull subscriptionsOpt then
                let mutable (going, enr) = (true, subscriptionsOpt.GetEnumerator ())
                let mutable (handling, world) = (Cascade, world)
                while going && enr.MoveNext () do
                    let (_, subscriptionEntry) = enr.Current
                    if  (match handling with Cascade -> true | Resolve -> false) &&
                        (match World.getLiveness world with Live -> true | Dead -> false) then
                        let subscriber = subscriptionEntry.Subscriber
                        if not selectedOnly || isSelected subscriber world then
                            let result =
                                let namesLength = subscriber.SimulantAddress.Names.Length
                                if namesLength >= 3
                                then EventSystem.publishEvent<'a, 'p, Entity, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                else
                                    match namesLength with
                                    | 0 ->
                                        match subscriber with
                                        | :? Game -> EventSystem.publishEvent<'a, 'p, Game, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                        | :? GlobalSimulantGeneralized -> EventSystem.publishEvent<'a, 'p, Simulant, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                        | _ -> failwithumf ()
                                    | 1 -> EventSystem.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                    | 2 -> EventSystem.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.CallbackBoxed world
                                    | _ -> failwithumf ()
                            handling <- fst result
                            world <- snd result
                            world |> World.choose |> ignore
                        else () // nothing to do
                    else going <- false
                world
            else world

        /// Publish an event with no subscription sorting.
        static member publish<'a, 'p when 'p :> Simulant>
            eventData eventAddress eventTrace publisher world =
            World.publishPlus<'a, 'p> eventData eventAddress eventTrace publisher false false world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionId world =
            World.choose (EventSystem.unsubscribe<World> subscriptionId world)

        /// Subscribe to an event using the given subscriptionId, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            subscriptionId callback eventAddress subscriber world =
            mapSnd World.choose (EventSystem.subscribePlus<'a, 's, World> subscriptionId callback eventAddress subscriber world)

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            World.choose (EventSystem.subscribe<'a, 's, World> callback eventAddress subscriber world)

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback..
        static member monitorPlus<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            mapSnd World.choose (EventSystem.monitorPlus<'a, 's, World> callback eventAddress subscriber world)

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            callback eventAddress subscriber world =
            World.choose (EventSystem.monitor<'a, 's, World> callback eventAddress subscriber world)

    type World with // Scripting

        static member internal getGlobalFrame (world : World) =
            ScriptingSystem.getGlobalFrame world

        static member internal getLocalFrame (world : World) =
            ScriptingSystem.getLocalFrame world

        static member internal setLocalFrame localFrame (world : World) =
            ScriptingSystem.setLocalFrame localFrame world

        /// Get the context of the script system.
        static member internal getScriptContext (world : World) =
            world.WorldExtension.ScriptingContext

        /// Set the context of the script system.
        static member internal setScriptContext context (world : World) =
            World.choose { world with WorldExtension = { world.WorldExtension with ScriptingContext = context }}

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

    type World with // Plugin

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle world =
            world.WorldExtension.Plugin.TryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle

        static member internal preProcess world =
            world.WorldExtension.Plugin.PreProcess world

        static member internal perProcess world =
            world.WorldExtension.Plugin.PerProcess world

        static member internal postProcess world =
            world.WorldExtension.Plugin.PostProcess world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getMemberProperties (state : SimulantState) =
            state |>
            getType |>
            (fun ty -> ty.GetProperties ()) |>
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

        /// Present properties for viewing.
        static member internal viewProperties state =
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c |> Array.sortBy fst

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