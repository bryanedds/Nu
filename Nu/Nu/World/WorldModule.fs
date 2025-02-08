// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Reflection
open Prime

[<AutoOpen>]
module WorldModuleOperators =

    /// Attempt to resolve a relationship from a simulant.
    let tryResolve<'t when 't :> Simulant> (simulant : Simulant) (relation : 't Relation) : 't option =
        let simulant2 = Relation.resolve<Simulant, 't> (itoa simulant.SimulantAddress) relation
        if simulant2.Names.Length >= 4 && typeof<'t> = typeof<Entity> then Some (Entity (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 3 && typeof<'t> = typeof<Group> then Some (Group (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 2 && typeof<'t> = typeof<Screen> then Some (Screen (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 1 && typeof<'t> = typeof<Game> then Some (Game.Handle :> Simulant :?> 't)
        else None

    /// Relate the second simulant to the first.
    let relate<'t when 't :> Simulant> (simulant : Simulant) (simulant2 : 't) : 't Relation =
        Relation.relate<Simulant, 't> (itoa simulant.SimulantAddress) (itoa simulant2.SimulantAddress)

[<AutoOpen>]
module WorldModule =

    /// Track if we're in the portion of the frame simulants are being updated.
    /// TODO: P1: consider making this an AmbientState flag.
    let mutable internal UpdatingSimulants = false

    /// Track if we're in the portion of the frame before tasklet processing has started or after.
    /// TODO: P1: consider making this an AmbientState flag.
    let mutable internal TaskletProcessingStarted = false

    /// F# reach-around for adding script unsubscriptions to simulants.
    let mutable internal addSimulantScriptUnsubscription : Unsubscription -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for unsubscribing script subscriptions of simulants.
    let mutable internal unsubscribeSimulantScripts : Simulant -> World -> World =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is selected.
    let mutable internal getSelected : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for checking that a simulant is ignoring bindings.
    let mutable internal ignorePropertyBindings : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for sorting subscriptions by elevation.
    let mutable internal sortSubscriptionsByElevation : (uint64 * SubscriptionEntry) seq -> obj -> (uint64 * SubscriptionEntry) seq =
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

    let mutable internal register : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal unregister : Simulant -> World -> World =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessGame : Game -> World -> World =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessScreen : bool -> Screen -> World -> World =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessGroup : Group -> World -> World =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessEntity : Entity -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal signal : obj -> Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal destroyImmediate : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal destroy : Simulant -> World -> World =
        Unchecked.defaultof<_>

    let mutable internal getEmptyEffect : unit -> obj =
        Unchecked.defaultof<_>

    type World with // Construction

        /// Choose a world to be used as the active world for debugging.
        static member internal choose (world : World) =
            world.Choose ()

    type World with // Caching

        /// Get the simulants.
        static member internal getSimulants world =
            world.Simulants

    type World with // JobGraph

        /// Enqueue a job for threaded execution.
        static member enqueueJob priority job world =
            world.JobGraph.Enqueue (priority, job)

        /// Await a job from threaded execution.
        /// Order of jobs with the same key is not guaranteed.
        static member tryAwaitJob deadline (jobId : obj) world =
            world.JobGraph.TryAwait (deadline, jobId)

    type World with // Destruction

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

    type World with // Dispatchers

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

    type World with // AmbientState

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal mapAmbientState mapper world =
            World.choose { world with AmbientState = mapper world.AmbientState }

        /// Check that the update rate is non-zero.
        static member getAdvancing (world : World) =
            world.Advancing

        /// Check that the update rate is zero.
        static member getHalted (world : World) =
            world.Halted

        /// Set whether the world state is advancing.
        static member setAdvancing advancing world =
            World.frame (World.mapAmbientState (AmbientState.setAdvancing advancing)) Game.Handle world

        /// Set whether the world's frame rate is being explicitly paced based on clock progression.
        static member setFramePacing clockPacing world =
            World.mapAmbientState (AmbientState.setFramePacing clockPacing) world

        /// Check that the world is executing with imperative semantics where applicable.
        static member getImperative (world : World) =
            world.Imperative

        /// Check that the world is executing with functional semantics.
        static member getFunctional (world : World) =
            world.Functional

        /// Get whether the engine is running accompanied, such as in an editor.
        static member getAccompanied (world : World) =
            world.Accompanied

        /// Get whether the engine is running unaccompanied, such as outside of an editor.
        static member getUnaccompanied (world : World) =
            world.Unaccompanied

        /// Get collection config value.
        static member getCollectionConfig world =
            World.getAmbientStateBy AmbientState.getConfig world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        static member internal updateTime world =
            World.mapAmbientState AmbientState.updateTime world

        /// Get the world's update time.
        static member getUpdateTime world =
            World.getAmbientStateBy AmbientState.getUpdateTime world

        /// Get the world's clock delta time.
        static member getClockDelta world =
            World.getAmbientStateBy AmbientState.getClockDelta world

        /// Get the world's clock time.
        static member getClockTime world =
            World.getAmbientStateBy AmbientState.getClockTime world

        /// Get the world's game delta time.
        static member getGameDelta world =
            World.getAmbientStateBy AmbientState.getGameDelta world

        /// Get the world's game time.
        static member getGameTime world =
            World.getAmbientStateBy AmbientState.getGameTime world

        /// Get the current ImNui context.
        static member getContextImNui (world : World) =
            world.ContextImNui

        /// Get the current ImNui context translated to a Game handle (throwing upon failure).
        static member getContextGame (world : World) =
            world.ContextGame

        /// Get the current ImNui context translated to a Screen handle (throwing upon failure).
        static member getContextScreen (world : World) =
            world.ContextScreen

        /// Get the current ImNui context translated to a Group handle (throwing upon failure).
        static member getContextGroup (world : World) =
            world.ContextGroup

        /// Get the current ImNui context translated to a Entity handle (throwing upon failure).
        static member getContextEntity (world : World) =
            world.ContextEntity

        /// Check that the current ImNui context is initializing this frame.
        static member getContextInitializing (world : World) =
            world.ContextInitializing

        /// Get the most recent ImNui context.
        static member getRecentImNui (world : World) =
            world.RecentImNui

        /// Get the most recent ImNui context translated to a Game handle (throwing upon failure).
        static member getRecentGame (world : World) =
            world.RecentGame

        /// Get the most recent ImNui context translated to a Screen handle (throwing upon failure).
        static member getRecentScreen (world : World) =
            world.RecentScreen

        /// Get the most recent ImNui context translated to a Group handle (throwing upon failure).
        static member getRecentGroup (world : World) =
            world.RecentGroup

        /// Get the most recent ImNui context translated to a Entity handle (throwing upon failure).
        static member getRecentEntity (world : World) =
            world.RecentEntity

        /// Check that the recent ImNui context is initializing this frame.
        static member getRecentInitializing (world : World) =
            world.RecentInitializing

        static member internal setContext context (world : World) =
            if world.Imperative then
                world.WorldExtension.RecentImNui <- world.WorldExtension.ContextImNui
                world.WorldExtension.ContextImNui <- context
                world
            else
                let worldExtension = { world.WorldExtension with RecentImNui = world.WorldExtension.ContextImNui; ContextImNui = context }
                World.choose { world with WorldExtension = worldExtension }

        static member internal advanceContext recent context (world : World) =
            if world.Imperative then
                world.WorldExtension.RecentImNui <- recent
                world.WorldExtension.ContextImNui <- context
                world
            else
                let worldExtension = { world.WorldExtension with RecentImNui = recent; ContextImNui = context }
                World.choose { world with WorldExtension = worldExtension }

        static member internal getSimulantImNuis (world : World) =
            world.SimulantImNuis

        static member internal setSimulantImNuis simulantImNuis (world : World) =
            if world.Imperative then
                world.WorldExtension.SimulantImNuis <- simulantImNuis
                world
            else
                let worldExtension = { world.WorldExtension with SimulantImNuis = simulantImNuis }
                World.choose { world with WorldExtension = worldExtension }

        static member internal getSimulantImNui simulant (world : World) =
            world.SimulantImNuis.[simulant]

        static member internal addSimulantImNui simulant simulantImNui (world : World) =
            let simulantImNuis = SUMap.add simulant simulantImNui world.SimulantImNuis
            World.setSimulantImNuis simulantImNuis world

        static member internal tryMapSimulantImNui mapper simulant (world : World) =
            match world.SimulantImNuis.TryGetValue simulant with
            | (true, simulantImNui) ->
                let simulantImNui = mapper simulantImNui
                World.addSimulantImNui simulant simulantImNui world
            | (false, _) -> world

        static member internal mapSimulantImNui mapper simulant world =
            let simulantImNui = World.getSimulantImNui simulant world
            let simulantImNui = mapper simulantImNui
            World.addSimulantImNui simulant simulantImNui world

        static member internal utilizeSimulantImNui simulant simulantImNui (world : World) =
            if world.Imperative then
                simulantImNui.SimulantUtilized <- true
                world
            else
                let simulantImNui = { simulantImNui with SimulantUtilized = true }
                let simulantImNuis = SUMap.add simulant simulantImNui world.SimulantImNuis
                World.setSimulantImNuis simulantImNuis world

        static member internal getSubscriptionImNuis (world : World) =
            world.SubscriptionImNuis

        static member internal setSubscriptionImNuis subscriptionImNuis (world : World) =
            if world.Imperative then
                world.WorldExtension.SubscriptionImNuis <- subscriptionImNuis
                world
            else
                let worldExtension = { world.WorldExtension with SubscriptionImNuis = subscriptionImNuis }
                World.choose { world with WorldExtension = worldExtension }

        static member internal getSubscriptionImNui subscription (world : World) =
            world.SubscriptionImNuis.[subscription]

        static member internal addSubscriptionImNui subscription subscriptionImNui (world : World) =
            let subscriptionImNuis = SUMap.add subscription subscriptionImNui world.SubscriptionImNuis
            World.setSubscriptionImNuis subscriptionImNuis world

        static member internal tryMapSubscriptionImNui mapper subscription (world : World) =
            match world.SubscriptionImNuis.TryGetValue subscription with
            | (true, subscriptionImNui) ->
                let subscriptionImNui = mapper subscriptionImNui
                World.addSubscriptionImNui subscription subscriptionImNui world
            | (false, _) -> world

        static member internal mapSubscriptionImNui mapper subscription world =
            let subscriptionImNui = World.getSubscriptionImNui subscription world
            let subscriptionImNui = mapper subscriptionImNui
            World.addSubscriptionImNui subscription subscriptionImNui world

        static member internal utilizeSubscriptionImNui subscription subscriptionImNui (world : World) =
            if world.Imperative then
                subscriptionImNui.SubscriptionUtilized <- true
                world
            else
                let subscriptionImNui = { subscriptionImNui with SubscriptionUtilized = true }
                let subscriptionImNuis = SUMap.add subscription subscriptionImNui world.SubscriptionImNuis
                World.setSubscriptionImNuis subscriptionImNuis world

        /// Switch simulation to use this ambient state.
        static member internal switchAmbientState world =
            World.choose { world with AmbientState = AmbientState.switch world.AmbientState }

        /// Place the engine into a state such that the app will exit at the end of the current frame.
        static member exit world =
            World.mapAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal removeTasklets simulant world =
            World.mapAmbientState (AmbientState.removeTasklets simulant) world

        static member internal clearTasklets world =
            World.mapAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.mapAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet simulant tasklet world =
            World.mapAmbientState (AmbientState.addTasklet simulant tasklet) world

        /// Schedule an operation to be executed by the engine with the given delay.
        static member schedule delay operation (simulant : Simulant) (world : World) =
            let time =
                match delay with
                | UpdateTime delay -> UpdateTime (world.UpdateTime + delay)
                | ClockTime delay -> ClockTime (world.ClockTime + delay)
            let tasklet = { ScheduledTime = time; ScheduledOp = operation }
            World.addTasklet simulant tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame or the next frame if we've already started processing tasklets.
        static member frame operation (simulant : Simulant) (world : World) =
            let time = if TaskletProcessingStarted && world.Advancing then UpdateTime 1L else UpdateTime 0L
            World.schedule time operation simulant world

        /// Attempt to get the window flags.
        static member tryGetWindowFlags world =
            World.getAmbientStateBy AmbientState.tryGetWindowFlags world

        /// Attempt to check that the window is minimized.
        static member tryGetWindowMinimized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMinimized world

        /// Attempt to check that the window is maximized.
        static member tryGetWindowMaximized world =
            World.getAmbientStateBy AmbientState.tryGetWindowMaximized world
            
        /// Attempt to check that the window is in a full screen state.
        static member tryGetWindowFullScreen world =
            World.getAmbientStateBy AmbientState.tryGetWindowFullScreen world

        /// Attempt to set the window's full screen state.
        static member trySetWindowFullScreen fullScreen world =
            World.mapAmbientState (AmbientState.trySetWindowFullScreen fullScreen) world

        /// Attempt to toggle the window's full screen state.
        static member tryToggleWindowFullScreen world =
            World.mapAmbientState AmbientState.tryToggleWindowFullScreen world

        /// Attempt to get the window position.
        static member tryGetWindowPosition world =
            World.getAmbientStateBy (AmbientState.tryGetWindowPosition) world

        /// Attempt to set the window position.
        static member trySetWindowPosition position world =
            World.getAmbientStateBy (AmbientState.trySetWindowPosition position) world
            world

        /// Attempt to get the window size.
        static member tryGetWindowSize world =
            World.getAmbientStateBy (AmbientState.tryGetWindowSize) world

        /// Get the window size, using resolution as default in case there is no window.
        static member getWindowSize world =
            match World.tryGetWindowSize world with
            | Some windowsSize -> windowsSize
            | None -> world.OuterViewport.Bounds.Size

        /// Attempt to set the window size.
        static member trySetWindowSize size world =
            World.getAmbientStateBy (AmbientState.trySetWindowSize size) world
            world

        /// Get the geometry viewport.
        static member getGeometryViewport (world : World) =
            world.GeometryViewport

        /// Set the geometry viewport.
        static member setGeometryViewport viewport (world : World) =
            { world with WorldExtension = { world.WorldExtension with GeometryViewport = viewport }}

        /// Get the inner viewport.
        static member getRasterViewport (world : World) =
            world.RasterViewport

        /// Set the inner viewport.
        static member setRasterViewport viewport (world : World) =
            { world with WorldExtension = { world.WorldExtension with RasterViewport = viewport }}

        /// Get the outer viewport.
        static member getOuterViewport (world : World) =
            world.OuterViewport

        /// Set the outer viewport.
        static member setOuterViewport viewport (world : World) =
            { world with WorldExtension = { world.WorldExtension with OuterViewport = viewport }}

        static member internal getSymbolicsBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolicsBy by) world

        static member internal getSymbolics world =
            World.getAmbientStateBy AmbientState.getSymbolics world

        static member internal setSymbolics symbolics world =
            World.mapAmbientState (AmbientState.setSymbolics symbolics) world

        static member internal mapSymbolics mapper world =
            World.mapAmbientState (AmbientState.mapSymbolics mapper) world

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
        static member reloadSymbols world =
            World.getSymbolicsBy Symbolics.reloadSymbols world
            world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.mapAmbientState (AmbientState.setOverlayer overlayer) world

        static member internal tryGetOverlayerPropertyValue propertyName propertyType overlayName facetNames world =
            World.getOverlayerBy (Overlayer.tryGetPropertyValue propertyName propertyType overlayName facetNames) world

        /// Get overlay names.
        static member getOverlayNames world =
            (World.getOverlayerBy Overlayer.getOverlays world).Keys

        /// Attempt to get the given dispatcher's optional routed overlay name.
        static member tryGetRoutedOverlayNameOpt dispatcherName world =
            World.getOverlayerBy (Overlayer.tryGetOverlayNameOpt dispatcherName) world

        static member internal acknowledgeLightMapRenderRequest world =
            World.mapAmbientState AmbientState.acknowledgeLightMapRenderRequest world

        /// Get whether a light map render was requested.
        static member getLightMapRenderRequested world =
            World.getAmbientStateBy AmbientState.getLightMapRenderRequested world

        /// Request a light map render for the current frame, such as when a light probe needs to be rendered.
        static member requestLightMapRender world =
            World.mapAmbientState AmbientState.requestLightMapRender world

    type World with // Quadtree

        static member internal getQuadtree world =
            world.Quadtree

    type World with // Octree

        static member internal getOctree world =
            world.Octree

    type World with // Subsystems

        static member internal getSubsystems world =
            world.Subsystems

        static member internal setSubsystems subsystems world =
            World.choose { world with Subsystems = subsystems }

        static member internal mapSubsystems mapper world =
            World.setSubsystems (mapper world.Subsystems) world

        static member internal cleanUpSubsystems world =
            World.mapSubsystems (fun subsystems ->
                subsystems.RendererProcess.Terminate ()
                subsystems.PhysicsEngine3d.CleanUp ()
                subsystems.PhysicsEngine2d.CleanUp ()
                subsystems.ImGui.CleanUp ()
                subsystems) world

    type World with // EventGraph

        static member internal getEventGraph world =
            world.EventGraph

        static member internal setEventGraph eventGraph world =
            World.choose { world with EventGraph = eventGraph }

        static member internal mapEventGraph mapper world =
            World.setEventGraph (mapper world.EventGraph) world

        static member inline internal boxCallback<'a, 's when 's :> Simulant> (callback : Callback<'a, 's>) : obj =
            let boxableCallback = fun (evt : Event<obj, Simulant>) (world : World) ->
                let evt = { Data = evt.Data :?> 'a; Subscriber = evt.Subscriber :?> 's; Publisher = evt.Publisher; Address = Address.specialize<'a> evt.Address; Trace = evt.Trace }
                callback evt world
            boxableCallback

        static member internal getGlobalSimulantGeneralized world =
            EventGraph.getGlobalSimulantGeneralized (World.getEventGraph world)

        static member internal getEventState<'a> key world : 'a =
            EventGraph.getEventState key (World.getEventGraph world)

        static member internal addEventState<'a> key (state : 'a) world =
            World.mapEventGraph (EventGraph.addEventState key state) world

        static member internal removeEventState key world =
            World.mapEventGraph (EventGraph.removeEventState key) world

        static member internal getSubscriptions world =
            EventGraph.getSubscriptions (World.getEventGraph world)

        static member internal setSubscriptions subscriptions world =
            World.mapEventGraph (EventGraph.setSubscriptions subscriptions) world

        static member internal getUnsubscriptions world =
            EventGraph.getUnsubscriptions (World.getEventGraph world)

        static member internal setUnsubscriptions unsubscriptions world =
            World.mapEventGraph (EventGraph.setUnsubscriptions unsubscriptions) world

        /// Get how events are being traced.
        static member getEventTracerOpt (world : World) =
            EventGraph.getEventTracerOpt world.EventGraph

        /// Set how events are being traced, if at all.
        static member setEventTracerOpt tracerOpt (world : World) =
            World.mapEventGraph (EventGraph.setEventTracerOpt tracerOpt) world

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventGraph.getEventFilter (World.getEventGraph world)

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            World.mapEventGraph (EventGraph.setEventFilter filter) world

        /// Publish an event.
        static member publishPlus<'a, 'p when 'p :> Simulant>
            (eventData : 'a)
            (eventAddress : 'a Address)
            eventTrace
            (publisher : 'p)
            hierarchical
            selectedOnly
            (world : World) =
#if DEBUG
            // log event based on event filter
            EventGraph.logEvent eventAddress eventTrace world.EventGraph
#endif
            // get subscriptions the fastest way possible
            // OPTIMIZATION: subscriptions nullable to elide allocation via Seq.empty.
            let subscriptionsOpt =
                if hierarchical then
                    EventGraph.getSubscriptionsSorted
                        sortSubscriptionsByElevation eventAddress world.EventGraph world
                else
                    let subscriptions = EventGraph.getSubscriptions world.EventGraph
                    match UMap.tryFind (eventAddress :> Address) subscriptions with
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
                        let subscriber = subscriptionEntry.SubscriptionSubscriber
                        if not selectedOnly || getSelected subscriber world then
                            let result =
                                let namesLength = subscriber.SimulantAddress.Names.Length
                                if namesLength >= 4 then
                                    // OPTIMIZATION: handling common case explicitly first.
                                    EventGraph.publishEvent<'a, 'p, Entity, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                else
                                    match namesLength with
                                    | 1 ->
                                        match subscriber with
                                        | :? Game -> EventGraph.publishEvent<'a, 'p, Game, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                        | :? GlobalSimulantGeneralized -> EventGraph.publishEvent<'a, 'p, Simulant, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                        | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with 1 name that is neither a Game or a GlobalSimulantGeneralized."); (Cascade, world)
                                    | 2 -> EventGraph.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | 3 -> EventGraph.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with no names."); (Cascade, world)
                            handling <- fst result
                            world <- snd result
                            world |> World.choose |> ignore
                        else () // nothing to do
                    else going <- false
                world
            else world

        /// Publish an event with no subscription sorting or wildcard utilization.
        static member inline publishUnsorted<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) (world : World) =
            World.publishPlus<'a, 'p> eventData eventAddress [] publisher false false world

        /// Publish an event with subscription sorting and wildcard utilization.
        static member inline publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) (world : World) =
            World.publishPlus<'a, 'p> eventData eventAddress [] publisher true false world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionId world =
            let subscriptions = World.getSubscriptions world
            let unsubscriptions = World.getUnsubscriptions world
            match UMap.tryFind subscriptionId unsubscriptions with
            | Some (eventAddress, _) ->
                match UMap.tryFind eventAddress subscriptions with
                | Some subscriptionEntries ->
                    let subscriptions =
                        let subscriptionEntries = OMap.remove subscriptionId subscriptionEntries
                        if OMap.isEmpty subscriptionEntries
                        then UMap.remove eventAddress subscriptions
                        else UMap.add eventAddress subscriptionEntries subscriptions
                    let unsubscriptions = UMap.remove subscriptionId unsubscriptions
                    let world = World.setSubscriptions subscriptions world
                    let world = World.setUnsubscriptions unsubscriptions world
                    let world = WorldTypes.handleSubscribeAndUnsubscribeEvent false eventAddress Game.Handle world :?> World
                    world
                | None -> world
            | None -> world

        /// Subscribe to an event using the given subscriptionId and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscriptionId : uint64)
            (callback : Event<'a, 's> -> World -> Handling * World)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            if not (Address.isEmpty eventAddress) then
                let (subscriptions, unsubscriptions) = (World.getSubscriptions world, World.getUnsubscriptions world)
                let subscriptions =
                    match UMap.tryFind (eventAddress :> Address) subscriptions with
                    | Some subscriptionEntries ->
                        match OMap.tryFind subscriptionId subscriptionEntries with
                        | Some subscriptionEntry ->
                            let subscriptionEntry = { subscriptionEntry with SubscriptionCallback = World.boxCallback callback }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add (eventAddress :> Address) subscriptionEntries subscriptions
                        | None ->
                            let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add eventAddress subscriptionEntries subscriptions
                    | None ->
                        let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                        UMap.add eventAddress (OMap.singleton HashIdentity.Structural (World.getCollectionConfig world) subscriptionId subscriptionEntry) subscriptions
                let unsubscriptions = UMap.add subscriptionId struct (eventAddress :> Address, subscriber :> Simulant) unsubscriptions
                let world = World.setSubscriptions subscriptions world
                let world = World.setUnsubscriptions unsubscriptions world
                let world = WorldTypes.handleSubscribeAndUnsubscribeEvent true eventAddress Game.Handle world :?> World
                (World.unsubscribe subscriptionId, world)
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribePlus Gen.id64 callback eventAddress subscriber world |> snd

        /// Keep active a subscription for the life span of a simulant.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling * World)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            let removalId = Gen.id64
            let monitorId = Gen.id64
            let world = World.subscribePlus<'a, 's> monitorId callback eventAddress subscriber world |> snd
            let unsubscribe = fun (world : World) ->
                let world = World.unsubscribe removalId world
                let world = World.unsubscribe monitorId world
                world
            let callback' = fun _ world -> (Cascade, unsubscribe world)
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> itoa subscriber.SimulantAddress
            let world = World.subscribePlus<unit, Simulant> removalId callback' unregisteringEventAddress subscriber world |> snd
            (unsubscribe, world)

        /// Keep active a subscription for the life span of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : 's) (world : World) =
            World.monitorPlus<'a, 's> callback eventAddress subscriber world |> snd

        /// Keep active a subscription for the life span of an entity and a given facet.
        static member sensePlus<'a>
            (callback : Event<'a, Entity> -> World -> Handling * World)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            let removalId = Gen.id64
            let fastenId = Gen.id64
            let senseId = Gen.id64
            let world = World.subscribePlus<'a, Entity> senseId callback eventAddress entity world |> snd
            let unsubscribe = fun (world : World) ->
                let world = World.unsubscribe removalId world
                let world = World.unsubscribe fastenId world
                let world = World.unsubscribe senseId world
                world
            let callback' = fun _ world -> (Cascade, unsubscribe world)
            let callback'' = fun changeEvent world ->
                let previous = changeEvent.Data.Previous :?> string Set
                let value = changeEvent.Data.Value :?> string Set
                if previous.Contains facetName && not (value.Contains facetName)
                then (Cascade, unsubscribe world)
                else (Cascade, world)
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> entity.EntityAddress
            let changeFacetNamesEventAddress = rtoa<ChangeData> [|"Change"; "FacetNames"; "Event"|] --> entity.EntityAddress
            let world = World.subscribePlus<unit, Simulant> removalId callback' unregisteringEventAddress entity world |> snd
            let world = World.subscribePlus<ChangeData, Simulant> fastenId callback'' changeFacetNamesEventAddress entity world |> snd
            (unsubscribe, world)

        /// Keep active a subscription for the life span of an entity and a given facet.
        static member sense<'a>
            (callback : Event<'a, Entity> -> World -> Handling * World) (eventAddress : 'a Address) (subscriber : Entity) (facetName : string) (world : World) =
            World.sensePlus callback eventAddress subscriber facetName world |> snd

    type World with // KeyValueStore (tho part of AmbientState, must come after EventGraph definitions since it publishes)

        static member internal getKeyValueStore world =
            World.getAmbientStateBy AmbientState.getKeyValueStore world

        static member internal getKeyValueStoreBy by world =
            World.getAmbientStateBy (AmbientState.getKeyValueStoreBy by) world

        static member internal setKeyValueStore symbolics world =
            World.mapAmbientState (AmbientState.setKeyValueStore symbolics) world

        static member internal mapKeyValueStore mapper world =
            World.mapAmbientState (AmbientState.mapKeyValueStore mapper) world

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
            match World.getKeyValueStoreBy (SUMap.tryFind key) world with
            | Some value -> Some (value :?> 'a)
            | None -> None

        /// Look up a value from the world's key value store, throwing an exception if it is not found.
        static member getKeyedValue<'a> key world =
            World.getKeyValueStoreBy (SUMap.find key) world :?> 'a

        /// Add a value to the world's key value store.
        static member addKeyedValue<'a> key (value : 'a) world =
            let previousOpt = World.tryGetKeyedValue key world
            let valueOpt = Some (value :> obj)
            let data = { Key = key; PreviousOpt = previousOpt; ValueOpt = valueOpt }
            let world = World.mapKeyValueStore (SUMap.add key (value :> obj)) world
            match previousOpt with
            | Some previous ->
                if previous =/= value
                then World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
                else world
            | None -> World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world

        /// Remove a value from the world's key value store.
        static member removeKeyedValue key world =
            let previousOpt = World.tryGetKeyedValue key world
            match previousOpt with
            | Some _ ->
                let world = World.mapKeyValueStore (SUMap.remove key) world
                World.publish { Key = key; PreviousOpt = previousOpt; ValueOpt = None } (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | None -> world

        /// Transform a value in the world's key value store if it exists.
        static member mapKeyedValue<'a> (mapper : 'a -> 'a) key world =
            World.addKeyedValue<'a> key (mapper (World.getKeyedValue<'a> key world)) world

    type World with // Plugin

        /// Whether the current plugin allow code reloading.
        static member getAllowCodeReload world =
            world.WorldExtension.Plugin.AllowCodeReload

        /// Get the user-defined edit modes.
        static member getEditModes world =
            world.WorldExtension.Plugin.EditModes

        /// Attempt to set the edit mode.
        static member trySetEditMode editMode world =
            match (World.getEditModes world).TryGetValue editMode with
            | (true, callback) -> callback world
            | (false, _) -> world

        /// Invoke a user-defined callback.
        static member invoke name args world =
            world.WorldExtension.Plugin.Invoke name args world

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle world =
            world.WorldExtension.Plugin.TryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle

        static member internal preProcess world =
            world.WorldExtension.Plugin.PreProcess world

        static member internal perProcess world =
            world.WorldExtension.Plugin.PerProcess world

        static member internal postProcess world =
            world.WorldExtension.Plugin.PostProcess world

        static member internal imGuiProcess world =
            world.WorldExtension.Plugin.ImGuiProcess world

        static member internal imGuiPostProcess world =
            world.WorldExtension.Plugin.ImGuiPostProcess world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getSimulantStateMemberProperties (state : SimulantState) =
            state |>
            getType |>
            (fun ty -> ty.GetProperties true) |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.PropertyType, property.GetValue state)) |>
            Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getSimulantStateXtensionProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            List.ofSeq |>
            List.sortBy fst |>
            List.map (fun (name, property) -> (name, property.PropertyType, property.PropertyValue))

        /// Provides a full view of all the properties of some SimulantState.
        static member internal getSimulantStateProperties state =
            List.append
                (World.getSimulantStateMemberProperties state)
                (World.getSimulantStateXtensionProperties state)

        /// Present SimulantState properties for viewing.
        static member internal viewSimulantStateProperties state =
            let properties = World.getSimulantStateProperties state
            properties |> Array.ofList |> Array.map a_c |> Array.sortBy fst

    type World with // Handlers

        /// Handle an event by doing nothing.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Handle an event by swallowing.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)

        /// Handle an event by exiting the application.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)