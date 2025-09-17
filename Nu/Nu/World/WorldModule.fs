// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Reflection
open Prime

/// Global operators for the world.
[<AutoOpen>]
module WorldModuleOperators =

    /// Attempt to resolve a simulant from the given relation and simulant.
    let tryResolve<'t when 't :> Simulant> (relation : 't Address) (simulant : Simulant) : 't option =
        let simulant2 = Address.resolve<Simulant, 't> relation (itoa simulant.SimulantAddress)
        if simulant2.Names.Length >= 4 && typeof<'t> = typeof<Entity> then Some (Entity (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 3 && typeof<'t> = typeof<Group> then Some (Group (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 2 && typeof<'t> = typeof<Screen> then Some (Screen (simulant2.Names) :> Simulant :?> 't)
        elif simulant2.Names.Length = 1 && typeof<'t> = typeof<Game> then Some (Game.Handle :> Simulant :?> 't)
        else None
        
    /// Relate the second simulant to the first. When the given simulants share common ancestors, the result is a
    /// relative address. Otherwise, the result is an absolute address.
    let relate<'t when 't :> Simulant> (simulant : Simulant) (simulant2 : 't) : 't Address =
        Address.relate<Simulant, 't> (itoa simulant.SimulantAddress) (itoa simulant2.SimulantAddress)

/// Universal function definitions for the world (1/4).
[<AutoOpen>]
module WorldModule =

    /// Track if we're in the portion of the frame simulants are being updated.
    /// TODO: P1: consider making this an AmbientState flag.
    let mutable internal UpdatingSimulants = false

    /// Track if we're in the portion of the frame when end-frame processing has started.
    /// TODO: P1: consider making this an AmbientState flag.
    let mutable internal EndFrameProcessingStarted = false

    /// F# reach-around for checking that a simulant is selected.
    let mutable internal getSelected : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// F# reach-around for sorting subscriptions by elevation.
    let mutable internal sortSubscriptionsByElevation : (uint64 * SubscriptionEntry) seq -> obj -> (uint64 * SubscriptionEntry) seq =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal evictScreenElements : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal admitScreenElements : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach-around for registering physics entities of an entire screen.
    let mutable internal registerScreenPhysics : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach-around for unregistering physics entities of an entire screen.
    let mutable internal unregisterScreenPhysics : Screen -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal register : Simulant -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal unregister : Simulant -> World -> unit =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessGame : bool -> Game -> World -> unit =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessScreen : bool -> Screen -> World -> unit =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessGroup : bool -> Group -> World -> unit =
        Unchecked.defaultof<_>
        
    let mutable internal tryProcessEntity : bool -> Entity -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal signal : obj -> Simulant -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal destroyImmediate : Simulant -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal destroy : Simulant -> World -> unit =
        Unchecked.defaultof<_>

    let mutable internal getEmptyEffect : unit -> obj =
        Unchecked.defaultof<_>

    type World with // JobGraph

        /// Enqueue a job for threaded execution.
        static member enqueueJob priority job (world : World) =
            world.WorldExtension.JobGraph.Enqueue (priority, job)

        /// Await a job from threaded execution.
        /// Order of jobs with the same key is not guaranteed.
        static member tryAwaitJob deadline (jobId : obj) (world : World) =
            world.WorldExtension.JobGraph.TryAwait (deadline, jobId)

    type World with // Destruction

        static member internal getDestructionListRev (world : World) =
            world.WorldExtension.DestructionListRev

        static member internal addSimulantToDestruction simulant (world : World) =
            let worldExtension = { world.WorldExtension with DestructionListRev = simulant :: world.WorldExtension.DestructionListRev }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal tryRemoveSimulantFromDestruction simulant (world : World) =
            let worldExtension = { world.WorldExtension with DestructionListRev = List.remove ((=) simulant) world.WorldExtension.DestructionListRev }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

    type World with // Dispatchers

        /// Get the facets of the world.
        static member getFacets (world : World) =
            world.WorldExtension.Dispatchers.Facets

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers (world : World) =
            world.WorldExtension.Dispatchers.EntityDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers (world : World) =
            world.WorldExtension.Dispatchers.GroupDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers (world : World) =
            world.WorldExtension.Dispatchers.ScreenDispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers (world : World) =
            world.WorldExtension.Dispatchers.GameDispatchers

    type World with // AmbientState

        static member internal getAmbientState (world : World) =
            world.AmbientState

        static member internal mapAmbientState mapper (world : World) =
            world.WorldState <- { world.WorldState with AmbientState = mapper world.AmbientState }

        /// Check that the update rate is non-zero.
        static member getAdvancing (world : World) =
            world.Advancing

        /// Check that the update rate is zero.
        static member getHalted (world : World) =
            world.Halted

        /// Set whether the world's frame rate is being explicitly paced based on clock progression.
        static member setFramePacing clockPacing (world : World) =
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
        static member getCollectionConfig (world : World) =
            AmbientState.getConfig world.AmbientState

        /// Get the the aliveness state of the engine.
        static member getAlive (world : World) =
            AmbientState.getAlive world.AmbientState

        static member internal updateTime world =
            World.mapAmbientState AmbientState.updateTime world

        /// Get the number of updates that have transpired between this and the previous frame.
        static member getUpdateDelta world =
            AmbientState.getUpdateDelta world.AmbientState
            
        /// Get the number of updates that have transpired since the game began advancing.
        static member getUpdateTime world =
            AmbientState.getUpdateTime world.AmbientState
            
        /// Get the amount of clock time (in seconds) between this and the previous frame. Clock time is the primary means
        /// for scaling frame-based phenomena like speeds and impulses.
        static member getClockDelta world =
            AmbientState.getClockDelta world.AmbientState
            
        /// Get the amount of clock time (in seconds) that has transpired since the world began advancing. Clock time is
        /// the primary means for scaling frame-based phenomena like speeds and impulses.
        static member getClockTime world =
            AmbientState.getClockTime world.AmbientState

        /// Get the tick delta as a number of environment ticks between this and the previous frame.
        static member getTickDelta world =
            AmbientState.getTickDelta world.AmbientState

        /// Get the tick time as a number of environment ticks that have transpired since the world began advancing.
        static member getTickTime world =
            AmbientState.getTickTime world.AmbientState

        /// Get the polymorphic engine time between this and the previous frame.
        static member getGameDelta world =
            AmbientState.getGameDelta world.AmbientState

        /// Get the polymorphic engine time that has transpired since the world began advancing.
        static member getGameTime world =
            AmbientState.getGameTime world.AmbientState

        /// Get the amount of date time that has transpired between this and the previous frame. This value is independent
        /// of whether the world was or is advancing.
        static member getDateDelta world =
            AmbientState.getDateDelta world.AmbientState

        /// Get the date time as of the start of this frame. This value is independent of whether the world was or is
        /// advancing.
        static member getDateTime world =
            AmbientState.getDateTime world.AmbientState

        /// Get the current ImSim context.
        static member getContextImSim (world : World) =
            world.ContextImSim

        /// Get the current ImSim context translated to a Game handle (throwing upon failure).
        static member getContextGame (world : World) =
            world.ContextGame

        /// Get the current ImSim context translated to a Screen handle (throwing upon failure).
        static member getContextScreen (world : World) =
            world.ContextScreen

        /// Get the current ImSim context translated to a Group handle (throwing upon failure).
        static member getContextGroup (world : World) =
            world.ContextGroup

        /// Get the current ImSim context translated to a Entity handle (throwing upon failure).
        static member getContextEntity (world : World) =
            world.ContextEntity

        /// Check that the current ImSim context is initializing this frame.
        static member getContextInitializing (world : World) =
            world.ContextInitializing

        /// Get the recent ImSim declaration.
        static member getDeclaredImSim (world : World) =
            world.DeclaredImSim

        /// Get the recent ImSim declaration translated to a Game handle (throwing upon failure).
        static member getDeclaredGame (world : World) =
            world.DeclaredGame

        /// Get the recent ImSim declaration translated to a Screen handle (throwing upon failure).
        static member getDeclaredScreen (world : World) =
            world.DeclaredScreen

        /// Get the recent ImSim declaration translated to a Group handle (throwing upon failure).
        static member getDeclaredGroup (world : World) =
            world.DeclaredGroup

        /// Get the recent ImSim declaration translated to a Entity handle (throwing upon failure).
        static member getDeclaredEntity (world : World) =
            world.DeclaredEntity

        /// Check that the recent ImSim declaration is initializing this frame.
        static member getDeclaredInitializing (world : World) =
            world.DeclaredInitializing

        static member internal setContextAndDeclared context declared (world : World) =
            if world.Imperative then
                world.WorldExtension.ContextImSim <- context
                world.WorldExtension.DeclaredImSim <- declared
            else
                let worldExtension = { world.WorldExtension with ContextImSim = context; DeclaredImSim = declared }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal setContext context (world : World) =
            World.setContextAndDeclared context world.WorldExtension.ContextImSim world

        static member internal getSimulantsImSim (world : World) =
            world.SimulantsImSim

        static member internal setSimulantsImSim simulantsImSim (world : World) =
            if world.Imperative then
                world.WorldExtension.SimulantsImSim <- simulantsImSim
            else
                let worldExtension = { world.WorldExtension with SimulantsImSim = simulantsImSim }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal getSimulantImSim simulant (world : World) =
            world.SimulantsImSim.[simulant]

        static member internal addSimulantImSim simulant simulantImSim (world : World) =
            let simulantsImSim = SUMap.add simulant simulantImSim world.SimulantsImSim
            World.setSimulantsImSim simulantsImSim world

        static member internal removeSimulantImSim (simulant : Simulant) (world : World) =
            World.setSimulantsImSim (SUMap.remove simulant.SimulantAddress world.SimulantsImSim) world

        static member internal tryMapSimulantImSim mapper simulant (world : World) =
            match world.SimulantsImSim.TryGetValue simulant with
            | (true, simulantImSim) ->
                let simulantImSim = mapper simulantImSim
                World.addSimulantImSim simulant simulantImSim world
            | (false, _) -> ()

        static member internal mapSimulantImSim mapper simulant world =
            let simulantImSim = World.getSimulantImSim simulant world
            let simulantImSim = mapper simulantImSim
            World.addSimulantImSim simulant simulantImSim world

        static member internal utilizeSimulantImSim simulant simulantImSim (world : World) =
            if world.Imperative then
                simulantImSim.SimulantUtilized <- true
            else
                let simulantImSim = { simulantImSim with SimulantUtilized = true }
                let simulantsImSim = SUMap.add simulant simulantImSim world.SimulantsImSim
                World.setSimulantsImSim simulantsImSim world

        static member internal getSubscriptionsImSim (world : World) =
            world.SubscriptionsImSim

        static member internal setSubscriptionsImSim subscriptionsImSim (world : World) =
            if world.Imperative then
                world.WorldExtension.SubscriptionsImSim <- subscriptionsImSim
            else
                let worldExtension = { world.WorldExtension with SubscriptionsImSim = subscriptionsImSim }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal getSubscriptionImSim subscription (world : World) =
            world.SubscriptionsImSim.[subscription]

        static member internal addSubscriptionImSim subscription subscriptionImSim (world : World) =
            let subscriptionsImSim = SUMap.add subscription subscriptionImSim world.SubscriptionsImSim
            World.setSubscriptionsImSim subscriptionsImSim world

        static member internal tryMapSubscriptionImSim mapper subscription (world : World) =
            match world.SubscriptionsImSim.TryGetValue subscription with
            | (true, subscriptionImSim) ->
                let subscriptionImSim = mapper subscriptionImSim
                World.addSubscriptionImSim subscription subscriptionImSim world
            | (false, _) -> ()

        static member internal mapSubscriptionImSim mapper subscription world =
            let subscriptionImSim = World.getSubscriptionImSim subscription world
            let subscriptionImSim = mapper subscriptionImSim
            World.addSubscriptionImSim subscription subscriptionImSim world

        static member internal utilizeSubscriptionImSim subscription subscriptionImSim (world : World) =
            if world.Imperative then
                subscriptionImSim.SubscriptionUtilized <- true
            else
                let subscriptionImSim = { subscriptionImSim with SubscriptionUtilized = true }
                let subscriptionsImSim = SUMap.add subscription subscriptionImSim world.SubscriptionsImSim
                World.setSubscriptionsImSim subscriptionsImSim world

        /// Switch simulation to use this ambient state.
        static member internal switchAmbientState (world : World) =
            let ambientState = AmbientState.switch world.AmbientState
            world.WorldState <- { world.WorldState with AmbientState = ambientState }

        /// Place the engine into a state such that the app will exit at the end of the current frame.
        static member exit world =
            World.mapAmbientState AmbientState.exit world

        static member internal getCoroutines (world : World) =
            AmbientState.getCoroutines world.AmbientState

        static member internal setCoroutines coroutines world =
            World.mapAmbientState (AmbientState.setCoroutines coroutines) world

        /// Launch a coroutine to be processed by the engine.
        static member launchCoroutine pred coroutine (world : World) =
            World.mapAmbientState (AmbientState.addCoroutine (world.GameTime, pred, coroutine)) world

        static member internal getTasklets (world : World) =
            AmbientState.getTasklets world.AmbientState

        static member internal removeTasklets simulant world =
            World.mapAmbientState (AmbientState.removeTasklets simulant) world

        static member internal clearTasklets world =
            World.mapAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.mapAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet (simulant : Simulant) tasklet world =
            World.mapAmbientState (AmbientState.addTasklet simulant tasklet) world

        /// Schedule an operation to be executed by the engine with the given delay.
        /// When called in an ImSim Process context, will provide the ImSim simulant context and declared values from
        /// World that were active in that Process context as well as time and advancement state.
        static member schedule delay operation (simulant : Simulant) (world : World) =

            // compute time at which to schedule the operation
            let time =
                match delay with
                | UpdateTime delay -> UpdateTime (world.UpdateTime + delay)
                | TickTime delay -> TickTime (world.TickTime + delay)

            // restore the ImSim context when we're in an ImSim Process context
            let operation =
                let context = world.ContextImSim
                if context.Names.Length > 0 then
                    let declared = world.DeclaredImSim
                    let advancing = world.Advancing
                    let advancementCleared = world.AdvancementCleared
                    let updateDelta = world.UpdateDelta
                    let clockDelta = world.ClockDelta
                    let tickDelta = world.TickDelta
                    fun (world : World) ->
                        let context' = world.ContextImSim
                        let declared' = world.DeclaredImSim
                        World.mapAmbientState AmbientState.clearAdvancement world
                        World.setContextAndDeclared context declared world
                        operation world
                        World.setContextAndDeclared context' declared' world
                        World.mapAmbientState (AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta) world
                else operation

            // add tasklet
            let tasklet = { ScheduledTime = time; ScheduledOp = operation }
            World.addTasklet simulant tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame or the next frame if
        /// we've already started processing tasklets.
        /// When called in an ImSim Process context, will provide the ImSim simulant context and declared values from
        /// World that were active in that Process context as well as time and advancement state.
        static member defer operation (simulant : Simulant) (world : World) =
            let time = if EndFrameProcessingStarted && world.Advancing then GameTime.epsilon else GameTime.zero
            World.schedule time operation simulant world

        /// Attempt to get the window flags.
        static member tryGetWindowFlags (world : World) =
            AmbientState.tryGetWindowFlags world.AmbientState

        /// Attempt to check that the window is minimized.
        static member tryGetWindowMinimized (world : World) =
            AmbientState.tryGetWindowMinimized world.AmbientState

        /// Attempt to check that the window is maximized.
        static member tryGetWindowMaximized (world : World) =
            AmbientState.tryGetWindowMaximized world.AmbientState
            
        /// Attempt to check that the window is in a full screen state.
        static member tryGetWindowFullScreen (world : World) =
            AmbientState.tryGetWindowFullScreen world.AmbientState

        /// Attempt to set the window's full screen state.
        static member trySetWindowFullScreen fullScreen world =
            World.mapAmbientState (AmbientState.trySetWindowFullScreen fullScreen) world

        /// Attempt to toggle the window's full screen state.
        static member tryToggleWindowFullScreen world =
            World.mapAmbientState AmbientState.tryToggleWindowFullScreen world

        /// Attempt to get the window position.
        static member tryGetWindowPosition (world : World) =
            AmbientState.tryGetWindowPosition world.AmbientState

        /// Attempt to set the window position.
        static member trySetWindowPosition position (world : World) =
            AmbientState.trySetWindowPosition position world.AmbientState

        /// Attempt to get the window size.
        static member tryGetWindowSize (world : World) =
            AmbientState.tryGetWindowSize world.AmbientState

        /// Get the window size, using resolution as default in case there is no window.
        static member getWindowSize world =
            match World.tryGetWindowSize world with
            | Some windowsSize -> windowsSize
            | None -> world.OuterViewport.Bounds.Size

        /// Attempt to set the window size.
        static member trySetWindowSize size (world : World) =
            AmbientState.trySetWindowSize size world.AmbientState

        /// Get the geometry viewport.
        static member getGeometryViewport (world : World) =
            world.GeometryViewport

        /// Set the geometry viewport.
        static member setGeometryViewport viewport (world : World) =
            let worldExtension = { world.WorldExtension with GeometryViewport = viewport }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Get the inner viewport.
        static member getRasterViewport (world : World) =
            world.RasterViewport

        /// Set the inner viewport.
        static member setRasterViewport viewport (world : World) =
            let worldExtension = { world.WorldExtension with RasterViewport = viewport }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Get the outer viewport.
        static member getOuterViewport (world : World) =
            world.OuterViewport

        /// Set the outer viewport.
        static member setOuterViewport viewport (world : World) =
            let worldExtension = { world.WorldExtension with OuterViewport = viewport }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal getSymbolics (world : World) =
            AmbientState.getSymbolics world.WorldState.AmbientState

        static member internal setSymbolics symbolics world =
            World.mapAmbientState (AmbientState.setSymbolics symbolics) world

        static member internal mapSymbolics mapper world =
            World.mapAmbientState (AmbientState.mapSymbolics mapper) world

        /// Try to load a symbol package with the given name.
        static member tryLoadSymbolPackage implicitDelimiters packageName world =
            let symbolics = World.getSymbolics world
            Symbolics.tryLoadSymbolPackage implicitDelimiters packageName symbolics

        /// Unload a symbol package with the given name.
        static member unloadSymbolPackage packageName world =
            let symbolics = World.getSymbolics world
            Symbolics.unloadSymbolPackage packageName symbolics

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
            let symbolics = World.getSymbolics world
            Symbolics.reloadSymbols symbolics

        static member internal getOverlayer (world : World) =
            AmbientState.getOverlayer world.AmbientState

        static member internal setOverlayer overlayer world =
            World.mapAmbientState (AmbientState.setOverlayer overlayer) world

        static member internal tryGetOverlayerPropertyValue propertyName propertyType overlayName facetNames world =
            let overlayer = World.getOverlayer world
            Overlayer.tryGetPropertyValue propertyName propertyType overlayName facetNames overlayer

        /// Get overlay names.
        static member getOverlayNames world =
            let overlayer = World.getOverlayer world
            (Overlayer.getOverlays overlayer).Keys

        /// Attempt to get the given dispatcher's optional routed overlay name.
        static member tryGetRoutedOverlayNameOpt dispatcherName world =
            let overlayer = World.getOverlayer world
            Overlayer.tryGetOverlayNameOpt dispatcherName overlayer

        static member internal acknowledgeLightMapRenderRequest world =
            World.mapAmbientState AmbientState.acknowledgeLightMapRenderRequest world

        /// Get whether a light map render was requested.
        static member getLightMapRenderRequested (world : World) =
            AmbientState.getLightMapRenderRequested world.AmbientState

        /// Request a light map render for the current frame, such as when a light probe needs to be rendered.
        static member requestLightMapRender world =
            World.mapAmbientState AmbientState.requestLightMapRender world

        /// A coroutine launcher.
        member this.Launcher =
            flip (World.launchCoroutine tautology) this

        /// A cancellable coroutine launcher.
        member this.LauncherWhile pred =
            flip (World.launchCoroutine pred) this

    type World with // Subsystems

        static member internal getSubsystems world =
            world.Subsystems

        static member internal setSubsystems subsystems world =
            world.WorldState <- { world.WorldState with Subsystems = subsystems }

        static member internal mapSubsystems mapper (world : World) =
            World.setSubsystems (mapper world.Subsystems) world

        static member internal cleanUpSubsystems world =
            World.mapSubsystems (fun subsystems ->
                subsystems.AudioPlayer.CleanUp ()
                match subsystems.RendererPhysics3dOpt with Some renderer -> renderer.Dispose () | None -> ()
                subsystems.RendererProcess.Terminate ()
                subsystems.PhysicsEngine3d.CleanUp ()
                subsystems.PhysicsEngine2d.CleanUp ()
                subsystems.ImGui.CleanUp ()
                subsystems)
                world

    type World with // EventGraph

        static member internal getEventGraph (world : World) =
            world.EventGraph

        static member internal setEventGraph eventGraph world =
            world.WorldState <- { world.WorldState with EventGraph = eventGraph }

        static member internal mapEventGraph mapper (world : World) =
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

            // OPTIMIZATION: generalize only once
            let eventAddressObj = Address.generalize eventAddress

#if DEBUG
            // log event based on event filter
            EventGraph.logEvent eventAddressObj eventTrace world.EventGraph
#endif

            // get subscriptions the fastest way possible
            // OPTIMIZATION: subscriptions nullable to elide allocation via Seq.empty.
            let subscriptionsOpt =
                if hierarchical then
                    EventGraph.getSubscriptionsSorted
                        sortSubscriptionsByElevation eventAddressObj world.EventGraph world
                else
                    let subscriptions = EventGraph.getSubscriptions world.EventGraph
                    match UMap.tryFind eventAddressObj subscriptions with
                    | Some subscriptions -> OMap.toSeq subscriptions
                    | None -> null

            // publish to each subscription
            // OPTIMIZATION: inlined foldWhile here in order to compact the call stack.
            // OPTIMIZATION: fused PublishEventHook for speed.
            if notNull subscriptionsOpt then
                let mutable (going, enr) = (true, subscriptionsOpt.GetEnumerator ())
                let mutable handling = Cascade
                while going && enr.MoveNext () do
                    let (_, subscriptionEntry) = enr.Current
                    if (match handling with Cascade -> true | Resolve -> false) && world.Alive then
                        let subscriber = subscriptionEntry.SubscriptionSubscriber
                        if not selectedOnly || getSelected subscriber world then
                            let namesLength = subscriber.SimulantAddress.Names.Length
                            if namesLength >= 4 then
                                // OPTIMIZATION: handling common case explicitly first.
                                handling <- EventGraph.publishEvent<'a, 'p, Entity, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                            else
                                match namesLength with
                                | 1 ->
                                    match subscriber with
                                    | :? Game -> handling <- EventGraph.publishEvent<'a, 'p, Game, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | :? GlobalSimulantGeneralized -> handling <- EventGraph.publishEvent<'a, 'p, Simulant, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with 1 name that is neither a Game or a GlobalSimulantGeneralized.")
                                | 2 -> handling <- EventGraph.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                | 3 -> handling <- EventGraph.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with no names.")
                    else going <- false

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
                    World.setSubscriptions subscriptions world
                    World.setUnsubscriptions unsubscriptions world
                    WorldTypes.handleSubscribeAndUnsubscribeEvent false eventAddress Game.Handle world
                | None -> ()
            | None -> ()

        /// Subscribe to an event using the given subscriptionId and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscriptionId : uint64)
            (callback : Event<'a, 's> -> World -> Handling)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            if not (Address.isEmpty eventAddress) then
                let eventAddressObj = atooa eventAddress
                let (subscriptions, unsubscriptions) = (World.getSubscriptions world, World.getUnsubscriptions world)
                let subscriptions =
                    match UMap.tryFind eventAddressObj subscriptions with
                    | Some subscriptionEntries ->
                        match OMap.tryFind subscriptionId subscriptionEntries with
                        | Some subscriptionEntry ->
                            let subscriptionEntry = { subscriptionEntry with SubscriptionCallback = World.boxCallback callback }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add eventAddressObj subscriptionEntries subscriptions
                        | None ->
                            let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add eventAddressObj subscriptionEntries subscriptions
                    | None ->
                        let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                        UMap.add eventAddressObj (OMap.singleton HashIdentity.Structural (World.getCollectionConfig world) subscriptionId subscriptionEntry) subscriptions
                let unsubscriptions = UMap.add subscriptionId struct (eventAddressObj, subscriber :> Simulant) unsubscriptions
                World.setSubscriptions subscriptions world
                World.setUnsubscriptions unsubscriptions world
                WorldTypes.handleSubscribeAndUnsubscribeEvent true eventAddressObj Game.Handle world
                World.unsubscribe subscriptionId
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribePlus Gen.id64 callback eventAddress subscriber world |> ignore

        /// Keep active a subscription for the life span of a simulant.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            let removalId = Gen.id64
            let monitorId = Gen.id64
            World.subscribePlus<'a, 's> monitorId callback eventAddress subscriber world |> ignore
            let unsubscribe = fun (world : World) ->
                World.unsubscribe removalId world
                World.unsubscribe monitorId world
            let callback' = fun _ world -> unsubscribe world; Cascade
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> itoa subscriber.SimulantAddress
            World.subscribePlus<unit, Simulant> removalId callback' unregisteringEventAddress subscriber world |> ignore
            unsubscribe

        /// Keep active a subscription for the life span of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling) (eventAddress : 'a Address) (subscriber : 's) (world : World) =
            World.monitorPlus<'a, 's> callback eventAddress subscriber world |> ignore

        /// Keep active a subscription for the life span of an entity and a given facet.
        static member sensePlus<'a>
            (callback : Event<'a, Entity> -> World -> Handling)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            let removalId = Gen.id64
            let fastenId = Gen.id64
            let senseId = Gen.id64
            World.subscribePlus<'a, Entity> senseId callback eventAddress entity world |> ignore
            let unsubscribe = fun (world : World) ->
                World.unsubscribe removalId world
                World.unsubscribe fastenId world
                World.unsubscribe senseId world
            let callback' = fun _ world -> unsubscribe world; Cascade
            let callback'' = fun changeEvent world ->
                let previous = changeEvent.Data.Previous :?> string Set
                let value = changeEvent.Data.Value :?> string Set
                if previous.Contains facetName && not (value.Contains facetName) then unsubscribe world
                Cascade
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> entity.EntityAddress
            let changeFacetNamesEventAddress = rtoa<ChangeData> [|"Change"; "FacetNames"; "Event"|] --> entity.EntityAddress
            World.subscribePlus<unit, Simulant> removalId callback' unregisteringEventAddress entity world |> ignore
            World.subscribePlus<ChangeData, Simulant> fastenId callback'' changeFacetNamesEventAddress entity world |> ignore
            unsubscribe

        /// Keep active a subscription for the life span of an entity and a given facet.
        static member sense<'a>
            (callback : Event<'a, Entity> -> World -> Handling) (eventAddress : 'a Address) (subscriber : Entity) (facetName : string) (world : World) =
            World.sensePlus callback eventAddress subscriber facetName world |> ignore

    type World with // KeyValueStore (tho part of AmbientState, must come after EventGraph definitions since it publishes)

        static member internal getKeyValueStore (world : World) =
            AmbientState.getKeyValueStore world.AmbientState

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
            let keyValueStore = World.getKeyValueStore world
            match SUMap.tryFind key keyValueStore with
            | Some value -> Some (value :?> 'a)
            | None -> None

        /// Look up a value from the world's key value store, throwing an exception if it is not found.
        static member getKeyedValue<'a> key world =
            let keyValueStore = World.getKeyValueStore world
            SUMap.find key keyValueStore :?> 'a

        /// Add a value to the world's key value store.
        static member addKeyedValue<'a> key (value : 'a) world =
            let previousOpt = World.tryGetKeyedValue key world
            let valueOpt = Some (value :> obj)
            let data = { Key = key; PreviousOpt = previousOpt; ValueOpt = valueOpt }
            World.mapKeyValueStore (SUMap.add key (value :> obj)) world
            match previousOpt with
            | Some previous ->
                if previous =/= value then
                    World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | None -> World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world

        /// Remove a value from the world's key value store.
        static member removeKeyedValue key world =
            let previousOpt = World.tryGetKeyedValue key world
            match previousOpt with
            | Some _ ->
                World.mapKeyValueStore (SUMap.remove key) world
                World.publish { Key = key; PreviousOpt = previousOpt; ValueOpt = None } (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | None -> ()

        /// Transform a value in the world's key value store if it exists.
        static member mapKeyedValue<'a> (mapper : 'a -> 'a) key world =
            World.addKeyedValue<'a> key (mapper (World.getKeyedValue<'a> key world)) world

    type World with // Plugin

        /// Whether the current plugin allow code reloading.
        static member getAllowCodeReload (world : World) =
            world.WorldExtension.Plugin.AllowCodeReload

        /// Get the user-defined edit modes.
        static member getEditModes (world : World) =
            world.WorldExtension.Plugin.EditModes

        /// Attempt to set the edit mode.
        static member trySetEditMode editMode world =
            match (World.getEditModes world).TryGetValue editMode with
            | (true, callback) -> callback world
            | (false, _) -> ()

        /// Invoke a user-defined callback.
        static member invoke name args (world : World) =
            world.WorldExtension.Plugin.Invoke name args world

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle (world : World) =
            world.WorldExtension.Plugin.TryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle

        static member internal preProcess (world : World) =
            world.WorldExtension.Plugin.PreProcess world

        static member internal perProcess (world : World) =
            world.WorldExtension.Plugin.PerProcess world

        static member internal postProcess (world : World) =
            world.WorldExtension.Plugin.PostProcess world

        static member internal imGuiProcess (world : World) =
            world.WorldExtension.Plugin.ImGuiProcess world

        static member internal imGuiPostProcess (world : World) =
            world.WorldExtension.Plugin.ImGuiPostProcess world

    type World with // Debugging

        /// View the member properties of some SimulantState.
        static member internal getSimulantStateMemberProperties (state : SimulantState) =
            getType state
            |> (fun ty -> ty.GetProperties true)
            |> Array.map (fun (property : PropertyInfo) -> (property.Name, property.PropertyType, property.GetValue state))
            |> Array.toList

        /// View the xtension properties of some SimulantState.
        static member internal getSimulantStateXtensionProperties (state : SimulantState) =
            state.GetXtension ()
            |> Xtension.toSeq
            |> List.ofSeq
            |> List.sortBy fst
            |> List.map (fun (name, property) -> (name, property.PropertyType, property.PropertyValue))

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
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (_ : World) =
            Cascade

        /// Handle an event by swallowing.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (_ : World) =
            Resolve

        /// Handle an event by exiting the application.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) world =
            World.exit world
            Resolve