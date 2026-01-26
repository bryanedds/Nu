// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
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
    let mutable internal sortSubscriptionsByElevation : KeyValuePair<uint64, SubscriptionEntry> array -> obj -> KeyValuePair<uint64, SubscriptionEntry> array =
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

    let mutable internal setEntitiesActive : bool -> Group -> World -> unit =
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
        static member enqueueJob priority job (world : World) =
            world.WorldExtension.JobGraph.Enqueue (job, priority)

        /// Await a job from threaded execution.
        /// Order of jobs with the same key is not guaranteed.
        static member tryAwaitJob deadline (jobId : obj) (world : World) =
            world.WorldExtension.JobGraph.TryAwait (jobId, deadline)

    type World with // Destruction

        static member internal getDestructionList world =
            world.WorldExtension.DestructionList

        static member internal addSimulantToDestruction simulant world =
            world.WorldExtension.DestructionList.Add simulant

        static member internal tryRemoveSimulantFromDestruction simulant world =
            world.WorldExtension.DestructionList.RemoveAll ((=) simulant) |> ignore<int>

    type World with // LateBindingsInstances

        /// Get the facets of the world.
        static member getFacets world =
            world.WorldExtension.LateBindingsInstances.Facets

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.WorldExtension.LateBindingsInstances.EntityDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.WorldExtension.LateBindingsInstances.GroupDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.WorldExtension.LateBindingsInstances.ScreenDispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.WorldExtension.LateBindingsInstances.GameDispatchers

    type World with // AmbientState

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        /// Check that the update rate is non-zero.
        static member getAdvancing (world : World) =
            world.Advancing

        /// Check that the update rate is zero.
        static member getHalted (world : World) =
            world.Halted

        /// Set whether the world's frame rate is being explicitly paced based on clock progression.
        static member setFramePacing clockPacing world =
            AmbientState.setFramePacing clockPacing world.AmbientState

        /// Get whether the engine is running accompanied, such as in an editor.
        static member getAccompanied (world : World) =
            world.Accompanied

        /// Get whether the engine is running unaccompanied, such as outside of an editor.
        static member getUnaccompanied (world : World) =
            world.Unaccompanied

        /// Get the the aliveness state of the engine.
        static member getAlive (world : World) =
            AmbientState.getAlive world.AmbientState

        static member internal updateTime world =
            AmbientState.updateTime world.AmbientState

        /// Get the number of updates that have transpired between this and the previous frame.
        static member getUpdateDelta world =
            World.getAmbientStateBy AmbientState.getUpdateDelta world
            
        /// Get the number of updates that have transpired since the game began advancing.
        static member getUpdateTime world =
            World.getAmbientStateBy AmbientState.getUpdateTime world
            
        /// Get the amount of clock time (in seconds) between this and the previous frame. Clock time is the primary means
        /// for scaling frame-based phenomena like speeds and impulses.
        static member getClockDelta world =
            World.getAmbientStateBy AmbientState.getClockDelta world
            
        /// Get the amount of clock time (in seconds) that has transpired since the world began advancing. Clock time is
        /// the primary means for scaling frame-based phenomena like speeds and impulses.
        static member getClockTime world =
            World.getAmbientStateBy AmbientState.getClockTime world

        /// Get the tick delta as a number of environment ticks between this and the previous frame.
        static member getTickDelta world =
            AmbientState.getTickDelta world.AmbientState

        /// Get the tick time as a number of environment ticks that have transpired since the world began advancing.
        static member getTickTime world =
            AmbientState.getTickTime world.AmbientState

        /// Get the polymorphic engine time between this and the previous frame.
        static member getGameDelta world =
            World.getAmbientStateBy AmbientState.getGameDelta world

        /// Get the polymorphic engine time that has transpired since the world began advancing.
        static member getGameTime world =
            World.getAmbientStateBy AmbientState.getGameTime world

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
            world.WorldExtension.ContextImSim <- context
            world.WorldExtension.DeclaredImSim <- declared

        static member internal setContext context (world : World) =
            World.setContextAndDeclared context world.WorldExtension.ContextImSim world

        static member internal getSimulantsImSim (world : World) =
            world.SimulantsImSim

        static member internal setSimulantsImSim simulantsImSim (world : World) =
            world.WorldExtension.SimulantsImSim <- simulantsImSim

        static member internal getSimulantImSim simulant (world : World) =
            world.SimulantsImSim.[simulant]

        static member internal addSimulantImSim simulant simulantImSim (world : World) =
            world.SimulantsImSim.[simulant] <- simulantImSim

        static member internal removeSimulantImSim (simulant : Simulant) (world : World) =
            world.SimulantsImSim.Remove simulant.SimulantAddress |> ignore<bool>

        static member internal utilizeSimulantImSim _ simulantImSim (_ : World) =
            simulantImSim.SimulantUtilized <- true

        static member internal getSubscriptionsImSim (world : World) =
            world.SubscriptionsImSim

        static member internal setSubscriptionsImSim subscriptionsImSim (world : World) =
            world.WorldExtension.SubscriptionsImSim <- subscriptionsImSim

        static member internal getSubscriptionImSim subscription (world : World) =
            world.SubscriptionsImSim.[subscription]

        static member internal addSubscriptionImSim subscription subscriptionImSim (world : World) =
            world.SubscriptionsImSim.[subscription] <- subscriptionImSim

        static member internal utilizeSubscriptionImSim _ subscriptionImSim (_ : World) =
            subscriptionImSim.SubscriptionUtilized <- true

        /// Place the engine into a state such that the app will exit at the end of the current frame.
        static member exit world =
            AmbientState.exit world.AmbientState

        static member internal getCoroutines (world : World) =
            AmbientState.getCoroutines world.AmbientState

        static member internal setCoroutines coroutines world =
            AmbientState.setCoroutines coroutines world.AmbientState

        /// Launch a coroutine to be processed by the engine.
        static member launchCoroutine pred coroutine (world : World) =
            AmbientState.addCoroutine (world.GameTime, pred, coroutine) world.AmbientState

        static member internal getTasklets (world : World) =
            AmbientState.getTasklets world.AmbientState

        static member internal removeTasklets simulant world =
            AmbientState.removeTasklets simulant world.AmbientState

        static member internal clearTasklets world =
            AmbientState.clearTasklets world.AmbientState

        static member internal restoreTasklets tasklets world =
            AmbientState.restoreTasklets tasklets world.AmbientState

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet (simulant : Simulant) tasklet world =
            AmbientState.addTasklet simulant tasklet world.AmbientState

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
                        AmbientState.clearAdvancement world.AmbientState
                        World.setContextAndDeclared context declared world
                        operation world
                        World.setContextAndDeclared context' declared' world
                        AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta world.AmbientState
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
            AmbientState.trySetWindowFullScreen fullScreen world.AmbientState

        /// Attempt to toggle the window's full screen state.
        static member tryToggleWindowFullScreen world =
            AmbientState.tryToggleWindowFullScreen world.AmbientState

        /// Attempt to get the window position.
        static member tryGetWindowPosition world =
            World.getAmbientStateBy (AmbientState.tryGetWindowPosition) world

        /// Attempt to set the window position.
        static member trySetWindowPosition position world =
            World.getAmbientStateBy (AmbientState.trySetWindowPosition position) world

        /// Attempt to get the window size.
        static member tryGetWindowSize world =
            World.getAmbientStateBy (AmbientState.tryGetWindowSize) world

        /// Get the window size, using resolution as default in case there is no window.
        static member getWindowSize world =
            match World.tryGetWindowSize world with
            | Some windowsSize -> windowsSize
            | None -> world.WindowViewport.Outer.Size

        /// Attempt to set the window size.
        static member trySetWindowSize size world =
            World.getAmbientStateBy (AmbientState.trySetWindowSize size) world

        /// Get the geometry viewport.
        static member getGeometryViewport (world : World) =
            world.GeometryViewport

        /// Set the geometry viewport.
        static member setGeometryViewport viewport (world : World) =
            world.WorldExtension.GeometryViewport <- viewport

        /// Get the window viewport.
        static member getWindowViewport (world : World) =
            world.WindowViewport

        /// Set the window viewport.
        static member setWindowViewport viewport (world : World) =
            world.WorldExtension.WindowViewport <- viewport

        static member internal getSymbolics world =
            World.getAmbientStateBy AmbientState.getSymbolics world

        /// Try to load a symbol package with the given name.
        static member tryLoadSymbolPackage implicitDelimiters packageName (world : World) =
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

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            AmbientState.setOverlayer overlayer world.AmbientState

        static member internal tryGetOverlayerPropertyValue propertyName propertyType overlayName facetNames world =
            World.getOverlayerBy (Overlayer.tryGetPropertyValue propertyName propertyType overlayName facetNames) world

        /// Get overlay names.
        static member getOverlayNames world =
            (World.getOverlayerBy Overlayer.getOverlays world).Keys

        /// Attempt to get the given dispatcher's optional routed overlay name.
        static member tryGetRoutedOverlayNameOpt dispatcherName world =
            World.getOverlayerBy (Overlayer.tryGetOverlayNameOpt dispatcherName) world

        static member internal acknowledgeLightMapRenderRequest world =
            AmbientState.acknowledgeLightMapRenderRequest world.AmbientState

        /// Get whether a light map render was requested.
        static member getLightMapRenderRequested world =
            World.getAmbientStateBy AmbientState.getLightMapRenderRequested world

        /// Request a light map render for the current frame, such as when a light probe needs to be rendered.
        static member requestLightMapRender world =
            AmbientState.requestLightMapRender world.AmbientState

    type World with // Quadtree

        static member internal getQuadtree world =
            world.Quadtree

    type World with // Octree

        static member internal getOctree world =
            world.Octree

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
            World.choose { world with Subsystems = subsystems }

        static member internal mapSubsystems mapper world =
            World.setSubsystems (mapper world.Subsystems) world

        static member internal cleanUpSubsystems world =
            World.mapSubsystems (fun subsystems ->
                subsystems.CursorClient.CleanUp ()
                subsystems.AudioPlayer.CleanUp ()
                match subsystems.RendererPhysics3dOpt with Some renderer -> renderer.Dispose () | None -> ()
                subsystems.RendererProcess.Terminate ()
                subsystems.PhysicsEngine3d.CleanUp ()
                subsystems.PhysicsEngine2d.CleanUp ()
                subsystems.ImGui.CleanUp ()
                subsystems)
                world

    type World with // EventGraph

        static member internal getEventGraph world =
            world.EventGraph

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
            let eventGraph = World.getEventGraph world
            EventGraph.addEventState key state eventGraph

        static member internal removeEventState key world =
            let eventGraph = World.getEventGraph world
            EventGraph.removeEventState key eventGraph

        static member internal getSubscriptions world =
            EventGraph.getSubscriptions (World.getEventGraph world)

        static member internal getUnsubscriptions world =
            EventGraph.getUnsubscriptions (World.getEventGraph world)

        /// Get how events are being traced.
        static member getEventTracerOpt (world : World) =
            EventGraph.getEventTracerOpt world.EventGraph

        /// Set how events are being traced, if at all.
        static member setEventTracerOpt tracerOpt (world : World) =
            let eventGraph = World.getEventGraph world
            EventGraph.setEventTracerOpt tracerOpt eventGraph

        /// Get the state of the event filter.
        static member getEventFilter (world : World) =
            EventGraph.getEventFilter (World.getEventGraph world)

        /// Set the state of the event filter.
        static member setEventFilter filter (world : World) =
            EventGraph.setEventFilter filter world.EventGraph

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
                    match subscriptions.TryGetValue eventAddressObj with
                    | (true, subscriptions) -> Array.ofSeq subscriptions // make copy to avoid invalidation
                    | (false, _) -> null

            // publish to each subscription
            // OPTIMIZATION: fused PublishEventHook for speed.
            if notNull subscriptionsOpt then
                let mutable (going, enr) = (true, (seq subscriptionsOpt).GetEnumerator ())
                let mutable handling = Cascade
                while going && enr.MoveNext () do
                    let subscriptionEntry = enr.Current.Value
                    if (match handling with Cascade -> true | Resolve -> false) && world.Alive then
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
                                        | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with 1 name that is neither a Game or a GlobalSimulantGeneralized."); Cascade
                                    | 2 -> EventGraph.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | 3 -> EventGraph.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace subscriptionEntry.SubscriptionCallback world
                                    | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with no names."); Cascade
                            handling <- result
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
            match unsubscriptions.TryGetValue subscriptionId with
            | (true, struct (eventAddress, _)) ->
                match subscriptions.TryGetValue eventAddress with
                | (true, subscriptionEntries) ->
                    subscriptionEntries.Remove subscriptionId |> ignore<bool>
                    if subscriptionEntries.Count = 0 then subscriptions.Remove eventAddress |> ignore<bool>
                    unsubscriptions.Remove subscriptionId |> ignore<bool>
                    WorldTypes.handleSubscribeAndUnsubscribeEvent false eventAddress Game.Handle world
                | (false, _) -> ()
            | (false, _) -> ()

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
                match subscriptions.TryGetValue eventAddressObj with
                | (true, subscriptionEntries) ->
                    let mutable subscriptionEntry = Unchecked.defaultof<SubscriptionEntry>
                    if subscriptionEntries.TryGetValue (subscriptionId, &subscriptionEntry) then
                        let subscriptionEntry = { subscriptionEntry with SubscriptionCallback = World.boxCallback callback }
                        subscriptionEntries.[subscriptionId] <- subscriptionEntry
                    else
                        let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                        subscriptionEntries.[subscriptionId] <- subscriptionEntry
                | (false, _) ->
                    let subscriptionEntry = { SubscriptionCallback = World.boxCallback callback; SubscriptionSubscriber = subscriber }
                    let subscriptionEntries = OrderedDictionary HashIdentity.Structural
                    subscriptionEntries.[subscriptionId] <- subscriptionEntry
                    subscriptions.[eventAddressObj] <- subscriptionEntries
                unsubscriptions.[subscriptionId] <- struct (eventAddressObj, subscriber :> Simulant) 
                WorldTypes.handleSubscribeAndUnsubscribeEvent true eventAddressObj Game.Handle world
                World.unsubscribe subscriptionId
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribePlus Gen.id64 callback eventAddress subscriber world

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
                if previous.Contains facetName && not (value.Contains facetName)
                then unsubscribe world; Cascade
                else Cascade
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

        static member internal getKeyValueStore world =
            World.getAmbientStateBy AmbientState.getKeyValueStore world

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
            match keyValueStore.TryGetValue key with
            | (true, value) -> Some (value :?> 'a)
            | (false, _) -> None

        /// Look up a value from the world's key value store, throwing an exception if it is not found.
        static member getKeyedValue<'a> key world =
            let keyValueStore = World.getKeyValueStore world
            keyValueStore.[key] :?> 'a

        /// Add a value to the world's key value store.
        static member addKeyedValue<'a> key (value : 'a) world =
            let previousOpt = World.tryGetKeyedValue key world
            let valueOpt = Some (value :> obj)
            let data = { Key = key; PreviousOpt = previousOpt; ValueOpt = valueOpt }
            let keyValueStore = World.getKeyValueStore world
            keyValueStore.[key] <- value :> obj
            match previousOpt with
            | Some previous when previous =/= value -> World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | None -> World.publish data (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | Some _ -> ()

        /// Remove a value from the world's key value store.
        static member removeKeyedValue key world =
            let previousOpt = World.tryGetKeyedValue key world
            match previousOpt with
            | Some _ ->
                let keyValueStore = World.getKeyValueStore world
                if keyValueStore.Remove key then
                    World.publish { Key = key; PreviousOpt = previousOpt; ValueOpt = None } (Events.KeyedValueChangeEvent key) Nu.Game.Handle world
            | None -> ()

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
            | (false, _) -> ()

        /// Invoke a user-defined callback.
        static member invoke name args world =
            world.WorldExtension.Plugin.Invoke name args world

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle world =
            world.WorldExtension.Plugin.TryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle

        static member internal makePhysicsEngine2dRenderContext segments circles world =
            world.WorldExtension.Plugin.MakePhysicsEngine2dRenderContext segments circles world.Eye2dBounds

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

    type World with // Edit Deferrals

        /// Schedule a property replacement operation on a simulant for the appropriate ImGui phase.
        static member deferReplaceProperty op simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ReplacePropertyDeferralId simulant) with
            | (true, ops) ->
                ops.Add (ReplacePropertyDeferral op)
            | (false, _) ->
                let ops = List [ReplacePropertyDeferral op]
                deferrals.Add (ReplacePropertyDeferralId simulant, ops)

        /// Schedule an append properties operation on a simulant for the appropriate ImGui phase.
        static member deferAppendProperties op simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (AppendPropertiesDeferralId simulant) with
            | (true, ops) ->
                ops.Add (AppendPropertiesDeferral op)
            | (false, _) ->
                let ops = List [AppendPropertiesDeferral op]
                deferrals.Add (AppendPropertiesDeferralId simulant, ops)

        /// Schedule a hierarchy context operation on a simulant for the appropriate ImGui phase.
        static member deferHierarchyContext op simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (HierarchyContextDeferralId simulant) with
            | (true, ops) ->
                ops.Add (HierarchyContextDeferral op)
            | (false, _) ->
                let ops = List [HierarchyContextDeferral op]
                deferrals.Add (HierarchyContextDeferralId simulant, ops)

        /// Schedule a viewport context operation on a simulant for the appropriate ImGui phase.
        static member deferViewportContext op simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ViewportContextDeferralId simulant) with
            | (true, ops) ->
                ops.Add (ViewportContextDeferral op)
            | (false, _) ->
                let ops = List [ViewportContextDeferral op]
                deferrals.Add (ViewportContextDeferralId simulant, ops)

        /// Schedule a viewport overlay operation on a simulant for the appropriate ImGui phase.
        static member deferViewportOverlay op simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ViewportOverlayDeferralId simulant) with
            | (true, ops) ->
                ops.Add (ViewportOverlayDeferral op)
            | (false, _) ->
                let ops = List [ViewportOverlayDeferral op]
                deferrals.Add (ViewportOverlayDeferralId simulant, ops)

        static member internal runEditDeferrals operation simulant world =
            let deferrals = world.WorldExtension.EditDeferrals
            match operation with
            | ReplaceProperty replaceProperty ->
                match deferrals.TryGetValue (ReplacePropertyDeferralId simulant) with
                | (true, ops) ->
                    for op in ops do
                        match op with
                        | ReplacePropertyDeferral deferral -> deferral replaceProperty world
                        | _ -> ()
                | (false, _) -> ()
            | AppendProperties appendProperties ->
                match deferrals.TryGetValue (AppendPropertiesDeferralId simulant) with
                | (true, ops) ->
                    for op in ops do
                        match op with
                        | AppendPropertiesDeferral deferral -> deferral appendProperties world
                        | _ -> ()
                | (false, _) -> ()
            | HierarchyContext hierarchyContext ->
                match deferrals.TryGetValue (HierarchyContextDeferralId simulant) with
                | (true, ops) ->
                    for op in ops do
                        match op with
                        | HierarchyContextDeferral deferral -> deferral hierarchyContext world
                        | _ -> ()
                | (false, _) -> ()
            | ViewportContext viewportContext ->
                match deferrals.TryGetValue (ViewportContextDeferralId simulant) with
                | (true, ops) ->
                    for op in ops do
                        match op with
                        | ViewportContextDeferral deferral -> deferral viewportContext world
                        | _ -> ()
                | (false, _) -> ()
            | ViewportOverlay viewportOverlay ->
                match deferrals.TryGetValue (ViewportOverlayDeferralId simulant) with
                | (true, ops) ->
                    for op in ops do
                        match op with
                        | ViewportOverlayDeferral deferral -> deferral viewportOverlay world
                        | _ -> ()
                | (false, _) -> ()

        static member internal clearEditDeferrals (world : World) =
            world.WorldExtension.EditDeferrals.Clear ()

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
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            World.exit world
            Resolve