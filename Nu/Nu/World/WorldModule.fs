// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
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

/// Internal function definitions for the world (1/2).
[<RequireQualifiedAccess>]
module internal WorldModuleInternal =

    /// Helps to track when content bound to event handlers and ImSim subscriptions need to be updated due to
    /// LateBindings changing, such as via code reloading.
    let mutable internal UpdateLateBindingsCount = 0

    /// Track if we're in the portion of the frame simulants are being updated.
    /// TODO: P1: make this an AmbientState flag.
    let mutable internal UpdatingSimulants = false

    /// Track if we're in the portion of the frame when end-frame processing has started.
    /// TODO: P1: make this an AmbientState flag.
    let mutable internal EndFrameProcessingStarted = false

    /// F# reach function for checking that a simulant is selected.
    let mutable internal getSelected : Simulant -> World -> bool =
        Unchecked.defaultof<_>

    /// The callbacks of named subscriptions, found by owner type, handler name, and event type.
    let internal NamedCallbacks = Dictionary<struct (Type * string * Type), obj> HashIdentity.Structural

    /// The callback of a named subscription whose owner or handler could not be found.
    let internal CascadeCallback = box (fun (_ : Event<obj, Simulant>) (_ : World) -> Cascade)

    /// F# reach function for sorting subscriptions by elevation.
    let mutable internal sortSubscriptionsByElevation : (uint64 * SubscriptionEntry) seq -> obj -> (uint64 * SubscriptionEntry) seq =
        Unchecked.defaultof<_>

    /// F# reach function for registering physics entities of an entire screen.
    let mutable internal evictScreenElements : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach function for unregistering physics entities of an entire screen.
    let mutable internal admitScreenElements : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach function for registering physics entities of an entire screen.
    let mutable internal registerScreenPhysics : Screen -> World -> unit =
        Unchecked.defaultof<_>

    /// F# reach function for unregistering physics entities of an entire screen.
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

/// Universal function definitions for the world (1/4).
[<AutoOpen>]
module WorldModule =

    type World with // JobGraph

        /// Enqueue a job for threaded execution.
        static member enqueueJob priority job (world : World) =
            world.WorldExtension.JobGraph.Enqueue (job, priority)

        /// Await a job from threaded execution.
        /// Order of jobs with the same key is not guaranteed.
        static member tryAwaitJob deadline (jobId : obj) (world : World) =
            world.WorldExtension.JobGraph.TryAwait (jobId, deadline)

    type World with // Destruction

        static member internal getDestructionListRev (world : World) =
            world.WorldExtension.DestructionListRev

        static member internal addSimulantToDestruction simulant (world : World) =
            let worldExtension = { world.WorldExtension with DestructionListRev = simulant :: world.WorldExtension.DestructionListRev }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal tryRemoveSimulantFromDestruction simulant (world : World) =
            let worldExtension = { world.WorldExtension with DestructionListRev = List.remove ((=) simulant) world.WorldExtension.DestructionListRev }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

    type World with // LateBindingsInstances

        /// Get the facets of the world.
        static member getFacets (world : World) =
            world.WorldExtension.LateBindingsInstances.Facets

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers (world : World) =
            world.WorldExtension.LateBindingsInstances.EntityDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers (world : World) =
            world.WorldExtension.LateBindingsInstances.GroupDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers (world : World) =
            world.WorldExtension.LateBindingsInstances.ScreenDispatchers

        /// Get the game dispatchers of the world.
        static member getGameDispatchers (world : World) =
            world.WorldExtension.LateBindingsInstances.GameDispatchers

    type World with // AmbientState

        static member internal getAmbientState (world : World) =
            world.AmbientState

        static member internal mapAmbientState mapper (world : World) =
            world.WorldState <- { world.WorldState with AmbientState = mapper world.AmbientState }

        static member internal clearTimeAdvancement (world : World) =
            let ambientState = AmbientState.clearTimeAdvancement world.AmbientState
            world.WorldState <- { world.WorldState with AmbientState = ambientState }

        static member internal restoreTimeAdvancement timeAdvancing updateDelta clockDelta tickDelta (world : World) =
            let ambientState = AmbientState.restoreTimeAdvancement timeAdvancing updateDelta clockDelta tickDelta  world.AmbientState
            world.WorldState <- { world.WorldState with AmbientState = ambientState }
            
        /// Check that world time is advancing (not halted).
        static member getTimeAdvancing (world : World) =
            world.TimeAdvancing

        /// Check that world time is halted (not advancing).
        static member getTimeHalted (world : World) =
            world.TimeHalted

        /// Get the time scalar - how fast scaled game time passes relative to unscaled game time, 0 holding it still.
        static member getTimeScalar world =
            AmbientState.getTimeScalar world.AmbientState

        /// Set the time scalar - how fast scaled game time passes relative to unscaled game time, 0 holding it still.
        /// Negative values are treated as 0. Takes effect as of the next frame.
        static member setTimeScalar scalar world =
            World.mapAmbientState (AmbientState.setTimeScalar scalar) world

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
            AmbientState.getCollectionConfig world.AmbientState

        /// Get the the aliveness state of the engine.
        static member getAlive (world : World) =
            AmbientState.getAlive world.AmbientState

        static member internal processTime world =
            World.mapAmbientState AmbientState.processTime world

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

        /// Get the polymorphic engine time between this and the previous frame, unscaled by the time scalar.
        static member getUnscaledDelta world =
            AmbientState.getUnscaledDelta world.AmbientState

        /// Get the polymorphic engine time that has transpired since the world began advancing, unscaled by the time
        /// scalar.
        static member getUnscaledTime world =
            AmbientState.getUnscaledTime world.AmbientState

        /// Get the polymorphic engine time between this and the previous frame, scaled by the time scalar.
        static member getScaledDelta world =
            AmbientState.getScaledDelta world.AmbientState

        /// Get the polymorphic engine time that has transpired since the world began advancing, scaled by the time
        /// scalar as it went.
        static member getScaledTime world =
            AmbientState.getScaledTime world.AmbientState

        /// Get the amount of date time that has transpired between this and the previous frame. This value is independent
        /// of whether the world was or is advancing.
        static member getDateDelta world =
            AmbientState.getDateDelta world.AmbientState

        /// Get the date time as of the start of this frame. This value is independent of whether the world was or is
        /// advancing.
        static member getDateTime world =
            AmbientState.getDateTime world.AmbientState

        /// Get the timers.
        static member getTimers world =
            AmbientState.getTimers world.AmbientState

        /// Get the current edit context, if any.
        static member getEditContextOpt (world : World) =
            world.EditContextOpt

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

        static member internal getSimulantJournals (world : World) =
            world.SimulantJournals

        static member internal setSimulantJournals simulantJournals (world : World) =
            if world.Imperative then
                world.WorldExtension.SimulantJournals <- simulantJournals
            else
                let worldExtension = { world.WorldExtension with SimulantJournals = simulantJournals }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal getSimulantJournal simulant (world : World) =
            world.SimulantJournals[simulant]

        static member internal addSimulantJournal simulant simulantJournal (world : World) =
            let simulantJournals = SUMap.add simulant simulantJournal world.SimulantJournals
            World.setSimulantJournals simulantJournals world

        static member internal removeSimulantJournal (simulant : Simulant) (world : World) =
            World.setSimulantJournals (SUMap.remove simulant.SimulantAddress world.SimulantJournals) world

        static member internal tryMapSimulantJournal mapper simulant (world : World) =
            match world.SimulantJournals.TryGetValue simulant with
            | (true, simulantJournal) ->
                let simulantJournal = mapper simulantJournal
                World.addSimulantJournal simulant simulantJournal world
            | (false, _) -> ()

        static member internal mapSimulantJournal mapper simulant world =
            let simulantJournal = World.getSimulantJournal simulant world
            let simulantJournal = mapper simulantJournal
            World.addSimulantJournal simulant simulantJournal world

        static member internal utilizeSimulantInJournal simulant simulantJournal (world : World) =
            if world.Imperative then
                simulantJournal.SimulantUtilized <- true
            else
                let simulantJournal = { simulantJournal with SimulantUtilized = true }
                let simulantJournals = SUMap.add simulant simulantJournal world.SimulantJournals
                World.setSimulantJournals simulantJournals world

        static member internal getSubscriptionJournals (world : World) =
            world.SubscriptionJournals

        static member internal setSubscriptionJournals subscriptionJournals (world : World) =
            if world.Imperative then
                world.WorldExtension.SubscriptionJournals <- subscriptionJournals
            else
                let worldExtension = { world.WorldExtension with SubscriptionJournals = subscriptionJournals }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal getSubscriptionJournal subscription (world : World) =
            world.SubscriptionJournals[subscription]

        static member internal addSubscriptionJournal subscription subscriptionJournal (world : World) =
            let subscriptionJournals = SUMap.add subscription subscriptionJournal world.SubscriptionJournals
            World.setSubscriptionJournals subscriptionJournals world

        static member internal tryMapSubscriptionJournal mapper subscription (world : World) =
            match world.SubscriptionJournals.TryGetValue subscription with
            | (true, subscriptionJournal) ->
                let subscriptionJournal = mapper subscriptionJournal
                World.addSubscriptionJournal subscription subscriptionJournal world
            | (false, _) -> ()

        static member internal mapSubscriptionJournal mapper subscription world =
            let subscriptionJournal = World.getSubscriptionJournal subscription world
            let subscriptionJournal = mapper subscriptionJournal
            World.addSubscriptionJournal subscription subscriptionJournal world

        static member internal utilizeSubscriptionInJournal subscription subscriptionJournal (world : World) =
            if world.Imperative then
                subscriptionJournal.SubscriptionUtilized <- true
            else
                let subscriptionJournal = { subscriptionJournal with SubscriptionUtilized = true }
                let subscriptionJournals = SUMap.add subscription subscriptionJournal world.SubscriptionJournals
                World.setSubscriptionJournals subscriptionJournals world

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
        /// NOTE: the coroutine is kept as a closure until it completes, so if a code reload happens before then, it
        /// runs the code it was created from rather than the reloaded code.
        static member launchCoroutine timeScaled pred coroutine (world : World) =
            let gameTime = if timeScaled then world.ScaledTime else world.UnscaledTime
            World.mapAmbientState (AmbientState.addCoroutine (gameTime, timeScaled, pred, coroutine)) world

        static member internal getTasklets (world : World) =
            AmbientState.getTasklets world.AmbientState

        static member internal removeTasklets simulant world =
            World.mapAmbientState (AmbientState.removeTasklets simulant) world

        static member internal clearTasklets world =
            World.mapAmbientState AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.mapAmbientState (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        /// NOTE: the tasklet's operation is kept as a closure until it runs, so if a code reload happens before then,
        /// it runs the code it was created from rather than the reloaded code.
        static member addTasklet (simulant : Simulant) tasklet world =
            World.mapAmbientState (AmbientState.addTasklet simulant tasklet) world

        /// Schedule an operation to be executed by the engine with the given delay.
        /// When called in an ImSim Process context, will provide the ImSim simulant context and declared values from
        /// World that were active in that Process context as well as time and advancement state.
        /// NOTE: the operation is kept as a closure until it runs, so if a code reload happens before then, it runs the
        /// code it was created from rather than the reloaded code.
        static member schedule timeScaled delay operation (simulant : Simulant) (world : World) =

            // compute time at which to schedule the operation
            let gameTime = (if timeScaled then world.ScaledTime else world.UnscaledTime) + delay

            // restore the ImSim context when we're in an ImSim Process context
            let operation =
                let context = world.ContextImSim
                if context.Names.Length > 0 then
                    let declared = world.DeclaredImSim
                    let timeAdvancing = world.TimeAdvancing
                    let updateDelta = world.UpdateDelta
                    let clockDelta = world.ClockDelta
                    let tickDelta = world.TickDelta
                    fun (world : World) ->
                        let context' = world.ContextImSim
                        let declared' = world.DeclaredImSim
                        if timeAdvancing then World.clearTimeAdvancement world
                        World.setContextAndDeclared context declared world
                        operation world
                        World.setContextAndDeclared context' declared' world
                        if timeAdvancing then World.restoreTimeAdvancement timeAdvancing updateDelta clockDelta tickDelta world
                else operation

            // add tasklet
            let tasklet = { ScheduledGameTime = gameTime; ScheduledTimeScaled = timeScaled; ScheduledOp = operation }
            World.addTasklet simulant tasklet world

        /// Schedule an operation to be executed by the engine at the end of the current frame or the next frame if
        /// we've already started processing tasklets, timed by the unscaled game time so it runs however time is
        /// scaled.
        /// When called in an ImSim Process context, will provide the ImSim simulant context and declared values from
        /// World that were active in that Process context as well as time and advancement state.
        /// NOTE: the operation is kept as a closure until it runs, so if a code reload happens before then, it runs the
        /// code it was created from rather than the reloaded code.
        static member defer operation (simulant : Simulant) (world : World) =
            let gameTime = if WorldModuleInternal.EndFrameProcessingStarted && world.TimeAdvancing then GameTime.epsilon else GameTime.zero
            World.schedule false gameTime operation simulant world

        /// Attempt to get the window flags.
        static member tryGetWindowFlags (world : World) =
            AmbientState.tryGetWindowFlags world.AmbientState

        /// Attempt to get the window pixel density.
        static member tryGetWindowPixelDensity (world : World) =
            AmbientState.tryGetWindowPixelDensity world.AmbientState

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

        /// Attempt to set the window size.
        static member trySetWindowSize size (world : World) =
            AmbientState.trySetWindowSize size world.AmbientState

        /// Attempt to get the window properties.
        static member tryGetWindowProperties (world : World) =
            AmbientState.tryGetWindowProperties world.AmbientState

        /// Get the window size, using viewport outer size when there is no window.
        static member getWindowSizeOtherwiseViewportSize world =
            match World.tryGetWindowSize world with
            | Some windowsSize -> windowsSize
            | None -> world.WindowViewport.Outer.Size

        /// Get the geometry viewport.
        static member getGeometryViewport (world : World) =
            world.GeometryViewport

        /// Set the geometry viewport.
        static member setGeometryViewport viewport (world : World) =
            let worldExtension = { world.WorldExtension with GeometryViewport = viewport }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Get the window viewport.
        static member getWindowViewport (world : World) =
            world.WindowViewport

        /// Set the window viewport.
        static member setWindowViewport viewport (world : World) =
            let worldExtension = { world.WorldExtension with WindowViewport = viewport }
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

        /// A coroutine launcher with its time scaled.
        member this.LauncherScaled =
            flip (World.launchCoroutine true tautology) this

        /// A cancellable coroutine launcher with its time scaled.
        member this.LauncherScaledWhile pred =
            flip (World.launchCoroutine true pred) this

        /// A coroutine launcher with its time unscaled.
        member this.LauncherUnscaled =
            flip (World.launchCoroutine false tautology) this

        /// A cancellable coroutine launcher with its time unscaled.
        member this.LauncherUnscaledWhile pred =
            flip (World.launchCoroutine false pred) this

    type World with // Subsystems

        static member internal getSubsystems world =
            world.Subsystems

        static member internal setSubsystems subsystems world =
            world.WorldState <- { world.WorldState with Subsystems = subsystems }

        static member internal mapSubsystems mapper (world : World) =
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

        static member private tryGetNamedHandlerMethod (ownerType : Type) (handlerName : string) (parameterTypes : Type array) : MethodInfo =
            match ownerType with
            | null -> null
            | _ ->
                match ownerType.GetMethod (handlerName, BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly, parameterTypes) with
                | null -> World.tryGetNamedHandlerMethod ownerType.BaseType handlerName parameterTypes
                | methodInfo -> methodInfo

        static member private getNamedHandlerMethods (ownerType : Type) (handlerName : string) : MethodInfo list =
            match ownerType with
            | null -> []
            | _ ->
                let methods =
                    ownerType.GetMethods (BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.DeclaredOnly)
                    |> Array.filter (fun methodInfo -> methodInfo.Name = handlerName)
                    |> List.ofArray
                methods @ World.getNamedHandlerMethods ownerType.BaseType handlerName

        static member private describeMissingNamedHandler<'a, 's when 's :> Simulant> (ownerType : Type) (handlerName : string) =
            let signature = "static " + handlerName + " : Event<" + typeof<'a>.Name + ", " + typeof<'s>.Name + "> -> World -> Handling"
            match World.getNamedHandlerMethods ownerType handlerName with
            | [] ->
                "Could not find named handler '" + handlerName + "' on type '" + ownerType.Name + "' or its base types. " +
                "Expected a method of the form '" + signature + "'."
            | methods when methods |> List.exists (fun methodInfo -> methodInfo.IsGenericMethodDefinition) ->
                "Named handler '" + handlerName + "' on type '" + ownerType.Name + "' is generic, so it cannot be found by name. " +
                "Give it concrete parameter types of the form '" + signature + "'."
            | _ ->
                "Named handler '" + handlerName + "' on type '" + ownerType.Name + "' does not have the expected signature '" + signature + "'."

        static member internal bindNamedCallback<'a, 's when 's :> Simulant> (handlerName : string) (ownerType : Type) : obj =
            let key = struct (ownerType, handlerName, typeof<Event<'a, 's>>)
            match WorldModuleInternal.NamedCallbacks.TryGetValue key with
            | (true, callback) -> callback
            | (false, _) ->
                let callback =
                    match World.tryGetNamedHandlerMethod ownerType handlerName [|typeof<Event<'a, 's>>; typeof<World>|] with
                    | null ->
                        Log.errorOnce (World.describeMissingNamedHandler<'a, 's> ownerType handlerName)
                        WorldModuleInternal.CascadeCallback
                    | methodInfo when methodInfo.ReturnType = typeof<Handling> ->
                        let handler = methodInfo.CreateDelegate<Func<Event<'a, 's>, World, Handling>> ()
                        let boxableCallback = fun (evt : Event<obj, Simulant>) (world : World) ->
                            let evt = { Data = evt.Data :?> 'a; Subscriber = evt.Subscriber :?> 's; Publisher = evt.Publisher; Address = Address.specialize<'a> evt.Address; Trace = evt.Trace }
                            handler.Invoke (evt, world)
                        box boxableCallback
                    | _ ->
                        Log.errorOnce (World.describeMissingNamedHandler<'a, 's> ownerType handlerName)
                        WorldModuleInternal.CascadeCallback
                WorldModuleInternal.NamedCallbacks[key] <- callback
                callback

        static member private tryGetFacetOrDispatcherType (ownerType : Type) (facetName : string) (entityState : EntityState) : Type =
            let dispatcherType = entityState.Dispatcher.GetType ()
            if obj.ReferenceEquals (dispatcherType, ownerType) then dispatcherType
            else
                let facets = entityState.Facets
                let mutable result = null
                let mutable i = 0
                while isNull result && i < facets.Length do
                    let facetType = facets[i].GetType ()
                    if obj.ReferenceEquals (facetType, ownerType) then result <- facetType
                    i <- inc i
                i <- 0
                while isNull result && i < facets.Length do
                    let facetType = facets[i].GetType ()
                    if facetType.Name = facetName then result <- facetType
                    i <- inc i
                if isNull result && dispatcherType.Name = facetName then result <- dispatcherType
                result

        static member private tryGetNamedCallbackOwnerType (named : NamedCallbackData) (subscriber : Simulant) (world : World) : Type =
            let namesLength = subscriber.SimulantAddress.Names.Length
            if namesLength >= 4 then
                let entity = subscriber :?> Entity
                let mutable entityState = entity.EntityStateOpt
                if isNull (entityState :> obj) || entityState.Invalidated then
                    entityState <- Unchecked.defaultof<EntityState>
                    SUMap.tryGetValue (entity, world.EntityStates, &entityState) |> ignore<bool>
                if notNull (entityState :> obj) then
                    match named.FacetNameOpt with
                    | ValueNone -> entityState.Dispatcher.GetType ()
                    | ValueSome facetName -> World.tryGetFacetOrDispatcherType named.OwnerType facetName entityState
                else null
            else
                match namesLength with
                | 1 ->
                    match subscriber with
                    | :? Game -> world.GameState.Dispatcher.GetType ()
                    | _ -> null
                | 2 ->
                    let mutable screenState = Unchecked.defaultof<ScreenState>
                    if UMap.tryGetValue (subscriber :?> Screen, world.ScreenStates, &screenState)
                    then screenState.Dispatcher.GetType ()
                    else null
                | 3 ->
                    let mutable groupState = Unchecked.defaultof<GroupState>
                    if UMap.tryGetValue (subscriber :?> Group, world.GroupStates, &groupState)
                    then groupState.Dispatcher.GetType ()
                    else null
                | _ -> null

        static member internal getNamedCallback (named : NamedCallbackData) (subscriber : Simulant) (world : World) : obj =
            let ownerType = World.tryGetNamedCallbackOwnerType named subscriber world
            if notNull ownerType then
                if not (obj.ReferenceEquals (ownerType, named.OwnerType)) then
                    named.Handler <- named.Bind ownerType
                    named.OwnerType <- ownerType
                named.Handler
            else WorldModuleInternal.CascadeCallback

        static member internal makeNamedCallback<'a, 's when 's :> Simulant> (handlerName : string) (facetNameOpt : string voption) (subscriber : 's) world =
            let named = { HandlerName = handlerName; FacetNameOpt = facetNameOpt; Bind = World.bindNamedCallback<'a, 's> handlerName; OwnerType = null; Handler = null }
            World.getNamedCallback named subscriber world |> ignore<obj> // NOTE: binding eagerly so that a missing handler is reported upon subscription.
            NamedCallback named

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
                        WorldModuleInternal.sortSubscriptionsByElevation eventAddressObj world.EventGraph world
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
                        if not selectedOnly || WorldModuleInternal.getSelected subscriber world then
                            let callback =
                                match subscriptionEntry.SubscriptionCallback with
                                | LambdaCallback lambda -> lambda
                                | NamedCallback named -> World.getNamedCallback named subscriber world
                            let namesLength = subscriber.SimulantAddress.Names.Length
                            if namesLength >= 4 then
                                // OPTIMIZATION: handling common case explicitly first.
                                handling <- EventGraph.publishEvent<'a, 'p, Entity, World> subscriber publisher eventData eventAddress eventTrace callback world
                            else
                                match namesLength with
                                | 1 ->
                                    match subscriber with
                                    | :? Game -> handling <- EventGraph.publishEvent<'a, 'p, Game, World> subscriber publisher eventData eventAddress eventTrace callback world
                                    | :? GlobalSimulantGeneralized -> handling <- EventGraph.publishEvent<'a, 'p, Simulant, World> subscriber publisher eventData eventAddress eventTrace callback world
                                    | _ -> Log.errorOnce ("Event publish operation failed. Cannot publish event '" + scstring eventAddress + "' to a subscriber with 1 name that is neither a Game or a GlobalSimulantGeneralized.")
                                | 2 -> handling <- EventGraph.publishEvent<'a, 'p, Screen, World> subscriber publisher eventData eventAddress eventTrace callback world
                                | 3 -> handling <- EventGraph.publishEvent<'a, 'p, Group, World> subscriber publisher eventData eventAddress eventTrace callback world
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

        static member internal subscribeCallback<'a, 's when 's :> Simulant>
            (subscriptionId : uint64)
            (callback : SubscriptionCallback)
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
                            let subscriptionEntry = { subscriptionEntry with SubscriptionCallback = callback }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add eventAddressObj subscriptionEntries subscriptions
                        | None ->
                            let subscriptionEntry = { SubscriptionCallback = callback; SubscriptionSubscriber = subscriber }
                            let subscriptionEntries = OMap.add subscriptionId subscriptionEntry subscriptionEntries
                            UMap.add eventAddressObj subscriptionEntries subscriptions
                    | None ->
                        let subscriptionEntry = { SubscriptionCallback = callback; SubscriptionSubscriber = subscriber }
                        UMap.add eventAddressObj (OMap.singleton HashIdentity.Structural (World.getCollectionConfig world) subscriptionId subscriptionEntry) subscriptions
                let unsubscriptions = UMap.add subscriptionId struct (eventAddressObj, subscriber :> Simulant) unsubscriptions
                World.setSubscriptions subscriptions world
                World.setUnsubscriptions unsubscriptions world
                WorldTypes.handleSubscribeAndUnsubscribeEvent true eventAddressObj Game.Handle world
                World.unsubscribe subscriptionId
            else failwith "Event name cannot be empty."

        /// Subscribe to an event using the given subscriptionId and be provided with an unsubscription callback.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.subscribePlus, which finds
        /// its handler by name when the event is handled.
        static member subscribeLambdaPlus<'a, 's when 's :> Simulant>
            (subscriptionId : uint64)
            (callback : Event<'a, 's> -> World -> Handling)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            World.subscribeCallback<'a, 's> subscriptionId (LambdaCallback (World.boxCallback callback)) eventAddress subscriber world

        /// Subscribe to an event.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.subscribe, which finds its
        /// handler by name when the event is handled.
        static member subscribeLambda<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribeLambdaPlus Gen.id64 callback eventAddress subscriber world |> ignore

        static member internal monitorCallback<'a, 's when 's :> Simulant>
            (callback : SubscriptionCallback)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            let removalId = Gen.id64
            let monitorId = Gen.id64
            World.subscribeCallback<'a, 's> monitorId callback eventAddress subscriber world |> ignore
            let unsubscribe = fun (world : World) ->
                World.unsubscribe removalId world
                World.unsubscribe monitorId world
            let callback' = fun _ world -> unsubscribe world; Cascade
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> itoa subscriber.SimulantAddress
            World.subscribeLambdaPlus<unit, Simulant> removalId callback' unregisteringEventAddress subscriber world |> ignore
            unsubscribe

        /// Keep active a subscription for the life span of a simulant.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.monitorPlus, which finds
        /// its handler by name when the event is handled.
        static member monitorLambdaPlus<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            World.monitorCallback<'a, 's> (LambdaCallback (World.boxCallback callback)) eventAddress subscriber world

        /// Keep active a subscription for the life span of a simulant.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.monitor, which finds its
        /// handler by name when the event is handled.
        static member monitorLambda<'a, 's when 's :> Simulant>
            (callback : Event<'a, 's> -> World -> Handling) (eventAddress : 'a Address) (subscriber : 's) (world : World) =
            World.monitorLambdaPlus<'a, 's> callback eventAddress subscriber world |> ignore

        static member internal senseCallback<'a>
            (callback : SubscriptionCallback)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            let removalId = Gen.id64
            let fastenId = Gen.id64
            let senseId = Gen.id64
            World.subscribeCallback<'a, Entity> senseId callback eventAddress entity world |> ignore
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
            World.subscribeLambdaPlus<unit, Simulant> removalId callback' unregisteringEventAddress entity world |> ignore
            World.subscribeLambdaPlus<ChangeData, Simulant> fastenId callback'' changeFacetNamesEventAddress entity world |> ignore
            unsubscribe

        /// Keep active a subscription for the life span of an entity and a given facet.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.sensePlus, which finds its
        /// handler by name when the event is handled.
        static member senseLambdaPlus<'a>
            (callback : Event<'a, Entity> -> World -> Handling)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            World.senseCallback<'a> (LambdaCallback (World.boxCallback callback)) eventAddress entity facetName world

        /// Keep active a subscription for the life span of an entity and a given facet.
        /// NOTE: the callback is kept as a closure for the life of the subscription, so after a code reload it keeps
        /// running the code it was created from rather than the reloaded code. Prefer World.sense, which finds its
        /// handler by name when the event is handled.
        static member senseLambda<'a>
            (callback : Event<'a, Entity> -> World -> Handling) (eventAddress : 'a Address) (subscriber : Entity) (facetName : string) (world : World) =
            World.senseLambdaPlus callback eventAddress subscriber facetName world |> ignore

        /// Subscribe to an event using the given subscriptionId and be provided with an unsubscription callback. The
        /// handler is the static method of the given name on the subscriber's dispatcher (or one of its base types),
        /// of the form Event<'a, 's> -> World -> Handling, found when the event is handled so that it follows the
        /// subscriber's dispatcher across code reloads.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscriptionId : uint64)
            (handlerName : string)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            let callback = World.makeNamedCallback<'a, 's> handlerName ValueNone subscriber world
            World.subscribeCallback<'a, 's> subscriptionId callback eventAddress subscriber world

        /// Unsubscribe the given subscriber's subscriptions to the given event address whose handler is found by the
        /// given name.
        static member unsubscribe4<'a> (handlerName : string) (eventAddress : 'a Address) (subscriber : Simulant) (world : World) =
            match UMap.tryFind (atooa eventAddress) (World.getSubscriptions world) with
            | Some subscriptionEntries ->
                let subscriptionIds =
                    [for (subscriptionId, subscriptionEntry) in OMap.toSeq subscriptionEntries do
                        match subscriptionEntry.SubscriptionCallback with
                        | NamedCallback named when named.HandlerName = handlerName && subscriptionEntry.SubscriptionSubscriber = subscriber -> subscriptionId
                        | _ -> ()]
                for subscriptionId in subscriptionIds do
                    World.unsubscribe subscriptionId world
            | None -> ()

        static member internal subscribeFacetPlus<'a>
            (subscriptionId : uint64)
            (handlerName : string)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            let callback = World.makeNamedCallback<'a, Entity> handlerName (ValueSome facetName) entity world
            World.subscribeCallback<'a, Entity> subscriptionId callback eventAddress entity world

        /// Subscribe to an event. The handler is the static method of the given name on the subscriber's dispatcher
        /// (or one of its base types), of the form Event<'a, 's> -> World -> Handling, found when the event is handled
        /// so that it follows the subscriber's dispatcher across code reloads.
        static member subscribe<'a, 's when 's :> Simulant>
            (handlerName : string) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribePlus<'a, 's> Gen.id64 handlerName eventAddress subscriber world |> ignore

        /// Keep active a subscription for the life span of a simulant. The handler is the static method of the given
        /// name on the subscriber's dispatcher (or one of its base types), of the form Event<'a, 's> -> World ->
        /// Handling, found when the event is handled so that it follows the subscriber's dispatcher across code
        /// reloads.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (handlerName : string)
            (eventAddress : 'a Address)
            (subscriber : 's)
            (world : World) =
            let callback = World.makeNamedCallback<'a, 's> handlerName ValueNone subscriber world
            World.monitorCallback<'a, 's> callback eventAddress subscriber world

        /// Keep active a subscription for the life span of a simulant. The handler is the static method of the given
        /// name on the subscriber's dispatcher (or one of its base types), of the form Event<'a, 's> -> World ->
        /// Handling, found when the event is handled so that it follows the subscriber's dispatcher across code
        /// reloads.
        static member monitor<'a, 's when 's :> Simulant>
            (handlerName : string) (eventAddress : 'a Address) (subscriber : 's) (world : World) =
            World.monitorPlus<'a, 's> handlerName eventAddress subscriber world |> ignore

        /// Keep active a subscription for the life span of an entity and a given facet. The handler is the static
        /// method of the given name on the named facet (or on the entity's dispatcher when its type has that name), or
        /// one of its base types, of the form Event<'a, Entity> -> World -> Handling, found when the event is handled
        /// so that it follows the facet across code reloads.
        static member sensePlus<'a>
            (handlerName : string)
            (eventAddress : 'a Address)
            (entity : Entity)
            (facetName : string)
            (world : World) =
            let callback = World.makeNamedCallback<'a, Entity> handlerName (ValueSome facetName) entity world
            World.senseCallback<'a> callback eventAddress entity facetName world

        /// Keep active a subscription for the life span of an entity and a given facet. The handler is the static
        /// method of the given name on the named facet (or on the entity's dispatcher when its type has that name), or
        /// one of its base types, of the form Event<'a, Entity> -> World -> Handling, found when the event is handled
        /// so that it follows the facet across code reloads.
        static member sense<'a>
            (handlerName : string) (eventAddress : 'a Address) (subscriber : Entity) (facetName : string) (world : World) =
            World.sensePlus<'a> handlerName eventAddress subscriber facetName world |> ignore

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

        /// Get the user-defined 2D pipeline descriptors.
        static member getUserDefinedPipeline2dDescriptors (world : World) =
            world.WorldExtension.Plugin.UserDefinedPipeline2dDescriptors

        /// Get the user-defined 3D pipeline descriptors.
        static member getUserDefinedPipeline3dDescriptors (world : World) =
            world.WorldExtension.Plugin.UserDefinedPipeline3dDescriptors

        /// Attempt to make an emitter with the given parameters.
        static member tryMakeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax emitterStyle (world : World) =
            match world.WorldExtension.Plugin.MakeEmitters.TryGetValue emitterStyle with
            | (true, makeEmitter) -> Some (makeEmitter time lifeTimeOpt particleLifeTimeMaxOpt particleRate particleMax)
            | (false, _)-> None

        static member internal makePhysicsEngine2dRenderContext segments circles (world : World) =
            world.WorldExtension.Plugin.MakePhysicsEngine2dRenderContext segments circles world.Eye2dBounds

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

    type World with // EditDeferrals

        /// Schedule a property replacement operation on a simulant for the appropriate ImGui phase.
        static member deferReplaceProperty op simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ReplacePropertyDeferralId simulant) with
            | (true, ops) ->
                let op = UList.add (ReplacePropertyDeferral op) ops
                let ops = UMap.add (ReplacePropertyDeferralId simulant) op deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }
            | (false, _) ->
                let ops = UList.singleton (World.getCollectionConfig world) (ReplacePropertyDeferral op)
                let ops = UMap.add (ReplacePropertyDeferralId simulant) ops deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Schedule an append properties operation on a simulant for the appropriate ImGui phase.
        static member deferAppendProperties op simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (AppendPropertiesDeferralId simulant) with
            | (true, ops) ->
                let op = UList.add (AppendPropertiesDeferral op) ops
                let ops = UMap.add (AppendPropertiesDeferralId simulant) op deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }
            | (false, _) ->
                let ops = UList.singleton (World.getCollectionConfig world) (AppendPropertiesDeferral op)
                let ops = UMap.add (AppendPropertiesDeferralId simulant) ops deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Schedule a hierarchy context operation on a simulant for the appropriate ImGui phase.
        static member deferHierarchyContext op simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (HierarchyContextDeferralId simulant) with
            | (true, ops) ->
                let op = UList.add (HierarchyContextDeferral op) ops
                let ops = UMap.add (HierarchyContextDeferralId simulant) op deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }
            | (false, _) ->
                let ops = UList.singleton (World.getCollectionConfig world) (HierarchyContextDeferral op)
                let ops = UMap.add (HierarchyContextDeferralId simulant) ops deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Schedule a viewport context operation on a simulant for the appropriate ImGui phase.
        static member deferViewportContext op simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ViewportContextDeferralId simulant) with
            | (true, ops) ->
                let op = UList.add (ViewportContextDeferral op) ops
                let ops = UMap.add (ViewportContextDeferralId simulant) op deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }
            | (false, _) ->
                let ops = UList.singleton (World.getCollectionConfig world) (ViewportContextDeferral op)
                let ops = UMap.add (ViewportContextDeferralId simulant) ops deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        /// Schedule a viewport overlay operation on a simulant for the appropriate ImGui phase.
        static member deferViewportOverlay op simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
            match deferrals.TryGetValue (ViewportOverlayDeferralId simulant) with
            | (true, ops) ->
                let op = UList.add (ViewportOverlayDeferral op) ops
                let ops = UMap.add (ViewportOverlayDeferralId simulant) op deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }
            | (false, _) ->
                let ops = UList.singleton (World.getCollectionConfig world) (ViewportOverlayDeferral op)
                let ops = UMap.add (ViewportOverlayDeferralId simulant) ops deferrals
                let worldExtension = { world.WorldExtension with EditDeferrals = ops }
                world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

        static member internal runEditDeferrals operation simulant world =
            let deferrals = world.WorldState.WorldExtension.EditDeferrals
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
            let deferrals = UMap.clear world.WorldState.WorldExtension.EditDeferrals
            let worldExtension = { world.WorldExtension with EditDeferrals = deferrals }
            world.WorldState <- { world.WorldState with WorldExtension = worldExtension }

    type World with // Debugging

        /// Provide the member properties of some SimulantState.
        static member internal getSimulantStateMemberProperties (state : SimulantState) =
            getType state
            |> (fun ty -> ty.GetProperties true)
            |> Array.map (fun (property : PropertyInfo) -> (property.Name, property.PropertyType, property.GetValue state))
            |> Array.toList

        /// Provide the xtension properties of some SimulantState.
        static member internal getSimulantStateXtensionProperties (state : SimulantState) =
            state.GetXtension().Properties
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
            properties
            |> Array.ofList
            |> Array.filter (fun (name, _, _) -> not (Constants.Engine.NonDebugViewablePropertyNames.Contains name))
            |> Array.map a_c
            |> Array.sortBy fst

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