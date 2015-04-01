// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.IO
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open FSharpx
open FSharpx.Collections
open SDL2
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldModule =

    let private ScreenTransitionMouseLeftKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseCenterKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseRightKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseX1Key = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseX2Key = World.makeSubscriptionKey ()
    let private ScreenTransitionKeyboardKeyKey = World.makeSubscriptionKey ()
    let private SplashScreenUpdateKey = World.makeSubscriptionKey ()
    let private LoadedAssemblies = Dictionary<string, Assembly> ()

    type World with

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
        
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)

        /// Try to query that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        static member tryGetIsSelectedScreenIdling world =
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> Some <| selectedScreen.IsIdling world
            | None -> None

        /// Query that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen (failing with an exception if no screen is selected).
        static member isSelectedScreenIdling world =
            match World.tryGetIsSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith <| "Cannot query state of non-existent selected screen."

        /// Try to query that the selected screen is transitioning.
        static member tryGetIsSelectedScreenTransitioning world =
            Option.map not <| World.tryGetIsSelectedScreenIdling world

        /// Query that the selected screen is transitioning (failing with an exception if no screen
        /// is selected).
        static member isSelectedScreenTransitioning world =
            not <| World.isSelectedScreenIdling world

        static member private setScreenTransitionState state (screen : Screen) world =
            let world = screen.SetTransitionStateNp state world
            match state with
            | IdlingState ->
                world |>
                    World.unsubscribe ScreenTransitionMouseLeftKey |>
                    World.unsubscribe ScreenTransitionMouseCenterKey |>
                    World.unsubscribe ScreenTransitionMouseRightKey |>
                    World.unsubscribe ScreenTransitionMouseX1Key |>
                    World.unsubscribe ScreenTransitionMouseX2Key |>
                    World.unsubscribe ScreenTransitionKeyboardKeyKey
            | IncomingState
            | OutgoingState ->
                world |>
                    World.subscribe ScreenTransitionMouseLeftKey World.handleAsSwallow (MouseLeftEventAddress ->- AnyEventAddress) Game |>
                    World.subscribe ScreenTransitionMouseCenterKey World.handleAsSwallow (MouseCenterEventAddress ->- AnyEventAddress) Game |>
                    World.subscribe ScreenTransitionMouseRightKey World.handleAsSwallow (MouseRightEventAddress ->- AnyEventAddress) Game |>
                    World.subscribe ScreenTransitionMouseX1Key World.handleAsSwallow (MouseX1EventAddress ->- AnyEventAddress) Game |>
                    World.subscribe ScreenTransitionMouseX2Key World.handleAsSwallow (MouseX2EventAddress ->- AnyEventAddress) Game |>
                    World.subscribe ScreenTransitionKeyboardKeyKey World.handleAsSwallow (KeyboardKeyEventAddress ->- AnyEventAddress) Game

        /// Select the given screen without transitioning.
        static member selectScreen screen world =
            let world =
                match World.getOptSelectedScreen world with
                | Some selectedScreen ->  World.publish4 () (DeselectEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                | None -> world
            let world = World.setScreenTransitionState IncomingState screen world
            let world = World.setOptSelectedScreen (Some screen) world
            World.publish4 () (SelectEventAddress ->>- screen.ScreenAddress) screen world

        /// Try to transition to the given screen if no other transition is in progress.
        static member tryTransitionScreen destination world =
            match World.getOptSelectedScreen world with
            | Some selectedScreen ->
                if World.containsScreen selectedScreen world then
                    let subscriptionKey = World.makeSubscriptionKey ()
                    let subscription = fun (_ : Event<unit, Screen>) world ->
                        match world.State.OptScreenTransitionDestination with
                        | Some destination ->
                            let world = World.unsubscribe subscriptionKey world
                            let world = World.setOptScreenTransitionDestination None world
                            let world = World.selectScreen destination world
                            (Cascade, world)
                        | None -> failwith "No valid OptScreenTransitionDestinationAddress during screen transition!"
                    let world = World.setOptScreenTransitionDestination (Some destination) world
                    let world = World.setScreenTransitionState OutgoingState selectedScreen world
                    let world = World.subscribe<unit, Screen> subscriptionKey subscription (OutgoingFinishEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                    Some world
                else None
            | None -> None

        /// Transition to the given screen (failing with an exception if another transition is
        /// in progress).
        static member transitionScreen destinationAddress world =
            Option.get <| World.tryTransitionScreen destinationAddress world
            
        // TODO: replace this with more sophisticated use of handleAsScreenTransition4, and so on for its brethren.
        static member private handleAsScreenTransitionFromSplash4<'a, 's when 's :> Simulant> eventHandling destination (_ : Event<'a, 's>) world =
            let world = World.selectScreen destination world
            (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransitionFromSplash<'a, 's when 's :> Simulant> destination event world =
            World.handleAsScreenTransitionFromSplash4<'a, 's> Cascade destination event world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen, as well as with additional
        /// handling provided via the 'by' procedure.
        static member handleAsScreenTransitionFromSplashBy<'a, 's when 's :> Simulant> by destination event (world : World) =
            let (eventHandling, world) = by event world
            World.handleAsScreenTransitionFromSplash4<'a, 's> eventHandling destination event world

        static member private handleAsScreenTransition4<'a, 's when 's :> Simulant>
            eventHandling destination (_ : Event<'a, 's>) world =
            match World.tryTransitionScreen destination world with
            | Some world -> (eventHandling, world)
            | None ->
                trace <| "Program Error: Invalid screen transition for destination address '" + acstring destination.ScreenAddress + "'."
                (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransition<'a, 's when 's :> Simulant> destination event world =
            World.handleAsScreenTransition4<'a, 's> Cascade destination event world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen, as well as with additional
        /// handling provided via the 'by' procedure.
        static member handleAsScreenTransitionBy<'a, 's when 's :> Simulant> by destination event (world : World) =
            let (eventHandling, world) = by event world
            World.handleAsScreenTransition4<'a, 's> eventHandling destination event world

        static member private updateScreenTransition1 (screen : Screen) transition world =
            let transitionTicks = screen.GetTransitionTicksNp world
            if transitionTicks = transition.TransitionLifetime then
                (true, screen.SetTransitionTicksNp 0L world)
            elif transitionTicks > transition.TransitionLifetime then
                debug <| "TransitionLifetime for screen '" + acstring screen.ScreenAddress + "' must be a consistent multiple of TickRate."
                (true, screen.SetTransitionTicksNp 0L world)
            else (false, screen.SetTransitionTicksNp (transitionTicks + World.getTickRate world) world)

        static member private updateScreenTransition world =
            // TODO: split this function up...
            match World.getOptSelectedScreen world with
            | Some selectedScreen ->
                match selectedScreen.GetTransitionStateNp world with
                | IncomingState ->
                    match world.State.Liveness with
                    | Running ->
                        let world =
                            if selectedScreen.GetTransitionTicksNp world = 0L
                            then World.publish4 () (IncomingStartEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                            else world
                        match world.State.Liveness with
                        | Running ->
                            let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetIncoming world) world
                            if finished then
                                let world = World.setScreenTransitionState IdlingState selectedScreen world
                                World.publish4 () (IncomingFinishEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                            else world
                        | Exiting -> world
                    | Exiting -> world
                | OutgoingState ->
                    let world =
                        if selectedScreen.GetTransitionTicksNp world <> 0L then world
                        else World.publish4 () (OutgoingStartEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                    match world.State.Liveness with
                    | Running ->
                        let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetOutgoing world) world
                        if finished then
                            let world = World.setScreenTransitionState IdlingState selectedScreen world
                            match world.State.Liveness with
                            | Running -> World.publish4 () (OutgoingFinishEventAddress ->>- selectedScreen.ScreenAddress) selectedScreen world
                            | Exiting -> world
                        else world
                    | Exiting -> world
                | IdlingState -> world
            | None -> world

        static member private handleSplashScreenIdleUpdate idlingTime ticks event world =
            let world = World.unsubscribe SplashScreenUpdateKey world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleUpdate idlingTime (inc ticks)
                let world = World.subscribe SplashScreenUpdateKey subscription event.EventAddress event.Subscriber world
                (Cascade, world)
            else
                match World.getOptSelectedScreen world with
                | Some selectedScreen ->
                    if World.containsScreen selectedScreen world then
                        let world = World.setScreenTransitionState OutgoingState selectedScreen world
                        (Cascade, world)
                    else
                        trace "Program Error: Could not handle splash screen update due to no selected screen."
                        (Resolve, World.exit world)
                | None ->
                    trace "Program Error: Could not handle splash screen update due to no selected screen."
                    (Resolve, World.exit world)

        static member private handleSplashScreenIdle idlingTime event world =
            let world = World.subscribe SplashScreenUpdateKey (World.handleSplashScreenIdleUpdate idlingTime 0L) UpdateEventAddress event.Subscriber world
            (Resolve, world)

        /// Create a splash screen that transitions to the given destination upon completion.
        static member createSplashScreen persistent splashData dispatcherName destination optName world =
            let cameraEyeSize = World.getCameraBy (fun camera -> camera.EyeSize) world
            let (splashScreen, world) = World.createDissolveScreen splashData.DissolveData dispatcherName optName world
            let (splashGroup, world) = World.createGroup typeof<GroupDispatcher>.Name (Some "SplashGroup") splashScreen world
            let (splashLabel, world) = World.createEntity typeof<LabelDispatcher>.Name (Some "SplashLabel") splashGroup world
            let world = splashScreen.SetPersistent persistent world
            let world = splashGroup.SetPersistent persistent world
            let world = splashLabel.SetPersistent persistent world
            let world = splashLabel.SetSize cameraEyeSize world
            let world = splashLabel.SetPosition (-cameraEyeSize * 0.5f) world
            let world = splashLabel.SetLabelImage splashData.SplashImage world
            let world = World.monitor (World.handleSplashScreenIdle splashData.IdlingTime) (IncomingFinishEventAddress ->>- splashScreen.ScreenAddress) splashScreen world
            let world = World.monitor (World.handleAsScreenTransitionFromSplash destination) (OutgoingFinishEventAddress ->>- splashScreen.ScreenAddress) splashScreen world
            (splashScreen, world)

        /// Create a dissolve screen whose contents is loaded from the given group file.
        static member createDissolveScreenFromGroupFile persistent dissolveData dispatcherName groupFilePath optName world =
            let (dissolveScreen, world) = World.createDissolveScreen dissolveData dispatcherName optName world
            let world = dissolveScreen.SetPersistent persistent world
            let world = snd <| World.readGroupFromFile groupFilePath None dissolveScreen world
            (dissolveScreen, world)

        static member private createIntrinsicOverlays entityDispatchers facets =
            let hasFacetNamesField = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let entityDispatchers = Map.toValueListBy objectify entityDispatchers
            let facets = Map.toValueListBy (fun facet -> facet :> obj) facets
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Reflection.createIntrinsicOverlays hasFacetNamesField sourceTypes

        /// Try to reload the overlays currently in use by the world.
        static member tryReloadOverlays inputDirectory outputDirectory world =
            
            // try to reload overlay file
            let inputOverlayFilePath = Path.Combine (inputDirectory, world.State.OverlayFilePath)
            let outputOverlayFilePath = Path.Combine (outputDirectory, world.State.OverlayFilePath)
            try File.Copy (inputOverlayFilePath, outputOverlayFilePath, true)

                // cache old overlayer and make new one
                let oldOverlayer = world.State.Overlayer
                let intrinsicOverlays = World.createIntrinsicOverlays world.Components.EntityDispatchers world.Components.Facets
                let overlayer = Overlayer.make outputOverlayFilePath intrinsicOverlays
                let world = World.setOverlayer overlayer world

                // apply overlays to all entities
                let entities = World.proxyEntities1 world
                let world =
                    Seq.fold
                        (fun world (entity : Entity) ->
                            let entityState = World.getEntityState entity world
                            match entityState.OptOverlayName with
                            | Some overlayName ->
                                let oldFacetNames = entityState.FacetNames
                                let entityState = { entityState with Id = entityState.Id } // hacky copy
                                Overlayer.applyOverlayToFacetNames overlayName overlayName entityState oldOverlayer world.State.Overlayer
                                match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                                | Right (entityState, world) ->
                                    let facetNames = World.getEntityFacetNamesReflectively entityState
                                    Overlayer.applyOverlay6 overlayName overlayName facetNames entityState oldOverlayer world.State.Overlayer
                                    World.setEntityStateWithoutEvent entityState entity world
                                | Left error -> note <| "There was an issue in applying a reloaded overlay: " + error; world
                            | None -> world)
                        world
                        entities

                // right!
                Right world

            // propagate error
            with exn -> Left <| acstring exn

        /// Try to release the assets in use by the world. Currently does not support reloading
        /// of song assets, and possibly others that are locked by the engine's subsystems.
        static member tryReloadAssets inputDirectory outputDirectory refinementDirectory world =
            
            // try to reload asset graph file
            try File.Copy (
                    Path.Combine (inputDirectory, world.State.AssetGraphFilePath),
                    Path.Combine (outputDirectory, world.State.AssetGraphFilePath), true)

                // reload asset graph
                match Assets.tryBuildAssetGraph inputDirectory outputDirectory refinementDirectory false world.State.AssetGraphFilePath with
                | Right () ->

                    // reload asset metadata
                    match Metadata.tryGenerateAssetMetadataMap world.State.AssetGraphFilePath with
                    | Right assetMetadataMap ->
                    
                        // reload assets
                        let world = World.setAssetMetadataMap assetMetadataMap world
                        let world = World.reloadRenderAssets world
                        let world = World.reloadAudioAssets world
                        Right world
            
                    // propagate errors
                    | Left error -> Left error
                | Left error -> Left error
            with exn -> Left <| acstring exn

        /// A hack for the physics subsystem that allows an old world value to displace the current
        /// one and have its physics values propagated to the imperative physics subsystem.
        static member continueHack group world =
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed.
            let world = World.clearSubsystemsMessages world
            let world = World.addPhysicsMessage RebuildPhysicsHackMessage world
            let entities = World.proxyEntities group world
            Seq.fold (flip World.propagateEntityPhysics) world entities

        static member private processSubsystems subsystemType world =
            Map.toList world.Subsystems |>
            List.filter (fun (_, subsystem) -> subsystem.SubsystemType = subsystemType) |>
            List.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystemResult, subsystem) = subsystem.ProcessMessages world
                let world = subsystem.ApplyResult subsystemResult world
                World.setSubsystem subsystem subsystemName world)
                world

        static member private cleanUpSubsystems world =
            Map.toList world.Subsystems |>
            List.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystem, world) = subsystem.CleanUp world
                World.setSubsystem subsystem subsystemName world)
                world

        static member private processTask (tasksNotRun, world) task =
            let tickTime = World.getTickTime world
            if tickTime = task.ScheduledTime then
                let world = task.Operation world
                (tasksNotRun, world)
            elif tickTime > task.ScheduledTime then
                debug <| "Task leak found for time '" + acstring tickTime + "'."
                (tasksNotRun, world)
            else (Queue.conj task tasksNotRun, world)

        static member private processTasks world =
            let tasks = world.Callbacks.Tasks
            let world = World.clearTasks world
            let (tasksNotRun, world) = Queue.fold World.processTask (Queue.empty, world) tasks
            World.restoreTasks tasksNotRun world

        /// Process an input event from SDL and ultimately publish any related game events.
        static member processInput (event : SDL.SDL_Event) world =
            let world =
                match event.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    World.exit world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single event.button.x, single event.button.y)
                    let world =
                        if World.isMouseButtonDown MouseLeft world
                        then World.publish World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } MouseDragEventAddress Game world
                        else world
                    World.publish World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } MouseMoveEventAddress Game world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseButtonEventAddress = ntoa <| MouseButton.toEventName mouseButton
                    let mouseButtonDownEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Down"
                    let mouseButtonChangeEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let world = World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonDownEventAddress Game world
                    World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress Game world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseButtonEventAddress = ntoa <| MouseButton.toEventName mouseButton
                    let mouseButtonUpEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Up"
                    let mouseButtonChangeEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let world = World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonUpEventAddress Game world
                    World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress Game world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let world = World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyDownEventAddress Game world
                    World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyChangeEventAddress Game world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let world = World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyUpEventAddress Game world
                    World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyChangeEventAddress Game world
                | _ -> world
            (world.State.Liveness, world)

        /// Update the game engine once per frame, updating its subsystems and publishing the
        /// Update event.
        static member processUpdate handleUpdate world =
            let world = handleUpdate world
            match world.State.Liveness with
            | Running ->
                let world = World.updateScreenTransition world
                match world.State.Liveness with
                | Running ->
                    let world = World.processSubsystems UpdateType world
                    match world.State.Liveness with
                    | Running ->
                        let world = World.publish4 () UpdateEventAddress Game world
                        match world.State.Liveness with
                        | Running ->
                            let world = World.processTasks world
                            (world.State.Liveness, world)
                        | Exiting -> (Exiting, world)
                    | Exiting -> (Exiting, world)
                | Exiting -> (Exiting, world)
            | Exiting -> (Exiting, world)

        /// TODO: document!
        static member processRender handleRender world =
            let world = World.processSubsystems RenderType world
            handleRender world

        /// TODO: document!
        static member processPlay world =
            let world = World.processSubsystems AudioType world
            let world = World.updateTickTime world
            World.incrementUpdateCount world

        /// TODO: document!
        static member run4 tryMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                tryMakeWorld
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.cleanUpSubsystems
                sdlConfig

        /// TODO: document!
        static member run tryMakeWorld handleUpdate sdlConfig =
            World.run4 tryMakeWorld handleUpdate id sdlConfig

        static member private pairWithName source =
            (Reflection.getTypeName source, source)

        static member private pairWithNames sources =
            Map.ofListBy World.pairWithName sources

        /// Try to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake useLoadedGameDispatcher tickRate userState (nuPlugin : NuPlugin) sdlDeps =

            // attempt to generate asset metadata so the rest of the world can be created
            match Metadata.tryGenerateAssetMetadataMap AssetGraphFilePath with
            | Right assetMetadataMap ->

                // make the world's subsystems
                let subsystems =
                    let userSubsystems = Map.ofList <| nuPlugin.MakeSubsystems ()
                    let integrator = Integrator.make Gravity
                    let integratorSubsystem = IntegratorSubsystem.make DefaultSubsystemOrder integrator :> Subsystem
                    let renderer = Renderer.make sdlDeps.RenderContext AssetGraphFilePath
                    let renderer = renderer.EnqueueMessage <| HintRenderPackageUseMessage { PackageName = DefaultPackageName }
                    let rendererSubsystem = RendererSubsystem.make DefaultSubsystemOrder renderer :> Subsystem
                    let audioPlayer = AudioPlayer.make AssetGraphFilePath
                    let audioPlayerSubsystem = AudioPlayerSubsystem.make DefaultSubsystemOrder audioPlayer :> Subsystem
                    let defaultSubsystems =
                        Map.ofList
                            [(IntegratorSubsystemName, integratorSubsystem)
                             (RendererSubsystemName, rendererSubsystem)
                             (AudioPlayerSubsystemName, audioPlayerSubsystem)]
                    defaultSubsystems @@ userSubsystems

                // make user-defined components
                let userFacets = World.pairWithNames <| nuPlugin.MakeFacets ()
                let userEntityDispatchers = World.pairWithNames <| nuPlugin.MakeEntityDispatchers ()
                let userGroupDispatchers = World.pairWithNames <| nuPlugin.MakeGroupDispatchers ()
                let userScreenDispatchers = World.pairWithNames <| nuPlugin.MakeScreenDispatchers ()
                let userOptGameDispatcher = nuPlugin.MakeOptGameDispatcher ()

                // infer the active game dispatcher
                let defaultGameDispatcher = GameDispatcher ()
                let activeGameDispatcher =
                    if useLoadedGameDispatcher then
                        match userOptGameDispatcher with
                        | Some gameDispatcher -> gameDispatcher
                        | None -> defaultGameDispatcher
                    else defaultGameDispatcher

                // make facets
                let defaultFacets =
                    Map.ofList
                        [(typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet)
                         (typeof<SpriteFacet>.Name, SpriteFacet () :> Facet)
                         (typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet)]
                let facets = defaultFacets @@ userFacets

                // make entity dispatchers
                // TODO: see if we can reflectively generate these
                let defaultEntityDispatcherList =
                    [EntityDispatcher ()
                     GuiDispatcher () :> EntityDispatcher
                     ButtonDispatcher () :> EntityDispatcher
                     LabelDispatcher () :> EntityDispatcher
                     TextDispatcher () :> EntityDispatcher
                     ToggleDispatcher () :> EntityDispatcher
                     FeelerDispatcher () :> EntityDispatcher
                     FillBarDispatcher () :> EntityDispatcher
                     BlockDispatcher () :> EntityDispatcher
                     BoxDispatcher () :> EntityDispatcher
                     TopViewCharacterDispatcher () :> EntityDispatcher
                     SideViewCharacterDispatcher () :> EntityDispatcher
                     TileMapDispatcher () :> EntityDispatcher]
                let defaultEntityDispatchers = World.pairWithNames defaultEntityDispatcherList
                let entityDispatchers = defaultEntityDispatchers @@ userEntityDispatchers

                // make group dispatchers
                let defaultGroupDispatchers = Map.ofList [World.pairWithName <| GroupDispatcher ()]
                let groupDispatchers = defaultGroupDispatchers @@ userGroupDispatchers

                // make screen dispatchers
                let defaultScreenDispatchers = Map.ofList [World.pairWithName <| ScreenDispatcher ()]
                let screenDispatchers = defaultScreenDispatchers @@ userScreenDispatchers

                // make game dispatchers
                let defaultGameDispatchers = Map.ofList [World.pairWithName <| defaultGameDispatcher]
                let gameDispatchers = 
                    match userOptGameDispatcher with
                    | Some gameDispatcher ->
                        let (gameDispatcherName, gameDispatcher) = World.pairWithName gameDispatcher
                        Map.add gameDispatcherName gameDispatcher defaultGameDispatchers
                    | None -> defaultGameDispatchers

                // make the world's components
                let components =
                    { EntityDispatchers = entityDispatchers
                      GroupDispatchers = groupDispatchers
                      ScreenDispatchers = screenDispatchers
                      GameDispatchers = gameDispatchers
                      Facets = facets }

                // make the world's callbacks
                let callbacks =
                    { Subscriptions = Map.empty
                      Unsubscriptions = Map.empty
                      Tasks = Queue.empty
                      CallbackStates = Map.empty }

                // make the world's state
                let worldState =
                    let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH)
                    let camera = { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                    let intrinsicOverlays = World.createIntrinsicOverlays entityDispatchers facets
                    let userOverlayRoutes = nuPlugin.MakeOverlayRoutes ()
                    { TickRate = tickRate
                      TickTime = 0L
                      UpdateCount = 0L
                      Liveness = Running
                      OptScreenTransitionDestination = None
                      AssetMetadataMap = assetMetadataMap
                      AssetGraphFilePath = AssetGraphFilePath
                      Overlayer = Overlayer.make OverlayFilePath intrinsicOverlays
                      OverlayRouter = OverlayRouter.make entityDispatchers userOverlayRoutes
                      OverlayFilePath = OverlayFilePath
                      Camera = camera
                      OptEntityCache = Unchecked.defaultof<KeyedCache<Address<EntityState> * World, EntityState option>>
                      UserState = userState }

                // make the simulant states
                let simulantStates = (World.makeGameState activeGameDispatcher, Map.empty)

                // make the world itself
                let world =
                    { Subsystems = subsystems
                      Components = components
                      Callbacks = callbacks
                      State = worldState
                      SimulantStates = simulantStates }

                // initialize OptEntityCache
                let world = { world with State = { world.State with OptEntityCache = KeyedCache.make (Address<EntityState>.empty, world) None }}

                // and finally, register the game
                let world = World.registerGame world
                Right world

            // forward error message
            | Left error -> Left error

        /// Make an empty world. Useful for unit-testing.
        static member makeEmpty (userState : 'u) =

            // make the world's subsystems
            let subsystems =
                let integratorSubsystem = IntegratorSubsystem.make DefaultSubsystemOrder { MockIntegrator = () } :> Subsystem
                let rendererSubsystem = RendererSubsystem.make DefaultSubsystemOrder { MockRenderer = () } :> Subsystem
                let audioPlayerSubsystem = AudioPlayerSubsystem.make DefaultSubsystemOrder { MockAudioPlayer = () } :> Subsystem
                Map.ofList
                    [(IntegratorSubsystemName, integratorSubsystem)
                     (RendererSubsystemName, rendererSubsystem)
                     (AudioPlayerSubsystemName, audioPlayerSubsystem)]

            // make the default dispatchers
            let entityDispatcher = EntityDispatcher ()
            let groupDispatcher = GroupDispatcher ()
            let screenDispatcher = ScreenDispatcher ()
            let gameDispatcher = GameDispatcher ()

            // make the world's components
            let components =
                { EntityDispatchers = Map.singleton (Reflection.getTypeName entityDispatcher) entityDispatcher
                  GroupDispatchers = Map.singleton (Reflection.getTypeName groupDispatcher) groupDispatcher
                  ScreenDispatchers = Map.singleton (Reflection.getTypeName screenDispatcher) screenDispatcher
                  GameDispatchers = Map.singleton (Reflection.getTypeName gameDispatcher) gameDispatcher
                  Facets = Map.empty }
            
            // make the world's callbacks
            let callbacks =
                { Subscriptions = Map.empty
                  Unsubscriptions = Map.empty
                  Tasks = Queue.empty
                  CallbackStates = Map.empty }

            // make the world's state
            let worldState =
                { TickRate = 1L
                  TickTime = 0L
                  UpdateCount = 0L
                  Liveness = Running
                  OptScreenTransitionDestination = None
                  AssetMetadataMap = Map.empty
                  AssetGraphFilePath = String.Empty
                  OverlayRouter = OverlayRouter.make (Map.ofList [World.pairWithName entityDispatcher]) []
                  OverlayFilePath = String.Empty
                  Overlayer = { Overlays = XmlDocument () }
                  Camera = { EyeCenter = Vector2.Zero; EyeSize = Vector2 (single ResolutionXDefault, single ResolutionYDefault) }
                  OptEntityCache = Unchecked.defaultof<KeyedCache<Address<EntityState> * World, EntityState option>>
                  UserState = userState }

            // make the simulant states
            let simulantStates = (World.makeGameState gameDispatcher, Map.empty)

            // make the world itself
            let world =
                { Subsystems = subsystems
                  Components = components
                  Callbacks = callbacks
                  State = worldState
                  SimulantStates = simulantStates }

            // initialize OptEntityCache
            let world = { world with State = { world.State with OptEntityCache = KeyedCache.make (Address<EntityState>.empty, world) None }}

            // and finally, register the game
            World.registerGame world

        /// Initialize the Nu game engine. Basically calls all the unavoidable imperative stuff
        /// needed to set up the .NET environment appropriately. MUST be called before making the
        /// world.
        static member init () =

            // make types load reflectively from pathed (non-static) assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add
                (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve <| ResolveEventHandler
                (fun _ args -> snd <| LoadedAssemblies.TryGetValue args.Name)

            // ensure the current culture is invariate
            System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture

            // init type converters
            Math.initTypeConverters ()

        /// Initialize the Nu game engine, and make an empty world. Useful for unit-testing.
        static member initAndMakeEmpty (userState : 'u) =
            World.init ()
            World.makeEmpty userState