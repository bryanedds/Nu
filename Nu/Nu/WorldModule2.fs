// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open SDL2
open OpenTK
open Prime
open global.Nu

[<AutoOpen; ModuleBinding>]
module WorldModule2 =

    let private ScreenTransitionMouseLeftKey = makeGuid ()
    let private ScreenTransitionMouseCenterKey = makeGuid ()
    let private ScreenTransitionMouseRightKey = makeGuid ()
    let private ScreenTransitionMouseX1Key = makeGuid ()
    let private ScreenTransitionMouseX2Key = makeGuid ()
    let private ScreenTransitionKeyboardKeyKey = makeGuid ()
    let private SplashScreenUpdateKey = makeGuid ()

    type World with

        static member internal rebuildEntityTree screen world =
            let tree = SpatialTree.make Constants.Engine.EntityTreeGranularity Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
            let entities = screen |> flip World.getLayers world |> Seq.map (flip World.getEntities world) |> Seq.concat
            for entity in entities do
                let boundsMax = entity.GetBoundsMax world
                SpatialTree.addElement (entity.GetOmnipresent world || entity.GetViewType world = Absolute) boundsMax entity tree
            tree

        /// Sort subscriptions by their depth priority.
        static member sortSubscriptionsByDepth subscriptions world =
            World.sortSubscriptionsBy
                (fun (participant : Participant) _ ->
                    let priority =
                        match participant with
                        | :? GlobalParticipantGeneralized
                        | :? Game -> { SortDepth = Constants.Engine.GameSortPriority; SortTarget = Default.Game }
                        | :? Screen as screen -> { SortDepth = Constants.Engine.ScreenSortPriority; SortTarget = screen }
                        | :? Layer as layer -> { SortDepth = Constants.Engine.LayerSortPriority + layer.GetDepth world; SortTarget = layer }
                        | :? Entity as entity -> { SortDepth = entity.GetDepthLayered world; SortTarget = entity }
                        | _ -> failwithumf ()
                    priority :> IComparable)
                subscriptions
                world
    
        /// Resolve a relation to an address in the current script context.
        static member resolve relation world =
            let scriptContext = World.getScriptContext world
            let address = Relation.resolve scriptContext.SimulantAddress relation
            address

        /// Resolve a Participant relation to an address in the current script context.
        [<FunctionBinding "resolve">]
        static member resolveGeneric (relation : obj Relation) world =
            World.resolve relation world

        /// Send a message to the renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadAssets world =
            let world = World.reloadRenderAssets world
            let world = World.reloadAudioAssets world
            World.reloadSymbols world

        /// Try to check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        [<FunctionBinding>]
        static member tryGetIsSelectedScreenIdling world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> Some (selectedScreen.IsIdling world)
            | None -> None

        /// Try to check that the selected screen is transitioning.
        [<FunctionBinding>]
        static member tryGetIsSelectedScreenTransitioning world =
            Option.map not (World.tryGetIsSelectedScreenIdling world)

        /// Check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen (failing with an exception if no screen is selected).
        [<FunctionBinding>]
        static member isSelectedScreenIdling world =
            match World.tryGetIsSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith "Cannot query state of non-existent selected screen."

        /// Check that the selected screen is transitioning (failing with an exception if no screen
        /// is selected).
        [<FunctionBinding>]
        static member isSelectedScreenTransitioning world =
            not (World.isSelectedScreenIdling world)

        static member private setScreenTransitionStatePlus state (screen : Screen) world =
            let world = screen.SetTransitionState state world
            match state with
            | IdlingState ->
                let world = World.unsubscribe ScreenTransitionMouseLeftKey world
                let world = World.unsubscribe ScreenTransitionMouseCenterKey world
                let world = World.unsubscribe ScreenTransitionMouseRightKey world
                let world = World.unsubscribe ScreenTransitionMouseX1Key world
                let world = World.unsubscribe ScreenTransitionMouseX2Key world
                let world = World.unsubscribe ScreenTransitionKeyboardKeyKey world
                world
            | IncomingState
            | OutgoingState ->
                let world = World.subscribePlus ScreenTransitionMouseLeftKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Left/@/Event") Default.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseCenterKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Center/@/Event") Default.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseRightKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Right/@/Event") Default.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX1Key World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X1/@/Event") Default.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX2Key World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X2/@/Event") Default.Game world |> snd
                let world = World.subscribePlus ScreenTransitionKeyboardKeyKey World.handleAsSwallow (stoa<KeyboardKeyData> "KeyboardKey/@/Event") Default.Game world |> snd
                world

        /// Select the given screen without transitioning, even if another transition is taking place.
        [<FunctionBinding>]
        static member selectScreen screen world =
            let world =
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    let eventTrace = EventTrace.record4 "World" "selectScreen" "Deselect" EventTrace.empty
                    World.publish () (Events.Deselect ->- selectedScreen) eventTrace selectedScreen world
                | None -> world
            let world = World.setScreenTransitionStatePlus IncomingState screen world
            let world = World.setSelectedScreen screen world
            let eventTrace = EventTrace.record4 "World" "selectScreen" "Select" EventTrace.empty
            World.publish () (Events.Select ->- screen) eventTrace screen world

        /// Try to transition to the given screen if no other transition is in progress.
        [<FunctionBinding>]
        static member tryTransitionScreen destination world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                if World.getScreenExists selectedScreen world then
                    let subscriptionKey = makeGuid ()
                    let subscription = fun (_ : Event<unit, Screen>) world ->
                        match World.getScreenTransitionDestinationOpt world with
                        | Some destination ->
                            let world = World.unsubscribe subscriptionKey world
                            let world = World.setScreenTransitionDestinationOpt None world
                            let world = World.selectScreen destination world
                            (Cascade, world)
                        | None -> failwith "No valid ScreenTransitionDestinationOpt during screen transition!"
                    let world = World.setScreenTransitionDestinationOpt (Some destination) world
                    let world = World.setScreenTransitionStatePlus OutgoingState selectedScreen world
                    let world = World.subscribePlus<unit, Screen> subscriptionKey subscription (Events.OutgoingFinish ->- selectedScreen) selectedScreen world |> snd
                    (true, world)
                else (false, world)
            | None -> (false, world)

        /// Transition to the given screen (failing with an exception if another transition is in
        /// progress).
        [<FunctionBinding>]
        static member transitionScreen destination world =
            World.tryTransitionScreen destination world |> snd
            
        // TODO: replace this with more sophisticated use of handleAsScreenTransition4, and so on for its brethren.
        static member private handleAsScreenTransitionFromSplash4<'a, 's when 's :> Simulant> handling destination (_ : Event<'a, 's>) world =
            let world = World.selectScreen destination world
            (handling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransitionFromSplash<'a, 's when 's :> Simulant> destination evt world =
            World.handleAsScreenTransitionFromSplash4<'a, 's> Cascade destination evt world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen, as well as with additional
        /// handling provided via the 'by' procedure.
        static member handleAsScreenTransitionFromSplashBy<'a, 's when 's :> Simulant> by destination evt (world : World) =
            let (handling, world) = by evt world
            World.handleAsScreenTransitionFromSplash4<'a, 's> handling destination evt world

        static member private handleAsScreenTransitionPlus<'a, 's when 's :> Simulant>
            handling destination (_ : Event<'a, 's>) world =
            match World.tryTransitionScreen destination world with
            | (true, world) -> (handling, world)
            | (false, world) ->
                Log.trace ("Program Error: Invalid screen transition for destination address '" + scstring destination.ScreenAddress + "'.")
                (handling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransition<'a, 's when 's :> Simulant> destination evt world =
            World.handleAsScreenTransitionPlus<'a, 's> Cascade destination evt world |> snd

        static member private updateScreenTransition1 (screen : Screen) transition world =
            let transitionTicks = screen.GetTransitionTicks world
            if transitionTicks = transition.TransitionLifetime then
                (true, screen.SetTransitionTicks 0L world)
            elif transitionTicks > transition.TransitionLifetime then
                Log.debug ("TransitionLifetime for screen '" + scstring screen.ScreenAddress + "' must be a consistent multiple of TickRate.")
                (true, screen.SetTransitionTicks 0L world)
            else (false, screen.SetTransitionTicks (transitionTicks + World.getTickRate world) world)

        static member private updateScreenTransition (selectedScreen : Screen) world =
            match selectedScreen.GetTransitionState world with
            | IncomingState ->
                match World.getLiveness world with
                | Running ->
                    let world =
                        if selectedScreen.GetTransitionTicks world = 0L then
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingStart" EventTrace.empty
                            World.publish () (Events.IncomingStart ->- selectedScreen) eventTrace selectedScreen world
                        else world
                    match World.getLiveness world with
                    | Running ->
                        let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetIncoming world) world
                        if finished then
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingFinish" EventTrace.empty
                            let world = World.setScreenTransitionStatePlus IdlingState selectedScreen world
                            World.publish () (Events.IncomingFinish ->- selectedScreen) eventTrace selectedScreen world
                        else world
                    | Exiting -> world
                | Exiting -> world
            | OutgoingState ->
                let world =
                    if selectedScreen.GetTransitionTicks world = 0L then
                        let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "OutgoingStart" EventTrace.empty
                        World.publish () (Events.OutgoingStart ->- selectedScreen) eventTrace selectedScreen world
                    else world
                match World.getLiveness world with
                | Running ->
                    let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetOutgoing world) world
                    if finished then
                        let world = World.setScreenTransitionStatePlus IdlingState selectedScreen world
                        match World.getLiveness world with
                        | Running ->
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "OutgoingFinish" EventTrace.empty
                            World.publish () (Events.OutgoingFinish ->- selectedScreen) eventTrace selectedScreen world
                        | Exiting -> world
                    else world
                | Exiting -> world
            | IdlingState -> world

        static member private handleSplashScreenIdleUpdate idlingTime ticks evt world =
            let world = World.unsubscribe SplashScreenUpdateKey world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleUpdate idlingTime (inc ticks)
                let world = World.subscribePlus SplashScreenUpdateKey subscription evt.Address evt.Subscriber world |> snd
                (Cascade, world)
            else
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    if World.getScreenExists selectedScreen world then
                        let world = World.setScreenTransitionStatePlus OutgoingState selectedScreen world
                        (Cascade, world)
                    else
                        Log.trace "Program Error: Could not handle splash screen update due to no selected screen."
                        (Resolve, World.exit world)
                | None ->
                    Log.trace "Program Error: Could not handle splash screen update due to no selected screen."
                    (Resolve, World.exit world)

        static member private handleSplashScreenIdle idlingTime (splashScreen : Screen) evt world =
            let world = World.subscribePlus SplashScreenUpdateKey (World.handleSplashScreenIdleUpdate idlingTime 0L) (Events.Update ->- splashScreen) evt.Subscriber world |> snd
            (Cascade, world)

        /// Set the splash aspects of a screen.
        [<FunctionBinding>]
        static member setScreenSplash splashDataOpt destination (screen : Screen) world =
            let splashLayer = screen / "SplashLayer"
            let splashLabel = splashLayer / "SplashLabel"
            let world = World.destroyLayerImmediate splashLayer world
            match splashDataOpt with
            | Some splashData ->
                let cameraEyeSize = World.getEyeSize world
                let world = World.createLayer<LayerDispatcher> (Some splashLayer.LayerName) screen world |> snd
                let world = splashLayer.SetPersistent false world
                let world = World.createEntity<LabelDispatcher> (Some splashLabel.EntityName) DefaultOverlay splashLayer world |> snd
                let world = splashLabel.SetPersistent false world
                let world = splashLabel.SetSize cameraEyeSize world
                let world = splashLabel.SetPosition (-cameraEyeSize * 0.5f) world
                let world = splashLabel.SetLabelImage splashData.SplashImage world
                let (unsub, world) = World.monitorPlus (World.handleSplashScreenIdle splashData.IdlingTime screen) (Events.IncomingFinish ->- screen) screen world
                let (unsub2, world) = World.monitorPlus (World.handleAsScreenTransitionFromSplash destination) (Events.OutgoingFinish ->- screen) screen world
                let world = World.monitor (fun _ -> unsub >> unsub2) (Events.Unregistering ->- splashLayer) screen world
                world
            | None -> world

        /// Create a dissolve screen whose contents is loaded from the given layer file.
        [<FunctionBinding>]
        static member createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveData layerFilePath world =
            let (dissolveScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt dissolveData world
            let world = World.readLayerFromFile layerFilePath None dissolveScreen world |> snd
            (dissolveScreen, world)

        /// Create a dissolve screen whose contents is loaded from the given layer file.
        [<FunctionBinding>]
        static member createDissolveScreenFromLayerFile<'d when 'd :> ScreenDispatcher> nameOpt dissolveData layerFilePath world =
            World.createDissolveScreenFromLayerFile6 typeof<'d>.Name nameOpt dissolveData layerFilePath world

        /// Create a splash screen that transitions to the given destination upon completion.
        [<FunctionBinding>]
        static member createSplashScreen6 dispatcherName nameOpt splashData destination world =
            let (splashScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt splashData.DissolveData world
            let world = World.setScreenSplash (Some splashData) destination splashScreen world
            (splashScreen, world)

        /// Create a splash screen that transitions to the given destination upon completion.
        [<FunctionBinding>]
        static member createSplashScreen<'d when 'd :> ScreenDispatcher> nameOpt splashData destination world =
            World.createSplashScreen6 typeof<'d>.Name nameOpt splashData destination world

        static member internal handleSubscribeAndUnsubscribe event world =
            // here we need to update the event publish flags for entities based on whether there are subscriptions to
            // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
            // that these optimization do not support, and warn the developer if they are invoked. Additionally, we
            // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
            let eventAddress = event.Data
            let eventNames = Address.getNames eventAddress
            match eventNames with
            | eventFirstName :: _ :: ([_ ;_ ; _] as entityAddress) ->
                let entity = Entity (ltoa entityAddress)
                match eventFirstName with
                | "Update" ->
                    if List.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug
                            ("Subscribing to entity update events with a wildcard is not supported. " +
                             "This will cause a bug where some entity update events are not published.")
                    World.updateEntityPublishUpdateFlag entity world
                | "PostUpdate" ->
                    if List.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug
                            ("Subscribing to entity post-update events with a wildcard is not supported. " +
                             "This will cause a bug where some entity post-update events are not published.")
                    World.updateEntityPublishPostUpdateFlag entity world
                | _ -> world
            | eventFirstName :: eventSecondName :: _ :: _ ->
                match eventFirstName with
                | "Change" when eventSecondName <> "ParentNodeOpt" ->
                    if List.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug "Subscribing to change events with a wildcard is not supported."
                    world
                | _ -> world
            | _ -> world

        static member internal makeIntrinsicOverlays facets entityDispatchers =
            let requiresFacetNames = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = Map.toValueListBy (fun facet -> facet :> obj) facets
            let entityDispatchers = Map.toValueListBy box entityDispatchers
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Reflection.makeIntrinsicOverlays requiresFacetNames sourceTypes

        /// Try to reload the overlayer currently in use by the world.
        static member tryReloadOverlays inputDirectory outputDirectory world =
            
            // attempt to reload overlay file
            let inputOverlayerFilePath = Path.Combine (inputDirectory, Assets.OverlayerFilePath)
            let outputOverlayerFilePath = Path.Combine (outputDirectory, Assets.OverlayerFilePath)
            try File.Copy (inputOverlayerFilePath, outputOverlayerFilePath, true)

                // cache old overlayer and make new one
                let oldOverlayer = World.getOverlayer world
                let entityDispatchers = World.getEntityDispatchers world
                let facets = World.getFacets world
                let intrinsicOverlays = World.makeIntrinsicOverlays facets entityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays outputOverlayerFilePath with
                | Right overlayer ->
                
                    // update overlayer and apply overlays to all entities
                    let world = World.setOverlayer overlayer world
                    let entities = World.getEntities1 world
                    let world = Seq.fold (World.applyEntityOverlay oldOverlayer overlayer) world entities
                    (Right overlayer, world)

                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// Try to reload the prelude currently in use by the world.
        static member tryReloadPrelude inputDirectory outputDirectory world =
            let inputPreludeFilePath = Path.Combine (inputDirectory, Assets.PreludeFilePath)
            let outputPreludeFilePath = Path.Combine (outputDirectory, Assets.PreludeFilePath)
            try File.Copy (inputPreludeFilePath, outputPreludeFilePath, true)
                match World.tryEvalPrelude world with
                | Right struct (preludeStr, world) -> (Right preludeStr, world)
                | Left struct (error, world) -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// Attempt to reload the asset graph.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =
            
            // attempt to reload asset graph file
            try File.Copy
                    (Path.Combine (inputDirectory, Assets.AssetGraphFilePath),
                     Path.Combine (outputDirectory, Assets.AssetGraphFilePath), true)

                // attempt to load asset graph
                match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
                | Right assetGraph ->

                    // build assets reload asset metadata
                    AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory false assetGraph
                    let metadata = Metadata.make assetGraph
                    let world = World.setMetadata metadata world
                    let world = World.reloadRenderAssets world
                    let world = World.reloadAudioAssets world
                    let world = World.reloadSymbols world
                    let world = World.publish () Events.AssetsReload (EventTrace.record "World" "publishAssetsReload" EventTrace.empty) Default.Game world
                    (Right assetGraph, world)
        
                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// A hack for the physics subsystem that allows an old world value to displace the current
        /// one and have its physics values propagated to the imperative physics subsystem.
        static member continueHack world =
            // NOTE: because there is an optimization that makes event context mutable, operations
            // on the exist current world may affect past and future ones if the containing event
            // world incidentally isn't copied. Therefore, we restore the initial event context
            // here.
            World.continueEventSystemHack world
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed.
            let world = World.clearSubsystemsMessages world
            let world = World.enqueuePhysicsMessage RebuildPhysicsHackMessage world
            let entities = World.getEntities1 world
            Seq.fold (fun world (entity : Entity) -> entity.PropagatePhysics world) world entities

        static member private processSubsystems subsystemType world =
            World.getSubsystemMap world |>
            UMap.toSeq |>
            Seq.filter (fun (_, subsystem) -> Subsystem.subsystemType subsystem = subsystemType) |>
            Seq.sortBy (fun (_, subsystem) -> Subsystem.subsystemOrder subsystem) |>
            Seq.fold (fun world (subsystemName, subsystem) ->
                let (subsystemResult, subsystem, world) = Subsystem.processMessages subsystem world
                let world = Subsystem.applyResult subsystemResult subsystem world
                World.addSubsystem subsystemName subsystem world)
                world

        static member private cleanUpSubsystems world =
            World.getSubsystemMap world |>
            UMap.toSeq |>
            Seq.sortBy (fun (_, subsystem) -> Subsystem.subsystemOrder subsystem) |>
            Seq.fold (fun world (subsystemName, subsystem) ->
                let (subsystem, world) = Subsystem.cleanUp subsystem world
                World.addSubsystem subsystemName subsystem world)
                world

        static member private processTasklet (taskletsNotRun, world) tasklet =
            let tickTime = World.getTickTime world
            if tickTime = tasklet.ScheduledTime then
                let world = tasklet.Command.Execute world
                (taskletsNotRun, world)
            elif tickTime > tasklet.ScheduledTime then
                Log.debug ("Tasklet leak found for time '" + scstring tickTime + "'.")
                (taskletsNotRun, world)
            else (UList.add tasklet taskletsNotRun, world)

        static member private processTasklets world =
            let tasklets = World.getTasklets world
            let world = World.clearTasklets world
            let (taskletsNotRun, world) = UList.fold World.processTasklet (UList.makeEmpty (UList.getConfig tasklets), world) tasklets
            World.restoreTasklets taskletsNotRun world

        /// Process an input event from SDL and ultimately publish any related game events.
        static member private processInput (evt : SDL.SDL_Event) world =
            let world =
                match evt.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    World.exit world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single evt.button.x, single evt.button.y)
                    let world =
                        if World.isMouseButtonDown MouseLeft world then
                            let eventTrace = EventTrace.record4 "World" "processInput" "MouseDrag" EventTrace.empty
                            World.publishPlus World.sortSubscriptionsByDepth { MouseMoveData.Position = mousePosition } Events.MouseDrag eventTrace Default.Game true world
                        else world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseMove" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth { MouseMoveData.Position = mousePosition } Events.MouseMove eventTrace Default.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonDownEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Down/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonDown" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonDownEvent eventTrace Default.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonChangeEvent eventTrace Default.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonUpEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Up/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonUp" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonUpEvent eventTrace Default.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonChangeEvent eventTrace Default.Game true world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyDown" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyDown eventTrace Default.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange eventTrace Default.Game true world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyUp" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyUp eventTrace Default.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange eventTrace Default.Game true world
                | _ -> world
            (World.getLiveness world, world)

        static member private getEntities3 getElementsFromTree (screen : Screen) world =
            let entityTree = screen.GetEntityTree world
            let (spatialTree, entityTree) = MutantCache.getMutant (fun () -> World.rebuildEntityTree screen world) entityTree
            let world = screen.SetEntityTreeNoEvent entityTree world
            let entities : Entity HashSet = getElementsFromTree spatialTree
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesInView2 (screen : Screen) world =
            let viewBounds = World.getViewBoundsRelative world
            World.getEntities3 (SpatialTree.getElementsInBounds viewBounds) screen world

        [<FunctionBinding>]
        static member getEntitiesInBounds3 bounds (screen : Screen) world =
            World.getEntities3 (SpatialTree.getElementsInBounds bounds) screen world

        [<FunctionBinding>]
        static member getEntitiesAtPoint3 point (screen : Screen) world =
            World.getEntities3 (SpatialTree.getElementsAtPoint point) screen world

        [<FunctionBinding>]
        static member getEntitiesInView world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                match World.getOmniScreenOpt world with
                | Some omniScreen ->
                    let (entities2, world) = World.getEntitiesInView2 omniScreen world
                    entities.UnionWith entities2
                    world
                | None -> world
            let world =
                match World.getSelectedScreenOpt world with
                | Some screen ->
                    let (entities2, world) = World.getEntitiesInView2 screen world
                    entities.UnionWith entities2
                    world
                | None -> world
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesInBounds bounds world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                match World.getOmniScreenOpt world with
                | Some omniScreen ->
                    let (entities2, world) = World.getEntitiesInBounds3 bounds omniScreen world
                    entities.UnionWith entities2
                    world
                | None -> world
            let world =
                match World.getSelectedScreenOpt world with
                | Some screen ->
                    let (entities2, world) = World.getEntitiesInBounds3 bounds screen world
                    entities.UnionWith entities2
                    world
                | None -> world
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesAtPoint point world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                match World.getOmniScreenOpt world with
                | Some omniScreen ->
                    let (entities2, world) = World.getEntitiesAtPoint3 point omniScreen world
                    entities.UnionWith entities2
                    world
                | None -> world
            let world =
                match World.getSelectedScreenOpt world with
                | Some screen ->
                    let (entities2, world) = World.getEntitiesAtPoint3 point screen world
                    entities.UnionWith entities2
                    world
                | None -> world
            (entities, world)

        static member private updateSimulants world =

            // gather simulants
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let layers = Seq.concat (List.map (flip World.getLayers world) screens)
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                List.fold (fun world screen ->
                    let (entities2, world) = World.getEntitiesInView2 screen world
                    entities.UnionWith (entities2 :> _ seq)
                    world)
                    world
                    screens

            // update simulants breadth-first
            let world = World.updateGame world
            let world = List.fold (fun world screen -> World.updateScreen screen world) world screens
            let world = match World.getSelectedScreenOpt world with Some selectedScreen -> World.updateScreenTransition selectedScreen world | None -> world
            let world = Seq.fold (fun world layer -> World.updateLayer layer world) world layers
            let world =
                Seq.fold (fun world (entity : Entity) ->
                    if World.isTicking world || entity.GetAlwaysUpdate world
                    then World.updateEntity entity world
                    else world)
                    world
                    entities

            // post-update simulants breadth-first
            let world = World.postUpdateGame world
            let world = List.fold (fun world screen -> World.postUpdateScreen screen world) world screens
            let world = Seq.fold (fun world layer -> World.postUpdateLayer layer world) world layers
            let world =
                Seq.fold (fun world (entity : Entity) ->
                    if World.isTicking world || entity.GetAlwaysUpdate world
                    then World.postUpdateEntity entity world
                    else world)
                    world
                    entities

            // fin
            world

        static member private actualizeScreenTransition5 (_ : Vector2) (eyeSize : Vector2) (screen : Screen) transition world =
            match transition.DissolveImageOpt with
            | Some dissolveImage ->
                let progress = single (screen.GetTransitionTicks world) / single transition.TransitionLifetime
                let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
                let color = Vector4 (Vector3.One, alpha)
                let position = -eyeSize * 0.5f // negation for right-handedness
                let size = eyeSize
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = Single.MaxValue
                              PositionY = position.Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = position
                                      Size = size
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = dissolveImage
                                      Color = color }}|])
                    world
            | None -> world

        static member private actualizeScreenTransition (screen : Screen) world =
            match screen.GetTransitionState world with
            | IncomingState -> World.actualizeScreenTransition5 (World.getEyeCenter world) (World.getEyeSize world) screen (screen.GetIncoming world) world
            | OutgoingState -> World.actualizeScreenTransition5 (World.getEyeCenter world) (World.getEyeSize world) screen (screen.GetOutgoing world) world
            | IdlingState -> world

        static member private actualizeSimulants world =

            // gather simulants
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let layers = Seq.concat (List.map (flip World.getLayers world) screens)
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                List.fold (fun world screen ->
                    let (entities2, world) = World.getEntitiesInView2 screen world
                    entities.UnionWith (entities2 :> _ seq)
                    world)
                    world
                    screens

            // actualize simulants breadth-first
            let world = World.actualizeGame world
            let world = List.fold (fun world screen -> World.actualizeScreen screen world) world screens
            let world = match World.getSelectedScreenOpt world with Some selectedScreen -> World.actualizeScreenTransition selectedScreen world | None -> world
            let world = Seq.fold (fun world layer -> World.actualizeLayer layer world) world layers
            let world = Seq.fold (fun world (entity : Entity) -> World.actualizeEntity entity world) world entities

            // fin
            world

        static member private processUpdate handleUpdate world =
            let world = handleUpdate world
            match World.getLiveness world with
            | Running ->
                let world = World.processSubsystems UpdateType world
                match World.getLiveness world with
                | Running ->
                    let world = World.updateSimulants world
                    match World.getLiveness world with
                    | Running ->
                        let world = World.processTasklets world
                        match World.getLiveness world with
                        | Running ->
                            let world = World.actualizeSimulants world
                            (World.getLiveness world, world)
                        | Exiting -> (Exiting, world)
                    | Exiting -> (Exiting, world)
                | Exiting -> (Exiting, world)
            | Exiting -> (Exiting, world)

        static member private processRender handleRender world =
            let world = World.processSubsystems RenderType world
            handleRender world

        static member private processPlay world =
            let world = World.processSubsystems AudioType world
            let world = World.updateTickTime world
            World.incrementUpdateCount world

        static member private cleanUp world =
            let world = World.unregisterGame world
            World.cleanUpSubsystems world |> ignore

        /// Run the game without cleaning up the world's resources.
        static member runWithoutCleanUp runWhile handleUpdate handleRender sdlDeps liveness world =
            Sdl.runWithoutCleanUp 
                runWhile
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                sdlDeps
                liveness
                world

        /// Run the world's game while runWhile succeeds.
        static member run6 runWhile handleUpdate handleRender sdlDeps liveness world =
            Sdl.run9
                runWhile
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.choose
                World.cleanUp
                sdlDeps
                liveness
                world

        /// Run the world's game while runWhile succeeds.
        static member run4 runWhile sdlDeps liveness world =
            World.run6 runWhile id id sdlDeps liveness world

        /// Run the world's game unto conclusion.
        static member run attemptMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                attemptMakeWorld
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.choose
                World.cleanUp
                sdlConfig

[<AutoOpen>]
module GameDispatcherModule =

    type [<AbstractClass>] GameDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit GameDispatcher ()

        member this.GetModel (_ : Game) world : 'model =
            match World.tryGetGameProperty Property? Model world with
            | Some property -> property.PropertyValue :?> 'model
            | None -> initial

        member this.SetModel (model : 'model) (_ : Game) world =
            let property = { PropertyType = typeof<'model>; PropertyValue = model }
            World.trySetGameProperty Property? Model property world |> snd

        member this.Model (game : Game) =
            Lens.make Property? Model (this.GetModel game) (flip this.SetModel game) game

        override this.Register (game, world) =
            let bindings = this.Bindings (this.GetModel game world, game, world)
            let world =
                List.fold (fun world binding ->
                    match binding with
                    | Message binding ->
                        Stream.monitor (fun evt world ->
                            let messageOpt = binding.MakeValueOpt evt
                            match messageOpt with
                            | Some message ->
                                let (model, commands) = this.Message (message, this.GetModel game world, game, world)
                                let world = this.SetModel model game world
                                List.fold (fun world command ->
                                    this.Command (command, this.GetModel game world, game, world))
                                    world commands
                            | None -> world)
                            game binding.Stream world
                    | Command binding ->
                        Stream.monitor (fun evt world ->
                            let messageOpt = binding.MakeValueOpt evt
                            match messageOpt with
                            | Some message -> this.Command (message, this.GetModel game world, game, world)
                            | None -> world)
                            game binding.Stream world)
                    world bindings
            let contents = this.Content (this.GetModel game world, game, world)
            let world =
                List.foldi (fun contentIndex world content ->
                    let (screen, world) = World.expandScreen World.setScreenSplash content game world
                    if contentIndex = 0 then World.selectScreen screen world else world)
                    world contents
            world

        override this.Actualize (game, world) =
            let views = this.View (this.GetModel game world, game, world)
            List.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Effect effect -> effect world)
                world views

        abstract member Bindings : 'model * Game * World -> Binding<'message, 'command, Game, World> list
        abstract member Message : 'message * 'model * Game * World -> 'model * 'command list
        abstract member Command : 'command * 'model * Game * World -> World
        abstract member Content : 'model * Game * World -> ScreenContent list
        abstract member View : 'model * Game * World -> View list
        default this.Message (_, model, _, _) = just model
        default this.Command (_, _, _, world) = world
        default this.View (_, _, _) = []

    type Game with
    
        member this.GetModel (dispatcher : GameDispatcher<_, _, _>) world = dispatcher.GetModel this world
        member this.SetModel (dispatcher : GameDispatcher<_, _, _>) value world = dispatcher.SetModel value this world
        member this.Model (dispatcher : GameDispatcher<_, _, _>) = dispatcher.Model this