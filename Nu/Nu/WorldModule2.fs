// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open SDL2
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModule2 =

    (* Performance Timers *)
    let private TotalTimer = Diagnostics.Stopwatch ()
    let private InputTimer = Diagnostics.Stopwatch ()
    let private PhysicsTimer = Diagnostics.Stopwatch ()
    let private UpdateTimer = Diagnostics.Stopwatch ()
    let private UpdateGatherTimer = Diagnostics.Stopwatch ()
    let private UpdateGameTimer = Diagnostics.Stopwatch ()
    let private UpdateScreensTimer = Diagnostics.Stopwatch ()
    let private UpdateLayersTimer = Diagnostics.Stopwatch ()
    let private UpdateEntitiesTimer = Diagnostics.Stopwatch ()
    let private PostUpdateTimer = Diagnostics.Stopwatch ()
    let private PostUpdateGatherTimer = Diagnostics.Stopwatch ()
    let private PostUpdateGameTimer = Diagnostics.Stopwatch ()
    let private PostUpdateScreensTimer = Diagnostics.Stopwatch ()
    let private PostUpdateLayersTimer = Diagnostics.Stopwatch ()
#if !DISABLE_ENTITY_POST_UPDATE
    let private PostUpdateEntitiesTimer = Diagnostics.Stopwatch ()
#endif
    let private TaskletsTimer = Diagnostics.Stopwatch ()
    let private PerFrameTimer = Diagnostics.Stopwatch ()
    let private PreFrameTimer = Diagnostics.Stopwatch ()
    let private PostFrameTimer = Diagnostics.Stopwatch ()
    let private ActualizeTimer = Diagnostics.Stopwatch ()
    let private ActualizeGatherTimer = Diagnostics.Stopwatch ()
    let private ActualizeEntitiesTimer = Diagnostics.Stopwatch ()
    let private RenderTimer = Diagnostics.Stopwatch ()
    let private AudioTimer = Diagnostics.Stopwatch ()

    (* Transition Values *)
    let private ScreenTransitionMouseLeftId = Gen.id
    let private ScreenTransitionMouseCenterId = Gen.id
    let private ScreenTransitionMouseRightId = Gen.id
    let private ScreenTransitionMouseX1Id = Gen.id
    let private ScreenTransitionMouseX2Id = Gen.id
    let private ScreenTransitionKeyboardKeyId = Gen.id
    let private SplashScreenUpdateId = Gen.id

    type World with

        static member internal makeEntityTree () =
            SpatialTree.make Constants.Engine.EntityTreeGranularity Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds

        static member internal rebuildEntityTree world =
            let omniEntities =
                match World.getOmniScreenOpt world with
                | Some screen -> World.getLayers screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                | None -> Seq.empty
            let selectedEntities =
                match World.getSelectedScreenOpt world with
                | Some screen -> World.getLayers screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                | None -> Seq.empty
            let entities = Seq.append omniEntities selectedEntities
            let tree = World.makeEntityTree ()
            for entity in entities do
                let boundsMax = entity.GetBoundsMax world
                SpatialTree.addElement (entity.GetOmnipresent world || entity.GetAbsolute world) boundsMax entity tree
            tree

        /// Resolve a relation to an address in the current script context.
        static member resolve relation world =
            let scriptContext = World.getScriptContext world
            let address = Relation.resolve scriptContext.SimulantAddress relation
            address

        /// Resolve a relation to an address in the current script context.
        [<FunctionBinding "resolve">]
        static member resolveGeneralized (relation : obj Relation) world =
            World.resolve relation world
    
        /// Relate an address to the current script context.
        static member relate address world =
            let scriptContext = World.getScriptContext world
            let address = Relation.relate scriptContext.SimulantAddress address
            address

        /// Relate an address to the current script context.
        [<FunctionBinding "relate">]
        static member relateGeneralized (address : obj Address) world =
            World.relate address world

        /// Send a message to the renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadAssets world =
            let world = World.reloadRenderAssets world
            let world = World.reloadAudioAssets world
            World.reloadSymbols world
            world

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
                let world = World.unsubscribe ScreenTransitionMouseLeftId world
                let world = World.unsubscribe ScreenTransitionMouseCenterId world
                let world = World.unsubscribe ScreenTransitionMouseRightId world
                let world = World.unsubscribe ScreenTransitionMouseX1Id world
                let world = World.unsubscribe ScreenTransitionMouseX2Id world
                let world = World.unsubscribe ScreenTransitionKeyboardKeyId world
                world
            | IncomingState
            | OutgoingState ->
                let world = World.subscribeWith ScreenTransitionMouseLeftId World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Left/@/Event") Simulants.Game world |> snd
                let world = World.subscribeWith ScreenTransitionMouseCenterId World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Center/@/Event") Simulants.Game world |> snd
                let world = World.subscribeWith ScreenTransitionMouseRightId World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Right/@/Event") Simulants.Game world |> snd
                let world = World.subscribeWith ScreenTransitionMouseX1Id World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X1/@/Event") Simulants.Game world |> snd
                let world = World.subscribeWith ScreenTransitionMouseX2Id World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X2/@/Event") Simulants.Game world |> snd
                let world = World.subscribeWith ScreenTransitionKeyboardKeyId World.handleAsSwallow (stoa<KeyboardKeyData> "KeyboardKey/@/Event") Simulants.Game world |> snd
                world

        /// Select the given screen without transitioning, even if another transition is taking place.
        [<FunctionBinding>]
        static member selectScreen screen world =
            let world =
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    let eventTrace = EventTrace.record4 "World" "selectScreen" "Deselect" EventTrace.empty
                    World.publish () (Events.Deselect --> selectedScreen) eventTrace selectedScreen false world
                | None -> world
            let world = World.setScreenTransitionStatePlus IncomingState screen world
            let world = World.setSelectedScreen screen world
            let eventTrace = EventTrace.record4 "World" "selectScreen" "Select" EventTrace.empty
            World.publish () (Events.Select --> screen) eventTrace screen false world

        /// Try to transition to the given screen if no other transition is in progress.
        [<FunctionBinding>]
        static member tryTransitionScreen destination world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                if  selectedScreen <> destination &&
                    not (World.isSelectedScreenTransitioning world) &&
                    World.getScreenExists selectedScreen world then
                    let subscriptionId = Gen.id
                    let subscription = fun (_ : Event<unit, Screen>) world ->
                        match World.getScreenTransitionDestinationOpt world with
                        | Some destination ->
                            let world = World.unsubscribe subscriptionId world
                            let world = World.setScreenTransitionDestinationOpt None world
                            let world = World.selectScreen destination world
                            (Cascade, world)
                        | None -> failwith "No valid ScreenTransitionDestinationOpt during screen transition!"
                    let world = World.setScreenTransitionDestinationOpt (Some destination) world
                    let world = World.setScreenTransitionStatePlus OutgoingState selectedScreen world
                    let world = World.subscribeWith<unit, Screen> subscriptionId subscription (Events.OutgoingFinish --> selectedScreen) selectedScreen world |> snd
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

        static member private updateScreenTransition3 (screen : Screen) transition world =
            let transitionTicks = screen.GetTransitionTicks world
            if transitionTicks = transition.TransitionLifetime then
                (true, screen.SetTransitionTicks 0L world)
            elif transitionTicks > transition.TransitionLifetime then
                Log.debug ("TransitionLifetime for screen '" + scstring screen.ScreenAddress + "' must be a consistent multiple of TickRate.")
                (true, screen.SetTransitionTicks 0L world)
            else (false, screen.SetTransitionTicks (transitionTicks + World.getTickRate world) world)

        static member private updateScreenTransition2 (selectedScreen : Screen) world =
            match selectedScreen.GetTransitionState world with
            | IncomingState ->
                match World.getLiveness world with
                | Running ->
                    let world =
                        if selectedScreen.GetTransitionTicks world = 0L then
                            let world =
                                match (selectedScreen.GetIncoming world).SongOpt with
                                | Some playSong -> World.playSong playSong.FadeOutMs playSong.Volume playSong.Song world
                                | None -> world
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingStart" EventTrace.empty
                            World.publish () (Events.IncomingStart --> selectedScreen) eventTrace selectedScreen false world
                        else world
                    match World.getLiveness world with
                    | Running ->
                        let (finished, world) = World.updateScreenTransition3 selectedScreen (selectedScreen.GetIncoming world) world
                        if finished then
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingFinish" EventTrace.empty
                            let world = World.setScreenTransitionStatePlus IdlingState selectedScreen world
                            World.publish () (Events.IncomingFinish --> selectedScreen) eventTrace selectedScreen false world
                        else world
                    | Exiting -> world
                | Exiting -> world
            | OutgoingState ->
                let world =
                    if selectedScreen.GetTransitionTicks world = 0L then
                        let world =
                            match (selectedScreen.GetOutgoing world).SongOpt with
                            | Some playSong -> World.fadeOutSong playSong.FadeOutMs world
                            | None -> world
                        let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "OutgoingStart" EventTrace.empty
                        World.publish () (Events.OutgoingStart --> selectedScreen) eventTrace selectedScreen false world
                    else world
                match World.getLiveness world with
                | Running ->
                    let (finished, world) = World.updateScreenTransition3 selectedScreen (selectedScreen.GetOutgoing world) world
                    if finished then
                        let world = World.setScreenTransitionStatePlus IdlingState selectedScreen world
                        match World.getLiveness world with
                        | Running ->
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "OutgoingFinish" EventTrace.empty
                            World.publish () (Events.OutgoingFinish --> selectedScreen) eventTrace selectedScreen false world
                        | Exiting -> world
                    else world
                | Exiting -> world
            | IdlingState -> world

        static member private handleSplashScreenIdleUpdate idlingTime ticks evt world =
            let world = World.unsubscribe SplashScreenUpdateId world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleUpdate idlingTime (inc ticks)
                let world = World.subscribeWith SplashScreenUpdateId subscription evt.Address evt.Subscriber world |> snd
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
            let world = World.subscribeWith SplashScreenUpdateId (World.handleSplashScreenIdleUpdate idlingTime 0L) (Events.Update --> splashScreen) evt.Subscriber world |> snd
            (Cascade, world)

        /// Set the splash aspects of a screen.
        [<FunctionBinding>]
        static member setScreenSplash splashDataOpt destination (screen : Screen) world =
            let splashLayer = screen / "SplashLayer"
            let splashLabel = splashLayer / "SplashLabel"
            let world = World.destroyLayerImmediate splashLayer world
            match splashDataOpt with
            | Some splashDescriptor ->
                let cameraEyeSize = World.getEyeSize world
                let world = World.createLayer<LayerDispatcher> (Some splashLayer.Name) screen world |> snd
                let world = splashLayer.SetPersistent false world
                let world = World.createEntity<LabelDispatcher> (Some splashLabel.Name) DefaultOverlay splashLayer world |> snd
                let world = splashLabel.SetPersistent false world
                let world = splashLabel.SetSize cameraEyeSize world
                let world = splashLabel.SetPosition (-cameraEyeSize * 0.5f) world
                let world = splashLabel.SetLabelImage splashDescriptor.SplashImage world
                let (unsub, world) = World.monitorCompressed Gen.id None None None (Left (World.handleSplashScreenIdle splashDescriptor.IdlingTime screen)) (Events.IncomingFinish --> screen) screen world
                let (unsub2, world) = World.monitorCompressed Gen.id None None None (Left (World.handleAsScreenTransitionFromSplash destination)) (Events.OutgoingFinish --> screen) screen world
                let world = World.monitor (fun _ -> unsub >> unsub2 >> pair Cascade) (Events.Unregistering --> splashLayer) screen world
                world
            | None -> world

        /// Create a dissolve screen whose content is loaded from the given layer file.
        [<FunctionBinding>]
        static member createDissolveScreenFromLayerFile6 dispatcherName nameOpt dissolveDescriptor songOpt layerFilePath world =
            let (dissolveScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt dissolveDescriptor songOpt world
            let world = World.readLayerFromFile layerFilePath None dissolveScreen world |> snd
            (dissolveScreen, world)

        /// Create a dissolve screen whose content is loaded from the given layer file.
        [<FunctionBinding>]
        static member createDissolveScreenFromLayerFile<'d when 'd :> ScreenDispatcher> nameOpt dissolveDescriptor songOpt layerFilePath world =
            World.createDissolveScreenFromLayerFile6 typeof<'d>.Name nameOpt dissolveDescriptor layerFilePath songOpt world

        /// Create a splash screen that transitions to the given destination upon completion.
        [<FunctionBinding>]
        static member createSplashScreen6 dispatcherName nameOpt splashDescriptor destination world =
            let (splashScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt splashDescriptor.DissolveDescriptor None world
            let world = World.setScreenSplash (Some splashDescriptor) destination splashScreen world
            (splashScreen, world)

        /// Create a splash screen that transitions to the given destination upon completion.
        [<FunctionBinding>]
        static member createSplashScreen<'d when 'd :> ScreenDispatcher> nameOpt splashDescriptor destination world =
            World.createSplashScreen6 typeof<'d>.Name nameOpt splashDescriptor destination world

        static member internal handleSubscribeAndUnsubscribe event world =
            // here we need to update the event publish flags for entities based on whether there are subscriptions to
            // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
            // that these optimization do not support, and warn the developer if they are invoked. Additionally, we
            // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
            let eventAddress = event.Data
            let eventNames = Address.getNames eventAddress
            match eventNames with
            | [|eventFirstName; _; screenName; layerName; entityName|] ->
                let entity = Entity [|screenName; layerName; entityName|]
                match eventFirstName with
                | "Update" ->
                    if Array.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug
                            ("Subscribing to entity update events with a wildcard is not supported. " +
                             "This will cause a bug where some entity update events are not published.")
                    let world = World.updateEntityPublishUpdateFlag entity world
                    (Cascade, world)
#if !DISABLE_ENTITY_POST_UPDATE
                | "PostUpdate" ->
                    if Array.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug
                            ("Subscribing to entity post-update events with a wildcard is not supported. " +
                             "This will cause a bug where some entity post-update events are not published.")
                    let world = World.updateEntityPublishPostUpdateFlag entity world
                    (Cascade, world)
#endif
                | _ -> (Cascade, world)
            | eventNames when eventNames.Length >= 3 ->
                let eventFirstName = eventNames.[0]
                let eventSecondName = eventNames.[1]
                match eventFirstName with
                | "Change" when eventSecondName <> "ParentNodeOpt" ->
                    if Array.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug "Subscribing to change events with a wildcard is not supported."
                    (Cascade, world)
                | _ -> (Cascade, world)
            | _ -> (Cascade, world)

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
            let inputOverlayerFilePath = inputDirectory + "/" + Assets.OverlayerFilePath
            let outputOverlayerFilePath = outputDirectory + "/" + Assets.OverlayerFilePath
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
            let inputPreludeFilePath = inputDirectory + "/" + Assets.PreludeFilePath
            let outputPreludeFilePath = outputDirectory + "/" + Assets.PreludeFilePath
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
                    (inputDirectory + "/" + Assets.AssetGraphFilePath,
                     outputDirectory + "/" + Assets.AssetGraphFilePath,
                     true)

                // attempt to load asset graph
                match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
                | Right assetGraph ->

                    // build assets reload asset metadata
                    AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory false assetGraph
                    let metadata = Metadata.make assetGraph
                    let world = World.setMetadata metadata world
                    let world = World.reloadRenderAssets world
                    let world = World.reloadAudioAssets world
                    World.reloadSymbols world
                    let world = World.publish () Events.AssetsReload (EventTrace.record "World" "publishAssetsReload" EventTrace.empty) Simulants.Game false world
                    (Right assetGraph, world)
        
                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// Clear all messages in all subsystems.
        static member clearMessages world =
             let world = World.updatePhysicsEngine PhysicsEngine.clearMessages world
             let world = World.updateRenderer (fun renderer -> Renderer.clearMessages renderer; renderer) world
             let world = World.updateAudioPlayer (fun audioPlayer -> AudioPlayer.clearMessages audioPlayer; audioPlayer) world
             world

        /// Shelve the a world for background storage.
        static member shelve world =

            // not sure if we really want to also clear physics messages here - we didn't used to
            World.clearMessages world

        /// Thaw the state of a world.
        static member unshelve world =

            // clear existing physics messages
            let world = World.updatePhysicsEngine PhysicsEngine.clearMessages world

            // rebuild physics state
            let world = World.enqueuePhysicsMessage RebuildPhysicsHackMessage world

            // propagate current physics state
            let entities = World.getEntities1 world
            let world = Seq.fold (fun world (entity : Entity) -> entity.PropagatePhysics world) world entities
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
        static member private processInput2 (evt : SDL.SDL_Event) world =
            let world =
                match evt.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    World.exit world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single evt.button.x, single evt.button.y)
                    let world =
                        if World.isMouseButtonDown MouseLeft world then
                            let eventTrace = EventTrace.record4 "World" "processInput" "MouseDrag" EventTrace.empty
                            World.publishPlus { MouseMoveData.Position = mousePosition } Events.MouseDrag eventTrace Simulants.Game true world
                        else world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseMove" EventTrace.empty
                    World.publishPlus { MouseMoveData.Position = mousePosition } Events.MouseMove eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonDownEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Down/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonDown" EventTrace.empty
                    let world = World.publishPlus eventData mouseButtonDownEvent eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus eventData mouseButtonChangeEvent eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonUpEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Up/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonUp" EventTrace.empty
                    let world = World.publishPlus eventData mouseButtonUpEvent eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus eventData mouseButtonChangeEvent eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { KeyboardKey = key.scancode |> int |> enum<KeyboardKey>; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyDown" EventTrace.empty
                    let world = World.publishPlus eventData Events.KeyboardKeyDown eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus eventData Events.KeyboardKeyChange eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { KeyboardKey = key.scancode |> int |> enum<KeyboardKey>; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyUp" EventTrace.empty
                    let world = World.publishPlus eventData Events.KeyboardKeyUp eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus eventData Events.KeyboardKeyChange eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_JOYHATMOTION ->
                    let index = evt.jhat.which
                    let direction = evt.jhat.hatValue
                    let eventData = { GamepadDirection = GamepadState.toNuDirection direction }
                    let eventTrace = EventTrace.record4 "World" "processInput" "GamepadDirectionChange" EventTrace.empty
                    World.publishPlus eventData (Events.GamepadDirectionChange index) eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_JOYBUTTONDOWN ->
                    let index = evt.jbutton.which
                    let button = int evt.jbutton.button
                    if GamepadState.isSdlButtonSupported button then
                        let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                        let eventTrace = EventTrace.record4 "World" "processInput" "GamepadButtonDown" EventTrace.empty
                        let world = World.publishPlus eventData (Events.GamepadButtonDown index) eventTrace Simulants.Game true world
                        let eventTrace = EventTrace.record4 "World" "processInput" "GamepadButtonChange" EventTrace.empty
                        World.publishPlus eventData (Events.GamepadButtonChange index) eventTrace Simulants.Game true world
                    else world
                | SDL.SDL_EventType.SDL_JOYBUTTONUP ->
                    let index = evt.jbutton.which
                    let button = int evt.jbutton.button
                    if GamepadState.isSdlButtonSupported button then
                        let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                        let eventTrace = EventTrace.record4 "World" "processInput" "GamepadButtonUp" EventTrace.empty
                        let world = World.publishPlus eventData (Events.GamepadButtonUp index) eventTrace Simulants.Game true world
                        let eventTrace = EventTrace.record4 "World" "processInput" "GamepadButtonChange" EventTrace.empty
                        World.publishPlus eventData (Events.GamepadButtonChange index) eventTrace Simulants.Game true world
                    else world
                | _ -> world
            (World.getLiveness world, world)

        static member private getEntities3 getElementsFromTree world =
            let entityTree = World.getEntityTree world
            let (spatialTree, entityTree) = MutantCache.getMutant (fun () -> World.rebuildEntityTree world) entityTree
            let world = World.setEntityTree entityTree world
            let entities : Entity HashSet = getElementsFromTree spatialTree
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesInView2 world =
            let viewBounds = World.getViewBoundsRelative world
            World.getEntities3 (SpatialTree.getElementsInBounds viewBounds) world

        [<FunctionBinding>]
        static member getEntitiesInBounds3 bounds world =
            World.getEntities3 (SpatialTree.getElementsInBounds bounds) world

        [<FunctionBinding>]
        static member getEntitiesAtPoint3 point world =
            World.getEntities3 (SpatialTree.getElementsAtPoint point) world

        [<FunctionBinding>]
        static member getEntitiesInView world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                let (entities2, world) = World.getEntitiesInView2 world
                entities.UnionWith entities2
                world
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesInBounds bounds world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                let (entities2, world) = World.getEntitiesInBounds3 bounds world
                entities.UnionWith entities2
                world
            (entities, world)

        [<FunctionBinding>]
        static member getEntitiesAtPoint point world =
            let entities = HashSet<Entity> HashIdentity.Structural
            let world =
                let (entities2, world) = World.getEntitiesAtPoint3 point world
                entities.UnionWith entities2
                world
            (entities, world)

        static member private updateScreenTransition world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> World.updateScreenTransition2 selectedScreen world
            | None -> world

        static member private updateSimulants world =

            // gather simulants
            UpdateGatherTimer.Start ()
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let layers = Seq.concat (List.map (flip World.getLayers world) screens)
            let (entities, world) = World.getEntitiesInView2 world
            UpdateGatherTimer.Stop ()

            // update game
            UpdateGameTimer.Start ()
            let world = World.updateGame world
            UpdateGameTimer.Stop ()
            
            // update screens
            UpdateScreensTimer.Start ()
            let world = List.fold (fun world screen -> World.updateScreen screen world) world screens
            UpdateScreensTimer.Stop ()

            // update layers
            UpdateLayersTimer.Start ()
            let world = Seq.fold (fun world layer -> World.updateLayer layer world) world layers
            UpdateLayersTimer.Stop ()

            // update entities
            UpdateEntitiesTimer.Start ()
            let world =
                Seq.fold (fun world (entity : Entity) ->
                    if World.isTicking world || entity.GetAlwaysUpdate world
                    then World.updateEntity entity world
                    else world)
                    world
                    entities
            UpdateEntitiesTimer.Stop ()

            // fin
            world

        static member private postUpdateSimulants world =

            // gather simulants
            PostUpdateGatherTimer.Start ()
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let layers = Seq.concat (List.map (flip World.getLayers world) screens)
#if !DISABLE_ENTITY_POST_UPDATE
            let (entities, world) = World.getEntitiesInView2 world
#endif
            PostUpdateGatherTimer.Stop ()

            // post-update game
            PostUpdateGameTimer.Start ()
            let world = World.postUpdateGame world
            PostUpdateGameTimer.Stop ()

            // post-update screens
            PostUpdateScreensTimer.Start ()
            let world = List.fold (fun world screen -> World.postUpdateScreen screen world) world screens
            PostUpdateScreensTimer.Stop ()

            // post-update layers
            PostUpdateLayersTimer.Start ()
            let world = Seq.fold (fun world layer -> World.postUpdateLayer layer world) world layers
            PostUpdateLayersTimer.Stop ()

#if !DISABLE_ENTITY_POST_UPDATE
            // post-update entities
            PostUpdateEntitiesTimer.Start ()
            let world =
                Seq.fold (fun world (entity : Entity) ->
                    if World.isTicking world || entity.GetAlwaysUpdate world
                    then World.postUpdateEntity entity world
                    else world)
                    world
                    entities
            PostUpdateEntitiesTimer.Stop ()
#endif

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
                let transform = { Position = position; Size = size; Rotation = 0.0f; Depth = Single.MaxValue; Flags = -1; RefCount = 0 }
                World.enqueueRenderMessage
                    (LayeredDescriptorMessage
                        { Depth = transform.Depth
                          PositionY = transform.Position.Y
                          AssetTag = AssetTag.generalize dissolveImage
                          RenderDescriptor =
                            SpriteDescriptor
                                { Transform = transform
                                  Offset = Vector2.Zero
                                  InsetOpt = None
                                  Image = dissolveImage
                                  Color = color
                                  Glow = Vector4.Zero
                                  Flip = FlipNone }})
                    world
            | None -> world

        static member private actualizeScreenTransition (screen : Screen) world =
            match screen.GetTransitionState world with
            | IncomingState -> World.actualizeScreenTransition5 (World.getEyeCenter world) (World.getEyeSize world) screen (screen.GetIncoming world) world
            | OutgoingState -> World.actualizeScreenTransition5 (World.getEyeCenter world) (World.getEyeSize world) screen (screen.GetOutgoing world) world
            | IdlingState -> world

        static member private actualizeSimulants world =

            // gather simulants
            ActualizeGatherTimer.Start ()
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let layers = Seq.concat (List.map (flip World.getLayers world) screens)
            let (entities, world) = World.getEntitiesInView2 world
            ActualizeGatherTimer.Stop ()

            // actualize simulants breadth-first
            let world = World.actualizeGame world
            let world = List.fold (fun world screen -> World.actualizeScreen screen world) world screens
            let world = match World.getSelectedScreenOpt world with Some selectedScreen -> World.actualizeScreenTransition selectedScreen world | None -> world
            let world = Seq.fold (fun world layer -> World.actualizeLayer layer world) world layers
#if DEBUG
            // layer visibility only has an effect on entities in debug mode
            let world =
                Seq.fold (fun world (entity : Entity) ->
                    let layer = entity.Parent
                    if layer.GetVisible world
                    then World.actualizeEntity entity world
                    else world)
                    world entities
#else
            ActualizeEntitiesTimer.Start ()
            let world = Seq.fold (fun world (entity : Entity) -> World.actualizeEntity entity world) world entities
            ActualizeEntitiesTimer.Stop ()
#endif
            world

        static member private processInput world =
            if SDL.SDL_WasInit SDL.SDL_INIT_TIMER <> 0u then
                let mutable result = (Running, world)
                let polledEvent = ref (SDL.SDL_Event ())
                while
                    SDL.SDL_PollEvent polledEvent <> 0 &&
                    (match fst result with Running -> true | Exiting -> false) do
                    result <- World.processInput2 !polledEvent (snd result)
                result
            else (Exiting, world)

        static member private processPhysics world =
            let physicsEngine = World.getPhysicsEngine world
            let (physicsMessages, physicsEngine) = physicsEngine.PopMessages ()
            let world = World.setPhysicsEngine physicsEngine world
            let integrationMessages = PhysicsEngine.integrate (World.getTickRate world) physicsMessages physicsEngine
            Seq.fold
                (fun world integrationMessage ->
                    match World.getLiveness world with
                    | Running ->
                        match integrationMessage with
                        | BodyCollisionMessage bodyCollisionMessage ->
                            let entity = bodyCollisionMessage.BodyShapeSource.Simulant :?> Entity
                            if entity.Exists world then
                                let collisionAddress = Events.Collision --> entity.EntityAddress
                                let collisionData =
                                    { Collider = BodyShapeSource.fromInternal bodyCollisionMessage.BodyShapeSource
                                      Collidee = BodyShapeSource.fromInternal bodyCollisionMessage.BodyShapeSource2
                                      Normal = bodyCollisionMessage.Normal
                                      Speed = bodyCollisionMessage.Speed }
                                let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                                World.publish collisionData collisionAddress eventTrace Simulants.Game false world
                            else world
                        | BodySeparationMessage bodySeparationMessage ->
                            let entity = bodySeparationMessage.BodyShapeSource.Simulant :?> Entity
                            if entity.Exists world then
                                let separationAddress = Events.Separation --> entity.EntityAddress
                                let separationData =
                                    { Separator = BodyShapeSource.fromInternal bodySeparationMessage.BodyShapeSource
                                      Separatee = BodyShapeSource.fromInternal bodySeparationMessage.BodyShapeSource2  }
                                let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                                World.publish separationData separationAddress eventTrace Simulants.Game false world
                            else world
                        | BodyTransformMessage bodyTransformMessage ->
                            let bodySource = bodyTransformMessage.BodySource
                            let entity = bodySource.Simulant :?> Entity
                            let transform = entity.GetTransform world
                            let position = bodyTransformMessage.Position - transform.Size * 0.5f
                            let rotation = bodyTransformMessage.Rotation
                            let world =
                                if bodyTransformMessage.BodySource.BodyId = Gen.idEmpty then
                                    let transform2 = { transform with Position = position; Rotation = rotation }
                                    if transform <> transform2
                                    then entity.SetTransformWithoutEvent transform2 world
                                    else world
                                else world
                            let transformAddress = Events.Transform --> entity.EntityAddress
                            let transformData = { BodySource = BodySource.fromInternal bodySource; Position = position; Rotation = rotation }
                            let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                            World.publish transformData transformAddress eventTrace Simulants.Game false world
                    | Exiting -> world)
                world integrationMessages

        static member private render renderMessages renderContext renderer eyeCenter eyeSize =
            match Constants.Render.ScreenClearing with
            | NoClear -> ()
            | ColorClear (r, g, b) ->
                SDL.SDL_SetRenderDrawColor (renderContext, r, g, b, 255uy) |> ignore
                SDL.SDL_RenderClear renderContext |> ignore
            Renderer.render eyeCenter eyeSize renderMessages renderer
            SDL.SDL_RenderPresent renderContext

        static member private play audioMessages audioPlayer =
            AudioPlayer.play audioMessages audioPlayer

        static member private cleanUp world =
            let world = World.unregisterGame world
            World.cleanUpSubsystems world |> ignore

        /// Run the game engine with the given handlers, but don't clean up at the end, and return the world.
        static member runWithoutCleanUp runWhile preProcess postProcess sdlDeps liveness (rendererThreadOpt : Task option) (audioPlayerThreadOpt : Task option) world =
            TotalTimer.Start ()
            if runWhile world then
                PreFrameTimer.Start ()
                let world = preProcess world
                let world = World.preFrame world
                PreFrameTimer.Stop ()
                match liveness with
                | Running ->
                    let world = World.updateScreenTransition world
                    match World.getLiveness world with
                    | Running ->
                        InputTimer.Start ()
                        let (liveness, world) = World.processInput world
                        InputTimer.Stop ()
                        match liveness with
                        | Running ->
                            PhysicsTimer.Start ()
                            let world = World.processPhysics world
                            PhysicsTimer.Stop ()
                            match World.getLiveness world with
                            | Running ->
                                UpdateTimer.Start ()
                                let world = World.updateSimulants world
                                UpdateTimer.Stop ()
                                match World.getLiveness world with
                                | Running ->
                                    PostUpdateTimer.Start ()
                                    let world = World.postUpdateSimulants world
                                    PostUpdateTimer.Stop ()
                                    match World.getLiveness world with
                                    | Running ->
                                        TaskletsTimer.Start ()
                                        let world = World.processTasklets world
                                        TaskletsTimer.Stop ()
                                        match World.getLiveness world with
                                        | Running ->
                                            ActualizeTimer.Start ()
                                            let world = World.actualizeSimulants world
                                            ActualizeTimer.Stop ()
                                            match World.getLiveness world with
                                            | Running ->
                                                PerFrameTimer.Start ()
                                                let world = World.perFrame world
                                                PerFrameTimer.Stop ()
                                                match World.getLiveness world with
                                                | Running ->
    #if MULTITHREAD
                                                    // attempt to finish renderer thread
                                                    let world =
                                                        match rendererThreadOpt with
                                                        | Some rendererThread ->
                                                            Async.AwaitTask rendererThread |> Async.RunSynchronously
                                                            world
                                                        | None -> world
    
                                                    // attempt to finish audio player thread
                                                    let world =
                                                        match audioPlayerThreadOpt with
                                                        | Some audioPlayerThread ->
                                                            Async.AwaitTask audioPlayerThread |> Async.RunSynchronously
                                                            world
                                                        | None -> world
    
                                                    // attempt to start renderer thread
                                                    let (rendererThreadOpt, world) =
                                                        match SdlDeps.getRenderContextOpt sdlDeps with
                                                        | Some renderContext ->
                                                            let renderer = World.getRenderer world
                                                            let renderMessages = renderer.PopMessages ()
                                                            let world = World.setRenderer renderer world
                                                            let eyeCenter = World.getEyeCenter world
                                                            let eyeSize = World.getEyeSize world
                                                            let rendererThread : Task = Task.Factory.StartNew (fun () -> World.render renderMessages renderContext renderer eyeCenter eyeSize)
                                                            (Some rendererThread, world)
                                                        | None -> (None, world)
    
                                                    // attempt to start audio player thread
                                                    let (audioPlayerThreadOpt, world) =
                                                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u then
                                                            let audioPlayer = World.getAudioPlayer world
                                                            let audioMessages = audioPlayer.PopMessages ()
                                                            let world = World.setAudioPlayer audioPlayer world
                                                            let audioPlayerThread : Task = Task.Factory.StartNew (fun () -> World.play audioMessages audioPlayer)
                                                            (Some audioPlayerThread, world)
                                                        else (None, world)
    #else
                                                    // process rendering on main thread
                                                    RenderTimer.Start ()
                                                    let world =
                                                        match SdlDeps.getRenderContextOpt sdlDeps with
                                                        | Some renderContext ->
                                                            let renderer = World.getRenderer world
                                                            let renderMessages = renderer.PopMessages ()
                                                            let world = World.setRenderer renderer world
                                                            let eyeCenter = World.getEyeCenter world
                                                            let eyeSize = World.getEyeSize world
                                                            World.render renderMessages renderContext renderer eyeCenter eyeSize
                                                            world
                                                        | None -> world
                                                    RenderTimer.Stop ()
    
                                                    // process audio on main thread
                                                    AudioTimer.Start ()
                                                    let world =
                                                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u then
                                                            let audioPlayer = World.getAudioPlayer world
                                                            let audioMessages = audioPlayer.PopMessages ()
                                                            let world = World.setAudioPlayer audioPlayer world
                                                            World.play audioMessages audioPlayer
                                                            world
                                                        else world
                                                    AudioTimer.Stop ()
    #endif
                                                    // post-process the world
                                                    PostFrameTimer.Start ()
                                                    let world = World.postFrame world
                                                    let world = postProcess world
                                                    PostFrameTimer.Stop ()
                                                    match World.getLiveness world with
                                                    | Running ->
    
                                                        // update counters and recur
                                                        TotalTimer.Stop ()
                                                        let world = World.updateTime world
                                                        World.runWithoutCleanUp runWhile preProcess postProcess sdlDeps liveness rendererThreadOpt audioPlayerThreadOpt world
    
                                                    | Exiting -> world
                                                | Exiting -> world
                                            | Exiting -> world
                                        | Exiting -> world
                                    | Exiting -> world
                                | Exiting -> world
                            | Exiting -> world
                        | Exiting -> world
                    | Exiting -> world
                | Exiting -> world
            else world

        /// Run the game engine with the given handlers.
        static member run4 runWhile sdlDeps liveness world =
            let result =
                try let world = World.runWithoutCleanUp runWhile id id sdlDeps liveness None None world
                    World.cleanUp world
                    Constants.Engine.SuccessExitCode
                with exn ->
                    let world = World.choose world
                    Log.trace (scstring exn)
                    World.cleanUp world
                    Constants.Engine.FailureExitCode
#if MULTITHREAD
            // stops background threads
            Environment.Exit result
#endif
            result

[<AutoOpen>]
module GameDispatcherModule =

    type World with

        static member internal signalGame<'model, 'message, 'command> signal (game : Game) world =
            match game.GetDispatcher world with
            | :? GameDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (game.Model<'model> ()) signal game world
            | _ ->
                Log.info "Failed to send signal to game."
                world

        /// Send a signal to a simulant.
        static member signal<'model, 'message, 'command> signal (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.signalEntity<'model, 'message, 'command> signal entity world
            | :? Layer as layer -> World.signalLayer<'model, 'message, 'command> signal layer world
            | :? Screen as screen -> World.signalScreen<'model, 'message, 'command> signal screen world
            | :? Game as game -> World.signalGame<'model, 'message, 'command> signal game world
            | _ -> failwithumf ()

    and Game with

        member this.UpdateModel<'model> updater world =
            this.SetModel<'model> (updater (this.GetModel<'model> world)) world

        member this.Signal<'model, 'message, 'command> signal world =
            World.signalGame<'model, 'message, 'command> signal this world

    and [<AbstractClass>] GameDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit GameDispatcher ()

        member this.GetModel (game : Game) world : 'model =
            game.GetModel<'model> world

        member this.SetModel (model : 'model) (game : Game) world =
            game.SetModel<'model> model world

        member this.Model (game : Game) =
            lens Property? Model (this.GetModel game) (flip this.SetModel game) game

        override this.Register (game, world) =
            let world =
                let property = World.getGameModelProperty world
                if property.DesignerType = typeof<unit>
                then game.SetModel<'model> initial world
                else world
            let channels = this.Channel (this.Model game, game)
            let world = Signal.processChannels this.Message this.Command (this.Model game) channels game world
            let content = this.Content (this.Model game, game)
            let world =
                List.foldi (fun contentIndex world content ->
                    let (screen, world) = World.expandScreenContent World.setScreenSplash content (SimulantOrigin game) game world
                    if contentIndex = 0 then World.selectScreen screen world else world)
                    world content
            let initializers = this.Initializers (this.Model game, game)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let propertyName = def.PropertyName
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = Reflection.isPropertyNonPersistentByName propertyName
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName alwaysPublish nonPersistent property game world
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> game
                    World.monitor (fun (evt : Event) world ->
                        let world = WorldModule.trySignal (handler evt) game world
                        (Cascade, world))
                        eventAddress (game :> Simulant) world
                | BindDefinition (left, right) ->
                    WorldModule.bind5 game left right world)
                world initializers

        override this.Actualize (game, world) =
            let views = this.View (this.GetModel game world, game, world)
            World.actualizeViews views world

        override this.TrySignal (signalObj, game, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> game.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> game.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Initializers : Lens<'model, World> * Game -> PropertyInitializer list
        default this.Initializers (_, _) = []

        abstract member Channel : Lens<'model, World> * Game -> Channel<'message, 'command, Game, World> list
        default this.Channel (_, _) = []

        abstract member Message : 'model * 'message * Game * World -> 'model * Signal<'message, 'command> list
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Game * World -> World * Signal<'message, 'command> list
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Game -> ScreenContent list
        default this.Content (_, _) = []

        abstract member View : 'model * Game * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module WorldModule2' =

    type World with

        /// Send a signal to a simulant.
        static member trySignalFacet signal facetName (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.TrySignalEntityFacet signal facetName world
            | _ -> failwithumf ()

        /// Send a signal to a simulant.
        static member signalFacet<'model, 'message, 'command> signal facetName (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.SignalEntityFacet<'model, 'message, 'command> signal facetName world
            | _ -> failwithumf ()

        /// Send a signal to a simulant.
        static member trySignal signal (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.TrySignal signal world
            | :? Layer as layer -> layer.TrySignal signal world
            | :? Screen as screen -> screen.TrySignal signal world
            | :? Game as game -> game.TrySignal signal world
            | _ -> failwithumf ()

        /// Send a signal to a simulant.
        static member signal<'model, 'message, 'command> signal (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.Signal<'model, 'message, 'command> signal world
            | :? Layer as layer -> layer.Signal<'model, 'message, 'command> signal world
            | :? Screen as screen -> screen.Signal<'model, 'message, 'command> signal world
            | :? Game as game -> game.Signal<'model, 'message, 'command> signal world
            | _ -> failwithumf ()