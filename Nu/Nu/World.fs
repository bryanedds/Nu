// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open SDL2
open OpenTK
open Prime
open global.Nu

[<AutoOpen>]
module WorldModule2 =

    let private ScreenTransitionMouseLeftKey = makeGuid ()
    let private ScreenTransitionMouseCenterKey = makeGuid ()
    let private ScreenTransitionMouseRightKey = makeGuid ()
    let private ScreenTransitionMouseX1Key = makeGuid ()
    let private ScreenTransitionMouseX2Key = makeGuid ()
    let private ScreenTransitionKeyboardKeyKey = makeGuid ()
    let private SplashScreenUpdateKey = makeGuid ()

    type World with

        static member private pairWithName source =
            (getTypeName source, source)

        static member private makeDefaultGameDispatchers () =
            Map.ofList [World.pairWithName (GameDispatcher ())]
            
        static member private makeDefaultScreenDispatchers () =
            Map.ofList [World.pairWithName (ScreenDispatcher ())]

        static member private makeDefaultLayerDispatchers () =
            Map.ofList [World.pairWithName (LayerDispatcher ())]

        static member private makeDefaultEntityDispatchers () =
            // TODO: see if we can reflectively generate these
            Map.ofListBy World.pairWithName ^
                [EntityDispatcher ()
                 ImperativeDispatcher () :> EntityDispatcher
                 NodeDispatcher () :> EntityDispatcher
                 EffectDispatcher () :> EntityDispatcher
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

        static member private makeDefaultFacets () =
            Map.ofList
                [(typeof<MountFacet>.Name, MountFacet () :> Facet)
                 (typeof<EffectFacet>.Name, EffectFacet () :> Facet)
                 (typeof<ScriptFacet>.Name, ScriptFacet () :> Facet)
                 (typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet)
                 (typeof<StaticSpriteFacet>.Name, StaticSpriteFacet () :> Facet)
                 (typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet)]

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
                        | :? Game as game -> { SortDepth = Constants.Engine.GameSortPriority; SortTarget = game }
                        | :? Screen as screen -> { SortDepth = Constants.Engine.ScreenSortPriority; SortTarget = screen }
                        | :? Layer as layer -> { SortDepth = Constants.Engine.LayerSortPriority + layer.GetDepth world; SortTarget = layer }
                        | :? Entity as entity -> { SortDepth = entity.GetDepthLayered world; SortTarget = entity }
                        | _ -> failwithumf ()
                    priority :> IComparable)
                subscriptions
                world

        /// Try to check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        static member tryGetSelectedScreenIdling world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> Some ^ selectedScreen.IsIdling world
            | None -> None

        /// Try to check that the selected screen is transitioning.
        static member tryGetSelectedScreenTransitioning world =
            Option.map not (World.tryGetSelectedScreenIdling world)

        /// Check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen (failing with an exception if no screen is selected).
        static member selectedScreenIdling world =
            match World.tryGetSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith "Cannot query state of non-existent selected screen."

        /// Check that the selected screen is transitioning (failing with an exception if no screen
        /// is selected).
        static member selectedScreenTransitioning world =
            not ^ World.selectedScreenIdling world

        static member private setScreenTransitionState state (screen : Screen) world =
            let world = screen.SetTransitionStateNp state world
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
                let world = World.subscribePlus ScreenTransitionMouseLeftKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Left/@/Event") Simulants.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseCenterKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Center/@/Event") Simulants.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseRightKey World.handleAsSwallow (stoa<MouseButtonData> "Mouse/Right/@/Event") Simulants.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX1Key World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X1/@/Event") Simulants.Game world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX2Key World.handleAsSwallow (stoa<MouseButtonData> "Mouse/X2/@/Event") Simulants.Game world |> snd
                let world = World.subscribePlus ScreenTransitionKeyboardKeyKey World.handleAsSwallow (stoa<KeyboardKeyData> "KeyboardKey/@/Event") Simulants.Game world |> snd
                world

        /// Select the given screen without transitioning, even if another transition is taking place.
        static member selectScreen screen world =
            let world =
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    let eventTrace = EventTrace.record4 "World" "selectScreen" "Deselect" EventTrace.empty
                    World.publish () (Events.Deselect ->- selectedScreen) eventTrace selectedScreen world
                | None -> world
            let world = World.setScreenTransitionState IncomingState screen world
            let world = World.setSelectedScreen screen world
            let eventTrace = EventTrace.record4 "World" "selectScreen" "Select" EventTrace.empty
            World.publish () (Events.Select ->- screen) eventTrace screen world

        /// Try to transition to the given screen if no other transition is in progress.
        static member tryTransitionScreen destination world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                if World.screenExists selectedScreen world then
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
                    let world = World.setScreenTransitionState OutgoingState selectedScreen world
                    let world = World.subscribePlus<unit, Screen> subscriptionKey subscription (Events.OutgoingFinish ->- selectedScreen) selectedScreen world |> snd
                    Some world
                else let _ = World.choose world in None
            | None -> None

        /// Transition to the given screen (failing with an exception if another transition is in
        /// progress).
        static member transitionScreen destinationAddress world =
            Option.get ^ World.tryTransitionScreen destinationAddress world
            
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
            | Some world -> (handling, world)
            | None ->
                Log.trace ^ "Program Error: Invalid screen transition for destination address '" + scstring destination.ScreenAddress + "'."
                (handling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransition<'a, 's when 's :> Simulant> destination evt world =
            World.handleAsScreenTransitionPlus<'a, 's> Cascade destination evt world |> snd

        static member private updateScreenTransition1 (screen : Screen) transition world =
            let transitionTicks = screen.GetTransitionTicksNp world
            if transitionTicks = transition.TransitionLifetime then
                (true, screen.SetTransitionTicksNp 0L world)
            elif transitionTicks > transition.TransitionLifetime then
                Log.debug ^ "TransitionLifetime for screen '" + scstring screen.ScreenAddress + "' must be a consistent multiple of TickRate."
                (true, screen.SetTransitionTicksNp 0L world)
            else (false, screen.SetTransitionTicksNp (transitionTicks + World.getTickRate world) world)

        static member private updateScreenTransition (selectedScreen : Screen) world =
            match selectedScreen.GetTransitionStateNp world with
            | IncomingState ->
                match World.getLiveness world with
                | Running ->
                    let world =
                        if selectedScreen.GetTransitionTicksNp world = 0L then
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingStart" EventTrace.empty
                            World.publish () (Events.IncomingStart ->- selectedScreen) eventTrace selectedScreen world
                        else world
                    match World.getLiveness world with
                    | Running ->
                        let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetIncoming world) world
                        if finished then
                            let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "IncomingFinish" EventTrace.empty
                            let world = World.setScreenTransitionState IdlingState selectedScreen world
                            World.publish () (Events.IncomingFinish ->- selectedScreen) eventTrace selectedScreen world
                        else world
                    | Exiting -> world
                | Exiting -> world
            | OutgoingState ->
                let world =
                    if selectedScreen.GetTransitionTicksNp world = 0L then
                        let eventTrace = EventTrace.record4 "World" "updateScreenTransition" "OutgoingStart" EventTrace.empty
                        World.publish () (Events.OutgoingStart ->- selectedScreen) eventTrace selectedScreen world
                    else world
                match World.getLiveness world with
                | Running ->
                    let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetOutgoing world) world
                    if finished then
                        let world = World.setScreenTransitionState IdlingState selectedScreen world
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
                    if World.screenExists selectedScreen world then
                        let world = World.setScreenTransitionState OutgoingState selectedScreen world
                        (Cascade, world)
                    else
                        Log.trace "Program Error: Could not handle splash screen update due to no selected screen."
                        (Resolve, World.exit world)
                | None ->
                    Log.trace "Program Error: Could not handle splash screen update due to no selected screen."
                    (Resolve, World.exit world)

        static member private handleSplashScreenIdle idlingTime (splashScreen : Screen) evt world =
            let world = World.subscribePlus SplashScreenUpdateKey (World.handleSplashScreenIdleUpdate idlingTime 0L) (Events.Update ->- splashScreen) evt.Subscriber world |> snd
            (Resolve, world)

        /// Create a dissolve screen whose contents is loaded from the given layer file.
        static member createDissolveScreenFromLayerFile<'d when 'd :> ScreenDispatcher> specializationOpt nameOpt dissolveData layerFilePath world =
            let (dissolveScreen, world) = World.createDissolveScreen<'d> dissolveData specializationOpt nameOpt world
            let world = World.readLayerFromFile layerFilePath None dissolveScreen world |> snd
            (dissolveScreen, world)

        /// Create a splash screen that transitions to the given destination upon completion.
        static member createSplashScreen<'d when 'd :> ScreenDispatcher> specializationOpt nameOpt splashData destination world =
            let cameraEyeSize = World.getEyeSize world
            let (splashScreen, world) = World.createDissolveScreen<'d> splashData.DissolveData specializationOpt nameOpt world
            let (splashLayer, world) = World.createLayer<LayerDispatcher> None (Some "SplashLayer") splashScreen world
            let (splashLabel, world) = World.createEntity<LabelDispatcher> None (Some "SplashLabel") splashLayer world
            let world = splashLabel.SetSize cameraEyeSize world
            let world = splashLabel.SetPosition (-cameraEyeSize * 0.5f) world
            let world = splashLabel.SetLabelImage splashData.SplashImage world
            let world = World.monitorPlus (World.handleSplashScreenIdle splashData.IdlingTime splashScreen) (Events.IncomingFinish ->- splashScreen) splashScreen world |> snd
            let world = World.monitorPlus (World.handleAsScreenTransitionFromSplash destination) (Events.OutgoingFinish ->- splashScreen) splashScreen world |> snd
            (splashScreen, world)

        static member private handleSubscribeAndUnsubscribe event world =
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
                        Log.debug ^
                            "Subscribing to entity update events with a wildcard is not supported. " +
                            "This will cause a bug where some entity update events are not published."
                    World.updateEntityPublishUpdateFlag entity world
                | "PostUpdate" ->
                    if List.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug ^
                            "Subscribing to entity post-update events with a wildcard is not supported. " +
                            "This will cause a bug where some entity post-update events are not published."
                    World.updateEntityPublishPostUpdateFlag entity world
                | _ -> world
            | eventFirstName :: _ :: _ :: _ ->
                match eventFirstName with
                | "Change" ->
                    if List.contains (Address.head Events.Wildcard) eventNames then
                        Log.debug "Subscribing to change events with a wildcard is not supported."
                    world
                | _ -> world
            | _ -> world

        static member private createIntrinsicOverlays facets entityDispatchers =
            let requiresFacetNames = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = Map.toValueListBy (fun facet -> facet :> obj) facets
            let entityDispatchers = Map.toValueListBy box entityDispatchers
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Reflection.createIntrinsicOverlays requiresFacetNames sourceTypes

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
                let intrinsicOverlays = World.createIntrinsicOverlays entityDispatchers facets
                match Overlayer.tryMakeFromFile intrinsicOverlays outputOverlayerFilePath with
                | Right overlayer ->
                
                    // update overlayer and apply overlays to all entities
                    let world = World.setOverlayer overlayer world
                    let entities = World.getEntities1 world
                    let world = Seq.fold (World.applyEntityOverlay oldOverlayer overlayer) world entities
                    Right (overlayer, world)

                // propagate errors
                | Left error -> Left (error, world)
            with exn -> Left (scstring exn, World.choose world)

        /// Try to reload the prelude currently in use by the world.
        static member tryReloadPrelude inputDirectory outputDirectory world =
            let inputPreludeFilePath = Path.Combine (inputDirectory, Assets.PreludeFilePath)
            let outputPreludeFilePath = Path.Combine (outputDirectory, Assets.PreludeFilePath)
            try File.Copy (inputPreludeFilePath, outputPreludeFilePath, true)
                match World.tryEvalPrelude world with
                | Right struct (preludeStr, world) -> Right (preludeStr, world)
                | Left struct (error, world) -> Left (error, world)
            with exn -> Left (scstring exn, World.choose world)

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
                    let world = World.publish () Events.AssetsReload (EventTrace.record "World" "publishAssetsReload" EventTrace.empty) Simulants.Game world
                    Right (assetGraph, world)
        
                // propagate errors
                | Left error -> Left (error, world)
            with exn -> Left (scstring exn, World.choose world)

        /// A hack for the physics subsystem that allows an old world value to displace the current
        /// one and have its physics values propagated to the imperative physics subsystem.
        static member continueHack layer world =
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed.
            let world = World.clearSubsystemsMessages world
            let world = World.enqueuePhysicsMessage RebuildPhysicsHackMessage world
            let entities = World.getEntities layer world
            Seq.fold (fun world (entity : Entity) -> entity.PropagatePhysics world) world entities

        static member private processSubsystems subsystemType world =
            World.getSubsystemMap world |>
            UMap.toSeq |>
            Seq.filter (fun (_, subsystem) -> subsystem.SubsystemType = subsystemType) |>
            Seq.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            Seq.fold (fun world (subsystemName, subsystem) ->
                let (subsystemResult, subsystem, world) = subsystem.ProcessMessages world
                let world = subsystem.ApplyResult (subsystemResult, world)
                World.addSubsystem subsystemName subsystem world)
                world

        static member private cleanUpSubsystems world =
            World.getSubsystemMap world |>
            UMap.toSeq |>
            Seq.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            Seq.fold (fun world (subsystemName, subsystem) ->
                let (subsystem, world) = subsystem.CleanUp world
                World.addSubsystem subsystemName subsystem world)
                world

        static member private processTasklet (taskletsNotRun, world) tasklet =
            let tickTime = World.getTickTime world
            if tickTime = tasklet.ScheduledTime then
                let world = tasklet.Command.Execute world
                (taskletsNotRun, world)
            elif tickTime > tasklet.ScheduledTime then
                Log.debug ^ "Tasklet leak found for time '" + scstring tickTime + "'."
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
                            World.publishPlus World.sortSubscriptionsByDepth { MouseMoveData.Position = mousePosition } Events.MouseDrag eventTrace Simulants.Game true world
                        else world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseMove" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth { MouseMoveData.Position = mousePosition } Events.MouseMove eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonDownEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Down/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonDown" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonDownEvent eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonChangeEvent eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonUpEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Up/Event")
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event")
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonUp" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonUpEvent eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "MouseButtonChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByDepth eventData mouseButtonChangeEvent eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyDown" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyDown eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange eventTrace Simulants.Game true world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyUp" EventTrace.empty
                    let world = World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyUp eventTrace Simulants.Game true world
                    let eventTrace = EventTrace.record4 "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange eventTrace Simulants.Game true world
                | _ -> world
            (World.getLiveness world, world)

        static member getEntitiesInView selectedScreen world =
            let viewBounds = World.getViewBoundsRelative world
            let (spatialTree, entityTree) = MutantCache.getMutant (fun () -> World.rebuildEntityTree selectedScreen world) (selectedScreen.GetEntityTreeNp world)
            let world = selectedScreen.SetEntityTreeNpNoEvent entityTree world
            let entities = SpatialTree.getElementsInBounds viewBounds spatialTree
            (entities, world)

        static member private updateSimulants world =
            
            // check for existence of screen; otherwise just operate on game
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
            
                // gather simulants. Note that we do not re-discover simulants during the update process because the
                // engine is defined to discourage the removal of simulants in the middle of a frame.
                let layers = World.getLayers selectedScreen world
                let (entities, world) = World.getEntitiesInView selectedScreen world

                // update simulants
                let world = World.updateGame world
                let world = World.updateScreen selectedScreen world
                let world = World.updateScreenTransition selectedScreen world
                let world = Seq.fold (fun world layer -> World.updateLayer layer world) world layers
                let world = Seq.fold (fun world entity -> World.updateEntity entity world) world entities

                // post-update simulants
                let world = World.postUpdateGame world
                let world = World.postUpdateScreen selectedScreen world
                let world = Seq.fold (fun world layer -> World.postUpdateLayer layer world) world layers
                let world = Seq.fold (fun world entity -> World.postUpdateEntity entity world) world entities
                world
            
            // no screen; just operate on the game
            | None ->
               let world = World.updateGame world
               let world = World.postUpdateGame world
               world

        static member private actualizeScreenTransition (_ : Vector2) (eyeSize : Vector2) (screen : Screen) transition world =
            match transition.DissolveImageOpt with
            | Some dissolveImage ->
                let progress = single (screen.GetTransitionTicksNp world) / single transition.TransitionLifetime
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

        static member private actualizeSimulants world =
            let world = World.actualizeGame world
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                let world = World.actualizeScreen selectedScreen world
                let world =
                    match selectedScreen.GetTransitionStateNp world with
                    | IncomingState -> World.actualizeScreenTransition (World.getEyeCenter world) (World.getEyeSize world) selectedScreen (selectedScreen.GetIncoming world) world
                    | OutgoingState -> World.actualizeScreenTransition (World.getEyeCenter world) (World.getEyeSize world) selectedScreen (selectedScreen.GetOutgoing world) world
                    | IdlingState -> world
                let layers = World.getLayers selectedScreen world
                let world = Seq.fold (fun world layer -> World.actualizeLayer layer world) world layers
                let (entities, world) = World.getEntitiesInView selectedScreen world
                Seq.fold (fun world entity -> World.actualizeEntity entity world) world entities
            | None -> world

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

        /// Make an empty world.
        static member makeEmpty userState =

            // ensure game engine is initialized
            // TODO: P1: parameterize hard-coded boolean
            Nu.init false

            // make the world's event system
            let eventSystem =
                let eventTracer = Log.remark "Event"
                let eventTracing = Core.getEventTracing ()
                let eventFilter = Core.getEventFilter ()
                EventSystem.make eventTracer eventTracing eventFilter Simulants.Game

            // make the world's dispatchers
            let dispatchers =
                { GameDispatchers = World.makeDefaultGameDispatchers ()
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  LayerDispatchers = World.makeDefaultLayerDispatchers ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  Facets = World.makeDefaultFacets ()
                  IsExtrinsic = World.isExtrinsic
                  EvalExtrinsic = World.evalExtrinsic
                  UpdateEntityInEntityTree = World.updateEntityInEntityTree
                  RebuildEntityTree = World.rebuildEntityTree }

            // make the world's subsystems
            let subsystems =
                let subsystemMap =
                    UMap.makeFromSeq
                        Functional
                        [(Constants.Engine.PhysicsEngineSubsystemName, PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockPhysicsEngine.make ()) :> World Subsystem)
                         (Constants.Engine.RendererSubsystemName, RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockRenderer.make ()) :> World Subsystem)
                         (Constants.Engine.AudioPlayerSubsystemName, AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder (MockAudioPlayer.make ()) :> World Subsystem)]
                Subsystems.make subsystemMap

            // make the world's scripting environment
            let scriptingEnv = Scripting.EnvModule.Env.make ()

            // make the world's ambient state
            let ambientState =
                let overlayRoutes = World.dispatchersToOverlayRoutes dispatchers.EntityDispatchers
                let overlayRouter = OverlayRouter.make overlayRoutes
                AmbientState.make 1L (Metadata.makeEmpty ()) overlayRouter Overlayer.empty SymbolStore.empty userState

            // select the first game dispatcher as active
            let activeGameDispatcher = dispatchers.GameDispatchers |> Seq.head |> fun kvp -> kvp.Value

            // make the world
            let world = World.make eventSystem dispatchers subsystems scriptingEnv ambientState None activeGameDispatcher
            
            // subscribe to subscribe and unsubscribe events
            let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Subscribe Simulants.Game world
            let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Unsubscribe Simulants.Game world

            // finally, register the game
            World.registerGame world

        /// Make a default world with a default screen, layer, and entity, such as for testing.
        static member makeDefault () =
            let world = World.makeEmpty ()
            let world = World.createScreen None (Some Simulants.DefaultScreen.ScreenName) world |> snd
            let world = World.createLayer None (Some Simulants.DefaultLayer.LayerName) Simulants.DefaultScreen world |> snd
            let world = World.createEntity None (Some Simulants.DefaultEntity.EntityName) Simulants.DefaultLayer world |> snd
            world

        /// Try to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member attemptMake preferPluginGameDispatcher gameSpecializationOpt tickRate userState (plugin : NuPlugin) sdlDeps =

            // ensure game engine is initialized
            // TODO: P1: parameterize hard-coded boolean
            Nu.init false

            // attempt to create asset graph
            match AssetGraph.tryMakeFromFile Assets.AssetGraphFilePath with
            | Right assetGraph ->

                // make the world's event system
                let eventSystem =
                    let eventTracer = Log.remark "Event"
                    let eventTracing = Core.getEventTracing ()
                    let eventFilter = Core.getEventFilter ()
                    EventSystem.make eventTracer eventTracing eventFilter Simulants.Game

                // make plug-in dispatchers
                let pluginFacets = plugin.MakeFacets () |> List.map World.pairWithName
                let pluginEntityDispatchers = plugin.MakeEntityDispatchers () |> List.map World.pairWithName
                let pluginLayerDispatchers = plugin.MakeLayerDispatchers () |> List.map World.pairWithName
                let pluginScreenDispatchers = plugin.MakeScreenDispatchers () |> List.map World.pairWithName
                let pluginGameDispatcherOpt = plugin.MakeGameDispatcherOpt ()

                // infer the active game dispatcher
                let defaultGameDispatcher = GameDispatcher ()
                let activeGameDispatcher =
                    if preferPluginGameDispatcher then
                        match pluginGameDispatcherOpt with
                        | Some gameDispatcher -> gameDispatcher
                        | None -> defaultGameDispatcher
                    else defaultGameDispatcher

                // make the world's dispatchers
                let dispatchers =
                    { GameDispatchers = Map.addMany [World.pairWithName activeGameDispatcher] (World.makeDefaultGameDispatchers ())
                      ScreenDispatchers = Map.addMany pluginScreenDispatchers (World.makeDefaultScreenDispatchers ())
                      LayerDispatchers = Map.addMany pluginLayerDispatchers (World.makeDefaultLayerDispatchers ())
                      EntityDispatchers = Map.addMany pluginEntityDispatchers (World.makeDefaultEntityDispatchers ())
                      Facets = Map.addMany pluginFacets (World.makeDefaultFacets ())
                      IsExtrinsic = World.isExtrinsic
                      EvalExtrinsic = World.evalExtrinsic
                      UpdateEntityInEntityTree = World.updateEntityInEntityTree
                      RebuildEntityTree = World.rebuildEntityTree }

                // make the world's subsystems
                let subsystems =
                    let userSubsystems = plugin.MakeSubsystems ()
                    let physicsEngine = PhysicsEngine.make Constants.Physics.Gravity
                    let physicsEngineSubsystem = PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder physicsEngine :> World Subsystem
                    let renderer =
                        match SdlDeps.getRenderContextOpt sdlDeps with
                        | Some renderContext -> Renderer.make renderContext :> IRenderer
                        | None -> MockRenderer.make () :> IRenderer
                    let renderer = renderer.EnqueueMessage (HintRenderPackageUseMessage Assets.DefaultPackageName)
                    let rendererSubsystem = RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder renderer :> World Subsystem
                    let audioPlayer =
                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                        then AudioPlayer.make () :> IAudioPlayer
                        else MockAudioPlayer.make () :> IAudioPlayer
                    let audioPlayerSubsystem = AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder audioPlayer :> World Subsystem
                    let defaultSubsystemMap =
                        UMap.makeFromSeq
                            Functional
                            [(Constants.Engine.PhysicsEngineSubsystemName, physicsEngineSubsystem)
                             (Constants.Engine.RendererSubsystemName, rendererSubsystem)
                             (Constants.Engine.AudioPlayerSubsystemName, audioPlayerSubsystem)]
                    let subsystemMap = UMap.addMany userSubsystems defaultSubsystemMap
                    Subsystems.make subsystemMap

                // attempt to make the overlayer
                let intrinsicOverlays = World.createIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays Assets.OverlayerFilePath with
                | Right overlayer ->

                    // make the world's scripting environment
                    let scriptingEnv = Scripting.EnvModule.Env.make ()
            
                    // make the world's ambient state
                    let ambientState =
                        let assetMetadataMap = Metadata.make assetGraph
                        let intrinsicOverlayRoutes = World.dispatchersToOverlayRoutes dispatchers.EntityDispatchers
                        let userOverlayRoutes = plugin.MakeOverlayRoutes ()
                        let overlayRoutes = intrinsicOverlayRoutes @ userOverlayRoutes
                        let overlayRouter = OverlayRouter.make overlayRoutes
                        AmbientState.make tickRate assetMetadataMap overlayRouter overlayer SymbolStore.empty userState

                    // make the world
                    let world = World.make eventSystem dispatchers subsystems scriptingEnv ambientState gameSpecializationOpt activeGameDispatcher

                    // subscribe to subscribe and unsubscribe events
                    let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Subscribe Simulants.Game world
                    let world = World.subscribe World.handleSubscribeAndUnsubscribe Events.Unsubscribe Simulants.Game world

                    // try to load the prelude for the scripting language
                    match World.tryEvalPrelude world with
                    | Right struct (_, world) ->
                        
                        // finally, register the game
                        let world = World.registerGame world
                        Right world

                    // forward error messages
                    | Left struct (error, _) -> Left error
                | Left error -> Left error
            | Left error -> Left error