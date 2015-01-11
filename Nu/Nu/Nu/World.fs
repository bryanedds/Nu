// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open System.IO
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
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
    let private SplashScreenTickKey = World.makeSubscriptionKey ()
    let private LoadedAssemblies = Dictionary<string, Assembly> ()

    type World with

        /// Try to get a simulant at the given address.
        static member getOptSimulant<'a when 'a :> Simulant> (address : 'a Address) world =
            match address.Names with
            | [] -> World.getGame world :> Simulant :?> 'a |> Some
            | [_] -> World.getOptScreen (atosa address) world |> Option.map (fun s -> s :> Simulant :?> 'a)
            | [_; _] -> World.getOptGroup (atoga address) world |> Option.map (fun g -> g :> Simulant :?> 'a)
            | [_; _; _] -> World.getOptEntity (atoea address) world |> Option.map (fun e -> e :> Simulant :?> 'a)
            | _ -> failwith <| "Invalid simulant address '" + acstring address + "'."

        /// Query that the world contains a simulant at the given address.
        static member containsSimulant address world =
            Option.isSome <| World.getOptSimulant address world

        /// Get a simulant at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getSimulantBy by address world =
            by ^^ Option.get ^^ World.getOptSimulant address world

        /// Get a simulant at the given address (failing with an exception otherwise).
        static member getSimulant address world =
            World.getSimulantBy id address world

        /// Set a simulant at the given address (failing with an exception if one doesn't exist).
        static member setSimulant<'a when 'a :> Simulant> (simulant : 'a) (address : 'a Address) world =
            match address.Names with
            | [] -> World.setGame (simulant :> obj :?> Game) world
            | [_] -> World.setScreen (simulant :> obj :?> Screen) (Address.changeType<'a, Screen> address) world
            | [_; _] -> World.setGroup (simulant :> obj :?> Group) (Address.changeType<'a, Group> address) world
            | [_; _; _] -> World.setEntity (simulant :> obj :?> Entity) (Address.changeType<'a, Entity> address) world
            | _ -> failwith <| "Invalid simulant address '" + acstring address + "'."

        /// Update a simulant with the given 'updater' procedure at the given address. Also passes
        /// the current world value to the procedure.
        static member updateSimulantW updater address world =
            let simulant = World.getSimulant address world
            let simulant = updater simulant world
            World.setSimulant simulant address world

        /// Update a simulant with the given 'updater' procedure at the given address.
        static member updateSimulant updater address world =
            World.updateSimulantW (fun simulant _ -> updater simulant) address world

        /// Update the world with the given 'updater' procedure that uses the simulant at given
        /// address in its computation.
        static member updateBySimulant updater address world : World =
            let simulant = World.getSimulant address world
            updater simulant world

        static member private getOptSimulantForPublishingDefinition (address : Simulant Address) world =
            World.getOptSimulant address world

        /// Try to query that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        static member tryGetIsSelectedScreenIdling world =
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> Some <| Screen.isIdling selectedScreen
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

        static member private setScreenState state address screen world =
            let screen = Screen.setScreenStateNp state screen
            let world =
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
                        World.subscribe ScreenTransitionMouseLeftKey World.handleAsSwallow (MouseLeftEventAddress ->- AnyEventAddress) GameAddress |>
                        World.subscribe ScreenTransitionMouseCenterKey World.handleAsSwallow (MouseCenterEventAddress ->- AnyEventAddress) GameAddress |>
                        World.subscribe ScreenTransitionMouseRightKey World.handleAsSwallow (MouseRightEventAddress ->- AnyEventAddress) GameAddress |>
                        World.subscribe ScreenTransitionMouseX1Key World.handleAsSwallow (MouseX1EventAddress ->- AnyEventAddress) GameAddress |>
                        World.subscribe ScreenTransitionMouseX2Key World.handleAsSwallow (MouseX2EventAddress ->- AnyEventAddress) GameAddress |>
                        World.subscribe ScreenTransitionKeyboardKeyKey World.handleAsSwallow (KeyboardKeyEventAddress ->- AnyEventAddress) GameAddress 
            let world = World.setScreen screen address world
            (screen, world)

        /// Select the given screen without transitioning.
        static member selectScreen screen screenAddress world =
            let world =
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->  World.publish4 () (DeselectEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                | None -> world
            let (screen, world) = World.setScreenState IncomingState screenAddress screen world
            let world = World.setOptSelectedScreenAddress (Some screenAddress) world
            let world = World.publish4 () (SelectEventAddress ->>- screenAddress) screenAddress world
            (screen, world)

        /// Try to transition to the screen at the destination address if no other transition is in
        /// progress.
        static member tryTransitionScreen destinationAddress world =
            match World.getOptSelectedScreenAddress world with
            | Some selectedScreenAddress ->
                match World.getOptScreen selectedScreenAddress world with
                | Some selectedScreen ->
                    let subscriptionKey = World.makeSubscriptionKey ()
                    let subscription = fun (_ : Event<unit, Screen>) world ->
                        match world.State.OptScreenTransitionDestinationAddress with
                        | Some address ->
                            let destinationScreen = World.getScreen destinationAddress world
                            let world = World.unsubscribe subscriptionKey world
                            let world = World.setOptScreenTransitionDestinationAddress None world
                            let world = snd <| World.selectScreen destinationScreen address world
                            (Cascade, world)
                        | None -> failwith "No valid OptScreenTransitionDestinationAddress during screen transition!"
                    let world = World.setOptScreenTransitionDestinationAddress (Some destinationAddress) world
                    let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                    let world = World.subscribe<unit, Screen> subscriptionKey subscription (OutgoingFinishEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                    Some world
                | None -> None
            | None -> None

        /// Transition to the screen at the destination address (failing with an exception if
        /// another transition is in progress).
        static member transitionScreen destinationAddress world =
            Option.get <| World.tryTransitionScreen destinationAddress world
            
        // TODO: replace this with more sophisticated use of handleAsScreenTransition4, and so on for its brethren.
        static member private handleAsScreenTransitionFromSplash4<'a, 's when 's :> Simulant> eventHandling destinationAddress (_ : Event<'a, 's>) world =
            let destinationScreen = World.getScreen destinationAddress world
            let world = snd <| World.selectScreen destinationScreen destinationAddress world
            (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the screen at the given destination address.
        static member handleAsScreenTransitionFromSplash<'a, 's when 's :> Simulant> destinationAddress event world =
            World.handleAsScreenTransitionFromSplash4<'a, 's> Cascade destinationAddress event world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the screen at the given destination address, as well as
        /// with additional provided via the 'by' procedure.
        static member handleAsScreenTransitionFromSplashBy<'a, 's when 's :> Simulant> by destinationAddress event  (world : World) =
            let (eventHandling, world) = by event world
            World.handleAsScreenTransitionFromSplash4<'a, 's> eventHandling destinationAddress event world

        static member private handleAsScreenTransition4<'a, 's when 's :> Simulant> eventHandling destinationAddress (_ : Event<'a, 's>) world =
            match World.tryTransitionScreen destinationAddress world with
            | Some world -> (eventHandling, world)
            | None ->
                trace <| "Program Error: Invalid screen transition for destination address '" + acstring destinationAddress + "'."
                (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the screen at the given destination address.
        static member handleAsScreenTransition<'a, 's when 's :> Simulant> destinationAddress event world =
            World.handleAsScreenTransition4<'a, 's> Cascade destinationAddress event world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the screen at the given destination address, as well as
        /// with additional provided via the 'by' procedure.
        static member handleAsScreenTransitionBy<'a, 's when 's :> Simulant> by destinationAddress event (world : World) =
            let (eventHandling, world) = by event world
            World.handleAsScreenTransition4<'a, 's> eventHandling destinationAddress event world

        static member private updateScreenTransition1 screen transition =
            if screen.TransitionTicksNp = transition.TransitionLifetime then (true, { screen with TransitionTicksNp = 0L })
            else (false, { screen with TransitionTicksNp = screen.TransitionTicksNp + 1L })

        static member private updateScreenTransition world =
            // TODO: split this function up...
            match World.getOptSelectedScreenAddress world with
            | Some selectedScreenAddress ->
                let selectedScreen = World.getScreen selectedScreenAddress world
                match selectedScreen.ScreenStateNp with
                | IncomingState ->
                    match world.State.Liveness with
                    | Running ->
                        let world =
                            if selectedScreen.TransitionTicksNp = 0L
                            then World.publish4 () (IncomingStartEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                            else world
                        match world.State.Liveness with
                        | Running ->
                            let (finished, selectedScreen) = World.updateScreenTransition1 selectedScreen selectedScreen.Incoming
                            let world = World.setScreen selectedScreen selectedScreenAddress world
                            if finished then
                                let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                                World.publish4 () (IncomingFinishEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                            else world
                        | Exiting -> world
                    | Exiting -> world
                | OutgoingState ->
                    let world =
                        if selectedScreen.TransitionTicksNp <> 0L then world
                        else World.publish4 () (OutgoingStartEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                    match world.State.Liveness with
                    | Running ->
                        let (finished, selectedScreen) = World.updateScreenTransition1 selectedScreen selectedScreen.Outgoing
                        let world = World.setScreen selectedScreen selectedScreenAddress world
                        if finished then
                            let world = snd <| World.setScreenState IdlingState selectedScreenAddress selectedScreen world
                            match world.State.Liveness with
                            | Running -> World.publish4 () (OutgoingFinishEventAddress ->>- selectedScreenAddress) selectedScreenAddress world
                            | Exiting -> world
                        else world
                    | Exiting -> world
                | IdlingState -> world
            | None -> world

        static member private handleSplashScreenIdleTick idlingTime ticks event world =
            let world = World.unsubscribe SplashScreenTickKey world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleTick idlingTime (inc ticks)
                let world = World.subscribe SplashScreenTickKey subscription event.EventAddress event.SubscriberAddress world
                (Cascade, world)
            else
                match World.getOptSelectedScreenAddress world with
                | Some selectedScreenAddress ->
                    match World.getOptScreen selectedScreenAddress world with
                    | Some selectedScreen ->
                        let world = snd <| World.setScreenState OutgoingState selectedScreenAddress selectedScreen world
                        (Cascade, world)
                    | None ->
                        trace "Program Error: Could not handle splash screen tick due to no selected screen."
                        (Resolve, World.exit world)
                | None ->
                    trace "Program Error: Could not handle splash screen tick due to no selected screen."
                    (Resolve, World.exit world)

        static member private handleSplashScreenIdle idlingTime event world =
            let world = World.subscribe SplashScreenTickKey (World.handleSplashScreenIdleTick idlingTime 0L) TickEventAddress event.SubscriberAddress world
            (Resolve, world)

        /// Add a splash screen to the world at the given address that transitions to the given
        /// destination upon completion.
        static member addSplashScreen persistent splashData dispatcherName destination address world =
            let splashScreen = { World.makeDissolveScreen splashData.DissolveData dispatcherName (Some <| Address.head address) world with Persistent = persistent }
            let splashGroup = { World.makeGroup typeof<GroupDispatcher>.Name (Some "SplashGroup") world with Persistent = persistent }
            let splashLabel = { World.makeEntity typeof<LabelDispatcher>.Name (Some "SplashLabel") world with Persistent = persistent }
            let splashLabel = Entity.setSize world.State.Camera.EyeSize splashLabel
            let splashLabel = Entity.setPosition (-world.State.Camera.EyeSize * 0.5f) splashLabel
            let splashLabel = Entity.setLabelImage splashData.SplashImage splashLabel
            let splashGroupHierarchies = Map.singleton splashGroup.Name (splashGroup, Map.singleton splashLabel.Name splashLabel)
            let splashScreenHierarchy = (splashScreen, splashGroupHierarchies)
            let world = snd <| World.addScreen splashScreenHierarchy address world
            let world = World.monitor (World.handleSplashScreenIdle splashData.IdlingTime) (IncomingFinishEventAddress ->>- address) address world
            let world = World.monitor (World.handleAsScreenTransitionFromSplash destination) (OutgoingFinishEventAddress ->>- address) address world
            (splashScreen, world)

        /// Add a screen to the world at the given address that uses a dissolve transition.
        static member addDissolveScreen persistent dissolveData dispatcherName address world =
            let dissolveScreen = { World.makeDissolveScreen dissolveData dispatcherName (Some <| Address.head address) world with Persistent = persistent }
            let dissolveScreenHierarchy = (dissolveScreen, Map.empty)
            World.addScreen dissolveScreenHierarchy address world

        /// Add a dissolve screen to the world at the given address whose contents is loaded from
        /// the given group file.
        static member addDissolveScreenFromGroupFile persistent dissolveData dispatcherName groupFilePath address world =
            let dissolveScreen = { World.makeDissolveScreen dissolveData dispatcherName (Some <| Address.head address) world with Persistent = persistent }
            let (group, entities) = World.readGroupHierarchyFromFile groupFilePath world
            let dissolveGroupHierarchies = Map.singleton group.Name (group, entities)
            let dissolveScreenHierarchy = (dissolveScreen, dissolveGroupHierarchies)
            World.addScreen dissolveScreenHierarchy address world

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

                // get all the entities in the world
                let entities =
                    [for screenKvp in snd world.Simulants do
                        for groupKvp in snd screenKvp.Value do
                            for entityKvp in snd groupKvp.Value do
                                let address = Address<Entity>.make [screenKvp.Key; groupKvp.Key; entityKvp.Key]
                                yield (address, entityKvp.Value)]

                // apply overlays to all entities
                let world =
                    Seq.fold
                        (fun world (address, entity : Entity) ->
                            let entity = { entity with Id = entity.Id } // hacky copy
                            match entity.OptOverlayName with
                            | Some overlayName ->
                                let oldFacetNames = entity.FacetNames
                                Overlayer.applyOverlayToFacetNames overlayName overlayName entity oldOverlayer world.State.Overlayer
                                match World.trySynchronizeFacets oldFacetNames entity (Some address) world with
                                | Right (entity, world) ->
                                    let facetNames = Entity.getFacetNamesReflectively entity
                                    Overlayer.applyOverlay6 overlayName overlayName facetNames entity oldOverlayer world.State.Overlayer
                                    World.setEntity entity address world
                                | Left error -> note <| "There was an issue in applying a reloaded overlay: " + error; world
                            | None -> world)
                        world
                        entities

                // right!
                Right world

            // propagate error
            with exn -> Left <| acstring exn

        /// Try to release the assets in use by the world. Currently does not support reloading
        /// of song assets, and possible others.
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
                    | Left errorMsg -> Left errorMsg
                | Left error -> Left error
            with exn -> Left <| acstring exn

        /// A hack for the physics subsystem that allows an old world value to displace the current
        /// one and have its physics values propagated to the imperative physics subsystem.
        static member continueHack groupAddress world =
            // NOTE: since messages may be invalid upon continuing a world (especially physics
            // messages), all messages are eliminated. If this poses an issue, the editor will have
            // to instead store past / future worlds only once their current frame has been
            // processed (integrated, advanced, rendered, played, et al).
            let world = World.clearRenderMessages world
            let world = World.clearAudioMessages world
            let world = World.clearPhysicsMessages world
            let world = World.addPhysicsMessage RebuildPhysicsHackMessage world
            let entityMap = World.getEntityMapInGroup groupAddress world
            Map.fold
                (fun world _ (entity : Entity) ->
                    let entityAddress = gatoea groupAddress entity.Name
                    Entity.propagatePhysics entity entityAddress world)
                world
                entityMap

        static member private processSubsystems subsystemType world =
            Map.toList world.Subsystems |>
            List.filter (fun (_, subsystem) -> subsystem.SubsystemType = subsystemType) |>
            List.sortBy (fun (_, subsystem) -> -subsystem.SubsystemPriority) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystemResult, subsystem) = subsystem.ProcessMessages world
                let world = subsystem.ApplyResult (subsystemResult, world)
                World.setSubsystem subsystem subsystemName world)
                world

        static member private cleanUpSubsystems world =
            Map.toList world.Subsystems |>
            List.sortBy (fun (_, subsystem) -> -subsystem.SubsystemPriority) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystem, world) = subsystem.CleanUp world
                World.setSubsystem subsystem subsystemName world)
                world

        static member private processTask (tasksNotRun, world) task =
            if task.ScheduledTime < world.State.TickTime then
                debug <| "Task leak found for time '" + acstring world.State.TickTime + "'."
                (tasksNotRun, world)
            elif task.ScheduledTime = world.State.TickTime then
                let world = task.Operation world
                (tasksNotRun, world)
            else (task :: tasksNotRun, world)

        static member private processTasks world =
            let tasks = List.rev world.Callbacks.Tasks
            let world = World.clearTasks world
            let (tasksNotRun, world) = List.fold World.processTask ([], world) tasks
            let tasksNotRun = List.rev tasksNotRun
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
                        then World.publish World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } MouseDragEventAddress GameAddress world
                        else world
                    World.publish World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } MouseMoveEventAddress GameAddress world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseButtonEventAddress = ntoa <| MouseButton.toEventName mouseButton
                    let mouseButtonDownEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Down"
                    let mouseButtonChangeEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let world = World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonDownEventAddress GameAddress world
                    World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress GameAddress world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton <| uint32 event.button.button
                    let mouseButtonEventAddress = ntoa <| MouseButton.toEventName mouseButton
                    let mouseButtonUpEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Up"
                    let mouseButtonChangeEventAddress = MouseEventAddress -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> "Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let world = World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonUpEventAddress GameAddress world
                    World.publish World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress GameAddress world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let world = World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyDownEventAddress GameAddress world
                    World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyChangeEventAddress GameAddress world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = event.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let world = World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyUpEventAddress GameAddress world
                    World.publish World.sortSubscriptionsByHierarchy eventData KeyboardKeyChangeEventAddress GameAddress world
                | _ -> world
            (world.State.Liveness, world)

        /// Update the Nu game engine once per frame, updating its subsystems and publishing the
        /// game tick event.
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
                        let world = World.publish4 () TickEventAddress GameAddress world
                        match world.State.Liveness with
                        | Running ->
                            let world = World.processTasks world
                            (world.State.Liveness, world)
                        | Exiting -> (Exiting, world)
                    | Exiting -> (Exiting, world)
                | Exiting -> (Exiting, world)
            | Exiting -> (Exiting, world)

        static member processRender handleRender world =
            let world = World.processSubsystems RenderType world
            handleRender world

        static member processPlay world =
            let world = World.processSubsystems AudioType world
            World.incrementTickTime world

        static member run4 tryMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                tryMakeWorld
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.cleanUpSubsystems
                sdlConfig

        static member run tryMakeWorld handleUpdate sdlConfig =
            World.run4 tryMakeWorld handleUpdate id sdlConfig

        static member private pairWithName source =
            (Reflection.getTypeName source, source)

        static member private pairWithNames sources =
            Map.ofListBy World.pairWithName sources

        /// Try to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake farseerCautionMode useLoadedGameDispatcher interactivity userState (nuPlugin : NuPlugin) sdlDeps =

            // attempt to generate asset metadata so the rest of the world can be created
            match Metadata.tryGenerateAssetMetadataMap AssetGraphFilePath with
            | Right assetMetadataMap ->

                // make the world's subsystems
                let subsystems =
                    let userSubsystems = Map.ofList <| nuPlugin.MakeSubsystems ()
                    let integrator = Integrator.make farseerCautionMode Gravity
                    let integratorSubsystem = { SubsystemType = UpdateType; SubsystemPriority = 1.0f; Integrator = integrator } :> Subsystem
                    let renderer = Renderer.make sdlDeps.RenderContext AssetGraphFilePath
                    let renderer = renderer.EnqueueMessage <| HintRenderPackageUseMessage { PackageName = DefaultPackageName }
                    let rendererSubsystem = { SubsystemType = RenderType; SubsystemPriority = 1.0f; Renderer = renderer } :> Subsystem
                    let audioPlayer = AudioPlayer.make AssetGraphFilePath
                    let audioPlayerSubsystem = { SubsystemType = AudioType; SubsystemPriority = 1.0f; AudioPlayer = audioPlayer } :> Subsystem
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
                    { Tasks = []
                      Subscriptions = Map.empty
                      Unsubscriptions = Map.empty
                      CallbackStates = Map.empty }

                // make the world's state
                let state =
                    let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH)
                    let camera = { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                    let intrinsicOverlays = World.createIntrinsicOverlays entityDispatchers facets
                    let userOverlayRoutes = nuPlugin.MakeOverlayRoutes ()
                    { TickTime = 0L
                      Liveness = Running
                      Interactivity = interactivity
                      OptScreenTransitionDestinationAddress = None
                      Camera = camera
                      AssetMetadataMap = assetMetadataMap
                      AssetGraphFilePath = AssetGraphFilePath
                      Overlayer = Overlayer.make OverlayFilePath intrinsicOverlays
                      OverlayRouter = OverlayRouter.make entityDispatchers userOverlayRoutes
                      OverlayFilePath = OverlayFilePath
                      UserState = userState }

                // make the game
                let game = World.makeGame activeGameDispatcher

                // make the world itself
                let world =
                    { Simulants = (game, Map.empty)
                      Subsystems = subsystems
                      Components = components
                      Callbacks = callbacks
                      State = state }

                // and finally, register the game
                let world = snd <| Game.register (World.getGame world) world
                Right world
            | Left errorMsg -> Left errorMsg

        /// Make an empty world. Useful for unit-testing.
        static member makeEmpty (userState : 'u) =

            // make the world's subsystems
            let subsystems =
                let integratorSubsystem = { SubsystemType = UpdateType; SubsystemPriority = 1.0f; Integrator = { MockIntegrator = () }} :> Subsystem
                let rendererSubsystem = { SubsystemType = RenderType; SubsystemPriority = 1.0f; Renderer = { MockRenderer = () }} :> Subsystem
                let audioPlayerSubsystem = { SubsystemType = AudioType; SubsystemPriority = 1.0f; AudioPlayer = { MockAudioPlayer = () }} :> Subsystem
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
                { Tasks = []
                  Subscriptions = Map.empty
                  Unsubscriptions = Map.empty
                  CallbackStates = Map.empty }

            // make the world's state
            let state =
                { TickTime = 0L
                  Liveness = Running
                  Interactivity = GuiOnly
                  OptScreenTransitionDestinationAddress = None
                  Camera = { EyeCenter = Vector2.Zero; EyeSize = Vector2 (single ResolutionXDefault, single ResolutionYDefault) }
                  AssetMetadataMap = Map.empty
                  AssetGraphFilePath = String.Empty
                  OverlayRouter = OverlayRouter.make (Map.ofList [World.pairWithName entityDispatcher]) []
                  OverlayFilePath = String.Empty
                  Overlayer = { Overlays = XmlDocument () }
                  UserState = userState }

            // make the game
            let game = World.makeGame gameDispatcher

            // make the world itself
            let world =
                { Simulants = (game, Map.empty)
                  Subsystems = subsystems
                  Components = components
                  Callbacks = callbacks
                  State = state }

            // and finally, register the game
            snd <| Game.register (World.getGame world) world

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

            // assign this crazy function
            World.getOptSimulantForPublishing <- World.getOptSimulantForPublishingDefinition

        /// Initialize the Nu game engine, and make an empty world. Useful for unit-testing.
        static member initAndMakeEmpty (userState : 'u) =
            World.init ()
            World.makeEmpty userState

        /// Initialize the Nu game engine and try to make the world, returning either a Right
        //// World on success, or a Left string (with an error message) on failure.
        static member initAndTryMake farseerCautionMode useLoadedGameDispatcher interactivity userState nuPlugin sdlDeps =
            World.init ()
            World.tryMake farseerCautionMode useLoadedGameDispatcher interactivity userState nuPlugin sdlDeps