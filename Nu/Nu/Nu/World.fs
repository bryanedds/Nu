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

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Nu =

    let mutable private Initialized = false
    let private LoadedAssemblies = Dictionary<string, Assembly> HashIdentity.Structural

    /// Initialize the game engine.
    let init () =

        // init only if needed
        if not Initialized then

            // make types load reflectively from pathed (non-static) assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add
                (fun args -> LoadedAssemblies.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve ^ ResolveEventHandler
                (fun _ args -> snd ^ LoadedAssemblies.TryGetValue args.Name)

            // ensure the current culture is invariate
            System.Threading.Thread.CurrentThread.CurrentCulture <-
                System.Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init ^ Some "Log.txt"

            // initialize math module
            Math.init ()

            // init F# reach-arounds
            // TODO: define this somewhere else...
            World.rebuildEntityTree <- fun screen world ->
                let tree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
                let entities = screen |> flip World.proxyGroups world |> Seq.map (flip World.proxyEntities world) |> Seq.concat
                for entity in entities do
                    let entityMaxBounds = World.getEntityBoundsMax entity world
                    QuadTree.addElement (entity.GetOmnipresent world || entity.GetViewType world = Absolute) entityMaxBounds entity tree
                tree

            // mark init flag
            Initialized <- true

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module WorldModule =

    let private ScreenTransitionMouseLeftKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseCenterKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseRightKey = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseX1Key = World.makeSubscriptionKey ()
    let private ScreenTransitionMouseX2Key = World.makeSubscriptionKey ()
    let private ScreenTransitionKeyboardKeyKey = World.makeSubscriptionKey ()
    let private SplashScreenUpdateKey = World.makeSubscriptionKey ()

    type World with

        static member private pairWithName source =
            (Reflection.getTypeName source, source)

        static member private makeDefaultFacets () =
            Map.ofList
                [(typeof<MountFacet>.Name, MountFacet () :> Facet)
                 (typeof<RigidBodyFacet>.Name, RigidBodyFacet () :> Facet)
                 (typeof<StaticSpriteFacet>.Name, StaticSpriteFacet () :> Facet)
                 (typeof<AnimatedSpriteFacet>.Name, AnimatedSpriteFacet () :> Facet)]

        static member private makeDefaultEntityDispatchers () =
            // TODO: see if we can reflectively generate these
            Map.ofListBy World.pairWithName ^
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

        static member private makeDefaultGroupDispatchers () =
            Map.ofList [World.pairWithName ^ GroupDispatcher ()]
            
        static member private makeDefaultScreenDispatchers () =
            Map.ofList [World.pairWithName ^ ScreenDispatcher ()]

        static member private makeDefaultGameDispatchers () =
            Map.ofList [World.pairWithName ^ GameDispatcher ()]

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
            | Some selectedScreen -> Some ^ selectedScreen.IsIdling world
            | None -> None

        /// Query that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen (failing with an exception if no screen is selected).
        static member isSelectedScreenIdling world =
            match World.tryGetIsSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith ^ "Cannot query state of non-existent selected screen."

        /// Try to query that the selected screen is transitioning.
        static member tryGetIsSelectedScreenTransitioning world =
            Option.map not ^ World.tryGetIsSelectedScreenIdling world

        /// Query that the selected screen is transitioning (failing with an exception if no screen
        /// is selected).
        static member isSelectedScreenTransitioning world =
            not ^ World.isSelectedScreenIdling world

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
                    World.subscribe5 ScreenTransitionMouseLeftKey World.handleAsSwallow (Events.MouseLeft ->- Events.Any) Simulants.Game |>
                    World.subscribe5 ScreenTransitionMouseCenterKey World.handleAsSwallow (Events.MouseCenter ->- Events.Any) Simulants.Game |>
                    World.subscribe5 ScreenTransitionMouseRightKey World.handleAsSwallow (Events.MouseRight ->- Events.Any) Simulants.Game |>
                    World.subscribe5 ScreenTransitionMouseX1Key World.handleAsSwallow (Events.MouseX1 ->- Events.Any) Simulants.Game |>
                    World.subscribe5 ScreenTransitionMouseX2Key World.handleAsSwallow (Events.MouseX2 ->- Events.Any) Simulants.Game |>
                    World.subscribe5 ScreenTransitionKeyboardKeyKey World.handleAsSwallow (Events.KeyboardKey ->- Events.Any) Simulants.Game

        /// Select the given screen without transitioning.
        static member selectScreen screen world =
            let world =
                match World.getOptSelectedScreen world with
                | Some selectedScreen ->  World.publish () (Events.Deselect ->- selectedScreen) ["World.selectScreen"] selectedScreen world
                | None -> world
            let world = World.setScreenTransitionState IncomingState screen world
            let world = World.setSelectedScreen screen world
            World.publish () (Events.Select ->- screen) ["World.selectScreen"] screen world

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
                        | None -> failwith "No valid OptScreenTransitionDestination during screen transition!"
                    let world = World.setOptScreenTransitionDestination (Some destination) world
                    let world = World.setScreenTransitionState OutgoingState selectedScreen world
                    let world = World.subscribe5<unit, Screen> subscriptionKey subscription (Events.OutgoingFinish ->- selectedScreen) selectedScreen world
                    Some world
                else None
            | None -> None

        /// Transition to the given screen (failing with an exception if another transition is
        /// in progress).
        static member transitionScreen destinationAddress world =
            Option.get ^ World.tryTransitionScreen destinationAddress world
            
        // TODO: replace this with more sophisticated use of handleAsScreenTransition4, and so on for its brethren.
        static member private handleAsScreenTransitionFromSplash4<'a, 's when 's :> Simulant> eventHandling destination (_ : Event<'a, 's>) world =
            let world = World.selectScreen destination world
            (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransitionFromSplash<'a, 's when 's :> Simulant> destination evt world =
            World.handleAsScreenTransitionFromSplash4<'a, 's> Cascade destination evt world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen, as well as with additional
        /// handling provided via the 'by' procedure.
        static member handleAsScreenTransitionFromSplashBy<'a, 's when 's :> Simulant> by destination evt (world : World) =
            let (eventHandling, world) = by evt world
            World.handleAsScreenTransitionFromSplash4<'a, 's> eventHandling destination evt world

        static member private handleAsScreenTransition4<'a, 's when 's :> Simulant>
            eventHandling destination (_ : Event<'a, 's>) world =
            match World.tryTransitionScreen destination world with
            | Some world -> (eventHandling, world)
            | None ->
                trace ^ "Program Error: Invalid screen transition for destination address '" + acstring destination.ScreenAddress + "'."
                (eventHandling, world)

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen.
        static member handleAsScreenTransition<'a, 's when 's :> Simulant> destination evt world =
            World.handleAsScreenTransition4<'a, 's> Cascade destination evt world

        /// A procedure that can be passed to an event handler to specify that an event is to
        /// result in a transition to the given destination screen, as well as with additional
        /// handling provided via the 'by' procedure.
        static member handleAsScreenTransitionBy<'a, 's when 's :> Simulant> by destination evt (world : World) =
            let (eventHandling, world) = by evt world
            World.handleAsScreenTransition4<'a, 's> eventHandling destination evt world

        static member private updateScreenTransition1 (screen : Screen) transition world =
            let transitionTicks = screen.GetTransitionTicksNp world
            if transitionTicks = transition.TransitionLifetime then
                (true, screen.SetTransitionTicksNp 0L world)
            elif transitionTicks > transition.TransitionLifetime then
                debug ^ "TransitionLifetime for screen '" + acstring screen.ScreenAddress + "' must be a consistent multiple of TickRate."
                (true, screen.SetTransitionTicksNp 0L world)
            else (false, screen.SetTransitionTicksNp (transitionTicks + World.getTickRate world) world)

        static member private updateScreenTransition (selectedScreen : Screen) world =
            // TODO: split this function up...
            match selectedScreen.GetTransitionStateNp world with
            | IncomingState ->
                match world.State.Liveness with
                | Running ->
                    let world =
                        if selectedScreen.GetTransitionTicksNp world = 0L
                        then World.publish () (Events.IncomingStart ->- selectedScreen) ["World.updateScreenTransition"] selectedScreen world
                        else world
                    match world.State.Liveness with
                    | Running ->
                        let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetIncoming world) world
                        if finished then
                            let world = World.setScreenTransitionState IdlingState selectedScreen world
                            World.publish () (Events.IncomingFinish ->- selectedScreen) ["World.updateScreenTransition"] selectedScreen world
                        else world
                    | Exiting -> world
                | Exiting -> world
            | OutgoingState ->
                let world =
                    if selectedScreen.GetTransitionTicksNp world <> 0L then world
                    else World.publish () (Events.OutgoingStart ->- selectedScreen) ["World.updateScreenTransition"] selectedScreen world
                match world.State.Liveness with
                | Running ->
                    let (finished, world) = World.updateScreenTransition1 selectedScreen (selectedScreen.GetOutgoing world) world
                    if finished then
                        let world = World.setScreenTransitionState IdlingState selectedScreen world
                        match world.State.Liveness with
                        | Running -> World.publish () (Events.OutgoingFinish ->- selectedScreen) ["World.updateScreenTransition"] selectedScreen world
                        | Exiting -> world
                    else world
                | Exiting -> world
            | IdlingState -> world

        static member private handleSplashScreenIdleUpdate idlingTime ticks evt world =
            let world = World.unsubscribe SplashScreenUpdateKey world
            if ticks < idlingTime then
                let subscription = World.handleSplashScreenIdleUpdate idlingTime (inc ticks)
                let world = World.subscribe5 SplashScreenUpdateKey subscription evt.Address evt.Subscriber world
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

        static member private handleSplashScreenIdle idlingTime (splashScreen : Screen) evt world =
            let world = World.subscribe5 SplashScreenUpdateKey (World.handleSplashScreenIdleUpdate idlingTime 0L) (Events.Update ->- splashScreen) evt.Subscriber world
            (Resolve, world)

        /// Create a splash screen that transitions to the given destination upon completion.
        static member createSplashScreen persistent splashData destination dispatcherName optSpecialization optName world =
            let cameraEyeSize = World.getCameraBy (fun camera -> camera.EyeSize) world
            let (splashScreen, world) = World.createDissolveScreen splashData.DissolveData dispatcherName optSpecialization optName world
            let (splashGroup, world) = World.createGroup typeof<GroupDispatcher>.Name None (Some !!"SplashGroup") splashScreen world
            let (splashLabel, world) = World.createEntity typeof<LabelDispatcher>.Name None (Some !!"SplashLabel") splashGroup world
            let world = splashScreen.SetPersistent persistent world
            let world = splashGroup.SetPersistent persistent world
            let world = splashLabel.SetPersistent persistent world
            let world = splashLabel.SetSize cameraEyeSize world
            let world = splashLabel.SetPosition (-cameraEyeSize * 0.5f) world
            let world = splashLabel.SetLabelImage splashData.SplashImage world
            let world = World.monitor (World.handleSplashScreenIdle splashData.IdlingTime splashScreen) (Events.IncomingFinish ->- splashScreen) splashScreen world
            let world = World.monitor (World.handleAsScreenTransitionFromSplash destination) (Events.OutgoingFinish ->- splashScreen) splashScreen world
            (splashScreen, world)

        /// Create a dissolve screen whose contents is loaded from the given group file.
        static member createDissolveScreenFromGroupFile persistent dissolveData groupFilePath dispatcherName optSpecialization optName world =
            let (dissolveScreen, world) = World.createDissolveScreen dissolveData dispatcherName optSpecialization optName world
            let world = dissolveScreen.SetPersistent persistent world
            let world = World.readGroupFromFile groupFilePath None dissolveScreen world |> snd
            (dissolveScreen, world)

        static member private createIntrinsicOverlays facets entityDispatchers =
            let hasFacetNamesField = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = Map.toValueListBy (fun facet -> facet :> obj) facets
            let entityDispatchers = Map.toValueListBy objectify entityDispatchers
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
                                    let oldWorld = world
                                    let facetNames = World.getEntityFacetNamesReflectively entityState // hacky copy elided
                                    Overlayer.applyOverlay6 overlayName overlayName facetNames entityState oldOverlayer world.State.Overlayer
                                    let world = World.setEntityStateWithoutEvent entityState entity world
                                    World.updateEntityInEntityTree entity oldWorld world
                                | Left error -> note ^ "There was an issue in applying a reloaded overlay: " + error; world
                            | None -> world)
                        world
                        entities

                // right!
                Right world

            // propagate error
            with exn -> Left ^ acstring exn

        /// Try to release the assets in use by the world. Currently does not support reloading
        /// of song assets, and possibly others that are locked by the engine's subsystems.
        static member tryReloadAssets inputDirectory outputDirectory refinementDirectory world =
            
            // try to reload asset graph file
            try File.Copy
                    (Path.Combine (inputDirectory, world.State.AssetGraphFilePath),
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
            with exn -> Left ^ acstring exn

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
            Map.toList world.Subsystems.SubsystemMap |>
            List.filter (fun (_, subsystem) -> subsystem.SubsystemType = subsystemType) |>
            List.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystemResult, subsystem, world) = subsystem.ProcessMessages world
                let world = subsystem.ApplyResult (subsystemResult, world)
                World.setSubsystem subsystem subsystemName world)
                world

        static member private cleanUpSubsystems world =
            Map.toList world.Subsystems.SubsystemMap |>
            List.sortBy (fun (_, subsystem) -> subsystem.SubsystemOrder) |>
            List.fold (fun world (subsystemName, subsystem) ->
                let (subsystem, world) = subsystem.CleanUp world
                World.setSubsystem subsystem subsystemName world)
                world

        static member private processTasklet (taskletsNotRun, world) tasklet =
            let tickTime = World.getTickTime world
            if tickTime = tasklet.ScheduledTime then
                let world = tasklet.Operation world
                (taskletsNotRun, world)
            elif tickTime > tasklet.ScheduledTime then
                debug ^ "Tasklet leak found for time '" + acstring tickTime + "'."
                (taskletsNotRun, world)
            else (Queue.conj tasklet taskletsNotRun, world)

        static member private processTasklets world =
            let tasklets = world.Callbacks.Tasklets
            let world = World.clearTasklets world
            let (taskletsNotRun, world) = Queue.fold World.processTasklet (Queue.empty, world) tasklets
            World.restoreTasklets taskletsNotRun world

        /// Process an input event from SDL and ultimately publish any related game events.
        static member processInput (evt : SDL.SDL_Event) world =
            let world =
                match evt.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    World.exit world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = Vector2 (single evt.button.x, single evt.button.y)
                    let world =
                        if World.isMouseButtonDown MouseLeft world
                        then World.publish5 World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } Events.MouseDrag ["World.processInput"] Simulants.Game world
                        else world
                    World.publish5 World.sortSubscriptionsByPickingPriority { MouseMoveData.Position = mousePosition } Events.MouseMove ["World.processInput"] Simulants.Game world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton ^ uint32 evt.button.button
                    let mouseButtonEventAddress = ntoa ^ MouseButton.toEventName mouseButton
                    let mouseButtonDownEventAddress = Events.Mouse -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> !!"Down"
                    let mouseButtonChangeEventAddress = Events.Mouse -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> !!"Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let world = World.publish5 World.sortSubscriptionsByPickingPriority eventData mouseButtonDownEventAddress ["World.processInput"] Simulants.Game world
                    World.publish5 World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress ["World.processInput"] Simulants.Game world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let mousePosition = World.getMousePositionF world
                    let mouseButton = World.toNuMouseButton ^ uint32 evt.button.button
                    let mouseButtonEventAddress = ntoa ^ MouseButton.toEventName mouseButton
                    let mouseButtonUpEventAddress = Events.Mouse -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> !!"Up"
                    let mouseButtonChangeEventAddress = Events.Mouse -<- mouseButtonEventAddress -<- ntoa<MouseButtonData> !!"Change"
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let world = World.publish5 World.sortSubscriptionsByPickingPriority eventData mouseButtonUpEventAddress ["World.processInput"] Simulants.Game world
                    World.publish5 World.sortSubscriptionsByPickingPriority eventData mouseButtonChangeEventAddress ["World.processInput"] Simulants.Game world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let world = World.publish5 World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyDown ["World.processInput"] Simulants.Game world
                    World.publish5 World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange ["World.processInput"] Simulants.Game world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let keyboard = evt.key
                    let key = keyboard.keysym
                    let eventData = { ScanCode = int key.scancode; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let world = World.publish5 World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyUp ["World.processInput"] Simulants.Game world
                    World.publish5 World.sortSubscriptionsByHierarchy eventData Events.KeyboardKeyChange ["World.processInput"] Simulants.Game world
                | _ -> world
            (world.State.Liveness, world)

        static member private updateSimulants world =
            let world = World.updateGame world
            match World.getOptSelectedScreen world with
            | Some selectedScreen ->
                let world = World.updateScreen selectedScreen world
                let world = World.updateScreenTransition selectedScreen world
                let groups = World.proxyGroups selectedScreen world
                let world = Seq.fold (fun world group -> World.updateGroup group world) world groups
                let viewBounds = World.getCameraBy Camera.getViewBoundsRelative world
                let (quadTree, entityTree) = MutantCache.getMutant (fun () -> World.rebuildEntityTree selectedScreen world) (selectedScreen.GetEntityTree world)
                let screenState = World.getScreenState selectedScreen world
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState selectedScreen world
                let entities = QuadTree.getElementsNearBounds viewBounds quadTree
                List.fold (fun world (entity : Entity) -> World.updateEntity entity world) world entities
            | None -> world

        static member private actualizeScreenTransition camera (screen : Screen) transition world =
            match transition.OptDissolveImage with
            | Some dissolveImage ->
                let progress = single (screen.GetTransitionTicksNp world) / single transition.TransitionLifetime
                let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
                let color = Vector4 (Vector3.One, alpha)
                World.addRenderMessage
                    (RenderDescriptorsMessage
                        [LayerableDescriptor
                            { Depth = Single.MaxValue
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = -camera.EyeSize * 0.5f // negation for right-handedness
                                      Size = camera.EyeSize
                                      Rotation = 0.0f
                                      ViewType = Absolute
                                      OptInset = None
                                      Image = dissolveImage
                                      Color = color }}])
                    world
            | None -> world

        static member private actualizeSimulants world =
            let world = World.actualizeGame world
            match World.getOptSelectedScreen world with
            | Some selectedScreen ->
                let world = World.actualizeScreen selectedScreen world
                let world =
                    match selectedScreen.GetTransitionStateNp world with
                    | IncomingState -> World.actualizeScreenTransition (World.getCamera world) selectedScreen (selectedScreen.GetIncoming world) world
                    | OutgoingState -> World.actualizeScreenTransition (World.getCamera world) selectedScreen (selectedScreen.GetOutgoing world) world
                    | IdlingState -> world
                let groups = World.proxyGroups selectedScreen world
                let world = Seq.fold (fun world group -> World.actualizeGroup group world) world groups
                let viewBounds = World.getCameraBy Camera.getViewBoundsRelative world
                let (quadTree, entityTree) = MutantCache.getMutant (fun () -> World.rebuildEntityTree selectedScreen world) (selectedScreen.GetEntityTree world)
                let screenState = World.getScreenState selectedScreen world
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState selectedScreen world
                let entities = QuadTree.getElementsNearBounds viewBounds quadTree
                List.fold (fun world (entity : Entity) -> World.actualizeEntity entity world) world entities
            | None -> world

        /// Update the game engine once per frame, updating its subsystems and publishing the
        /// Update event.
        static member processUpdate handleUpdate world =
            let world = handleUpdate world
            match world.State.Liveness with
            | Running ->
                let world = World.processSubsystems UpdateType world
                match world.State.Liveness with
                | Running ->
                    let world = World.updateSimulants world
                    match world.State.Liveness with
                    | Running ->
                        let world = World.processTasklets world
                        match world.State.Liveness with
                        | Running ->
                            let world = World.actualizeSimulants world
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
        static member cleanUp world =
            World.cleanUpSubsystems world |> ignore

        /// TODO: document!
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

        /// TODO: document!
        static member run6 runWhile handleUpdate handleRender sdlDeps liveness world =
            Sdl.run8
                runWhile
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.cleanUp
                sdlDeps
                liveness
                world

        /// TODO: document!
        static member run4 runWhile sdlDeps liveness world =
            World.run6 runWhile id id sdlDeps liveness world

        /// TODO: document!
        static member run attemptMakeWorld handleUpdate handleRender sdlConfig =
            Sdl.run
                attemptMakeWorld
                World.processInput
                (World.processUpdate handleUpdate)
                (World.processRender handleRender)
                World.processPlay
                World.cleanUp
                sdlConfig

        /// Make an empty world.
        static member makeEmpty userState =

            // ensure game engine is initialized
            Nu.init ()

            // make the world's subsystems
            let subsystems =
                Subsystems.make ^
                    Map.ofList
                        [(Constants.Engine.PhysicsEngineSubsystemName, PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder { MockPhysicsEngine = () } :> Subsystem)
                         (Constants.Engine.RendererSubsystemName, RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder { MockRenderer = () } :> Subsystem)
                         (Constants.Engine.AudioPlayerSubsystemName, AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder { MockAudioPlayer = () } :> Subsystem)]

            // make the world's components
            let components =
                Components.make
                    (World.makeDefaultFacets ())
                    (World.makeDefaultEntityDispatchers ())
                    (World.makeDefaultGroupDispatchers ())
                    (World.makeDefaultScreenDispatchers ())
                    (World.makeDefaultGameDispatchers ())

            // make the world's callbacks
            let callbacks =
                Callbacks.make ()

            // make the world's state
            let worldState =
                let overlayRouter = OverlayRouter.make components.EntityDispatchers []
                let overlayer = Overlayer.makeEmpty ()
                let eyeSize = Vector2 (single Constants.Render.ResolutionXDefault, single Constants.Render.ResolutionYDefault)
                let camera = { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                WorldState.make 1L Map.empty overlayRouter overlayer camera userState

            // make the simulant states
            let simulantStates =
                let gameDispatcher = components.GameDispatchers |> Seq.head |> fun kvp -> kvp.Value
                let gameState = World.makeGameState gameDispatcher
                (gameState, Vmap.makeEmpty (KeyEq Name.equals) Constants.Engine.ScreenMapDepth)

            // make the world itself
            let world =
                { Subsystems = subsystems
                  Components = components
                  Callbacks = callbacks
                  State = worldState
                  SimulantStates = simulantStates }

            // initialize OptEntityCache after the fact due to back reference
            let world = { world with State = { world.State with OptEntityCache = KeyedCache.make (Address.empty<Entity>, world) None }}

            // finally, register the game
            World.registerGame world

        /// Try to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member attemptMake preferPluginGameDispatcher tickRate userState (plugin : NuPlugin) sdlDeps =

            // ensure game engine is initialized
            Nu.init ()

            // attempt to generate asset metadata so the rest of the world can be created
            match Metadata.tryGenerateAssetMetadataMap Constants.Assets.AssetGraphFilePath with
            | Right assetMetadataMap ->

                // make world's subsystems
                let subsystems =
                    let userSubsystems = plugin.MakeSubsystems ()
                    let physicsEngine = PhysicsEngine.make Constants.Physics.Gravity
                    let physicsEngineSubsystem = PhysicsEngineSubsystem.make Constants.Engine.DefaultSubsystemOrder physicsEngine :> Subsystem
                    let renderer =
                        match sdlDeps.OptRenderContext with
                        | Some renderContext -> Renderer.make renderContext Constants.Assets.AssetGraphFilePath :> IRenderer
                        | None -> MockRenderer.make () :> IRenderer
                    let renderer = renderer.EnqueueMessage ^ HintRenderPackageUseMessage { PackageName = Constants.Assets.DefaultPackageName }
                    let rendererSubsystem = RendererSubsystem.make Constants.Engine.DefaultSubsystemOrder renderer :> Subsystem
                    let audioPlayer =
                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                        then AudioPlayer.make Constants.Assets.AssetGraphFilePath :> IAudioPlayer
                        else MockAudioPlayer.make () :> IAudioPlayer
                    let audioPlayerSubsystem = AudioPlayerSubsystem.make Constants.Engine.DefaultSubsystemOrder audioPlayer :> Subsystem
                    let defaultSubsystems =
                        [(Constants.Engine.PhysicsEngineSubsystemName, physicsEngineSubsystem)
                         (Constants.Engine.RendererSubsystemName, rendererSubsystem)
                         (Constants.Engine.AudioPlayerSubsystemName, audioPlayerSubsystem)]
                    Subsystems.make ^ Map.ofList (defaultSubsystems @ userSubsystems)

                // make plug-in components
                let pluginFacets = plugin.MakeFacets () |> List.map World.pairWithName
                let pluginEntityDispatchers = plugin.MakeEntityDispatchers () |> List.map World.pairWithName
                let pluginGroupDispatchers = plugin.MakeGroupDispatchers () |> List.map World.pairWithName
                let pluginScreenDispatchers = plugin.MakeScreenDispatchers () |> List.map World.pairWithName
                let pluginOptGameDispatcher = plugin.MakeOptGameDispatcher ()

                // infer the active game dispatcher
                let defaultGameDispatcher = GameDispatcher ()
                let activeGameDispatcher =
                    if preferPluginGameDispatcher then
                        match pluginOptGameDispatcher with
                        | Some gameDispatcher -> gameDispatcher
                        | None -> defaultGameDispatcher
                    else defaultGameDispatcher

                // make the world's components
                let components =
                    Components.make
                        (Map.addMany pluginFacets ^ World.makeDefaultFacets ())
                        (Map.addMany pluginEntityDispatchers ^ World.makeDefaultEntityDispatchers ())
                        (Map.addMany pluginGroupDispatchers ^ World.makeDefaultGroupDispatchers ())
                        (Map.addMany pluginScreenDispatchers ^ World.makeDefaultScreenDispatchers ())
                        (Map.addMany [World.pairWithName activeGameDispatcher] ^ World.makeDefaultGameDispatchers ())

                // make the world's callbacks
                let callbacks =
                    Callbacks.make ()

                // make the world's state
                let worldState =
                    let intrinsicOverlays = World.createIntrinsicOverlays components.Facets components.EntityDispatchers
                    let pluginOverlayRoutes = plugin.MakeOverlayRoutes ()
                    let overlayRouter = OverlayRouter.make components.EntityDispatchers pluginOverlayRoutes
                    let overlayer = Overlayer.make Constants.Assets.OverlayFilePath intrinsicOverlays
                    let eyeSize = Vector2 (single sdlDeps.Config.ViewW, single sdlDeps.Config.ViewH)
                    let camera = { EyeCenter = Vector2.Zero; EyeSize = eyeSize }
                    WorldState.make tickRate assetMetadataMap overlayRouter overlayer camera userState

                // make the world's simulant states
                let simulantStates =
                    let gameState = World.makeGameState activeGameDispatcher
                    (gameState, Vmap.makeEmpty (KeyEq (Name.equals)) Constants.Engine.ScreenMapDepth)

                // make the world itself, and register the game
                let world =
                    { Subsystems = subsystems
                      Components = components
                      Callbacks = callbacks
                      State = worldState
                      SimulantStates = simulantStates }

                // initialize OptEntityCache after the fact due to back reference
                let world = { world with State = { world.State with OptEntityCache = KeyedCache.make (Address.empty<Entity>, world) None }}

                // finally, register the game
                let world = World.registerGame world
                Right world

            // forward error message
            | Left error -> Left error