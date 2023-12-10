// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Threading
open SDL2
open ImGuiNET
open Prime

[<AutoOpen>]
module WorldModule2 =

    (* Frame Pacing Timer *)
    let private FrameTimer = Stopwatch ()

    (* Performance Timers *)
    let private TotalTimer = Stopwatch ()
    let private InputTimer = Stopwatch ()
    let private PhysicsTimer = Stopwatch ()
    let private PreUpdateTimer = Stopwatch ()
    let private PreUpdateGatherTimer = Stopwatch ()
    let private PreUpdateGameTimer = Stopwatch ()
    let private PreUpdateScreensTimer = Stopwatch ()
    let private PreUpdateGroupsTimer = Stopwatch ()
#if !DISABLE_ENTITY_PRE_UPDATE
    let private PreUpdateEntitiesTimer = Stopwatch ()
#endif
    let private UpdateTimer = Stopwatch ()
    let private UpdateGatherTimer = Stopwatch ()
    let private UpdateGameTimer = Stopwatch ()
    let private UpdateScreensTimer = Stopwatch ()
    let private UpdateGroupsTimer = Stopwatch ()
    let private UpdateEntitiesTimer = Stopwatch ()
    let private PostUpdateTimer = Stopwatch ()
    let private PostUpdateGatherTimer = Stopwatch ()
    let private PostUpdateGameTimer = Stopwatch ()
    let private PostUpdateScreensTimer = Stopwatch ()
    let private PostUpdateGroupsTimer = Stopwatch ()
#if !DISABLE_ENTITY_POST_UPDATE
    let private PostUpdateEntitiesTimer = Stopwatch ()
#endif
    let private TaskletsTimer = Stopwatch ()
    let private DestructionTimer = Stopwatch ()
    let private PerProcessTimer = Stopwatch ()
    let private PreProcessTimer = Stopwatch ()
    let private PostProcessTimer = Stopwatch ()
    let private RenderGatherTimer = Stopwatch ()
    let private RenderEntitiesTimer = Stopwatch ()
    let private RenderTimer = Stopwatch ()
    let private AudioTimer = Stopwatch ()

    (* Transition Values *)
    let private ScreenTransitionMouseLeftId = Gen.id
    let private ScreenTransitionMouseMiddleId = Gen.id
    let private ScreenTransitionMouseRightId = Gen.id
    let private ScreenTransitionMouseX1Id = Gen.id
    let private ScreenTransitionMouseX2Id = Gen.id
    let private ScreenTransitionKeyboardKeyId = Gen.id

    (* Cached HashSets *)
    let private CachedHashSet2d = HashSet (QuadelementEqualityComparer ())
    let private CachedHashSet3d = HashSet (OctelementEqualityComparer ())

    type World with

        static member internal makeQuadtree () =
            Quadtree.make Constants.Engine.QuadtreeDepth Constants.Engine.QuadtreeSize

        static member internal makeOctree () =
            Octree.make Constants.Engine.OctreeDepth Constants.Engine.OctreeSize

        static member internal rebuildQuadtree world =
            let omniEntities =
                match World.getOmniScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat
                | None -> Seq.empty
            let selectedEntities =
                match World.getSelectedScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat
                | None -> Seq.empty
            let entities = Seq.append omniEntities selectedEntities
            let quadtree = World.makeQuadtree ()
            for entity in entities do
                let bounds = entity.GetBounds world
                let visible = entity.GetVisible world || entity.GetAlwaysRender world
                let static_ = entity.GetStatic world
                let presence = entity.GetPresence world
                if entity.GetIs2d world then
                    let element = Quadelement.make visible static_ entity
                    Quadtree.addElement presence bounds.Box2 element quadtree
            quadtree

        static member internal rebuildOctree world =
            let omniEntities =
                match World.getOmniScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat
                | None -> Seq.empty
            let selectedEntities =
                match World.getSelectedScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat
                | None -> Seq.empty
            let entities = Seq.append omniEntities selectedEntities
            let octree = World.makeOctree ()
            for entity in entities do
                let bounds = entity.GetBounds world
                let visible = entity.GetVisible world || entity.GetAlwaysRender world
                let static_ = entity.GetStatic world
                let lightProbe = entity.GetLightProbe world
                let light = entity.GetLight world
                let presence = entity.GetPresence world
                if entity.GetIs3d world then
                    let element = Octelement.make visible static_ lightProbe light presence bounds entity
                    Octree.addElement presence bounds element octree
            octree

        /// Select the given screen without transitioning, even if another transition is taking place.
        static member internal selectScreenOpt transitionStateAndScreenOpt world =
            let world =
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    let eventTrace = EventTrace.debug "World" "selectScreen" "Deselecting" EventTrace.empty
                    World.publishPlus () selectedScreen.DeselectingEvent eventTrace selectedScreen false false world
                | None -> world
            match transitionStateAndScreenOpt with
            | Some (transitionState, screen) ->
                let world = World.setScreenTransitionStatePlus transitionState screen world
                let world = World.setSelectedScreen screen world
                let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                World.publishPlus () screen.SelectEvent eventTrace screen false false world
            | None ->
                World.setSelectedScreenOpt None world

        /// Select the given screen without transitioning, even if another transition is taking place.
        static member selectScreen transitionState screen world =
            World.selectScreenOpt (Some (transitionState, screen)) world

        /// Try to check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        static member tryGetSelectedScreenIdling world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> Some (selectedScreen.Idling world)
            | None -> None

        /// Try to check that the selected screen is transitioning.
        static member tryGetSelectedScreenTransitioning world =
            Option.map not (World.tryGetSelectedScreenIdling world)

        /// Check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen (failing with an exception if no screen is selected).
        static member getSelectedScreenIdling world =
            match World.tryGetSelectedScreenIdling world with
            | Some answer -> answer
            | None -> failwith "Cannot query state of non-existent selected screen."

        /// Check that the selected screen is transitioning (failing with an exception if no screen
        /// is selected).
        static member getSelectedScreenTransitioning world =
            not (World.getSelectedScreenIdling world)

        /// Set screen transition state, enabling or disabling input events respectively.
        static member private setScreenTransitionStatePlus state (screen : Screen) world =
            let world = screen.SetTransitionState state world
            match state with
            | IdlingState _ ->
                let world = World.unsubscribe ScreenTransitionMouseLeftId world
                let world = World.unsubscribe ScreenTransitionMouseMiddleId world
                let world = World.unsubscribe ScreenTransitionMouseRightId world
                let world = World.unsubscribe ScreenTransitionMouseX1Id world
                let world = World.unsubscribe ScreenTransitionMouseX2Id world
                let world = World.unsubscribe ScreenTransitionKeyboardKeyId world
                world
            | IncomingState _ | OutgoingState _ ->
                let world = World.subscribePlus ScreenTransitionMouseLeftId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Left/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                let world = World.subscribePlus ScreenTransitionMouseMiddleId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Middle/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                let world = World.subscribePlus ScreenTransitionMouseRightId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Right/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX1Id World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/X1/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                let world = World.subscribePlus ScreenTransitionMouseX2Id World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/X2/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                let world = World.subscribePlus ScreenTransitionKeyboardKeyId World.handleAsSwallow (stoa<KeyboardKeyData> ("KeyboardKey/" + Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> snd
                world

        static member private updateScreenTransition3 transitionType (selectedScreen : Screen) world =
            let transition =
                match transitionType with
                | Incoming -> selectedScreen.GetIncoming world
                | Outgoing -> selectedScreen.GetOutgoing world
            let transitionTime = (selectedScreen.GetTransitionState world).TransitionTime
            match (transitionTime, transition.TransitionLifeTime) with
            | (UpdateTime time, UpdateTime lifeTime) ->
                let localTime = world.UpdateTime - time
                localTime - 2L = lifeTime
            | (ClockTime time, ClockTime lifeTime) ->
                let localTime = world.ClockTime - time
                localTime - world.ClockDelta * 2.0f >= lifeTime
            | (_, _) -> failwithumf ()

        static member private updateScreenIdling3 transitionTime slide (_ : Screen) (world : World) =
            match (transitionTime, slide.IdlingTime) with
            | (UpdateTime time, UpdateTime lifeTime) ->
                let localTime = world.UpdateTime - time
                localTime - 2L = lifeTime
            | (ClockTime time, ClockTime lifeTime) ->
                let localTime = world.ClockTime - time
                localTime - world.ClockDelta * 2.0f >= lifeTime
            | (_, _) -> failwithumf ()

        static member private updateScreenIncoming transitionTime (selectedScreen : Screen) world =
            match World.getLiveness world with
            | Live ->
                let world =
                    if (match transitionTime with
                        | UpdateTime time -> time + 1L = world.UpdateTime
                        | ClockTime time -> time + world.ClockDelta >= world.ClockTime) then
                        let world =
                            match (selectedScreen.GetIncoming world).SongOpt with
                            | Some playSong ->
                                match World.getCurrentSongOpt world with
                                | Some song when assetEq song.Song playSong.Song -> world // do nothing when song is the same
                                | _ -> World.playSong playSong.FadeInTime playSong.FadeOutTime GameTime.zero playSong.Volume playSong.Song world // play song when song is different
                            | None -> world
                        let eventTrace = EventTrace.debug "World" "updateScreenIncoming" "IncomingStart" EventTrace.empty
                        World.publishPlus () selectedScreen.IncomingStartEvent eventTrace selectedScreen false false world
                    else world
                match World.getLiveness world with
                | Live ->
                    if World.updateScreenTransition3 Incoming selectedScreen world then
                        let eventTrace = EventTrace.debug "World" "updateScreenIncoming" "IncomingFinish" EventTrace.empty
                        let world = World.setScreenTransitionStatePlus (IdlingState world.GameTime) selectedScreen world
                        World.publishPlus () selectedScreen.IncomingFinishEvent eventTrace selectedScreen false false world
                    else world
                | Dead -> world
            | Dead -> world

        static member private updateScreenIdling transitionTime (selectedScreen : Screen) world =
            match World.getLiveness world with
            | Live ->
                match selectedScreen.GetSlideOpt world with
                | Some slide ->
                    if World.updateScreenIdling3 transitionTime slide selectedScreen world
                    then World.setScreenTransitionStatePlus (OutgoingState world.GameTime) selectedScreen world
                    else world
                | None ->
                    match World.getDesiredScreen world with
                    | Desire desiredScreen ->
                        if desiredScreen <> selectedScreen then
                            if world.Unaccompanied || world.Advancing
                            then World.setScreenTransitionStatePlus (OutgoingState world.GameTime) selectedScreen world
                            else World.selectScreenOpt (Some (TransitionState.IdlingState world.GameTime, desiredScreen)) world // quick cut
                        else world
                    | DesireNone -> World.setScreenTransitionStatePlus (OutgoingState world.GameTime) selectedScreen world
                    | DesireIgnore -> world
            | Dead -> world

        static member private updateScreenOutgoing transitionTime (selectedScreen : Screen) (world : World) =
            let world =
                if (match transitionTime with
                    | UpdateTime time -> time + 1L = world.UpdateTime
                    | ClockTime time -> time + world.ClockDelta >= world.ClockTime) then
                    let incoming = selectedScreen.GetIncoming world
                    let outgoing = selectedScreen.GetOutgoing world
                    let world =
                        match outgoing.SongOpt with
                        | Some playSong ->
                            let destinationOpt =
                                match selectedScreen.GetSlideOpt world with
                                | Some slide -> Some slide.Destination
                                | None ->
                                    match World.getScreenTransitionDestinationOpt world with
                                    | Some destination -> Some destination
                                    | None ->
                                        match World.getDesiredScreen world with
                                        | Desire destination -> Some destination
                                        | DesireNone -> None
                                        | DesireIgnore -> None
                            match destinationOpt with
                            | Some destination ->
                                match (incoming.SongOpt, (destination.GetIncoming world).SongOpt) with
                                | (Some song, Some song2) when assetEq song.Song song2.Song -> world // do nothing when song is the same
                                | (None, None) -> world // do nothing when neither plays a song (allowing manual control)
                                | (_, _) -> World.fadeOutSong playSong.FadeOutTime world // fade out when song is different
                            | None ->
                                match incoming.SongOpt with
                                | Some _ -> World.fadeOutSong playSong.FadeOutTime world
                                | None -> world
                        | None -> world
                    let eventTrace = EventTrace.debug "World" "updateScreenTransition" "OutgoingStart" EventTrace.empty
                    World.publishPlus () selectedScreen.OutgoingStartEvent eventTrace selectedScreen false false world
                else world
            match World.getLiveness world with
            | Live ->
                if World.updateScreenTransition3 Outgoing selectedScreen world then
                    let world = World.setScreenTransitionStatePlus (IdlingState world.GameTime) selectedScreen world
                    let world =
                        match World.getLiveness world with
                        | Live ->
                            let eventTrace = EventTrace.debug "World" "updateScreenOutgoing" "OutgoingFinish" EventTrace.empty
                            World.publishPlus () selectedScreen.OutgoingFinishEvent eventTrace selectedScreen false false world
                        | Dead -> world
                    match World.getLiveness world with
                    | Live ->
                        let destinationOpt =
                            match selectedScreen.GetSlideOpt world with
                            | Some slide -> Some slide.Destination
                            | None ->
                                match World.getScreenTransitionDestinationOpt world with
                                | Some destination -> Some destination
                                | None ->
                                    match World.getDesiredScreen world with
                                    | Desire destination -> Some destination
                                    | DesireNone -> None
                                    | DesireIgnore -> None
                        match destinationOpt with
                        | Some destination ->
                            if destination <> selectedScreen
                            then World.selectScreen (IncomingState world.GameTime) destination world
                            else world
                        | None ->
                            let world = World.selectScreenOpt None world
                            match World.getDesiredScreen world with // handle the possibility that screen deselect event changed destination
                            | Desire destination -> World.selectScreen (IncomingState world.GameTime) destination world
                            | DesireNone -> world
                            | DesireIgnore -> world
                    | Dead -> world
                else world
            | Dead -> world

        static member private updateScreenTransition world =
            // NOTE: transitions always take one additional frame because it needs to render frame 0 and frame MAX + 1 for
            // full opacity if fading and an extra frame for the render messages to actually get processed.
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                match selectedScreen.GetTransitionState world with
                | IncomingState transitionTime -> World.updateScreenIncoming transitionTime selectedScreen world
                | IdlingState transitionTime -> World.updateScreenIdling transitionTime selectedScreen world
                | OutgoingState transitionTime -> World.updateScreenOutgoing transitionTime selectedScreen world
            | None ->
                match World.getDesiredScreen world with
                | Desire desiredScreen -> World.transitionScreen desiredScreen world
                | DesireNone -> world
                | DesireIgnore -> world

        /// Try to transition to the given screen if no other transition is in progress.
        static member tryTransitionScreen destination world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                if  selectedScreen <> destination &&
                    not (World.getSelectedScreenTransitioning world) then
                    let world = World.setScreenTransitionDestinationOpt (Some destination) world
                    let world = World.setScreenTransitionStatePlus (OutgoingState world.GameTime) selectedScreen world
                    (true, world)
                else (false, world)
            | None ->
                let world = World.setScreenTransitionStatePlus (IncomingState world.GameTime) destination world
                let world = World.setSelectedScreen destination world
                (true, world)

        /// Transition to the given screen.
        static member transitionScreen destination world =
            World.tryTransitionScreen destination world |> snd

        /// Set the slide aspects of a screen.
        static member setScreenSlide (slideDescriptor : SlideDescriptor) destination (screen : Screen) world =

            // destroy existing slide group if any
            let slideGroup = screen / "SlideGroup"
            let slideSprite = slideGroup / "SlideSprite"
            let world = World.destroyGroupImmediate slideGroup world

            // create slide group
            let eyeSize = World.getEyeSize2d world
            let world = screen.SetSlideOpt (Some { IdlingTime = slideDescriptor.IdlingTime; Destination = destination }) world
            let world = World.createGroup<GroupDispatcher> (Some slideGroup.Name) screen world |> snd
            let world = World.setGroupProtected true slideGroup world |> snd'
            let world = slideGroup.SetPersistent false world

            // create slide sprite
            let world = World.createEntity<StaticSpriteDispatcher> DefaultOverlay (Some slideSprite.Surnames) slideGroup world |> snd
            let world = World.setEntityProtected true slideSprite world |> snd'
            let world = slideSprite.SetPersistent false world
            let world = slideSprite.SetSize eyeSize.V3 world
            let world =
                if not Constants.Engine.EntityPerimeterCentered2dDefault
                then slideSprite.SetPosition (-eyeSize.V3 * 0.5f) world
                else world
            let world = slideSprite.SetAbsolute true world
            let world =
                match slideDescriptor.SlideImageOpt with
                | Some slideImage ->
                    let world = slideSprite.SetStaticImage slideImage world
                    let world = slideSprite.SetVisible true world
                    world
                | None ->
                    let world = slideSprite.SetStaticImage Assets.Default.NuSlide world
                    let world = slideSprite.SetVisible false world
                    world
            world

        /// Create a dissolve screen whose content is loaded from the given group file.
        static member createDissolveScreenFromGroupFile6 dispatcherName nameOpt dissolveDescriptor songOpt groupFilePath world =
            let (dissolveScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt dissolveDescriptor songOpt world
            let world = World.readGroupFromFile groupFilePath None dissolveScreen world |> snd
            (dissolveScreen, world)

        /// Create a dissolve screen whose content is loaded from the given group file.
        static member createDissolveScreenFromGroupFile<'d when 'd :> ScreenDispatcher> nameOpt dissolveDescriptor songOpt groupFilePath world =
            World.createDissolveScreenFromGroupFile6 typeof<'d>.Name nameOpt dissolveDescriptor groupFilePath songOpt world

        /// Create a slide screen that transitions to the given destination upon completion.
        static member createSlideScreen6 dispatcherName nameOpt slideDescriptor destination world =
            let (slideScreen, world) = World.createDissolveScreen5 dispatcherName nameOpt slideDescriptor.DissolveDescriptor None world
            let world = World.setScreenSlide slideDescriptor destination slideScreen world
            (slideScreen, world)

        /// Create a slide screen that transitions to the given destination upon completion.
        static member createSlideScreen<'d when 'd :> ScreenDispatcher> nameOpt slideDescriptor destination world =
            World.createSlideScreen6 typeof<'d>.Name nameOpt slideDescriptor destination world

        static member internal makeIntrinsicOverlays facets entityDispatchers =
            let requiresFacetNames = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = facets |> Map.toValueList |> List.map box
            let entityDispatchers = entityDispatchers |> Map.toValueList |> List.map box
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Overlay.makeIntrinsicOverlays requiresFacetNames sourceTypes

        /// Try to reload the overlayer currently in use by the world.
        static member tryReloadOverlayer inputDirectory outputDirectory world =
            
            // attempt to reload overlay file
            let inputOverlayerFilePath = inputDirectory + "/" + Assets.Global.OverlayerFilePath
            let outputOverlayerFilePath = outputDirectory + "/" + Assets.Global.OverlayerFilePath
            try File.Copy (inputOverlayerFilePath, outputOverlayerFilePath, true)

                // cache old overlayer and make new one
                let overlayerOld = World.getOverlayer world
                let entityDispatchers = World.getEntityDispatchers world
                let facets = World.getFacets world
                let intrinsicOverlays = World.makeIntrinsicOverlays facets entityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays outputOverlayerFilePath with
                | Right overlayer ->

                    // update overlayer and overlay router
                    let overlays = Overlayer.getIntrinsicOverlays overlayer @ Overlayer.getExtrinsicOverlays overlayer
                    let overlayRoutes =
                        overlays |>
                        List.map (fun overlay -> overlay.OverlaidTypeNames |> List.map (fun typeName -> (typeName, overlay.OverlayName))) |>
                        List.concat
                    let overlayRouter = OverlayRouter.make overlayRoutes
                    let world = World.setOverlayer overlayer world
                    let world = World.setOverlayRouter overlayRouter world

                    // apply overlays to all entities
                    let entities = World.getEntitiesFlattened1 world
                    let world = Seq.fold (World.applyEntityOverlay overlayerOld overlayer) world entities
                    (Right overlayer, world)

                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// Send a message to the subsystems to reload their existing assets.
        static member reloadExistingAssets world =
            let world = World.reloadRenderAssets2d world
            let world = World.reloadRenderAssets3d world
            let world = World.reloadAudioAssets world
            let world = World.reloadSymbols world
            Metadata.regenerateMetadata ()
            world

        /// Attempt to reload the asset graph.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =

            // attempt to reload asset graph file
            try File.Copy
                    (inputDirectory + "/" + Assets.Global.AssetGraphFilePath,
                     outputDirectory + "/" + Assets.Global.AssetGraphFilePath,
                     true)

                // attempt to load asset graph
                match AssetGraph.tryMakeFromFile (outputDirectory + "/" + Assets.Global.AssetGraphFilePath) with
                | Right assetGraph ->

                    // build and reload assets
                    AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory false assetGraph
                    Metadata.generateMetadata (World.getImperative world) assetGraph
                    let world = World.reloadExistingAssets world
                    let world = World.publishPlus () Nu.Game.Handle.AssetsReloadEvent (EventTrace.debug "World" "publishAssetsReload" "" EventTrace.empty) Nu.Game.Handle false false world
                    (Right assetGraph, world)

                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.choose world)

        /// Reload asset graph, build assets, then reload built assets.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssets world =
            let targetDir = AppDomain.CurrentDomain.BaseDirectory
            let assetSourceDir = Pathf.GetFullPath (targetDir + "../../..")
            match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world with
            | (Right _, world) -> (true, world)
            | (Left _, world) -> (false, world)

        /// Shelve a non-current world for background storage (such as for an undo / redo system).
        static member shelveNonCurrent world =
            World.shelveAmbientStateNonCurrent world

        /// Shelve the current world for background storage (such as for an undo / redo system).
        static member shelveCurrent world =
            World.shelveAmbientStateCurrent world

        /// Unshelve the world from background storage (such as for an undo / redo system).
        static member unshelve (world : World) =

            // sync tick watch state to advancing
            let world = World.unshelveAmbientState world

            // clear existing 3d physics messages and rebuild
            let world = World.clearPhysicsMessages3d world
            let world = World.enqueuePhysicsMessage3d ClearPhysicsMessageInternal world

            // clear existing 2d physics messages and rebuild
            let world = World.clearPhysicsMessages2d world
            let world = World.enqueuePhysicsMessage2d ClearPhysicsMessageInternal world

            // register the physics of entities in the current screen
            match World.getSelectedScreenOpt world with
            | Some screen ->
                let groups = World.getGroups screen world
                Seq.fold (fun world (group : Group) ->
                    if group.Exists world then
                        let entities = World.getEntitiesFlattened group world
                        Seq.fold (fun world (entity : Entity) ->
                            if entity.Exists world
                            then World.registerEntityPhysics entity world
                            else world)
                            world entities
                        else world)
                    world groups
            | None -> world

        static member private processTasklet simulant tasklet (taskletsNotRun : OMap<Simulant, World Tasklet UList>) (world : World) =
            let shouldRun =
                match tasklet.ScheduledTime with
                | UpdateTime time -> time <= world.UpdateTime
                | ClockTime time -> time <= world.ClockTime
            if shouldRun
            then (taskletsNotRun, tasklet.ScheduledOp world)
            else
                let taskletsNotRun =
                    match taskletsNotRun.TryGetValue simulant with
                    | (true, taskletList) -> OMap.add simulant (UList.add tasklet taskletList) taskletsNotRun
                    | (false, _) -> OMap.add simulant (UList.singleton (OMap.getConfig taskletsNotRun) tasklet) taskletsNotRun
                (taskletsNotRun, world)

        static member private processTasklets world =
            let tasklets = World.getTasklets world
            let world = World.clearTasklets world
            let (taskletsNotRun, world) =
                OMap.fold (fun (taskletsNotRun, world) simulant taskletList ->
                    UList.fold (fun (taskletsNotRun, world) tasklet ->
                        if World.getExists simulant world
                        then World.processTasklet simulant tasklet taskletsNotRun world
                        else (taskletsNotRun, world))
                        (taskletsNotRun, world)
                        taskletList)
                    (OMap.makeEmpty HashIdentity.Structural (OMap.getConfig tasklets), world)
                    tasklets
            let taskletsNotRun = OMap.filter (fun simulant _ -> World.getExists simulant world) taskletsNotRun
            World.restoreTasklets taskletsNotRun world

        static member private destroySimulants world =
            let destructionListRev = World.getDestructionListRev world
            let world = List.foldBack (fun simulant world -> World.destroyImmediate simulant world) destructionListRev world
            if List.notEmpty (World.getDestructionListRev world) then World.destroySimulants world else world

        /// Process an input event from SDL and ultimately publish any related game events.
        static member private processInput2 (evt : SDL.SDL_Event) (world : World) =
            let world =
                match evt.``type`` with
                | SDL.SDL_EventType.SDL_QUIT ->
                    if world.Unaccompanied
                    then World.exit world
                    else world
                | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                    let mousePosition = v2 (single evt.button.x) (single evt.button.y)
                    let world =
                        if World.isMouseButtonDown MouseLeft world then
                            let eventTrace = EventTrace.debug "World" "processInput" "MouseDrag" EventTrace.empty
                            World.publishPlus { MouseMoveData.Position = mousePosition } Nu.Game.Handle.MouseDragEvent eventTrace Nu.Game.Handle true true world
                        else world
                    let eventTrace = EventTrace.debug "World" "processInput" "MouseMove" EventTrace.empty
                    World.publishPlus { MouseMoveData.Position = mousePosition } Nu.Game.Handle.MouseMoveEvent eventTrace Nu.Game.Handle true true world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                    let io = ImGui.GetIO ()
                    if not (io.WantCaptureMousePlus) then
                        let mousePosition = World.getMousePosition world
                        let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                        let mouseButtonDownEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Down/Event/" + Constants.Engine.GameName)
                        let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event/" + Constants.Engine.GameName)
                        let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                        let eventTrace = EventTrace.debug "World" "processInput" "MouseButtonDown" EventTrace.empty
                        let world = World.publishPlus eventData mouseButtonDownEvent eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "MouseButtonChange" EventTrace.empty
                        World.publishPlus eventData mouseButtonChangeEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                    let io = ImGui.GetIO ()
                    if not (io.WantCaptureMousePlus) then
                        let mousePosition = World.getMousePosition world
                        let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                        let mouseButtonUpEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Up/Event/" + Constants.Engine.GameName)
                        let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event/" + Constants.Engine.GameName)
                        let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                        let eventTrace = EventTrace.debug "World" "processInput" "MouseButtonUp" EventTrace.empty
                        let world = World.publishPlus eventData mouseButtonUpEvent eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "MouseButtonChange" EventTrace.empty
                        World.publishPlus eventData mouseButtonChangeEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_MOUSEWHEEL ->
                    let imGui = World.getImGui world
                    if evt.wheel.y <> 0 then imGui.HandleMouseWheelChange (single evt.wheel.y)
                    // TODO: P1: publish mouse wheel engine events.
                    world
                | SDL.SDL_EventType.SDL_TEXTINPUT ->
                    let io = ImGui.GetIO ()
                    let imGui = World.getImGui world
                    let textInput = char evt.text.text.FixedElementField
                    imGui.HandleKeyChar textInput
                    if not (io.WantCaptureKeyboardPlus) then
                        let eventData = { TextInput = textInput }
                        let eventTrace = EventTrace.debug "World" "processInput" "TextInput" EventTrace.empty
                        World.publishPlus eventData Nu.Game.Handle.TextInputEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let io = ImGui.GetIO ()
                    if not (io.WantCaptureKeyboardPlus) then
                        let keyboard = evt.key
                        let key = keyboard.keysym
                        let eventData = { KeyboardKey = key.scancode |> int |> enum<KeyboardKey>; Repeated = keyboard.repeat <> byte 0; Down = true }
                        let eventTrace = EventTrace.debug "World" "processInput" "KeyboardKeyDown" EventTrace.empty
                        let world = World.publishPlus eventData Nu.Game.Handle.KeyboardKeyDownEvent eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                        World.publishPlus eventData Nu.Game.Handle.KeyboardKeyChangeEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_KEYUP ->
                    let io = ImGui.GetIO ()
                    if not (io.WantCaptureKeyboardPlus) then
                        let keyboard = evt.key
                        let key = keyboard.keysym
                        let eventData = { KeyboardKey = key.scancode |> int |> enum<KeyboardKey>; Repeated = keyboard.repeat <> byte 0; Down = false }
                        let eventTrace = EventTrace.debug "World" "processInput" "KeyboardKeyUp" EventTrace.empty
                        let world = World.publishPlus eventData Nu.Game.Handle.KeyboardKeyUpEvent eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "KeyboardKeyChange" EventTrace.empty
                        World.publishPlus eventData Nu.Game.Handle.KeyboardKeyChangeEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_JOYHATMOTION ->
                    let index = evt.jhat.which
                    let direction = evt.jhat.hatValue
                    let eventData = { GamepadDirection = GamepadState.toNuDirection direction }
                    let eventTrace = EventTrace.debug "World" "processInput" "GamepadDirectionChange" EventTrace.empty
                    World.publishPlus eventData (Nu.Game.Handle.GamepadDirectionChangeEvent index) eventTrace Nu.Game.Handle true true world
                | SDL.SDL_EventType.SDL_JOYBUTTONDOWN ->
                    let index = evt.jbutton.which
                    let button = int evt.jbutton.button
                    if GamepadState.isSdlButtonSupported button then
                        let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                        let eventTrace = EventTrace.debug "World" "processInput" "GamepadButtonDown" EventTrace.empty
                        let world = World.publishPlus eventData (Nu.Game.Handle.GamepadButtonDownEvent index) eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "GamepadButtonChange" EventTrace.empty
                        World.publishPlus eventData (Nu.Game.Handle.GamepadButtonChangeEvent index) eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_JOYBUTTONUP ->
                    let index = evt.jbutton.which
                    let button = int evt.jbutton.button
                    if GamepadState.isSdlButtonSupported button then
                        let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                        let eventTrace = EventTrace.debug "World" "processInput" "GamepadButtonUp" EventTrace.empty
                        let world = World.publishPlus eventData (Nu.Game.Handle.GamepadButtonUpEvent index) eventTrace Nu.Game.Handle true true world
                        let eventTrace = EventTrace.debug "World" "processInput" "GamepadButtonChange" EventTrace.empty
                        World.publishPlus eventData (Nu.Game.Handle.GamepadButtonChangeEvent index) eventTrace Nu.Game.Handle true true world
                    else world
                | _ -> world
            (World.getLiveness world, world)

        static member private processIntegrationMessage integrationMessage world =
            match World.getLiveness world with
            | Live ->
                match integrationMessage with
                | BodyCollisionMessage bodyCollisionMessage ->
                    match bodyCollisionMessage.BodyShapeSource.BodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.Exists world && entity.Selected world then
                            let collisionData =
                                { BodyShapeCollider = bodyCollisionMessage.BodyShapeSource
                                  BodyShapeCollidee = bodyCollisionMessage.BodyShapeSource2
                                  Normal = bodyCollisionMessage.Normal }
                            let collisionAddress = entity.BodyCollisionEvent
                            let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                            World.publishPlus collisionData collisionAddress eventTrace Nu.Game.Handle false false world
                        else world
                    | _ -> world
                | BodySeparationMessage bodySeparationMessage ->
                    match bodySeparationMessage.BodyShapeSource.BodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.Exists world && entity.Selected world then
                            let explicit =
                                { BodyShapeSeparator = bodySeparationMessage.BodyShapeSource
                                  BodyShapeSeparatee = bodySeparationMessage.BodyShapeSource2 }
                            let separationAddress = entity.BodySeparationExplicitEvent
                            let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                            World.publishPlus explicit separationAddress eventTrace Nu.Game.Handle false false world
                        else world
                    | _ -> world
                | BodyTransformMessage bodyTransformMessage ->
                    let bodyId = bodyTransformMessage.BodyId
                    match bodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.Exists world && entity.Selected world then
                            let center = bodyTransformMessage.Center
                            if not (Single.IsNaN center.X) then
                                let rotation = bodyTransformMessage.Rotation
                                let linearVelocity = bodyTransformMessage.LinearVelocity
                                let angularVelocity = bodyTransformMessage.AngularVelocity
                                if entity.GetModelDriven world || bodyId.BodyIndex <> Constants.Physics.InternalIndex then
                                    let transformData =
                                        { BodyCenter = center
                                          BodyRotation = rotation
                                          BodyLinearVelocity = linearVelocity
                                          BodyAngularVelocity = angularVelocity }
                                    let transformAddress = entity.BodyTransformEvent
                                    let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                                    World.publishPlus transformData transformAddress eventTrace Nu.Game.Handle false false world
                                else entity.ApplyPhysics center rotation linearVelocity angularVelocity world
                            else
                                let mutable destroying = false
                                World.inspectMessages (fun message ->
                                    match message with
                                    | DestroyBodyMessage dbm -> if dbm.BodyId = bodyId then destroying <- true
                                    | DestroyBodiesMessage dbm -> if List.contains bodyId dbm.BodyIds then destroying <- true
                                    | CreateBodyMessage cbm -> if cbm.BodyId = bodyId then destroying <- false
                                    | CreateBodiesMessage cbm -> if cbm.BodySource = entity then destroying <- false
                                    | ClearPhysicsMessageInternal -> destroying <- true
                                    | _ -> ())
                                    false world
                                if not destroying then
                                    Log.info ("Entity physics out of range. Re-propagating physics for '" + scstring entity + "'.")
                                    World.propagateEntityPhysics entity world
                                else world
                        else world
                    | _ -> world
            | Dead -> world

        static member private getElements2dBy (getElementsFromQuadree : Entity Quadtree -> Entity Quadelement seq) world =
            let quadtree = World.getQuadtree world
            let (quadtree, quadtreeCache) = MutantCache.getMutant (fun () -> World.rebuildQuadtree world) quadtree
            let world = World.setQuadtree quadtreeCache world
            let elements = getElementsFromQuadree quadtree
            (elements, world)

        static member private getElementsInView2d set world =
            let viewBounds = World.getViewBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInView viewBounds set) world

        static member private getElementsInPlay2d set world =
            let playBounds = World.getPlayBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInPlay playBounds set) world

        static member private getElements2d set world =
            World.getElements2dBy (Quadtree.getElements set) world

        static member private getEntities2dBy getElementsFromQuadtree world =
            let quadtree = World.getQuadtree world
            let (quadtree, quadtreeCache) = MutantCache.getMutant (fun () -> World.rebuildQuadtree world) quadtree
            let world = World.setQuadtree quadtreeCache world
            let elements = getElementsFromQuadtree quadtree
            let entities = Seq.map (fun (element : Entity Quadelement) -> element.Entry) elements
            (entities, world)

        /// Get all 2d entities in the given bounds, including all uncullable entities.
        static member getEntitiesInBounds2d bounds set world =
            World.getEntities2dBy (Quadtree.getElementsInBounds bounds set) world

        /// Get all 2d entities at the given point, including all uncullable entities.
        static member getEntitiesAtPoint2d point set world =
            World.getEntities2dBy (Quadtree.getElementsAtPoint point set) world

        /// Get all 2d entities in the current 2d view, including all uncullable entities.
        static member getEntitiesInView2d set world =
            let viewBounds = World.getViewBounds2dRelative world
            World.getEntities2dBy (Quadtree.getElementsInView viewBounds set) world

        /// Get all 2d entities needing to update for the current 2d play zone, including all uncullable entities.
        static member getEntitiesInPlay2d set world =
            let playBounds = World.getPlayBounds2dRelative world
            World.getEntities2dBy (Quadtree.getElementsInPlay playBounds set) world

        /// Get all 2d entities in the current selected screen, including all uncullable entities.
        static member getEntities2d set world =
            World.getEntities2dBy (Quadtree.getElements set) world

        static member private getElements3dBy (getElementsFromOctree : Entity Octree -> Entity Octelement seq) world =
            let octree = World.getOctree world
            let (octree, octreeCache) = MutantCache.getMutant (fun () -> World.rebuildOctree world) octree
            let world = World.setOctree octreeCache world
            let elements = getElementsFromOctree octree
            (elements, world)

        static member private getElementsInPlay3d set world =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            World.getElements3dBy (Octree.getElementsInPlay playBox playFrustum set) world

        static member private getElementsInView3d set world =
            let frustumEnclosed = World.getEyeFrustum3dEnclosed world
            let frustumExposed = World.getEyeFrustum3dExposed world
            let frustumImposter = World.getEyeFrustum3dImposter world
            let lightBox = World.getLightBox3d world
            World.getElements3dBy (Octree.getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox set) world

        static member private getElements3d set world =
            World.getElements3dBy (Octree.getElements set) world

        static member private getEntities3dBy getElementsFromOctree world =
            let octree = World.getOctree world
            let (octree, octreeCache) = MutantCache.getMutant (fun () -> World.rebuildOctree world) octree
            let world = World.setOctree octreeCache world
            let elements = getElementsFromOctree octree
            let entities = Seq.map (fun (element : Entity Octelement) -> element.Entry) elements
            (entities, world)

        /// Get all 3d entities in the given bounds, including all uncullable entities.
        static member getEntitiesInBounds3d bounds set world =
            World.getEntities3dBy (Octree.getElementsInBounds bounds set) world

        /// Get all 3d entities at the given point, including all uncullable entities.
        static member getEntitiesAtPoint3d point set world =
            World.getEntities3dBy (Octree.getElementsAtPoint point set) world

        /// Get all 3d entities in the current 3d play zone, including all uncullable entities.
        static member getEntitiesInPlay3d set world =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            World.getEntities3dBy (Octree.getElementsInPlay playBox playFrustum set) world

        /// Get all 3d entities in the current 3d view, including all uncullable entities.
        static member getEntitiesInView3d set world =
            let frustumEnclosed = World.getEyeFrustum3dEnclosed world
            let frustumExposed = World.getEyeFrustum3dExposed world
            let frustumImposter = World.getEyeFrustum3dImposter world
            let lightBox = World.getLightBox3d world
            World.getEntities3dBy (Octree.getElementsInView frustumEnclosed frustumExposed frustumImposter lightBox set) world

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable light probes.
        static member getLightProbesInFrustum3d frustum set world =
            World.getEntities3dBy (Octree.getLightProbesInFrustum frustum set) world

        /// Get all 3d light entities in the current 3d light box, including all uncullable lights.
        static member getLightsInFrustum3d frustum set world =
            World.getEntities3dBy (Octree.getLightsInFrustum frustum set) world

        /// Get all 3d entities in the current selected screen, including all uncullable entities.
        static member getEntities3d set world =
            World.getEntities3dBy (Octree.getElements set) world

        static member private preUpdateSimulants (world : World) =

            // gather simulants
            PreUpdateGatherTimer.Start ()
            let game = Nu.Game.Handle
            let advancing = world.Advancing
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let groups = Seq.concat (List.map (flip World.getGroups world) screens)
#if !DISABLE_ENTITY_PRE_UPDATE
            let (elements3d, world) = World.getElementsInPlay3d CachedHashSet3d world
            let (elements2d, world) = World.getElementsInPlay2d CachedHashSet2d world
#endif
            PreUpdateGatherTimer.Stop ()

            // pre-update game
            PreUpdateGameTimer.Start ()
            let world = if advancing then World.preUpdateGame game world else world
            PreUpdateGameTimer.Stop ()

            // pre-update screens
            PreUpdateScreensTimer.Start ()
            let world = List.fold (fun world screen -> if advancing then World.preUpdateScreen screen world else world) world screens
            PreUpdateScreensTimer.Stop ()

            // pre-update groups
            PreUpdateGroupsTimer.Start ()
            let world = Seq.fold (fun world group -> if advancing then World.preUpdateGroup group world else world) world groups
            PreUpdateGroupsTimer.Stop ()

#if !DISABLE_ENTITY_PRE_UPDATE
            // pre-update entities
            PreUpdateEntitiesTimer.Start ()
            let advancing = world.Advancing
            let world = Seq.fold (fun world (element : Entity Octelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.preUpdateEntity element.Entry world else world) world elements3d
            let world = Seq.fold (fun world (element : Entity Quadelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.preUpdateEntity element.Entry world else world) world elements2d
            PreUpdateEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3d.Clear ()
            CachedHashSet2d.Clear ()
#endif

            // fin
            world

        static member private updateSimulants (world : World) =

            // gather simulants
            UpdateGatherTimer.Start ()
            let game = Nu.Game.Handle
            let advancing = world.Advancing
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let groups = Seq.concat (List.map (flip World.getGroups world) screens)
            let (elements3d, world) = World.getElementsInPlay3d CachedHashSet3d world
            let (elements2d, world) = World.getElementsInPlay2d CachedHashSet2d world
            UpdateGatherTimer.Stop ()

            // update game
            UpdateGameTimer.Start ()
            let world = if advancing then World.updateGame game world else world
            UpdateGameTimer.Stop ()
            
            // update screens
            UpdateScreensTimer.Start ()
            let world = List.fold (fun world screen -> if advancing then World.updateScreen screen world else world) world screens
            UpdateScreensTimer.Stop ()

            // update groups
            UpdateGroupsTimer.Start ()
            let world = Seq.fold (fun world group -> if advancing then World.updateGroup group world else world) world groups
            UpdateGroupsTimer.Stop ()

            // update entities
            UpdateEntitiesTimer.Start ()
            let world = Seq.fold (fun world (element : Entity Octelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.updateEntity element.Entry world else world) world elements3d
            let world = Seq.fold (fun world (element : Entity Quadelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.updateEntity element.Entry world else world) world elements2d
            UpdateEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3d.Clear ()
            CachedHashSet2d.Clear ()

            // fin
            world

        static member private postUpdateSimulants (world : World) =

            // gather simulants
            PostUpdateGatherTimer.Start ()
            let game = Nu.Game.Handle
            let advancing = world.Advancing
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let groups = Seq.concat (List.map (flip World.getGroups world) screens)
#if !DISABLE_ENTITY_POST_UPDATE
            let (elements3d, world) = World.getElementsInPlay3d CachedHashSet3d world
            let (elements2d, world) = World.getElementsInPlay2d CachedHashSet2d world
#endif
            PostUpdateGatherTimer.Stop ()

            // post-update game
            PostUpdateGameTimer.Start ()
            let world = if advancing then World.postUpdateGame game world else world
            PostUpdateGameTimer.Stop ()

            // post-update screens
            PostUpdateScreensTimer.Start ()
            let world = List.fold (fun world screen -> if advancing then World.postUpdateScreen screen world else world) world screens
            PostUpdateScreensTimer.Stop ()

            // post-update groups
            PostUpdateGroupsTimer.Start ()
            let world = Seq.fold (fun world group -> if advancing then World.postUpdateGroup group world else world) world groups
            PostUpdateGroupsTimer.Stop ()

#if !DISABLE_ENTITY_POST_UPDATE
            // post-update entities
            PostUpdateEntitiesTimer.Start ()
            let advancing = world.Advancing
            let world = Seq.fold (fun world (element : Entity Octelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.postUpdateEntity element.Entry world else world) world elements3d
            let world = Seq.fold (fun world (element : Entity Quadelement) -> if not (element.Entry.GetStatic world) && (element.Entry.GetAlwaysUpdate world || advancing) then World.postUpdateEntity element.Entry world else world) world elements2d
            PostUpdateEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3d.Clear ()
            CachedHashSet2d.Clear ()
#endif

            // fin
            world

        static member private renderScreenTransition5 transitionTime (_ : Vector2) (eyeSize : Vector2) (_ : Screen) transition (world : World) =
            match transition.DissolveImageOpt with
            | Some dissolveImage ->
                let progress =
                    match (transitionTime , transition.TransitionLifeTime) with
                    | (UpdateTime time, UpdateTime lifeTime) ->
                        let localTime = world.UpdateTime - time
                        single localTime / (single lifeTime + 1.0f)
                    | (ClockTime time, ClockTime lifeTime) ->
                        let localTime = world.ClockTime - time
                        single localTime / (lifeTime + world.ClockDelta)
                    | (_, _) -> failwithumf ()
                let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
                let color = Color.One.WithA alpha
                let position = -eyeSize.V3 * 0.5f
                let size = eyeSize.V3
                let mutable transform = Transform.makeDefault false
                transform.Position <- position
                transform.Size <- size
                transform.Elevation <- Single.MaxValue
                transform.Absolute <- true
                World.enqueueLayeredOperation2d
                    { Elevation = transform.Elevation
                      Horizon = transform.Horizon
                      AssetTag = AssetTag.generalize dissolveImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = transform
                              InsetOpt = ValueNone
                              Image = dissolveImage
                              Color = color
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> ()

        static member private renderScreenTransition (screen : Screen) world =
            match screen.GetTransitionState world with
            | IncomingState transitionTime -> World.renderScreenTransition5 transitionTime (World.getEyeCenter2d world) (World.getEyeSize2d world) screen (screen.GetIncoming world) world
            | OutgoingState transitionTime -> World.renderScreenTransition5 transitionTime (World.getEyeCenter2d world) (World.getEyeSize2d world) screen (screen.GetOutgoing world) world
            | IdlingState _ -> ()

        static member private renderSimulants skipCulling world =

            // gather simulants
            RenderGatherTimer.Start ()
            let game = Nu.Game.Handle
            let screens = match World.getOmniScreenOpt world with Some omniScreen -> [omniScreen] | None -> []
            let screens = match World.getSelectedScreenOpt world with Some selectedScreen -> selectedScreen :: screens | None -> screens
            let screens = List.rev screens
            let groups = Seq.concat (List.map (flip World.getGroups world) screens)
            let groupsInvisible =
                if world.Accompanied
                then hashSetPlus HashIdentity.Structural (Seq.filter (fun (group : Group) -> not (group.GetVisible world)) groups)
                else hashSetPlus HashIdentity.Structural []
            let (elements3d, world) = if skipCulling then World.getElements3d CachedHashSet3d world else World.getElementsInView3d CachedHashSet3d world
            let (elements2d, world) = if skipCulling then World.getElements2d CachedHashSet2d world else World.getElementsInView2d CachedHashSet2d world
            RenderGatherTimer.Stop ()

            // render simulants breadth-first
            World.renderGame game world
            for screen in screens do
                World.renderScreen screen world
            match World.getSelectedScreenOpt world with Some selectedScreen -> World.renderScreenTransition selectedScreen world | None -> ()
            for group in groups do
                if not (groupsInvisible.Contains group) then
                    World.renderGroup group world

            // render entities
            RenderEntitiesTimer.Start ()
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements3d do
                    if element.Visible then
                        World.renderEntity element.Entry world
            else
                for element in elements3d do
                    if element.Visible && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity element.Entry world
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements2d do
                    if element.Visible then
                        World.renderEntity element.Entry world
            else
                for element in elements2d do
                    if element.Visible && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity element.Entry world
            RenderEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3d.Clear ()
            CachedHashSet2d.Clear ()

            // fin
            world

        static member private processInput world =
            if SDL.SDL_WasInit SDL.SDL_INIT_TIMER <> 0u then
                let mutable result = (World.getLiveness world, world)
                let mutable polledEvent = SDL.SDL_Event ()
                while
                    (match fst result with Live -> true | Dead -> false) &&
                    SDL.SDL_PollEvent &polledEvent <> 0 do
                    result <- World.processInput2 polledEvent (snd result)
                let (liveness, world) = result
                match liveness with Dead -> World.exit world | Live -> world
            else world

        static member private processPhysics2d world =
            let physicsEngine = World.getPhysicsEngine2d world
            let physicsMessages = physicsEngine.PopMessages ()
            let integrationMessages = physicsEngine.Integrate world.GameDelta physicsMessages
            let eventTrace = EventTrace.debug "World" "processPhysics2d" "" EventTrace.empty
            let world = World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
            let world = Seq.fold (flip World.processIntegrationMessage) world integrationMessages
            world

        static member private processPhysics3d world =
            let physicsEngine = World.getPhysicsEngine3d world
            let physicsMessages = physicsEngine.PopMessages ()
            let integrationMessages = physicsEngine.Integrate world.GameDelta physicsMessages
            let eventTrace = EventTrace.debug "World" "processPhysics3d" "" EventTrace.empty
            let world = World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
            let world = Seq.fold (flip World.processIntegrationMessage) world integrationMessages
            world

        static member private processPhysics world =
            let world = World.processPhysics3d world
            let world = World.processPhysics2d world
            world

        /// Clean-up the resources held by the world.
        static member cleanUp world =
            let game = Nu.Game.Handle
            let world = World.unregisterGame game world
            World.cleanUpSubsystems world |> ignore

        /// Run the game engine with the given handlers, but don't clean up at the end, and return the world.
        static member runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess liveness firstFrame world =

            // run loop if user-defined run-while predicate passes
            TotalTimer.Start ()
            if runWhile world then

                // run user-defined pre-process callbacks
                PreProcessTimer.Start ()
                let world = World.preProcess world
                let world = preProcess world
                PreProcessTimer.Stop ()
                match liveness with
                | Live ->

                    // update screen transitioning process
                    let world = World.updateScreenTransition world
                    match World.getLiveness world with
                    | Live ->

                        // process HID inputs
                        InputTimer.Start ()
                        let world = World.processInput world
                        InputTimer.Stop ()
                        match World.getLiveness world with
                        | Live ->

                            // process physics
                            PhysicsTimer.Start ()
                            let world = World.processPhysics world
                            PhysicsTimer.Stop ()
                            match World.getLiveness world with
                            | Live ->

                                // pre-update simulants
                                PreUpdateTimer.Start ()
                                let world = World.preUpdateSimulants world
                                PreUpdateTimer.Stop ()
                                match World.getLiveness world with
                                | Live ->

                                    // update simulants
                                    UpdateTimer.Start ()
                                    let world = World.updateSimulants world
                                    UpdateTimer.Stop ()
                                    match World.getLiveness world with
                                    | Live ->

                                        // post-update simulants
                                        PostUpdateTimer.Start ()
                                        let world = World.postUpdateSimulants world
                                        PostUpdateTimer.Stop ()
                                        match World.getLiveness world with
                                        | Live ->

                                            // run user-defined per-process callbacks
                                            PerProcessTimer.Start ()
                                            let world = World.perProcess world
                                            let world = perProcess world
                                            PerProcessTimer.Stop ()
                                            match World.getLiveness world with
                                            | Live ->

                                                // process tasklets that have been scheduled and are ready to run
                                                TaskletsTimer.Start ()
                                                WorldModule.TaskletProcessingStarted <- true
                                                let world = World.processTasklets world
                                                TaskletsTimer.Stop ()
                                                match World.getLiveness world with
                                                | Live ->

                                                    // destroy simulants that have been marked for destruction at the end of frame
                                                    DestructionTimer.Start ()
                                                    let world = World.destroySimulants world
                                                    DestructionTimer.Stop ()
                                                    match World.getLiveness world with
                                                    | Live ->
                                                    
                                                        // run engine and user-defined post-process callbacks
                                                        PostProcessTimer.Start ()
                                                        let world = World.postProcess world
                                                        let world = postProcess world
                                                        PostProcessTimer.Stop ()
                                                        match World.getLiveness world with
                                                        | Live ->

                                                            // render simulants, skipping culling upon request (like if a light probe needs to be rendered)
                                                            RenderTimer.Start ()
                                                            let skipCulling = World.getUnculledRenderRequested world
                                                            let world = World.acknowledgeUnculledRenderRequest world
                                                            let world = World.renderSimulants skipCulling world
                                                            RenderTimer.Stop ()
                                                            match World.getLiveness world with
                                                            | Live ->

                                                                // process audio
                                                                AudioTimer.Start ()
                                                                let world =
                                                                    if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u then
                                                                        let audioPlayer = World.getAudioPlayer world
                                                                        let audioMessages = audioPlayer.PopMessages ()
                                                                        audioPlayer.Play audioMessages
                                                                        world
                                                                    else world
                                                                AudioTimer.Stop ()

                                                                // process rendering (1/2)
                                                                let rendererProcess = World.getRendererProcess world
                                                                if not firstFrame then rendererProcess.Swap ()

                                                                // process imgui frame
                                                                let imGui = World.getImGui world
                                                                if not firstFrame then imGui.EndFrame ()
                                                                imGui.BeginFrame ()
                                                                let world = World.imGuiProcess world
                                                                let world = imGuiProcess world
                                                                imGui.InputFrame ()
                                                                let drawData = imGui.RenderFrame ()

                                                                // avoid updating faster than desired
                                                                if FrameTimer.IsRunning then
                                                                    while FrameTimer.Elapsed.TotalSeconds < Constants.GameTime.DesiredFrameTimeMinimum do
                                                                        let timeToSleep = Constants.GameTime.DesiredFrameTimeMinimum - FrameTimer.Elapsed.TotalSeconds
                                                                        if timeToSleep > 0.016 then Thread.Sleep 16
                                                                        elif timeToSleep > 0.008 then Thread.Sleep 8
                                                                        elif timeToSleep > 0.004 then Thread.Sleep 4
                                                                        elif timeToSleep > 0.002 then Thread.Sleep 2
                                                                        elif timeToSleep > 0.001 then Thread.Sleep 1
                                                                        else Thread.Yield () |> ignore<bool>
                                                                FrameTimer.Restart ()

                                                                // process rendering (2/2)
                                                                rendererProcess.SubmitMessages
                                                                    skipCulling
                                                                    (World.getEyeFrustum3dEnclosed world)
                                                                    (World.getEyeFrustum3dExposed world)
                                                                    (World.getEyeFrustum3dImposter world)
                                                                    (World.getLightBox3d world)
                                                                    (World.getEyeCenter3d world)
                                                                    (World.getEyeRotation3d world)
                                                                    (World.getEyeCenter2d world)
                                                                    (World.getEyeSize2d world)
                                                                    (World.getWindowSize world)
                                                                    drawData

                                                                // update time and recur
                                                                TotalTimer.Stop ()
                                                                let world = World.updateTime world
                                                                WorldModule.TaskletProcessingStarted <- false
                                                                World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess liveness false world

                                                            // fin
                                                            | Dead -> world
                                                        | Dead -> world
                                                    | Dead -> world
                                                | Dead -> world
                                            | Dead -> world
                                        | Dead -> world
                                    | Dead -> world
                                | Dead -> world
                            | Dead -> world
                        | Dead -> world
                    | Dead -> world
                | Dead -> world
            else world

        /// Run the game engine using the given world and returning exit code upon termination.
        static member runWithCleanUp runWhile preProcess perProcess postProcess imGuiProcess liveness firstFrame world =
            try let world = World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess liveness firstFrame world
                World.cleanUp world
                Constants.Engine.ExitCodeSuccess
            with exn ->
                let world = World.choose world
                Log.trace (scstring exn)
                World.cleanUp world
                Constants.Engine.ExitCodeFailure

[<AutoOpen>]
module EntityDispatcherModule2 =

    type World with

        static member internal signalEntity<'model, 'message, 'command when 'message :> Message and 'command :> Command> (signal : Signal) (entity : Entity) world =
            match entity.GetDispatcher world with
            | :? EntityDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (entity.ModelGeneric<'model> ()) signal entity world
            | _ ->
                Log.info "Failed to send signal to entity."
                world

    and Entity with

        /// Send a signal to the entity, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalEntity<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for entities.
    and [<AbstractClass>] EntityDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command>
        (is2d, perimeterCentered, physical, makeInitial : World -> 'model) =
        inherit EntityDispatcher (is2d, perimeterCentered, physical)

        new (is2d, perimeterCentered, physical, initial : 'model) =
            EntityDispatcher<'model, 'message, 'command> (is2d, perimeterCentered, physical, fun _ -> initial)

        /// Get the entity's model.
        member this.GetModel (entity : Entity) world : 'model =
            entity.GetModelGeneric<'model> world

        /// Set the entity's model.
        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetModelGeneric<'model> model world

        /// The entity's model lens.
        member this.Model (entity : Entity) =
            lens (nameof this.Model) entity (this.GetModel entity) (flip this.SetModel entity)

        override this.Register (entity, world) =
            let property = World.getEntityModelProperty entity world
            let model =
                match property.DesignerValue with
                | _ when property.DesignerType = typeof<unit> -> makeInitial world
                | :? 'model as model -> model
                | null -> null :> obj :?> 'model
                | modelObj ->
                    try modelObj |> valueToSymbol |> symbolToValue
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setEntityModel<'model> true model entity world |> snd'

        override this.ApplyPhysics (center, rotation, linearVelocity, angularVelocity, entity, world) =
            let model = this.GetModel entity world
            let (signals, model) = this.Physics (center, rotation, linearVelocity, angularVelocity, model, entity, world)
            let world = this.SetModel model entity world
            Signal.processSignals this.Message this.Command (this.Model entity) signals entity world

        override this.Render (entity, world) =
            this.View (this.GetModel entity world, entity, world)

        override this.Edit (operation, entity, world) =
            let model = entity.GetModelGeneric<'model> world
            let (signals, model) = this.Edit (model, operation, entity, world)
            let world = this.SetModel model entity world
            Signal.processSignals this.Message this.Command (this.Model entity) signals entity world

        override this.Signal (signalObj : obj, entity, world) =
            match signalObj with
            | :? 'message as message -> entity.SignalPlus<'model, 'message, 'command> message world
            | :? 'command as command -> entity.SignalPlus<'model, 'message, 'command> command world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    entity.SignalPlus<'model, 'message, 'command> message world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        entity.SignalPlus<'model, 'message, 'command> command world
                    with _ ->
                        Log.debugOnce
                            ("Incompatible signal type received by entity (signal = '" + scstring signalObj + "'; entity = '" + scstring entity + "').\n" +
                             "This may come about due to sending an incorrect signal type to the entity or due to too significant a change in the signal type when reloading code.")
                        world

        override this.TryGetInitialModel<'a> world =
            makeInitial world :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, entity, world) =
            let contentOld = World.getEntityContent entity world
            let model = this.GetModel entity world
            let initializers = this.Initialize (model, entity)
            let entities = this.Content (model, entity)
            let content = Content.composite entity.Name initializers entities
            let world = Content.synchronizeEntity initializing contentOld content entity entity world
            World.setEntityContent content entity world

        override this.TryTruncateModel<'a> (model : 'a) =
            match model :> obj with
            | :? 'model as model -> Some (this.TruncateModel model :> obj :?> 'a)
            | _ -> None

        override this.TryUntruncateModel<'a> (incoming : 'a, entity, world) =
            match incoming :> obj with
            | :? 'model as incoming ->
                let current = entity.GetModelGeneric<'model> world
                Some (this.UntruncateModel (current, incoming) :> obj :?> 'a)
            | _ -> None

        /// Initialize the game's own properties.
        abstract Initialize : 'model * Entity -> InitializerContent list
        default this.Initialize (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : 'model * 'message * Entity * World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The physics synchronization handler for the MMCC programming model.
        abstract Physics : Vector3 * Quaternion * Vector3 * Vector3 * 'model * Entity * World -> Signal list * 'model
        default this.Physics (_, _, _, _, model, _, _) = just model

        /// Implements additional editing behavior for an entity via the ImGui API.
        abstract Edit : 'model * EditOperation * Entity * World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : 'model * 'command * Entity * World -> Signal list * World
        default this.Command (_, _, _, world) = just world

        /// The content specifier of the MMCC programming model.
        abstract Content : 'model * Entity -> EntityContent list
        default this.Content (_, _) = []

        /// Render the entity using the given model.
        abstract View : 'model * Entity * World -> unit
        default this.View (_, _, _) = ()

        /// Truncate the given model.
        abstract TruncateModel : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : 'model * 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

    /// A 2d entity dispatcher.
    and [<AbstractClass>] EntityDispatcher2d<'model, 'message, 'command when 'message :> Message and 'command :> Command> (perimeterCentered, physical, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, perimeterCentered, physical, makeInitial)

        new (centered, physical, initial : 'model) =
            EntityDispatcher2d<'model, 'message, 'command> (centered, physical, fun _ -> initial)

        new (physical, makeInitial : World -> 'model) =
            EntityDispatcher2d<'model, 'message, 'command> (Constants.Engine.EntityPerimeterCentered2dDefault, physical, makeInitial)

        new (physical, initial : 'model) =
            EntityDispatcher2d<'model, 'message, 'command> (physical, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize2dDefault
             define Entity.PerimeterCentered Constants.Engine.EntityPerimeterCentered2dDefault]

    /// A gui entity dispatcher.
    and [<AbstractClass>] GuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, Constants.Engine.EntityPerimeterCenteredGuiDefault, false, makeInitial)

        new (initial : 'model) =
            GuiDispatcher<'model, 'message, 'command> (fun _ -> initial)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.PerimeterCentered Constants.Engine.EntityPerimeterCenteredGuiDefault
             define Entity.Presence Omnipresent
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

    /// A 3d entity dispatcher.
    and [<AbstractClass>] EntityDispatcher3d<'model, 'message, 'command when 'message :> Message and 'command :> Command> (physical, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, true, physical, makeInitial)

        new (physical, initial : 'model) =
            EntityDispatcher3d<'model, 'message, 'command> (physical, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize3dDefault]

    /// A vui dispatcher (gui in 3d).
    and [<AbstractClass>] VuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, true, false, makeInitial)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySizeVuiDefault]

[<RequireQualifiedAccess>]
module EntityPropertyDescriptor =

    let containsPropertyDescriptor (propertyDescriptor : PropertyDescriptor) (entity : Entity) world =
        propertyDescriptor.PropertyName = Constants.Engine.NamePropertyName && propertyDescriptor.PropertyType = typeof<string> ||
        PropertyDescriptor.containsPropertyDescriptor<EntityState> propertyDescriptor entity world

    let getPropertyDescriptors (entity : Entity) world =
        let nameDescriptor = { PropertyName = Constants.Engine.NamePropertyName; PropertyType = typeof<string> }
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<EntityState> (Some entity) world
        nameDescriptor :: propertyDescriptors

    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        let baseProperties = Reflection.getPropertyDefinitions typeof<EntityDispatcher>
        let rigidBodyProperties = Reflection.getPropertyDefinitions typeof<RigidBodyFacet>
        if propertyName = "Name" || propertyName = "Surnames" || propertyName = "Model" || propertyName = "MountOpt" || propertyName = "OverlayNameOpt" then "Ambient Properties"
        elif propertyName = "FacetNames" then "Applied Facet Names"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "Built-In Properties"
        elif propertyName = "MaterialProperties" then "Material Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "Physics Properties"
        else "Xtension Properties"

    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        propertyName = "Degrees" || propertyName = "DegreesLocal" || // HACK: we allow degrees specifically for the editor.
        not (Reflection.isPropertyNonPersistentByName propertyName)

    let getValue propertyDescriptor (entity : Entity) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor entity world with
        | Some value -> value
        | None -> null

    let trySetValue (value : obj) propertyDescriptor (entity : Entity) world =

        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value

        // change property
        match propertyDescriptor.PropertyName with

        // change the surnames property
        | "Surnames" ->
            let surnames = value :?> string array
            if Array.forall (fun (name : string) -> name.IndexOfAny Symbol.IllegalNameCharsArray = -1) surnames then
                let target = Nu.Entity (entity.Group.GroupAddress <-- rtoa surnames)
                let world = World.renameEntityImmediate entity target world
                Right world
            else Left ("Invalid entity surnames '" + scstring surnames + "'.", world)

        // change the name property
        | Constants.Engine.NamePropertyName ->
            let name = value :?> string
            if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                let targetNames =
                    entity.Group.GroupAddress.Names |>
                    flip Array.append (Array.allButLast entity.Surnames) |>
                    Array.add name
                let target = Nu.Entity targetNames
                let world = World.renameEntityImmediate entity target world
                Right world
            else Left ("Invalid entity name '" + name + "'.", world)

        // change facet names
        | Constants.Engine.FacetNamesPropertyName ->
            let facetNames = value :?> string Set
            match World.trySetEntityFacetNames facetNames entity world with
            | (Right (), world) -> Right world
            | (Left error, world) -> Left (error, world)

        // change the property dynamically
        | _ ->
            match propertyDescriptor.PropertyName with
            | Constants.Engine.OverlayNameOptPropertyName ->
                match World.trySetEntityOverlayNameOpt (value :?> string option) entity world with
                | (Right (), world) -> Right world
                | (Left error, world) -> Left (error, world)
            | _ ->
                let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value entity world
                Right world

[<AutoOpen>]
module GroupDispatcherModule =

    type World with

        static member internal signalGroup<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (group : Group) world =
            match group.GetDispatcher world with
            | :? GroupDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (group.ModelGeneric<'model> ()) signal group world
            | _ ->
                Log.info "Failed to send signal to group."
                world

    and Group with

        /// Send a signal to the group, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalGroup<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for groups.
    and [<AbstractClass>] GroupDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit GroupDispatcher ()

        new (initial : 'model) =
            GroupDispatcher<'model, 'message, 'command> (fun _ -> initial)

        /// Get the group's model.
        member this.GetModel (group : Group) world : 'model =
            group.GetModelGeneric<'model> world

        /// Set the group's model.
        member this.SetModel (model : 'model) (group : Group) world =
            group.SetModelGeneric<'model> model world

        /// The group's model lens.
        member this.Model (group : Group) =
            lens (nameof this.Model) group (this.GetModel group) (flip this.SetModel group)

        override this.Register (group, world) =
            let property = World.getGroupModelProperty group world
            let model =
                match property.DesignerValue with
                | _ when property.DesignerType = typeof<unit> -> makeInitial world
                | :? 'model as model -> model
                | null -> null :> obj :?> 'model
                | modelObj ->
                    try modelObj |> valueToSymbol |> symbolToValue
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGroupModel<'model> true model group world |> snd'

        override this.Render (group, world) =
            this.View (this.GetModel group world, group, world)

        override this.Signal (signalObj : obj, group, world) =
            match signalObj with
            | :? 'message as message -> group.SignalPlus<'model, 'message, 'command> message world
            | :? 'command as command -> group.SignalPlus<'model, 'message, 'command> command world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    group.SignalPlus<'model, 'message, 'command> message world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        group.SignalPlus<'model, 'message, 'command> command world
                    with _ ->
                        Log.debugOnce
                            ("Incompatible signal type received by group (signal = '" + scstring signalObj + "'; group = '" + scstring group + "').\n" +
                             "This may come about due to sending an incorrect signal type to the group or due to too significant a change in the signal type when reloading code.")
                        world

        override this.TryGetInitialModel<'a> world =
            makeInitial world :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, group, world) =
            let contentOld = World.getGroupContent group world
            let model = this.GetModel group world
            let initializers = this.Initialize (model, group)
            let entities = this.Content (model, group)
            let content = Content.group group.Name initializers entities
            let world = Content.synchronizeGroup initializing contentOld content group group world
            World.setGroupContent content group world

        override this.TryTruncateModel<'a> (model : 'a) =
            match model :> obj with
            | :? 'model as model -> Some (this.TruncateModel model :> obj :?> 'a)
            | _ -> None

        override this.TryUntruncateModel<'a> (incoming : 'a, group, world) =
            match incoming :> obj with
            | :? 'model as incoming ->
                let current = group.GetModelGeneric<'model> world
                Some (this.UntruncateModel (current, incoming) :> obj :?> 'a)
            | _ -> None

        /// Initialize the group's own properties.
        abstract Initialize : 'model * Group -> InitializerContent list
        default this.Initialize (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : 'model * 'message * Group * World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : 'model * 'command * Group * World -> Signal list * World
        default this.Command (_, _, _, world) = just world

        /// The content specifier of the MMCC programming model.
        abstract Content : 'model * Group -> EntityContent list
        default this.Content (_, _) = []

        /// Render the group using the given model.
        abstract View : 'model * Group * World -> unit
        default this.View (_, _, _) = ()

        /// Implements additional editing behavior for a group via the ImGui API.
        abstract Edit : 'model * EditOperation * Group * World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : 'model * 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

[<RequireQualifiedAccess>]
module GroupPropertyDescriptor =

    let containsPropertyDescriptor (propertyDescriptor : PropertyDescriptor) (group : Group) world =
        PropertyDescriptor.containsPropertyDescriptor<GroupState> propertyDescriptor group world

    let getPropertyDescriptors (group : Group) world =
        PropertyDescriptor.getPropertyDescriptors<GroupState> (Some group) world

    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" ||  propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "Persistent" || propertyName = "Elevation" || propertyName = "Visible" then "Built-In Properties"
        else "Xtension Properties"

    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    let getValue propertyDescriptor (group : Group) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor group world with
        | Some value -> value
        | None -> null

    let trySetValue (value : obj) propertyDescriptor (group : Group) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a group after it has been created is not yet implemented.", world)

        // change the property dynamically
        | _ ->
            let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value group world
            Right world

[<AutoOpen>]
module ScreenDispatcherModule =

    type World with

        static member internal signalScreen<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (screen : Screen) world =
            match screen.GetDispatcher world with
            | :? ScreenDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (screen.ModelGeneric<'model> ()) signal screen world
            | _ ->
                Log.info "Failed to send signal to screen."
                world

    and Screen with

        /// Send a signal to the screen, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalScreen<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for screens.
    and [<AbstractClass>] ScreenDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit ScreenDispatcher ()

        new (initial : 'model) =
            ScreenDispatcher<'model, 'message, 'command> (fun _ -> initial)

        /// Get the screen's model.
        member this.GetModel (screen : Screen) world : 'model =
            screen.GetModelGeneric<'model> world

        /// Set the screen's model.
        member this.SetModel (model : 'model) (screen : Screen) world =
            screen.SetModelGeneric<'model> model world

        /// The screen's model lens.
        member this.Model (screen : Screen) =
            lens (nameof this.Model) screen (this.GetModel screen) (flip this.SetModel screen)

        override this.Register (screen, world) =
            let property = World.getScreenModelProperty screen world
            let model =
                match property.DesignerValue with
                | _ when property.DesignerType = typeof<unit> -> makeInitial world
                | :? 'model as model -> model
                | null -> null :> obj :?> 'model
                | modelObj ->
                    try modelObj |> valueToSymbol |> symbolToValue
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setScreenModel<'model> true model screen world |> snd'

        override this.Render (screen, world) =
            this.View (this.GetModel screen world, screen, world)

        override this.Signal (signalObj : obj, screen, world) =
            match signalObj with
            | :? 'message as message -> screen.SignalPlus<'model, 'message, 'command> message world
            | :? 'command as command -> screen.SignalPlus<'model, 'message, 'command> command world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    screen.SignalPlus<'model, 'message, 'command> message world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        screen.SignalPlus<'model, 'message, 'command> command world
                    with _ ->
                        Log.debugOnce
                            ("Incompatible signal type received by screen (signal = '" + scstring signalObj + "'; screen = '" + scstring screen + "').\n" +
                             "This may come about due to sending an incorrect signal type to the screen or due to too significant a change in the signal type when reloading code.")
                        world

        override this.TryGetInitialModel<'a> world =
            makeInitial world :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, screen, world) =
            let contentOld = World.getScreenContent screen world
            let model = this.GetModel screen world
            let initializers = this.Initialize (model, screen)
            let group = this.Content (model, screen)
            let content = Content.screen screen.Name Vanilla initializers group
            let world = Content.synchronizeScreen initializing contentOld content screen screen world
            World.setScreenContent content screen world

        override this.TryTruncateModel<'a> (model : 'a) =
            match model :> obj with
            | :? 'model as model -> Some (this.TruncateModel model :> obj :?> 'a)
            | _ -> None

        override this.TryUntruncateModel<'a> (incoming : 'a, screen, world) =
            match incoming :> obj with
            | :? 'model as incoming ->
                let current = screen.GetModelGeneric<'model> world
                Some (this.UntruncateModel (current, incoming) :> obj :?> 'a)
            | _ -> None

        /// Initialize the screen's own properties.
        abstract Initialize : 'model * Screen -> InitializerContent list
        default this.Initialize (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : 'model * 'message * Screen * World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : 'model * 'command * Screen * World -> Signal list * World
        default this.Command (_, _, _, world) = just world

        /// The content specifier of the MMCC programming model.
        abstract Content : 'model * Screen -> GroupContent list
        default this.Content (_, _) = []

        /// Render the screen using the given model.
        abstract View : 'model * Screen * World -> unit
        default this.View (_, _, _) = ()

        /// Implements additional editing behavior for a screen via the ImGui API.
        abstract Edit : 'model * EditOperation * Screen * World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : 'model * 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

[<RequireQualifiedAccess>]
module ScreenPropertyDescriptor =

    let containsPropertyDescriptor (propertyDescriptor : PropertyDescriptor) (screen : Screen) world =
        PropertyDescriptor.containsPropertyDescriptor<ScreenState> propertyDescriptor screen world

    let getPropertyDescriptors (screen : Screen) world =
        PropertyDescriptor.getPropertyDescriptors<ScreenState> (Some screen) world

    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" ||  propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "Persistent" || propertyName = "Incoming" || propertyName = "Outgoing" || propertyName = "SlideOpt" then "Built-In Properties"
        else "Xtension Properties"

    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    let getValue propertyDescriptor (screen : Screen) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor screen world with
        | Some value -> value
        | None -> null

    let trySetValue (value : obj) propertyDescriptor (screen : Screen) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a screen after it has been created is not yet implemented.", world)

        // change the property dynamically
        | _ ->
            let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value screen world
            Right world

[<AutoOpen>]
module GameDispatcherModule =

    type World with

        static member internal signalGame<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (game : Game) world =
            match game.GetDispatcher world with
            | :? GameDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (game.ModelGeneric<'model> ()) signal game world
            | _ -> Log.info "Failed to send signal to game."; world

    and Game with

        /// Send a signal to the game, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalGame<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for games.
    and [<AbstractClass>] GameDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit GameDispatcher ()

        static let synchronize initializing game world (this : GameDispatcher<'model, 'message, 'command>) =
            let contentOld = World.getGameContent game world
            let model = this.GetModel game world
            let initializers = this.Initialize (model, game)
            let screens = this.Content (model, game)
            let content = Content.game game.Name initializers screens
            let (initialScreenOpt, world) = Content.synchronizeGame World.setScreenSlide initializing contentOld content game game world
            (initialScreenOpt, World.setGameContent content game world)

        new (initial : 'model) =
            GameDispatcher<'model, 'message, 'command> (fun _ -> initial)

        /// Get the game's model.
        member this.GetModel (game : Game) world : 'model =
            game.GetModelGeneric<'model> world

        /// Set the game's model.
        member this.SetModel (model : 'model) (game : Game) world =
            game.SetModelGeneric<'model> model world

        /// The game's model lens.
        member this.Model (game : Game) =
            lens (nameof this.Model) game (this.GetModel game) (flip this.SetModel game)

        override this.Register (game, world) =
            let property = World.getGameModelProperty game world
            let model =
                match property.DesignerValue with
                | _ when property.DesignerType = typeof<unit> -> makeInitial world
                | :? 'model as model -> model
                | null -> null :> obj :?> 'model
                | modelObj ->
                    try modelObj |> valueToSymbol |> symbolToValue
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGameModel<'model> true model game world |> snd'

        override this.Render (game, world) =
            this.View (this.GetModel game world, game, world)

        override this.Signal (signalObj : obj, game, world) =
            match signalObj with
            | :? 'message as message -> game.SignalPlus<'model, 'message, 'command> message world
            | :? 'command as command -> game.SignalPlus<'model, 'message, 'command> command world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    game.SignalPlus<'model, 'message, 'command> message world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        game.SignalPlus<'model, 'message, 'command> command world
                    with _ ->
                        Log.debugOnce
                            ("Incompatible signal type received by game (signal = '" + scstring signalObj + "'; game = '" + scstring game + "').\n" +
                             "This may come about due to sending an incorrect signal type to the game or due to too significant a change in the signal type when reloading code.")
                        world

        override this.TryGetInitialModel<'a> world =
            makeInitial world :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, game, world) =
            synchronize initializing game world this |> snd

        override this.TryTruncateModel<'a> (model : 'a) =
            match model :> obj with
            | :? 'model as model -> Some (this.TruncateModel model :> obj :?> 'a)
            | _ -> None

        override this.TryUntruncateModel<'a> (incoming : 'a, game, world) =
            match incoming :> obj with
            | :? 'model as incoming ->
                let current = game.GetModelGeneric<'model> world
                Some (this.UntruncateModel (current, incoming) :> obj :?> 'a)
            | _ -> None

        /// Initialize the game's own properties.
        abstract Initialize : 'model * Game -> InitializerContent list
        default this.Initialize (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : 'model * 'message * Game * World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : 'model * 'command * Game * World -> Signal list * World
        default this.Command (_, _, _, world) = just world

        /// The content specifier of the MMCC programming model.
        abstract Content : 'model * Game -> ScreenContent list
        default this.Content (_, _) = []

        /// Render the game using the given model.
        abstract View : 'model * Game * World -> unit
        default this.View (_, _, _) = ()

        /// Implements additional editing behavior for a game via the ImGui API.
        abstract Edit : 'model * EditOperation * Game * World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : 'model * 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

[<RequireQualifiedAccess>]
module GamePropertyDescriptor =

    let containsPropertyDescriptor (propertyDescriptor : PropertyDescriptor) (game : Game) world =
        PropertyDescriptor.containsPropertyDescriptor<GameState> propertyDescriptor game world

    let getPropertyDescriptors (game : Game) world =
        PropertyDescriptor.getPropertyDescriptors<GameState> (Some game) world

    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" ||  propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "DesiredScreen" || propertyName = "OmniScreenOpt" || propertyName = "ScreenTransitionDestinationOpt" || propertyName = "SelectedScreenOpt" ||
             propertyName = "EyeCenter2d" || propertyName = "EyeSize2d" || propertyName = "EyeCenter3d" || propertyName = "EyeRotation3d" then
             "Built-In Properties"
        else "Xtension Properties"

    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    let getValue propertyDescriptor (game : Game) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor game world with
        | Some value -> value
        | None -> null

    let trySetValue (value : obj) propertyDescriptor (game : Game) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a game after it has been created is not yet implemented.", world)

        // change the property dynamically
        | _ ->
            let struct (_, _, world) = PropertyDescriptor.trySetValue propertyDescriptor value game world
            Right world

[<RequireQualifiedAccess>]
module SimulantPropertyDescriptor =

    let containsPropertyDescriptor propertyDescriptor (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.containsPropertyDescriptor propertyDescriptor entity world
        | :? Group as group -> GroupPropertyDescriptor.containsPropertyDescriptor propertyDescriptor group world
        | :? Screen as screen -> ScreenPropertyDescriptor.containsPropertyDescriptor propertyDescriptor screen world
        | :? Game as game -> GamePropertyDescriptor.containsPropertyDescriptor propertyDescriptor game world
        | _ -> failwithumf ()

    let getPropertyDescriptors (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.getPropertyDescriptors entity world
        | :? Group as group -> GroupPropertyDescriptor.getPropertyDescriptors group world
        | :? Screen as screen -> ScreenPropertyDescriptor.getPropertyDescriptors screen world
        | :? Game as game -> GamePropertyDescriptor.getPropertyDescriptors game world
        | _ -> failwithumf ()

    let getCategory propertyDesciptor (simulant : Simulant) =
        match simulant with
        | :? Entity -> EntityPropertyDescriptor.getCategory propertyDesciptor
        | :? Group -> GroupPropertyDescriptor.getCategory propertyDesciptor
        | :? Screen -> ScreenPropertyDescriptor.getCategory propertyDesciptor
        | :? Game -> GamePropertyDescriptor.getCategory propertyDesciptor
        | _ -> failwithumf ()

    let getEditable propertyDesciptor (simulant : Simulant) =
        match simulant with
        | :? Entity -> EntityPropertyDescriptor.getEditable propertyDesciptor
        | :? Group -> GroupPropertyDescriptor.getEditable propertyDesciptor
        | :? Screen -> ScreenPropertyDescriptor.getEditable propertyDesciptor
        | :? Game -> GamePropertyDescriptor.getEditable propertyDesciptor
        | _ -> failwithumf ()

    let getValue propertyDescriptor (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.getValue propertyDescriptor entity world
        | :? Group as group -> GroupPropertyDescriptor.getValue propertyDescriptor group world
        | :? Screen as screen -> ScreenPropertyDescriptor.getValue propertyDescriptor screen world
        | :? Game as game -> GamePropertyDescriptor.getValue propertyDescriptor game world
        | _ -> failwithumf ()

    let trySetValue value propertyDescriptor (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.trySetValue value propertyDescriptor entity world
        | :? Group as group -> GroupPropertyDescriptor.trySetValue value propertyDescriptor group world
        | :? Screen as screen -> ScreenPropertyDescriptor.trySetValue value propertyDescriptor screen world
        | :? Game as game -> GamePropertyDescriptor.trySetValue value propertyDescriptor game world
        | _ -> failwithumf ()

[<AutoOpen>]
module WorldModule2' =

    type World with

        /// Send a signal to a simulant.
        static member signal (signal : Signal) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.Signal signal world
            | :? Group as group -> group.Signal signal world
            | :? Screen as screen -> screen.Signal signal world
            | :? Game as game -> game.Signal signal world
            | _ -> failwithumf ()

        /// Send a signal to a simulant, explicitly specifing MMCC types.
        static member signalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> entity.SignalPlus<'model, 'message, 'command> signal world
            | :? Group as group -> group.SignalPlus<'model, 'message, 'command> signal world
            | :? Screen as screen -> screen.SignalPlus<'model, 'message, 'command> signal world
            | :? Game as game -> game.SignalPlus<'model, 'message, 'command> signal world
            | _ -> failwithumf ()

        static member internal updateLateBindings3 (latebindings : LateBindings) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity ->
                let entityState = World.getEntityState entity world
                match latebindings with
                | :? Facet as facet ->
                    match Array.tryFindIndex (fun (facet2 : Facet) -> getTypeName facet2 = getTypeName facet) entityState.Facets with
                    | Some index ->
                        if entityState.Imperative
                        then entityState.Facets.[index] <- facet; world
                        else
                            let facets = entityState.Facets.Clone () :?> Facet array
                            facets.[index] <- facet
                            let entityState = { entityState with Facets = Array.ofSeq entityState.Facets }
                            World.setEntityState entityState entity world
                    | None -> world
                | :? EntityDispatcher as entityDispatcher ->
                    if getTypeName entityState.Dispatcher = getTypeName entityDispatcher then
                        if entityState.Imperative
                        then entityState.Dispatcher <- entityDispatcher; world
                        else World.setEntityState { entityState with Dispatcher = entityDispatcher } entity world
                    else world
                | _ -> world
            | :? Group as group ->
                let groupState = World.getGroupState group world
                match latebindings with
                | :? GroupDispatcher as groupDispatcher ->
                    if getTypeName groupState.Dispatcher = getTypeName groupDispatcher
                    then World.setGroupState { groupState with Dispatcher = groupDispatcher } group world
                    else world
                | _ -> world
            | :? Screen as screen ->
                let screenState = World.getScreenState screen world
                match latebindings with
                | :? ScreenDispatcher as screenDispatcher ->
                    if getTypeName screenState.Dispatcher = getTypeName screenDispatcher
                    then World.setScreenState { screenState with Dispatcher = screenDispatcher } screen world
                    else world
                | _ -> world
            | :? Game as game ->
                let gameState = World.getGameState game world
                match latebindings with
                | :? GameDispatcher as gameDispatcher ->
                    if getTypeName gameState.Dispatcher = getTypeName gameDispatcher
                    then World.setGameState { gameState with Dispatcher = gameDispatcher } game world
                    else world
                | _ -> world
            | _ -> failwithumf ()