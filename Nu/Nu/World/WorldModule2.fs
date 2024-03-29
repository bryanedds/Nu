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
    let private CachedHashSet2dNormal = HashSet (QuadelementEqualityComparer ())
    let private CachedHashSet3dNormal = HashSet (OctelementEqualityComparer ())
    let private CachedHashSet3dShadow = HashSet (OctelementEqualityComparer ())

    (* Frame Pacing *)
    let mutable private FramePaceIssues = 0
    let mutable private FramePaceChecks = 0

    type World with

        static member internal rebuildQuadtree world =
            let quadtree = World.getQuadtree world
            Quadtree.clear quadtree
            let omniEntities =
                match World.getOmniScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                | None -> Seq.empty
            let selectedEntities =
                match World.getSelectedScreenOpt world with
                | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                | None -> Seq.empty
            let entities = Seq.append omniEntities selectedEntities
            for entity in entities do
                let bounds = entity.GetBounds world
                let visible = entity.GetVisible world || entity.GetAlwaysRender world
                let static_ = entity.GetStatic world
                let presence = entity.GetPresence world
                if entity.GetIs2d world then
                    let element = Quadelement.make visible static_ entity
                    Quadtree.addElement presence bounds.Box2 element quadtree
            world

        static member internal rebuildOctree world =
            match World.getOctreeOpt world with
            | Some octree ->
                Octree.clear octree
                let omniEntities =
                    match World.getOmniScreenOpt world with
                    | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                    | None -> Seq.empty
                let selectedEntities =
                    match World.getSelectedScreenOpt world with
                    | Some screen -> World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat
                    | None -> Seq.empty
                let entities =
                    Seq.append omniEntities selectedEntities
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
            | None -> ()
            world

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
            let eyeSize = World.getEye2dSize world
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

        static member private mapEntityDescriptors entityDescriptors =
            entityDescriptors |>
            List.map (fun descriptor ->
                match descriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                | Atom (entityName, _) | Text (entityName, _) -> (entityName, descriptor)
                | _ -> failwithumf ()) |>
            Map.ofList

        static member private propagateEntityDescriptor previousDescriptor currentDescriptor targetDescriptor (currentEntityOpt : Entity option) world =

            // propagate dispatcher at this level
            let propagatedDescriptor =
                if String.notEmpty previousDescriptor.EntityDispatcherName then
                    if targetDescriptor.EntityDispatcherName = previousDescriptor.EntityDispatcherName
                    then { targetDescriptor with EntityDispatcherName = currentDescriptor.EntityDispatcherName }
                    else targetDescriptor
                else { targetDescriptor with EntityDispatcherName = currentDescriptor.EntityDispatcherName }

            // consider using current entity as propagation source at this level
            let propagatedDescriptor =
                let propagatedDescriptor = { propagatedDescriptor with EntityProperties = Map.remove Constants.Engine.PropagatedDescriptorOptPropertyName propagatedDescriptor.EntityProperties }
                let considerUsingCurrentEntityAsPropagationSource =
                    match currentDescriptor.EntityProperties.TryGetValue Constants.Engine.PropagationSourceOptPropertyName with
                    | (true, propagationSourceOptSymbol) -> propagationSourceOptSymbol |> symbolToValue<string option> |> Option.isNone
                    | (false, _) -> true
                if considerUsingCurrentEntityAsPropagationSource then
                    match currentEntityOpt with
                    | Some currentEntity ->
                        if currentEntity.Exists world && World.hasPropagationTargets currentEntity world
                        then { propagatedDescriptor with EntityProperties = Map.add Constants.Engine.PropagationSourceOptPropertyName (valueToSymbol (Some currentEntity)) propagatedDescriptor.EntityProperties }
                        else propagatedDescriptor
                    | None -> propagatedDescriptor
                else propagatedDescriptor

            // propagate properties at this level
            let propagatedDescriptor =
                Set.ofSeq currentDescriptor.EntityProperties.Keys |>
                Set.addMany propagatedDescriptor.EntityProperties.Keys |>
                Seq.fold (fun targetDescriptor propertyName ->
                    if  propertyName <> nameof Entity.Name &&
                        propertyName <> nameof Entity.Position &&
                        propertyName <> nameof Entity.Rotation &&
                        propertyName <> nameof Entity.Elevation &&
                        propertyName <> nameof Entity.Visible &&
                        propertyName <> nameof Entity.PropagationSourceOpt &&
                        propertyName <> nameof Entity.PropagatedDescriptorOpt then
                        let currentPropertySymbolOpt =
                            match currentDescriptor.EntityProperties.TryGetValue propertyName with
                            | (true, currentPropertySymbol) -> Some currentPropertySymbol
                            | (false, _) ->
                                let overlayName =
                                    match currentDescriptor.EntityProperties.TryGetValue Constants.Engine.OverlayNameOptPropertyName with
                                    | (true, overlayNameOptSymbol) ->
                                        try let overlayNameOpt = symbolToValue<string option> overlayNameOptSymbol
                                            match overlayNameOpt with
                                            | Some overlayName -> overlayName
                                            | None -> Overlay.dispatcherNameToOverlayName currentDescriptor.EntityDispatcherName
                                        with _ -> Overlay.dispatcherNameToOverlayName currentDescriptor.EntityDispatcherName
                                    | (false, _) -> Overlay.dispatcherNameToOverlayName currentDescriptor.EntityDispatcherName
                                let facetNames =
                                    match currentDescriptor.EntityProperties.TryGetValue Constants.Engine.FacetNamesPropertyName with
                                    | (true, facetNamesSymbol) -> symbolToValue<string Set> facetNamesSymbol
                                    | (false, _) -> Set.empty
                                let overlayer = World.getOverlayer world
                                let overlaySymbols = Overlayer.getOverlaySymbols overlayName facetNames overlayer
                                match overlaySymbols.TryGetValue propertyName with
                                | (true, overlayPropertySymbol) -> Some overlayPropertySymbol
                                | (false, _) -> None
                        match currentPropertySymbolOpt with
                        | Some currentPropertySymbol ->
                            match previousDescriptor.EntityProperties.TryGetValue propertyName with
                            | (true, previousPropertySymbol) ->
                                match targetDescriptor.EntityProperties.TryGetValue propertyName with
                                | (true, targetPropertySymbol) ->
                                    if targetPropertySymbol = previousPropertySymbol
                                    then { targetDescriptor with EntityProperties = Map.add propertyName currentPropertySymbol targetDescriptor.EntityProperties }
                                    else targetDescriptor
                                | (false, _) -> { targetDescriptor with EntityProperties = Map.add propertyName currentPropertySymbol targetDescriptor.EntityProperties }
                            | (false, _) ->
                                match targetDescriptor.EntityProperties.TryGetValue propertyName with
                                | (true, targetPropertySymbol) ->
                                    let overlayName =
                                        match targetDescriptor.EntityProperties.TryGetValue Constants.Engine.OverlayNameOptPropertyName with
                                        | (true, overlayNameOptSymbol) ->
                                            try let overlayNameOpt = symbolToValue<string option> overlayNameOptSymbol
                                                match overlayNameOpt with
                                                | Some overlayName -> overlayName
                                                | None -> Overlay.dispatcherNameToOverlayName targetDescriptor.EntityDispatcherName
                                            with _ -> Overlay.dispatcherNameToOverlayName targetDescriptor.EntityDispatcherName
                                        | (false, _) -> Overlay.dispatcherNameToOverlayName targetDescriptor.EntityDispatcherName
                                    let facetNames =
                                        match targetDescriptor.EntityProperties.TryGetValue Constants.Engine.FacetNamesPropertyName with
                                        | (true, facetNamesSymbol) -> symbolToValue<string Set> facetNamesSymbol
                                        | (false, _) -> Set.empty
                                    let overlayer = World.getOverlayer world
                                    let overlaySymbols = Overlayer.getOverlaySymbols overlayName facetNames overlayer
                                    match overlaySymbols.TryGetValue propertyName with
                                    | (true, overlayPropertySymbol) ->
                                        if targetPropertySymbol = overlayPropertySymbol // property unchanged from default value
                                        then { targetDescriptor with EntityProperties = Map.add propertyName currentPropertySymbol targetDescriptor.EntityProperties }
                                        else targetDescriptor
                                    | (false, _) -> { targetDescriptor with EntityProperties = Map.add propertyName currentPropertySymbol targetDescriptor.EntityProperties }
                                | (false, _) -> { targetDescriptor with EntityProperties = Map.add propertyName currentPropertySymbol targetDescriptor.EntityProperties }
                        | None -> targetDescriptor
                    else targetDescriptor)
                    propagatedDescriptor

            // attempt to propagate entity descriptors
            let propagatedDescriptorOpts =
                let previousDescriptorMap = World.mapEntityDescriptors previousDescriptor.EntityDescriptors
                let currentDescriptorMap = World.mapEntityDescriptors currentDescriptor.EntityDescriptors
                let targetDescriptorMap = World.mapEntityDescriptors targetDescriptor.EntityDescriptors
                let keys = Set.ofSeq (previousDescriptorMap.Keys |> Seq.append currentDescriptorMap.Keys |> Seq.append targetDescriptorMap.Keys)
                let entityDescriptorOptsList = [for key in keys do (previousDescriptorMap.TryFind key, currentDescriptorMap.TryFind key, targetDescriptorMap.TryFind key)]
                List.map (fun (previousDescriptorOpt, currentDescriptorOpt, targetDescriptorOpt) ->
                    let currentEntityOpt =
                        match currentEntityOpt with
                        | Some currentEntity ->
                            match currentDescriptorOpt with
                            | Some currentDescriptor ->
                                match currentDescriptor.EntityProperties.TryGetValue Constants.Engine.NamePropertyName with
                                | (true, nameSymbol) ->
                                    match nameSymbol with
                                    | Atom (name, _) | Text (name, _) ->
                                        let currentEntity = currentEntity / name
                                        if currentEntity.Exists world
                                        then Some currentEntity
                                        else None
                                    | _ -> None
                                | (false, _) -> None
                            | None -> None
                        | None -> None
                    match (previousDescriptorOpt, currentDescriptorOpt, targetDescriptorOpt) with
                    | (Some previousDescriptor, Some currentDescriptor, Some targetDescriptor) ->
                        Some (World.propagateEntityDescriptor previousDescriptor currentDescriptor targetDescriptor currentEntityOpt world)
                    | (Some previousDescriptor, Some currentDescriptor, None) ->
                        Some (World.propagateEntityDescriptor previousDescriptor currentDescriptor EntityDescriptor.empty currentEntityOpt world)
                    | (Some _, None, None) ->
                        None
                    | (Some _, None, Some _) ->
                        None
                    | (None, None, Some targetDescriptor) ->
                        Some targetDescriptor
                    | (None, Some currentDescriptor, None) ->
                        Some currentDescriptor
                    | (None, Some currentDescriptor, Some targetDescriptor) ->
                        Some (World.propagateEntityDescriptor EntityDescriptor.empty currentDescriptor targetDescriptor currentEntityOpt world)
                    | (None, None, None) -> None)
                    entityDescriptorOptsList

            // compose fully propagated descriptor in the order they are found in the current descriptor
            let currentDescriptorsOrder =
                currentDescriptor.EntityDescriptors |>
                Seq.mapi (fun i currentDescriptor ->
                    match currentDescriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                    | Atom (entityName, _) | Text (entityName, _) -> (entityName, i)
                    | _ -> ("", Int32.MaxValue)) |>
                Map.ofSeq
            let propagatedDescriptors =
                propagatedDescriptorOpts |>
                List.definitize |>
                List.filter (fun propagatedDescriptor -> String.notEmpty propagatedDescriptor.EntityDispatcherName) |>
                List.sortBy (fun propagatedDescriptor ->
                    match propagatedDescriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                    | (Atom (entityName, _) | Text (entityName, _)) ->
                        match currentDescriptorsOrder.TryGetValue entityName with
                        | (true, order) -> order
                        | (false, _) -> Int32.MaxValue
                    | _ -> Int32.MaxValue)
            { propagatedDescriptor with EntityDescriptors = propagatedDescriptors }

        /// Propagate the structure of an entity to all other entities with it as their propagation source.
        /// TODO: expose this through Entity API.
        static member propagateEntityStructure entity world =

            // propagate entity
            let targets = World.getPropagationTargets entity world
            let currentDescriptor = World.writeEntity true EntityDescriptor.empty entity world
            let previousDescriptor = Option.defaultValue EntityDescriptor.empty (entity.GetPropagatedDescriptorOpt world)
            let world =
                Seq.fold (fun world target ->
                    if World.getEntityExists target world then
                        let targetDescriptor = World.writeEntity false EntityDescriptor.empty target world
                        let propagatedDescriptor = World.propagateEntityDescriptor previousDescriptor currentDescriptor targetDescriptor (Some entity) world
                        let order = target.GetOrder world
                        let world = World.destroyEntityImmediate target world
                        let world = World.readEntity propagatedDescriptor (Some target.Name) target.Parent world |> snd
                        let world = World.propagateEntityAffineMatrix target world
                        let world = target.SetOrder order world
                        world
                    else world)
                    world targets
            let currentDescriptor = { currentDescriptor with EntityProperties = Map.remove (nameof Entity.PropagatedDescriptorOpt) currentDescriptor.EntityProperties }
            let world = entity.SetPropagatedDescriptorOpt (Some currentDescriptor) world

            // propagate sourced ancestor entities
            seq {
                for target in World.getPropagationTargets entity world do
                    if target.Exists world then
                        for ancestor in World.getEntityAncestors target world do
                            if ancestor.Exists world && World.hasPropagationTargets ancestor world then
                                ancestor } |>
            Set.ofSeq |>
            Set.fold (fun world ancestor ->
                if ancestor.Exists world && World.hasPropagationTargets ancestor world
                then World.propagateEntityStructure ancestor world
                else world)
                world

        /// Clear all propagation targets pointing back to the given entity.
        static member clearPropagationTargets entity world =
            let targets = World.getPropagationTargets entity world
            Seq.fold (fun world target ->
                if World.getEntityExists target world
                then target.SetPropagationSourceOpt None world
                else world)
                world targets

        static member internal makeIntrinsicOverlays facets entityDispatchers =
            let requiresFacetNames = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = facets |> Map.toValueList |> List.map box
            let entityDispatchers = entityDispatchers |> Map.toValueList |> List.map box
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Overlay.makeIntrinsicOverlays requiresFacetNames sourceTypes

        static member internal handleSubscribeAndUnsubscribeEvent subscribing (eventAddress : obj Address) (_ : Simulant) world =
            // here we need to update the event publish flags for entities based on whether there are subscriptions to
            // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
            // that these optimizations do not support, and warn the developer if they are invoked. Additionally, we
            // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
            let eventNames = Address.getNames eventAddress
            let eventNamesLength = Array.length eventNames
            let world =
                if eventNamesLength >= 6 then
                    let eventFirstName = eventNames.[0]
                    match eventFirstName with
                    | "Update" ->
#if DEBUG
                        if  Array.contains Address.WildcardName eventNames ||
                            Array.contains Address.EllipsisName eventNames then
                            Log.debug
                                ("Subscribing to entity update events with a wildcard or ellipsis is not supported. " +
                                 "This will cause a bug where some entity update events are not published.")
#endif
                        let entity = Nu.Entity (Array.skip 2 eventNames)
                        World.updateEntityPublishUpdateFlag entity world |> snd'
                    | _ -> world
                else world
            let world =
                if eventNamesLength >= 4 then
                    match eventNames.[0] with
                    | "Change" ->
                        let world =
                            if eventNamesLength >= 6 then
                                let entityAddress = rtoa (Array.skip 3 eventNames)
                                let entity = Nu.Entity entityAddress
                                match World.tryGetKeyedValueFast<Guid, UMap<Entity Address, int>> (EntityChangeCountsId, world) with
                                | (true, entityChangeCounts) ->
                                    match entityChangeCounts.TryGetValue entityAddress with
                                    | (true, entityChangeCount) ->
                                        let entityChangeCount = if subscribing then inc entityChangeCount else dec entityChangeCount
                                        let entityChangeCounts =
                                            if entityChangeCount = 0
                                            then UMap.remove entityAddress entityChangeCounts
                                            else UMap.add entityAddress entityChangeCount entityChangeCounts
                                        let world =
                                            if entity.Exists world then
                                                if entityChangeCount = 0 then World.setEntityPublishChangeEvents false entity world |> snd'
                                                elif entityChangeCount = 1 then World.setEntityPublishChangeEvents true entity world |> snd'
                                                else world
                                            else world
                                        World.addKeyedValue EntityChangeCountsId entityChangeCounts world
                                    | (false, _) ->
                                        if not subscribing then failwithumf ()
                                        let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                        World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                                | (false, _) ->
                                    if not subscribing then failwithumf ()
                                    let config = World.getCollectionConfig world
                                    let entityChangeCounts = UMap.makeEmpty HashIdentity.Structural config
                                    let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                    World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                            else world
                        if  Array.contains Address.WildcardName eventNames ||
                            Array.contains Address.EllipsisName eventNames then
                            Log.debug "Subscribing to change events with a wildcard or ellipsis is not supported."
                        world
                    | _ -> world
                else world
            world

        static member internal sortSubscriptionsByElevation subscriptions world =
            EventGraph.sortSubscriptionsBy
                (fun (simulant : Simulant) _ ->
                    match simulant with
                    | :? Entity as entity -> { SortElevation = entity.GetElevation world; SortHorizon = 0.0f; SortTarget = entity } :> IComparable
                    | :? Group as group -> { SortElevation = Constants.Engine.GroupSortPriority; SortHorizon = 0.0f; SortTarget = group } :> IComparable
                    | :? Screen as screen -> { SortElevation = Constants.Engine.ScreenSortPriority; SortHorizon = 0.0f; SortTarget = screen } :> IComparable
                    | :? Game | :? GlobalSimulantGeneralized -> { SortElevation = Constants.Engine.GameSortPriority; SortHorizon = 0.0f; SortTarget = Game } :> IComparable
                    | _ -> failwithumf ())
                subscriptions
                world

        static member internal admitScreenElements screen world =
            let entities = World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat |> SList.ofSeq
            let (entities2d, entities3d) = SList.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
            let quadtree = World.getQuadtree world
            for entity in entities2d do
                let entityState = World.getEntityState entity world
                let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entity
                Quadtree.addElement entityState.Presence entityState.Bounds.Box2 element quadtree
            if SList.notEmpty entities3d then
                let octree = World.getOrCreateOctree world
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                    Octree.addElement entityState.Presence entityState.Bounds element octree
            world
                
        static member internal evictScreenElements screen world =
            let entities = World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat |> SArray.ofSeq
            let (entities2d, entities3d) = SArray.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
            let quadtree = World.getQuadtree world
            for entity in entities2d do
                let entityState = World.getEntityState entity world
                let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entity
                Quadtree.removeElement entityState.Presence entityState.Bounds.Box2 element quadtree
            if SArray.notEmpty entities3d then
                let octree = World.getOrCreateOctree world
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) (entityState.Static && not entityState.AlwaysUpdate) entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                    Octree.removeElement entityState.Presence entityState.Bounds element octree
            world

        static member internal registerScreenPhysics screen world =
            let entities =
                World.getGroups screen world |>
                Seq.map (flip World.getEntities world) |>
                Seq.concat |>
                SList.ofSeq
            SList.fold (fun world (entity : Entity) ->
                World.registerEntityPhysics entity world)
                world entities

        static member internal unregisterScreenPhysics screen world =
            let entities =
                World.getGroups screen world |>
                Seq.map (flip World.getEntities world) |>
                Seq.concat |>
                SList.ofSeq
            SList.fold (fun world (entity : Entity) ->
                World.unregisterEntityPhysics entity world)
                world entities

        static member internal signalFn (signalObj : obj) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> (entity.GetDispatcher world).Signal (signalObj, entity, world)
            | :? Group as group -> (group.GetDispatcher world).Signal (signalObj, group, world)
            | :? Screen as screen -> (screen.GetDispatcher world).Signal (signalObj, screen, world)
            | :? Game as game -> (game.GetDispatcher world).Signal (signalObj, game, world)
            | _ -> failwithumf ()

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

                    // update and apply overlays to all entities
                    let world = World.setOverlayer overlayer world
                    let entities = World.getEntities1 world
                    let world = Seq.fold (World.applyEntityOverlay overlayerOld overlayer) world entities
                    (Right overlayer, world)

                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.switch world)

        /// Send a message to the subsystems to reload their existing assets.
        static member reloadExistingAssets world =
            let world = World.reloadRenderAssets2d world
            let world = World.reloadRenderAssets3d world
            let world = World.reloadAudioAssets world
            let world = World.reloadSymbols world
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

                    // rebuild and reload assets
                    AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory false assetGraph
                    Metadata.regenerateMetadata ()
                    let world = World.reloadExistingAssets world
                    let world = World.publishPlus () Nu.Game.Handle.AssetsReloadEvent (EventTrace.debug "World" "publishAssetsReload" "" EventTrace.empty) Nu.Game.Handle false false world
                    (Right assetGraph, world)

                // propagate errors
                | Left error -> (Left error, world)
            with exn -> (Left (scstring exn), World.switch world)

        /// Reload asset graph, build assets, then reload built assets.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssets world =
            let targetDir = AppDomain.CurrentDomain.BaseDirectory
            let assetSourceDir = PathF.GetFullPath (targetDir + "../../..")
            match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world with
            | (Right _, world) -> (true, world)
            | (Left _, world) -> (false, world)

        /// Switch simulation to this world, resynchronizing the imperative subsystems with its current state.
        /// Needed when abandoning execution of the current world in favor of an old world, such as in the case of an
        /// exception where the try expression resulted in a transformed world that is to be discarded.
        static member switch (world : World) =

            // manually choose world to override choose count check
            WorldTypes.Chosen <- world

            // sync tick watch state to advancing
            let world = World.switchAmbientState world

            // rebuild spatial trees
            let world = World.rebuildOctree world
            let world = World.rebuildQuadtree world

            // clear existing physics
            let world = World.handlePhysicsMessage3d ClearPhysicsMessageInternal world
            let world = World.handlePhysicsMessage2d ClearPhysicsMessageInternal world

            // register the physics of entities in the current screen
            match World.getSelectedScreenOpt world with
            | Some screen ->
                let groups = World.getGroups screen world
                Seq.fold (fun world (group : Group) ->
                    if group.Exists world then
                        let entities = World.getEntities group world
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
                    if not (io.WantCaptureMouseGlobal) then
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
                    if not (io.WantCaptureMouseGlobal) then
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
                    if evt.wheel.preciseY <> 0.0f then
                        let flipped = evt.wheel.direction = uint SDL.SDL_MouseWheelDirection.SDL_MOUSEWHEEL_FLIPPED
                        let travel = evt.wheel.preciseY * if flipped then -1.0f else 1.0f
                        imGui.HandleMouseWheelChange travel
                        let eventData = { Travel = travel }
                        let eventTrace = EventTrace.debug "World" "processInput" "MouseWheel" EventTrace.empty
                        World.publishPlus eventData Nu.Game.Handle.MouseWheelEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_TEXTINPUT ->
                    let io = ImGui.GetIO ()
                    let imGui = World.getImGui world
                    let textInput = char evt.text.text.FixedElementField
                    imGui.HandleKeyChar textInput
                    if not (io.WantCaptureKeyboardGlobal) then
                        let eventData = { TextInput = textInput }
                        let eventTrace = EventTrace.debug "World" "processInput" "TextInput" EventTrace.empty
                        World.publishPlus eventData Nu.Game.Handle.TextInputEvent eventTrace Nu.Game.Handle true true world
                    else world
                | SDL.SDL_EventType.SDL_KEYDOWN ->
                    let io = ImGui.GetIO ()
                    if not (io.WantCaptureKeyboardGlobal) then
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
                    if not (io.WantCaptureKeyboardGlobal) then
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
                                let bodyMotion = entity.GetBodyMotion world
                                if  (match bodyMotion with MixedMotion | ManualMotion -> true | _ -> false) ||
                                    bodyId.BodyIndex <> Constants.Physics.InternalIndex then
                                    let transformData =
                                        { BodyCenter = center
                                          BodyRotation = rotation
                                          BodyLinearVelocity = linearVelocity
                                          BodyAngularVelocity = angularVelocity }
                                    let transformAddress = entity.BodyTransformEvent
                                    let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                                    World.publishPlus transformData transformAddress eventTrace Nu.Game.Handle false false world
                                else entity.ApplyPhysics center rotation linearVelocity angularVelocity world
                            else world
                        else world
                    | _ -> world
            | Dead -> world

        static member private getElements2dBy (getElementsFromQuadree : Entity Quadtree -> Entity Quadelement seq) world =
            let quadtree = World.getQuadtree world
            let elements = getElementsFromQuadree quadtree
            (elements, world)

        static member private getElements2dInView set world =
            let viewBounds = World.getViewBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInView viewBounds set) world

        static member private getElements2dInPlay set world =
            let playBounds = World.getPlayBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInPlay playBounds set) world

        static member private getElements2d set world =
            World.getElements2dBy (Quadtree.getElements set) world

        static member private getEntities2dBy getElementsFromQuadtree world =
            let quadtree = World.getQuadtree world
            let elements = getElementsFromQuadtree quadtree
            let entities = Seq.map (fun (element : Entity Quadelement) -> element.Entry) elements
            (entities, world)

        /// Get all 2d entities in the given bounds, including all uncullable entities.
        static member getEntities2dInBounds bounds set world =
            World.getEntities2dBy (Quadtree.getElementsInBounds bounds set) world

        /// Get all 2d entities at the given point, including all uncullable entities.
        static member getEntities2dAtPoint point set world =
            World.getEntities2dBy (Quadtree.getElementsAtPoint point set) world

        /// Get all 2d entities in the current 2d view, including all uncullable entities.
        static member getEntities2dInView set world =
            let viewBounds = World.getViewBounds2dRelative world
            World.getEntities2dBy (Quadtree.getElementsInView viewBounds set) world

        /// Get all 2d entities needing to update for the current 2d play zone, including all uncullable entities.
        static member getEntities2dInPlay set world =
            let playBounds = World.getPlayBounds2dRelative world
            World.getEntities2dBy (Quadtree.getElementsInPlay playBounds set) world

        /// Get all 2d entities in the current selected screen, including all uncullable entities.
        static member getEntities2d set world =
            World.getEntities2dBy (Quadtree.getElements set) world

        static member private getElements3dBy (getElementsFromOctree : Entity Octree -> Entity Octelement seq) world =
            match World.getOctreeOpt world with
            | Some octree ->
                let elements = getElementsFromOctree octree
                (elements, world)
            | None -> (Seq.empty, world)

        static member private getElements3dInPlay set world =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            World.getElements3dBy (Octree.getElementsInPlay playBox playFrustum set) world

        static member private getElements3dInViewFrustum interior exterior frustum set world =
            World.getElements3dBy (Octree.getElementsInViewFrustum interior exterior frustum set) world

        static member private getElements3dInView set world =
            let frustumInterior = World.getEye3dFrustumInterior world
            let frustumExterior = World.getEye3dFrustumExterior world
            let frustumImposter = World.getEye3dFrustumImposter world
            let lightBox = World.getLight3dBox world
            World.getElements3dBy (Octree.getElementsInView frustumInterior frustumExterior frustumImposter lightBox set) world

        static member private getElements3d set world =
            World.getElements3dBy (Octree.getElements set) world

        static member private getEntities3dBy getElementsFromOctree world =
            match World.getOctreeOpt world with
            | Some octree ->
                let elements = getElementsFromOctree octree
                let entities = Seq.map (fun (element : Entity Octelement) -> element.Entry) elements
                (entities, world)
            | None -> (Seq.empty, world)

        /// Get all 3d entities in the given bounds, including all uncullable entities.
        static member getEntities3dInBounds bounds set world =
            World.getEntities3dBy (Octree.getElementsInBounds bounds set) world

        /// Get all 3d entities at the given point, including all uncullable entities.
        static member getEntities3dAtPoint point set world =
            World.getEntities3dBy (Octree.getElementsAtPoint point set) world

        /// Get all 3d entities in the current 3d play zone, including all uncullable entities.
        static member getEntities3dInPlay set world =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            World.getEntities3dBy (Octree.getElementsInPlay playBox playFrustum set) world

        /// Get all 3d entities in the current 3d view, including all uncullable entities.
        static member getEntities3dInView set world =
            let frustumInterior = World.getEye3dFrustumInterior world
            let frustumExterior = World.getEye3dFrustumExterior world
            let frustumImposter = World.getEye3dFrustumImposter world
            let lightBox = World.getLight3dBox world
            World.getEntities3dBy (Octree.getElementsInView frustumInterior frustumExterior frustumImposter lightBox set) world

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable light probes.
        static member getLightProbes3dInFrustum frustum set world =
            World.getEntities3dBy (Octree.getLightProbesInFrustum frustum set) world

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable lights.
        static member getLightProbes3dInBox box set world =
            World.getEntities3dBy (Octree.getLightProbesInBox box set) world

        /// Get all 3d light entities in the current 3d light box, including all uncullable lights.
        static member getLights3dInFrustum frustum set world =
            World.getEntities3dBy (Octree.getLightsInFrustum frustum set) world

        /// Get all 3d light entities in the current 3d light box, including all uncullable lights.
        static member getLights3dInBox box set world =
            World.getEntities3dBy (Octree.getLightsInBox box set) world

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
            let (elements3d, world) = World.getElements3dInPlay CachedHashSet3dNormal world
            let (elements2d, world) = World.getElements2dInPlay CachedHashSet2dNormal world
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
            let world =
                Seq.fold (fun world (element : Entity Octelement) ->
                    if element.Entry.GetAlwaysUpdate world || advancing && not (element.Entry.GetStatic world)
                    then World.updateEntity element.Entry world
                    else world)
                    world elements3d
            let world =
                Seq.fold (fun world (element : Entity Quadelement) ->
                    if element.Entry.GetAlwaysUpdate world || advancing && not (element.Entry.GetStatic world)
                    then World.updateEntity element.Entry world
                    else world)
                    world elements2d
            UpdateEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3dNormal.Clear ()
            CachedHashSet2dNormal.Clear ()

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
                      AssetTag = dissolveImage
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
            | IncomingState transitionTime -> World.renderScreenTransition5 transitionTime (World.getEye2dCenter world) (World.getEye2dSize world) screen (screen.GetIncoming world) world
            | OutgoingState transitionTime -> World.renderScreenTransition5 transitionTime (World.getEye2dCenter world) (World.getEye2dSize world) screen (screen.GetOutgoing world) world
            | IdlingState _ -> ()

        static member private renderSimulantsInternal renderPass world =

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
            let (elements3d, world) =
                match renderPass with
                | NormalPass skipCulling ->
                    if skipCulling
                    then World.getElements3d CachedHashSet3dNormal world
                    else World.getElements3dInView CachedHashSet3dNormal world
                | ShadowPass (_, shadowDirectional, shadowFrustum) -> World.getElements3dInViewFrustum (not shadowDirectional) true shadowFrustum CachedHashSet3dNormal world
                | ReflectionPass _ -> (Seq.empty, world)
            let (elements2d, world) =
                match renderPass with
                | NormalPass skipCulling ->
                    if skipCulling
                    then World.getElements2d CachedHashSet2dNormal world
                    else World.getElements2dInView CachedHashSet2dNormal world
                | ShadowPass _ -> (Seq.empty, world)
                | ReflectionPass _ -> (Seq.empty, world)
            RenderGatherTimer.Stop ()

            // render simulants breadth-first
            World.renderGame renderPass game world
            for screen in screens do
                World.renderScreen renderPass screen world
            match World.getSelectedScreenOpt world with Some selectedScreen -> World.renderScreenTransition selectedScreen world | None -> ()
            for group in groups do
                if not (groupsInvisible.Contains group) then
                    World.renderGroup renderPass group world

            // render entities
            RenderEntitiesTimer.Start ()
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements3d do
                    if element.Visible then
                        World.renderEntity renderPass element.Entry world
            else
                for element in elements3d do
                    if element.Visible && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity renderPass element.Entry world
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements2d do
                    if element.Visible then
                        World.renderEntity renderPass element.Entry world
            else
                for element in elements2d do
                    if element.Visible && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity renderPass element.Entry world
            RenderEntitiesTimer.Stop ()

            // clear cached hash sets
            CachedHashSet3dNormal.Clear ()
            CachedHashSet2dNormal.Clear ()

            // fin
            world

        static member private renderSimulants skipCulling world =

            // create shadow pass descriptors
            let lightBox = World.getLight3dBox world
            let (lights, world) = World.getLights3dInBox lightBox CachedHashSet3dShadow world // NOTE: this may not be the optimal way to query.
            let eyeCenter = World.getEye3dCenter world
            let sortableShadowPassDescriptors =
                [|for light in lights do
                    if light.GetDesireShadows world then
                        let (directional, coneOuter) =
                            match light.GetLightType world with
                            | PointLight -> (false, MathF.TWO_PI)
                            | SpotLight (_, coneOuter)-> (false, coneOuter)
                            | DirectionalLight -> (true, 0.0f)
                        let (shadowView, shadowProjection) =
                            if not directional then
                                let shadowRotation = light.GetRotation world
                                let mutable shadowView = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion shadowRotation
                                shadowView.Translation <- light.GetPosition world
                                shadowView <- shadowView.Inverted
                                let shadowFov = max (min coneOuter Constants.Render.ShadowFovMax) 0.01f
                                let shadowCutoff = max (light.GetLightCutoff world) 0.1f
                                let shadowProjection = Matrix4x4.CreatePerspectiveFieldOfView (shadowFov, 1.0f, Constants.Render.NearPlaneDistanceInterior, shadowCutoff)
                                (shadowView, shadowProjection)
                            else
                                let shadowRotation = light.GetRotation world
                                let mutable shadowView = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion shadowRotation
                                shadowView.Translation <- light.GetPosition world
                                shadowView <- shadowView.Inverted
                                let shadowCutoff = light.GetLightCutoff world
                                let shadowProjection = Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)
                                (shadowView, shadowProjection)
                        let shadowFrustum =
                            Frustum (shadowView * shadowProjection)
                        let shadowInView =
                            let frustumInterior = World.getEye3dFrustumInterior world
                            let frustumExterior = World.getEye3dFrustumExterior world
                            let frustumImposter = World.getEye3dFrustumImposter world
                            match light.GetPresence world with
                            | Interior -> frustumInterior.Intersects shadowFrustum
                            | Exterior -> frustumExterior.Intersects shadowFrustum || frustumInterior.Intersects shadowFrustum
                            | Imposter -> frustumImposter.Intersects shadowFrustum
                            | Omnipresent -> true
                        if shadowInView then
                            let directionalSort = if not directional then 1 else 0
                            let distanceSquared = Vector3.DistanceSquared (eyeCenter, light.GetPosition world)
                            struct (struct (directionalSort, distanceSquared), struct (shadowFrustum, light))|]

            // render simulant shadows in descriptor sort order
            let world =
                sortableShadowPassDescriptors |>
                Array.sortBy fst' |>
                Array.tryTake Constants.Render.ShadowsMax |>
                Array.fold (fun world struct (struct (directionalSort, _), struct (shadowFrustum, light)) ->
                    World.renderSimulantsInternal (ShadowPass (light.GetId world, isZero directionalSort, shadowFrustum)) world)
                    world

            // render simulants normally, remember to clear 3d shadow cache
            let world = World.renderSimulantsInternal (NormalPass skipCulling) world
            CachedHashSet3dShadow.Clear ()
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
            match physicsEngine.TryIntegrate world.GameDelta with
            | Some integrationMessages ->
                let eventTrace = EventTrace.debug "World" "processPhysics2d" "" EventTrace.empty
                let world = World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
                let world = Seq.fold (flip World.processIntegrationMessage) world integrationMessages
                world
            | None -> world

        static member private processPhysics3d world =
            let physicsEngine = World.getPhysicsEngine3d world
            match physicsEngine.TryIntegrate world.GameDelta with
            | Some integrationMessages ->
                let eventTrace = EventTrace.debug "World" "processPhysics3d" "" EventTrace.empty
                let world = World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
                let world = Seq.fold (flip World.processIntegrationMessage) world integrationMessages
                world
            | None -> world

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
        static member runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess liveness firstFrame world =

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

                                                            // render simulants, skipping culling upon request (like when a light probe needs to be rendered)
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

                                                                // process frame pacing mechanics
                                                                let world =
                                                                    if FrameTimer.IsRunning then

                                                                        // automatically enable frame pacing when need is detected
                                                                        let world =
                                                                            if not world.FramePacing then
                                                                                if FrameTimer.Elapsed.TotalSeconds < Constants.GameTime.DesiredFrameTimeMinimum * 0.9 then FramePaceIssues <- inc FramePaceIssues
                                                                                FramePaceChecks <- inc FramePaceChecks
                                                                                let world = if FramePaceIssues = 15 then World.setFramePacing true world else world
                                                                                if FramePaceChecks % 30 = 0 then FramePaceIssues <- 0
                                                                                world
                                                                            else world

                                                                        // pace frame when enabled
                                                                        if world.FramePacing then
                                                                            while FrameTimer.Elapsed.TotalSeconds < Constants.GameTime.DesiredFrameTimeMinimum do
                                                                                let timeToSleep = Constants.GameTime.DesiredFrameTimeMinimum - FrameTimer.Elapsed.TotalSeconds
                                                                                if timeToSleep > 0.008 then Thread.Sleep 7
                                                                                elif timeToSleep > 0.004 then Thread.Sleep 3
                                                                                elif timeToSleep > 0.002 then Thread.Sleep 1
                                                                                else Thread.Yield () |> ignore<bool> // NOTE: this seems to cause 100% core utilizaiton on linux. Perhaps we should special case for linux to use Sleep 0|1 instead?
                                                                        
                                                                        // fin
                                                                        world
                                                                    else world
                                                                FrameTimer.Restart ()

                                                                // process imgui frame
                                                                let imGui = World.getImGui world
                                                                if not firstFrame then imGui.EndFrame ()
                                                                imGui.BeginFrame ()
                                                                let world = World.imGuiProcess world
                                                                let world = imGuiProcess world
                                                                imGui.InputFrame ()
                                                                let drawData = imGui.RenderFrame ()

                                                                // process rendering (2/2)
                                                                rendererProcess.SubmitMessages
                                                                    skipCulling
                                                                    (World.getEye3dFrustumInterior world)
                                                                    (World.getEye3dFrustumExterior world)
                                                                    (World.getEye3dFrustumImposter world)
                                                                    (World.getLight3dBox world)
                                                                    (World.getEye3dCenter world)
                                                                    (World.getEye3dRotation world)
                                                                    (World.getEye2dCenter world)
                                                                    (World.getEye2dSize world)
                                                                    (World.getWindowSize world)
                                                                    drawData

                                                                // post-process imgui frame
                                                                let world = World.imGuiPostProcess world
                                                                let world = imGuiPostProcess world

                                                                // update time and recur
                                                                TotalTimer.Stop ()
                                                                WorldModule.TaskletProcessingStarted <- false
                                                                let world = World.updateTime world
                                                                let world =
                                                                    if world.Advancing then
                                                                        let world = World.publish () (Events.TimeUpdateEvent --> Game) Game world
                                                                        match World.getSelectedScreenOpt world with
                                                                        | Some selectedScreen ->
                                                                            let world = World.publish () (Events.TimeUpdateEvent --> selectedScreen) selectedScreen world
                                                                            let groups = World.getGroups selectedScreen world
                                                                            Seq.fold (fun world (group : Group) ->
                                                                                if group.Exists world
                                                                                then World.publish () (Events.TimeUpdateEvent --> group) group world
                                                                                else world)
                                                                                world groups
                                                                        | None -> world
                                                                    else world

                                                                // recur or return
                                                                match World.getLiveness world with
                                                                | Live -> World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess liveness false world
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
                | Dead -> world
            else world

        /// Run the game engine using the given world and returning exit code upon termination.
        static member runWithCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess liveness firstFrame world =
            try let world = World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess liveness firstFrame world
                World.cleanUp world
                Constants.Engine.ExitCodeSuccess
            with exn ->
                let world = World.switch world
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
                    try let model = modelObj |> valueToSymbol |> symbolToValue
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setEntityModel<'model> true model entity world |> snd'

        override this.ApplyPhysics (center, rotation, linearVelocity, angularVelocity, entity, world) =
            let model = this.GetModel entity world
            let (signals, model) = this.Physics (center, rotation, linearVelocity, angularVelocity, model, entity, world)
            let world = this.SetModel model entity world
            Signal.processSignals this.Message this.Command (this.Model entity) signals entity world

        override this.Render (renderPass, entity, world) =
            this.Render (this.GetModel entity world, renderPass, entity, world)

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
        abstract Render : 'model * RenderPass * Entity * World -> unit
        default this.Render (_, _, _, _) = ()

        /// Truncate the given model.
        abstract TruncateModel : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : 'model * 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

    /// A 2d entity dispatcher.
    and [<AbstractClass>] Entity2dDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (perimeterCentered, physical, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, perimeterCentered, physical, makeInitial)

        new (centered, physical, initial : 'model) =
            Entity2dDispatcher<'model, 'message, 'command> (centered, physical, fun _ -> initial)

        new (physical, makeInitial : World -> 'model) =
            Entity2dDispatcher<'model, 'message, 'command> (Constants.Engine.EntityPerimeterCentered2dDefault, physical, makeInitial)

        new (physical, initial : 'model) =
            Entity2dDispatcher<'model, 'message, 'command> (physical, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity2dSizeDefault
             define Entity.PerimeterCentered Constants.Engine.EntityPerimeterCentered2dDefault]

    /// A gui entity dispatcher.
    and [<AbstractClass>] GuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, Constants.Engine.EntityPerimeterCenteredGuiDefault, false, makeInitial)

        new (initial : 'model) =
            GuiDispatcher<'model, 'message, 'command> (fun _ -> initial)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Size Constants.Engine.EntityGuiSizeDefault
             define Entity.PerimeterCentered Constants.Engine.EntityPerimeterCenteredGuiDefault
             define Entity.Presence Omnipresent
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

    /// A 3d entity dispatcher.
    and [<AbstractClass>] Entity3dDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (physical, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, true, physical, makeInitial)

        new (physical, initial : 'model) =
            Entity3dDispatcher<'model, 'message, 'command> (physical, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity3dSizeDefault]

    /// A vui dispatcher (gui in 3d).
    and [<AbstractClass>] VuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, true, false, makeInitial)

        static member Properties =
            [define Entity.Size Constants.Engine.EntityVuiSizeDefault]

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
        if propertyName = "Name" || propertyName = "Surnames" || propertyName = "Model" || propertyName = "MountOpt" || propertyName = "PropagationSourceOpt" || propertyName = "OverlayNameOpt" then "Ambient Properties"
        elif propertyName = "Degrees" || propertyName = "DegreesLocal" ||
             propertyName = "Elevation" || propertyName = "ElevationLocal" ||
             propertyName = "Offset" || propertyName = "Overflow" ||
             propertyName = "Position" || propertyName = "PositionLocal" ||
             propertyName = "Presence" ||
             propertyName = "Rotation" || propertyName = "RotationLocal" ||
             propertyName = "Scale" || propertyName = "ScaleLocal" ||
             propertyName = "Size" then
             "Basic Transform Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "Configuration Properties"
        elif propertyName = "MaterialProperties" || propertyName = "Material" then "Material Properties"
        elif propertyName = "NavShape" || propertyName = "Nav3dConfig" then "Navigation Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "Physics Properties"
        else "Uncategorized Properties"

    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if  propertyName = Constants.Engine.OverlayNameOptPropertyName ||
            propertyName = Constants.Engine.FacetNamesPropertyName ||
            propertyName = Constants.Engine.PropagatedDescriptorOptPropertyName ||
            propertyName = "Rotation" ||
            propertyName = "RotationLocal" then
            false
        else
            propertyName = "Degrees" ||
            propertyName = "DegreesLocal" ||
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
                    try let model = modelObj |> valueToSymbol |> symbolToValue
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGroupModel<'model> true model group world |> snd'

        override this.Render (renderPass, group, world) =
            this.Render (this.GetModel group world, renderPass, group, world)

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
        abstract Render : 'model * RenderPass * Group * World -> unit
        default this.Render (_, _, _, _) = ()

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
                    try let model = modelObj |> valueToSymbol |> symbolToValue
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setScreenModel<'model> true model screen world |> snd'

        override this.Render (renderPass, screen, world) =
            this.Render (this.GetModel screen world, renderPass, screen, world)

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
        abstract Render : 'model * RenderPass * Screen * World -> unit
        default this.Render (_, _, _, _) = ()

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
                    try let model = modelObj |> valueToSymbol |> symbolToValue
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGameModel<'model> true model game world |> snd'

        override this.Render (renderPass, game, world) =
            this.Render (this.GetModel game world, renderPass, game, world)

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
        abstract Render : 'model * RenderPass * Game * World -> unit
        default this.Render (_, _, _, _) = ()

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
             propertyName = "Eye2dCenter" || propertyName = "Eye2dSize" || propertyName = "Eye3dCenter" || propertyName = "Eye3dRotation" then
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
                            let entityState = { entityState with Facets = facets }
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