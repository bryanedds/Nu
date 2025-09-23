// Nu Game Engine.
// Copyright (C) Bryan Edds.

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

/// Universal function definitions for the world (2/4).
[<AutoOpen>]
module WorldModule2 =

    (* Transition Values *)
    let private ScreenTransitionMouseLeftId = Gen.id64
    let private ScreenTransitionMouseMiddleId = Gen.id64
    let private ScreenTransitionMouseRightId = Gen.id64
    let private ScreenTransitionMouseX1Id = Gen.id64
    let private ScreenTransitionMouseX2Id = Gen.id64
    let private ScreenTransitionKeyboardKeyId = Gen.id64

    (* Cached HashSets *)
    let private HashSet2dNormalCached = HashSet (QuadelementEqualityComparer ())
    let private HashSet3dNormalCached = HashSet (OctelementEqualityComparer ())
    let private HashSet3dShadowCached = HashSet (OctelementEqualityComparer ())

    (* Frame Pacing *)
    let mutable private FramePaceIssues = 0
    let mutable private FramePaceChecks = 0

    (* Cached ImSim Collections *)
    let private ImSimSimulantsToDestroy = List ()
    let private SimulantImSimComparer = Comparer<int64 * Simulant>.Create (fun (a, _) (b, _) -> a.CompareTo b)

    type World with

        /// Set whether the world state is advancing.
        static member setAdvancing advancing (world : World) =
            if world.ContextImSim.Names.Length = 0 then
                World.defer (World.mapAmbientState (AmbientState.setAdvancing advancing)) Nu.Game.Handle world
            else

                // HACK: in order to avoid unintentional interaction with the ImSim hack that clears and restores
                // advancement state ImSim contexts, we schedule the advancement change outside of the normal workflow.
                let time = if EndFrameProcessingStarted && world.Advancing then GameTime.epsilon else GameTime.zero
                World.addTasklet Nu.Game.Handle { ScheduledTime = time; ScheduledOp = World.mapAmbientState (AmbientState.setAdvancing advancing) } world

        /// Select the given screen without transitioning, even if another transition is taking place.
        static member internal selectScreenOpt transitionStateAndScreenOpt world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                let deselecting =
                    match transitionStateAndScreenOpt with
                    | Some (_, screen) when selectedScreen = screen -> false
                    | Some _ | None -> true
                if deselecting then
                    let eventTrace = EventTrace.debug "World" "selectScreen" "Deselecting" EventTrace.empty
                    World.publishPlus () selectedScreen.DeselectingEvent eventTrace selectedScreen false false world
            | None -> ()
            match transitionStateAndScreenOpt with
            | Some (transitionState, screen) ->
                match World.getSelectedScreenOpt world with
                | Some selectedScreen ->
                    let select =
                        match transitionStateAndScreenOpt with
                        | Some (_, screen) when selectedScreen = screen -> false
                        | Some _ | None -> true
                    if select then
                        World.setSelectedScreen screen world
                        let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                        World.publishPlus () screen.SelectEvent eventTrace screen false false world
                | None ->
                    World.setSelectedScreen screen world
                    let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                    World.publishPlus () screen.SelectEvent eventTrace screen false false world
                World.setScreenTransitionStatePlus transitionState screen world
            | None -> World.setSelectedScreenOpt None world

        /// Select the given screen without transitioning, even if another transition is taking place.
        static member selectScreen transitionState screen world =
            World.selectScreenOpt (Some (transitionState, screen)) world

        /// Try to check that the selected screen is idling; that is, neither transitioning in or
        /// out via another screen.
        static member tryGetSelectedScreenIdling world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> Some (selectedScreen.GetIdling world)
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
            screen.SetTransitionState state world
            match state with
            | IdlingState _ ->
                World.unsubscribe ScreenTransitionMouseLeftId world
                World.unsubscribe ScreenTransitionMouseMiddleId world
                World.unsubscribe ScreenTransitionMouseRightId world
                World.unsubscribe ScreenTransitionMouseX1Id world
                World.unsubscribe ScreenTransitionMouseX2Id world
                World.unsubscribe ScreenTransitionKeyboardKeyId world
            | IncomingState _ | OutgoingState _ ->
                World.subscribePlus ScreenTransitionMouseLeftId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Left/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                World.subscribePlus ScreenTransitionMouseMiddleId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Middle/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                World.subscribePlus ScreenTransitionMouseRightId World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/Right/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                World.subscribePlus ScreenTransitionMouseX1Id World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/X1/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                World.subscribePlus ScreenTransitionMouseX2Id World.handleAsSwallow (stoa<MouseButtonData> ("Mouse/X2/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                World.subscribePlus ScreenTransitionKeyboardKeyId World.handleAsSwallow (stoa<KeyboardKeyData> ("KeyboardKey/" + Constants.Address.WildcardName + "/Event/Game")) Nu.Game.Handle world |> ignore
                
        static member private updateScreenTransition3 transitionType (selectedScreen : Screen) world =
            let transition =
                match transitionType with
                | Incoming -> selectedScreen.GetIncoming world
                | Outgoing -> selectedScreen.GetOutgoing world
            let transitionTime = (selectedScreen.GetTransitionState world).TransitionTime
            match (transitionTime, transition.TransitionLifeTime) with
            | (UpdateTime time, UpdateTime lifeTime) ->
                let localTime = world.UpdateTime - time
                localTime - 2L >= lifeTime
            | (TickTime time, TickTime lifeTime) ->
                let localTime = world.TickTime - time
                localTime - world.TickDelta * 2L >= lifeTime
            | (_, _) -> failwithumf ()

        static member private updateScreenIdling3 transitionTime slide (_ : Screen) (world : World) =
            match (transitionTime, slide.IdlingTime) with
            | (UpdateTime time, UpdateTime lifeTime) ->
                let localTime = world.UpdateTime - time
                localTime - 2L >= lifeTime
            | (TickTime time, TickTime lifeTime) ->
                let localTime = world.TickTime - time
                localTime - world.TickDelta * 2L >= lifeTime
            | (_, _) -> failwithumf ()

        static member private updateScreenIncoming transitionTime (selectedScreen : Screen) (world : World) =
            if world.Alive then
                if transitionTime = world.GameTime then
                    let eventTrace = EventTrace.debug "World" "updateScreenIncoming" "IncomingStart" EventTrace.empty
                    World.publishPlus () selectedScreen.IncomingStartEvent eventTrace selectedScreen false false world
                    match (selectedScreen.GetIncoming world).SongOpt with
                    | Some playSong ->
                        match World.getSongOpt world with
                        | Some song when assetEq song.Song playSong.Song -> () // do nothing when song is the same
                        | _ -> World.playSong playSong.FadeInTime playSong.FadeOutTime GameTime.zero playSong.RepeatLimitOpt playSong.Volume playSong.Song world // play song when song is different
                    | None -> ()
                if world.Alive then
                    if World.updateScreenTransition3 Incoming selectedScreen world then
                        let eventTrace = EventTrace.debug "World" "updateScreenIncoming" "IncomingFinish" EventTrace.empty
                        World.setScreenTransitionStatePlus (IdlingState world.GameTime) selectedScreen world
                        World.publishPlus () selectedScreen.IncomingFinishEvent eventTrace selectedScreen false false world

        static member private updateScreenIdling transitionTime (selectedScreen : Screen) (world : World) =
            if world.Alive then
                if world.Accompanied && world.Halted then // special case to play song when halted in editor
                    match (selectedScreen.GetIncoming world).SongOpt with
                    | Some playSong ->
                        match World.getSongOpt world with
                        | Some song when assetEq song.Song playSong.Song -> () // do nothing when song is the same
                        | _ -> World.playSong playSong.FadeInTime playSong.FadeOutTime GameTime.zero playSong.RepeatLimitOpt playSong.Volume playSong.Song world // play song when song is different
                    | None -> ()
                match selectedScreen.GetSlideOpt world with
                | Some slide ->
                    // slide-specific behavior currently has to ignore desired screen in order to work. However, we
                    // special case it here to pay attention to desired screen when it is a non-slide screen (IE, not
                    // executing a series of slides). Additionally, to keep this hack's implementation self-contained,
                    // we use a special case to quick cut when halted in the editor.
                    match World.getDesiredScreen world with
                    | Desire desiredScreen when desiredScreen <> selectedScreen && (desiredScreen.GetSlideOpt world).IsNone ->
                        World.defer (fun world ->
                            let transitionTime = world.GameTime
                            World.selectScreen (IdlingState transitionTime) desiredScreen world
                            World.updateScreenIdling transitionTime desiredScreen world)
                            desiredScreen
                            world
                    | DesireNone ->
                        World.selectScreenOpt None world
                    | _ ->
                        if World.updateScreenIdling3 transitionTime slide selectedScreen world then
                            let transitionTime = world.GameTime
                            World.setScreenTransitionStatePlus (OutgoingState transitionTime) selectedScreen world
                            World.updateScreenOutgoing transitionTime selectedScreen world
                | None ->
                    match World.getDesiredScreen world with
                    | Desire desiredScreen ->
                        if desiredScreen <> selectedScreen then
                            if world.Accompanied && world.Halted && not world.AdvancementCleared then // special case to quick cut when halted in the editor.
                                World.defer (fun world ->
                                    let transitionTime = world.GameTime
                                    World.selectScreen (IdlingState transitionTime) desiredScreen world
                                    World.updateScreenIdling transitionTime desiredScreen world)
                                    desiredScreen
                                    world
                            else
                                let transitionTime = world.GameTime
                                World.setScreenTransitionStatePlus (OutgoingState transitionTime) selectedScreen world
                                World.updateScreenOutgoing transitionTime selectedScreen world
                    | DesireNone ->
                        let transitionTime = world.GameTime
                        World.setScreenTransitionStatePlus (OutgoingState transitionTime) selectedScreen world
                        World.updateScreenOutgoing transitionTime selectedScreen world
                    | DesireIgnore -> ()

        static member private updateScreenOutgoing transitionTime (selectedScreen : Screen) (world : World) =
            if transitionTime = world.GameTime then
                let incoming = selectedScreen.GetIncoming world
                let outgoing = selectedScreen.GetOutgoing world
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
                        | (Some song, Some song2) when assetEq song.Song song2.Song -> () // do nothing when song is the same
                        | (None, None) -> () // do nothing when neither plays a song (allowing manual control)
                        | (_, _) -> World.fadeOutSong playSong.FadeOutTime world // fade out when song is different
                    | None ->
                        match incoming.SongOpt with
                        | Some _ -> World.fadeOutSong playSong.FadeOutTime world
                        | None -> ()
                | None -> ()
                let eventTrace = EventTrace.debug "World" "updateScreenTransition" "OutgoingStart" EventTrace.empty
                World.publishPlus () selectedScreen.OutgoingStartEvent eventTrace selectedScreen false false world
            if world.Alive then
                if World.updateScreenTransition3 Outgoing selectedScreen world then
                    let transitionTime = world.GameTime
                    World.setScreenTransitionStatePlus (IdlingState transitionTime) selectedScreen world
                    World.updateScreenIdling transitionTime selectedScreen world
                    if world.Alive then
                        let eventTrace = EventTrace.debug "World" "updateScreenOutgoing" "OutgoingFinish" EventTrace.empty
                        World.publishPlus () selectedScreen.OutgoingFinishEvent eventTrace selectedScreen false false world
                    if world.Alive then
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
                            if destination <> selectedScreen then
                                let transitionTime = world.GameTime
                                World.selectScreen (IncomingState transitionTime) destination world
                                World.updateScreenIncoming transitionTime destination world
                        | None ->
                            World.selectScreenOpt None world
                            match World.getDesiredScreen world with // handle the possibility that screen deselect event changed destination
                            | Desire destination ->
                                let transitionTime = world.GameTime
                                World.selectScreen (IncomingState transitionTime) destination world
                                World.updateScreenIncoming transitionTime destination world
                            | DesireNone -> ()
                            | DesireIgnore -> ()

        static member private updateScreenTransition world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                match selectedScreen.GetTransitionState world with
                | IncomingState transitionTime -> World.updateScreenIncoming transitionTime selectedScreen world
                | IdlingState transitionTime -> World.updateScreenIdling transitionTime selectedScreen world
                | OutgoingState transitionTime -> World.updateScreenOutgoing transitionTime selectedScreen world
            | None ->
                match World.getDesiredScreen world with
                | Desire desiredScreen -> World.transitionScreen desiredScreen world
                | DesireNone -> ()
                | DesireIgnore -> ()

        static member private updateScreenRequestedSong world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                match World.getScreenRequestedSong selectedScreen world with
                | Request song ->
                    match World.getSongOpt world with
                    | Some current ->
                        if  current.FadeInTime <> song.FadeInTime ||
                            current.FadeOutTime <> song.FadeOutTime ||
                            current.StartTime <> song.StartTime ||
                            current.RepeatLimitOpt <> song.RepeatLimitOpt ||
                            assetNeq current.Song song.Song then
                            World.playSong song.FadeInTime song.FadeOutTime song.StartTime song.RepeatLimitOpt song.Volume song.Song world
                        elif current.Volume <> song.Volume then
                            World.setSongVolume song.Volume world
                    | None -> World.playSong song.FadeInTime song.FadeOutTime song.StartTime song.RepeatLimitOpt song.Volume song.Song world
                | RequestFadeOut fadeOutTime -> if not (World.getSongFadingOut world) then World.fadeOutSong fadeOutTime world
                | RequestNone -> World.stopSong world
                | RequestIgnore -> ()
            | None -> ()

        static member private processScreenTransitioning world =
            World.updateScreenTransition world
            World.updateScreenRequestedSong world

        /// Try to transition to the given screen if no other transition is in progress.
        static member tryTransitionScreen destination world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                if  selectedScreen <> destination &&
                    not (World.getSelectedScreenTransitioning world) then
                    let transitionTime = world.GameTime
                    World.setScreenTransitionDestinationOpt (Some destination) world
                    World.setScreenTransitionStatePlus (OutgoingState transitionTime) selectedScreen world
                    World.updateScreenOutgoing transitionTime selectedScreen world
                    true
                else false
            | None ->
                let transitionTime = world.GameTime
                World.setScreenTransitionStatePlus (IncomingState transitionTime) destination world
                World.setSelectedScreen destination world
                let eventTrace = EventTrace.debug "World" "selectScreen" "Select" EventTrace.empty
                World.publishPlus () destination.SelectEvent eventTrace destination false false world
                World.updateScreenIncoming transitionTime destination world
                true

        /// Transition to the given screen.
        static member transitionScreen destination world =
            World.tryTransitionScreen destination world |> ignore<bool>

        static member internal beginScreenPlus10<'d, 'r when 'd :> ScreenDispatcher> (zero : 'r) init transitionScreen setScreenSlide name select behavior groupFilePathOpt (args : Screen ArgImSim seq) (world : World) : SelectionEventData FQueue * 'r =
            Address.assertIdentifierName name
            if world.ContextImSim.Names.Length <> 1 then raise (InvalidOperationException "ImSim screen declared outside of valid ImSim context (must be called in a Game context).")
            let screenAddress = Address.makeFromArray (Array.add name world.ContextImSim.Names)
            World.setContext screenAddress world
            let screen = Nu.Screen screenAddress
            let screenCreation = not (screen.GetExists world)
            let initializing =
                match world.SimulantsImSim.TryGetValue screen.ScreenAddress with
                | (true, screenImSim) -> World.utilizeSimulantImSim screen.ScreenAddress screenImSim world; false
                | (false, _) ->

                    // init subscriptions _before_ potentially creating screen
                    World.addSimulantImSim screen.ScreenAddress { SimulantInitializing = true; SimulantUtilized = true; InitializationTime = Core.getTimeStampUnique (); Result = (FQueue.empty<SelectionEventData>, zero) } world
                    let mapFstResult (mapper : SelectionEventData FQueue -> SelectionEventData FQueue) world =
                        let mapScreenImSim screenImSim =
                            let (screenResult, userResult) = screenImSim.Result :?> SelectionEventData FQueue * 'r
                            { screenImSim with Result = (mapper screenResult, userResult) }
                        World.tryMapSimulantImSim mapScreenImSim screen.ScreenAddress world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj Select) world; Cascade) screen.SelectEvent screen world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj IncomingStart) world; Cascade) screen.IncomingStartEvent screen world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj IncomingFinish) world; Cascade) screen.IncomingFinishEvent screen world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj OutgoingStart) world; Cascade) screen.OutgoingStartEvent screen world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj OutgoingFinish) world; Cascade) screen.OutgoingFinishEvent screen world
                    World.monitor (fun _ world -> mapFstResult (FQueue.conj Deselecting) world; Cascade) screen.DeselectingEvent screen world
                    let mapSndResult (mapper : 'r -> 'r) world =
                        let mapScreenImSim screenImSim =
                            let (screenResult, userResult) = screenImSim.Result :?> SelectionEventData FQueue * 'r
                            { screenImSim with Result = (screenResult, mapper userResult) }
                        World.tryMapSimulantImSim mapScreenImSim screen.ScreenAddress world
                    init mapSndResult screen world

                    // create screen only when needed
                    if screenCreation then
                        World.createScreen4 typeof<'d>.Name (Some name) world |> ignore<Screen>
                        match groupFilePathOpt with
                        | Some groupFilePath -> World.readGroupFromFile groupFilePath None screen world |> ignore<Group>
                        | None -> ()

                    // protect screen
                    World.setScreenProtected true screen world |> ignore<bool>

                    // fin
                    true

            for arg in args do
                if (match arg.ArgType with
                    | InitializingArg -> initializing
                    | ReinitializingArg -> initializing || Reinitializing
                    | DynamicArg -> true) && screen.GetExists world then
                    screen.TrySetProperty arg.ArgLens.Name { PropertyType = arg.ArgLens.Type; PropertyValue = arg.ArgValue } world |> ignore
            if (initializing || Reinitializing) && screen.GetExists world then
                World.applyScreenBehavior setScreenSlide behavior screen world
            if screenCreation && screen.GetExists world then
                WorldModule.tryProcessScreen true screen world
            if screen.GetExists world && select && not (Option.contains screen (World.getSelectedScreenOpt world)) then
                if world.Accompanied && world.Halted && not world.AdvancementCleared then // special case to quick cut when halted in the editor.
                    World.defer (fun world ->
                        let transitionTime = world.GameTime
                        World.selectScreen (IdlingState transitionTime) screen world
                        World.updateScreenIdling transitionTime screen world)
                        screen
                        world
                else transitionScreen screen world
            let (screenResult, userResult) = (World.getSimulantImSim screen.ScreenAddress world).Result :?> SelectionEventData FQueue * 'r
            World.mapSimulantImSim (fun simulantImSim -> { simulantImSim with Result = (FQueue.empty<SelectionEventData>, zero) }) screen.ScreenAddress world
            (screenResult, userResult)

        static member inline private beginScreen8<'d when 'd :> ScreenDispatcher> transitionScreen setScreenSlide name select behavior groupFilePathOpt args world : SelectionEventData FQueue =
            World.beginScreenPlus10<'d, unit> () (fun _ _ _ -> ()) transitionScreen setScreenSlide name select behavior groupFilePathOpt args world |> fst

        /// End the ImSim declaration of a screen.
        static member endScreen (world : World) =
            match world.ContextImSim with
            | :? (Screen Address) -> World.setContext Game.GameAddress world
            | _ -> raise (InvalidOperationException "World.beginScreen mismatch.")

        /// Begin the ImSim declaration of a screen with the given arguments using a child group read from the given file path.
        /// Note that changing the screen behavior and file path over time has no effect as only the first moment is used.
        static member beginScreenWithGroupFromFilePlus<'d, 'r when 'd :> ScreenDispatcher> (zero : 'r) init name select behavior groupFilePath args world =
            World.beginScreenPlus10<'d, 'r> zero init World.transitionScreen World.setScreenSlide name select behavior (Some groupFilePath) args world

        /// Begin the ImSim declaration of a screen with the given arguments using a child group read from the given file path.
        /// Note that changing the screen behavior and file path over time has no effect as only the first moment is used.
        static member beginScreenWithGroupFromFile<'d when 'd :> ScreenDispatcher> name select behavior groupFilePath args world =
            World.beginScreen8<'d> World.transitionScreen World.setScreenSlide name select behavior (Some groupFilePath) args world

        /// Begin the ImSim declaration of a screen with the given arguments.
        /// Note that changing the screen behavior over time has no effect as only the first moment is used.
        static member beginScreenPlus<'d, 'r when 'd :> ScreenDispatcher> zero init name select behavior args world =
            World.beginScreenPlus10<'d, 'r> zero init World.transitionScreen World.setScreenSlide name select behavior None args world

        /// Begin the ImSim declaration of a screen with the given arguments.
        /// Note that changing the screen behavior over time has no effect as only the first moment is used.
        static member beginScreen<'d when 'd :> ScreenDispatcher> name select behavior args world =
            World.beginScreen8<'d> World.transitionScreen World.setScreenSlide name select behavior None args world

        /// ImSim declare a screen with the given arguments using a child group read from the given file path.
        /// Note that changing the screen behavior and file path over time has no effect as only the first moment is used.
        static member doScreenWithGroupFromFilePlus<'d, 'r when 'd :> ScreenDispatcher> (zero : 'r) init name select behavior groupFilePath args world =
            let (result, userResult) = World.beginScreenPlus10<'d, 'r> zero init World.transitionScreen World.setScreenSlide name select behavior (Some groupFilePath) args world
            World.endScreen world
            (result, userResult)

        /// ImSim declare a screen with the given arguments using a child group read from the given file path.
        /// Note that changing the screen behavior and file path over time has no effect as only the first moment is used.
        static member doScreenWithGroupFromFile<'d when 'd :> ScreenDispatcher> name select behavior groupFilePath args world =
            let result = World.beginScreen8<'d> World.transitionScreen World.setScreenSlide name select behavior (Some groupFilePath) args world
            World.endScreen world
            result

        /// ImSim declare a screen with the given arguments.
        /// Note that changing the screen behavior over time has no effect as only the first moment is used.
        static member doScreenPlus<'d, 'r when 'd :> ScreenDispatcher> zero init name select behavior args world =
            let (result, userResult) = World.beginScreenPlus10<'d, 'r> zero init World.transitionScreen World.setScreenSlide name select behavior None args world
            World.endScreen world
            (result, userResult)

        /// ImSim declare a screen with the given arguments.
        /// Note that changing the screen behavior over time has no effect as only the first moment is used.
        static member doScreen<'d when 'd :> ScreenDispatcher> name select behavior args world =
            let result = World.beginScreen8<'d> World.transitionScreen World.setScreenSlide name select behavior None args world
            World.endScreen world
            result

        /// Set the slide aspects of a screen.
        static member setScreenSlide (slideDescriptor : SlideDescriptor) destination (screen : Screen) world =

            // destroy existing slide group if any
            let slideGroup = screen / "SlideGroup"
            let slideSprite = slideGroup / "SlideSprite"
            World.destroyGroupImmediate slideGroup world

            // create slide group
            screen.SetSlideOpt (Some { IdlingTime = slideDescriptor.IdlingTime; Destination = destination }) world
            World.createGroup<GroupDispatcher> (Some slideGroup.Name) screen world |> ignore<Group>
            World.setGroupProtected true slideGroup world |> ignore<bool>
            slideGroup.SetPersistent false world

            // create slide sprite
            World.createEntity<StaticSpriteDispatcher> None DefaultOverlay (Some slideSprite.Surnames) slideGroup world |> ignore<Entity>
            World.setEntityProtected true slideSprite world |> ignore<bool>
            slideSprite.SetPersistent false world
            slideSprite.SetSize world.Eye2dSize.V3 world
            slideSprite.SetAbsolute true world
            match slideDescriptor.SlideImageOpt with
            | Some slideImage ->
                slideSprite.SetStaticImage slideImage world
                slideSprite.SetVisible true world
            | None ->
                slideSprite.SetStaticImage Assets.Default.NuSlide world
                slideSprite.SetVisible false world

        /// Create a dissolve screen whose content is loaded from the given group file.
        static member createDissolveScreenFromGroupFile6 dispatcherName nameOpt dissolveDescriptor songOpt groupFilePath world =
            let dissolveScreen = World.createDissolveScreen5 dispatcherName nameOpt dissolveDescriptor songOpt world
            World.readGroupFromFile groupFilePath None dissolveScreen world |> ignore<Group>
            dissolveScreen

        /// Create a dissolve screen whose content is loaded from the given group file.
        static member createDissolveScreenFromGroupFile<'d when 'd :> ScreenDispatcher> nameOpt dissolveDescriptor songOpt groupFilePath world =
            World.createDissolveScreenFromGroupFile6 typeof<'d>.Name nameOpt dissolveDescriptor groupFilePath songOpt world

        /// Create a slide screen that transitions to the given destination upon completion.
        static member createSlideScreen6 dispatcherName nameOpt slideDescriptor destination world =
            let slideScreen = World.createDissolveScreen5 dispatcherName nameOpt slideDescriptor.DissolveDescriptor None world
            World.setScreenSlide slideDescriptor destination slideScreen world
            slideScreen

        /// Create a slide screen that transitions to the given destination upon completion.
        static member createSlideScreen<'d when 'd :> ScreenDispatcher> nameOpt slideDescriptor destination world =
            World.createSlideScreen6 typeof<'d>.Name nameOpt slideDescriptor destination world

        static member private mapEntityDescriptors entityDescriptors =
            entityDescriptors
            |> List.map (fun descriptor ->
                match descriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                | Atom (entityName, _) | Text (entityName, _) -> (entityName, descriptor)
                | _ -> failwithumf ())
            |> Map.ofList

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
                let propagatedDescriptor = { propagatedDescriptor with EntityProperties = Map.remove "PropagatedDescriptorOpt" propagatedDescriptor.EntityProperties }
                let considerUsingCurrentEntityAsPropagationSource =
                    match currentDescriptor.EntityProperties.TryGetValue "PropagationSourceOpt" with
                    | (true, propagationSourceOptSymbol) -> propagationSourceOptSymbol |> symbolToValue<string option> |> Option.isNone
                    | (false, _) -> true
                if considerUsingCurrentEntityAsPropagationSource then
                    match currentEntityOpt with
                    | Some currentEntity ->
                        if currentEntity.GetExists world && currentEntity.HasPropagationTargets world
                        then { propagatedDescriptor with EntityProperties = Map.add "PropagationSourceOpt" (valueToSymbol (Some currentEntity)) propagatedDescriptor.EntityProperties }
                        else propagatedDescriptor
                    | None -> propagatedDescriptor
                else propagatedDescriptor

            // propagate properties at this level
            let propagatedDescriptor =
                Set.ofSeq currentDescriptor.EntityProperties.Keys
                |> Set.addMany propagatedDescriptor.EntityProperties.Keys
                |> Seq.fold (fun targetDescriptor propertyName ->
                    if  propertyName <> nameof Entity.Name &&
                        propertyName <> nameof Entity.Position &&
                        propertyName <> nameof Entity.Rotation &&
                        propertyName <> nameof Entity.Elevation &&
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
                                let facetNamesIntrinsic =
                                    let entityDispatchers = World.getEntityDispatchers world
                                    let currentDispatcher = entityDispatchers.[currentDescriptor.EntityDispatcherName]
                                    currentDispatcher |> getType |> Reflection.getIntrinsicFacetNames
                                let facetNamesExtrinsic =
                                    match currentDescriptor.EntityProperties.TryGetValue Constants.Engine.FacetNamesPropertyName with
                                    | (true, facetNamesSymbol) -> symbolToValue<string Set> facetNamesSymbol
                                    | (false, _) -> Set.empty
                                let facetNames = Set.addMany facetNamesIntrinsic facetNamesExtrinsic
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
                                    let facetNamesIntrinsic =
                                        let entityDispatchers = World.getEntityDispatchers world
                                        let targetDispatcher = entityDispatchers.[targetDescriptor.EntityDispatcherName]
                                        targetDispatcher |> getType |> Reflection.getIntrinsicFacetNames
                                    let facetNamesExtrinsic =
                                        match targetDescriptor.EntityProperties.TryGetValue Constants.Engine.FacetNamesPropertyName with
                                        | (true, facetNamesSymbol) -> symbolToValue<string Set> facetNamesSymbol
                                        | (false, _) -> Set.empty
                                    let facetNames = Set.addMany facetNamesIntrinsic facetNamesExtrinsic
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
                                        if currentEntity.GetExists world
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
                currentDescriptor.EntityDescriptors
                |> Seq.mapi (fun i currentDescriptor ->
                    match currentDescriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                    | Atom (entityName, _) | Text (entityName, _) -> (entityName, i)
                    | _ -> ("", Int32.MaxValue))
                |> Map.ofSeq
            let propagatedDescriptors =
                propagatedDescriptorOpts
                |> List.definitize
                |> List.filter (fun propagatedDescriptor -> String.notEmpty propagatedDescriptor.EntityDispatcherName)
                |> List.sortBy (fun propagatedDescriptor ->
                    match propagatedDescriptor.EntityProperties.[Constants.Engine.NamePropertyName] with
                    | (Atom (entityName, _) | Text (entityName, _)) ->
                        match currentDescriptorsOrder.TryGetValue entityName with
                        | (true, order) -> order
                        | (false, _) -> Int32.MaxValue
                    | _ -> Int32.MaxValue)
            { propagatedDescriptor with EntityDescriptors = propagatedDescriptors }

        /// Propagate the structure of an entity to all other entities with it as their propagation source.
        /// TODO: expose this through Entity API.
        static member propagateEntityStructure (entity : Entity) world =

            // propagate entity
            let targets = entity.GetPropagationTargets world
            let targetsValid =
                targets
                |> Seq.filter (fun (target : Entity) ->
                    let targetToEntity = Address.relate target.EntityAddress entity.EntityAddress
                    let nameHeadOpt = Array.tryHead targetToEntity.Names
                    let nameLastOpt = Array.tryLast targetToEntity.Names
                    let valid =
                        not (nameHeadOpt = Some Constants.Address.ParentName && nameLastOpt = Some target.Name) && // propagation target is not descendent
                        Array.contains Constants.Address.ParentName targetToEntity.Names && // propagation target is not ancestor
                        nameLastOpt <> Some Constants.Address.CurrentName // propagation target is not self
                    // NOTE: dummying this out because it causes false negatives.
                    //if not valid then Log.warn ("Invalid propagation target '" + scstring target + "' from source '" + scstring entity + "'.")
                    valid)
                |> Array.ofSeq // copy references to avoid enumerator invalidation
            let currentDescriptor = World.writeEntity true true EntityDescriptor.empty entity world
            let previousDescriptor = Option.defaultValue EntityDescriptor.empty (entity.GetPropagatedDescriptorOpt world)
            for target in targetsValid do
                if World.getEntityExists target world then
                    let targetDescriptor = World.writeEntity true false EntityDescriptor.empty target world
                    let propagatedDescriptor = World.propagateEntityDescriptor previousDescriptor currentDescriptor targetDescriptor (Some entity) world
                    World.destroyEntityImmediate target world
                    World.readEntity true false propagatedDescriptor (Some target.Name) target.Parent world |> ignore<Entity>
                    World.propagateEntityAffineMatrix target world
            let currentDescriptor = { currentDescriptor with EntityProperties = Map.remove (nameof Entity.PropagatedDescriptorOpt) currentDescriptor.EntityProperties }
            entity.SetPropagatedDescriptorOpt (Some currentDescriptor) world

            // propagate sourced ancestor entities
            seq {
                let targets = entity.GetPropagationTargets world
                let targetsValid =
                    Seq.filter (fun (target : Entity) ->
                        let targetToEntity = Address.relate target.EntityAddress entity.EntityAddress
                        let nameHeadOpt = Array.tryHead targetToEntity.Names
                        let nameLastOpt = Array.tryLast targetToEntity.Names
                        let valid =
                            not (nameHeadOpt = Some Constants.Address.ParentName && nameLastOpt = Some target.Name) && // propagation target is not descendent
                            Array.contains Constants.Address.ParentName targetToEntity.Names && // propagation target is not ancestor
                            nameLastOpt <> Some Constants.Address.CurrentName // propagation target is not self
                        // NOTE: dummying this out because it causes false negatives.
                        //if not valid then Log.warn ("Invalid propagation target '" + scstring target + "' from source '" + scstring entity + "'.")
                        valid)
                        targets
                for target in targetsValid do
                    if target.GetExists world then
                        for ancestor in World.getEntityAncestors target world do
                            if ancestor.GetExists world && ancestor.HasPropagationTargets world then
                                ancestor }
            |> Set.ofSeq // also copies references to avoid enumerator invalidation
            |> fun ancestors ->
                for ancestor in ancestors do
                    if ancestor.GetExists world && ancestor.HasPropagationTargets world then
                        World.propagateEntityStructure ancestor world

        /// Clear all propagation targets pointing back to the given entity.
        static member clearPropagationTargets (entity : Entity) world =
            for target in entity.GetPropagationTargets world do
                if World.getEntityExists target world then
                    target.SetPropagationSourceOpt None world

        static member internal makeIntrinsicOverlays facets entityDispatchers =
            let requiresFacetNames = fun sourceType -> sourceType = typeof<EntityDispatcher>
            let facets = facets |> Map.toValueList |> List.map box
            let entityDispatchers = entityDispatchers |> Map.toValueList |> List.map box
            let sources = facets @ entityDispatchers
            let sourceTypes = List.map (fun source -> source.GetType ()) sources
            Overlay.makeIntrinsicOverlays requiresFacetNames sourceTypes

        static member internal handleSubscribeAndUnsubscribeEvent subscribing (eventAddress : Address) (_ : Simulant) world =
            // here we need to update the event publish flags for entities based on whether there are subscriptions to
            // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
            // that these optimizations do not support, and warn the developer if they are invoked. Additionally, we
            // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
            let eventNames = eventAddress.Names
            let eventNamesLength = Array.length eventNames
            if eventNamesLength >= 6 then
                let eventFirstName = eventNames.[0]
                match eventFirstName with
                | "Update" ->
#if DEBUG
                    if  Array.contains Constants.Address.WildcardName eventNames ||
                        Array.contains Constants.Address.EllipsisName eventNames then
                        Log.error
                            ("Subscribing to entity update events with a wildcard or ellipsis is not supported. " +
                                "This will cause a bug where some entity update events are not published.")
#endif
                    let entity = Nu.Entity (Array.skip 2 eventNames)
                    World.updateEntityPublishUpdateFlag entity world |> ignore<bool>
                | _ -> ()
            if eventNamesLength >= 4 then
                match eventNames.[0] with
                | "Change" ->
                    if eventNamesLength >= 7 then
                        let entityAddress = rtoa (Array.skip 3 eventNames)
                        let entity = Nu.Entity entityAddress
                        match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityChangeCountsKey, world) with
                        | (true, entityChangeCounts) ->
                            match entityChangeCounts.TryGetValue entityAddress with
                            | (true, entityChangeCount) ->
                                let entityChangeCount = if subscribing then inc entityChangeCount else dec entityChangeCount
                                let entityChangeCounts =
                                    if entityChangeCount = 0
                                    then UMap.remove entityAddress entityChangeCounts
                                    else UMap.add entityAddress entityChangeCount entityChangeCounts
                                if entity.GetExists world then
                                    if entityChangeCount = 0 then World.setEntityPublishChangeEvents false entity world |> ignore<bool>
                                    elif entityChangeCount = 1 then World.setEntityPublishChangeEvents true entity world |> ignore<bool>
                                World.mapKeyValueStore (SUMap.add EntityChangeCountsKey entityChangeCounts) world // no event
                            | (false, _) ->
                                if not subscribing then failwithumf ()
                                if entity.GetExists world then World.setEntityPublishChangeEvents true entity world |> ignore<bool>
                                World.mapKeyValueStore (SUMap.add EntityChangeCountsKey (UMap.add entityAddress 1 entityChangeCounts)) world // no event
                        | (false, _) ->
                            if not subscribing then failwithumf ()
                            let config = World.getCollectionConfig world
                            let entityChangeCounts = UMap.makeEmpty HashIdentity.Structural config
                            if entity.GetExists world then World.setEntityPublishChangeEvents true entity world |> ignore<bool>
                            World.mapKeyValueStore (SUMap.add EntityChangeCountsKey (UMap.add entityAddress 1 entityChangeCounts)) world // no event
                    if  Array.contains Constants.Address.WildcardName eventNames ||
                        Array.contains Constants.Address.EllipsisName eventNames then
                        Log.error "Subscribing to change events with a wildcard or ellipsis is not supported."
                | _ -> ()

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
            for entity in entities2d do
                let entityState = World.getEntityState entity world
                let element = Quadelement.make entityState.VisibleInView entityState.StaticInPlay entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 entity
                Quadtree.addElement entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 element world.Quadtree
            if SList.notEmpty entities3d then
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make entityState.VisibleInView entityState.StaticInPlay entityState.LightProbe entityState.Light entityState.Presence entityState.PresenceInPlay entityState.Bounds entity
                    Octree.addElement entityState.Presence entityState.PresenceInPlay entityState.Bounds element world.Octree
            if SList.exists (fun (entity : Entity) -> entity.Has<LightProbe3dFacet> world && entity.GetProbeStale world) entities3d then
                World.requestLightMapRender world
                
        static member internal evictScreenElements screen world =
            let entities = World.getGroups screen world |> Seq.map (flip World.getEntities world) |> Seq.concat |> SArray.ofSeq
            let (entities2d, entities3d) = SArray.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
            for entity in entities2d do
                let entityState = World.getEntityState entity world
                let element = Quadelement.make entityState.VisibleInView entityState.StaticInPlay entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 entity
                Quadtree.removeElement entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 element world.Quadtree
            if SArray.notEmpty entities3d then
                for entity in entities3d do
                    let entityState = World.getEntityState entity world
                    let element = Octelement.make entityState.VisibleInView entityState.StaticInPlay entityState.LightProbe entityState.Light entityState.Presence entityState.PresenceInPlay entityState.Bounds entity
                    Octree.removeElement entityState.Presence entityState.PresenceInPlay entityState.Bounds element world.Octree

        static member internal registerScreenPhysics screen world =
            let entities =
                World.getGroups screen world
                |> Seq.map (flip World.getEntities world)
                |> Seq.concat
                |> SList.ofSeq
            for entity in entities do
                World.registerEntityPhysics entity world

        static member internal unregisterScreenPhysics screen world =
            let entities =
                World.getGroups screen world
                |> Seq.map (flip World.getEntities world)
                |> Seq.concat
                |> SList.ofSeq
            for entity in entities do
                World.unregisterEntityPhysics entity world

        static member private synchronizeViewports world =
            let windowSize = World.getWindowSize world
            let outerViewport = Viewport.makeOuter windowSize
            World.setOuterViewport outerViewport world
            World.setRasterViewport (Viewport.makeRaster outerViewport.Bounds) world
            World.setGeometryViewport (Viewport.makeGeometry windowSize) world

        /// Try to reload the overlayer currently in use by the world.
        static member tryReloadOverlayer inputDirectory outputDirectory world =
            
            // attempt to reload overlay file
            let inputOverlayerFilePath = inputDirectory + "/" + Assets.Global.OverlayerFilePath
            let outputOverlayerFilePath = outputDirectory + "/" + Assets.Global.OverlayerFilePath
            try if File.Exists outputOverlayerFilePath then File.SetAttributes (outputOverlayerFilePath, FileAttributes.None)
                File.Copy (inputOverlayerFilePath, outputOverlayerFilePath, true)
                File.SetAttributes (outputOverlayerFilePath, FileAttributes.ReadOnly)

                // cache old overlayer and make new one
                let overlayerOld = World.getOverlayer world
                let entityDispatchers = World.getEntityDispatchers world
                let facets = World.getFacets world
                let intrinsicOverlays = World.makeIntrinsicOverlays facets entityDispatchers
                let overlayer = Overlayer.makeFromFileOpt intrinsicOverlays outputOverlayerFilePath

                // update and apply overlays to all entities
                World.setOverlayer overlayer world
                let entities = World.getEntities1 world
                for entity in entities do
                    World.applyEntityOverlay overlayerOld overlayer entity world
                Right overlayer

            // propagate errors
            with exn -> Left (scstring exn)

        /// Send a message to the subsystems to reload their existing assets.
        static member reloadExistingAssets world =
            World.reloadPhysicsAssets world
            World.reloadRenderAssets2d world
            World.reloadRenderAssets3d world
            World.reloadRenderAssetsImGui world
            World.reloadAudioAssets world
            World.reloadSymbols world

        /// Attempt to reload asset graph, build assets, then reload built assets.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssetGraph inputDirectory outputDirectory refinementDirectory world =

            // attempt to reload asset graph file
            let inputAssetGraphFilePath = inputDirectory + "/" + Assets.Global.AssetGraphFilePath
            let outputAssetGraphFilePath = outputDirectory + "/" + Assets.Global.AssetGraphFilePath
            try if File.Exists outputAssetGraphFilePath then File.SetAttributes (outputAssetGraphFilePath, FileAttributes.None)
                File.Copy (inputAssetGraphFilePath, outputAssetGraphFilePath, true)
                // NOTE: dummied out the following because it seems to be somehow responsible for the asset graph's file lock leaking when closing Gaia...
                //File.SetAttributes (outputAssetGraphFilePath, FileAttributes.ReadOnly)

                // attempt to load asset graph
                let assetGraph = AssetGraph.makeFromFileOpt outputAssetGraphFilePath

                // rebuild and reload assets
                AssetGraph.buildAssets inputDirectory outputDirectory refinementDirectory false assetGraph
                Metadata.reloadMetadata ()
                World.reloadExistingAssets world
                World.publishPlus () Nu.Game.Handle.AssetsReloadEvent (EventTrace.debug "World" "publishAssetsReload" "" EventTrace.empty) Nu.Game.Handle false false world
                Right assetGraph

            // propagate error
            with exn -> Left (scstring exn)

        /// Attempt to reload asset graph, build assets, then reload built assets.
        /// Currently does not support reloading of song assets, and possibly others that are
        /// locked by the engine's subsystems.
        static member tryReloadAssets world =
            let targetDir = AppDomain.CurrentDomain.BaseDirectory
            let assetSourceDir = PathF.GetFullPath (targetDir + "../../..")
            match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world with
            | Right _ -> true
            | Left _ -> false

        /// Switch simulation to this world, resynchronizing the imperative subsystems with its current state.
        /// Needed when abandoning execution of the current world in favor of a previous world, such as in the case of
        /// an exception where the try expression resulted in a transformed world that is to be discarded.
        static member switch worldStateOld (world : World) =

            // wipe memoized named content
            Content.wipe ()

            // update world state
            world.WorldState <- worldStateOld

            // sync tick watch state to advancing
            World.switchAmbientState world

            // synchronize viewports in case they get out of sync, such as during an undo operation
            World.synchronizeViewports world

            // rebuild spatial trees
            Octree.clear world.Octree
            Quadtree.clear world.Quadtree
            match World.getSelectedScreenOpt world with
            | Some screen -> World.admitScreenElements screen world
            | None -> ()

            // rebuild physics states
            let physics3d = World.getPhysicsEngine3d world in physics3d.ClearInternal ()
            let physics2d = World.getPhysicsEngine2d world in physics2d.ClearInternal ()
            match World.getSelectedScreenOpt world with
            | Some screen -> World.registerScreenPhysics screen world
            | None -> ()

        static member private processCoroutines (world : World) =
            if world.Advancing then
                let coroutines = World.getCoroutines world
                let coroutinesRemaining =
                    OMap.fold (fun coroutines id (scheduledTime, pred, coroutine) ->
                        if pred () then
                            if scheduledTime <= world.GameTime then
                                match coroutine () with
                                | Sleep (duration, continuation) ->
                                    OMap.add id (scheduledTime + duration, pred, continuation) coroutines
                                | Cancel | Complete -> coroutines
                            else OMap.add id (scheduledTime, pred, coroutine) coroutines
                        else coroutines)
                        (OMap.makeEmpty (OMap.comparer coroutines) (OMap.config coroutines))
                        coroutines
                let coroutineKeys = coroutines |> SArray.ofSeq |> SArray.map fst
                let coroutinesAdded = OMap.removeMany coroutineKeys (World.getCoroutines world)
                World.setCoroutines (OMap.concat coroutinesRemaining coroutinesAdded) world

        static member private processTasklet simulant tasklet (taskletsNotRun : OMap<Simulant, World Tasklet UList>) (world : World) =
            let shouldRun =
                match tasklet.ScheduledTime with
                | UpdateTime time -> time <= world.UpdateTime
                | TickTime time -> time <= world.TickTime
            if shouldRun then
                tasklet.ScheduledOp world
                taskletsNotRun
            else
                match taskletsNotRun.TryGetValue simulant with
                | (true, taskletList) -> OMap.add simulant (UList.add tasklet taskletList) taskletsNotRun
                | (false, _) -> OMap.add simulant (UList.singleton (OMap.config taskletsNotRun) tasklet) taskletsNotRun

        static member private processTasklets (world : World) =
            let tasklets = World.getTasklets world
            World.clearTasklets world
            let taskletsNotRun =
                OMap.fold (fun taskletsNotRun simulant taskletList ->
                    UList.fold (fun taskletsNotRun tasklet ->
                        if World.getExists simulant world
                        then World.processTasklet simulant tasklet taskletsNotRun world
                        else taskletsNotRun)
                        taskletsNotRun
                        taskletList)
                    (OMap.makeEmpty HashIdentity.Structural (OMap.config tasklets))
                    tasklets
            let taskletsNotRun = OMap.filter (fun simulant _ -> World.getExists simulant world) taskletsNotRun
            World.restoreTasklets taskletsNotRun world

        static member private processImSim (world : World) =
            WorldImSim.Reinitializing <- false
            World.sweepSimulants world

        static member private destroySimulants world =
            for simulant in world |> World.getDestructionListRev |> List.rev do
                World.destroyImmediate simulant world
            if List.notEmpty (World.getDestructionListRev world) then
                World.destroySimulants world

        static member private toImGuiMouseButton mouseButton =
            match mouseButton with
            | MouseLeft -> 0
            | MouseRight -> 1
            | MouseMiddle -> 2
            | MouseX1 -> 3
            | MouseX2 -> 4

        static member private toImGuiKeys keyboardKey =
            match keyboardKey with
            | KeyboardKey.Space -> [ImGuiKey.Space]
            | KeyboardKey.Tab -> [ImGuiKey.Tab]
            | KeyboardKey.Left -> [ImGuiKey.LeftArrow]
            | KeyboardKey.Right -> [ImGuiKey.RightArrow]
            | KeyboardKey.Up -> [ImGuiKey.UpArrow]
            | KeyboardKey.Down -> [ImGuiKey.DownArrow]
            | KeyboardKey.PageUp -> [ImGuiKey.PageUp]
            | KeyboardKey.PageDown -> [ImGuiKey.PageDown]
            | KeyboardKey.Home -> [ImGuiKey.Home]
            | KeyboardKey.End -> [ImGuiKey.End]
            | KeyboardKey.Delete -> [ImGuiKey.Delete]
            | KeyboardKey.Backspace -> [ImGuiKey.Backspace]
            | KeyboardKey.Enter -> [ImGuiKey.Enter]
            | KeyboardKey.Escape -> [ImGuiKey.Escape]
            | KeyboardKey.LCtrl -> [ImGuiKey.LeftCtrl; ImGuiKey.ModCtrl]
            | KeyboardKey.RCtrl -> [ImGuiKey.RightCtrl; ImGuiKey.ModCtrl]
            | KeyboardKey.LAlt -> [ImGuiKey.LeftAlt; ImGuiKey.ModAlt]
            | KeyboardKey.RAlt -> [ImGuiKey.RightAlt; ImGuiKey.ModAlt]
            | KeyboardKey.LShift -> [ImGuiKey.LeftShift; ImGuiKey.ModShift]
            | KeyboardKey.RShift -> [ImGuiKey.RightShift; ImGuiKey.ModShift]
            | _ ->
                if int keyboardKey >= int KeyboardKey.Num1 && int keyboardKey <= int KeyboardKey.Num9 then int ImGuiKey._1 + (int keyboardKey - int KeyboardKey.Num1) |> enum<ImGuiKey> |> List.singleton
                elif int keyboardKey >= int KeyboardKey.A && int keyboardKey <= int KeyboardKey.Z then int ImGuiKey.A + (int keyboardKey - int KeyboardKey.A) |> enum<ImGuiKey> |> List.singleton
                elif int keyboardKey >= int KeyboardKey.F1 && int keyboardKey <= int KeyboardKey.F12 then int ImGuiKey.F1 + (int keyboardKey - int KeyboardKey.F1) |> enum<ImGuiKey> |> List.singleton
                else []

        static member private processInput2 (evt : SDL.SDL_Event) (world : World) =
            match evt.``type`` with
            | SDL.SDL_EventType.SDL_QUIT ->
                if world.Accompanied then
                    let eventTrace = EventTrace.debug "World" "processInput2" "ExitRequest" EventTrace.empty
                    World.publishPlus () Nu.Game.Handle.ExitRequestEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_WINDOWEVENT ->
                if evt.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED then

                    // ensure window size is a factor of display virtual resolution, going to full screen otherwise
                    let windowSize = World.getWindowSize world
                    let windowScalar =
                        max (single windowSize.X / single Constants.Render.DisplayVirtualResolution.X |> ceil |> int |> max 1)
                            (single windowSize.Y / single Constants.Render.DisplayVirtualResolution.Y |> ceil |> int |> max 1)
                    let windowSize' = windowScalar * Constants.Render.DisplayVirtualResolution
                    World.trySetWindowSize windowSize' world
                    let windowSize'' = World.getWindowSize world
                    if windowSize''.X < windowSize'.X || windowSize''.Y < windowSize'.Y then
                        World.trySetWindowFullScreen true world

                    // synchronize display virtual scalar
                    let windowSize'' = World.getWindowSize world
                    let xScalar = windowSize''.X / Constants.Render.DisplayVirtualResolution.X
                    let yScalar = windowSize''.Y / Constants.Render.DisplayVirtualResolution.Y
                    Globals.Render.DisplayScalar <- min xScalar yScalar

                    // synchronize view ports
                    World.synchronizeViewports world

            | SDL.SDL_EventType.SDL_MOUSEMOTION ->
                let io = ImGui.GetIO ()
                let outerOffset = world.OuterViewport.Bounds.Min
                io.AddMousePosEvent (single (evt.button.x - outerOffset.X), single (evt.button.y - outerOffset.Y))
                let mousePosition = v2 (single evt.button.x) (single evt.button.y)
                if World.isMouseButtonDown MouseLeft world then
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseDrag" EventTrace.empty
                    World.publishPlus { MouseMoveData.Position = mousePosition } Nu.Game.Handle.MouseDragEvent eventTrace Nu.Game.Handle true true world
                let eventTrace = EventTrace.debug "World" "processInput2" "MouseMove" EventTrace.empty
                World.publishPlus { MouseMoveData.Position = mousePosition } Nu.Game.Handle.MouseMoveEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN ->
                let io = ImGui.GetIO ()
                let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                io.AddMouseButtonEvent (World.toImGuiMouseButton mouseButton, true)
                if not (io.WantCaptureMouseGlobal) then
                    let mousePosition = World.getMousePosition world
                    let mouseButtonDownEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Down/Event/" + Constants.Engine.GameName)
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event/" + Constants.Engine.GameName)
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = true }
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseButtonDown" EventTrace.empty
                    World.publishPlus eventData mouseButtonDownEvent eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseButtonChange" EventTrace.empty
                    World.publishPlus eventData mouseButtonChangeEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_MOUSEBUTTONUP ->
                let io = ImGui.GetIO ()
                let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                io.AddMouseButtonEvent (World.toImGuiMouseButton mouseButton, false)
                if not (io.WantCaptureMouseGlobal) then
                    let mousePosition = World.getMousePosition world
                    let mouseButton = World.toNuMouseButton (uint32 evt.button.button)
                    let mouseButtonUpEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Up/Event/" + Constants.Engine.GameName)
                    let mouseButtonChangeEvent = stoa<MouseButtonData> ("Mouse/" + MouseButton.toEventName mouseButton + "/Change/Event/" + Constants.Engine.GameName)
                    let eventData = { Position = mousePosition; Button = mouseButton; Down = false }
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseButtonUp" EventTrace.empty
                    World.publishPlus eventData mouseButtonUpEvent eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseButtonChange" EventTrace.empty
                    World.publishPlus eventData mouseButtonChangeEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_MOUSEWHEEL ->
                let imGui = World.getImGui world
                if evt.wheel.preciseY <> 0.0f then
                    let flipped = evt.wheel.direction = uint SDL.SDL_MouseWheelDirection.SDL_MOUSEWHEEL_FLIPPED
                    let travel = evt.wheel.preciseY * if flipped then -1.0f else 1.0f
                    imGui.HandleMouseWheelChange travel
                    let eventData = { Travel = travel }
                    let eventTrace = EventTrace.debug "World" "processInput2" "MouseWheel" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.MouseWheelEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_TEXTINPUT ->
                let io = ImGui.GetIO ()
                let imGui = World.getImGui world
                let textInput = char evt.text.text.FixedElementField
                imGui.HandleKeyChar textInput
                if not (io.WantCaptureKeyboardGlobal) then
                    let eventData = { TextInput = textInput }
                    let eventTrace = EventTrace.debug "World" "processInput2" "TextInput" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.TextInputEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_KEYDOWN ->
                let io = ImGui.GetIO ()
                let keyboard = evt.key
                let key = keyboard.keysym
                let keyboardKey = key.scancode |> int |> enum<KeyboardKey>
                for imGuiKey in World.toImGuiKeys keyboardKey do
                    io.AddKeyEvent (imGuiKey, true)
                if not (io.WantCaptureKeyboardGlobal) then
                    let eventData = { KeyboardKey = keyboardKey; Repeated = keyboard.repeat <> byte 0; Down = true }
                    let eventTrace = EventTrace.debug "World" "processInput2" "KeyboardKeyDown" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.KeyboardKeyDownEvent eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.KeyboardKeyChangeEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_KEYUP ->
                let io = ImGui.GetIO ()
                let keyboard = evt.key
                let key = keyboard.keysym
                let keyboardKey = key.scancode |> int |> enum<KeyboardKey>
                for imGuiKey in World.toImGuiKeys keyboardKey do
                    io.AddKeyEvent (imGuiKey, false)
                if not (io.WantCaptureKeyboardGlobal) then
                    let eventData = { KeyboardKey = key.scancode |> int |> enum<KeyboardKey>; Repeated = keyboard.repeat <> byte 0; Down = false }
                    let eventTrace = EventTrace.debug "World" "processInput2" "KeyboardKeyUp" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.KeyboardKeyUpEvent eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "KeyboardKeyChange" EventTrace.empty
                    World.publishPlus eventData Nu.Game.Handle.KeyboardKeyChangeEvent eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_JOYHATMOTION ->
                let index = evt.jhat.which
                let direction = evt.jhat.hatValue
                let eventData = { GamepadDirection = GamepadState.toNuDirection direction }
                let eventTrace = EventTrace.debug "World" "processInput2" "GamepadDirectionChange" EventTrace.empty
                World.publishPlus eventData (Nu.Game.Handle.GamepadDirectionChangeEvent index) eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_JOYBUTTONDOWN ->
                let index = evt.jbutton.which
                let button = int evt.jbutton.button
                if GamepadState.isSdlButtonSupported button then
                    let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                    let eventTrace = EventTrace.debug "World" "processInput2" "GamepadButtonDown" EventTrace.empty
                    World.publishPlus eventData (Nu.Game.Handle.GamepadButtonDownEvent index) eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "GamepadButtonChange" EventTrace.empty
                    World.publishPlus eventData (Nu.Game.Handle.GamepadButtonChangeEvent index) eventTrace Nu.Game.Handle true true world
            | SDL.SDL_EventType.SDL_JOYBUTTONUP ->
                let index = evt.jbutton.which
                let button = int evt.jbutton.button
                if GamepadState.isSdlButtonSupported button then
                    let eventData = { GamepadButton = GamepadState.toNuButton button; Down = true }
                    let eventTrace = EventTrace.debug "World" "processInput2" "GamepadButtonUp" EventTrace.empty
                    World.publishPlus eventData (Nu.Game.Handle.GamepadButtonUpEvent index) eventTrace Nu.Game.Handle true true world
                    let eventTrace = EventTrace.debug "World" "processInput2" "GamepadButtonChange" EventTrace.empty
                    World.publishPlus eventData (Nu.Game.Handle.GamepadButtonChangeEvent index) eventTrace Nu.Game.Handle true true world
            | _ -> ()

        static member private processIntegrationMessage integrationMessage (world : World) =
            if world.Alive then
                match integrationMessage with
                | BodyPenetrationMessage bodyPenetrationMessage ->
                    match bodyPenetrationMessage.BodyShapeSource.BodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.GetExists world && entity.GetSelected world then
                            let penetrationData =
                                { BodyShapePenetrator = bodyPenetrationMessage.BodyShapeSource
                                  BodyShapePenetratee = bodyPenetrationMessage.BodyShapeSource2
                                  Normal = bodyPenetrationMessage.Normal }
                            let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                            World.publishPlus penetrationData entity.BodyPenetrationEvent eventTrace entity false false world
                    | _ -> ()
                | BodySeparationMessage bodySeparationMessage ->
                    match bodySeparationMessage.BodyShapeSource.BodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.GetExists world && entity.GetSelected world then
                            let separationData =
                                { BodyShapeSeparator = bodySeparationMessage.BodyShapeSource
                                  BodyShapeSeparatee = bodySeparationMessage.BodyShapeSource2 }
                            let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                            World.publishPlus separationData entity.BodySeparationExplicitEvent eventTrace entity false false world
                    | _ -> ()
                | BodyTransformMessage bodyTransformMessage ->
                    let bodyId = bodyTransformMessage.BodyId
                    match bodyId.BodySource with
                    | :? Entity as entity ->
                        if entity.GetExists world && entity.GetSelected world then
                            let center = bodyTransformMessage.Center
                            if not (Single.IsNaN center.X) then
                                entity.SetXtensionPropertyWithoutEvent "AwakeTimeStamp" world.UpdateTime world
                                if  entity.GetPhysicsMotion world = ManualMotion ||
                                    bodyId.BodyIndex <> Constants.Physics.InternalIndex then
                                    let transformData =
                                        { BodyCenter = center
                                          BodyRotation = bodyTransformMessage.Rotation
                                          BodyLinearVelocity = bodyTransformMessage.LinearVelocity
                                          BodyAngularVelocity = bodyTransformMessage.AngularVelocity }
                                    let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                                    World.publishPlus transformData entity.BodyTransformEvent eventTrace entity false false world
                                else entity.Physics center bodyTransformMessage.Rotation bodyTransformMessage.LinearVelocity bodyTransformMessage.AngularVelocity world
                    | _ -> ()
                | BodyJointBreakMessage bodyJointBreakMessage ->
                    let bodyJointId = bodyJointBreakMessage.BodyJointId
                    match bodyJointId.BodyJointSource with
                    | :? Entity as entity ->
                        if entity.GetExists world && entity.GetSelected world then
                            entity.SetXtensionPropertyWithoutEvent "Broken" true world
                            let breakData =
                                { BodyJointId = bodyJointId
                                  BreakingPoint = bodyJointBreakMessage.BreakingPoint
                                  BreakingOverflow = bodyJointBreakMessage.BreakingOverflow }
                            let eventTrace = EventTrace.debug "World" "processIntegrationMessage" "" EventTrace.empty
                            World.publishPlus breakData entity.BodyJointBreakEvent eventTrace entity false false world
                    | _ -> ()

        /// Sweep the quadtree clean of all empty nodes.
        /// It can make sense to call this after loading a new level.
        static member sweepQuadtree (world : World) =
            Quadtree.sweep world.Quadtree

        /// Sweep the octree clean of all empty nodes.
        /// It can make sense to call this after loading a new level.
        static member sweepOctree (world : World) =
            Octree.sweep world.Octree

        /// Process ImSim for a single frame.
        /// HACK: needed only as a hack for Gaia and other accompanying programs to ensure ImGui simulants are created at a
        /// meaningful time. Do NOT call this in the course of normal operations!
        static member tryProcessSimulants zeroDelta (world : World) =

            // use a finally block to free cached values
            try

                // gather simulants
                world.Timers.UpdateGatherTimer.Restart ()
                let game = Nu.Game.Handle
                let screenOpt = World.getSelectedScreenOpt world
                let groups = World.getGroups1 world
                World.getElements3dInPlay HashSet3dNormalCached world
                World.getElements2dInPlay HashSet2dNormalCached world
                world.Timers.UpdateGatherTimer.Stop ()

                // attempt to process game
                world.Timers.UpdateGameTimer.Restart ()
                World.tryProcessGame zeroDelta game world
                world.Timers.UpdateGameTimer.Stop ()

                // attempt to process screen if any
                world.Timers.UpdateScreensTimer.Restart ()
                match screenOpt with
                | Some screen ->
                    if screen.GetExists world then
                        World.tryProcessScreen zeroDelta screen world
                | None -> ()
                world.Timers.UpdateScreensTimer.Stop ()

                // attempt to process groups
                world.Timers.UpdateGroupsTimer.Restart ()
                for group in groups do
                    if group.GetExists world then
                        World.tryProcessGroup zeroDelta group world
                world.Timers.UpdateGroupsTimer.Stop ()

                // attempt to process entities
                world.Timers.UpdateEntitiesTimer.Restart ()
                for element in HashSet3dNormalCached do
                    if element.Entry.GetExists world then
                        World.tryProcessEntity zeroDelta element.Entry world
                for element in HashSet2dNormalCached do
                    if element.Entry.GetExists world then
                        World.tryProcessEntity zeroDelta element.Entry world
                world.Timers.UpdateEntitiesTimer.Stop ()

            // free cached values
            finally
                HashSet3dNormalCached.Clear ()
                HashSet2dNormalCached.Clear ()

        static member internal sweepSimulants (world : World) =

            // update simulant bookkeeping, collecting simulants to destroy in the process
            for (simulantAddress, simulantImSim) in world.SimulantsImSim do
                if not simulantImSim.SimulantUtilized then
                    let simulant = World.deriveFromAddress simulantAddress
                    ImSimSimulantsToDestroy.Add (simulantImSim.InitializationTime, simulant)
                    World.setSimulantsImSim (SUMap.remove simulantAddress world.SimulantsImSim) world
                else
                    if world.Imperative then
                        simulantImSim.SimulantUtilized <- false
                        simulantImSim.SimulantInitializing <- false
                    else
                        let simulantsImSim = SUMap.add simulantAddress { simulantImSim with SimulantUtilized = false; SimulantInitializing = false } world.SimulantsImSim
                        World.setSimulantsImSim simulantsImSim world
            ImSimSimulantsToDestroy.Sort SimulantImSimComparer

            // destroy simulants
            for (_, simulant) in ImSimSimulantsToDestroy do World.destroy simulant world
            ImSimSimulantsToDestroy.Clear ()

            // update subscription bookkeeping
            for (subscriptionKey, subscriptionImSim) in world.SubscriptionsImSim do
                if not subscriptionImSim.SubscriptionUtilized then
                    World.unsubscribe subscriptionImSim.SubscriptionId world
                    World.setSubscriptionsImSim (SUMap.remove subscriptionKey world.SubscriptionsImSim) world
                else
                    if world.Imperative then
                        subscriptionImSim.SubscriptionUtilized <- false
                    else
                        let simulantsImSim = SUMap.add subscriptionKey { subscriptionImSim with SubscriptionUtilized = false } world.SubscriptionsImSim
                        World.setSubscriptionsImSim simulantsImSim world

        static member private preUpdateSimulants (world : World) =

            // gather simulants
            world.Timers.PreUpdateGatherTimer.Restart ()
            let game = Nu.Game.Handle
            let advancing = world.Advancing
            let screenOpt = World.getSelectedScreenOpt world
            let groups = match screenOpt with Some screen -> World.getGroups screen world | None -> Seq.empty
            world.Timers.PreUpdateGatherTimer.Stop ()

            // pre-update game
            world.Timers.PreUpdateGameTimer.Restart ()
            if advancing then World.preUpdateGame game world
            world.Timers.PreUpdateGameTimer.Stop ()

            // pre-update screen if any
            world.Timers.PreUpdateScreensTimer.Restart ()
            match screenOpt with
            | Some screen ->
                if advancing && screen.GetExists world then
                    World.preUpdateScreen screen world
            | None -> ()
            world.Timers.PreUpdateScreensTimer.Stop ()

            // pre-update groups
            world.Timers.PreUpdateGroupsTimer.Restart ()
            for group in groups do
                if advancing && group.GetExists world then
                    World.preUpdateGroup group world
            world.Timers.PreUpdateGroupsTimer.Stop ()

        static member private updateSimulants (world : World) =

            // use a finally block to free cached values
            try

                // gather simulants
                world.Timers.UpdateGatherTimer.Restart ()
                let game = Nu.Game.Handle
                let advancing = world.Advancing
                let screens = World.getScreens world
                let selectedScreenOpt = World.getSelectedScreenOpt world
                let groups = World.getGroups1 world
                World.getElements3dInPlay HashSet3dNormalCached world
                World.getElements2dInPlay HashSet2dNormalCached world
                world.Timers.UpdateGatherTimer.Stop ()

                // update game
                world.Timers.UpdateGameTimer.Restart ()
                World.tryProcessGame false game world
                if advancing then World.updateGame game world
                world.Timers.UpdateGameTimer.Stop ()

                // process screens
                world.Timers.UpdateScreensTimer.Restart ()
                for screen in screens do
                    if screen.GetExists world then World.tryProcessScreen false screen world
                    if advancing && screen.GetExists world && Option.contains screen selectedScreenOpt then World.updateScreen screen world
                world.Timers.UpdateScreensTimer.Stop ()

                // update groups
                world.Timers.UpdateGroupsTimer.Restart ()
                for group in groups do
                    if group.GetExists world then World.tryProcessGroup false group world
                    if advancing && Option.contains group.Screen selectedScreenOpt && group.GetExists world then World.updateGroup group world
                world.Timers.UpdateGroupsTimer.Stop ()

                // update entities
                world.Timers.UpdateEntitiesTimer.Restart ()
                for element in HashSet3dNormalCached do
                    if element.Entry.GetExists world then
                        World.tryProcessEntity false element.Entry world
                    if element.Entry.GetExists world && (advancing && not (element.Entry.GetStatic world) || element.Entry.GetAlwaysUpdate world) then
                        World.updateEntity element.Entry world
                for element in HashSet2dNormalCached do
                    if element.Entry.GetExists world then
                        World.tryProcessEntity false element.Entry world
                    if element.Entry.GetExists world && (advancing && not (element.Entry.GetStatic world) || element.Entry.GetAlwaysUpdate world) then
                        World.updateEntity element.Entry world
                world.Timers.UpdateEntitiesTimer.Stop ()

            // free cached values
            finally
                HashSet3dNormalCached.Clear ()
                HashSet2dNormalCached.Clear ()

        static member private postUpdateSimulants (world : World) =

            // gather simulants
            world.Timers.PostUpdateGatherTimer.Restart ()
            let game = Nu.Game.Handle
            let advancing = world.Advancing
            let screenOpt = World.getSelectedScreenOpt world
            let groups = match screenOpt with Some screen -> World.getGroups screen world | None -> []
            world.Timers.PostUpdateGatherTimer.Stop ()

            // post-update game
            world.Timers.PostUpdateGameTimer.Restart ()
            if advancing then World.postUpdateGame game world
            world.Timers.PostUpdateGameTimer.Stop ()

            // post-update screen if any
            world.Timers.PostUpdateScreensTimer.Restart ()
            match screenOpt with
            | Some screen -> if advancing && screen.GetExists world then World.postUpdateScreen screen world
            | None -> ()
            world.Timers.PostUpdateScreensTimer.Stop ()

            // post-update groups
            world.Timers.PostUpdateGroupsTimer.Restart ()
            for group in groups do
                if advancing && group.GetExists world then World.postUpdateGroup group world
            world.Timers.PostUpdateGroupsTimer.Stop ()

        static member private renderScreenTransition5 transitionTime (eyeSize : Vector2) renderPass transition (world : World) =
            match renderPass with
            | NormalPass ->
                match transition.DissolveImageOpt with
                | Some dissolveImage ->
                    let progress =
                        match (transitionTime, transition.TransitionLifeTime) with
                        | (UpdateTime time, UpdateTime lifeTime) ->
                            let localTime = world.UpdateTime - time
                            single localTime / single lifeTime
                        | (TickTime time, TickTime lifeTime) ->
                            let localTime = world.TickTime - time
                            single localTime / single lifeTime
                        | (_, _) -> failwithumf ()
                    let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
                    let color = Color.One.WithA alpha
                    let size = eyeSize.V3
                    let mutable transform = Transform.makeDefault ()
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
                                  ClipOpt = ValueNone
                                  Image = dissolveImage
                                  Color = color
                                  Blend = Transparent
                                  Emission = Color.Zero
                                  Flip = FlipNone }}
                        world
                | None -> ()
            | _ -> ()

        static member private renderScreenTransition renderPass (screen : Screen) world =
            match screen.GetTransitionState world with
            | IncomingState transitionTime -> World.renderScreenTransition5 transitionTime world.Eye2dSize renderPass (screen.GetIncoming world) world
            | OutgoingState transitionTime -> World.renderScreenTransition5 transitionTime world.Eye2dSize renderPass (screen.GetOutgoing world) world
            | IdlingState _ -> ()

        static member private renderSimulantsInternal8
            game screenOpt groups (groupsInvisible : _ HashSet)
            (elements3d : _ Octelement HashSet) (elements2d : _ Quadelement HashSet)
            renderPass (world : World) =

            // render game
            World.renderGame renderPass game world

            // render screens
            match screenOpt with
            | Some screen -> World.renderScreen renderPass screen world
            | None -> ()

            // render screen transition
            match World.getSelectedScreenOpt world with
            | Some selectedScreen -> World.renderScreenTransition renderPass selectedScreen world
            | None -> ()

            // render groups
            for group in groups do
                if not (groupsInvisible.Contains group) then
                    World.renderGroup renderPass group world

            // render entities
            world.Timers.RenderEntityMessagesTimer.Restart ()
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements3d do
                    if element.VisibleInView then
                        World.renderEntity renderPass element.Entry world
            else
                for element in elements3d do
                    if element.VisibleInView && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity renderPass element.Entry world
            if world.Unaccompanied || groupsInvisible.Count = 0 then
                for element in elements2d do
                    if element.VisibleInView then
                        World.renderEntity renderPass element.Entry world
            else
                for element in elements2d do
                    if element.VisibleInView && not (groupsInvisible.Contains element.Entry.Group) then
                        World.renderEntity renderPass element.Entry world
            world.Timers.RenderEntityMessagesTimer.Stop ()

        static member private renderSimulantsInternal renderPass (world : World) =

            // use a finally block to free cached values
            try

                // gather simulants
                world.Timers.RenderGatherTimer.Restart ()
                let game = Nu.Game.Handle
                let screenOpt = World.getSelectedScreenOpt world
                let groups = match screenOpt with Some screen -> World.getGroups screen world | None -> Seq.empty
                let groupsInvisible =
                    if world.Accompanied
                    then hashSetPlus HashIdentity.Structural (Seq.filter (fun (group : Group) -> not (group.GetVisible world)) groups)
                    else hashSetPlus HashIdentity.Structural []
                match renderPass with
                | LightMapPass (_, lightMapBounds) ->
                    let hashSet = HashSet ()
                    World.getElements3dInViewBox lightMapBounds hashSet world
                    for element in hashSet do
                        if element.StaticInPlay then
                            HashSet3dNormalCached.Add element |> ignore<bool>
                | ShadowPass (_, _, shadowLightType, _, shadowFrustum) ->
                    let shadowInterior = LightType.shouldShadowInterior shadowLightType
                    World.getElements3dInViewFrustum shadowInterior true shadowFrustum HashSet3dNormalCached world
                | ReflectionPass (_, _) -> ()
                | NormalPass -> World.getElements3dInView HashSet3dNormalCached world
                match renderPass with
                | LightMapPass (_, _) -> ()
                | ShadowPass (_, _, _, _, _) -> ()
                | ReflectionPass (_, _) -> ()
                | NormalPass -> World.getElements2dInView HashSet2dNormalCached world
                world.Timers.RenderGatherTimer.Stop ()

                // render simulants
                World.renderSimulantsInternal8 game screenOpt groups groupsInvisible HashSet3dNormalCached HashSet2dNormalCached renderPass world

            // free cached values
            finally
                HashSet3dNormalCached.Clear ()
                HashSet2dNormalCached.Clear ()

        static member private renderSimulants lightMapRenderRequested world =

            // use a finally block to free cached values
            try

                // render light maps
                if lightMapRenderRequested then
                    let lightProbes = World.getLightProbes3dInView (HashSet HashIdentity.Structural) world // NOTE: this may not be the optimal way to query.
                    let lightProbesStale = Seq.filter (fun (lightProbe : Entity) -> lightProbe.GetProbeStale world) lightProbes
                    for lightProbe in lightProbesStale do
                        let id = lightProbe.GetId world
                        let bounds = lightProbe.GetProbeBounds world
                        let boundsPlus = bounds.ScaleUniform 4.0f // TODO: allow user to specify bounds scalar?
                        let renderPass = LightMapPass (id, boundsPlus)
                        World.renderSimulantsInternal renderPass world
                        World.enqueueRenderMessage3d (RenderLightMap3d { LightProbeId = id; RenderPass = renderPass }) world
                        lightProbe.SetProbeStale false world

                // create shadow pass descriptors
                let eyeCenter = World.getEye3dCenter world
                let lightBox = World.getLight3dViewBox world
                let lights = World.getLights3dInViewBox lightBox HashSet3dShadowCached world // NOTE: this may not be the optimal way to query.
                let shadowPassDescriptorsSortable =
                    [|for light in lights do
                        if light.GetDesireShadows world then
                            let shadowFrustum =
                                light.ComputeShadowFrustum world
                            let shadowInView =
                                let frustumInterior = world.Eye3dFrustumInterior
                                let frustumExterior = world.Eye3dFrustumExterior
                                let frustumImposter = world.Eye3dFrustumImposter
                                match light.GetPresence world with
                                | Interior -> frustumInterior.Intersects shadowFrustum
                                | Exterior -> frustumExterior.Intersects shadowFrustum || frustumInterior.Intersects shadowFrustum
                                | Imposter -> frustumImposter.Intersects shadowFrustum
                                | Omnipresent -> true
                            if shadowInView then
                                let distanceSquared = eyeCenter.DistanceSquared (light.GetPosition world)
                                struct (distanceSquared, struct (shadowFrustum, light))|]

                // sort shadow pass descriptors
                let shadowPassDescriptors =
                    shadowPassDescriptorsSortable
                    |> Array.sortBy fst'
                    |> Array.map snd'

                // render simulant shadows
                let mutable shadowTexturesCount = 0
                let mutable shadowMapsCount = 0
                let mutable shadowCascadesCount = 0
                for struct (shadowFrustum, light : Entity) in shadowPassDescriptors do
                    let lightType = light.GetLightType world
                    match lightType with
                    | PointLight ->
                        if shadowMapsCount < Constants.Render.ShadowMapsMax then

                            // grab light info
                            let lightId = light.GetId world
                            let shadowOrigin = light.GetPosition world
                            let shadowNearDistance = Constants.Render.NearPlaneDistanceInterior
                            let shadowFarDistance = max (light.GetLightCutoff world) (shadowNearDistance * 2.0f)

                            // construct eye rotations
                            let eyeRotations =
                                [|(v3Right, v3Down)     // (+x) right
                                  (v3Left, v3Down)      // (-x) left
                                  (v3Up, v3Back)        // (+y) top
                                  (v3Down, v3Forward)   // (-y) bottom
                                  (v3Back, v3Down)      // (+z) back
                                  (v3Forward, v3Down)|] // (-z) front

                            // construct projection
                            let shadowProjection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, shadowNearDistance, shadowFarDistance)

                            // render faces
                            for i in 0 .. dec 6 do
                                let (eyeForward, eyeUp) = eyeRotations.[i]
                                let shadowRotation = Quaternion.CreateLookAt (eyeForward, eyeUp)
                                let shadowView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + eyeForward, eyeUp)
                                let shadowViewProjection = shadowView * shadowProjection
                                let shadowFrustum = Frustum shadowViewProjection
                                World.renderSimulantsInternal (ShadowPass (lightId, Some (i, shadowView, shadowProjection), lightType, shadowRotation, shadowFrustum)) world

                            // fin
                            shadowMapsCount <- inc shadowMapsCount

                    | SpotLight (_, _) ->
                        if shadowTexturesCount < Constants.Render.ShadowTexturesMax then
                            World.renderSimulantsInternal (ShadowPass (light.GetId world, None, lightType, light.GetRotation world, shadowFrustum)) world
                            shadowTexturesCount <- inc shadowTexturesCount

                    | DirectionalLight ->
                        if shadowTexturesCount < Constants.Render.ShadowTexturesMax then

                            // compute cull frustum
                            let shadowOrigin = light.GetPosition world
                            let shadowRotation = light.GetRotation world
                            let shadowForward = shadowRotation.Down
                            let shadowUp = shadowForward.OrthonormalUp
                            let shadowNearDistance = Constants.Render.NearPlaneDistanceInterior
                            let shadowFarDistance = max (light.GetLightCutoff world) (shadowNearDistance * 2.0f)
                            let cullView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + shadowForward, shadowUp)
                            let cullProjection =
                                Matrix4x4.CreateOrthographic
                                    (shadowFarDistance * +2.0f * inc Constants.Render.ShadowDirectionalMarginRatioCull,
                                     shadowFarDistance * +2.0f * inc Constants.Render.ShadowDirectionalMarginRatioCull,
                                     shadowFarDistance * -1.0f * inc Constants.Render.ShadowDirectionalMarginRatioCull,
                                     shadowFarDistance * +1.0f * inc Constants.Render.ShadowDirectionalMarginRatioCull)
                            let cullFrustum = Frustum (cullView * cullProjection)

                            // render
                            World.renderSimulantsInternal (ShadowPass (light.GetId world, None, lightType, light.GetRotation world, cullFrustum)) world

                            // fin
                            shadowTexturesCount <- inc shadowTexturesCount

                    | CascadedLight ->
                        if shadowCascadesCount < Constants.Render.ShadowCascadesMax then

                            // compute shadow info
                            let lightId = light.GetId world
                            let shadowOrigin = light.GetPosition world
                            let shadowRotation = light.GetRotation world
                            let shadowForward = shadowRotation.Down
                            let shadowUp = shadowForward.OrthonormalUp
                            let shadowNearDistance = Constants.Render.NearPlaneDistanceInterior
                            let shadowFarDistance = max (light.GetLightCutoff world) (shadowNearDistance * 2.0f)

                            // compute eye values
                            let eyeRotation = World.getEye3dRotation world
                            let eyeForward = eyeRotation.Forward
                            let eyeUp = eyeForward.OrthonormalUp
                            let eyeView = Matrix4x4.CreateLookAt (eyeCenter, eyeCenter + eyeForward, eyeUp)
                            let eyeFov = World.getEye3dFieldOfView world
                            let eyeAspectRatio = World.getEye3dAspectRatio world

                            // compute cull frustum
                            let cullView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + shadowForward, shadowUp)
                            let cullProjection =
                                Matrix4x4.CreateOrthographic
                                    (shadowFarDistance * +2.0f * inc Constants.Render.ShadowCascadeMarginRatioCull,
                                     shadowFarDistance * +2.0f * inc Constants.Render.ShadowCascadeMarginRatioCull,
                                     shadowFarDistance * -1.0f * inc Constants.Render.ShadowCascadeMarginRatioCull,
                                     shadowFarDistance * +1.0f * inc Constants.Render.ShadowCascadeMarginRatioCull)
                            let cullFrustum = Frustum (cullView * cullProjection)
                            
                            // use a finally block to free cached values
                            try

                                // gather simulants for rendering
                                // OPTIMIZATION: gather simulants for all cascades so we can call
                                // World.renderSimulantsInternal8.
                                world.Timers.RenderGatherTimer.Restart ()
                                let game = Nu.Game.Handle
                                let screenOpt = World.getSelectedScreenOpt world
                                let groups = match screenOpt with Some screen -> World.getGroups screen world | None -> Seq.empty
                                let groupsInvisible =
                                    if world.Accompanied
                                    then hashSetPlus HashIdentity.Structural (Seq.filter (fun (group : Group) -> not (group.GetVisible world)) groups)
                                    else hashSetPlus HashIdentity.Structural []
                                let shadowInterior = LightType.shouldShadowInterior CascadedLight
                                World.getElements3dInViewFrustum shadowInterior true cullFrustum HashSet3dNormalCached world
                                World.getElements2dInView HashSet2dNormalCached world
                                world.Timers.RenderGatherTimer.Stop ()

                                // render cascades
                                for i in 0 .. dec Constants.Render.ShadowCascadeLevels do

                                    // compute section frustum
                                    let sectionNear =
                                        match i with
                                        | 0 -> Constants.Render.NearPlaneDistanceInterior
                                        | _ -> shadowFarDistance * Constants.Render.ShadowCascadeLimits.[dec i]
                                    let sectionFar = shadowFarDistance * Constants.Render.ShadowCascadeLimits.[i]
                                    let sectionProjection = Matrix4x4.CreatePerspectiveFieldOfView (eyeFov, eyeAspectRatio, sectionNear, sectionFar)
                                    let sectionViewProjection = eyeView * sectionProjection
                                    let sectionFrustum = Frustum sectionViewProjection

                                    // compute frustum corners and center in world space
                                    let sectionCornersWorld = sectionFrustum.Corners
                                    let sectionCenterWorld = Array.sum sectionCornersWorld / single sectionCornersWorld.Length

                                    // compute frustum corner bounds in ortho space
                                    let sectionViewOrtho = Matrix4x4.CreateLookAt (sectionCenterWorld, sectionCenterWorld + shadowForward, shadowUp)
                                    let mutable minX = Single.MaxValue
                                    let mutable maxX = Single.MinValue
                                    let mutable minY = Single.MaxValue
                                    let mutable maxY = Single.MinValue
                                    let mutable minZ = Single.MaxValue
                                    let mutable maxZ = Single.MinValue
                                    for corner in sectionCornersWorld do
                                        let cornerView = corner.Transform sectionViewOrtho
                                        minX <- min minX cornerView.X
                                        maxX <- max maxX cornerView.X
                                        minY <- min minY cornerView.Y
                                        maxY <- max maxY cornerView.Y
                                        minZ <- min minZ cornerView.Z
                                        maxZ <- max maxZ cornerView.Z

                                    // add margins to section along Z's
                                    let depth = maxZ - minZ
                                    let margin = depth * Constants.Render.ShadowCascadeMarginRatio
                                    let margin = max margin Constants.Render.ShadowCascadeMarginSizeMin
                                    let minZ' = minZ - margin
                                    let maxZ' = maxZ + margin

                                    // compute ortho projection
                                    let sectionProjectionOrtho = Matrix4x4.CreateOrthographicOffCenter (minX, maxX, minY, maxY, minZ', maxZ')

                                    // render
                                    World.renderSimulantsInternal8
                                        game screenOpt groups groupsInvisible
                                        HashSet3dNormalCached HashSet2dNormalCached
                                        (ShadowPass (lightId, Some (i, sectionViewOrtho, sectionProjectionOrtho), lightType, shadowRotation, cullFrustum)) world

                            // free cached values
                            finally
                                HashSet3dNormalCached.Clear ()
                                HashSet2dNormalCached.Clear ()

                            // fin
                            shadowCascadesCount <- inc shadowCascadesCount

                // render simulants normally
                World.renderSimulantsInternal NormalPass world

            // free cached values
            finally
                HashSet3dShadowCached.Clear ()

        static member private processInput (world : World) =
            if SDL.SDL_WasInit SDL.SDL_INIT_TIMER <> 0u then
                MouseState.update ()
                KeyboardState.update ()
                let mutable alive = world.Alive
                let mutable polledEvent = SDL.SDL_Event ()
                while
                    alive &&
                    SDL.SDL_PollEvent &polledEvent <> 0 do
                    World.processInput2 polledEvent world
                    alive <- world.Alive
                if not alive then
                    World.exit world

        static member private processPhysics2d world =
            let physicsEngine = World.getPhysicsEngine2d world
            match physicsEngine.TryIntegrate world.GameDelta with
            | Some integrationMessages ->
                let eventTrace = EventTrace.debug "World" "processPhysics2d" "" EventTrace.empty
                World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
                for message in integrationMessages do
                    World.processIntegrationMessage message world
            | None -> ()

        static member private processPhysics3d world =
            let physicsEngine = World.getPhysicsEngine3d world
            match physicsEngine.TryIntegrate world.GameDelta with
            | Some integrationMessages ->
                let eventTrace = EventTrace.debug "World" "processPhysics3d" "" EventTrace.empty
                World.publishPlus { IntegrationMessages = integrationMessages } Nu.Game.Handle.IntegrationEvent eventTrace Nu.Game.Handle false false world
                for message in integrationMessages do
                    World.processIntegrationMessage message world
            | None -> ()

        static member private processPhysics world =
            World.processPhysics3d world
            World.processPhysics2d world

        /// Clean-up the resources held by the world.
        static member cleanUp (world : World) =
            world.WorldExtension.JobGraph.CleanUp ()
            World.unregisterGame Nu.Game.Handle world
            World.cleanUpSubsystems world |> ignore
            world.WorldExtension.Plugin.CleanUp ()

        /// Run the game engine with the given handlers, but don't clean up at the end.
        static member runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess firstFrame (world : World) =

            // run loop if user-defined run-while predicate passes
            world.Timers.FrameTimer.Restart ()
            if world.Alive && runWhile world then

                // run user-defined pre-process callbacks
                world.Timers.PreProcessTimer.Restart ()
                World.preProcess world
                preProcess world
                world.Timers.PreProcessTimer.Stop ()
                if world.Alive then

                    // process screen transitioning
                    // NOTE: not bothering to do timing on this.
                    World.processScreenTransitioning world
                    if world.Alive then

                        // process HID inputs
                        world.Timers.InputTimer.Restart ()
                        World.processInput world
                        world.Timers.InputTimer.Stop ()
                        if world.Alive then

                            // process physics
                            world.Timers.PhysicsTimer.Restart ()
                            World.processPhysics world
                            world.Timers.PhysicsTimer.Stop ()
                            if world.Alive then

                                // pre-update simulants
                                world.Timers.PreUpdateTimer.Restart ()
                                World.preUpdateSimulants world
                                world.Timers.PreUpdateTimer.Stop ()
                                if world.Alive then

                                    // update simulants
                                    world.Timers.UpdateTimer.Restart ()
                                    WorldModule.UpdatingSimulants <- true
                                    World.updateSimulants world
                                    WorldModule.UpdatingSimulants <- false
                                    world.Timers.UpdateTimer.Stop ()
                                    if world.Alive then

                                        // post-update simulants
                                        world.Timers.PostUpdateTimer.Restart ()
                                        World.postUpdateSimulants world
                                        world.Timers.PostUpdateTimer.Stop ()
                                        if world.Alive then

                                            // run user-defined per-process callbacks
                                            world.Timers.PerProcessTimer.Restart ()
                                            World.perProcess world
                                            perProcess world
                                            world.Timers.PerProcessTimer.Stop ()
                                            if world.Alive then

                                                // process coroutines
                                                world.Timers.CoroutinesTimer.Restart ()
                                                World.processCoroutines world
                                                world.Timers.CoroutinesTimer.Stop ()
                                                if world.Alive then

                                                    // process tasklets that have been scheduled and are ready to run
                                                    world.Timers.TaskletsTimer.Restart ()
                                                    WorldModule.EndFrameProcessingStarted <- true
                                                    World.processTasklets world
                                                    world.Timers.TaskletsTimer.Stop ()
                                                    if world.Alive then

                                                        // destroy simulants that have been marked for destruction at the end of frame
                                                        world.Timers.DestructionTimer.Restart ()
                                                        World.processImSim world
                                                        World.destroySimulants world
                                                        world.Timers.DestructionTimer.Stop ()
                                                        if world.Alive then

                                                            // run engine and user-defined post-process callbacks
                                                            world.Timers.PostProcessTimer.Restart ()
                                                            World.postProcess world
                                                            postProcess world
                                                            world.Timers.PostProcessTimer.Stop ()
                                                            if world.Alive then

                                                                // render simulants, skipping culling upon request (like when a light probe needs to be rendered)
                                                                world.Timers.RenderMessagesTimer.Restart ()
                                                                let lightMapRenderRequested = World.getLightMapRenderRequested world
                                                                World.acknowledgeLightMapRenderRequest world
                                                                World.renderSimulants lightMapRenderRequested world
                                                                world.Timers.RenderMessagesTimer.Stop ()
                                                                if world.Alive then

                                                                    // process audio
                                                                    world.Timers.AudioTimer.Restart ()
                                                                    if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u then
                                                                        let audioPlayer = World.getAudioPlayer world
                                                                        let audioMessages = audioPlayer.PopMessages ()
                                                                        audioPlayer.Play audioMessages
                                                                    world.Timers.AudioTimer.Stop ()

                                                                    // process main thread time recording
                                                                    world.Timers.MainThreadTime <- world.Timers.MainThreadTimer.Elapsed

                                                                    // process rendering (1/2)
                                                                    let rendererProcess = World.getRendererProcess world
                                                                    if not firstFrame then rendererProcess.RequestSwap ()

                                                                    // process frame pacing mechanics
                                                                    if world.Timers.MainThreadTimer.IsRunning then

                                                                        // automatically enable frame pacing when need is detected
                                                                        if not world.FramePacing then
                                                                            let frameTimeMinimum = GameTime.DesiredFrameTimeMinimum
                                                                            if world.Timers.MainThreadTimer.Elapsed.TotalSeconds < frameTimeMinimum * 0.9 then FramePaceIssues <- inc FramePaceIssues
                                                                            FramePaceChecks <- inc FramePaceChecks
                                                                            if FramePaceIssues = 15 then World.setFramePacing true world
                                                                            if FramePaceChecks % 30 = 0 then FramePaceIssues <- 0

                                                                        // pace frame when enabled
                                                                        if world.FramePacing then
                                                                            let frameTimeMinimum = GameTime.DesiredFrameTimeMinimum
                                                                            while world.Timers.MainThreadTimer.Elapsed.TotalSeconds < frameTimeMinimum do
                                                                                let timeToSleep = frameTimeMinimum - world.Timers.MainThreadTimer.Elapsed.TotalSeconds
                                                                                if timeToSleep > 0.008 then Thread.Sleep 7
                                                                                elif timeToSleep > 0.004 then Thread.Sleep 3
                                                                                elif timeToSleep > 0.002 then Thread.Sleep 1
                                                                                else Thread.Yield () |> ignore<bool>

                                                                    // process main thread timer
                                                                    world.Timers.MainThreadTimer.Restart ()

                                                                    // process additional frame time recording
                                                                    let gcTotalTime = GC.GetTotalPauseDuration ()
                                                                    let gcFrameTime = gcTotalTime - world.Timers.GcTotalTime
                                                                    world.Timers.GcTotalTime <- gcTotalTime
                                                                    world.Timers.GcFrameTime <- gcFrameTime
                                                                    world.Timers.ImGuiTime <- world.Timers.ImGuiTimer.Elapsed

                                                                    // process imgui frame
                                                                    world.Timers.ImGuiTimer.Restart ()
                                                                    let imGui = World.getImGui world
                                                                    imGui.BeginFrame (single world.DateDelta.TotalSeconds)
                                                                    World.imGuiProcess world
                                                                    imGuiProcess world
                                                                    imGui.InputFrame ()
                                                                    let drawData = imGui.RenderFrame ()
                                                                    world.Timers.ImGuiTimer.Stop ()

                                                                    // process rendering (2/2)
                                                                    rendererProcess.SubmitMessages
                                                                        world.Eye3dFrustumInterior
                                                                        world.Eye3dFrustumExterior
                                                                        world.Eye3dFrustumImposter
                                                                        (World.getLight3dViewBox world)
                                                                        world.Eye3dCenter
                                                                        world.Eye3dRotation
                                                                        world.Eye3dFieldOfView
                                                                        world.Eye2dCenter
                                                                        world.Eye2dSize
                                                                        (World.getWindowSize world)
                                                                        world.GeometryViewport
                                                                        world.RasterViewport
                                                                        world.OuterViewport
                                                                        drawData

                                                                    // post-process imgui frame
                                                                    World.imGuiPostProcess world
                                                                    imGuiPostProcess world

                                                                    // update time and recur
                                                                    world.Timers.FrameTimer.Stop ()
                                                                    WorldModule.EndFrameProcessingStarted <- false
                                                                    World.updateTime world
                                                                    if world.Advancing then
                                                                        World.publish () (Events.TimeUpdateEvent --> Game) Game world
                                                                        match World.getSelectedScreenOpt world with
                                                                        | Some selectedScreen ->
                                                                            World.publish () (Events.TimeUpdateEvent --> selectedScreen) selectedScreen world
                                                                            for group in World.getGroups selectedScreen world do
                                                                                if group.GetExists world then
                                                                                    World.publish () (Events.TimeUpdateEvent --> group) group world
                                                                        | None -> ()

                                                                    // recur
                                                                    World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess false world

        /// Run the game engine using the given world and returning exit code upon termination.
        static member runWithCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess firstFrame world =
            try World.runWithoutCleanUp runWhile preProcess perProcess postProcess imGuiProcess imGuiPostProcess firstFrame world
                World.cleanUp world
                Constants.Engine.ExitCodeSuccess
            with exn ->
                Log.error (scstring exn)
                World.cleanUp world
                Constants.Engine.ExitCodeFailure

[<AutoOpen>]
module EntityDispatcherModule =

    /// The ImSim dispatcher for entities.
    type [<AbstractClass>] EntityDispatcherImSim (is2d, physical, lightProbe, light) =
        inherit EntityDispatcher (is2d, physical, lightProbe, light)

        override this.PresenceOverride =
            ValueSome Omnipresent // by default, we presume Process may produce child entities that may be referred to unconditionally

        override this.TryProcess (zeroDelta, entity, world) =
            let context = world.ContextImSim
            World.scopeEntity entity [] world
            if zeroDelta then
                let advancing = world.Advancing
                let advancementCleared = world.AdvancementCleared
                let updateDelta = world.UpdateDelta
                let clockDelta = world.ClockDelta
                let tickDelta = world.TickDelta
                World.mapAmbientState AmbientState.clearAdvancement world
                this.Process (entity, world)
                World.mapAmbientState (AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta) world
            else this.Process (entity, world)
#if DEBUG
            if world.ContextImSim <> entity.EntityAddress then
                Log.warnOnce
                    ("ImSim context expected to be " +
                     scstring entity.EntityAddress + " but was " +
                     scstring world.ContextImSim + ". Did you forget to call the appropriate World.end function?")
#endif
            World.setContextAndDeclared context entity.EntityAddress world

        /// ImSim process an entity.
        abstract Process : entity : Entity * world : World -> unit
        default this.Process (_, _) = ()

    /// An ImSim 2d entity dispatcher.
    type [<AbstractClass>] Entity2dDispatcherImSim (physical, lightProbe, light) =
        inherit EntityDispatcherImSim (true, physical, lightProbe, light)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity2dSizeDefault]

    /// An ImSim gui entity dispatcher.
    type [<AbstractClass>] GuiDispatcherImSim () =
        inherit EntityDispatcherImSim (true, false, false, false)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Size Constants.Engine.EntityGuiSizeDefault
             define Entity.Presence Omnipresent
             define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

    /// An ImSim 3d entity dispatcher.
    type [<AbstractClass>] Entity3dDispatcherImSim (physical, lightProbe, light) =
        inherit EntityDispatcherImSim (false, physical, lightProbe, light)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity3dSizeDefault]

    /// An ImSim vui dispatcher (gui in 3d).
    type [<AbstractClass>] VuiDispatcherImSim () =
        inherit EntityDispatcherImSim (false, false, false, false)

        static member Properties =
            [define Entity.Size Constants.Engine.EntityVuiSizeDefault]

    type World with

        static member inline internal signalEntity<'model, 'message, 'command when 'message :> Message and 'command :> Command> (signal : Signal) (entity : Entity) world =
            match entity.GetDispatcher world with
            | :? EntityDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (entity.ModelGeneric<'model> ()) signal entity world
            | _ ->
                Log.info "Failed to send signal to entity."

    and Entity with

        /// Send a signal to the entity, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalEntity<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for entities.
    and [<AbstractClass>] EntityDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command>
        (is2d, physical, lightProbe, light, makeInitial : World -> 'model) =
        inherit EntityDispatcher (is2d, physical, lightProbe, light)

        new (is2d, physical, lightProbe, light, initial : 'model) =
            EntityDispatcher<'model, 'message, 'command> (is2d, physical, lightProbe, light, fun _ -> initial)

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
                        property.DesignerType <- typeof<'model>
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.warnOnce "Could not convert existing entity model to new type. Falling back on initial model value."
                        makeInitial world
            World.setEntityModelGeneric<'model> true false model entity world |> ignore<bool>

        override this.Physics (center, rotation, linearVelocity, angularVelocity, entity, world) =
            let model = this.GetModel entity world
            let (signals, model) = this.Physics (center, rotation, linearVelocity, angularVelocity, model, entity, world)
            this.SetModel model entity world
            for signal in signals do
                Signal.processSignal this.Message this.Command (this.Model entity) signal entity world

        override this.Render (renderPass, entity, world) =
            this.Render (this.GetModel entity world, renderPass, entity, world)

        override this.Edit (operation, entity, world) =
            let model = entity.GetModelGeneric<'model> world
            let (signals, model) = this.Edit (model, operation, entity, world)
            this.SetModel model entity world
            for signal in signals do
                Signal.processSignal this.Message this.Command (this.Model entity) signal entity world

        [<DebuggerHidden>]
        override this.Signal (signalObj : obj, entity, world) =
            match signalObj with
            | :? 'message as message -> World.signalEntity<'model, 'message, 'command> message entity world
            | :? 'command as command -> World.signalEntity<'model, 'message, 'command> command entity world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    World.signalEntity<'model, 'message, 'command> message entity world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        World.signalEntity<'model, 'message, 'command> command entity world
                    with _ ->
                        Log.errorOnce
                            ("Incompatible signal type received by entity (signal = '" + scstring signalObj + "'; entity = '" + scstring entity + "').\n" +
                             "This may come about due to sending an incorrect signal type to the entity or due to too significant a change in the signal type when reloading code.")

        override this.TryGetFallbackModel<'a> (modelSymbol, entity, world) =
            this.GetFallbackModel (modelSymbol, entity, world) :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, reinitializing, entity, world) =
            let contentOld = World.getEntityContent entity world
            let model = this.GetModel entity world
            let definitions = this.Definitions (model, entity)
            let entities = this.Content (model, entity)
            let content = Content.composite entity.Name definitions entities
            Content.synchronizeEntity initializing reinitializing contentOld content entity entity world
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

        /// The fallback model value.
        abstract GetFallbackModel : modelSymbol : Symbol * entity : Entity * world : World -> 'model
        default this.GetFallbackModel (_, _, world) = makeInitial world

        /// The entity's own MMCC definitions.
        abstract Definitions : model : 'model * entity : Entity -> Entity DefinitionContent list
        default this.Definitions (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : model : 'model * message : 'message * entity : Entity * world : World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The physics application handler for the MMCC programming model.
        abstract Physics : center : Vector3 * rotation : Quaternion * linearVelocity : Vector3 * angularVelocity : Vector3 * model : 'model * entity : Entity * world : World -> Signal list * 'model
        default this.Physics (_, _, _, _, model, _, _) = just model

        /// Implements additional editing behavior for an entity via the ImGui API.
        abstract Edit : model : 'model * op : EditOperation * entity : Entity * world : World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : model : 'model * command : 'command * entity : Entity * world : World -> unit
        default this.Command (_, _, _, _) = ()

        /// The content specifier of the MMCC programming model.
        abstract Content : model : 'model * entity : Entity -> EntityContent list
        default this.Content (_, _) = []

        /// Render the entity using the given model.
        abstract Render : model : 'model * renderPass : RenderPass * entity : Entity * world : World -> unit
        default this.Render (_, _, _, _) = ()

        /// Truncate the given model.
        abstract TruncateModel : model : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : current : 'model * incoming : 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

    /// A 2d entity dispatcher.
    type [<AbstractClass>] Entity2dDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (physical, lightProbe, light, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, physical, lightProbe, light, makeInitial)

        new (physical, lightProbe, light, initial : 'model) =
            Entity2dDispatcher<'model, 'message, 'command> (physical, lightProbe, light, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity2dSizeDefault]

    /// A gui entity dispatcher.
    type [<AbstractClass>] GuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, false, false, false, makeInitial)

        new (initial : 'model) =
            GuiDispatcher<'model, 'message, 'command> (fun _ -> initial)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Size Constants.Engine.EntityGuiSizeDefault
             define Entity.Presence Omnipresent
             define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

    /// A 3d entity dispatcher.
    type [<AbstractClass>] Entity3dDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (physical, lightProbe, light, makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, physical, lightProbe, light, makeInitial)

        new (physical, lightProbe, light, initial : 'model) =
            Entity3dDispatcher<'model, 'message, 'command> (physical, lightProbe, light, fun _ -> initial)

        static member Properties =
            [define Entity.Size Constants.Engine.Entity3dSizeDefault]

    /// A vui dispatcher (gui in 3d).
    type [<AbstractClass>] VuiDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, false, false, false, makeInitial)

        static member Properties =
            [define Entity.Size Constants.Engine.EntityVuiSizeDefault]

/// Entity PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module EntityPropertyDescriptor =

    /// Check that the described property exists for the given entity.
    let containsPropertyDescriptor propertyName (entity : Entity) world =
        propertyName = Constants.Engine.NamePropertyName ||
        PropertyDescriptor.containsPropertyDescriptor<EntityState> propertyName entity world

    /// Get the property descriptors for the given entity.
    let getPropertyDescriptors (entity : Entity) world =
        let nameDescriptor = { PropertyName = Constants.Engine.NamePropertyName; PropertyType = typeof<string> }
        let propertyDescriptors = PropertyDescriptor.getPropertyDescriptors<EntityState> (Some entity) world
        nameDescriptor :: propertyDescriptors

    /// Get the editor category of the described property.
    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        let baseProperties = Reflection.getPropertyDefinitions typeof<EntityDispatcher>
        let rigidBodyProperties = Reflection.getPropertyDefinitions typeof<RigidBodyFacet>
        if  propertyName = "Name" ||
            propertyName = "Surnames" ||
            propertyName = "MountOpt" ||
            propertyName = "PropagationSourceOpt" ||
            propertyName = "OverlayNameOpt" then
            "Ambient Properties"
        elif propertyName = "Model" then
            "Basic Model Properties"
        elif propertyName = "Degrees" || propertyName = "DegreesLocal" ||
             propertyName = "Elevation" || propertyName = "ElevationLocal" ||
             propertyName = "Offset" || propertyName = "Overflow" ||
             propertyName = "Position" || propertyName = "PositionLocal" ||
             propertyName = "Presence" ||
             propertyName = "Rotation" || propertyName = "RotationLocal" ||
             propertyName = "Scale" || propertyName = "ScaleLocal" ||
             propertyName = "Size" then
             "Basic Transform Properties"
        elif propertyName = "Incoming" || propertyName = "Outgoing" then
             "Transition Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) baseProperties then "Configuration Properties"
        elif propertyName = "MaterialProperties" then "Material Properties"
        elif propertyName = "Material" || propertyName = "Clipped" then "Material Properties 2"
        elif propertyName = "NavShape" || propertyName = "Nav3dConfig" then "Navigation Properties"
        elif List.exists (fun (property : PropertyDefinition) -> propertyName = property.PropertyName) rigidBodyProperties then "Physics Properties"
        else "~ More Properties"

    /// Get whether the described property is editable.
    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if  propertyName = Constants.Engine.OverlayNameOptPropertyName ||
            propertyName = Constants.Engine.FacetNamesPropertyName ||
            propertyName = "PropagatedDescriptorOpt" ||
            propertyName = "Rotation" ||
            propertyName = "RotationLocal" ||
            propertyName = "Angles" ||
            propertyName = "AnglesLocal" ||
            propertyName = "Light" ||
            propertyName = "LightProbe" ||
            propertyName = "PermafrozenPreBatches" ||
            propertyName = "PermafrozenShapes" then
            false
        else
            propertyName = "Degrees" ||
            propertyName = "DegreesLocal" ||
            not (Reflection.isPropertyNonPersistentByName propertyName)

    /// Get the value of the described property for the given entity.
    let getValue propertyDescriptor (entity : Entity) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor entity world with
        | Some value -> value
        | None -> null

    /// Attempt to set the value of the described property for the given entity.
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
                World.renameEntityImmediate entity target world
                Right ()
            else Left ("Invalid entity surnames '" + scstring surnames + "'.")

        // change the name property
        | Constants.Engine.NamePropertyName ->
            let name = value :?> string
            if name.IndexOfAny Symbol.IllegalNameCharsArray = -1 then
                let targetNames =
                    entity.Group.GroupAddress.Names
                    |> flip Array.append (Array.allButLast entity.Surnames)
                    |> Array.add name
                let target = Nu.Entity targetNames
                World.renameEntityImmediate entity target world
                Right ()
            else Left ("Invalid entity name '" + name + "'.")

        // change facet names
        | Constants.Engine.FacetNamesPropertyName ->
            let facetNames = value :?> string Set
            World.trySetEntityFacetNames facetNames entity world |> Either.mapRight ignore

        // change the property dynamically
        | _ ->
            match propertyDescriptor.PropertyName with
            | Constants.Engine.OverlayNameOptPropertyName ->
                World.trySetEntityOverlayNameOpt (value :?> string option) entity world
            | _ ->
                PropertyDescriptor.trySetValue propertyDescriptor value entity world |> ignore
                Right ()

[<AutoOpen>]
module GroupDispatcherModule =

    /// The ImSim dispatcher for groups.
    type [<AbstractClass>] GroupDispatcherImSim () =
        inherit GroupDispatcher ()

        override this.TryProcess (zeroDelta, group, world) =
            let context = world.ContextImSim
            World.scopeGroup group [] world
            if zeroDelta then
                let advancing = world.Advancing
                let advancementCleared = world.AdvancementCleared
                let updateDelta = world.UpdateDelta
                let clockDelta = world.ClockDelta
                let tickDelta = world.TickDelta
                World.mapAmbientState AmbientState.clearAdvancement world
                this.Process (group, world)
                World.mapAmbientState (AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta) world
            else this.Process (group, world)
#if DEBUG
            if world.ContextImSim <> group.GroupAddress then
                Log.warnOnce
                    ("ImSim context expected to be " +
                     scstring group.GroupAddress + " but was " +
                     scstring world.ContextImSim + ". Did you forget to call the appropriate World.end function?")
#endif
            World.setContextAndDeclared context group.GroupAddress world

        /// ImSim process a group.
        abstract Process : group : Group * world : World -> unit
        default this.Process (_, _) = ()

    type World with

        static member inline internal signalGroup<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (group : Group) world =
            match group.GetDispatcher world with
            | :? GroupDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (group.ModelGeneric<'model> ()) signal group world
            | _ ->
                Log.info "Failed to send signal to group."

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
                        property.DesignerType <- typeof<'model>
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.warnOnce "Could not convert existing group model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGroupModelGeneric<'model> true false model group world |> ignore<bool>

        override this.Render (renderPass, group, world) =
            this.Render (this.GetModel group world, renderPass, group, world)

        override this.Edit (operation, group, world) =
            let model = group.GetModelGeneric<'model> world
            let (signals, model) = this.Edit (model, operation, group, world)
            this.SetModel model group world
            for signal in signals do
                Signal.processSignal this.Message this.Command (this.Model group) signal group world

        [<DebuggerHidden>]
        override this.Signal (signalObj : obj, group, world) =
            match signalObj with
            | :? 'message as message -> World.signalGroup<'model, 'message, 'command> message group world
            | :? 'command as command -> World.signalGroup<'model, 'message, 'command> command group world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    World.signalGroup<'model, 'message, 'command> message group world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        World.signalGroup<'model, 'message, 'command> command group world
                    with _ ->
                        Log.errorOnce
                            ("Incompatible signal type received by group (signal = '" + scstring signalObj + "'; group = '" + scstring group + "').\n" +
                             "This may come about due to sending an incorrect signal type to the group or due to too significant a change in the signal type when reloading code.")

        override this.TryGetFallbackModel<'a> (modelSymbol, group, world) =
            this.GetFallbackModel (modelSymbol, group, world) :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, reinitializing, group, world) =
            let contentOld = World.getGroupContent group world
            let model = this.GetModel group world
            let definitions = this.Definitions (model, group)
            let entities = this.Content (model, group)
            let content = Content.group group.Name definitions entities
            Content.synchronizeGroup initializing reinitializing contentOld content group group world
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

        /// The fallback model value.
        abstract GetFallbackModel : Symbol * Group * World -> 'model
        default this.GetFallbackModel (_, _, world) = makeInitial world

        /// The group's own MMCC definitions.
        abstract Definitions : model : 'model * group : Group -> Group DefinitionContent list
        default this.Definitions (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : model : 'model * message : 'message * group : Group * world : World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : model : 'model * command : 'command * group : Group * world : World -> unit
        default this.Command (_, _, _, _) = ()

        /// The content specifier of the MMCC programming model.
        abstract Content : model : 'model * group : Group -> EntityContent list
        default this.Content (_, _) = []

        /// Render the group using the given model.
        abstract Render : model : 'model * renderPass : RenderPass * group : Group * world : World -> unit
        default this.Render (_, _, _, _) = ()

        /// Implements additional editing behavior for a group via the ImGui API.
        abstract Edit : model : 'model * op : EditOperation * group : Group * world : World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : model : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : current : 'model * incoming : 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

/// Group PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module GroupPropertyDescriptor =

    /// Check that the described property exists for the given group.
    let containsPropertyDescriptor propertyName (group : Group) world =
        PropertyDescriptor.containsPropertyDescriptor<GroupState> propertyName group world

    /// Get the property descriptors for the given group.
    let getPropertyDescriptors (group : Group) world =
        PropertyDescriptor.getPropertyDescriptors<GroupState> (Some group) world

    /// Get the editor category of the described property.
    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" ||  propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "Persistent" || propertyName = "Elevation" || propertyName = "Visible" then "Built-In Properties"
        else "Xtension Properties"

    /// Get whether the described property is editable.
    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    /// Get the value of the described property for the given group.
    let getValue propertyDescriptor (group : Group) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor group world with
        | Some value -> value
        | None -> null

    /// Attempt to set the value of the described property for the given group.
    let trySetValue (value : obj) propertyDescriptor (group : Group) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a group after it has been created is not yet implemented.")

        // change the property dynamically
        | _ ->
            PropertyDescriptor.trySetValue propertyDescriptor value group world |> ignore
            Right ()

[<AutoOpen>]
module ScreenDispatcherModule =

    let private ScreenDispatcherImSimTryProcessSubscriptionName = string Gen.id

    /// The ImSim dispatcher for screens.
    type [<AbstractClass>] ScreenDispatcherImSim () =
        inherit ScreenDispatcher ()

        override this.TryProcess (zeroDelta, screen, world) =
            let context = world.ContextImSim
            World.scopeScreen screen [] world
            let results = World.doSubscriptionToSelectionEvents ScreenDispatcherImSimTryProcessSubscriptionName screen world
            if zeroDelta then
                let advancing = world.Advancing
                let advancementCleared = world.AdvancementCleared
                let updateDelta = world.UpdateDelta
                let clockDelta = world.ClockDelta
                let tickDelta = world.TickDelta
                World.mapAmbientState AmbientState.clearAdvancement world
                this.Process (FQueue.ofSeq results, screen, world)
                World.mapAmbientState (AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta) world
            else this.Process (FQueue.ofSeq results, screen, world)
#if DEBUG
            if world.ContextImSim <> screen.ScreenAddress then
                Log.warnOnce
                    ("ImSim context expected to be " +
                     scstring screen.ScreenAddress + " but was " +
                     scstring world.ContextImSim + ". Did you forget to call World.endGroup?")
#endif
            World.setContextAndDeclared context screen.ScreenAddress world

        /// ImSim process a screen.
        abstract Process : selectionResults : SelectionEventData FQueue * screen : Screen * world : World -> unit
        default this.Process (_, _, _) = ()

    type World with

        static member inline internal signalScreen<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (screen : Screen) world =
            match screen.GetDispatcher world with
            | :? ScreenDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (screen.ModelGeneric<'model> ()) signal screen world
            | _ ->
                Log.info "Failed to send signal to screen."

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
                        property.DesignerType <- typeof<'model>
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.warnOnce "Could not convert existing screen model to new type. Falling back on initial model value."
                        makeInitial world
            World.setScreenModelGeneric<'model> true false model screen world |> ignore<bool>

        override this.Render (renderPass, screen, world) =
            this.Render (this.GetModel screen world, renderPass, screen, world)

        override this.Edit (operation, screen, world) =
            let model = screen.GetModelGeneric<'model> world
            let (signals, model) = this.Edit (model, operation, screen, world)
            this.SetModel model screen world
            for signal in signals do
                Signal.processSignal this.Message this.Command (this.Model screen) signal screen world

        [<DebuggerHidden>]
        override this.Signal (signalObj : obj, screen, world) =
            match signalObj with
            | :? 'message as message -> World.signalScreen<'model, 'message, 'command> message screen world
            | :? 'command as command -> World.signalScreen<'model, 'message, 'command> command screen world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    World.signalScreen<'model, 'message, 'command> message screen world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        World.signalScreen<'model, 'message, 'command> command screen world
                    with _ ->
                        Log.errorOnce
                            ("Incompatible signal type received by screen (signal = '" + scstring signalObj + "'; screen = '" + scstring screen + "').\n" +
                             "This may come about due to sending an incorrect signal type to the screen or due to too significant a change in the signal type when reloading code.")

        override this.TryGetFallbackModel<'a> (modelSymbol, screen, world) =
            this.GetFallbackModel (modelSymbol, screen, world) :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, reinitializing, screen, world) =
            let contentOld = World.getScreenContent screen world
            let model = this.GetModel screen world
            let definitions = this.Definitions (model, screen)
            let group = this.Content (model, screen)
            let content = Content.screen screen.Name Vanilla definitions group
            Content.synchronizeScreen initializing reinitializing contentOld content screen screen world
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

        /// The fallback model value.
        abstract GetFallbackModel : modelSymbol : Symbol * screen : Screen * world : World -> 'model
        default this.GetFallbackModel (_, _, world) = makeInitial world

        /// The screen's own MMCC definitions.
        abstract Definitions : model : 'model * screen : Screen -> Screen DefinitionContent list
        default this.Definitions (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : model : 'model * message : 'message * screen : Screen * world : World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : model : 'model * command : 'command * screen : Screen * world : World -> unit
        default this.Command (_, _, _, _) = ()

        /// The content specifier of the MMCC programming model.
        abstract Content : model : 'model * screen : Screen -> GroupContent list
        default this.Content (_, _) = []

        /// Render the screen using the given model.
        abstract Render : model : 'model * renderPass : RenderPass * screen : Screen * world : World -> unit
        default this.Render (_, _, _, _) = ()

        /// Implements additional editing behavior for a screen via the ImGui API.
        abstract Edit : model : 'model * op :  EditOperation * screen : Screen * world : World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : model : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : current : 'model * incoming : 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

/// Screen PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module ScreenPropertyDescriptor =

    /// Check that the described property exists for the given screen.
    let containsPropertyDescriptor propertyName (screen : Screen) world =
        PropertyDescriptor.containsPropertyDescriptor<ScreenState> propertyName screen world

    /// Get the property descriptors for the given screen.
    let getPropertyDescriptors (screen : Screen) world =
        PropertyDescriptor.getPropertyDescriptors<ScreenState> (Some screen) world

    /// Get the editor category of the described property.
    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" || propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "Persistent" || propertyName = "Incoming" || propertyName = "Outgoing" || propertyName = "SlideOpt" then "Built-In Properties"
        else "Xtension Properties"

    /// Get whether the described property is editable.
    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    /// Get the value of the described property for the given screen.
    let getValue propertyDescriptor (screen : Screen) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor screen world with
        | Some value -> value
        | None -> null

    /// Attempt to set the value of the described property for the given screen.
    let trySetValue (value : obj) propertyDescriptor (screen : Screen) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a screen after it has been created is not yet implemented.")

        // change the property dynamically
        | _ ->
            PropertyDescriptor.trySetValue propertyDescriptor value screen world |> ignore
            Right ()

[<AutoOpen>]
module GameDispatcherModule =

    /// The ImSim dispatcher for games.
    type [<AbstractClass>] GameDispatcherImSim () =
        inherit GameDispatcher ()

        override this.TryProcess (zeroDelta, game, world) =
            let context = world.ContextImSim
            World.scopeGame [] world
            if zeroDelta then
                let advancing = world.Advancing
                let advancementCleared = world.AdvancementCleared
                let updateDelta = world.UpdateDelta
                let clockDelta = world.ClockDelta
                let tickDelta = world.TickDelta
                World.mapAmbientState AmbientState.clearAdvancement world
                this.Process (game, world)
                World.mapAmbientState (AmbientState.restoreAdvancement advancing advancementCleared updateDelta clockDelta tickDelta) world
            else this.Process (game, world)
#if DEBUG
            if world.ContextImSim <> game.GameAddress then
                Log.warnOnce
                    ("ImSim context expected to be " +
                     scstring game.GameAddress + " but was " +
                     scstring world.ContextImSim + ". Did you forget to call World.endScreen?")
#endif
            World.setContextAndDeclared context game.GameAddress world

        /// ImSim process a game.
        abstract Process : game : Game * world : World -> unit
        default this.Process (_, _) = ()

    type World with

        static member inline internal signalGame<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (game : Game) world =
            match game.GetDispatcher world with
            | :? GameDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (game.ModelGeneric<'model> ()) signal game world
            | _ -> Log.info "Failed to send signal to game."

    and Game with

        /// Send a signal to the game, explicitly specifing MMCC types.
        member this.SignalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal world =
            World.signalGame<'model, 'message, 'command> signal this world

    /// The MMCC dispatcher for games.
    and [<AbstractClass>] GameDispatcher<'model, 'message, 'command when 'message :> Message and 'command :> Command> (makeInitial : World -> 'model) =
        inherit GameDispatcher ()

        static let synchronize initializing reinitializing game world (this : GameDispatcher<'model, 'message, 'command>) =
            let contentOld = World.getGameContent game world
            let model = this.GetModel game world
            let definitions = this.Definitions (model, game)
            let screens = this.Content (model, game)
            let content = Content.game definitions screens
            let initialScreenOpt = Content.synchronizeGame World.setScreenSlide initializing reinitializing contentOld content game game world
            World.setGameContent content game world
            initialScreenOpt

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
                        property.DesignerType <- typeof<'model>
                        property.DesignerValue <- model
                        model
                    with _ ->
                        Log.warnOnce "Could not convert existing game model to new type. Falling back on initial model value."
                        makeInitial world
            World.setGameModelGeneric<'model> true false model game world |> ignore<bool>

        override this.Render (renderPass, game, world) =
            this.Render (this.GetModel game world, renderPass, game, world)

        override this.Edit (operation, game, world) =
            let model = game.GetModelGeneric<'model> world
            let (signals, model) = this.Edit (model, operation, game, world)
            this.SetModel model game world
            for signal in signals do
                Signal.processSignal this.Message this.Command (this.Model game) signal game world

        [<DebuggerHidden>]
        override this.Signal (signalObj : obj, game, world) =
            match signalObj with
            | :? 'message as message -> World.signalGame<'model, 'message, 'command> message game world
            | :? 'command as command -> World.signalGame<'model, 'message, 'command> command game world
            | _ ->
                try let message = signalObj |> valueToSymbol |> symbolToValue : 'message
                    World.signalGame<'model, 'message, 'command> message game world
                with _ ->
                    try let command = signalObj |> valueToSymbol |> symbolToValue : 'command
                        World.signalGame<'model, 'message, 'command> command game world
                    with _ ->
                        Log.errorOnce
                            ("Incompatible signal type received by game (signal = '" + scstring signalObj + "'; game = '" + scstring game + "').\n" +
                             "This may come about due to sending an incorrect signal type to the game or due to too significant a change in the signal type when reloading code.")

        override this.TryGetFallbackModel<'a> (modelSymbol, game, world) =
            this.GetFallbackModel (modelSymbol, game, world) :> obj :?> 'a |> Some

        override this.TrySynchronize (initializing, reinitializing, game, world) =
            synchronize initializing reinitializing game world this |> ignore<Screen option>

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

        /// The fallback model value.
        abstract GetFallbackModel : modelSymbol : Symbol * game : Game * world : World -> 'model
        default this.GetFallbackModel (_, _, world) = makeInitial world

        /// The game own MMCC definitions.
        abstract Definitions : model : 'model * game : Game -> Game DefinitionContent list
        default this.Definitions (_, _) = []

        /// The message handler of the MMCC programming model.
        abstract Message : model : 'model * message : 'message * game : Game * world : World -> Signal list * 'model
        default this.Message (model, _, _, _) = just model

        /// The command handler of the MMCC programming model.
        abstract Command : model : 'model * command : 'command * game : Game * world : World -> unit
        default this.Command (_, _, _, _) = ()

        /// The content specifier of the MMCC programming model.
        abstract Content : model : 'model * game : Game -> ScreenContent list
        default this.Content (_, _) = []

        /// Render the game using the given model.
        abstract Render : model : 'model * renderPass : RenderPass * game : Game * world : World -> unit
        default this.Render (_, _, _, _) = ()

        /// Implements additional editing behavior for a game via the ImGui API.
        abstract Edit : model : 'model * op : EditOperation * game : Game * world : World -> Signal list * 'model
        default this.Edit (model, _, _, _) = just model

        /// Truncate the given model.
        abstract TruncateModel : model : 'model -> 'model
        default this.TruncateModel model = model

        /// Untruncate the given model.
        abstract UntruncateModel : current : 'model * incoming : 'model -> 'model
        default this.UntruncateModel (_, incoming) = incoming

/// Game PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module GamePropertyDescriptor =

    /// Check that the described property exists for the game.
    let containsPropertyDescriptor propertyName (game : Game) world =
        PropertyDescriptor.containsPropertyDescriptor<GameState> propertyName game world

    /// Get the property descriptors for the game.
    let getPropertyDescriptors (game : Game) world =
        PropertyDescriptor.getPropertyDescriptors<GameState> (Some game) world

    /// Get the editor category of the described property.
    let getCategory propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        if propertyName = "Name" ||  propertyName.EndsWith "Model" then "Ambient Properties"
        elif propertyName = "DesiredScreen" || propertyName = "ScreenTransitionDestinationOpt" || propertyName = "SelectedScreenOpt" ||
             propertyName = "Eye2dCenter" || propertyName = "Eye2dSize" || propertyName = "Eye3dCenter" || propertyName = "Eye3dRotation" || propertyName = "Eye3dFieldOfView" then
             "Built-In Properties"
        else "Xtension Properties"

    /// Get whether the described property is editable.
    let getEditable propertyDescriptor =
        let propertyName = propertyDescriptor.PropertyName
        not (Reflection.isPropertyNonPersistentByName propertyName)

    /// Get the value of the described property for the game.
    let getValue propertyDescriptor (game : Game) world : obj =
        match PropertyDescriptor.tryGetValue propertyDescriptor game world with
        | Some value -> value
        | None -> null

    /// Attempt to set the value of the described property for the game.
    let trySetValue (value : obj) propertyDescriptor (game : Game) world =
        
        // pull string quotes out of string
        let value =
            match value with
            | :? string as str -> str.Replace ("\"", "") :> obj
            | _ -> value
            
        // change the name property
        match propertyDescriptor.PropertyName with
        | Constants.Engine.NamePropertyName ->
            Left ("Changing the name of a game after it has been created is not yet implemented.")

        // change the property dynamically
        | _ ->
            PropertyDescriptor.trySetValue propertyDescriptor value game world |> ignore
            Right ()

/// Simulant PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module SimulantPropertyDescriptor =

    /// Check that the described property exists for the given simulant.
    let containsPropertyDescriptor propertyName (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.containsPropertyDescriptor propertyName entity world
        | :? Group as group -> GroupPropertyDescriptor.containsPropertyDescriptor propertyName group world
        | :? Screen as screen -> ScreenPropertyDescriptor.containsPropertyDescriptor propertyName screen world
        | :? Game as game -> GamePropertyDescriptor.containsPropertyDescriptor propertyName game world
        | _ -> failwithumf ()

    /// Get the property descriptors for the given simulant.
    let getPropertyDescriptors (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.getPropertyDescriptors entity world
        | :? Group as group -> GroupPropertyDescriptor.getPropertyDescriptors group world
        | :? Screen as screen -> ScreenPropertyDescriptor.getPropertyDescriptors screen world
        | :? Game as game -> GamePropertyDescriptor.getPropertyDescriptors game world
        | _ -> failwithumf ()

    /// Get the editor category of the described property.
    let getCategory propertyDesciptor (simulant : Simulant) =
        match simulant with
        | :? Entity -> EntityPropertyDescriptor.getCategory propertyDesciptor
        | :? Group -> GroupPropertyDescriptor.getCategory propertyDesciptor
        | :? Screen -> ScreenPropertyDescriptor.getCategory propertyDesciptor
        | :? Game -> GamePropertyDescriptor.getCategory propertyDesciptor
        | _ -> failwithumf ()

    /// Get whether the described property is editable.
    let getEditable propertyDesciptor (simulant : Simulant) =
        match simulant with
        | :? Entity -> EntityPropertyDescriptor.getEditable propertyDesciptor
        | :? Group -> GroupPropertyDescriptor.getEditable propertyDesciptor
        | :? Screen -> ScreenPropertyDescriptor.getEditable propertyDesciptor
        | :? Game -> GamePropertyDescriptor.getEditable propertyDesciptor
        | _ -> failwithumf ()

    /// Get the value of the described property for the given simulant.
    let getValue propertyDescriptor (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.getValue propertyDescriptor entity world
        | :? Group as group -> GroupPropertyDescriptor.getValue propertyDescriptor group world
        | :? Screen as screen -> ScreenPropertyDescriptor.getValue propertyDescriptor screen world
        | :? Game as game -> GamePropertyDescriptor.getValue propertyDescriptor game world
        | _ -> failwithumf ()

    /// Attempt to set the value of the described property for the given simulant.
    let trySetValue value propertyDescriptor (simulant : Simulant) world =
        match simulant with
        | :? Entity as entity -> EntityPropertyDescriptor.trySetValue value propertyDescriptor entity world
        | :? Group as group -> GroupPropertyDescriptor.trySetValue value propertyDescriptor group world
        | :? Screen as screen -> ScreenPropertyDescriptor.trySetValue value propertyDescriptor screen world
        | :? Game as game -> GamePropertyDescriptor.trySetValue value propertyDescriptor game world
        | _ -> failwithumf ()

/// Universal function definitions for the world (3/4).
[<AutoOpen>]
module WorldModule3 =

    type World with

        /// Send a signal to a simulant.
        static member inline signal (signal : Signal) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> (entity.GetDispatcher world).Signal (signal, entity, world)
            | :? Group as group -> (group.GetDispatcher world).Signal (signal, group, world)
            | :? Screen as screen -> (screen.GetDispatcher world).Signal (signal, screen, world)
            | :? Game as game -> (game.GetDispatcher world).Signal (signal, game, world)
            | _ -> failwithumf ()

        /// Send a signal to a simulant, explicitly specifing MMCC types.
        static member inline signalPlus<'model, 'message, 'command when 'message :> Message and 'command :> Command> signal (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.signalEntity<'model, 'message, 'command> signal entity world
            | :? Group as group -> World.signalGroup<'model, 'message, 'command> signal group world
            | :? Screen as screen -> World.signalScreen<'model, 'message, 'command> signal screen world
            | :? Game as game -> World.signalGame<'model, 'message, 'command> signal game world
            | _ -> failwithumf ()

        static member internal updateLateBindings3 (lateBindings : LateBindings) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity ->
                let entityState = World.getEntityState entity world
                match lateBindings with
                | :? Facet as facet ->
                    match Array.tryFindIndex (fun (facet2 : Facet) -> getTypeName facet2 = getTypeName facet) entityState.Facets with
                    | Some index ->
                        let visibleInViewOld = entityState.VisibleInView
                        let staticInPlayOld = entityState.StaticInPlay
                        let lightProbeOld = entityState.LightProbe
                        let lightOld = entityState.Light
                        let presenceOld = entityState.Presence
                        let presenceInPlayOld = entityState.PresenceInPlay
                        let boundsOld = entityState.Bounds
                        World.unregisterEntityIndex (getType entityState.Facets.[index]) entity world
                        if world.Imperative then
                            entityState.Facets.[index] <- facet
                        else
                            let facets = entityState.Facets.Clone () :?> Facet array
                            facets.[index] <- facet
                            let entityState = { entityState with Facets = facets }
                            World.setEntityState entityState entity world
                        World.registerEntityIndex (getType facet) entity world
                        World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                        World.updateEntityPresenceOverride entity world
                        World.attachEntityMissingProperties entity world
                    | None -> ()
                | :? EntityDispatcher as dispatcher ->
                    if getTypeName entityState.Dispatcher = getTypeName dispatcher then
                        let visibleInViewOld = entityState.VisibleInView
                        let staticInPlayOld = entityState.StaticInPlay
                        let lightProbeOld = entityState.LightProbe
                        let lightOld = entityState.Light
                        let presenceOld = entityState.Presence
                        let presenceInPlayOld = entityState.PresenceInPlay
                        let boundsOld = entityState.Bounds
                        let intrinsicFacetNamesOld = World.getEntityIntrinsicFacetNames entityState
                        World.unregisterEntityIndex (getType entityState.Dispatcher) entity world
                        if world.Imperative then
                            entityState.Dispatcher <- dispatcher
                        else
                            let entityState = { entityState with Dispatcher = dispatcher }
                            World.setEntityState entityState entity world
                        World.registerEntityIndex (getType dispatcher) entity world
                        World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                        let entityState = World.getEntityState entity world
                        let intrinsicFacetNamesNew = World.getEntityIntrinsicFacetNames entityState
                        let intrinsicFacetNamesAdded = Set.difference intrinsicFacetNamesNew intrinsicFacetNamesOld
                        let entityState = World.tryAddFacets intrinsicFacetNamesAdded entityState (Some entity) world |> Either.getRight
                        let intrinsicFacetNamesRemoved = Set.difference intrinsicFacetNamesOld intrinsicFacetNamesNew
                        let _ = World.tryRemoveFacets intrinsicFacetNamesRemoved entityState (Some entity) world |> Either.getRight
                        World.updateEntityPresenceOverride entity world
                        World.attachEntityMissingProperties entity world
                | _ -> ()
            | :? Group as group ->
                let groupState = World.getGroupState group world
                match lateBindings with
                | :? GroupDispatcher as dispatcher ->
                    if getTypeName groupState.Dispatcher = getTypeName dispatcher then
                        World.setGroupState { groupState with Dispatcher = dispatcher } group world
                        World.attachGroupMissingProperties group world
                | _ -> ()
            | :? Screen as screen ->
                let screenState = World.getScreenState screen world
                match lateBindings with
                | :? ScreenDispatcher as dispatcher ->
                    if getTypeName screenState.Dispatcher = getTypeName dispatcher then
                        World.setScreenState { screenState with Dispatcher = dispatcher } screen world
                        World.attachScreenMissingProperties screen world
                | _ -> ()
            | :? Game as game ->
                let gameState = World.getGameState game world
                match lateBindings with
                | :? GameDispatcher as dispatcher ->
                    if getTypeName gameState.Dispatcher = getTypeName dispatcher then
                        World.setGameState { gameState with Dispatcher = dispatcher } game world
                        World.attachGameMissingProperties game world
                | _ -> ()
            | _ -> failwithumf ()