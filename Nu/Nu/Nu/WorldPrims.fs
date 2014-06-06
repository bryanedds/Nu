namespace Nu
open System
open Prime
open Nu
open Nu.NuCore
open Nu.NuConstants
open Nu.NuMath
open Nu.Audio
open Nu.Rendering
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game

// TODO: when module exportation is implemented in F# - http://fslang.uservoice.com/forums/245727-f-language/suggestions/5688199-allow-re-exporting-from-modules
// make this module exported from World and rename to WorldPrimsModule.

module WorldPrims =

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        initMathConverters ()
        initAudioConverters ()
        initRenderConverters ()
    
    /// Mark a message as handled.
    let handleMessage message =
        { Handled = true; Data = message.Data }

    let private getSimulant address world =
        match address with
        | [] -> Game <| world.Game
        | [_] as screenAddress -> Screen <| get world (worldScreenLens screenAddress)
        | [_; _] as groupAddress -> Group <| get world (worldGroupLens groupAddress)
        | [_; _; _] as entityAddress -> Entity <| get world (worldEntityLens entityAddress)
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let private getSubscribedSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let private isAddressSelected address world =
        let optScreenAddress = get world worldOptSelectedScreenAddressLens
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: addressTail, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let private getPublishingPriority simulant =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> getPickingPriority entity

    let private subscriptionSort subscriptions world =
        let simulants = getSubscribedSimulants subscriptions world
        let priorities = List.map getPublishingPriority simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message for the given event.
    let rec publish event publisher message world : bool * World =
        let optSubList = Map.tryFind event world.Subscriptions
        match optSubList with
        | None -> (true, world)
        | Some subList ->
            let subListSorted = subscriptionSort subList world
            let (_, keepRunning, world'') =
                List.foldWhile
                    (fun (message', keepRunning', world'3) (subscriber, subscription) ->
                        if message'.Handled || not keepRunning' then None
                        elif not <| isAddressSelected subscriber world'3 then Some (message', keepRunning', world'3)
                        else
                            let result =
                                match subscription with
                                | ExitSub -> handleEventAsExit message' world'3
                                | SwallowSub -> handleEventAsSwallow message' world'3
                                | ScreenTransitionSub destination -> handleEventAsScreenTransition destination message' world'3
                                | CustomSub fn -> fn event publisher subscriber message' world'3
                            Some result)
                    (message, true, world)
                    subListSorted
            (keepRunning, world'')

    /// Subscribe to messages for the given event.
    and subscribe event subscriber subscription world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind event subs
        { world with
            Subscriptions =
                match optSubList with
                | None -> Map.add event [(subscriber, subscription)] subs
                | Some subList -> Map.add event ((subscriber, subscription) :: subList) subs }

    /// Unsubscribe to messages for the given event.
    and unsubscribe event subscriber world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind event subs
        match optSubList with
        | None -> world
        | Some subList ->
            let subList' = List.remove (fun (address, _) -> address = subscriber) subList
            let subscriptions' = Map.add event subList' subs
            { world with Subscriptions = subscriptions' }

    /// Execute a procedure within the context of a given subscription for the given event.
    and withSubscription event subscription subscriber procedure world =
        let world' = subscribe event subscriber subscription world
        let world'' = procedure world'
        unsubscribe event subscriber world''
    
    and handleEventAsSwallow message (world : World) =
        (handleMessage message, true, world)

    and handleEventAsExit message (world : World) =
        (handleMessage message, false, world)

    // TODO: consider turning this into a lens, and removing the screenStateLens
    and private getScreenState address world =
        let screen = get world <| worldScreenLens address
        get screen screenStateLens

    // TODO: consider turning this into a lens, and removing the screenStateLens
    and private setScreenState address state world =
        let screen = set state (get world <| worldScreenLens address) screenStateLens
        let world' = set screen world <| worldScreenLens address
        match state with
        | IdlingState ->
            world' |>
                unsubscribe DownMouseLeftEvent address |>
                unsubscribe UpMouseLeftEvent address
        | IncomingState | OutgoingState ->
            world' |>
                subscribe DownMouseLeftEvent address SwallowSub |>
                subscribe UpMouseLeftEvent address SwallowSub

    and transitionScreen destination world =
        let world' = setScreenState destination IncomingState world
        set (Some destination) world' worldOptSelectedScreenAddressLens

    and private updateTransition1 (transition : Transition) =
        if transition.Ticks = transition.Lifetime then ({ transition with Ticks = 0 }, true)
        else ({ transition with Ticks = transition.Ticks + 1 }, false)

    and updateTransition update world =
        let (keepRunning, world') =
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> (true, world)
            | Some selectedScreenAddress ->
                let screenState = getScreenState selectedScreenAddress world
                match screenState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let incoming = get selectedScreen screenIncomingLens
                    let (incoming', finished) = updateTransition1 incoming
                    let selectedScreen' = set incoming' selectedScreen screenIncomingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else IncomingState) world''
                    if finished then
                        publish
                            (FinishedIncomingEvent @ selectedScreenAddress)
                            selectedScreenAddress
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | OutgoingState ->
                    let selectedScreen = get world <| worldScreenLens selectedScreenAddress
                    let outgoing = get selectedScreen screenOutgoingLens
                    let (outgoing', finished) = updateTransition1 outgoing
                    let selectedScreen' = set outgoing' selectedScreen screenOutgoingLens
                    let world'' = set selectedScreen' world <| worldScreenLens selectedScreenAddress
                    let world'3 = setScreenState selectedScreenAddress (if finished then IdlingState else OutgoingState) world''
                    if finished then
                        publish
                            (FinishedOutgoingEvent @ selectedScreenAddress)
                            selectedScreenAddress
                            { Handled = false; Data = NoData }
                            world'3
                    else (true, world'3)
                | IdlingState -> (true, world)
        if keepRunning then update world'
            else (keepRunning, world')

    and private handleSplashScreenIdleTick idlingTime ticks event publisher subscriber message world =
        let world' = unsubscribe event subscriber world
        if ticks < idlingTime then
            let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime (ticks + 1)
            let world'' = subscribe event subscriber subscription world'
            (message, true, world'')
        else
            let optSelectedScreenAddress = get world' worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world'' = setScreenState selectedScreenAddress OutgoingState world'
                (message, true, world'')

    and handleSplashScreenIdle idlingTime event publisher subscriber message world =
        let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime 0
        let world' = subscribe TickEvent subscriber subscription world
        (handleMessage message, true, world')

    and private handleFinishedScreenOutgoing destination event publisher subscriber message world =
        let world' = unsubscribe event subscriber world
        let world'' = transitionScreen destination world'
        (handleMessage message, true, world'')

    and handleEventAsScreenTransition destination message world =
        let world' = transitionScreen destination world
        (handleMessage message, true, world')