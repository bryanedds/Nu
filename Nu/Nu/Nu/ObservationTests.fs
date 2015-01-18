namespace Nu
open System
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
module ObservationTests =

    let IntEventAddress = stoa<int> "Test"
    let UnitEventAddress = stoa<unit> "Test"
    let JimName = "Jim"
    let BobName = "Bob"
    let incUserStateAndCascade _ world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.updateUserState inc world)

    let [<Fact>] subscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let world = observe UnitEventAddress GameRep |> subscribe incUserStateAndCascade <| world
        let world = World.publish4 () UnitEventAddress GameRep world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        let world = World.initAndMakeEmpty 0
        let observation = observe UnitEventAddress GameRep
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribeWithUnsub incUserStateAndCascade observation world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress GameRep world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let (unsubscribe, world) = observe UnitEventAddress GameRep |> subscribeWithUnsub incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress GameRep world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe UnitEventAddress GameRep |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish4 () UnitEventAddress GameRep world
        let world = World.publish4 () UnitEventAddress GameRep world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe IntEventAddress GameRep |>
            map (fun event _ -> event.Data * 2) |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress GameRep world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe IntEventAddress GameRep |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress GameRep world
        let world = World.publish4 2 IntEventAddress GameRep world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        let world = World.initAndMakeEmpty 0
        let (unsubscribe, world) =
            observe IntEventAddress GameRep |>
            scan2 (fun a _ _ -> a) |>
            subscribeWithUnsub incUserStateAndCascade <|
            world
        let world = World.publish4 0 IntEventAddress GameRep world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] discreteFrpWorks () =
        let world = World.initAndMakeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name (Some BobName) group world
        let world = world |> (bob, bob.GetVisible) *--> (jim, jim.SetVisible)
        let world = bob.SetVisible false world
        Assert.False <| bob.GetVisible world
        Assert.False <| bob.GetVisible world

    let [<Fact>] discreteFrpCyclicWorks () =
        let world = World.initAndMakeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name (Some BobName) group world
        let world =
            world |>
                (bob, bob.GetVisible) *--> (jim, jim.SetVisible) |>
                (jim, jim.GetVisible) /--> (bob, not >> bob.SetVisible)
        let world = bob.SetVisible false world
        Assert.False <| bob.GetVisible world
        Assert.False <| bob.GetVisible world