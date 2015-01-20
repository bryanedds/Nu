// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

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
        let world = observe UnitEventAddress Game |> subscribe incUserStateAndCascade <| world
        let world = World.publish4 () UnitEventAddress Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        let world = World.initAndMakeEmpty 0
        let observation = observe UnitEventAddress Game
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribeWithUnsub incUserStateAndCascade observation world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let (unsubscribe, world) = observe UnitEventAddress Game |> subscribeWithUnsub incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress Game world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe UnitEventAddress Game |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish4 () UnitEventAddress Game world
        let world = World.publish4 () UnitEventAddress Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe IntEventAddress Game |>
            map (fun event _ -> event.Data * 2) |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        let world = World.initAndMakeEmpty 0
        let world =
            observe IntEventAddress Game |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress Game world
        let world = World.publish4 2 IntEventAddress Game world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        let world = World.initAndMakeEmpty 0
        let (unsubscribe, world) =
            observe IntEventAddress Game |>
            scan2 (fun a _ _ -> a) |>
            subscribeWithUnsub incUserStateAndCascade <|
            world
        let world = World.publish4 0 IntEventAddress Game world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] interativeFrpWorks () =
        let world = World.initAndMakeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name (Some BobName) group world
        let world = world |> (bob, bob.GetVisible) *--> (jim, jim.SetVisible)
        let world = bob.SetVisible false world
        Assert.False <| bob.GetVisible world
        Assert.False <| jim.GetVisible world

    let [<Fact>] interativeFrpCyclicWorks () =
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
        Assert.True <| bob.GetVisible world
        Assert.True <| jim.GetVisible world