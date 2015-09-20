// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
open Nu.Observation
module ObservationTests =

    let IntEventAddress = ntoa<int> "Test"
    let UnitEventAddress = ntoa<unit> "Test"
    let JimName = "Jim"
    let BobName = "Bob"
    let incUserStateAndCascade _ world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.updateUserState inc world)

    let [<Fact>] subscribeWorks () =
        let world = World.empty |> World.setUserState 0
        let world = observe UnitEventAddress Simulants.Game |> subscribe incUserStateAndCascade <| world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        let world = World.empty |> World.setUserState 0
        let observation = observe UnitEventAddress Simulants.Game
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribePlus incUserStateAndCascade observation world
        let world = unsubscribe world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let world = World.empty |> World.setUserState 0
        let (unsubscribe, world) = observe UnitEventAddress Simulants.Game |> subscribePlus incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.True ^ Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        let world = World.empty |> World.setUserState 0
        let world =
            observe UnitEventAddress Simulants.Game |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish () UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        let world = World.empty |> World.setUserState 0
        let world =
            observe IntEventAddress Simulants.Game |>
            map (fun event _ -> event.Data * 2) |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish 1 IntEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        let world = World.empty |> World.setUserState 0
        let world =
            observe IntEventAddress Simulants.Game |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish 1 IntEventAddress Simulants.Game world
        let world = World.publish 2 IntEventAddress Simulants.Game world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        let world = World.empty |> World.setUserState 0
        let (unsubscribe, world) =
            observe IntEventAddress Simulants.Game |>
            scan2 (fun a _ _ -> a) |>
            subscribePlus incUserStateAndCascade <|
            world
        let world = World.publish 0 IntEventAddress Simulants.Game world
        let world = unsubscribe world
        Assert.True ^ Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] iterativeFrpWorks () =
        let world = World.empty
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name None (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name None (Some Constants.Engine.DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name None (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name None (Some BobName) group world
        let world = world |> (bob, bob.GetVisible) *-> (jim, jim.SetVisible)
        let world = bob.SetVisible false world
        Assert.False ^ bob.GetVisible world
        Assert.False ^ jim.GetVisible world

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.empty
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name None (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name None (Some Constants.Engine.DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name None (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name None (Some BobName) group world
        let world =
            world |>
                (bob, bob.GetVisible) *-> (jim, jim.SetVisible) |>
                (jim, jim.GetVisible) /-> (bob, not >> bob.SetVisible)
        let world = bob.SetVisible false world
        Assert.True ^ bob.GetVisible world
        Assert.True ^ jim.GetVisible world