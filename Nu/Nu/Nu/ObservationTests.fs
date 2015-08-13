// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
open Nu.Observation
module ObservationTests =

    let IntEventAddress = stoa<int> "Test"
    let UnitEventAddress = stoa<unit> "Test"
    let JimName = "Jim"
    let BobName = "Bob"
    let incUserStateAndCascade _ world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.updateUserState inc world)

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = observe UnitEventAddress Simulants.Game |> subscribe incUserStateAndCascade <| world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let observation = observe UnitEventAddress Simulants.Game
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribeWithUnsub incUserStateAndCascade observation world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) = observe UnitEventAddress Simulants.Game |> subscribeWithUnsub incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe UnitEventAddress Simulants.Game |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress Simulants.Game |>
            map (fun event _ -> event.Data * 2) |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish4 1 IntEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress Simulants.Game |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish4 1 IntEventAddress Simulants.Game world
        let world = World.publish4 2 IntEventAddress Simulants.Game world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) =
            observe IntEventAddress Simulants.Game |>
            scan2 (fun a _ _ -> a) |>
            subscribeWithUnsub incUserStateAndCascade <|
            world
        let world = World.publish4 0 IntEventAddress Simulants.Game world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] interativeFrpWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name (Some BobName) group world
        let world = world |> (bob, bob.GetVisible) *-> (jim, jim.SetVisible)
        let world = bob.SetVisible false world
        Assert.False <| bob.GetVisible world
        Assert.False <| jim.GetVisible world

    let [<Fact>] interativeFrpCyclicWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (jim, world) = World.createEntity typeof<EntityDispatcher>.Name (Some JimName) group world
        let (bob, world) = World.createEntity typeof<EntityDispatcher>.Name (Some BobName) group world
        let world =
            world |>
                (bob, bob.GetVisible) *-> (jim, jim.SetVisible) |>
                (jim, jim.GetVisible) /-> (bob, not >> bob.SetVisible)
        let world = bob.SetVisible false world
        Assert.True <| bob.GetVisible world
        Assert.True <| jim.GetVisible world