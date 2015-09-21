// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
open Nu.Observation
module ObservationTests =

    let Jim = Simulants.DefaultGroup => "Jim"
    let Bob = Simulants.DefaultGroup => "Bob"
    let IntEventAddress = ntoa<int> "Test"
    let UnitEventAddress = ntoa<unit> "Test"
    let incUserStateAndCascade _ world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.updateUserState inc world)

    let [<Fact>] subscribeWorks () =
        let world = World.makeEmpty 0
        let world = observe UnitEventAddress Simulants.Game |> subscribe incUserStateAndCascade <| world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        let world = World.makeEmpty 0
        let observation = observe UnitEventAddress Simulants.Game
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribePlus incUserStateAndCascade observation world
        let world = unsubscribe world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let world = World.makeEmpty 0
        let (unsubscribe, world) = observe UnitEventAddress Simulants.Game |> subscribePlus incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.True ^ Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        let world = World.makeEmpty 0
        let world =
            observe UnitEventAddress Simulants.Game |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish () UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress Simulants.Game |>
            map (fun event _ -> event.Data * 2) |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish 1 IntEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress Simulants.Game |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.updateUserState (fun _ -> event.Data) world)) <|
            world
        let world = World.publish 1 IntEventAddress Simulants.Game world
        let world = World.publish 2 IntEventAddress Simulants.Game world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        let world = World.makeEmpty 0
        let (unsubscribe, world) =
            observe IntEventAddress Simulants.Game |>
            scan2 (fun a _ _ -> a) |>
            subscribePlus incUserStateAndCascade <|
            world
        let world = World.publish 0 IntEventAddress Simulants.Game world
        let world = unsubscribe world
        Assert.True ^ Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world = world |> Jim.SetPublishChanges true |> Bob.SetPublishChanges true
        let world = world |> (Bob, Bob.GetVisible) *-> (Jim, Jim.SetVisible)
        let world = Bob.SetVisible false world
        Assert.False ^ Bob.GetVisible world
        Assert.False ^ Jim.GetVisible world

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world = world |> Jim.SetPublishChanges true |> Bob.SetPublishChanges true
        let world =
            world |>
                (Bob, Bob.GetVisible) *-> (Jim, Jim.SetVisible) |>
                (Jim, Jim.GetVisible) /-> (Bob, not >> Bob.SetVisible)
        let world = Bob.SetVisible false world
        Assert.True ^ Bob.GetVisible world
        Assert.True ^ Jim.GetVisible world