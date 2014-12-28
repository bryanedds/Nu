namespace Nu
open System
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
module ReactTests =

    let IntEventAddress = stoa<int> "Test"
    let UnitEventAddress = stoa<unit> "Test"
    let incUserStateAndCascade _ world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.updateUserState inc world)

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = observe UnitEventAddress GameAddress |> subscribe incUserStateAndCascade <| world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let observable = observe UnitEventAddress GameAddress |> using incUserStateAndCascade
        let world = subscribe2 observable world
        let (unsubscribe, world) = subscribeWithUnsub2 observable world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) = observe UnitEventAddress GameAddress |> subscribeWithUnsub incUserStateAndCascade <| world
        let world = unsubscribe world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe UnitEventAddress GameAddress |>
            filter (fun _ world -> World.getUserState world = 0) |>
            subscribe incUserStateAndCascade <|
            world
        let world = World.publish4 () UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress GameAddress |>
            map (fun event _ -> event.Data * 2) |>
            scan (fun _ world -> World.getUserState world = 0) |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            observe IntEventAddress GameAddress |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            subscribe (fun event world -> (Cascade, World.setUserState event.Data world)) <|
            world
        let world = World.publish4 1 IntEventAddress GameAddress world
        let world = World.publish4 2 IntEventAddress GameAddress world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) = observe IntEventAddress GameAddress |> scan2 (fun a _ _ -> a) |> subscribeWithUnsub2 <| world
        let world = World.publish4 0 IntEventAddress GameAddress world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates