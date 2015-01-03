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
    let JimAddress = gatoea DefaultGroupAddress JimName
    let BobName = "Bob"
    let BobAddress = gatoea DefaultGroupAddress BobName
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
        let observation = observe UnitEventAddress GameAddress
        let world = subscribe incUserStateAndCascade observation world
        let (unsubscribe, world) = subscribeWithUnsub incUserStateAndCascade observation world
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
        let (unsubscribe, world) =
            observe IntEventAddress GameAddress |>
            scan2 (fun a _ _ -> a) |>
            subscribeWithUnsub incUserStateAndCascade <|
            world
        let world = World.publish4 0 IntEventAddress GameAddress world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates

    let [<Fact>] discreteFrpWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let jim = World.makeEntity typeof<EntityDispatcher>.Name (Some JimName) world
        let bob = World.makeEntity typeof<EntityDispatcher>.Name (Some BobName) world
        let group = World.makeGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) world
        let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some DefaultScreenName) world
        let world = snd <| World.addScreen (screen, Map.singleton group.Name (group, Map.ofList [(jim.Name, jim); (bob.Name, bob)])) DefaultScreenAddress world
        let world = world |> (BobAddress, Entity.getVisible) --> (JimAddress, Entity.setVisible)
        let world = World.updateEntity (Entity.setVisible false) BobAddress world
        Assert.True <| World.getEntityBy (not << Entity.getVisible) BobAddress world
        Assert.True <| World.getEntityBy (not << Entity.getVisible) JimAddress world

    let [<Fact>] discreteFrpCyclicWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let jim = World.makeEntity typeof<EntityDispatcher>.Name (Some JimName) world
        let bob = World.makeEntity typeof<EntityDispatcher>.Name (Some BobName) world
        let group = World.makeGroup typeof<GroupDispatcher>.Name (Some DefaultGroupName) world
        let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some DefaultScreenName) world
        let world = snd <| World.addScreen (screen, Map.singleton group.Name (group, Map.ofList [(jim.Name, jim); (bob.Name, bob)])) DefaultScreenAddress world
        let world =
            world |>
            (BobAddress, Entity.getVisible) --> (JimAddress, Entity.setVisible) |>
            (JimAddress, Entity.getVisible) -/> (BobAddress, not >> Entity.setVisible)
        let world = World.updateEntity (Entity.setVisible false) BobAddress world
        Assert.True <| World.getEntityBy Entity.getVisible BobAddress world
        Assert.True <| World.getEntityBy Entity.getVisible JimAddress world