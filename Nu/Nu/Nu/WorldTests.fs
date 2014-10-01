namespace Nu
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Nu
open Nu.Constants
module WorldTests =

    let TestEventName = !* "Test"
    let incUserStateAndPropagate _ world = (Propagate, World.transformUserState incI world)
    let incUserStateAndResolve _ world = (Resolved, World.transformUserState incI world)

    let [<Fact>] worldDoesntExplode () =
        World.init ()
        ignore <| World.makeEmpty ()

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe key TestEventName Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.unsubscribe key world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] reactSubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor = React.subscribe TestEventName ^^ React.using Address.empty incUserStateAndPropagate world
        let world = World.publish4 TestEventName Address.empty (NoData ()) reactor.World
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] reactUnsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor = React.subscribe TestEventName ^^ React.using Address.empty incUserStateAndPropagate world
        let world = reactor.Unsubscriber reactor.World
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] reactFilterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.filter (fun _ world -> World.getUserState world = 0) ^^
            React.using Address.empty incUserStateAndPropagate world
        let reactor = React.subscribe TestEventName reactor
        let reactor = React.subscribe TestEventName reactor
        let world = World.publish4 TestEventName Address.empty (NoData ()) reactor.World
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] reactMapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventName ^^
            React.map React.unwrapV ^^
            React.using Address.empty (fun a world -> (Propagate, World.setUserState a world)) world
        let world = World.publish4 TestEventName Address.empty (UserData { UserValue = 0 }) reactor.World
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] reactScanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventName ^^
            React.map React.unwrapV ^^
            React.scan (fun b a _ -> b + a) 0 ^^
            React.using Address.empty (fun data world -> (Propagate, World.setUserState data world)) world
        let world = World.publish4 TestEventName Address.empty (UserData { UserValue = 1 }) reactor.World
        let world = World.publish4 TestEventName Address.empty (UserData { UserValue = 2 }) world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] reactScanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventName ^^
            React.scan2 (fun e _ _ -> e) ^^
            React.using Address.empty (fun _ world -> (Propagate, world)) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) reactor.World
        let world = reactor.Unsubscriber world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates

    (*
            let world =
                subscribe TickEventName ^^
                filter isGamePlaying ^^
                map unwrapAS ^^
                choice (CollisionEventName + address) ^^
                using address testHandler world
                *)