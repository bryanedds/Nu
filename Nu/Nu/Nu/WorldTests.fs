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

    let [<Fact>] publishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] publishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] publishWithTwoSubscribesWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] publishWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.subscribe4 TestEventName Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] publishWithUnsubscribeWorks () =
        World.init ()
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe key TestEventName Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.unsubscribe key world
        let world = World.publish4 TestEventName Address.empty (NoData ()) world
        Assert.Equal (0, World.getUserState world)
