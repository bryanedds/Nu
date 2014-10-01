namespace Nu
open System
open System.IO
open System.Xml
open SDL2
open Xunit
open Prime
open Nu
open Nu.Constants
module WorldTests =

    let TestEventName = !* "Test"
    let incUserStateAndPropagate _ world = (Propagate, World.transformUserState incI world)
    let incUserStateAndResolve _ world = (Resolved, World.transformUserState incI world)

    let [<Fact>] emptyWorldDoesntExplode () =
        World.init ()
        let world = World.makeEmpty ()
        match World.processInput (SDL.SDL_Event ()) world with
        | (Running, world) ->
            match World.processUpdate id world with
            | (Running, world) ->
                let world = World.processRender id world
                ignore <| World.processPlay world
            | (Exiting, _) -> failwith "Test should not reach here."
        | (Exiting, _) -> failwith "Test should not reach here."

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