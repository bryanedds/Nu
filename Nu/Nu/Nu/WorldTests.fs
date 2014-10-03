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

    let TestEventAddress = !* "Test"
    let TestEntityName = "TestEntity"
    let TestGroupName = "TestGroup"
    let TestScreenName = "TestScreen"
    let TestEntityAddress = !+ [TestScreenName; TestGroupName; TestEntityName]
    let TestGroupAddress = !+ [TestScreenName; TestGroupName]
    let TestScreenAddress = !+ [TestScreenName]
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
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.subscribe4 TestEventAddress Address.empty (CustomSub incUserStateAndPropagate) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe key TestEventAddress Address.empty (CustomSub incUserStateAndResolve) world
        let world = World.unsubscribe key world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] entitySubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let entity = World.makeEntity typeof<EntityDispatcher>.Name (Some TestEntityName) world
        let group = World.makeGroup typeof<GroupDispatcher>.Name (Some TestGroupName) world
        let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some TestScreenName) world
        let descriptors = Map.singleton group.Name (group, Map.singleton entity.Name entity)
        let world = snd <| World.addScreen TestScreenAddress screen descriptors world
        let handleEvent = fun event world ->
            let entity = Event.unwrapS<Entity> event
            let world = World.setUserState entity.Name world
            (Propagate, world)
        let world = World.subscribe4 TestEventAddress TestEntityAddress (CustomSub handleEvent) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal<string> (TestEntityName, World.getUserState world)