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

    let UnitEventAddress = stoa<unit> "Test"
    let StringEventAddress = stoa<string> "Test"
    let TestEntityName = "TestEntity"
    let TestGroupName = "TestGroup"
    let TestScreenName = "TestScreen"
    let TestEntityAddress = ltoa<Entity> [TestScreenName; TestGroupName; TestEntityName]
    let TestGroupAddress = ltoa<Group> [TestScreenName; TestGroupName]
    let TestScreenAddress = ltoa<Screen> [TestScreenName]
    let incUserStateAndCascade (_ : unit Event) world = (Cascade, World.transformUserState inc world)
    let incUserStateAndResolve (_ : unit Event) world = (Resolve, World.transformUserState inc world)

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
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndCascade world
        let world = World.publish4 Address.empty UnitEventAddress () world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndCascade world
        let world = World.publish4 Address.empty UnitEventAddress () world
        let world = World.publish4 Address.empty UnitEventAddress () world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndCascade world
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndCascade world
        let world = World.publish4 Address.empty UnitEventAddress () world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndResolve world
        let world = World.subscribe4 Address.empty UnitEventAddress incUserStateAndCascade world
        let world = World.publish4 Address.empty UnitEventAddress () world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe key Address.empty UnitEventAddress incUserStateAndResolve world
        let world = World.unsubscribe key world
        let world = World.publish4 Address.empty UnitEventAddress () world
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
            let entity = World.unwrapS<Entity, string> event world
            let world = World.setUserState entity.Name world
            (Cascade, world)
        let world = World.subscribe4 TestEntityAddress StringEventAddress handleEvent world
        let world = World.publish4 Address.empty StringEventAddress String.Empty world
        Assert.Equal<string> (TestEntityName, World.getUserState world)