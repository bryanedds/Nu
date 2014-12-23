namespace Nu
open System
open System.IO
open System.Xml
open SDL2
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
module WorldTests =

    let UnitEventAddress = stoa<unit> "Test"
    let StringEventAddress = stoa<string> "Test"
    let TestEntityName = "TestEntity"
    let TestGroupName = "TestGroup"
    let TestScreenName = "TestScreen"
    let TestEntityAddress = ltoa<Entity> [TestScreenName; TestGroupName; TestEntityName]
    let TestGroupAddress = ltoa<Group> [TestScreenName; TestGroupName]
    let TestScreenAddress = ltoa<Screen> [TestScreenName]
    let TestFilePath = "Test.xml"
    let incUserStateAndCascade (_ : Event<unit, Game>) world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve (_ : Event<unit, Game>) world = (Resolve, World.updateUserState inc world)

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
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress GameAddress world
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress GameAddress world
        let world = World.subscribe4 incUserStateAndResolve UnitEventAddress GameAddress world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe key incUserStateAndResolve UnitEventAddress GameAddress world
        let world = World.unsubscribe key world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] entitySubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let entity = World.makeEntity typeof<EntityDispatcher>.Name (Some TestEntityName) world
        let group = World.makeGroup typeof<GroupDispatcher>.Name (Some TestGroupName) world
        let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some TestScreenName) world
        let groupHierarchies = Map.singleton group.Name (group, Map.singleton entity.Name entity)
        let screenHierarchy = (screen, groupHierarchies)
        let world = snd <| World.addScreen screenHierarchy TestScreenAddress world
        let handleEvent = fun event world -> (Cascade, World.setUserState event.Subscriber.Name world)
        let world = World.subscribe4 handleEvent StringEventAddress TestEntityAddress world
        let world = World.publish4 String.Empty StringEventAddress GameAddress world
        Assert.Equal<string> (TestEntityName, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        // TODO: make stronger assertions in here!!!
        World.init ()
        let world = World.makeEmpty 0
        let entity = World.makeEntity typeof<EntityDispatcher>.Name (Some TestEntityName) world
        let group = World.makeGroup typeof<GroupDispatcher>.Name (Some TestGroupName) world
        let screen = World.makeScreen typeof<ScreenDispatcher>.Name (Some TestScreenName) world
        let game = world.Game
        let screenHierarchies =
            Map.singleton screen.Name <|
                (screen, Map.singleton group.Name <|
                    (group, Map.singleton entity.Name entity))
        let gameHierarchy = (game, screenHierarchies)
        World.writeGameHierarchyToFile TestFilePath gameHierarchy world
        let (_, screenHierarchies) = World.readGameHierarchyFromFile TestFilePath world
        let (screen', groupHierarchies) = Map.find TestScreenName screenHierarchies
        Assert.Equal<string> (screen.Name, screen'.Name)
        let (group', entities) = Map.find TestGroupName groupHierarchies
        Assert.Equal<string> (group.Name, group'.Name)
        let entity' = Map.find TestEntityName entities
        Assert.Equal<string> (entity.Name, entity'.Name)