// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open System.IO
open System.Xml
open SDL2
open Xunit
open Prime
open Nu
module WorldTests =

    let UnitEventAddress = stoa<unit> "Unit"
    let StringEventAddress = stoa<string> "String"
    let TestFilePath = "TestFile.xml"
    let incUserStateAndCascade (_ : Event<unit, Game>) world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve (_ : Event<unit, Game>) world = (Resolve, World.updateUserState inc world)

    let [<Fact>] emptyWorldDoesntExplode () =
        let world = World.initAndMakeEmpty ()
        match World.processInput (SDL.SDL_Event ()) world with
        | (Running, world) ->
            match World.processUpdate id world with
            | (Running, world) ->
                let world = World.processRender id world
                ignore <| World.processPlay world
            | (Exiting, _) -> failwith "Test should not reach here."
        | (Exiting, _) -> failwith "Test should not reach here."

    let [<Fact>] subscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Proxies.Game world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = World.initAndMakeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Proxies.Game world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = World.initAndMakeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Proxies.Game world
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Proxies.Game world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = World.initAndMakeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Proxies.Game world
        let world = World.subscribe4 incUserStateAndResolve UnitEventAddress Proxies.Game world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let key = World.makeSubscriptionKey ()
        let world = World.subscribe key incUserStateAndResolve UnitEventAddress Proxies.Game world
        let world = World.unsubscribe key world
        let world = World.publish4 () UnitEventAddress Proxies.Game world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] entitySubscribeWorks () =
        let world = World.initAndMakeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (entity, world) = World.createEntity typeof<EntityDispatcher>.Name (Some Constants.Engine.DefaultEntityName) group world
        let handleEvent = fun event world -> (Cascade, World.updateUserState (fun _ -> event.Subscriber) world)
        let world = World.subscribe4 handleEvent StringEventAddress entity world
        let world = World.publish4 String.Empty StringEventAddress Proxies.Game world
        Assert.Equal<Simulant> (Proxies.DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        // TODO: make stronger assertions in here!!!
        let world = World.initAndMakeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (entity, world) = World.createEntity typeof<EntityDispatcher>.Name (Some Constants.Engine.DefaultEntityName) group world
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<string> (screen.GetName oldWorld, screen.GetName world)
        Assert.Equal<string> (group.GetName oldWorld, group.GetName world)
        Assert.Equal<string> (entity.GetName oldWorld, entity.GetName world)