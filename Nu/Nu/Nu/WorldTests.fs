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

    let [<Fact>] runWorldForOneFrameThenCleanUp () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.run6 id id SdlDeps.empty (Some 1) Running world
        let world = World.cleanUp world
        ignore world

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = World.subscribe4 incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.subscribe4 incUserStateAndResolve UnitEventAddress Simulants.Game world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let key = World.makeSubscriptionKey ()
        let world = World.subscribe key incUserStateAndResolve UnitEventAddress Simulants.Game world
        let world = World.unsubscribe key world
        let world = World.publish4 () UnitEventAddress Simulants.Game world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] entitySubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (entity, world) = World.createEntity typeof<EntityDispatcher>.Name (Some Constants.Engine.DefaultEntityName) group world
        let handleEvent = fun event world -> (Cascade, World.updateUserState (fun _ -> event.Subscriber) world)
        let world = World.subscribe4 handleEvent StringEventAddress entity world
        let world = World.publish4 String.Empty StringEventAddress Simulants.Game world
        Assert.Equal<Simulant> (Simulants.DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        // TODO: make stronger assertions in here!!!
        World.init ()
        let world = World.makeEmpty 0
        let (screen, world) = World.createScreen typeof<ScreenDispatcher>.Name (Some Constants.Engine.DefaultScreenName) world
        let (group, world) = World.createGroup typeof<GroupDispatcher>.Name (Some Constants.Engine.DefaultGroupName) screen world
        let (entity, world) = World.createEntity typeof<EntityDispatcher>.Name (Some Constants.Engine.DefaultEntityName) group world
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<string> (screen.GetName oldWorld, screen.GetName world)
        Assert.Equal<string> (group.GetName oldWorld, group.GetName world)
        Assert.Equal<string> (entity.GetName oldWorld, entity.GetName world)