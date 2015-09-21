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

    let UnitEventAddress = ntoa<unit> "Unit"
    let StringEventAddress = ntoa<string> "String"
    let TestFilePath = "TestFile.xml"
    let incUserStateAndCascade (_ : Event<unit, Game>) world = (Cascade, World.updateUserState inc world)
    let incUserStateAndResolve (_ : Event<unit, Game>) world = (Resolve, World.updateUserState inc world)

    let [<Fact>] runNuForOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] subscribeWorks () =
        let world = World.makeEmpty 0
        let world = World.subscribe incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = World.makeEmpty 0
        let world = World.subscribe incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = World.makeEmpty 0
        let world = World.subscribe incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.subscribe incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = World.makeEmpty 0
        let world = World.subscribe incUserStateAndCascade UnitEventAddress Simulants.Game world
        let world = World.subscribe incUserStateAndResolve UnitEventAddress Simulants.Game world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        let key = World.makeSubscriptionKey ()
        let world = World.makeEmpty 0
        let world = World.subscribe5 key incUserStateAndResolve UnitEventAddress Simulants.Game world
        let world = World.unsubscribe key world
        let world = World.publish () UnitEventAddress Simulants.Game world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Simulants.DefaultEntity.EntityName) Simulants.DefaultGroup world |> snd
        let handleEvent = fun event world -> (Cascade, World.updateUserState (fun _ -> event.Subscriber) world)
        let world = World.subscribe handleEvent StringEventAddress Simulants.DefaultEntity world
        let world = World.publish String.Empty StringEventAddress Simulants.Game world
        Assert.Equal<Simulant> (Simulants.DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        // TODO: make stronger assertions in here!!!
        let world = World.makeEmpty ()
        let world = World.createScreen typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Simulants.DefaultEntity.EntityName) Simulants.DefaultGroup world |> snd
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<string> (Simulants.DefaultScreen.GetName oldWorld, Simulants.DefaultScreen.GetName world)
        Assert.Equal<string> (Simulants.DefaultGroup.GetName oldWorld, Simulants.DefaultGroup.GetName world)
        Assert.Equal<string> (Simulants.DefaultEntity.GetName oldWorld, Simulants.DefaultEntity.GetName world)