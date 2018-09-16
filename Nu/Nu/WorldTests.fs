// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
open Nu.Simulants
module WorldTests =

    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = DefaultLayer => "Jim"
    let Bob = DefaultLayer => "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> World.updateUserValue (fun _ -> evt.Subscriber) world
        let world = World.subscribe handleEvent StringEvent DefaultEntity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Game world
        Assert.Equal<Simulant> (DefaultEntity :> Simulant, World.getUserValue world)

    let [<Fact>] gameSerializationWorks () =
        let world = World.makeDefault ()
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<string> (DefaultScreen.GetName oldWorld, DefaultScreen.GetName world)
        Assert.Equal<string> (DefaultLayer.GetName oldWorld, DefaultLayer.GetName world)
        Assert.Equal<string> (DefaultEntity.GetName oldWorld, DefaultEntity.GetName world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.EntityName) DefaultOverlay DefaultLayer world |> snd
        let world = World.createEntity (Some Bob.EntityName) DefaultOverlay DefaultLayer world |> snd
        let world = !-- Bob.Visible --- Stream.map not --> Jim.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.EntityName) DefaultOverlay DefaultLayer world |> snd
        let world = World.createEntity (Some Bob.EntityName) DefaultOverlay DefaultLayer world |> snd
        let world = !-- Bob.Visible -/> Jim.Visible ^ world
        let world = !-- Jim.Visible --> Bob.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)