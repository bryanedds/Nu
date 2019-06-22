// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module WorldTests =

    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = Default.Layer / "Jim"
    let Bob = Default.Layer / "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> World.updateUserValue (fun _ -> evt.Subscriber) world
        let world = World.subscribe handleEvent StringEvent Default.Entity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Default.Game world
        Assert.Equal<Simulant> (Default.Entity :> Simulant, World.getUserValue world)

    let [<Fact>] gameSerializationWorks () =
        let world = World.makeDefault ()
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<string> (Default.Screen.GetName oldWorld, Default.Screen.GetName world)
        Assert.Equal<string> (Default.Layer.GetName oldWorld, Default.Layer.GetName world)
        Assert.Equal<string> (Default.Entity.GetName oldWorld, Default.Entity.GetName world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.EntityName) DefaultOverlay Default.Layer world |> snd
        let world = World.createEntity (Some Bob.EntityName) DefaultOverlay Default.Layer world |> snd
        let world = !-- Bob.Visible --- Stream.map not -|> Jim.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.EntityName) DefaultOverlay Default.Layer world |> snd
        let world = World.createEntity (Some Bob.EntityName) DefaultOverlay Default.Layer world |> snd
        let world = !-- Bob.Visible -/> Jim.Visible $ world
        let world = !-- Jim.Visible -|> Bob.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)