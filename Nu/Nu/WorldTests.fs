// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module WorldTests =

    let TestValueKey = scstring Gen.id
    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = Default.Layer / "Jim"
    let Bob = Default.Layer / "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let worldConfig = WorldConfig.defaultConfig
        let world = World.makeEmpty worldConfig
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> World.addKeyedValue TestValueKey evt.Subscriber world
        let world = World.subscribe handleEvent StringEvent Default.Entity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Default.Game world
        Assert.Equal<Simulant> (Default.Entity :> Simulant, World.getKeyedValue TestValueKey world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Name) DefaultOverlay Default.Layer world |> snd
        let world = World.createEntity (Some Bob.Name) DefaultOverlay Default.Layer world |> snd
        let world = !-- Bob.Visible --- Stream.map not -|> Jim.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Name) DefaultOverlay Default.Layer world |> snd
        let world = World.createEntity (Some Bob.Name) DefaultOverlay Default.Layer world |> snd
        let world = !-- Bob.Visible -/> Jim.Visible $ world
        let world = !-- Jim.Visible -|> Bob.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)