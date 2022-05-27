// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module WorldTests =

    let TestValueKey = Gen.id
    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = Simulants.Default.Group / "Jim"
    let Bob = Simulants.Default.Group / "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let worldConfig = WorldConfig.defaultConfig
        let world = World.makeEmpty worldConfig
        World.run4 (fun world -> World.getUpdateTime world < 1L) SdlDeps.empty Live world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> (Cascade, World.addKeyedValue TestValueKey evt.Subscriber world)
        let world = World.subscribe handleEvent StringEvent Simulants.Default.Entity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Simulants.Game world
        Assert.Equal<Simulant> (Simulants.Default.Entity :> Simulant, World.getKeyedValue TestValueKey world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Surnames) DefaultOverlay Simulants.Default.Group world |> snd
        let world = World.createEntity (Some Bob.Surnames) DefaultOverlay Simulants.Default.Group world |> snd
        let world = !-- Bob.Visible --- Stream.map not -|> Jim.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity (Some Jim.Surnames) DefaultOverlay Simulants.Default.Group world |> snd
        let world = World.createEntity (Some Bob.Surnames) DefaultOverlay Simulants.Default.Group world |> snd
        let world = !-- Bob.Visible -/> Jim.Visible $ world
        let world = !-- Jim.Visible -|> Bob.Visible $ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)