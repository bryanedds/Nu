// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open Xunit
open Prime
open Prime.Stream
open Nu
module WorldTests =

    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = Simulants.DefaultGroup => "Jim"
    let Bob = Simulants.DefaultGroup => "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> (Cascade, World.updateUserState (fun _ -> evt.Subscriber) world)
        let world = World.subscribe handleEvent StringEvent Simulants.DefaultEntity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Simulants.Game world
        Assert.Equal<Simulant> (Simulants.DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        let world = World.makeDefault ()
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<Name> (Simulants.DefaultScreen.GetName oldWorld, Simulants.DefaultScreen.GetName world)
        Assert.Equal<Name> (Simulants.DefaultGroup.GetName oldWorld, Simulants.DefaultGroup.GetName world)
        Assert.Equal<Name> (Simulants.DefaultEntity.GetName oldWorld, Simulants.DefaultEntity.GetName world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world = !-- Bob.Visible --- map not --> Jim.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world = !-- Bob.Visible --> Jim.Visible ^ world
        let world = !-- Jim.Visible -/> Bob.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.False (Jim.GetVisible world)