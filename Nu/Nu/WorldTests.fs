// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open Xunit
open Prime
open Prime.Stream
open Nu
open Nu.Simulants
module WorldTests =

    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Jim = DefaultGroup => "Jim"
    let Bob = DefaultGroup => "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeDefault ()
        let handleEvent = fun evt world -> (Cascade, World.updateUserState (fun _ -> evt.Subscriber) world)
        let world = World.subscribe handleEvent StringEvent DefaultEntity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Game world
        Assert.Equal<Simulant> (DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        let world = World.makeDefault ()
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<Name> (DefaultScreen.GetName oldWorld, DefaultScreen.GetName world)
        Assert.Equal<Name> (DefaultGroup.GetName oldWorld, DefaultGroup.GetName world)
        Assert.Equal<Name> (DefaultEntity.GetName oldWorld, DefaultEntity.GetName world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) DefaultGroup world |> snd
        let world = !-- Bob.Visible --- map not --> Jim.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.True (Jim.GetVisible world)

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeDefault ()
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Jim.EntityName) DefaultGroup world |> snd
        let world = World.createEntity typeof<EntityDispatcher>.Name None (Some Bob.EntityName) DefaultGroup world |> snd
        let world = !-- Bob.Visible --> Jim.Visible ^ world
        let world = !-- Jim.Visible -/> Bob.Visible ^ world
        let world = Bob.SetVisible false world
        Assert.False (Bob.GetVisible world)
        Assert.False (Jim.GetVisible world)