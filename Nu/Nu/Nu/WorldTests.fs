// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open System.IO
open SDL2
open Xunit
open Prime
open Nu
module WorldTests =

    let TestFilePath = "TestFile.nugame"
    let StringEvent = ntoa<string> !!"String"
    let Jim = Simulants.DefaultGroup => "Jim"
    let Bob = Simulants.DefaultGroup => "Bob"

    let [<Fact>] runOneFrameThenCleanUp () =
        let world = World.makeEmpty ()
        World.run4 (fun world -> World.getTickTime world < 1L) SdlDeps.empty Running world

    let [<Fact>] entitySubscribeWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen !!typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup !!typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Simulants.DefaultEntity.EntityName) Simulants.DefaultGroup world |> snd
        let handleEvent = fun evt world -> (Cascade, World.updateUserState (fun _ -> evt.Subscriber) world)
        let world = World.subscribe handleEvent StringEvent Simulants.DefaultEntity world
        let world = World.publish String.Empty StringEvent EventTrace.empty Simulants.Game world
        Assert.Equal<Simulant> (Simulants.DefaultEntity :> Simulant, World.getUserState world)

    let [<Fact>] gameSerializationWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen !!typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup !!typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Simulants.DefaultEntity.EntityName) Simulants.DefaultGroup world |> snd
        let oldWorld = world
        World.writeGameToFile TestFilePath world
        let world = World.readGameFromFile TestFilePath world
        Assert.Equal<Name> (Simulants.DefaultScreen.GetName oldWorld, Simulants.DefaultScreen.GetName world)
        Assert.Equal<Name> (Simulants.DefaultGroup.GetName oldWorld, Simulants.DefaultGroup.GetName world)
        Assert.Equal<Name> (Simulants.DefaultEntity.GetName oldWorld, Simulants.DefaultEntity.GetName world)

    let [<Fact>] iterativeFrpWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen !!typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup !!typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world = world |> Bob.TagVisible *-> Jim.TagVisible
        let world = Bob.SetVisible false world
        Assert.False ^ Bob.GetVisible world
        Assert.False ^ Jim.GetVisible world

    let [<Fact>] iterativeFrpCyclicWorks () =
        let world = World.makeEmpty ()
        let world = World.createScreen !!typeof<ScreenDispatcher>.Name None (Some Simulants.DefaultScreen.ScreenName) world |> snd
        let world = World.createGroup !!typeof<GroupDispatcher>.Name None (Some Simulants.DefaultGroup.GroupName) Simulants.DefaultScreen world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Jim.EntityName) Simulants.DefaultGroup world |> snd
        let world = World.createEntity !!typeof<EntityDispatcher>.Name None (Some Bob.EntityName) Simulants.DefaultGroup world |> snd
        let world =
            world |>
                Bob.TagVisible *-> Jim.TagVisible |>
                Jim.TagVisible /-> Bob.TagVisible.MapSet not
        let world = Bob.SetVisible false world
        Assert.True ^ Bob.GetVisible world
        Assert.True ^ Jim.GetVisible world