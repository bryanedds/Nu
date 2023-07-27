// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
module WorldTests =

    let TestValueKey = Gen.id
    let TestFilePath = "TestFile.nugame"
    let StringEvent = stoa<string> "String/Event"
    let Screen = Simulants.Game / "Screen"
    let Group = Screen / "Group"
    let Jim = Group / "Jim"
    let Bob = Group / "Bob"

    let [<Test>] runOneEmptyFrameThenCleanUp () =
        let nuConfig = { NuConfig.defaultConfig with Accompanied = true }
        Nu.init nuConfig
        let worldConfig = { WorldConfig.defaultConfig with NuConfig = nuConfig }
        let world = World.makeEmpty worldConfig (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> World.getUpdateTime world < 1L) id id id id Live true world
        Assert.Equal (result, Constants.Engine.ExitCodeSuccess)

    let [<Test; Category "Integration">] runOneIntegrationFrameThenCleanUp () =
        let nuConfig = { NuConfig.defaultConfig with Accompanied = true }
        Nu.init nuConfig
        let worldConfig = { WorldConfig.defaultConfig with NuConfig = nuConfig }
        match SdlDeps.tryMake worldConfig.SdlConfig with
        | Right sdlDeps ->
            use sdlDeps = sdlDeps // bind explicitly to dispose automatically
            match World.tryMake sdlDeps worldConfig (TestPlugin ()) with
            | Right world ->
                let result = World.runWithCleanUp (fun world -> World.getUpdateTime world < 1L) id id id id Live true world
                Assert.Equal (result, Constants.Engine.ExitCodeSuccess)
            | Left _ -> Assert.Fail ()
        | Left _ -> Assert.Fail ()