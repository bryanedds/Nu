// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module WorldTests =

    let [<Test>] runEmptyFrameThenCleanUp () =
        Nu.init ()
        let world = World.makeEmpty { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) id id id id id Live true world
        Assert.Equal (result, Constants.Engine.ExitCodeSuccess)

    let [<Test; Category "Integration">] runIntegrationFrameThenCleanUp () =
        Nu.init ()
        let worldConfig = { WorldConfig.defaultConfig with Accompanied = true }
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        match SdlDeps.tryMake worldConfig.SdlConfig false windowSize with
        | Right sdlDeps ->
            use sdlDeps = sdlDeps // bind explicitly to dispose automatically
            let outerViewport = Viewport.makeOuter windowSize
            let rasterViewport = Viewport.makeRaster outerViewport.Bounds
            let geometryViewport = Viewport.makeGeometry outerViewport.Bounds.Size
            match World.tryMake sdlDeps worldConfig geometryViewport rasterViewport outerViewport (TestPlugin ()) with
            | Right world ->
                let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) id id id id id Live true world
                Assert.Equal (result, Constants.Engine.ExitCodeSuccess)
            | Left _ -> Assert.Fail ()
        | Left _ -> Assert.Fail ()

    let [<Test; Category "Integration">] runIntegrationFrameThenCleanUpThreeTimes () =
        for _ in 0 .. dec 3 do
            runIntegrationFrameThenCleanUp ()