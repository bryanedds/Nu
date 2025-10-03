// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module WorldTests =

    let [<Test>] ``Run empty frame then clean up.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore true world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test; Category "Integration">] ``Run integration frame then clean up.`` () =
        Nu.init ()
        let worldConfig = { WorldConfig.defaultConfig with Accompanied = true }
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        match SdlDeps.tryMake worldConfig.SdlConfig false windowSize with
        | Right sdlDeps ->
            use sdlDeps = sdlDeps // bind explicitly to dispose automatically
            let outerViewport = Viewport.makeOuter windowSize
            let rasterViewport = Viewport.makeRaster outerViewport.Inset outerViewport.Bounds
            let geometryViewport = Viewport.makeGeometry outerViewport.Bounds.Size
            let world = World.make sdlDeps worldConfig geometryViewport rasterViewport outerViewport (TestPlugin ())
            let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore true world
            Assert.Equal (Constants.Engine.ExitCodeSuccess, result)
        | Left _ -> Assert.Fail ()

    let [<Test; Category "Integration">] ``Run integration frame then clean up - three times.`` () =
        for _ in 0 .. dec 3 do
            ``Run integration frame then clean up.`` ()