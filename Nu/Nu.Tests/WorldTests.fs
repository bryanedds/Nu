// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module WorldTests =

    let private makeStubWorld () =
        Nu.init ()
        World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())

    let [<Test; NonParallelizable>] ``Display virtual resolution synchronizes world state immediately.`` () =
        let previousScalar = Globals.Render.DisplayScalar
        let world = makeStubWorld ()
        let oldWindowViewport = world.WindowViewport
        let eyeSizePrevious = world.Eye2dSize
        let resolution = System.Numerics.Vector2i (800, 600)
        World.setDisplayVirtualResolution resolution world
        Assert.Equal (resolution, World.getDisplayVirtualResolution world)
        Assert.Equal (eyeSizePrevious, world.Eye2dSize)
        Assert.Equal (Globals.Render.DisplayScalar, world.WindowViewport.DisplayScalar)
        Assert.That (world.WindowViewport, Is.Not.EqualTo oldWindowViewport)
        let geometryBounds = System.Numerics.Box2i (System.Numerics.Vector2i.Zero, world.GeometryViewport.Bounds.Size)
        let expectedViewport =
            Viewport.make
                Constants.Render.NearPlaneDistanceInterior
                Constants.Render.FarPlaneDistanceInterior
                geometryBounds
                geometryBounds
                geometryBounds
        let expectedFrustum =
            Viewport.getFrustum world.Eye3dCenter world.Eye3dRotation world.Eye3dFieldOfView expectedViewport
        Assert.Equal (expectedFrustum, world.Eye3dFrustumInterior)
        Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Display virtual resolution preserves custom eye size.`` () =
        let previousScalar = Globals.Render.DisplayScalar
        let world = makeStubWorld ()
        let customEyeSize = System.Numerics.Vector2 (123.0f, 77.0f)
        World.setEye2dSize customEyeSize world
        World.setDisplayVirtualResolution (System.Numerics.Vector2i (800, 600)) world
        Assert.Equal (customEyeSize, world.Eye2dSize)
        Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Invalid display virtual resolution is rejected without changing state.`` () =
        let previousScalar = Globals.Render.DisplayScalar
        let world = makeStubWorld ()
        let eyeSize = world.Eye2dSize
        let windowViewport = world.WindowViewport
        let geometryViewport = world.GeometryViewport
        let resolutionPrevious = World.getDisplayVirtualResolution world
        World.setDisplayVirtualResolution (System.Numerics.Vector2i (0, 450)) world
        Assert.Equal (resolutionPrevious, World.getDisplayVirtualResolution world)
        Assert.Equal (eyeSize, world.Eye2dSize)
        Assert.Equal (windowViewport, world.WindowViewport)
        Assert.Equal (geometryViewport, world.GeometryViewport)
        Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Display virtual resolution remains supported without an SDL display.`` () =
        let previousScalar = Globals.Render.DisplayScalar
        let world = makeStubWorld ()
        World.setDisplayVirtualResolution (System.Numerics.Vector2i (2000, 1200)) world
        Assert.Equal (System.Numerics.Vector2i (2000, 1200), World.getDisplayVirtualResolution world)
        Globals.Render.DisplayScalar <- previousScalar

    let [<Test>] ``Run empty frame then clean up.`` () =
        Nu.init ()
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore (Some ignore) world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test; Category "Integration">] ``Run integration frame then clean up.`` () =
        Nu.init ()
        let worldConfig = { WorldConfig.defaultConfig with Accompanied = true }
        let windowSize = Constants.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
        match SdlDeps.tryMake worldConfig.SdlConfig false windowSize with
        | Right sdlDeps ->
            use _ = sdlDeps // bind explicitly to dispose automatically
            let windowViewport = Viewport.makeWindow1 windowSize
            let geometryViewport = Viewport.makeGeometry windowViewport.Bounds.Size
            let world = World.make (constant None) sdlDeps worldConfig windowSize geometryViewport windowViewport (TestPlugin ())
            let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore (Some ignore) world
            Assert.Equal (Constants.Engine.ExitCodeSuccess, result)
        | Left _ -> Assert.Fail ()

    let [<Test; Category "Integration">] ``Run integration frame then clean up - three times.`` () =
        for _ in 0 .. dec 3 do
            ``Run integration frame then clean up.`` ()