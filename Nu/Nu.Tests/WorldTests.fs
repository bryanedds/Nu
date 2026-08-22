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
        let previousResolution = Globals.Render.DisplayVirtualResolution
        let previousScalar = Globals.Render.DisplayScalar
        try
            let world = makeStubWorld ()
            let eyeSizePrevious = world.Eye2dSize
            let resolution = System.Numerics.Vector2i (800, 600)
            World.setDisplayVirtualResolution resolution world
            Assert.Equal (resolution, World.getDisplayVirtualResolution ())
            Assert.Equal (eyeSizePrevious, world.Eye2dSize)
            Assert.Equal (Globals.Render.DisplayScalar, world.WindowViewport.DisplayScalar)
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
        finally
            Globals.Render.DisplayVirtualResolution <- previousResolution
            Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Display virtual resolution preserves custom eye size.`` () =
        let previousResolution = Globals.Render.DisplayVirtualResolution
        let previousScalar = Globals.Render.DisplayScalar
        try
            let world = makeStubWorld ()
            let customEyeSize = System.Numerics.Vector2 (123.0f, 77.0f)
            World.setEye2dSize customEyeSize world
            World.setDisplayVirtualResolution (System.Numerics.Vector2i (800, 600)) world
            Assert.Equal (customEyeSize, world.Eye2dSize)
        finally
            Globals.Render.DisplayVirtualResolution <- previousResolution
            Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Invalid display virtual resolution leaves state unchanged.`` () =
        let previousResolution = Globals.Render.DisplayVirtualResolution
        let previousScalar = Globals.Render.DisplayScalar
        try
            let world = makeStubWorld ()
            let eyeSize = world.Eye2dSize
            Assert.Throws<ArgumentException> (fun () -> World.setDisplayVirtualResolution (System.Numerics.Vector2i (0, 450)) world |> ignore) |> ignore
            Assert.Equal (previousResolution, Globals.Render.DisplayVirtualResolution)
            Assert.Equal (eyeSize, world.Eye2dSize)
        finally
            Globals.Render.DisplayVirtualResolution <- previousResolution
            Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Display virtual resolution remains supported without an SDL display.`` () =
        let previousResolution = Globals.Render.DisplayVirtualResolution
        let previousScalar = Globals.Render.DisplayScalar
        try
            let world = makeStubWorld ()
            World.setDisplayVirtualResolution (System.Numerics.Vector2i (2000, 1200)) world
            Assert.Equal (System.Numerics.Vector2i (2000, 1200), World.getDisplayVirtualResolution ())
        finally
            Globals.Render.DisplayVirtualResolution <- previousResolution
            Globals.Render.DisplayScalar <- previousScalar

    let [<Test; NonParallelizable>] ``Constrain eye bounds includes the visible eye margin.`` () =
        let eyeMarginPrevious = Constants.Engine.EyeMarginMaxScalar
        try
            Constants.Engine.EyeMarginMaxScalar <- System.Numerics.Vector2 (0.5f, 0.5f)
            let world = makeStubWorld ()
            World.setEye2dSize (System.Numerics.Vector2 (100.0f, 100.0f)) world
            World.setEye2dCenter (System.Numerics.Vector2 (450.0f, 0.0f)) world
            let bounds = System.Numerics.Box2 (System.Numerics.Vector2 (-500.0f, -500.0f), System.Numerics.Vector2 (1000.0f, 1000.0f))
            World.constrainEye2dBounds bounds world
            Assert.That (world.Eye2dBoundsViewed.Min.X, Is.GreaterThanOrEqualTo bounds.Min.X)
            Assert.That
                (world.Eye2dBoundsViewed.Min.X + world.Eye2dBoundsViewed.Size.X,
                 Is.LessThanOrEqualTo (bounds.Min.X + bounds.Size.X))
        finally
            Constants.Engine.EyeMarginMaxScalar <- eyeMarginPrevious

    let [<Test>] ``Run empty frame then clean up.`` () =
        Nu.init ()
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore (Some ignore) world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test; Category "Integration">] ``Run integration frame then clean up.`` () =
        Nu.init ()
        let worldConfig = { WorldConfig.defaultConfig with Accompanied = true }
        let windowSize = Globals.Render.DisplayVirtualResolution * Globals.Render.DisplayScalar
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