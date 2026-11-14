// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Diagnostics
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module WorldTests =

    let [<Test>] ``Run empty frame then clean up.`` () =
        Nu.init ()
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let result = World.runWithCleanUp (fun world -> world.UpdateTime < 1L) ignore ignore ignore ignore ignore (Some ignore) world
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test>] ``Scaled game time advances by the time scalar, and unscaled game time regardless.`` () =
        Nu.init ()
        match Constants.GameTime.DesiredFrameRate with
        | StaticFrameRate _ ->
            let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
            World.setTimeScalar 0.5f world
            World.runWithoutCleanUp (fun world -> world.UpdateTime < 10L) ignore ignore ignore ignore ignore (Some ignore) world |> ignore
            Assert.Equal (UpdateTime 10L, world.UnscaledTime)
            Assert.Equal (UpdateTime 5L, world.ScaledTime)
            World.setTimeScalar 0.0f world
            World.runWithoutCleanUp (fun world -> world.UpdateTime < 15L) ignore ignore ignore ignore ignore (Some ignore) world |> ignore
            Assert.Equal (UpdateTime 15L, world.UnscaledTime)
            Assert.Equal (UpdateTime 5L, world.ScaledTime)
            Assert.Equal (UpdateTime 0L, world.ScaledDelta)
            World.cleanUp world
        | DynamicFrameRate _ -> ()

    let [<Test>] ``With a tick delta override, each frame advances time by exactly it, faster than real time.`` () =
        Nu.init ()
        let tickDeltaOverride = Stopwatch.Frequency // a whole second per frame
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true; TickDeltaOverride = Some tickDeltaOverride } (TestPlugin ())
        let stopwatch = Stopwatch.StartNew ()
        World.runWithoutCleanUp (fun world -> world.UpdateTime < 10L) ignore ignore ignore ignore ignore (Some ignore) world |> ignore
        Assert.Equal (tickDeltaOverride, world.TickDelta)
        Assert.Equal (tickDeltaOverride * 10L, world.TickTime)
        Assert.Equal (10.0f, world.ClockTime)
        Assert.IsTrue (stopwatch.Elapsed.TotalSeconds < 10.0)
        World.cleanUp world

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