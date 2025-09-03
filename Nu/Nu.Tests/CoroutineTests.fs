// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module CoroutineTests =

    let [<Test>] ``Coroutine can run.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray()
        let runWhile (world : World) = world.UpdateTime < 10L
        let perProcess (world : World) =
            if world.UpdateTime = 0 then
                coroutine world.Launcher {
                    do! fun w -> numbers.Add w.UpdateTime
                    do! fun w -> numbers.Add w.UpdateTime
                    do! fun w -> numbers.Add w.UpdateTime
                    do! Coroutine.pass
                    do! fun w -> numbers.Add w.UpdateTime
                    do! Coroutine.sleep (GameTime.ofUpdates 1)
                    do! fun w -> numbers.Add w.UpdateTime
                    do! Coroutine.cancel
                    do! fun w -> numbers.Add w.UpdateTime }
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([0; 0; 0; 1; 2], numbers)
        Assert.Equal (result, Constants.Engine.ExitCodeSuccess)
        
    let [<Test>] ``Coroutine is tail recursive.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 10L
        let perProcess (world : World) =
            if world.UpdateTime = 0 then
                let rec c (w : World) =
                    coroutine w.Launcher {
                        do! fun w -> numbers.Add w.UpdateTime
                        do! c }
                c world
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9], numbers)
        Assert.Equal (result, Constants.Engine.ExitCodeSuccess)