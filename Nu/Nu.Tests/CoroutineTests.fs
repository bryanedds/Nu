// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests
module CoroutineTests =

    let [<Test>] ``Coroutine can run absolute.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 10L
        let perProcess (world : World) =
            if world.UpdateTime = 0 then
                coroutine world.Launcher {
                    numbers.Add world.UpdateTime
                    numbers.Add world.UpdateTime
                    do! Coroutine.sleep 1L
                    numbers.Add world.UpdateTime
                    do! Coroutine.pass
                    numbers.Add world.UpdateTime
                    do! Coroutine.sleep 2L
                    numbers.Add world.UpdateTime
                    do! Coroutine.cancel
                    numbers.Add world.UpdateTime }
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([0; 0; 1; 2; 4], numbers)
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test>] ``Coroutine can run relative.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 10L
        let perProcess (world : World) =
            if world.UpdateTime = 1L then
                coroutine world.Launcher {
                    numbers.Add world.UpdateTime
                    do! Coroutine.sleep 1L
                    numbers.Add world.UpdateTime
                    do! Coroutine.sleep 2L
                    numbers.Add world.UpdateTime
                    do! Coroutine.cancel
                    numbers.Add world.UpdateTime }
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([1; 2; 4], numbers)
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test>] ``Coroutine can recurse.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 10L
        let perProcess (world : World) =
            if world.UpdateTime = 0 then
                let rec c () =
                    coroutine world.Launcher {
                        numbers.Add world.UpdateTime
                        c () }
                c ()
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([0; 1; 2; 3; 4; 5; 6; 7; 8; 9], numbers)
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)
    
    let [<Test>] ``Coroutine can cancel even when sleeping.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 15L
        let perProcess (world : World) =
            if world.UpdateTime = 2 then
                coroutine (world.LauncherWhile (fun () -> world.UpdateTime <> 10L)) {
                    while true do
                        numbers.Add world.UpdateTime
                        do! Coroutine.sleep 3L }
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([2; 5; 8], numbers)
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)

    let [<Test>] ``Coroutine can contain loops.`` () =
        Nu.init ()
        let world = World.makeStub { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let numbers = ResizeArray ()
        let runWhile (world : World) = world.UpdateTime < 20L
        let perProcess (world : World) =
            if world.UpdateTime = 3 then
                coroutine (world.LauncherWhile (fun () -> world.UpdateTime < 12L)) {
                    numbers.Add world.UpdateTime
                    for n in [3L; 4L; 5L] do
                        numbers.Add (world.UpdateTime - n)
                        do! Coroutine.pass
                    numbers.Add world.UpdateTime
                    while true do
                        numbers.Add world.UpdateTime
                        do! Coroutine.sleep 2L
                    Assert.Fail () }
        let result = World.runWithCleanUp runWhile ignore perProcess ignore ignore ignore true world
        CollectionAssert.AreEqual ([3; 0; 0; 0; 6; 6; 8; 10], numbers)
        Assert.Equal (Constants.Engine.ExitCodeSuccess, result)
