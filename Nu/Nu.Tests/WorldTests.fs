// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Numerics
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

    let [<Test>] ``Write screen to file then read it back.`` () =
        Nu.init ()
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let screen = World.createScreen<ScreenDispatcher> (Some "Screen") world
        let group = World.createGroup<GroupDispatcher> (Some "Group") screen world
        let entity = World.createEntity<StaticSpriteDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        entity.SetPosition (v3 1.0f 2.0f 3.0f) world
        let filePath = IO.Path.Combine (IO.Path.GetTempPath (), "Nu.Tests." + string Gen.id64 + ".nuscreen")
        try World.writeScreenToFile filePath screen world
            World.destroyScreenImmediate screen world
            Assert.False (screen.GetExists world)
            let screen' = World.readScreenFromFile filePath None world
            Assert.Equal (screen, screen')
            Assert.True (screen.GetExists world)
            Assert.True (group.GetExists world)
            Assert.True (entity.GetExists world)
            Assert.Equal (v3 1.0f 2.0f 3.0f, entity.GetPosition world)
        finally IO.File.Delete filePath