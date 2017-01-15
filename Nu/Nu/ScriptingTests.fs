// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Tests
open System
open Xunit
open Prime
open Prime.Stream
open Nu
open Nu.Simulants
open OpenTK
module ScriptingTests =

    let [<Fact>] setEyeCenterFromGameScriptWorks () =
        let world = World.makeEmpty ()
        let initialize = scvalue<Scripting.Expr> "[set EyeCenter [v2 10 10]]"
        let script = { Script.empty with Initialize = initialize }
        let world = Game.SetScript script world
        Assert.Equal (Vector2 (10.0f, 10.0f), Game.GetEyeCenter world)