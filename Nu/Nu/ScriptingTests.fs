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

    let [<Fact>] oneAndOneIsTwo () =
        let world = World.makeEmpty ()
        let env = Env.make World.choose false (dictPlus []) (Game :> Simulant) world
        let expr = scvalue<Scripting.Expr> "[+ 1 1]"
        match ScriptSystem.eval expr env with
        | (Scripting.Int (result, _), _) -> Assert.Equal (2, result)
        | (_, _) -> Assert.False true

    let [<Fact>] oneAndOneAndOneAndOneIsFour () =
        let world = World.makeEmpty ()
        let env = Env.make World.choose false (dictPlus []) (Game :> Simulant) world
        let expr = scvalue<Scripting.Expr> "[+ [+ 1 1] [+ 1 1]]"
        match ScriptSystem.eval expr env with
        | (Scripting.Int (result, _), _) -> Assert.Equal (4, result)
        | (_, _) -> Assert.False true

    let [<Fact>] setEyeCenterFromGameScriptWorks () =
        let world = World.makeEmpty ()
        let onInit = scvalue<Scripting.Expr> "[set EyeCenter [v2 10f 10f]]"
        let script = { Script.empty with OnInit = onInit }
        let world = Game.SetScript script world
        Assert.Equal (Vector2 (10.0f, 10.0f), Game.GetEyeCenter world)