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

    let eval exprStr =
        let world = World.makeEmpty ()
        let env = Env.make World.choose false (dictPlus []) (Game :> Simulant) world
        let expr = scvalue<Scripting.Expr> exprStr
        ScriptSystem.eval expr env |> fst

    let [<Fact>] oneAndOneIsTwo () =
        match eval "[+ 1 1]" with
        | Scripting.Int (result, _) -> Assert.Equal (2, result)
        | _ -> Assert.True false

    let [<Fact>] nestedApplicationWorks () =
        match eval "[+ [+ 1 1] [+ 1 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (4, result)
        | _ -> Assert.True false

    let [<Fact>] equalityWorks () =
        match eval "[= 1 1]" with
        | Scripting.Bool (result, _) -> Assert.True result
        | _ -> Assert.True false

    let [<Fact>] conditionalsWork () =
        match eval "[if [= 1 1] 1 0]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] letWorks () =
        match eval "[let [x 1] [+ x x]]" with
        | Scripting.Int (result, _) -> Assert.Equal (2, result)
        | _ -> Assert.True false

    let [<Fact>] letFWorks () =
        match eval "[let [f [x] x] [f 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] setEyeCenterFromGameScriptWorks () =
        let world = World.makeEmpty ()
        let onInit = scvalue<Scripting.Expr> "[set EyeCenter [v2 10f 10f]]"
        let script = { Script.empty with OnInit = onInit }
        let world = Game.SetScript script world
        Assert.Equal (Vector2 (10.0f, 10.0f), Game.GetEyeCenter world)