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

    let [<Fact>] keywordsWork () =
        match eval "Keyword" with
        | Scripting.Keyword (result, _) -> Assert.Equal ("Keyword", result)
        | _ -> Assert.True false

    let [<Fact>] plusWorks () =
        match eval "[+ 1 1]" with
        | Scripting.Int (result, _) -> Assert.Equal (2, result)
        | _ -> Assert.True false

    let [<Fact>] equalityWorks () =
        match eval "[= 1 1]" with
        | Scripting.Bool (result, _) -> Assert.True result
        | _ -> Assert.True false

    let [<Fact>] nestedApplicationWorks () =
        match eval "[+ [+ 1 1] [+ 1 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (4, result)
        | _ -> Assert.True false

    let [<Fact>] optionsWork () =
        match eval "[isSome [some 1]]" with
        | Scripting.Bool (result, _) -> Assert.True result
        | _ -> Assert.True false

    let [<Fact>] tuplesWork () =
        match eval "[fst [tuple 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] listsWork () =
        match eval "[fst [list 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] keyphrasesWork () =
        match eval "[fst [K 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] outOfRangeWorks () =
        match eval "[fst empty]" with
        | Scripting.Violation _ -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] conditionalWorks () =
        match eval "[if [= 1 1] 1 0]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] matchWorks () =
        match eval "[match 1 [0 0] [1 2]]" with
        | Scripting.Int (result, _) -> Assert.Equal (2, result)
        | _ -> Assert.True false

    let [<Fact>] matchFailureWorks () =
        match eval "[match 2 [0 0] [1 2]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] selectWorks () =
        match eval "[select [false 0] [true 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] selectFailureWorks () =
        match eval "[select [false 0] [false 1]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] letWorks () =
        match eval "[let [x 1] [+ x x]]" with
        | Scripting.Int (result, _) -> Assert.Equal (2, result)
        | _ -> Assert.True false

    let [<Fact>] letManyWorks () =
        match eval "[let [x 1] [y 2] [+ x y]]" with
        | Scripting.Int (result, _) -> Assert.Equal (3, result)
        | _ -> Assert.True false

    let [<Fact>] letFxWorks () =
        match eval "[let [f [x] x] [f 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] letFunWorks () =
        match eval "[let [f [fun [x] x]] [f 1]]" with
        | Scripting.Int (result, _) -> Assert.Equal (1, result)
        | _ -> Assert.True false

    let [<Fact>] doWorks () =
        match eval "[do [+ 1 1] [+ 2 2]]" with
        | Scripting.Int (result, _) -> Assert.Equal (4, result)
        | _ -> Assert.True false

    let [<Fact>] setEyeCenterFromGameScriptWorks () =
        let world = World.makeEmpty ()
        let onRegister = scvalue<Scripting.Expr> "[set EyeCenter [v2 10f 10f]]"
        let world = Game.SetOnRegister onRegister world
        Assert.Equal (Vector2 (10.0f, 10.0f), Game.GetEyeCenter world)