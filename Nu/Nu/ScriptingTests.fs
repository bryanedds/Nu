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

    let evalPartial exprStr =
        let world = World.makeEmpty ()
        match World.tryEvalPrelude world with
        | Right world ->
            let expr = scvalue<Scripting.Expr> exprStr
            World.eval expr (Simulants.Game.GetScriptFrameNp world) Simulants.Game world |> fst
        | Left _ ->
            Assert.True false
            Scripting.Unit

    let eval exprStr =
        let evaled = evalPartial exprStr
        let evaledSymbol = SymbolicDescriptor.convertTo evaled typeof<Scripting.Expr> typeof<Symbol> :?> Symbol
        Symbol.toString evaledSymbol

    let [<Fact>] keywordsWork () = Assert.Equal ("Keyword", eval "Keyword")
    let [<Fact>] plusWorks () = Assert.Equal ("2", eval "[+ 1 1]")
    let [<Fact>] equalityWorks () = Assert.Equal ("true", eval "[= 1 1]")
    let [<Fact>] nestedApplicationWorks () = Assert.Equal ("4", eval "[+ [+ 1 1] [+ 1 1]]")
    let [<Fact>] optionsWork () = Assert.Equal ("true", eval "[isSome [some 1]]")
    let [<Fact>] tuplesWork () = Assert.Equal ("1", eval "[fst [tuple 1]]")
    let [<Fact>] listsWork () = Assert.Equal ("1", eval "[head [list 1]]")
    let [<Fact>] keyphrasesWork () = Assert.Equal ("1", eval "[fst [K 1]]")
    let [<Fact>] conditionalWorks () = Assert.Equal ("1", eval "[if [= 1 1] 1 0]")
    let [<Fact>] matchWorks () = Assert.Equal ("2", eval "[match 1 [0 0] [1 2]]")
    let [<Fact>] selectWorks () = Assert.Equal ("1", eval "[select [false 0] [true 1]]")
    let [<Fact>] letWorks () = Assert.Equal ("2", eval "[let [x 1] [+ x x]]")
    let [<Fact>] letManyWorks () = Assert.Equal ("3", eval "[let [x 1] [y 2] [+ x y]]")
    let [<Fact>] letFxWorks () = Assert.Equal ("1", eval "[let [f [x] x] [f 1]]")
    let [<Fact>] letFunWorks () = Assert.Equal ("1", eval "[let [f [fun [x] x]] [f 1]]")
    let [<Fact>] doWorks () = Assert.Equal ("4", eval "[do [+ 1 1] [+ 2 2]]")

    let [<Fact>] matchFailureWorks () =
        match evalPartial "[match 2 [0 0] [1 2]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] selectFailureWorks () =
        match evalPartial "[select [false 0] [false 1]]" with
        | Scripting.Violation (_, _, _) -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] outOfRangeWorks () =
        match evalPartial "[fst empty]" with
        | Scripting.Violation _ -> Assert.True true
        | _ -> Assert.True false

    let [<Fact>] setEyeCenterFromGameScriptWorks () =
        let world = World.makeEmpty ()
        let game = Simulants.Game
        let expr = scvalue<Scripting.Expr> "[set EyeCenter [v2 10f 10f]]"
        let world = World.eval expr (game.GetScriptFrameNp world) game world |> snd
        Assert.Equal (Vector2 (10.0f, 10.0f), Game.GetEyeCenter world)