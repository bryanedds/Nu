// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime.Tests
open System
open Xunit
open Prime
module ScriptingTests =

    type [<ReferenceEquality>] TestWorld =
        { ScriptingEnv : Scripting.Env }
        interface TestWorld ScriptingWorld with
            member this.GetEnv () = this.ScriptingEnv
            member this.TryGetExtrinsic _ = failwithumf ()
            member this.TryImport _ _ = failwithnie ()
            member this.TryExport _ _ = failwithnie ()
        static member make () = { ScriptingEnv = Scripting.EnvModule.Env.make () }

    let evalPartial exprStr =
        let world = TestWorld.make ()
        match ScriptingWorld.tryEvalScript id Constants.Scripting.PreludeFilePath world with
        | Right struct (_, _, world) ->
            let expr = scvalue<Scripting.Expr> exprStr
            ScriptingWorld.eval expr world |> fst'
        | Left _ ->
            Assert.True false
            Scripting.Unit

    let eval exprStr =
        let converter = SymbolicConverter (true, typeof<Scripting.Expr>)
        let evaled = evalPartial exprStr
        let evaledSymbol = converter.ConvertTo (evaled, typeof<Symbol>) :?> Symbol
        Symbol.toString evaledSymbol

    let [<Fact>] keywordsWork () = Assert.Equal ("Keyword", eval "Keyword")
    let [<Fact>] plusWorks () = Assert.Equal ("2", eval "[+ 1 1]")
    let [<Fact>] equalityWorks () = Assert.Equal ("true", eval "[= 1 1]")
    let [<Fact>] nestedApplicationWorks () = Assert.Equal ("4", eval "[+ [+ 1 1] [+ 1 1]]")
    let [<Fact>] optionsWork () = Assert.Equal ("true", eval "[isSome [some 1]]")
    let [<Fact>] tuplesWork () = Assert.Equal ("1", eval "[fst [tuple 1]]")
    let [<Fact>] listsWork () = Assert.Equal ("1", eval "[head [list 1]]")
    let [<Fact>] unionsWork () = Assert.Equal ("1", eval "[fst [U 1]]")
    let [<Fact>] recordsWork () = Assert.Equal ("1", eval "[fst [record R [F 1]]]")
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