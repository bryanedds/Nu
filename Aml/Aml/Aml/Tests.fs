// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Aml
open FParsec.CharParsers
open Xunit
open Aml.Ast
open Aml.AmlConstants
open Aml.Primitives
open Aml.Reader
open Aml.Writer
open Aml.Initial
open Aml.Evaluator
open Aml.Environment
module Tests =

    /// Write an expr for test consumption by truncating top-level violations.
    let writeExprForTest exprToWrite =
        match exprToWrite with
        | Violation violation -> parenthesize (ViolationStr + SpaceStr + violation.VioCategory)
        | expr -> writeExpr expr

    /// Write multiple expressions as a string for test consumption.
    let writeExprsForTest exprs = writeValues writeExprForTest exprs

    type TestCase =
        { Expected : string
          Actual : string }

    type [<ReferenceEquality>] ParsedTestCase =
        { ParsedExpected : string
          ParsedActual : ParserResult<Expr list, unit> }
    
    type [<ReferenceEquality>] EvaledTestCase =
        { EvaledExpected : string
          EvaledActual : Expr list }

    let parseTestCase testCase = {
        ParsedExpected = testCase.Expected
        ParsedActual = run readExprsTillEnd testCase.Actual }

    let evalTestCase testCase = {
        EvaledExpected = testCase.ParsedExpected
        EvaledActual =
            List.map
                (fun result -> result.Value)
                (sequentiallyEvalReadResults testCase.ParsedActual false (makeInitialEnv ())) }

    let writeTestCase testCase = {
        Expected = testCase.EvaledExpected
        Actual = writeExprsForTest testCase.EvaledActual }

    let test testCase =
        let parsedTestCase = parseTestCase testCase
        let evaledTestCase = evalTestCase parsedTestCase
        let writtenTestCase = writeTestCase evaledTestCase
        Assert.Equal<string> (writtenTestCase.Expected, writtenTestCase.Actual)

    let amlFilePathStr = "../../TestData/TestFile.aml"
    let missingAmlFilePathStr = "../../TestData/MissingFile.aml"
    let stdlibFilePathStr = "../../Stdlib/Stdlib.aml"

    let [<Fact>] lineCommentTest () = test {
        Expected = "2"
        Actual = "; comment\n2" }
    
    let [<Fact>] lineCommentInOperationTest () = test {
        Expected = "4"
        Actual = "(i+; comment\n2; another comment\n2)" }

    let [<Fact>] multilineCommentTest () = test {
        Expected = "5 6 7"
        Actual = "5 #| stdlib |# 6 #| iMin |# 7 ;haha" }

    let [<Fact>] multilineCommentInOperationTest () = test {
        Expected = "4"
        Actual = "(i+#| comment\n |#2#| another\ncomment |#2)" }

    let [<Fact>] nestedMultilineCommentTest () = test {
        Expected = "2"
        Actual = "#| nested #| comment\n |# ftw |#2" }

    let [<Fact>] whitespaceInsensitiveBeforeTest () = test {
        Expected = "2"
        Actual = " 2" }

    let [<Fact>] whitespaceInsensitiveAfterTest () = test {
        Expected = "2"
        Actual = "2 " }

    let [<Fact>] whitespaceInsensitiveAroundTest () = test {
        Expected = "2"
        Actual = " 2 " }

    let [<Fact>] whitespaceInsensitiveBetweenTest () = test {
        Expected = "() ()"
        Actual = "()()" }

    let [<Fact>] whitespaceInsensitiveCrazyTest () = test {
        Expected = "2"
        Actual = "\n\t\r 2\t \n\r" }

    let [<Fact>] whitespaceInsensitiveOperationTest () = test {
        Expected = "4"
        Actual = " ( i+ 2  2 )  " }

    let [<Fact>] literalTest () = test {
        Expected = "2"
        Actual = "2" }

    let [<Fact>] violationTest () = test {
        Expected = "(violation :v/test)" // violation output truncated for testing
        Actual = "(violation :v/test \"\" ())" }

    let [<Fact>] andTest () = test {
        Expected = "#t #f #f #f #f"
        Actual =
            "(and #t #t)
             (and #t #f)
             (and #f #t)
             (and #f #f)
             (and #f invalidSymbol)" }

    let [<Fact>] orTest () = test {
        Expected = "#t #t #t #f #t"
        Actual =
            "(or #t #t)
             (or #t #f)
             (or #f #t)
             (or #f #f)
             (or #t invalidSymbol)" }

    let [<Fact>] negIntTest () = test {
        Expected = "-2"
        Actual = "-2" }

    let [<Fact>] negIntNameTest () = test {
        Expected = "(violation :v/eval/nonexistentSymbol)"
        Actual = "-2a" }

    let [<Fact>] charTest () = test {
        Expected = "\\\"c\""
        Actual = "\\\"c\"" }

    let [<Fact>] stringTest () = test {
        Expected = "\"str\" \"\\n\""
        Actual = "\"str\" \"\\n\"" }

    let [<Fact>] verbatimStringTest () = test {
        Expected = "#\"str\" #\"\n\" #\"\\n\""
        Actual = "#\"str\" #\"\n\" #\"\\n\"" }

    let [<Fact>] unitTest () = test {
        Expected = "()"
        Actual = "()" }

    let [<Fact>] reservedCharsTest () = test {
        Expected = "(violation :v/reader/reservedChars)"
        Actual = "?a" }

    let [<Fact>] reservedUppercaseCharsTest () = test {
        Expected = "(violation :v/reader/reservedChars)"
        Actual = "Abcd" }

    let [<Fact>] addTest () = test {
        Expected = "4"
        Actual = "(i+ 2 2)" }

    let [<Fact>] addShortTest () = test {
        Expected = "(violation :v/eval/malformedBinop)"
        Actual = "(i+ 2)" }

    let [<Fact>] addInconsistentTest () = test {
        Expected = "(violation :v/contract/invalidBinopArgumentType)"
        Actual = "(i+ 2 \"\")" }

    let [<Fact>] subTest () = test {
        Expected = "-2"
        Actual = "(i- 2 4)" }

    let [<Fact>] mulTest () = test {
        Expected = "4"
        Actual = "(i* 2 2)" }

    let [<Fact>] divTwiceTest () = test {
        Expected = "4"
        Actual = "(i/ 12 (i/ 9 3))" }

    let [<Fact>] remTest () = test {
        Expected = "5"
        Actual = "(iRem 12 7)" }

    let [<Fact>] divByZeroTest () = test {
        Expected = "(violation :v/contract/divByZero) (violation :v/contract/divByZero)"
        Actual = "(i/ 12 0) (iRem 12 0)" }

    let [<Fact>] emptyListTest () = test {
        Expected = "(list)"
        Actual = "(list)" }

    let [<Fact>] emptyArrayTest () = test {
        Expected = "(array)"
        Actual = "(array)" }

    let [<Fact>] lambdaTest () = test {
        Expected = "(fun (x) x)"
        Actual = "(fun (x) x)" }

    let [<Fact>] parameterlessLambdaTest () = test {
        Expected = "(fun () x)"
        Actual = "(fun () x)" }

    let [<Fact>] emptyLambdaTest () = test {
        Expected = "(violation :v/reader/readFailure)"
        Actual = "(fun (x))" }

    let [<Fact>] applyLambdaTest () = test {
        Expected = "5"
        Actual = "((fun (x) x) 5)" }

    let [<Fact>] applyWrongNumberArgsToLambdaTest () = test {
        Expected = "(violation :v/eval/malformedLambdaInvocation)"
        Actual = "((fun (x y) (i+ x y)) 5 6 7)" }

    let [<Fact>] makeListTest () = test {
        Expected = "(list 0 5 10)"
        Actual = "(list 0 5 10)" }

    let [<Fact>] applyHeadTest () = test {
        Expected = "() 0"
        Actual = "[def lst (list 0 5 10)] (head lst)" }

    let [<Fact>] applyTailTest () = test {
        Expected = "() (list 5 10)"
        Actual = "[def lst (list 0 5 10)] (tail lst)" }

    let [<Fact>] applyConsTest () = test {
        Expected = "() (list -5 0 5 10)"
        Actual = "[def lst (list 0 5 10)] (cons -5 lst)" }

    let [<Fact>] applyListAppendTest () = test {
        Expected = "() (list 0 5 10 15 20)"
        Actual = "[def lst (list 0 5 10)] (t+ lst (list 15 20))" }

    let [<Fact>] applyCharToInt () = test {
        Expected = "65 (violation :v/contract/invalidConversionType)"
        Actual = "(charToInt \\\"A\") (charToInt 65)" }

    let [<Fact>] applyIntToChar () = test {
        Expected = "\\\"A\" (violation :v/contract/invalidConversionType)"
        Actual = "(intToChar 65) (intToChar \\\"A\")" }

    let [<Fact>] applyIntToFloat () = test {
        Expected = "1.0 (violation :v/contract/invalidConversionType)"
        Actual = "(intToFloat 1) (intToFloat 1.0)" }

    let [<Fact>] applyFloatToInt () = test {
        Expected = "1 (violation :v/contract/invalidConversionType)"
        Actual = "(floatToInt 1.0) (floatToInt 1)" }

    let [<Fact>] applyStringToArray () = test {
        Expected = "(array \\\"s\" \\\"t\" \\\"r\") (violation :v/contract/invalidConversionType)"
        Actual = "(stringToArray \"str\") (stringToArray \\\"c\")" }

    let [<Fact>] applyArrayToString () = test {
        Expected =
            "\"str\" " +
            "(violation :v/contract/invalidConversionType) " +
            "(violation :v/contract/invalidStringElements)"
        Actual =
            "(arrayToString (array \\\"s\" \\\"t\" \\\"r\"))
             (arrayToString \"str\")
             (arrayToString (array 0))" }

    let [<Fact>] applyListToArray () = test {
        Expected = "(array 0 5 10) (array 0) (violation :v/contract/invalidConversionType)"
        Actual = "(listToArray (list 0 5 10)) (listToArray (list 0)) (listToArray (array 0))" }

    let [<Fact>] applyArrayToList () = test {
        Expected = "(list 0 5 10) (list 0) (violation :v/contract/invalidConversionType)"
        Actual = "(arrayToList (array 0 5 10)) (arrayToList (array 0)) (arrayToList (list 0))" }

    let [<Fact>] applyInvalidConsTest () = test {
        Expected = "() (violation :v/contract/consToNonList)"
        Actual = "[def lst (list 0 5 10)] (cons lst 0)" }

    let [<Fact>] selectArrayElementTest () = test {
        Expected = "() 5"
        Actual = "[def v (array 0 5 10)] (select 1 v)" }

    let [<Fact>] selectCompositeMemberTest () = test {
        Expected = "() 5"
        Actual = "[def c (composite (m 5))] (select :m/m c)" }

    let [<Fact>] selectStructureMemberTest () = test {
        Expected = "() 5"
        Actual = "[struct s [m]] (select :m/m (s 5))" }

    let [<Fact>] dotSelectorTest () = test {
        Expected = "() 5"
        Actual = "[struct s [m]] (s 5).m" }

    let [<Fact>] applyAutoIsTypeTest () = test {
        Expected = "() #t #f"
        Actual = "[struct s [m]] (isS (s 5)) (isS 5)" }

    let [<Fact>] appendStringTest () = test {
        Expected = "\"0123\""
        Actual = "(s+ \"01\" \"23\")" }

    let [<Fact>] appendArrayTest () = test {
        Expected = "(array 0 1 2 3)"
        Actual = "(a+ (array 0 1) (array 2 3))" }

    let [<Fact>] functionTest () = test {
        Expected = "()"
        Actual = "[def fn [x] (i* x x)]" }

    let [<Fact>] preconditionTest () = test {
        Expected = "() (violation :v/contract/preconditionFailed) 25"
        Actual = "[def fn [x] pre: (i> x 1) (i* x x)] (fn 0) (fn 5)" }

    let [<Fact>] postconditionTest () = test {
        Expected = "() (violation :v/contract/postconditionFailed) 25"
        Actual = "[def fn [x] post: (i> result 0) (i* x x)] (fn 0) (fn 5)" }

    let [<Fact>] structurePreconditionTest () = test {
        Expected = "() (violation :v/contract/preconditionFailed) (composite (a 5))"
        Actual = "[struct s [a] req: (hasType :t/int a)] (s \"c\") (s 5)" }

    let [<Fact>] incompleteInstanceTest () = test {
        Expected = "(violation :v/eval/missingProtocol)"
        Actual = "[instance addable [x y] where: [[int x] [int y]] [def + [x y] (i+ x y)]]" }

    let [<Fact>] constraintFailureTest () = test {
        Expected =
            "() " +
            "(violation :v/eval/malformedInstance) " +
            "(violation :v/eval/invalidInstanceConstraint) " +
            "()"
        Actual =
            "[protocol p [a] [sig f [a a]]]
             [instance p [s s] where: [[string s] [string s]] [def f [s s] ()]]
             [instance p [s i] where: [[string s] [int i]] [def f [s i] ()]]
             [instance p [s s2] where: [[string s] [string s2]] [def f [s s2] ()]]" }

    let [<Fact>] applyDocTest () = test {
        Expected = "\"Describes an unhandled evaluation error.\""
        Actual = "(doc :t/violation)" }

    let [<Fact>] applyLetTest () = test {
        Expected = "6"
        Actual = "(let (a 2) (b 3) (i* a b))" }

    let [<Fact>] applyLetWithFunctionTest () = test {
        Expected = "10"
        Actual = "(let (f (x) (i* x 2)) (f 5))" }

    let [<Fact>] applyLetRecTest () = test {
        Expected = "5"
        Actual = "(let (f () (g)) (g () 5) (g))" }

    let [<Fact>] applyExtendTest () = test {
        Expected = "(composite (a :a) (b :b) (c :c))"
        Actual = "(extend (composite (a :a) (b :b)) (c :c))" }

    let [<Fact>] applyCaseTest () = test {
        Expected = "100"
        Actual = "(case (i+ 5 5) (0 0) (10 100))" }

    let [<Fact>] applyConditionTest () = test {
        Expected = "100"
        Actual = "(condition (#f 0) (#t 100))" }

    let [<Fact>] applyFunctionTest () = test {
        Expected = "() 25"
        Actual = "[def fn [x] (i* x x)] (fn 5)" }

    let [<Fact>] applyLabeledFunctionBothArgsTest () = test {
        Expected = "() 20"
        Actual = "[def fn [a: 2 b: 3] (i* a b)] (fn 4 5)" }

    let [<Fact>] applyLabeledFunctionFirstArgTest () = test {
        Expected = "() 12"
        Actual = "[def fn [a: 2 b: 3] (i* a b)] (fn 4)" }

    let [<Fact>] applyLabeledFunctionSecondArgTest () = test {
        Expected = "() 8"
        Actual = "[def fn [a: 2 b: 3] (i* a b)] (fn b: 4)" }

    let [<Fact>] applyLabeledFunctionNoArgsTest () = test {
        Expected = "() 6"
        Actual = "[def fn [a: 2 b: 3] (i* a b)] (fn)" }

    let [<Fact>] applyVariadicFunctionTest () = test {
        Expected = "() (list 1 2 3)"
        Actual = "[def fn [xs...] xs] (fn 1 2 3)" }

    let [<Fact>] applyMixedFunctionTest () = test {
        Expected = "() 1"
        Actual = "[def fn [a b: 3 c...] a] (fn 1 2 3 4)" }

    let [<Fact>] applyMixedFunction2Test () = test {
        Expected = "() 2"
        Actual = "[def fn [a b: 3 c...] b] (fn 1 2 3 4)" }

    let [<Fact>] applyMixedFunction3Test () = test {
        Expected = "() (list 3 4)"
        Actual = "[def fn [a b: 3 c...] c] (fn 1 2 3 4)" }

    let [<Fact>] applyRecursionTest () = test {
        Expected = "() 120"
        Actual = "[def fact [n] (if (i= n 0) 1 (i* n (fact (i- n 1))))] (fact 5)" }

    let [<Fact>] applyApplyTest() = test {
        Expected = "5"
        Actual = "(apply i+ (list 2 3))" }

    let [<Fact>] applyAttemptTest () = test {
        Expected = "5"
        Actual = "(attempt (violation :v/aa/bb \"\" 5) (:v/a 0) (:v/aa data) (:v 10))" }

    let [<Fact>] applyHasTypeTest () = test {
        Expected = "#t"
        Actual = "(hasType :t/string \"s\")" }

    let [<Fact>] applyInstantiatedFunctionTest () = test {
        Expected = "() () 4"
        Actual =
            "[protocol addable [a] [sig + [a a]]]
             [instance addable [x y] where: [[int x] [int y]] [def + [x y] (i+ x y)]]
             (+ 2 2)" }

    let [<Fact>] applyHasProtocolTest () = test {
        Expected = "() () #t #f"
        Actual =
            "[protocol p [a] [sig f [a b]]]
             [instance p [s] where: [[string s]] [def f [s i] ()]]
             (hasProtocol :p/p \"s\")
             (hasProtocol :p/p 5)" }

    let [<Fact>] affirmationTest () = test {
        Expected = "()"
        Actual = "[affirmation a (i< 0 1)]" }

    let [<Fact>] affirmationFailureTest () = test {
        Expected = "(violation :v/affirmation/affirmationFailure)"
        Actual = "[affirmation a (i> 0 1)]" }

    let [<Fact>] affirmationInvalidTest () = test {
        Expected = "(violation :v/affirmation/invalidResultType)"
        Actual = "[affirmation a 0]" }

    let [<Fact>] usingFileTest () = test {
        Expected = "() 5 5 7"
        Actual = "[usingFile \"" + amlFilePathStr + "\"] x y z" }

    let [<Fact>] missingFileTest () = test {
        Expected = "(violation :v/exception)"
        Actual = "[usingFile \"" + missingAmlFilePathStr + "\"]" }

    let [<Fact>] usingFileStdlibTest () = test {
        Expected = "()"
        Actual = "[usingFile \"" + stdlibFilePathStr + "\"]" }

    let [<Fact>] declarationInProceduralScopeTest () = test {
        Expected = "(violation :v/reader/readFailure)"
        Actual = "[def f [x] pre: [def a 5] 5]" }

    let [<Fact>] declarationInProceduralScope2Test () = test {
        Expected = "(violation :v/reader/readFailure)"
        Actual = "(fun (x) [def a 5])" }

    let [<Fact>] refEqualityTest () = test {
        Expected = "#f"
        Actual = "(== (ref 0) (ref 0))" }

    let [<Fact>] refEquality2Test () = test {
        Expected = "#t"
        Actual = "(let (r (ref 0)) (== r r))" }

    let [<Fact>] refEquality3Test () = test {
        Expected = "#t"
        Actual = "(let (r (ref 0)) (r2 r) (== r r2))" }

    let [<Fact>] interveneTest () = test {
        Expected = "0"
        Actual = "(intervene {} (:v/languageModule 0))" }

    let [<Fact>] intervene2Test () = test {
        Expected = "0"
        Actual = "(let (f () {}) (intervene (f) (:v/languageModule 0)))" }

    let [<Fact>] intervene3Test () = test {
        Expected = "(violation :v/languageModule/missingLanguageModule)"
        Actual = "(intervene {} (:v/languageModule {}))" }

    let [<Fact>] interveneHideTest () = test {
        Expected = "0"
        Actual = "(intervene (intervene {} (:v 0 hide: #t)) (:v 1))" }

    let [<Fact>] interveneShowTest () = test {
        Expected = "1"
        Actual = "(intervene (intervene {} (:v 0)) (:v 1))" }

    let [<Fact>] abstractingArgTest () = test {
        Expected = "(ref 2)"
        Actual = "(let (x (ref 0)) (twice (<|e|>) (steps! (e) (e))) (twice (set! x (iInc (get x)))))" }