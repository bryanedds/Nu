// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime.Tests
open System
open Xunit
open Prime

type IntIntRecord = { Int : int; Int2 : int }

[<SymbolicExpansion>]
type IntIntRecordExpanded = { IntX : int; IntX2 : int }

type SimpleUnion =
    | SimpleUnion
    | SimpleUnion2

type [<StructuralEquality; NoComparison>] ComplexUnion =
    | ComplexUnion of int
    | ComplexUnion2 of int * int

module SymbolTests =
    
    let [<Fact>] canConvertStringToAtom () =
        let converter = SymbolicConverter typeof<Symbol>
        match converter.ConvertFromString "atom" :?> Symbol with
        | Atom (str, _) -> Assert.Equal<string> ("atom", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToNumber () =
        let converter = SymbolicConverter typeof<Symbol>
        match converter.ConvertFromString "0" :?> Symbol with
        | Number (str, _) -> Assert.Equal<string> ("0", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToNegativeNumber () =
        let converter = SymbolicConverter typeof<Symbol>
        match converter.ConvertFromString "-1" :?> Symbol with
        | Number (str, _) -> Assert.Equal<string> ("-1", str)
        | _ -> Assert.True false
    
    let [<Fact>] canConvertStringToString () =
        let converter = SymbolicConverter typeof<Symbol>
        match converter.ConvertFromString "\"str\"" :?> Symbol with
        | String (str, _) -> Assert.Equal<string> ("str", str)
        | _ -> Assert.True false

    let [<Fact>] canConvertStringToInt () =
        let value = scvalue<int> "0"
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToNone () =
        let value = scvalue<string option> "None"
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        let value = scvalue<string option> "[Some string]"
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToRightString () =
        let value = scvalue<Either<unit, string>> "[Right string]"
        Assert.Equal<Either<unit, string>> (Right "string", value)

    let [<Fact>] canConvertStringToIntList () =
        let value = scvalue<int list> "[0 1]"
        Assert.Equal<int list> ([0; 1], value)

    let [<Fact>] canConvertStringToIntListList () =
        let value = scvalue<int list list> "[[]]"
        Assert.Equal<int list list> ([[]], value)

    let [<Fact>] canConvertStringToIntListListEmpty () =
        let value = scvalue<int list list> "[]"
        Assert.Equal<int list list> ([], value)

    let [<Fact>] canConvertStringToTuple () =
        let value = scvalue<int * int> "[0 1]"
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let value = scvalue<(int * int) * (int * int)> "[[0 1] [2 3]]"
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let value = scvalue<IntIntRecord> "[0 1]"
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToExpandedRecord () =
        let value = scvalue<IntIntRecordExpanded> "[[IntX 0] [IntX2 1]]"
        Assert.Equal ({ IntX = 0; IntX2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let value = scvalue<SimpleUnion> "SimpleUnion"
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let value = scvalue<ComplexUnion> "[ComplexUnion 0]"
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let value = scvalue<ComplexUnion * ComplexUnion> "[[ComplexUnion 0] [ComplexUnion2 1 2]]"
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)

    let [<Fact>] canConvertStringToMapIntInt () =
        let value = scvalue<Map<int, int>> "[[0 1]]"
        Assert.Equal (1, Map.find 0 value)

    let [<Fact>] canPrettyPrintGuid () =
        let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<Guid>).PrettyPrinter
        let symbolStr = "[5ec8734f-6a3d-4472-b86a-78125d238dc2]"
        let prettyStr = PrettyPrinter.prettyPrint symbolStr prettyPrinter
        Assert.Equal (symbolStr, prettyStr)