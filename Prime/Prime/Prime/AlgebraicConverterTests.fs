// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime.Tests
open System
open System.ComponentModel
open FParsec
open Xunit
open Prime

type IntIntRecord = { Int : int; Int2 : int }

type SimpleUnion =
    | SimpleUnion
    | SimpleUnion2

type [<StructuralEquality; NoComparison>] ComplexUnion =
    | ComplexUnion of int
    | ComplexUnion2 of int * int

module AlgebraicConverterTests =
    
    let [<Fact>] canConvertStringToInt () =
        let value = acvalue<int> "0"
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToNone () =
        let value = acvalue<string option> "None"
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        let value = acvalue<string option> "[Some string]"
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToRightString () =
        let value = acvalue<Either<unit, string>> "[Right string]"
        Assert.Equal<Either<unit, string>> (Right "string", value)

    let [<Fact>] canConvertStringToIntList () =
        let value = acvalue<int list> "[0 1]"
        Assert.Equal<int list> ([0; 1], value)

    let [<Fact>] canConvertStringToTuple () =
        let value = acvalue<int * int> "[0 1]"
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let value = acvalue<(int * int) * (int * int)> "[[0 1] | [2 3]]"
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let value = acvalue<IntIntRecord> "[0 1]"
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let value = acvalue<SimpleUnion> "SimpleUnion"
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let value = acvalue<ComplexUnion> "[ComplexUnion 0]"
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let value = acvalue<ComplexUnion * ComplexUnion> "[[ComplexUnion 0] [ComplexUnion2 1 2]]"
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)

    let [<Fact>] canConvertStringToMapIntInt () =
        let value = acvalue<Map<int, int>> "[[0 1]]"
        ignore value // TODO: assert for values