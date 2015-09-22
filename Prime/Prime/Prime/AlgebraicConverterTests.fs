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
        let value = (AlgebraicConverter typeof<int>).ConvertFromString "0" :?> int
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToNone () =
        let value = (AlgebraicConverter typeof<string option>).ConvertFromString "None" :?> string option
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        let value = (AlgebraicConverter typeof<string option>).ConvertFromString "[Some string]" :?> string option
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToRightString () =
        let value = (AlgebraicConverter typeof<Either<unit, string>>).ConvertFromString "[Right string]" :?> Either<unit, string>
        Assert.Equal<Either<unit, string>> (Right "string", value)

    let [<Fact>] canConvertStringToIntList () =
        let value = (AlgebraicConverter typeof<int list>).ConvertFromString "[0 1]" :?> int list
        Assert.Equal<int list> ([0; 1], value)

    let [<Fact>] canConvertStringToTuple () =
        let value = (AlgebraicConverter typeof<int * int>).ConvertFromString "[0 1]" :?> int * int
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let value = (AlgebraicConverter typeof<(int * int) * (int * int)>).ConvertFromString "[[0 1] | [2 3]]" :?> (int * int) * (int * int)
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let value = (AlgebraicConverter typeof<IntIntRecord>).ConvertFromString "[0 1]" :?> IntIntRecord
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let value = (AlgebraicConverter typeof<SimpleUnion>).ConvertFromString "SimpleUnion" :?> SimpleUnion
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let value = (AlgebraicConverter typeof<ComplexUnion>).ConvertFromString "[ComplexUnion 0]" :?> ComplexUnion
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let value = (AlgebraicConverter typeof<ComplexUnion * ComplexUnion>).ConvertFromString "[[ComplexUnion 0] [ComplexUnion2 1 2]]" :?> ComplexUnion * ComplexUnion
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)