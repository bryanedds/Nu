namespace Nu
open System
open System.Collections.Generic
open FParsec
open OpenTK
open Xunit
open Prime
open Nu
open Nu.Constants
module AlgebraicConverterTests =

    type IntIntRecord = { Int : int; Int2 : int }

    type SimpleUnion =
        | SimpleUnion
        | SimpleUnion2

    type [<NoComparison>] ComplexUnion =
        | ComplexUnion of int
        | ComplexUnion2 of int * int

    let [<Fact>] canConvertStringToInt () =
        let converter = AlgebraicConverter<int> ()
        let value = converter.ConvertFromString "0" :?> int
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToVector2 () =
        Math.initTypeConverters ()
        let converter = AlgebraicConverter<Vector2> ()
        let value = converter.ConvertFromString "0, 1" :?> Vector2
        Assert.Equal (Vector2 (0.0f, 1.0f), value)

    let [<Fact>] canConvertStringToNone () =
        Math.initTypeConverters ()
        let converter = AlgebraicConverter<string option> ()
        let value = converter.ConvertFromString "None" :?> string option
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        Math.initTypeConverters ()
        let converter = AlgebraicConverter<string option> ()
        let value = converter.ConvertFromString "[Some; string]" :?> string option
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToIntList () =
        Math.initTypeConverters ()
        let converter = AlgebraicConverter<int list> ()
        let value = converter.ConvertFromString "[0; 1]" :?> obj list
        Assert.Equal<obj list> ([0 :> obj; 1 :> obj], value)

    let [<Fact>] canConvertStringToTuple () =
        let converter = AlgebraicConverter<int * int> ()
        let value = converter.ConvertFromString "[0; 1]" :?> int * int
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let converter = AlgebraicConverter<(int * int) * (int * int)> ()
        let value = converter.ConvertFromString "[[0; 1]; [2; 3]]" :?> (int * int) * (int * int)
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let converter = AlgebraicConverter<IntIntRecord> ()
        let value = converter.ConvertFromString "[0; 1]" :?> IntIntRecord
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let converter = AlgebraicConverter<SimpleUnion> ()
        let value = converter.ConvertFromString "SimpleUnion" :?> SimpleUnion
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let converter = AlgebraicConverter<ComplexUnion> ()
        let value = converter.ConvertFromString "[ComplexUnion; 0]" :?> ComplexUnion
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let converter = AlgebraicConverter<ComplexUnion * ComplexUnion> ()
        let value = converter.ConvertFromString "[[ComplexUnion; 0]; [ComplexUnion2; 1; 2]]" :?> ComplexUnion * ComplexUnion
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)