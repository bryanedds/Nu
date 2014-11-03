namespace Prime
open System
open FParsec
open Xunit
open Prime
module AlgebraicConverterTests =

    type IntIntRecord = { Int : int; Int2 : int }

    type SimpleUnion =
        | SimpleUnion
        | SimpleUnion2

    type [<NoComparison>] ComplexUnion =
        | ComplexUnion of int
        | ComplexUnion2 of int * int

    let [<Fact>] canConvertStringToInt () =
        let value = AlgebraicConverter.convertFromString "0" typeof<int> :?> int
        Assert.Equal (0, value)

    let [<Fact>] canConvertStringToNone () =
        AlgebraicConverter.initTypeConverters ()
        let value = AlgebraicConverter.convertFromString "None" typeof<string option> :?> string option
        Assert.Equal<string option> (None, value)

    let [<Fact>] canConvertStringToSomeString () =
        AlgebraicConverter.initTypeConverters ()
        let value = AlgebraicConverter.convertFromString "[Some | string]" typeof<string option> :?> string option
        Assert.Equal<string option> (Some "string", value)

    let [<Fact>] canConvertStringToRightString () =
        let value = AlgebraicConverter.convertFromString "[Right | string]" typeof<Either<unit, string>> :?> Either<unit, string>
        Assert.Equal<Either<unit, string>> (Right "string", value)

    let [<Fact>] canConvertStringToIntList () =
        let value = AlgebraicConverter.convertFromString"[0 | 1]" typeof<int list> :?> obj list
        Assert.Equal<obj list> ([0 :> obj; 1 :> obj], value)

    let [<Fact>] canConvertStringToTuple () =
        let value = AlgebraicConverter.convertFromString "[0 | 1]" typeof<int * int> :?> int * int
        Assert.Equal ((0, 1), value)

    let [<Fact>] canConvertStringToTupleTuple () =
        let value = AlgebraicConverter.convertFromString "[[0 | 1] | [2 | 3]]" typeof<(int * int) * (int * int)> :?> (int * int) * (int * int)
        Assert.Equal (((0, 1), (2, 3)), value)

    let [<Fact>] canConvertStringToRecord () =
        let value = AlgebraicConverter.convertFromString "[0 | 1]" typeof<IntIntRecord> :?> IntIntRecord
        Assert.Equal ({ Int = 0; Int2 = 1 }, value)

    let [<Fact>] canConvertStringToSimpleUnion () =
        let value = AlgebraicConverter.convertFromString "SimpleUnion" typeof<SimpleUnion> :?> SimpleUnion
        Assert.Equal (SimpleUnion, value)

    let [<Fact>] canConvertStringToComplexUnion () =
        let value = AlgebraicConverter.convertFromString "[ComplexUnion | 0]" typeof<ComplexUnion> :?> ComplexUnion
        Assert.Equal (ComplexUnion 0, value)

    let [<Fact>] canConvertStringToComplexUnionTuple () =
        let value = AlgebraicConverter.convertFromString "[[ComplexUnion | 0] | [ComplexUnion2 | 1 | 2]]" typeof<ComplexUnion * ComplexUnion> :?> ComplexUnion * ComplexUnion
        // each tuple element must be tested individually as Assert.Equal doesn't seem to support tuple unions...
        Assert.Equal (ComplexUnion 0, fst value)
        Assert.Equal (ComplexUnion2 (1, 2), snd value)