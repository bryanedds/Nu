// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime.Tests
open System
open System.Collections.Generic
open FParsec
open Xunit
open Prime
module AlgebraicParserTests =

    let [<Fact>] canReadSimpleValue () =
        let simpleValue = AlgebraicReader.stringToValue "  test  "
        Assert.Equal ("test" :> obj, simpleValue)

    let [<Fact>] canReadComplexValue () =
        let value = AlgebraicReader.stringToValue "  [  test  ]  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["test"], valueStrList)

    let [<Fact>] canReadComplexValue2 () =
        let value = AlgebraicReader.stringToValue "  [  test  ing  it  ]  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["test"; "ing"; "it"], valueStrList)

    let [<Fact>] canReadComplexValue3 () =
        ignore ^ AlgebraicReader.stringToValue "  [  test  [ing  it]  ]  "