namespace Prime
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
        let value = AlgebraicReader.stringToValue "  [  test  |  ing  |  it  ]  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["test"; "ing"; "it"], valueStrList)