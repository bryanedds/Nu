namespace Nu
open System
open System.Collections.Generic
open FParsec
open Xunit
open Prime
open Nu
open Nu.Constants
module ParserTests =

    let [<Fact>] canReadSimpleValue () =
        let simpleValue = Reader.stringToValue "  test  "
        Assert.Equal ("test" :> obj, simpleValue)

    let [<Fact>] canReadComplexValue () =
        let value = Reader.stringToValue "  [  test  ]  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["test"], valueStrList)

    let [<Fact>] canReadComplexValue2 () =
        let value = Reader.stringToValue "  [  test  ;  ing  ;  it  ]  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["test"; "ing"; "it"], valueStrList)

    let [<Fact>] cannotReadInvalidComplexUnion () =
        Assert.Throws<Exception> (fun () -> Reader.stringToValue "  {  union  }  ")

    let [<Fact>] canReadComplexUnion () =
        let value = Reader.stringToValue "  {  union  ;  field  }  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["union"; "field"], valueStrList)

    let [<Fact>] canReadComplexUnion2 () =
        let value = Reader.stringToValue "  {  union  ;  field  ;  field2  }  "
        let valueStrList = value :?> obj list |> List.map string
        Assert.Equal<string list> (["union"; "field"; "field2"], valueStrList)
