// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type TabDirection =
    | TabForward of bool
    | TabBackward

type TabLocation =
    { OpenIndex : int64
      OpenIndex2 : int64
      CloseIndex : int64
      Reductions : int }

[<RequireQualifiedAccess>]
module AlgebraicReader =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = " \t" + NewlineChars
    let [<Literal>] SeparatorChar = ' '
    let [<Literal>] SeparatorStr = " "
    let [<Literal>] OpenComplexValueChar = '['
    let [<Literal>] OpenComplexValueStr = "["
    let [<Literal>] CloseComplexValueChar = ']'
    let [<Literal>] CloseComplexValueStr = "]"
    let [<Literal>] StructureChars = "[]"

    let skipWhitespaceChar = skipAnyOf WhitespaceChars
    let skipWhitespace = skipMany skipWhitespaceChar

    let charForm character = skipChar character >>. skipWhitespace
    let openComplexValueForm = charForm OpenComplexValueChar
    let closeComplexValueForm = charForm CloseComplexValueChar
    let readSimpleValueChars = many1 (noneOf (StructureChars + WhitespaceChars))

    module private AlgebraicValueReader =

        let (readValue : Parser<obj, unit>, refReadValue : Parser<obj, unit> ref) =
            createParserForwardedToRef ()

        let readSimpleValue =
            parse {
                let! chars = readSimpleValueChars
                do! skipWhitespace
                return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> objectify }

        let readComplexValue =
            parse {
                do! openComplexValueForm
                let! values = many readValue
                do! closeComplexValueForm
                do! skipWhitespace
                return values :> obj }

        do refReadValue :=
            attempt readSimpleValue <|>
            readComplexValue

    module private AlgebraicTabReader =

        let (readTabLocations : Parser<TabLocation list, unit>, refReadTabLocations : Parser<TabLocation list, unit> ref) =
            createParserForwardedToRef ()

        let rec readTabLocationsImpl =
            parse {
                let! openPosition = getPosition
                do! openComplexValueForm
                do! skipWhitespace
                let! openPosition2 = getPosition
                let! nextCharFn = fun (stream : _ CharStream) -> Reply (fun () -> stream.Peek ())
                let nextChar = nextCharFn ()
                let! algebraicTabLists = many readTabLocations
                let! closePosition = getPosition
                do! closeComplexValueForm
                return
                    let algebraicTabs =
                        if nextChar = OpenComplexValueChar then
                            [{ OpenIndex = openPosition.Index
                               OpenIndex2 = openPosition2.Index
                               CloseIndex = closePosition.Index
                               Reductions = 0 }]
                        else [] in
                    algebraicTabs @ List.concat algebraicTabLists }

        do refReadTabLocations :=
            attempt readTabLocationsImpl <|>
            parse {
                let! _ = readSimpleValueChars
                do! skipWhitespace
                return [] }

        let rec reduceTabLocations again tabLocations =
            let (again, tabLocationsReduced) =
                List.foldBack
                    (fun (tab, tab2) (again, tabs) ->
                        if tab.OpenIndex + 1L = tab2.OpenIndex then
                            let tabCombined =
                                { tab with
                                    OpenIndex2 = tab2.OpenIndex2
                                    CloseIndex = tab2.CloseIndex
                                    Reductions = tab.Reductions + 1 }
                            (true, tabCombined :: tabs)
                        else (again, tab :: tabs))
                    (List.pairwise tabLocations)
                    (again, [])
            if again
            then reduceTabLocations false tabLocationsReduced
            else tabLocationsReduced

    /// Convert a string to an algebraic value, with the following parses:
    /// 
    /// (* Simple Values *)
    /// 
    /// 0
    /// None
    /// [2 2]
    /// Hello_World
    /// CharacterAnimationFacing
    /// 
    /// (* Complex Values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    let stringToValue str =
        match run (skipWhitespace >>. AlgebraicValueReader.readValue) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a string to a list of its tab locations.
    let stringToTabLocations str =
        if String.IsNullOrWhiteSpace str then []
        else
            match run (skipWhitespace >>. AlgebraicTabReader.readTabLocations) str with
            | Success (value, _, _) -> value//AlgebraicTabReader.reduceTabLocations false value
            | Failure (error, _, _) -> failwith error
