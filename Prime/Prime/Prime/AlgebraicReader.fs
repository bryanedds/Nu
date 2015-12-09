// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type IndexLocation =
    | TabLocation of int64 * IndexLocation list
    | ContentLocation of int64 * IndexLocation list

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

        let (readIndexLocation : Parser<IndexLocation, unit>, refReadIndexLocation : Parser<IndexLocation, unit> ref) =
            createParserForwardedToRef ()

        let readTabLocation =
            parse {
                let! openPosition = getPosition
                do! openComplexValueForm
                do! skipWhitespace
                let! indexLocations = many readIndexLocation
                do! closeComplexValueForm
                do! skipWhitespace
                return TabLocation (openPosition.Index, indexLocations) }

        let readContentLocation =
            parse {
                let! openPosition = getPosition
                let! _ = many1 (readSimpleValueChars .>> skipWhitespace)
                let! indexLocations = many readIndexLocation
                return ContentLocation (openPosition.Index, indexLocations) }

        do refReadIndexLocation :=
            attempt readTabLocation <|>
            readContentLocation

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
    let stringToOptIndexLocation str =
        if String.IsNullOrWhiteSpace str then None
        else
            match run (skipWhitespace >>. AlgebraicTabReader.readIndexLocation) str with
            | Success (value, _, _) -> Some value
            | Failure (error, _, _) -> failwith error
