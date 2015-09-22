// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<RequireQualifiedAccess>]
module AlgebraicReader =

    (* Reads strings with the following parses -

        (* Simple Values *)
        
        0
        None
        [2 2]
        "Hello World"
        CharacterAnimationFacing

        (* Complex Values *)
        []
        [Some 0]
        [Left 0]
        [[0 1] [2 4]]
        [AnimationData 4 8] *)

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
            let! values = many !refReadValue
            do! closeComplexValueForm
            do! skipWhitespace
            return values :> obj }

    do refReadValue :=
        attempt readSimpleValue <|>
        readComplexValue

    let stringToValue str =
        match run (skipWhitespace >>. !refReadValue) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error