// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type SymbolIndex =
    | ListIndex of int64 * SymbolIndex list
    | ContentIndex of int64 * SymbolIndex list

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

    module private AlgebraicSymbolIndexer =

        let (readSymbolIndex : Parser<SymbolIndex, unit>, refReadSymbolIndex : Parser<SymbolIndex, unit> ref) =
            createParserForwardedToRef ()

        let readListIndex =
            parse {
                let! openPosition = getPosition
                do! openComplexValueForm
                do! skipWhitespace
                let! symbolIndices = many readSymbolIndex
                do! closeComplexValueForm
                do! skipWhitespace
                return ListIndex (openPosition.Index, symbolIndices) }

        let readContentIndex =
            parse {
                let! openPosition = getPosition
                let! _ = many1 (readSimpleValueChars .>> skipWhitespace)
                let! symbolIndices = many readSymbolIndex
                return ContentIndex (openPosition.Index, symbolIndices) }

        do refReadSymbolIndex :=
            attempt readListIndex <|>
            readContentIndex

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
    let stringToOptSymbolIndex str =
        if String.IsNullOrWhiteSpace str then None
        else
            match run (skipWhitespace >>. AlgebraicSymbolIndexer.readSymbolIndex) str with
            | Success (value, _, _) -> Some value
            | Failure (error, _, _) -> failwith error

    /// Pretty-print a string in the form an algebraic-expression.
    let prettyPrint (str : string) =
        let builder = Text.StringBuilder str
        let mutable builderIndex = 0
        let rec advance hasContentParent tabDepth childIndex symbolIndex =
            match symbolIndex with
            | ListIndex (index, symbolIndices) ->
                if index <> 0L && (hasContentParent || childIndex <> 0) then
                    let whitespace = "\n" + String.replicate tabDepth " "
                    ignore ^ builder.Insert (int index + builderIndex, whitespace)
                    builderIndex <- builderIndex + whitespace.Length
                List.iteri (advance false (tabDepth + 1)) symbolIndices
            | ContentIndex (_, symbolIndices) ->
                List.iteri (advance true tabDepth) symbolIndices
        match stringToOptSymbolIndex str with
        | Some indexLocation -> advance false 0 0 indexLocation
        | None -> ()
        builder.ToString ()