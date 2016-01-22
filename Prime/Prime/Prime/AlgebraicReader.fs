// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type AlgebraicQuote =
    | AlgebraicQuote of string

type AlgebraicIndex =
    | ListIndex of int64 * AlgebraicIndex list
    | ContentIndex of int64 * AlgebraicIndex list

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
    let [<Literal>] OpenStringChar = '\"'
    let [<Literal>] OpenStringStr = "\""
    let [<Literal>] CloseStringChar = '\"'
    let [<Literal>] CloseStringStr = "\""
    let [<Literal>] OpenQuoteChar = '`'
    let [<Literal>] OpenQuoteStr = "`"
    let [<Literal>] CloseQuoteChar = '\''
    let [<Literal>] CloseQuoteStr = "\'"
    let [<Literal>] StructureChars = "[]\"`\'"

    let skipWhitespace = skipAnyOf WhitespaceChars
    let skipWhitespaces = skipMany skipWhitespace

    let charForm character = skipChar character >>. skipWhitespaces
    let openComplexValueForm = charForm OpenComplexValueChar
    let closeComplexValueForm = charForm CloseComplexValueChar
    let openStringForm = skipChar OpenStringChar
    let closeStringForm = charForm CloseStringChar
    let openQuoteForm = charForm OpenQuoteChar
    let closeQuoteForm = charForm CloseQuoteChar

    let readAtomicValueChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let readQuoteChars = many (noneOf [CloseQuoteChar])
    let readContentChars =
        many1
            ((attempt (openQuoteForm >>. readQuoteChars .>> closeQuoteForm)) <|>
             (attempt (openStringForm >>. readStringChars .>> closeStringForm)) <|>
             (readAtomicValueChars .>> skipWhitespaces))

    module private AlgebraicValueReader =

        let (readValue : Parser<obj, unit>, refReadValue : Parser<obj, unit> ref) =
            createParserForwardedToRef ()

        let readAtomicValue =
            parse {
                let! chars = readAtomicValueChars
                do! skipWhitespaces
                return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> objectify }

        let readStringValue =
            parse {
                do! openStringForm
                let! stringChars = readStringChars
                do! closeStringForm
                return stringChars |> String.implode |> (fun str -> OpenStringStr + str + CloseStringStr) |> objectify }

        let readQuoteValue =
            parse {
                do! openQuoteForm
                let! quoteChars = readQuoteChars // TODO: enable reading quotes that have inner quotes
                do! closeQuoteForm
                return quoteChars |> String.implode |> AlgebraicQuote |> objectify }

        let readComplexValue =
            parse {
                do! openComplexValueForm
                let! values = many readValue
                do! closeComplexValueForm
                return values :> obj }

        do refReadValue :=
            attempt readStringValue <|>
            attempt readQuoteValue <|>
            attempt readAtomicValue <|>
            readComplexValue

    module private AlgebraicIndexer =

        let (readAlgebraicIndex : Parser<AlgebraicIndex, unit>, refReadAlgebraicIndex : Parser<AlgebraicIndex, unit> ref) =
            createParserForwardedToRef ()

        let readListIndex =
            parse {
                let! openPosition = getPosition
                do! openComplexValueForm
                let! algebraicIndices = many readAlgebraicIndex
                do! closeComplexValueForm
                return ListIndex (openPosition.Index, algebraicIndices) }

        let readContentIndex =
            parse {
                let! openPosition = getPosition
                let! _ = readContentChars
                let! algebraicIndices = many readAlgebraicIndex
                return ContentIndex (openPosition.Index, algebraicIndices) }

        do refReadAlgebraicIndex :=
            attempt readListIndex <|>
            readContentIndex

    /// Convert a string to an algebraic value, with the following parses:
    /// 
    /// (* Atomic Values *)
    /// 
    /// 0
    /// None
    /// [2 2]
    /// Hello_World
    /// CharacterAnimationFacing
    /// "String with quoted spaces."
    /// String_with_underscores_for_spaces.
    /// 
    /// (* Complex Values *)
    ///
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    let stringToValue str =
        match run (skipWhitespaces >>. AlgebraicValueReader.readValue) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Attempt to convert a string to a its algebraic indices.
    let stringToOptAlgebraicIndex str =
        if String.IsNullOrWhiteSpace str then None
        else
            match run (skipWhitespaces >>. AlgebraicIndexer.readAlgebraicIndex) str with
            | Success (value, _, _) -> Some value
            | Failure (error, _, _) -> failwith error

    /// Pretty-print a string in the form an algebraic-expression.
    let prettyPrint (str : string) =
        let builder = Text.StringBuilder str
        let mutable builderIndex = 0
        let rec advance hasContentParent tabDepth childIndex algebraicIndex =
            match algebraicIndex with
            | ListIndex (index, algebraicIndices) ->
                if index <> 0L && (hasContentParent || childIndex <> 0) then
                    let whitespace = "\n" + String.replicate tabDepth " "
                    ignore ^ builder.Insert (int index + builderIndex, whitespace)
                    builderIndex <- builderIndex + whitespace.Length
                List.iteri (advance false (tabDepth + 1)) algebraicIndices
            | ContentIndex (_, algebraicIndices) ->
                List.iteri (advance true tabDepth) algebraicIndices
        match stringToOptAlgebraicIndex str with
        | Some indexLocation -> advance false 0 0 indexLocation
        | None -> ()
        builder.ToString ()