// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type Symbol =
    | Atom of string
    | Quote of string
    | Molecule of Symbol list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = " \t" + NewlineChars
    let [<Literal>] SeparatorChar = ' '
    let [<Literal>] SeparatorStr = " "
    let [<Literal>] OpenMoleculeChar = '['
    let [<Literal>] OpenMoleculeStr = "["
    let [<Literal>] CloseMoleculeChar = ']'
    let [<Literal>] CloseMoleculeStr = "]"
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
    let openMoleculeForm = charForm OpenMoleculeChar
    let closeMoleculeForm = charForm CloseMoleculeChar
    let openStringForm = skipChar OpenStringChar
    let closeStringForm = charForm CloseStringChar
    let openQuoteForm = charForm OpenQuoteChar
    let closeQuoteForm = charForm CloseQuoteChar

    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let readQuoteChars = many (noneOf [CloseQuoteChar])
    let readContentChars =
        many1
            ((attempt (openQuoteForm >>. readQuoteChars .>> closeQuoteForm)) <|>
             (attempt (openStringForm >>. readStringChars .>> closeStringForm)) <|>
             (readAtomChars .>> skipWhitespaces))

    let (readSymbol : Parser<Symbol, unit>, private refReadSymbol : Parser<Symbol, unit> ref) =
        createParserForwardedToRef ()

    let readAtom =
        parse {
            let! chars = readAtomChars
            do! skipWhitespaces
            return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> Atom }

    let readAtomAsString =
        parse {
            do! openStringForm
            let! stringChars = readStringChars
            do! closeStringForm
            return stringChars |> String.implode |> Atom }

    let readQuote =
        parse {
            do! openQuoteForm
            let! quoteChars = readQuoteChars
            do! closeQuoteForm
            return quoteChars |> String.implode |> Quote }

    let readMolecule =
        parse {
            do! openMoleculeForm
            let! values = many readSymbol
            do! closeMoleculeForm
            return values |> Molecule }

    do refReadSymbol :=
        attempt readAtomAsString <|>
        attempt readQuote <|>
        attempt readAtom <|>
        readMolecule

    let rec writeSymbol symbol =
        match symbol with
        | Atom str -> str
        | Quote str -> OpenQuoteStr + str + CloseQuoteStr
        | Molecule symbols -> OpenMoleculeStr + String.Join (" ", List.map writeSymbol symbols) + CloseMoleculeStr

    /// Convert a string to a symbol, with the following parses:
    /// 
    /// (* Atom values *)
    /// 
    /// 0
    /// None
    /// [2 2]
    /// Hello_World
    /// CharacterAnimationFacing
    /// "String with quoted spaces."
    /// String_with_underscores_for_spaces.
    /// 
    /// (* Molecule values *)
    ///
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let fromString str =
        match run (skipWhitespaces >>. readSymbol) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a symbol to a string, with the following unparses:
    /// 
    /// (* Atom values *)
    /// 
    /// 0
    /// None
    /// [2 2]
    /// Hello_World
    /// CharacterAnimationFacing
    /// "String with quoted spaces."
    /// String_with_underscores_for_spaces.
    /// 
    /// (* Molecule values *)
    ///
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let rec toString symbol = writeSymbol symbol