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

    type SymbolIndex =
        | AtomIndex of int64 * SymbolIndex list
        | MoleculeIndex of int64 * SymbolIndex list

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

    module private Reader =

        let (readSymbol : Parser<Symbol, unit>, refReadSymbol : Parser<Symbol, unit> ref) =
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

    module private Indexer =

        let (readSymbolIndex : Parser<SymbolIndex, unit>, refReadSymbolIndex : Parser<SymbolIndex, unit> ref) =
            createParserForwardedToRef ()

        let readAtomIndex =
            parse {
                let! openPosition = getPosition
                let! _ = readContentChars
                let! symbolIndices = many readSymbolIndex
                return AtomIndex (openPosition.Index, symbolIndices) }

        let readMoleculeIndex =
            parse {
                let! openPosition = getPosition
                do! openMoleculeForm
                let! symbolIndices = many readSymbolIndex
                do! closeMoleculeForm
                return MoleculeIndex (openPosition.Index, symbolIndices) }

        do refReadSymbolIndex :=
            attempt readMoleculeIndex <|>
            readAtomIndex

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
    let fromString str =
        match run (skipWhitespaces >>. Reader.readSymbol) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a symbol to a string.
    let rec toString symbol =
        match symbol with
        | Atom str -> str
        | Quote str -> OpenQuoteStr + str + CloseQuoteStr
        | Molecule symbols -> OpenMoleculeStr + String.Join (" ", List.map toString symbols) + CloseMoleculeStr

    /// Attempt to convert a string to a its symbol indices.
    let stringToOptSymbolIndex str =
        if String.IsNullOrWhiteSpace str then None
        else
            match run (skipWhitespaces >>. Indexer.readSymbolIndex) str with
            | Success (value, _, _) -> Some value
            | Failure (error, _, _) -> failwith error

    /// Pretty-print a string in the form an symbolic-expression.
    let prettyPrint (str : string) =
        let builder = Text.StringBuilder str
        let mutable builderIndex = 0
        let rec advance hasContentParent tabDepth childIndex symbolIndex =
            match symbolIndex with
            | AtomIndex (_, symbolIndices) ->
                List.iteri (advance true tabDepth) symbolIndices
            | MoleculeIndex (index, symbolIndices) ->
                if index <> 0L && (hasContentParent || childIndex <> 0) then
                    let whitespace = "\n" + String.replicate tabDepth " "
                    ignore ^ builder.Insert (int index + builderIndex, whitespace)
                    builderIndex <- builderIndex + whitespace.Length
                List.iteri (advance false (tabDepth + 1)) symbolIndices
        match stringToOptSymbolIndex str with
        | Some indexLocation -> advance false 0 0 indexLocation
        | None -> ()
        builder.ToString ()