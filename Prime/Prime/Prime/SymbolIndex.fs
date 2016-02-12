// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open FParsec
open Prime

// TODO: see if this can be got rid of by having an alternatve way to write symbols directly.
type SymbolIndex =
    | AtomIndex of int64 * SymbolIndex list
    | MoleculeIndex of int64 * SymbolIndex list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SymbolIndex =

    let (readSymbolIndex : Parser<SymbolIndex, unit>, private refReadSymbolIndex : Parser<SymbolIndex, unit> ref) =
        createParserForwardedToRef ()

    let readAtomIndex =
        parse {
            let! openPosition = getPosition
            let! _ = Symbol.readContentChars
            let! symbolIndices = many readSymbolIndex
            return AtomIndex (openPosition.Index, symbolIndices) }

    let readMoleculeIndex =
        parse {
            let! openPosition = getPosition
            do! Symbol.openMoleculeForm
            let! symbolIndices = many readSymbolIndex
            do! Symbol.closeMoleculeForm
            return MoleculeIndex (openPosition.Index, symbolIndices) }

    do refReadSymbolIndex :=
        attempt readMoleculeIndex <|>
        readAtomIndex

    /// Attempt to a symbol index from a string.
    let fromString str =
        if String.IsNullOrWhiteSpace str then None
        else
            match run (Symbol.skipWhitespaces >>. readSymbolIndex) str with
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
        match fromString str with
        | Some indexLocation -> advance false 0 0 indexLocation
        | None -> ()
        builder.ToString ()