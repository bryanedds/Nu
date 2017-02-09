// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open FParsec
open Prime

type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>] SyntaxAttribute (keywords0 : string, keywords1 : string) =
    inherit Attribute ()
    member this.Keywords0 = keywords0
    member this.Keywords1 = keywords1

type SymbolSource =
    { FileNameOpt : string option
      Text : string }

type SymbolState =
    { SymbolSource : SymbolSource }

type SymbolOrigin =
    { Source : SymbolSource
      Start : Position
      Stop : Position }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Origin =

    let printStart origin = "[Ln: " + string origin.Start.Line + ", Col: " + string origin.Start.Column + "]"
    let printStop origin = "[Ln: " + string origin.Stop.Line + ", Col: " + string origin.Stop.Column + "]"
    let print origin = "Error found starting at " + printStart origin + " and stopping at " + printStop origin + "."
    let tryPrint originOpt = match originOpt with Some origin -> print origin | None -> "Error origin unknown or not applicable."

type Symbol =
    | Atom of string * SymbolOrigin option
    | Number of string * SymbolOrigin option
    | String of string * SymbolOrigin option
    | Quote of string * SymbolOrigin option
    | Symbols of Symbol list * SymbolOrigin option

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = "\t " + NewlineChars
    let [<Literal>] SeparatorChar = ' '
    let [<Literal>] SeparatorStr = " "
    let [<Literal>] OpenSymbolsChar = '['
    let [<Literal>] OpenSymbolsStr = "["
    let [<Literal>] CloseSymbolsChar = ']'
    let [<Literal>] CloseSymbolsStr = "]"
    let [<Literal>] OpenStringChar = '\"'
    let [<Literal>] OpenStringStr = "\""
    let [<Literal>] CloseStringChar = '\"'
    let [<Literal>] CloseStringStr = "\""
    let [<Literal>] OpenQuoteChar = '`'
    let [<Literal>] OpenQuoteStr = "`"
    let [<Literal>] CloseQuoteChar = '\''
    let [<Literal>] CloseQuoteStr = "\'"
    let [<Literal>] LineCommentChar = ';'
    let [<Literal>] LineCommentStr = ";"
    let [<Literal>] OpenMultilineCommentStr = "#|"
    let [<Literal>] CloseMultilineCommentStr = "|#"
    let [<Literal>] ReservedChars = ":"
    let [<Literal>] StructureCharsNoStr = "[]`\'"
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let [<Literal>] NumberFormat =
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowPlusSign |||
        NumberLiteralOptions.AllowExponent |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowHexadecimal |||
        NumberLiteralOptions.AllowSuffix
    
    let isWhitespaceChar chr = isAnyOf WhitespaceChars chr
    let isStructureChar chr = isAnyOf StructureChars chr
    let isExplicit (str : string) = str.StartsWith OpenStringStr && str.EndsWith CloseStringStr
    let distillate (str : string) = (str.Replace (OpenStringStr, "")).Replace (CloseStringStr, "")
    
    let skipLineComment = skipChar LineCommentChar >>. skipRestOfLine true
    let skipMultilineComment =
        // TODO: make multiline comments nest.
        between
            (skipString OpenMultilineCommentStr)
            (skipString CloseMultilineCommentStr)
            (skipCharsTillString CloseMultilineCommentStr false System.Int32.MaxValue)
    
    let skipWhitespace = skipLineComment <|> skipMultilineComment <|> skipAnyOf WhitespaceChars
    let skipWhitespaces = skipMany skipWhitespace
    let followedByWhitespaceOrStructureCharOrAtEof = nextCharSatisfies (fun chr -> isWhitespaceChar chr || isStructureChar chr) <|> eof
    
    let openSymbols = skipChar OpenSymbolsChar
    let closeSymbols = skipChar CloseSymbolsChar
    let openString = skipChar OpenStringChar
    let closeString = skipChar CloseStringChar
    let openQuote = skipChar OpenQuoteChar
    let closeQuote = skipChar CloseQuoteChar
    
    let isNumberParser = numberLiteral NumberFormat "number" >>. eof
    let isNumber str = match run isNumberParser str with Success (_, _, position) -> position.Index = int64 str.Length | Failure _ -> false
    let shouldBeExplicit str = Seq.exists (fun chr -> Char.IsWhiteSpace chr || Seq.contains chr StructureCharsNoStr) str

    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let readQuoteChars = many (noneOf [CloseQuoteChar])
    let readContentChars =
        many1
            ((attempt (openQuote >>. skipWhitespaces >>. readQuoteChars .>> closeQuote .>> skipWhitespaces)) <|>
             (attempt (openString >>. skipWhitespaces >>. readStringChars .>> closeString .>> skipWhitespaces)) <|>
             (readAtomChars .>> skipWhitespaces))

    let (readSymbol : Parser<Symbol, SymbolState>, private refReadSymbol : Parser<Symbol, SymbolState> ref) =
        createParserForwardedToRef ()

    let readAtom =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! chars = readAtomChars
            let! stop = getPosition
            do! skipWhitespaces
            let str = chars |> String.implode |> fun str -> str.TrimEnd ()
            let origin = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Atom (str, origin) }

    let readNumber =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! number = numberLiteral NumberFormat "number"
            do! followedByWhitespaceOrStructureCharOrAtEof
            let! stop = getPosition
            do! skipWhitespaces
            let origin = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            let suffix =
                (if number.SuffixChar1 <> (char)65535 then string number.SuffixChar1 else "") + 
                (if number.SuffixChar2 <> (char)65535 then string number.SuffixChar2 else "") + 
                (if number.SuffixChar3 <> (char)65535 then string number.SuffixChar3 else "") + 
                (if number.SuffixChar4 <> (char)65535 then string number.SuffixChar4 else "")
            return Number (number.String + suffix, origin) }

    let readString =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! openString
            do! skipWhitespaces
            let! escaped = readStringChars
            do! closeString
            let! stop = getPosition
            do! skipWhitespaces
            let str = escaped |> String.implode
            let origin = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return String (str, origin) }

    let readQuote =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! openQuote
            do! skipWhitespaces
            let! quoteChars = readQuoteChars
            do! closeQuote
            let! stop = getPosition
            do! skipWhitespaces
            let str = quoteChars |> String.implode
            let origin = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Quote (str, origin) }

    let readSymbols =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! openSymbols
            do! skipWhitespaces
            let! symbols = many readSymbol
            do! closeSymbols
            let! stop = getPosition
            do! skipWhitespaces
            let origin = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Symbols (symbols, origin) }

    do refReadSymbol :=
        attempt readQuote <|>
        attempt readString <|>
        attempt readNumber <|>
        attempt readAtom <|>
        readSymbols

    let rec writeSymbol symbol =
        match symbol with
        | Atom (str, _) ->
            let str = distillate str
            if Seq.isEmpty str then OpenStringStr + CloseStringStr
            elif not (isExplicit str) && shouldBeExplicit str then OpenStringStr + str + CloseStringStr
            elif isExplicit str && not (shouldBeExplicit str) then str.Substring (1, str.Length - 2)
            else str
        | Number (str, _) -> distillate str
        | String (str, _) -> OpenStringStr + distillate str + CloseStringStr
        | Quote (str, _) -> OpenQuoteStr + distillate str + CloseQuoteStr
        | Symbols (symbols, _) -> OpenSymbolsStr + String.Join (" ", List.map writeSymbol symbols) + CloseSymbolsStr

    /// Convert a string to a symbol, with the following parses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0.0f
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]'
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let fromString str =
        let symbolState = { SymbolSource = { FileNameOpt = None; Text = str }}
        match runParserOnString (skipWhitespaces >>. readSymbol) symbolState String.Empty str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a symbol to a string, with the following unparses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0.0f
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]'
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let rec toString symbol = writeSymbol symbol

    /// Try to get the Origin of the symbol if it has one.
    let tryGetOrigin symbol =
        match symbol with
        | Atom (_, originOpt)
        | Number (_, originOpt)
        | String (_, originOpt)
        | Quote (_, originOpt)
        | Symbols (_, originOpt) -> originOpt

    /// Cascade a symbol string into multiple lines with proper tabbing.
    /// TODO: split up this function.
    let private cascade str =
        let rec getCascadeDepth symbol depth =
            match symbol with
            | Atom _ -> depth
            | Symbols (_ :: _ as symbols', _) -> getCascadeDepth (List.head symbols') (depth + 1)
            | _ -> 0
        let symbol = fromString str
        let symbolStr = toString symbol
        let builder = Text.StringBuilder symbolStr
        let mutable builderIndex = 0
        let rec advance tabDepth cascadeDepth symbol =
            match tryGetOrigin symbol with
            | Some origin ->
                let cascadeDepth' = getCascadeDepth symbol 0
                if origin.Start.Index <> 0L && cascadeDepth < 1 && cascadeDepth' > 0 then
                    let whitespace = "\r\n" + String.replicate tabDepth " "
                    ignore ^ builder.Insert (int origin.Start.Index + builderIndex, whitespace)
                    builderIndex <- builderIndex + whitespace.Length
                match symbol with
                | Symbols (symbols, _) ->
                    let tabDepth' = tabDepth + 1
                    let cascadeDepth'' = if cascadeDepth' > 0 then cascadeDepth' - 1 else cascadeDepth - 1
                    List.iteri (fun i symbol -> advance tabDepth' (if i = 0 then cascadeDepth'' else 0) symbol) symbols
                | _ -> ()
            | None -> failwithumf ()
        advance 0 -1 symbol
        string builder

    /// Pretty-print a symbol string in the form an symbolic-expression.
    let prettyPrint str =
        let strCascaded = cascade str
        let lines = strCascaded.Split ([|"\r\n"|], StringSplitOptions.None)
        let linesTrimmed = Array.map (fun (str : string) -> str.TrimEnd ()) lines
        let strPretty = String.Join ("\r\n", linesTrimmed)
        strPretty

type ConversionException (message : string, symbolOpt : Symbol option) =
    inherit Exception (message)
    member this.SymbolOpt = symbolOpt
    override this.ToString () =
        message + "\r\n" +
        (match symbolOpt with Some symbol -> Origin.tryPrint (Symbol.tryGetOrigin symbol) + "\r\n" | _ -> "") +
        base.ToString ()

[<AutoOpen>]
module ConversionExceptionOperators =
    let failconv message symbol =
        raise ^ ConversionException (message, symbol)