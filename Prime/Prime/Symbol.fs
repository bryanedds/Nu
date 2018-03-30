// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Text
open System.Reflection
open FParsec
open Prime

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
module SymbolOrigin =

    let printStart origin =
        "[Ln: " + string origin.Start.Line + ", Col: " + string origin.Start.Column + "]"

    let printStop origin =
        "[Ln: " + string origin.Stop.Line + ", Col: " + string origin.Stop.Column + "]"

    let printContext origin =
        try // there's more than one thing that can go wrong in here...
            let sourceLines = origin.Source.Text.Split '\n'
            let problemLineIndex = int origin.Start.Line - 1
            let problemLinesStartCount = problemLineIndex - Math.Max (0, problemLineIndex - 3)
            let problemLinesStart =
                sourceLines |>
                Array.trySkip (problemLineIndex - problemLinesStartCount) |>
                Array.take (inc problemLinesStartCount) |>
                String.concat "\n" |>
                fun str -> if String.isEmpty str then "" else str + "\n"
            let problemLinesStop =
                sourceLines |>
                Array.skip (inc problemLineIndex) |>
                Array.tryTake 4 |>
                String.concat "\n" |>
                fun str -> if String.isEmpty str then "" else "\n" + str
            let problemUnderline =
                String.replicate (int origin.Start.Column - 1) " " +
                if origin.Start.Line = origin.Stop.Line
                then String.replicate (int origin.Stop.Column - int origin.Start.Column) "^"
                else "^^^^^^^" // just use lots of carets...
            problemLinesStart + problemUnderline + problemLinesStop
        with exn ->
            // ...and I don't feel like dealing with all the specifics.
            "Error creating violation context."

    let print origin =
        "At location: " + printStart origin + " thru " + printStop origin + "\n" +
        "In context:\n" +
        "\n" +
        printContext origin

    let tryPrint originOpt =
        match originOpt with
        | Some origin -> print origin
        | None -> "Error origin unknown or not applicable."

type Symbol =
    | Atom of string * SymbolOrigin option
    | Number of string * SymbolOrigin option
    | String of string * SymbolOrigin option
    | Quote of Symbol * SymbolOrigin option
    | Symbols of Symbol list * SymbolOrigin option

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = "\t " + NewlineChars
    let [<Literal>] IndexChar = '.'
    let [<Literal>] IndexStr = "."
    let [<Literal>] OpenSymbolsChar = '['
    let [<Literal>] OpenSymbolsStr = "["
    let [<Literal>] CloseSymbolsChar = ']'
    let [<Literal>] CloseSymbolsStr = "]"
    let [<Literal>] OpenStringChar = '\"'
    let [<Literal>] OpenStringStr = "\""
    let [<Literal>] CloseStringChar = '\"'
    let [<Literal>] CloseStringStr = "\""
    let [<Literal>] QuoteChar = '`'
    let [<Literal>] QuoteStr = "`"
    let [<Literal>] LineCommentChar = ';'
    let [<Literal>] LineCommentStr = ";"
    let [<Literal>] OpenMultilineCommentStr = "#|"
    let [<Literal>] CloseMultilineCommentStr = "|#"
    let [<Literal>] IndexExpansion = "Index"
    let [<Literal>] ReservedChars = "(){}\\#$:,"
    let [<Literal>] StructureCharsNoStr = "[]`."
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let (*Literal*) IllegalNameChars = ReservedChars + StructureChars + WhitespaceChars
    let (*Literal*) IllegalNameCharsArray = Array.ofSeq IllegalNameChars
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
    
    let startIndex = skipChar IndexChar
    let startQuote = skipChar QuoteChar
    let openSymbols = skipChar OpenSymbolsChar
    let closeSymbols = skipChar CloseSymbolsChar
    let openString = skipChar OpenStringChar
    let closeString = skipChar CloseStringChar
    
    let isNumberParser = numberLiteral NumberFormat "number" >>. eof
    let isNumber str = match run isNumberParser str with Success (_, _, position) -> position.Index = int64 str.Length | Failure _ -> false
    let shouldBeExplicit str = Seq.exists (fun chr -> Char.IsWhiteSpace chr || Seq.contains chr StructureCharsNoStr) str
    
    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let (readSymbol : Parser<Symbol, SymbolState>, private readSymbolRef : Parser<Symbol, SymbolState> ref) = createParserForwardedToRef ()

    let readAtom =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! chars = readAtomChars
            let! stop = getPosition
            do! skipWhitespaces
            let str = chars |> String.implode |> fun str -> str.TrimEnd ()
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Atom (str, originOpt) }

    let readNumber =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! number = numberLiteral NumberFormat "number"
            do! followedByWhitespaceOrStructureCharOrAtEof
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            let suffix =
                (if number.SuffixChar1 <> (char)65535 then string number.SuffixChar1 else "") + 
                (if number.SuffixChar2 <> (char)65535 then string number.SuffixChar2 else "") + 
                (if number.SuffixChar3 <> (char)65535 then string number.SuffixChar3 else "") + 
                (if number.SuffixChar4 <> (char)65535 then string number.SuffixChar4 else "")
            return Number (number.String + suffix, originOpt) }

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
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return String (str, originOpt) }

    let readQuote =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! startQuote
            let! quoted = readSymbol
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Quote (quoted, originOpt) }

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
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Symbols (symbols, originOpt) }

    let readIndex =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! startIndex
            do! skipWhitespaces
            let! stop = getPosition
            do! skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return fun target indexer ->
                match indexer with
                | Symbols ([Number _ as number], _) -> Symbols ([Atom (IndexExpansion, originOpt); number; target], originOpt)
                | _ -> Symbols ([Atom (IndexExpansion, originOpt); indexer; target], originOpt) }

    let readSymbolBirecursive =
        attempt readQuote <|>
        attempt readString <|>
        attempt readNumber <|>
        attempt readAtom <|>
        readSymbols

    do readSymbolRef :=
        chainl1 readSymbolBirecursive readIndex

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
        | Quote (symbol, _) -> QuoteStr + writeSymbol symbol
        | Symbols (symbols, _) ->
            match symbols with
            | [Atom (str, _); Number _ as indexer; target] when str = IndexExpansion ->
                writeSymbol target + IndexStr + OpenSymbolsStr + writeSymbol indexer + CloseSymbolsStr
            | [Atom (str, _); indexer; target] when str = IndexExpansion ->
                writeSymbol target + IndexStr + writeSymbol indexer
            | _ ->
                OpenSymbolsStr + String.concat " " (List.map writeSymbol symbols) + CloseSymbolsStr

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
    /// `[Some 1]
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
    /// `[Some 1]
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]]
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

/// Pretty prints Symbols, as well as strings by converting them to Symbols.
type PrettyPrinter =
    { TitleWords : string Set
      HeaderWords : string Set
      DetailWords : string Set
      ThresholdMin : int
      ThresholdMax : int }

    static member defaulted =
        { TitleWords = Set.empty
          HeaderWords = Set.empty
          DetailWords = Set.empty
          ThresholdMin = Constants.PrettyPrinter.DefaultThresholdMin
          ThresholdMax = Constants.PrettyPrinter.DefaultThresholdMax }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PrettyPrinter =

    type private PrettySymbol =
        | PrettyAtom of bool * bool * bool * string * Symbol
        | PrettyNumber of string * Symbol
        | PrettyString of string * Symbol
        | PrettyIndex of int * PrettySymbol * PrettySymbol
        | PrettyQuote of int * PrettySymbol
        | PrettySymbols of bool * bool * int * PrettySymbol list

    let rec private getTitled prettySymbol =
        match prettySymbol with
        | PrettyAtom (titled, _, _, _, _) -> titled
        | _ -> false

    let rec private getHeadered prettySymbol =
        match prettySymbol with
        | PrettyAtom (_, headered, _, _, _) -> headered
        | _ -> false

    let rec private getDetailed prettySymbol =
        match prettySymbol with
        | PrettyAtom (_, _, detailed, _, _) -> detailed
        | _ -> false

    let rec private getMaxDepth prettySymbol =
        match prettySymbol with
        | PrettyAtom _ -> 0
        | PrettyNumber _ -> 0
        | PrettyString _ -> 0
        | PrettyIndex _ -> 0
        | PrettyQuote (maxDepth, _) -> maxDepth
        | PrettySymbols (_, _, maxDepth, _) -> maxDepth

    let rec private symbolToPrettySymbol symbol prettyPrinter =
        match symbol with
        | Atom (str, _) ->
            PrettyAtom
                (Set.contains str prettyPrinter.TitleWords,
                 Set.contains str prettyPrinter.HeaderWords,
                 Set.contains str prettyPrinter.DetailWords,
                 str,
                 symbol)
        | Number (str, _) -> PrettyNumber (str, symbol)
        | String (str, _) -> PrettyString (str, symbol)
        | Quote (quoted, _) ->
            let prettyQuoted = symbolToPrettySymbol quoted prettyPrinter
            let maxDepth = getMaxDepth prettyQuoted
            PrettyQuote (inc maxDepth, prettyQuoted)
        | Symbols (symbols, _) ->
            match symbols with
            | [Atom ("Index", _); indexer; target] ->
                let prettyIndexer = symbolToPrettySymbol indexer prettyPrinter
                let prettyTarget = symbolToPrettySymbol target prettyPrinter
                PrettyIndex (0, prettyIndexer, prettyTarget)
            | _ ->
                let prettySymbols = List.map (flip symbolToPrettySymbol prettyPrinter) symbols
                let titled = match prettySymbols with head :: _ -> getTitled head | [] -> false
                let headered = match prettySymbols with head :: _ -> getHeadered head | [] -> false
                let detailed = match prettySymbols with head :: _ -> getDetailed head | [] -> false
                let maxDepths = 0 :: List.map getMaxDepth prettySymbols
                let maxDepth = List.max maxDepths
                let maxDepth = if headered || detailed then maxDepth else maxDepth + 1
                PrettySymbols (titled, headered, maxDepth, prettySymbols)

    let rec private prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter =
        if unfolding then
            let symbolsLength = List.length symbols
            let prettyStrs =
                List.mapi (fun i prettySymbol ->
                    let whitespace =
                        if titled then
                            if i > 1 then "\n" + String.init (inc depth) (fun _ -> " ")
                            elif i > 0 then " "
                            else ""
                        elif headered then
                            if i = dec symbolsLength then "\n" + String.init (inc depth) (fun _ -> " ")
                            elif i > 0 then " "
                            else ""
                        else
                            if i > 0 then "\n" + String.init (inc depth) (fun _ -> " ")
                            else ""
                    let text = prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter
                    whitespace + text)
                    symbols
            let prettyStr = Symbol.OpenSymbolsStr + String.concat "" prettyStrs + Symbol.CloseSymbolsStr
            prettyStr
        else
            let prettyStrs =
                List.map (fun prettySymbol ->
                    prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter)
                    symbols
            let prettyStr = Symbol.OpenSymbolsStr + String.concat " " prettyStrs + Symbol.CloseSymbolsStr
            prettyStr

    and private prettySymbolToPrettyStr depth prettySymbol prettyPrinter =
        match prettySymbol with
        | PrettyAtom (_, _, _, _, symbol)
        | PrettyNumber (_, symbol)
        | PrettyString (_, symbol) -> Symbol.writeSymbol symbol
        | PrettyIndex (depth, prettyIndexer, prettyTarget) ->
            let prettyIndexerStr = prettySymbolToPrettyStr depth prettyIndexer prettyPrinter
            let prettyTargetStr = prettySymbolToPrettyStr depth prettyTarget prettyPrinter
            match prettyIndexer with
            | PrettyNumber _ -> prettyTargetStr + Symbol.IndexStr + Symbol.OpenSymbolsStr + prettyIndexerStr + Symbol.CloseSymbolsStr
            | _ -> prettyTargetStr + Symbol.IndexStr + prettyIndexerStr
        | PrettyQuote (_, prettySymbol) ->
            let prettyStr = prettySymbolToPrettyStr (inc depth) prettySymbol prettyPrinter
            Symbol.QuoteStr + prettyStr
        | PrettySymbols (titled, headered, maxDepth, symbols) ->
            let unfolding = depth < prettyPrinter.ThresholdMin || maxDepth > prettyPrinter.ThresholdMax
            prettySymbolsToPrettyStr titled headered depth unfolding symbols prettyPrinter

    let prettyPrintSymbol symbol prettyPrinter =
        let prettySymbol = symbolToPrettySymbol symbol prettyPrinter
        prettySymbolToPrettyStr 0 prettySymbol prettyPrinter

    let prettyPrint str prettyPrinter =
        let symbol = Symbol.fromString str
        prettyPrintSymbol symbol prettyPrinter

type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>]
    SyntaxAttribute
        (keywords0 : string,
         keywords1 : string,
         titleWordsStr : string,
         headerWordsStr : string,
         detailWordsStr : string,
         prettyPrinterThresholdMin : int,
         prettyPrinterThresholdMax : int) =
    inherit Attribute ()
    member this.Keywords0 = keywords0
    member this.Keywords1 = keywords1
    member this.PrettyPrinter =
        { TitleWords = Set.ofArray (titleWordsStr.Split ' ')
          HeaderWords = Set.ofArray (headerWordsStr.Split ' ')
          DetailWords = Set.ofArray (detailWordsStr.Split ' ')
          ThresholdMin = prettyPrinterThresholdMin
          ThresholdMax = prettyPrinterThresholdMax }
    static member getOrDefault (ty : Type) =
        match ty.GetCustomAttribute<SyntaxAttribute> true with
        | null ->
            SyntaxAttribute
                ("", "", "", "", "",
                 PrettyPrinter.defaulted.ThresholdMin,
                 PrettyPrinter.defaulted.ThresholdMax)
        | syntax -> syntax

type ConversionException (message : string, symbolOpt : Symbol option) =
    inherit Exception (message)
    member this.SymbolOpt = symbolOpt
    override this.ToString () =
        message + "\n" +
        (match symbolOpt with Some symbol -> SymbolOrigin.tryPrint (Symbol.tryGetOrigin symbol) + "\n" | _ -> "") +
        base.ToString ()

[<AutoOpen>]
module ConversionExceptionOperators =
    let failconv message symbol =
        raise (ConversionException (message, symbol))