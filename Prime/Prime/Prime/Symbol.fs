// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type [<AttributeUsage (AttributeTargets.Class); AllowNullLiteral>] SyntaxAttribute (keywords0 : string, keywords1 : string) =
    inherit Attribute ()
    member this.Keywords0 = keywords0
    member this.Keywords1 = keywords1

type Origin =
    { Start : Position
      Stop : Position }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Origin =

    let printStart origin = "[Ln: " + string origin.Start.Line + ", Col: " + string origin.Start.Column + "]"
    let printStop origin = "[Ln: " + string origin.Stop.Line + ", Col: " + string origin.Stop.Column + "]"
    let print origin = "Error found starting at " + printStart origin + " and stopping at " + printStop origin + "."
    let tryPrint optOrigin = match optOrigin with Some origin -> print origin | None -> "Error origin unknown or not applicable."

type Symbol =
    | Atom of string * Origin option
    | Number of string * Origin option
    | String of string * Origin option
    | Quote of string * Origin option
    | Symbols of Symbol list * Origin option

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = " \t" + NewlineChars
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
    let [<Literal>] StructureCharsNoStr = "[]`\'"
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let [<Literal>] OpsReserved = "@#:." // @ reserved for wildcards, # reserved for comment syntax, : reserved for pair syntax (expands to [key value]), . reserved for pathing
    let (*Literal*) OpsSeparator =  [|  '_'|]
    let (*Literal*) OpsExpanded =   [|  "Nor";  "Not";  "Nand"; "Mod";  "Xor";  "And";  "Mul";  "Add";  "Sub";  "Eq";   "Or";   "Lt";   "Gt";   "Get";  "Div"   |]
    let (*Literal*) OpsUnexpanded = [|  '~';    '!';    '$';    '%';    '^';    '&';    '*';    '+';    '-';    '=';    '|';    '<';    '>';    '?';    '/'     |]
    let [<Literal>] NumberFormat =
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowPlusSign |||
        NumberLiteralOptions.AllowExponent |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowHexadecimal

    let expand (unexpanded : string) =
        if unexpanded.IndexOfAny OpsUnexpanded = 0 then
            let partsExpanded = Seq.map (fun (part : char) -> match Array.IndexOf (OpsUnexpanded, part) with -1 -> string part | i -> OpsExpanded.[i]) unexpanded
            let expanded = String.Join (string OpsSeparator.[0], partsExpanded)
            expanded
        else unexpanded

    let unexpand (expanded : string) =
        let parts = expanded.Split OpsSeparator
        if Array.contains parts.[0] OpsExpanded then
            let partsUnexpanded = Array.map (fun (part : string) -> match Array.IndexOf (OpsExpanded, part) with -1 -> part | i -> string OpsUnexpanded.[i]) parts
            let unexpanded = String.Concat partsUnexpanded
            unexpanded
        else expanded

    /// Try to get the Origin of the symbol if it has one.
    let tryGetOrigin symbol =
        match symbol with
        | Atom (_, optOrigin)
        | Number (_, optOrigin)
        | String (_, optOrigin)
        | Quote (_, optOrigin)
        | Symbols (_, optOrigin) -> optOrigin

    let isExplicit (str : string) =
        str.StartsWith OpenStringStr && str.EndsWith CloseStringStr
    
    let shouldBeExplicit (str : string) =
        Seq.exists (fun chr -> Char.IsWhiteSpace chr || Seq.contains chr StructureCharsNoStr) str

    let skipWhitespace = skipAnyOf WhitespaceChars
    let skipWhitespaces = skipMany skipWhitespace

    let openSymbols = skipChar OpenSymbolsChar
    let closeSymbols = skipChar CloseSymbolsChar
    let openString = skipChar OpenStringChar
    let closeString = skipChar CloseStringChar
    let openQuote = skipChar OpenQuoteChar
    let closeQuote = skipChar CloseQuoteChar

    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let readQuoteChars = many (noneOf [CloseQuoteChar])
    let readContentChars =
        many1
            ((attempt (openQuote >>. skipWhitespaces >>. readQuoteChars .>> closeQuote .>> skipWhitespaces)) <|>
             (attempt (openString >>. skipWhitespaces >>. readStringChars .>> closeString .>> skipWhitespaces)) <|>
             (readAtomChars .>> skipWhitespaces))

    let (readSymbol : Parser<Symbol, unit>, private refReadSymbol : Parser<Symbol, unit> ref) =
        createParserForwardedToRef ()

    let readAtom =
        parse {
            let! start = getPosition
            let! chars = readAtomChars
            let! stop = getPosition
            do! skipWhitespaces
            let str = chars |> String.implode |> (fun str -> str.TrimEnd ()) |> expand
            let origin = Some { Start = start; Stop = stop }
            return Atom (str, origin) }

    let readNumber =
        parse {
            let! start = getPosition
            let! number = numberLiteral NumberFormat "number"
            let! stop = getPosition
            do! skipWhitespaces
            let origin = Some { Start = start; Stop = stop }
            return Number (number.String, origin) }

    let readString =
        parse {
            let! start = getPosition
            do! openString
            do! skipWhitespaces
            let! escaped = readStringChars
            do! closeString
            let! stop = getPosition
            do! skipWhitespaces
            let str = escaped |> String.implode
            let origin = Some { Start = start; Stop = stop }
            return String (str, origin) }

    let readQuote =
        parse {
            let! start = getPosition
            do! openQuote
            do! skipWhitespaces
            let! quoteChars = readQuoteChars
            do! closeQuote
            let! stop = getPosition
            do! skipWhitespaces
            let str = quoteChars |> String.implode
            let origin = Some { Start = start; Stop = stop }
            return Quote (str, origin) }

    let readSymbols =
        parse {
            let! start = getPosition
            do! openSymbols
            do! skipWhitespaces
            let! symbols = many readSymbol
            do! closeSymbols
            let! stop = getPosition
            do! skipWhitespaces
            let origin = Some { Start = start; Stop = stop }
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
            if Seq.isEmpty str then OpenStringStr + CloseStringStr
            elif not (isExplicit str) && shouldBeExplicit str then OpenStringStr + str + CloseStringStr
            elif isExplicit str && not (shouldBeExplicit str) then str.Substring (1, str.Length - 2)
            else unexpand str
        | Number (str, _) -> str
        | String (str, _) -> OpenStringStr + str + CloseStringStr
        | Quote (str, _) -> OpenQuoteStr + str + CloseQuoteStr
        | Symbols (symbols, _) -> OpenSymbolsStr + String.Join (" ", List.map writeSymbol symbols) + CloseSymbolsStr

    /// Convert a string to a symbol, with the following parses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0
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
        match run (skipWhitespaces >>. readSymbol) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a symbol to a string, with the following unparses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0
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

    /// Cascade a symbol string into multiple lines with proper tabbing.
    let private cascade keywords str =
    
        let rec getCascadeDepth symbol depth =
            match symbol with
            | Atom (str, _) when List.isEmpty keywords || List.contains str keywords -> depth
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
    let prettyPrint (keywords : string) symbol =
        let keywordsSplit = keywords.Split [|' '|] |> List.ofArray
        let strCascaded = cascade keywordsSplit symbol
        let lines = strCascaded.Split ([|"\r\n"|], StringSplitOptions.None)
        let linesTrimmed = Array.map (fun (str : string) -> str.TrimEnd ()) lines
        let strPretty = String.Join ("\r\n", linesTrimmed)
        // HACK: adding this newline is a workaround for https://github.com/jacobslusser/ScintillaNET/issues/249
        let strPrettyScintilla = strPretty + "\r\n"
        strPrettyScintilla

type ConversionException (message : string, optSymbol : Symbol option) =
    inherit Exception (message)
    member this.OptSymbol = optSymbol
    override this.ToString () =
        message + "\r\n" +
        (match optSymbol with Some symbol -> Origin.tryPrint (Symbol.tryGetOrigin symbol) + "\r\n" | _ -> "") +
        base.ToString ()

[<AutoOpen>]
module ConversionExceptionOperators =
    let failconv message symbol =
        raise ^ ConversionException (message, symbol)