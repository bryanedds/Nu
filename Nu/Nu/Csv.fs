namespace Nu
open Prime
open FParsec

[<RequireQualifiedAccess>]
module Csv =

    // TODO: P1: Place this into Prime.Symbol.

    let readAtomFromCsv =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! chars = many1 (noneOf (Symbol.StructureChars + "\r\n,"))
            let! stop = getPosition
            let str = chars |> String.implode |> fun str -> str.TrimEnd ()
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Atom (str, originOpt) }

    let readNumberFromCsv =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! number = numberLiteral Symbol.NumberFormat "number"
            do! nextCharSatisfies (function '\r' | '\n' | ',' -> true | _ -> false) <|> eof
            let! stop = getPosition
            do! Symbol.skipWhitespaces
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            let suffix =
                (if number.SuffixChar1 <> (char)65535 then string number.SuffixChar1 else "") + 
                (if number.SuffixChar2 <> (char)65535 then string number.SuffixChar2 else "") + 
                (if number.SuffixChar3 <> (char)65535 then string number.SuffixChar3 else "") + 
                (if number.SuffixChar4 <> (char)65535 then string number.SuffixChar4 else "")
            return Number (number.String + suffix, originOpt) }
    
    let readStringFromCsv =
        parse {
            let! userState = getUserState
            let! start = getPosition
            do! Symbol.openString
            let! escaped = many (noneOf Symbol.CloseStringStr)
            do! Symbol.closeString
            let! stop = getPosition
            do! Symbol.skipWhitespaces
            let str = String.implode escaped
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return String (str, originOpt) }

    let readFieldFromCsv =
        attempt readStringFromCsv <|>
        attempt readNumberFromCsv <|>
        attempt readAtomFromCsv

    let readRowFromCsv =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! symbols = sepBy readFieldFromCsv (skipChar ',')
            let! stop = getPosition
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Symbols (symbols, originOpt) }

    let readRowsFromCsv =
        parse {
            let! userState = getUserState
            let! start = getPosition
            let! symbols = sepBy readRowFromCsv skipNewline
            let! stop = getPosition
            let originOpt = Some { Source = userState.SymbolSource; Start = start; Stop = stop }
            return Symbols (symbols, originOpt) }

    let readSymbolFromCsv csvStr filePathOpt =
        let symbolSource = { SymbolSource = { FileNameOpt = filePathOpt; Text = csvStr }}
        match runParserOnString readRowsFromCsv symbolSource "" csvStr with
        | Success (symbol, _, _) -> symbol
        | Failure (_, error, _) -> failwithf "Csv parse error at %s." (scstring error.Position)
