namespace Nu
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<RequireQualifiedAccess>]
module Reader =

    (* Reads strings with the following parses -

        (* Simple Values *)
        0
        2, 2
        Some(1)
        Hello World
        CharacterAnimationFacing

        (* Complex Values *)
        [0, 1; 2, 4]
        [Fst; Snd]

        (* Complex Unions *)
        {AnimationData; 4; 8}
    *)

    let [<Literal>] WhitespaceChars = " \t\n\r"
    let [<Literal>] SeparatorChar = ';'
    let [<Literal>] SeparatorStr = ";"
    let [<Literal>] OpenComplexValueChar = '['
    let [<Literal>] OpenComplexValueStr = "["
    let [<Literal>] CloseComplexValueChar = ']'
    let [<Literal>] CloseComplexValueStr = "]"
    let [<Literal>] OpenComplexUnionChar = '{'
    let [<Literal>] OpenComplexUnionStr = "{"
    let [<Literal>] CloseComplexUnionChar = '}'
    let [<Literal>] CloseComplexUnionStr = "}"
    let [<Literal>] StructureChars = ";{}[]"
    
    let skipWhitespaceChar = skipAnyOf WhitespaceChars
    let skipWhitespace = skipMany skipWhitespaceChar

    let charForm character = skipChar character >>. skipWhitespace
    let openComplexValueForm = charForm OpenComplexValueChar
    let closeComplexValueForm = charForm CloseComplexValueChar
    let openComplexUnionForm = charForm OpenComplexUnionChar
    let closeComplexUnionForm = charForm CloseComplexUnionChar
    let separatorForm = charForm SeparatorChar

    let (readValue : Parser<obj, unit>, readValueRef : Parser<obj, unit> ref) =
        createParserForwardedToRef ()

    let chainValues =
        parse {
            do! separatorForm
            do! skipWhitespace
            return fun left right -> left @ right }

    let readName =
        parse {
            let! chars = many1 <| noneOf (StructureChars + WhitespaceChars)
            do! skipWhitespace
            return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> objectify }

    let readSimpleValue =
        parse {
            let! chars = many1 <| noneOf StructureChars // includes whitespace, so no need to skip it explicitly
            return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> objectify }

    let readValues =
        parse {
            let! values = chainl1 (!readValueRef |>> List.singleton) chainValues
            return values }

    let readComplexValue =
        parse {
            do! openComplexValueForm 
            let! values = readValues
            do! closeComplexValueForm
            do! skipWhitespace
            return values :> obj }

    let readComplexUnionName =
        parse {
            let! unionName = readName
            do! separatorForm
            do! skipWhitespace
            return unionName }

    let readComplexUnion =
        parse {
            do! openComplexUnionForm
            let! unionName = readComplexUnionName
            let! values = readValues
            do! closeComplexUnionForm
            do! skipWhitespace
            return unionName :: values :> obj }

    do readValueRef :=
        parse {
            let! value =
                attempt readSimpleValue <|>
                attempt readComplexValue <|>
                readComplexUnion
            return value }
    
    let stringToValue str =
        match run (skipWhitespace >>. !readValueRef) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error