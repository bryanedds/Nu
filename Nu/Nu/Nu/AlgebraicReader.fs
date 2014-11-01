namespace Nu
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<RequireQualifiedAccess>]
module AlgebraicReader =

    (* Reads strings with the following parses -

        (* Simple Values *)
        
        0
        2, 2
        {0; 1}
        Some(1)
        Hello World
        CharacterAnimationFacing

        (* Complex Values *)
        [0, 1 | 2, 4]
        [AnimationData | 4 | 8] *)

    let [<Literal>] WhitespaceChars = " \t\n\r"
    let [<Literal>] SeparatorChar = '|'
    let [<Literal>] SeparatorStr = "|"
    let [<Literal>] OpenComplexValueChar = '['
    let [<Literal>] OpenComplexValueStr = "["
    let [<Literal>] CloseComplexValueChar = ']'
    let [<Literal>] CloseComplexValueStr = "]"
    let [<Literal>] StructureChars = "[]|"
    
    let skipWhitespaceChar = skipAnyOf WhitespaceChars
    let skipWhitespace = skipMany skipWhitespaceChar

    let charForm character = skipChar character >>. skipWhitespace
    let openComplexValueForm = charForm OpenComplexValueChar
    let closeComplexValueForm = charForm CloseComplexValueChar
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

    do readValueRef :=
        attempt readSimpleValue <|>
        readComplexValue
    
    let stringToValue str =
        match run (skipWhitespace >>. !readValueRef) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error