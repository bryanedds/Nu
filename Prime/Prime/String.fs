// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Text

[<RequireQualifiedAccess>]
module String =

    /// Convert a bool to a string that works well in code.
    let boolToCodeString (bool : bool) =
        if bool then "true" else "false"

    /// Convert a single to a string that works well in code.
    let singleToCodeString (num : single) =
        let decimaled = num.ToString ("N7")
        let trimmed = decimaled.TrimEnd('0')
        let zeroed = if trimmed.EndsWith "." then trimmed + "0" else trimmed
        zeroed + "f"

    /// Convert a double to a string that works well in code.
    let doubleToCodeString (num : double) =
        let decimaled = num.ToString ("N15")
        let trimmed = decimaled.TrimEnd('0')
        if trimmed.EndsWith "." then trimmed + "0" else trimmed

    /// Convert a number to a string that works well in code.
    let numberToCodeString (num : obj) =
        match num with
        | :? bool as bool -> boolToCodeString bool
        | :? char as char -> string char
        | :? int as int -> string int
        | :? int64 as int64 -> string int64
        | :? single as single -> singleToCodeString single
        | :? double as double -> doubleToCodeString double
        | _ -> failwithumf ()

    /// Converts a string into a list of characters.
    let explode (str : string) =
        let rec loop n acc =
            if n = 0 then acc
            else
                let n = n - 1
                loop n ^ str.[n] :: acc
        loop (String.length str) []
    
    /// Converts a list of characters into a string.
    let implode chars =
        let sb = StringBuilder ()
        List.iter (fun (chr : char) -> ignore (sb.Append chr)) chars
        string sb

    /// Capitalize a string.
    let capitalize (str : string) =
        match str.ToCharArray () |> List.ofArray with
        | [] -> str
        | [head] -> [|Char.ToUpperInvariant head|] |> String
        | head :: tail -> Char.ToUpperInvariant head :: tail |> Array.ofList |> String

    /// Textualize a string for usage as text.
    let textualize (str : string) =
        str.Replace('_', '\"')

    /// Get the string with the given ending.
    let withEnd str target =
        let length = String.length str
        let endLength = String.length target
        if endLength >= length then (false, String.Empty)
        else
            let beginLength = length - endLength
            let beginStr = str.Substring (0, beginLength)
            let endStr = str.Substring (beginLength, endLength)
            (endStr = target, beginStr)
    
    /// Convert a string to an array of characters.
    /// TODO: optimize this.
    let toArray str = Array.ofList ^ explode str
    
    /// Surround a string with another surrounding string.
    let surround (str : string) (surrounding : string) =
        surrounding + str + surrounding

    /// Contract escaped characters in a string.
    let unescape (str : string) =
        let unescaped =
            Seq.fold (fun (escaped, chars) y ->
                if escaped then
                    let chr = 
                        match y with
                        | '0' -> '\u0000'
                        | '\\' -> '\\'
                        | 'a' -> '\a'
                        | 'b' -> '\b'
                        | 'f' -> '\u000c'
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | 'v' -> '\v'
                        | c -> c
                    (false, chr :: chars)
                elif y = '\\' then (true, chars)
                else (false, y :: chars))
                (false, [])
                str 
        unescaped |> snd |> List.rev |> implode

    /// Expand escaped characters in a string.
    let escape (str : string) =
        // NOTE: doing escape character substitution in-place with a linked-list may prevent speed issues
        str
            .Replace("\\", "\\\\") // NOTE: this line must come first
            .Replace("\u0000", "\\0")
            .Replace("\a", "\\a")
            .Replace("\b", "\\b")
            .Replace("\f", "\\f")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")
            .Replace("\v", "\\v")