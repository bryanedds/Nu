// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Text

[<RequireQualifiedAccess>]
module String =
    
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

    /// Ensure a string is clean of any quotes.
    let clean (str : string) =
        str.Replace ("\"", "")

    /// Capitalize a string.
    let capitalize (str : string) =
        match str.ToCharArray () |> List.ofArray with
        | [] -> str
        | [head] -> [|Char.ToUpperInvariant head|] |> String
        | head :: tail -> Char.ToUpperInvariant head :: tail |> Array.ofList |> String

    /// Textualize a string for usage as text.
    let textualize (str : string) =
        (clean str).Replace ('_', '\"')

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