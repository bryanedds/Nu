// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module String
open System
open System.Text

/// Converts a string into a list of characters.
let explode (str : string) =
    let rec loop n acc =
        if n = 0 then acc
        else
            let n' = n - 1
            loop n' (str.[n'] :: acc)
    loop str.Length []

/// Converts a list of characters into a string.
let implode (chars : char list) =
    let sb = StringBuilder ()
    List.iter (fun (chr : char) -> ignore (sb.Append chr)) chars
    sb.ToString ()

/// Get the string with the given ending.
let withEnd (str : string) (target : string) =
    let length = str.Length
    let endLength = target.Length
    if endLength >= length then (false, String.Empty)
    else
        let beginLength = length - endLength
        let beginStr = str.Substring (0, beginLength)
        let endStr = str.Substring (beginLength, endLength)
        (endStr = target, beginStr)

/// Convert a string to an array of characters.
/// TODO: optimize this.
let toArray str = Array.ofList (explode str)

/// Surround a string with another surrounding string.
let surround (str : string) (surrounding : string) =
    surrounding + str + surrounding