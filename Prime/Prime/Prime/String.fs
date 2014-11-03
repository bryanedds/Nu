// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.ComponentModel
open System.Text

[<AutoOpen>]
module StringModule =

    /// Uses a type converter to convert source to a string.
    let tcstring (source : obj) =
        let converter = TypeDescriptor.GetConverter source
        converter.ConvertToString source

[<RequireQualifiedAccess>]
module String =
    
    /// Converts a string into a list of characters.
    let explode (str : string) =
        let rec loop n acc =
            if n = 0 then acc
            else
                let n = n - 1
                loop n <| str.[n] :: acc
        loop (String.length str) []
    
    /// Converts a list of characters into a string.
    let implode chars =
        let sb = StringBuilder ()
        List.iter (fun (chr : char) -> ignore (sb.Append chr)) chars
        sb.ToString ()
    
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
    let toArray str = Array.ofList <| explode str
    
    /// Surround a string with another surrounding string.
    let surround (str : string) (surrounding : string) =
        surrounding + str + surrounding