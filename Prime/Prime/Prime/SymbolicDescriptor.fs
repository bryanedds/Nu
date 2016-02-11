// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

[<RequireQualifiedAccess>]
module SymbolicDescriptor =
    
    /// Query that a value of the source type can be converted to the destination type.
    let canConvertTo sourceType destType =
        (SymbolicConverter sourceType).CanConvertTo destType
    
    /// Query that a value of the source type can be converted to a string.
    let canConvertToString sourceType =
        (SymbolicConverter sourceType).CanConvertTo typeof<string>

    /// Query that a value of the destination type can be converted from the source type.
    let canConvertFrom sourceType destType =
        (SymbolicConverter destType).CanConvertFrom sourceType

    /// Query that a value of the destination type can be converted from a string.
    let canConvertFromString sourceType =
        (SymbolicConverter sourceType).CanConvertFrom typeof<string>

    /// Convert a value to the given type using its assigned type converter.
    let convertTo (source : obj, destType) =
        (SymbolicConverter (source.GetType ())).ConvertTo (source, destType)

    /// Convert a value from given type using its assigned type converter.
    let convertFrom source destType =
        (SymbolicConverter destType).ConvertFrom source

    /// Convert a value to a string using its assigned type converter.
    let convertToString source =
        convertTo (source, typeof<string>) :?> string

    /// Convert a value from a string using its assigned type converter.
    let convertFromString (str : string) destType =
        convertFrom str destType