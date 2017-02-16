// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime

[<RequireQualifiedAccess>]
module SymbolicDescriptor =

    /// Check that a value of the source type can be converted to the destination type.
    let canConvertTo sourceType destType =
        (SymbolicConverter sourceType).CanConvertTo destType
    
    /// Check that a value of the destination type can be converted from the source type.
    let canConvertFrom sourceType destType =
        (SymbolicConverter destType).CanConvertFrom sourceType

    /// Convert a value to the given type using its assigned type converter.
    let convertTo (source : obj) sourceType destType =
        (SymbolicConverter sourceType).ConvertTo (source, destType)

    /// Convert a value from given type using its assigned type converter.
    let convertFrom source destType =
        (SymbolicConverter destType).ConvertFrom source