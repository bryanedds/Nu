// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<AutoOpen>]
module Sectioning =

    /// Sectioned unsigned long decrement.
    let inline zero () = LanguagePrimitives.GenericZero

    /// Sectioned unsigned long decrement.
    let inline one () = LanguagePrimitives.GenericOne

    /// Sectioned unsigned long decrement.
    let inline inc n = n + one ()

    /// Sectioned unsigned long decrement.
    let inline dec n = n - one ()

    /// Sectioned list cons.
    let inline cons head tail = head :: tail

    /// Sectioned addition.
    let inline add x y = x + y

    /// Sectioned subtraction.
    let inline sub x y = x - y

    /// Sectioned multiplication.
    let inline mul x y = x * y

    /// Sectioned division.
    let inline div x y = x / y