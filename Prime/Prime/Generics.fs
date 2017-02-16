// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime

[<AutoOpen>]
module Generics =

    /// The generic zero value.
    let inline zero () = LanguagePrimitives.GenericZero

    /// The generic one value.
    let inline one () = LanguagePrimitives.GenericOne

    /// The generic monoidal empty value.
    let inline empty () = LanguagePrimitives.GenericZero

    /// The generic monoidal append operation.
    let inline append a b = a + b

    /// Generic (and sectioned) increment.
    let inline inc n = n + one ()

    /// Generic (and sectioned) decrement.
    let inline dec n = n + -(one ())

    /// Generic (and sectioned) addition.
    let inline add x y = x + y

    /// Generic (and sectioned) subtraction.
    let inline sub x y = x - y

    /// Generic (and sectioned) multiplication.
    let inline mul x y = x * y

    /// Generic (and sectioned) division.
    let inline div x y = x / y