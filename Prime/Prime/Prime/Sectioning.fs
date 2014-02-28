namespace Prime

[<AutoOpen>]
module Sectioning =

    /// Sectioned byte increment.
    let inline incUy (n : byte) = n + 1uy

    /// Sectioned byte decrement.
    let inline decUy (n : byte) = n - 1uy

    /// Sectioned signed byte increment.
    let inline incY (n : sbyte) = n + 1y

    /// Sectioned signed byte decrement.
    let inline decY (n : sbyte) = n - 1y

    /// Sectioned int increment.
    let inline incI (n : int) = n + 1

    /// Sectioned int decrement.
    let inline decI (n : int) = n - 1

    /// Sectioned unsigned int increment.
    let inline incU (n : uint32) = n + 1u

    /// Sectioned unsigned unt decrement.
    let inline decU (n : uint32) = n - 1u

    /// Sectioned long increment.
    let inline incL (n : int64) = n + 1L

    /// Sectioned long decrement.
    let inline decL (n : int64) = n - 1L

    /// Sectioned unsigned long increment.
    let inline incUl (n : uint64) = n + 1UL

    /// Sectioned unsigned long decrement.
    let inline decUl (n : uint64) = n - 1UL

    /// Sectioned list cons.
    let inline cons head tail = head :: tail

    /// Sectioned addition.
    let plus = (+)

    /// Sectioned subtraction.
    let minus = (-)

    /// Sectioned multiplication.
    let mul = ( * )

    /// Sectioned division.
    let div = (/)