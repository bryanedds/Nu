// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<AutoOpen>]
module EitherModule =

    /// Haskell-style Either type.
    /// TODO: more nice operators definitions.
    type Either<'a, 'b> =
        | Left of 'a
        | Right of 'b

    /// Monadic bind.
    let inline (>>=) either fn =
        match either with
        | Left _ -> either
        | Right value -> Right <| fn value

    /// Bind that allows indication of failure.
    let inline (>>=?) either fn =
        match either with
        | Left _ -> either
        | Right value -> fn value

    /// Bind that allows handling of failure.
    let inline (>>=??) (either : Either<_, _>) fn : Either<_, _> =
        fn either

module Either =

    /// Monadic return.
    let inline return' value =
        Right value

    /// Monadic returnFrom.
    /// TODO: ensure this is defined correctly!
    let inline returnFrom value =
        value

    type EitherBuilder () =
        member this.Bind (either, fn) = either >>= fn
        member this.Return value = return' value
        member this.ReturnFrom value = returnFrom value

    let either = EitherBuilder ()