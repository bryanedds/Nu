[<AutoOpen>]
module Either

/// Hsakell-style Either type.
/// TODO: more nice operators definitions.
type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

/// Monadic return.
let inline return' a =
    Right a

/// Monadic returnFrom.
/// TODO: ensure this is defined correctly!
let inline returnFrom a =
    a

/// Monadic bind.
let inline (>>=) c f =
    match c with
    | Left _ -> c
    | Right a -> Right <| f a

/// Bind that allows indication of failure.
let inline (>>=?) c f =
    match c with
    | Left _ -> c
    | Right a -> f a

/// Bind that allows handling of failure.
let inline (>>=??) (c : Either<_, _>) f : Either<_, _> =
    f c

type EitherBuilder () =
    member this.Bind (c, f) = c >>= f
    member this.Return a = return' a
    member this.ReturnFrom a = returnFrom a

let either = EitherBuilder ()