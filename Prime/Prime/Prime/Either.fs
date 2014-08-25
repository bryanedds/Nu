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
        override this.ToString () =
            match this with
            | Left a -> "Left(" + string a + ")"
            | Right b -> "Right(" + string b + ")"

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
    let inline returnM value =
        Right value

    /// Monadic returnFrom.
    /// TODO: ensure this is defined correctly!
    let inline returnFrom value =
        value

    type EitherBuilder () =
        member this.Bind (either, fn) = either >>= fn
        member this.Return value = returnM value
        member this.ReturnFrom value = returnFrom value

    let either = EitherBuilder ()

    let isLeft either =
        match either with
        | Left _ -> true
        | Right _ -> false

    let isRight either =
        match either with
        | Right _ -> true
        | Left _ -> false

    let getLeftValue either =
        match either with
        | Left a -> a
        | Right _ -> failwith "Could not get Left value from a Right value."

    let getRightValue either =
        match either with
        | Left _ -> failwith "Could not get Left value from a Right value."
        | Right b -> b

    let getLefts eithers =
        Seq.filter isLeft eithers

    let getRights eithers =
        Seq.filter isRight eithers

    let getLeftValues eithers =
        let lefts = getLefts eithers
        Seq.map getLeftValue lefts

    let getRightValues eithers =
        let rights = getRights eithers
        Seq.map getRightValue rights

    let split eithers =
        Seq.fold
            (fun (ls, rs) either ->
                match either with
                | Left l -> (l :: ls, rs)
                | Right r -> (ls, r :: rs))
            ([], [])
            eithers