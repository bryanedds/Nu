// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.ComponentModel

[<AutoOpen>]
module EitherModule =

    /// Haskell-style Either type.
    /// TODO: more nice operators definitions.
    type [<TypeConverter (typeof<AlgebraicConverter>)>] Either<'l, 'r> =
        | Right of 'r
        | Left of 'l
        override this.ToString () =
            match this with
            | Right r -> "{Right; " + TypeDescriptor.ConvertToString r + "}"
            | Left l -> "{Left; " + TypeDescriptor.ConvertToString l + "}"

    /// Monadic bind.
    let inline (>>=) either fn =
        match either with
        | Right r -> Right <| fn r
        | Left _ -> either

    /// Bind that allows indication of failure.
    let inline (>>=?) either fn =
        match either with
        | Right r -> fn r
        | Left _ -> either

    /// Bind that allows handling of failure.
    let inline (>>=??) (either : Either<_, _>) fn : Either<_, _> = fn either

module Either =

    /// Monadic return.
    let inline returnM r = Right r

    /// Monadic returnFrom.
    /// TODO: ensure this is defined correctly!
    let inline returnFrom r = r

    /// Builds an either monad.
    type EitherBuilder () =
        member this.Bind (either, fn) = either >>= fn
        member this.Return r = returnM r
        member this.ReturnFrom r = returnFrom r

    /// The either monad.
    let either = EitherBuilder ()

    /// Queries if the either is a Left value.
    let isLeft either =
        match either with
        | Right _ -> false
        | Left _ -> true
    
    /// Queries if the either is a Right value.
    let isRight either =
        match either with
        | Right _ -> true
        | Left _ -> false

    /// Get the Left value of an either, failing if not available.
    let getLeftValue either =
        match either with
        | Right _ -> failwith "Could not get Left value from a Right value."
        | Left l -> l

    /// Get the Right value of an either, failing if not available.
    let getRightValue either =
        match either with
        | Right r -> r
        | Left _ -> failwith "Could not get Left value from a Right value."

    /// Get only the Left values of a sequence of either.
    let getLeftValues eithers =
        Seq.fold
            (fun lefts either -> match either with Right _ -> lefts | Left left -> left :: lefts)
            eithers

    /// Get only the Right values of a sequence of either.
    let getRightValues eithers =
        Seq.fold
            (fun rights either -> match either with Right _ -> rights | Left right -> right :: rights)
            eithers

    /// Map over the left side of an either value.
    let mapLeft mapper either =
        match either with
        | Right r -> Right r
        | Left l -> Left <| mapper l

    /// Map over the right side of an either value.
    let mapRight mapper either =
        match either with
        | Right r -> Right <| mapper r
        | Left l -> Left l

    /// Split a sequences of eithers into a pair of left and right value lists.
    let split eithers =
        Seq.fold
            (fun (ls, rs) either ->
                match either with
                | Right r -> (ls, r :: rs)
                | Left l -> (l :: ls, rs))
            ([], [])
            eithers