// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.ComponentModel
open Prime

/// Haskell-style Either type.
/// TODO: more nice operators definitions.
type Either<'l, 'r> =
    | Right of 'r
    | Left of 'l

module Either =

    /// Monadic return for Either.
    let returnM r = Right r
    
    /// Monadic bind for Either.
    let bind either fn =
        match either with
        | Right r -> fn r
        | Left l -> Left l

    /// Builds an either monad.
    type EitherBuilder () =
        member this.Bind (either, fn) = bind either fn
        member this.Return r = returnM r

    /// The computation expression builder for Either.
    let either = EitherBuilder ()

    /// Query whether an Either value is a Left value.
    let isLeft either =
        match either with
        | Right _ -> false
        | Left _ -> true
    
    /// Query whether an Either value is a Right value.
    let isRight either =
        match either with
        | Right _ -> true
        | Left _ -> false

    /// Get the Left value of an Either value, failing if not available.
    let getLeftValue either =
        match either with
        | Right _ -> failwith "Could not get Left value from a Right value."
        | Left l -> l

    /// Get the Right value of an Either value, failing if not available.
    let getRightValue either =
        match either with
        | Right r -> r
        | Left _ -> failwith "Could not get Right value from a Left value."

    /// Get only the Left values of a sequence of an Either value.
    let getLeftValues eithers =
        List.foldBack
            (fun either lefts -> match either with Right _ -> lefts | Left left -> left :: lefts)
            (List.ofSeq eithers)

    /// Get only the Right values of a sequence of an Either value.
    let getRightValues eithers =
        List.foldBack
            (fun either rights -> match either with Right right -> right :: rights | Left _ -> rights)
            (List.ofSeq eithers)

    /// Map over the left side of an Either value.
    let mapLeft mapper either =
        match either with
        | Right r -> Right r
        | Left l -> Left ^ mapper l

    /// Map over the right side of an Either value.
    let mapRight mapper either =
        match either with
        | Right r -> Right ^ mapper r
        | Left l -> Left l

    /// Split a sequences of Either values into a pair of left and right value lists.
    let split eithers =
        List.foldBack
            (fun either (ls, rs) ->
                match either with
                | Right r -> (ls, r :: rs)
                | Left l -> (l :: ls, rs))
            (List.ofSeq eithers)
            ([], [])