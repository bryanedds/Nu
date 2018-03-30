// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime

/// Haskell-style Either type.
type Either<'l, 'r> =
    | Right of 'r
    | Left of 'l

/// Builds an either monad.
type EitherBuilder () =
    member inline this.Return a = Right a
    member inline this.ReturnFrom a = a
    member inline this.Bind (a, f) = match a with Right r -> f r | Left l -> Left l
    member this.Using (d, b) = use u = d in b u
    member this.TryWith (b, h) = try b () with exn -> h exn
    member this.TryFinally (b, h) = try b () finally h ()
    member this.Delay f = f ()
    member this.Run f = f ()
    member this.Zero () = Right ()
    member this.Yield a = Right a
    member this.YieldFrom e = e
    member this.Combine (a, b) = this.Bind (a, b)

    member this.While (g, b) =
        if g ()
        then match b () with Right () -> this.While (g, b) | error -> error
        else this.Zero ()

    member this.For (sequence : _ seq, body) =
        use enr = sequence.GetEnumerator ()
        let mutable errorOpt = None
        while enr.MoveNext () && Option.isNone errorOpt do
            match body enr.Current with
            | Right () -> ()
            | left -> errorOpt <- Some left
        match errorOpt with
        | Some error -> error
        | None -> this.Zero ()

[<AutoOpen>]
module EitherBuilderModule =

    /// Builds the either monad.
    let either = EitherBuilder ()

[<RequireQualifiedAccess>]
module Either =

    /// Monadic return for Either.
    let inline returnM a = either.Return a

    /// Monadic 'return from' for Either.
    let inline returnFrom a = either.ReturnFrom a

    /// Monadic bind for Either.
    let inline bind a f = either.Bind (a, f)

    /// Query whether an Either value is a Left value.
    let isLeft eir =
        match eir with
        | Right _ -> false
        | Left _ -> true
    
    /// Query whether an Either value is a Right value.
    let isRight eir =
        match eir with
        | Right _ -> true
        | Left _ -> false

    /// Get the Left value of an Either value, failing if not available.
    let getLeftValue eir =
        match eir with
        | Right _ -> failwith "Could not get Left value from a Right value."
        | Left l -> l

    /// Get the Right value of an Either value, failing if not available.
    let getRightValue eir =
        match eir with
        | Right r -> r
        | Left _ -> failwith "Could not get Right value from a Left value."

    /// Get only the Left values of a sequence of an Either value.
    let getLeftValues eirs =
        List.foldBack
            (fun eir lefts -> match eir with Right _ -> lefts | Left left -> left :: lefts)
            (List.ofSeq eirs)

    /// Get only the Right values of a sequence of an Either value.
    let getRightValues eirs =
        List.foldBack
            (fun eir rights -> match eir with Right right -> right :: rights | Left _ -> rights)
            (List.ofSeq eirs)

    /// Map over the left side of an Either value.
    let mapLeft mapper eir =
        match eir with
        | Right r -> Right r
        | Left l -> Left (mapper l)

    /// Map over the right side of an Either value.
    let mapRight mapper eir =
        match eir with
        | Right r -> Right (mapper r)
        | Left l -> Left l

    /// Split a sequences of Either values into a pair of left and right value lists.
    let split eirs =
        List.foldBack
            (fun eir (ls, rs) ->
                match eir with
                | Right r -> (ls, r :: rs)
                | Left l -> (l :: ls, rs))
            (List.ofSeq eirs)
            ([], [])

    /// Pick whichever of the eir values exists so long as they are the same type.
    let amb (eir : Either<'a, 'a>) =
        match eir with
        | Right value -> value
        | Left value -> value

    /// Pick whichever of the eir values exists.
    let ambBy pickFst pickSnd (eir : Either<'a, 'b>) =
        match eir with
        | Right value -> pickFst value
        | Left value -> pickSnd value