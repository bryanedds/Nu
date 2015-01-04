namespace Prime
open Prime

[<AutoOpen>]
module DesyncModule =

    /// The Desync monad. Allows the user to define in a sequential style an operation that spans
    /// across a bounded number of events. Span is bounded because I've yet to figure out how to
    /// make Desync implementation tail-recursive (see note about unbounded recursion in bind). And
    /// frankly, I'm not sure if there is a tail-recursive implementation of it...
    type [<NoComparison; NoEquality>] Desync<'e, 's, 'a> =
        Desync of ('s -> 's * Either<'e -> Desync<'e, 's, 'a>, 'a>)

    /// Monadic return for the Desync monad.
    let internal returnM (a : 'a) : Desync<'e, 's, 'a> =
        Desync (fun s -> (s, Right a))
        
    /// Monadic bind for the Desync monad.
    let rec internal bind (m : Desync<'e, 's, 'a>) (cont : 'a -> Desync<'e, 's, 'b>) : Desync<'e, 's, 'b> =
        Desync (fun s ->
            match (match m with Desync f -> f s) with
            //                              ^--- NOTE: unbounded recursion here
            | (s', Left m') -> (s', Left (fun e -> bind (m' e) cont))
            | (s', Right v) -> match cont v with Desync f -> f s')

    /// Builds the Desync monad.
    type DesyncBuilder () =
        member this.Return op = returnM op
        member this.Bind (m, cont) = bind m cont

    /// The Desync builder.
    let desync = DesyncBuilder ()

module Desync =

    /// Monadic return for the Desync monad.
    let returnM = returnM

    /// Monadic bind for the Desync monad.
    let bind = bind

    /// Get the state.
    let get : Desync<'e, 's, 's> =
        Desync (fun s -> (s, Right s))

    /// Get the state transformed by 'by'.
    let getBy by : Desync<'e, 's, 'a> =
        Desync (fun s -> (s, Right <| by s))

    /// Set the state.
    let set s : Desync<'e, 's, unit> =
        Desync (fun _ -> (s, Right ()))

    /// Update the state with an additional transformed state parameter.
    let updateBy by expr : Desync<'e, 's, unit> =
        Desync (fun s -> (expr (by s) s, Right ()))

    /// Update the state.
    let update expr : Desync<'e, 's, unit> =
        Desync (fun s -> (expr s, Right ()))

    /// Get the next event.
    let next : Desync<'e, 's, 'e> =
        Desync (fun s -> (s, Left returnM))

    /// Pass over the next event.
    let pass : Desync<'e, 's, unit> =
        Desync (fun s -> (s, Left (fun _ -> returnM ())))

    /// React to the next event.
    let reactE expr : Desync<'e, 's, unit> =
        desync {
            let! e = next
            let! s = get
            let s = expr e s
            do! set s }

    /// React to the happening of the next event.
    let react expr : Desync<'e, 's, unit> =
        desync {
            do! pass
            let! s = get
            let s = expr s
            do! set s }

    /// Loop in a desynchronous context while 'pred' evaluate to true.
    let rec loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> 's -> bool) (m : 'i -> Desync<'e, 's, unit>) =
        desync {
            let! s = get
            do! if pred i s then
                    desync {
                        do! m i
                        let i = next i
                        do! loop i next pred m }
                else returnM () }

    /// Loop in a desynchronous context while 'pred' evaluates to true.
    let during (pred : 's -> bool) (m : Desync<'e, 's, unit>) =
        loop () id (fun _ -> pred) (fun _ -> m)

    /// Step once into a desync.
    let step (m : Desync<'e, 's, 'a>) (s : 's) : 's * Either<'e -> Desync<'e, 's, 'a>, 'a> =
        match m with Desync f -> f s

    /// Advance a desync value by one step with the given event 'e'.
    let advanceDesync (m : 'e -> Desync<'e, 's, 'a>) (e : 'e) (s : 's) : 's * Either<'e -> Desync<'e, 's, 'a>, 'a> =
        step (m e) s
        
    /// Run a desync to its end, providing the event 'e' for all its steps.
    let rec runDesync (m : Desync<'e, 's, 'a>) (e : 'e) (s : 's) : ('s * 'a) =
        match step m s with
        | (s', Left m') -> runDesync (m' e) e s'
        | (s', Right v) -> (s', v)