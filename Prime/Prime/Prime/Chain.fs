// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open Prime

/// The Chain monad. Allows the user to define a chain of operations over the world that
/// optionally spans across a bounded number of events.
///
/// The following is a potentially tail-recursible representation as speculated by @tpetracek -
/// 'w -> ('w * Either<'e -> Chain<'e, 'a, 'w>, 'a> -> 'a) -> 'a
type [<NoComparison; NoEquality>] Chain<'e, 'a, 'w when 'w :> 'w EventWorld> =
    Chain of ('w -> 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a>)

/// Implements the chain monad.
type ChainBuilder () =

    /// Monadic return for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return (a : 'a) : Chain<'e, 'a, 'w> =
        Chain (fun s -> (s, Right a))

    /// Monadic bind for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Bind (m : Chain<'e, 'a, 'w>, cont : 'a -> Chain<'e, 'b, 'w>) : Chain<'e, 'b, 'w> =
        Chain (fun world ->

            // match m with Chain f -> 
            //    f world (function
            //        | world, Left m ->    ...
            //        | world, Right v ->    ...    ) 

            // Async<'T> = ('T -> unit) -> unit
            // Async<'T> = ('T -> unit) * (exn -> unit) -> unit
            // ???? Async<'T> = ('T * 'W -> 'W) -> unit

            match (match m with Chain f -> f world) with
            //                             ^--- NOTE: unbounded recursion here
            | (world, Left m) -> (world, Left (fun e -> this.Bind (m e, cont)))
            | (world, Right v) -> match cont v with Chain f -> f world)

[<AutoOpen>]
module ChainBuilderModule =

    /// Builds the chain monad.
    let chain = ChainBuilder ()

module Chain =

    /// Monadic return for the chain monad.
    let inline returnM a = chain.Return a

    /// Monadic bind for the chain monad.
    let inline bind m a = chain.Bind (m, a)

    /// Get the world.
    let get : Chain<'e, 'w, 'w> =
        Chain (fun world -> (world, Right world))

    /// Get the world transformed by 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] getBy by : Chain<'e, 'a, 'w> =
        Chain (fun world -> (world, Right ^ by world))

    /// Set the world.
    let [<DebuggerHidden; DebuggerStepThrough>] set world : Chain<'e, unit, 'w> =
        Chain (fun _ -> (world, Right ()))

    /// Update the world with an additional transformed world parameter.
    let [<DebuggerHidden; DebuggerStepThrough>] updateBy by expr : Chain<'e, unit, 'w> =
        Chain (fun world -> (expr (by world) world, Right ()))

    /// Update the world.
    let [<DebuggerHidden; DebuggerStepThrough>] update expr : Chain<'e, unit, 'w> =
        Chain (fun world -> (expr world, Right ()))

    /// Get the next event.
    let next : Chain<'e, 'e, 'w> =
        Chain (fun world -> (world, Left returnM))

    /// Pass over the next event.
    let pass : Chain<'e, unit, 'w> =
        Chain (fun world -> (world, Left (fun _ -> returnM ())))

    /// React to the next event, using the event's value in the reaction.
    let [<DebuggerHidden; DebuggerStepThrough>] reactE expr : Chain<'e, unit, 'w> =
        chain {
            let! e = next
            let! world = get
            let world = expr e world
            do! set world }

    /// React to the next event, discarding the event's value.
    let [<DebuggerHidden; DebuggerStepThrough>] react expr : Chain<'e, unit, 'w> =
        chain {
            do! pass
            let! world = get
            let world = expr world
            do! set world }

    /// Loop in a chain context while 'pred' evaluate to true.
    let rec [<DebuggerHidden; DebuggerStepThrough>] loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> 'w -> bool) (m : 'i -> Chain<'e, unit, 'w>) =
        chain {
            let! world = get
            do! if pred i world then
                    chain {
                        do! m i
                        let i = next i
                        do! loop i next pred m }
                else returnM () }

    /// Loop in a chain context while 'pred' evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] during (pred : 'w -> bool) (m : Chain<'e, unit, 'w>) =
        loop () id (fun _ -> pred) (fun _ -> m)

    /// Step once into a chain.
    let [<DebuggerHidden; DebuggerStepThrough>] step (m : Chain<'e, 'a, 'w>) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> =
        match m with Chain f -> f world

    /// Advance a chain value by one step, providing 'e'.
    let [<DebuggerHidden; DebuggerStepThrough>] advance (m : 'e -> Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : 'w * Either<'e -> Chain<'e, 'a, 'w>, 'a> =
        step (m e) world

    /// Run a chain to its end, providing 'e' for all its steps.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run3 (m : Chain<'e, 'a, 'w>) (e : 'e) (world : 'w) : ('w * 'a) =
        match step m world with
        | (world', Left m') -> run3 (m' e) e world'
        | (world', Right v) -> (world', v)

    /// Run a chain to its end, providing unit for all its steps.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run2 (m : Chain<unit, 'a, 'w>) (world : 'w) : ('w * 'a) =
        run3 m () world

    /// Run a chain to its end, providing unit for all its steps.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run (m : Chain<unit, 'a, 'w>) (world : 'w) : 'w =
        run3 m () world |> fst

    let private run4 handling (chain : Chain<Event<'a, 'o>, unit, 'w>) (observation : Observation<'a, 'o, 'w>) world =
        let stateKey = makeGuid ()
        let subscriptionKey = makeGuid ()
        let world = EventWorld.addEventState stateKey (fun (_ : Event<'a, 'o>) -> chain) world
        let (eventAddress, unsubscribe, world) = observation.Subscribe world
        let unsubscribe = fun world ->
            let world = EventWorld.removeEventState stateKey world
            let world = unsubscribe world
            EventWorld.unsubscribe subscriptionKey world
        let advance = fun evt world ->
            let chain = EventWorld.getEventState stateKey world : Event<'a, 'o> -> Chain<Event<'a, 'o>, unit, 'w>
            let (world, advanceResult) = advance chain evt world
            match advanceResult with
            | Right () -> unsubscribe world
            | Left chainNext -> EventWorld.addEventState stateKey chainNext world
        let subscription = fun evt world ->
            let world = advance evt world
            (handling, world)
        let world = advance Unchecked.defaultof<Event<'a, 'o>> world
        let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
        (unsubscribe, world)

    /// Run a chain over Nu's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runAssumingCascade chain (observation : Observation<'a, 'o, 'w>) world =
        run4 Cascade chain observation world

    /// Run a chain over Nu's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runAssumingResolve chain (observation : Observation<'a, 'o, 'w>) world =
        run4 Resolve chain observation world