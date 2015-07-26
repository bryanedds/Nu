// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open FSharpx
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

/// The Chain monad. Allows the user to define a chain of operations over the world that
/// optionally spans across a bounded number of events.
type [<NoComparison; NoEquality>] Chain<'e, 'a> =
    Chain of (World -> World * Either<'e -> Chain<'e, 'a>, 'a>)

[<AutoOpen>]
module ChainBuilder =

    // World -> (World * CHoice<'E -> Chain<'E, 'A>, 'A> -> 'A) -> 'A

    /// Monadic return for the chain monad.
    let internal returnM (a : 'a) : Chain<'e, 'a> =
        Chain (fun s -> (s, Right a))
        
    /// Monadic bind for the chain monad.
    let rec internal bind (m : Chain<'e, 'a>) (cont : 'a -> Chain<'e, 'b>) : Chain<'e, 'b> =
        Chain (fun world ->
            // match m with Chain f -> 
            //    f world (function
            //        | world, Left m ->    ...
            //        | world, Right v ->    ...    ) 

            // Async<'T> = ('T -> unit) -> unit
            // Async<'T> = ('T -> unit) * (exn -> unit) -> unit
            // ???? Async<'T> = ('T * World -> World) -> unit

            //
            match (match m with Chain f -> f world) with
            //                             ^--- NOTE: unbounded recursion here
            | (world, Left m) -> (world, Left (fun e -> bind (m e) cont))
            | (world, Right v) -> match cont v with Chain f -> f world)

    /// Implements the chain monad.
    type ChainBuilder () =
        member this.Return op = returnM op
        member this.Bind (m, cont) = bind m cont

    /// Builds the chain monad.
    let chain = ChainBuilder ()

module Chain =

    /// Monadic return for the chain monad.
    let returnM = returnM

    /// Monadic bind for the chain monad.
    let bind = bind

    /// Get the world.
    let get : Chain<'e, World> =
        Chain (fun world -> (world, Right world))

    /// Get the world transformed by 'by'.
    let getBy by : Chain<'e, 'a> =
        Chain (fun world -> (world, Right <| by world))

    /// Set the world.
    let set world : Chain<'e, unit> =
        Chain (fun _ -> (world, Right ()))

    /// Update the world with an additional transformed world parameter.
    let updateBy by expr : Chain<'e, unit> =
        Chain (fun world -> (expr (by world) world, Right ()))

    /// Update the world.
    let update expr : Chain<'e, unit> =
        Chain (fun world -> (expr world, Right ()))

    /// Get the next event.
    let next : Chain<'e, 'e> =
        Chain (fun world -> (world, Left returnM))

    /// Pass over the next event.
    let pass : Chain<'e, unit> =
        Chain (fun world -> (world, Left (fun _ -> returnM ())))

    /// React to the next event, using the event's value in the reaction.
    let reactE expr : Chain<'e, unit> =
        chain {
            let! e = next
            let! world = get
            let world = expr e world
            do! set world }

    /// React to the next event, discarding the event's value.
    let react expr : Chain<'e, unit> =
        chain {
            do! pass
            let! world = get
            let world = expr world
            do! set world }

    /// Loop in a chain context while 'pred' evaluate to true.
    let rec loop (i : 'i) (next : 'i -> 'i) (pred : 'i -> World -> bool) (m : 'i -> Chain<'e, unit>) =
        chain {
            let! world = get
            do! if pred i world then
                    chain {
                        do! m i
                        let i = next i
                        do! loop i next pred m }
                else returnM () }

    /// Loop in a chain context while 'pred' evaluates to true.
    let during (pred : World -> bool) (m : Chain<'e, unit>) =
        loop () id (fun _ -> pred) (fun _ -> m)

    /// Step once into a chain.
    let step (m : Chain<'e, 'a>) (world : World) : World * Either<'e -> Chain<'e, 'a>, 'a> =
        match m with Chain f -> f world

    /// Advance a chain value by one step, providing 'e'.
    let advance (m : 'e -> Chain<'e, 'a>) (e : 'e) (world : World) : World * Either<'e -> Chain<'e, 'a>, 'a> =
        step (m e) world

    /// Run a chain to its end, providing 'e' for all its steps.
    let rec run3 (m : Chain<'e, 'a>) (e : 'e) (world : World) : (World * 'a) =
        match step m world with
        | (world', Left m') -> run3 (m' e) e world'
        | (world', Right v) -> (world', v)

    /// Run a chain to its end, providing unit for all its steps.
    let rec run2 (m : Chain<unit, 'a>) (world : World) : (World * 'a) =
        run3 m () world

    /// Run a chain to its end, providing unit for all its steps.
    let rec run (m : Chain<unit, 'a>) (world : World) : World =
        fst <| run3 m () world

    let private run4 eventHandling (chain : Chain<Event<'a, 'o>, unit>) (observation : Observation<'a, 'o>) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey (fun (_ : Event<'a, 'o>) -> chain) world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observation.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let advance = fun event world ->
            let chain = World.getCallbackState callbackKey world : Event<'a, 'o> -> Chain<Event<'a, 'o>, unit>
            let (world, advanceResult) = advance chain event world
            match advanceResult with
            | Right () -> unsubscribe world
            | Left chainNext -> World.addCallbackState callbackKey chainNext world
        let subscription = fun event world ->
            let world = advance event world
            (eventHandling, world)
        let world = advance Unchecked.defaultof<Event<'a, 'o>> world
        let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
        (unsubscribe, world)

    /// Run a chain over Nu's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runAssumingCascade chain (observation : Observation<'a, 'o>) world =
        run4 Cascade chain observation world

    /// Run a chain over Nu's event system.
    /// Allows each chainhronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runAssumingResolve chain (observation : Observation<'a, 'o>) world =
        run4 Resolve chain observation world