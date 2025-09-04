// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Diagnostics
open Prime

/// The Chain monad. Allows the user to define a chain of operations over the world that
/// optionally spans across a bounded number of events.
type [<ReferenceEquality>] Chain<'e, 'a> =
    Chain of (unit -> Either<'e -> Chain<'e, 'a>, 'a>)

/// Implements the chain monad.
type ChainBuilder () =

    /// Functor map for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Map (f : 'a -> 'b) (a : Chain<'e, 'a>) : Chain<'e, 'b> =
        Chain (fun () ->                
            let chainMapper eir =
                match eir with
                | Left c -> Left (fun a -> this.Map f (c a))
                | Right a -> Right (f a)
            let eir = match a with Chain b -> b ()
            chainMapper eir)

    /// Applicative apply for the chain monad.
    /// TODO: Implement!
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Apply (c : Chain<'e, 'a -> 'b>) (_ : Chain<'e, 'a>) : Chain<'e, 'b> =
        Chain (fun () ->
            match (match c with Chain f -> f ()) with
            //                             ^--- NOTE: unbounded recursion here
            | _ -> failwithnie ())

    /// Monadic return for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return (a : 'a) : Chain<'e, 'a> =
        Chain (fun _ -> Right a)

    /// Monadic bind for the chain monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Bind (c : Chain<'e, 'a>, cont : 'a -> Chain<'e, 'b>) : Chain<'e, 'b> =
        Chain (fun () ->
            match (match c with Chain f -> f ()) with
            //                             ^--- NOTE: unbounded recursion here
            | Left c -> Left (fun e -> this.Bind (c e, cont))
            | Right v -> match cont v with Chain f -> f ())

/// ChainBuilder operators.
[<AutoOpen>]
module ChainBuilder =

    /// Builds chains.
    let [<DebuggerHidden>] chain = ChainBuilder ()

/// Chain operators.
module Chain =

    /// Functor map for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline map f a = chain.Map f a

    /// Functor map for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline apply c a = chain.Apply c a

    /// Monadic return for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline returnM a = chain.Return a

    /// Monadic bind for the chain monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline bind c a = chain.Bind (c, a)

    /// Get the next event.
    let [<DebuggerHidden>] next : Chain<'e, 'e> =
        Chain (fun _ -> Left returnM)

    /// Pass over the next event.
    let [<DebuggerHidden>] pass : Chain<'e, unit> =
        Chain (fun _ -> Left (fun _ -> returnM ()))

    /// React to the next event, using the event's data in the reaction.
    let [<DebuggerHidden; DebuggerStepThrough>] reactData<'a, 's when 's :> Simulant> expr : Chain<Event<'a, 's>, unit> =
       chain {
           let! e = next
           expr (e.Data) ()
           return () }

    /// React to the next event, using the event's value in the reaction.
    let [<DebuggerHidden; DebuggerStepThrough>] reactEvent expr : Chain<'e, unit> =
        chain {
            let! e = next
            expr e ()
            return () }

    /// React to the next event, discarding the event's value.
    let [<DebuggerHidden; DebuggerStepThrough>] react expr : Chain<'e, unit> =
        chain {
            do! pass
            expr ()
            return () }

    /// Loop in a chain context while 'pred' evaluate to true considering only the loop data.
    let rec [<DebuggerHidden; DebuggerStepThrough>] loop (i : 'i) (step : 'i -> 'i) (pred : 'i -> bool) (c : 'i -> Chain<'e, unit>) =
        chain {
            do! if pred i then
                    chain {
                        do! c i
                        let i = step i
                        do! loop i step pred c }
                else returnM () }

    /// Loop in a chain context while 'pred' evaluates to true considering only the world state.
    let [<DebuggerHidden; DebuggerStepThrough>] during (pred : unit -> bool) (c : Chain<'e, unit>) =
        loop () id pred (fun _ -> c)

    /// Step once into a chain.
    let [<DebuggerHidden; DebuggerStepThrough>] step (c : Chain<'e, 'a>) : Either<'e -> Chain<'e, 'a>, 'a> =
        match c with Chain f -> f ()

    /// Advance a chain value by one step, providing 'e'.
    let [<DebuggerHidden; DebuggerStepThrough>] advance (c : 'e -> Chain<'e, 'a>) (e : 'e) : Either<'e -> Chain<'e, 'a>, 'a> =
        step (c e)

/// Chain functions for the world.
[<AutoOpen>]
module WorldChain =

    type World with

        /// Run a chain to its end, providing 'e' for all its steps.
        [<DebuggerHidden; DebuggerStepThrough>] 
        static member chainConstant (c : Chain<'e, 'a>) (e : 'e) : 'a =
            match Chain.step c with
            | Left chain -> World.chainConstant (chain e) e
            | Right v -> v

        /// Run a chain to its end, providing unit for all its steps.
        [<DebuggerHidden; DebuggerStepThrough>]
        static member chainUnit (c : Chain<unit, 'a>) : 'a =
            World.chainConstant c ()
            
        /// Execute a chain over the given stream.
        [<DebuggerHidden; DebuggerStepThrough>]
        static member chainPlus (c : Chain<Event<'a, Simulant>, unit>) (stream : Stream<'a>) (handling : Handling) (world : World) =
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let stateId = Gen.id64
            let subscriptionId = Gen.id64
            World.addEventState stateId (fun (_ : Event<'a, Simulant>) -> c) world
            let (eventAddress, unsubscribe) = stream.Subscribe world
            let unsubscribe = fun world ->
                World.removeEventState stateId world
                unsubscribe world
                World.unsubscribe subscriptionId world
            let advance = fun evt world ->
                let chain = World.getEventState stateId world : Event<'a, Simulant> -> Chain<Event<'a, Simulant>, unit>
                let advanceResult = Chain.advance chain evt
                match advanceResult with
                | Right () -> unsubscribe world
                | Left chainNext -> World.addEventState stateId chainNext world
            let callback = fun evt world ->
                advance evt world
                handling
            advance Unchecked.defaultof<Event<'a, Simulant>> world
            World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> ignore
            unsubscribe

        /// Execute a chain over the given stream.
        /// Allows each chained operation to run without referencing its source event, and without specifying its event
        /// handling approach by assuming Cascade.
        [<DebuggerHidden; DebuggerStepThrough>]
        static member chain (c : Chain<Event<'a, Simulant>, unit>) (stream : Stream<'a>) world =
            World.chainPlus c stream Cascade world |> ignore