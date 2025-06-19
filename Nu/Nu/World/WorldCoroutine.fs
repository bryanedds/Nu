// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Diagnostics
open Prime

/// The Coroutine monad context of the world.
type [<ReferenceEquality>] 'a Coroutine =
    Coroutine of (World -> Either<'a Coroutine, 'a>)

/// Implements the coroutine monad.
type CoroutineBuilder () =

    /// Functor map for the coroutine monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Map (f : 'a -> 'b) (Coroutine a : 'a Coroutine) : 'b Coroutine =
        Coroutine (fun world ->
            match a world with
            | Left next -> Left (this.Map f next)
            | Right value -> Right (f value))

    /// Applicative apply for the coroutine monad.
    /// TODO: implement correctly!
    //[<DebuggerHidden; DebuggerStepThrough>]
    //member this.Apply (Coroutine f) (Coroutine a) : 'b Coroutine =
    //    Coroutine (fun world ->
    //        match f world, a world with
    //        | (Left f', _) -> Left (apply f' a)
    //        | (_, Left a') -> Left (apply f a')
    //        | (Right f, Right a) -> Right (f a))

    /// Monadic return for the coroutine monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return (a : 'a) : 'a Coroutine =
        Coroutine (fun _ -> Right a)

    /// Monadic bind for the coroutine monad.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Bind (Coroutine a, f) =
        Coroutine (fun world ->
            match a world with
            | Left next -> Left (this.Bind (next, f))
            | Right value -> let (Coroutine next) = f value in next world)

[<AutoOpen>]
module CoroutineBuilder =

    /// Builds coroutines.
    let [<DebuggerHidden>] coroutine = CoroutineBuilder ()

[<RequireQualifiedAccess>]
module Coroutine =

    /// Functor map for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline map f a = coroutine.Map f a

    /// Functor map for the coroutine monad.
    /// TODO: uncomment when implemented correctly!
    //let [<DebuggerHidden; DebuggerStepThrough>] inline apply c a = coroutine.Apply c a

    /// Monadic return for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline returnM a = coroutine.Return a

    /// Monadic bind for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline bind c a = coroutine.Bind (c, a)

    /// Get the world.
    let [<DebuggerHidden>] get : World Coroutine =
        Coroutine (fun world -> Right world)

    /// 
    let step (Coroutine coroutine) world =
        match coroutine world with
        | Left next -> Left next
        | Right value -> Right value

    /// Run a coroutine to its end.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run (Coroutine coroutine) world =
        match coroutine world with
        | Left next -> run next world
        | Right value -> value

    /// Wait for the given duration until resuming.
    let [<DebuggerHidden; DebuggerStepThrough>] sleep duration (crt : 'a Coroutine) : 'a Coroutine =
        coroutine {
            let! world = get
            let id = World.registerDuration duration world
            World.subscribe
                (fun _ world ->
                    run crt world |> ignore<'a>
                    Cascade)
                (Events.TranspireEvent id)
                Game.Handle
                world
            return () }

    /// Wait to the next frame.
    let [<DebuggerHidden; DebuggerStepThrough>] pass expr =
        sleep GameTime.zero expr

[<AutoOpen>]
module WorldCoroutine =

    type World with

        /// Run a coroutine to its end.
        [<DebuggerHidden; DebuggerStepThrough>] 
        static member runCoroutine coroutine world =
            Coroutine.run coroutine world