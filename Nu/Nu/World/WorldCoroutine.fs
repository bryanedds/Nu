// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading.Tasks
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

    let mutable private SyncIn =
        Unchecked.defaultof<ConcurrentDictionary<uint64, TaskCompletionSource<unit>>>

    let copySyncIn (omap : KeyValuePair<uint64, TaskCompletionSource<unit>> List) =
        SyncIn <- ConcurrentDictionary<uint64, TaskCompletionSource<unit>> omap

    let copySyncOut () =
        let result = List SyncIn
        SyncIn <- Unchecked.defaultof<ConcurrentDictionary<uint64, TaskCompletionSource<unit>>>
        result

    /// Functor map for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline map f a = coroutine.Map f a

    /// Monadic return for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline returnM a = coroutine.Return a

    /// Monadic bind for the coroutine monad.
    let [<DebuggerHidden; DebuggerStepThrough>] inline bind c a = coroutine.Bind (c, a)

    /// Get the world.
    let [<DebuggerHidden>] get : World Coroutine =
        Coroutine (fun world -> Right world)

    /// Run a coroutine to its end.
    let rec [<DebuggerHidden; DebuggerStepThrough>] run<'a> (Coroutine coroutine : 'a Coroutine) world =
        match coroutine world with
        | Left next -> run next world
        | Right value -> value

    /// Wait for the given duration until resuming.
    let [<DebuggerHidden; DebuggerStepThrough>] sleep duration : unit Coroutine =
        coroutine {
            let! world = get
            let id = World.scheduleDuration duration world
            let mutable published = false
            World.subscribePlus
                id
                (fun _ _ ->
                    published <- true
                    World.unsubscribe id world
                    Cascade)
                (Events.TranspireEvent id)
                Game.Handle
                world |> ignore
            let await () =
                let tcs = new TaskCompletionSource<unit> ()
                let added = SyncIn.TryAdd (id, tcs)
                if not added then failwith ("Coroutine sleep failed to add TaskCompletionSource for id " + string id + ".")
                tcs.Task.Wait ()
            let () =
                while not published do await ()
            return () }

    /// Run this once per frame.
    let update () =
        let syncInCopy = SyncIn.ToArray ()
        for entry in syncInCopy do
            match SyncIn.TryRemove entry.Key with
            | (true, tcs) -> tcs.SetResult ()
            | _ -> Log.error ("Coroutine updateAll failed to remove TaskCompletionSource for id " + string entry.Key + ".")

[<AutoOpen>]
module WorldCoroutine =

    type World with

        /// Run a coroutine to its end.
        [<DebuggerHidden; DebuggerStepThrough>] 
        static member runCoroutine (coroutine : unit Coroutine) world =
            Task.Run<unit> (fun () -> Coroutine.run<unit> coroutine world) |> ignore<Task<unit>>