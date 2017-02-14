// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Prime

/// Async is missing a couple of functions, as we know...
module Async =

    /// Creates an asynchronous operation that runs 'f' over computation of 'a'.
    let Map f a =
        async
            { let! b = a
              return f b }

[<AutoOpen>]
module VsyncModule =

    /// The 'Vsync' (AKA, 'Variable Synchrony') monad.
    /// Allows code to run in either an async or synchronous fashion to aid in debugging.
    /// NOTE: to reference how all this stuff works in F#, see here - https://msdn.microsoft.com/en-us/library/dd233182.aspx
    type [<ReferenceEquality>] 'a Vsync =
        private
            | Sync of (unit -> 'a)
            | Async of 'a Async

    [<RequireQualifiedAccess>]
    module Vsync =
    
        /// Configures whether to use synchronized processing.
        let mutable private SyncOpt = None
    
        /// Initialize Vsync to use synchronized or asynchronous processing.
        let init sync =
            match SyncOpt with
            | Some _ -> Log.debug "Cannot init Vsync.sync once it's been set. Consider calling init earlier in your program."
            | None -> SyncOpt <- Some sync
    
        /// Query whether Vsync is using synchronized or asynchronous processing.
        let isSync () =
            match SyncOpt with
            | Some sync -> sync
            | None ->
                Log.debug "Sync not set manually before first invocation; automatically setting to true."
                let result = true
                SyncOpt <- Some result
                result
    
        let [<DebuggerHidden; DebuggerStepThrough>] private Extract v =
            match v with
            | Sync _ -> failwithumf ()
            | Async a -> a
    
        /// Creates a potentially asynchronous operation that runs computation, and when computation results, runs binder resolution.
        let [<DebuggerHidden; DebuggerStepThrough>] Bind v f =
            match v with
            | Sync a -> f ^ a ()
            | Async a -> Async ^ async.Bind (a, f >> Extract)
    
        /// Creates a potentially asynchronous operation that returns the result 'a'.
        let [<DebuggerHidden; DebuggerStepThrough>] Return a =
            if isSync ()
            then Sync ^ fun () -> a
            else Async ^ async.Return a
    
        /// Delegates to input computation.
        let [<DebuggerHidden; DebuggerStepThrough>] ReturnFrom v =
            match v with
            | Sync a -> Sync ^ fun () -> a ()
            | Async a -> Async ^ async.ReturnFrom a
    
        /// Creates a potentially asynchronous computation that runs binder 'f' over resource 'd'.
        /// Dispose is executed as this computation yields its result or if the asynchronous computation raises or by cancellation.
        let [<DebuggerHidden; DebuggerStepThrough>] Using d f =
            if isSync ()
            then Sync ^ fun () -> use u = d in match f u with Sync b -> b () | Async _ -> failwithumf ()
            else Async ^ async.Using (d, f >> Extract)
    
        /// Creates a potentially asynchronous computation that runs generator 'f'.
        let [<DebuggerHidden; DebuggerStepThrough>] Delay f =
            if isSync ()
            then Sync ^ fun () -> match f () with Sync a -> a () | _ -> failwithumf ()
            else Async ^ async.Delay (f >> Extract)
    
        /// Creates a potentially asynchronous computation that just returns unit.
        let [<DebuggerHidden; DebuggerStepThrough>] Zero () =
            if isSync ()
            then Sync ^ fun () -> ()
            else Async ^ async.Zero ()
    
        /// Creates a potentially asynchronous computation that first runs computation 'a' and then computation 'b', returning the result of the latter.
        let [<DebuggerHidden; DebuggerStepThrough>] Combine a b =
            match b with
            | Sync b' -> Sync ^ fun () -> b' ()
            | Async b' -> Async ^ async.Combine (Extract a, b')
    
        /// Creates a potentially asynchronous computation that enumerates the sequence 's', and runs the body 'f' for each item.
        let [<DebuggerHidden; DebuggerStepThrough>] For s f =
            if isSync ()
            then Sync ^ fun () -> Seq.iter (f >> ignore) s
            else Async ^ async.For (s, f >> Extract)
    
        /// Creates a potentially asynchronous computation that runs computation until guard 'g' becomes false.
        let [<DebuggerHidden; DebuggerStepThrough>] While g v =
            match v with
            | Sync a -> Sync ^ fun () -> while g () do a ()
            | Async a -> Async ^ async.While (g, a)
    
        /// Creates a potentially asynchronous computation that runs computation and returns its result.
        /// If an exception happens, then handler 'h' is called and the resulting computation executes instead.
        let [<DebuggerHidden; DebuggerStepThrough>] TryWith (v : 'a Vsync) (h : exn -> 'a Vsync) : 'a Vsync =
            match v with
            | Sync a -> Sync ^ fun () -> try a () with exn -> match h exn with Sync b -> b () | Async _ -> failwithumf ()
            | Async a -> Async ^ async.TryWith (a, h >> Extract)
    
        /// Creates a potentially asynchronous computation that runs computation.
        /// The action compensation 'h' is executed after the computation completes regardless of the outcome.
        /// If the computation raises and exception itself, the original exception is discarded and the new exception becomes the overall result.
        let [<DebuggerHidden; DebuggerStepThrough>] TryFinally (v : 'a Vsync) (h : unit -> unit) : 'a Vsync =
            match v with
            | Sync a -> Sync ^ fun () -> try a () finally h ()
            | Async a -> Async ^ async.TryFinally (a, h)
    
        /// Creates a potentially asynchronous computation that runs the given computation and ignores its results.
        let [<DebuggerHidden; DebuggerStepThrough>] Ignore v =
            match v with
            | Sync a -> Sync ^ fun () -> a () |> ignore
            | Async a -> Async ^ Async.Ignore a
    
        /// Creates a potentially asynchronous computation that will sleep for the given time.
        /// The operation will not block operating system threads for the duration of the wait when running asynchronously.
        /// The operation will block operating system thread for the duration of the wait otherwise.
        let [<DebuggerHidden; DebuggerStepThrough>] Sleep (t : int) =
            if isSync ()
            then Sync ^ fun () -> Thread.Sleep t
            else Async ^ Async.Sleep t
    
        /// Runs the potentially asynchronous computation and awaits its result.
        let [<DebuggerHidden; DebuggerStepThrough>] RunSynchronously v =
            match v with
            | Sync a -> a ()
            | Async a -> Async.RunSynchronously a
    
        /// Starts the potentially asynchronous computation.
        /// Computation is run in the thread pool not awaiting its result when asynchronous.
        /// Computation is run in the current thread awaiting its result otherwise.
        let [<DebuggerHidden; DebuggerStepThrough>] Start v =
            match v with
            | Sync a -> a ()
            | Async a -> Async.Start a
    
        /// Executes a computation in the thread pool when asynchronous, in the same thread otherwise.
        let [<DebuggerHidden; DebuggerStepThrough>] StartAsTask v =
            match v with
            | Sync a -> Task.Factory.StartNew a
            | Async a -> Async.StartAsTask a
    
        /// Return a potentially asynchronous computation that will wait for the given task to complete and return its result.
        let [<DebuggerHidden; DebuggerStepThrough>] AwaitTaskT (t : _ Task) =
            if isSync ()
            then Sync ^ fun () -> t.Result
            else Async ^ Async.AwaitTask t
    
        /// Return a potentially asynchronous computation that will wait for the given task to complete and return its result.
        let [<DebuggerHidden; DebuggerStepThrough>] AwaitTask (t : Task) =
            if isSync ()
            then Sync ^ fun () -> t.Wait ()
            else Async ^ Async.AwaitTask t
    
        /// Creates a potentially asynchronous computation that executes computation.
        /// If this computation completes successfully, then return Choice1Of2 with the returned value.
        /// If this computation raises before completion, then return Choice2Of2 with the raised exception.
        let [<DebuggerHidden; DebuggerStepThrough>] Catch v =
            match v with
            | Sync a -> Sync ^ fun () -> try Choice1Of2 ^ a () with exn -> Choice2Of2 exn
            | Async a -> Async ^ Async.Catch a
    
        /// Creates a potentially asynchronous computation that executes all the given computations
        /// Initially queues each as work item using a fork/join pattern when asynchronous.
        /// Executes each work item sequentially on the same thread otherwise.
        let [<DebuggerHidden; DebuggerStepThrough>] Parallel s =
            if isSync ()
            then Sync ^ fun () -> Array.ofSeq ^ Seq.map (function Sync a -> a () | Async _ -> failwithumf ()) s
            else Async ^ Async.Parallel ^ Seq.map Extract s
    
        /// Creates a potentially asynchronous operation that runs 'f' over computation of 'a'.
        let [<DebuggerHidden; DebuggerStepThrough>] Map f v =
            match v with
            | Sync a -> Sync ^ fun () -> f ^ a ()
            | Async a -> Async ^ Async.Map f a

/// The Vsync computation expression builder.
type [<Sealed>] VsyncBuilder () =

    member inline this.Bind (m, f) = Vsync.Bind m f
    member inline this.Return a = Vsync.Return a
    member inline this.ReturnFrom m = Vsync.ReturnFrom m
    member inline this.Using (d, b) = Vsync.Using d b
    member inline this.Delay f = Vsync.Delay f
    member inline this.Zero () = Vsync.Zero ()
    member inline this.Combine (a, b) = Vsync.Combine a b
    member inline this.For (m, f) = Vsync.For m f
    member inline this.While (g, b) = Vsync.While g b
    member inline this.TryWith (b, h) = Vsync.TryWith b h
    member inline this.TryFinally (b, c) = Vsync.TryFinally b c
    static member inline Ignore v = Vsync.Ignore v
    static member inline Sleep t = Vsync.Sleep t
    static member inline RunSynchronously v = Vsync.RunSynchronously v
    static member inline Start v = Vsync.Start v
    static member inline StartAsTask v = Vsync.StartAsTask v
    static member inline AwaitTaskT (t : _ Task) = Vsync.AwaitTaskT t
    static member inline AwaitTask (t : Task) = Vsync.AwaitTask t
    static member inline Catch v = Vsync.Catch v
    static member inline Parallel s = Vsync.Parallel s
    static member inline Map f v = Vsync.Map f v

[<AutoOpen>]
module VsyncBuilderModule =

    /// The VsyncBuilder instance.
    /// Used like: vsync { return 0 }
    let vsync = VsyncBuilder ()

/// The 'Vsync' (AKA, 'Variable Synchrony') monad.
/// Allows code to run in either an async or synchronous fashion to aid in debugging.
/// NOTE: to reference how all this stuff works in F#, see here - https://msdn.microsoft.com/en-us/library/dd233182.aspx
type 'a Vsync = 'a VsyncModule.Vsync