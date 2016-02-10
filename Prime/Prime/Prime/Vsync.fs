namespace Prime
open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Prime

/// The 'Vsync' (AKA, 'Variable Synchrony') monad.
/// NOTE: to reference how all this stuff works in F#, see here - https://msdn.microsoft.com/en-us/library/dd233182.aspx
/// TODO: forward documentation from FSharp.Core.
type [<ReferenceEquality>] 'a Vsync =
    private
        | Sync of (unit -> 'a)
        | Async of 'a Async

/// Async is missing a couple of functions, as we know...
module Async =

    /// The missing Async.Map function.
    let Map f a =
        async
            { let! b = a
              return f b }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vsync =

    let mutable private optSync =
        None

    let init sync =
        match optSync with
        | Some _ -> debug "Cannot init Vsync.sync once it's been set. Consider calling init earlier in your program."
        | None -> optSync <- Some sync

    let isSync () =
        match optSync with
        | Some sync -> sync
        | None ->
            debug "Sync not set manually before first invocation; automatically setting to true."
            let result = true
            optSync <- Some result
            result

    let [<DebuggerHidden; DebuggerStepThrough>] private Extract v =
        match v with
        | Sync _ -> failwithumf ()
        | Async a -> a

    let [<DebuggerHidden; DebuggerStepThrough>] Bind v f =
        match v with
        | Sync a -> f ^ a ()
        | Async a -> Async ^ async.Bind (a, f >> Extract)

    let [<DebuggerHidden; DebuggerStepThrough>] Return a =
        if isSync ()
        then Sync ^ fun () -> a
        else Async ^ async.Return a

    let [<DebuggerHidden; DebuggerStepThrough>] ReturnFrom v =
        match v with
        | Sync a -> Sync ^ fun () -> a ()
        | Async a -> Async ^ async.ReturnFrom a

    let [<DebuggerHidden; DebuggerStepThrough>] Zero () =
        if isSync ()
        then Sync ^ fun () -> ()
        else Async ^ async.Zero ()

    let [<DebuggerHidden; DebuggerStepThrough>] Combine a b =
        match b with
        | Sync b' -> Sync ^ fun () -> b' ()
        | Async b' -> Async ^ async.Combine (Extract a, b')

    let [<DebuggerHidden; DebuggerStepThrough>] Delay f =
        if isSync ()
        then Sync ^ fun () -> match f () with Sync a -> a () | _ -> failwithumf ()
        else Async ^ async.Delay (f >> Extract)

    let [<DebuggerHidden; DebuggerStepThrough>] For s f =
        if isSync ()
        then Sync ^ fun () -> Seq.iter (f >> ignore) s
        else Async ^ async.For (s, f >> Extract)

    let [<DebuggerHidden; DebuggerStepThrough>] While g v =
        match v with
        | Sync a -> Sync ^ fun () -> while g () do a ()
        | Async a -> Async ^ async.While (g, a)

    let [<DebuggerHidden; DebuggerStepThrough>] TryWith (v : 'a Vsync) (h : exn -> 'a Vsync) : 'a Vsync =
        match v with
        | Sync a -> Sync ^ fun () -> try a () with exn -> match h exn with Sync b -> b () | Async _ -> failwithumf ()
        | Async a -> Async ^ async.TryWith (a, h >> Extract)

    let [<DebuggerHidden; DebuggerStepThrough>] TryFinally (v : 'a Vsync) (h : unit -> unit) : 'a Vsync =
        match v with
        | Sync a -> Sync ^ fun () -> try a () finally h ()
        | Async a -> Async ^ async.TryFinally (a, h)

    let [<DebuggerHidden; DebuggerStepThrough>] Using d f =
        if isSync ()
        then Sync ^ fun () -> use u = d in match f u with Sync b -> b () | Async _ -> failwithumf ()
        else Async ^ async.Using (d, f >> Extract)
        
    let [<DebuggerHidden; DebuggerStepThrough>] Ignore v =
        match v with
        | Sync a -> Sync ^ fun () -> ignore ^ a ()
        | Async a -> Async ^ Async.Ignore a

    let [<DebuggerHidden; DebuggerStepThrough>] Sleep (t : int) =
        if isSync ()
        then Sync ^ fun () -> Thread.Sleep t
        else Async ^ Async.Sleep t

    let [<DebuggerHidden; DebuggerStepThrough>] RunSynchronously v =
        match v with
        | Sync a -> a ()
        | Async a -> Async.RunSynchronously a

    let [<DebuggerHidden; DebuggerStepThrough>] Start v =
        match v with
        | Sync a -> a ()
        | Async a -> Async.Start a

    let [<DebuggerHidden; DebuggerStepThrough>] StartAsTask v =
        match v with
        | Sync a -> Task.Factory.StartNew a
        | Async a -> Async.StartAsTask a

    let [<DebuggerHidden; DebuggerStepThrough>] AwaitTaskT (t : _ Task) =
        if isSync ()
        then Sync ^ fun () -> t.Result
        else Async ^ Async.AwaitTask t

    let [<DebuggerHidden; DebuggerStepThrough>] AwaitTask (t : Task) =
        if isSync ()
        then Sync ^ fun () -> t.Wait ()
        else Async ^ Async.AwaitTask t

    let [<DebuggerHidden; DebuggerStepThrough>] Catch v =
        match v with
        | Sync a -> Sync ^ fun () -> try Choice1Of2 ^ a () with exn -> Choice2Of2 exn
        | Async a -> Async ^ Async.Catch a

    let [<DebuggerHidden; DebuggerStepThrough>] Parallel s =
        if isSync ()
        then Sync ^ fun () -> Array.ofSeq ^ Seq.map (function Sync a -> a () | Async _ -> failwithumf ()) s
        else Async ^ Async.Parallel ^ Seq.map Extract s

    let [<DebuggerHidden; DebuggerStepThrough>] Map f v =
        match v with
        | Sync a -> Sync ^ fun () -> f ^ a ()
        | Async a -> Async ^ Async.Map f a

    (* TODO: Re-enable this code once we find a good definition of Async.ParallelThrottled.
    let [<DebuggerHidden; DebuggerStepThrough>] ParallelThrottled i s =
        if isSync ()
        then Sync ^ fun () -> Array.ofSeq ^ Seq.map (function Sync a -> a () | Async _ -> failwithumf ()) s
        else Async ^ Async.ParallelThrottled i ^ Seq.map Extract s*)

    (* TODO: Re-enable this code once we find a good definition of Async.ParallelIgnore.
    let [<DebuggerHidden; DebuggerStepThrough>] ParallelIgnore i s =
        if isSync ()
        then Sync ^ fun () -> ignore ^ Array.ofSeq ^ Seq.map (function Sync a -> a () | Async _ -> failwithumf ()) s
        else Async ^ Async.ParallelIgnore i ^ Seq.map Extract s*)

/// The Vsync computation expression builder.
type [<Sealed>] VsyncBuilder () =

    member inline this.Bind (m, f) = Vsync.Bind m f
    member inline this.Return x = Vsync.Return x
    member inline this.ReturnFrom m = Vsync.ReturnFrom m
    member inline this.Zero () = Vsync.Zero ()
    member inline this.Combine (a, b) = Vsync.Combine a b
    member inline this.Delay f = Vsync.Delay f
    member inline this.For (m, f) = Vsync.For m f
    member inline this.While (g, b) = Vsync.While g b
    member inline this.TryWith (b, h) = Vsync.TryWith b h
    member inline this.TryFinally (b, c) = Vsync.TryFinally b c
    member inline this.Using (d, b) = Vsync.Using d b
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
    (* TODO: Re-enable this code once we find a good definition of Async.ParallelThrottled.
    static member inline ParallelThrottled i s = Vsync.ParallelThrottled i s*)
    (* TODO: Re-enable this code once we find a good definition of Async.ParallelIgnore.
    static member inline ParallelIgnore i s = Vsync.ParallelIgnore i s*)

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module VsyncBuilder =

    /// The VsyncBuilder instance.
    /// Used like: vsync { return 0 }
    let vsync = VsyncBuilder ()