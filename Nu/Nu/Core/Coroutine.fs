// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Diagnostics
open Prime
open Nu

/// Describes the desired execution of a coroutine.
type CoroutineInstruction =
    | ToCancel
    | ToSleep of Duration : GameTime

/// A coroutine in Nu allows easy definition of static behavior over multiple frames.
type Coroutine =
    | Cancel
    | Complete
    | Sleep of Duration : GameTime * Continuation : (unit -> Coroutine)

    /// A coroutine instruction that cancels the entire tree.
    [<DebuggerHidden>]
    static member cancel : CoroutineInstruction = ToCancel

    /// A coroutine instruction that sleeps until the given game time.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member sleep (duration : GameTime) = ToSleep duration

    /// A coroutine instruction that sleeps until the next frame (the shortest possible time in DynamicFrameRate mode).
    [<DebuggerHidden>]
    static member pass : CoroutineInstruction =
        let gameTime =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate _ -> UpdateTime 1L
            | DynamicFrameRate frameRate -> TickTime (1.0 / double frameRate * double Stopwatch.Frequency |> int64)
        ToSleep gameTime

/// A coroutine with delayed execution until the computation is run.
type CoroutineDelayed = unit -> Coroutine

/// A computation expression builder for Coroutine.
/// Note that the "value" carried is simply unit as we are sequencing actions.
type 'w CoroutineBuilder (launcher : CoroutineDelayed -> unit) =
    
    /// Run i, then run the coroutine produced by f.
    member this.Bind (i : CoroutineInstruction, f : unit -> Coroutine) : Coroutine =
        match i with
        | ToCancel -> Cancel
        | ToSleep until -> Sleep (until, f)
    
    /// Delay evaluation until the computation is run.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Delay (f : unit -> Coroutine) : CoroutineDelayed = f
        
    /// Iterate over the sequence and combine coroutines.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.For (seq : seq<'t>, f : 't -> Coroutine) : Coroutine =
        Seq.fold (fun c t -> this.Combine (c, fun () -> f t)) (this.Zero ()) seq

    /// While the guard is true, run the coroutine produced by body.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.While (guard : unit -> bool, body : CoroutineDelayed) : Coroutine =
        if guard ()
        then this.Combine (body (), fun () -> this.While (guard, body))
        else this.Zero ()
    
    /// Sequence two coroutines.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Combine (m1 : Coroutine, m2 : CoroutineDelayed) : Coroutine =
        match m1 with
        | Cancel -> Cancel
        | Complete -> m2 ()
        | Sleep (until, continuation) ->
            Sleep (until, fun () -> this.Combine (continuation (), m2))

    /// Zero is just completion.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Zero () : Coroutine = Complete

    /// Run the coroutine by launching it.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Run (coroutine : CoroutineDelayed) =
        launcher coroutine

/// CoroutineBuilder operators.
[<AutoOpen>]
module CoroutineBuilder =

    /// The coroutine builder.
    [<DebuggerHidden; DebuggerStepThrough>]
    let inline coroutine launcher = CoroutineBuilder launcher