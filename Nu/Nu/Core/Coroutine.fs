// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Diagnostics
open Prime
open Nu

/// The result of stepping a coroutine.
type 'w CoroutineResult =
    | CoroutineCompleted
    | CoroutineCancelled
    | CoroutineProgressing of 'w Coroutine

/// A coroutine in Nu allows easy definition of static behavior over multiple frames.
and 'w Coroutine =
    | Cancel
    | Sleep of GameTime
    | Coroutine of ('w -> unit)
    | Coroutines of 'w Coroutine list

    /// A coroutine that cancels the entire tree.
    [<DebuggerHidden>]
    static member cancel : 'w Coroutine =
        Cancel

    /// A coroutine that sleeps until the given game time.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member sleep (gameTime : GameTime) : 'w Coroutine =
        Sleep gameTime

    /// A coroutine that sleeps for the length of one frame (the shortest possible one in DynamicFrameRate mode.
    [<DebuggerHidden>]
    static member pass : 'w Coroutine =
        let gameTime =
            match Constants.GameTime.DesiredFrameRate with
            | StaticFrameRate _ -> UpdateTime 1L
            | DynamicFrameRate frameRate -> TickTime (1.0 / double frameRate * double Stopwatch.Frequency |> int64)
        Sleep gameTime

    /// Step a coroutine.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member step (pred : 'w -> bool) (coroutine : 'w Coroutine) (gameTime : GameTime) (world : 'w) : 'w CoroutineResult =
        if pred world then
            match coroutine with
            | Cancel -> CoroutineCancelled
            | Sleep gameTime' -> if gameTime' >= gameTime then CoroutineProgressing coroutine else CoroutineCompleted
            | Coroutine action -> action world; CoroutineCompleted
            | Coroutines coroutines ->
                match coroutines with
                | [] -> CoroutineCompleted
                | head :: tail ->
                    match Coroutine.step pred head gameTime world with
                    | CoroutineProgressing head' -> CoroutineProgressing (Coroutines (head' :: tail))
                    | CoroutineCompleted -> Coroutine.step pred (Coroutines tail) gameTime world
                    | CoroutineCancelled -> CoroutineCancelled
        else CoroutineCancelled

    /// Prepare a coroutine for execution at the given starting game time.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member prepare (coroutine : 'w Coroutine) gameTime =
        match coroutine with
        | Cancel -> (gameTime, coroutine)
        | Sleep gameTime' -> let gameTime'' = gameTime + gameTime' in (gameTime'', Sleep gameTime'')
        | Coroutine _ -> (gameTime, coroutine)
        | Coroutines coroutines ->
            coroutines
            |> List.fold (fun (gameTime, coroutines) coroutine ->
                let (gameTime', coroutine') = Coroutine.prepare coroutine gameTime
                (gameTime', coroutine' :: coroutines))
                (gameTime, [])
            |> mapSnd (List.rev >> Coroutines)

/// A computation expression builder for Coroutine.
/// Note that the "value" carried is simply unit as we are sequencing actions.
type 'w CoroutineBuilder (launcher : 'w Coroutine -> unit) =

    /// A no-op action.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Return (()) : 'w Coroutine =
        Coroutine ignore
    
    /// Run c, then run the coroutine produced by f.
    member this.Bind (c : 'w Coroutine, f : unit -> 'w Coroutine) : 'w Coroutine =
        Coroutines [c; f ()]
    
    /// Run c, then run the coroutine produced by f.
    member this.Bind (c : 'w -> unit, f : unit -> 'w Coroutine) : 'w Coroutine =
        Coroutines [Coroutine c; f ()]
    
    /// Delay evaluation until the computation is run.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Delay (f : unit -> 'w Coroutine) : 'w Coroutine =
        f ()
        
    /// Iterate over the sequence and combine coroutines.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.For (seq : seq<'t>, f : 't -> 'w Coroutine) : 'w Coroutine =
        Seq.fold (fun c t -> this.Combine (c, f t)) (this.Zero ()) seq
    
    /// Sequence two coroutines.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Combine (m1 : 'w Coroutine, m2 : 'w Coroutine) : 'w Coroutine =
        Coroutines [m1; m2]
    
    /// Zero is just a no-op.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Zero () : 'w Coroutine =
        Coroutine ignore

    /// Run the coroutine by launching it.
    [<DebuggerHidden; DebuggerStepThrough>]
    member this.Run (coroutine : 'w Coroutine) =
        launcher coroutine

[<AutoOpen>]
module CoroutineBuilder =

    /// The coroutine builder.
    [<DebuggerHidden; DebuggerStepThrough>]
    let inline coroutine launcher = CoroutineBuilder launcher

    /// A coroutine that cancels the entire tree.
    [<DebuggerHidden; DebuggerStepThrough>]
    let inline cancel<'w> : 'w Coroutine = Coroutine.cancel

    /// A coroutine that sleeps until the next frame.
    [<DebuggerHidden; DebuggerStepThrough>]
    let inline sleep gameTime = Coroutine.sleep gameTime

    /// Sleep until the next frame (approximate in DynamicFrameRate mode).
    [<DebuggerHidden; DebuggerStepThrough>]
    let inline pass<'w> : 'w Coroutine = Coroutine.pass