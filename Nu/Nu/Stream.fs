// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Diagnostics
open Prime
open Prime.Stream
open global.Nu
module Stream =

    /// Take only one event from a stream per update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate (stream : Stream<'a, Game, World>) =
        stream |>
        trackEvent4
            (fun (a, current) _ world ->
                let previous = current
                let current = World.getUpdateCount world
                ((a, current), previous < current))
            id (Unchecked.defaultof<'a>, -1L) |>
        first

    /// Take events from a stream only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking stream =
        filterEvent (fun _ -> World.isTicking) stream

    /// Take events from a stream only when the simulant is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] isSimulantSelected simulant stream =
        filterEvent (fun _ -> World.isSimulantSelected simulant) stream

    /// Take events from a stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).+
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling stream =
        filterEvent (fun _ -> World.isSelectedScreenIdling) stream
    
    /// Take events from a stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning stream =
        filterEvent (fun _ -> World.isSelectedScreenTransitioning) stream

[<AutoOpen>]
module StreamOperators =

    // open related module
    open Stream

    /// Stream sequencing operator.
    let (---) = (|>)

    /// Make a stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( !-- ) (property : PropertyTag<'a, 'b, World>) = !-- property

    /// Propagate the event data of a stream to a value in the observing participant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] ( --> ) stream (property : PropertyTag<'a, 'b, World>) = stream --> property

    // Propagate a value from the given source participant to a value in the given destination participant, but with update-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] ( -/> ) stream property = noMoreThanOncePerUpdate stream --> property