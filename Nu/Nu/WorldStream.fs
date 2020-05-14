// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Diagnostics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Stream =

    /// Take only one event from a stream per update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate (stream : Stream<'a, World>) =
        World.noMoreThanOncePerUpdate stream

    /// Take events from a stream only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking stream =
        Stream.filterEvent (fun _ -> World.isTicking) stream

    /// Take events from a stream only when the simulant is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] isSelected simulant stream =
        Stream.filterEvent (fun _ -> World.isSelected simulant) stream

    /// Take events from a stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenIdling) stream
    
    /// Take events from a stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenTransitioning) stream

[<AutoOpen>]
module StreamOperators =

    /// Stream sequencing operator.
    let [<DebuggerHidden; DebuggerStepThrough>] (---) = (|>)

    /// Make a stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] (!--) (lens : Lens<'b, World>) = !-- lens

    /// Propagate the event data of a stream to a value in the observing simulant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] (-|>) stream (lens : Lens<'b, World>) = stream -|> lens

    // Propagate a value from the given source simulant to a value in the given destination
    // simulant, but with update-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] (-/>) stream lens = Stream.noMoreThanOncePerUpdate stream -|> lens