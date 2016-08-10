// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Diagnostics
open LanguagePrimitives
open Prime
open Prime.Stream
open Nu

/// Apparently we can have two modules with the same namespace and name so long as they are
/// compiled into different DLLs... This seems strange and potentially fragile...
module Stream =

    /// Take only one event from an stream per update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate (stream : Stream<'a, 's, World>) =
        stream |>
        track4
            (fun (a, (_, current)) _ world ->
                let previous = current
                let current = World.getUpdateCount world
                ((a, (previous, current)), previous < current))
            id (Unchecked.defaultof<'a>, (0L, 0L)) |>
        first

    /// Take events from an stream only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking _ world =
        World.isTicking world

    /// Take events from an stream only when the subscriber is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] isObserverSelected evt world =
        World.isSimulantSelected evt.Subscriber world

    /// Take events from an stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    // TODO: re-enable let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    // TODO: re-enable let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

[<AutoOpen>]
module ObservationModule =

    // open related module
    open Stream

    /// Pipe-right arrow that provides special precedence for observations.
    let (-|>) = (|>)

    /// Make an stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-- ) (property : PropertyTag<'a, 'b, World>) (subscriber : 's) = property *-- subscriber

    /// Propagate the event data of an stream to a value in the observing participant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] ( --> ) stream (property : PropertyTag<'a, 'b, World>) = stream --> property

    // Propagate a value from the given source participant to a value in the given destination participant.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-> )
        (sourceProperty : PropertyTag<'a, 'b, World>)
        (destinationProperty : PropertyTag<'s, 'b, World>) =
        sourceProperty *-> destinationProperty

    /// Make an stream of one of the subscriber's change events per frame.
    let [<DebuggerHidden; DebuggerStepThrough>] ( /-- ) property subscriber =
        property *-- subscriber |> noMoreThanOncePerUpdate

    // Propagate a value from the given source participant to a value in the given destination participant, but with frame-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] ( /-> )
        (sourceProperty : PropertyTag<'a, 'b, World>)
        (destinationProperty : PropertyTag<'s, 'b, World>) =
        sourceProperty /-- destinationProperty.This -|> map (fun _ world -> sourceProperty.Get world) --> destinationProperty