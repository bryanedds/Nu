// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open LanguagePrimitives
open Prime
open Prime.Observation
open Nu

/// Apparently we can have two modules with the same namespace and name so long as they are
/// compiled into different DLLs... This seems strange and potentially fragile...
module Observation =

    /// Take only one event from an observation per game update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate observation =
        observation |> organize (fun _ world -> World.getUpdateCount world) |> toFst |> choose

    /// Take events from an observation only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking _ world =
        World.isTicking world

    /// Take events from an observation only when the observer is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] isObserverSelected evt world =
        World.isSimulantSelected evt.Subscriber world

    /// Take events from an observation only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    // TODO: re-enable let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an observation only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    // TODO: re-enable let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

[<AutoOpen>]
module ObservationModule =
    open Observation

    /// Make an observation of one of the observer's change events per frame.
    let [<DebuggerHidden; DebuggerStepThrough>] (/--) (participant, valueGetter) observer =
        (participant, valueGetter) *-- observer |> noMoreThanOncePerUpdate

    // Propagate a value from the given source participant to a value in the given destination participant, but with frame-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] (/->) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) /-- destination --> fun _ world ->
            let sourceValue = valueGetter world
            valueSetter sourceValue world