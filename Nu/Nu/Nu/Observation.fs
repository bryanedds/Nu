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

    /// Take events from an observation only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking _ world = World.isTicking world

    /// Take events from an observation only when the observer is selected in the world (see
    /// documentation for World.isAddressSelected for what this means (it's very useful!)).
    let [<DebuggerHidden; DebuggerStepThrough>] isObserverSelected evt world = World.isSimulantSelected evt.Subscriber world

    /// Take events from an observation only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an observation only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

    /// Take only one event from an observation per game update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate observation =
        observation |> organize (fun _ world -> World.getUpdateCount world) |> toFst |> choose

    /// Filter out simulant change events that do not relate to those returned by 'valueGetter'.
    let [<DebuggerHidden; DebuggerStepThrough>] simulantValue (valueGetter : World -> 'b) (observation : Observation<'a SimulantChangeData, 'o, World>) =
        filter (fun a world ->
            let oldValue = valueGetter a.Data.OldWorld
            let newValue = valueGetter world
            oldValue <> newValue)
            observation

[<AutoOpen>]
module ObservationModule =
    open Observation

    /// Pipe-right arrow that provides special precedence for observations.
    let (-|>) = (|>)

    /// Make an observation of the observer's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-- ) (simulant : 'a, valueGetter : World -> 'b) (observer : 'o) =
        let changeEventAddress = ftoa<'a SimulantChangeData> !!(typeof<'a>.Name + "/Change") ->>- simulant.ObjAddress
        observe changeEventAddress observer |> simulantValue valueGetter

    /// Make an observation of one of the observer's change events per frame.
    let [<DebuggerHidden; DebuggerStepThrough>] (/--) (simulant, valueGetter) observer =
        (simulant, valueGetter) *-- observer |> noMoreThanOncePerUpdate

    /// Propagate the event data of an observation to a value in the observing simulant when the
    /// observer exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] (-->) observation valueSetter =
        subscribe (fun a world ->
            let world =
                if World.containsSimulant a.Subscriber world
                then valueSetter a.Data world
                else world
            (Cascade, world))
            observation

    // Propagate a value from the given source simulant to a value in the given destination simulant.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-> ) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) *-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world

    // Propagate a value from the given source simulant to a value in the given destination simulant, but with frame-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] (/->) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) /-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world