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