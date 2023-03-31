// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Stream =

    /// Take events from a stream only when World.getAdvancing evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] whenAdvancing stream =
        Stream.filterEvent (fun _ -> World.getAdvancing) stream

    /// Take events from a stream only when World.getHalted evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] whenHalted stream =
        Stream.filterEvent (fun _ -> World.getHalted) stream

    /// Take events from a stream only when the simulant is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelected simulant stream =
        Stream.filterEvent (fun _ -> World.isSelected simulant) stream

    /// Take events from a stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelectedScreenIdling stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenIdling) stream
    
    /// Take events from a stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelectedScreenTransitioning stream =
        Stream.filterEvent (fun _ -> World.isSelectedScreenTransitioning) stream