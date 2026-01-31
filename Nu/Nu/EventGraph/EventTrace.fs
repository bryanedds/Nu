// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System

/// Conveys an event's trace information.
type EventTrace = EventInfo list

/// EventTrace functions.
[<RequireQualifiedAccess>]
module EventTrace =

    /// Record an event trace.
    let record moduleName functionName moreInfo eventTrace : EventTrace =
        EventInfo.make moduleName functionName moreInfo :: eventTrace

    /// Record event only in debug mode.
    let debug (moduleName : string) (functionName : string) (moreInfo : string) (eventTrace : EventTrace) =
#if DEBUG
        record moduleName functionName moreInfo eventTrace
#else
        ignore moduleName
        ignore functionName
        ignore moreInfo
        eventTrace
#endif

    /// The empty event trace.
    let empty : EventTrace = []