﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System

/// Conveys an event's trace information.
type EventTrace = EventInfo list

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