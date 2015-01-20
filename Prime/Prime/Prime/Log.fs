// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics

[<AutoOpen>]
module Log =

    /// Log a message with a Trace.WriteLine.
    let note message =
        Trace.WriteLine message

    /// Log a message with a Trace.Fail and call to note.
    let trace message =
        Trace.Fail message
        note message

    /// Conditional trace call where condition is eagerly evaluted.
    let traceIf bl message =
        if bl then trace message

    /// Log a message with Debug.Fail and call to note.
    let debug message =
#if DEBUG
        Debug.Fail message
        note message
#else
        let _ = message
        ()
#endif

    /// Conditional debug call where condition is lazily evaluated.
    let debugIf predicate message =
#if DEBUG
        if predicate () then
            Debug.Fail message
            note message
#else
        let _ = (predicate, message)
        ()
#endif