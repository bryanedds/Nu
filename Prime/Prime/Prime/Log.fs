[<AutoOpen>]
module Log
open System
open System.Diagnostics

let note message =
    Trace.WriteLine message

let trace message =
    Trace.Fail message
    note message

let traceIf bl message =
    if bl then trace message

let debug message =
    Debug.Fail message
    note message

let debugIf predicate message =
#if DEBUG
    if predicate () then
        Debug.Fail message
        note message
#else
    ()
#endif