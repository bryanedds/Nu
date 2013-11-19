[<AutoOpen>]
module Log
open System
open System.Diagnostics

let log issue =
    Trace.WriteLine issue

let trace issue =
    Trace.Fail issue
    log issue

let traceIf bl issue =
    if bl then trace issue

let debug issue =
    Debug.Fail issue
    log issue

let debugIf predicate issue =
#if DEBUG
    if predicate () then
        Debug.Fail issue
        log issue
#endif