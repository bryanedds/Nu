[<AutoOpen>]
module Miscellanea
open System
open System.Diagnostics

/// A generic identification code type.
type Id = int64

/// The invalid Id.
let [<Literal>] InvalidId = 0L

/// Perform a ToString operation on an object.
let str obj =
    obj.ToString ()

/// Perform a formatted ToString operation on an object.
let strf (obj : IFormattable) format =
    obj.ToString (format, null)

/// Perform a formatted ToString operation on an object.
let strfp (obj : IFormattable) format formatProvider =
    obj.ToString (format, formatProvider)

/// Apply a function recursively n times.
let rec doTimes f x n =
    if n < 0 then failwith "Cannot call doTimes with n < 0."
    elif n = 0 then x
    else doTimes f (f x) (n - 1)

/// Perform an operation until a predicate passes.
let rec doUntil op pred =
    if not <| pred () then
        op ()
        doUntil op pred

// TODO: place a mutex lock in this
// TODO: see if returned function can be optimized by minimizing dereferences
let createGetNextId () =
    let nextId = ref InvalidId
    let getNextId =
        (fun () ->
            nextId := !nextId + 1L
            if !nextId = 0L then Debug.Fail "Id counter overflowed (flipped back to zero). Big trouble likely ahead!"
            !nextId)
    getNextId