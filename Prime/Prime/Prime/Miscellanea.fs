// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System
open System.Diagnostics
open System.ComponentModel
open System.Reflection

[<AutoOpen>]
module Miscellanea =

    /// The tautology function.
    /// No matter what you pass it, it returns true.
    let inline tautology _ = true

    /// The absurdity function.
    /// No matter what you pass it, it returns false.
    let inline absurdity _ = false

    /// A generic identification code type.
    type Id = int64

    /// The invalid Id.
    let [<Literal>] InvalidId = 0L

    /// Perform a ToString operation on an object.
    let inline str obj =
        obj.ToString ()

    /// Perform a formatted ToString operation on an object.
    let inline strf (obj : IFormattable) format =
        obj.ToString (format, null)

    /// Perform a formatted ToString operation on an object.
    let inline strfp (obj : IFormattable) format formatProvider =
        obj.ToString (format, formatProvider)

    /// Apply a function recursively a number of times.
    let rec doTimes fn arg times =
        if times < 0 then failwith "Cannot call doTimes with times < 0."
        elif times = 0 then arg
        else doTimes fn (fn arg) (times - 1)

    /// Perform an operation until a predicate passes.
    let rec doUntil op pred =
        if not <| pred () then
            op ()
            doUntil op pred

    /// Creates a function that creates a unique number.
    /// TODO: place a mutex lock in this
    /// TODO: see if returned function can be optimized by minimizing dereferences
    let createGetNextId () =
        let nextId = ref InvalidId
        let getNextId =
            (fun () ->
                nextId := !nextId + 1L
                if !nextId = 0L then Debug.Fail "Id counter overflowed (flipped back to zero). Big trouble likely ahead!"
                !nextId)
        getNextId

    /// Add a custom TypeConverter to an existing type.
    let assignTypeConverter<'t, 'c> () =
        ignore <| TypeDescriptor.AddAttributes (typeof<'t>, TypeConverterAttribute typeof<'c>)

    /// Short-hand for linq enumerable cast.
    let enbCast =
        System.Linq.Enumerable.Cast

    /// Try to find a type by its name from all the loaded assemblies. Time-intensive.
    /// TODO: move into TypeExtension.fs.
    let tryFindType typeName =
        match Type.GetType typeName with
        | null ->
            let allAssemblies = AppDomain.CurrentDomain.GetAssemblies ()
            let types =
                Seq.choose
                    (fun (assembly : Assembly) -> match assembly.GetType typeName with null -> None | aType -> Some aType)
                    allAssemblies
            Seq.tryFind (fun _ -> true) types 
        | aType -> Some aType

    /// Find a type by its name from all the loaded assemblies. Time-intensive.
    /// TODO: move into TypeExtension.fs.
    let findType typeName =
        match tryFindType typeName with
        | None -> failwith <| "Could not find type with name '" + typeName + "'."
        | Some aType -> aType