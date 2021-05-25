// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

[<RequireQualifiedAccess>]
module Unique =

    /// A unique number allocator.
    type [<NoEquality; NoComparison>] Unique =
        { UniquesFree : int HashSet
          mutable UniqueCurrent : int }

    /// Allocate a unique number.
    let alloc unique =
        if unique.UniquesFree.Count = 0 then
            if unique.UniqueCurrent = Int32.MaxValue then failwith "No unique ints available - likely due to resource leak."
            let number = unique.UniqueCurrent
            unique.UniqueCurrent <- inc unique.UniqueCurrent
            number
        else
            let number = Seq.head unique.UniquesFree
            unique.UniquesFree.Remove number |> ignore
            number

    /// Free a unique number.
    let free number unique =
        unique.UniquesFree.Add number

    /// Make a unique number allocator.
    let make () =
        { UniquesFree = HashSet ()
          UniqueCurrent = System.Int32.MinValue }

/// A unique number allocator.
type Unique = Unique.Unique