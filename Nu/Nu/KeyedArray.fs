// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System.Collections.Generic

/// A garbage-collected keyed array.
/// TODO: once this is well-tested, let's consider moving into Prime.
type [<NoEquality; NoComparison>] KeyedArray<'k, 'v when 'k : comparison> =
    private
        { Keys_ : SortedDictionary<int, 'k>
          Indices_ : SortedDictionary<'k, int>
          Values_ : struct (bool * 'k * 'v) array
          mutable Current_ : int
          mutable Removed_ : single // single to elide unecessary conversions
          Threshold_ : single }

    member this.Values =
        this.Values_

[<RequireQualifiedAccess>]
module KeyedArray =

    let compact karr =
        let mutable current = 0
        for kvp in karr.Keys_ do
            let key = kvp.Key
            let index = karr.Indices_.[key]
            let value = karr.Values_.[index]
            karr.Keys_.[current] <- key
            karr.Indices_.[key] <- current
            karr.Values_.[current] <- value
            current <- inc current

    let add key value karr =
        match karr.Indices_.TryGetValue key with
        | (false, _) ->
            if karr.Current_ = karr.Values.Length then failwith "KeyedArray capacity exceeded."
            let index = karr.Current_
            karr.Keys_.Add (index, key) |> ignore
            karr.Indices_.Add (key, index)
            karr.Values_.[index] <- struct (true, key, value)
            karr.Current_ <- inc karr.Current_
        | (true, index) ->
            karr.Values_.[index] <- struct (true, key, value)

    let remove key karr =
        match karr.Indices_.TryGetValue key with
        | (true, index) ->
            karr.Keys_.Remove index |> ignore
            karr.Indices_.Remove key |> ignore
            karr.Values_.[index] <- (false, key, Unchecked.defaultof<'v>)
            karr.Removed_ <- inc karr.Removed_
            if karr.Removed_ / single karr.Keys_.Count > karr.Threshold_
            then compact karr
            else ()
        | (false, _) -> ()

    let make<'k, 'v when 'k : comparison> threshold maxSize =
        { Keys_ = SortedDictionary<int, 'k> ()
          Indices_ = SortedDictionary<'k, int> ()
          Values_ = Array.zeroCreate maxSize
          Current_ = 0
          Removed_ = 0.0f
          Threshold_ = threshold }