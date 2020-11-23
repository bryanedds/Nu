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
          mutable Values_ : struct (bool * 'k * 'v) array
          mutable Current_ : int
          mutable Removed_ : single // single to elide unecessary conversions
          mutable Threshold_ : single }

    member this.Values =
        this.Values_

    member this.Threshold =
        this.Threshold_

[<RequireQualifiedAccess>]
module KeyedArray =

    let overflowing karr =
        karr.Current_ = karr.Values.Length

    let underflowing karr =
        karr.Removed_ / single karr.Keys_.Count > karr.Threshold_

    let expand karr =

        // check overflow
        if overflowing karr then

            // grow
            let values = Array.zeroCreate (karr.Current_ * int (1.0f / karr.Threshold_))
            Array.blit karr.Values_ 0 values 0 karr.Current_
            karr.Values_ <- values

    let compact karr =
        
        // check underflow
        if underflowing karr then

            // reorg
            let mutable current = 0
            for kvp in karr.Keys_ do
                let key = kvp.Key
                let index = karr.Indices_.[key]
                let value = karr.Values_.[index]
                karr.Keys_.[current] <- key
                karr.Indices_.[key] <- current
                karr.Values_.[current] <- value
                current <- inc current

            // shrink
            let values = Array.zeroCreate (karr.Current_ * int karr.Threshold_)
            Array.blit karr.Values_ 0 values 0 values.Length
            karr.Values_ <- values

            // clean up
            karr.Removed_ <- 0.0f

    let add key value karr =
        match karr.Indices_.TryGetValue key with
        | (false, _) ->
            expand karr
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
            compact karr
        | (false, _) -> ()

    let make<'k, 'v when 'k : comparison> threshold size =
        { Keys_ = SortedDictionary<int, 'k> ()
          Indices_ = SortedDictionary<'k, int> ()
          Values_ = Array.zeroCreate size
          Current_ = 0
          Removed_ = 0.0f
          Threshold_ = threshold }