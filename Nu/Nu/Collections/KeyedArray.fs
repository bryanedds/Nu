// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open Prime
open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module KeyedArray =

    /// A garbage-collected keyed array.
    /// TODO: once this is well-tested, let's consider moving into Prime.
    /// NOTE: not supported by SymbolicConverter.
    type [<ReferenceEquality>] KeyedArray<'k, 'v when 'k : equality> =
        private
            { Keys_ : SortedDictionary<int, 'k> // sorted so that compacting does not change order
              Indices_ : Dictionary<'k, int>
              mutable Values_ : struct (bool * 'k * 'v) array
              mutable Current_ : int
              mutable Removed_ : single // single to elide unecessary conversions
              mutable Threshold_ : single }

        interface struct (bool * 'k * 'v) IEnumerable with
            member this.GetEnumerator () = this.Values_.GetEnumerator ()
            member this.GetEnumerator () = (this.Values_ :> _ IEnumerable).GetEnumerator ()

        /// The keyed array values.
        member this.Values =
            this.Values_

        /// The threshold for compaction.
        member this.Threshold =
            this.Threshold_

        /// The current number of values (some of which may be empty).
        member this.Length =
            this.Current_

        /// Index a keyed value.
        member this.Item key =
            &this.Values_.[this.Indices_.[key]]

    let private overflowing karr =
        karr.Current_ = karr.Values.Length

    let private underflowing karr =
        karr.Removed_ / single karr.Keys_.Count >= 1.0f / karr.Threshold_

    let private expand karr =

        // check overflow
        if overflowing karr then

            // grow
            let values = Array.zeroCreate (max 1 karr.Current_ * int karr.Threshold_)
            Array.blit karr.Values_ 0 values 0 karr.Current_
            karr.Values_ <- values

    let private compact karr =
        
        // check underflow
        if underflowing karr then

            // reorg
            let mutable current = 0
            for key in Seq.toArray karr.Keys_.Values do
                let index = karr.Indices_.[key]
                let value = karr.Values_.[index]
                karr.Keys_.Remove index |> ignore
                karr.Keys_.Add (current, key)
                karr.Indices_.[key] <- current
                karr.Values_.[current] <- value
                current <- inc current

            // shrink
            let values = Array.zeroCreate (int (single karr.Current_ * (1.0f / karr.Threshold_)))
            Array.blit karr.Values_ 0 values 0 values.Length
            karr.Values_ <- values

            // clean up
            karr.Current_ <- current
            karr.Removed_ <- 0.0f

    /// Get the keyed array values.
    let getValues karr =
        karr.Values_

    /// Get the threshold for compaction.
    let getThreshold karr =
        karr.Threshold_
    
    /// Get the current number of values (some of which may be empty).
    let getLength karr =
        karr.Current_

    /// Add a keyed value, or update one if it already exists.
    let add key value karr =
        match karr.Indices_.TryGetValue key with
        | (false, _) ->
            expand karr
            let index = karr.Current_
            karr.Keys_.Add (index, key) |> ignore
            karr.Indices_.Add (key, index)
            karr.Values_.[index] <- struct (true, key, value)
            karr.Current_ <- inc karr.Current_
            index
        | (true, index) ->
            karr.Values_.[index] <- struct (true, key, value)
            index

    /// Remove a keyed value if it exists.
    let remove key karr =
        match karr.Indices_.TryGetValue key with
        | (true, index) ->
            karr.Keys_.Remove index |> ignore
            karr.Indices_.Remove key |> ignore
            karr.Values_.[index] <- struct (false, key, Unchecked.defaultof<'v>)
            karr.Removed_ <- inc karr.Removed_
            compact karr
        | (false, _) -> ()

    /// Query that a keyed value exists.
    let containsKey key karr =
        karr.Indices_.ContainsKey key

    /// Attempt to find a keyed value.
    let tryFind key karr =
        match karr.Indices_.TryGetValue key with
        | (true, index) -> Some karr.Values_.[index]
        | (false, _) -> None

    /// Find a keyed value.
    let find key (karr : KeyedArray<_, _>) =
        &karr.[key]

    /// Make a KeyedArray with the given compaction threshold and initial capaticy.
    let make<'k, 'v when 'k : equality> threshold capacity : KeyedArray<'k, 'v> =
        { Keys_ = SortedDictionary<int, 'k> ()
          Indices_ = Dictionary<'k, int> ()
          Values_ = Array.zeroCreate capacity
          Current_ = 0
          Removed_ = 0.0f
          Threshold_ = threshold }

    /// Convert a keyed array to a sequence.
    let toSeq (karr : KeyedArray<_, _>) =
        seq karr

    /// Convert a keyed array from a sequence.
    let ofSeq threshold seq =
        let arr = Seq.toArray seq
        let karr = make threshold arr.Length
        for (key, value) in arr do add key value karr |> ignore
        karr

    /// Fold over a keyed array.
    let fold folder state karr =
        let arr = karr.Values_
        let length = Array.length arr
        let mutable state = state
        let mutable index = 0
        while index < length do
            let struct (exists, key, value) = arr.[index]
            if exists then state <- folder state key value
        state

/// A garbage-collected keyed array.
/// TODO: once this is well-tested, let's consider moving into Prime.
type KeyedArray<'k, 'v when 'k : equality> = KeyedArray.KeyedArray<'k, 'v>