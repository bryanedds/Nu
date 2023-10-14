// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open Prime

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
              mutable Removed_ : single // single to elide unnecessary conversions
              mutable Threshold_ : single }

        /// The keyed array values.
        member this.Values =
            this.Values_

        /// The threshold for compaction.
        member this.Threshold =
            this.Threshold_

        /// The current number of values (some of which may be empty).
        member this.Length =
            this.Current_

        member private this.Overflowing =
            this.Current_ = this.Values.Length

        member private this.Underflowing =
            this.Removed_ / single this.Keys_.Count >= 1.0f / this.Threshold_

        member private this.Expand () =

            // check overflow
            if this.Overflowing then

                // grow
                let values = Array.zeroCreate (max 1 this.Current_ * int this.Threshold_)
                Array.blit this.Values_ 0 values 0 this.Current_
                this.Values_ <- values

        member private this.Compact () =
        
            // check underflow
            if this.Underflowing then

                // reorg
                let mutable current = 0
                for key in Seq.toArray this.Keys_.Values do
                    let index = this.Indices_.[key]
                    let value = this.Values_.[index]
                    this.Keys_.Remove index |> ignore
                    this.Keys_.Add (current, key)
                    this.Indices_.[key] <- current
                    this.Values_.[current] <- value
                    current <- inc current

                // shrink
                let values = Array.zeroCreate (int (single this.Current_ * (1.0f / this.Threshold_)))
                Array.blit this.Values_ 0 values 0 values.Length
                this.Values_ <- values

                // clean up
                this.Current_ <- current
                this.Removed_ <- 0.0f

        /// Add a keyed value, or update one if it already exists.
        member this.Add (key, value) =
            match this.Indices_.TryGetValue key with
            | (false, _) ->
                this.Expand ()
                let index = this.Current_
                this.Keys_.Add (index, key) |> ignore
                this.Indices_.Add (key, index)
                this.Values_.[index] <- struct (true, key, value)
                this.Current_ <- inc this.Current_
                index
            | (true, index) ->
                this.Values_.[index] <- struct (true, key, value)
                index
        
        /// Remove a keyed value if it exists.
        member this.Remove key =
            match this.Indices_.TryGetValue key with
            | (true, index) ->
                this.Keys_.Remove index |> ignore
                this.Indices_.Remove key |> ignore
                this.Values_.[index] <- struct (false, key, Unchecked.defaultof<'v>)
                this.Removed_ <- inc this.Removed_
                this.Compact ()
            | (false, _) -> ()
        
        /// Query that a keyed value exists.
        member this.ContainsKey key =
            this.Indices_.ContainsKey key
        
        /// Attempt to find a keyed value.
        member this.TryFind key =
            match this.Indices_.TryGetValue key with
            | (true, index) -> Some this.Values_.[index]
            | (false, _) -> None

        /// Attempt to get a keyed value.
        member this.TryGetValue (key, valueRef : _ outref) =
            this.Indices_.TryGetValue (key, valueRef)

        /// Index a keyed value.
        member this.Item key =
            &this.Values_.[this.Indices_.[key]]

        interface struct (bool * 'k * 'v) IEnumerable with
            member this.GetEnumerator () = this.Values_.GetEnumerator ()
            member this.GetEnumerator () = (this.Values_ :> _ IEnumerable).GetEnumerator ()

    /// Make a KeyedArray with the given compaction threshold and initial capaticy.
    let make<'k, 'v when 'k : equality> threshold capacity : KeyedArray<'k, 'v> =
        { Keys_ = SortedDictionary<int, 'k> ()
          Indices_ = Dictionary<'k, int> ()
          Values_ = Array.zeroCreate capacity
          Current_ = 0
          Removed_ = 0.0f
          Threshold_ = threshold }

    /// Get the keyed array values.
    let getValues (karr : KeyedArray<'k, 'v>) =
        karr.Values

    /// Get the threshold for compaction.
    let getThreshold (karr : KeyedArray<'k, 'v>) =
        karr.Threshold

    /// Get the current number of values (some of which may be empty).
    let getLength (karr : KeyedArray<'k, 'v>) =
        karr.Length

    /// Add a keyed value, or update one if it already exists.
    let add key value (karr : KeyedArray<'k, 'v>) =
        karr.Add (key, value)

    /// Remove a keyed value if it exists.
    let remove key (karr : KeyedArray<'k, 'v>) =
        karr.Remove key

    /// Query that a keyed value exists.
    let containsKey key (karr : KeyedArray<'k, 'v>) =
        karr.ContainsKey key

    /// Attempt to find a keyed value.
    let tryFind key (karr : KeyedArray<'k, 'v>) =
        karr.TryFind key

    /// Attempt to get a keyed value.
    let tryGetValue (key, karr : KeyedArray<'k, 'v>, valueRef : _ outref) =
        karr.TryGetValue (key, valueRef)

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
/// NOTE: not supported by SymbolicConverter.
type KeyedArray<'k, 'v when 'k : equality> = KeyedArray.KeyedArray<'k, 'v>