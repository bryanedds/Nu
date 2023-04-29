// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module SegmentedDictionary =

    type [<ReferenceEquality>] SegmentedDictionary<'k, 'v when 'k : equality> =
        private
            { Dictionaries : Dictionary<'k, 'v> array
              Comparer : 'k IEqualityComparer }

        member this.Length =
            let mutable length = 0
            for set in this.Dictionaries do
                length <- length + set.Count
            length

        member this.Item (key : 'k) : 'v =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32) // TODO: use constant.
            this.Dictionaries.[index].[key]

        member this.ContainsKey key =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32) // TODO: use Prime's `modulus` operator instead?
            this.Dictionaries.[index].ContainsKey key

        member this.TryFind key =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            match this.Dictionaries.[index].TryGetValue key with
            | (true, value) -> Some value
            | (false, _) -> None

        member this.TryGetValue (key, valueRef : _ outref) =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries.[index].TryGetValue (key, &valueRef)

        member this.Add (key, value) =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries.[index].Add (key, value)

        member this.Remove key =
            let hashCode = this.Comparer.GetHashCode key
            let index = Math.Abs (hashCode % 32)
            this.Dictionaries.[index].Remove key

        member this.Clear () =
            for dict in this.Dictionaries do
                dict.Clear ()

        member this.GetEnumerator () =
            (Seq.concat this.Dictionaries).GetEnumerator ()

        interface IEnumerable<KeyValuePair<'k, 'v>> with
            member this.GetEnumerator () = (Seq.concat this.Dictionaries).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Dictionaries).GetEnumerator () :> IEnumerator

    let make (comparer : 'k IEqualityComparer) =
        let hashSets = Array.init 32 (fun _ -> Dictionary<'k, 'v> comparer)
        { Dictionaries = hashSets
          Comparer = comparer }

    let length (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.Length

    let isEmpty sdict =
        length sdict = 0

    let notEmpty sdict =
        length sdict > 0

    let containsKey key (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.ContainsKey key

    let tryFind (key, sdict : SegmentedDictionary<'k, 'v>) =
        sdict.TryFind key

    let tryGetValue (key, sdict : SegmentedDictionary<'k, 'v>, valueRef : _ outref) =
        sdict.TryGetValue (key, valueRef)

    let add key value (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.Add (key, value)

    let remove key (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.Remove key

    let clear (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.Clear ()

    let toSeq sdict =
        Seq.concat sdict.Dictionaries

    let ofSeq comparer seq =
        let sdict = make comparer
        for (kvp : KeyValuePair<'k, 'v>) in seq do add kvp.Key kvp.Value sdict
        sdict

    let singleton comparer key value =
        let sdict = make comparer
        add key value sdict
        sdict

    let map<'k, 'v, 'u when 'k : equality and 'u : equality> comparer (mapper : 'k -> 'v -> 'u) (sdict : SegmentedDictionary<'k, 'v>) =
        toSeq sdict |>
        Seq.map (fun kvp -> let value = mapper kvp.Key kvp.Value in KeyValuePair<'k, 'u> (kvp.Key, value)) |>
        ofSeq comparer

    let filter pred sdict =
        ofSeq sdict.Comparer (Seq.filter pred (toSeq sdict))

    let fold folder sdict =
        Seq.fold folder (toSeq sdict)

type SegmentedDictionary<'k, 'v when 'k : equality> = SegmentedDictionary.SegmentedDictionary<'k, 'v>
