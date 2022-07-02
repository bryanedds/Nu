// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SegmentedDictionary =

    type [<ReferenceEquality; NoComparison>] SegmentedDictionary<'k, 'v when 'k : equality> =
        private
            { Dictionaries : Dictionary<'k, 'v> array
              Comparer : 'k EqualityComparer }

        member this.Length =
            let mutable length = 0
            for set in this.Dictionaries do
                length <- length + set.Count
            length

        interface IEnumerable<KeyValuePair<'k, 'v>> with
            member this.GetEnumerator () = (Seq.concat this.Dictionaries).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Dictionaries).GetEnumerator () :> IEnumerator

    let make (comparer : 'k EqualityComparer) =
        let hashSets = Array.init 32 (fun _ -> Dictionary<'k, 'v> comparer)
        { Dictionaries = hashSets
          Comparer = comparer }

    let length (sdict : SegmentedDictionary<'k, 'v>) =
        sdict.Length

    let containsKey key sdict =
        let hashCode = hash key
        let index = hashCode % 32
        sdict.Dictionaries.[index].ContainsKey key

    let add key value sdict =
        let hashCode = hash key
        let index = hashCode % 32
        sdict.Dictionaries.[index].Add (key, value)

    let remove key sdict =
        let hashCode = hash key
        let index = hashCode % 32
        sdict.Dictionaries.[index].Remove key

    let clear sdict =
        for dict in sdict.Dictionaries do
            dict.Clear ()

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
