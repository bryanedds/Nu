// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SegmentedHashSet =

    type [<ReferenceEquality>] SegmentedHashSet<'a when 'a : equality> =
        private
            { HashSets : 'a HashSet array
              Comparer : 'a IEqualityComparer }

        member this.Length =
            let mutable length = 0
            for set in this.HashSets do
                length <- length + set.Count
            length

        member this.GetEnumerator () =
            (Seq.concat this.HashSets).GetEnumerator ()

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.HashSets).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.HashSets).GetEnumerator () :> IEnumerator

    let make (comparer : 'a IEqualityComparer) =
        let hashSets = Array.init 32 (fun _ -> HashSet<'a> comparer) // TODO: use constant.
        { HashSets = hashSets
          Comparer = comparer }

    let length (sset : 'a SegmentedHashSet) =
        sset.Length

    let isEmpty sset =
        length sset = 0

    let notEmpty sset =
        length sset > 0

    let contains item sset =
        let hashCode = hash item
        let index = Math.Abs (hashCode % 32) // TODO: use Prime's `modulus` operator instead?
        sset.HashSets.[index].Contains item

    let add item sset =
        let hashCode = hash item
        let index = Math.Abs (hashCode % 32)
        sset.HashSets.[index].Add item

    let remove item sset =
        let hashCode = hash item
        let index = Math.Abs (hashCode % 32)
        sset.HashSets.[index].Remove item

    let clear sset =
        for set in sset.HashSets do
            set.Clear ()

    let toSeq sset =
        Seq.concat sset.HashSets

    let ofSeq comparer seq =
        let sset = make comparer
        for item in seq do add item sset |> ignore<bool>
        sset

    let singleton comparer item =
        let sset = make comparer
        add item sset |> ignore<bool>
        sset

    let map<'a, 'b when 'a : equality and 'b : equality> comparer (mapper : 'a -> 'b) (sset : 'a SegmentedHashSet) =
        ofSeq comparer (Seq.map mapper (toSeq sset))

    let filter pred sset =
        ofSeq sset.Comparer (Seq.filter pred (toSeq sset))

    let fold folder sset =
        Seq.fold folder (toSeq sset)

type SegmentedHashSet<'a when 'a : equality> = SegmentedHashSet.SegmentedHashSet<'a>
