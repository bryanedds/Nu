// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

// TODO: document this!

[<RequireQualifiedAccess>]
module SegmentedHashSet =

    type [<ReferenceEquality>] SegmentedHashSet<'a when 'a : equality> =
        private
            { HashSets : 'a HashSet array
              Comparer : 'a IEqualityComparer }

        member this.Count =
            let mutable count = 0
            for set in this.HashSets do
                count <- count + set.Count
            count

        member this.Contains item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32) // TODO: use Prime's `modulus` operator instead?
            this.HashSets.[index].Contains item

        member this.Add item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32)
            this.HashSets.[index].Add item

        member this.Remove item =
            let hashCode = hash item
            let index = Math.Abs (hashCode % 32)
            this.HashSets.[index].Remove item

        member this.Clear () =
            for set in this.HashSets do
                set.Clear ()

        member this.GetEnumerator () =
            (Seq.concat this.HashSets).GetEnumerator ()

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.HashSets).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.HashSets).GetEnumerator () :> IEnumerator

    let make (comparer : 'a IEqualityComparer) =
        let hashSets = Array.init 32 (fun _ -> HashSet<'a> comparer) // TODO: use constant.
        { HashSets = hashSets
          Comparer = comparer }

    let count (sset : 'a SegmentedHashSet) =
        sset.Count

    let isEmpty sset =
        count sset = 0

    let notEmpty sset =
        count sset > 0

    let contains item (sset : 'a SegmentedHashSet) =
        sset.Contains item

    let add item (sset : 'a SegmentedHashSet) =
        sset.Add item

    let remove item (sset : 'a SegmentedHashSet) =
        sset.Remove item

    let clear (sset : 'a SegmentedHashSet) =
        sset.Clear ()

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
