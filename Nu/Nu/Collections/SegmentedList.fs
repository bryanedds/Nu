// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SegmentedList =

    type [<ReferenceEquality; NoComparison>] 'a SegmentedList =
        private
            { mutable TotalLength : int
              mutable ListCapacity : int
              Lists : 'a List List }

        member this.Length =
            this.TotalLength

        member this.Item (i : int) =
            if i < this.TotalLength then
                let j = i / this.ListCapacity
                let k = i % this.ListCapacity
                this.Lists.[j].[k]
            else raise (IndexOutOfRangeException "Index out of range.")

        member this.GetEnumerator () =
            Seq.concat this.Lists

        interface 'a IEnumerable with
            member this.GetEnumerator () = (Seq.concat this.Lists).GetEnumerator ()
            member this.GetEnumerator () = (Seq.concat this.Lists).GetEnumerator () :> IEnumerator

    let make<'a> () =
        let size = sizeof<'a>
        let listCapacity = Constants.Engine.LohSize / size / 2
        { TotalLength = 0; ListCapacity = listCapacity; Lists = List [List<'a> ()] }

    let isEmpty slist =
        slist.TotalLength = 0

    let notEmpty slist =
        slist.TotalLength > 0

    let length slist =
        slist.TotalLength

    let item index (slist : 'a SegmentedList) =
        slist.[index]

    let add item slist =
        let lastList = slist.Lists.[dec slist.Lists.Count]
        if lastList.Count < slist.ListCapacity then
            lastList.Add item
        else
            let newList = List ()
            newList.Add item
            slist.Lists.Add newList
        slist.TotalLength <- inc slist.TotalLength

    let addMany (seq : 'a seq) slist =
        for item in seq do
            add item slist

    let clear slist =
        let firstList = slist.Lists.[0]
        firstList.Clear ()
        slist.Lists.Clear ()
        slist.Lists.Add firstList
        slist.TotalLength <- 0

    let skip count (slist : 'a SegmentedList) =
        if count > slist.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = make ()
        for i in count .. dec slist.TotalLength do
            add slist.[i] result
        result

    let take count (slist : 'a SegmentedList) =
        if count > slist.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = make ()
        for i in 0 .. dec slist.TotalLength - count do
            add slist.[i] result
        result

    let append left (right : 'a SegmentedList) =
        addMany right left

    let map mapper slist =
        let result = make ()
        for item in slist do
            add (mapper item) result
        result

    let map2 mapper left right =
        if left.TotalLength <> right.TotalLength then raise (ArgumentException ("SegmentedList length does not match.", nameof right))
        let result = make ()
        for i in 0 .. dec left.TotalLength do
            add (mapper left.[i] right.[i]) result
        result

    let foreach op slist =
        for item in slist do
            op item

    let filter predicate slist =
        let result = make ()
        for item in slist do
            if predicate item then
                add item result
        result

    let partition discriminator slist =
        let pass = make ()
        let fail = make ()
        for item in slist do
            if discriminator item
            then add item pass
            else add item fail
        (pass, fail)

    let fold folder state slist =
        let mutable state = state
        for item in slist do
            state <- folder state item
        state

    let singleton item =
        let result = make ()
        add item result
        result

    let ofSeq (seq : 'a seq) =
        let result = make<'a> ()
        for item in seq do
            add item result
        result

    let ofList (list : 'a list) =
        ofSeq list

type 'a SegmentedList = 'a SegmentedList.SegmentedList