// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SegmentedList =

    type 'a SegmentedListEnumerator (slist : 'a SegmentedList) =

        let mutable i = -1
        let mutable j = -1
        let mutable k = -1

        member this.MoveNext () =
            if k < dec slist.TotalLength then
                if i = -1 then
                    if slist.Segments.Count = 0 then false
                    else
                        i <- inc i
                        j <- inc j // guaranteed no empty segments in SegementedList.
                        k <- inc k
                        true
                elif i < slist.Segments.Count then
                    let segment = slist.Segments.[i]
                    if  j < dec segment.Length then
                        j <- inc j
                        k <- inc k
                        true
                    elif i < dec slist.Segments.Count then
                        i <- inc i
                        j <- 0
                        k <- inc k
                        true
                    else false
                else false
            else false

        member this.Current =
            if i < slist.Segments.Count then
                let segment = slist.Segments.[i]
                if j < segment.Length
                then segment.[j]
                else raise (InvalidOperationException "Current SegmentedList item out of range.")
            else raise (InvalidOperationException "Current SegmentedList item out of range.")

        member this.Reset () =
            i <- -1
            j <- -1

        interface 'a IEnumerator with
            member this.MoveNext () = this.MoveNext ()
            member this.Current = this.Current
            member this.Current = (this :> 'a IEnumerator).Current :> obj
            member this.Reset () = this.Reset ()
            member this.Dispose () = ()

    and [<ReferenceEquality; NoComparison>] 'a SegmentedList =
        private
            { mutable TotalLength : int
              mutable TotalCapacity : int
              SegmentSize : int
              Segments : 'a array List }

        member this.Length =
            this.TotalLength

        member this.Item (i : int) : 'a byref =
            if i < this.TotalCapacity then
                let (segmentIndex, segmentOffset) = Math.DivRem (i, this.SegmentSize)
                let segment = this.Segments.[segmentIndex]
                &segment.[segmentOffset]
            else
                raise (IndexOutOfRangeException "Index out of range.")
                &this.Segments.[-1].[-1] // fool compiler

        member this.GetEnumerator () =
            new SegmentedListEnumerator<'a> (this)

        interface 'a IEnumerable with
            member this.GetEnumerator () = new SegmentedListEnumerator<'a> (this) :> 'a IEnumerator
            member this.GetEnumerator () = new SegmentedListEnumerator<'a> (this) :> IEnumerator

    let make<'a> () =
        let size = sizeof<'a>
        let segmentSize = Constants.Engine.LohSizeMinusArraySlop / size
        let segment = Array.zeroCreate segmentSize
        let segments = List<'a array> ()
        segments.Add segment
        { TotalLength = 0; TotalCapacity = segmentSize; SegmentSize = segmentSize; Segments = segments }

    let makeWithCapacity<'a> capacity =
        if capacity < 0 then raise (ArgumentException ("Invalid argument.", nameof capacity))
        if capacity = 0 then make<'a> ()
        else
            let size = sizeof<'a>
            let segmentSize = Constants.Engine.LohSizeMinusArraySlop / size
            let segmentCount = max (capacity / segmentSize) 1
            let segments = Array.init segmentCount (fun _ -> Array.zeroCreate<'a> segmentSize)
            { TotalLength = 0; TotalCapacity = segmentCount * segmentSize; SegmentSize = segmentSize; Segments = List segments }

    let isEmpty slist =
        slist.TotalLength = 0

    let notEmpty slist =
        slist.TotalLength > 0

    let length slist =
        slist.TotalLength

    let item index (slist : 'a SegmentedList) =
        slist.[index]

    let inline private mutateInline mutator slist =
        for i in 0 .. dec slist.Segments.Count do
            let segment = slist.Segments.[i]
            let i' = i * slist.SegmentSize
            for j in 0 .. dec segment.Length do
                if i' + j < slist.TotalLength then
                    segment.[j] <- mutator segment.[j]

    let mutate mutator slist =
        mutateInline mutator slist

    let add item slist =
        if slist.TotalLength < slist.TotalCapacity then
            let (i, j) = Math.DivRem (slist.TotalLength, slist.SegmentSize)
            slist.Segments.[i].[j] <- item
            slist.TotalLength <- inc slist.TotalLength
        else
            let segment = Array.zeroCreate slist.SegmentSize
            segment.[0] <- item
            slist.Segments.Add segment
            slist.TotalCapacity <- slist.TotalCapacity + slist.SegmentSize
            slist.TotalLength <- inc slist.TotalLength

    let addMany (seq : 'a seq) slist =
        for item in seq do
            add item slist

    let removeAt index slist =
        if index >= slist.TotalLength then raise (IndexOutOfRangeException "Index out of range.")
        let indexLast = dec slist.TotalLength
        slist.[index] <- slist.[indexLast]
        slist.[indexLast] <- Unchecked.defaultof<'a>
        slist.TotalLength <- dec slist.TotalLength

    let clear slist =
        mutateInline (fun _ -> Unchecked.defaultof<'a>) slist
        slist.TotalLength <- 0

    let skip count slist =
        if count > slist.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = makeWithCapacity (slist.TotalLength - count)
        for i in count .. dec slist.TotalLength do
            add slist.[i] result
        result

    let take count slist =
        if count > slist.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = makeWithCapacity count
        for i in 0 .. dec count do
            add slist.[i] result
        result

    let append left right =
        let length = left.TotalLength + right.TotalLength
        let result = makeWithCapacity length
        for i in 0 .. dec left.TotalLength do
            add left.[i] result
        for i in 0 .. dec right.TotalLength do
            add right.[i] result
        result

    let map mapper slist =
        let result = makeWithCapacity slist.TotalLength
        for i in 0 .. dec slist.TotalLength do
            add (mapper slist.[i]) result
        result

    let map2 mapper left right =
        if left.TotalLength <> right.TotalLength then raise (ArgumentException ("SegmentedList length does not match.", nameof right))
        let result = makeWithCapacity left.TotalLength
        for i in 0 .. dec left.TotalLength do
            add (mapper left.[i] right.[i]) result
        result

    let transform (transformer : 'a -> 'a) slist =
        let result = makeWithCapacity slist.TotalCapacity
        for i in 0 .. dec slist.Segments.Count do
            let sourceSegment = slist.Segments.[i]
            let resultSegment = result.Segments.[i]
            let i' = i * slist.SegmentSize
            for j in 0 .. dec sourceSegment.Length do
                if i' + j < slist.TotalLength then
                    resultSegment.[j] <- transformer sourceSegment.[j]
        result.TotalLength <- slist.TotalLength
        result

    let transform2 (transformer : 'a -> 'a -> 'a) left right =
        if left.TotalLength <> right.TotalLength then raise (ArgumentException "")
        let result = makeWithCapacity left.TotalLength
        for i in 0 .. dec left.Segments.Count do
            let leftSegment = left.Segments.[i]
            let rightSegment = right.Segments.[i]
            let resultSegment = result.Segments.[i]
            let i' = i * left.SegmentSize
            for j in 0 .. dec leftSegment.Length do
                if i' + j < left.TotalLength then
                    resultSegment.[j] <- transformer leftSegment.[j] rightSegment.[j]
        result

    let foreach op slist =
        for i in 0 .. dec slist.Segments.Count do
            let segment = slist.Segments.[i]
            let i' = i * slist.SegmentSize
            for j in 0 .. dec segment.Length do
                if i' + j < slist.TotalLength then
                    op segment.[j]

    let filter predicate slist =
        let result = make ()
        for i in 0 .. dec slist.Segments.Count do
            let segment = slist.Segments.[i]
            let i' = i * slist.SegmentSize
            for j in 0 .. dec segment.Length do
                if i' + j < slist.TotalLength then
                    let item = &segment.[j]
                    if predicate item then
                        add item result
        result

    let partition discriminator slist =
        let pass = make ()
        let fail = make ()
        for i in 0 .. dec slist.TotalLength do
            let item = &slist.[i]
            if discriminator item
            then add item pass
            else add item fail
        (pass, fail)

    let fold folder state slist =
        let mutable state = state
        for i in 0 .. dec slist.Segments.Count do
            let segment = slist.Segments.[i]
            let i' = i * slist.SegmentSize
            for j in 0 .. dec segment.Length do
                if i' + j < slist.TotalLength then
                    let item = &segment.[j]
                    state <- folder state item
        state

    let singleton item =
        let result = makeWithCapacity 1
        add item result
        result

    let ofSeq (seq : 'a seq) =
        let result = make<'a> ()
        for item in seq do
            add item result
        result

    let ofList (list : 'a list) =
        ofSeq list

type 'a SegmentedListEnumerator = 'a SegmentedList.SegmentedListEnumerator

type 'a SegmentedList = 'a SegmentedList.SegmentedList