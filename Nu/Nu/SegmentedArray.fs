// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System
open System.Collections
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SegmentedArray =

    type 'a SegmentedArrayEnumerator (sarray : 'a SegmentedArray) =

        let mutable i = -1
        let mutable j = -1

        member this.MoveNext () =
            if i = -1 then
                if sarray.Segments.Length = 0 then false
                else
                    i <- inc i
                    j <- inc j // guaranteed no empty segments in SegementedArray.
                    true
            elif i < sarray.Segments.Length then
                let segment = sarray.Segments.[i]
                if j < dec segment.Length then
                    j <- inc j
                    true
                elif i < dec sarray.Segments.Length then
                    i <- inc i
                    j <- 0
                    true
                else false
            else false

        member this.Current =
            if i < sarray.Segments.Length then
                let segment = sarray.Segments.[i]
                if j < segment.Length
                then segment.[j]
                else raise (InvalidOperationException "Current SegmentedArray item out of range.")
            else raise (InvalidOperationException "Current SegmentedArray item out of range.")

        member this.Reset () =
            i <- -1
            j <- -1

        interface 'a IEnumerator with
            member this.MoveNext () = this.MoveNext ()
            member this.Current = this.Current
            member this.Current = (this :> 'a IEnumerator).Current :> obj
            member this.Reset () = this.Reset ()
            member this.Dispose () = ()

    and [<ReferenceEquality; NoComparison>] 'a SegmentedArray =
        private
            { TotalLength : int
              SegmentSize : int
              SegmentRemainder : int
              Segments : 'a array array }

        member this.Length =
            this.TotalLength

        member this.Item (i : int) : 'a byref =
            if i < this.TotalLength then
                let (segmentIndex, segmentOffset) = Math.DivRem (i, this.SegmentSize)
                let segment = this.Segments.[segmentIndex]
                &segment.[segmentOffset]
            else
                raise (IndexOutOfRangeException "Index out of range.")
                &this.Segments.[-1].[-1] // fool compiler

        member this.GetEnumerator () =
            new SegmentedArrayEnumerator<'a> (this)

        interface 'a IEnumerable with
            member this.GetEnumerator () = new SegmentedArrayEnumerator<'a> (this) :> 'a IEnumerator
            member this.GetEnumerator () = new SegmentedArrayEnumerator<'a> (this) :> IEnumerator

    let empty =
        { TotalLength = 0; SegmentSize = 0; SegmentRemainder = 0; Segments = [||] }

    let zeroCreate<'a> length =
        if length < 0 then raise (ArgumentException ("Invalid argument.", nameof length))
        let size = sizeof<'a>
        let segmentSize = Constants.Engine.LohSizeMinusArraySlop / size
        let (segmentCount, segmentRemainder) = Math.DivRem (length, segmentSize)
        let segments =
            Array.init
                (if segmentRemainder = 0 then segmentCount else inc segmentCount)
                (fun i ->
                    if i < segmentCount
                    then Array.zeroCreate<'a> segmentSize
                    else Array.zeroCreate<'a> segmentRemainder)
        { TotalLength = length; SegmentSize = segmentSize; SegmentRemainder = segmentRemainder; Segments = segments }

    let isEmpty sarray =
        sarray.TotalLength = 0

    let notEmpty sarray =
        sarray.TotalLength > 0

    let item index (sarray : 'a SegmentedArray) =
        sarray.[index]

    let mutate mutator sarray =
        for i in 0 .. dec sarray.Segments.Length do
            let segment = sarray.Segments.[i]
            if i = dec sarray.Segments.Length then
                for j in 0 .. dec sarray.SegmentRemainder do
                    segment.[j] <- mutator segment.[j]
            else
                for j in 0 .. dec segment.Length do
                    segment.[j] <- mutator segment.[j]

    let skip count sarray =
        if count > sarray.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = zeroCreate (sarray.TotalLength - count)
        for i in count .. dec sarray.TotalLength do
            result.[i - count] <- sarray.[i]
        result

    let take count sarray =
        if count > sarray.TotalLength then raise (ArgumentException ("Invalid argument.", nameof count))
        let result = zeroCreate count
        for i in 0 .. dec count do
            result.[i] <- sarray.[i]
        result

    let append left right =
        let length = left.TotalLength + right.TotalLength
        let result = zeroCreate length
        for i in 0 .. dec left.TotalLength do
            result.[i] <- left.[i]
        let floor = left.TotalLength
        for i in floor .. floor + dec right.TotalLength do
            result.[i] <- right.[i - floor]
        result

    let map mapper sarray =
        let result = zeroCreate sarray.TotalLength
        for i in 0 .. dec sarray.TotalLength do
            result.[i] <- mapper sarray.[i]
        result

    let map2 mapper left right =
        if left.TotalLength <> right.TotalLength then raise (ArgumentException ("SegmentedArray length does not match.", nameof right))
        let result = zeroCreate left.TotalLength
        for i in 0 .. dec left.TotalLength do
            result.[i] <- mapper left.[i] right.[i]
        result

    let transform (transformer : 'a -> 'a) sarray =
        let result = zeroCreate sarray.TotalLength
        for i in 0 .. dec sarray.Segments.Length do
            let sourceSegment = sarray.Segments.[i]
            let resultSegment = result.Segments.[i]
            if i = dec sarray.Segments.Length then
                for j in 0 .. dec sarray.SegmentRemainder do
                    resultSegment.[j] <- transformer sourceSegment.[j]
            else
                for j in 0 .. dec sourceSegment.Length do
                    resultSegment.[j] <- transformer sourceSegment.[j]
        result

    let transform2 (transformer : 'a -> 'a -> 'a) left right =
        if left.TotalLength <> right.TotalLength then raise (ArgumentException "")
        let result = zeroCreate left.TotalLength
        for i in 0 .. dec left.Segments.Length do
            let leftSegment = left.Segments.[i]
            let rightSegment = right.Segments.[i]
            let resultSegment = result.Segments.[i]
            if i = dec left.Segments.Length then
                for j in 0 .. dec left.SegmentRemainder do
                    resultSegment.[j] <- transformer leftSegment.[j] rightSegment.[j]
            else
                for j in 0 .. dec leftSegment.Length do
                    resultSegment.[j] <- transformer leftSegment.[j] rightSegment.[j]
        result

    let foreach op sarray =
        for i in 0 .. dec sarray.Segments.Length do
            let segment = sarray.Segments.[i]
            if i = dec sarray.Segments.Length then
                for j in 0 .. dec sarray.SegmentRemainder do
                    op segment.[j]
            else
                for j in 0 .. dec segment.Length do
                    op segment.[j]

    let filter predicate sarray =
        let mutable count = 0
        for i in 0 .. dec sarray.TotalLength do
            if predicate sarray.[i] then count <- inc count
        let result = zeroCreate count
        let mutable j = 0
        for i in 0 .. dec sarray.TotalLength do
            let item = &sarray.[i]
            if predicate item then
                result.[j] <- item
                j <- inc j
        result

    let partition discriminator sarray =
        let mutable count = 0
        for i in 0 .. dec sarray.TotalLength do
            if discriminator sarray.[i] then count <- inc count
        let pass = zeroCreate count
        let fail = zeroCreate (sarray.Length - count)
        let mutable p = 0
        let mutable f = 0
        for i in 0 .. dec sarray.TotalLength do
            let item = &sarray.[i]
            if discriminator item then
                pass.[p] <- item
                p <- inc p
            else
                fail.[f] <- item
                f <- inc f
        (pass, fail)

    let fold folder state sarray =
        let mutable state = state
        for i in 0 .. dec sarray.Segments.Length do
            let segment = sarray.Segments.[i]
            if i = dec sarray.Segments.Length then
                for j in 0 .. dec sarray.SegmentRemainder do
                    state <- folder state segment.[j]
            else
                for j in 0 .. dec segment.Length do
                    state <- folder state segment.[j]
        state

    let singleton item =
        let result = zeroCreate 1
        result.[0] <- item
        result

    let ofList (list : 'a list) =
        let count = list.Length
        let result = zeroCreate count
        let mutable i = 0
        for item in list do
            result.[i] <- item
            i <- inc i
        result

    let ofSeq seq =
        let list = List.ofSeq seq
        ofList list

type 'a SegmentedArrayEnumerator = 'a SegmentedArray.SegmentedArrayEnumerator

type 'a SegmentedArray = 'a SegmentedArray.SegmentedArray