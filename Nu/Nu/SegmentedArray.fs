// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open System

[<RequireQualifiedAccess>]
module SegmentedArray =

    type SegmentedArray<'a> =
        private
            { TotalLength : int
              SegmentSize : int
              SegmentRemainder : int
              Segments : 'a array array }

        member this.Length =
            this.TotalLength

        member this.Item i =
            if i < this.TotalLength then
                let (segmentIndex, segmentOffset) = Math.DivRem (i, this.SegmentSize)
                let segment = this.Segments.[segmentIndex]
                &segment.[segmentOffset]
            else
                raise (IndexOutOfRangeException "Index out of range.")
                &this.Segments.[-1].[-1] // fool compiler

    let zeroCreate<'a> length =
        if length < 0 then raise (ArgumentException ("Invalid argument.", nameof length))
        let size = sizeof<'a>
        let segmentSize = Constants.Engine.LohSizeMinusArraySlop / size
        let segmentCount = length / segmentSize
        let segmentRemainder = length % segmentSize
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
        let count = left.TotalLength + right.TotalLength
        let result = zeroCreate count
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

    let transform transformer sarray =
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

    let transform2 transformer left right =
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

    let mutate mutator sarray =
        for i in 0 .. dec sarray.Segments.Length do
            let segment = sarray.Segments.[i]
            if i = dec sarray.Segments.Length then
                for j in 0 .. dec sarray.SegmentRemainder do
                    segment.[j] <- mutator segment.[j]
            else
                for j in 0 .. dec segment.Length do
                    segment.[j] <- mutator segment.[j]

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

    let fold folder state sarray =
        let mutable state = state
        for i in 0 .. dec sarray.TotalLength do
            let item = &sarray.[i]
            state <- folder state item
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

    let empty =
        { TotalLength = 0; SegmentSize = 0; SegmentRemainder = 0; Segments = [||] }

type SegmentedArray<'a> = SegmentedArray.SegmentedArray<'a>