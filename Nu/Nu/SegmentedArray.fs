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
              Segments : 'a array array }

        member this.Length =
            this.TotalLength

        member this.Item i =
            if i < this.TotalLength then
                let segmentIndex = i / this.SegmentSize
                let segmentOffset = i % this.SegmentSize
                let segment = this.Segments.[segmentIndex]
                &segment.[segmentOffset]
            else
                raise (IndexOutOfRangeException "")
                &this.Segments.[-1].[-1] // fool compiler

    let zeroCreate<'a> length =
        if length < 0 then raise (ArgumentException "")
        let size = sizeof<'a>
        let segmentSize = 85000 / size
        let segmentCount = length / segmentSize
        let segmentRemainder = length % segmentSize
        let segments =
            Array.init
                (if segmentRemainder = 0 then segmentCount else inc segmentCount)
                (fun i ->
                    if i < segmentCount
                    then Array.zeroCreate<'a> segmentSize
                    else Array.zeroCreate<'a> segmentRemainder)
        { TotalLength = length; SegmentSize = segmentSize; Segments = segments }

    let isEmpty sarray =
        sarray.TotalLength = 0

    let notEmpty sarray =
        sarray.TotalLength > 0

    let item index (sarray : 'a SegmentedArray) =
        sarray.[index]

    let skip count sarray =
        if count > sarray.TotalLength then raise (ArgumentException "")
        let result = zeroCreate (sarray.TotalLength - count)
        for i in count .. dec sarray.TotalLength do
            result.[i - count] <- sarray.[i]
        result

    let take count sarray =
        if count > sarray.TotalLength then raise (ArgumentException "")
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
        if left.TotalLength <> right.TotalLength then raise (ArgumentException "")
        let result = zeroCreate left.TotalLength
        for i in 0 .. dec left.TotalLength do
            result.[i] <- mapper left.[i] right.[i]
        result

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
        { TotalLength = 0; SegmentSize = 0; Segments = [||] }

type SegmentedArray<'a> = SegmentedArray.SegmentedArray<'a>