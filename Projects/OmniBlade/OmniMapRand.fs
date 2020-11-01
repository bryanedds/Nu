// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open TiledSharp

type OriginRand =
    | OriginC
    | OriginN
    | OriginE
    | OriginS
    | OriginW
    | OriginNE
    | OriginNW
    | OriginSE
    | OriginSW

type SegmentRand =
    | Segment0 = 0b0000
    | Segment1N = 0b0001
    | Segment1E = 0b0010
    | Segment1S = 0b0100
    | Segment1W = 0b1000
    | Segment2NE = 0b0011
    | Segment2NW = 0b1001
    | Segment2SE = 0b0110
    | Segment2SW = 0b1100
    | Segment2EW = 0b1010
    | Segment2NS = 0b0101
    | Segment3N = 0b1011
    | Segment3E = 0b0111
    | Segment3S = 0b1110
    | Segment3W = 0b1101
    | Segment4 = 0b1111

type [<NoComparison>] SegmentsRand =
    { Segment1N : TmxMap
      Segment1E : TmxMap
      Segment1S : TmxMap
      Segment1W : TmxMap
      Segment2NE : TmxMap
      Segment2NW : TmxMap
      Segment2SE : TmxMap
      Segment2SW : TmxMap
      Segment2NS : TmxMap
      Segment2EW : TmxMap
      Segment3N : TmxMap
      Segment3E : TmxMap
      Segment3S : TmxMap
      Segment3W : TmxMap
      Segment4 : TmxMap }

    static member load (filePath : string) =
      { Segment1N = TmxMap (filePath + "+1N.tmx")
        Segment1E = TmxMap (filePath + "+1E.tmx")
        Segment1S = TmxMap (filePath + "+1S.tmx")
        Segment1W = TmxMap (filePath + "+1W.tmx")
        Segment2NE = TmxMap (filePath + "+2NE.tmx")
        Segment2NW = TmxMap (filePath + "+2NW.tmx")
        Segment2SE = TmxMap (filePath + "+2SE.tmx")
        Segment2SW = TmxMap (filePath + "+2SW.tmx")
        Segment2NS = TmxMap (filePath + "+2NS.tmx")
        Segment2EW = TmxMap (filePath + "+2EW.tmx")
        Segment3N = TmxMap (filePath + "+3N.tmx")
        Segment3E = TmxMap (filePath + "+3E.tmx")
        Segment3S = TmxMap (filePath + "+3S.tmx")
        Segment3W = TmxMap (filePath + "+3W.tmx")
        Segment4 = TmxMap (filePath + "+4A.tmx") }

type MapRand =
    { MapSegments : SegmentRand array array
      MapOriginOpt : Vector2i option
      MapSize : Vector2i }

    static member printfn map =
        match map.MapOriginOpt with
        | Some start ->
            printfn "Start: %s" (scstring start)
            for i in 0 .. map.MapSize.Y - 1 do
                for j in 0 .. map.MapSize.X - 1 do
                    printf "%s\t" (scstring map.MapSegments.[i].[j])
                printfn ""
        | None -> ()

    static member clone map =
        { MapSegments = Seq.toArray (Array.map Seq.toArray map.MapSegments)
          MapOriginOpt = map.MapOriginOpt
          MapSize = map.MapSize }

    static member concat left right =
        if left.MapOriginOpt = right.MapOriginOpt then
            if left.MapSize = right.MapSize then
                let map = MapRand.clone left
                for i in 0 .. map.MapSize.X - 1 do
                    for j in 0 .. map.MapSize.Y - 1 do
                        map.MapSegments.[i].[j] <- left.MapSegments.[i].[j] ||| right.MapSegments.[i].[j]
                map
            else failwith "Cannot concat two RandMaps of differing sizes."
        else failwith "Cannot concat two RandMaps with different origins."

    static member private walk biasChance bias (cursor : Vector2i) map rand =
        let bounds = v4iBounds v2iZero map.MapSize
        let mutable cursor = cursor
        let (i, rand) = Rand.nextIntUnder 4 rand
        let (chance, rand) = Rand.nextSingleUnder 1.0f rand
        let direction = if chance < biasChance then bias else i
        match direction with
        | 0 ->
            // try go north
            if  cursor.Y > 1 then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1N
                cursor.Y <- dec cursor.Y
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1S
        | 1 ->
            // try go east
            if  cursor.X < dec bounds.Z then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1E
                cursor.X <- inc cursor.X
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1W
        | 2 ->
            // try go south
            if  cursor.Y < dec bounds.W then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1S
                cursor.Y <- inc cursor.Y
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1N
        | 3 ->
            // try go west
            if  cursor.X > 1 then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1W
                cursor.X <- dec cursor.X
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1E
        | _ -> failwithumf ()
        (cursor, rand)

    static member makeFromRand walkLength biasChance (size : Vector2i) origin rand =
        if size.X < 4 || size.Y < 4 then failwith "Invalid MapRand size."
        let bounds = v4iBounds v2iZero size
        let (cursor, biases) =
            match origin with
            | OriginC ->        (bounds.Center,                     [0; 1; 2; 3]) // 0 = n; 1 = e; 2 = s; 3 = w
            | OriginN ->        (bounds.Bottom,                     [2; 2; 1; 3])
            | OriginE ->        (bounds.Right - v2iRight,           [3; 3; 0; 2])
            | OriginS ->        (bounds.Top - v2iUp,                [0; 0; 1; 3])
            | OriginW ->        (bounds.Left,                       [1; 1; 0; 2])
            | OriginNE ->       (v2i (dec bounds.Z) bounds.Y,       [0; 1; 2; 3])
            | OriginNW ->       (v2i bounds.X bounds.Y,             [0; 1; 2; 3])
            | OriginSE ->       (v2i (dec bounds.Z) (dec bounds.W), [0; 1; 2; 3])
            | OriginSW ->       (v2i bounds.X (dec bounds.W),       [0; 1; 2; 3])
        let (maps, rand) =
            List.fold (fun (maps, rand) bias ->
                let map = { MapRand.make size with MapOriginOpt = Some cursor }
                let (_, rand) = List.fold (fun (cursor, rand) _ -> MapRand.walk biasChance bias cursor map rand) (cursor, rand) [0 .. dec walkLength]
                (map :: maps, rand))
                ([], rand)
                biases
        let map = List.reduce MapRand.concat maps
        (map, rand)

    static member make (size : Vector2i) =
        if size.X < 4 || size.Y < 4 then failwith "Invalid MapRand size."
        { MapSegments = Array.init size.X (fun _ -> Array.init size.Y (constant SegmentRand.Segment0))
          MapOriginOpt = None
          MapSize = size }

    static member toTmx abstractPath map =
        let mapTmx = TmxMap (abstractPath + "+7x7.tmx")
        let segments = SegmentsRand.load abstractPath
        for l in 0 .. 2 - 1 do
            mapTmx.Layers.[l].Tiles.Clear ()
            for j in 0 .. 7 - 1 do
                for jj in 0 .. 32 - 1 do
                    for i in 0 .. 7 - 1 do
                        for ii in 0 .. 32 - 1 do
                            let iii = i * 32 + ii
                            let jjj = j * 32 + jj
                            let segment = map.MapSegments.[j].[i]
                            let tileRef =
                                match segment with
                                | SegmentRand.Segment0 -> TmxLayerTile (0u, iii, jjj)
                                | SegmentRand.Segment1N -> segments.Segment1N.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment1E -> segments.Segment1E.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment1S -> segments.Segment1S.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment1W -> segments.Segment1W.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2NE -> segments.Segment2NE.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2NW -> segments.Segment2NW.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2SE -> segments.Segment2SE.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2SW -> segments.Segment2SW.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2EW -> segments.Segment2EW.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment2NS -> segments.Segment2NS.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment3N -> segments.Segment3N.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment3E -> segments.Segment3E.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment3S -> segments.Segment3S.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment3W -> segments.Segment3W.Layers.[l].Tiles.[ii + jj * 32]
                                | SegmentRand.Segment4 -> segments.Segment4.Layers.[l].Tiles.[ii + jj * 32]
                                | _ -> failwithumf ()
                            let tileGidPlus =
                                uint32 tileRef.Gid |||
                                (if tileRef.HorizontalFlip then 0x40000000u else 0x0u) |||
                                (if tileRef.VerticalFlip then 0x20000000u else 0x0u)
                            let tile = TmxLayerTile (tileGidPlus, iii, jjj)
                            mapTmx.Layers.[l].Tiles.Add tile
        mapTmx
