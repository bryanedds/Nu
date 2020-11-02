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

    static member getSegmentOpt segment segments =
        match segment with
        | SegmentRand.Segment0 -> None
        | SegmentRand.Segment1N -> Some segments.Segment1N
        | SegmentRand.Segment1E -> Some segments.Segment1E
        | SegmentRand.Segment1S -> Some segments.Segment1S
        | SegmentRand.Segment1W -> Some segments.Segment1W
        | SegmentRand.Segment2NE -> Some segments.Segment2NE
        | SegmentRand.Segment2NW -> Some segments.Segment2NW
        | SegmentRand.Segment2SE -> Some segments.Segment2SE
        | SegmentRand.Segment2SW -> Some segments.Segment2SW
        | SegmentRand.Segment2EW -> Some segments.Segment2EW
        | SegmentRand.Segment2NS -> Some segments.Segment2NS
        | SegmentRand.Segment3N -> Some segments.Segment3N
        | SegmentRand.Segment3E -> Some segments.Segment3E
        | SegmentRand.Segment3S -> Some segments.Segment3S
        | SegmentRand.Segment3W -> Some segments.Segment3W
        | SegmentRand.Segment4 -> Some segments.Segment4
        | _ -> failwithumf ()

    static member private walk biasChance bias (cursor : Vector2i) map rand =
        let bounds = v4iBounds v2iZero map.MapSize
        let mutable cursor = cursor
        let (i, rand) = Rand.nextIntUnder 4 rand
        let (chance, rand) = Rand.nextSingleUnder 1.0f rand
        let direction = if chance < biasChance then bias else i
        match direction with
        | 0 ->
            // try go north (negative y)
            if  cursor.Y > 1 then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1N
                cursor.Y <- dec cursor.Y
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1S
        | 1 ->
            // try go east (positive x)
            if  cursor.X < dec bounds.Z then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1E
                cursor.X <- inc cursor.X
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1W
        | 2 ->
            // try go south (positive y)
            if  cursor.Y < dec bounds.W then
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1S
                cursor.Y <- inc cursor.Y
                map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| SegmentRand.Segment1N
        | 3 ->
            // try go west (negative x)
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
        let openingOpt =
            match origin with
            | OriginC -> None
            | OriginN -> Some SegmentRand.Segment1N
            | OriginE -> Some SegmentRand.Segment1E
            | OriginS -> Some SegmentRand.Segment1S
            | OriginW -> Some SegmentRand.Segment1W
            | OriginNE -> Some SegmentRand.Segment1N
            | OriginNW -> Some SegmentRand.Segment1W
            | OriginSE -> Some SegmentRand.Segment1E
            | OriginSW -> Some SegmentRand.Segment1S
        match openingOpt with
        | Some opening -> map.MapSegments.[cursor.Y].[cursor.X] <- map.MapSegments.[cursor.Y].[cursor.X] ||| opening
        | None -> ()
        (cursor, map, rand)

    static member make (size : Vector2i) =
        if size.X < 4 || size.Y < 4 then failwith "Invalid MapRand size."
        { MapSegments = Array.init size.X (fun _ -> Array.init size.Y (constant SegmentRand.Segment0))
          MapOriginOpt = None
          MapSize = size }

    static member toTmx abstractPath origin (cursor : Vector2i) map =

        // locals
        let mapTmx = TmxMap (abstractPath + "+7x7.tmx")
        let objects = mapTmx.ObjectGroups.[0].Objects
        let segments = SegmentsRand.load abstractPath

        // configure opening prop
        let (openingX, openingY, openingWidth, openingHeight, openingInfo) =
            match origin with
            | OriginC ->    (15 * mapTmx.TileWidth, 15 * mapTmx.TileHeight, mapTmx.TileWidth * 2, mapTmx.TileHeight * 2, "[Portal Center Downward TombInner [IX 8]]")
            | OriginN ->    (14 * mapTmx.TileWidth, 31 * mapTmx.TileHeight, mapTmx.TileWidth * 4, mapTmx.TileHeight * 1, "[Portal North Downward TombInner [IX 7]]")
            | OriginE ->    (31 * mapTmx.TileWidth, 14 * mapTmx.TileHeight, mapTmx.TileWidth * 1, mapTmx.TileHeight * 4, "[Portal East Leftward TombInner [IX 6]]")
            | OriginS ->    (14 * mapTmx.TileWidth, 0  * mapTmx.TileHeight, mapTmx.TileWidth * 4, mapTmx.TileHeight * 1, "[Portal South Upward TombInner [IX 5]]")
            | OriginW ->    (0  * mapTmx.TileWidth, 14 * mapTmx.TileHeight, mapTmx.TileWidth * 1, mapTmx.TileHeight * 4, "[Portal West Rightward TombInner [IX 4]]")
            | OriginNE ->   (14 * mapTmx.TileWidth, 31 * mapTmx.TileHeight, mapTmx.TileWidth * 4, mapTmx.TileHeight * 1, "[Portal South Downward TombInner [IX 3]]")
            | OriginNW ->   (0  * mapTmx.TileWidth, 14 * mapTmx.TileHeight, mapTmx.TileWidth * 1, mapTmx.TileHeight * 4, "[Portal West Rightward TombInner [IX 2]]")
            | OriginSE ->   (31 * mapTmx.TileWidth, 14 * mapTmx.TileHeight, mapTmx.TileWidth * 1, mapTmx.TileHeight * 4, "[Portal East Leftward TombInner [IX 1]]")
            | OriginSW ->   (14 * mapTmx.TileWidth, 0  * mapTmx.TileHeight, mapTmx.TileWidth * 4, mapTmx.TileHeight * 1, "[Portal South Upward TombInner [IX 0]]")
        let openingXX = openingX + cursor.X * mapTmx.TileWidth * 32
        let openingYY = openingY + inc cursor.Y * mapTmx.TileHeight * 32
        let object = objects.[0]
        let objectType = typeof<TmxObject>
        (objectType.GetProperties "Id").[0].SetValue (object, 0)
        (objectType.GetProperties "X").[0].SetValue (object, openingXX)
        (objectType.GetProperties "Y").[0].SetValue (object, openingYY)
        (objectType.GetProperties "Width").[0].SetValue (object, openingWidth)
        (objectType.GetProperties "Height").[0].SetValue (object, openingHeight)
        object.Properties.Add ("I", openingInfo)

        // add objects from segments
        for i in 0 .. 7 - 1 do
            for j in 0 .. 7 - 1 do
                match MapRand.getSegmentOpt map.MapSegments.[j].[i] segments with
                | Some segment ->
                    if segment.ObjectGroups.Count <> 0 then
                        for object in segment.ObjectGroups.[0].Objects do
                            ((getType object).GetProperties "Id").[0].SetValue (object, i + j * 7)
                            objects.Add object
                | None -> ()

        // add tiles from segments
        for l in 0 .. 2 - 1 do
            let layer = mapTmx.Layers.[l]
            layer.Tiles.Clear ()
            for j in 0 .. 7 - 1 do
                for jj in 0 .. 32 - 1 do
                    for i in 0 .. 7 - 1 do
                        for ii in 0 .. 32 - 1 do
                            let x = i * 32 + ii
                            let y = j * 32 + jj
                            let tileRef =
                                match MapRand.getSegmentOpt map.MapSegments.[j].[i] segments with
                                | Some segment -> segment.Layers.[l].Tiles.[ii + jj * 32]
                                | None -> TmxLayerTile (0u, x, y)
                            let tileGidPlus =
                                uint32 tileRef.Gid |||
                                (if tileRef.HorizontalFlip then 0x40000000u else 0x0u) |||
                                (if tileRef.VerticalFlip then 0x20000000u else 0x0u)
                            let tile = TmxLayerTile (tileGidPlus, x, y)
                            layer.Tiles.Add tile

        // le map tmx
        mapTmx
