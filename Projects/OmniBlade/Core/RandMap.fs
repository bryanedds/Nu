// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open TiledSharp
open Prime
open Nu

type Origin =
    | OriginC
    | OriginN
    | OriginE
    | OriginS
    | OriginW
    | OriginNE
    | OriginNW
    | OriginSE
    | OriginSW

    static member approximatePosition (perimeter : Box2) origin =
        match origin with
        | OriginC -> perimeter.Center
        | OriginN -> perimeter.Top
        | OriginE -> perimeter.Right
        | OriginS -> perimeter.Bottom
        | OriginW -> perimeter.Left
        | OriginNE -> perimeter.TopRight
        | OriginNW -> perimeter.TopLeft
        | OriginSE -> perimeter.BottomRight
        | OriginSW -> perimeter.BottomLeft

type Segment =
    | Segment0 = 0b000000
    | Segment1N = 0b000001
    | Segment1E = 0b000010
    | Segment1S = 0b000100
    | Segment1W = 0b001000
    | Segment2NE = 0b000011
    | Segment2NW = 0b001001
    | Segment2SE = 0b000110
    | Segment2SW = 0b001100
    | Segment2EW = 0b001010
    | Segment2NS = 0b000101
    | Segment3N = 0b001011
    | Segment3E = 0b000111
    | Segment3S = 0b001110
    | Segment3W = 0b001101
    | Segment4 = 0b001111
    | SegmentNN = 0b010001
    | SegmentNS = 0b010100
    | SegmentBN = 0b100001
    | SegmentBS = 0b100100

[<RequireQualifiedAccess>]
module Segment =

    let isNarrative (segment : Segment) =
        int segment &&& 0b010000 <> 0

    let isBoss (segment : Segment) =
        int segment &&& 0b100000 <> 0

    let isSpecial (segment : Segment) =
        isNarrative segment &&
        isBoss segment

    let notSpecial segment =
        not (isNarrative segment) &&
        not (isBoss segment)

    let isEmpty (segment : Segment) =
        int segment = 0

    let notEmpty (segment : Segment) =
        int segment <> 0

type SpecialSegment =
    | NarrativeSegment
    | BossSegment

type Segments =
    { Segment0 : TmxMap
      Segment1N : TmxMap
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
      Segment4 : TmxMap
      SegmentNN : TmxMap
      SegmentNS : TmxMap
      SegmentBN : TmxMap
      SegmentBS : TmxMap
      SegmentF : TmxMap }

    static member load (floor : int) useAlternativeSegments (filePath : string) =
        let suffix = if useAlternativeSegments then "+B" else ""
        { Segment0 = TmxMap (filePath + "+0A" + suffix + ".tmx")
          Segment1N = TmxMap (filePath + "+1N" + suffix + ".tmx")
          Segment1E = TmxMap (filePath + "+1E" + suffix + ".tmx")
          Segment1S = TmxMap (filePath + "+1S" + suffix + ".tmx")
          Segment1W = TmxMap (filePath + "+1W" + suffix + ".tmx")
          Segment2NE = TmxMap (filePath + "+2NE" + suffix + ".tmx")
          Segment2NW = TmxMap (filePath + "+2NW" + suffix + ".tmx")
          Segment2SE = TmxMap (filePath + "+2SE" + suffix + ".tmx")
          Segment2SW = TmxMap (filePath + "+2SW" + suffix + ".tmx")
          Segment2NS = TmxMap (filePath + "+2NS" + suffix + ".tmx")
          Segment2EW = TmxMap (filePath + "+2EW" + suffix + ".tmx")
          Segment3N = TmxMap (filePath + "+3N" + suffix + ".tmx")
          Segment3E = TmxMap (filePath + "+3E" + suffix + ".tmx")
          Segment3S = TmxMap (filePath + "+3S" + suffix + ".tmx")
          Segment3W = TmxMap (filePath + "+3W" + suffix + ".tmx")
          Segment4 = TmxMap (filePath + "+4A" + suffix + ".tmx")
          SegmentNN = TmxMap (filePath + "+NN+" + string floor + suffix + ".tmx")
          SegmentNS = TmxMap (filePath + "+NS+" + string floor + suffix + ".tmx")
          SegmentBN = TmxMap (filePath + "+BN+" + string floor + suffix + ".tmx")
          SegmentBS = TmxMap (filePath + "+BS+" + string floor + suffix + ".tmx")
          SegmentF = TmxMap (filePath + "+F" + suffix + ".tmx") }

type RandMap =
    { RandMapSize : Vector2i
      Segments : Segment array array
      OriginOpt : Vector2i option }

    static member printn map =
        match map.OriginOpt with
        | Some start ->
            printfn "Start: %s" (scstring start)
            for i in 0 .. map.RandMapSize.Y - 1 do
                for j in 0 .. map.RandMapSize.X - 1 do
                    printf "%s\t" (scstring map.Segments.[i].[j])
                printfn ""
        | None -> ()

    static member clone map =
        { RandMapSize = map.RandMapSize
          Segments = Seq.toArray (Array.map Seq.toArray map.Segments)
          OriginOpt = map.OriginOpt }

    static member concat left right =
        if left.OriginOpt = right.OriginOpt then
            if left.RandMapSize = right.RandMapSize then
                let map = RandMap.clone left
                for i in 0 .. map.RandMapSize.X - 1 do
                    for j in 0 .. map.RandMapSize.Y - 1 do
                        map.Segments.[i].[j] <- left.Segments.[i].[j] ||| right.Segments.[i].[j]
                map
            else failwith "Cannot concat two RandMaps of differing sizes."
        else failwith "Cannot concat two RandMaps of differing origins."

    static member getSegmentOpt segment segments =
        match segment with
        | Segment.Segment0 -> Some segments.SegmentF
        | Segment.Segment1N -> Some segments.Segment1N
        | Segment.Segment1E -> Some segments.Segment1E
        | Segment.Segment1S -> Some segments.Segment1S
        | Segment.Segment1W -> Some segments.Segment1W
        | Segment.Segment2NE -> Some segments.Segment2NE
        | Segment.Segment2NW -> Some segments.Segment2NW
        | Segment.Segment2SE -> Some segments.Segment2SE
        | Segment.Segment2SW -> Some segments.Segment2SW
        | Segment.Segment2EW -> Some segments.Segment2EW
        | Segment.Segment2NS -> Some segments.Segment2NS
        | Segment.Segment3N -> Some segments.Segment3N
        | Segment.Segment3E -> Some segments.Segment3E
        | Segment.Segment3S -> Some segments.Segment3S
        | Segment.Segment3W -> Some segments.Segment3W
        | Segment.Segment4 -> Some segments.Segment4
        | Segment.SegmentNN -> Some segments.SegmentNN
        | Segment.SegmentNS -> Some segments.SegmentNS
        | Segment.SegmentBN -> Some segments.SegmentBN
        | Segment.SegmentBS -> Some segments.SegmentBS
        | _ -> failwithumf ()

    static member private walk biasChance bias (cursor : Vector2i) map rand =
        let bounds = box2i v2iZero map.RandMapSize
        let mutable cursor = cursor
        let (i, rand) = Rand.nextIntUnder 4 rand
        let (chance, rand) = Rand.nextSingleUnder 1.0f rand
        let direction = if chance < biasChance then bias else i
        match direction with
        | 0 ->
            // try go north (negative y)
            if  cursor.Y > 1 then
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1N
                cursor.Y <- dec cursor.Y
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1S
        | 1 ->
            // try go east (positive x)
            if  cursor.X < dec bounds.Size.X then
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1E
                cursor.X <- inc cursor.X
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1W
        | 2 ->
            // try go south (positive y)
            if  cursor.Y < dec bounds.Size.Y then
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1S
                cursor.Y <- inc cursor.Y
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1N
        | 3 ->
            // try go west (negative x)
            if  cursor.X > 1 then
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1W
                cursor.X <- dec cursor.X
                map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| Segment.Segment1E
        | _ -> failwithumf ()
        (cursor, rand)

    static member tryAddSpecialRoomSouthFromNorthWest specialSegment map =
        let mutable bossRoomAdded = false
        for i in 0 .. dec Constants.Field.RandMapSize.X do // starting from the north row
            if not bossRoomAdded then
                for j in 0 .. dec Constants.Field.RandMapSize.Y do // travel east
                    if not bossRoomAdded then
                        if  i > 0 &&
                            map.Segments.[i].[j] |> Segment.notEmpty &&
                            map.Segments.[i].[j] |> Segment.notSpecial &&
                            map.Segments.[dec i].[j] |> Segment.isEmpty then
                            map.Segments.[i].[j] <- map.Segments.[i].[j] ||| Segment.Segment1N
                            map.Segments.[dec i].[j] <-
                                match specialSegment with
                                | NarrativeSegment -> Segment.SegmentNS
                                | BossSegment -> Segment.SegmentBS
                            bossRoomAdded <- true
        bossRoomAdded

    static member tryAddSpecialRoomSouthFromNorthEast specialSegment map =
        let mutable bossRoomAdded = false
        for i in 0 .. dec Constants.Field.RandMapSize.X do // starting from the north row
            if not bossRoomAdded then
                for j in dec Constants.Field.RandMapSize.Y .. - 1 .. 0 do // travel west
                    if not bossRoomAdded then
                        if  i > 0 &&
                            map.Segments.[i].[j] |> Segment.notEmpty &&
                            map.Segments.[i].[j] |> Segment.notSpecial &&
                            map.Segments.[dec i].[j] |> Segment.isEmpty then
                            map.Segments.[i].[j] <- map.Segments.[i].[j] ||| Segment.Segment1N
                            map.Segments.[dec i].[j] <-
                                match specialSegment with
                                | NarrativeSegment -> Segment.SegmentNS
                                | BossSegment -> Segment.SegmentBS
                            bossRoomAdded <- true
        bossRoomAdded

    static member tryAddSpecialRoomNorthFromSouthWest specialSegment map =
        let mutable bossRoomAdded = false
        for i in dec Constants.Field.RandMapSize.X .. - 1 .. 0 do // starting from the south row
            if not bossRoomAdded then
                for j in 0 .. dec Constants.Field.RandMapSize.Y do // travel east
                    if not bossRoomAdded then
                        if  i < dec Constants.Field.RandMapSize.X &&
                            map.Segments.[i].[j] |> Segment.notEmpty &&
                            map.Segments.[i].[j] |> Segment.notSpecial &&
                            map.Segments.[inc i].[j] |> Segment.isEmpty then
                            map.Segments.[i].[j] <- map.Segments.[i].[j] ||| Segment.Segment1S
                            map.Segments.[inc i].[j] <-
                                match specialSegment with
                                | NarrativeSegment -> Segment.SegmentNN
                                | BossSegment -> Segment.SegmentBN
                            bossRoomAdded <- true
        bossRoomAdded

    static member tryAddSpecialRoomNorthFromSouthEast specialSegment map =
        let mutable bossRoomAdded = false
        for i in dec Constants.Field.RandMapSize.X .. - 1 .. 0 do // starting from the south row
            if not bossRoomAdded then
                for j in dec Constants.Field.RandMapSize.Y .. - 1 .. 0 do // travel west
                    if not bossRoomAdded then
                        if  i < dec Constants.Field.RandMapSize.X &&
                            map.Segments.[i].[j] |> Segment.notEmpty &&
                            map.Segments.[i].[j] |> Segment.notSpecial &&
                            map.Segments.[inc i].[j] |> Segment.isEmpty then
                            map.Segments.[i].[j] <- map.Segments.[i].[j] ||| Segment.Segment1S
                            map.Segments.[inc i].[j] <-
                                match specialSegment with
                                | NarrativeSegment -> Segment.SegmentNN
                                | BossSegment -> Segment.SegmentBN
                            bossRoomAdded <- true
        bossRoomAdded

    static member makeFromRand walkCount walkLength biasChance (size : Vector2i) origin floor rand =
        if size.X < 4 || size.Y < 4 then failwith "Invalid RandMap size."
        let bounds = box2i v2iZero size
        let (cursor, biases) =
            match origin with
            | OriginC ->        (bounds.Center,                                 [0; 1; 2; 3]) // 0 = n; 1 = e; 2 = s; 3 = w
            | OriginN ->        (bounds.Bottom,                                 [2; 1; 3])
            | OriginE ->        (bounds.Right - v2iRight,                       [3; 0; 2])
            | OriginS ->        (bounds.Top - v2iUp,                            [0; 1; 3])
            | OriginW ->        (bounds.Left,                                   [1; 0; 2])
            | OriginNE ->       (v2i (dec bounds.Size.X) bounds.Min.Y,          [2; 3])
            | OriginNW ->       (v2i bounds.Min.X bounds.Min.Y,                 [2; 1])
            | OriginSE ->       (v2i (dec bounds.Size.X) (dec bounds.Size.Y),   [0; 3])
            | OriginSW ->       (v2i bounds.Min.X (dec bounds.Size.Y),          [0; 1])
        let biases = Seq.initInfinite (constant biases) |> Seq.concat |> Seq.take walkCount
        let (maps, rand) =
            Seq.fold (fun (maps, rand) bias ->
                let map = { RandMap.make size with OriginOpt = Some cursor }
                let (_, rand) = List.fold (fun (cursor, rand) _ -> RandMap.walk biasChance bias cursor map rand) (cursor, rand) [0 .. dec walkLength]
                (map :: maps, rand))
                ([], rand)
                biases
        let (maps, rand) = Rand.nextPermutation maps rand
        let maps = List.take 3 maps
        let map = List.reduce RandMap.concat maps
        let opening =
            if floor = 0 then
                match origin with
                | OriginC ->    Segment.Segment0
                | OriginE ->    Segment.Segment1E
                | OriginW ->    Segment.Segment1W
                | OriginN ->    Segment.Segment1N
                | OriginNE ->   Segment.Segment1N
                | OriginNW ->   Segment.Segment1N
                | OriginS ->    Segment.Segment1S
                | OriginSE ->   Segment.Segment1S
                | OriginSW ->   Segment.Segment1S
            else Segment.Segment0
        map.Segments.[cursor.Y].[cursor.X] <- map.Segments.[cursor.Y].[cursor.X] ||| opening
        let (tryAddBoss, rand) =
            Rand.nextItem
                [RandMap.tryAddSpecialRoomNorthFromSouthEast BossSegment
                 RandMap.tryAddSpecialRoomNorthFromSouthWest BossSegment
                 RandMap.tryAddSpecialRoomSouthFromNorthEast BossSegment
                 RandMap.tryAddSpecialRoomSouthFromNorthWest BossSegment]
                rand
        let enoughRooms walkLength (map : RandMap) =
            let mutable roomCount = 0
            for i in 0 .. map.Segments.Length - 1 do
                for j in 0 .. map.Segments.[i].Length - 1 do
                    if map.Segments.[i].[j] <> Segment.Segment0 then
                        roomCount <- inc roomCount
            let roomMin = walkLength + int (Operators.floor (single walkLength * 0.667f))
            let roomMax = walkLength + int (Operators.ceil (single walkLength * 1.0f))
            roomCount >= roomMin && roomCount <= roomMax
        let noContiguousSameRooms (map : RandMap) =
            let mutable found = false
            for i in 0 .. map.Segments.Length - 2 do
                for j in 0 .. map.Segments.[i].Length - 1 do
                    if  map.Segments.[i].[j] <> Segment.Segment0 &&
                        map.Segments.[i].[j] = map.Segments.[inc i].[j] then
                        found <- true
            for i in 0 .. map.Segments.Length - 1 do
                for j in 0 .. map.Segments.[i].Length - 2 do
                    if  map.Segments.[i].[j] <> Segment.Segment0 &&
                        map.Segments.[i].[j] = map.Segments.[i].[inc j] then
                        found <- true
            not found
        let isMapValid =
            RandMap.tryAddSpecialRoomSouthFromNorthWest NarrativeSegment map &&
            RandMap.tryAddSpecialRoomNorthFromSouthEast NarrativeSegment map &&
            tryAddBoss map &&
            enoughRooms walkLength map &&
            noContiguousSameRooms map
#if DEV
        printfn "Floor: %i" floor
        RandMap.printn map
#endif
        if not isMapValid // make another if no valid map could be created
        then RandMap.makeFromRand walkCount walkLength biasChance size origin floor rand
        else (cursor, map, rand)

    static member make (size : Vector2i) =
        if size.X < 4 || size.Y < 4 then failwith "Invalid RandMap size."
        { RandMapSize = size
          Segments = Array.init size.X (fun _ -> Array.init size.Y (constant Segment.Segment0))
          OriginOpt = None }

    static member toTmx fieldName abstractPath origin (cursor : Vector2i) floor useWindPortal useAlternativeSegments map =

        // locals
        let mapTmx = TmxMap (abstractPath + "+9x9.tmx")
        let objects = mapTmx.ObjectGroups.[0].Objects
        let segments = Segments.load floor useAlternativeSegments abstractPath
        let slop = 12 // extra translation so that collision doesn't happen due to portal being directly aligned with a wall
        let entryId = 0
        
        // create entry prop when needed
        // NOTE: this code is broken for origins other than south. It's hard to get right because the Y coordinate in tmx is flipped.
        // TODO: pull out some graph paper and fix this code taking into account the Y frame changes required by tmx.
        if floor = 0 then
            let (openingX, openingY, openingWidth, openingHeight, openingInfo) =
                match origin with
                | OriginC ->    (15 * mapTmx.TileWidth,         15 * mapTmx.TileHeight,         mapTmx.TileWidth * 2,  mapTmx.TileHeight * 2,  "[Portal AirPortal [IX 0] Downward " + fieldName + "Connector [IX 0]]")
                | OriginE ->    (31 * mapTmx.TileWidth + slop,  7  * mapTmx.TileHeight,         mapTmx.TileWidth * 1,  mapTmx.TileHeight * 18, "[Portal AirPortal [IX 0] Leftward " + fieldName + "Connector [IX 0]]")
                | OriginW ->    (0  * mapTmx.TileWidth - slop,  7  * mapTmx.TileHeight,         mapTmx.TileWidth * 1,  mapTmx.TileHeight * 18, "[Portal AirPortal [IX 0] Rightward " + fieldName + "Connector [IX 0]]")
                | OriginN ->    (7  * mapTmx.TileWidth,         31 * mapTmx.TileHeight - slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Downward " + fieldName + "Connector [IX 0]]")
                | OriginNE ->   (7  * mapTmx.TileWidth,         31 * mapTmx.TileHeight - slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Downward " + fieldName + "Connector [IX 0]]")
                | OriginNW ->   (7  * mapTmx.TileWidth,         31 * mapTmx.TileHeight - slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Downward " + fieldName + "Connector [IX 0]]")
                | OriginS ->    (7  * mapTmx.TileWidth,         0  * mapTmx.TileHeight + slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Upward " + fieldName + "Connector [IX 0]]")
                | OriginSE ->   (7  * mapTmx.TileWidth,         0  * mapTmx.TileHeight + slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Upward " + fieldName + "Connector [IX 0]]")
                | OriginSW ->   (7  * mapTmx.TileWidth,         0  * mapTmx.TileHeight + slop,  mapTmx.TileWidth * 18, mapTmx.TileHeight * 1,  "[Portal AirPortal [IX 0] Upward " + fieldName + "Connector [IX 0]]")
            let openingXX = openingX + cursor.X * mapTmx.TileWidth * Constants.Field.RoomSize.X
            let openingYY = openingY + inc cursor.Y * mapTmx.TileHeight * Constants.Field.RoomSize.Y
            let object = TmxMap.makeObject entryId 0 (double openingXX) (double openingYY) (double openingWidth) (double openingHeight)
            object.Properties.Add ("I", openingInfo)
            objects.[0] <- object

        // add objects from segments
        // HACK: except chest spawns at origin!
        let mutable propId = inc entryId
        let mutable stairsCreated = false
        let stairsInfo = "[Portal [StairsPortal True " + string useWindPortal + "] [IX 1] Upward [" + fieldName + " " + string (dec floor) + "] [IX 2]]"
        for i in 0 .. dec Constants.Field.RandMapSize.X do
            for j in 0 .. dec Constants.Field.RandMapSize.Y do
                match RandMap.getSegmentOpt map.Segments.[j].[i] segments with
                | Some segment ->
                    if segment.ObjectGroups.Count <> 0 then
                        for objectRef in segment.ObjectGroups.[0].Objects do
                            if  not stairsCreated &&
                                floor > 0 &&
                                i = cursor.X &&
                                j = cursor.Y &&
                                objectRef.Properties.ContainsKey "S" then
                                let x = objectRef.X + double i * double Constants.Field.RoomSize.X * double mapTmx.TileWidth
                                let y = objectRef.Y + double j * double Constants.Field.RoomSize.Y * double mapTmx.TileHeight
                                let w = mapTmx.TileWidth
                                let h = mapTmx.TileHeight
                                let object = TmxMap.makeObject entryId 0 (double x) (double y) (double w) (double h)
                                object.Properties.Add ("I", stairsInfo)
                                objects.[0] <- object
                                stairsCreated <- true
                            match objectRef.Properties.TryGetValue "I" with
                            | (true, propStr) when i = cursor.X && j = cursor.Y && propStr.Contains "ChestSpawn" ->
                                () // do not spawn in origin section
                            | (true, propStr) when propStr.Contains "PortalSpawn" ->
                                let x = objectRef.X + double i * double Constants.Field.RoomSize.X * double mapTmx.TileWidth
                                let y = objectRef.Y + double j * double Constants.Field.RoomSize.Y * double mapTmx.TileHeight
                                let object = TmxMap.makeObject propId 0 x y objectRef.Width objectRef.Height
                                let portalIndex = "[Room " + string i + " " + string j + "]"
                                let portalInfo = "[Portal AirPortal " + portalIndex + " Downward [" + fieldName + "Room " + string floor + " " + string i + " " + string j + "] South]"
                                object.Properties.Add ("I", portalInfo)
                                propId <- inc propId
                                objects.Add object
                            | (_, _) ->
                                let x = objectRef.X + double i * double Constants.Field.RoomSize.X * double mapTmx.TileWidth
                                let y = objectRef.Y + double j * double Constants.Field.RoomSize.Y * double mapTmx.TileHeight
                                let object = TmxMap.makeObject propId 0 x y objectRef.Width objectRef.Height
                                for propertyKvp in objectRef.Properties do object.Properties.Add (propertyKvp.Key, propertyKvp.Value)
                                propId <- inc propId
                                objects.Add object
                | None -> ()

        // add stairs in default location if none created yet
        if floor > 0 && not stairsCreated then
            let (stairsX, stairsY, stairsWidth, stairsHeight) = (16 * mapTmx.TileWidth, 16 * mapTmx.TileHeight, mapTmx.TileWidth, mapTmx.TileHeight)
            let stairsXX = stairsX + cursor.X * mapTmx.TileWidth * Constants.Field.RoomSize.X
            let stairsYY = stairsY + cursor.Y * mapTmx.TileHeight * Constants.Field.RoomSize.Y
            let object = TmxMap.makeObject entryId 0 (double stairsXX) (double stairsYY) (double stairsWidth) (double stairsHeight)
            object.Properties.Add ("I", stairsInfo)
            objects.[0] <- object

        // add tiles from segments
        for l in 0 .. mapTmx.TileLayers.Count - 1 do
            let layer = mapTmx.TileLayers.[l]
            for j in 0 .. dec Constants.Field.RandMapSize.Y do
                for jj in 0 .. dec Constants.Field.RoomSize.Y do
                    for i in 0 .. dec Constants.Field.RandMapSize.X do
                        for ii in 0 .. dec Constants.Field.RoomSize.X do
                            let x = i * Constants.Field.RoomSize.X + ii
                            let y = j * Constants.Field.RoomSize.Y + jj
                            let tileFromSegment =
                                match RandMap.getSegmentOpt map.Segments.[j].[i] segments with
                                | Some segment ->
                                    match Seq.tryFind (fun (segmentLayer : TmxLayer) -> segmentLayer.Name = layer.Name) segment.TileLayers with
                                    | Some segmentLayer -> segmentLayer.Tiles.[ii + jj * Constants.Field.RoomSize.X]
                                    | None -> TmxLayerTile 0u
                                | Some _ | None -> TmxLayerTile 0u
                            let tile = TmxMap.makeLayerTile tileFromSegment.Gid tileFromSegment.HorizontalFlip tileFromSegment.VerticalFlip tileFromSegment.DiagonalFlip
                            layer.Tiles.[x + y * Constants.Field.RoomSize.X * Constants.Field.RandMapSize.X] <- tile

        // le map tmx
        mapTmx