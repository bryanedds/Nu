namespace InfinityRpg
open System
open Prime
open Nu

type FieldTileType =
    | Impassable
    | Passable

type [<ReferenceEquality; NoComparison>] FieldTile =
    { TileSheetPositionM : Vector2i
      TileType : FieldTileType }

type [<ReferenceEquality; NoComparison>] FieldMapUnit =
    { RandSeed : uint64
      OffsetCount : Vector2i
      IsHorizontal : bool
      PathStart : Vector2i
      PathEnd : Vector2i }

    static member make fieldMapUnitOpt =
        let sysrandom = System.Random ()
        let randSeed = uint64 (sysrandom.Next ())
        let randResult = Gen.random1 (Constants.Layout.FieldUnitSizeM.X - 4) // assumes X and Y are equal
        let pathEnd =
            if randResult % 2 = 0
            then v2i (randResult + 2) (Constants.Layout.FieldUnitSizeM.Y - 2)
            else v2i (Constants.Layout.FieldUnitSizeM.X - 2) (randResult + 2)
        let (offsetCount, pathStart) =
            match fieldMapUnitOpt with
            | Some fieldMapUnit ->
                match fieldMapUnit.IsHorizontal with
                | true -> (fieldMapUnit.OffsetCount + v2iRight, v2i 1 fieldMapUnit.PathEnd.Y)
                | false -> (fieldMapUnit.OffsetCount + v2iUp, v2i fieldMapUnit.PathEnd.X 1)
            | None -> (v2iZero, v2iOne)
        { RandSeed = randSeed
          OffsetCount = offsetCount
          IsHorizontal = pathEnd.X > pathEnd.Y
          PathStart = pathStart
          PathEnd = pathEnd }

type [<ReferenceEquality; NoComparison>] FieldMap =
    { FieldSizeM : Vector2i
      FieldTiles : Map<Vector2i, FieldTile>
      FieldTileSheet : Image AssetTag }

[<RequireQualifiedAccess>]
module FieldMap =

    let PathTile = { TileSheetPositionM = v2i 3 0; TileType = Passable }
    let GrassTile = { TileSheetPositionM = v2i 3 3; TileType = Passable }
    let TreeTile = { TileSheetPositionM = v2i 1 1; TileType = Impassable }
    let StoneTile = { TileSheetPositionM = v2i 2 3; TileType = Impassable }
    let WaterTile = { TileSheetPositionM = v2i 0 1; TileType = Impassable }

    let makeGrid boundsM =
        seq {
            for i in boundsM.CornerNegative.X .. boundsM.CornerPositive.X do
                for j in boundsM.CornerNegative.Y .. boundsM.CornerPositive.Y do
                    yield v2i i j }

    let generateEmptyMap (offsetM : Vector2i) (sizeM : Vector2i) =
        Map.ofList
            [for i in offsetM.X .. offsetM.X + sizeM.X - 1 do
                for j in offsetM.Y .. offsetM.Y + sizeM.Y - 1 do
                    let tileCoordsM = v2i i j
                    yield (tileCoordsM, GrassTile)]

    let addPaths buildBoundsM pathEdgesM generatedMap rand =
        
        let (paths, rand) =
            List.fold
                (fun (paths, rand) (sourceM, destinationM) ->
                    let (path, rand) = Direction.wanderToDestination buildBoundsM sourceM destinationM rand
                    (path :: paths, rand))
                ([], rand)
                pathEdgesM

        let generatedMap =
            Seq.fold
                (fun generatedMap path ->
                    let generatedMap' =
                        Seq.fold
                            (fun generatedMap tilePositionM -> Map.add tilePositionM PathTile generatedMap)
                            generatedMap
                            path
                    generatedMap @@ generatedMap')
                generatedMap
                paths

        (generatedMap, rand)

    let addTrees buildBoundsM generatedMap rand =
        let grid = makeGrid buildBoundsM
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        let treeDilution =
            if pathTileCount < 25 then 32
            elif pathTileCount < 50 then 16
            elif pathTileCount < 60 then 8
            elif pathTileCount < 70 then 4
            elif pathTileCount < 80 then 3
            elif pathTileCount < 90 then 2
            else 1
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                let (n, rand) = Rand.nextIntUnder treeDilution rand // original value is 16
                if n = 0 && Map.find positionM generatedMap <> PathTile
                then (Map.add positionM TreeTile generatedMap, rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid

    let spreadTrees buildBoundsM generatedMap rand =
        let originalMap = generatedMap
        let grid = makeGrid buildBoundsM
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                let tile = Map.find positionM originalMap
                if  tile <> PathTile &&
                    MapBounds.isPointInBounds positionM buildBoundsM then
                    let upPositionM = positionM + v2iUp
                    let rightPositionM = positionM + v2iRight
                    let downPositionM = positionM + v2iDown
                    let leftPositionM = positionM + v2iLeft
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM originalMap = TreeTile ||
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM originalMap = TreeTile then
                        let (n, rand) = Rand.nextIntUnder 3 rand
                        if n = 0 then (Map.add positionM TreeTile generatedMap, rand)
                        else (generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid

    let addWater buildBoundsM generatedMap rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 128 rand
                    let upPositionM = positionM + v2iUp
                    let rightPositionM = positionM + v2iRight
                    let downPositionM = positionM + v2iDown
                    let leftPositionM = positionM + v2iLeft
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM generatedMap = GrassTile &&
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM generatedMap = GrassTile then
                        if n = 0 && Map.find positionM generatedMap = GrassTile
                        then (Map.add positionM WaterTile generatedMap, rand)
                        else (generatedMap, rand)
                    else (generatedMap, rand))
                (generatedMap, rand)
                grid
        else (generatedMap, rand)

    let spreadWater1 buildBoundsM generatedMap rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 2 rand
                    let upPositionM = positionM + v2iUp
                    let rightPositionM = positionM + v2iRight
                    let downPositionM = positionM + v2iDown
                    let leftPositionM = positionM + v2iLeft
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM generatedMap = WaterTile ||
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM generatedMap = WaterTile then
                        if n = 0 && Map.find positionM generatedMap <> PathTile
                        then (Map.add positionM WaterTile generatedMap, rand)
                        else (generatedMap, rand)
                    else (generatedMap, rand))
                (generatedMap, rand)
                grid
        else (generatedMap, rand)

    let spreadWater2 buildBoundsM generatedMap rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) generatedMap |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsM
            let originalMap = generatedMap
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 1 rand
                    let upPositionM = positionM + v2iUp
                    let rightPositionM = positionM + v2iRight
                    let downPositionM = positionM + v2iDown
                    let leftPositionM = positionM + v2iLeft
                    if  MapBounds.isPointInBounds upPositionM buildBoundsM && Map.find upPositionM originalMap = WaterTile ||
                        MapBounds.isPointInBounds rightPositionM buildBoundsM && Map.find rightPositionM originalMap = WaterTile ||
                        MapBounds.isPointInBounds downPositionM buildBoundsM && Map.find downPositionM originalMap = WaterTile ||
                        MapBounds.isPointInBounds leftPositionM buildBoundsM && Map.find leftPositionM originalMap = WaterTile then
                        if n = 0 && Map.find positionM generatedMap <> PathTile
                        then (Map.add positionM WaterTile generatedMap, rand)
                        else (generatedMap, rand)
                    else (generatedMap, rand))
                (generatedMap, rand)
                grid
        else (generatedMap, rand)
    
    let addStones buildBoundsM generatedMap rand =
        let grid = makeGrid buildBoundsM
        Seq.fold
            (fun (generatedMap, rand) positionM ->
                if Map.find positionM generatedMap = GrassTile then
                    let upPositionM = positionM + v2iUp
                    let rightPositionM = positionM + v2iRight
                    let downPositionM = positionM + v2iDown
                    let leftPositionM = positionM + v2iLeft
                    if  Map.find upPositionM generatedMap = PathTile &&
                        Map.find rightPositionM generatedMap = PathTile &&
                        Map.find downPositionM generatedMap = PathTile &&
                        Map.find leftPositionM generatedMap = PathTile then
                        (Map.add positionM StoneTile generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand)
                else (generatedMap, Rand.advance rand))
            (generatedMap, rand)
            grid
    
    let make tileSheet (offsetM : Vector2i) sizeM pathEdgesM rand =
        let buildBoundsM = { CornerNegative = offsetM + v2iOne; CornerPositive = offsetM + sizeM - v2iOne * 2 }
        let generatedMap = generateEmptyMap offsetM sizeM
        let (generatedMap, rand) = addPaths buildBoundsM pathEdgesM generatedMap rand
        let (generatedMap, rand) = addTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadTrees buildBoundsM generatedMap rand
        let (generatedMap, rand) = addWater buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater1 buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater1 buildBoundsM generatedMap rand
        let (generatedMap, rand) = spreadWater2 buildBoundsM generatedMap rand
        let (generatedMap, rand) = addStones buildBoundsM generatedMap rand
        let fieldMap = { FieldSizeM = sizeM; FieldTiles = generatedMap; FieldTileSheet = tileSheet }
        (fieldMap, rand)

    let makeFromFieldMapUnit fieldMapUnit =
        let rand = Rand.makeFromSeedState fieldMapUnit.RandSeed
        make Assets.FieldTileSheetImage v2iZero Constants.Layout.FieldUnitSizeM [(fieldMapUnit.PathStart, fieldMapUnit.PathEnd)] rand |> fst