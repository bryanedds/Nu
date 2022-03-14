namespace InfinityRpg
open System
open Prime
open Nu

type [<StructuralEquality; NoComparison>] FieldTileType =
    | Impassable
    | Passable

type [<ReferenceEquality; NoComparison>] FieldTile =
    { TileSheetCoordinates : Vector2i
      TileType : FieldTileType }

type [<ReferenceEquality; NoComparison>] FieldMap =
    { FieldSizeC : Vector2i
      FieldTiles : Map<Vector2i, FieldTile>
      FieldTileSheet : Image AssetTag }

[<RequireQualifiedAccess>]
module FieldMap =

    let PathTile = { TileSheetCoordinates = v2i 3 0; TileType = Passable }
    let GrassTile = { TileSheetCoordinates = v2i 3 3; TileType = Passable }
    let TreeTile = { TileSheetCoordinates = v2i 1 1; TileType = Impassable }
    let StoneTile = { TileSheetCoordinates = v2i 2 3; TileType = Impassable }
    let WaterTile = { TileSheetCoordinates = v2i 0 1; TileType = Impassable }

    let getTileInDirection coordinates direction =
        match direction with
        | Upward -> coordinates + v2iUp
        | Rightward -> coordinates + v2iRight
        | Downward -> coordinates + v2iDown
        | Leftward -> coordinates + v2iLeft

    let getAdjacentTiles coordinates =
        [coordinates + v2iUp
         coordinates + v2iRight
         coordinates + v2iDown
         coordinates + v2iLeft]
    
    let hasAtLeastNAdjacentTiles n coordinates tile buildBoundsC fieldTiles =
        let passingTiles =
            coordinates |>
            getAdjacentTiles |>
            List.filter (fun coordinates ->
                Math.isPointInBoundsI coordinates buildBoundsC &&
                Map.find coordinates fieldTiles = tile)
        passingTiles.Length >= n
    
    let makeGrid (boundsC : Vector4i) =
        seq {
            for i in boundsC.BottomLeft.X .. boundsC.TopRight.X do
                for j in boundsC.BottomLeft.Y .. boundsC.TopRight.Y do
                    yield v2i i j }

    let addPaths buildBoundsC pathEdgesC fieldTiles rand =
        let (paths, rand) =
            List.fold
                (fun (paths, rand) (sourceM, destinationM) ->
                    let (path, rand) = Direction.wanderToDestination buildBoundsC sourceM destinationM rand
                    (path :: paths, rand))
                ([], rand)
                pathEdgesC
        let fieldTiles =
            Seq.fold
                (fun fieldTiles path ->
                    let fieldTiles' =
                        Seq.fold
                            (fun fieldTiles tileCoordinates -> Map.add tileCoordinates PathTile fieldTiles)
                            fieldTiles
                            path
                    fieldTiles @@ fieldTiles')
                fieldTiles
                paths
        (fieldTiles, rand)

    let addTrees buildBoundsC fieldTiles rand =
        let grid = makeGrid buildBoundsC
        let pathTileCount = fieldTiles |> Map.filter (fun _ v -> v = PathTile) |> Map.count
        let treeDilution =
            if pathTileCount < 25 then 32
            elif pathTileCount < 50 then 16
            elif pathTileCount < 60 then 8
            elif pathTileCount < 70 then 4
            elif pathTileCount < 80 then 3
            elif pathTileCount < 90 then 2
            else 1
        Seq.fold
            (fun (fieldTiles, rand) coordinates ->
                let (n, rand) = Rand.nextIntUnder treeDilution rand // original value is 16
                if n = 0 && Map.find coordinates fieldTiles <> PathTile
                then (Map.add coordinates TreeTile fieldTiles, rand)
                else (fieldTiles, Rand.advance rand))
            (fieldTiles, rand)
            grid

    let spreadTrees buildBoundsC fieldTiles rand =
        let originalMap = fieldTiles
        let grid = makeGrid buildBoundsC
        Seq.fold
            (fun (fieldTiles, rand) coordinates ->
                let (n, rand) = Rand.nextIntUnder 3 rand
                if n = 0 && Map.find coordinates originalMap <> PathTile && hasAtLeastNAdjacentTiles 1 coordinates TreeTile buildBoundsC originalMap && Math.isPointInBoundsI coordinates buildBoundsC
                then (Map.add coordinates TreeTile fieldTiles, rand)
                else (fieldTiles, Rand.advance rand))
            (fieldTiles, rand)
            grid

    let addWater buildBoundsC fieldTiles rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) fieldTiles |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsC
            Seq.fold
                (fun (fieldTiles, rand) coordinates ->
                    let (n, rand) = Rand.nextIntUnder 128 rand
                    if n = 0 && Map.find coordinates fieldTiles = GrassTile && hasAtLeastNAdjacentTiles 4 coordinates GrassTile buildBoundsC fieldTiles
                    then (Map.add coordinates WaterTile fieldTiles, rand)
                    else (fieldTiles, rand))
                (fieldTiles, rand)
                grid
        else (fieldTiles, rand)

    let spreadWater1 buildBoundsC fieldTiles rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) fieldTiles |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsC
            Seq.fold
                (fun (fieldTiles, rand) coordinates ->
                    let (n, rand) = Rand.nextIntUnder 2 rand
                    if n = 0 && Map.find coordinates fieldTiles <> PathTile && hasAtLeastNAdjacentTiles 1 coordinates WaterTile buildBoundsC fieldTiles
                    then (Map.add coordinates WaterTile fieldTiles, rand)
                    else (fieldTiles, rand))
                (fieldTiles, rand)
                grid
        else (fieldTiles, rand)

    let spreadWater2 buildBoundsC fieldTiles rand =
        let pathTileCount = Map.filter (fun _ v -> v = PathTile) fieldTiles |> Map.count
        if pathTileCount < 25 then
            let grid = makeGrid buildBoundsC
            let originalMap = fieldTiles
            Seq.fold
                (fun (fieldTiles, rand) coordinates ->
                    let (n, rand) = Rand.nextIntUnder 1 rand
                    if n = 0 && Map.find coordinates fieldTiles <> PathTile && hasAtLeastNAdjacentTiles 1 coordinates WaterTile buildBoundsC originalMap
                    then (Map.add coordinates WaterTile fieldTiles, rand)
                    else (fieldTiles, rand))
                (fieldTiles, rand)
                grid
        else (fieldTiles, rand)

    let addStones buildBoundsC fieldTiles rand =
        let grid = makeGrid buildBoundsC
        Seq.fold
            (fun (fieldTiles, rand) coordinates ->
                if Map.find coordinates fieldTiles = GrassTile && hasAtLeastNAdjacentTiles 4 coordinates PathTile buildBoundsC fieldTiles
                then (Map.add coordinates StoneTile fieldTiles, Rand.advance rand)
                else (fieldTiles, Rand.advance rand))
            (fieldTiles, rand)
            grid

    let makeEmptyFieldTiles (positionC : Vector2i) (sizeC : Vector2i) =
        Map.ofList
            [for i in positionC.X .. dec sizeC.X do
                for j in positionC.Y .. dec sizeC.Y do
                    let tileCoordinatesC = v2i i j
                    yield (tileCoordinatesC, GrassTile)]
    
    let make tileSheet (positionC : Vector2i) sizeC pathEdgesC rand =
        let buildBoundsC = v4iBounds positionC (sizeC - v2iOne)
        let fieldTiles = makeEmptyFieldTiles positionC sizeC
        let (fieldTiles, rand) = addPaths buildBoundsC pathEdgesC fieldTiles rand
        let (fieldTiles, rand) = addTrees buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadTrees buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadTrees buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadTrees buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = addWater buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadWater1 buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadWater1 buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = spreadWater2 buildBoundsC fieldTiles rand
        let (fieldTiles, rand) = addStones buildBoundsC fieldTiles rand
        let fieldMap = { FieldSizeC = sizeC; FieldTiles = fieldTiles; FieldTileSheet = tileSheet }
        (fieldMap, rand)

    let makeFromMetaTile (metaTile : MetaTile) =
        let rand = Rand.makeFromSeedState metaTile.RandSeed
        let (fieldMap, _) = make Assets.Gameplay.FieldTileSheetImage v2iZero Constants.Gameplay.FieldMapSizeC [(metaTile.PathStart, metaTile.PathEnd)] rand
        fieldMap

    let initial =
        let defaultRand = Rand.make ()
        let defaultSizeC = v2i 4 4
        let defaultPathEdgesC = [(v2i 1 1, v2i 2 2)]
        let defaultFieldMap = fst (make Assets.Gameplay.FieldTileSheetImage v2iZero defaultSizeC defaultPathEdgesC defaultRand)
        defaultFieldMap