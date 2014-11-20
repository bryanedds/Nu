namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module FieldMapModule =

    type FieldTileType =
        | Impassable
        | Passable

    type FieldTile =
        { FieldTileSheetPositionM : Vector2i
          FieldTileType : FieldTileType }

    type [<NoEquality; NoComparison>] FieldMap =
        { FieldSizeM : Vector2i
          FieldTiles : Map<Vector2i, FieldTile>
          FieldTileSheet : Image }

        static member PathTile = { FieldTileSheetPositionM = Vector2i (3, 0); FieldTileType = Passable }
        static member GrassTile = { FieldTileSheetPositionM = Vector2i (3, 3); FieldTileType = Passable }
        static member TreeTile = { FieldTileSheetPositionM = Vector2i (1, 1); FieldTileType = Impassable }

        static member makeGrid boundsM =
            seq {
                for i in boundsM.CornerNegative.X .. boundsM.CornerPositive.X do
                    for j in boundsM.CornerNegative.Y .. boundsM.CornerPositive.Y do
                        yield Vector2i (i, j) }

        static member generateEmptyMap (sizeM : Vector2i) =
            Map.ofList
                [for i in 0 .. sizeM.X - 1 do
                    for j in 0 .. sizeM.Y - 1 do
                        let tileCoordsM = Vector2i (i, j)
                        yield (tileCoordsM, FieldMap.GrassTile)]

        static member addPaths buildBoundsM pathEdgesM generatedMap rand =
            
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
                                (fun generatedMap tilePositionM -> Map.add tilePositionM FieldMap.PathTile generatedMap)
                                generatedMap
                                path
                        generatedMap @@ generatedMap')
                    generatedMap
                    paths

            (generatedMap, rand)

        static member addTrees buildBoundsM generatedMap rand =
            let grid = FieldMap.makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let (n, rand) = Rand.nextIntUnder 16 rand
                    if n = 0 && Map.find positionM generatedMap <> FieldMap.PathTile
                    then (Map.add positionM FieldMap.TreeTile generatedMap, rand)
                    else (generatedMap, Rand.advance rand))
                (generatedMap, rand)
                grid

        static member spreadTrees buildBoundsM generatedMap rand =
            let originalMap = generatedMap
            let grid = FieldMap.makeGrid buildBoundsM
            Seq.fold
                (fun (generatedMap, rand) positionM ->
                    let tile = Map.find positionM originalMap
                    if  tile <> FieldMap.PathTile &&
                        MapBounds.isPointInBounds positionM buildBoundsM then
                        let upM = positionM + Vector2i.Up
                        let rightM = positionM + Vector2i.Right
                        let downM = positionM + Vector2i.Down
                        let leftM = positionM + Vector2i.Left
                        if  MapBounds.isPointInBounds upM buildBoundsM && Map.find upM originalMap = FieldMap.TreeTile ||
                            MapBounds.isPointInBounds rightM buildBoundsM && Map.find rightM originalMap = FieldMap.TreeTile ||
                            MapBounds.isPointInBounds downM buildBoundsM && Map.find downM originalMap = FieldMap.TreeTile ||
                            MapBounds.isPointInBounds leftM buildBoundsM && Map.find leftM originalMap = FieldMap.TreeTile then
                            let (n, rand) = Rand.nextIntUnder 3 rand
                            if n = 0 then (Map.add positionM FieldMap.TreeTile generatedMap, rand)
                            else (generatedMap, Rand.advance rand)
                        else (generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand))
                (generatedMap, rand)
                grid

        static member make tileSheet sizeM pathEdgesM rand =
            let buildBoundsM = { CornerNegative = Vector2i.One; CornerPositive = sizeM - Vector2i.One * 2 }
            let generatedMap = FieldMap.generateEmptyMap sizeM
            let (generatedMap, rand) = FieldMap.addPaths buildBoundsM pathEdgesM generatedMap rand
            let (generatedMap, rand) = FieldMap.addTrees buildBoundsM generatedMap rand
            let (generatedMap, rand) = FieldMap.spreadTrees buildBoundsM generatedMap rand
            let (generatedMap, rand) = FieldMap.spreadTrees buildBoundsM generatedMap rand
            { FieldSizeM = sizeM
              FieldTiles = generatedMap
              FieldTileSheet = tileSheet }