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
        { FieldTileSheetCoords : Vector2i
          FieldTileType : FieldTileType }

    type [<NoEquality; NoComparison>] FieldMap =
        { FieldSize : Vector2i
          FieldTiles : Map<Vector2i, FieldTile>
          FieldTileSheet : Image }

        static member PathTile = { FieldTileSheetCoords = Vector2i (3, 0); FieldTileType = Passable }
        static member GrassTile = { FieldTileSheetCoords = Vector2i (3, 3); FieldTileType = Passable }
        static member TreeTile = { FieldTileSheetCoords = Vector2i (1, 1); FieldTileType = Impassable }

        static member makeGrid bounds =
            seq {
                for i in bounds.CornerNegative.X .. bounds.CornerPositive.X do
                    for j in bounds.CornerNegative.Y .. bounds.CornerPositive.Y do
                        yield Vector2i (i, j) }

        static member generateEmptyMap (size : Vector2i) =
            Map.ofList
                [for i in 0 .. size.X - 1 do
                    for j in 0 .. size.Y - 1 do
                        let tileCoords = Vector2i (i, j)
                        yield (tileCoords, FieldMap.GrassTile)]

        static member addPaths buildBounds pathEdges generatedMap rand =
            
            let (paths, rand) =
                List.fold
                    (fun (paths, rand) (source, destination) ->
                        let (path, rand) = Direction.wanderToDestination buildBounds source destination rand
                        (path :: paths, rand))
                    ([], rand)
                    pathEdges

            let generatedMap =
                Seq.fold
                    (fun generatedMap path ->
                        let generatedMap' =
                            Seq.fold
                                (fun generatedMap tileCoords -> Map.add tileCoords FieldMap.PathTile generatedMap)
                                generatedMap
                                path
                        generatedMap @@ generatedMap')
                    generatedMap
                    paths

            (generatedMap, rand)

        static member addTrees buildBounds generatedMap rand =
            let grid = FieldMap.makeGrid buildBounds
            Seq.fold
                (fun (generatedMap, rand) point ->
                    let (n, rand) = Rand.nextIntUnder 16 rand
                    if n = 0 && Map.find point generatedMap <> FieldMap.PathTile then (Map.add point FieldMap.TreeTile generatedMap, rand)
                    else (generatedMap, Rand.advance rand))
                (generatedMap, rand)
                grid

        static member spreadTrees buildBounds generatedMap rand =
            let originalMap = generatedMap
            let grid = FieldMap.makeGrid buildBounds
            Seq.fold
                (fun (generatedMap, rand) point ->
                    let tile = Map.find point originalMap
                    if  tile <> FieldMap.PathTile &&
                        Bounds.isPointInBounds point buildBounds then
                        let upPoint = point + Vector2i.Up
                        let rightPoint = point + Vector2i.Right
                        let downPoint = point + Vector2i.Down
                        let leftPoint = point + Vector2i.Left
                        if  Bounds.isPointInBounds upPoint buildBounds && Map.find upPoint originalMap = FieldMap.TreeTile ||
                            Bounds.isPointInBounds rightPoint buildBounds && Map.find rightPoint originalMap = FieldMap.TreeTile ||
                            Bounds.isPointInBounds downPoint buildBounds && Map.find downPoint originalMap = FieldMap.TreeTile ||
                            Bounds.isPointInBounds leftPoint buildBounds && Map.find leftPoint originalMap = FieldMap.TreeTile then
                            let (n, rand) = Rand.nextIntUnder 3 rand
                            if n = 0 then (Map.add point FieldMap.TreeTile generatedMap, rand)
                            else (generatedMap, Rand.advance rand)
                        else (generatedMap, Rand.advance rand)
                    else (generatedMap, Rand.advance rand))
                (generatedMap, rand)
                grid

        static member make tileSheet size pathEdges rand =
            let buildBounds = { CornerNegative = Vector2i.One; CornerPositive = size - Vector2i.One }
            let generatedMap = FieldMap.generateEmptyMap size
            let (generatedMap, rand) = FieldMap.addPaths buildBounds pathEdges generatedMap rand
            let (generatedMap, rand) = FieldMap.addTrees buildBounds generatedMap rand
            let (generatedMap, rand) = FieldMap.spreadTrees buildBounds generatedMap rand
            let (generatedMap, rand) = FieldMap.spreadTrees buildBounds generatedMap rand
            { FieldSize = size
              FieldTiles = generatedMap
              FieldTileSheet = tileSheet }