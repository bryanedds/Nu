namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu

[<RequireQualifiedAccess>]
module OccupationMap =

    let isOpenAtPositionM positionM occupationMap =
        match Map.tryFind positionM occupationMap with
        | Some occupied -> not occupied
        | None -> false

    let getOpenDirectionsAtPositionM positionM occupationMap =
        Set.ofSeq ^
            seq {
                if isOpenAtPositionM (positionM + Vector2i.Up) occupationMap then yield Upward
                if isOpenAtPositionM (positionM + Vector2i.Right) occupationMap then yield Rightward
                if isOpenAtPositionM (positionM + Vector2i.Down) occupationMap then yield Downward
                if isOpenAtPositionM (positionM + Vector2i.Left) occupationMap then yield Leftward }

    let getOpenNeighborPositionMsAtPositionM positionM occupationMap =
        let openDirections = getOpenDirectionsAtPositionM positionM occupationMap
        Set.map (fun direction -> positionM + dtovm direction) openDirections

    let occupyByDesiredTurn desiredTurn occupationMap =
        match desiredTurn with
        | ActionTurn _ -> occupationMap
        | NavigationTurn navigationDescriptor ->
            let nextPositionM = NavigationDescriptor.nextPositionM navigationDescriptor
            Map.add nextPositionM true occupationMap
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap

    let occupyByCharacter characterPosition occupationMap =
        let characterPositionM = vftovm characterPosition
        Map.add characterPositionM true occupationMap

    let occupyByCharacters characterPositions occupationMap =
        Seq.fold (flip occupyByCharacter) occupationMap characterPositions

    let occupyByAdjacentCharacter positionM characterPosition occupationMap =
        let characterPositionM = vftovm characterPosition
        if Math.arePositionMsAdjacent characterPositionM positionM
        then Map.add characterPositionM true occupationMap
        else occupationMap

    let occupyByAdjacentCharacters positionM characterPositions occupationMap =
        Seq.fold (flip ^ occupyByAdjacentCharacter positionM) occupationMap characterPositions

    let unoccupyByCharacter characterPosition occupationMap =
        let characterPositionM = vftovm characterPosition
        Map.add characterPositionM false occupationMap

    let unoccupyByCharacters characterPositions occupationMap =
        Seq.fold (flip unoccupyByCharacter) occupationMap characterPositions

    let transferByDesiredTurn desiredTurn characterPosition occupationMap =
        match desiredTurn with
        | ActionTurn _ -> occupationMap
        | NavigationTurn _ -> unoccupyByCharacter characterPosition occupationMap |> occupyByDesiredTurn desiredTurn
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap

    let makeFromFieldTiles fieldTiles =
        Map.fold
            (fun occupationMap fieldTilePositionM fieldTile ->
                match fieldTile.TileType with
                | Impassable -> Map.add fieldTilePositionM true occupationMap
                | Passable -> Map.add fieldTilePositionM false occupationMap)
            Map.empty
            fieldTiles

    let makeFromFieldTilesAndCharacters fieldTiles characterPositions =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByCharacters characterPositions occupationMap

    let makeFromFieldTilesAndCharactersAndDesiredTurn fieldTiles characterPositions desiredTurn =
        let occupationMap = makeFromFieldTilesAndCharacters fieldTiles characterPositions
        occupyByDesiredTurn desiredTurn occupationMap

    let makeFromFieldTilesAndAdjacentCharacters positionM fieldTiles characterPositions =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByAdjacentCharacters positionM characterPositions occupationMap

    let makeNavigationNodes occupationMap =
        
        // make the nodes without neighbors
        let nodes = Map.map (fun positionM _ -> { PositionM = positionM; Neighbors = [] }) occupationMap

        // OPTIMIZATION: populate node neghbors imperatively for speed
        Map.iter
            (fun positionM node -> 
                let neighborPositionMs = List.ofSeq ^ getOpenNeighborPositionMsAtPositionM positionM occupationMap
                let neighbors =
                    List.fold
                        (fun neighbors neighborPositionM ->
                            match Map.tryFind neighborPositionM nodes with
                            | Some node -> node :: neighbors
                            | None -> neighbors)
                        []
                        neighborPositionMs
                node.Neighbors <- neighbors)
            nodes

        // teh nodes
        nodes