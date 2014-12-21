namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg.Constants

module OccupationMap =

    let isOpenAtPositionM positionM occupationMap =
        match Map.tryFind positionM occupationMap with
        | Some occupied -> not occupied
        | None -> false

    let getOpenDirectionsAtPositionM positionM occupationMap =
        Set.ofSeq <|
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

    let occupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = vftovm character.Position
        Map.add characterPositionM true occupationMap

    let occupyByCharacters characters occupationMap =
        List.fold (flip occupyByCharacter) occupationMap characters

    let occupyByAdjacentCharacter positionM (character : Entity) occupationMap =
        let characterPositionM = vftovm character.Position
        if Math.arePositionMsAdjacent characterPositionM positionM
        then Map.add characterPositionM true occupationMap
        else occupationMap

    let occupyByAdjacentCharacters positionM characters occupationMap =
        List.fold (flip <| occupyByAdjacentCharacter positionM) occupationMap characters

    let unoccupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = vftovm character.Position
        Map.add characterPositionM false occupationMap

    let unoccupyByCharacters characters occupationMap =
        List.fold (flip unoccupyByCharacter) occupationMap characters

    let transferByDesiredTurn desiredTurn character occupationMap =
        match desiredTurn with
        | ActionTurn _ -> occupationMap
        | NavigationTurn _ -> unoccupyByCharacter character occupationMap |> occupyByDesiredTurn desiredTurn
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

    let makeFromFieldTilesAndCharacters fieldTiles characters =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByCharacters characters occupationMap

    let makeFromFieldTilesAndCharactersAndDesiredTurn fieldTiles characters desiredTurn =
        let occupationMap = makeFromFieldTilesAndCharacters fieldTiles characters
        occupyByDesiredTurn desiredTurn occupationMap

    let makeFromFieldTilesAndAdjacentCharacters positionM fieldTiles characters =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByAdjacentCharacters positionM characters occupationMap

    let makeNavigationNodes occupationMap =
        
        // make the nodes without neighbors
        let nodes = Map.map (fun positionM _ -> { PositionM = positionM; Neighbors = [] }) occupationMap

        // OPTIMIZATION: populate node neghbors imperatively for speed
        Map.iter
            (fun positionM node -> 
                let neighborPositionMs = List.ofSeq <| getOpenNeighborPositionMsAtPositionM positionM occupationMap
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