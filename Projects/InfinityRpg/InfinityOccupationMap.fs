namespace InfinityRpg
open System
open Prime
open Nu

type OccupationMap =
    Map<Vector2i, bool>

[<RequireQualifiedAccess>]
module OccupationMap =

    let isOpenAtCoordinates coordinates (occupationMap : OccupationMap) =
        match Map.tryFind coordinates occupationMap with
        | Some occupied -> not occupied
        | None -> false

    let getOpenDirectionsAtCoordinates coordinates (occupationMap : OccupationMap) =
        Set.ofSeq $
            seq {
                if isOpenAtCoordinates (coordinates + v2iUp) occupationMap then yield Upward
                if isOpenAtCoordinates (coordinates + v2iRight) occupationMap then yield Rightward
                if isOpenAtCoordinates (coordinates + v2iDown) occupationMap then yield Downward
                if isOpenAtCoordinates (coordinates + v2iLeft) occupationMap then yield Leftward }

    let getOpenNeighborCoordinates coordinates occupationMap =
        let openDirections = getOpenDirectionsAtCoordinates coordinates occupationMap
        Set.map (fun direction -> coordinates + dtovc direction) openDirections

    let occupyByCharacter characterPosition (occupationMap : OccupationMap) =
        let characterCoordinates = vftovc characterPosition
        Map.add characterCoordinates true occupationMap

    let occupyByCharacters characterPositions occupationMap =
        Seq.fold (flip occupyByCharacter) occupationMap characterPositions

    let occupyByAdjacentCharacter coordinates characterPosition (occupationMap : OccupationMap) =
        let characterCoordinates = vftovc characterPosition
        if Math.areCoordinatesAdjacent characterCoordinates coordinates
        then Map.add characterCoordinates true occupationMap
        else occupationMap

    let occupyByAdjacentCharacters coordinates characterPositions occupationMap =
        Seq.fold (flip (occupyByAdjacentCharacter coordinates)) occupationMap characterPositions

    let unoccupyByCharacter characterPosition (occupationMap : OccupationMap) =
        let characterCoordinates = vftovc characterPosition
        Map.add characterCoordinates false occupationMap

    let unoccupyByCharacters characterPositions occupationMap =
        Seq.fold (flip unoccupyByCharacter) occupationMap characterPositions

    // NOTE: the function 'makeFromFieldTiles' makes an FSharp.Map from every single tile, creating a performance
    // bottleneck for larger maps. solution is to simply leave the entry empty when the tile is passable, only adding entries for tiles that are not passable.
    let makeFromFieldTiles fieldTiles =
        Map.fold
            (fun occupationMap fieldTileCoordinates fieldTile ->
                match fieldTile.TileType with
                | Impassable -> Map.add fieldTileCoordinates true occupationMap
                | Passable -> Map.add fieldTileCoordinates false occupationMap)
            Map.empty
            fieldTiles

    let makeFromFieldTilesAndCharacters fieldTiles characterPositions =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByCharacters characterPositions occupationMap

    let makeFromFieldTilesAndAdjacentCharacters coordinates fieldTiles characterPositions =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByAdjacentCharacters coordinates characterPositions occupationMap

    let makeNavigationNodes occupationMap =
        
        // make the nodes without neighbors
        let nodes = Map.map (fun coordinates _ -> { Coordinates = coordinates; Neighbors = [] }) occupationMap

        // OPTIMIZATION: populate node neighbors imperatively for speed
        Map.iter
            (fun coordinates node -> 
                let neighborCoordinates = List.ofSeq (getOpenNeighborCoordinates coordinates occupationMap)
                let neighbors =
                    List.fold
                        (fun neighbors neighborCoordinates ->
                            match Map.tryFind neighborCoordinates nodes with
                            | Some node -> node :: neighbors
                            | None -> neighbors)
                        []
                        neighborCoordinates
                node.Neighbors <- neighbors)
            nodes

        // teh nodes
        nodes