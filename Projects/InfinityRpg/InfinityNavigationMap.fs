namespace InfinityRpg
open System
open Prime
open Nu

type [<CustomEquality; NoComparison>] NavigationNode =
    { Coordinates : Vector2i
      mutable Neighbors : NavigationNode list } // OPTIMIZATION: has to be mutable to be efficiently populated.

    interface NavigationNode IHasNeighbors with
        member this.Neighbors = this.Neighbors :> _ seq

    interface NavigationNode IEquatable with
        member this.Equals that =
            this.Coordinates = that.Coordinates

    override this.Equals that =
        match that with
        | :? NavigationNode as that -> this.Coordinates = that.Coordinates
        | _ -> false

    override this.GetHashCode () =
        this.Coordinates.GetHashCode ()

type NavigationMap =
    Map<Vector2i, bool>

[<RequireQualifiedAccess>]
module NavigationMap =

    let isOpenAtCoordinates coordinates (navigationMap : NavigationMap) =
        match Map.tryFind coordinates navigationMap with
        | Some occupied -> not occupied
        | None -> false

    let getOpenDirectionsAtCoordinates coordinates (navigationMap : NavigationMap) =
        Set.ofSeq $
            seq {
                if isOpenAtCoordinates (coordinates + v2iUp) navigationMap then yield Upward
                if isOpenAtCoordinates (coordinates + v2iRight) navigationMap then yield Rightward
                if isOpenAtCoordinates (coordinates + v2iDown) navigationMap then yield Downward
                if isOpenAtCoordinates (coordinates + v2iLeft) navigationMap then yield Leftward }

    let getOpenNeighborCoordinates coordinates navigationMap =
        let openDirections = getOpenDirectionsAtCoordinates coordinates navigationMap
        Set.map (fun direction -> coordinates + dtovc direction) openDirections

    let occupyByCharacter characterPosition (navigationMap : NavigationMap) =
        let characterCoordinates = vftovc characterPosition
        Map.add characterCoordinates true navigationMap

    let occupyByCharacters characterPositions navigationMap =
        Seq.fold (flip occupyByCharacter) navigationMap characterPositions

    let occupyByAdjacentCharacter coordinates characterPosition (navigationMap : NavigationMap) =
        let characterCoordinates = vftovc characterPosition
        if Math.areCoordinatesAdjacent characterCoordinates coordinates
        then Map.add characterCoordinates true navigationMap
        else navigationMap

    let occupyByAdjacentCharacters coordinates characterPositions navigationMap =
        Seq.fold (flip (occupyByAdjacentCharacter coordinates)) navigationMap characterPositions

    let unoccupyByCharacter characterPosition (navigationMap : NavigationMap) =
        let characterCoordinates = vftovc characterPosition
        Map.add characterCoordinates false navigationMap

    let unoccupyByCharacters characterPositions navigationMap =
        Seq.fold (flip unoccupyByCharacter) navigationMap characterPositions

    // NOTE: the function 'makeFromFieldTiles' makes an FSharp.Map from every single tile, creating a performance
    // bottleneck for larger maps. solution is to simply leave the entry empty when the tile is passable, only adding entries for tiles that are not passable.
    let makeFromFieldTiles fieldTiles =
        Map.fold
            (fun navigationMap fieldTileCoordinates fieldTile ->
                match fieldTile.TileType with
                | Impassable -> Map.add fieldTileCoordinates true navigationMap
                | Passable -> Map.add fieldTileCoordinates false navigationMap)
            Map.empty
            fieldTiles

    let makeFromFieldTilesAndCharacters fieldTiles characterPositions =
        let navigationMap = makeFromFieldTiles fieldTiles
        occupyByCharacters characterPositions navigationMap

    let makeFromFieldTilesAndAdjacentCharacters coordinates fieldTiles characterPositions =
        let navigationMap = makeFromFieldTiles fieldTiles
        occupyByAdjacentCharacters coordinates characterPositions navigationMap

    let makeNavigationNodes navigationMap =
        
        // make the nodes without neighbors
        let nodes = Map.map (fun coordinates _ -> { Coordinates = coordinates; Neighbors = [] }) navigationMap

        // OPTIMIZATION: populate node neighbors imperatively for speed
        Map.iter
            (fun coordinates node -> 
                let neighborCoordinates = List.ofSeq (getOpenNeighborCoordinates coordinates navigationMap)
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