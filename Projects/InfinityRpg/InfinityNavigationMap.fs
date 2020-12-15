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

        // the nodes
        nodes
    
    let tryMakeNavigationPath currentCoordinates targetCoordinates navigationMap =
        let nodes = makeNavigationNodes navigationMap
        let goalNode = Map.find targetCoordinates nodes
        let currentNode = Map.find currentCoordinates nodes
        let navigationPathOpt =
            AStar.FindPath
                (currentNode,
                goalNode,
                (fun n n2 -> if n2.Coordinates.Y <> n.Coordinates.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                (fun _ -> 0.0f))
        match navigationPathOpt with
        | null -> None
        | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)