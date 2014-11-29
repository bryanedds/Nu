namespace InfinityRpg
open System
open SDL2
open OpenTK
open AStar
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module CharacterActivityModule =

    type [<StructuralEquality; NoComparison>] TurnDescriptor =
        | NavigationTurn of NavigationDescriptor
        | CancelTurn
        | NoTurn

    type [<StructuralEquality; NoComparison>] PlayerTurnInput =
        | Touch of Vector2
        | DetailNavigation of Direction
        | NoInput

[<RequireQualifiedAccess>]
module CharacterActivity =

    let private walk positive current destination =
        let walkSpeed = if positive then CharacterWalkSpeed else -CharacterWalkSpeed
        let next = current + walkSpeed
        let delta = if positive then destination - next else next - destination
        if delta < CharacterWalkSpeed then (destination, WalkFinished) else (next, WalkContinuing)

    let private isTilePassable occupationMap positionM =
        match Map.tryFind positionM occupationMap with
        | Some occupied -> not occupied
        | None -> false

    let private getOpenDirectionsFromPositionM positionM occupationMap =
        Set.ofSeq <|
            seq {
                if isTilePassable occupationMap (positionM + Vector2i.Up) then yield North
                if isTilePassable occupationMap (positionM + Vector2i.Right) then yield East
                if isTilePassable occupationMap (positionM + Vector2i.Down) then yield South
                if isTilePassable occupationMap (positionM + Vector2i.Left) then yield West }

    let private getOpenDirectionsFromPosition (position : Vector2) occupationMap =
        let positionM = Vector2i (int <| position.X / TileSize.X, int <| position.Y / TileSize.Y)
        getOpenDirectionsFromPositionM positionM occupationMap

    let private getOpenNeighborPositionMsFromPositionM occupationMap positionM =
        let openDirections = getOpenDirectionsFromPositionM positionM occupationMap
        Set.map (fun direction -> positionM + Direction.toVector2i direction) openDirections

    let private determineTurnFromDirectionInternal walkDirection occupationMap (character : Entity) =
        let openDirections = getOpenDirectionsFromPosition character.Position occupationMap
        if Set.contains walkDirection openDirections then
            let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
            NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
        else NoTurn

    let determineTurnFromDirection walkDirection occupationMap (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity -> determineTurnFromDirectionInternal walkDirection occupationMap character

    let private advanceNavigationAfterWalkFinished navigationDescriptor (character : Entity) =
        let characterPositionM = Vector2i.Divide (Vector2i character.Position, TileSizeI)
        match navigationDescriptor.OptNavigationPath with
        | Some [] -> failwith "NavigationPath should never be empty here."
        | Some (_ :: []) -> Entity.setActivityState NoActivity character
        | Some (currentNode :: navigationPath) ->
            let walkDirection = Direction.fromVector2i <| (List.head navigationPath).PositionM - currentNode.PositionM
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
            let navigationDescriptor = { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
            Entity.setActivityState (Navigation navigationDescriptor) character
        | None -> Entity.setActivityState NoActivity character

    let private advanceNavigationInternal navigationDescriptor (character : Entity) =
        let walkDescriptor = navigationDescriptor.WalkDescriptor
        let walkDirection = walkDescriptor.WalkDirection
        let walkDistanceI = match walkDirection with North | South -> TileSizeI.Y | East | West -> TileSizeI.X
        let walkDestinationI = walkDistanceI * (walkDescriptor.WalkOriginM + Direction.toVector2i walkDirection)
        let walkDestination = walkDestinationI.Vector2
        let (newPosition, walkState) =
            match walkDirection with
            | North -> let (newY, arrival) = walk true character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | East -> let (newX, arrival) = walk true character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
            | South -> let (newY, arrival) = walk false character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | West -> let (newX, arrival) = walk false character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
        let character = Entity.setPosition newPosition character
        let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
        let character = Entity.setCharacterAnimationState characterAnimationState character
        match walkState with
        | WalkFinished -> advanceNavigationAfterWalkFinished navigationDescriptor character
        | WalkContinuing -> character

    let advanceNavigation (character : Entity) =
        match character.ActivityState with
        | Action _ -> character
        | Navigation navigationDescriptor -> advanceNavigationInternal navigationDescriptor character
        | NoActivity -> character

    let determineTurnFromActivityState (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation navigationDescriptor ->
            let walkDescriptor = navigationDescriptor.WalkDescriptor
            let walkOriginM = walkDescriptor.WalkOriginM
            let walkOrigin = Vector2.Multiply (walkOriginM.Vector2, TileSize)
            if character.Position = walkOrigin then
                match navigationDescriptor.OptNavigationPath with
                | Some _ -> NavigationTurn navigationDescriptor
                | None -> NoTurn
            else NoTurn
        | NoActivity -> NoTurn

    let private makeNodes occupationMap =
        
        // make the nodes without neighbors
        let nodes = Map.map (fun positionM _ -> { PositionM = positionM; Neighbors = [] }) occupationMap

        // OPTIMIZATION: populate node neghbors imperatively for speed
        Map.iter
            (fun positionM node -> 
                let neighborPositionMs = List.ofSeq <| getOpenNeighborPositionMsFromPositionM occupationMap positionM
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

    let private tryGetNavigationPath touchPosition occupationMap (character : Entity) =
        let nodes = makeNodes occupationMap
        let touchPositionM = Vector2i (Vector2.Divide (touchPosition, TileSize))
        let goalNode = Map.find touchPositionM nodes
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        let currentNode = Map.find characterPositionM nodes
        let optNavigationPath =
            AStar.FindPath (
                currentNode,
                goalNode,
                (fun n n2 -> if (n2.PositionM.Y <> n.PositionM.Y) then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                (fun n -> 0.0f))
        match optNavigationPath with
        | null -> None
        | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

    let private determineTurnFromNavigationTouch touchPosition occupationMap character =
        match tryGetNavigationPath touchPosition occupationMap character with
        | Some navigationPath ->
            match navigationPath with
            | [] -> NoTurn
            | _ ->
                let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let walkDirection = Direction.fromVector2i <| (List.head navigationPath).PositionM - currentPositionM
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
                NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
        | None -> NoTurn

    let determineTurnFromTouch touchPosition occupationMap (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity -> determineTurnFromNavigationTouch touchPosition occupationMap character

    let anyActivitiesInProgress characters =
        List.exists (fun (character : Entity) -> character.ActivityState <> NoActivity) characters