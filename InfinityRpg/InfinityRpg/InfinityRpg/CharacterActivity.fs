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

[<RequireQualifiedAccess>]
module CharacterActivity =

    let private walk positive current destination =
        let (walkSpeed, delta) =
            if positive
            then (CharacterWalkSpeed, destination - current)
            else (-CharacterWalkSpeed, current - destination)
        let next = current + walkSpeed
        let newDelta = if positive then destination - next else next - destination
        if newDelta < CharacterWalkSpeed
        then (destination, WalkFinished)
        else (next, WalkContinuing)

    let private isTilePassable fieldTiles positionM =
        match Map.tryFind positionM fieldTiles with
        | Some tile -> tile.FieldTileType = Passable
        | None -> false

    let private getOpenDirectionsFromPositionM positionM (field : Entity) =
        let fieldTiles = field.FieldMapNp.FieldTiles
        Set.ofSeq <|
            seq {
                if isTilePassable fieldTiles (positionM + Vector2i.Up) then yield North
                if isTilePassable fieldTiles (positionM + Vector2i.Right) then yield East
                if isTilePassable fieldTiles (positionM + Vector2i.Down) then yield South
                if isTilePassable fieldTiles (positionM + Vector2i.Left) then yield West }

    let private getOpenDirectionsFromPosition (position : Vector2) field =
        let positionM = Vector2i (int <| position.X / TileSize.X, int <| position.Y / TileSize.Y)
        getOpenDirectionsFromPositionM positionM field

    let private getOpenNeighborPositionMsFromPositionM field positionM =
        let openDirections = getOpenDirectionsFromPositionM positionM field
        Set.map (fun direction -> positionM + Direction.toVector2i direction) openDirections

    let private queryTurnOnDirectionInternal walkDirection field (character : Entity) =
        let openDirections = getOpenDirectionsFromPosition character.Position field
        if Set.contains walkDirection openDirections then
            let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
            NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
        else NoTurn

    let queryTurnOnDirection walkDirection field (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity -> queryTurnOnDirectionInternal walkDirection field character

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

    let advanceNavigation field (character : Entity) =
        match character.ActivityState with
        | Action _ -> character
        | Navigation navigationDescriptor -> advanceNavigationInternal navigationDescriptor character
        | NoActivity -> character

    let getNavigationTurn (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation navigationDescriptor ->
            let walkDescriptor = navigationDescriptor.WalkDescriptor
            let walkOriginM = walkDescriptor.WalkOriginM
            let walkOrigin = Vector2.Multiply (walkOriginM.Vector2, TileSize)
            if character.Position = walkOrigin then
                match navigationDescriptor.OptNavigationPath with
                | Some navigationPath -> NavigationTurn navigationDescriptor
                | None -> NoTurn
            else NoTurn
        | NoActivity -> NoTurn

    let private makeNodes (field : Entity) =
        
        // make the nodes without neighbors
        let nodes =
            Map.map
                (fun positionM _ -> { PositionM = positionM; Neighbors = [] })
                field.FieldMapNp.FieldTiles

        // OPTIMIZATION: populate node neghbors imperatively for speed
        Map.iter
            (fun positionM node -> 
                let neighborPositionMs = List.ofSeq <| getOpenNeighborPositionMsFromPositionM field positionM
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

    let private tryGetNavigationPath (field : Entity) touchPosition (character : Entity) =
        let nodes = makeNodes field
        let touchPositionE = touchPosition - (character.Position + character.Size * 0.5f)
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

    let private tryChangeActivityToNavigationByTouch touchPosition field character =
        match tryGetNavigationPath field touchPosition character with
        | Some navigationPath ->
            match navigationPath with
            | [] -> NoTurn
            | _ ->
                let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let walkDirection = Direction.fromVector2i <| (List.head navigationPath).PositionM - currentPositionM
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
                NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
        | None -> NoTurn

    let touch touchPosition field (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity -> tryChangeActivityToNavigationByTouch touchPosition field character