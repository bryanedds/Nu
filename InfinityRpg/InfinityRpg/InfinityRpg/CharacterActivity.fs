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
        else (next, Walking)

    let private isTilePassable fieldTiles positionM =
        match Map.tryFind positionM fieldTiles with
        | Some tile -> tile.FieldTileType = Passable
        | None -> false

    let private getOpenDirectionsFromPositionM (field : Entity) positionM =
        let fieldTiles = field.FieldMapNp.FieldTiles
        Set.ofSeq <|
            seq {
                if isTilePassable fieldTiles (positionM + Vector2i.Up) then yield North
                if isTilePassable fieldTiles (positionM + Vector2i.Right) then yield East
                if isTilePassable fieldTiles (positionM + Vector2i.Down) then yield South
                if isTilePassable fieldTiles (positionM + Vector2i.Left) then yield West }

    let private getOpenDirections (position : Vector2) field =
        let positionM = Vector2i (int <| position.X / TileSize.X, int <| position.Y / TileSize.Y)
        getOpenDirectionsFromPositionM field positionM

    let private getOpenNeighborPositions field positionM =
        let openDirections = getOpenDirectionsFromPositionM field positionM
        Set.map (fun direction -> positionM + Direction.toVector2i direction) openDirections

    let private beginWalkingIfApplicable walkDirection field (character : Entity) =
        let openDirections = getOpenDirections character.Position field
        let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
        let character = Entity.setCharacterAnimationState characterAnimationState character
        let startWalking = Set.contains walkDirection openDirections
        if startWalking then
            let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
            let activityState = Navigating { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
            Entity.setActivityState activityState character
        else character

    let private continueWalkingIfApplicable navigationDescriptor (character : Entity) =
        let characterPositionM = Vector2i.Divide (Vector2i character.Position, TileSizeI)
        match navigationDescriptor.OptNavigationPath with
        | Some [] -> failwith "NavigationPath should never be empty here."
        | Some (_ :: []) -> Entity.setActivityState Standing character
        | Some (currentNode :: navigationPath) ->
            let walkDirection = Direction.fromVector2i <| (List.head navigationPath).PositionM - currentNode.PositionM
            let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
            let character = Entity.setCharacterAnimationState characterAnimationState character
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
            let navigationDescriptor = { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
            Entity.setActivityState (Navigating navigationDescriptor) character
        | None -> Entity.setActivityState Standing character

    let private advanceNavigation navigationDescriptor (character : Entity) =
        let walkDescriptor = navigationDescriptor.WalkDescriptor
        let walkDistanceI = match walkDescriptor.WalkDirection with North | South -> TileSizeI.Y | East | West -> TileSizeI.X
        let walkDestinationI = walkDistanceI * (walkDescriptor.WalkOriginM + Direction.toVector2i walkDescriptor.WalkDirection)
        let walkDestination = walkDestinationI.Vector2
        let (newPosition, walkState) =
            match walkDescriptor.WalkDirection with
            | North -> let (newY, arrival) = walk true character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | East -> let (newX, arrival) = walk true character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
            | South -> let (newY, arrival) = walk false character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | West -> let (newX, arrival) = walk false character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
        let character = Entity.setPosition newPosition character
        match walkState with
        | WalkFinished -> continueWalkingIfApplicable navigationDescriptor character
        | Walking -> character

    let advance advancementType field (character : Entity) =
        match character.ActivityState with
        | Standing ->
            match advancementType with
            | AdvanceWithDirection direction -> beginWalkingIfApplicable direction field character
            | AdvanceWithAI -> character
            | AdvanceOnly -> character
        | Navigating navigationDescriptor ->
            match advancementType with
            | AdvanceWithDirection direction ->
                let character = advanceNavigation navigationDescriptor character
                match character.ActivityState with // advance stand state if possible for smooth direction input
                | Standing -> beginWalkingIfApplicable direction field character
                | Navigating _ | Acting _ -> character
            | AdvanceWithAI -> character
            | AdvanceOnly -> advanceNavigation navigationDescriptor character
        | Acting _ -> character

    let private makeNodes (field : Entity) =
        
        // make the nodes without neighbors
        let nodes =
            Map.map
                (fun positionM _ -> { PositionM = positionM; Neighbors = [] })
                field.FieldMapNp.FieldTiles

        // OPTIMIZATION: populate node neghbors imperatively for speed
        Map.iter
            (fun positionM node -> 
                let neighborPositions = getOpenNeighborPositions field positionM |> List.ofSeq
                let neighbors =
                    List.fold
                        (fun neighbors neighborPosition ->
                            match Map.tryFind neighborPosition nodes with
                            | Some node -> node :: neighbors
                            | None -> neighbors)
                        []
                        neighborPositions
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

    let private touchDuringStandingState touchPosition field character =
        match tryGetNavigationPath field touchPosition character with
        | Some navigationPath ->
            match navigationPath with
            | [] -> character
            | _ ->
                let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let walkDirection = Direction.fromVector2i <| (List.head navigationPath).PositionM - currentPositionM
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
                let activityState = Navigating { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                let character = Entity.setActivityState activityState character
                let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
                Entity.setCharacterAnimationState characterAnimationState character
        | None -> character

    let touch touchPosition field (character : Entity) =
        match character.ActivityState with
        | Standing -> touchDuringStandingState touchPosition field character
        | Navigating _ | Acting _ -> character

    let detailTouch touchDirection field (character : Entity) =
        match character.ActivityState with
        | Standing -> beginWalkingIfApplicable touchDirection field character
        | Navigating _ | Acting _ -> character