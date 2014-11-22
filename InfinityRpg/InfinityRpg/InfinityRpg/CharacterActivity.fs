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

    let private getOpenDirections (position : Vector2) field world =
        let positionM = Vector2i (int <| position.X / TileSize.X, int <| position.Y / TileSize.Y)
        getOpenDirectionsFromPositionM field positionM

    let private getOpenNeighborPositions field positionM =
        let openDirections = getOpenDirectionsFromPositionM field positionM
        Set.map (fun direction -> positionM + Direction.toVector2i direction) openDirections

    let private advanceDuringStandingStateWithDirection walkDirection field (character : Entity) world =
        let openDirections = getOpenDirections character.Position field world
        let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
        let character = Entity.setCharacterAnimationState characterAnimationState character
        let startWalking = Set.contains walkDirection openDirections
        if startWalking then
            let currentPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = currentPositionM }
            let activityState = Navigating { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
            Entity.setActivityState activityState character
        else character

    let private advanceDuringNavigatingStateAfterWalk navigationDescriptor (character : Entity) world =
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

    let private advanceDuringNavigatingState navigationDescriptor (character : Entity) world =
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
        | WalkFinished -> advanceDuringNavigatingStateAfterWalk navigationDescriptor character world
        | Walking -> character

    let advance advancementType field (character : Entity) world =
        match character.ActivityState with
        | Standing ->
            match advancementType with
            | AdvanceWithDirection direction -> advanceDuringStandingStateWithDirection direction field character world
            | AdvanceWithAI -> character
            | AdvanceOnly -> character
        | Navigating navigationDescriptor ->
            match advancementType with
            | AdvanceWithDirection direction ->
                let character = advanceDuringNavigatingState navigationDescriptor character world
                match character.ActivityState with // advance stand state if possible for smooth direction input
                | Standing -> advanceDuringStandingStateWithDirection direction field character world
                | Navigating _ | Acting _ -> character
            | AdvanceWithAI -> character
            | AdvanceOnly -> advanceDuringNavigatingState navigationDescriptor character world
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

    let private tryGetNavigationPath camera (field : Entity) touchPosition (character : Entity) =
        let nodes = makeNodes field
        let touchPositionW = Camera.mouseToWorld character.ViewType touchPosition camera
        let touchPositionE = touchPositionW - (character.Position + character.Size * 0.5f)
        let touchGoalM = Vector2i (Vector2.Divide (touchPositionW, TileSize))
        let goalNode = Map.find touchGoalM nodes
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        let currentNode = Map.find characterPositionM nodes
        match AStar.FindPath (currentNode, goalNode, (fun n n2 -> 1.0f), (fun n -> let v = n.PositionM - touchGoalM in v.LengthSquared)) with
        | null -> None
        | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

    let private touchDuringStandingState touchPosition field character world =
        match tryGetNavigationPath world.Camera field touchPosition character with
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

    let touch touchPosition field (character : Entity) world =
        match character.ActivityState with
        | Standing -> touchDuringStandingState touchPosition field character world
        | Navigating _ | Acting _ -> character