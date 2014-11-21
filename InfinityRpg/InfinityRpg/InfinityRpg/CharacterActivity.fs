namespace InfinityRpg
open System
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<RequireQualifiedAccess>]
module CharacterActivity =

    let walk positive current destination =
        let (walkSpeed, delta) =
            if positive
            then (CharacterWalkSpeed, destination - current)
            else (-CharacterWalkSpeed, current - destination)
        let next = current + walkSpeed
        let newDelta = if positive then destination - next else next - destination
        if newDelta < CharacterWalkSpeed
        then (destination, WalkFinished)
        else (next, Walking)

    let checkOpenDirection currentPositionM fieldTiles direction =
        let directionVector = Direction.toVector2i direction
        match Map.tryFind (currentPositionM + directionVector) fieldTiles with
        | Some tile -> tile.FieldTileType = Passable
        | None -> true

    let getOpenDirections (position : Vector2) world =
        match World.getOptEntity FieldAddress world with
        | Some field ->
            let currentPositionM =
                Vector2i
                    (int <| position.X / TileSize.X,
                     int <| position.Y / TileSize.Y)
            let fieldTiles = field.FieldMapNp.FieldTiles
            Set.ofSeq <|
                seq {
                    if checkOpenDirection currentPositionM fieldTiles North then yield North
                    if checkOpenDirection currentPositionM fieldTiles East then yield East
                    if checkOpenDirection currentPositionM fieldTiles South then yield South
                    if checkOpenDirection currentPositionM fieldTiles West then yield West }
        | None -> Set.ofList [North; East; South; West]

    let advanceStandingState walkDirection (character : Entity) world =
        let openDirections = getOpenDirections character.Position world
        let startWalking = Set.contains walkDirection openDirections
        let characterAnimationState = character.CharacterAnimationState
        let characterAnimationState = { characterAnimationState with CharacterAnimationDirection = walkDirection }
        let character = Entity.setCharacterAnimationState characterAnimationState character
        if startWalking then
            let walkOriginM = Vector2i (Vector2.Divide (character.Position, TileSize))
            let navigationGoalM = walkOriginM + Direction.toVector2i walkDirection
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = walkOriginM }
            let activityState = Navigating { WalkDescriptor = walkDescriptor; NavigationGoalM = navigationGoalM }
            Entity.setActivityState activityState character
        else character

    let private advanceNavigatingStatePostWalk navigationDescriptor (character : Entity) world =
        let characterPositionM = Vector2i.Divide (Vector2i character.Position, TileSizeI)
        let navigationRemaining = navigationDescriptor.NavigationGoalM - characterPositionM
        if navigationRemaining <> Vector2i.Zero then
            let walkDirection = Direction.fromVector2i navigationRemaining
            let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
            let navigationDescriptor = { navigationDescriptor with WalkDescriptor = walkDescriptor }
            let openDirections = getOpenDirections character.Position world
            let stillNavigating = Set.contains walkDirection openDirections
            if stillNavigating then
                let character = Entity.setActivityState (Navigating navigationDescriptor) character
                let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
                Entity.setCharacterAnimationState characterAnimationState character
            else Entity.setActivityState Standing character
        else Entity.setActivityState Standing character

    let private advanceNavigatingState navigationDescriptor (character : Entity) world =
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
        | WalkFinished -> advanceNavigatingStatePostWalk navigationDescriptor character world
        | Walking -> character

    let advance advancementType (character : Entity) world =
        match character.ActivityState with
        | Standing ->
            match advancementType with
            | AdvanceWithDirection direction -> advanceStandingState direction character world
            | AdvanceWithAI -> character
            | AdvanceOnly -> character
        | Navigating navigationDescriptor ->
            match advancementType with
            | AdvanceWithDirection direction ->
                let character = advanceNavigatingState navigationDescriptor character world
                match character.ActivityState with // advance stand state if possible for smooth direction input
                | Standing -> advanceStandingState direction character world
                | Navigating _ | Acting _ -> character
            | AdvanceWithAI -> character
            | AdvanceOnly -> advanceNavigatingState navigationDescriptor character world
        | Acting _ -> character

    let private getTouchGoalAndDirection touchPosition (character : Entity) world =
        let touchPositionW = Camera.mouseToWorld character.ViewType touchPosition world.Camera
        let touchPositionE = touchPositionW - (character.Position + character.Size * 0.5f)
        let touchDirection = Direction.fromVector2 touchPositionE
        let touchGoalM = Vector2i (Vector2.Divide (touchPositionW, TileSize))
        (touchDirection, touchGoalM)

    let private touchDuringStandState touchPosition character world =
        let (walkDirection, navigationGoalM) = getTouchGoalAndDirection touchPosition character world    
        let openDirections = getOpenDirections character.Position world
        let startWalking = Set.contains walkDirection openDirections
        let character =
            if startWalking then
                let walkOriginM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = walkOriginM }
                let activityState = Navigating { WalkDescriptor = walkDescriptor; NavigationGoalM = navigationGoalM }
                Entity.setActivityState activityState character
            else character
        let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
        Entity.setCharacterAnimationState characterAnimationState character

    let touch touchPosition (character : Entity) world =
        match character.ActivityState with
        | Standing -> touchDuringStandState touchPosition character world
        | Navigating _ | Acting _ -> character