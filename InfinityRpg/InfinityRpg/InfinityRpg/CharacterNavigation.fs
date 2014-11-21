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
        then (destination, Arrived)
        else (next, Arriving)

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

    let advanceStandState characterAddress (character : Entity) world =
        let optWalkDirection =
            if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then Some North
            elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then Some South
            elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then Some East
            elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then Some West
            else None
        let openDirections = getOpenDirections character.Position world
        let startWalking =
            match optWalkDirection with
            | Some walkDirection -> Set.contains walkDirection openDirections
            | None -> false
        let characterAnimationState = character.CharacterAnimationState
        let characterAnimationState =
            match optWalkDirection with
            | Some walkDirection -> {characterAnimationState  with CharacterAnimationDirection = walkDirection }
            | None -> characterAnimationState
        let character = Entity.setCharacterAnimationState characterAnimationState character
        let character =
            match (optWalkDirection, startWalking) with
            | (Some walkDirection, true) ->
                let walkOriginM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let navigationGoalM = walkOriginM + Direction.toVector2i walkDirection
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = walkOriginM }
                let activityState = Navigating { WalkDescriptor = walkDescriptor; NavigationGoalM = navigationGoalM }
                Entity.setActivityState activityState character
            | (None, true) ->
                failwith <|
                    "Unexpected match in InfinityRpg.CharacterControlFacet.advanceInput. " +
                    "Logically, this match should never happen."
            | (Some _, false) -> character
            | (None, false) -> character
        World.setEntity characterAddress character world

    let private advanceNavigationState characterAddress (character : Entity) navigationDescriptor world =
        let walkDescriptor = navigationDescriptor.WalkDescriptor
        let walkDistanceI = match walkDescriptor.WalkDirection with North | South -> TileSizeI.Y | East | West -> TileSizeI.X
        let walkDestinationM = walkDistanceI * (walkDescriptor.WalkOriginM + Direction.toVector2i walkDescriptor.WalkDirection)
        let walkDestination = walkDestinationM.Vector2
        let (newPosition, arrival) =
            match walkDescriptor.WalkDirection with
            | North -> let (newY, arrival) = walk true character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | East -> let (newX, arrival) = walk true character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
            | South -> let (newY, arrival) = walk false character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | West -> let (newX, arrival) = walk false character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
        let character = Entity.setPosition newPosition character
        let character = match arrival with Arriving -> character | Arrived -> Entity.setActivityState Standing character
        let world = World.setEntity characterAddress character world
        match character.ActivityState with
        | Standing -> advanceStandState characterAddress character world
        | Navigating _ | Acting _ -> world

    let advance characterAddress (character : Entity) activityAdvancement world =
        ignore activityAdvancement // TODO: use this
        match character.ActivityState with
        | Standing -> advanceStandState characterAddress character world
        | Navigating navigationDescriptor -> advanceNavigationState characterAddress character navigationDescriptor world
        | Acting _ -> world

[<AutoOpen>]
module CharacterActivityModule =

    type CharacterActivityAdvancement =
        | AdvanceWithDirection of Direction
        | AdvanceWithAI // of ...
        | AdvanceOnly

    type World with

        static member advanceCharacterActivity characterAddress character activityAdvancement world =
            CharacterActivity.advance characterAddress character activityAdvancement world

        static member touchCharacterActivity characterAddress (character : Entity) touchPosition world =
            match character.ActivityState with
            | Standing ->
                let touchPositionW = Camera.mouseToWorld touchPosition character.ViewType world.Camera
                let walkDirection = Direction.fromVector2 touchPositionW
                let walkOriginM = Vector2i (Vector2.Divide (character.Position, TileSize))
                let navigationGoalM = walkOriginM + Direction.toVector2i walkDirection
                let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = walkOriginM }
                let activityState = Navigating { WalkDescriptor = walkDescriptor; NavigationGoalM = navigationGoalM }
                let character = Entity.setActivityState activityState character
                World.setEntity characterAddress character world
            | Navigating _ | Acting _ -> world