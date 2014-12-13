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

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
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
                if isTilePassable occupationMap (positionM + Vector2i.Up) then yield Upward
                if isTilePassable occupationMap (positionM + Vector2i.Right) then yield Rightward
                if isTilePassable occupationMap (positionM + Vector2i.Down) then yield Downward
                if isTilePassable occupationMap (positionM + Vector2i.Left) then yield Leftward }

    let private getOpenDirectionsFromPosition (position : Vector2) occupationMap =
        getOpenDirectionsFromPositionM (vftovm position) occupationMap

    let private getOpenNeighborPositionMsFromPositionM occupationMap positionM =
        let openDirections = getOpenDirectionsFromPositionM positionM occupationMap
        Set.map (fun direction -> positionM + dtovm direction) openDirections

    let determineTurnFromDirection direction occupationMap (character : Entity) opponents =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity ->
            let openDirections = getOpenDirectionsFromPosition character.Position occupationMap
            if Set.contains direction openDirections then
                let walkDescriptor = { WalkDirection = direction; WalkOriginM = vftovm character.Position }
                NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
            else
                let targetPosition = character.Position + dtovf direction
                if List.exists (fun (opponent : Entity) -> opponent.Position = targetPosition) opponents
                then makeAttackTurn targetPosition
                else NoTurn

    let private advanceNavigationAfterWalkFinished navigationDescriptor (character : Entity) =
        match navigationDescriptor.OptNavigationPath with
        | Some [] -> failwith "NavigationPath should never be empty here."
        | Some (_ :: []) -> Entity.setActivityState NoActivity character
        | Some (currentNode :: navigationPath) ->
            let walkDescriptor =
                { WalkDirection = vmtod <| (List.head navigationPath).PositionM - currentNode.PositionM
                  WalkOriginM = vftovm character.Position }
            let navigationDescriptor = { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
            Entity.setActivityState (Navigation navigationDescriptor) character
        | None -> Entity.setActivityState NoActivity character

    let private advanceNavigation navigationDescriptor (character : Entity) =
        let walkDescriptor = navigationDescriptor.WalkDescriptor
        let walkDirection = walkDescriptor.WalkDirection
        let walkOrigin = vmtovf walkDescriptor.WalkOriginM
        let walkDestination = walkOrigin + dtovf walkDirection
        let (newPosition, walkState) =
            match walkDirection with
            | Upward -> let (newY, arrival) = walk true character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | Rightward -> let (newX, arrival) = walk true character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
            | Downward -> let (newY, arrival) = walk false character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
            | Leftward -> let (newX, arrival) = walk false character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
        let character = Entity.setPosition newPosition character
        let characterAnimationState = { character.CharacterAnimationState with CharacterAnimationDirection = walkDirection }
        let character = Entity.setCharacterAnimationState characterAnimationState character
        match walkState with
        | WalkFinished -> advanceNavigationAfterWalkFinished navigationDescriptor character
        | WalkContinuing -> character

    let private advanceAction actionDescriptor (character : Entity) =
        if actionDescriptor.ActionTicks < ActionTicksMax then
            let character =
                let rotation =
                    if actionDescriptor.ActionTicks < ActionTicksMax / 2L
                    then (single Math.PI * -0.03f * single actionDescriptor.ActionTicks)
                    else (single Math.PI * -0.03f * single (ActionTicksMax - actionDescriptor.ActionTicks - 1L))
                Entity.setRotation rotation character
            Entity.setActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks }) character
        else Entity.setActivityState NoActivity character

    let advanceActivity (character : Entity) =
        match character.ActivityState with
        | Action actionDescriptor -> advanceAction actionDescriptor character
        | Navigation navigationDescriptor -> advanceNavigation navigationDescriptor character
        | NoActivity -> character

    let determineTurnFromActivityState (character : Entity) =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation navigationDescriptor ->
            if character.Position = vmtovf navigationDescriptor.WalkDescriptor.WalkOriginM then
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
        let goalNode = Map.find (vftovm touchPosition) nodes
        let currentNode = Map.find (vftovm character.Position) nodes
        let optNavigationPath =
            AStar.FindPath (
                currentNode,
                goalNode,
                (fun n n2 -> if (n2.PositionM.Y <> n.PositionM.Y) then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                (fun n -> 0.0f))
        match optNavigationPath with
        | null -> None
        | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

    let determineTurnFromTouch touchPosition occupationMap (character : Entity) opponents =
        match character.ActivityState with
        | Action _ -> NoTurn
        | Navigation _ -> NoTurn
        | NoActivity ->
            match tryGetNavigationPath touchPosition occupationMap character with
            | Some navigationPath ->
                match navigationPath with
                | [] -> NoTurn
                | _ ->
                    let characterPositionM = vftovm character.Position
                    let walkDirection = vmtod <| (List.head navigationPath).PositionM - characterPositionM
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                    NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
            | None ->
                let targetPosition = touchPosition |> vftovm |> vmtovf
                if Math.arePositionsAdjacent targetPosition character.Position then
                    if List.exists (fun (opponent : Entity) -> opponent.Position = targetPosition) opponents
                    then makeAttackTurn targetPosition
                    else NoTurn
                else NoTurn

    let cancelNavigation (character : Entity) =
        let characterActivity = character.ActivityState
        let characterActivity =
            match characterActivity with
            | Action _ as action -> action
            | NoActivity -> NoActivity
            | Navigation navDescriptor -> Navigation { navDescriptor with OptNavigationPath = None }
        Entity.setActivityState characterActivity character