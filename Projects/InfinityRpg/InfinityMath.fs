namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open InfinityRpg

type Tracking =
    | BackTracking
    | NoBackTracking
    | NoAdjacentTracking

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

[<RequireQualifiedAccess>]
module Direction =

    let mutable GlobalWalkCounter = 0
    let mutable GlobalTryStumbleCounter = 0
    let mutable GlobalWanderCounter = 0

    let ofInt n =
        match n with
        | 0 -> Upward
        | 1 -> Rightward
        | 2 -> Downward
        | 3 -> Leftward
        | _ -> failwith ("Invalid conversion to Direction from int '" + scstring n + "'.")

    let next rand =
        let randMax = 4
        let (randValue, rand) = Rand.nextIntUnder randMax rand
        let direction = ofInt randValue
        (direction, rand)

    let walk (source : Vector2i) direction =
        GlobalWalkCounter <- GlobalWalkCounter + 1
        match direction with
        | Upward -> Vector2i (source.X, source.Y + 1)
        | Rightward -> Vector2i (source.X + 1, source.Y)
        | Downward -> Vector2i (source.X, source.Y - 1)
        | Leftward -> Vector2i (source.X - 1, source.Y)

    let private stumbleUnbiased source rand =
        let (direction, rand) = next rand
        let destination = walk source direction
        (destination, rand)

    let stumble biasOpt source rand =
        match biasOpt with
        | Some (goal : Vector2i, bias) ->
            let (biasing, rand) = Rand.nextIntUnder bias rand
            if biasing = 5 then
                let goalDelta = goal - source
                if Math.Abs goalDelta.X > Math.Abs goalDelta.Y
                then (Vector2i (source.X + (if goalDelta.X > 0 then 1 else -1), source.Y), rand)
                else (Vector2i (source.X, source.Y + (if goalDelta.Y > 0 then 1 else -1)), rand)
            else stumbleUnbiased source rand
        | None -> stumbleUnbiased source rand

    let stumbleCandidates tryLimit biasOpt source rand =
        let destinations =
            Seq.unfold
                (fun rand ->
                    let (destination, rand) = stumble biasOpt source rand
                    Some ((destination, rand), rand))
                rand
        let destinations = if tryLimit <= 0 then destinations else Seq.take tryLimit destinations
        destinations
    
    let tryStumbleUntil predicate tryLimit biasOpt source rand =
        GlobalTryStumbleCounter <- GlobalTryStumbleCounter + 1
        let destinations = stumbleCandidates tryLimit biasOpt source rand
        let destinations = Seq.tryFind predicate destinations
        destinations

    let wander stumbleLimit stumbleBounds tracking biasOpt source rand =
        GlobalWanderCounter <- GlobalWanderCounter + 1
        let stumblePredicate =
            fun (trail : Vector2i Set) (destination : Vector2i, _) ->
                Math.isPointInBoundsI destination stumbleBounds &&
                (match tracking with
                 | BackTracking -> true
                 | NoBackTracking -> not (Set.contains destination trail)
                 | NoAdjacentTracking ->
                    let contains =
                        [Set.contains (destination + v2iUp) trail
                         Set.contains (destination + v2iRight) trail
                         Set.contains (destination + v2iDown) trail
                         Set.contains (destination + v2iLeft) trail]
                    let containCount = List.filter ((=) true) contains |> List.length
                    containCount <= 1)
        let pathHead = (source, rand)
        let pathTail =
            Seq.unfold
                (fun (trail, source, rand) ->
                    match tryStumbleUntil (stumblePredicate trail) stumbleLimit biasOpt source rand with
                    | Some (destination, rand) ->
                        let state = (Set.add destination trail, destination, rand)
                        Some ((destination, rand), state)
                    | None -> None)
                (Set.singleton source, source, rand)
        let path = seq { yield pathHead; yield! pathTail }
        path

    let wanderCandidates stumbleLimit stumbleBounds tracking biasOpt tryLimit source rand =
        let paths =
            Seq.unfold
                (fun (source, rand) ->
                    let path = wander stumbleLimit stumbleBounds tracking biasOpt source rand
                    let state = (source, Rand.advance rand)
                    Some (path, state))
                (source, rand)
        let paths = if tryLimit <= 0 then paths else Seq.take tryLimit paths
        paths
    
    let tryWanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt tryLimit source rand =
        let paths = wanderCandidates stumbleLimit stumbleBounds tracking biasOpt tryLimit source rand
        let paths = Seq.tryFind predicate paths
        paths

    let wanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt source rand =
        Option.get (tryWanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt 0 source rand)

    let concretizePath maxLength abstractPath =
        let path = List.ofSeq (Seq.tryTake maxLength abstractPath)
        (List.map fst path, snd (List.last path))

    let concretizePathOpt maxLength pathOpt rand =
        match pathOpt with
        | Some path ->
            let (path, rand) = concretizePath maxLength path
            (Some path, rand)
        | None -> (None, rand)

    let wanderAimlessly stumbleBounds source rand =
        let minLength = 10
        let maxLength = 15
        let tryLimit = 100
        let stumbleLimit = 16
        let predicate = fun (path : (Vector2i * Rand) seq) ->
            let path = List.ofSeq (Seq.tryTake maxLength path)
            if List.length path >= minLength then
                let sites = List.map fst path
                let uniqueSites = Set.ofList sites
                List.length sites = Set.count uniqueSites
            else false
        let pathOpt = tryWanderUntil predicate stumbleLimit stumbleBounds BackTracking None tryLimit source rand
        let path = concretizePathOpt maxLength pathOpt rand
        path

    let wanderToDestination (stumbleBounds : Vector4i) source destination rand =
        let biasOpt = Some (destination, 6)
        let maxPathLength = stumbleBounds.Z * stumbleBounds.W / 2 + 1
        let stumbleLimit = 16        
        let predicate = fun path ->
            let path = Seq.tryTake maxPathLength path
            Seq.exists (fun (point, _) -> point = destination) path
        let path = wanderUntil predicate stumbleLimit stumbleBounds NoAdjacentTracking biasOpt source rand
        let pathDesiredEnd = (Seq.findIndex (fun (point, _) -> point = destination) path) + 1
        let pathTrimmed = Seq.take pathDesiredEnd path
        let path = concretizePath maxPathLength pathTrimmed
        path

[<AutoOpen>]
module MathOperators =
    
    let itoc i =
        i / Constants.Gameplay.TileSizeI.X
    
    let vctovi vm =
        Vector2i.Multiply (vm, Constants.Gameplay.TileSizeI)

    let vitovc vi =
        Vector2i.Divide (vi, Constants.Gameplay.TileSizeI)

    let vitovf (vi : Vector2i) =
        vi.Vector2

    let vftovi (vf : Vector2) =
        Vector2i vf

    let vctovf vm =
        vm |> vctovi |> vitovf

    let vftovc vf =
        vf |> vftovi |> vitovc

    let v2UpScaled i =
        v2 0.0f (float32 i)

    let v2RightScaled i =
        v2 (float32 i) 0.0f

    let v2DownScaled i =
        v2 0.0f -(float32 i)

    let v2LeftScaled i =
        v2 -(float32 i) 0.0f
    
    let dtovc d =
        match d with
        | Upward -> v2iUp
        | Rightward -> v2iRight
        | Downward -> v2iDown
        | Leftward -> v2iLeft

    let dtovcScaled d i =
        dtovc d * v2iDup i

    let dtovf d =
        match d with
        | Upward -> v2Up
        | Rightward -> v2Right
        | Downward -> v2Down
        | Leftward -> v2Left

    let dtovfScaled d i =
        dtovf d * v2Dup i

    let dtovi d =
        dtovc d |> vctovi

    let dtoviScaled d i =
        dtovfScaled d i |> vftovi

    let vftod v =
        if v <> v2Zero then
            let atan2 = Math.Atan2 (float v.Y, float v.X)
            let angle = if atan2 < 0.0 then atan2 + Math.PI * 2.0 else atan2
            if angle < Math.PI * 0.75 && angle >= Math.PI * 0.25 then Upward
            elif angle < Math.PI * 0.25 || angle >= Math.PI * 1.75 then Rightward
            elif angle < Math.PI * 1.75 && angle >= Math.PI * 1.25 then Downward
            else Leftward
        else failwith "Direction cannot be derived from [0.0f 0.0f]."

    let vitod v =
        vitovf v |> vftod

    let vctod v =
        vctovf v |> vftod

[<RequireQualifiedAccess>]
module Math =

    let isSnapped i =
        i % Constants.Gameplay.TileSizeI.X = 0

    let directionToTarget current target =
        target - current |> vctod

    let areCoordinatesAdjacent coordinates coordinates2 =
        coordinates = coordinates2 + v2iUp ||
        coordinates = coordinates2 + v2iRight ||
        coordinates = coordinates2 + v2iDown ||
        coordinates = coordinates2 + v2iLeft

    let arePositionIsAdjacent positionI positionI2 =
        areCoordinatesAdjacent (vitovc positionI) (vitovc positionI2)

    let arePositionsAdjacent position position2 =
        arePositionIsAdjacent (vftovi position) (vftovi position2)