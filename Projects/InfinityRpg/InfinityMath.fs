namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

type Tracking =
    | BackTracking
    | NoBackTracking
    | NoAdjacentTracking

type MapBounds =
    { CornerNegative : Vector2i
      CornerPositive : Vector2i }

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MapBounds =

    let isPointInBounds (point : Vector2i) bounds =
        not
            (point.X < bounds.CornerNegative.X ||
             point.X > bounds.CornerPositive.X ||
             point.Y < bounds.CornerNegative.Y ||
             point.Y > bounds.CornerPositive.Y)

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Direction =

    let mutable GlobalWalkCounter = 0
    let mutable GlobalTryStumbleCounter = 0
    let mutable GlobalWanderCounter = 0

    let fromInt n =
        match n with
        | 0 -> Upward
        | 1 -> Rightward
        | 2 -> Downward
        | 3 -> Leftward
        | _ -> failwith ^ "Invalid conversion to Direction from int '" + scstring n + "'."

    let next rand =
        let randMax = 4
        let (randValue, rand) = Rand.nextIntUnder randMax rand
        let direction = fromInt randValue
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
            if biasing = 0 then
                let goalDelta = goal - source
                if Math.Abs goalDelta.X > Math.Abs goalDelta.Y
                then (Vector2i (source.X + (if goalDelta.X > 0 then 1 else -1), source.Y), rand)
                else (Vector2i (source.X, source.Y + (if goalDelta.Y > 0 then 1 else -1)), rand)
            else stumbleUnbiased source rand
        | None -> stumbleUnbiased source rand

    let tryStumbleUntil predicate tryLimit biasOpt source rand =
        GlobalTryStumbleCounter <- GlobalTryStumbleCounter + 1
        let destinations =
            Seq.unfold
                (fun rand ->
                    let (destination, rand) = stumble biasOpt source rand
                    Some ((destination, rand), rand))
                rand
        let destinations = if tryLimit <= 0 then destinations else Seq.take tryLimit destinations
        let destinations = Seq.tryFind predicate destinations
        destinations

    let wander stumbleLimit stumbleBounds tracking biasOpt source rand =
        GlobalWanderCounter <- GlobalWanderCounter + 1
        let stumblePredicate =
            fun (trail : Vector2i Set) (destination : Vector2i, _) ->
                MapBounds.isPointInBounds destination stumbleBounds &&
                (match tracking with
                 | BackTracking -> true
                 | NoBackTracking -> not ^ Set.contains destination trail
                 | NoAdjacentTracking ->
                    let contains =
                        [Set.contains (destination + Vector2i.Up) trail
                         Set.contains (destination + Vector2i.Right) trail
                         Set.contains (destination + Vector2i.Down) trail
                         Set.contains (destination + Vector2i.Left) trail]
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

    let tryWanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt tryLimit source rand =
        let paths =
            Seq.unfold
                (fun (source, rand) ->
                    let path = wander stumbleLimit stumbleBounds tracking biasOpt source rand
                    let state = (source, Rand.advance rand)
                    Some (path, state))
                (source, rand)
        let paths = if tryLimit <= 0 then paths else Seq.take tryLimit paths
        let paths = Seq.tryFind predicate paths
        paths

    let wanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt source rand =
        Option.get ^ tryWanderUntil predicate stumbleLimit stumbleBounds tracking biasOpt 0 source rand

    let concretizePath maxLength abstractPath =
        let path = List.ofSeq ^ Seq.tryTake maxLength abstractPath
        (List.map fst path, snd ^ List.last path)

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
            let path = List.ofSeq ^ Seq.tryTake maxLength path
            if List.length path >= minLength then
                let sites = List.map fst path
                let uniqueSites = Set.ofList sites
                List.length sites = Set.count uniqueSites
            else false
        let pathOpt = tryWanderUntil predicate stumbleLimit stumbleBounds BackTracking None tryLimit source rand
        let path = concretizePathOpt maxLength pathOpt rand
        path

    let wanderToDestination stumbleBounds source destination rand =
        let biasOpt = Some (destination, 6)
        let maxPathLength = stumbleBounds.CornerPositive.X * stumbleBounds.CornerPositive.Y / 2 + 1
        let stumbleLimit = 16
        let predicate = fun path ->
            let path = Seq.tryTake maxPathLength path
            Seq.exists (fun point -> fst point = destination) path
        let path = wanderUntil predicate stumbleLimit stumbleBounds NoAdjacentTracking biasOpt source rand
        let pathDesiredEnd = Seq.findIndex (fun (point, _) -> point = destination) path + 1
        let pathTrimmed = Seq.take pathDesiredEnd path
        let path = concretizePath maxPathLength pathTrimmed
        path

[<AutoOpen>]
module MathModule =

    let vmtovi vm =
        Vector2i.Multiply (vm, Constants.Layout.TileSizeI)

    let vitovm vi =
        Vector2i.Divide (vi, Constants.Layout.TileSizeI)

    let vitovf (vi : Vector2i) =
        vi.Vector2

    let vftovi (vf : Vector2) =
        Vector2i vf

    let vmtovf vm =
        vm |> vmtovi |> vitovf

    let vftovm vf =
        vf |> vftovi |> vitovm

    let dtovm d =
        match d with
        | Upward -> Vector2i.Up
        | Rightward -> Vector2i.Right
        | Downward -> Vector2i.Down
        | Leftward -> Vector2i.Left

    let dtovi d =
        dtovm d |> vmtovi

    let dtovf d =
        d |> dtovi |> vitovf

    let vftod v =
        if v <> Vector2.Zero then
            let atan2 = Math.Atan2 (float v.Y, float v.X)
            let angle = if atan2 < 0.0 then atan2 + Math.PI * 2.0 else atan2
            if angle < Math.PI * 0.75 && angle >= Math.PI * 0.25 then Upward
            elif angle < Math.PI * 0.25 || angle >= Math.PI * 1.75 then Rightward
            elif angle < Math.PI * 1.75 && angle >= Math.PI * 1.25 then Downward
            else Leftward
        else failwith "Direction cannot be derived from Vector2.Zero."

    let vitod v =
        v |> vitovf |> vftod

    let vmtod v =
        v |> vmtovf |> vftod

module Math =

    let arePositionMsAdjacent positionM positionM2 =
        positionM = positionM2 + Vector2i.Up ||
        positionM = positionM2 + Vector2i.Right ||
        positionM = positionM2 + Vector2i.Down ||
        positionM = positionM2 + Vector2i.Left

    let arePositionIsAdjacent positionI positionI2 =
        arePositionMsAdjacent (vitovm positionI) (vitovm positionI2)

    let arePositionsAdjacent position position2 =
        arePositionIsAdjacent (vftovi position) (vftovi position2)