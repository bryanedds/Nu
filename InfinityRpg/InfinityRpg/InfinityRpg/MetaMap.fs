namespace InfinityRpg
open System
open OpenTK
open TiledSharp
open Prime
open Nu

[<AutoOpen>]
module MetamapModule =

    let mutable DebugWalkCounter = 0
    let mutable DebugTryStumbleCounter = 0
    let mutable DebugWanderCounter = 0

    type Bounds =
        { CornerNegative : Vector2i
          CornerPositive : Vector2i }

        static member isPointInBounds (point : Vector2i) bounds =
            not
                (point.X < bounds.CornerNegative.X ||
                 point.X > bounds.CornerPositive.X ||
                 point.Y < bounds.CornerNegative.Y ||
                 point.Y > bounds.CornerPositive.Y)

    type Tracking =
        | BackTracking
        | NoBackTracking
        | NoAdjacentTracking

    type Direction with

        static member fromInt n =
            match n with
            | 0 -> North
            | 1 -> East
            | 2 -> South
            | 3 -> West
            | _ -> failwith <| "Invalid conversion to Direction from int '" + acstring n + "'."

        static member toVector2i direction =
            match direction with
            | North -> Vector2i.Up
            | East -> Vector2i.Right
            | South -> Vector2i.Down
            | West -> Vector2i.Left

        static member next rand =
            let randMax = 4
            let (randValue, rand) = Rand.nextIntUnder randMax rand
            let direction = Direction.fromInt randValue
            (direction, rand)

        static member walk (source : Vector2i) direction =
            DebugWalkCounter <- DebugWalkCounter + 1
            match direction with
            | North -> Vector2i (source.X, source.Y + 1)
            | East -> Vector2i (source.X + 1, source.Y)
            | South -> Vector2i (source.X, source.Y - 1)
            | West -> Vector2i (source.X - 1, source.Y)

        static member private stumbleUnbiased source rand =
            let (direction, rand) = Direction.next rand
            let destination = Direction.walk source direction
            (destination, rand)

        static member stumble optBias source rand =
            match optBias with
            | Some (goal : Vector2i, bias) ->
                let (biasing, rand) = Rand.nextIntUnder bias rand
                if biasing = 0 then
                    let goalDelta = goal - source
                    if Math.Abs goalDelta.X > Math.Abs goalDelta.Y
                    then (Vector2i (source.X + (if goalDelta.X > 0 then 1 else -1), source.Y), rand)
                    else (Vector2i (source.X, source.Y + (if goalDelta.Y > 0 then 1 else -1)), rand)
                else Direction.stumbleUnbiased source rand
            | None -> Direction.stumbleUnbiased source rand

        static member tryStumbleUntil predicate tryLimit optBias source rand =
            DebugTryStumbleCounter <- DebugTryStumbleCounter + 1
            let take = if tryLimit <= 0 then id else Seq.take tryLimit
            let destinations =
                take <|
                    Seq.unfold
                        (fun rand ->
                            let (destination, rand) = Direction.stumble optBias source rand
                            Some ((destination, rand), rand))
                        rand
            Seq.tryFind predicate destinations

        static member wander stumbleLimit stumbleBounds tracking optBias source rand =
            DebugWanderCounter <- DebugWanderCounter + 1
            let stumblePredicate =
                fun (trail : Vector2i Set) (destination : Vector2i, rand) ->
                    Bounds.isPointInBounds destination stumbleBounds &&
                    (match tracking with
                     | BackTracking -> true
                     | NoBackTracking -> not <| Set.contains destination trail
                     | NoAdjacentTracking ->
                        let contains =
                            [Set.contains (destination + Vector2i.Up) trail
                             Set.contains (destination + Vector2i.Right) trail
                             Set.contains (destination + Vector2i.Down) trail
                             Set.contains (destination + Vector2i.Left) trail]
                        let containCount = List.filter ((=) true) contains |> List.length
                        containCount <= 1)
            Seq.unfold
                (fun (source, trail, rand) ->
                    match Direction.tryStumbleUntil (stumblePredicate trail) stumbleLimit optBias source rand with
                    | Some (destination, rand) ->
                        let state = (destination, Set.add destination trail, rand)
                        Some ((source, rand), state)
                    | None -> None)
                (source, Set.empty, rand)

        static member tryWanderUntil predicate stumbleLimit stumbleBounds tracking optBias tryLimit source rand =
            let take = if tryLimit <= 0 then id else Seq.take tryLimit
            let paths =
                take <|
                    Seq.unfold
                        (fun (source, rand) ->
                            let path = Direction.wander stumbleLimit stumbleBounds tracking optBias source rand
                            let state = (source, Rand.advance rand)
                            Some (path, state))
                        (source, rand)
            Seq.tryFind predicate paths

        static member wanderUntil predicate stumbleLimit stumbleBounds tracking optBias source rand =
            Option.get <| Direction.tryWanderUntil predicate stumbleLimit stumbleBounds tracking optBias 0 source rand

        static member concretizePath maxLength abstractPath rand =
            let path = List.ofSeq <| Seq.tryTake maxLength abstractPath
            (List.map fst path, snd ^^ List.last path)

        static member concretizeOptPath maxLength abstractPath rand =
            match abstractPath with
            | Some path ->
                let (path, rand) = Direction.concretizePath maxLength path rand
                (Some path, rand)
            | None -> (None, rand)

        static member wanderAimlessly stumbleBounds source rand =
            let minLength = 10
            let maxLength = 15
            let tryLimit = 100
            let stumbleLimit = 16
            let predicate = fun (path : (Vector2i * Rand) seq) ->
                let path = List.ofSeq <| Seq.tryTake maxLength path
                if List.length path >= minLength then
                    let sites = List.map fst path
                    let uniqueSites = Set.ofList sites
                    List.length sites = Set.count uniqueSites
                else false
            let path = Direction.tryWanderUntil predicate stumbleLimit stumbleBounds BackTracking None tryLimit source rand
            Direction.concretizeOptPath maxLength path rand

        static member wanderToDestination stumbleBounds source destination rand =
            let optBias = Some (destination, 6)
            let maxPathLength = stumbleBounds.CornerPositive.X * stumbleBounds.CornerPositive.Y / 2 + 1
            let stumbleLimit = 16
            let predicate = fun path ->
                let path = Seq.tryTake maxPathLength path
                Seq.exists (fun point -> fst point = destination) path
            let path = Direction.wanderUntil predicate stumbleLimit stumbleBounds NoAdjacentTracking optBias source rand
            let pathDesiredEnd = Seq.findIndex (fun (point, _) -> point = destination) path + 1
            let pathTrimmed = Seq.take pathDesiredEnd path
            Direction.concretizePath maxPathLength pathTrimmed rand

    type MetaTile<'k when 'k : comparison> =
        { ClosedSides : Direction Set
          LockedSides : Map<Direction, 'k>
          Keys : 'k Set }

    type MetaMap<'k when 'k : comparison>  =
        { NavigableSize : Vector2i
          PotentiallyNavigableTiles : Map<Vector2i, 'k MetaTile> }