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

    type Direction with

        static member intToDirection n =
            match n with
            | 0 -> North
            | 1 -> East
            | 2 -> South
            | 3 -> West
            | _ -> failwith <| "Invalid conversion to Direction from int '" + acstring n + "'."

        static member next rand =
            let randMax = 4
            let (randValue, rand) = Rand.nextIntUnder randMax rand
            let direction = Direction.intToDirection randValue
            (direction, rand)

        static member walk (source : Vector2I) direction =
            DebugWalkCounter <- DebugWalkCounter + 1
            match direction with
            | North -> Vector2I (source.X, source.Y + 1)
            | East -> Vector2I (source.X + 1, source.Y)
            | South -> Vector2I (source.X, source.Y - 1)
            | West -> Vector2I (source.X - 1, source.Y)

        static member private stumbleUnbiased source rand =
            let (direction, rand) = Direction.next rand
            let destination = Direction.walk source direction
            (destination, rand)

        static member stumble optBias (source : Vector2I) rand =
            match optBias with
            | Some ((goal : Vector2I), bias) ->
                let (biasing, rand) = Rand.nextIntUnder bias rand
                if biasing = 0 then
                    let goalDelta = goal - source
                    if Math.Abs goalDelta.X > Math.Abs goalDelta.Y
                    then (Vector2I (source.X + (if goalDelta.X > 0 then 1 else -1), source.Y), rand)
                    else (Vector2I (source.X, source.Y + (if goalDelta.Y > 0 then 1 else -1)), rand)
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

        static member wander stumbleLimit (stumbleBounds : Vector2I * Vector2I) backtracking optBias source rand =
            DebugWanderCounter <- DebugWanderCounter + 1
            let stumblePredicate =
                fun (trail : Vector2I Set) (destination : Vector2I, rand) ->
                    destination.X >= (fst stumbleBounds).X &&
                    destination.X <= (snd stumbleBounds).X &&
                    destination.Y >= (fst stumbleBounds).Y &&
                    destination.Y <= (snd stumbleBounds).Y &&
                    (backtracking || not <| Set.contains destination trail) // NOTE: this line is almost certainly the bottleneck!
            Seq.unfold
                (fun (source, trail, rand) ->
                    match Direction.tryStumbleUntil (stumblePredicate trail) stumbleLimit optBias source rand with
                    | Some (destination, rand) ->
                        let trail = Set.add source trail
                        let state = (destination, trail, rand)
                        Some ((source, rand), state)
                    | None -> None)
                (source, Set.empty, rand)

        static member tryWanderUntil predicate stumbleLimit stumbleBounds backtracking optBias tryLimit source rand =
            let take = if tryLimit <= 0 then id else Seq.take tryLimit
            let paths =
                take <|
                    Seq.unfold
                        (fun (source, rand) ->
                            let path = Direction.wander stumbleLimit stumbleBounds backtracking optBias source rand
                            let state = (source, Rand.advance rand)
                            Some (path, state))
                        (source, rand)
            Seq.tryFind predicate paths

        static member wanderUntil predicate stumbleLimit stumbleBounds backtracking optBias source rand =
            Option.get <| Direction.tryWanderUntil predicate stumbleLimit stumbleBounds backtracking optBias 0 source rand

        static member concretizePath maxLength abstractPath rand =
            let path = List.ofSeq <| Seq.tryTake maxLength abstractPath
            (List.map fst path, snd ^^ List.last path)

        static member concretizeOptPath maxLength abstractPath rand =
            match abstractPath with
            | Some path ->
                let (path, rand) = Direction.concretizePath maxLength path rand
                (Some path, rand)
            | None -> (None, rand)

        static member wanderTenToFifteenUnits stumbleBounds source rand =
            let minLength = 10
            let maxLength = 15
            let tryLimit = 100
            let stumbleLimit = 16
            let predicate = fun (path : (Vector2I * Rand) seq) ->
                let path = List.ofSeq <| Seq.tryTake maxLength path
                if List.length path >= minLength then
                    let sites = List.map fst path
                    let uniqueSites = Set.ofList sites
                    List.length sites = Set.count uniqueSites
                else false
            let path = Direction.tryWanderUntil predicate stumbleLimit stumbleBounds false None tryLimit source rand
            Direction.concretizeOptPath maxLength path rand

        static member wanderToDestination (stumbleBounds : Vector2I * Vector2I) source destination rand =
            let optBias = Some (destination, 4)
            let maxPathLength = (snd stumbleBounds).X * (snd stumbleBounds).Y / 2
            let stumbleLimit = 16
            let predicate = fun (path : (Vector2I * Rand) seq) ->
                let path = Seq.tryTake maxPathLength path
                Seq.exists (fun point -> fst point = destination) path
            let path = Direction.wanderUntil predicate stumbleLimit stumbleBounds true optBias source rand
            let pathDesiredEnd = Seq.findIndex (fun (point, _) -> point = destination) path + 1
            let pathTrimmed = Seq.take pathDesiredEnd path
            Direction.concretizePath maxPathLength pathTrimmed rand

    type MetaTile<'k when 'k : comparison> =
        { ClosedSides : Direction Set
          LockedSides : Map<Direction, 'k>
          Keys : 'k Set }

    type MetaMap<'k when 'k : comparison>  =
        { NavigableSize : Vector2I
          PotentiallyNavigableTiles : Map<Vector2I, 'k MetaTile> }