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

        static member rand rand =
            let randMax = 4
            let (randValue, rand) = Rand.nextInt2 randMax rand
            let cardinality = Direction.intToDirection randValue
            (cardinality, rand)

        static member walk (source : Vector2I) cardinality =
            DebugWalkCounter <- DebugWalkCounter + 1
            match cardinality with
            | North -> Vector2I (source.X, source.Y + 1)
            | East -> Vector2I (source.X + 1, source.Y)
            | South -> Vector2I (source.X, source.Y - 1)
            | West -> Vector2I (source.X - 1, source.Y)

        static member private stumbleUnbiased source rand =
            let (cardinality, rand) = Direction.rand rand
            let destination = Direction.walk source cardinality
            (destination, rand)

        static member stumble optBias (source : Vector2I) rand =
            match optBias with
            | Some ((goal : Vector2I), bias) ->
                let (biasing, rand) = Rand.nextInt2 bias rand
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

        static member wander stumbleLimit (stumbleBounds : Vector2I) backtracking optBias source rand =
            DebugWanderCounter <- DebugWanderCounter + 1
            let stumblePredicate =
                fun (trail : Vector2I Set) (destination : Vector2I, rand) ->
                    destination.X >= 0 &&
                    destination.X <= stumbleBounds.X &&
                    destination.Y >= 0 &&
                    destination.Y <= stumbleBounds.Y &&
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

        static member concretizePath maxLength unevaluatedPath rand =
            let path = List.ofSeq <| Seq.tryTake maxLength unevaluatedPath
            (List.map fst path, snd ^^ List.last path)

        static member concretizeOptPath maxLength unevaluatedPath rand =
            match unevaluatedPath with
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

        static member wanderToDestination (stumbleBounds : Vector2I) source destination rand =
            let optBias = Some (destination, 8)
            let maxPathLength = stumbleBounds.X * stumbleBounds.Y / 2
            let stumbleLimit = 16
            let predicate = fun (path : (Vector2I * Rand) seq) ->
                let path = Seq.tryTake maxPathLength path
                Seq.exists (fun point -> fst point = destination) path
            let path = Direction.wanderUntil predicate stumbleLimit stumbleBounds true optBias source rand
            Direction.concretizePath maxPathLength path rand

    type MetaTile<'k when 'k : comparison> =
        { ClosedSides : Direction Set
          LockedSides : Map<Direction, 'k>
          Keys : 'k Set }

    type MetaMap<'k when 'k : comparison>  =
        { NavigableSize : Vector2I
          PotentiallyNavigableTiles : Map<Vector2I, 'k MetaTile> }