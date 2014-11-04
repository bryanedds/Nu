namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module MetamapModule =

    // TODO: see if we can get a proper immutable random type to avoid some of the likely
    // surprising behavior that will happen here with System.Random is mixed with the use of
    // infinite sequences.

    type Direction with

        static member intToDirection n =
            match n with
            | 0 -> North
            | 1 -> East
            | 2 -> South
            | 3 -> West
            | _ -> failwith <| "Invalid Direction conversion from int '" + xstring n + "'."

        static member random (random : Random) =
            let randomValueMax = 3
            let randomValue = random.Next randomValueMax
            let cardinality = Direction.intToDirection randomValue
            (random, cardinality)

        static member walk (source : Vector2I) cardinality =
            match cardinality with
            | North -> Vector2I (source.X, source.Y + 1)
            | East -> Vector2I (source.X + 1, source.Y)
            | South -> Vector2I (source.X, source.Y - 1)
            | West -> Vector2I (source.X - 1, source.Y)

        static member stumble (random : Random) (source : Vector2I) =
            let (random, cardinality) = Direction.random random
            let destination = Direction.walk source cardinality
            (random, destination)

        static member tryStumbleUntil predicate tryLimit (random : Random) (source : Vector2I) =
            let take = if tryLimit < 0 then id else Seq.take tryLimit
            let stumblings =
                take <|
                    Seq.unfold
                        (fun random -> Some (Direction.stumble random source, random))
                        random
            Seq.tryFind predicate stumblings

        static member wander stumbleLimit (random : Random) (source : Vector2I) =
            let stumblePredicate = fun trail (_, destination) -> Set.ofList trail |> Set.contains destination |> not
            Seq.definitize <|
                Seq.unfold
                    (fun ((random, trail), source) -> Some (Direction.tryStumbleUntil (stumblePredicate trail) stumbleLimit random source, ((random, source :: trail), source)))
                    ((random, []), source)

        static member tryWanderUntil predicate tryLimit stumbleLimit (random : Random) (source : Vector2I) =
            let take = if tryLimit < 0 then id else Seq.take tryLimit
            let wanderings =
                take <|
                    Seq.unfold
                        (fun (random, source) -> Some (Direction.wander stumbleLimit random source, (random, source)))
                        (random, source)
            Seq.tryFind predicate wanderings

        static member tryJourney (random : Random) (source : Vector2I) =
            let minLength = 10;
            let maxLength = 15;
            let tryLimit = 100;
            let stumbleLimit = 100;
            let predicate = fun (trail : (Random * Vector2I) seq) ->
                let trail = List.ofSeq <| Seq.take maxLength trail
                if List.length trail >= minLength then
                    let sites = List.map snd trail
                    let uniqueSites = Set.ofList sites
                    List.length sites = Set.count uniqueSites
                else false
            Direction.tryWanderUntil predicate tryLimit stumbleLimit random source

    type Metapiece<'k when 'k : comparison> =
        { ClosedSides : Direction Set
          LockedSides : Map<Direction, 'k>
          Keys : 'k Set }

    type Metamap<'k when 'k : comparison>  =
        { NavigableSize : Vector2I
          PotentiallyNavigablePieces : Map<Vector2I, 'k Metapiece> }