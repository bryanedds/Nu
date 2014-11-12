namespace InfinityRpg
open System
open OpenTK
open TiledSharp
open Prime
open Nu

[<AutoOpen>]
module MetamapModule =

    type Direction with

        static member intToDirection n =
            match n with
            | 0 -> North
            | 1 -> East
            | 2 -> South
            | 3 -> West
            | _ -> failwith <| "Invalid Direction conversion from int '" + acstring n + "'."

        static member rand rand =
            let randMax = 3
            let (randValue, rand) = Rand.next2 randMax rand
            let cardinality = Direction.intToDirection randValue
            (cardinality, rand)

        static member walk (source : Vector2I) cardinality =
            match cardinality with
            | North -> Vector2I (source.X, source.Y + 1)
            | East -> Vector2I (source.X + 1, source.Y)
            | South -> Vector2I (source.X, source.Y - 1)
            | West -> Vector2I (source.X - 1, source.Y)

        static member stumble rand (source : Vector2I) =
            let (cardinality, rand) = Direction.rand rand
            let destination = Direction.walk source cardinality
            (rand, destination)

        static member tryStumbleUntil predicate tryLimit rand (source : Vector2I) =
            let take = if tryLimit < 0 then id else Seq.take tryLimit
            let stumblings =
                take <|
                    Seq.unfold
                        (fun rand -> Some (Direction.stumble rand source, rand))
                        rand
            Seq.tryFind predicate stumblings

        static member wander stumbleLimit rand (source : Vector2I) =
            let stumblePredicate = fun trail (_, destination) -> Set.ofList trail |> Set.contains destination |> not
            Seq.definitize <|
                Seq.unfold
                    (fun ((trail, rand), source) -> Some (Direction.tryStumbleUntil (stumblePredicate trail) stumbleLimit rand source, ((source :: trail, rand), source)))
                    (([], rand), source)

        static member tryWanderUntil predicate tryLimit stumbleLimit rand (source : Vector2I) =
            let take = if tryLimit < 0 then id else Seq.take tryLimit
            let wanderings =
                take <|
                    Seq.unfold
                        (fun (source, rand) -> Some (Direction.wander stumbleLimit rand source, (source, rand)))
                        (source, rand)
            Seq.tryFind predicate wanderings

        static member tryJourney rand (source : Vector2I) =
            let minLength = 10;
            let maxLength = 15;
            let tryLimit = 100;
            let stumbleLimit = 100;
            let predicate = fun (trail : (Rand * Vector2I) seq) ->
                let trail = List.ofSeq <| Seq.take maxLength trail
                if List.length trail >= minLength then
                    let sites = List.map snd trail
                    let uniqueSites = Set.ofList sites
                    List.length sites = Set.count uniqueSites
                else false
            Direction.tryWanderUntil predicate tryLimit stumbleLimit rand source

    type Metatile<'k when 'k : comparison> =
        { ClosedSides : Direction Set
          LockedSides : Map<Direction, 'k>
          Keys : 'k Set }

    type Metamap<'k when 'k : comparison>  =
        { NavigableSize : Vector2I
          PotentiallyNavigableTiles : Map<Vector2I, 'k Metatile> }