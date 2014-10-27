namespace InfinityRpg
open System
open OpenTK
open Nu

[<AutoOpen>]
module MetamapModule =

    // TODO: see if we can get a proper immutable random type to avoid some of the likely
    // surprising behavior that will happen here with System.Random mixed with the use of infinite
    // sequences.

    type Cardinality =
        | North
        | East
        | South
        | West

        static member intToCardinality n =
            match n with
            | 0 -> North
            | 1 -> East
            | 2 -> South
            | 3 -> West
            | _ -> failwith <| "Invalid Cardinality converstion from number '" + string n + "'."

        static member random (random : Random) =
            let randomValueMax = 3
            let randomValue = random.Next randomValueMax
            let cardinality = Cardinality.intToCardinality randomValue
            (random, cardinality)

        static member walk (source : Vector2I) cardinality =
            match cardinality with
            | North -> Vector2I (source.X, source.Y + 1)
            | East -> Vector2I (source.X + 1, source.Y)
            | South -> Vector2I (source.X, source.Y - 1)
            | West -> Vector2I (source.X - 1, source.Y)

        static member stumble (random : Random) (source : Vector2I) =
            let (random, cardinality) = Cardinality.random random
            let destination = Cardinality.walk source cardinality
            (random, destination)

        static member stumbleForever (random : Random) (source : Vector2I) =
            Seq.unfold
                (fun random -> Some <| (Cardinality.stumble random source, random))
                random

        static member stumbleUntil predicate limit (random : Random) (source : Vector2I) =
            let taker = if limit < 0 then id else Seq.take limit
            let attempts = taker <| Cardinality.stumbleForever random source
            Seq.tryFind predicate attempts

        static member wanderForever (random : Random) (source : Vector2I) =
            Seq.unfold
                (fun (random, source) -> Some <| (Cardinality.stumbleForever random source, (random, source)))
                (random, source)

        static member wanderUntil predicate limit stumbleLimit (random : Random) (source : Vector2I) =
            let taker = if limit < 0 then id else Seq.take limit
            let seq = 
                Seq.unfold
                    (fun (random, source) -> Some (Cardinality.stumbleUntil predicate stumbleLimit random source, (random, source)))
                    (random, source)
            taker seq

    type Metapiece<'k when 'k : comparison> =
        { ClosedSides : Cardinality Set
          LockedSides : Map<Cardinality, 'k>
          Keys : 'k Set }

    type Metamap<'k when 'k : comparison>  =
        { NavigableSize : Vector2I
          PotentiallyNavigablePieces : Map<Vector2I, 'k Metapiece> }