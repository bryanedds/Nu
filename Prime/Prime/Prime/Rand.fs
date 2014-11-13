namespace Prime
open System

[<AutoOpen>]
module RandModule =

    /// Implements an immutable random number generator using the xorshift* algorithm.
    type [<StructuralEquality; NoComparison>] Rand =
        { Current : uint64 }

[<RequireQualifiedAccess>]
module Rand =

    let make seed =
        if seed = 0UL then failwith "Seed for Rand may not be zero."
        { Current = seed }

    let makeDefault () =
        // NOTE: number generated via http://www.random.org/bytes/
        make 0xa529cb6f5f0385edUL

    let makeFromInt (seed : int) =
        let seedMultiplier = UInt64.MaxValue / uint64 UInt32.MaxValue
        let seedUi64 = uint64 seed * seedMultiplier
        make seedUi64

    let advance rand =
        let c = rand.Current
        let c = c ^^^ (c >>> 12)
        let c = c ^^^ (c <<< 25)
        let c = c ^^^ (c >>> 27)
        { Current = c }

    let sample rand =
        rand.Current * 2685821657736338717UL

    let nextDouble rand =
        let rand = advance rand
        let sampleDouble = double <| sample rand
        let sampleDoubleMax = double UInt64.MaxValue
        let number = sampleDouble / sampleDoubleMax
        (number, rand)

    let nextDoubleUnder max rand =
        let (numberDouble, rand) = nextDouble rand
        (numberDouble % max, rand)

    let nextSingle rand =
        let (numberDouble, rand) = nextDouble rand
        (single numberDouble, rand)

    let nextSingleUnder max rand =
        let (numberSingle, rand) = nextSingle rand
        (numberSingle % max, rand)
        
    // NOTE: System.Random.Next will never return Int32.MaxValue, but this will.
    let nextInt rand =
        let rand = advance rand
        let sampleInt = int (sample rand >>> 32)
        let number = if sampleInt < 0 then sampleInt + Int32.MaxValue else sampleInt
        (number, rand)

    let nextIntUnder max rand =
        let (numberInt, rand) = nextInt rand
        (numberInt % max, rand)