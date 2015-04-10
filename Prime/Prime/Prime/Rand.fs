// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System

/// Implements an immutable random number generator using the xorshift* algorithm.
type [<StructuralEquality; NoComparison>] Rand =
    { RandState : uint64 }

    // NOTE: number generated via http://www.random.org/bytes/
    static member DefaultSeedState =
        0xa529cb6f5f0385edUL

    static member make seedState =
        if seedState = 0UL then failwith "Seed for Rand may not be zero."
        { RandState = seedState }

    static member makeDefault () =
        Rand.make Rand.DefaultSeedState

    static member makeFromInt (seedState : int) =
        let seedStateMultiplier = UInt64.MaxValue / uint64 UInt32.MaxValue
        let seedStateUi64 = uint64 seedState * seedStateMultiplier
        Rand.make seedStateUi64

    static member getState rand =
        rand.RandState

    static member advance rand =
        let c = rand.RandState
        let c = c ^^^ (c >>> 12)
        let c = c ^^^ (c <<< 25)
        let c = c ^^^ (c >>> 27)
        { RandState = c }

    static member sample rand =
        rand.RandState * 2685821657736338717UL

    static member nextDouble rand =
        let rand = Rand.advance rand
        let sampleDouble = double <| Rand.sample rand
        let sampleDoubleMax = double UInt64.MaxValue
        let number = sampleDouble / sampleDoubleMax
        (number, rand)

    static member nextDoubleUnder max rand =
        let (numberDouble, rand) = Rand.nextDouble rand
        (numberDouble % max, rand)

    static member nextSingle rand =
        let (numberDouble, rand) = Rand.nextDouble rand
        (single numberDouble, rand)

    static member nextSingleUnder max rand =
        let (numberSingle, rand) = Rand.nextSingle rand
        (numberSingle % max, rand)
        
    // NOTE: System.Random.Next will never return Int32.MaxValue, but this will.
    static member nextInt rand =
        let rand = Rand.advance rand
        let sampleInt = int (Rand.sample rand >>> 32)
        let number = if sampleInt < 0 then sampleInt + Int32.MaxValue else sampleInt
        (number, rand)

    static member nextIntUnder max rand =
        let (numberInt, rand) = Rand.nextInt rand
        (numberInt % max, rand)