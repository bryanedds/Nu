// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System

/// Implements an immutable random number generator using the xorshift* algorithm.
/// TODO: document.
type [<StructuralEquality; NoComparison>] Rand =
    { RandState : uint64 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Rand =

    // NOTE: number generated via http://www.random.org/bytes/
    let DefaultSeedState =
        0xa529cb6f5f0385edUL

    let getState rand =
        rand.RandState

    let advance rand =
        let c = rand.RandState
        let c = c ^^^ (c >>> 12)
        let c = c ^^^ (c <<< 25)
        let c = c ^^^ (c >>> 27)
        { RandState = c }

    let sample rand =
        rand.RandState * 2685821657736338717UL

    let nextDouble rand =
        let rand = advance rand
        let sampleDouble = double ^ sample rand
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

    let make seedState =
        if seedState = 0UL then failwith "Seed for Rand may not be zero."
        { RandState = seedState }

    let makeDefault () =
        make DefaultSeedState

    let makeFromInt (seedState : int) =
        let seedStateMultiplier = UInt64.MaxValue / uint64 UInt32.MaxValue
        let seedStateUi64 = uint64 seedState * seedStateMultiplier
        make seedStateUi64