// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System

[<AutoOpen>]
module RandModule =

    /// An immutable random number generator using the xorshift* algorithm.
    type [<StructuralEquality; NoComparison>] Rand =
        private
            { RandState : uint64 }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Rand =
    
        /// Get the sample value used to generate the current random value.
        let private sample rand =
            rand.RandState * 2685821657736338717UL
    
        /// The default seed state for rand.
        /// NOTE: number generated via http://www.random.org/bytes/
        let DefaultSeedState = 0xa529cb6f5f0385edUL
    
        /// Advance the state of rand, thus yielding a new outcome.
        let advance rand =
            let c = rand.RandState
            let c = c ^^^ (c >>> 12)
            let c = c ^^^ (c <<< 25)
            let c = c ^^^ (c >>> 27)
            { RandState = c }
    
        /// The internal state of rand, useful for serialization and duplication.
        let getState rand =
            rand.RandState
    
        /// Get the next random value as a double type.
        let nextDouble rand =
            let rand = advance rand
            let sampleDouble = double (sample rand)
            let sampleDoubleMax = double UInt64.MaxValue
            let number = sampleDouble / sampleDoubleMax
            (number, rand)
    
        /// Get the next random value below the given maximum as a double type.
        let nextDoubleUnder max rand =
            let (number, rand) = nextDouble rand
            (number % max, rand)
    
        /// Get the next random value as a double type.
        let nextSingle rand =
            let (numberDouble, rand) = nextDouble rand
            (single numberDouble, rand)
    
        /// Get the next random value below the given maximum as a single type.
        let nextSingleUnder max rand =
            let (number, rand) = nextSingle rand
            (number % max, rand)
            
        /// Get the next random value as an int type.
        /// NOTE: System.Random.Next will never return Int32.MaxValue, but this will.
        let nextInt rand =
            let rand = advance rand
            let sampleInt = int (sample rand >>> 32)
            let number = if sampleInt < 0 then sampleInt + Int32.MaxValue else sampleInt
            (number, rand)
    
        /// Get the next random value below the given maximum as an int type.
        let nextIntUnder max rand =
            let (number, rand) = nextInt rand
            (number % max, rand)
            
        /// Get the next random value as an int64 type.
        /// NOTE: System.Random.Next will never return Int64.MaxValue, but this will.
        let nextInt64 rand =
            let rand = advance rand
            let number = sample rand
            (number, rand)
    
        /// Get the next random value below the given maximum as an int64 type.
        let nextInt64Under max rand =
            let (number, rand) = nextInt64 rand
            (number % max, rand)
    
        /// Make a rand value generator from the given seed state.
        /// May not be zero.
        let makeFromSeedState seedState =
            if seedState = 0UL then failwith "Seed for Rand may not be zero."
            { RandState = seedState }
    
        /// Make a rand value generator from the given int seed state.
        /// May not be zero.
        let makeFromInt (intSeedState : int) =
            let lowState = uint64 intSeedState
            let highState = uint64 intSeedState <<< 32
            let seedState = highState ||| lowState
            makeFromSeedState seedState
    
        /// Make a rand value generator from the default seed state.
        let make () =
            makeFromSeedState DefaultSeedState

/// An immutable random number generator using the xorshift* algorithm.
type Rand = RandModule.Rand