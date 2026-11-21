// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Threading
open Prime

/// Provides engine-specific value generation functionality.
[<RequireQualifiedAccess>]
module Gen =

    let private Lock = obj ()
    let mutable private Id32 = 0u
    let mutable private Id64 = 0UL
    let mutable private IdForEditor = 0UL

    /// The prefix of a generated name
    let NamePrefix = "@"

    /// The separator of a generated name
    let NameSeparator = "-"

    /// Generates engine-specific values on-demand.
    type Gen =
        private | Gen of unit

        /// Get the next random number integer.
        /// Thread-safe.
        static member random =
            Random.Shared.Next ()

        /// Get the next random boolean.
        /// Thread-safe.
        static member randomb =
            Random.Shared.Next () < Int32.MaxValue / 2

        /// Get the next random byte.
        /// Thread-safe.
        static member randomy =
            byte (Random.Shared.Next ())

        /// Get the next random unsigned.
        /// Thread-safe.
        static member randomu =
            uint (Random.Shared.Next ())

        /// Get the next random long.
        /// Thread-safe.
        static member randoml =
            Random.Shared.NextInt64 ()

        /// Get the next random unsigned long.
        /// Thread-safe.
        static member randomul =
            uint64 (Random.Shared.NextInt64 ())

        /// Get the next random single >= 0.0f and < 1.0f.
        /// Thread-safe.
        static member randomf =
            Random.Shared.NextSingle ()

        /// Get the next random double >= 0.0 and < 1.0.
        /// Thread-safe.
        static member randomd =
            Random.Shared.NextDouble ()
            
        /// Get the next random number integer below ceiling.
        /// Thread-safe.
        static member random1 ceiling =
            Random.Shared.Next ceiling
            
        /// Get the next random number single below ceiling.
        /// Thread-safe.
        static member randomy1 (ceiling : byte) =
            byte (Random.Shared.Next (int ceiling))
            
        /// Get the next random number single below ceiling.
        /// Thread-safe.
        static member randomf1 ceiling =
            Random.Shared.NextSingle () * ceiling
            
        /// Get the next random number single below ceiling.
        /// Thread-safe.
        static member randomd1 ceiling =
            Random.Shared.NextDouble () * ceiling

        /// Get the next random number integer GTE minValue and LT ceiling.
        /// Thread-safe.
        static member random2 minValue ceiling =
            Random.Shared.Next (minValue, ceiling)

        /// Get a random element from a sequence if there are any elements or None.
        /// If seq is large, this may allocate to the LOH.
        /// Thread-safe.
        static member randomChoiceOpt seq =
            if Seq.isEmpty seq then None else Some (Seq.randomChoice seq)

        /// Get a random element from a sequence or a default if sequence is empty.
        /// Thread-safe.
        static member randomChoiceOrDefault default_ seq =
            match Gen.randomChoiceOpt seq with
            | Some item -> item
            | None -> default_

        /// Get a random element from a sequence, throwing if the sequence is empty.
        /// Thread-safe.
        static member randomChoice seq =
            Seq.randomChoice seq

        /// Get a random key if there are any or None.
        /// Thread-safe.
        static member randomKeyOpt (dict : IDictionary<'k, 'v>) =
            Gen.randomChoiceOpt dict.Keys

        /// Get a random value if there are any or None.
        /// Thread-safe.
        static member randomValueOpt (dict : IDictionary<'k, 'v>) =
            Gen.randomChoiceOpt dict.Values

        /// Randomly shuffle a sequence.
        /// If seq is large, this may allocate to the LOH.
        /// Thread-safe.
        static member randomShuffle (seq : 'a seq) =
            Seq.randomShuffle seq

        /// Get the random number integer for a target under a seed, GTE 0 and LTE Int32.MaxValue. The target is any
        /// value HashdFolder folds, so the same seed and target give the same number on every run and machine.
        /// Thread-safe.
        static member inline randomk (target : ^a) (seed : uint64) =
            int (Randomk.toUInt64 target (Randomk.make seed) >>> 33)

        /// Get the random boolean for a target under a seed.
        /// Thread-safe.
        static member inline randomkb (target : ^a) (seed : uint64) =
            (Randomk.toUInt64 target (Randomk.make seed)) >>> 63 = 1UL

        /// Get the random byte for a target under a seed.
        /// Thread-safe.
        static member inline randomky (target : ^a) (seed : uint64) =
            byte ((Randomk.toUInt64 target (Randomk.make seed)) >>> 56)

        /// Get the random unsigned for a target under a seed, GTE 0 and LTE Int32.MaxValue.
        /// Thread-safe.
        static member inline randomku (target : ^a) (seed : uint64) =
            uint (Gen.randomk target seed)

        /// Get the random long for a target under a seed, GTE 0 and LTE Int64.MaxValue.
        /// Thread-safe.
        static member inline randomkl (target : ^a) (seed : uint64) =
            int64 ((Randomk.toUInt64 target (Randomk.make seed)) >>> 1)

        /// Get the random unsigned long for a target under a seed, GTE 0 and LTE Int64.MaxValue.
        /// Thread-safe.
        static member inline randomkul (target : ^a) (seed : uint64) =
            uint64 (Gen.randomkl target seed)

        /// Get the random single for a target under a seed, >= 0.0f and < 1.0f.
        /// Thread-safe.
        static member inline randomkf (target : ^a) (seed : uint64) =
            Randomk.toSingle target (Randomk.make seed)

        /// Get the random double for a target under a seed, >= 0.0 and < 1.0.
        /// Thread-safe.
        static member inline randomkd (target : ^a) (seed : uint64) =
            Randomk.toDouble target (Randomk.make seed)

        /// Get the random number integer for a target under a seed, below ceiling, or 0 when ceiling is 0.
        /// Thread-safe.
        static member inline randomk3 (ceiling : int) (target : ^a) (seed : uint64) =
            if ceiling < 0 then raise (ArgumentOutOfRangeException (nameof ceiling, "Ceiling must not be negative."))
            if ceiling = 0 then 0 else Randomk.toIndex ceiling target (Randomk.make seed)

        /// Get the random byte for a target under a seed, below ceiling, or 0 when ceiling is 0.
        /// Thread-safe.
        static member inline randomky3 (ceiling : byte) (target : ^a) (seed : uint64) =
            byte (Gen.randomk3 (int ceiling) target seed)

        /// Get the random number single for a target under a seed, below ceiling.
        /// Thread-safe.
        static member inline randomkf3 (ceiling : single) (target : ^a) (seed : uint64) =
            Gen.randomkf target seed * ceiling

        /// Get the random number double for a target under a seed, below ceiling.
        /// Thread-safe.
        static member inline randomkd3 (ceiling : double) (target : ^a) (seed : uint64) =
            Gen.randomkd target seed * ceiling

        /// Get the random number integer for a target under a seed, GTE minValue and LT ceiling, or minValue when
        /// they're equal.
        /// Thread-safe.
        static member inline randomk4 (minValue : int) (ceiling : int) (target : ^a) (seed : uint64) =
            if minValue > ceiling then
                raise (ArgumentOutOfRangeException (nameof minValue, "Min value must not exceed ceiling."))
            let range = uint64 (int64 ceiling - int64 minValue)
            if range = 0UL then minValue
            else
                let bits = Randomk.toUInt64 target (Randomk.make seed)
                int (int64 minValue + int64 (bits % range))

        /// Get the element of a sequence for a target under a seed if there are any elements or None.
        /// Each call enumerates seq, so a sequence whose order varies between calls gives varying elements.
        /// Thread-safe.
        static member inline randomkChoiceOpt (target : ^a) (seq : 'b seq) (seed : uint64) =
            let array = Seq.toArray seq
            if array.Length = 0 then None
            else Some array.[Randomk.toIndex array.Length target (Randomk.make seed)]

        /// Get the element of a sequence for a target under a seed or a default if sequence is empty.
        /// Thread-safe.
        static member inline randomkChoiceOrDefault (target : ^a) (default_ : 'b) (seq : 'b seq) (seed : uint64) =
            match Gen.randomkChoiceOpt target seq seed with
            | Some item -> item
            | None -> default_

        /// Get the element of a sequence for a target under a seed, throwing if the sequence is empty.
        /// Thread-safe.
        static member inline randomkChoice (target : ^a) (seq : 'b seq) (seed : uint64) =
            match Gen.randomkChoiceOpt target seq seed with
            | Some item -> item
            | None -> raise (ArgumentException ("The input sequence was empty.", nameof seq))

        /// Get a key of a dictionary for a target under a seed if there are any or None.
        /// A dictionary's enumeration order isn't guaranteed, so use one whose order is stable between calls.
        /// Thread-safe.
        static member inline randomkKeyOpt (target : ^a) (dict : IDictionary<'k, 'v>) (seed : uint64) =
            Gen.randomkChoiceOpt target dict.Keys seed

        /// Get a value of a dictionary for a target under a seed if there are any or None.
        /// A dictionary's enumeration order isn't guaranteed, so use one whose order is stable between calls.
        /// Thread-safe.
        static member inline randomkValueOpt (target : ^a) (dict : IDictionary<'k, 'v>) (seed : uint64) =
            Gen.randomkChoiceOpt target dict.Values seed

        /// Shuffle a sequence in the order for a target under a seed.
        /// If seq is large, this may allocate to the LOH.
        /// Thread-safe.
        static member inline randomkShuffle (target : ^a) (seq : 'b seq) (seed : uint64) =
            let randomk = Randomk.make seed
            let array = Seq.toArray seq
            for i in dec array.Length .. -1 .. 1 do
                let j = Randomk.toIndex (inc i) (target, i) randomk
                let item = array.[i]
                array.[i] <- array.[j]
                array.[j] <- item
            array :> 'b seq

        /// Generate a unique name based on a 64-bit id.
        /// Thread-safe.
        static member name =
            NamePrefix + string Gen.id64

        /// Check that a name is generated from a 64-bit id.
        /// Thread-safe.
        static member isNameGenerated (name : string) =
            let mutable p = 0UL
            name.StartsWith NamePrefix &&
            UInt64.TryParse (String.skip 1 name, &p)

        /// Generate a unique id.
        /// Thread-safe.
        static member id =
            Guid.CreateVersion7 ()

        /// Generate an id from a couple of ints.
        /// It is the user's responsibility to ensure uniqueness when using the resulting ids.
        /// Thread-safe.
        static member idFromInts m n =
            let bytes = Array.create<byte> 8 (byte 0)
            Guid (m, int16 (n >>> 16), int16 n, bytes)

        /// Generate a unique non-zero 32-bit id.
        /// Thread-safe.
        static member id32 =
            let id = Interlocked.Increment &Id32
            if id = 0u then Log.fail "Overflowed Gen.Id32."
            id

        /// Generate a unique non-zero 64-bit id.
        /// Thread-safe.
        static member id64 =
            Interlocked.Increment &Id64

        /// Derive a unique id and name if given none.
        /// Thread-safe.
        static member id64AndNameIf nameOpt =
            let (id, name) =
                match nameOpt with
                | Some name ->
                    let id =
                        if Gen.isNameGenerated name then
                            match UInt64.TryParse (String.skip 1 name) with
                            | (true, id) -> id
                            | (false, _) -> Gen.id64
                        else Gen.id64
                    (id, name)
                | None ->
                    let id = Gen.id64
                    let name = NamePrefix + string id
                    (id, name)
            (id, name)

        /// Derive a unique id and surnames if given none.
        /// Thread-safe.
        static member id64AndSurnamesIf surnamesOpt =
            match surnamesOpt with
            | Some surnames ->
                if Array.length surnames = 0 then failwith "Entity must have at least one surname."
                let id =
                    match surnames with
                    | [||] -> Gen.id64
                    | _ ->
                        let name = Array.last surnames
                        if Gen.isNameGenerated name then
                            match UInt64.TryParse (String.skip 1 name) with
                            | (true, id) -> id
                            | (false, _) -> Gen.id64
                        else Gen.id64
                (id, surnames)
            | None ->
                let id = Gen.id64
                let name = NamePrefix + string id
                (id, [|name|])

        /// Generate a unique name for use in an editor.
        /// Thread-safe.
        static member nameForEditor (dispatcherName : string) =
            let id = lock Lock (fun () -> IdForEditor <- inc IdForEditor; IdForEditor)
            let truncatedName = dispatcherName.Replace ("Dispatcher", "")
            truncatedName + NameSeparator + id.ToString "D4"

/// Generates engine-specific values on-demand.
type Gen = Gen.Gen