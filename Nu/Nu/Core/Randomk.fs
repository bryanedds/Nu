// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open Prime

/// Keyed randomness - a seed that gives a random output for each value it's passed, the target saying what the output
/// is for. The same seed and target give the same output on every run, machine and .NET version, and no output depends
/// on any other, so randomness keyed this way is pure without any state to thread, and adding or reordering what's
/// random changes nothing else. A target is any value HashdFolder folds - an integer, float, boolean, string, guid,
/// tuple, option, array or list of them, or a type with its own HashdFold - as ("rise", creatureName).
[<Struct>]
type Randomk =
    { Seed : uint64 }

    /// Keyed randomness from a seed, as a game's.
    static member make (seed : uint64) =
        { Seed = seed }

    /// The output for a target as 64 evenly spread bits.
    static member inline toUInt64 (target : ^a) (randomk : Randomk) =
        hashd2 target randomk.Seed

    /// The output for a target between zero (included) and one (excluded).
    static member inline toSingle (target : ^a) (randomk : Randomk) =
        single (hashd2 target randomk.Seed >>> 40) / 16777216.0f

    /// The output for a target between zero (included) and one (excluded), at double precision.
    static member inline toDouble (target : ^a) (randomk : Randomk) =
        double (hashd2 target randomk.Seed >>> 11) / 9007199254740992.0

    /// The output for a target as a whole number from zero up to, but not including, a count.
    static member inline toIndex (count : int) (target : ^a) (randomk : Randomk) =
        if count <= 0 then failwith "A random index needs a count of at least one."
        int (hashd2 target randomk.Seed % uint64 count)

    /// Whether something with the given chance happens for a target.
    static member inline roll (chance : single) (target : ^a) (randomk : Randomk) =
        Randomk.toSingle target randomk < chance
