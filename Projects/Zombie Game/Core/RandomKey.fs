// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Text
open Prime

/// What a random value is for - a seed and the names and indices that set it apart from every other value. The same
/// key gives the same value on every run, machine and .NET version, and no value depends on any other, so randomness
/// keyed this way is pure without any state to thread, and adding or reordering what's random changes nothing else.
[<Struct>]
type RandomKey =
    private { Hash : uint64 }

    /// Fold a byte into a hash.
    static let foldByte (hash : uint64) (b : byte) =
        (hash ^^^ uint64 b) * 1099511628211UL

    /// Fold a number into a hash, a byte at a time.
    static let foldUInt64 (hash : uint64) (value : uint64) =
        let mutable hash = hash
        for i in 0 .. 7 do hash <- foldByte hash (byte (value >>> (i * 8)))
        hash

    /// Spread a folded hash's bits evenly over the whole value, by SplitMix64's finalizer.
    static let finish (hash : uint64) =
        let z = (hash ^^^ (hash >>> 30)) * 0xbf58476d1ce4e5b9UL
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebUL
        z ^^^ (z >>> 31)

    /// A key from a seed, as a game's.
    static member make (seed : uint64) =
        { Hash = foldUInt64 14695981039346656037UL seed } // FNV-1a, folding in each part of the key a byte at a time

    /// The key set apart by an index, as which creature or which of several.
    static member withIndex (index : int) (key : RandomKey) =
        { Hash = foldUInt64 key.Hash (uint64 (int64 index)) }

    /// The key set apart by a name, as what the value is for.
    static member withName (name : string) (key : RandomKey) =
        let mutable hash = key.Hash
        for b in Encoding.UTF8.GetBytes name do hash <- foldByte hash b
        { Hash = foldByte hash 0xFFuy } // ends the name, so names run together never collide with one another

    /// The key's value between zero (included) and one (excluded).
    static member toSingle (key : RandomKey) =
        single (finish key.Hash >>> 40) / 16777216.0f

    /// The key's value as a whole number from zero up to, but not including, a count.
    static member toIndex (count : int) (key : RandomKey) =
        if count <= 0 then failwith "A random index needs a count of at least one."
        int (finish key.Hash % uint64 count)

    /// Whether something with the given chance happens.
    static member roll (chance : single) (key : RandomKey) =
        RandomKey.toSingle key < chance
