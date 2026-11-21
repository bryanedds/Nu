// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open FSharp.NativeInterop
open Prime

/// Deterministic hashing - the same value hashes the same on every run, machine and .NET version, unlike hash and
/// GetHashCode, which .NET randomizes per process for strings and anything containing them. A hash is built by folding
/// values into a running FNV-1a state, starting from Hashd.Basis, then finished with Hashd.hashdFinish to spread its bits.
[<RequireQualifiedAccess>]
module Hashd =

    /// The state a deterministic hash starts from, FNV-1a's 64-bit offset basis.
    let Basis = 14695981039346656037UL

    /// Fold a byte into a deterministic hash.
    let hashdByte (value : byte) (hash : uint64) =
        (hash ^^^ uint64 value) * 1099511628211UL

    /// Fold an unsigned long into a deterministic hash, a byte at a time, least significant first.
    let hashdUInt64 (value : uint64) (hash : uint64) =
        let mutable hash = hash
        for i in 0 .. 7 do hash <- hashdByte (byte (value >>> (i * 8))) hash
        hash

    /// Fold a long into a deterministic hash. Any integer of equal value folds alike.
    let hashdInt64 (value : int64) (hash : uint64) =
        hashdUInt64 (uint64 value) hash

    /// Fold an integer into a deterministic hash, as the long of equal value.
    let hashdInt (value : int) (hash : uint64) =
        hashdInt64 (int64 value) hash

    /// Fold a double into a deterministic hash, -0.0 as 0.0 and every NaN as one NaN, so equal values fold alike.
    let hashdDouble (value : double) (hash : uint64) =
        let value = if value = 0.0 then 0.0 elif Double.IsNaN value then Double.NaN else value
        hashdUInt64 (uint64 (BitConverter.DoubleToInt64Bits value)) hash

    /// Fold a single into a deterministic hash, as the double of equal value.
    let hashdSingle (value : single) (hash : uint64) =
        hashdDouble (double value) hash

    /// Fold a boolean into a deterministic hash.
    let hashdBool (value : bool) (hash : uint64) =
        hashdByte (if value then 1uy else 0uy) hash

    /// Fold a string into a deterministic hash as its UTF-8 bytes, then an end marker, so strings run together never
    /// collide with one another. Doesn't allocate.
    let hashdString (value : string) (hash : uint64) =
        let buffer = Span<byte> (NativePtr.toVoidPtr (NativePtr.stackalloc<byte> 4), 4) // OPTIMIZATION: on the stack.
        let mutable hash = hash
        for rune in value.EnumerateRunes () do
            let count = rune.EncodeToUtf8 buffer
            for i in 0 .. dec count do hash <- hashdByte buffer.[i] hash
        hashdByte 0xFFuy hash

    /// Fold a guid into a deterministic hash, as its bytes in little-endian order. Doesn't allocate.
    let hashdGuid (value : Guid) (hash : uint64) =
        let bytes = Span<byte> (NativePtr.toVoidPtr (NativePtr.stackalloc<byte> 16), 16) // OPTIMIZATION: on the stack.
        value.TryWriteBytes bytes |> ignore<bool>
        let mutable hash = hash
        for i in 0 .. 15 do hash <- hashdByte bytes.[i] hash
        hash

    /// Fold an enum into a deterministic hash, as its underlying integer.
    let inline hashdEnum (value : 'e) (hash : uint64) =
        hashdInt64 (int64 (LanguagePrimitives.EnumToValue value)) hash

    /// Fold a value into a deterministic hash by the Hashd overload for its type, found on a witness type or on the
    /// value's own type.
    let inline hashdBy (witness : ^w) (value : ^a) (hash : uint64) : uint64 =
        ((^w or ^a) : (static member Hashd : ^w * ^a * uint64 -> uint64) (witness, value, hash))

    /// Spread a deterministic hash's bits evenly over the whole value, by SplitMix64's finalizer.
    let hashdFinalize (hash : uint64) =
        let z = (hash ^^^ (hash >>> 30)) * 0xbf58476d1ce4e5b9UL
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebUL
        z ^^^ (z >>> 31)

/// The types hashdFold folds, an overload for each, chosen at compile time. A record or union of your own folds by
/// giving its type a Hashd of the same shape, folding its fields in turn, as -
///     static member Hashd (_ : HashdFolder, value : Spawn, hash) =
///         hash |> hashdFold value.Name |> hashdFold value.Count
[<AbstractClass; Sealed>]
type HashdFolder =
    static member Hashd (_ : HashdFolder, value : int, hash) = Hashd.hashdInt value hash
    static member Hashd (_ : HashdFolder, value : int64, hash) = Hashd.hashdInt64 value hash
    static member Hashd (_ : HashdFolder, value : uint64, hash) = Hashd.hashdUInt64 value hash
    static member Hashd (_ : HashdFolder, value : int16, hash) = Hashd.hashdInt64 (int64 value) hash
    static member Hashd (_ : HashdFolder, value : uint16, hash) = Hashd.hashdInt64 (int64 value) hash
    static member Hashd (_ : HashdFolder, value : uint32, hash) = Hashd.hashdInt64 (int64 value) hash
    static member Hashd (_ : HashdFolder, value : byte, hash) = Hashd.hashdInt64 (int64 value) hash
    static member Hashd (_ : HashdFolder, value : sbyte, hash) = Hashd.hashdInt64 (int64 value) hash
    static member Hashd (_ : HashdFolder, value : single, hash) = Hashd.hashdSingle value hash
    static member Hashd (_ : HashdFolder, value : double, hash) = Hashd.hashdDouble value hash
    static member Hashd (_ : HashdFolder, value : bool, hash) = Hashd.hashdBool value hash
    static member Hashd (_ : HashdFolder, value : string, hash) = Hashd.hashdString value hash
    static member Hashd (_ : HashdFolder, value : Guid, hash) = Hashd.hashdGuid value hash

    static member inline Hashd (w : HashdFolder, (a, b) : ^a * ^b, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b

    static member inline Hashd (w : HashdFolder, (a, b, c) : ^a * ^b * ^c, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c

    static member inline Hashd (w : HashdFolder, (a, b, c, d) : ^a * ^b * ^c * ^d, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d

    static member inline Hashd (w : HashdFolder, (a, b, c, d, e) : ^a * ^b * ^c * ^d * ^e, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e

    static member inline Hashd (w : HashdFolder, (a, b, c, d, e, f) : ^a * ^b * ^c * ^d * ^e * ^f, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f

    static member inline Hashd (w : HashdFolder, (a, b, c, d, e, f, g) : ^a * ^b * ^c * ^d * ^e * ^f * ^g, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g

    static member inline Hashd (w : HashdFolder, (a, b, c, d, e, f, g, h) : ^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g |> Hashd.hashdBy w h

    static member inline Hashd (w : HashdFolder, (a, b, c, d, e, f, g, h, i) : ^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h * ^i, hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g |> Hashd.hashdBy w h |> Hashd.hashdBy w i

    static member inline Hashd (w : HashdFolder, struct (a, b) : struct (^a * ^b), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b

    static member inline Hashd (w : HashdFolder, struct (a, b, c) : struct (^a * ^b * ^c), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d) : struct (^a * ^b * ^c * ^d), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d, e) : struct (^a * ^b * ^c * ^d * ^e), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d, e, f) : struct (^a * ^b * ^c * ^d * ^e * ^f), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d, e, f, g) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d, e, f, g, h) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g |> Hashd.hashdBy w h

    static member inline Hashd (w : HashdFolder, struct (a, b, c, d, e, f, g, h, i) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h * ^i), hash) =
        hash |> Hashd.hashdBy w a |> Hashd.hashdBy w b |> Hashd.hashdBy w c |> Hashd.hashdBy w d |> Hashd.hashdBy w e |> Hashd.hashdBy w f |> Hashd.hashdBy w g |> Hashd.hashdBy w h |> Hashd.hashdBy w i

    static member inline Hashd (w : HashdFolder, value : ^a option, hash) =
        match value with
        | Some a -> hash |> Hashd.hashdBool true |> Hashd.hashdBy w a
        | None -> Hashd.hashdBool false hash

    static member inline Hashd (w : HashdFolder, value : ^a voption, hash) =
        match value with
        | ValueSome a -> hash |> Hashd.hashdBool true |> Hashd.hashdBy w a
        | ValueNone -> Hashd.hashdBool false hash

    static member inline Hashd (w : HashdFolder, values : ^a array, hash) =
        let mutable hash = Hashd.hashdInt values.Length hash // length first, so arrays run together never collide
        for value in values do hash <- Hashd.hashdBy w value hash
        hash

    static member inline Hashd (w : HashdFolder, values : ^a list, hash) =
        let mutable hash = Hashd.hashdInt (List.length values) hash
        for value in values do hash <- Hashd.hashdBy w value hash
        hash

/// Deterministic hashing of any value HashdFolder folds - an integer, float, boolean, string, guid, tuple, option,
/// array or list of them, or a type with its own Hashd - chosen by the value's type at compile time. Use
/// Hashd.hashdEnum to fold an enum.
[<AutoOpen>]
module HashdOperators =

    /// Fold a value into a deterministic hash.
    let inline hashdFold (value : ^a) (hash : uint64) : uint64 =
        Hashd.hashdBy Unchecked.defaultof<HashdFolder> value hash

    /// Hash a value deterministically under a seed, so the same value hashes differently under different seeds.
    let inline hashdSeeded (value : ^a) (seed : uint64) : uint64 =
        Hashd.hashdFinalize (hashdFold value (Hashd.hashdUInt64 seed Hashd.Basis))

    /// Hash a value deterministically.
    let inline hashd (value : ^a) : uint64 =
        hashdSeeded value 0UL