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
/// values into a running FNV-1a state, starting from Hashd.Basis, then finalized with hashdFinalize to spread its
/// bits.
[<RequireQualifiedAccess>]
module Hashd =

    /// The state a deterministic hash starts from, FNV-1a's 64-bit offset basis.
    let Basis = 14695981039346656037UL

    /// Fold a byte into a deterministic hash.
    let foldByte (value : byte) (hash : uint64) =
        (hash ^^^ uint64 value) * 1099511628211UL

    /// Fold an unsigned long into a deterministic hash, a byte at a time, least significant first.
    let foldUInt64 (value : uint64) (hash : uint64) =
        let mutable hash = hash
        for i in 0 .. 7 do hash <- foldByte (byte (value >>> (i * 8))) hash
        hash

    /// Fold a long into a deterministic hash. Any integer of equal value folds alike.
    let foldInt64 (value : int64) (hash : uint64) =
        foldUInt64 (uint64 value) hash

    /// Fold an integer into a deterministic hash, as the long of equal value.
    let foldInt (value : int) (hash : uint64) =
        foldInt64 (int64 value) hash

    /// Fold a double into a deterministic hash, -0.0 as 0.0 and every NaN as one NaN, so equal values fold alike.
    let foldDouble (value : double) (hash : uint64) =
        let value = if value = 0.0 then 0.0 elif Double.IsNaN value then Double.NaN else value
        foldUInt64 (uint64 (BitConverter.DoubleToInt64Bits value)) hash

    /// Fold a single into a deterministic hash, as the double of equal value.
    let foldSingle (value : single) (hash : uint64) =
        foldDouble (double value) hash

    /// Fold a boolean into a deterministic hash.
    let foldBool (value : bool) (hash : uint64) =
        foldByte (if value then 1uy else 0uy) hash

    /// Fold a string into a deterministic hash as its UTF-8 bytes, then an end marker, so strings run together never
    /// collide with one another. Doesn't allocate.
    let foldString (value : string) (hash : uint64) =
        let buffer = Span<byte> (NativePtr.toVoidPtr (NativePtr.stackalloc<byte> 4), 4) // OPTIMIZATION: on the stack.
        let mutable hash = hash
        for rune in value.EnumerateRunes () do
            let count = rune.EncodeToUtf8 buffer
            for i in 0 .. dec count do hash <- foldByte buffer.[i] hash
        foldByte 0xFFuy hash

    /// Fold a guid into a deterministic hash, as its bytes in little-endian order. Doesn't allocate.
    let foldGuid (value : Guid) (hash : uint64) =
        let bytes = Span<byte> (NativePtr.toVoidPtr (NativePtr.stackalloc<byte> 16), 16) // OPTIMIZATION: on the stack.
        value.TryWriteBytes bytes |> ignore<bool>
        let mutable hash = hash
        for i in 0 .. 15 do hash <- foldByte bytes.[i] hash
        hash

    /// Fold an enum into a deterministic hash, as its underlying integer.
    let inline foldEnum (value : 'e) (hash : uint64) =
        foldInt64 (int64 (LanguagePrimitives.EnumToValue value)) hash

    /// Fold a value into a deterministic hash by the HashdFold overload for its type, found on a witness type or on the
    /// value's own type.
    let inline foldBy (witness : ^w) (value : ^a) (hash : uint64) : uint64 =
        ((^w or ^a) : (static member HashdFold : ^w * ^a * uint64 -> uint64) (witness, value, hash))

/// The types hashdFold folds, an overload for each, chosen at compile time. A record or union of your own folds by
/// giving its type a HashdFold of the same shape, folding its fields in turn, as -
///     static member HashdFold (_ : HashdFolder, value : MyType, hash) =
///         hash |> hashdFold value.Name |> hashdFold value.Count
[<AbstractClass; Sealed>]
type HashdFolder =

    static member HashdFold (_ : HashdFolder, value : int, hash) = Hashd.foldInt value hash
    static member HashdFold (_ : HashdFolder, value : int64, hash) = Hashd.foldInt64 value hash
    static member HashdFold (_ : HashdFolder, value : uint64, hash) = Hashd.foldUInt64 value hash
    static member HashdFold (_ : HashdFolder, value : int16, hash) = Hashd.foldInt64 (int64 value) hash
    static member HashdFold (_ : HashdFolder, value : uint16, hash) = Hashd.foldInt64 (int64 value) hash
    static member HashdFold (_ : HashdFolder, value : uint32, hash) = Hashd.foldInt64 (int64 value) hash
    static member HashdFold (_ : HashdFolder, value : byte, hash) = Hashd.foldInt64 (int64 value) hash
    static member HashdFold (_ : HashdFolder, value : sbyte, hash) = Hashd.foldInt64 (int64 value) hash
    static member HashdFold (_ : HashdFolder, value : single, hash) = Hashd.foldSingle value hash
    static member HashdFold (_ : HashdFolder, value : double, hash) = Hashd.foldDouble value hash
    static member HashdFold (_ : HashdFolder, value : bool, hash) = Hashd.foldBool value hash
    static member HashdFold (_ : HashdFolder, value : string, hash) = Hashd.foldString value hash
    static member HashdFold (_ : HashdFolder, value : Guid, hash) = Hashd.foldGuid value hash

    static member inline HashdFold (w : HashdFolder, (a, b) : ^a * ^b, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b

    static member inline HashdFold (w : HashdFolder, (a, b, c) : ^a * ^b * ^c, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c

    static member inline HashdFold (w : HashdFolder, (a, b, c, d) : ^a * ^b * ^c * ^d, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d

    static member inline HashdFold (w : HashdFolder, (a, b, c, d, e) : ^a * ^b * ^c * ^d * ^e, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e

    static member inline HashdFold (w : HashdFolder, (a, b, c, d, e, f) : ^a * ^b * ^c * ^d * ^e * ^f, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f

    static member inline HashdFold (w : HashdFolder, (a, b, c, d, e, f, g) : ^a * ^b * ^c * ^d * ^e * ^f * ^g, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g

    static member inline HashdFold (w : HashdFolder, (a, b, c, d, e, f, g, h) : ^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g |> Hashd.foldBy w h

    static member inline HashdFold (w : HashdFolder, (a, b, c, d, e, f, g, h, i) : ^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h * ^i, hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g |> Hashd.foldBy w h |> Hashd.foldBy w i

    static member inline HashdFold (w : HashdFolder, struct (a, b) : struct (^a * ^b), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b

    static member inline HashdFold (w : HashdFolder, struct (a, b, c) : struct (^a * ^b * ^c), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d) : struct (^a * ^b * ^c * ^d), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d, e) : struct (^a * ^b * ^c * ^d * ^e), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d, e, f) : struct (^a * ^b * ^c * ^d * ^e * ^f), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d, e, f, g) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d, e, f, g, h) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g |> Hashd.foldBy w h

    static member inline HashdFold (w : HashdFolder, struct (a, b, c, d, e, f, g, h, i) : struct (^a * ^b * ^c * ^d * ^e * ^f * ^g * ^h * ^i), hash) =
        hash |> Hashd.foldBy w a |> Hashd.foldBy w b |> Hashd.foldBy w c |> Hashd.foldBy w d |> Hashd.foldBy w e |> Hashd.foldBy w f |> Hashd.foldBy w g |> Hashd.foldBy w h |> Hashd.foldBy w i

    static member inline HashdFold (w : HashdFolder, value : ^a option, hash) =
        match value with
        | Some a -> hash |> Hashd.foldBool true |> Hashd.foldBy w a
        | None -> Hashd.foldBool false hash

    static member inline HashdFold (w : HashdFolder, value : ^a voption, hash) =
        match value with
        | ValueSome a -> hash |> Hashd.foldBool true |> Hashd.foldBy w a
        | ValueNone -> Hashd.foldBool false hash

    static member inline HashdFold (w : HashdFolder, values : ^a array, hash) =
        let mutable hash = Hashd.foldInt values.Length hash // length first, so arrays run together never collide
        for value in values do hash <- Hashd.foldBy w value hash
        hash

    static member inline HashdFold (w : HashdFolder, values : ^a list, hash) =
        let mutable hash = Hashd.foldInt (List.length values) hash
        for value in values do hash <- Hashd.foldBy w value hash
        hash

/// Deterministic hashing of any value HashdFolder folds - an integer, float, boolean, string, guid, tuple, option,
/// array or list of them, or a type with its own HashdFold - chosen by the value's type at compile time. Use
/// Hashd.foldEnum to fold an enum.
[<AutoOpen>]
module HashdOperators =

    /// Spread a deterministic hash's bits evenly over the whole value, by SplitMix64's finalizer.
    let hashdFinalize (hash : uint64) =
        let z = (hash ^^^ (hash >>> 30)) * 0xbf58476d1ce4e5b9UL
        let z = (z ^^^ (z >>> 27)) * 0x94d049bb133111ebUL
        z ^^^ (z >>> 31)

    /// Fold a value into a deterministic hash.
    let inline hashdFold (value : ^a) (hash : uint64) : uint64 =
        Hashd.foldBy Unchecked.defaultof<HashdFolder> value hash

    /// Hash a value deterministically under a seed, so the same value hashes differently under different seeds.
    let inline hashd2 (value : ^a) (seed : uint64) : uint64 =
        hashdFinalize (hashdFold value (Hashd.foldUInt64 seed Hashd.Basis))

    /// Hash a value deterministically.
    let inline hashd (value : ^a) : uint64 =
        hashd2 value 0UL