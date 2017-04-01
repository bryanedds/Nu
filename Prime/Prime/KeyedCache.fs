// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System.Collections.Generic;
open Prime

[<AutoOpen>]
module KeyedCacheModule =

    /// Presents a purely-functional interface to a cached value.
    /// Works by associating a cached value with a given cache key such that the cached value remains valid when queried
    /// for using the same cache key (as decided by a simple key comparer function), automatically rebuilding the cached
    /// value and key (as done with a simple factory function).
    type [<ReferenceEquality>] KeyedCache<'k, 'v> =
        private
            { mutable CacheKey : 'k
              mutable CacheValue : 'v }

    [<RequireQualifiedAccess>]
    module KeyedCache =

        /// <summary>Get the cached value.</summary>
        /// <param name="keyEquality">Determines the equality of the key used to consider if the cache is valid.</param>
        /// <param name="getFreshKeyAndValue">Generates a fresh key and corresponding value to cache.</param>
        /// <param name="cacheKey">The current key against which to validate the cache.</param>
        /// <param name="keyedCache">The keyed cache.</param>
        let getValue
            (keyEquality : 'k -> 'k -> bool)
            (getFreshKeyAndValue : unit -> KeyValuePair<'k, 'v>)
            cacheKey
            keyedCache =
            if not ^ keyEquality keyedCache.CacheKey cacheKey then
                let freshKvp = getFreshKeyAndValue ()
                keyedCache.CacheKey <- freshKvp.Key
                keyedCache.CacheValue <- freshKvp.Value
                freshKvp.Value
            else keyedCache.CacheValue

        /// <summary>Get the cached value without allocating if possible.</summary>
        /// <param name="keyEquality">Determines the equality of the key used to consider if the cache is valid.</param>
        /// <param name="getFreshKeyAndValue">Generates a fresh key and corresponding value to cache.</param>
        /// <param name="cacheKey">The current key against which to validate the cache.</param>
        /// <param name="keyedCache">The keyed cache.</param>
        let getValueFast
            (keyEquality : OptimizedClosures.FSharpFunc<'k, 'k, bool>)
            (getFreshKeyAndValue : FSharpFunc<unit, KeyValuePair<'k, 'v>>)
            cacheKey
            keyedCache =
            if not ^ keyEquality.Invoke (keyedCache.CacheKey, cacheKey) then
                let freshKvp = getFreshKeyAndValue.Invoke ()
                keyedCache.CacheKey <- freshKvp.Key
                keyedCache.CacheValue <- freshKvp.Value
                freshKvp.Value
            else keyedCache.CacheValue

        /// <summary>Make a keyed cache value.</summary>
        /// <param name="cacheKey">The current key against which to validate the cache.</param>
        /// <param name="cacheValue">The value associated with the cache key.</param>
        let make (cacheKey : 'k) (cacheValue : 'v) =
            { CacheKey = cacheKey
              CacheValue = cacheValue }

/// Presents a purely-functional interface to a cached value.
/// Works by associating a cached value with a given cache key such that the cached value remains valid when queried
/// for using the same cache key (as decided by a simple key comparer function), automatically rebuilding the cached
/// value and key (as done with a simple factory function).
type KeyedCache<'k, 'v> = KeyedCacheModule.KeyedCache<'k, 'v>