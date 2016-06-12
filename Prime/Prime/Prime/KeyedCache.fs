// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open Prime

[<AutoOpen>]
module KeyedCacheModule =

    /// Presents a purely-functional interface to a cached value.
    /// Works by associating a cached value with a given cache key such that the cached value remains valid when queried
    /// for using the same cache key (as decided by a simple key comparer function), automatically rebuilding the cached
    /// value and key (as done with a simple factory function).
    type [<ReferenceEquality>] KeyedCache<'k, 'v when 'k : equality> =
        private
            { mutable CacheKey : 'k
              mutable CacheValue : 'v }

    [<RequireQualifiedAccess>]
    module KeyedCache =

#if DEBUG
        let mutable private GlobalCacheHits = 0L
        let mutable private GlobalCacheMisses = 0L
        let private GlobalCacheTrackingLock = obj ()
#endif

        /// The number of cache hits that have occured when using this type.
        /// Useful for performance trouble-shooting in Debug mode.
        let getGlobalCacheHits () =
            let mutable result = 0L
#if DEBUG
            lock GlobalCacheTrackingLock (fun () -> result <- GlobalCacheHits)
#endif
            result

        /// The number of cache misses that have occured when using this type.
        /// Useful for performance trouble-shooting in Debug mode.
        let getGlobalCacheMisses () =
            let mutable result = 0L
#if DEBUG
            lock GlobalCacheTrackingLock (fun () -> result <- GlobalCacheMisses)
#endif
            result

        /// <summary>Get the cached value.</summary>
        /// <param name="keyEquality">Determines the equality of the key used to consider if the cache is valid.</param>
        /// <param name="getFreshKeyAndValue">Generates a fresh key and corresponding value to cache.</param>
        /// <param name="cacheKey">The current key against which to validate the cache.</param>
        /// <param name="keyedCache">The keyed cache.</param>
        let getValue (keyEquality : 'k -> 'k -> bool) getFreshKeyAndValue cacheKey keyedCache : 'v =
            if not ^ keyEquality keyedCache.CacheKey cacheKey then
#if DEBUG
                lock GlobalCacheTrackingLock (fun () -> GlobalCacheMisses <- GlobalCacheMisses + 1L)
#endif
                let (freshKey, freshValue) = getFreshKeyAndValue ()
                keyedCache.CacheKey <- freshKey
                keyedCache.CacheValue <- freshValue
                freshValue
            else
#if DEBUG
                GlobalCacheHits <- GlobalCacheHits + 1L
#endif
                keyedCache.CacheValue

        /// <summary>Make a keyed cache value.</summary>
        /// <param name="cacheKey">The current key against which to validate the cache.</param>
        /// <param name="cacheValue">The value associated with the cache key.</param>
        let make (cacheKey : 'k) (cacheValue : 'v) =
            { CacheKey = cacheKey
              CacheValue = cacheValue }