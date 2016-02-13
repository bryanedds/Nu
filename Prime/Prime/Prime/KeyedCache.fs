// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime

/// Presents a purely-functional interface to a cached value.
type [<ReferenceEquality>] KeyedCache<'k, 'v when 'k : equality> =
    private
        { mutable CacheKey : 'k
          mutable CacheValue : 'v }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module KeyedCache =

    let mutable private GlobalCacheHits = 0L
    let mutable private GlobalCacheMisses = 0L
    let getGlobalCacheHits () = GlobalCacheHits
    let getGlobalCacheMisses () = GlobalCacheMisses

    let getValue (keyEquality : 'k -> 'k -> bool) getFreshKeyAndValue cacheKey keyedCache : 'v =
        if not ^ keyEquality keyedCache.CacheKey cacheKey then
#if DEBUG
            GlobalCacheMisses <- GlobalCacheMisses + 1L
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

    let make (cacheKey : 'k) (cacheValue : 'v) =
        { CacheKey = cacheKey
          CacheValue = cacheValue }