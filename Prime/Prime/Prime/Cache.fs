namespace Prime

type [<ReferenceEquality>] 'v RefCache =
    private
        { Volatile : 'v ref
          Stable : 'v }

    static member make (value : 'v) =
        { Volatile = ref value
          Stable = value }

    static member getValue volatility refCache : 'v =
        if volatility
        then refCache.Volatile.Value
        else refCache.Stable

    static member setValue volatility (value : 'v) refCache =
        let refCache = { refCache with Stable = value }
        if volatility then refCache.Volatile := value
        refCache

type [<ReferenceEquality>] KeyedCache<'k, 'v when 'k : equality> =
    private
        { mutable CacheKey : 'k
          mutable CacheValue : 'v }

    static member make (cacheKey : 'k) (cacheValue : 'v) =
        { CacheKey = cacheKey
          CacheValue = cacheValue }

    static member getValue keyEquality getFreshKeyAndValue (cacheKey : 'k) keyedCache : 'v =
        if not <| keyEquality keyedCache.CacheKey cacheKey then
            let (freshKey, freshValue) = getFreshKeyAndValue ()
            keyedCache.CacheKey <- freshKey
            keyedCache.CacheValue <- freshValue
            freshValue
        else keyedCache.CacheValue