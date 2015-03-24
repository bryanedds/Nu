namespace Prime

type [<ReferenceEquality>] CacheValue =
    private
        { Volatile : obj ref
          Stable : obj }

    static member make value =
        { Volatile = ref value
          Stable = value }

    static member getValue volatility cacheValue =
        if volatility
        then cacheValue.Volatile.Value
        else cacheValue.Stable

    static member setValue volatility value cacheValue =
        let cacheValue = { cacheValue with Stable = value }
        if volatility then cacheValue.Volatile := value
        cacheValue

type [<ReferenceEquality>] Cache =
    private
        { CacheMap : Map<string, CacheValue>
          Volatility : bool }

    static member getOptValue name cache =
        match Map.tryFind name cache.CacheMap with
        | Some cacheValue -> Some <| CacheValue.getValue cache.Volatility cacheValue
        | None -> None

    static member getValue name cache =
        Option.get <| Cache.getOptValue name cache

    static member setValue name value cache =
        let cacheValue =
            match Map.tryFind name cache.CacheMap with
            | Some cacheValue -> cacheValue
            | None -> { Volatile = ref value; Stable = value }
        let cacheValue = CacheValue.setValue cache.Volatility value cacheValue 
        Map.add name cacheValue cache.CacheMap