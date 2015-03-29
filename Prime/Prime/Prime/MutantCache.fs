namespace Prime

[<RequireQualifiedAccess>]
module internal MutantCacheMetrics =
    let mutable GlobalMutantRebuilds = 0L

type [<ReferenceEquality>] MutantCache<'k, 'm when 'k : equality> =
    private
        { ConstantKey : 'k
          RefKey : 'k ref
          RefMutant : 'm ref }

    static member getMutant keyEquality generateKey rebuildMutant (mutantCache : MutantCache<'k, 'm>) =
        if not <| keyEquality !mutantCache.RefKey mutantCache.ConstantKey then
#if DEBUG
            MutantCacheMetrics.GlobalMutantRebuilds <- MutantCacheMetrics.GlobalMutantRebuilds + 1L
#endif
            let newKey = generateKey ()
            let newMutant = rebuildMutant ()
            mutantCache.RefKey := newKey
            mutantCache.RefMutant := newMutant
            (newMutant, { mutantCache with ConstantKey = newKey })
        else (!mutantCache.RefMutant, mutantCache)

    static member mutateMutant keyEquality rebuildMutant generateKey mutateMutant mutantCache =
        let (mutant, mutantCache) = MutantCache<'k, 'm>.getMutant keyEquality generateKey rebuildMutant mutantCache
        let newKey = generateKey ()
        mutantCache.RefKey := newKey
        mutantCache.RefMutant := mutateMutant mutant
        { mutantCache with ConstantKey = newKey }

    static member tryGetGlobalMutantRebuilds () : int64 option =
#if DEBUG
        Some MutantCacheMetrics.GlobalMutantRebuilds
#else
        None
#endif