namespace Prime

[<RequireQualifiedAccess>]
module internal MutantCacheMetrics =
    let mutable private GlobalMutantRebuilds = 0L
    let getGlobalMutantRebuilds () = GlobalMutantRebuilds

/// Presents a purely-functional interface to a mutable object / record / whatever.
/// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
/// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
/// returned mutant!
/// TODO: document this type's functions!
type [<ReferenceEquality>] MutantCache<'k, 'm when 'k : equality> =
    private
        { KeyEquality : 'k -> 'k -> bool
          CloneMutant : 'm -> 'm
          ConstantKey : 'k
          RefKey : 'k ref
          RefMutant : 'm ref }

    static member make keyEquality cloneMutant key mutant =
        { KeyEquality = keyEquality
          CloneMutant = cloneMutant
          ConstantKey = key
          RefKey = ref key
          RefMutant = ref mutant }

    static member getMutant generateKey rebuildMutant (mutantCache : MutantCache<'k, 'm>) =
        let mutantCache =
            if not <| mutantCache.KeyEquality !mutantCache.RefKey mutantCache.ConstantKey then
#if DEBUG
                MutantCacheMetrics.GlobalMutantRebuilds <- MutantCacheMetrics.GlobalMutantRebuilds + 1L
#endif
                let newKey = generateKey ()
                let newMutant = rebuildMutant ()
                mutantCache.RefKey := newKey
                mutantCache.RefMutant := newMutant
                { mutantCache with ConstantKey = newKey }
            else mutantCache
        (mutantCache.CloneMutant !mutantCache.RefMutant, mutantCache)

    static member mutateMutant rebuildMutant generateKey mutateMutant mutantCache =
        let (mutant, mutantCache) = MutantCache<'k, 'm>.getMutant generateKey rebuildMutant mutantCache
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