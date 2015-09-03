namespace Prime

/// Presents a purely-functional interface to a mutable object / record / whatever.
/// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
/// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
/// returned mutant!
/// TODO: document this type's functions!
type [<ReferenceEquality>] MutantCache<'k, 'm when 'k : equality> =
    private
        { KeyEquality : 'k -> 'k -> bool
          CloneMutant : 'm -> 'm
          RefOptConstantKey : 'k option ref
          RefKey : 'k ref
          RefMutant : 'm ref }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MutantCache =

    let mutable private GlobalMutantRebuilds = 0L
    let getGlobalMutantRebuilds () = GlobalMutantRebuilds

    let private rebuildCache generateKey rebuildMutant mutantCache =
#if DEBUG
        GlobalMutantRebuilds <- GlobalMutantRebuilds + 1L
#endif
        let newKey = generateKey ()
        let newMutant = rebuildMutant ()
        mutantCache.RefKey := newKey
        mutantCache.RefMutant := newMutant
        { mutantCache with RefOptConstantKey = ref ^ Some newKey }

    let private getMutantUncloned generateKey rebuildMutant (mutantCache : MutantCache<'k, 'm>) =
        match !mutantCache.RefOptConstantKey with
        | Some constantKey ->
            let mutantCache =
                if not ^ mutantCache.KeyEquality !mutantCache.RefKey constantKey
                then rebuildCache generateKey rebuildMutant mutantCache
                else mutantCache
            (!mutantCache.RefMutant, mutantCache)
        | None ->
            mutantCache.RefOptConstantKey := None // break cycle
            let mutantCache = rebuildCache generateKey rebuildMutant mutantCache
            (!mutantCache.RefMutant, mutantCache)

    let getMutant generateKey rebuildMutant (mutantCache : MutantCache<'k, 'm>) =
        let (mutantUncloned, mutantCache) = getMutantUncloned generateKey rebuildMutant mutantCache
        mutantCache.CloneMutant mutantUncloned

    let mutateMutant generateKey rebuildMutant mutateMutant mutantCache =
        let (mutant : 'm, mutantCache) = getMutantUncloned generateKey rebuildMutant mutantCache
        let newKey = generateKey () : 'k
        mutantCache.RefKey := newKey
        mutantCache.RefMutant := mutateMutant mutant
        mutantCache.RefOptConstantKey := None // break cycle
        { mutantCache with RefOptConstantKey = ref ^ Some newKey }

    let make keyEquality cloneMutant (key : 'k) (mutant : 'm) =
        { KeyEquality = keyEquality
          CloneMutant = cloneMutant
          RefOptConstantKey = ref ^ Some key
          RefKey = ref key
          RefMutant = ref mutant }