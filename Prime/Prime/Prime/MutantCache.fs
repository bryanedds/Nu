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
          ConstantKey : 'k
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
        { mutantCache with ConstantKey = newKey }

    let getMutant generateKey rebuildMutant (mutantCache : MutantCache<'k, 'm>) =
        let mutantCache =
            if not ^ mutantCache.KeyEquality !mutantCache.RefKey mutantCache.ConstantKey
            then rebuildCache generateKey rebuildMutant mutantCache
            else mutantCache
        (mutantCache.CloneMutant !mutantCache.RefMutant, mutantCache)

    let mutateMutant generateKey rebuildMutant mutateMutant mutantCache =
        let (mutant : 'm, mutantCache) = getMutant generateKey rebuildMutant mutantCache
        let newKey = generateKey () : 'k
        mutantCache.RefKey := newKey
        mutantCache.RefMutant := mutateMutant mutant
        { mutantCache with ConstantKey = newKey }

    let make keyEquality cloneMutant (key : 'k) (mutant : 'm) =
        { KeyEquality = keyEquality
          CloneMutant = cloneMutant
          ConstantKey = key
          RefKey = ref key
          RefMutant = ref mutant }