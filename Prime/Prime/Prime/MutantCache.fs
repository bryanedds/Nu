namespace Prime

/// Presents a purely-functional interface to a mutable object / record / whatever.
/// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
/// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
/// returned mutant!
/// TODO: document this type's functions!
type [<ReferenceEquality>] 'm MutantCache =
    private
        { CloneMutant : 'm -> 'm
          mutable OptValidMutant : 'm option }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MutantCache =

    let mutable private GlobalMutantRebuilds = 0L
    let getGlobalMutantRebuilds () = GlobalMutantRebuilds

    let private rebuildCache (rebuildMutant : unit -> 'c * 'm) (mutantCache : 'm MutantCache)=
#if DEBUG
        GlobalMutantRebuilds <- GlobalMutantRebuilds + 1L
#endif
        let (context, validMutant) = rebuildMutant ()
        mutantCache.OptValidMutant <- None
        let mutantCache = { mutantCache with OptValidMutant = Some validMutant }
        (context, validMutant, mutantCache)

    let private getMutantUncloned rebuildMutant (context : 'c) (mutantCache : 'm MutantCache) =
        match mutantCache.OptValidMutant with
        | Some mutant -> (context, mutant, mutantCache)
        | None -> rebuildCache rebuildMutant mutantCache

    let getMutant rebuildMutant (context : 'c) (mutantCache : 'm MutantCache) =
        let (context, mutantUncloned, mutantCache) = getMutantUncloned rebuildMutant context mutantCache
        let mutantCloned = mutantCache.CloneMutant mutantUncloned
        (context, mutantCloned, mutantCache)

    let mutateMutant rebuildMutant mutateMutant (context : 'c) (mutantCache : 'm MutantCache) : 'c * 'm MutantCache =
        let (context, mutant, mutantCache) = getMutantUncloned rebuildMutant context mutantCache
        mutantCache.OptValidMutant <- None
        let (context, mutant) = mutateMutant context mutant
        let mutantCache = { mutantCache with OptValidMutant = Some mutant }
        (context, mutantCache)

    let make cloneMutant (mutant : 'm) =
        { CloneMutant = cloneMutant
          OptValidMutant = Some mutant }