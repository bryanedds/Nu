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

    let private rebuildCache rebuildMutant mutantCache =
#if DEBUG
        GlobalMutantRebuilds <- GlobalMutantRebuilds + 1L
#endif
        let validMutant = rebuildMutant ()
        mutantCache.OptValidMutant <- None
        { mutantCache with OptValidMutant = Some validMutant }

    let private getMutantUncloned rebuildMutant (mutantCache : 'm MutantCache) =
        match mutantCache.OptValidMutant with
        | Some mutant -> (mutant, mutantCache)
        | None -> let mutantCache = rebuildCache rebuildMutant mutantCache in (mutantCache.OptValidMutant.Value, mutantCache)

    let getMutant rebuildMutant (mutantCache : 'm MutantCache) =
        let (mutantUncloned, mutantCache) = getMutantUncloned rebuildMutant mutantCache
        mutantCache.CloneMutant mutantUncloned

    let mutateMutant rebuildMutant mutateMutant mutantCache =
        let (mutant : 'm, mutantCache) = getMutantUncloned rebuildMutant mutantCache
        mutantCache.OptValidMutant <- None
        { mutantCache with OptValidMutant = Some ^ mutateMutant mutant }

    let make cloneMutant (mutant : 'm) =
        { CloneMutant = cloneMutant
          OptValidMutant = Some mutant }