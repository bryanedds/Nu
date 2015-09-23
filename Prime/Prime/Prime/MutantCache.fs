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

    let private rebuildCache (rebuildMutant : unit -> 'm) (mutantCache : 'm MutantCache)=
#if DEBUG
        GlobalMutantRebuilds <- GlobalMutantRebuilds + 1L
#endif
        let validMutant = rebuildMutant ()
        mutantCache.OptValidMutant <- None
        let mutantCache = { mutantCache with OptValidMutant = Some validMutant }
        (validMutant, mutantCache)

    let private getMutantUncloned rebuildMutant (mutantCache : 'm MutantCache) =
        match mutantCache.OptValidMutant with
        | Some mutant -> (mutant, mutantCache)
        | None -> rebuildCache rebuildMutant mutantCache

    let getMutant rebuildMutant (mutantCache : 'm MutantCache) =
        let (mutantUncloned, mutantCache) = getMutantUncloned rebuildMutant mutantCache
        let mutantCloned = mutantCache.CloneMutant mutantUncloned
        (mutantCloned, mutantCache)

    let mutateMutant rebuildMutant mutateMutant (mutantCache : 'm MutantCache) : 'm MutantCache =
        let (mutant, mutantCache) = getMutantUncloned rebuildMutant mutantCache
        mutantCache.OptValidMutant <- None
        let mutant = mutateMutant mutant
        { mutantCache with OptValidMutant = Some mutant }

    let make cloneMutant (mutant : 'm) =
        { CloneMutant = cloneMutant
          OptValidMutant = Some mutant }