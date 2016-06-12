// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime

[<AutoOpen>]
module MutantCacheModule =

    /// Presents a purely-functional interface to a mutable object / record / whatever.
    /// If it is not satisfactorily efficient to run a clone operation on the mutant for every get,
    /// just pass in the id function for make's cloneMutant arg, but make sure to NEVER mutate the
    /// returned mutant!
    type [<ReferenceEquality>] 'm MutantCache =
        private
            { CloneMutant : 'm -> 'm
              mutable OptValidMutant : 'm option }

    [<RequireQualifiedAccess>]
    module MutantCache =

#if DEBUG
        let mutable private GlobalMutantRebuilds = 0L
        let private GlobalMutantRebuildsLock = obj ()
#endif

        let private rebuildCache (rebuildMutant : unit -> 'm) (mutantCache : 'm MutantCache)=
#if DEBUG
            lock GlobalMutantRebuildsLock (fun () -> GlobalMutantRebuilds <- GlobalMutantRebuilds + 1L)
#endif
            let validMutant = rebuildMutant ()
            mutantCache.OptValidMutant <- None
            let mutantCache = { mutantCache with OptValidMutant = Some validMutant }
            (validMutant, mutantCache)

        let private getMutantUncloned rebuildMutant (mutantCache : 'm MutantCache) =
            match mutantCache.OptValidMutant with
            | Some mutant -> (mutant, mutantCache)
            | None -> rebuildCache rebuildMutant mutantCache

        /// The number of mutant rebuilds that have occured when using this type.
        /// Useful for performance trouble-shooting in Debug mode.
        let getGlobalMutantRebuilds () =
            let mutable result = 0L
#if DEBUG
            lock GlobalMutantRebuildsLock (fun () -> result <- GlobalMutantRebuilds)
#endif
            result

        /// <summary>Get the underlying mutant (mutable object / record / whatever).</summary>
        /// <param name="rebuildMutant">A function that rebuilds the mutant from scratch in case the current underlying mutant is out of date.</param>
        /// <param name="mutantCache">The mutant cache.</param>
        let getMutant rebuildMutant (mutantCache : 'm MutantCache) =
            let (mutantUncloned, mutantCache) = getMutantUncloned rebuildMutant mutantCache
            let mutantCloned = mutantCache.CloneMutant mutantUncloned
            (mutantCloned, mutantCache)

        /// <summary>Mutate the underlying mutant (mutable object / record / whatever).</summary>
        /// <param name="rebuildMutant">A function that rebuilds the mutant from scratch in case the current underlying mutant is out of date.</param>
        /// <param name="mutateMutant">A function that mutates the underlying mutant.</param>
        /// <param name="mutantCache">The mutant cache.</param>
        let mutateMutant rebuildMutant mutateMutant (mutantCache : 'm MutantCache) =
            let (mutant, mutantCache) = getMutantUncloned rebuildMutant mutantCache
            mutantCache.OptValidMutant <- None
            let mutant = mutateMutant mutant
            { mutantCache with OptValidMutant = Some mutant }

        /// <summary>Make a mutant cache.</summary>
        /// <param name="cloneMutant">
        /// A function to clone the mutant before presenting it to the outside world.
        /// Feel free to pass id if you can ensure that the presented mutant will never be mutated externally.
        /// </param>
        /// <param name="mutant">The mutant (mutable object / record / whatever) to be cached.</param>
        let make cloneMutant (mutant : 'm) =
            { CloneMutant = cloneMutant
              OptValidMutant = Some mutant }