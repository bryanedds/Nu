// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime

[<AutoOpen>]
module UsetModule =

    type [<NoEquality; NoComparison>] Uset<'a when 'a : comparison> =
        private
            { RefSet : 'a Tset ref }

    [<RequireQualifiedAccess>]
    module Uset =

        let makeEmpty<'a when 'a : comparison> optCommitMultiplier =
            { RefSet = ref ^ Tset.makeEmpty<'a> optCommitMultiplier }

        let add value set =
            { RefSet = ref ^ Tset.add value !set.RefSet }

        let remove value set =
            { RefSet = ref ^ Tset.remove value !set.RefSet }

        let isEmpty set =
            Tset.isEmpty !set.RefSet

        let notEmpty set =
            Tset.notEmpty !set.RefSet

        let contains value set =
            let (result, tset) = Tset.contains value !set.RefSet
            set.RefSet := tset
            result

        let toSeq set =
            let (seq, tset) = Tset.toSeq !set.RefSet
            set.RefSet := tset
            seq

        let ofSeq pairs =
            { RefSet = ref ^ Tset.ofSeq pairs }

        let fold folder state set =
            let (result, tset) = Tset.fold folder state !set.RefSet
            set.RefSet := tset
            result

        let map mapper set =
            let (result, tset) = Tset.map mapper !set.RefSet
            set.RefSet := tset
            result

        let filter pred set =
            let (result, tset) = Tset.filter pred !set.RefSet
            set.RefSet := tset
            result

type Uset<'a when 'a : comparison> = UsetModule.Uset<'a>