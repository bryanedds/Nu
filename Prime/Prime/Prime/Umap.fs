// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime

[<AutoOpen>]
module UmapModule =

    type [<NoEquality; NoComparison>] Umap<'k, 'v when 'k : comparison> =
        private
            { RefMap : Tmap<'k, 'v> ref }

    [<RequireQualifiedAccess>]
    module Umap =

        let makeEmpty<'k, 'v when 'k : comparison> optCommitMultiplier =
            { RefMap = ref ^ Tmap.makeEmpty<'k, 'v> optCommitMultiplier }

        let add key value map =
            { RefMap = ref ^ Tmap.add key value !map.RefMap }

        let remove key map =
            { RefMap = ref ^ Tmap.remove key !map.RefMap }

        let tryFind key map =
            let (optValue, tmap) = Tmap.tryFind key !map.RefMap
            map.RefMap := tmap
            optValue

        let find key map =
            let (value, tmap) = Tmap.find key !map.RefMap
            map.RefMap := tmap
            value

        let containsKey key map =
            let (result, tmap) = Tmap.containsKey key !map.RefMap
            map.RefMap := tmap
            result

        let toSeq map =
            let (seq, tmap) = Tmap.toSeq !map.RefMap
            map.RefMap := tmap
            seq
    
        let ofSeq pairs =
            { RefMap = ref ^ Tmap.ofSeq pairs }

        let fold folder state map =
            let (result, tmap) = Tmap.fold folder state !map.RefMap
            map.RefMap := tmap
            result

        let map mapper map =
            let (result, tmap) = Tmap.map mapper !map.RefMap
            map.RefMap := tmap
            result

        let filter pred map =
            let (result, tmap) = Tmap.filter pred !map.RefMap
            map.RefMap := tmap
            result

type Umap<'k, 'v when 'k : comparison> = UmapModule.Umap<'k, 'v>