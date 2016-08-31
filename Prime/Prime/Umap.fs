// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UmapModule =

    type [<NoEquality; NoComparison>] Umap<'k, 'v when 'k : comparison> =
        private
            { RefMap : Tmap<'k, 'v> ref }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let (seq, tmap) = Tmap.toSeq !this.RefMap
                this.RefMap := tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'k * 'v>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module Umap =

        let makeEmpty<'k, 'v when 'k : comparison> optBloatFactor =
            { RefMap = ref ^ Tmap.makeEmpty<'k, 'v> optBloatFactor }

        let add key value map =
            { RefMap = ref ^ Tmap.add key value !map.RefMap }

        let remove key map =
            { RefMap = ref ^ Tmap.remove key !map.RefMap }
    
        /// Add all the given entries to the map.
        let addMany entries map =
            { RefMap = ref ^ Tmap.addMany entries !map.RefMap }
    
        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            { RefMap = ref ^ Tmap.removeMany keys !map.RefMap }

        let isEmpty map =
            Tmap.isEmpty !map.RefMap

        let notEmpty map =
            Tmap.notEmpty !map.RefMap

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
    
        let ofSeq pairs =
            { RefMap = ref ^ Tmap.ofSeq pairs }

        let toSeq (map : Umap<_, _>) =
            map :> _ seq

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