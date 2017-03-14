// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UMapModule =

    type [<NoEquality; NoComparison>] UMap<'k, 'v when 'k : equality> =
        private
            { RefMap : TMap<'k, 'v> ref }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let (seq, tmap) = TMap.toSeq !this.RefMap
                this.RefMap := tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'k * 'v>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module UMap =

        let makeFromSeq<'k, 'v when 'k : equality> bloatFactorOpt entries =
            { RefMap = ref ^ TMap.makeFromSeq<'k, 'v> bloatFactorOpt entries }

        let makeEmpty<'k, 'v when 'k : equality> bloatFactorOpt =
            { RefMap = ref ^ TMap.makeEmpty<'k, 'v> bloatFactorOpt }

        let add key value map =
            { RefMap = ref ^ TMap.add key value !map.RefMap }

        let remove key map =
            { RefMap = ref ^ TMap.remove key !map.RefMap }
    
        /// Add all the given entries to the map.
        let addMany entries map =
            { RefMap = ref ^ TMap.addMany entries !map.RefMap }
    
        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            { RefMap = ref ^ TMap.removeMany keys !map.RefMap }

        let isEmpty map =
            let (result, tmap) = TMap.isEmpty !map.RefMap
            map.RefMap := tmap
            result

        let notEmpty map =
            not ^ isEmpty map

        let tryFind key map =
            let (valueOpt, tmap) = TMap.tryFind key !map.RefMap
            map.RefMap := tmap
            valueOpt

        let find key map =
            let (item, tmap) = TMap.find key !map.RefMap
            map.RefMap := tmap
            item

        let containsKey key map =
            let (result, tmap) = TMap.containsKey key !map.RefMap
            map.RefMap := tmap
            result
    
        let ofSeq pairs =
            { RefMap = ref ^ TMap.ofSeq pairs }

        let toSeq (map : UMap<_, _>) =
            map :> _ seq

        let fold folder state map =
            let (result, tmap) = TMap.fold folder state !map.RefMap
            map.RefMap := tmap
            result

        let map mapper map =
            let (result, tmap) = TMap.map mapper !map.RefMap
            map.RefMap := tmap
            { RefMap = ref result }

        let filter pred map =
            let (result, tmap) = TMap.filter pred !map.RefMap
            map.RefMap := tmap
            { RefMap = ref result }

type UMap<'k, 'v when 'k : equality> = UMapModule.UMap<'k, 'v>