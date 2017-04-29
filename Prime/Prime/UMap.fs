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
                let struct (seq, tmap) = TMap.toSeq !this.RefMap
                this.RefMap := tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'k * 'v>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module UMap =

        let makeFromSeq<'k, 'v when 'k : equality> configOpt entries =
            { RefMap = ref ^ TMap.makeFromSeq<'k, 'v> configOpt entries }

        let makeEmpty<'k, 'v when 'k : equality> configOpt =
            { RefMap = ref ^ TMap.makeEmpty<'k, 'v> configOpt }

        let add key value map =
            { RefMap = ref ^ TMap.add key value !map.RefMap }

        let remove key map =
            { RefMap = ref ^ TMap.remove key !map.RefMap }

        let isEmpty map =
            let struct (result, tmap) = TMap.isEmpty !map.RefMap
            map.RefMap := tmap
            result

        let notEmpty map =
            not ^ isEmpty map

        let tryFindFast key map =
            let struct (valueOpt, tmap) = TMap.tryFindFast key !map.RefMap
            map.RefMap := tmap
            valueOpt

        let tryFind key map =
            let struct (valueOpt, tmap) = TMap.tryFind key !map.RefMap
            map.RefMap := tmap
            valueOpt

        let find key map =
            let struct (item, tmap) = TMap.find key !map.RefMap
            map.RefMap := tmap
            item

        let containsKey key map =
            let struct (result, tmap) = TMap.containsKey key !map.RefMap
            map.RefMap := tmap
            result

        /// Add all the given entries to the map.
        let addMany entries map =
            { RefMap = ref ^ TMap.addMany entries !map.RefMap }
    
        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            { RefMap = ref ^ TMap.removeMany keys !map.RefMap }

        let toSeq (map : UMap<_, _>) =
            map :> _ seq

        let fold folder state map =
            let struct (result, tmap) = TMap.fold folder state !map.RefMap
            map.RefMap := tmap
            result

        let map mapper map =
            let struct (result, tmap) = TMap.map mapper !map.RefMap
            map.RefMap := tmap
            { RefMap = ref result }

        let filter pred map =
            let struct (result, tmap) = TMap.filter pred !map.RefMap
            map.RefMap := tmap
            { RefMap = ref result }

type UMap<'k, 'v when 'k : equality> = UMapModule.UMap<'k, 'v>