// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UMapModule =

    type [<NoEquality; NoComparison>] UMap<'k, 'v when 'k : equality> =
        private
            { MapRef : TMap<'k, 'v> ref }
    
        interface IEnumerable<'k * 'v> with
            member this.GetEnumerator () =
                let struct (seq, tmap) = TMap.toSeq !this.MapRef
                this.MapRef := tmap
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'k * 'v>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module UMap =

        let makeFromSeq<'k, 'v when 'k : equality> config entries =
            { MapRef = ref (TMap.makeFromSeq<'k, 'v> config entries) }

        let makeEmpty<'k, 'v when 'k : equality> config =
            { MapRef = ref (TMap.makeEmpty<'k, 'v> config) }

        let getConfig map =
            let struct (result, tmap) = TMap.getConfig !map.MapRef
            map.MapRef := tmap
            result

        let add key value map =
            { MapRef = ref (TMap.add key value !map.MapRef) }

        let remove key map =
            { MapRef = ref (TMap.remove key !map.MapRef) }

        let isEmpty map =
            let struct (result, tmap) = TMap.isEmpty !map.MapRef
            map.MapRef := tmap
            result

        let notEmpty map =
            not (isEmpty map)

        let tryFindFast key map =
            let struct (valueOpt, tmap) = TMap.tryFindFast key !map.MapRef
            map.MapRef := tmap
            valueOpt

        let tryFind key map =
            let struct (valueOpt, tmap) = TMap.tryFind key !map.MapRef
            map.MapRef := tmap
            valueOpt

        let find key map =
            let struct (item, tmap) = TMap.find key !map.MapRef
            map.MapRef := tmap
            item

        let containsKey key map =
            let struct (result, tmap) = TMap.containsKey key !map.MapRef
            map.MapRef := tmap
            result

        /// Add all the given entries to the map.
        let addMany entries map =
            { MapRef = ref (TMap.addMany entries !map.MapRef) }
    
        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            { MapRef = ref (TMap.removeMany keys !map.MapRef) }

        let toSeq (map : UMap<_, _>) =
            map :> _ seq

        let fold folder state map =
            let struct (result, tmap) = TMap.fold folder state !map.MapRef
            map.MapRef := tmap
            result

        let map mapper map =
            let struct (result, tmap) = TMap.map mapper !map.MapRef
            map.MapRef := tmap
            { MapRef = ref result }

        let filter pred map =
            let struct (result, tmap) = TMap.filter pred !map.MapRef
            map.MapRef := tmap
            { MapRef = ref result }

type UMap<'k, 'v when 'k : equality> = UMapModule.UMap<'k, 'v>