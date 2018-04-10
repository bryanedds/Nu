// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module USetModule =

    type [<NoEquality; NoComparison>] USet<'a when 'a : equality> =
        private
            { SetRef : 'a TSet ref }
    
        interface IEnumerable<'a> with
            member this.GetEnumerator () =
                let struct (seq, tset) = TSet.toSeq !this.SetRef
                this.SetRef := tset
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'a>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module USet =

        let makeFromSeq<'a when 'a : equality> config items =
            { SetRef = ref (TSet.makeFromSeq<'a> config items) }

        let makeEmpty<'a when 'a : equality> config =
            { SetRef = ref (TSet.makeEmpty<'a> config) }

        let getConfig set =
            let struct (result, tset) = TSet.getConfig !set.SetRef
            set.SetRef := tset
            result

        let add value set =
            { SetRef = ref (TSet.add value !set.SetRef) }

        let remove value set =
            { SetRef = ref (TSet.remove value !set.SetRef) }

        let clear set =
            { SetRef = ref (TSet.clear !set.SetRef) }
    
        /// Add all the given values to the set.
        let addMany values set =
            { SetRef = ref (TSet.addMany values !set.SetRef) }
    
        /// Remove all the given values from the set.
        let removeMany values set =
            { SetRef = ref (TSet.removeMany values !set.SetRef) }

        let isEmpty set =
            let struct (result, tset) = TSet.isEmpty !set.SetRef
            set.SetRef := tset
            result

        let notEmpty set =
            not (isEmpty set)

        let contains value set =
            let struct (result, tset) = TSet.contains value !set.SetRef
            set.SetRef := tset
            result

        let toSeq (set : _ USet) =
            set :> _ seq

        let fold folder state set =
            let struct (result, tset) = TSet.fold folder state !set.SetRef
            set.SetRef := tset
            result

        let map mapper set =
            let struct (result, tset) = TSet.map mapper !set.SetRef
            set.SetRef := tset
            { SetRef = ref result }

        let filter pred set =
            let struct (result, tset) = TSet.filter pred !set.SetRef
            set.SetRef := tset
            { SetRef = ref result }

type USet<'a when 'a : equality> = USetModule.USet<'a>