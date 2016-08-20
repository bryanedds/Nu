// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UsetModule =

    type [<NoEquality; NoComparison>] Uset<'a when 'a : comparison> =
        private
            { RefSet : 'a Tset ref }
    
        interface IEnumerable<'a> with
            member this.GetEnumerator () =
                let (seq, tset) = Tset.toSeq !this.RefSet
                this.RefSet := tset
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'a>).GetEnumerator () :> IEnumerator

    [<RequireQualifiedAccess>]
    module Uset =

        let makeEmpty<'a when 'a : comparison> optBloatFactor =
            { RefSet = ref ^ Tset.makeEmpty<'a> optBloatFactor }

        let add value set =
            { RefSet = ref ^ Tset.add value !set.RefSet }

        let remove value set =
            { RefSet = ref ^ Tset.remove value !set.RefSet }
    
        /// Add all the given values to the set.
        let addMany values set =
            { RefSet = ref ^ Tset.addMany values !set.RefSet }
    
        /// Remove all the given values from the set.
        let removeMany values set =
            { RefSet = ref ^ Tset.removeMany values !set.RefSet }

        let isEmpty set =
            Tset.isEmpty !set.RefSet

        let notEmpty set =
            Tset.notEmpty !set.RefSet

        let contains value set =
            let (result, tset) = Tset.contains value !set.RefSet
            set.RefSet := tset
            result

        let ofSeq pairs =
            { RefSet = ref ^ Tset.ofSeq pairs }

        let toSeq (set : _ Uset) =
            set :> _ seq

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