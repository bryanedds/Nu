// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UlistModule =

    type [<NoEquality; NoComparison>] Ulist<'a when 'a : comparison> =
        private
            { RefList : 'a Tlist ref }
    
        interface IEnumerable<'a> with
            member this.GetEnumerator () =
                let (seq, tlist) = Tlist.toSeq !this.RefList
                this.RefList := tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> IEnumerable<'a>).GetEnumerator () :> IEnumerator

        member this.Item index =
            let (result, tlist) = Tlist.get index !this.RefList
            this.RefList := tlist
            result

    [<RequireQualifiedAccess>]
    module Ulist =

        let makeEmpty<'a when 'a : comparison> optBloatFactor =
            { RefList = ref ^ Tlist.makeEmpty<'a> optBloatFactor }

        let get (index : int) (list : 'a Ulist) =
            list.[index]

        let set index value list =
            { RefList = ref ^ Tlist.set index value !list.RefList }

        let add value list =
            { RefList = ref ^ Tlist.add value !list.RefList }

        let remove value list =
            { RefList = ref ^ Tlist.remove value !list.RefList }
    
        /// Add all the given values to the list.
        let addMany values list =
            { RefList = ref ^ Tlist.addMany values !list.RefList }
    
        /// Remove all the given values from the list.
        let removeMany values list =
            { RefList = ref ^ Tlist.removeMany values !list.RefList }

        let isEmpty list =
            Tlist.isEmpty !list.RefList

        let notEmpty list =
            Tlist.notEmpty !list.RefList

        let contains value list =
            let (result, tlist) = Tlist.contains value !list.RefList
            list.RefList := tlist
            result

        let ofSeq items =
            { RefList = ref ^ Tlist.ofSeq items }

        let toSeq (list : _ Ulist) =
            list :> _ seq

        let fold folder state list =
            let (result, tlist) = Tlist.fold folder state !list.RefList
            list.RefList := tlist
            result

        let map mapper list =
            let (result, tlist) = Tlist.map mapper !list.RefList
            list.RefList := tlist
            result

        let filter pred list =
            let (result, tlist) = Tlist.filter pred !list.RefList
            list.RefList := tlist
            result

type Ulist<'a when 'a : comparison> = UlistModule.Ulist<'a>