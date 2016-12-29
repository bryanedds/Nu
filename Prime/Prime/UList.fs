// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UListModule =

    type [<NoEquality; NoComparison>] 'a UList =
        private
            { RefList : 'a TList ref }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let (seq, tlist) = TList.toSeq !this.RefList
                this.RefList := tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

        member this.Item index =
            let (result, tlist) = TList.get index !this.RefList
            this.RefList := tlist
            result

    [<RequireQualifiedAccess>]
    module UList =

        let makeFromSeq bloatFactorOpt items =
            { RefList = ref ^ TList.makeFromSeq bloatFactorOpt items }

        let makeEmpty<'a> bloatFactorOpt =
            { RefList = ref ^ TList.makeEmpty<'a> bloatFactorOpt }

        let singleton item =
            { RefList = ref ^ TList.singleton item }

        let get (index : int) (list : 'a UList) =
            list.[index]

        let set index value list =
            { RefList = ref ^ TList.set index value !list.RefList }

        let add value list =
            { RefList = ref ^ TList.add value !list.RefList }

        let remove value list =
            { RefList = ref ^ TList.remove value !list.RefList }

        let isEmpty list =
            let (result, tlist) = TList.isEmpty !list.RefList
            list.RefList := tlist
            result

        let notEmpty list =
            not ^ isEmpty list

        let contains value list =
            let (result, tlist) = TList.contains value !list.RefList
            list.RefList := tlist
            result

        let ofSeq values =
            { RefList = ref ^ TList.ofSeq values }

        let toSeq (list : _ UList) =
            list :> _ seq

        let fold folder state list =
            let (result, tlist) = TList.fold folder state !list.RefList
            list.RefList := tlist
            result

        let map mapper list =
            let (result, tlist) = TList.map mapper !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let filter pred list =
            let (result, tlist) = TList.filter pred !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let rev list =
            let (result, tlist) = TList.rev !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sortWith comparison list =
            let (result, tlist) = TList.sortWith comparison !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sortBy by list =
            let (result, tlist) = TList.sortBy by !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sort list =
            let (result, tlist) = TList.sort !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let definitize list =
            let (result, tlist) = TList.definitize !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let concat lists =
            let tlists = !(map (fun (list : 'a UList) -> !list.RefList) lists).RefList
            let tlist = TList.concat tlists
            { RefList = ref tlist }

        /// Add all the given values to the list.
        let addMany values list =
            { RefList = ref ^ TList.addMany values !list.RefList }

        /// Remove all the given values from the list.
        let removeMany values list =
            { RefList = ref ^ TList.removeMany values !list.RefList }

type 'a UList = 'a UListModule.UList