// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

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
                let struct (seq, tlist) = TList.toSeq !this.RefList
                this.RefList := tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

        member this.Item index =
            let struct (result, tlist) = TList.get index !this.RefList
            this.RefList := tlist
            result

    [<RequireQualifiedAccess>]
    module UList =

        let makeFromSeq configOpt items =
            { RefList = ref ^ TList.makeFromSeq configOpt items }

        let makeFromArray configOpt items =
            { RefList = ref ^ TList.makeFromArray configOpt items }

        let makeEmpty<'a> configOpt =
            { RefList = ref ^ TList.makeEmpty<'a> configOpt }

        let get (index : int) (list : 'a UList) =
            list.[index]

        let set index value list =
            { RefList = ref ^ TList.set index value !list.RefList }

        let add value list =
            { RefList = ref ^ TList.add value !list.RefList }

        let remove value list =
            { RefList = ref ^ TList.remove value !list.RefList }

        let isEmpty list =
            let struct (result, tlist) = TList.isEmpty !list.RefList
            list.RefList := tlist
            result

        let notEmpty list =
            not ^ isEmpty list

        let length list =
            let struct (result, tlist) = TList.length !list.RefList
            list.RefList := tlist
            result

        let contains value list =
            let struct (result, tlist) = TList.contains value !list.RefList
            list.RefList := tlist
            result

        let toArray (list : _ UList) =
            let struct (arr, tlist) = TList.toArray !list.RefList
            list.RefList := tlist
            arr

        let toSeq (list : _ UList) =
            list :> _ seq

        let map mapper list =
            let struct (result, tlist) = TList.map mapper !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let filter pred list =
            let struct (result, tlist) = TList.filter pred !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let rev list =
            let struct (result, tlist) = TList.rev !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sortWith comparison list =
            let struct (result, tlist) = TList.sortWith comparison !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sortBy by list =
            let struct (result, tlist) = TList.sortBy by !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let sort list =
            let struct (result, tlist) = TList.sort !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let fold folder state list =
            let struct (result, tlist) = TList.fold folder state !list.RefList
            list.RefList := tlist
            result

        let definitize list =
            let struct (result, tlist) = TList.definitize !list.RefList
            list.RefList := tlist
            { RefList = ref result }

        let makeFromLists configOpt lists =
            let tlists = !(map (fun (list : 'a UList) -> !list.RefList) lists).RefList
            let tlist = TList.makeFromLists configOpt tlists
            { RefList = ref tlist }

        /// Add all the given values to the list.
        let addMany values list =
            { RefList = ref ^ TList.addMany values !list.RefList }

        /// Remove all the given values from the list.
        let removeMany values list =
            { RefList = ref ^ TList.removeMany values !list.RefList }

type 'a UList = 'a UListModule.UList