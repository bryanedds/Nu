// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System.Collections
open System.Collections.Generic

[<AutoOpen>]
module UListModule =

    type [<NoEquality; NoComparison>] 'a UList =
        private
            { ListRef : 'a TList ref }
    
        interface 'a IEnumerable with
            member this.GetEnumerator () =
                let struct (seq, tlist) = TList.toSeq !this.ListRef
                this.ListRef := tlist
                seq.GetEnumerator ()
    
        interface IEnumerable with
            member this.GetEnumerator () =
                (this :> 'a IEnumerable).GetEnumerator () :> IEnumerator

        member this.Item index =
            let struct (result, tlist) = TList.get index !this.ListRef
            this.ListRef := tlist
            result

    [<RequireQualifiedAccess>]
    module UList =

        let makeFromSeq config items =
            { ListRef = ref (TList.makeFromSeq config items) }

        let makeFromArray config items =
            { ListRef = ref (TList.makeFromArray config items) }

        let makeEmpty<'a> config =
            { ListRef = ref (TList.makeEmpty<'a> config) }

        let getConfig list =
            let struct (result, tlist) = TList.getConfig !list.ListRef
            list.ListRef := tlist
            result

        let get (index : int) (list : 'a UList) =
            list.[index]

        let set index value list =
            { ListRef = ref (TList.set index value !list.ListRef) }

        let add value list =
            { ListRef = ref (TList.add value !list.ListRef) }

        let remove value list =
            { ListRef = ref (TList.remove value !list.ListRef) }

        let clear list =
            { ListRef = ref (TList.clear !list.ListRef) }

        let isEmpty list =
            let struct (result, tlist) = TList.isEmpty !list.ListRef
            list.ListRef := tlist
            result

        let notEmpty list =
            not (isEmpty list)

        let length list =
            let struct (result, tlist) = TList.length !list.ListRef
            list.ListRef := tlist
            result

        let contains value list =
            let struct (result, tlist) = TList.contains value !list.ListRef
            list.ListRef := tlist
            result

        let toArray (list : _ UList) =
            let struct (arr, tlist) = TList.toArray !list.ListRef
            list.ListRef := tlist
            arr

        let toSeq (list : _ UList) =
            list :> _ seq

        let map mapper list =
            let struct (result, tlist) = TList.map mapper !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let filter pred list =
            let struct (result, tlist) = TList.filter pred !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let rev list =
            let struct (result, tlist) = TList.rev !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let sortWith comparison list =
            let struct (result, tlist) = TList.sortWith comparison !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let sortBy by list =
            let struct (result, tlist) = TList.sortBy by !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let sort list =
            let struct (result, tlist) = TList.sort !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let fold folder state list =
            let struct (result, tlist) = TList.fold folder state !list.ListRef
            list.ListRef := tlist
            result

        let definitize list =
            let struct (result, tlist) = TList.definitize !list.ListRef
            list.ListRef := tlist
            { ListRef = ref result }

        let makeFromLists config lists =
            let tlists = !(map (fun (list : 'a UList) -> !list.ListRef) lists).ListRef
            let tlist = TList.makeFromLists config tlists
            { ListRef = ref tlist }

        /// Add all the given values to the list.
        let addMany values list =
            { ListRef = ref (TList.addMany values !list.ListRef) }

        /// Remove all the given values from the list.
        let removeMany values list =
            { ListRef = ref (TList.removeMany values !list.ListRef) }

type 'a UList = 'a UListModule.UList