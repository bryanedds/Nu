// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TListModule =

    type [<NoEquality; NoComparison>] private 'a Log =
        | Add of 'a
        | Remove of 'a
        | Set of int * 'a

    // TODO: P1: Make operations return struct tuples in next version of F#.
    type [<NoEquality; NoComparison>] 'a TList =
        private
            { TList : 'a TList WeakReference
              ImpList : 'a List
              ImpListOrigin : 'a List
              Logs : 'a Log list
              LogsLength : int
              BloatFactor : int }

        static member (>>.) (list : 'a2 TList, builder : TExpr<unit, 'a2 TList>) =
            (snd ^ builder list)

        static member (.>>) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            (fst ^ builder list)

        static member (.>>.) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            builder list

    let tlist<'a> = TExprBuilder<'a TList> ()

    [<RequireQualifiedAccess>]
    module TList =

        let private commit list =
            let oldList = list
            let impListOrigin = List<'a> list.ImpListOrigin
            List.foldBack (fun log () ->
                match log with
                | Add value -> impListOrigin.Add value
                | Remove value -> impListOrigin.Remove value |> ignore
                | Set (index, value) -> impListOrigin.[index] <- value)
                list.Logs ()
            let impList = List<'a> impListOrigin
            let list = { list with ImpList = impList; ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
            list.TList.SetTarget list
            oldList.TList.SetTarget list
            list

        let private compress list =
            let oldList = list
            let impListOrigin = List<'a> list.ImpList
            let list = { list with ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
            list.TList.SetTarget list
            oldList.TList.SetTarget list
            list

        let private validate list =
            match list.TList.TryGetTarget () with
            | (true, target) ->
                match obj.ReferenceEquals (target, list) with
                | true -> if list.LogsLength > list.ImpList.Count * list.BloatFactor then compress list else list
                | false -> commit list
            | (false, _) -> commit list

        let private update updater list =
            let oldList = list
            let list = validate list
            let list = updater list
            list.TList.SetTarget list
            oldList.TList.SetTarget list
            list

        let private makeFromTempList bloatFactorOpt (tempList : 'a List) =
            let list =
                { TList = WeakReference<'a TList> Unchecked.defaultof<'a TList>
                  ImpList = tempList
                  ImpListOrigin = List<'a> tempList
                  Logs = []
                  LogsLength = 0
                  BloatFactor = Option.getOrDefault 1 bloatFactorOpt }
            list.TList.SetTarget list
            list

        let makeFromSeq bloatFactorOpt (items : 'a seq) =
            makeFromTempList bloatFactorOpt (List<'a> items)

        let makeEmpty<'a> bloatFactorOpt =
            makeFromSeq bloatFactorOpt (List<'a> ())

        let singleton item =
            makeFromSeq None (Seq.singleton item)

        let isEmpty list =
            let list = validate list
            (list.ImpList.Count = 0, list)

        let notEmpty list =
            let list = validate list
            mapFst not ^ isEmpty list

        let get index list =
            let list = validate list
            (list.ImpList.[index], list)

        let set index value list =
            update (fun list ->
                let list = { list with Logs = Set (index, value) :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.[index] <- value
                list)
                list

        let add value list =
            update (fun list ->
                let list = { list with Logs = Add value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Add value |> ignore
                list)
                list

        let remove value list =
            update (fun list ->
                let list = { list with Logs = Remove value :: list.Logs; LogsLength = list.LogsLength + 1 }
                list.ImpList.Remove value |> ignore
                list)
                list

        /// Get the length of the list (constant-time, obviously).
        let length list =
            let list = validate list
            (list.ImpList.Count, list)

        let contains value list =
            let list = validate list
            (list.ImpList.Contains value, list)

        /// Convert a TList to a seq. Note that entire list is iterated eagerly since the underlying .NET List could
        /// otherwise opaquely change during iteration.
        let toSeq list =
            let list = validate list
            let seq = list.ImpList |> Array.ofSeq :> 'a seq
            (seq, list)

        let ofSeq items =
            makeFromSeq None items

        let fold folder state list =
            let (seq, list) = toSeq list
            let result = Seq.fold folder state seq
            (result, list)

        let map (mapper : 'a -> 'b) (list : 'a TList) =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'b> impList.Count
            for i in 0 .. impList.Count - 1 do tempList.Add ^ mapper impList.[i]
            let listMapped = makeFromTempList (Some list.BloatFactor) tempList
            (listMapped, list)

        let filter pred list =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'a> impList.Count
            for i in 0 .. impList.Count - 1 do let item = impList.[i] in if pred item then tempList.Add item
            let listFiltered = makeFromTempList (Some list.BloatFactor) tempList
            (listFiltered, list)

        let rev list =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'a> impList
            tempList.Reverse ()
            let listReversed = makeFromTempList (Some list.BloatFactor) tempList
            (listReversed, list)

        let sortWith comparison list =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'b> impList
            let tempListSorted = Seq.sortWith comparison tempList // NOTE: Generic.List.Sort is _not_ stable, so using a stable one instead...
            let listSorted = makeFromSeq (Some list.BloatFactor) tempListSorted
            (listSorted, list)

        let sortBy by list =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'b> impList.Count
            for i in 0 .. impList.Count - 1 do tempList.Add (by impList.[i])
            let tempListSorted = Seq.sort tempList // NOTE: Generic.List.Sort is _not_ stable, so using a stable one instead...
            let listSorted = makeFromSeq (Some list.BloatFactor) tempListSorted
            (listSorted, list)

        let sort list =
            // OPTIMIZATION: elides building of avoidable transactions.
            let list = validate list
            let impList = list.ImpList
            let tempList = List<'b> impList
            let tempListSorted = Seq.sort tempList // NOTE: Generic.List.Sort is _not_ stable, so using a stable one instead...
            let listSorted = makeFromSeq (Some list.BloatFactor) tempListSorted
            (listSorted, list)

        let definitize list =
            let listMapped = filter Option.isSome list |> fst
            map Option.get listMapped

        let concat lists =
            // OPTIMIZATION: elides building of avoidable transactions.
            let listsAsSeq = toSeq lists |> fst
            let tempList = List<'a> ()
            for list in listsAsSeq do tempList.AddRange (toSeq list |> fst)
            makeFromSeq None tempList

        /// Add all the given values to the list.
        let addMany (values : 'a seq) list =
            let list = validate list
            let lists = add list (singleton (ofSeq values))
            concat lists

        /// Remove all the given values from the list.
        let removeMany values list =
            Seq.fold (flip remove) list values

type 'a TList = 'a TListModule.TList