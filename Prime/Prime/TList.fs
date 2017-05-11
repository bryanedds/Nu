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
        | Clear
        
    type [<NoEquality; NoComparison>] 'a TList =
        private
            { mutable TListOpt : 'a TList
              TConfig : TConfig
              ImpList : 'a List
              ImpListOrigin : 'a List
              Logs : 'a Log list
              LogsLength : int }

        static member (>>.) (list : 'a2 TList, builder : TExpr<unit, 'a2 TList>) =
            snd' (builder list)

        static member (.>>) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            fst' (builder list)

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
                | Set (index, value) -> impListOrigin.[index] <- value
                | Clear -> impListOrigin.Clear ())
                list.Logs ()
            let impList = List<'a> impListOrigin
            let list = { list with ImpList = impList; ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private compress list =
            let oldList = list
            let impListOrigin = List<'a> list.ImpList
            let list = { list with ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private validate2 list =
            match box list.TListOpt with
            | null -> commit list
            | target ->
                match obj.ReferenceEquals (target, list) with
                | true ->
                    if list.LogsLength > list.ImpList.Count
                    then compress list
                    else list
                | false -> commit list

        let private update updater list =
            let oldList = list
            let list = validate2 list
            let list = updater list
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private validate list =
            if TConfig.isFunctional list.TConfig
            then validate2 list
            else list

        let makeFromSeq config (items : 'a seq) =
            if TConfig.isFunctional config then 
                let impList = List<'a> items
                let impListOrigin = List<'a> impList
                let list =
                    { TListOpt = Unchecked.defaultof<'a TList>
                      TConfig = config
                      ImpList = impList
                      ImpListOrigin = impListOrigin
                      Logs = []
                      LogsLength = 0 }
                list.TListOpt <- list
                list
            else
                { TListOpt = Unchecked.defaultof<'a TList>
                  TConfig = config
                  ImpList = List<'a> items
                  ImpListOrigin = List<'a> ()
                  Logs = []
                  LogsLength = 0 }

        let makeFromArray config (items : 'a array) =
            makeFromSeq config items

        let makeEmpty<'a> config =
            makeFromSeq config (List<'a> ())

        let getConfig list =
            struct (list.TConfig, list)

        let get index list =
            let list = validate list
            struct (list.ImpList.[index], list)

        let set index value list =
            if TConfig.isFunctional list.TConfig then 
                update (fun list ->
                    let list = { list with Logs = Set (index, value) :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.[index] <- value
                    list)
                    list
            else list.ImpList.[index] <- value; list

        let add value list =
            if TConfig.isFunctional list.TConfig then 
                update (fun list ->
                    let list = { list with Logs = Add value :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.Add value |> ignore
                    list)
                    list
            else list.ImpList.Add value |> ignore; list

        let remove value list =
            if TConfig.isFunctional list.TConfig then
                update (fun list ->
                    let list = { list with Logs = Remove value :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.Remove value |> ignore
                    list)
                    list
            else list.ImpList.Remove value |> ignore; list

        let clear list =
            if TConfig.isFunctional list.TConfig then
                update (fun list ->
                    let list = { list with Logs = Clear :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.Clear ()
                    list)
                    list
            else list.ImpList.Clear (); list

        let isEmpty list =
            let list = validate list
            struct (list.ImpList.Count = 0, list)

        let notEmpty list =
            let list = validate list
            mapFst' not (isEmpty list)

        /// Get the length of the list (constant-time, obviously).
        let length list =
            let list = validate list
            struct (list.ImpList.Count, list)

        /// Check that a value is contain in the list.
        let contains value list =
            let list = validate list
            struct (list.ImpList.Contains value, list)

        /// Convert a TList to an array. Note that entire list is iterated eagerly since the underlying .NET List could
        /// otherwise opaquely change during iteration.
        let toArray list =
            let list = validate list
            struct (Array.ofSeq list.ImpList, list)

        /// Convert a TList to a seq. Note that entire list is iterated eagerly since the underlying .NET List could
        /// otherwise opaquely change during iteration.
        let toSeq list =
            let struct (arr, list) = toArray list
            struct (Seq.ofArray arr, list)

        let map (mapper : 'a -> 'b) (list : 'a TList) =
            let list = validate list
            let seqMapped = Seq.map mapper list.ImpList
            let listMapped = makeFromSeq list.TConfig seqMapped
            struct (listMapped, list)

        let filter pred list =
            let list = validate list
            let seqFiltered = Seq.filter pred list.ImpList
            let listFiltered = makeFromSeq list.TConfig seqFiltered
            struct (listFiltered, list)

        let rev list =
            let list = validate list
            let seqReversed = Seq.rev list.ImpList
            let listReversed = makeFromSeq list.TConfig seqReversed
            struct (listReversed, list)

        let sortWith comparison list =
            let list = validate list
            let seqSorted = Seq.sortWith comparison list.ImpList
            let listSorted = makeFromSeq list.TConfig seqSorted
            struct (listSorted, list)

        let sortBy by list =
            let list = validate list
            let seqSorted = Seq.sortBy by list.ImpList
            let listSorted = makeFromSeq list.TConfig seqSorted
            struct (listSorted, list)

        let sort list =
            let list = validate list
            let seqSorted = Seq.sort list.ImpList
            let listSorted = makeFromSeq list.TConfig seqSorted
            struct (listSorted, list)

        let fold folder state list =
            let struct (seq, list) = toSeq list
            let folded = Seq.fold folder state seq
            struct (folded, list)

        let definitize list =
            let listMapped = filter Option.isSome list |> fst'
            map Option.get listMapped

        let makeFromLists config lists =
            // OPTIMIZATION: elides building of avoidable transactions.
            let listsAsSeq = toSeq lists |> fst'
            let tempList = List<'a> ()
            for list in listsAsSeq do tempList.AddRange (toSeq list |> fst')
            makeFromSeq config tempList

        /// Add all the given values to the list.
        let addMany (values : 'a seq) list =
            let list = validate list
            let lists = add list (makeFromArray list.TConfig [|makeFromSeq list.TConfig values|])
            makeFromLists list.TConfig lists

        /// Remove all the given values from the list.
        let removeMany values list =
            Seq.fold (flip remove) list values

type 'a TList = 'a TListModule.TList