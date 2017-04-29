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
        
    type [<NoEquality; NoComparison>] 'a TList =
        private
            { mutable TListOpt : 'a TList
              ImpList : 'a List
              ImpListOrigin : 'a List
              Logs : 'a Log list
              LogsLength : int
              Config : TConfig }

        static member (>>.) (list : 'a2 TList, builder : TExpr<unit, 'a2 TList>) =
            snd' (builder list)

        static member (.>>) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            fst' (builder list)

        static member (.>>.) (list : 'a2 TList, builder : TExpr<'a2, 'a2 TList>) =
            builder list

    let tlist<'a> = TExprBuilder<'a TList> ()

    [<RequireQualifiedAccess>]
    module TList =

        let private commit (_ : int) list =
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
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private compress (_ : int) list =
            let oldList = list
            let impListOrigin = List<'a> list.ImpList
            let list = { list with ImpListOrigin = impListOrigin; Logs = []; LogsLength = 0 }
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private validate2 bloatFactor list =
            match box list.TListOpt with
            | null -> commit bloatFactor list
            | target ->
                match obj.ReferenceEquals (target, list) with
                | true ->
                    if list.LogsLength > list.ImpList.Count * bloatFactor
                    then compress bloatFactor list
                    else list
                | false -> commit bloatFactor list

        let private update updater bloatFactor list =
            let oldList = list
            let list = validate2 bloatFactor list
            let list = updater list
            oldList.TListOpt <- Unchecked.defaultof<'a TList>
            list.TListOpt <- list
            list

        let private validate list =
            match list.Config with
            | BloatFactor bloatFactor -> validate2 bloatFactor list
            | Imperative -> list

        let makeFromSeq configOpt (items : 'a seq) =
            let config = Option.getOrDefault (BloatFactor 1) configOpt
            match config with
            | BloatFactor _ ->
                let impList = List<'a> items
                let impListOrigin = List<'a> impList
                let list =
                    { TListOpt = Unchecked.defaultof<'a TList>
                      ImpList = impList
                      ImpListOrigin = impListOrigin
                      Logs = []
                      LogsLength = 0
                      Config = config }
                list.TListOpt <- list
                list
            | Imperative ->
                { TListOpt = Unchecked.defaultof<'a TList>
                  ImpList = List<'a> items
                  ImpListOrigin = List<'a> ()
                  Logs = []
                  LogsLength = 0
                  Config = config }

        let makeFromArray configOpt (items : 'a array) =
            makeFromSeq configOpt items

        let makeEmpty<'a> configOpt =
            makeFromSeq configOpt (List<'a> ())

        let get index list =
            let list = validate list
            struct (list.ImpList.[index], list)

        let set index value list =
            match list.Config with
            | BloatFactor bloatFactor ->
                update (fun list ->
                    let list = { list with Logs = Set (index, value) :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.[index] <- value
                    list)
                    bloatFactor
                    list
            | Imperative -> list.ImpList.[index] <- value; list

        let add value list =
            match list.Config with
            | BloatFactor bloatFactor ->
                update (fun list ->
                    let list = { list with Logs = Add value :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.Add value |> ignore
                    list)
                    bloatFactor
                    list
            | Imperative -> list.ImpList.Add value |> ignore; list

        let remove value list =
            match list.Config with
            | BloatFactor bloatFactor ->
                update (fun list ->
                    let list = { list with Logs = Remove value :: list.Logs; LogsLength = list.LogsLength + 1 }
                    list.ImpList.Remove value |> ignore
                    list)
                    bloatFactor
                    list
            | Imperative -> list.ImpList.Remove value |> ignore; list

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
            let listMapped = makeFromSeq (Some list.Config) seqMapped
            struct (listMapped, list)

        let filter pred list =
            let list = validate list
            let seqFiltered = Seq.filter pred list.ImpList
            let listFiltered = makeFromSeq (Some list.Config) seqFiltered
            struct (listFiltered, list)

        let rev list =
            let list = validate list
            let seqReversed = Seq.rev list.ImpList
            let listReversed = makeFromSeq (Some list.Config) seqReversed
            struct (listReversed, list)

        let sortWith comparison list =
            let list = validate list
            let seqSorted = Seq.sortWith comparison list.ImpList
            let listSorted = makeFromSeq (Some list.Config) seqSorted
            struct (listSorted, list)

        let sortBy by list =
            let list = validate list
            let seqSorted = Seq.sortBy by list.ImpList
            let listSorted = makeFromSeq (Some list.Config) seqSorted
            struct (listSorted, list)

        let sort list =
            let list = validate list
            let seqSorted = Seq.sort list.ImpList
            let listSorted = makeFromSeq (Some list.Config) seqSorted
            struct (listSorted, list)

        let fold folder state list =
            let struct (seq, list) = toSeq list
            let folded = Seq.fold folder state seq
            struct (folded, list)

        let definitize list =
            let listMapped = filter Option.isSome list |> fst'
            map Option.get listMapped

        let makeFromLists configOpt lists =
            // OPTIMIZATION: elides building of avoidable transactions.
            let listsAsSeq = toSeq lists |> fst'
            let tempList = List<'a> ()
            for list in listsAsSeq do tempList.AddRange (toSeq list |> fst')
            makeFromSeq configOpt tempList

        /// Add all the given values to the list.
        let addMany (values : 'a seq) list =
            let list = validate list
            let lists = add list (makeFromArray (Some list.Config) [|makeFromSeq (Some list.Config) values|])
            makeFromLists (Some list.Config) lists

        /// Remove all the given values from the list.
        let removeMany values list =
            Seq.fold (flip remove) list values

type 'a TList = 'a TListModule.TList