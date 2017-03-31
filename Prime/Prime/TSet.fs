// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TSetModule =

    type private Log<'a when 'a : equality> =
        | Add of 'a
        | Remove of 'a

    type [<NoEquality; NoComparison>] TSet<'a when 'a : equality> =
        private
            { mutable TSet : 'a TSet
              HashSet : 'a HashSet
              HashSetOrigin : 'a HashSet
              Logs : 'a Log list
              LogsLength : int
              BloatFactor : int }

        static member (>>.) (set : 'a2 TSet, builder : TExpr<unit, 'a2 TSet>) =
            (snd ^ builder set)

        static member (.>>) (set : 'a2 TSet, builder : TExpr<'a2, 'a2 TSet>) =
            (fst ^ builder set)

        static member (.>>.) (set : 'a2 TSet, builder : TExpr<'a2, 'a2 TSet>) =
            builder set

    let tset<'a when 'a : equality> = TExprBuilder<'a TSet> ()

    [<RequireQualifiedAccess>]
    module TSet =

        let private commit set =
            let oldSet = set
            let hashSetOrigin = HashSet<'a> (set.HashSetOrigin, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add value -> hashSetOrigin.TryAdd value |> ignore
                | Remove value -> hashSetOrigin.Remove value |> ignore)
                set.Logs ()
            let hashSet = HashSet<'a> (hashSetOrigin, HashIdentity.Structural)
            let set = { set with HashSet = hashSet; HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
            set.TSet <- set
            oldSet.TSet <- set
            set

        let private compress set =
            let oldSet = set
            let hashSetOrigin = HashSet<'a> (set.HashSet, HashIdentity.Structural)
            let set = { set with HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
            set.TSet <- set
            oldSet.TSet <- set
            set

        let private validate set =
            match obj.ReferenceEquals (set.TSet, set) with
            | true -> if set.LogsLength > set.HashSet.Count * set.BloatFactor then compress set else set
            | false -> commit set

        let private update updater set =
            let oldSet = set
            let set = validate set
            let set = updater set
            set.TSet <- set
            oldSet.TSet <- set
            set

        let makeFromSeq<'a when 'a : equality> optBloatFactor items =
            let set =
                { TSet = Unchecked.defaultof<'a TSet>
                  HashSet = hashPlus items
                  HashSetOrigin = hashPlus items
                  Logs = []
                  LogsLength = 0
                  BloatFactor = Option.getOrDefault 1 optBloatFactor }
            set.TSet <- set
            set

        let makeEmpty<'a when 'a : equality> optBloatFactor =
            makeFromSeq<'a> optBloatFactor Seq.empty

        let isEmpty set =
            let set = validate set
            (set.HashSet.Count = 0, set)

        let notEmpty set =
            mapFst not ^ isEmpty set

        /// Get the length of the set (constant-time, obviously).
        let length set =
            let set = validate set
            (set.HashSet.Count, set)

        let add value set =
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.TryAdd value |> ignore
                set)
                set

        let remove value set =
            update (fun set ->
                let set = { set with Logs = Remove value :: set.Logs; LogsLength = set.LogsLength + 1 }
                set.HashSet.Remove value |> ignore
                set)
                set

        /// Add all the given values to the set.
        let addMany values set =
            Seq.fold (flip add) set values

        /// Remove all the given values from the set.
        let removeMany values set =
            Seq.fold (flip remove) set values

        let contains value set =
            let set = validate set
            (set.HashSet.Contains value, set)

        /// Convert a TSet to a seq. Note that entire set is iterated eagerly since the underlying HashMap could
        /// otherwise opaquely change during iteration.
        let toSeq set =
            let set = validate set
            let seq =
                set.HashSet |>
                Array.ofSeq :>
                'a seq
            (seq, set)

        let ofSeq items =
            makeFromSeq None items

        let fold folder state set =
            let (seq, set) = toSeq set
            let result = Seq.fold folder state seq
            (result, set)

        let map mapper set =
            fold
                (fun set value -> add (mapper value) set)
                (makeEmpty ^ Some set.BloatFactor)
                set

        let filter pred set =
            fold
                (fun set value -> if pred value then add value set else set)
                (makeEmpty ^ Some set.BloatFactor)
                set

type TSet<'a when 'a : equality> = TSetModule.TSet<'a>