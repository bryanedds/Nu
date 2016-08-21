// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TsetModule =

    type private Log<'a when 'a : comparison> =
        | Add of 'a
        | Remove of 'a

    type [<NoEquality; NoComparison>] Tset<'a when 'a : comparison> =
        private
            { mutable Tset : 'a Tset
              HashSet : 'a HashSet
              HashSetOrigin : 'a HashSet
              Logs : 'a Log list
              LogsLength : int
              BloatFactor : int }

        static member (>>.) (set : 'a2 Tset, builder : Texpr<unit, 'a2 Tset>) =
            (snd ^ builder set)

        static member (.>>) (set : 'a2 Tset, builder : Texpr<'a2, 'a2 Tset>) =
            (fst ^ builder set)

        static member (.>>.) (set : 'a2 Tset, builder : Texpr<'a2, 'a2 Tset>) =
            builder set

    let tset<'a when 'a : comparison> = TexprBuilder<'a Tset> ()

    [<RequireQualifiedAccess>]
    module Tset =

        let private commit set =
            let oldSet = set
            let hashSetOrigin = HashSet<'a> (set.HashSetOrigin, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add value -> ignore ^ hashSetOrigin.TryAdd value
                | Remove value -> ignore ^ hashSetOrigin.Remove value)
                set.Logs ()
            let hashSet = HashSet<'a> (hashSetOrigin, HashIdentity.Structural)
            let set = { set with HashSet = hashSet; HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
            set.Tset <- set
            oldSet.Tset <- set
            set

        let private compress set =
            let hashSetOrigin = HashSet<'a> (set.HashSet, HashIdentity.Structural)
            let set = { set with HashSetOrigin = hashSetOrigin; Logs = []; LogsLength = 0 }
            set.Tset <- set
            set

        let private validate set =
            match obj.ReferenceEquals (set.Tset, set) with
            | true -> if set.LogsLength > set.HashSet.Count * set.BloatFactor then compress set else set
            | false -> commit set

        let private update updater set =
            let oldSet = set
            let set = validate set
            let set = updater set
            set.Tset <- set
            oldSet.Tset <- set
            set

        let makeEmpty<'a when 'a : comparison> optBloatFactor =
            let set =
                { Tset = Unchecked.defaultof<'a Tset>
                  HashSet = HashSet<'a> HashIdentity.Structural
                  HashSetOrigin = HashSet<'a> HashIdentity.Structural
                  Logs = []
                  LogsLength = 0
                  BloatFactor = Option.getOrDefault 1 optBloatFactor }
            set.Tset <- set
            set

        let isEmpty set =
            set.HashSet.Count = 0

        let notEmpty set =
            not ^ isEmpty set

        let add value set =
            update (fun set ->
                let set = { set with Logs = Add value :: set.Logs; LogsLength = set.LogsLength + 1 }
                ignore ^ set.HashSet.TryAdd value
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

        /// Convert a Tset to a seq. Note that entire set is iterated eagerly since the underlying HasMap could
        /// otherwise opaquely change during iteration.
        let toSeq set =
            let set = validate set
            let seq =
                set.HashSet |>
                Array.ofSeq :>
                'a seq
            (seq, set)

        let ofSeq pairs =
            Seq.fold
                (flip add)
                (makeEmpty None)
                pairs

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

type Tset<'a when 'a : comparison> = TsetModule.Tset<'a>