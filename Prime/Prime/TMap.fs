// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TMapModule =

    type private Log<'k, 'v when 'k : equality> =
        | Add of 'k * 'v
        | Remove of 'k

    type [<NoEquality; NoComparison>] TMap<'k, 'v when 'k : equality> =
        private
            { TMap : TMap<'k, 'v> WeakReference
              Dict : Dictionary<'k, 'v>
              DictOrigin : Dictionary<'k, 'v>
              Logs : Log<'k, 'v> list
              LogsLength : int
              BloatFactor : int }

        static member (>>.) (map : TMap<'k2, 'v2>, builder : TExpr<unit, TMap<'k2, 'v2>>) =
            (snd ^ builder map)

        static member (.>>) (map : TMap<'k2, 'v2>, builder : TExpr<'v2, TMap<'k2, 'v2>>) =
            (fst ^ builder map)

        static member (.>>.) (map : TMap<'k2, 'v2>, builder : TExpr<'v2, TMap<'k2, 'v2>>) =
            builder map

    let tmap<'k, 'v when 'k : equality> = TExprBuilder<TMap<'k, 'v>> ()

    [<RequireQualifiedAccess>]
    module TMap =

        let private commit map =
            let oldMap = map
            let dictOrigin = Dictionary<'k, 'v> (map.DictOrigin, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add (key, value) -> dictOrigin.ForceAdd (key, value)
                | Remove key -> dictOrigin.Remove key |> ignore)
                map.Logs ()
            let dict = Dictionary<'k, 'v> (dictOrigin, HashIdentity.Structural)
            let map = { map with Dict = dict; DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
            map.TMap.SetTarget map
            oldMap.TMap.SetTarget map
            map

        let private compress map =
            let oldMap = map
            let dictOrigin = Dictionary<'k, 'v> (map.Dict, HashIdentity.Structural)
            let map = { map with DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
            map.TMap.SetTarget map
            oldMap.TMap.SetTarget map
            map

        let private validate map =
            match map.TMap.TryGetTarget () with
            | (true, target) ->
                match obj.ReferenceEquals (target, map) with
                | true -> if map.LogsLength > map.Dict.Count * map.BloatFactor then compress map else map
                | false -> commit map 
            | (false, _) -> commit map

        let private update updater map =
            let oldMap = map
            let map = validate map
            let map = updater map
            map.TMap.SetTarget map
            oldMap.TMap.SetTarget map
            map

        let makeFromSeq<'k, 'v when 'k : equality> optBloatFactor entries =
            let map =
                { TMap = WeakReference<TMap<'k, 'v>> Unchecked.defaultof<TMap<'k, 'v>>
                  Dict = dictPlus entries
                  DictOrigin = dictPlus entries
                  Logs = []
                  LogsLength = 0
                  BloatFactor = Option.getOrDefault 1 optBloatFactor }
            map.TMap.SetTarget map
            map

        let makeEmpty<'k, 'v when 'k : equality> optBloatFactor =
            makeFromSeq<'k, 'v> optBloatFactor Seq.empty

        let isEmpty map =
            let map = validate map
            (map.Dict.Count = 0, map)

        let notEmpty map =
            mapFst not ^ isEmpty map

        /// Get the length of the map (constant-time, obviously).
        let length map =
            let map = validate map
            (map.Dict.Count, map)

        let add key value map =
            update (fun map ->
                let map = { map with Logs = Add (key, value) :: map.Logs; LogsLength = map.LogsLength + 1 }
                map.Dict.ForceAdd (key, value)
                map)
                map

        let remove key map =
            update (fun map ->
                let map = { map with Logs = Remove key :: map.Logs; LogsLength = map.LogsLength + 1 }
                map.Dict.Remove key |> ignore
                map)
                map

        /// Add all the given entries to the map.
        let addMany entries map =
            Seq.fold (flip ^ uncurry add) map entries

        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            Seq.fold (flip remove) map keys

        let tryFindFast key map =
            let map = validate map
            match map.Dict.TryGetValue key with
            | (true, value) -> KeyValuePair (FOption.some value, map)
            | (false, _) -> KeyValuePair (FOption.none (), map)

        let tryFind key map =
            let map = validate map
            match map.Dict.TryGetValue key with
            | (true, value) -> (Some value, map)
            | (false, _) -> (None, map)

        let find key map =
            let map = validate map
            (map.Dict.[key], map)

        let containsKey key map =
            match tryFind key map with
            | (Some _, map) -> (true, map)
            | (None, map) -> (false, map)

        /// Convert a TMap to a seq. Note that entire map is iterated eagerly since the underlying
        /// Dictionary could otherwise opaquely change during iteration.
        let toSeq map =
            let map = validate map
            let seq =
                map.Dict |>
                Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |>
                Array.ofSeq :>
                seq<'k * 'v>
            (seq, map)

        let ofSeq pairs =
            makeFromSeq None pairs

        let fold folder state map =
            let (seq, map) = toSeq map
            let result = Seq.fold (folder >> uncurry) state seq
            (result, map)

        let map mapper map =
            fold
                (fun map key value -> add key (mapper value) map)
                (makeEmpty ^ Some map.BloatFactor)
                map

        let filter pred map =
            fold
                (fun state k v -> if pred k v then add k v state else state)
                (makeEmpty ^ Some map.BloatFactor)
                map

type TMap<'k, 'v when 'k : equality> = TMapModule.TMap<'k, 'v>