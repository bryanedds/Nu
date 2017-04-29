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
            { mutable TMapOpt : TMap<'k, 'v>
              TConfig : TConfig
              Dict : Dictionary<'k, 'v>
              DictOrigin : Dictionary<'k, 'v>
              Logs : Log<'k, 'v> list
              LogsLength : int }

        static member (>>.) (map : TMap<'k2, 'v2>, builder : TExpr<unit, TMap<'k2, 'v2>>) =
            snd' (builder map)

        static member (.>>) (map : TMap<'k2, 'v2>, builder : TExpr<'v2, TMap<'k2, 'v2>>) =
            fst' (builder map)

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
            oldMap.TMapOpt <- Unchecked.defaultof<TMap<'k, 'v>>
            map.TMapOpt <- map
            map

        let private compress map =
            let oldMap = map
            let dictOrigin = Dictionary<'k, 'v> (map.Dict, HashIdentity.Structural)
            let map = { map with DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
            oldMap.TMapOpt <- Unchecked.defaultof<TMap<'k, 'v>>
            map.TMapOpt <- map
            map

        let private validate2 bloatFactor map =
            match box map.TMapOpt with
            | null -> commit map
            | target ->
                match obj.ReferenceEquals (target, map) with
                | true -> if map.LogsLength > map.Dict.Count * bloatFactor then compress map else map
                | false -> commit map

        let private update updater bloatFactor map =
            let oldMap = map
            let map = validate2 bloatFactor map
            let map = updater map
            oldMap.TMapOpt <- Unchecked.defaultof<TMap<'k, 'v>>
            map.TMapOpt <- map
            map

        let private validate map =
            match map.TConfig with
            | BloatFactor bloatFactor -> validate2 bloatFactor map
            | Imperative -> map

        let makeFromSeq<'k, 'v when 'k : equality> configOpt entries =
            let config = Option.getOrDefault (BloatFactor 1) configOpt
            match config with
            | BloatFactor _ ->
                let dict = dictPlus entries
                let dictOrigin = Dictionary (dict, HashIdentity.Structural)
                let map =
                    { TMapOpt = Unchecked.defaultof<TMap<'k, 'v>>
                      TConfig = config
                      Dict = dict
                      DictOrigin = dictOrigin
                      Logs = []
                      LogsLength = 0 }
                map.TMapOpt <- map
                map
            | Imperative ->
                { TMapOpt = Unchecked.defaultof<TMap<'k, 'v>>
                  TConfig = config
                  Dict = dictPlus entries
                  DictOrigin = Dictionary HashIdentity.Structural
                  Logs = []
                  LogsLength = 0 }

        let makeEmpty<'k, 'v when 'k : equality> configOpt =
            makeFromSeq<'k, 'v> configOpt Seq.empty

        let add key value map =
            match map.TConfig with
            | BloatFactor bloatFactor ->
                update (fun map ->
                    let map = { map with Logs = Add (key, value) :: map.Logs; LogsLength = map.LogsLength + 1 }
                    map.Dict.ForceAdd (key, value)
                    map)
                    bloatFactor
                    map
            | Imperative -> map.Dict.ForceAdd (key, value); map

        let remove key map =
            match map.TConfig with
            | BloatFactor bloatFactor ->
                update (fun map ->
                    let map = { map with Logs = Remove key :: map.Logs; LogsLength = map.LogsLength + 1 }
                    map.Dict.Remove key |> ignore
                    map)
                    bloatFactor
                    map
            | Imperative -> map.Dict.Remove key |> ignore; map

        let isEmpty map =
            let map = validate map
            struct (map.Dict.Count = 0, map)

        let notEmpty map =
            mapFst' not (isEmpty map)

        /// Get the length of the map (constant-time, obviously).
        let length map =
            let map = validate map
            struct (map.Dict.Count, map)

        let tryFindFast key map =
            let map = validate map
            match map.Dict.TryGetValue key with
            | (true, value) -> struct (FOption.some value, map)
            | (false, _) -> struct (FOption.none (), map)

        let tryFind key map =
            let map = validate map
            match map.Dict.TryGetValue key with
            | (true, value) -> struct (Some value, map)
            | (false, _) -> struct (None, map)

        let find key map =
            let map = validate map
            struct (map.Dict.[key], map)

        let containsKey key map =
            match tryFind key map with
            | struct (Some _, map) -> struct (true, map)
            | struct (None, map) -> struct (false, map)

        /// Add all the given entries to the map.
        let addMany entries map =
            Seq.fold (flip (uncurry add)) map entries

        /// Remove all values with the given keys from the map.
        let removeMany keys map =
            Seq.fold (flip remove) map keys

        /// Convert a TMap to a seq. Note that entire map is iterated eagerly since the underlying
        /// Dictionary could otherwise opaquely change during iteration.
        let toSeq map =
            let map = validate map
            let seq =
                map.Dict |>
                Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |>
                Array.ofSeq :>
                seq<'k * 'v>
            struct (seq, map)

        let fold folder state map =
            let struct (seq, map) = toSeq map
            let result = Seq.fold (folder >> uncurry) state seq
            struct (result, map)

        let map mapper map =
            fold
                (fun map key value -> add key (mapper value) map)
                (makeEmpty (Some map.TConfig))
                map

        let filter pred map =
            fold
                (fun state k v -> if pred k v then add k v state else state)
                (makeEmpty (Some map.TConfig))
                map

type TMap<'k, 'v when 'k : equality> = TMapModule.TMap<'k, 'v>