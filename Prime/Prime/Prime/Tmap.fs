// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TmapModule =

    type private Log<'k, 'a when 'k : comparison> =
        | Add of 'k * 'a
        | Remove of 'k

    type [<NoEquality; NoComparison>] Tmap<'k, 'v when 'k : comparison> =
        private
            { mutable Tmap : Tmap<'k, 'v>
              Dict : Dictionary<'k, 'v>
              DictOrigin : Dictionary<'k, 'v>
              Logs : Log<'k, 'v> list
              LogsLength : int
              CommitMultiplier : int }

        static member (>>.) (map : Tmap<'k2, 'v2>, builder : Texpr<unit, Tmap<'k2, 'v2>>) =
            (snd ^ builder map)

        static member (.>>) (map : Tmap<'k2, 'v2>, builder : Texpr<'a2, Tmap<'k2, 'v2>>) =
            (fst ^ builder map)

        static member (.>>.) (map : Tmap<'k2, 'v2>, builder : Texpr<'a2, Tmap<'k2, 'v2>>) =
            builder map

    let tmap<'k, 'v when 'k : comparison> = TexprBuilder<Tmap<'k, 'v>> ()

    [<RequireQualifiedAccess>]
    module Tmap =

        let private commit map =
            let dictOrigin = Dictionary<'k, 'a> (map.DictOrigin, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add (key, value) -> dictOrigin.[key] <- value
                | Remove key -> ignore ^ dictOrigin.Remove(key))
                map.Logs ()
            let dict = Dictionary<'k, 'a> (dictOrigin, HashIdentity.Structural)
            let map = { map with Dict = dict; DictOrigin = dictOrigin; Logs = []; LogsLength = 0 }
            map.Tmap <- map
            map

        let private isValid map =
            obj.ReferenceEquals (map.Tmap, map) &&
            map.LogsLength <= map.Dict.Count * map.CommitMultiplier

        let private validate map =
            if not ^ isValid map
            then commit map
            else map
    
        let makeEmpty<'k, 'a when 'k : comparison> optCommitMultiplier =
            let map =
                { Tmap = Unchecked.defaultof<Tmap<'k, 'a>>
                  Dict = Dictionary<'k, 'a> HashIdentity.Structural
                  DictOrigin = Dictionary<'k, 'a> HashIdentity.Structural
                  Logs = []
                  LogsLength = 0
                  CommitMultiplier = match optCommitMultiplier with Some cm -> cm | None -> 2 }
            map.Tmap <- map
            map

        let add key value map =
            let map = validate map
            let map = { map with Logs = Add (key, value) :: map.Logs; LogsLength = map.LogsLength + 1; Tmap = map }
            map.Dict.[key] <- value
            map.Tmap <- map
            map
            
        let remove key map =
            let map = validate map
            let map = { map with Logs = Remove key :: map.Logs; LogsLength = map.LogsLength + 1 }
            map.Dict.Remove key |> ignore
            map.Tmap <- map
            map

        let tryFind key map =
            let map = validate map
            match map.Dict.TryGetValue key with
            | (true, value) -> (Some value, map)
            | (false, _) -> (None, map)

        let find key map =
            tryFind key map |> mapFst Option.get

        let containsKey key map =
            match tryFind key map with
            | (Some _, map) -> (true, map)
            | (None, map) -> (false, map)

        /// Convert a Tmap to a seq. Note that entire map is iterated eagerly since the underlying Dictionary could
        /// otherwise opaquely change during iteration.
        let toSeq map =
            let map = validate map
            let seq =
                map.Dict |>
                Seq.map (fun kvp -> (kvp.Key, kvp.Value)) |>
                List.ofSeq |>
                Seq.ofList
            (seq, map)

        let ofSeq pairs =
            Seq.fold
                (fun map (k, v) -> add k v map)
                (makeEmpty None)
                pairs

        let fold folder state map =
            let (seq, map) = toSeq map
            let result = Seq.fold (fun state (key, value) -> folder state key value) state seq
            (result, map)

        let map mapper map =
            fold
                (fun state key value -> add key (mapper value) state)
                (makeEmpty ^ Some map.CommitMultiplier)
                map

        let filter pred map =
            fold
                (fun state k v -> if pred k v then add k v state else state)
                (makeEmpty ^ Some map.CommitMultiplier)
                map

type Tmap<'k, 'v when 'k : comparison> = TmapModule.Tmap<'k, 'v>