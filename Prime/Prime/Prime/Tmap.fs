// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections.Generic

[<AutoOpen>]
module Tmap =

    type private Log<'k, 'v when 'k : comparison> =
        | Add of 'k * 'v
        | Remove of 'k

    type [<NoEquality; NoComparison>] Tmap<'k, 'v when 'k : comparison> =
        private
            { Dict : Dictionary<'k, 'v>
              Logs : Log<'k, 'v> list
              LogsLength : int
              CommitMultiplier : int
              mutable Tmap : Tmap<'k, 'v> }

    module Tmap =

        let commit map =
            let dict = Dictionary<'k, 'v> (map.Dict, HashIdentity.Structural)
            List.foldBack (fun tlog () ->
                match tlog with
                | Add (key, value) -> dict.[key] <- value
                | Remove key -> ignore ^ dict.Remove(key))
                map.Logs ()
            let map = { map with Dict = dict; Logs = []; LogsLength = 0 }
            map.Tmap <- map
            map

        let validate map =
            if  not ^ obj.ReferenceEquals (map.Tmap, map) ||
                map.LogsLength = map.Dict.Count * map.CommitMultiplier then
                commit map
            else map

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

        let makeEmpty<'k, 'v when 'k : comparison> commitMultiplier =
            let map =
                { Dict = Dictionary<'k, 'v> HashIdentity.Structural
                  Logs = []
                  LogsLength = 0
                  Tmap = Unchecked.defaultof<Tmap<'k, 'v>>
                  CommitMultiplier = commitMultiplier }
            map.Tmap <- map
            map