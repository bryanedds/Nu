// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic

[<AutoOpen>]
module TexprModule =

    type Texpr<'a, 'env> = 'env -> 'a * 'env

    type TexprBuilder<'env> () =

        member inline this.Bind (expr : Texpr<'a, 'env>, lift : 'a -> Texpr<'b, 'env>) : Texpr<'b, 'env> =
            fun env ->
                let (result, env') = expr env
                let expr' = lift result
                expr' env'

        member inline this.Return (value : 'a) : Texpr<'a, 'env> =
            fun expr ->
                (value, expr)

        member inline this.ReturnFrom (value : 'a) =
            value

        member this.Zero () =
            this.Return ()
        
        member this.Combine (l, r) =
            this.Bind (l, fun () -> r)
        
        member this.TryWith (body : Texpr<'a, 'expr>, handler : exn -> Texpr<'a, 'expr>) : Texpr<'a, 'expr> =
            fun env ->
                try body env
                with exn -> handler exn env

        member this.TryFinally (body : Texpr<'a, 'expr>, compensation) : Texpr<'a,'expr> =
            fun env ->
                try body env
                finally compensation()

        member this.Using (res : #IDisposable, body) =
            this.TryFinally (body res, (fun () ->
                match res with null -> () | disp -> disp.Dispose()))

        member this.Delay f =
            this.Bind (this.Return (), f)

        member this.While (guard, body) =
            if not ^ guard ()
            then this.Zero ()
            else this.Bind (body, (fun () -> this.While (guard, body)))

        member this.For (seq : _ seq, body) =
            this.Using (seq.GetEnumerator (), (fun enr ->
                this.While (enr.MoveNext, this.Delay (fun () ->
                    body enr.Current))))

[<AutoOpen>]
module TmapModule =

    type private Log<'k, 'a when 'k : comparison> =
        | Add of 'k * 'a
        | Remove of 'k

    type [<NoEquality; NoComparison>] Tmap<'k, 'a when 'k : comparison> =
        private
            { Dict : Dictionary<'k, 'a>
              mutable Logs : Log<'k, 'a> list
              mutable LogsLength : int
              mutable Tmap : Tmap<'k, 'a>
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

        let commit map =
            let dict = Dictionary<'k, 'a> (map.Dict, HashIdentity.Structural)
            List.foldBack (fun log () ->
                match log with
                | Add (key, value) -> dict.[key] <- value
                | Remove key -> ignore ^ dict.Remove(key))
                map.Logs ()
            let map = { map with Dict = dict; Logs = []; LogsLength = 0 }
            map.Tmap <- map
            map

        let isValid map =
            obj.ReferenceEquals (map.Tmap, map) &&
            map.LogsLength <= map.Dict.Count * map.CommitMultiplier

        let validate map =
            if not ^ isValid map
            then commit map
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

        let tryFind' key refMap =
            let (optValue, map) = tryFind key !refMap
            refMap := map
            optValue

        let find key map =
            tryFind key map |> mapFst Option.get

        let find' key refMap =
            let (optValue, map) = find key !refMap
            refMap := map
            optValue
    
        let containsKey key map =
            match tryFind key map with
            | (Some _, map) -> (true, map)
            | (None, map) -> (false, map)

        let containsKey' key refMap =
            let (optValue, map) = containsKey key !refMap
            refMap := map
            optValue
    
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
    
        /// Convert a Tmap to a seq. Note that entire map is iterated eagerly since the underlying Dictionary could
        /// otherwise opaquely change during iteration.
        let toSeq' refMap =
            let (seq, map) = toSeq !refMap
            refMap := map
            seq

        let makeEmpty<'k, 'a when 'k : comparison> optCommitMultiplier =
            let map =
                { Dict = Dictionary<'k, 'a> HashIdentity.Structural
                  Logs = []
                  LogsLength = 0
                  Tmap = Unchecked.defaultof<Tmap<'k, 'a>>
                  CommitMultiplier = match optCommitMultiplier with Some cm -> cm | None -> 4 }
            map.Tmap <- map
            map

        let fold folder state map =
            let (seq, map) = toSeq map
            let result = Seq.fold (fun state (key, value) -> folder state key value) state seq
            (result, map)

        let fold' folder state refMap =
            let (result, map) = fold folder state !refMap
            refMap := map
            result
                
        let map mapper map =
            fold
                (fun state key value -> add key (mapper value) state)
                (makeEmpty ^ Some map.CommitMultiplier)
                map

        let map' mapper refMap =
            let (result, map_) = map mapper !refMap
            refMap := map_
            result
    
        let filter pred map =
            fold
                (fun state k v -> if pred k v then add k v state else state)
                (makeEmpty ^ Some map.CommitMultiplier)
                map

        let filter' pred refMap =
            let (result, map) = filter pred !refMap
            refMap := map
            result
    
        let ofSeq kvps =
            Seq.fold
                (fun map (k, v) -> add k v map)
                (makeEmpty None)
                kvps