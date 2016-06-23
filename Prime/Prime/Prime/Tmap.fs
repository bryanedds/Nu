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

        member this.For (sequence : _ seq, body) =
            this.Using (sequence.GetEnumerator (), (fun enr ->
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

        static member (>.) (map : Tmap<'k2, 'v2>, builder : Texpr<unit, Tmap<'k2, 'v2>>) =
            (snd ^ builder map)

        static member (.>) (map : Tmap<'k2, 'v2>, builder : Texpr<'a2, Tmap<'k2, 'v2>>) =
            (fst ^ builder map)

        static member (.>.) (map : Tmap<'k2, 'v2>, builder : Texpr<'a2, Tmap<'k2, 'v2>>) =
            builder map

    let tmap<'k, 'v when 'k : comparison> = TexprBuilder<Tmap<'k, 'v>> ()

    [<RequireQualifiedAccess>]
    module Tmap =

        let commit map =
            let dict = Dictionary<'k, 'a> (map.Dict, HashIdentity.Structural)
            List.foldBack (fun tlog () ->
                match tlog with
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

        let find key map =
            tryFind key map |> mapFst Option.get

        let makeEmpty<'k, 'a when 'k : comparison> commitMultiplier =
            let map =
                { Dict = Dictionary<'k, 'a> HashIdentity.Structural
                  Logs = []
                  LogsLength = 0
                  Tmap = Unchecked.defaultof<Tmap<'k, 'a>>
                  CommitMultiplier = commitMultiplier }
            map.Tmap <- map
            map

        let private test () =
            let map = makeEmpty 4
            map .>. tmap {
                let! optA = tryFind "A"
                return optA.Value }