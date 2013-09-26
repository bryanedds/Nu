// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module LunTrieModule
open System

// A trie indexed with 
// TODO: consider creating a LunTrie enumerator.
type LunTrie<'v> =
    | Empty
    | Leaf of Lun * 'v
    | Branch of Lun * 'v option * Map<char, LunTrie<'v>>

    static member KeyValueToString key value =
        "[Key = \"" + str key + "\"; " +
        "Value = \"" + str value + "\"]"

    static member MapToString map =
        Map.fold
            (fun (accStr : string) _ trie ->
                if accStr.Length = 0 then trie.ToString ()
                else accStr + " " + trie.ToString ())
            String.Empty map

    member this.ToSeqBy by =
        seq {
            match this with
            | Empty -> yield! Seq.empty
            | Leaf (key, value) -> yield by key value
            | Branch (key, optValue, map) ->
                match optValue with
                | None -> for subTrieKvp in map do yield! subTrieKvp.Value.ToSeqBy by
                | Some value ->
                    yield by key value
                    for subTrieKvp in map do yield! subTrieKvp.Value.ToSeqBy by }

    member this.DebugView
        with get () =
            let seq = this.ToSeqBy (fun key value -> ("Key = \"" + key.LunStr + "\"", value))
            List.ofSeq seq