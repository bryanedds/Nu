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
        "{Key = \"" + key.ToString () + "\"; " +
        "Value = \"" + value.ToString () + "\"}"

    static member MapToString map =
        Map.fold
            (fun (accStr : string) _ trie ->
                if accStr.Length = 0 then trie.ToString ()
                else accStr + " " + trie.ToString ())
            String.Empty map

    override this.ToString () =
        this.DebugView

    member this.DebugView
        with get () =
            match this with
            | Empty -> String.Empty
            | Leaf (key, value) -> LunTrie<'v>.KeyValueToString key value
            | Branch (key, optValue, map) ->
                match optValue with
                | None -> LunTrie<'v>.MapToString map
                | Some value -> LunTrie<'v>.KeyValueToString key value + " " + LunTrie<'v>.MapToString map
        