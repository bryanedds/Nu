// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module LunTriePlus
open System

type 'p Key = 'p * Lun

let empty : LunTriePlus<'p, 'v> = Map.empty

let isEmpty (triePlus : LunTriePlus<'p, 'v>) = triePlus.IsEmpty

let tryFind (plusKey, trieKey) triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> None
    | Some trie -> LunTrie.tryFind trieKey trie

let containsKey (plusKey, trieKey) plus triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> false
    | Some trie -> (LunTrie.tryFind trieKey trie).IsSome

let add (plusKey, trieKey) value triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> Map.singleton plusKey (LunTrie.singleton trieKey value)
    | Some trie -> Map.add plusKey (LunTrie.add trieKey value trie) triePlus

let rec addMany kvps triePlus =
    if Seq.isEmpty kvps then triePlus
    else
        let kvpHead = Seq.head kvps
        let kvpTail = Seq.skip 1 kvps
        let newTrie = add (fst kvpHead) (snd kvpHead) triePlus
        addMany kvpTail newTrie

let ofList kvps =
    addMany kvps empty

let ofListBy by kvps =
    let pairs = List.map by kvps
    ofList pairs

let fold (folder : 'a -> 'p * Lun -> 'b -> 'a) state triePlus =
    let foldFolder = fun state (plusKey, trieKey) value -> folder state (plusKey, trieKey) value
    Map.fold foldFolder state triePlus

let map (mapper : 'p * Lun -> 'v -> 'a) (triePlus : LunTriePlus<'p, 'v>) : LunTriePlus<'p, 'a> =
    let mapMapper = fun plusKey trie -> LunTrie.map (fun trieKey value -> mapper (plusKey, trieKey) value) trie
    Map.map mapMapper triePlus

let toValueListBy by trie =
    fold (fun state _ value -> value :: state) [] trie

let toValueList trie =
    toValueListBy (fun value -> value) trie

let singleton (key, value) =
    add key value empty