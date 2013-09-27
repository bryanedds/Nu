// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module MapPlus
open System

type Key<'p, 'm> = 'p * 'm

let empty : MapPlus<'p, 'm, 'v> = Map.empty

let isEmpty (triePlus : MapPlus<'p, 'm, 'v>) = Map.isEmpty

let tryFind (plusKey, trieKey) triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> None
    | Some trie -> Map.tryFind trieKey trie

let containsKey (plusKey, trieKey) plus triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> false
    | Some trie -> (Map.tryFind trieKey trie).IsSome

let add (plusKey, trieKey) value triePlus =
    let optTrie = Map.tryFind plusKey triePlus
    match optTrie with
    | None -> Map.singleton plusKey (Map.singleton trieKey value)
    | Some trie -> Map.add plusKey (Map.add trieKey value trie) triePlus

let rec addMany kvps triePlus =
    if Seq.isEmpty kvps then triePlus
    else
        let kvpHead = Seq.head kvps
        let kvpTail = Seq.skip 1 kvps
        let trie2 = add (fst kvpHead) (snd kvpHead) triePlus
        addMany kvpTail trie2

let remove (versionKey, trieKey) triePlus =
    let optTrie = Map.tryFind versionKey triePlus
    match optTrie with
    | None -> Map.empty
    | Some trie -> Map.add versionKey (Map.remove trieKey trie) triePlus

let rec removeMany keys trie =
    if Seq.isEmpty keys then trie
    else
        let keyHead = Seq.head keys
        let keyTail = Seq.skip 1 keys
        let trie2 = remove keyHead trie
        removeMany keyTail trie2

let ofList kvps =
    addMany kvps empty

let ofListBy by kvps =
    let pairs = List.map by kvps
    ofList pairs

let fold (folder : 'a -> Key<'p, 'm> -> 'b -> 'a) state triePlus =
    let foldFolder = fun state (plusKey, trieKey) value -> folder state (plusKey, trieKey) value
    Map.fold foldFolder state triePlus

let map (mapper : Key<'p, 'm> -> 'v -> 'a) (triePlus : MapPlus<'p, 'm, 'v>) : MapPlus<'p, 'm, 'a> =
    let mapMapper = fun plusKey trie -> Map.map (fun trieKey value -> mapper (plusKey, trieKey) value) trie
    Map.map mapMapper triePlus

let toValueListBy by trie =
    fold (fun state _ value -> by value :: state) [] trie

let toValueList trie =
    toValueListBy (fun value -> value) trie

let singleton (key, value) =
    add key value empty