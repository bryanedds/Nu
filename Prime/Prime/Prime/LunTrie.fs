// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module LunTrie
open System

let empty = Empty

let isEmpty trie = trie = Empty

let rec private tryFindInternal key (keyStr : string) index trie =
    match trie with
    | Empty -> None
    | Leaf (_, value) -> if keyStr.Length = index then Some value else None
    | Branch (_, optValue, map) ->
        if keyStr.Length = index then optValue
        else
            let optSubTrie = Map.tryFind keyStr.[index] map
            match optSubTrie with
            | None -> None
            | Some subTrie -> tryFindInternal key keyStr (index + 1) subTrie

let tryFind (key : Lun) trie =
    tryFindInternal key key.LunStr 0 trie

let containsKey key trie =
    (tryFind key trie).IsSome

let rec private addInternal key (keyStr : string) index value trie =
    match trie with
    | Empty ->
        if keyStr.Length = index then Leaf (key, value)
        else
            let char = keyStr.[index]
            let newTrie = addInternal key keyStr (index + 1) value empty
            Branch (Lun.empty, None, Map.singleton char newTrie)
    | Leaf (leafKey, leafValue) ->
        if keyStr.Length = index then Leaf (key, value)
        else
            let char = keyStr.[index]
            let newTrie = addInternal key keyStr (index + 1) value empty
            Branch (leafKey, Some leafValue, Map.singleton char newTrie)
    | Branch (branchKey, optBranchValue, branchMap) ->
        if keyStr.Length = index then Branch (key, Some value, branchMap)
        else
            let char = keyStr.[index]
            let subTrie =
                match Map.tryFind char branchMap with
                | None -> empty
                | Some subTrie -> subTrie
            let newTrie = addInternal key keyStr (index + 1) value subTrie
            let newMap = Map.add char newTrie branchMap
            Branch (branchKey, optBranchValue, newMap)
            
let add key value trie =
    addInternal key key.LunStr 0 value trie

let rec addMany kvps trie =
    if Seq.isEmpty kvps then trie
    else
        let kvpHead = Seq.head kvps
        let kvpTail = Seq.skip 1 kvps
        let newTrie = add (fst kvpHead) (snd kvpHead) trie
        addMany kvpTail newTrie

let remove key trie =
    trie // TODO: implement

let removeMany keys trie =
    trie // TODO: implement

let ofList kvps =
    addMany kvps empty

let ofListBy by kvps =
    let pairs = List.map by kvps
    ofList pairs

let toSeqBy by (trie : 'v LunTrie) =
    trie.ToSeqBy by

let toValueSeqBy by trie =
    toSeqBy (fun _ value -> by value) trie

let toValueSeq trie =
    toValueSeqBy id trie

let rec private foldInternal (folder : 'a -> char list -> 'b -> 'a) (rev : char list) (state : 'a) (trie : 'b LunTrie) : 'a =
    match trie with
    | Empty -> state
    | Leaf (_, value) -> folder state rev value
    | Branch (_, optValue, map) ->
        let mapFolder = fun state key value -> foldInternal folder (key :: rev) state value
        let mapFold = Map.fold mapFolder state map
        match optValue with
        | None -> mapFold
        | Some value -> folder mapFold (List.rev rev) value
    
let fold folder state trie =
    foldInternal folder [] state trie

let rec map (mapper : Lun -> 'v -> 'a) (trie : 'v LunTrie) : 'a LunTrie =
    match trie with
    | Empty -> empty
    | Leaf (key, value) -> Leaf (key, mapper key value)
    | Branch (key, optValue, trieMap) ->
        let mapMapper = fun _ trie -> map mapper trie
        let mapMap = Map.map mapMapper trieMap
        match optValue with
        | None -> Branch (key, None, mapMap)
        | Some value -> Branch (key, Some (mapper key value), mapMap)

let toValueListBy by trie =
    fold (fun state _ value -> by value :: state) [] trie

let toValueList trie =
    toValueListBy id trie

let singleton key value =
    add key value empty