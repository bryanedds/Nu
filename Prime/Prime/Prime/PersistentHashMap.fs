// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

[<RequireQualifiedAccess>]
module PersistentHashMap
open FSharpx.Collections
open Prime

/// TODO: document all these fns.
let singleton key value =
    PersistentHashMap.add key value PersistentHashMap.empty

let addMany kvps map =
    Seq.fold
        (fun map (key, value) -> PersistentHashMap.add key value map)
        map
        kvps

let toValueListBy by map =
    PersistentHashMap.toSeq map |> Seq.map (snd >> by) |> List.ofSeq

let toValueList map =
    toValueListBy id map

let toKeyListBy by map =
    PersistentHashMap.toSeq map |> Seq.map (fst >> by) |> List.ofSeq

let toKeyList map =
    toKeyListBy id map

let ofList (list : List<_>) =
    PersistentHashMap.ofSeq list

// TODO: implement this efficiently, that is, without having to do two look-ups or having to
// catch an exception if the key is not present (may require changing FSharpx source...)
let tryFind key map =
    if PersistentHashMap.containsKey key map
    then Some ^ PersistentHashMap.find key map
    else None

let filter pred map =
    PersistentHashMap.toSeq map |> Seq.filter pred |> PersistentHashMap.ofSeq

let fold (folder : 's -> 'k * 'v -> 's) (state : 's) map =
    let seq = PersistentHashMap.toSeq map
    Seq.fold folder state seq