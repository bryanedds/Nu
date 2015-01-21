// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

[<RequireQualifiedAccess>]
module Map
open Prime

/// Make a singleton map.
let singleton key value =
    Map.add key value Map.empty

/// Add multiple values to a map.
let addMany kvps map =
    Seq.fold
        (fun map (key, value) -> Map.add key value map)
        map
        kvps

/// Make a map from a seq by a function.
/// TODO: Optimize by program fusion.
let ofSeqBy by seq =
    let pairs = Seq.map by seq
    Map.ofSeq pairs

/// Convert a map to a seq by a function.
/// TODO: Optimize by program fusion.
let toSeqBy by map =
    let seq = Map.toSeq map
    Seq.map (fun (k, v) -> by k v) seq

/// Get a seq of a map's keys.
let toKeySeq map =
    toSeqBy (fun k _ -> k) map

/// Convert a seq of a map's keys by a function.
let toKeySeqBy by map =
    toSeqBy (by << fst) map

/// Get a seq of a map's values.
let toValueSeq map =
    toSeqBy (fun _ v -> v) map

/// Convert a seq of a map's values by a function.
let toValueSeqBy by map =
    toSeqBy (by << snd) map

/// Make a map from a list by a function.
/// TODO: Optimize by program fusion.
let ofListBy by list =
    let pairs = List.map by list
    Map.ofList pairs

/// Convert a map to a list by a function.
/// TODO: Optimize by program fusion.
let toListBy by map =
    let list = Map.toList map
    List.map by list

/// Get a list of a map's keys.
let toKeyList map =
    toListBy fst map

/// Convert a list of a map's keys by a function.
let toKeyListBy by map =
    toListBy (by << fst) map

/// Get a list of a map's values.
let toValueList map =
    toListBy snd map

/// Convert a list of a map's values by a function.
let toValueListBy by map =
    toListBy (by << snd) map

/// Convert any map value to an obj.
let inline objectify _ x =
    objectify x