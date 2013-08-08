// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Map

/// Create a singleton map.
let singleton key value =
    Map.add key value Map.empty

/// Create a map from a list by a function.
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