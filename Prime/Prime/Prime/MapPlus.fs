// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module MapPlus
open System

type Key<'p, 'm> = 'p * 'm

let empty : MapPlus<'p, 'm, 'v> = Map.empty

let isEmpty (mapPlus : MapPlus<'p, 'm, 'v>) = Map.isEmpty

let tryFind (plusKey, mapKey) mapPlus =
    let optMap = Map.tryFind plusKey mapPlus
    match optMap with
    | None -> None
    | Some map -> Map.tryFind mapKey map

let containsKey (plusKey, mapKey) plus mapPlus =
    let optMap = Map.tryFind plusKey mapPlus
    match optMap with
    | None -> false
    | Some map -> (Map.tryFind mapKey map).IsSome

let add (plusKey, mapKey) value mapPlus =
    let optMap = Map.tryFind plusKey mapPlus
    match optMap with
    | None -> Map.singleton plusKey (Map.singleton mapKey value)
    | Some map -> Map.add plusKey (Map.add mapKey value map) mapPlus

let rec addMany kvps mapPlus =
    if Seq.isEmpty kvps then mapPlus
    else
        let kvpHead = Seq.head kvps
        let kvpTail = Seq.skip 1 kvps
        let map' = add (fst kvpHead) (snd kvpHead) mapPlus
        addMany kvpTail map'

let remove (versionKey, mapKey) mapPlus =
    let optMap = Map.tryFind versionKey mapPlus
    match optMap with
    | None -> Map.empty
    | Some map -> Map.add versionKey (Map.remove mapKey map) mapPlus

let rec removeMany keys map =
    if Seq.isEmpty keys then map
    else
        let keyHead = Seq.head keys
        let keyTail = Seq.skip 1 keys
        let map' = remove keyHead map
        removeMany keyTail map'

let ofList kvps =
    addMany kvps empty

let ofListBy by kvps =
    let pairs = List.map by kvps
    ofList pairs

let fold (folder : 'a -> Key<'p, 'm> -> 'b -> 'a) state mapPlus =
    let foldFolder = fun state (plusKey, mapKey) value -> folder state (plusKey, mapKey) value
    Map.fold foldFolder state mapPlus

let map (mapper : Key<'p, 'm> -> 'v -> 'a) (mapPlus : MapPlus<'p, 'm, 'v>) : MapPlus<'p, 'm, 'a> =
    let mapMapper = fun plusKey map -> Map.map (fun mapKey value -> mapper (plusKey, mapKey) value) map
    Map.map mapMapper mapPlus

let toValueListBy by map =
    fold (fun state _ value -> by value :: state) [] map

let toValueList map =
    toValueListBy (fun value -> value) map

let singleton (key, value) =
    add key value empty