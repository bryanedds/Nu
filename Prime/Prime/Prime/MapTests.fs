// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open System.IO
open System.Collections.Generic
open Xunit
open FsCheck
open FsCheck.Xunit
open Prime
module MapTests =

    type MapAction<'k, 'v> = 
        | SetKey of 'k * 'v
        | RemoveRandom

    /// Keeps a reference to all persistent collections returned after
    /// performing an action, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Map
    let eqMapsAfterSteps
        (fsMap : Map<'k, 'v>)
        (testMap : 'm)
        (actions : MapAction<'k, 'v>[])
        (setKey : 'k->'v->'m->'m when 'k : comparison and 'v : comparison)
        (removeKey : 'k->'m->'m when 'k : comparison and 'v : comparison)
        (eqMap : 'm->Map<'k, 'v>->bool) =

        let applyAction fsMap testMap action =
            match action with
            | MapAction.SetKey (k, v) ->
                (Map.add k v fsMap, setKey k v testMap)
            | MapAction.RemoveRandom when Map.isEmpty fsMap ->
                (fsMap, testMap)
            | MapAction.RemoveRandom ->
                let idx = Gen.choose (0, fsMap.Count - 1) |> Gen.sample 0 1 |> List.head
                let ary = Map.toArray fsMap
                let key = fst ary.[idx]
                (Map.remove key fsMap, removeKey key testMap)

        let (fsMaps, testMaps) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsMap :: fsMaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsMap testMap action
                        (newF :: fsMap :: fsMaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([fsMap], [testMap])
                actions

        List.forall2 eqMap testMaps fsMaps

    [<Property>]
    let vmapsEqualFsmapsAfterSteps (startFsMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Vmap.ofSeq ^ Map.toSeq startFsMap
        let eqMap (vmap : Vmap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq ^ Vmap.toSeq vmap = fsmap
        eqMapsAfterSteps startFsMap testMap actions Vmap.add Vmap.remove eqMap

    [<Property>]
    let umapsEqualFsmapsAfterSteps (startFsMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Umap.ofSeq ^ Map.toSeq startFsMap
        let eqMap (umap : Umap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq ^ Umap.toSeq umap = fsmap
        eqMapsAfterSteps startFsMap testMap actions Umap.add Umap.remove eqMap