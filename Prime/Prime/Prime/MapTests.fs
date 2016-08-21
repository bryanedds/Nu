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
        | FoldCombine of 'v

    /// Keeps a reference to all persistent collections returned after
    /// performing an action, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Map
    let eqMapsAfterSteps
        (fsmap : Map<'k, 'v>)
        (testMap : 'm)
        (actions : MapAction<'k, 'v> array)
        (add : 'k->'v->'m->'m when 'k : comparison and 'v : comparison)
        (remove : 'k->'m->'m when 'k : comparison and 'v : comparison)
        (fold : ('m->'k->'v->'m)->'m->'m->'m)
        (combineValues : 'v->'v->'v)
        (eq : 'm->Map<'k, 'v>->bool) =

        let applyAction fsmap testMap action =
            match action with
            | MapAction.SetKey (k, v) ->
                (Map.add k v fsmap, add k v testMap)
            | MapAction.RemoveRandom when Map.isEmpty fsmap ->
                (fsmap, testMap)
            | MapAction.RemoveRandom ->
                let idx = Gen.choose (0, fsmap.Count - 1) |> Gen.sample 0 1 |> List.head
                let ary = Map.toArray fsmap
                let key = fst ary.[idx]
                (Map.remove key fsmap, remove key testMap)
            | MapAction.FoldCombine arg ->
                let newFsmap = Map.fold (fun acc k v -> Map.add k (combineValues v arg) acc) fsmap fsmap
                let newTestMap = fold (fun acc k v -> add k (combineValues v arg) acc) testMap testMap
                (newFsmap, newTestMap)

        let (fsmaps, testMaps) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsmap :: fsmaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsmap testMap action
                        (newF :: fsmap :: fsmaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([fsmap], [testMap])
                actions

        List.forall2 eq testMaps fsmaps

    [<Property>]
    let vmapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Vmap.ofSeq ^ Map.toSeq initialMap
        let eq (vmap : Vmap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq vmap = fsmap
        eqMapsAfterSteps initialMap testMap actions Vmap.add Vmap.remove Vmap.fold (+) eq

    [<Property>]
    let umapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Umap.ofSeq ^ Map.toSeq initialMap
        let eq (umap : Umap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq umap = fsmap
        eqMapsAfterSteps initialMap testMap actions Umap.add Umap.remove Umap.fold (+) eq