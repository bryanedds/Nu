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
        (fsmap : Map<'k, 'v>)
        (testMap : 'm)
        (actions : MapAction<'k, 'v>[])
        (setKey : 'k->'v->'m->'m when 'k : comparison and 'v : comparison)
        (removeKey : 'k->'m->'m when 'k : comparison and 'v : comparison)
        (eqMap : 'm->Map<'k, 'v>->bool) =

        let applyAction fsmap testMap action =
            match action with
            | MapAction.SetKey (k, v) ->
                (Map.add k v fsmap, setKey k v testMap)
            | MapAction.RemoveRandom when Map.isEmpty fsmap ->
                (fsmap, testMap)
            | MapAction.RemoveRandom ->
                let idx = Gen.choose (0, fsmap.Count - 1) |> Gen.sample 0 1 |> List.head
                let ary = Map.toArray fsmap
                let key = fst ary.[idx]
                (Map.remove key fsmap, removeKey key testMap)

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

        List.forall2 eqMap testMaps fsmaps

    [<Property>]
    let vmapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Vmap.ofSeq ^ Map.toSeq initialMap
        let eqMap (vmap : Vmap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq ^ Vmap.toSeq vmap = fsmap
        eqMapsAfterSteps initialMap testMap actions Vmap.add Vmap.remove eqMap

    [<Property>]
    let umapsEqualFsmapsAfterSteps (initialMap : Map<int, string>) (actions : MapAction<int, string>[]) =
        let testMap = Umap.ofSeq ^ Map.toSeq initialMap
        let eqMap (umap : Umap<_,_>) (fsmap : Map<_,_>) = Map.ofSeq ^ Umap.toSeq umap = fsmap
        eqMapsAfterSteps initialMap testMap actions Umap.add Umap.remove eqMap