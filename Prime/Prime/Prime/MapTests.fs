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
    type [<RequireQualifiedAccess>] MapAction = 
        | SetKey
        | RemoveRandom

    /// Checks if a map is equal to a dictionary with the all the actions applied
    let inline eqDictAfterSteps
        (actions: MapAction[]) 
        (mapCtor: unit->'m) 
        (setKey: 'k->'v->'m->'m when 'k: comparison and 'v: comparison) 
        (removeKey: 'k->'m->'m when 'k: comparison and 'v: comparison) 
        (genKey: Gen<'k>) 
        (genVal: Gen<'v>) 
        (eqDict: 'm->Dictionary<'k, 'v>->bool) =

        let testMap = ref (mapCtor())
        let dict = Dictionary<'k, 'v>()
        
        for action in actions do
            match action with
            | MapAction.SetKey ->
                let k = genKey.Sample(0, 1).Head
                let v = genVal.Sample(0, 1).Head
                dict.[k] <- v
                testMap := setKey k v !testMap
            | MapAction.RemoveRandom ->
                if dict.Keys.Count > 0 then
                    let idx = Gen.choose(0, dict.Keys.Count - 1).Sample(0, 1).Head
                    let k = dict.Keys |> Seq.item idx
                    let _ = dict.Remove(k)
                    testMap := removeKey k !testMap
        eqDict !testMap dict

    [<Property>]
    let vMapEqualsDictionaryAfterSteps (actions: MapAction[]) =
        let genKey = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let genVal = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let eqDict (m:Vmap<_,_>) (dict:Dictionary<_,_>) =
            Seq.forall (fun (KeyValue(k,v)) -> Vmap.find k m = v) dict

        eqDictAfterSteps actions (Vmap.makeEmpty) (Vmap.add) (Vmap.remove) genKey genVal eqDict

    [<Property>]
    let uMapEqualsDictionaryAfterSteps (actions: MapAction[]) =
        let genKey = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let genVal = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let eqDict (m:Umap<_,_>) (dict:Dictionary<_,_>) =
            Seq.forall (fun (KeyValue(k,v)) -> Umap.find k m = v) dict
        let ctor() = Umap.makeEmpty(None)

        eqDictAfterSteps actions ctor (Umap.add) (Umap.remove) genKey genVal eqDict

    /// Keeps a reference to all persistent collections returned after
    /// performing an action, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Map
    let inline eqMapsAfterSteps
        (actions: MapAction[])
        (mapCtor: unit->'m)
        (setKey: 'k->'v->'m->'m when 'k: comparison and 'v: comparison)
        (removeKey: 'k->'m->'m when 'k: comparison and 'v: comparison)
        (genKey: Gen<'k>)
        (genVal: Gen<'v>)
        (eqDict: 'm->Map<'k, 'v>->bool) =

        let applyAction fsMap testMap action =
            match action with
            | MapAction.SetKey ->
                let k = genKey.Sample(0, 1).Head
                let v = genVal.Sample(0, 1).Head
                (Map.add k v fsMap, setKey k v testMap)
            | MapAction.RemoveRandom when Map.isEmpty fsMap ->
                (fsMap, testMap)
            | MapAction.RemoveRandom ->
                let idx = Gen.choose(0, fsMap.Count - 1).Sample(0, 1).Head
                let ary = Map.toArray fsMap
                let key = fst ary.[idx]
                (Map.remove key fsMap, removeKey key testMap)

        let (fsMaps, testMaps) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsMap::fsMaps, testMap::testMaps) ->
                        let (newF, newT) = applyAction fsMap testMap action
                        (newF::fsMap::fsMaps, newT::testMap::testMaps)
                    | _ -> failwith "Logic error")
                ([Map.empty], [mapCtor()])
                actions

        List.forall2
            eqDict
            testMaps
            fsMaps

    [<Property>]
    let vMapsEqualFsMapsAfterSteps (actions: MapAction[]) =
        let genKey = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let genVal = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let eqDict (m:Vmap<_,_>) (dict:Map<_,_>) =
            Seq.forall (fun (KeyValue(k,v)) -> Vmap.find k m = v) dict

        eqMapsAfterSteps actions (Vmap.makeEmpty) (Vmap.add) (Vmap.remove) genKey genVal eqDict

    [<Property>]
    let uMapsEqualFsMapsAfterSteps (actions: MapAction[]) =
        let genKey = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let genVal = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let eqDict (m:Umap<_,_>) (dict:Map<_,_>) =
            Seq.forall (fun (KeyValue(k,v)) -> Umap.find k m = v) dict
        let ctor() = Umap.makeEmpty(None)

        eqMapsAfterSteps actions ctor (Umap.add) (Umap.remove) genKey genVal eqDict