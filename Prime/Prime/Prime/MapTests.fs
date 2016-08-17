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
            dict.Keys
            |> Seq.cast<_>
            |> Seq.forall(fun k ->
                let dictVal = dict.[k]
                let vmapVal = Vmap.find k m
                dictVal = vmapVal)
        eqDictAfterSteps actions (Vmap.makeEmpty) (Vmap.add) (Vmap.remove) genKey genVal eqDict

    [<Property>]
    let uMapEqualsDictionaryAfterSteps (actions: MapAction[]) =
        let genKey = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let genVal = Gen.choose(Int32.MinValue + 1, Int32.MaxValue)
        let eqDict (m:Umap<_,_>) (dict:Dictionary<_,_>) =
            dict.Keys
            |> Seq.cast<_>
            |> Seq.forall(fun k ->
                let dictVal = dict.[k]
                let vmapVal = Umap.find k m
                dictVal = vmapVal)
        let ctor() = Umap.makeEmpty(None)
        eqDictAfterSteps actions ctor (Umap.add) (Umap.remove) genKey genVal eqDict

       

            

    

    

