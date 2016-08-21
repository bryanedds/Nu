// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open Xunit
open FsCheck
open FsCheck.Xunit
open Prime
open System.Diagnostics

module SetTests =

    type SetAction<'k when 'k: comparison> = 
        | Add of 'k
        | FoldAddingCombination of 'k
        | Remove of 'k

    /// Keeps a reference to all persistent collections returned after
    /// performing actions, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Set
    let eqSetsAfterSteps
        (fsset : Set<'k> )
        (testSet : 's)
        (actions : SetAction<'k>[])
        (addKey : 'k->'s->'s)
        (removeKey : 'k->'s->'s)
        (fold: ('s->'k->'s)->'s->'s->'s)
        (combine : 'k->'k->'k)
        (eqSet : 's->Set<'k>->bool) =

        let applyAction fsset testSet action =
            match action with
            | SetAction.Add(k) ->
                (Set.add k fsset, addKey k testSet)
            | SetAction.FoldAddingCombination(arg) ->
                let newFsset = Set.fold (fun acc e -> Set.add (combine arg e) acc) fsset fsset
                let newTestset = fold (fun acc e -> addKey (combine arg e) acc) testSet testSet
                (newFsset, newTestset)
            | SetAction.Remove(k) ->
                (Set.remove k fsset, removeKey k testSet)

        let (fssets, testMaps) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsmap :: fsmaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsmap testMap action
                        (newF :: fsmap :: fsmaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([fsset], [testSet])
                actions

        let success = List.forall2 eqSet testMaps fssets
        if not success then
            Trace.WriteLine("FAILURE:")
            List.iteri2(fun i fsset testSet  ->
                if i > 0 then
                    Trace.WriteLine(sprintf "After action %A" (actions.[i-1]))
                Trace.WriteLine(sprintf "fsset: %A\ntestSet: %A" fsset testSet)
            ) (List.rev fssets) (List.rev testMaps)
        success

    [<Property>]
    let vsetsEqSetsAfterSteps (initialSet : Set<int>) (actions : SetAction<int>[]) =
        let testSet = Vset.ofSeq ^ Set.toSeq initialSet
        let eqSet (vset : Vset<_>) (fsset : Set<_>) = Set.ofSeq vset = fsset
        eqSetsAfterSteps initialSet testSet actions Vset.add Vset.remove Vset.fold (+) eqSet

    [<Property>]
    let usetsEqSetsAfterSteps (initialSet : Set<int>) (actions : SetAction<int>[]) =
        let testSet = Uset.ofSeq ^ Set.toSeq initialSet
        let eqSet (uset : Uset<_>) (fsset : Set<_>) = Set.ofSeq uset = fsset
        eqSetsAfterSteps initialSet testSet actions Uset.add Uset.remove Uset.fold (+) eqSet
