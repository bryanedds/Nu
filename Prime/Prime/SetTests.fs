// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open FsCheck.Xunit
open Prime
open System.Diagnostics
module SetTests =

    type SetAction<'a when 'a : comparison> = 
        | Add of 'a
        | Remove of 'a
        | FoldAddingCombination of 'a

    /// Keeps a reference to all persistent collections returned after
    /// performing actions, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.Set
    let eqSetsAfterSteps
        (fsset : Set<'a>)
        (testSet : 's)
        (actions : SetAction<'a> array)
        (add : 'a->'s->'s)
        (remove : 'a->'s->'s)
        (fold : ('s->'a->'s)->'s->'s->'s)
        (combine : 'a->'a->'a)
        (eq : 's->Set<'a>->bool) =

        let applyAction fsset testSet action =
            match action with
            | SetAction.Add k ->
                (Set.add k fsset, add k testSet)
            | SetAction.FoldAddingCombination arg ->
                let newFsset = Set.fold (fun acc e -> Set.add (combine arg e) acc) fsset fsset
                let newTestSet = fold (fun acc e -> add (combine arg e) acc) testSet testSet
                (newFsset, newTestSet)
            | SetAction.Remove k ->
                (Set.remove k fsset, remove k testSet)

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

        let success = List.forall2 eq testMaps fssets
        if not success then
            Trace.WriteLine "FAILURE:"
            List.iteri2 (fun i fsset testSet  ->
                if i > 0 then Trace.WriteLine (sprintf "After action %A" actions.[i-1])
                Trace.WriteLine (sprintf "fsset: %A\ntestSet: %A" fsset testSet))
                (List.rev fssets)
                (List.rev testMaps)
        success

    [<Property>]
    let vsetsEqSetsAfterSteps (initialSet : Set<int>) (actions : SetAction<int>[]) =
        let testSet = Vset.ofSeq ^ Set.toSeq initialSet
        let eq (vset : Vset<_>) (fsset : Set<_>) = Set.ofSeq vset = fsset
        eqSetsAfterSteps initialSet testSet actions Vset.add Vset.remove Vset.fold (+) eq

    [<Property>]
    let usetsEqSetsAfterSteps (initialSet : Set<int>) (actions : SetAction<int>[]) =
        let testSet = Uset.ofSeq ^ Set.toSeq initialSet
        let eq (uset : Uset<_>) (fsset : Set<_>) = Set.ofSeq uset = fsset
        eqSetsAfterSteps initialSet testSet actions Uset.add Uset.remove Uset.fold (+) eq
