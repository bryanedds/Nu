// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open FsCheck.Xunit
open Prime
open System.Diagnostics
module ListTests =
    type ListAction<'v> = 
        | AddLast of 'v
        | MapIncrementFn
        | FilterWithFn
        | SetNthToNth of int * int

    /// Keeps a reference to all persistent collections returned after
    /// performing actions, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.List
    let eqSetsAfterSteps
        (fslist : 'v list)
        (testlist : 'l)
        (actions : ListAction<'v> array)
        (addLast : 'v->'l->'l)
        (getNth : int->'l->'v)
        (setNth : int->'v->'l->'l)
        (incrf : 'v->'v)
        (mapf : ('v->'v)->'l->'l)
        (pred: 'v->bool)
        (filter: ('v->bool)->'l->'l)
        (eq : 'l->'v list->bool) =

        let applyAction fslist testlist action =
            match action with
            | ListAction.AddLast v ->
                (fslist @ [v], addLast v testlist)
            | ListAction.MapIncrementFn ->
                (List.map incrf fslist, mapf incrf testlist)
            | ListAction.FilterWithFn ->
                (List.filter pred fslist, filter pred testlist)
            | ListAction.SetNthToNth(n1, n2) ->
                let len = List.length fslist
                if len > 0 then
                    let idx1, idx2 = Math.Abs(n1) % len, Math.Abs(n2) % len
                    let fs =
                        let v2 = Seq.item idx2 fslist
                        List.mapi (fun i v -> if i = idx1 then v2 else v) fslist
                    let test =
                        let v2 = getNth idx2 testlist
                        setNth idx1 v2 testlist
                    (fs, test)
                else
                    (fslist, testlist)

        let (fslists, testlists) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsmap :: fsmaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsmap testMap action
                        (newF :: fsmap :: fsmaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([fslist], [testlist])
                actions

        let success = List.forall2 eq testlists fslists
        if not success then
            Trace.WriteLine "FAILURE:"
            List.iteri2 (fun i fsset testSet  ->
                if i > 0 then Trace.WriteLine (sprintf "After action %A" actions.[i-1])
                Trace.WriteLine (sprintf "fsset: %A\ntestSet: %A" fsset testSet))
                (List.rev fslists)
                (List.rev fslists)
        success

    [<Property>]
    /// Proof of concept, we can delete this after we know test is correct
    let aryEqListsAfterSteps (initialList : int list) (actions : ListAction<int> []) =
        let ary = Array.ofList initialList
        let eq (ary: int[]) (fslist: int list) = List.ofSeq ary = fslist
        let pred i = i % 2 = 0

        let add (v:int) (ary: int[]) =
            let ary2 = Array.zeroCreate (ary.Length + 1)
            Array.Copy(ary, ary2, ary.Length)
            ary2.[ary.Length] <- v
            ary2

        let get (i:int) (ary: int[]) = ary.[i]

        let set (i:int) (v:int) (ary: int[]) =
            let ary2 = Array.copy ary
            ary2.[i] <- v
            ary2

        eqSetsAfterSteps initialList ary actions add get set ((+) 1) Array.map pred Array.filter eq

    [<Property>]
    let ulistEqListsAfterSteps (initialList : int list) (actions : ListAction<int>[]) =
        let testList = Ulist.addMany initialList (Ulist.makeEmpty(None))
        let eq (ulist : Ulist<_>) (fslist : _ list) = List.ofSeq ulist = fslist
        let pred i = i % 2 = 0
        eqSetsAfterSteps initialList testList actions Ulist.add Ulist.get Ulist.set ((+) 1) Ulist.map pred Ulist.filter eq

