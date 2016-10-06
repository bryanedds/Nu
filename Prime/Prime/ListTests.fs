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

    let cloneAdd v (vs : ResizeArray<_>) =
        seq {
            for x in vs do yield x
            yield v
        } |> ResizeArray

    let cloneMap f (vs : ResizeArray<_>) = Seq.map f vs |> ResizeArray

    let cloneFilter f (vs : ResizeArray<_>) = Seq.filter f vs |> ResizeArray

    let cloneSet (idx : int) (v : 'v) (vs : ResizeArray<_>) =
        let vs2 = ResizeArray(vs.Capacity)
        vs2.AddRange(vs)
        vs2.[idx] <- v
        vs2

    /// Keeps a reference to all persistent collections returned after
    /// performing actions, and after they are all applied, checks
    /// that they equal what we would get from FSharp.Core.List
    let eqListsAfterSteps
        (list : 'v ResizeArray)
        (testlist : 'l)
        (actions : ListAction<'v> array)
        (addLast : 'v->'l->'l)
        (getNth : int->'l->'v)
        (setNth : int->'v->'l->'l)
        (incrf : 'v->'v)
        (mapf : ('v->'v)->'l->'l)
        (pred: 'v->bool)
        (filter: ('v->bool)->'l->'l)
        (eq : 'l->'v ResizeArray->bool)
        (lookBackwards : bool) =

        let applyAction (list:'v ResizeArray) testlist action =
            match action with
            | ListAction.AddLast v ->
                (cloneAdd v list, addLast v testlist)
            | ListAction.MapIncrementFn ->
                (cloneMap incrf list, mapf incrf testlist)
            | ListAction.FilterWithFn ->
                (cloneFilter pred list, filter pred testlist)
            | ListAction.SetNthToNth(n1, n2) ->
                let len = list.Count
                if len > 0 then
                    let idx1, idx2 = Math.Abs(n1) % len, Math.Abs(n2) % len
                    let newlist = 
                        let v2 = Seq.item idx2 list
                        cloneSet idx1 v2 list
                    let test =
                        let v2 = getNth idx2 testlist
                        setNth idx1 v2 testlist
                    (newlist, test)
                else
                    (list, testlist)

        let (lists, testlists) =
            Array.fold
                (fun acc action ->
                    match acc with
                    | (fsmap :: fsmaps, testMap :: testMaps) ->
                        let (newF, newT) = applyAction fsmap testMap action
                        (newF :: fsmap :: fsmaps, newT :: testMap :: testMaps)
                    | _ -> failwithumf ())
                ([list], [testlist])
                actions

        let success = 
            if lookBackwards then
                List.forall2 eq testlists lists
            else
                eq (List.head testlists) (List.head lists)

        if not success then
            Trace.WriteLine "FAILURE:"
            List.iteri2 (fun i list testlist  ->
                if i > 0 then Trace.WriteLine (sprintf "After action %A" actions.[i-1])
                Trace.WriteLine (sprintf "list: %A\ntestlist: %A" list testlist))
                (List.rev lists)
                (List.rev testlists)
        success

    open FsCheck

    let getActionGen pred =
        Gen.filter pred Arb.generate<ListAction<int>>
        |> Gen.arrayOf
        |> Arb.fromGen

    [<Property>]
    /// Proof of concept, we can delete this after we know test is correct
    let aryEqListsLookingBackwards (initialList : ResizeArray<int>) (actions : ListAction<int> []) =
        let ary = Array.ofSeq initialList
        let eq (ary: int[]) (fslist: int ResizeArray) = List.ofSeq ary = List.ofSeq fslist
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

        eqListsAfterSteps initialList ary actions add get set ((+) 1) Array.map pred Array.filter eq true

    let ulistEqLists (initialList : ResizeArray<int>) (actions : ListAction<int>[]) (lookBackwards : bool) =
        let testList = Ulist.addMany initialList (Ulist.makeEmpty(None))
        let eq (ulist : Ulist<_>) (fslist : _ ResizeArray) = List.ofSeq ulist = List.ofSeq fslist
        let pred i = i % 2 = 0
        eqListsAfterSteps initialList testList actions Ulist.add Ulist.get Ulist.set ((+) 1) Ulist.map pred Ulist.filter eq lookBackwards 

    [<Property>]
    let ulistEqList (initialList : ResizeArray<int>) (actions : ListAction<int>[]) =
        ulistEqLists initialList actions false 

    [<Property>]
    let ulistEqListsLookingBackwards (initialList : ResizeArray<int>) =
        let actionGen = getActionGen (fun _ -> true)
        Prop.forAll actionGen (fun actions ->
            ulistEqLists initialList actions true)

    [<Property>]
    let ulistEqListsLookingBackwardsAddOnly (initialList : ResizeArray<int>) =
        let actionGen = getActionGen (function
            | ListAction.AddLast(_) -> true
            | _ -> false)

        Prop.forAll actionGen (fun actions ->
            ulistEqLists initialList actions true)

    [<Property>]
    let ulistEqListsLookingBackwardsAddSet (initialList : ResizeArray<int>) =
        let actionGen = getActionGen (function
            | ListAction.SetNthToNth(_,_)
            | ListAction.AddLast(_) -> true
            | _ -> false)

        Prop.forAll actionGen (fun actions ->
            ulistEqLists initialList actions true)

    [<Property>]
    let ulistEqListsLookingBackwardsMapFilter (initialList : ResizeArray<int>) =
        let actionGen = getActionGen (function
            | ListAction.MapIncrementFn(_)
            | ListAction.FilterWithFn -> true
            | _ -> false)

        Prop.forAll actionGen (fun actions ->
            ulistEqLists initialList actions true)