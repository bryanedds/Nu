// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type Component =
    abstract RefCount : int with get, set

type [<Struct>] ComponentRef<'t when 't : struct and 't :> Component> =
    { ComponentIndex : int
      ComponentArr : 't array }

    static member (<!) (componentRef, value) =
        let mutable ref = &componentRef.ComponentArr.[componentRef.ComponentIndex]
        ref <- value

    static member (!>) componentRef =
        &componentRef.ComponentArr.[componentRef.ComponentIndex]

    static member make index arr =
        { ComponentIndex = index
          ComponentArr = arr }

type [<AbstractClass>] 'w System () =
    abstract Update : 'w Ecs -> obj

and [<AbstractClass>] SystemGeneric<'t, 'w> () =
    inherit System<'w> ()
    abstract Components : 't array

and [<AbstractClass>] SystemSingleton<'t, 'w when 't : struct and 't :> Component> (comp : 't) =
    inherit SystemGeneric<'t, 'w> ()
    let mutable comp = [|comp|]
    override this.Components with get () = comp
    member this.Component with get () = &comp.[0]

and [<AbstractClass>] SystemUncorrelated<'t, 'w when 't : struct and 't :> Component> () =
    inherit System<'w> ()

    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    let freeList = Queue<int> ()

    member this.Components
        with get () = components
    
    member this.FreeIndex
        with get () = freeIndex

    member this.GetComponent index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        &components.[index]

    member this.AddComponent comp =
        if freeList.Count = 0 then
            if freeIndex < components.Length - 1 then
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
            else
                let arr = Array.zeroCreate (components.Length * 2)
                components.CopyTo (arr, 0)
                components <- arr
                components.[freeIndex] <- comp
                freeIndex <- inc freeIndex
        else components.[freeList.Dequeue ()] <- comp

    member this.RemoveComponent index =
        if index <> freeIndex then
            components.[index].RefCount <- dec components.[index].RefCount
            if components.[index].RefCount = 0 then freeList.Enqueue index
        else freeIndex <- dec freeIndex

and [<AbstractClass>] SystemCorrelated<'t, 'w when 't : struct and 't :> Component> () =
    inherit System<'w> ()

    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    let freeList = Queue<int> ()
    let correlations = dictPlus [] : Dictionary<Guid, int>
    
    member this.Components
        with get () = components
    
    member this.FreeIndex
        with get () = freeIndex

    member this.GetComponentIndex entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (InvalidOperationException "entityId")
        index

    member this.GetComponent entityId =
        let index = this.GetComponentIndex entityId
        &components.[index]

    member this.Register entityId =
        match Dictionary.tryGetValue entityId correlations with
        | (false, _) ->
            let comp = Unchecked.defaultof<'t>
            if freeList.Count = 0 then
                if freeIndex < components.Length - 1 then
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                else
                    let arr = Array.zeroCreate (components.Length * 2)
                    components.CopyTo (arr, 0)
                    components <- arr
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                let index = dec freeIndex
                correlations.Add (entityId, index)
                index
            else
                let index = freeList.Dequeue ()
                components.[index] <- comp
                index
        | (true, index) ->
            let mutable comp = components.[index]
            comp.RefCount <- inc comp.RefCount
            index

    member this.Unregister entityId =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            if index <> freeIndex then
                components.[index].RefCount <- dec components.[index].RefCount
                if components.[index].RefCount = 0 then freeList.Enqueue index
            else freeIndex <- dec freeIndex
            true
        | (false, _) -> false

and [<AbstractClass>] SystemIntersection<'t, 'w when 't : struct and 't :> Component>
    (correlatedSystemNames : string array) =
    inherit System<'w> ()

    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    let mutable intersectionsOpt = None : Dictionary<string, 'w System> option
    let freeList = Queue<int> ()
    let correlations = dictPlus [] : Dictionary<Guid, int>

    member this.Components
        with get () = components

    member this.FreeIndex
        with get () = freeIndex

    member this.GetIntersections getSystem (ecs : 'w Ecs) =
        match intersectionsOpt with
        | Some intersections -> intersections
        | None ->
            let intersections = correlatedSystemNames |> Array.map (fun sourceName -> (sourceName, getSystem sourceName ecs)) |> dictPlus
            intersectionsOpt <- Some intersections
            intersections

    member this.GetComponent entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (InvalidOperationException "entityId")
        &components.[index]

    member this.Register entityId getSystem ecs =
        match Dictionary.tryGetValue entityId correlations with
        | (false, _) ->
            let intersections = this.GetIntersections getSystem ecs
            let comp = this.Intersect intersections entityId ecs
            if freeList.Count = 0 then
                if freeIndex < components.Length - 1 then
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                else
                    let arr = Array.zeroCreate (components.Length * 2)
                    components.CopyTo (arr, 0)
                    components <- arr
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                let index = dec freeIndex
                correlations.Add (entityId, index)
                index
            else
                let index = freeList.Dequeue ()
                components.[index] <- comp
                index
        | (true, index) ->
            let mutable comp = components.[index]
            comp.RefCount <- inc comp.RefCount
            index

    member this.Unregister entityId =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            if index <> freeIndex then
                components.[index].RefCount <- dec components.[index].RefCount
                if components.[index].RefCount = 0 then freeList.Enqueue index
            else freeIndex <- dec freeIndex
            true
        | (false, _) -> false

    abstract Intersect : Dictionary<string, 'w System> -> Guid -> 'w Ecs -> 't
    abstract Separate : Dictionary<string, 'w System> -> Guid -> 'w Ecs -> unit

/// NOTE: Uncorrelated systems can update in parallel.
and [<NoEquality; NoComparison>] 'w Ecs =
    { Systems : Dictionary<string, 'w System>
      Correlations : Dictionary<Guid, string List> }

[<RequireQualifiedAccess>]
module Ecs =

    let update ecs =
        ecs.Systems |>
        Seq.map (fun (system : KeyValuePair<string, 'w System>) -> (system.Key, system.Value.Update ecs)) |>
        dictPlus

    let addSystem systemName system ecs =
        ecs.Systems.Add (systemName, system)

    let removeSystem systemName ecs =
        ecs.Systems.Remove systemName |> ignore

    let tryGetSystem systemName ecs =
        match ecs.Systems.TryGetValue systemName with
        | (true, system) -> Some system
        | (false, _) -> None

    let getSystem systemName ecs =
        tryGetSystem systemName ecs |> Option.get

    let getComponent<'t, 'w when 't : struct and 't :> Component> systemName index ecs =
        let systemOpt = tryGetSystem systemName ecs
        if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
        let system = Option.get systemOpt
        if not (system :? SystemUncorrelated<'t, 'w>) then failwith ("Could not find expected system '" + systemName + "' of required type.")
        let systemUnc = system :?> SystemUncorrelated<'t, 'w>
        &systemUnc.GetComponent index

    let addComponent<'t, 'w when 't : struct and 't :> Component> systemName (comp : 't) ecs =
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemUncorrelated<'t, 'w> as systemUnc -> systemUnc.AddComponent comp
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let removeComponent<'t, 'w when 't : struct and 't :> Component> systemName index ecs =
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemUncorrelated<'t, 'w> as systemUnc -> systemUnc.RemoveComponent index
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let getComponentCorrelated<'t, 'w when 't : struct and 't :> Component> systemName entityId ecs =
        let systemOpt = tryGetSystem systemName ecs
        if Option.isNone systemOpt then failwith ("Could not find expected system '" + systemName + "'.")
        let system = Option.get systemOpt
        if not (system :? SystemCorrelated<'t, 'w>) then failwith ("Could not find expected system '" + systemName + "' of required type.")
        let systemCorr = system :?> SystemCorrelated<'t, 'w>
        &systemCorr.GetComponent entityId

    let registerEntity<'t, 'w when 't : struct and 't :> Component> systemName entityId ecs =
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemCorrelated<'t, 'w> as systemCorr ->
                let _ = systemCorr.Register entityId
                match ecs.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> ecs.Correlations.Add (entityId, List [systemName])
            | :? SystemIntersection<'t, 'w> as systemInter ->
                let _ = systemInter.Register entityId getSystem ecs
                match ecs.Correlations.TryGetValue entityId with
                | (true, correlation) -> correlation.Add systemName
                | (false, _) -> ecs.Correlations.Add (entityId, List [systemName])
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | None -> failwith ("Could not find expected system '" + systemName + "'.")

    //let unregisterEntity systemName entityId ecs =
    //    match tryGetSystem systemName ecs with
    //    | Some system ->
    //        match system with
    //        | :? SystemCorrelated<'t, 'w> as systemCorr ->
    //            if systemCorr.Unregister entityId then
    //                match ecs.Correlations.TryGetValue entityId with
    //                | (true, correlation) -> correlation.Add systemName
    //                | (false, _) -> ecs.Correlations.Add (entityId, List [systemName])
    //            else ()
    //        | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
    //    | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let correlateEntity entityId ecs =
        ecs.Correlations.[entityId] |>
        Seq.map (fun systemName -> (systemName, getSystem systemName ecs)) |>
        dictPlus

    let make () =
        { Systems = dictPlus []
          Correlations = dictPlus [] }

[<AutoOpen>]
module EcsOperators =

    let intersect<'t, 'w when 't : struct and 't :> Component>
        (intersections : Dictionary<string, 'w System>)
        (entityId : Guid) =
        let system = intersections.[typeof<'t>.Name] :?> SystemCorrelated<'t, 'w>
        let index = system.Register entityId
        ComponentRef<'t>.make index system.Components

    let separate<'t, 'w when 't : struct and 't :> Component>
        (intersections : Dictionary<string, 'w System>)
        (entityId : Guid) =
        let system = intersections.[typeof<'t>.Name] :?> SystemCorrelated<'t, 'w>
        system.Unregister entityId |> ignore