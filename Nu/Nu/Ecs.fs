// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type Component =
    abstract Occupied : bool with get, set

type [<AbstractClass>] 'w System () =
    abstract Update : 'w Ecs -> obj
    abstract PostUpdate : 'w Ecs -> obj
    abstract Actualize : 'w Ecs -> obj

and [<AbstractClass>] SystemSingleton<'t, 'w when 't : struct and 't :> Component> (comp : 't) =
    inherit System<'w> ()
    let mutable comp = comp
    member this.GetComponent () = &comp

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
            components.[index].Occupied <- false
            freeList.Enqueue index
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

    member this.GetComponent entityId =
        let (found, index) = correlations.TryGetValue entityId
        if not found then raise (InvalidOperationException "entityId")
        &components.[index]

    member this.AddComponent entityId comp =
        if not (correlations.ContainsKey entityId) then
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
            else components.[freeList.Dequeue ()] <- comp
            true
        else false

    member this.RemoveComponent entityId =
        match correlations.TryGetValue entityId with
        | (true, index) ->
            if index <> freeIndex then
                components.[index].Occupied <- false
                freeList.Enqueue index
            else freeIndex <- dec freeIndex
            true
        | (false, _) -> false

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

    let postUpdate ecs =
        ecs.Systems |>
        Seq.map (fun (system : KeyValuePair<string, 'w System>) -> (system.Key, system.Value.PostUpdate ecs)) |>
        dictPlus

    let actualize ecs =
        ecs.Systems |>
        Seq.map (fun (system : KeyValuePair<string, 'w System>) -> (system.Key, system.Value.Actualize ecs)) |>
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
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemUncorrelated<'t, 'w> as systemUnc -> systemUnc.GetComponent index
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

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
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemCorrelated<'t, 'w> as systemCorr -> systemCorr.GetComponent entityId
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let addComponentCorrelated<'t, 'w when 't : struct and 't :> Component> systemName entityId (comp : 't) ecs =
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemCorrelated<'t, 'w> as systemCorr ->
                if systemCorr.AddComponent entityId comp then
                    match ecs.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> ecs.Correlations.Add (entityId, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let removeComponentCorrelated<'t, 'w when 't : struct and 't :> Component> systemName entityId ecs =
        match tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? SystemCorrelated<'t, 'w> as systemCorr ->
                if systemCorr.RemoveComponent entityId then
                    match ecs.Correlations.TryGetValue entityId with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> ecs.Correlations.Add (entityId, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of required type.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    let correlateComponent entityId ecs =
        ecs.Correlations.[entityId] |>
        Seq.map (fun systemName -> (systemName, getSystem systemName ecs)) |>
        dictPlus

    let make () =
        { Systems = dictPlus []
          Correlations = dictPlus [] }

type [<NoEquality; NoComparison>] ComponentSource =
    | ComponentSimulant of Simulant
    | ComponentUncorrelated of string * int
    | ComponentCorrelated of string * Guid

type SystemMapper =
    interface end

type [<NoEquality; NoComparison>] SystemMapper<'i, 'd, 'w when 'i :> Component and 'd :> Component> =
    { SourcesToIntersection : ComponentSource array -> 'i
      IntersectionToDestination : 'i -> 'w -> 'd
      SystemSourceNames : string array
      SystemIntersectionName : string
      SystemDestinationName : string }
    interface SystemMapper

type [<NoEquality; NoComparison>] SystemMappers =
  { Mappers : SystemMapper array }