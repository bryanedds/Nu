// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type Component =
    abstract Occupied : bool with get, set

type [<AbstractClass>] System () =
    abstract Update : Ecs -> obj
    abstract PostUpdate : Ecs -> obj

and [<AbstractClass>] SystemSingleton<'t when 't : struct and 't :> Component> (comp : 't) =
    inherit System ()
    let mutable comp = comp
    member this.GetComponent () = &comp

and [<AbstractClass>] SystemUncorrelated<'t when 't : struct and 't :> Component> () =
    inherit System ()

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

and [<AbstractClass>] SystemCorrelated<'t when 't : struct and 't :> Component> () =
    inherit System ()

    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    let freeList = Queue<int> ()
    let correlations = dictPlus [] : Dictionary<Guid, int>
    
    member this.Components
        with get () = components
    
    member this.FreeIndex
        with get () = freeIndex

    member this.GetComponent guid =
        let (found, index) = correlations.TryGetValue guid
        if not found then raise (InvalidOperationException "guid")
        &components.[index]

    member this.AddComponent guid comp =
        if not (correlations.ContainsKey guid) then
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
                correlations.Add (guid, index)
            else components.[freeList.Dequeue ()] <- comp
            true
        else false

    member this.RemoveComponent guid =
        match correlations.TryGetValue guid with
        | (true, index) ->
            if index <> freeIndex then
                components.[index].Occupied <- false
                freeList.Enqueue index
            else freeIndex <- dec freeIndex
            true
        | (false, _) -> false

/// NOTE: Uncorrelated systems can update in parallel.
and [<NoEquality; NoComparison>] Ecs =
    { Systems : Dictionary<string, System>
      Correlations : Dictionary<Guid, string List> }

    static member update ecs =
        ecs.Systems |>
        Seq.map (fun (system : KeyValuePair<string, System>) -> (system.Key, system.Value.Update ecs)) |>
        dictPlus

    static member postUpdate ecs =
        ecs.Systems |>
        Seq.map (fun (system : KeyValuePair<string, System>) -> (system.Key, system.Value.PostUpdate ecs)) |>
        dictPlus

    static member addSystem systemName system ecs =
        ecs.Systems.Add (systemName, system)

    static member removeSystem systemName ecs =
        ecs.Systems.Remove systemName |> ignore

    static member tryGetSystem systemName ecs =
        match ecs.Systems.TryGetValue systemName with
        | (true, system) -> Some system
        | (false, _) -> None

    static member getSystem systemName ecs =
        Ecs.tryGetSystem systemName ecs |> Option.get

    static member getComponent<'t when 't : struct and 't :> Component> systemName index ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemUncorrelated) as systemUnc -> systemUnc.GetComponent index
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemUncorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member addComponent<'t when 't : struct and 't :> Component> systemName (comp : 't) ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemUncorrelated) as systemUnc -> systemUnc.AddComponent comp
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemUncorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member removeComponent<'t when 't : struct and 't :> Component> systemName index ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemUncorrelated) as systemUnc -> systemUnc.RemoveComponent index
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemUncorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member getComponentCorrelated<'t when 't : struct and 't :> Component> systemName guid ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemCorrelated) as systemCorr -> systemCorr.GetComponent guid
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemCorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member addComponentCorrelated<'t when 't : struct and 't :> Component> systemName guid (comp : 't) ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemCorrelated) as systemCorr ->
                if systemCorr.AddComponent guid comp then
                    match ecs.Correlations.TryGetValue guid with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> ecs.Correlations.Add (guid, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemCorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member removeComponentCorrelated<'t when 't : struct and 't :> Component> systemName guid ecs =
        match Ecs.tryGetSystem systemName ecs with
        | Some system ->
            match system with
            | :? ('t SystemCorrelated) as systemCorr ->
                if systemCorr.RemoveComponent guid then
                    match ecs.Correlations.TryGetValue guid with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> ecs.Correlations.Add (guid, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemCorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member correlateComponent guid ecs =
        ecs.Correlations.[guid] |>
        Seq.map (fun systemName -> (systemName, Ecs.getSystem systemName ecs)) |>
        dictPlus

    static member make () =
        { Systems = dictPlus []
          Correlations = dictPlus [] }