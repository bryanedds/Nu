// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type [<AbstractClass>] System () =
    abstract Update : Systems -> obj

and [<AbstractClass>] SystemSingleton<'t when 't : struct> (comp : 't) =
    inherit System ()
    let mutable comp = comp
    member this.GetComponent () = &comp

and [<AbstractClass>] SystemUncorrelated<'t when 't : struct> () =
    inherit System ()

    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    member this.Components with get () = components
    member this.FreeIndex with get () = freeIndex

    member this.GetComponent index =
        if index >= freeIndex then raise (ArgumentOutOfRangeException "index")
        &components.[index]

    member this.AddComponent comp =
        if freeIndex < components.Length - 1 then
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex
        else
            let arr = Array.zeroCreate (components.Length * 2)
            components.CopyTo (arr, 0)
            components <- arr
            components.[freeIndex] <- comp
            freeIndex <- inc freeIndex
        freeIndex - 1

    member this.RemoveComponent index =
        if index <> freeIndex then
            let last = components.[components.Length - 1]
            components.[index] <- last
        freeIndex <- dec freeIndex
        index

and [<AbstractClass>] SystemCorrelated<'t when 't : struct> () =
    inherit System ()

    let correlations = dictPlus [] : Dictionary<Guid, int>
    let mutable components = [||] : 't array
    let mutable freeIndex = 0
    member this.Components with get () = components
    member this.FreeIndex with get () = freeIndex

    member this.GetComponent guid =
        let (found, index) = correlations.TryGetValue guid
        if not found then raise (InvalidOperationException "guid")
        &components.[index]

    member this.AddComponent guid comp =
        if not (correlations.ContainsKey guid) then
            let index =
                if freeIndex < components.Length - 1 then
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                else
                    let arr = Array.zeroCreate (components.Length * 2)
                    components.CopyTo (arr, 0)
                    components <- arr
                    components.[freeIndex] <- comp
                    freeIndex <- inc freeIndex
                freeIndex - 1
            correlations.Add (guid, index)
            true
        else false

    member this.RemoveComponent guid =
        match correlations.TryGetValue guid with
        | (true, index) ->
            if index <> freeIndex then
                let last = components.[components.Length - 1]
                components.[index] <- last
            freeIndex <- dec freeIndex
            true
        | (false, _) -> false

and VanillaSystem () =
    inherit SystemCorrelated<Transform> ()
    override this.Update _ = () :> obj // vanilla components have no ECS update

and RotationSystem (rotation) =
    inherit SystemUncorrelated<Transform> ()
    override this.Update _ =
        let count = this.Components.Length
        let mutable i = 0
        while i < count do
            let transform = &this.Components.[i]
            transform.Rotation <- transform.Rotation + rotation
            i <- inc i
        () :> obj

and [<NoEquality; NoComparison>] Systems =
    { Systems : Dictionary<string, System>
      Correlations : Dictionary<Guid, string List> }

    static member update systems =
        Seq.map
            (fun (system : KeyValuePair<string, System>) -> system.Value.Update systems)
            systems.Systems

    static member addSystem systemName system systems =
        systems.Systems.Add (systemName, system)

    static member removeSystem systemName systems =
        systems.Systems.Remove systemName |> ignore

    static member tryGetSystem systemName systems =
        match systems.Systems.TryGetValue systemName with
        | (true, system) -> Some system
        | (false, _) -> None

    static member getSystem systemName systems =
        Systems.tryGetSystem systemName systems |> Option.get

    static member addComponent<'t when 't : struct> systemName (comp : 't) systems =
        match Systems.tryGetSystem systemName systems with
        | Some system ->
            match system with
            | :? ('t SystemUncorrelated) as systemUnc -> systemUnc.AddComponent comp
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemUncorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member removeComponent<'t when 't : struct> systemName index systems =
        match Systems.tryGetSystem systemName systems with
        | Some system ->
            match system with
            | :? ('t SystemUncorrelated) as systemUnc -> systemUnc.RemoveComponent index
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemUncorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member addComponentCorrelated<'t when 't : struct> systemName guid (comp : 't) systems =
        match Systems.tryGetSystem systemName systems with
        | Some system ->
            match system with
            | :? ('t SystemCorrelated) as systemCorr ->
                if systemCorr.AddComponent guid comp then
                    match systems.Correlations.TryGetValue guid with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> systems.Correlations.Add (guid, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemCorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member removeComponentCorrelated<'t when 't : struct> systemName guid systems =
        match Systems.tryGetSystem systemName systems with
        | Some system ->
            match system with
            | :? ('t SystemCorrelated) as systemCorr ->
                if systemCorr.RemoveComponent guid then
                    match systems.Correlations.TryGetValue guid with
                    | (true, correlation) -> correlation.Add systemName
                    | (false, _) -> systems.Correlations.Add (guid, List [systemName])
                else ()
            | _ -> failwith ("Could not find expected system '" + systemName + "' of type '" + typeof<'t>.Name + " SystemCorrelated'.")
        | _ -> failwith ("Could not find expected system '" + systemName + "'.")

    static member correlateComponent guid systems =
        systems.Correlations.[guid] |>
        Seq.map (fun systemName -> Systems.getSystem systemName systems) |>
        Array.ofSeq

    static member make () =
        let systems = 
            [("Vanilla", VanillaSystem () :> System)
             ("Rotation", RotationSystem 0.01f :> System)]
        { Systems = dictPlus systems
          Correlations = dictPlus [] }