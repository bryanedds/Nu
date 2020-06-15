// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type [<AbstractClass>] System () =
    abstract Update : unit -> obj

type [<AbstractClass>] SystemOne<'t when 't : struct> (comp : 't) =
    inherit System ()
    let mutable comp = comp
    member this.GetComponent () = &comp

type [<AbstractClass>] SystemMany<'t when 't : struct> () =
    inherit System ()

    let mutable components = [||] : Transform array
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

type VanillaSystem () =
    inherit SystemMany<Transform> ()
    override this.Update () = () :> obj // vanilla components have no ECS update

type RotationSystem (rotation) =
    inherit SystemMany<Transform> ()
    override this.Update () =
        let count = this.Components.Length
        let mutable i = 0
        while i < count do
            let transform = &this.Components.[i]
            transform.Rotation <- transform.Rotation + rotation
            i <- inc i
        () :> obj

type [<NoEquality; NoComparison>] Systems =
    { Systems : Dictionary<string, System> }

    static member update systems =
        Seq.map
            (fun (system : KeyValuePair<string, System>) -> system.Value.Update ())
            systems.Systems

    static member add name system systems =
        systems.Systems.Add (name, system)

    static member make () =
        let systems = 
            [("Vanilla", VanillaSystem () :> System)
             ("Rotation", RotationSystem 0.01f :> System)]
        { Systems = dictPlus systems }