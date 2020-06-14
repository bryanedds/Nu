// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime

type System =
    abstract Update : unit -> obj

type 't SystemOne =
    inherit System
    abstract GetComponent : unit -> 't

type 't SystemMany =
    inherit System
    abstract GetComponent : int -> 't byref
    abstract GetComponents : unit -> 't array
    abstract AddComponent : 't -> int
    abstract RemoveComponent : int -> int

type 't SystemStreamed =
    inherit System
    abstract GetComponent : int -> 't byref
    abstract AddComponents : byte array -> int array
    abstract ClearComponents : unit -> unit

type [<NoEquality; NoComparison>] VanillaSystem =
    private
        { mutable Components : Transform array
          mutable FreeIndex : int }

    interface Transform SystemMany with

        member this.Update () =
            () :> obj // vanilla components have no ECS update

        member this.GetComponent index =
            if index >= this.FreeIndex then raise (ArgumentOutOfRangeException "index")
            &this.Components.[index]

        member this.GetComponents () =
            let components = Array.zeroCreate this.FreeIndex
            this.Components.CopyTo (components, 0)
            components

        member this.AddComponent comp =
            if this.FreeIndex < this.Components.Length - 1 then
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
                this.FreeIndex - 1
            else
                let arr = Array.zeroCreate (this.Components.Length * 2)
                this.Components.CopyTo (arr, 0)
                this.Components <- arr
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
                this.FreeIndex - 1

        member this.RemoveComponent index =
            if index <> this.FreeIndex then
                let last = this.Components.[this.Components.Length - 1]
                this.Components.[index] <- last
            this.FreeIndex <- dec this.FreeIndex
            index

type [<NoEquality; NoComparison>] 't RotationSystem =
    private
        { mutable Components : Transform array
          mutable FreeIndex : int
          mutable Rotation : single }

    interface Transform SystemMany with

        member this.Update () =
            let count = this.Components.Length
            let mutable i = 0
            while i < count do
                let transform = &this.Components.[i]
                transform.Rotation <- transform.Rotation + this.Rotation
                i <- inc i
            () :> obj

        member this.GetComponent index =
            if index >= this.FreeIndex then raise (ArgumentOutOfRangeException "index")
            &this.Components.[index]

        member this.GetComponents () =
            let components = Array.zeroCreate this.FreeIndex
            this.Components.CopyTo (components, 0)
            components

        member this.AddComponent comp =
            if this.FreeIndex < this.Components.Length - 1 then
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
                this.FreeIndex - 1
            else
                let arr = Array.zeroCreate (this.Components.Length * 2)
                this.Components.CopyTo (arr, 0)
                this.Components <- arr
                this.Components.[this.FreeIndex] <- comp
                this.FreeIndex <- inc this.FreeIndex
                this.FreeIndex - 1

        member this.RemoveComponent index =
            if index <> this.FreeIndex then
                let last = this.Components.[this.Components.Length - 1]
                this.Components.[index] <- last
            this.FreeIndex <- dec this.FreeIndex
            index

type [<NoEquality; NoComparison>] Systems =
    { Systems : Dictionary<string, System> }

    static member update systems =
        Seq.map
            (fun (system : KeyValuePair<string, System>) -> system.Value.Update ())
            systems.Systems

    static member add name system systems =
        systems.Systems.Add (name, system)

    static member make () =
        let vanilla = { Components = [||]; FreeIndex = 0 }
        let rotation = { Components = [||]; FreeIndex = 0; Rotation = 0.01f }
        { Systems = dictPlus [("Vanilla", vanilla :> System); ("Rotation", rotation :> System)] }