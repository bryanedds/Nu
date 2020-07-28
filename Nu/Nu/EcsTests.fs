// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module EcsTests =

    type [<NoEquality; NoComparison; Struct>] private Airship =
        { mutable RefCount : int
          Transform : Transform ComponentRef }
        interface Airship Junction with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.Junction junctions entityId _ ecs = { RefCount = 0; Transform = ecs.Junction<Transform> junctions entityId }
            member this.Disjunction junctions entityId _ ecs = ecs.Disjunction<Transform> junctions entityId

    let example (ecs : World Ecs) =
        let system = SystemJunctioned<Airship, World> [|typeof<Transform>.Name|]
        do ecs.RegisterSystem "AirshipSystem" system
        let subId = ecs.Subscribe "Update" (fun _ _ _ world ->
            for i = 0 to system.Components.Length - 1 do
                let comp = &system.Components.[i]
                let transform = &comp.Transform.Value
                transform.Enabled <- false
                transform.Absolute <- false
            world)
        ecs.Unsubscribe "Update" subId