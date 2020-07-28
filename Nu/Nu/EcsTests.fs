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
            member this.Junction junctions entityId ecs = { RefCount = 0; Transform = ecs.Junction<Transform> junctions entityId }
            member this.Disjunction junctions entityId ecs = ecs.Disjunction<Transform> junctions entityId

    let example (ecs : World Ecs) (world : World) =

        // create and register our system
        let system = ecs.RegisterSystem "AirshipSystem" (SystemJunctioned<Airship, World> [|typeof<Transform>.Name|])

        // create and register our entity with the system
        let entity = ecs.RegisterJunctioned "AirshipSystem" Unchecked.defaultof<Airship> Gen.id

        // subscribe to the update event
        let subId = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            for i = 0 to system.Components.Length - 1 do
                let comp = &system.Components.[i]
                let transform = &comp.Transform.Value
                transform.Enabled <- not transform.Enabled
            world)

        // publish update event
        let world = ecs.Publish EcsEvents.Update () Unchecked.defaultof<_> world

        // unsubscribe to the update event
        let _ = ecs.Unsubscribe EcsEvents.Update subId

        // fin
        world