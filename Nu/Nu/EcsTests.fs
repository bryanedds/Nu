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

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> (System<World> ())

        // create and register our system
        let system = ecs.RegisterSystem "AirshipSystem" (SystemJunctioned<Airship, World> [|"Transform"|])

        // create and register our entity with the system
        let entity = ecs.RegisterJunctioned "AirshipSystem" Unchecked.defaultof<Airship> Gen.id

        // define our system's update behavior
        let subId = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            for i = 0 to system.Components.Length - 1 do
                let comp = &system.Components.[i]
                let transform = &comp.Transform.Value
                transform.Enabled <- not transform.Enabled
            world)

        // invoke update behavior
        let world = ecs.Publish EcsEvents.Update () ecs.GlobalSystem world

        // fin
        world