// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module EcsTests =

    type [<Struct>] Skin =
        { mutable RefCount : int
          mutable Color : Vector4 }
        interface Component with
            member this.RefCount
                with get () = this.RefCount and set value = this.RefCount <- value

    type [<NoEquality; NoComparison; Struct>] Airship =
        { mutable RefCount : int
          Transform : Transform ComponentRef
          Skin : Skin ComponentRef }
        interface Airship Junction with
            member this.RefCount
                with get () = this.RefCount and set value = this.RefCount <- value
            member this.Junction junctions entityId ecs =
                { RefCount = 0
                  Transform = ecs.Junction junctions entityId
                  Skin = ecs.Junction junctions entityId }
            member this.Disjunction junctions entityId ecs =
                do ecs.Disjunction<Transform> junctions entityId
                do ecs.Disjunction<Skin> junctions entityId

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> (System<World> ())

        // create and register our transform system
        let transformSystem = ecs.RegisterSystem (SystemCorrelated<Transform, World> ())

        // create and register our skin system
        let skinSystem = ecs.RegisterSystem (SystemCorrelated<Skin, World> ())

        // create and register our airship system
        let airshipSystem = ecs.RegisterSystem (SystemJunctioned<Airship, World> [|transformSystem.Name; skinSystem.Name|])

        // define our airship system's update behavior
        let subId = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            for i = 0 to airshipSystem.Components.Length - 1 do
                let comp = &airshipSystem.Components.[i]
                comp.Transform.Value.Enabled <- i % 2 = 0
                comp.Skin.Value.Color.Z <- 0.5f
            world)

        // create and register our airship
        let airship = ecs.RegisterJunctioned airshipSystem.Name Unchecked.defaultof<Airship> Gen.id

        // invoke update behavior
        let world = ecs.Publish EcsEvents.Update () ecs.GlobalSystem world

        // fin
        world