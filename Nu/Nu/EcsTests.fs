// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module EcsTests =

    type [<NoEquality; NoComparison; Struct>] Skin =
        { mutable RefCount : int
          mutable Color : Vector4 }
        interface Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value

    type [<NoEquality; NoComparison; Struct>] Airship =
        { mutable RefCount : int
          Transform : Transform ComponentRef
          Skin : Skin ComponentRef }
        interface Airship Junction with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.SystemNames = [|"Transform"; "Skin"|]
            member this.Junction systems registration ecs = { id this with Transform = ecs.Junction registration systems.[0]; Skin = ecs.Junction registration systems.[1] }
            member this.Disjunction systems entityId ecs = ecs.Disjunction<Transform> entityId systems.[0]; ecs.Disjunction<Skin> entityId systems.[1]

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> ()

        // create and register our transform system
        let transformSystem = ecs.RegisterSystem (SystemCorrelated<Transform, World> ())

        // create and register our skin system
        let skinSystem = ecs.RegisterSystem (SystemCorrelated<Skin, World> ())

        // create and register our airship system
        let airshipSystem = ecs.RegisterSystem (SystemJunctioned<Airship, World> ())

        // define our airship system's update behavior
        let subscriptionId = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            let (arr, last) = airshipSystem.Iter
            for i = 0 to last do
                let comp = &arr.[i]
                comp.Transform.Index.Enabled <- i % 2 = 0
                comp.Skin.Index.Color.Z <- 0.5f
            world)

        // create and register our airship
        let airshipId = ecs.RegisterJunctioned Unchecked.defaultof<Airship> airshipSystem.Name (Alloc Gen.id)

        // change some airship properties
        let airship = ecs.IndexJunctioned<Airship> airshipSystem.Name airshipId
        airship.Transform.Index.Position.X <- 0.5f
        airship.Skin.Index.Color.X <- 0.1f

        // for non-junctioned entities, you can alternatively construct and use a much slower entity reference
        let airshipRef = ecs.GetEntityRef airshipId
        airshipRef.Index<Transform>().Position.Y <- 5.0f
        airshipRef.Index<Skin>().Color.Y <- 1.0f

        // invoke update behavior
        ecs.Publish EcsEvents.Update () ecs.GlobalSystem world