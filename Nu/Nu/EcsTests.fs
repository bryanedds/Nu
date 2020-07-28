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
            member this.Junction systems entityId ecs =
                { RefCount = 0
                  Transform = ecs.Junction systems.[0] entityId
                  Skin = ecs.Junction systems.[1] entityId }
            member this.Disjunction systems entityId ecs =
                ecs.Disjunction<Transform> systems.[0] entityId
                ecs.Disjunction<Skin> systems.[1] entityId

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
            for i = 0 to airshipSystem.Components.Length - 1 do
                let comp = &airshipSystem.Components.[i]
                comp.Transform.Index.Enabled <- i % 2 = 0
                comp.Skin.Index.Color.Z <- 0.5f
            world)

        // create and register our airship
        let airshipId = ecs.RegisterJunctioned airshipSystem.Name Unchecked.defaultof<Airship> Gen.id

        // change some airship properties
        let airship = ecs.IndexJunctioned<Airship> airshipSystem.Name airshipId
        airship.Transform.Index.Position.X <- 0.5f
        airship.Transform.Index.Visible <- false

        // for non-junctioned entities, you can construct and use a much slower correlated reference
        let airshipRef = ecs.GetCorrelatedRef airshipId
        do airshipRef.Index<Transform>().Position.Y <- 5.0f
        do airshipRef.Index<Transform>().Enabled <- false

        // invoke update behavior
        ecs.Publish EcsEvents.Update () ecs.GlobalSystem world