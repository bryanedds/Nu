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
        interface Skin Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.SystemNames = [||]
            member this.Junction _ _ _ = this
            member this.Disjunction _ _ _ = ()

    type [<NoEquality; NoComparison; Struct>] Airship =
        { mutable RefCount : int
          Transform : Transform ComponentRef
          Skin : Skin ComponentRef }
        interface Airship Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.SystemNames = [|"Transform"; "Skin"|]
            member this.Junction systems entityId ecs = { id this with Transform = ecs.Junction entityId systems.[0]; Skin = ecs.Junction entityId systems.[1] }
            member this.Disjunction systems entityId ecs = ecs.Disjunction<Transform> entityId systems.[0]; ecs.Disjunction<Skin> entityId systems.[1]

    type [<NoEquality; NoComparison; Struct>] Node =
        { mutable RefCount : int
          Transform : Transform }
        interface Node Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.SystemNames = [||]
            member this.Junction _ _ _ = this
            member this.Disjunction _ _ _ = ()

    type [<NoEquality; NoComparison; Struct>] Prop =
        { mutable RefCount : int
          Transform : Transform ComponentRef
          NodeId : Guid }
        interface Prop Component with
            member this.RefCount with get () = this.RefCount and set value = this.RefCount <- value
            member this.SystemNames = [|"Node"|]
            member this.Junction systems entityId ecs = { id this with Transform = ecs.JunctionHierarchical this.NodeId entityId systems.[0] }
            member this.Disjunction systems entityId ecs = ecs.DisjunctionHierarchical<Transform> this.NodeId entityId systems.[0]

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> ()

        // create and register our transform system
        let _ = ecs.RegisterSystem (SystemCorrelated<Transform, World> ())

        // create and register our skin system
        let _ = ecs.RegisterSystem (SystemCorrelated<Skin, World> ())

        // create and register our airship system
        let airshipSystem = ecs.RegisterSystem (SystemCorrelated<Airship, World> ())

        // define our airship system's update behavior
        let _ = ecs.Subscribe EcsEvents.Update (fun _ _ _ world ->
            let comps = airshipSystem.Components
            for i in 0 .. comps.Length do
                let comp = &comps.[i]
                if  comp.RefCount > 0 then
                    comp.Transform.Index.Enabled <- i % 2 = 0
                    comp.Skin.Index.Color.Z <- 0.5f
            world)

        // create and register our airship
        let airshipId = ecs.RegisterCorrelated Unchecked.defaultof<Airship> airshipSystem.Name Gen.id

        // change some airship properties
        let airship = ecs.IndexCorrelated<Airship> airshipSystem.Name airshipId
        airship.Index.Transform.Index.Position.X <- 0.5f
        airship.Index.Skin.Index.Color.X <- 0.1f

        // for non-junctioned entities, you can alternatively construct and use a much slower entity reference
        let airshipRef = ecs.GetEntityRef airshipId
        airshipRef.Index<Transform>().Position.Y <- 5.0f
        airshipRef.Index<Skin>().Color.Y <- 1.0f

        // invoke update behavior
        ecs.Publish EcsEvents.Update () ecs.GlobalSystem world