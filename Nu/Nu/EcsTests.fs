// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
module EcsTests =

    type [<NoEquality; NoComparison; Struct>] Skin =
        { mutable Active : bool
          mutable Color : Color }
        interface Skin Component with
            member this.TypeName = nameof Skin
            member this.Active with get () = this.Active and set value = this.Active <- value

    type [<NoEquality; NoComparison; Struct>] Airship =
        { mutable Active : bool
          Transform : Transform ComponentRef
          Skin : Skin ComponentRef }
        interface Airship Component with
            member this.TypeName = nameof Airship
            member this.Active with get () = this.Active and set value = this.Active <- value

    type [<NoEquality; NoComparison; Struct>] Node =
        { mutable Active : bool
          Transform : Transform }
        interface Node Component with
            member this.TypeName = nameof Node
            member this.Active with get () = this.Active and set value = this.Active <- value

    type [<NoEquality; NoComparison; Struct>] Prop =
        { mutable Active : bool
          Node : Node ComponentRef
          NodeId : Guid }
        interface Prop Component with
            member this.TypeName = nameof Prop
            member this.Active with get () = this.Active and set value = this.Active <- value

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> ()

        // create and register our transform system
        let _ = ecs.RegisterSystem (SystemCorrelated<Transform, World> ecs)

        // create and register our skin system
        let _ = ecs.RegisterSystem (SystemCorrelated<Skin, World> ecs)

        // create and register our airship system
        let _ = ecs.RegisterSystem (SystemCorrelated<Airship, World> ecs)

        // define our airship system's update behavior
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            for components in ecs.GetComponentArrays<Airship> () do
                for i in 0 .. components.Length - 1 do
                    let mutable comp = &components.[i]
                    if  comp.Active then
                        comp.Transform.Index.Enabled <- i % 2 = 0
                        comp.Skin.Index.Color.A <- byte 128

        // create and register our airship
        let airshipId = Gen.id64
        let airship = ecs.RegisterCorrelated false Unchecked.defaultof<Airship> airshipId

        // change some airship properties
        airship.Index.Transform.Index.Position.X <- 0.5f
        airship.Index.Skin.Index.Color.R <- byte 16

        // for non-junctioned entities, you can alternatively construct and use a much slower entity reference
        //let airshipRef = ecs.GetEntityRef ...
        //airshipRef.Index<Transform>().Position.Y <- 5.0f
        //airshipRef.Index<Skin>().Color.G <- byte 255

        // invoke update behavior
        ecs.Publish EcsEvents.Update () ecs.SystemGlobal world