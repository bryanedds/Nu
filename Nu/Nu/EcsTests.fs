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
            member this.Active with get () = this.Active and set value = this.Active <- value
            member this.AllocateJunctions _ = [||]
            member this.ResizeJunctions _ _ _ = ()
            member this.MoveJunctions _ _ _ _ = ()
            member this.Junction _ _ _ _ = this
            member this.Disjunction _ _ _ = ()

    type [<NoEquality; NoComparison; Struct>] Airship =
        { mutable Active : bool
          Transform : Transform ComponentRef
          Skin : Skin ComponentRef }
        interface Airship Component with
            member this.Active with get () = this.Active and set value = this.Active <- value
            member this.AllocateJunctions ecs = [|ecs.AllocateJunction<Transform> (); ecs.AllocateJunction<Skin> ()|]
            member this.ResizeJunctions size junctions ecs = ecs.ResizeJunction<Transform> size junctions.[0]; ecs.ResizeJunction<Skin> size junctions.[1]
            member this.MoveJunctions src dst junctions ecs = ecs.MoveJunction<Transform> src dst junctions.[0]; ecs.MoveJunction<Skin> src dst junctions.[1]
            member this.Junction index junctions buffereds ecs = { id this with Transform = ecs.Junction<Transform> index junctions.[0] buffereds.[0]; Skin = ecs.Junction<Skin> index junctions.[1] buffereds.[1] }
            member this.Disjunction index junctions ecs = ecs.Disjunction<Transform> index junctions.[0]; ecs.Disjunction<Skin> index junctions.[1]

    type [<NoEquality; NoComparison; Struct>] Node =
        { mutable Active : bool
          Transform : Transform }
        interface Node Component with
            member this.Active with get () = this.Active and set value = this.Active <- value
            member this.AllocateJunctions _ = [||]
            member this.ResizeJunctions _ _ _ = ()
            member this.MoveJunctions _ _ _ _ = ()
            member this.Junction _ _ _ _ = this
            member this.Disjunction _ _ _ = ()

    type [<NoEquality; NoComparison; Struct>] Prop =
        { mutable Active : bool
          Node : Node ComponentRef
          NodeId : Guid }
        interface Prop Component with
            member this.Active with get () = this.Active and set value = this.Active <- value
            member this.AllocateJunctions ecs = [|ecs.AllocateJunction<Node> ()|]
            member this.ResizeJunctions size junctions ecs = ecs.ResizeJunction<Node> size junctions.[0]
            member this.MoveJunctions src dst junctions ecs = ecs.MoveJunction<Node> src dst junctions.[0]
            member this.Junction index junctions buffereds ecs = { id this with Node = ecs.Junction<Node> index junctions.[0] buffereds.[0] }
            member this.Disjunction index junctions ecs = ecs.Disjunction<Node> index junctions.[0]

    let example (world : World) =

        // create our ecs
        let ecs = Ecs<World> ()

        // create and register our transform system
        let _ = ecs.RegisterSystem (SystemCorrelated<Transform, World> ecs)

        // create and register our skin system
        let _ = ecs.RegisterSystem (SystemCorrelated<Skin, World> ecs)

        // create and register our airship system
        let airshipSystem = ecs.RegisterSystem (SystemCorrelated<Airship, World> ecs)

        // define our airship system's update behavior
        ecs.Subscribe EcsEvents.Update $ fun _ _ _ ->
            let components = airshipSystem.Components.Array
            let transforms = airshipSystem.IndexJunction<Transform>().Array
            let skins = airshipSystem.IndexJunction<Skin>().Array
            for i in 0 .. components.Length - 1 do
                let comp = &components.[i]
                if  comp.Active then
                    let transform = &transforms.[i]
                    let skin = &skins.[i]
                    transform.Enabled <- i % 2 = 0
                    skin.Color.A <- byte 128

        // create and register our airship
        let airshipId = Gen.id
        let airship = ecs.RegisterCorrelated Unchecked.defaultof<Airship> airshipId

        // change some airship properties
        airship.Index.Transform.Index.Position.X <- 0.5f
        airship.Index.Skin.Index.Color.R <- byte 16

        // for non-junctioned entities, you can alternatively construct and use a much slower entity reference
        //let airshipRef = ecs.GetEntityRef ...
        //airshipRef.Index<Transform>().Position.Y <- 5.0f
        //airshipRef.Index<Skin>().Color.G <- byte 255

        // invoke update behavior
        ecs.Publish EcsEvents.Update () ecs.SystemGlobal world