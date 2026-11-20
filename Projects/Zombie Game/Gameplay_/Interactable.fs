// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What can be acted on, and the event of its being acted on.
[<AutoOpen>]
module InteractableFacetExtensions =
    type Entity with
        member this.GetInteractionType world : InteractionType = this.Get (nameof this.InteractionType) world
        member this.SetInteractionType (value : InteractionType) world = this.Set (nameof this.InteractionType) value world
        member this.InteractionType = lens (nameof this.InteractionType) this this.GetInteractionType this.SetInteractionType
        member this.GetReach world : single = this.Get (nameof this.Reach) world
        member this.SetReach (value : single) world = this.Set (nameof this.Reach) value world
        member this.Reach = lens (nameof this.Reach) this this.GetReach this.SetReach
        member this.InteractEvent = Events.InteractEvent --> this

/// Makes an entity something the player can interact with - a door, a pickup, a feature - standing at its middle, its
/// interaction type named on the prompt, and how far its body reaches out from its middle, so it's reached across it. Acting on it publishes its interact event, which the gameplay screen answers.
type InteractableFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InteractionType Examine
         define Entity.Reach 0.0f]
