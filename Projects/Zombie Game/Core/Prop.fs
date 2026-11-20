// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// A light shining in a scene, as a ceiling lamp.
type Light =
    { LightPosition : Vector3
      LightColor : Color
      LightBrightness : single
      LightReach : single
      LightShadowed : bool // whether it casts shadows - each costing six shadow renders, so only an area's main lights
      LightPowered : bool } // dim and flickering until the power's restored

/// Where a prop stands in a scene and which way it faces - its origin on the floor (or as high as it hangs), its yaw,
/// its size, and how far it stands out from the wall behind it, if it's set against one.
type Framing =
    { Origin : Vector3
      Yaw : single
      Scale : single
      WallDistanceOpt : single option }

/// A prop of an area once placed - its name, the name of its appearance, where it stands, the spot on the floor it's
/// interacted with at, at the middle of it, and how far its solid body reaches out from that spot to its nearest face,
/// the parts, colliders and lights it built - a door's leaves among its parts, and also on their own - the condition
/// it's there only while, if any, and its interaction, if it has one.
type Prop =
    { PropName : string
      PropAppearanceName : string
      PropFraming : Framing
      PropSpot : Vector3
      PropReach : single
      PropParts : Part array
      PropColliders : Collider array
      PropLights : Light list
      PropLeaves : Part array
      PropConditionOpt : CueCondition option
      PropInteractOpt : InteractData option }

    /// The door the prop is, if interacting with it tries one.
    member this.EnterOpt =
        match this.PropInteractOpt with
        | Some { Cue = Enter enter } -> Some enter
        | Some _ | None -> None

    /// What lies about as the prop, if interacting with it picks it up.
    member this.PickUpOpt =
        match this.PropInteractOpt with
        | Some { Cue = PickUp pickUp } -> Some pickUp
        | Some _ | None -> None

    /// Where one arrives through a door, a step before it, facing away from it.
    static member getArrival (door : Prop) =
        let yaw = door.PropFraming.Yaw
        (door.PropSpot + Math.YawForward yaw * Constants.Gameplay.DoorArrivalDistance, yaw)