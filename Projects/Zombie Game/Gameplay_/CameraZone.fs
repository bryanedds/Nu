// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A fixed eye's zone and how it looks.
[<AutoOpen>]
module CameraZoneDispatcherExtensions =
    type Entity with
        member this.GetZone world : Box2 = this.Get (nameof this.Zone) world
        member this.SetZone (value : Box2) world = this.Set (nameof this.Zone) value world
        member this.Zone = lens (nameof this.Zone) this this.GetZone this.SetZone
        member this.GetEye world : Vector3 = this.Get (nameof this.Eye) world
        member this.SetEye (value : Vector3) world = this.Set (nameof this.Eye) value world
        member this.Eye = lens (nameof this.Eye) this this.GetEye this.SetEye
        member this.GetLookAt world : Vector3 = this.Get (nameof this.LookAt) world
        member this.SetLookAt (value : Vector3) world = this.Set (nameof this.LookAt) value world
        member this.LookAt = lens (nameof this.LookAt) this this.GetLookAt this.SetLookAt
        member this.GetFieldOfView world : single = this.Get (nameof this.FieldOfView) world
        member this.SetFieldOfView (value : single) world = this.Set (nameof this.FieldOfView) value world
        member this.FieldOfView = lens (nameof this.FieldOfView) this this.GetFieldOfView this.SetFieldOfView

/// A stretch of an area's floor watched by a fixed eye - where the eye stands, what it looks at, and how wide it sees.
/// The first declared holding the player wins.
type CameraZoneDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.Zone (box2 v2Zero v2One)
         define Entity.Eye v3Zero
         define Entity.LookAt v3Zero
         define Entity.FieldOfView 60.0f
         define Entity.Visible false]
