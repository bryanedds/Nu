// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a pickup is, and how many.
[<AutoOpen>]
module PickupDispatcherExtensions =
    type Entity with
        member this.GetItem world : ItemType = this.Get (nameof this.Item) world
        member this.SetItem (value : ItemType) world = this.Set (nameof this.Item) value world
        member this.Item = lens (nameof this.Item) this this.GetItem this.SetItem
        member this.GetCount world : int = this.Get (nameof this.Count) world
        member this.SetCount (value : int) world = this.Set (nameof this.Count) value world
        member this.Count = lens (nameof this.Count) this this.GetCount this.SetCount

/// Something lying about to pick up, drawn as it looks with a faint glint over it, as the old games' items were.
type PickupDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    /// Draw a part of a pickup, offset from where it lies.
    static let renderPart renderPass surface shape (position : Vector3) (offset : Vector3) (size : Vector3) color world =
        Draw.shape renderPass surface shape (position + offset) quatIdentity size color 0.0f true world

    /// Draw a key with a tag of a color.
    static let renderKey renderPass (position : Vector3) (tag : Color) world =
        renderPart renderPass Steel PartBox position (v3 0.0f 0.01f 0.0f) (v3 0.08f 0.012f 0.02f) (Color (0.7f, 0.7f, 0.72f, 1.0f)) world
        renderPart renderPass Plastic PartBox position (v3 -0.07f 0.012f 0.0f) (v3 0.05f 0.008f 0.035f) tag world

    static member Facets =
        [typeof<InteractableFacet>]

    static member Properties =
        [define Entity.Item Antiseptic
         define Entity.Count 1
         define Entity.Size (v3Dup 0.3f)]

    override this.Render (renderPass, entity, world) =
        let position = entity.GetPosition world
        match entity.GetItem world with
        | Antiseptic | DoubleAntiseptic ->
            renderPart renderPass Plastic PartBox position (v3 0.0f 0.025f 0.0f) (v3 0.12f 0.05f 0.08f) (Color (0.88f, 0.9f, 0.88f, 1.0f)) world
            renderPart renderPass Plastic PartBox position (v3 0.0f 0.051f 0.0f) (v3 0.05f 0.002f 0.03f) (Color (0.2f, 0.55f, 0.25f, 1.0f)) world // a green cross
        | Coagulant | TraumaMix -> renderPart renderPass Glass PartCylinder position (v3 0.0f 0.05f 0.0f) (v3 0.04f 0.1f 0.04f) (Color (0.55f, 0.06f, 0.06f, 1.0f)) world
        | FirstAidSpray -> renderPart renderPass Steel PartCylinder position (v3 0.0f 0.1f 0.0f) (v3 0.07f 0.2f 0.07f) (Color (0.85f, 0.85f, 0.8f, 1.0f)) world
        | HandgunBullets -> renderPart renderPass Plastic PartBox position (v3 0.0f 0.04f 0.0f) (v3 0.14f 0.08f 0.1f) (Color (0.2f, 0.3f, 0.5f, 1.0f)) world
        | ShotgunShells -> renderPart renderPass Plastic PartBox position (v3 0.0f 0.05f 0.0f) (v3 0.16f 0.1f 0.1f) (Color (0.6f, 0.15f, 0.1f, 1.0f)) world
        | MagnumRounds -> renderPart renderPass Plastic PartBox position (v3 0.0f 0.03f 0.0f) (v3 0.12f 0.06f 0.08f) (Color (0.55f, 0.45f, 0.15f, 1.0f)) world
        | Grenades ->
            renderPart renderPass Steel PartCylinder position (v3 -0.04f 0.035f 0.0f) (v3 0.045f 0.07f 0.045f) (Color (0.7f, 0.58f, 0.3f, 1.0f)) world // two brass cases
            renderPart renderPass Steel PartCylinder position (v3 0.04f 0.035f 0.0f) (v3 0.045f 0.07f 0.045f) (Color (0.7f, 0.58f, 0.3f, 1.0f)) world
            renderPart renderPass Paint PartCylinder position (v3 -0.04f 0.085f 0.0f) (v3 0.04f 0.03f 0.04f) (Color (0.3f, 0.34f, 0.2f, 1.0f)) world // their olive heads
            renderPart renderPass Paint PartCylinder position (v3 0.04f 0.085f 0.0f) (v3 0.04f 0.03f 0.04f) (Color (0.3f, 0.34f, 0.2f, 1.0f)) world
        | InkRibbon -> renderPart renderPass Plastic PartCylinder position (v3 0.0f 0.03f 0.0f) (v3 0.08f 0.06f 0.08f) (Color (0.05f, 0.05f, 0.05f, 1.0f)) world
        | PharmacyKey -> renderKey renderPass position (Color (0.2f, 0.6f, 0.25f, 1.0f)) world
        | MorgueKey -> renderKey renderPass position (Color (0.05f, 0.05f, 0.05f, 1.0f)) world
        | AmbulanceKeys -> renderKey renderPass position (Color (0.85f, 0.75f, 0.1f, 1.0f)) world
        | MasterKey -> renderKey renderPass position (Color (0.75f, 0.1f, 0.1f, 1.0f)) world
        | VanKeys -> renderKey renderPass position (Color (0.15f, 0.3f, 0.7f, 1.0f)) world
        | ElevatorKey -> renderKey renderPass position (Color (0.8f, 0.15f, 0.1f, 1.0f)) world
        | Keycard ->
            renderPart renderPass Plastic PartBox position (v3 0.0f 0.004f 0.0f) (v3 0.085f 0.004f 0.055f) (Color (0.82f, 0.86f, 0.9f, 1.0f)) world
            renderPart renderPass Steel PartBox position (v3 0.02f 0.007f 0.0f) (v3 0.03f 0.002f 0.025f) (Color (0.75f, 0.65f, 0.3f, 1.0f)) world // its chip
        | BoltCutters ->
            renderPart renderPass Steel PartBox position (v3 0.0f 0.02f 0.0f) (v3 0.12f 0.03f 0.05f) (Color (0.35f, 0.36f, 0.38f, 1.0f)) world // the jaws
            renderPart renderPass Rubber PartBox position (v3 0.3f 0.02f 0.0f) (v3 0.5f 0.025f 0.1f) (Color (0.7f, 0.15f, 0.1f, 1.0f)) world // the long red handles
        | StudentId ->
            renderPart renderPass Plastic PartBox position (v3 0.0f 0.004f 0.0f) (v3 0.085f 0.004f 0.055f) (Color (0.85f, 0.85f, 0.8f, 1.0f)) world
            renderPart renderPass Plastic PartBox position (v3 -0.02f 0.007f 0.0f) (v3 0.035f 0.002f 0.04f) (Color (0.15f, 0.35f, 0.25f, 1.0f)) world // its photo and crest
        | Fuse ->
            renderPart renderPass Tile PartCylinder position (v3 0.0f 0.04f 0.0f) (v3 0.05f 0.12f 0.05f) (Color (0.9f, 0.88f, 0.82f, 1.0f)) world
            renderPart renderPass Steel PartCylinder position (v3 0.0f 0.04f 0.0f) (v3 0.055f 0.14f 0.055f) (Color (0.6f, 0.6f, 0.62f, 1.0f)) world
        | Handgun -> renderPart renderPass Steel PartBox position (v3 0.0f 0.02f 0.0f) (v3 0.2f 0.035f 0.04f) (Color (0.12f, 0.12f, 0.13f, 1.0f)) world
        | Shotgun ->
            renderPart renderPass Steel PartBox position (v3 0.0f 0.04f 0.0f) (v3 0.95f 0.05f 0.06f) (Color (0.12f, 0.12f, 0.13f, 1.0f)) world
            renderPart renderPass Wood PartBox position (v3 0.36f 0.04f 0.0f) (v3 0.3f 0.07f 0.07f) (Color (0.36f, 0.22f, 0.12f, 1.0f)) world
        | Magnum ->
            renderPart renderPass Steel PartBox position (v3 0.0f 0.025f 0.0f) (v3 0.28f 0.045f 0.045f) (Color (0.55f, 0.56f, 0.58f, 1.0f)) world
            renderPart renderPass Wood PartBox position (v3 0.13f 0.025f 0.0f) (v3 0.09f 0.06f 0.05f) (Color (0.3f, 0.18f, 0.1f, 1.0f)) world
        | GrenadeLauncher ->
            renderPart renderPass Steel PartBox position (v3 -0.14f 0.04f 0.0f) (v3 0.36f 0.07f 0.07f) (Color (0.25f, 0.29f, 0.18f, 1.0f)) world // the fat short barrel
            renderPart renderPass Wood PartBox position (v3 0.2f 0.04f 0.0f) (v3 0.32f 0.08f 0.06f) (Color (0.36f, 0.22f, 0.12f, 1.0f)) world // its stock
        | Knife -> renderPart renderPass Steel PartBox position (v3 0.0f 0.01f 0.0f) (v3 0.22f 0.01f 0.03f) (Color (0.7f, 0.72f, 0.75f, 1.0f)) world
        let gameTime = if entity.GetTimeScaled world then world.ScaledTime else world.UnscaledTime
        let glint = 0.5f + 0.5f * sin (gameTime.SecondsF * 3.0f + position.X)
        Draw.shape renderPass Light PartBall (position + v3 0.0f 0.32f 0.0f) quatIdentity (v3Dup (0.03f + 0.02f * glint)) (Color (1.0f, 0.95f, 0.8f, 1.0f)) (1.5f + 1.5f * glint) false world
