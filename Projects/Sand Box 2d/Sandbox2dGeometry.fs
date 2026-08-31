// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d

open System
open System.Numerics
open Nu

/// Pure geometry conventions used by the physics demonstrations.
module Sandbox2dGeometry =

    [<Literal>]
    let RaceCourseScale = 16f

    [<Literal>]
    let CarMotorSpeedMax = 8f

    [<Literal>]
    let BridgeLinkCount = 20

    [<Literal>]
    let BridgeLinkPitch = 32f

    [<Literal>]
    let BridgeLinkThickness = 4f

    [<Literal>]
    let MinimumBridgeSpan = 32f

    [<Literal>]
    let BubbleDiameterPerRadius = 2f

    [<Literal>]
    let RagdollArmClearance = 2f

    [<Literal>]
    let RagdollLimbSpacing = 20f

    [<Literal>]
    let CarTrackClearance = 8f

    [<Literal>]
    let BridgeCollideConnected = false

    let CarContour =
        [|v2 -2.5f 0.92f; v2 -2.375f 1.46f; v2 -0.58f 1.92f; v2 0.46f 1.92f
          v2 2.5f 1.17f; v2 2.5f 0.795f; v2 2.3f 0.67f; v2 -2.25f 0.65f|]

    let CarContourBounds = Box2.Enclose CarContour

    let CarRearWheelModelOffset = v2 -1.709f 0.78f

    let CarChassisDensity = 4f

    let CarChassisFriction = 0.2f

    let CarWheelDampingRatio = 0.85f

    let CarWheelSpecs : (string * Vector2 * single * single * single * single * bool) array =
        [| ("Back", CarRearWheelModelOffset, 0.8f, 5f, 0.9f, 20f, true)
           ("Front", v2 1.54f 0.8f, 1f, 8.5f, 0.2f, 10f, false) |]

    let CarWheelRadius = RaceCourseScale / 2f

    let carWheelOffset position rotation =
        position - CarContourBounds.Center
        |> fun value -> value.Rotate rotation
        |> fun value -> value * RaceCourseScale

    let carSize =
        CarContour
        |> Array.map (fun position -> (position - CarContourBounds.Center) * RaceCourseScale)
        |> Box2.Enclose
        |> fun bounds -> bounds.Size

    let bridgeEndpoints (center : Vector3) (offset : Vector2) =
        let offset =
            if offset.LengthSquared () >= MinimumBridgeSpan * MinimumBridgeSpan then offset
            else v2 (MinimumBridgeSpan / 2f) 0f
        (center + offset.V3, center - offset.V3)

    let bridgeLinkCenters (first : Vector3) (last : Vector3) linkCount =
        if linkCount <= 0 then invalidArg (nameof linkCount) "Bridge link count must be positive."
        let delta = (last - first) / single linkCount
        [| for i in 0 .. linkCount - 1 -> first + delta * (single i + 0.5f) |], delta.Magnitude

    let bubbleDiameter radius =
        if radius < 0f then invalidArg (nameof radius) "Bubble radius cannot be negative."
        radius * BubbleDiameterPerRadius

    let ragdollArmCenterX torsoWidth armLength direction =
        direction * (torsoWidth / 2f + armLength / 2f + RagdollArmClearance)

    /// Gets the scene-space anchor between two adjacent ragdoll limbs.
    let limbJointAnchor (spawnCenter : Vector3) (pos : Vector3) (posIncrement : Vector3) =
        spawnCenter + pos - posIncrement / 2f

    let carSpawnHeight trackHeight wheelOffsetY wheelRadius =
        trackHeight + CarTrackClearance + wheelRadius - wheelOffsetY
