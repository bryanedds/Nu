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

    /// Conversion from authored source units to Nu physics meters.
    let SourceToPhysicsScale = RaceCourseScale / Constants.Engine.Meter2d

    /// Source-authored dynamic density scales with the inverse square of the authored-to-physics length conversion.
    let SourceMassScale = 1f / (SourceToPhysicsScale * SourceToPhysicsScale)

    /// Source-authored torque and angular impulse scale with the square of the authored-to-physics length conversion.
    let SourceTorqueScale = SourceToPhysicsScale * SourceToPhysicsScale

    /// Aether's source gravity (-10 source units/s^2), expressed in Nu pixel units.
    let SourceGravity = -10f * RaceCourseScale

    [<Literal>]
    let CarMotorSpeedMax = 50f

    [<Literal>]
    let BridgeLinkCount = 20

    [<Literal>]
    let BridgeSagRatio = 0.025f

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
    let TeeterBoardHalfLength = 10f

    [<Literal>]
    let TeeterBoardHalfThickness = 0.25f

    let TeeterAngleLimit = 8f * MathF.PI / 180f
    let TeeterInitialAngle = 0f
    let TeeterCenterY = 1f

    let TeeterAngularImpulse = 100f * SourceTorqueScale

    [<Literal>]
    let RagdollTorsoJointHalfSpacing = 0.5f

    [<Literal>]
    let BridgeCollideConnected = false

    [<Literal>]
    let BridgeLinearDamping = 1f

    [<Literal>]
    let BridgeAngularDamping = 1f

    let CarContour =
        [|v2 -2.5f -0.08f; v2 -2.375f 0.46f; v2 -0.58f 0.92f; v2 0.46f 0.92f
          v2 2.5f 0.17f; v2 2.5f -0.205f; v2 2.3f -0.33f; v2 -2.25f -0.35f|]

    let CarContourBounds = Box2.Enclose CarContour

    let CarRearWheelModelOffset = v2 -1.709f 0.78f

    let CarChassisDensity = 2f

    let CarChassisFriction = 0.2f

    let CarWheelDampingRatio = 0.7f

    let CarWheelSpecs : (string * Vector2 * single * single * single * single * bool) array =
        [| ("Back", CarRearWheelModelOffset, 0.8f, 4f, 0.9f, 20f, true)
           ("Front", v2 1.54f 0.8f, 1f, 4f, 0.2f, 10f, false) |]

    /// The authored open contour used by the race-course ground.
    let RaceTrackContour =
        [|v2 -20f 5f; v2 -20f 5f; v2 -20f 0f; v2 20f 0f; v2 25f 0.25f; v2 30f 1f; v2 35f 4f; v2 40f 0f; v2 45f 0f;
          v2 50f -1f; v2 55f -2f; v2 60f -2f; v2 65f -1.25f; v2 70f 0f; v2 75f 0.3f; v2 80f 1.5f; v2 85f 3.5f;
          v2 90f 0f; v2 95f -0.5f; v2 100f -1f; v2 105f -2f; v2 110f -2.5f; v2 115f -1.3f; v2 120f 0f; v2 160f 0f;
          v2 159f -10f; v2 201f -10f; v2 200f 0f; v2 240f 0f; v2 250f 5f; v2 250f -10f; v2 270f -10f; v2 270f 0f;
          v2 310f 0f; v2 310f 5f; v2 310f 5f|]
        |> Array.rev

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

    let bridgeLinkEndpoints (first : Vector3) (last : Vector3) linkCount =
        if linkCount <= 0 then invalidArg (nameof linkCount) "Bridge link count must be positive."
        let delta = (last - first) / single linkCount
        let perpendicular =
            let value = v3 -(last.Y - first.Y) (last.X - first.X) 0f
            if value.Y < 0f then value else -value
        let points =
            [| for i in 0 .. linkCount ->
                let t = single i / single linkCount
                first + delta * single i + perpendicular * (4f * t * (1f - t) * BridgeSagRatio) |]
        [| for i in 0 .. linkCount - 1 -> points[i], points[i + 1] |]

    let bridgeJointLocalEndpoint halfLength towardPositive =
        v2 0f (if towardPositive then halfLength else -halfLength)

    /// Rotation used by both bridge renderings and physics.  Links use their
    /// local Y axis for the span, hence the quarter-turn from the span angle.
    let bridgeRotation (first : Vector3) (last : Vector3) =
        Quaternion.CreateFromAngle2d (MathF.Atan2 (last.Y - first.Y, last.X - first.X) - MathF.PI_OVER_2)

    let ragdollTorsoJointLocalOffset torsoHeight towardPositive =
        torsoHeight * RagdollTorsoJointHalfSpacing * (if towardPositive then 1f else -1f)

    let bubbleDiameter radius =
        if radius < 0f then invalidArg (nameof radius) "Bubble radius cannot be negative."
        radius * BubbleDiameterPerRadius

    /// The authored bubble artwork includes transparent padding around its visible pixels.
    let BubbleImageInset = box2 (v2 133f 129f) (v2 748f 810f)

    let ragdollArmCenterX torsoWidth armLength direction =
        direction * (torsoWidth / 2f + armLength / 2f + RagdollArmClearance)

    /// Gets the scene-space anchor between two adjacent ragdoll limbs.
    let limbJointAnchor (spawnCenter : Vector3) (pos : Vector3) (posIncrement : Vector3) =
        spawnCenter + pos - posIncrement / 2f


    let carMotorSpeed acceleration =
        single (sign acceleration) * Math.SmoothStep (0f, CarMotorSpeedMax, abs acceleration)