// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
module Tests =

    [<Test>]
    let ``Car source contract preserves contour origin and wheel tuning.`` () =
        Assert.That (Sandbox2dGeometry.CarContour.Length, Is.EqualTo 8)
        Assert.That (Sandbox2dGeometry.CarContourBounds.Center, Is.EqualTo (v2 0f 0.285f))
        Assert.That (Sandbox2dGeometry.CarContour[0], Is.EqualTo (v2 -2.5f -0.08f))
        Assert.That (Sandbox2dGeometry.CarContour[7], Is.EqualTo (v2 -2.25f -0.35f))
        Assert.That (Sandbox2dGeometry.CarRearWheelModelOffset, Is.EqualTo (v2 -1.709f 0.78f))
        Assert.That (Sandbox2dGeometry.CarWheelDampingRatio, Is.EqualTo 0.7f)
        Assert.That (Sandbox2dGeometry.CarChassisDensity, Is.EqualTo 2f)
        let back, front = Sandbox2dGeometry.CarWheelSpecs[0], Sandbox2dGeometry.CarWheelSpecs[1]
        Assert.That (back, Is.EqualTo ("Back", v2 -1.709f 0.78f, 0.8f, 4f, 0.9f, 20f, true))
        Assert.That (front, Is.EqualTo ("Front", v2 1.54f 0.8f, 1f, 4f, 0.2f, 10f, false))

    [<Test>]
    let ``Ragdoll arm center leaves torso clearance.`` () =
        let centerX = Sandbox2dGeometry.ragdollArmCenterX 30f 24f 1f
        Assert.That (centerX - 12f - 15f, Is.EqualTo Sandbox2dGeometry.RagdollArmClearance)

    [<Test>]
    let ``Ragdoll torso joints use exact shared anchors.`` () =
        let spacing = Sandbox2dGeometry.RagdollLimbSpacing
        let upper = Sandbox2dGeometry.ragdollTorsoJointLocalOffset spacing false
        let lower = Sandbox2dGeometry.ragdollTorsoJointLocalOffset spacing true
        Assert.That (upper + lower, Is.EqualTo 0f)
        Assert.That (abs upper, Is.EqualTo (spacing / 2f))

    [<Test>]
    let ``Right upper arm anchor preserves authored increment.`` () =
        let anchor = Sandbox2dGeometry.limbJointAnchor (v3 120f 80f 0f) (v3 37f 40f 0f) (v3 30f 0f 0f)
        Assert.That (anchor, Is.EqualTo (v3 142f 120f 0f))

    [<Test>]
    let ``Bridge links share authored endpoints at arbitrary angle.`` () =
        let first, last = v3 10f 20f 0f, v3 106f 52f 0f
        let endpoints = Sandbox2dGeometry.bridgeLinkEndpoints first last 6
        Assert.That (endpoints.Length, Is.EqualTo 6)
        for i in 1 .. endpoints.Length - 1 do Assert.That (snd endpoints[i - 1], Is.EqualTo (fst endpoints[i]))
        let point ((first : Vector3), (last : Vector3)) positive =
            let halfLength = (last - first).Magnitude / 2f
            (first + last) / 2f + ((Sandbox2dGeometry.bridgeJointLocalEndpoint halfLength positive).Transform (Sandbox2dGeometry.bridgeRotation first last)).V3
        for endpointPair in endpoints do
            Assert.That ((point endpointPair true - snd endpointPair).Magnitude, Is.LessThan 0.001f)
            Assert.That ((point endpointPair false - fst endpointPair).Magnitude, Is.LessThan 0.001f)

    [<Test>]
    let ``Bridge endpoints are separated by the requested minimum span.`` () =
        let first, last = Sandbox2dGeometry.bridgeEndpoints v3Zero v2Zero
        Assert.That ((first - last).Magnitude, Is.GreaterThanOrEqualTo Sandbox2dGeometry.MinimumBridgeSpan)

    [<Test>]
    let ``Race track contour preserves Nu ghost endpoints and authored order.`` () =
        let contour = Sandbox2dGeometry.RaceTrackContour
        Assert.That (contour.Length, Is.EqualTo 36)
        Assert.That (contour[0], Is.EqualTo (v2 310f 5f))
        Assert.That (contour[0], Is.EqualTo contour[1])
        Assert.That (contour[34], Is.EqualTo contour[35])
        Assert.That (contour[35], Is.EqualTo (v2 -20f 5f))

    [<Test>]
    let ``Teeter source contract preserves start pose and limits.`` () =
        Assert.That (Sandbox2dGeometry.TeeterInitialAngle, Is.EqualTo 0f)
        Assert.That (Sandbox2dGeometry.TeeterCenterY, Is.EqualTo 1f)
        Assert.That (Sandbox2dGeometry.TeeterBoardHalfLength, Is.EqualTo 10f)
        Assert.That (Sandbox2dGeometry.TeeterBoardHalfThickness, Is.EqualTo 0.25f)
        Assert.That (Sandbox2dGeometry.TeeterAngleLimit, Is.EqualTo (8f * MathF.PI / 180f))