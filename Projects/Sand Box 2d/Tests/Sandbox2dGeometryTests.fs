// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d.Tests

open System
open System.Numerics
open NUnit.Framework
open SandBox2d

module Sandbox2dGeometryTests =
    [<Test>]
    let ``bridge endpoints are separated by the requested minimum span`` () =
        let first, last = Sandbox2dGeometry.bridgeEndpoints Vector3.Zero Vector2.Zero
        let distance = (first - last).Magnitude
        Assert.That (distance, Is.GreaterThanOrEqualTo Sandbox2dGeometry.MinimumBridgeSpan)

        let first, last =
            Sandbox2dGeometry.bridgeEndpoints (Vector3 (10f, 20f, 0f)) (Vector2 (96f, 32f))
        Assert.That (first.X, Is.EqualTo 106f)
        Assert.That (last.Y, Is.EqualTo -12f)
        Assert.That ((first + last) * 0.5f, Is.EqualTo (Vector3 (10f, 20f, 0f)))

    [<Test>]
    let ``bridge links are centered between endpoints with the expected pitch`` () =
        let centers, pitch =
            Sandbox2dGeometry.bridgeLinkCenters
                (Vector3 (-320f, 0f, 0f))
                (Vector3 (320f, 0f, 0f))
                Sandbox2dGeometry.BridgeLinkCount
        Assert.That (centers.Length, Is.EqualTo Sandbox2dGeometry.BridgeLinkCount)
        Assert.That (centers.[0].X, Is.EqualTo -304f)
        Assert.That (centers[centers.Length - 1].X, Is.EqualTo 304f)
        Assert.That (pitch, Is.EqualTo Sandbox2dGeometry.BridgeLinkPitch)

    [<Test>]
    let ``ragdoll arm center leaves torso clearance`` () =
        let torsoWidth = 30f
        let armLength = 24f
        let centerX = Sandbox2dGeometry.ragdollArmCenterX torsoWidth armLength 1f
        let edgeToEdgeClearance = centerX - armLength / 2f - torsoWidth / 2f
        Assert.That (edgeToEdgeClearance, Is.EqualTo Sandbox2dGeometry.RagdollArmClearance)

    [<Test>]
    let ``car spawn height leaves wheel clearance above track`` () =
        let height = Sandbox2dGeometry.carSpawnHeight 100f 12f Sandbox2dGeometry.CarWheelRadius
        Assert.That (
            height + 12f - Sandbox2dGeometry.CarWheelRadius,
            Is.EqualTo (100f + Sandbox2dGeometry.CarTrackClearance))

    [<Test>]
    let ``bubble visual size doubles a nonnegative radius`` () =
        let radius = 12f
        Assert.That (Sandbox2dGeometry.bubbleDiameter radius, Is.EqualTo (radius * 2f))
