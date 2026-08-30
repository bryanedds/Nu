// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d.Tests
open System
open Box2D.NET
open NUnit.Framework
open Nu
open SandBox2d
module PhysicsTests =

    let private step count world =
        for _ in 1 .. count do
            B2Worlds.b2World_Step (world, 1f / 60f, Constants.Physics.Collision2dSteps)

    let private createWorld gravity =
        let mutable definition = B2Types.b2DefaultWorldDef ()
        definition.gravity <- gravity
        B2Worlds.b2CreateWorld &definition

    let private createBody world bodyType x y =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- bodyType
        definition.position <- B2Vec2 (x, y)
        B2Bodies.b2CreateBody (world, &definition)

    let private addBox body halfWidth halfHeight density =
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- density
        let mutable polygon = B2Geometries.b2MakeBox (halfWidth, halfHeight)
        B2Shapes.b2CreatePolygonShape (body, &shapeDefinition, &polygon) |> ignore

    let private speed body =
        let velocity = B2Bodies.b2Body_GetLinearVelocity body
        sqrt (B2MathFunction.b2Dot (velocity, velocity))

    let private jointError bodyA bodyB localA localB =
        let mutable transformA =
            B2Transform (B2Bodies.b2Body_GetPosition bodyA, B2Bodies.b2Body_GetRotation bodyA)
        let mutable transformB =
            B2Transform (B2Bodies.b2Body_GetPosition bodyB, B2Bodies.b2Body_GetRotation bodyB)
        let pointA = B2MathFunction.b2TransformPoint (&transformA, localA)
        let pointB = B2MathFunction.b2TransformPoint (&transformB, localB)
        B2MathFunction.b2Distance (pointA, pointB)

    let private simulateBridge collideConnected =
        let world = createWorld (B2Vec2 (0f, -9.80665f))
        try
            let start = 80f
            let pitch = Sandbox2dGeometry.BridgeLinkPitch / Constants.Engine.Meter2d
            let finish = start + single Sandbox2dGeometry.BridgeLinkCount * pitch
            let leftAnchor = createBody world B2BodyType.b2_staticBody start 0f
            let rightAnchor = createBody world B2BodyType.b2_staticBody finish 0f
            let links =
                [| for index in 0 .. Sandbox2dGeometry.BridgeLinkCount - 1 do
                       let link =
                           createBody world B2BodyType.b2_dynamicBody (start + (single index + 0.5f) * pitch) 0f
                       addBox link (pitch / 2f)
                           (Sandbox2dGeometry.BridgeLinkThickness / Constants.Engine.Meter2d / 2f) 1f
                       B2Bodies.b2Body_SetAngularVelocity (link, if index % 2 = 0 then 1.5f else -1.5f)
                       link |]
            let joints = ResizeArray<_> ()

            let createRevoluteJoint bodyA bodyB anchorX =
                let mutable definition = B2Joints.b2DefaultRevoluteJointDef ()
                let anchor = B2Vec2 (anchorX, 0f)
                definition.``base``.bodyIdA <- bodyA
                definition.``base``.bodyIdB <- bodyB
                let localA = B2Bodies.b2Body_GetLocalPoint (bodyA, anchor)
                let localB = B2Bodies.b2Body_GetLocalPoint (bodyB, anchor)
                definition.``base``.localFrameA.p <- localA
                definition.``base``.localFrameB.p <- localB
                definition.``base``.collideConnected <- collideConnected
                joints.Add (bodyA, bodyB, localA, localB)
                B2Joints.b2CreateRevoluteJoint (world, &definition) |> ignore

            createRevoluteJoint leftAnchor links[0] start
            for index in 1 .. links.Length - 1 do
                createRevoluteJoint links[index - 1] links[index] (start + single index * pitch)
            createRevoluteJoint links[links.Length - 1] rightAnchor finish

            let mutable transientSpeed = 0f
            for _ in 1 .. 600 do
                step 1 world
                transientSpeed <- max transientSpeed (links |> Array.map speed |> Array.max)
            let maximumSpeed = links |> Array.map speed |> Array.max
            let maximumError =
                joints
                |> Seq.map (fun (bodyA, bodyB, localA, localB) -> jointError bodyA bodyB localA localB)
                |> Seq.max
            (maximumSpeed, maximumError, transientSpeed)
        finally
            B2Worlds.b2DestroyWorld world

    [<Test>]
    let ``Production bridge chain settles with collision disabled`` () =
        let (speed, error, transient) = simulateBridge Sandbox2dGeometry.BridgeCollideConnected
        let (controlSpeed, controlError, controlTransient) = simulateBridge true
        let message =
            $"bridge production speed={speed} error={error} transient={transient}; " +
            $"control speed={controlSpeed} error={controlError} transient={controlTransient}"
        Console.WriteLine message
        Assert.That (speed, Is.LessThan 3f)
        Assert.That (error, Is.LessThan 0.03f)
        Assert.That (error, Is.LessThan (controlError * 0.98f))