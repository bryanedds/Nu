// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d.Tests

open System
open System.Numerics
open Box2D.NET
open NUnit.Framework
open Nu
open SandBox2d

module Sandbox2dCarPhysicsTests =

    let private meter = Constants.Engine.Meter2d

    let private step count world =
        for _ in 1 .. count do
            B2Worlds.b2World_Step (world, 1f / 60f, Constants.Physics.Collision2dSteps)

    let private createWorld () =
        let mutable definition = B2Types.b2DefaultWorldDef ()
        definition.gravity <- B2Vec2 (0f, -9.80665f)
        B2Worlds.b2CreateWorld &definition

    let private createBody world bodyType position =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- bodyType
        definition.position <- position
        B2Bodies.b2CreateBody (world, &definition)

    let private addBox body halfWidth halfHeight density friction =
        let mutable definition = B2Types.b2DefaultShapeDef ()
        definition.density <- density
        definition.material.friction <- friction
        let mutable polygon = B2Geometries.b2MakeBox (halfWidth, halfHeight)
        B2Shapes.b2CreatePolygonShape (body, &definition, &polygon) |> ignore

    let private addCarPolygon body =
        let points =
            Sandbox2dGeometry.CarContour
            |> Array.map (fun point ->
                (point - Sandbox2dGeometry.CarContourBounds.Center)
                * Sandbox2dGeometry.RaceCourseScale
                / meter)
            |> Array.map (fun point -> B2Vec2 (point.X, point.Y))
        let mutable hull = B2Hulls.b2ComputeHull (points.AsSpan (), points.Length)
        let mutable polygon = B2Geometries.b2MakePolygon (&hull, 0f)
        let mutable definition = B2Types.b2DefaultShapeDef ()
        definition.density <- Sandbox2dGeometry.CarChassisDensity
        definition.material.friction <- Sandbox2dGeometry.CarChassisFriction
        B2Shapes.b2CreatePolygonShape (body, &definition, &polygon) |> ignore

    let private createWheel (world : B2WorldId) (car : B2BodyId) (spawnPosition : B2Vec2)
        (position, density, frequency, friction, maxTorque, isMotor) =
        let offset =
            Sandbox2dGeometry.carWheelOffset position 0f
            / meter
        let wheelPosition = B2Vec2 (spawnPosition.X + offset.X, spawnPosition.Y + offset.Y)
        let wheel = createBody world B2BodyType.b2_dynamicBody wheelPosition
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- density * 2f
        shapeDefinition.material.friction <- friction
        let mutable circle = B2Circle (B2MathFunction.b2Vec2_zero, Sandbox2dGeometry.CarWheelRadius / meter)
        B2Shapes.b2CreateCircleShape (wheel, &shapeDefinition, &circle) |> ignore

        let mutable jointDefinition = B2Joints.b2DefaultWheelJointDef ()
        jointDefinition.``base``.bodyIdA <- car
        jointDefinition.``base``.bodyIdB <- wheel
        jointDefinition.``base``.localFrameA.p <- B2Bodies.b2Body_GetLocalPoint (car, wheelPosition)
        jointDefinition.``base``.localFrameB.p <- B2Bodies.b2Body_GetLocalPoint (wheel, wheelPosition)
        jointDefinition.``base``.localFrameA.q <- B2MathFunction.b2MakeRot MathF.PI_OVER_2
        jointDefinition.enableSpring <- true
        jointDefinition.hertz <- frequency
        jointDefinition.dampingRatio <- Sandbox2dGeometry.CarWheelDampingRatio
        jointDefinition.enableMotor <- isMotor
        jointDefinition.motorSpeed <- 0f
        jointDefinition.maxMotorTorque <- maxTorque
        let jointId = B2Joints.b2CreateWheelJoint (world, &jointDefinition)
        wheel, jointId

    let private runCar motorSpeed =
        let world = createWorld ()
        try
            let track = createBody world B2BodyType.b2_staticBody (B2Vec2 (0f, -0.05f))
            addBox track 100f 0.05f 0f Constants.Physics.FrictionDefault

            let rearOffsetY =
                Sandbox2dGeometry.carWheelOffset Sandbox2dGeometry.CarRearWheelModelOffset 0f
                |> fun offset -> offset.Y
            let spawnHeight =
                Sandbox2dGeometry.carSpawnHeight 0f rearOffsetY Sandbox2dGeometry.CarWheelRadius
                / meter
            let chassis = createBody world B2BodyType.b2_dynamicBody (B2Vec2 (0f, spawnHeight))
            addCarPolygon chassis

            let wheelPairs =
                Sandbox2dGeometry.CarWheelSpecs
                |> Array.map (fun (_name, position, density, frequency, friction, maxTorque, isMotor) ->
                    createWheel world chassis (B2Vec2 (0f, spawnHeight))
                        (position, density, frequency, friction, maxTorque, isMotor))
            let wheels = wheelPairs |> Array.map fst
            let rearMotorJoint = wheelPairs[0] |> snd
            B2WheelJoints.b2WheelJoint_SetMotorSpeed (rearMotorJoint, motorSpeed)
            B2WheelJoints.b2WheelJoint_EnableMotor (rearMotorJoint, true)
            step 600 world

            let chassisVelocity = B2Bodies.b2Body_GetLinearVelocity chassis
            let rearWheelBottom =
                B2Bodies.b2Body_GetPosition wheels[0]
                |> fun position -> position.Y - Sandbox2dGeometry.CarWheelRadius / meter
            let rotation = B2Bodies.b2Body_GetRotation chassis
            abs (MathF.Atan2 (rotation.s, rotation.c)), rearWheelBottom, abs chassisVelocity.X
        finally
            B2Worlds.b2DestroyWorld world

    [<Test>]
    let ``production car motor avoids high speed instability`` () =
        let pitch, rearClearance, velocity = runCar Sandbox2dGeometry.CarMotorSpeedMax
        let oldPitch, oldRearClearance, oldVelocity = runCar 50f
        Console.WriteLine (
            $"car production pitch={pitch} rear clearance={rearClearance} velocity={velocity}; "
            + $"old pitch={oldPitch} rear clearance={oldRearClearance} velocity={oldVelocity}")
        Assert.That (pitch, Is.LessThan 0.1f)
        Assert.That (rearClearance, Is.GreaterThan (-0.02f))
        Assert.That (rearClearance, Is.GreaterThan (oldRearClearance + 0.01f))
        Assert.That (velocity, Is.LessThan (oldVelocity * 0.5f))
