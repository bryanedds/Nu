// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace SandBox2d
open System
open System.Numerics
open Box2D.NET
open NUnit.Framework
open Nu
module Tests =

    type private FixtureMetrics =
        { InitialEnergy : single
          PeakEnergy : single
          FinalAnchorError : single
          PeakAnchorError : single }

    let private Meter = Constants.Engine.Meter2d

    let private toPhysics (position : Vector3) =
        B2Vec2 (position.X / Meter, position.Y / Meter)

    let private step count world =
        for _ in 1 .. count do
            B2Worlds.b2World_Step (world, 1f / 60f, Constants.Physics.Collision2dSteps)

    let private createWorld1 gravity =
        let mutable definition = B2Types.b2DefaultWorldDef ()
        definition.gravity <- gravity
        B2Worlds.b2CreateWorld &definition

    let private createWorld () =
        let mutable definition = B2Types.b2DefaultWorldDef ()
        definition.gravity <- B2Vec2 (0f, -9.80665f)
        B2Worlds.b2CreateWorld &definition

    let private createBody4 world bodyType x y =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- bodyType
        definition.position <- B2Vec2 (x, y)
        B2Bodies.b2CreateBody (world, &definition)

    let private createBody2 world position =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- B2BodyType.b2_dynamicBody
        definition.position <- position
        B2Bodies.b2CreateBody (world, &definition)

    let private createBody3 world bodyType position =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- bodyType
        definition.position <- position
        B2Bodies.b2CreateBody (world, &definition)

    let private createWheel (world : B2WorldId) (car : B2BodyId) (spawnPosition : B2Vec2)
        (position, density, frequency, friction, maxTorque, isMotor) =
        let offset = Sandbox2dGeometry.carWheelOffset position 0f / Meter
        let wheelPosition = B2Vec2 (spawnPosition.X + offset.X, spawnPosition.Y + offset.Y)
        let wheel = createBody3 world B2BodyType.b2_dynamicBody wheelPosition
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- density * 2f
        shapeDefinition.material.friction <- friction
        let mutable circle = B2Circle (B2MathFunction.b2Vec2_zero, Sandbox2dGeometry.CarWheelRadius / Meter)
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
        (wheel, jointId)

    let private addBox5 body halfWidth halfHeight density friction =
        let mutable definition = B2Types.b2DefaultShapeDef ()
        definition.density <- density
        definition.material.friction <- friction
        let mutable polygon = B2Geometries.b2MakeBox (halfWidth, halfHeight)
        B2Shapes.b2CreatePolygonShape (body, &definition, &polygon) |> ignore

    let private addBox4 body halfWidth halfHeight density =
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- density
        let mutable polygon = B2Geometries.b2MakeBox (halfWidth, halfHeight)
        B2Shapes.b2CreatePolygonShape (body, &shapeDefinition, &polygon) |> ignore

    let private addCapsule body width height =
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- 1f
        let radius = height / (2f * Meter)
        let halfSegment = (width - height) / (2f * Meter)
        let mutable capsule =
            B2Capsule (B2Vec2 (-halfSegment, 0f), B2Vec2 (halfSegment, 0f), radius)
        B2Shapes.b2CreateCapsuleShape (body, &shapeDefinition, &capsule) |> ignore

    let private addPolygon1 body =
        let points =
            Sandbox2dGeometry.CarContour
            |> Array.map (fun point ->
                (point - Sandbox2dGeometry.CarContourBounds.Center) *
                Sandbox2dGeometry.RaceCourseScale /
                Meter)
            |> Array.map (fun point -> B2Vec2 (point.X, point.Y))
        let mutable hull = B2Hulls.b2ComputeHull (points.AsSpan (), points.Length)
        let mutable polygon = B2Geometries.b2MakePolygon (&hull, 0f)
        let mutable definition = B2Types.b2DefaultShapeDef ()
        definition.density <- Sandbox2dGeometry.CarChassisDensity
        definition.material.friction <- Sandbox2dGeometry.CarChassisFriction
        B2Shapes.b2CreatePolygonShape (body, &definition, &polygon) |> ignore

    let private speed body =
        let mutable velocity = B2Bodies.b2Body_GetLinearVelocity body
        sqrt (B2MathFunction.b2Dot (&velocity, &velocity))

    let private kineticEnergy body =
        let mutable velocity = B2Bodies.b2Body_GetLinearVelocity body
        let angularVelocity = B2Bodies.b2Body_GetAngularVelocity body
        let mass = B2Bodies.b2Body_GetMass body
        let inertia = B2Bodies.b2Body_GetRotationalInertia body
        0.5f * mass * B2MathFunction.b2Dot (&velocity, &velocity) +
        0.5f * inertia * angularVelocity * angularVelocity

    let private jointError bodyA bodyB (localA : B2Vec2) (localB : B2Vec2) =
        let mutable transformA =
            B2Transform (B2Bodies.b2Body_GetPosition bodyA, B2Bodies.b2Body_GetRotation bodyA)
        let mutable transformB =
            B2Transform (B2Bodies.b2Body_GetPosition bodyB, B2Bodies.b2Body_GetRotation bodyB)
        let mutable localA = localA
        let mutable localB = localB
        let mutable pointA = B2MathFunction.b2TransformPoint (&transformA, &localA)
        let mutable pointB = B2MathFunction.b2TransformPoint (&transformB, &localB)
        B2MathFunction.b2Distance (&pointA, &pointB)

    let private simulateBridge collideConnected =
        let world = createWorld1 (B2Vec2 (0f, -9.80665f))
        try
            let start = 80f
            let pitch = Sandbox2dGeometry.BridgeLinkPitch / Constants.Engine.Meter2d
            let finish = start + single Sandbox2dGeometry.BridgeLinkCount * pitch
            let leftAnchor = createBody4 world B2BodyType.b2_staticBody start 0f
            let rightAnchor = createBody4 world B2BodyType.b2_staticBody finish 0f
            let links =
                [|for index in 0 .. Sandbox2dGeometry.BridgeLinkCount - 1 do
                   let link =
                       createBody4 world B2BodyType.b2_dynamicBody (start + (single index + 0.5f) * pitch) 0f
                   addBox4 link (pitch / 2f)
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

    [<Test>]
    let ``Bridge endpoints are separated by the requested minimum span`` () =
        let (first, last) = Sandbox2dGeometry.bridgeEndpoints Vector3.Zero Vector2.Zero
        let distance = (first - last).Magnitude
        Assert.That (distance, Is.GreaterThanOrEqualTo Sandbox2dGeometry.MinimumBridgeSpan)

        let (first, last) =
            Sandbox2dGeometry.bridgeEndpoints (Vector3 (10f, 20f, 0f)) (Vector2 (96f, 32f))
        Assert.That (first.X, Is.EqualTo 106f)
        Assert.That (last.Y, Is.EqualTo -12f)
        Assert.That ((first + last) * 0.5f, Is.EqualTo (Vector3 (10f, 20f, 0f)))

    [<Test>]
    let ``Bridge links are centered between endpoints with the expected pitch`` () =
        let (centers, pitch) =
            Sandbox2dGeometry.bridgeLinkCenters
                (Vector3 (-320f, 0f, 0f))
                (Vector3 (320f, 0f, 0f))
                Sandbox2dGeometry.BridgeLinkCount
        Assert.That (centers.Length, Is.EqualTo Sandbox2dGeometry.BridgeLinkCount)
        Assert.That (centers.[0].X, Is.EqualTo -304f)
        Assert.That (centers[centers.Length - 1].X, Is.EqualTo 304f)
        Assert.That (pitch, Is.EqualTo Sandbox2dGeometry.BridgeLinkPitch)

    [<Test>]
    let ``Ragdoll arm center leaves torso clearance`` () =
        let torsoWidth = 30f
        let armLength = 24f
        let centerX = Sandbox2dGeometry.ragdollArmCenterX torsoWidth armLength 1f
        let edgeToEdgeClearance = centerX - armLength / 2f - torsoWidth / 2f
        Assert.That (edgeToEdgeClearance, Is.EqualTo Sandbox2dGeometry.RagdollArmClearance)

    [<Test>]
    let ``Car spawn height leaves wheel clearance above track`` () =
        let height = Sandbox2dGeometry.carSpawnHeight 100f 12f Sandbox2dGeometry.CarWheelRadius
        Assert.That (
            height + 12f - Sandbox2dGeometry.CarWheelRadius,
            Is.EqualTo (100f + Sandbox2dGeometry.CarTrackClearance))

    [<Test>]
    let ``Bubble visual size doubles a nonnegative radius`` () =
        let radius = 12f
        Assert.That (Sandbox2dGeometry.bubbleDiameter radius, Is.EqualTo (radius * 2f))

    [<Test>]
    let ``Right upper arm anchor preserves authored increment`` () =
        let anchor =
            Sandbox2dGeometry.limbJointAnchor
                (v3 120f 80f 0f)
                (v3 37f 40f 0f)
                (v3 30f 0f 0f)
        Assert.That (anchor, Is.EqualTo (v3 142f 120f 0f))

    let private runFixture useProductionJoint =
        let mutable worldDefinition = B2Types.b2DefaultWorldDef ()
        worldDefinition.gravity <- B2Vec2 (0f, 0f)
        let world = B2Worlds.b2CreateWorld &worldDefinition
        try
            let spawnCenter = v3Zero
            let torsoCenter = v3 0f 40f 0f
            let armCenter = v3 (if useProductionJoint then 37f else 33f) 40f 0f
            let armIncrement = v3 30f 0f 0f
            let torso = createBody2 world (toPhysics (spawnCenter + torsoCenter))
            let arm = createBody2 world (toPhysics (spawnCenter + armCenter))
            addCapsule torso 40f 20f
            addCapsule arm 30f 15f
            let anchor =
                Sandbox2dGeometry.limbJointAnchor spawnCenter armCenter armIncrement
                |> toPhysics
            let mutable localA = B2Bodies.b2Body_GetLocalPoint (torso, anchor)
            let mutable localB = B2Bodies.b2Body_GetLocalPoint (arm, anchor)
            if useProductionJoint then
                let mutable jointDefinition = B2Joints.b2DefaultRevoluteJointDef ()
                jointDefinition.``base``.bodyIdA <- torso
                jointDefinition.``base``.bodyIdB <- arm
                jointDefinition.``base``.localFrameA.p <- localA
                jointDefinition.``base``.localFrameB.p <- localB
                jointDefinition.``base``.collideConnected <- false
                B2Joints.b2CreateRevoluteJoint (world, &jointDefinition) |> ignore
            else
                let mutable jointDefinition = B2Joints.b2DefaultDistanceJointDef ()
                jointDefinition.``base``.bodyIdA <- torso
                jointDefinition.``base``.bodyIdB <- arm
                jointDefinition.``base``.localFrameA.p <- localA
                jointDefinition.``base``.localFrameB.p <- localB
                jointDefinition.``base``.collideConnected <- true
                jointDefinition.length <- 4f / Meter
                jointDefinition.enableSpring <- true
                jointDefinition.hertz <- 25f
                jointDefinition.dampingRatio <- 1f
                B2Joints.b2CreateDistanceJoint (world, &jointDefinition) |> ignore
            B2Bodies.b2Body_SetAngularVelocity (arm, 2f)
            let initialEnergy = kineticEnergy torso + kineticEnergy arm
            let mutable peakEnergy = initialEnergy
            let mutable peakAnchorError = 0f
            for _ in 1 .. 30 do
                step 1 world
                peakEnergy <- max peakEnergy (kineticEnergy torso + kineticEnergy arm)
                let mutable torsoTransform =
                    B2Transform (B2Bodies.b2Body_GetPosition torso, B2Bodies.b2Body_GetRotation torso)
                let mutable armTransform =
                    B2Transform (B2Bodies.b2Body_GetPosition arm, B2Bodies.b2Body_GetRotation arm)
                let mutable torsoAnchor =
                    B2MathFunction.b2TransformPoint (&torsoTransform, &localA)
                let mutable armAnchor =
                    B2MathFunction.b2TransformPoint (&armTransform, &localB)
                peakAnchorError <- max peakAnchorError (B2MathFunction.b2Distance (&torsoAnchor, &armAnchor))
            let mutable torsoTransform =
                B2Transform (B2Bodies.b2Body_GetPosition torso, B2Bodies.b2Body_GetRotation torso)
            let mutable armTransform =
                B2Transform (B2Bodies.b2Body_GetPosition arm, B2Bodies.b2Body_GetRotation arm)
            let mutable torsoAnchor =
                B2MathFunction.b2TransformPoint (&torsoTransform, &localA)
            let mutable armAnchor =
                B2MathFunction.b2TransformPoint (&armTransform, &localB)
            let finalAnchorError = B2MathFunction.b2Distance (&torsoAnchor, &armAnchor)
            Console.WriteLine
                ($"ragdoll fixture production={useProductionJoint} initialEnergy={initialEnergy} " +
                 $"peakEnergy={peakEnergy} peakAnchorError={peakAnchorError} " +
                 $"finalAnchorError={finalAnchorError}")
            { InitialEnergy = initialEnergy
              PeakEnergy = peakEnergy
              FinalAnchorError = finalAnchorError
              PeakAnchorError = peakAnchorError }
        finally
            B2Worlds.b2DestroyWorld world

    [<Test>]
    let ``Right upper arm revolute joint remains stable after perturbation`` () =
        let production = runFixture true
        let oldControl = runFixture false
        Console.WriteLine
            ($"ragdoll production peak energy={production.PeakEnergy}; " +
             $"old control peak energy={oldControl.PeakEnergy}")
        Assert.That (production.PeakEnergy, Is.LessThanOrEqualTo (production.InitialEnergy * 1.001f))
        Assert.That (production.PeakAnchorError, Is.LessThanOrEqualTo Constants.Physics.Collision2dLinearSlop)
        Assert.That (oldControl.PeakEnergy, Is.GreaterThan (oldControl.InitialEnergy * 10f))
        Assert.That (production.PeakEnergy, Is.LessThan (oldControl.PeakEnergy * 0.1f))

    let private runCar motorSpeed =
        let world = createWorld ()
        try
            let track = createBody3 world B2BodyType.b2_staticBody (B2Vec2 (0f, -0.05f))
            addBox5 track 100f 0.05f 0f Constants.Physics.FrictionDefault

            let rearOffsetY =
                Sandbox2dGeometry.carWheelOffset Sandbox2dGeometry.CarRearWheelModelOffset 0f
                |> fun offset -> offset.Y
            let spawnHeight =
                Sandbox2dGeometry.carSpawnHeight 0f rearOffsetY Sandbox2dGeometry.CarWheelRadius
                / Meter
            let chassis = createBody3 world B2BodyType.b2_dynamicBody (B2Vec2 (0f, spawnHeight))
            addPolygon1 chassis

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
                |> fun position -> position.Y - Sandbox2dGeometry.CarWheelRadius / Meter
            let rotation = B2Bodies.b2Body_GetRotation chassis
            (abs (MathF.Atan2 (rotation.s, rotation.c)), rearWheelBottom, abs chassisVelocity.X)
        finally
            B2Worlds.b2DestroyWorld world

    [<Test>]
    let ``Production car motor avoids high speed instability`` () =
        let (pitch, rearClearance, velocity) = runCar Sandbox2dGeometry.CarMotorSpeedMax
        let (oldPitch, oldRearClearance, oldVelocity) = runCar 50f
        Console.WriteLine (
            $"car production pitch={pitch} rear clearance={rearClearance} velocity={velocity}; "
            + $"old pitch={oldPitch} rear clearance={oldRearClearance} velocity={oldVelocity}")
        Assert.That (pitch, Is.LessThan 0.1f)
        Assert.That (rearClearance, Is.GreaterThan (-0.02f))
        Assert.That (rearClearance, Is.GreaterThan (oldRearClearance + 0.01f))
        Assert.That (velocity, Is.LessThan (oldVelocity * 0.5f))