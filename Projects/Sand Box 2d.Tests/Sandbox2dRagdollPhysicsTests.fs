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

module Sandbox2dRagdollPhysicsTests =

    type private Metrics =
        { InitialEnergy : single
          PeakEnergy : single
          FinalAnchorError : single
          PeakAnchorError : single }

    let private meter = Constants.Engine.Meter2d

    let private toPhysics (position : Vector3) =
        B2Vec2 (position.X / meter, position.Y / meter)

    let private step count world =
        for _ in 1 .. count do
            B2Worlds.b2World_Step (world, 1f / 60f, Constants.Physics.Collision2dSteps)

    let private createBody world position =
        let mutable definition = B2Types.b2DefaultBodyDef ()
        definition.``type`` <- B2BodyType.b2_dynamicBody
        definition.position <- position
        B2Bodies.b2CreateBody (world, &definition)

    let private addCapsule body width height =
        let mutable shapeDefinition = B2Types.b2DefaultShapeDef ()
        shapeDefinition.density <- 1f
        let radius = height / (2f * meter)
        let halfSegment = (width - height) / (2f * meter)
        let mutable capsule =
            B2Capsule (B2Vec2 (-halfSegment, 0f), B2Vec2 (halfSegment, 0f), radius)
        B2Shapes.b2CreateCapsuleShape (body, &shapeDefinition, &capsule) |> ignore

    let private kineticEnergy body =
        let velocity = B2Bodies.b2Body_GetLinearVelocity body
        let angularVelocity = B2Bodies.b2Body_GetAngularVelocity body
        let mass = B2Bodies.b2Body_GetMass body
        let inertia = B2Bodies.b2Body_GetRotationalInertia body
        0.5f * mass * B2MathFunction.b2Dot (velocity, velocity) +
        0.5f * inertia * angularVelocity * angularVelocity

    let private runFixture useProductionJoint =
        let mutable worldDefinition = B2Types.b2DefaultWorldDef ()
        worldDefinition.gravity <- B2Vec2 (0f, 0f)
        let world = B2Worlds.b2CreateWorld &worldDefinition
        try
            let spawnCenter = v3Zero
            let torsoCenter = v3 0f 40f 0f
            let armCenter = v3 (if useProductionJoint then 37f else 33f) 40f 0f
            let armIncrement = v3 30f 0f 0f
            let torso = createBody world (toPhysics (spawnCenter + torsoCenter))
            let arm = createBody world (toPhysics (spawnCenter + armCenter))
            addCapsule torso 40f 20f
            addCapsule arm 30f 15f
            let anchor =
                Sandbox2dGeometry.limbJointAnchor spawnCenter armCenter armIncrement
                |> toPhysics
            let localA = B2Bodies.b2Body_GetLocalPoint (torso, anchor)
            let localB = B2Bodies.b2Body_GetLocalPoint (arm, anchor)
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
                jointDefinition.length <- 4f / meter
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
                let torsoAnchor =
                    B2MathFunction.b2TransformPoint (&torsoTransform, localA)
                let armAnchor =
                    B2MathFunction.b2TransformPoint (&armTransform, localB)
                peakAnchorError <- max peakAnchorError (B2MathFunction.b2Distance (torsoAnchor, armAnchor))
            let mutable torsoTransform =
                B2Transform (B2Bodies.b2Body_GetPosition torso, B2Bodies.b2Body_GetRotation torso)
            let mutable armTransform =
                B2Transform (B2Bodies.b2Body_GetPosition arm, B2Bodies.b2Body_GetRotation arm)
            let torsoAnchor =
                B2MathFunction.b2TransformPoint (&torsoTransform, localA)
            let armAnchor =
                B2MathFunction.b2TransformPoint (&armTransform, localB)
            let finalAnchorError = B2MathFunction.b2Distance (torsoAnchor, armAnchor)
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
    let ``right upper arm anchor preserves authored increment`` () =
        let anchor =
            Sandbox2dGeometry.limbJointAnchor
                (v3 120f 80f 0f)
                (v3 37f 40f 0f)
                (v3 30f 0f 0f)
        Assert.That (anchor, Is.EqualTo (v3 142f 120f 0f))

    [<Test>]
    let ``right upper arm revolute joint remains stable after perturbation`` () =
        let production = runFixture true
        let oldControl = runFixture false
        Console.WriteLine
            ($"ragdoll production peak energy={production.PeakEnergy}; " +
             $"old control peak energy={oldControl.PeakEnergy}")
        Assert.That (production.PeakEnergy, Is.LessThanOrEqualTo (production.InitialEnergy * 1.001f))
        Assert.That (production.PeakAnchorError, Is.LessThanOrEqualTo B2Constants.B2_LINEAR_SLOP)
        Assert.That (oldControl.PeakEnergy, Is.GreaterThan (oldControl.InitialEnergy * 10f))
        Assert.That (production.PeakEnergy, Is.LessThan (oldControl.PeakEnergy * 0.1f))
