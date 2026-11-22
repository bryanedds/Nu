// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
module PhysicsEngineQueryTests =

    // NOTE: we test the 2D physics engines directly instead of via the World API since a stub world has no physics.

    let private makeBodyProperties center bodyType sensor =
        { Enabled = true
          Center = center
          Rotation = Quaternion.Identity
          Scale = v3One
          BodyType = bodyType
          BodyShape = BoxShape { Size = v3 32.0f 32.0f 0.0f; TransformOpt = None; PropertiesOpt = None }
          SleepingAllowed = true
          Friction = Constants.Physics.FrictionDefault
          Restitution = 0.0f
          LinearVelocity = v3Zero
          LinearDamping = 0.0f
          AngularVelocity = v3Zero
          AngularDamping = 0.0f
          AngularFactor = v3Zero
          KinematicPushLimitOpt = None
          Substance = Mass 1.0f
          Gravity = GravityIgnore
          CharacterProperties = StairSteppingCharacterProperties StairSteppingCharacterProperties.defaultProperties
          VehicleProperties = VehiclePropertiesAbsent
          CollisionDetection = Discrete
          CollisionGroup = 0
          CollisionCategories = 1UL
          CollisionMask = UInt64.MaxValue
          Sensor = sensor
          BodyIndex = 0 }

    let private makeBodyId name =
        { BodySource = Nu.Entity ("Game/Screen/Group/" + name); BodyIndex = 0 }

    let private createBodyPlus (physicsEngine : PhysicsEngine) bodyId center linearVelocity bodyType sensor =
        let bodyProperties = { makeBodyProperties center bodyType sensor with LinearVelocity = linearVelocity }
        physicsEngine.HandleMessage (CreateBodyMessage { BodyId = bodyId; BodyProperties = bodyProperties })

    let private createBody physicsEngine bodyId center bodyType sensor =
        createBodyPlus physicsEngine bodyId center v3Zero bodyType sensor

    let private step (physicsEngine : PhysicsEngine) =
        for _ in 0 .. dec 3 do physicsEngine.TryIntegrate (GameTime.ofUpdates 1L) |> ignore

    let private intersectees (physicsEngine : PhysicsEngine) bodyId =
        physicsEngine.GetBodyIntersections bodyId
        |> Array.map (fun intersection ->
            Assert.AreEqual (bodyId, intersection.BodyShapeIntersector.BodyId) // intersectors are the body's shapes
            intersection.BodyShapeIntersectee.BodyId)
        |> Set.ofArray

    let private reportsCurrentIntersections (physicsEngine : PhysicsEngine) =
        let sensor = makeBodyId "Sensor"
        let mover = makeBodyId "Mover"
        let wall = makeBodyId "Wall"
        let away = makeBodyId "Away"
        try createBody physicsEngine sensor (v3 -8.0f 0.0f 0.0f) Static true
            createBody physicsEngine mover (v3 8.0f 0.0f 0.0f) Dynamic false
            createBody physicsEngine wall (v3 32.0f 0.0f 0.0f) Static false
            createBody physicsEngine away (v3 500.0f 0.0f 0.0f) Dynamic false
            step physicsEngine

            // a sensor's intersection is reported from both sides, as is a touching contact
            Assert.AreEqual (Set.ofList [mover], intersectees physicsEngine sensor)
            Assert.AreEqual (Set.ofList [sensor; wall], intersectees physicsEngine mover)
            Assert.AreEqual (Set.ofList [mover], intersectees physicsEngine wall)
            Assert.AreEqual (Set.empty<BodyId>, intersectees physicsEngine away)

            // a destroyed body's intersections are gone from its former partners at once, with no step between
            physicsEngine.HandleMessage (DestroyBodyMessage { BodyId = mover })
            Assert.AreEqual (Set.empty<BodyId>, intersectees physicsEngine sensor)
            Assert.AreEqual (Set.empty<BodyId>, intersectees physicsEngine wall)

            // a body re-created elsewhere under the same id has only its new intersections
            createBody physicsEngine mover (v3 500.0f 8.0f 0.0f) Dynamic false
            step physicsEngine
            Assert.AreEqual (Set.empty<BodyId>, intersectees physicsEngine sensor)
            Assert.AreEqual (Set.ofList [away], intersectees physicsEngine mover)

        finally physicsEngine.CleanUp ()

    let [<Test>] ``Box2D reports a body's current intersections.`` () =
        Nu.init ()
        reportsCurrentIntersections (Box2dNetPhysicsEngine.make v3Zero)

    let [<Test>] ``Aether reports a body's current intersections.`` () =
        Nu.init ()
        reportsCurrentIntersections (AetherPhysicsEngine.make v3Zero)

    let private jointExistsAndBreaksAsDeclared (physicsEngine : PhysicsEngine) bodyJoint =
        let body = makeBodyId "Body"
        let body2 = makeBodyId "Body2"
        let jointSource = Nu.Entity "Game/Screen/Group/Joint"
        let bodyJointId = { BodyJointSource = jointSource; BodyJointIndex = 0 }
        let declarePlus enabled broken breakingPointOpt =
            let bodyJointProperties =
                { BodyJoint = bodyJoint; BodyJointTarget = body; BodyJointTarget2 = body2; BodyJointEnabled = enabled
                  BreakingPointOpt = breakingPointOpt; Broken = broken; CollideConnected = false; BodyJointIndex = 0 }
            let message = { BodyJointSource = jointSource; BodyJointProperties = bodyJointProperties }
            physicsEngine.HandleMessage (CreateBodyJointMessage message)
        let declare enabled broken = declarePlus enabled broken None
        let undeclare () =
            let message = { BodyJointId = bodyJointId; BodyJointTarget = body; BodyJointTarget2 = body2 }
            physicsEngine.HandleMessage (DestroyBodyJointMessage message)
        try createBody physicsEngine body v3Zero Dynamic false
            createBody physicsEngine body2 (v3 100.0f 0.0f 0.0f) Dynamic false

            // a declared joint exists whether enabled, disabled or broken
            for (enabled, broken) in [(true, false); (false, false); (true, true)] do
                declare enabled broken
                Assert.IsTrue (physicsEngine.GetBodyJointExists bodyJointId)
                undeclare ()
                Assert.IsFalse (physicsEngine.GetBodyJointExists bodyJointId)

            // a joint doesn't exist while one of its bodies doesn't, and exists again once it's re-created
            declare false false
            physicsEngine.HandleMessage (DestroyBodyMessage { BodyId = body2 })
            Assert.IsFalse (physicsEngine.GetBodyJointExists bodyJointId)
            createBody physicsEngine body2 (v3 100.0f 0.0f 0.0f) Dynamic false
            Assert.IsTrue (physicsEngine.GetBodyJointExists bodyJointId)
            undeclare ()

            // a joint declared broken is broken, and one re-declared unbroken is whole
            declare true true
            Assert.IsTrue (physicsEngine.GetBodyJointBroken bodyJointId)
            undeclare ()
            declare true false
            Assert.IsFalse (physicsEngine.GetBodyJointBroken bodyJointId)
            undeclare ()

            // a joint that breaks during simulation is broken but still exists, and is whole again once one of its
            // bodies is re-created (the second body flying off so the joint is loaded enough to break in every engine)
            physicsEngine.HandleMessage (DestroyBodyMessage { BodyId = body2 })
            createBodyPlus physicsEngine body2 (v3 100.0f 0.0f 0.0f) (v3 10000.0f 0.0f 0.0f) Dynamic false
            declarePlus true false (Some 0.001f)
            step physicsEngine
            Assert.IsTrue (physicsEngine.GetBodyJointBroken bodyJointId)
            Assert.IsTrue (physicsEngine.GetBodyJointExists bodyJointId)
            physicsEngine.HandleMessage (DestroyBodyMessage { BodyId = body2 })
            createBody physicsEngine body2 (v3 100.0f 0.0f 0.0f) Dynamic false
            Assert.IsFalse (physicsEngine.GetBodyJointBroken bodyJointId)

        finally physicsEngine.CleanUp ()

    let [<Test>] ``Box2D reports a joint's existence and breakage as declared.`` () =
        Nu.init ()
        let bodyJoint =
            Box2dNetBodyJoint
                { CreateBodyJoint = fun body body2 world ->
                    let mutable jointDef = Box2D.NET.B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- body
                    jointDef.``base``.bodyIdB <- body2
                    jointDef.length <- 1.0f // much shorter than the bodies are apart so that a breakable joint breaks
                    Box2D.NET.B2Joints.b2CreateDistanceJoint (world, &jointDef) }
        jointExistsAndBreaksAsDeclared (Box2dNetPhysicsEngine.make v3Zero) bodyJoint

    let [<Test>] ``Aether reports a joint's existence and breakage as declared.`` () =
        Nu.init ()
        let bodyJoint =
            AetherBodyJoint
                { CreateBodyJoint = fun _ _ body body2 ->
                    let anchor = nkast.Aether.Physics2D.Common.Vector2.Zero
                    let joint = nkast.Aether.Physics2D.Dynamics.Joints.DistanceJoint (body, body2, anchor, anchor)
                    joint.Length <- 0.1f // much shorter than the bodies are apart so that a breakable joint breaks
                    joint }
        jointExistsAndBreaksAsDeclared (AetherPhysicsEngine.make v3Zero) bodyJoint
