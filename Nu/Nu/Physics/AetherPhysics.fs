// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open tainicom.Aether.Physics2D
open tainicom.Aether.Physics2D.Dynamics
open tainicom.Aether.Physics2D.Dynamics.Contacts
open tainicom.Aether.Physics2D.Dynamics.Joints
open Prime
open Nu

/// Tracks Aether physics bodies by their PhysicsIds.
type internal AetherBodyDictionary = OrderedDictionary<PhysicsId, Vector3 option * Dynamics.Body>

/// Tracks Aether physics joints by their PhysicsIds.
type internal AetherJointDictionary = OrderedDictionary<PhysicsId, Dynamics.Joints.Joint>

/// The Aether 2d implementation of PhysicsEngine.
type [<ReferenceEquality>] AetherPhysicsEngine =
    private
        { PhysicsContext : Dynamics.World
          Bodies : AetherBodyDictionary
          Joints : AetherJointDictionary
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          mutable RebuildingHack : bool }

    static member private toPixel value =
        value * Constants.Physics.PhysicsToPixelRatio

    static member private toPhysics value =
        value * Constants.Physics.PixelToPhysicsRatio

    static member private toPixelV3 (v2 : Common.Vector2) =
        Vector3 (AetherPhysicsEngine.toPixel v2.X, AetherPhysicsEngine.toPixel v2.Y, 0.0f)

    static member private toPhysicsV2 (v3 : Vector3) =
        Common.Vector2 (AetherPhysicsEngine.toPhysics v3.X, AetherPhysicsEngine.toPhysics v3.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = AetherPhysicsEngine.toPhysics value
        value - Settings.PolygonRadius * 2.0f

    static member private toPhysicsPolygonRadius value =
        let value = AetherPhysicsEngine.toPhysics value
        value - Settings.PolygonRadius

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    static member private toPhysicsDensity bodyWeight =
        match bodyWeight with
        | Density density -> density
        | Mass _ ->
            Log.debugOnce "Currently 2D physics only supports BodyWeight in terms of Density; using Density = 1.0f."
            1.0f

    static member private handleCollision
        physicsEngine (bodyShape : Dynamics.Fixture) (bodyShape2 : Dynamics.Fixture) (contact : Dynamics.Contacts.Contact) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyCollisionMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeSourceInternal
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeSourceInternal
              Normal = Vector3 (normal.X, normal.Y, 0.0f)
              Speed = contact.TangentSpeed * Constants.Physics.PhysicsToPixelRatio }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        physicsEngine.IntegrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        physicsEngine (bodyShape : Dynamics.Fixture) (bodyShape2 : Dynamics.Fixture) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeSourceInternal
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeSourceInternal }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private getBodyContacts (physicsId : PhysicsId) physicsEngine =
        let (_, body) = physicsEngine.Bodies.[physicsId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while notNull current do
            contacts.Add current.Contact
            current <- current.Next
        Array.ofSeq contacts

    static member private configureBodyShapeProperties bodyProperties bodyShapePropertiesOpt (bodyShape : Fixture) =
        match bodyShapePropertiesOpt with
        | Some bodyShapeProperties ->
            bodyShape.Friction <- match bodyShapeProperties.FrictionOpt with Some f -> f | None -> bodyProperties.Friction
            bodyShape.Restitution <- match bodyShapeProperties.RestitutionOpt with Some r -> r | None -> bodyProperties.Restitution
            bodyShape.CollisionCategories <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> enum<Category> cc | None -> enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> enum<Category> cm | None -> enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- match bodyShapeProperties.SensorOpt with Some sensor -> sensor | None -> bodyProperties.Sensor
            bodyShape
        | None ->
            bodyShape.Friction <- bodyProperties.Friction
            bodyShape.Restitution <- bodyProperties.Restitution
            bodyShape.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- bodyProperties.Sensor
            bodyShape

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.Awake <- bodyProperties.Awake
        body.SleepingAllowed <- not bodyProperties.AwakeAlways
        body.Enabled <- bodyProperties.Enabled
        body.Position <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center
        body.Rotation <- -bodyProperties.Rotation.RollPitchYaw.Z
        body.SetFriction bodyProperties.Friction
        body.SetRestitution bodyProperties.Restitution
        body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.FixedRotation
        body.IgnoreGravity <- true // we do all gravity processing ourselves due to: https://github.com/tainicom/Aether.Physics2D/issues/85#issuecomment-716051707
        body.SetCollisionCategories (enum<Category> bodyProperties.CollisionCategories)
        body.SetCollidesWith (enum<Category> bodyProperties.CollisionMask)
        body.BodyType <- AetherPhysicsEngine.toPhysicsBodyType bodyProperties.BodyType
        body.IgnoreCCD <- bodyProperties.IgnoreCCD
        body.SetIsSensor bodyProperties.Sensor

    static member private attachBoxBody sourceSimulant (bodyProperties : BodyProperties) (bodyBox : BodyBox) (body : Body) =
        let bodyShape =
            body.CreateRectangle
                (AetherPhysicsEngine.toPhysicsPolygonDiameter bodyBox.Size.X,
                 AetherPhysicsEngine.toPhysicsPolygonDiameter bodyBox.Size.Y,
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.BodyWeight,
                 AetherPhysicsEngine.toPhysicsV2 bodyBox.Center)
        bodyShape.Tag <-
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyBox.PropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBox.PropertiesOpt bodyShape

    static member private attachBodySphere sourceSimulant (bodyProperties : BodyProperties) (bodySphere : BodySphere) (body : Body) =
        let bodyShape =
            body.CreateCircle
                (AetherPhysicsEngine.toPhysicsPolygonRadius bodySphere.Radius,
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.BodyWeight,
                 AetherPhysicsEngine.toPhysicsV2 bodySphere.Center)
        bodyShape.Tag <-
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodySphere.PropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodySphere.PropertiesOpt bodyShape

    static member private attachBodyCapsule sourceSimulant (bodyProperties : BodyProperties) (bodyCapsule : BodyCapsule) (body : Body) =
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter bodyCapsule.Height
        let endRadius = AetherPhysicsEngine.toPhysicsPolygonRadius bodyCapsule.Radius
        let density = AetherPhysicsEngine.toPhysicsDensity bodyProperties.BodyWeight
        let center = AetherPhysicsEngine.toPhysicsV2 bodyCapsule.Center
        let rectangle = Common.PolygonTools.CreateRectangle (endRadius * 0.9f, height * 0.5f, center, 0.0f) // scaled in the capsule's box to stop corner sticking.
        let list = List<Common.Vertices> ()
        list.Add rectangle
        let bodyShapes = body.CreateCompoundPolygon (list, density)
        let bodyShapeTop = body.CreateCircle (endRadius, density, Common.Vector2 (0.0f, height * 0.5f) + center)
        let bodyShapeBottom = body.CreateCircle (endRadius, density, Common.Vector2 (0.0f, 0.0f - height * 0.5f) + center)
        bodyShapes.Add bodyShapeTop
        bodyShapes.Add bodyShapeBottom
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { Simulant = sourceSimulant
                  BodyId = bodyProperties.BodyId
                  ShapeId = match bodyCapsule.PropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyCapsule.PropertiesOpt bodyShape |> ignore
        Array.ofSeq bodyShapes

    static member private attachBodyBoxRounded sourceSimulant (bodyProperties : BodyProperties) (bodyBoxRounded : BodyBoxRounded) (body : Body) =
        let width = AetherPhysicsEngine.toPhysicsPolygonDiameter bodyBoxRounded.Size.X
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter bodyBoxRounded.Size.Y
        let radius = AetherPhysicsEngine.toPhysicsPolygonRadius bodyBoxRounded.Radius
        let center = AetherPhysicsEngine.toPhysicsV2 bodyBoxRounded.Center
        let boxVerticalWidth = width - radius * 2.0f
        let boxHorizontalHeight = height - radius * 2.0f
        let density = AetherPhysicsEngine.toPhysicsDensity bodyProperties.BodyWeight
        let rectangleV = Common.PolygonTools.CreateRectangle (boxVerticalWidth * 0.5f, height * 0.5f * 0.9f, center, 0.0f) // scaled in height to stop corner sticking
        let rectangleH = Common.PolygonTools.CreateRectangle (width * 0.5f * 0.9f, boxHorizontalHeight * 0.5f, center, 0.0f) // scaled in width to stop corner sticking
        let list = List<Common.Vertices> ()
        list.Add rectangleV
        list.Add rectangleH
        let bodyShapes = body.CreateCompoundPolygon (list, density)
        let bodyShapeTopLeft =      body.CreateCircle (radius, density, Common.Vector2 (-width * 0.5f + radius, +height * 0.5f - radius) + center)
        let bodyShapeTopRight =     body.CreateCircle (radius, density, Common.Vector2 (+width * 0.5f - radius, +height * 0.5f - radius) + center)
        let bodyShapeBottomLeft =   body.CreateCircle (radius, density, Common.Vector2 (-width * 0.5f + radius, -height * 0.5f + radius) + center)
        let bodyShapeBottomRight =  body.CreateCircle (radius, density, Common.Vector2 (+width * 0.5f - radius, -height * 0.5f + radius) + center)
        bodyShapes.Add bodyShapeTopLeft
        bodyShapes.Add bodyShapeTopRight
        bodyShapes.Add bodyShapeBottomLeft
        bodyShapes.Add bodyShapeBottomRight
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { Simulant = sourceSimulant
                  BodyId = bodyProperties.BodyId
                  ShapeId = match bodyBoxRounded.PropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBoxRounded.PropertiesOpt bodyShape |> ignore
        Array.ofSeq bodyShapes

    static member private attachBodyPolygon sourceSimulant bodyProperties bodyPolygon (body : Body) =
        let vertices =
            bodyPolygon.Vertices |>
            Array.map (fun vertex -> vertex + bodyPolygon.Center) |>
            Array.map AetherPhysicsEngine.toPhysicsV2
        let bodyShape =
            body.CreatePolygon
                (Common.Vertices vertices,
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.BodyWeight)
        bodyShape.Tag <-
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyPolygon.PropertiesOpt with Some p -> p.BodyShapeId | None -> 0UL }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyPolygon.PropertiesOpt bodyShape

    static member private attachBodyShapes sourceSimulant bodyProperties bodyShapes (body : Body) =
        let list = List ()
        for bodyShape in bodyShapes do
            let bodyShapes = AetherPhysicsEngine.attachBodyShape sourceSimulant bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape sourceSimulant bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | BodyEmpty -> [||]
        | BodyBox bodyBox -> AetherPhysicsEngine.attachBoxBody sourceSimulant bodyProperties bodyBox body |> Array.singleton
        | BodySphere bodySphere -> AetherPhysicsEngine.attachBodySphere sourceSimulant bodyProperties bodySphere body |> Array.singleton
        | BodyCapsule bodyCapsule -> AetherPhysicsEngine.attachBodyCapsule sourceSimulant bodyProperties bodyCapsule body |> Array.ofSeq
        | BodyBoxRounded bodyBoxRounded -> AetherPhysicsEngine.attachBodyBoxRounded sourceSimulant bodyProperties bodyBoxRounded body |> Array.ofSeq
        | BodyPolygon bodyPolygon -> AetherPhysicsEngine.attachBodyPolygon sourceSimulant bodyProperties bodyPolygon body |> Array.singleton
        | BodyShapes bodyShapes -> AetherPhysicsEngine.attachBodyShapes sourceSimulant bodyProperties bodyShapes body
        
    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let sourceSimulant = createBodyMessage.SourceSimulant
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = -bodyProperties.Rotation.RollPitchYaw.Z
        let bodySource = { Simulant = sourceSimulant; BodyId = bodyProperties.BodyId }

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodySource

        // configure body
        AetherPhysicsEngine.configureBodyProperties bodyProperties body

        // attach body shape
        AetherPhysicsEngine.attachBodyShape sourceSimulant bodyProperties bodyProperties.BodyShape body |> ignore

        // listen for collisions
        body.add_OnCollision (fun fn fn2 collision -> AetherPhysicsEngine.handleCollision physicsEngine fn fn2 collision)

        // listen for separations
        // TODO: P1: use the contact variable as well?
        body.add_OnSeparation (fun fn fn2 _ -> AetherPhysicsEngine.handleSeparation physicsEngine fn fn2)

        // attempt to add the body
        if not (physicsEngine.Bodies.TryAdd ({ SourceId = createBodyMessage.SourceId; CorrelationId = bodyProperties.BodyId }, (bodyProperties.GravityOpt, body))) then
            Log.debug ("Could not add body via '" + scstring bodyProperties + "'.")

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun bodyProperties ->
                let createBodyMessage =
                    { SourceSimulant = createBodiesMessage.SourceSimulant
                      SourceId = createBodiesMessage.SourceId
                      BodyProperties = bodyProperties }
                AetherPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        let physicsId = destroyBodyMessage.PhysicsId
        match physicsEngine.Bodies.TryGetValue physicsId with
        | (true, (_, body)) ->
            physicsEngine.Bodies.Remove physicsId |> ignore
            physicsEngine.PhysicsContext.Remove body
        | (false, _) ->
            if not physicsEngine.RebuildingHack then
                Log.debug ("Could not destroy non-existent body with PhysicsId = " + scstring physicsId + "'.")

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun physicsId ->
            let destroyBodyMessage : DestroyBodyMessage = { SourceSimulant = destroyBodiesMessage.SourceSimulant; PhysicsId = physicsId }
            AetherPhysicsEngine.destroyBody destroyBodyMessage physicsEngine)
            destroyBodiesMessage.PhysicsIds

    static member private createJoint (createJointMessage : CreateJointMessage) physicsEngine =
        match createJointMessage.JointProperties.JointDevice with
        | JointEmpty ->
            ()
        | JointAngle jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, (_, body)), (true, (_, body2))) ->
                let joint = JointFactory.CreateAngleJoint (physicsEngine.PhysicsContext, body, body2)
                joint.TargetAngle <- -jointAngle.TargetAngle
                joint.Softness <- jointAngle.Softness
            | (_, _) -> Log.debug "Could not set create a joint for one or more non-existent bodies."
        | JointDistance jointDistance ->
            match (physicsEngine.Bodies.TryGetValue jointDistance.TargetId, physicsEngine.Bodies.TryGetValue jointDistance.TargetId2) with
            | ((true, (_, body)), (true, (_, body2))) ->
                let joint = JointFactory.CreateDistanceJoint (physicsEngine.PhysicsContext, body, body2, AetherPhysicsEngine.toPhysicsV2 jointDistance.Anchor, AetherPhysicsEngine.toPhysicsV2 jointDistance.Anchor2)
                joint.Length <- AetherPhysicsEngine.toPhysics jointDistance.Length
                joint.Frequency <- jointDistance.Frequency
            | (_, _) -> Log.debug "Could not set create a joint for one or more non-existent bodies."
        | _ -> failwithnie ()

    static member private createJoints (createJointsMessage : CreateJointsMessage) physicsEngine =
        List.iter
            (fun jointProperties ->
                let createJointMessage =
                    { SourceSimulant = createJointsMessage.SourceSimulant
                      SourceId = createJointsMessage.SourceId
                      JointProperties = jointProperties }
                AetherPhysicsEngine.createJoint createJointMessage physicsEngine)
            createJointsMessage.JointsProperties

    static member private destroyJoint (destroyJointMessage : DestroyJointMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue destroyJointMessage.PhysicsId with
        | (true, joint) ->
            physicsEngine.Joints.Remove destroyJointMessage.PhysicsId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) ->
            if not physicsEngine.RebuildingHack then
                Log.debug ("Could not destroy non-existent joint with PhysicsId = " + scstring destroyJointMessage.PhysicsId + "'.")

    static member private destroyJoints (destroyJointsMessage : DestroyJointsMessage) physicsEngine =
        List.iter (fun physicsId ->
            let destroyJointMessage = { SourceSimulant = destroyJointsMessage.SourceSimulant; PhysicsId = physicsId }
            AetherPhysicsEngine.destroyJoint destroyJointMessage physicsEngine)
            destroyJointsMessage.PhysicsIds

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.PhysicsId with
        | (true, (_, body)) -> body.Enabled <- setBodyEnabledMessage.Enabled
        | (false, _) -> Log.debug ("Could not set enabled of non-existent body with PhysicsId = " + scstring setBodyEnabledMessage.PhysicsId + "'.")

    static member private setBodyPosition (setBodyPositionMessage : SetBodyPositionMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyPositionMessage.PhysicsId with
        | (true, (_, body)) -> body.Position <- AetherPhysicsEngine.toPhysicsV2 setBodyPositionMessage.Position
        | (false, _) -> Log.debug ("Could not set position of non-existent body with PhysicsId = " + scstring setBodyPositionMessage.PhysicsId + "'.")

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.PhysicsId with
        | (true, (_, body)) -> body.Rotation <- -setBodyRotationMessage.Rotation.RollPitchYaw.Z
        | (false, _) -> Log.debug ("Could not set rotation of non-existent body with PhysicsId = " + scstring setBodyRotationMessage.PhysicsId + "'.")

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.PhysicsId with
        | (true, (_, body)) -> body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> Log.debug ("Could not set linear velocity of non-existent body with PhysicsId = " + scstring setBodyLinearVelocityMessage.PhysicsId + "'.")

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.PhysicsId with
        | (true, (_, body)) -> body.ApplyLinearImpulse (AetherPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
        | (false, _) -> Log.debug ("Could not apply linear impulse to non-existent body with PhysicsId = " + scstring applyBodyLinearImpulseMessage.PhysicsId + "'.")

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.PhysicsId with
        | (true, (_, body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity.X
        | (false, _) -> Log.debug ("Could not set angular velocity of non-existent body with PhysicsId = " + scstring setBodyAngularVelocityMessage.PhysicsId + "'.")

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.PhysicsId with
        | (true, (_, body)) -> body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse.X)
        | (false, _) -> Log.debug ("Could not apply angular impulse to non-existent body with PhysicsId = " + scstring applyBodyAngularImpulseMessage.PhysicsId + "'.")

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.PhysicsId with
        | (true, (_, body)) -> body.ApplyForce (AetherPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force)
        | (false, _) -> Log.debug ("Could not apply force to non-existent body with PhysicsId = " + scstring applyBodyForceMessage.PhysicsId + "'.")

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.PhysicsId with
        | (true, (_, body)) -> body.ApplyTorque (applyBodyTorqueMessage.Torque.X)
        | (false, _) -> Log.debug ("Could not apply torque to non-existent body with PhysicsId = " + scstring applyBodyTorqueMessage.PhysicsId + "'.")

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> AetherPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> AetherPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> AetherPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> AetherPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateJointMessage createJointMessage -> AetherPhysicsEngine.createJoint createJointMessage physicsEngine
        | CreateJointsMessage createJointsMessage -> AetherPhysicsEngine.createJoints createJointsMessage physicsEngine
        | DestroyJointMessage destroyJointMessage -> AetherPhysicsEngine.destroyJoint destroyJointMessage physicsEngine
        | DestroyJointsMessage destroyJointsMessage -> AetherPhysicsEngine.destroyJoints destroyJointsMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> AetherPhysicsEngine.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyPositionMessage setBodyPositionMessage -> AetherPhysicsEngine.setBodyPosition setBodyPositionMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> AetherPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> AetherPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> AetherPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> AetherPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> AetherPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> AetherPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> AetherPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- AetherPhysicsEngine.toPhysicsV2 gravity
        | RebuildPhysicsHackMessage ->
            physicsEngine.RebuildingHack <- true
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private handlePhysicsMessages physicsMessages physicsEngine =
        for physicsMessage in physicsMessages do
            AetherPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage
        physicsEngine.RebuildingHack <- false

    static member private createIntegrationMessages physicsEngine =
        // NOTE: P1: We should really be querying these bodies from the physics engine's internally-maintained
        // awake-body list for better performance. It's quite suboptimal to have to iterate through all bodies!
        // Note also that I tried building Farseer with #define USE_AWAKE_BODY_SET so we can query from that
        // AwakeBodyList, but there are compilation errors that, when I tried to fix, broke the whole system.
        for body in physicsEngine.PhysicsContext.BodyList do
            if body.Awake && body.BodyType <> Dynamics.BodyType.Static then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodySource = body.Tag :?> BodySourceInternal
                          Center = AetherPhysicsEngine.toPixelV3 body.Position
                          Rotation = (v3 0.0f 0.0f -body.Rotation).RollPitchYaw
                          LinearVelocity = AetherPhysicsEngine.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 body.AngularVelocity 0.0f 0.0f }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    static member private applyGravity physicsStepAmount physicsEngine =
        for (gravityOpt, body) in physicsEngine.Bodies.Values do
            if  body.BodyType = Dynamics.BodyType.Dynamic then
                let gravity =
                    match gravityOpt with
                    | Some gravity -> AetherPhysicsEngine.toPhysicsV2 gravity
                    | None -> physicsEngine.PhysicsContext.Gravity
                body.LinearVelocity <- body.LinearVelocity + physicsStepAmount * gravity

    /// Make a physics engine.
    static member make imperative gravity =
        let config = if imperative then Imperative else Functional
        let physicsEngine =
            { PhysicsContext = World (AetherPhysicsEngine.toPhysicsV2 gravity)
              Bodies = AetherBodyDictionary (HashIdentity.FromFunctions PhysicsId.hash PhysicsId.equals)
              Joints = AetherJointDictionary (HashIdentity.FromFunctions PhysicsId.hash PhysicsId.equals)
              PhysicsMessages = UList.makeEmpty config
              IntegrationMessages = List<IntegrationMessage> ()
              RebuildingHack = false }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.BodyExists physicsId =
            physicsEngine.Bodies.ContainsKey physicsId

        member physicsEngine.GetBodyContactNormals physicsId =
            AetherPhysicsEngine.getBodyContacts physicsId physicsEngine |>
            Array.map (fun (contact : Contact) -> let normal = fst (contact.GetWorldManifold ()) in Vector3 (normal.X, normal.Y, 0.0f)) |>
            Array.toList

        member physicsEngine.GetBodyLinearVelocity physicsId =
            let (_, body) = physicsEngine.Bodies.[physicsId]
            AetherPhysicsEngine.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyToGroundContactNormals physicsId =
            List.filter
                (fun normal ->
                    let theta = Vector2.Dot (normal.V2, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
                    theta < Math.PI * 0.25)
                ((physicsEngine :> PhysicsEngine).GetBodyContactNormals physicsId)

        member physicsEngine.GetBodyToGroundContactNormalOpt physicsId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals physicsId
            match groundNormals with
            | [] -> None
            | _ ->
                let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyToGroundContactTangentOpt physicsId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt physicsId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.IsBodyOnGround physicsId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals physicsId
            List.notEmpty groundNormals

        member physicsEngine.PopMessages () =
            let messages = physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = UList.makeEmpty (UList.getConfig physicsEngine.PhysicsMessages) }
            (messages, physicsEngine :> PhysicsEngine)

        member physicsEngine.ClearMessages () =
            let physicsEngine = { physicsEngine with PhysicsMessages = UList.makeEmpty (UList.getConfig physicsEngine.PhysicsMessages) }
            physicsEngine :> PhysicsEngine

        member physicsEngine.EnqueueMessage physicsMessage =
#if HANDLE_PHYSICS_MESSAGES_IMMEDIATE
            AetherPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage
            physicsEngine
#else
            let physicsMessages = UList.add physicsMessage physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = physicsMessages }
            physicsEngine :> PhysicsEngine
#endif

        member physicsEngine.Integrate stepTime physicsMessages =
            AetherPhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            let physicsStepAmount =
                match (Constants.GameTime.DesiredFrameRate, stepTime) with
                | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames
                | (DynamicFrameRate _, ClockTime secs) -> secs
                | (_, _) -> failwithumf ()
            AetherPhysicsEngine.applyGravity physicsStepAmount physicsEngine
            physicsEngine.PhysicsContext.Step physicsStepAmount
            AetherPhysicsEngine.createIntegrationMessages physicsEngine
            let integrationMessages = SegmentedArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages