// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open tainicom.Aether.Physics2D
open tainicom.Aether.Physics2D.Dynamics
open tainicom.Aether.Physics2D.Dynamics.Contacts
open tainicom.Aether.Physics2D.Dynamics.Joints
open Prime

#nowarn "44" // ignore aether deprecation warnings

/// Tracks Aether physics bodies by their BodyIds.
type internal AetherBodyDictionary = OrderedDictionary<BodyId, Vector3 option * Dynamics.Body>

/// Tracks Aether physics joints by their BodyIds.
type internal AetherJointDictionary = OrderedDictionary<JointId, Dynamics.Joints.Joint>

/// The Aether 2d implementation of PhysicsEngine.
type [<ReferenceEquality>] AetherPhysicsEngine =
    private
        { PhysicsContext : Dynamics.World
          Bodies : AetherBodyDictionary
          Joints : AetherJointDictionary
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          CollisionHandler : OnCollisionEventHandler
          SeparationHandler : OnSeparationEventHandler }

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
        max Settings.PolygonRadius (value - Settings.PolygonRadius * 2.0f)

    static member private toPhysicsPolygonRadius value =
        let value = AetherPhysicsEngine.toPhysics value
        max Settings.PolygonRadius (value - Settings.PolygonRadius)

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    static member private toPhysicsDensity substance =
        match substance with
        | Density density -> density
        | Mass _ ->
            Log.debugOnce "Currently 2D physics supports Substance only in terms of Density; using Density = 1.0f."
            1.0f

    static member private handleCollision
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact)
        (integrationMessages : IntegrationMessage List) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyCollisionMessage =
            { BodyShapeSource = bodyShape.Tag :?> ShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> ShapeIndex
              Normal = Vector3 (normal.X, normal.Y, 0.0f) }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        integrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (integrationMessages : IntegrationMessage List) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.Tag :?> ShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> ShapeIndex }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        integrationMessages.Add integrationMessage

    static member private getBodyContacts (bodyId : BodyId) physicsEngine =
        let (_, body) = physicsEngine.Bodies.[bodyId]
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
        body.SleepingAllowed <- bodyProperties.SleepingAllowed
        body.Enabled <- bodyProperties.Enabled
        body.Position <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center
        body.Rotation <- bodyProperties.Rotation.RollPitchYaw.Z
        body.SetFriction bodyProperties.Friction
        body.SetRestitution bodyProperties.Restitution
        body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        body.IgnoreGravity <- true // we do all gravity processing ourselves due to: https://github.com/tainicom/Aether.Physics2D/issues/85#issuecomment-716051707
        body.IgnoreCCD <- match bodyProperties.CollisionDetection with Discontinuous -> true | Continuous _ -> false
        body.SetCollisionCategories (enum<Category> bodyProperties.CollisionCategories)
        body.SetCollidesWith (enum<Category> bodyProperties.CollisionMask)
        body.BodyType <- AetherPhysicsEngine.toPhysicsBodyType bodyProperties.BodyType
        body.SetIsSensor bodyProperties.Sensor

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (bodyBox : BodyBox) (body : Body) =
        let transform = Option.defaultValue m4Identity bodyBox.TransformOpt
        let shape =
            body.CreateRectangle
                (AetherPhysicsEngine.toPhysicsPolygonDiameter (bodyBox.Size.X * transform.Scale.X),
                 AetherPhysicsEngine.toPhysicsPolygonDiameter (bodyBox.Size.Y * transform.Scale.Y),
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.Substance,
                 AetherPhysicsEngine.toPhysicsV2 transform.Translation)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyBox.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBox.PropertiesOpt shape

    static member private attachBodySphere bodySource (bodyProperties : BodyProperties) (bodySphere : BodySphere) (body : Body) =
        let transform = Option.defaultValue m4Identity bodySphere.TransformOpt
        let shape =
            body.CreateCircle
                (AetherPhysicsEngine.toPhysicsPolygonRadius (bodySphere.Radius * transform.Scale.X),
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.Substance,
                 AetherPhysicsEngine.toPhysicsV2 transform.Translation)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodySphere.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodySphere.PropertiesOpt shape

    static member private attachBodyCapsule bodySource (bodyProperties : BodyProperties) (bodyCapsule : BodyCapsule) (body : Body) =
        let transform = Option.defaultValue m4Identity bodyCapsule.TransformOpt
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter (bodyCapsule.Height * transform.Scale.Y)
        let endRadius = AetherPhysicsEngine.toPhysicsPolygonRadius (bodyCapsule.Radius * transform.Scale.Y)
        let density = AetherPhysicsEngine.toPhysicsDensity bodyProperties.Substance
        let center = AetherPhysicsEngine.toPhysicsV2 transform.Translation
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
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  ShapeIndex = match bodyCapsule.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyCapsule.PropertiesOpt bodyShape |> ignore
        Array.ofSeq bodyShapes

    static member private attachBodyBoxRounded bodySource (bodyProperties : BodyProperties) (bodyBoxRounded : BodyBoxRounded) (body : Body) =
        let transform = Option.defaultValue m4Identity bodyBoxRounded.TransformOpt
        let width = AetherPhysicsEngine.toPhysicsPolygonDiameter (bodyBoxRounded.Size.X * transform.Scale.X)
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter (bodyBoxRounded.Size.Y * transform.Scale.Y)
        let radius = AetherPhysicsEngine.toPhysicsPolygonRadius (bodyBoxRounded.Radius * transform.Scale.X)
        let center = AetherPhysicsEngine.toPhysicsV2 transform.Translation
        let boxVerticalWidth = width - radius * 2.0f
        let boxHorizontalHeight = height - radius * 2.0f
        let density = AetherPhysicsEngine.toPhysicsDensity bodyProperties.Substance
        let rectangleV = Common.PolygonTools.CreateRectangle (boxVerticalWidth * 0.5f, height * 0.5f * 0.9f, center, 0.0f) // scaled in height to stop corner sticking
        let rectangleH = Common.PolygonTools.CreateRectangle (width * 0.5f * 0.9f, boxHorizontalHeight * 0.5f, center, 0.0f) // scaled in width to stop corner sticking
        let list = List<Common.Vertices> ()
        list.Add rectangleV
        list.Add rectangleH
        let bodyShapes =            body.CreateCompoundPolygon (list, density)
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
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  ShapeIndex = match bodyBoxRounded.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBoxRounded.PropertiesOpt bodyShape |> ignore
        Array.ofSeq bodyShapes

    static member private attachBodyConvexHull bodySource bodyProperties (bodyConvexHull : BodyConvexHull) (body : Body) =
        let transform = Option.defaultValue m4Identity bodyConvexHull.TransformOpt
        let vertices = Array.zeroCreate bodyConvexHull.Vertices.Length
        for i in 0 .. dec bodyConvexHull.Vertices.Length do
            vertices.[i] <- AetherPhysicsEngine.toPhysicsV2 (Vector3.Transform (bodyConvexHull.Vertices.[i], transform))
        let bodyShape =
            body.CreatePolygon
                (Common.Vertices vertices,
                 AetherPhysicsEngine.toPhysicsDensity bodyProperties.Substance)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyConvexHull.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties bodyConvexHull.PropertiesOpt bodyShape

    static member private attachBodyShapes bodySource bodyProperties bodyShapes (body : Body) =
        let list = List ()
        for bodyShape in bodyShapes do
            let bodyShapes = AetherPhysicsEngine.attachBodyShape bodySource bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape bodySource bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | BodyEmpty -> [||]
        | BodyBox bodyBox -> AetherPhysicsEngine.attachBoxBody bodySource bodyProperties bodyBox body |> Array.singleton
        | BodySphere bodySphere -> AetherPhysicsEngine.attachBodySphere bodySource bodyProperties bodySphere body |> Array.singleton
        | BodyCapsule bodyCapsule -> AetherPhysicsEngine.attachBodyCapsule bodySource bodyProperties bodyCapsule body |> Array.ofSeq
        | BodyBoxRounded bodyBoxRounded -> AetherPhysicsEngine.attachBodyBoxRounded bodySource bodyProperties bodyBoxRounded body |> Array.ofSeq
        | BodyConvexHull bodyConvexHull -> AetherPhysicsEngine.attachBodyConvexHull bodySource bodyProperties bodyConvexHull body |> Array.singleton
        | BodyTerrain _ -> [||]
        | BodyStaticModel _ -> [||]
        | BodyStaticModelSurface _ -> [||]
        | BodyShapes bodyShapes -> AetherPhysicsEngine.attachBodyShapes bodySource bodyProperties bodyShapes body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation.RollPitchYaw.Z

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodyId

        // configure body
        AetherPhysicsEngine.configureBodyProperties bodyProperties body

        // attempt to attach body shape
        try AetherPhysicsEngine.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body |> ignore
        with :? ArgumentOutOfRangeException -> ()

        // always listen for collisions if not internal body
        if not (bodyId.BodyIndex = Constants.Physics.InternalIndex) then
            body.add_OnCollision physicsEngine.CollisionHandler
            body.add_OnSeparation physicsEngine.SeparationHandler

        // attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        if not (physicsEngine.Bodies.TryAdd (bodyId, (bodyProperties.GravityOverride, body))) then
            Log.debug ("Could not add body for '" + scstring bodyId + "'.")

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                AetherPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (_, body)) ->
            physicsEngine.Bodies.Remove bodyId |> ignore
            physicsEngine.PhysicsContext.Remove body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            AetherPhysicsEngine.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createJoint (createJointMessage : CreateJointMessage) physicsEngine =
        let jointProperties = createJointMessage.JointProperties
        match jointProperties.JointDevice with
        | JointEmpty -> ()
        | JointAngle jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, (_, body)), (true, (_, body2))) ->
                let joint = JointFactory.CreateAngleJoint (physicsEngine.PhysicsContext, body, body2)
                joint.TargetAngle <- -(jointAngle.AngleMax - jointAngle.AngleMin)
                joint.Softness <- jointAngle.Softness
                joint.BiasFactor <- jointAngle.BiasFactor
                joint.Breakpoint <- jointAngle.BreakImpulseThreshold
            | (_, _) -> Log.debug "Could not create a joint for one or more non-existent bodies."
        | _ -> failwithnie ()

    static member private createJoints (createJointsMessage : CreateJointsMessage) physicsEngine =
        List.iter
            (fun (jointProperties : JointProperties) ->
                let createJointMessage =
                    { JointSource = createJointsMessage.JointsSource
                      JointProperties = jointProperties }
                AetherPhysicsEngine.createJoint createJointMessage physicsEngine)
            createJointsMessage.JointsProperties

    static member private destroyJoint (destroyJointMessage : DestroyJointMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue destroyJointMessage.JointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove destroyJointMessage.JointId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) -> ()

    static member private destroyJoints (destroyJointsMessage : DestroyJointsMessage) physicsEngine =
        List.iter (fun jointId ->
            AetherPhysicsEngine.destroyJoint { JointId = jointId } physicsEngine)
            destroyJointsMessage.JointIds

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, (_, body)) -> body.Enabled <- setBodyEnabledMessage.Enabled
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, (_, body)) ->
            body.Position <- AetherPhysicsEngine.toPhysicsV2 setBodyCenterMessage.Center
            do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, (_, body)) ->
            body.Rotation <- setBodyRotationMessage.Rotation.RollPitchYaw.Z
            do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (_, body)) -> body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (_, body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity.X
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (_, body)) ->
            body.ApplyLinearImpulse
                (AetherPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                 AetherPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.Offset)
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (_, body)) -> body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse.X)
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (_, body)) ->
            body.ApplyForce
                (AetherPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force,
                 AetherPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Offset)
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (_, body)) -> body.ApplyTorque (applyBodyTorqueMessage.Torque.X)
        | (false, _) -> ()

    static member private setBodyObservable (setBodyObservableMessage : SetBodyObservableMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyObservableMessage.BodyId with
        | (true, (_, body)) ->
            body.remove_OnCollision physicsEngine.CollisionHandler
            body.remove_OnSeparation physicsEngine.SeparationHandler
            if setBodyObservableMessage.Observable then
                body.add_OnCollision physicsEngine.CollisionHandler
                body.add_OnSeparation physicsEngine.SeparationHandler
        | (false, _) -> ()

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
        | SetBodyCenterMessage setBodyCenterMessage -> AetherPhysicsEngine.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> AetherPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> AetherPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> AetherPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> AetherPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> AetherPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> AetherPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> AetherPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | SetBodyObservableMessage setBodyObservableMessage -> AetherPhysicsEngine.setBodyObservable setBodyObservableMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- AetherPhysicsEngine.toPhysicsV2 gravity
        | ClearPhysicsMessageInternal ->
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private handlePhysicsMessages physicsMessages physicsEngine =
        for physicsMessage in physicsMessages do
            AetherPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage

    static member private createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (_, body) = bodyEntry.Value
            if body.Awake then

                // append transform message
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.Tag :?> BodyId
                          Center = AetherPhysicsEngine.toPixelV3 body.Position
                          Rotation = (v3 0.0f 0.0f body.Rotation).RollPitchYaw
                          LinearVelocity = AetherPhysicsEngine.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 body.AngularVelocity 0.0f 0.0f }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

                // manually sleep static bodies since aether won't sleep them itself
                if body.BodyType = Dynamics.BodyType.Static then body.Awake <- false

    static member private applyGravity physicsStepAmount physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (gravityOverride, body) = bodyEntry.Value
            if  body.BodyType = Dynamics.BodyType.Dynamic then
                let gravity =
                    match gravityOverride with
                    | Some gravity -> AetherPhysicsEngine.toPhysicsV2 gravity
                    | None -> physicsEngine.PhysicsContext.Gravity
                body.LinearVelocity <- body.LinearVelocity + gravity * physicsStepAmount

    /// Make a physics engine.
    static member make imperative gravity =
        let config = if imperative then Imperative else Functional
        let integrationMessages = List ()
        let collisionHandler = fun fixture fixture2 collision -> AetherPhysicsEngine.handleCollision fixture fixture2 collision integrationMessages
        let separationHandler = fun fixture fixture2 _ -> AetherPhysicsEngine.handleSeparation fixture fixture2 integrationMessages
        let physicsEngine =
            { PhysicsContext = World (AetherPhysicsEngine.toPhysicsV2 gravity)
              Bodies = AetherBodyDictionary (HashIdentity.FromFunctions BodyId.hash BodyId.equals)
              Joints = AetherJointDictionary HashIdentity.Structural
              PhysicsMessages = UList.makeEmpty config
              IntegrationMessages = integrationMessages
              CollisionHandler = collisionHandler
              SeparationHandler = separationHandler }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            AetherPhysicsEngine.getBodyContacts bodyId physicsEngine |>
            Array.map (fun (contact : Contact) -> let normal = fst (contact.GetWorldManifold ()) in Vector3 (normal.X, normal.Y, 0.0f)) |>
            Array.toList

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            AetherPhysicsEngine.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            List.filter
                (fun normal ->
                    let theta = Vector2.Dot (normal.V2, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
                    theta < Math.PI * 0.25)
                ((physicsEngine :> PhysicsEngine).GetBodyContactNormals bodyId)

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            match groundNormals with
            | [] -> None
            | _ ->
                let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.IsBodyOnGround bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
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
            AetherPhysicsEngine.createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine
            let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages

        member physicsEngine.CleanUp () =
            ()