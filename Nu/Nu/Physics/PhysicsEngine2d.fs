// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open nkast.Aether.Physics2D
open nkast.Aether.Physics2D.Dynamics
open nkast.Aether.Physics2D.Dynamics.Contacts
open nkast.Aether.Physics2D.Dynamics.Joints
open Prime

#nowarn "44" // ignore aether deprecation warnings

/// The 2d implementation of PhysicsEngine in terms of Aether Physics.
type [<ReferenceEquality>] PhysicsEngine2d =
    private
        { PhysicsContext : Dynamics.World
          Bodies : Dictionary<BodyId, Vector3 option * Dynamics.Body>
          Joints : Dictionary<BodyJointId, Dynamics.Joints.Joint>
          IntegrationMessages : IntegrationMessage List
          CollisionHandler : OnCollisionEventHandler
          SeparationHandler : OnSeparationEventHandler }

    static member private toPixel value =
        value * Constants.Physics.PhysicsToPixelRatio

    static member private toPhysics value =
        value * Constants.Physics.PixelToPhysicsRatio

    static member private toPixelV3 (v2 : Common.Vector2) =
        Vector3 (PhysicsEngine2d.toPixel v2.X, PhysicsEngine2d.toPixel v2.Y, 0.0f)

    static member private toPhysicsV2 (v3 : Vector3) =
        Common.Vector2 (PhysicsEngine2d.toPhysics v3.X, PhysicsEngine2d.toPhysics v3.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = PhysicsEngine2d.toPhysics value
        max Settings.PolygonRadius (value - Settings.PolygonRadius * 2.0f)

    static member private toPhysicsPolygonRadius value =
        let value = PhysicsEngine2d.toPhysics value
        max Settings.PolygonRadius (value - Settings.PolygonRadius)

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | KinematicCharacter -> Log.infoOnce "KinematicCharacter not supported by PhysicsEngine2d. Using Kinematic configuration instead."; Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic
        | DynamicCharacter -> Log.infoOnce "DynamicCharacter not supported by PhysicsEngine2d. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic

    static member private handleCollision
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact)
        (integrationMessages : IntegrationMessage List) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyCollisionMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex
              Normal = Vector3 (normal.X, normal.Y, 0.0f) }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        integrationMessages.Add integrationMessage
        let bodyCollisionMessage2 =
            { BodyShapeSource = bodyCollisionMessage.BodyShapeSource2
              BodyShapeSource2 = bodyCollisionMessage.BodyShapeSource
              Normal = -bodyCollisionMessage.Normal }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage2
        integrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (integrationMessages : IntegrationMessage List) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex }
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
        | None ->
            bodyShape.Friction <- bodyProperties.Friction
            bodyShape.Restitution <- bodyProperties.Restitution
            bodyShape.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- bodyProperties.Sensor

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.SleepingAllowed <- bodyProperties.SleepingAllowed
        body.Enabled <- bodyProperties.Enabled
        body.Position <- PhysicsEngine2d.toPhysicsV2 bodyProperties.Center
        body.Rotation <- bodyProperties.Rotation.RollPitchYaw.Z
        body.SetFriction bodyProperties.Friction
        body.SetRestitution bodyProperties.Restitution
        body.LinearVelocity <- PhysicsEngine2d.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        body.IgnoreGravity <- true // we do all gravity processing ourselves due to: https://github.com/nkast/Aether.Physics2D/issues/85#issuecomment-716051707
        body.IgnoreCCD <- match bodyProperties.CollisionDetection with Discontinuous -> true | Continuous _ -> false
        body.SetCollisionCategories (enum<Category> bodyProperties.CollisionCategories)
        body.SetCollidesWith (enum<Category> bodyProperties.CollisionMask)
        body.BodyType <- PhysicsEngine2d.toPhysicsBodyType bodyProperties.BodyType
        body.SetIsSensor bodyProperties.Sensor

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (boxShape : BoxShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity boxShape.TransformOpt
        let width = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.X * transform.Scale.X)
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.Y * transform.Scale.Y)
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let shape = body.CreateRectangle (width, height, density, offset)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match boxShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties boxShape.PropertiesOpt shape
        shape

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : SphereShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity sphereShape.TransformOpt
        let radius = PhysicsEngine2d.toPhysicsPolygonRadius (sphereShape.Radius * transform.Scale.X)
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (MathF.PI * radius * radius)
        let shape = body.CreateCircle (radius, density, offset)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match sphereShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties sphereShape.PropertiesOpt shape
        shape

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : CapsuleShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity capsuleShape.TransformOpt
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (capsuleShape.Height * transform.Scale.Y)
        let endRadius = PhysicsEngine2d.toPhysicsPolygonRadius (capsuleShape.Radius * transform.Scale.Y)
        let skinnyScalar = 0.9f // scales in the capsule's width to stop corner sticking.
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (endRadius * skinnyScalar * height * 0.5f + MathF.PI * endRadius * endRadius)
        let center = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let rectangle = Common.PolygonTools.CreateRectangle (endRadius * skinnyScalar, height * 0.5f, center, 0.0f)
        let list = List<Common.Vertices> ()
        list.Add rectangle
        let bodyShapes = body.CreateCompoundPolygon (list, density)
        let bodyShapeTop = body.CreateCircle (endRadius, density * 0.5f, Common.Vector2 (0.0f, height * 0.5f) + center)
        let bodyShapeBottom = body.CreateCircle (endRadius, density * 0.5f, Common.Vector2 (0.0f, 0.0f - height * 0.5f) + center)
        bodyShapes.Add bodyShapeTop
        bodyShapes.Add bodyShapeBottom
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match capsuleShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties capsuleShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : BoxRoundedShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (a : Affine) -> let mutable t = a in t.Matrix) m4Identity boxRoundedShape.TransformOpt
        let width = PhysicsEngine2d.toPhysicsPolygonDiameter (boxRoundedShape.Size.X * transform.Scale.X)
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (boxRoundedShape.Size.Y * transform.Scale.Y)
        let radius = PhysicsEngine2d.toPhysicsPolygonRadius (boxRoundedShape.Radius * transform.Scale.X)
        let center = PhysicsEngine2d.toPhysicsV2 transform.Translation
        let boxVerticalWidth = width - radius * 2.0f
        let boxHorizontalHeight = height - radius * 2.0f
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let rectangleV = Common.PolygonTools.CreateRectangle (boxVerticalWidth * 0.5f, height * 0.5f * 0.9f, center, 0.0f) // scaled in height to stop corner sticking
        let rectangleH = Common.PolygonTools.CreateRectangle (width * 0.5f * 0.9f, boxHorizontalHeight * 0.5f, center, 0.0f) // scaled in width to stop corner sticking
        let list = List<Common.Vertices> ()
        list.Add rectangleV
        list.Add rectangleH
        let bodyShapes =            body.CreateCompoundPolygon (list, density)
        let bodyShapeTopLeft =      body.CreateCircle (radius, density * 0.25f, Common.Vector2 (-width * 0.5f + radius, +height * 0.5f - radius) + center)
        let bodyShapeTopRight =     body.CreateCircle (radius, density * 0.25f, Common.Vector2 (+width * 0.5f - radius, +height * 0.5f - radius) + center)
        let bodyShapeBottomLeft =   body.CreateCircle (radius, density * 0.25f, Common.Vector2 (-width * 0.5f + radius, -height * 0.5f + radius) + center)
        let bodyShapeBottomRight =  body.CreateCircle (radius, density * 0.25f, Common.Vector2 (+width * 0.5f - radius, -height * 0.5f + radius) + center)
        bodyShapes.Add bodyShapeTopLeft
        bodyShapes.Add bodyShapeTopRight
        bodyShapes.Add bodyShapeBottomLeft
        bodyShapes.Add bodyShapeBottomRight
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match boxRoundedShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties boxRoundedShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBodyConvexHull bodySource bodyProperties (pointsShape : PointsShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity pointsShape.TransformOpt
        let vertices = Array.zeroCreate pointsShape.Points.Length
        for i in 0 .. dec pointsShape.Points.Length do
            vertices.[i] <- PhysicsEngine2d.toPhysicsV2 (Vector3.Transform (pointsShape.Points.[i], transform))
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = vertices |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let bodyShape = body.CreatePolygon (Common.Vertices vertices, density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match pointsShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties pointsShape.PropertiesOpt bodyShape
        bodyShape

    static member private attachBodyTriangles bodySource bodyProperties (geometryShape : GeometryShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity geometryShape.TransformOpt
        let vertices = Array.zeroCreate geometryShape.Vertices.Length
        for i in 0 .. dec geometryShape.Vertices.Length do
            vertices.[i] <- PhysicsEngine2d.toPhysicsV2 (Vector3.Transform (geometryShape.Vertices.[i], transform))
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = vertices |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let triangles = vertices |> Array.chunkBySize 3 |> Array.map Common.Vertices |> List
        let bodyShapes = body.CreateCompoundPolygon (triangles, density)
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match geometryShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties geometryShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachGeometryShape bodySource bodyProperties (geometryShape : GeometryShape) (body : Body) =
        if geometryShape.Convex then
            let pointsShape = { Points = geometryShape.Vertices; TransformOpt = geometryShape.TransformOpt; PropertiesOpt = geometryShape.PropertiesOpt }
            PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties pointsShape body |> Array.singleton
        else PhysicsEngine2d.attachBodyTriangles bodySource bodyProperties geometryShape body

    static member private attachBodyShapes bodySource bodyProperties bodyShapes (body : Body) =
        let list = List ()
        for bodyShape in bodyShapes do
            let bodyShapes = PhysicsEngine2d.attachBodyShape bodySource bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape bodySource bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | EmptyShape -> [||]
        | BoxShape boxShape -> PhysicsEngine2d.attachBoxBody bodySource bodyProperties boxShape body |> Array.singleton
        | SphereShape sphereShape -> PhysicsEngine2d.attachSphereShape bodySource bodyProperties sphereShape body |> Array.singleton
        | CapsuleShape capsuleShape -> PhysicsEngine2d.attachCapsuleShape bodySource bodyProperties capsuleShape body |> Array.ofSeq
        | BoxRoundedShape boxRoundedShape -> PhysicsEngine2d.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape body |> Array.ofSeq
        | PointsShape pointsShape -> PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties pointsShape body |> Array.singleton
        | GeometryShape geometryShape -> PhysicsEngine2d.attachGeometryShape bodySource bodyProperties geometryShape body
        | StaticModelShape _ -> [||]
        | StaticModelSurfaceShape _ -> [||]
        | TerrainShape _ -> [||]
        | BodyShapes bodyShapes -> PhysicsEngine2d.attachBodyShapes bodySource bodyProperties bodyShapes body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation.RollPitchYaw.Z

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (PhysicsEngine2d.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodyId

        // configure body
        PhysicsEngine2d.configureBodyProperties bodyProperties body

        // attempt to attach body shape
        try PhysicsEngine2d.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body |> ignore
        with :? ArgumentOutOfRangeException -> ()

        // listen for collisions when observable
        if bodyProperties.ShouldObserve then
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
                PhysicsEngine2d.createBody createBodyMessage physicsEngine)
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
            PhysicsEngine2d.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =
        let bodyJointProperties = createBodyJointMessage.BodyJointProperties
        match bodyJointProperties.BodyJoint with
        | EmptyJoint -> ()
        | AngleJoint jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, (_, body)), (true, (_, body2))) ->
                let joint = JointFactory.CreateAngleJoint (physicsEngine.PhysicsContext, body, body2)
                joint.TargetAngle <- -(jointAngle.AngleMax - jointAngle.AngleMin)
                joint.Softness <- jointAngle.Softness
                joint.BiasFactor <- jointAngle.BiasFactor
                joint.Breakpoint <- jointAngle.BreakImpulseThreshold
            | (_, _) -> Log.debug "Could not create a joint for one or more non-existent bodies."
        | _ -> failwithnie ()

    static member private createBodyJoints (createBodyJointsMessage : CreateBodyJointsMessage) physicsEngine =
        List.iter (fun (bodyJointProperties : BodyJointProperties) ->
            let createBodyJointMessage = { BodyJointSource = createBodyJointsMessage.BodyJointsSource; BodyJointProperties = bodyJointProperties }
            PhysicsEngine2d.createBodyJoint createBodyJointMessage physicsEngine)
            createBodyJointsMessage.BodyJointsProperties

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue destroyBodyJointMessage.BodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove destroyBodyJointMessage.BodyJointId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) -> ()

    static member private destroyBodyJoints (destroyBodyJointsMessage : DestroyBodyJointsMessage) physicsEngine =
        List.iter (fun bodyJointId ->
            PhysicsEngine2d.destroyBodyJoint { BodyJointId = bodyJointId } physicsEngine)
            destroyBodyJointsMessage.BodyJointIds

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, (_, body)) -> body.Enabled <- setBodyEnabledMessage.Enabled
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, (_, body)) ->
            let center = PhysicsEngine2d.toPhysicsV2 setBodyCenterMessage.Center
            if body.Position <> center then
                body.Position <- center
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, (_, body)) ->
            let rotation = setBodyRotationMessage.Rotation.RollPitchYaw.Z
            if body.Rotation <> rotation then
                body.Rotation <- rotation
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (_, body)) -> body.LinearVelocity <- PhysicsEngine2d.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (_, body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity.X
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                body.ApplyLinearImpulse
                    (PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                     PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.Offset)
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.X) then
                body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse.X)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                body.ApplyForce
                    (PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Force,
                     PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Offset)
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.X) then
                body.ApplyTorque (applyBodyTorqueMessage.Torque.X)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private jumpBody (_ : JumpBodyMessage) (_ : PhysicsEngine) =
        () // character body type not yet supported

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine2d.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine2d.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine2d.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine2d.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> PhysicsEngine2d.createBodyJoint createBodyJointMessage physicsEngine
        | CreateBodyJointsMessage createBodyJointsMessage -> PhysicsEngine2d.createBodyJoints createBodyJointsMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> PhysicsEngine2d.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | DestroyBodyJointsMessage destroyBodyJointsMessage -> PhysicsEngine2d.destroyBodyJoints destroyBodyJointsMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngine2d.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngine2d.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine2d.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine2d.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine2d.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine2d.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine2d.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine2d.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngine2d.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> PhysicsEngine2d.jumpBody jumpBodyMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- PhysicsEngine2d.toPhysicsV2 gravity
        | ClearPhysicsMessageInternal ->
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Joints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (_, body) = bodyEntry.Value
            if body.Awake then

                // append transform message
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.Tag :?> BodyId
                          Center = PhysicsEngine2d.toPixelV3 body.Position
                          Rotation = (v3 0.0f 0.0f body.Rotation).RollPitchYaw
                          LinearVelocity = PhysicsEngine2d.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 body.AngularVelocity 0.0f 0.0f }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

                // manually sleep static bodies since aether won't sleep them itself
                if body.BodyType = Dynamics.BodyType.Static then body.Awake <- false

    static member private applyGravity physicsStepAmount physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (gravityOverride, body) = bodyEntry.Value
            if body.BodyType = Dynamics.BodyType.Dynamic then
                let gravity =
                    match gravityOverride with
                    | Some gravity -> PhysicsEngine2d.toPhysicsV2 gravity
                    | None -> physicsEngine.PhysicsContext.Gravity
                body.LinearVelocity <- body.LinearVelocity + gravity * physicsStepAmount

    /// Make a physics engine.
    static member make gravity =
        let integrationMessages = List ()
        let collisionHandler = fun fixture fixture2 collision -> PhysicsEngine2d.handleCollision fixture fixture2 collision integrationMessages
        let separationHandler = fun fixture fixture2 _ -> PhysicsEngine2d.handleSeparation fixture fixture2 integrationMessages
        let physicsEngine =
            { PhysicsContext = World (PhysicsEngine2d.toPhysicsV2 gravity)
              Bodies = Dictionary<BodyId, Vector3 option * Dynamics.Body> (HashIdentity.FromFunctions BodyId.hash BodyId.equals)
              Joints = Dictionary<BodyJointId, Dynamics.Joints.Joint> HashIdentity.Structural
              IntegrationMessages = integrationMessages
              CollisionHandler = collisionHandler
              SeparationHandler = separationHandler }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            PhysicsEngine2d.getBodyContacts bodyId physicsEngine |>
            Array.map (fun (contact : Contact) -> let normal = fst (contact.GetWorldManifold ()) in Vector3 (normal.X, normal.Y, 0.0f)) |>
            Array.toList

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            PhysicsEngine2d.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyAngularVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            v3 body.AngularVelocity 0.0f 0.0f

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            List.filter (fun normal ->
                let theta = Vector2.Dot (normal.V2, Vector2.UnitY) |> acos |> abs
                theta < Constants.Physics.GroundAngleMax)
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

        member physicsEngine.GetBodyGrounded bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            List.notEmpty groundNormals

        member physicsEngine.RayCast (start, stop, collisionCategories, collisionMask, closestOnly) =
            ignore collisionMask // TODO: P1: try to figure out how this variable can / should be used here!
            let results = List ()
            let mutable fractionMin = Single.MaxValue
            let mutable closestOpt = None
            let callback =
                RayCastReportFixtureDelegate (fun fixture point normal fraction ->
                    match fixture.Body.Tag with
                    | :? BodyId as bodyId ->
                        match fixture.Tag with
                        | :? BodyShapeIndex as bodyShapeIndex ->
                            if (int fixture.CollidesWith &&& collisionCategories) <> 0 then
                                let report = (v3 point.X point.Y 0.0f, v3 normal.X normal.Y 0.0f, fraction, bodyShapeIndex, bodyId)
                                if fraction < fractionMin then
                                    fractionMin <- fraction
                                    closestOpt <- Some report
                                results.Add report
                        | _ -> ()
                    | _ -> ()
                    if closestOnly then fraction else 1.0f)
            physicsEngine.PhysicsContext.RayCast
                (callback,
                 Common.Vector2 (start.X, start.Y),
                 Common.Vector2 (stop.X, stop.Y))
            if closestOnly then
                match closestOpt with
                | Some closest -> [|closest|]
                | None -> [||]
            else Array.ofSeq results

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngine2d.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate stepTime =
            let physicsStepAmount =
                match (Constants.GameTime.DesiredFrameRate, stepTime) with
                | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames
                | (DynamicFrameRate _, ClockTime time) -> if time > 0.0f && time < 0.001f then 0.001f elif time > 0.1f then 0.1f else time
                | (_, _) -> failwithumf ()
            if physicsStepAmount > 0.0f then
                PhysicsEngine2d.applyGravity physicsStepAmount physicsEngine
                physicsEngine.PhysicsContext.Step physicsStepAmount
                PhysicsEngine2d.createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine
                let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                physicsEngine.IntegrationMessages.Clear ()
                Some integrationMessages
            else None

        member physicsEngine.CleanUp () =
            ()