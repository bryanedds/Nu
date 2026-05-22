// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open nkast.Aether.Physics2D
open nkast.Aether.Physics2D.Dynamics
open nkast.Aether.Physics2D.Dynamics.Contacts
open nkast.Aether.Physics2D.Dynamics.Joints
open Prime

/// The Aether interface of PhysicsEngineRenderContext.
type AetherPhysicsEngineRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : Start : Vector2 * Stop : Vector2 * Color : Color -> unit
    abstract DrawCircle : Position : Vector2 * Radius : single * Color : Color -> unit

/// The Aether implementation of PhysicsEngine.
and [<ReferenceEquality>] AetherPhysicsEngine =
    private
        { PhysicsContext : Dynamics.World
          Bodies : Dictionary<BodyId, Gravity * Dynamics.Body>
          Joints : Dictionary<BodyJointId, Dynamics.Joints.Joint>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          IntegrationMessages : IntegrationMessage List
          PenetrationHandler : OnCollisionEventHandler
          SeparationHandler : OnSeparationEventHandler
          BreakHandler : Action<Joint, single> }

    static member private toPixel value =
        value * Constants.Engine.Meter2d

    static member private toPhysics value =
        value / Constants.Engine.Meter2d

    static member private toPixelV2 (v2 : Common.Vector2) =
        Vector2 (AetherPhysicsEngine.toPixel v2.X, AetherPhysicsEngine.toPixel v2.Y)

    static member private toPixelV3 (v2 : Common.Vector2) =
        (AetherPhysicsEngine.toPixelV2 v2).V3

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
        | KinematicCharacter -> Log.infoOnce "KinematicCharacter not supported by AetherPhysicsEngine. Using Kinematic configuration instead."; Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic
        | DynamicCharacter -> Log.infoOnce "DynamicCharacter not supported by AetherPhysicsEngine. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic
        | Vehicle -> Log.infoOnce "Vehicle not supported by AetherPhysicsEngine. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic

    static member private handlePenetration
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact)
        (integrationMessages : IntegrationMessage List) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyPenetrationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeTarget = bodyShape2.Tag :?> BodyShapeIndex
              Normal = Vector3 (normal.X, normal.Y, 0.0f) }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        integrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (integrationMessages : IntegrationMessage List) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeTarget = bodyShape2.Tag :?> BodyShapeIndex }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        integrationMessages.Add integrationMessage

    static member private handleBreak
        (joint : Joint)
        (jointError : single)
        (integrationMessages : IntegrationMessage List) =
        let jointBreakPointPixel = AetherPhysicsEngine.toPixel joint.Breakpoint
        let jointErrorPixel = AetherPhysicsEngine.toPixel jointError
        let bodyJointBreakMessage =
            { BodyJointId = joint.Tag :?> BodyJointId
              BreakingPoint = jointBreakPointPixel
              BreakingOverflow = jointErrorPixel - jointBreakPointPixel }
        let integrationMessage = BodyJointBreakMessage bodyJointBreakMessage
        integrationMessages.Add integrationMessage

    static member private getBodyContacts (bodyId : BodyId) physicsEngine =
        let (_, body) = physicsEngine.Bodies[bodyId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while notNull current do
            contacts.Add current.Contact
            current <- current.Next
        Array.ofSeq contacts

    static member private configureBodyShapeProperties (bodyProperties : BodyProperties) bodyShapePropertiesOpt (bodyShape : Fixture) =
        // NOTE: compared to Box2D.NET (Box2D v3), Aether.Physics2D (Box2D v2) only supports int16 instead of int for
        // collision group, and int instead of uint64 for collision categories / mask.
        // NOTE: setting Fixture.IsSensor triggers a cascade of internal behavior we don't want when it's false, so
        // we only set it when it's true.
        match bodyShapePropertiesOpt with
        | Some bodyShapeProperties ->
            bodyShape.Friction <- match bodyShapeProperties.FrictionOpt with Some f -> f | None -> bodyProperties.Friction
            bodyShape.Restitution <- match bodyShapeProperties.RestitutionOpt with Some r -> r | None -> bodyProperties.Restitution
            bodyShape.CollisionGroup <- int16 <| match bodyShapeProperties.CollisionGroupOpt with Some cg -> cg | None -> bodyProperties.CollisionGroup
            bodyShape.CollisionCategories <- match bodyShapeProperties.CollisionCategoriesOpt with Some cc -> enum<Category> (int cc) | None -> enum<Category> (int bodyProperties.CollisionCategories)
            bodyShape.CollidesWith <- match bodyShapeProperties.CollisionMaskOpt with Some cm -> enum<Category> (int cm) | None -> enum<Category> (int bodyProperties.CollisionMask)
            let sensor = match bodyShapeProperties.SensorOpt with Some sensor -> sensor | None -> bodyProperties.Sensor
            if sensor then bodyShape.IsSensor <- sensor
        | None ->
            bodyShape.Friction <- bodyProperties.Friction
            bodyShape.Restitution <- bodyProperties.Restitution
            bodyShape.CollisionGroup <- int16 bodyProperties.CollisionGroup
            bodyShape.CollisionCategories <- enum<Category> (int bodyProperties.CollisionCategories)
            bodyShape.CollidesWith <- enum<Category> (int bodyProperties.CollisionMask)
            if bodyProperties.Sensor then bodyShape.IsSensor <- true

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.BodyType <- AetherPhysicsEngine.toPhysicsBodyType bodyProperties.BodyType // NOTE: BodyType must be set first or other configurations may be ignored!
        body.Enabled <- bodyProperties.Enabled
        body.SleepingAllowed <- bodyProperties.SleepingAllowed
        body.Position <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center
        body.Rotation <- bodyProperties.Rotation.Angle2d
        body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        body.IgnoreGravity <- true // NOTE: body-specific gravity isn't supported by Aether, so we handle gravity ourselves.
        body.IgnoreCCD <- match bodyProperties.CollisionDetection with Discrete -> true | Continuous -> false

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (boxShape : BoxShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity boxShape.TransformOpt
        let width = AetherPhysicsEngine.toPhysicsPolygonDiameter (boxShape.Size.X * transform.Scale.X)
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter (boxShape.Size.Y * transform.Scale.Y)
        let offset = AetherPhysicsEngine.toPhysicsV2 transform.Translation
        let angle = transform.Rotation.Angle2d
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let shape =
            let rectangleVertices = Common.PolygonTools.CreateRectangle (width / 2.0f, height / 2.0f, offset, angle);
            let rectangleShape = Collision.Shapes.PolygonShape (rectangleVertices, density)
            body.CreateFixture rectangleShape
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match boxShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties boxShape.PropertiesOpt shape
        shape

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : SphereShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity sphereShape.TransformOpt
        let radius = AetherPhysicsEngine.toPhysicsPolygonRadius (sphereShape.Radius * transform.Scale.X)
        let offset = AetherPhysicsEngine.toPhysicsV2 transform.Translation
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (MathF.PI * radius * radius)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let shape = body.CreateCircle (radius, density, offset)
        shape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match sphereShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties sphereShape.PropertiesOpt shape
        shape

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : CapsuleShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity capsuleShape.TransformOpt
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter (capsuleShape.Height * transform.Scale.Y)
        let endRadius = AetherPhysicsEngine.toPhysicsPolygonRadius (capsuleShape.Radius * transform.Scale.Y)
        let skinnyScalar = 0.9f // scales in the capsule's width to stop corner sticking.
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (endRadius * skinnyScalar * height * 0.5f + MathF.PI * endRadius * endRadius)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let offset = AetherPhysicsEngine.toPhysicsV2 transform.Translation
        let angle = transform.Rotation.Angle2d
        let rectangle = Common.PolygonTools.CreateRectangle (endRadius * skinnyScalar, height * 0.5f, offset, angle)
        let list = List<Common.Vertices> ()
        list.Add rectangle
        let bodyShapes = body.CreateCompoundPolygon (list, density)
        let circleOffset = Common.Complex.FromAngle angle
        let circleOffset = Common.Complex.Multiply (Common.Vector2 (0.0f, height * 0.5f), ref circleOffset)
        let bodyShapeTop = body.CreateCircle (endRadius, density * 0.5f, circleOffset + offset)
        let bodyShapeBottom = body.CreateCircle (endRadius, density * 0.5f, -circleOffset + offset)
        bodyShapes.Add bodyShapeTop
        bodyShapes.Add bodyShapeBottom
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match capsuleShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties capsuleShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : BoxRoundedShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (a : Affine) -> let mutable t = a in t.Matrix) m4Identity boxRoundedShape.TransformOpt
        if transform.Rotation <> quatIdentity then Log.warnOnce "BoxRoundedShape rotation not yet supported by AetherPhysicsEngine." // TODO: implement!
        let width = AetherPhysicsEngine.toPhysicsPolygonDiameter (boxRoundedShape.Size.X * transform.Scale.X)
        let height = AetherPhysicsEngine.toPhysicsPolygonDiameter (boxRoundedShape.Size.Y * transform.Scale.Y)
        let radius = AetherPhysicsEngine.toPhysicsPolygonRadius (boxRoundedShape.Radius * transform.Scale.X)
        let center = AetherPhysicsEngine.toPhysicsV2 transform.Translation
        let boxVerticalWidth = width - radius * 2.0f
        let boxHorizontalHeight = height - radius * 2.0f
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (width * height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
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
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties boxRoundedShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachEdgeShape bodySource bodyProperties (edgeShape : EdgeShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity edgeShape.TransformOpt
        let bodyShape =
            body.CreateEdge
                (AetherPhysicsEngine.toPhysicsV2 (edgeShape.Start.Transform transform),
                 AetherPhysicsEngine.toPhysicsV2 (edgeShape.Stop.Transform transform))
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match edgeShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties edgeShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachContourShape bodySource bodyProperties (contourShape : ContourShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity contourShape.TransformOpt
        let vertices' = Array.zeroCreate contourShape.Links.Length
        for i in 0 .. dec contourShape.Links.Length do
            vertices'[i] <- AetherPhysicsEngine.toPhysicsV2 (contourShape.Links[i].Transform transform)
        let bodyShape =
            if contourShape.Closed
            then body.CreateLoopShape (Common.Vertices vertices')
            else body.CreateChainShape (Common.Vertices vertices')
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match contourShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties contourShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachBodyConvexHull bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        assert Settings.UseConvexHullPolygons // NOTE: this approach seems to assume this.
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let points' = Array.zeroCreate points.Length
        for i in 0 .. dec points.Length do
            points'[i] <- AetherPhysicsEngine.toPhysicsV2 (points[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = points' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (Common.Vertices points', density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachBodyTriangles bodySource bodyProperties (vertices : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let vertices' = Array.zeroCreate vertices.Length
        for i in 0 .. dec vertices.Length do
            vertices'[i] <- AetherPhysicsEngine.toPhysicsV2 (vertices[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = vertices' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let triangles = vertices' |> Array.chunkBySize 3 |> Array.map Common.Vertices |> List
        let bodyShapes = body.CreateCompoundPolygon (triangles, density)
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            AetherPhysicsEngine.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBodyBounds bodySource bodyProperties (points : Vector3 array) transformOpt (propertiesOpt : BodyShapeProperties option) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let bounds = points |> Array.map _.V2 |> Box2.Enclose
        let corners = bounds.Corners
        let corners' = Array.zeroCreate points.Length
        for i in 0 .. dec corners.Length do
            corners'[i] <- AetherPhysicsEngine.toPhysicsV2 (corners[i].V3.Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (bounds.Width * bounds.Height)
        let density = max 0.001f density // NOTE: Aether has collision response issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (bounds.Corners |> Array.map (fun v -> Common.Vector2 (v.X, v.Y)) |> Common.Vertices, density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        AetherPhysicsEngine.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachPointsShape bodySource bodyProperties (pointsShape : PointsShape) (body : Body) =
        match pointsShape.Profile with
        | Convex -> AetherPhysicsEngine.attachBodyConvexHull bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton
        | Concave ->
            Log.warnOnce "Creating a compound polygon with PointsShape; PointsShape generally specifies individual points rather than triangulated vertices, so unintended behavior may arise."
            AetherPhysicsEngine.attachBodyTriangles bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body
        | Bounds -> AetherPhysicsEngine.attachBodyBounds bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton

    static member private attachGeometryShape bodySource bodyProperties (geometryShape : GeometryShape) body =
        match geometryShape.Profile with
        | Convex -> AetherPhysicsEngine.attachBodyConvexHull bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton
        | Concave -> AetherPhysicsEngine.attachBodyTriangles bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body
        | Bounds -> AetherPhysicsEngine.attachBodyBounds bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton

    static member private attachBodyShapes bodySource bodyProperties bodyShapes (body : Body) =
        let list = List ()
        for bodyShape in bodyShapes do
            let bodyShapes = AetherPhysicsEngine.attachBodyShape bodySource bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape bodySource bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | EmptyShape -> [||]
        | BoxShape boxShape -> AetherPhysicsEngine.attachBoxBody bodySource bodyProperties boxShape body |> Array.singleton
        | SphereShape sphereShape -> AetherPhysicsEngine.attachSphereShape bodySource bodyProperties sphereShape body |> Array.singleton
        | CapsuleShape capsuleShape -> AetherPhysicsEngine.attachCapsuleShape bodySource bodyProperties capsuleShape body |> Array.ofSeq
        | BoxRoundedShape boxRoundedShape -> AetherPhysicsEngine.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape body |> Array.ofSeq
        | EdgeShape edgeShape -> AetherPhysicsEngine.attachEdgeShape bodySource bodyProperties edgeShape body
        | ContourShape contourShape -> AetherPhysicsEngine.attachContourShape bodySource bodyProperties contourShape body
        | PointsShape pointsShape -> AetherPhysicsEngine.attachPointsShape bodySource bodyProperties pointsShape body |> Array.ofSeq
        | GeometryShape geometryShape -> AetherPhysicsEngine.attachGeometryShape bodySource bodyProperties geometryShape body
        | StaticModelShape _ -> [||]
        | StaticModelSurfaceShape _ -> [||]
        | TerrainShape _ -> [||]
        | BodyShapes bodyShapes -> AetherPhysicsEngine.attachBodyShapes bodySource bodyProperties bodyShapes body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation.Angle2d

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (AetherPhysicsEngine.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodyId

        // configure body
        AetherPhysicsEngine.configureBodyProperties bodyProperties body

        // attempt to attach body shape
        try AetherPhysicsEngine.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body |> ignore
        with :? ArgumentOutOfRangeException -> ()

        // listen for collisions
        body.add_OnCollision physicsEngine.PenetrationHandler
        body.add_OnSeparation physicsEngine.SeparationHandler

        // attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        if not (physicsEngine.Bodies.TryAdd (bodyId, (bodyProperties.Gravity, body))) then
            Log.error ("Could not add body for '" + scstring bodyId + "'.")

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                AetherPhysicsEngine.destroyBodyJointInternal bodyJointId physicsEngine
                AetherPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                AetherPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                AetherPhysicsEngine.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy body
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (_, body)) ->
            physicsEngine.Bodies.Remove bodyId |> ignore
            physicsEngine.PhysicsContext.Remove body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            AetherPhysicsEngine.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds
            
    /// unlike createBodyJoint, whether the body joint is re-created on connected body re-creation is unchanged.
    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =
        let resultOpt =
            match bodyJointProperties.BodyJoint with
            | EmptyJoint -> None
            | AetherBodyJoint bodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                let body2Id = bodyJointProperties.BodyJointTarget2
                match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
                | ((true, (_, body)), (true, (_, body2))) ->
                    let joint = bodyJoint.CreateBodyJoint AetherPhysicsEngine.toPhysics AetherPhysicsEngine.toPhysicsV2 body body2
                    Some (joint, body, Some body2)
                | _ -> None
            | _ -> Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for AetherPhysicsEngine."); None
        match resultOpt with
        | Some (joint, body, body2Opt) ->
            joint.Tag <- bodyJointId
            match bodyJointProperties.BreakingPointOpt with
            | Some bp -> joint.Breakpoint <- AetherPhysicsEngine.toPhysics bp
            | None -> ()
            joint.CollideConnected <- bodyJointProperties.CollideConnected
            joint.Enabled <- bodyJointProperties.BodyJointEnabled && not bodyJointProperties.Broken
            joint.add_Broke physicsEngine.BreakHandler
            body.Awake <- true
            match body2Opt with Some body2 -> body2.Awake <- true | None -> ()
            if physicsEngine.Joints.TryAdd (bodyJointId, joint)
            then physicsEngine.PhysicsContext.Add joint
            else Log.warn ("Could not add body joint for '" + scstring bodyJointId + "'.")
        | None -> ()

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =
    
        // log creation message for body joint re-creation on connected body re-creation
        for bodyTarget in [|createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2|] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) -> messages.Add createBodyJointMessage
            | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        AetherPhysicsEngine.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        
    /// unlike destroyBodyJoint, whether the body joint is re-created on connected body re-creation is unchanged.
    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Joints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove bodyJointId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =
    
        // unlog creation message for stopping body joint re-creation on connected body re-creation
        for bodyTarget in [|destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2|] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) ->
                messages.RemoveAll (fun message ->
                    message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                    message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex)
                |> ignore<int>
            | (false, _) -> ()

        // attempt to destroy body joint
        AetherPhysicsEngine.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, (_, body)) -> body.Enabled <- setBodyEnabledMessage.BodyEnabled
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, (_, body)) ->
            let center = AetherPhysicsEngine.toPhysicsV2 setBodyCenterMessage.Center
            if body.Position <> center then
                body.Position <- center
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, (_, body)) ->
            let rotation = setBodyRotationMessage.Rotation.Angle2d
            if body.Rotation <> rotation then
                body.Rotation <- rotation
                do (body.Awake <- false; body.Awake <- true) // force sleep time to zero so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (_, body)) -> body.LinearVelocity <- AetherPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (_, body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity.Z
        | (false, _) -> ()

    static member private setBodyJointMotorEnabled (setBodyJointMotorEnabledMessage : SetBodyJointMotorEnabledMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorEnabledMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorEnabled <- setBodyJointMotorEnabledMessage.MotorEnabled
            | _ -> ()
        | (false, _) -> ()

    static member private setBodyJointMotorSpeed (setBodyJointMotorSpeedMessage : SetBodyJointMotorSpeedMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointMotorSpeedMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorSpeed <- setBodyJointMotorSpeedMessage.MotorSpeed
            | _ -> ()
        | (false, _) -> ()

    static member private setBodyJointTargetAngle (setBodyJointTargetAngleMessage : SetBodyJointTargetAngleMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue setBodyJointTargetAngleMessage.BodyJointId with
        | (true, joint) ->
            match joint with
            | :? Dynamics.Joints.AngleJoint as joint -> joint.TargetAngle <- setBodyJointTargetAngleMessage.TargetAngle
            | _ -> ()
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                match applyBodyLinearImpulseMessage.OriginWorldOpt with
                | Some originWorld ->
                    body.ApplyLinearImpulse
                        (AetherPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                         AetherPhysicsEngine.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyLinearImpulse
                        (AetherPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.Z) then
                body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse.Z)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                match applyBodyForceMessage.OriginWorldOpt with
                | Some originWorld ->
                    body.ApplyForce
                        (AetherPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force,
                         AetherPhysicsEngine.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyForce
                        (AetherPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force)
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (_, body)) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.Z) then
                body.ApplyTorque (applyBodyTorqueMessage.Torque.Z)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Aether.")
        | (false, _) -> ()

    static member private getBodyContactNormals bodyId physicsEngine =
        [|for contact in AetherPhysicsEngine.getBodyContacts bodyId physicsEngine do
            let normal = fst (contact.GetWorldManifold ())
            if normal <> Common.Vector2.Zero then // may be zero if from broad phase but not in narrow phase
                let bodyShapeIndex = contact.FixtureA.Tag :?> BodyShapeIndex
                let normal = if bodyShapeIndex.BodyId = bodyId then -normal else normal // negate normal when appropriate
                Vector3 (normal.X, normal.Y, 0.0f)|]

    static member private getBodyGroundDirection bodyId physicsEngine =
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (gravity, body)) ->
            let gravity = Gravity.localize (physicsEngine :> PhysicsEngine).Gravity gravity
            if gravity <> v3Zero
            then gravity.Normalized // ground relative to gravity
            else v3Down.Transform (Quaternion.CreateFromAngle2d body.Rotation) // ground relative to body rotation
        | (false, _) -> (physicsEngine :> PhysicsEngine).Gravity.Normalized

    static member private getBodyToGroundContactNormals groundDirection bodyId physicsEngine =
        assert (Constants.Physics.GroundAngleMax < MathF.PI_OVER_2) // any larger would allow wall jumping without pushing back against the wall
        let up = -groundDirection
        AetherPhysicsEngine.getBodyContactNormals bodyId physicsEngine
        |> Array.filter (fun contactNormal ->
            let projectionToUp = contactNormal.Dot up
            assert (abs projectionToUp <= 1.0f) // contactNormal and upDirection are normalized. -1 <= dot product <= 1
            let theta = acos projectionToUp
            theta <= Constants.Physics.GroundAngleMax)

    static member private getBodyToGroundContactNormalOpt bodyId physicsEngine =
        let groundDirection = AetherPhysicsEngine.getBodyGroundDirection bodyId physicsEngine 
        match AetherPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine with
        | [||] -> None
        | groundNormals ->
            groundNormals
            |> Seq.map (fun normal -> struct (normal.Dot groundDirection, normal))
            |> Seq.maxBy fst'
            |> snd'
            |> Some

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
        | (true, (_, body)) ->
            let groundDirection = AetherPhysicsEngine.getBodyGroundDirection jumpBodyMessage.BodyId physicsEngine
            if jumpBodyMessage.CanJumpInAir || Array.notEmpty (AetherPhysicsEngine.getBodyToGroundContactNormals groundDirection jumpBodyMessage.BodyId physicsEngine) then
                body.LinearVelocity <- body.LinearVelocity - AetherPhysicsEngine.toPhysicsV2 (groundDirection * jumpBodyMessage.JumpSpeed)
                body.Awake <- true
        | (false, _) -> ()


    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> AetherPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> AetherPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> AetherPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> AetherPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> AetherPhysicsEngine.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> AetherPhysicsEngine.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | CreateFluidEmitterMessage _ -> () // no fluid emitter support
        | DestroyFluidEmitterMessage _ -> () // no fluid emitter support
        | SetBodyEnabledMessage setBodyEnabledMessage -> AetherPhysicsEngine.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> AetherPhysicsEngine.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> AetherPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> AetherPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> AetherPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | SetBodyVehicleForwardInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleRightInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleHandBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyJointMotorEnabledMessage setBodyJointMotorEnabledMessage -> AetherPhysicsEngine.setBodyJointMotorEnabled setBodyJointMotorEnabledMessage physicsEngine
        | SetBodyJointMotorSpeedMessage setBodyJointMotorSpeedMessage -> AetherPhysicsEngine.setBodyJointMotorSpeed setBodyJointMotorSpeedMessage physicsEngine
        | SetBodyJointTargetAngleMessage setBodyJointTargetAngleMessage -> AetherPhysicsEngine.setBodyJointTargetAngle setBodyJointTargetAngleMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> AetherPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> AetherPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> AetherPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> AetherPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | ApplyExplosionMessage _ -> () // no explosion support
        | JumpBodyMessage jumpBodyMessage -> AetherPhysicsEngine.jumpBody jumpBodyMessage physicsEngine
        | UpdateFluidEmitterMessage _ -> () // no fluid emitter support
        | EmitFluidParticlesMessage _ -> () // no fluid emitter support
        | SetFluidParticlesMessage _ -> () // no fluid emitter support
        | ChooseFluidParticlesMessage _ -> () // no fluid emitter support
        | ClearFluidParticlesMessage _ -> () // no fluid emitter support
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- AetherPhysicsEngine.toPhysicsV2 gravity

    static member private createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (_, body) = bodyEntry.Value
            if body.Awake then

                // append transform message
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.Tag :?> BodyId
                          Center = AetherPhysicsEngine.toPixelV3 body.Position
                          Rotation = Quaternion.CreateFromAngle2d body.Rotation
                          LinearVelocity = AetherPhysicsEngine.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 0.0f 0.0f body.AngularVelocity }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

                // manually sleep static bodies since aether won't sleep them itself
                if body.BodyType = Dynamics.BodyType.Static then body.Awake <- false

    static member private stepGravity (physicsStepAmount : single) physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (gravity, body) = bodyEntry.Value
            if body.BodyType = Dynamics.BodyType.Dynamic then
                let gravityLocal = AetherPhysicsEngine.toPhysicsV2 (Gravity.localize (physicsEngine :> PhysicsEngine).Gravity gravity)
                body.LinearVelocity <- body.LinearVelocity + gravityLocal * physicsStepAmount

    /// Make a physics engine.
    static member make gravity =
        Settings.UseConvexHullPolygons <- true
        Settings.PositionIterations <- Constants.Physics.Collision2dSteps
        let integrationMessages = List ()
        let penetrationHandler = fun fixture fixture2 collision -> AetherPhysicsEngine.handlePenetration fixture fixture2 collision integrationMessages
        let separationHandler = fun fixture fixture2 _ -> AetherPhysicsEngine.handleSeparation fixture fixture2 integrationMessages
        let breakHandler = fun joint jointError -> AetherPhysicsEngine.handleBreak joint jointError integrationMessages
        let physicsEngine =
            { PhysicsContext = World (AetherPhysicsEngine.toPhysicsV2 gravity)
              Bodies = Dictionary<BodyId, Gravity * Dynamics.Body> HashIdentity.Structural
              Joints = Dictionary<BodyJointId, Dynamics.Joints.Joint> HashIdentity.Structural
              CreateBodyJointMessages = Dictionary<BodyId, CreateBodyJointMessage List> HashIdentity.Structural
              IntegrationMessages = integrationMessages
              PenetrationHandler = penetrationHandler
              SeparationHandler = separationHandler
              BreakHandler = breakHandler }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.GravityDefault =
            let gravityDefault = Common.Vector2 (Constants.Physics.GravityDefault.X, Constants.Physics.GravityDefault.Y)
            AetherPhysicsEngine.toPixelV3 gravityDefault

        member physicsEngine.Gravity =
            AetherPhysicsEngine.toPixelV3 physicsEngine.PhysicsContext.Gravity

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            AetherPhysicsEngine.getBodyContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let (_, body) = physicsEngine.Bodies[bodyId]
            AetherPhysicsEngine.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyAngularVelocity bodyId =
            let (_, body) = physicsEngine.Bodies[bodyId]
            v3 0.0f 0.0f body.AngularVelocity

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            let groundDirection = AetherPhysicsEngine.getBodyGroundDirection bodyId physicsEngine
            AetherPhysicsEngine.getBodyToGroundContactNormals groundDirection bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            AetherPhysicsEngine.getBodyToGroundContactNormalOpt bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.GetBodyGrounded bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            Array.notEmpty groundNormals

        member physicsEngine.GetBodySensor bodyId =
            let (_, body) = physicsEngine.Bodies[bodyId]
            let mutable found = false
            let mutable sensor = false
            let mutable i = 0
            while i < body.FixtureList.Count && not found do
                let fixture = body.FixtureList[i]
                let fixtureBodyId = (fixture.Tag :?> BodyShapeIndex).BodyId
                if fixtureBodyId = bodyId then
                    sensor <- fixture.IsSensor
                    found <- true
                i <- inc i
            sensor

        member physicsEngine.GetBodyWheelSpeedAtClutch _ =
            0.0f // no vehicle controller support

        member physicsEngine.GetBodyWheelModelMatrix (_, _, _, _) =
            m4Identity // no vehicle controller support

        member physicsEngine.GetBodyWheelAngularVelocity (_, _) =
            0.0f // no vehicle controller support

        member physicsEngine.GetBodyJointExists bodyJointId =
            physicsEngine.Joints.ContainsKey bodyJointId

        member physicsEngine.GetBodyJointMotorSpeed bodyJointId =
            match physicsEngine.Joints.TryGetValue bodyJointId with
            | (true, joint) ->
                match joint with
                | :? Dynamics.Joints.PrismaticJoint as joint -> joint.MotorSpeed
                | :? Dynamics.Joints.RevoluteJoint as joint -> joint.MotorSpeed
                | :? Dynamics.Joints.WheelJoint as joint -> joint.MotorSpeed
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.GetBodyJointTargetAngle bodyJointId =
            match physicsEngine.Joints.TryGetValue bodyJointId with
            | (true, joint) ->
                match joint with
                | :? Dynamics.Joints.AngleJoint as joint -> joint.TargetAngle
                | _ -> 0.0f
            | (false, _) -> 0.0f

        member physicsEngine.GetFluidEmitterExists _ =
            false // no fluid emitter support

        member physicsEngine.GetFluidEmitterFluidGrounded _ =
            false // no fluid emitter support

        member physicsEngine.RayCast (ray, collisionCategory, collisionMask, closestOnly) =
            let results = List ()
            let mutable fractionMin = Single.MaxValue
            let mutable closestOpt = None
            let callback =
                RayCastReportFixtureDelegate (fun fixture point normal fraction ->
                    match fixture.Tag with
                    | :? BodyShapeIndex as bodyShapeIndex ->
                        if (int fixture.CollisionCategories &&& int collisionMask) <> 0 &&
                           (int fixture.CollidesWith &&& int collisionCategory) <> 0 then
                            let report = BodyIntersection.make bodyShapeIndex fraction (AetherPhysicsEngine.toPixelV3 point) (v3 normal.X normal.Y 0.0f)
                            if fraction < fractionMin then
                                fractionMin <- fraction
                                closestOpt <- Some report
                            results.Add report
                    | _ -> ()
                    if closestOnly then fraction else 1.0f)
            let point = AetherPhysicsEngine.toPhysicsV2 ray.Origin
            let offset = AetherPhysicsEngine.toPhysicsV2 ray.Direction
            physicsEngine.PhysicsContext.RayCast (callback, point, point + offset)
            if closestOnly then
                match closestOpt with
                | Some closest -> [|closest|]
                | None -> [||]
            else Array.ofSeq results

        member physicsEngine.ShapeCast (_, _, _, _, _, _) =
            Log.warn "ShapeCast not yet implemented for AetherPhysicsEngine."
            [||]

        member physicsEngine.HandleMessage physicsMessage =
            AetherPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate gameDelta =

            // constrain step time
            let stepTime = gameDelta.SecondsF
            let stepTime =
                if stepTime > 0.0f && stepTime < 0.001f then 0.001f
                elif stepTime > 0.1f then 0.1f
                else stepTime

            // integrate only when time has passed
            if stepTime > 0.0f then
                AetherPhysicsEngine.stepGravity stepTime physicsEngine
                physicsEngine.PhysicsContext.Step stepTime
                AetherPhysicsEngine.createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine
                let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                physicsEngine.IntegrationMessages.Clear ()
                Some integrationMessages
            else None

        member physicsEngine.TryRender renderContext =
            match renderContext with
            | :? AetherPhysicsEngineRenderContext as renderContext ->
                for bodyEntry in physicsEngine.Bodies do

                    // render fixtures in body
                    let (_, body) = bodyEntry.Value
                    let transform =
                        Matrix3x2.CreateRotation body.Rotation *
                        Matrix3x2.CreateTranslation (AetherPhysicsEngine.toPixelV2 body.Position)
                    let eyeBounds = renderContext.EyeBounds
                    for fixture in body.FixtureList do

                        // compute color consistent with JoltSharp which defaults to MotionTypeColor:
                        // https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/DrawSettings.cs#L33
                        // which is defined here:
                        // https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/ShapeColor.cs#L20
                        let color =
                            match body.BodyType with
                            | BodyType.Dynamic -> // dynamic = random color per instance
                                bodyEntry.Key.GetHashCode () |> uint |> colorPacked |> _.WithA(1f)
                            | BodyType.Kinematic -> // keyframed
                                Color.Green
                            | _ -> // static or anything else
                                Color.Gray

                        // render shape
                        // TODO: see if we can optimize these by quickly getting the shape bounds and checking for its
                        // view intersection instead of per-edge checking.
                        match fixture.Shape with
                        | :? Collision.Shapes.PolygonShape as polygonShape ->
                            let vertices = polygonShape.Vertices
                            for i in 0 .. dec vertices.Count do
                                let start = (AetherPhysicsEngine.toPixelV2 vertices[i]).Transform transform
                                let stop = (AetherPhysicsEngine.toPixelV2 vertices[if i < dec vertices.Count then inc i else 0]).Transform transform
                                let bounds = Box2.Enclose (start, stop)
                                if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                    renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.CircleShape as circleShape ->
                            let position = (AetherPhysicsEngine.toPixelV2 circleShape.Position).Transform transform
                            let radius = AetherPhysicsEngine.toPixel circleShape.Radius
                            if eyeBounds.Contains (box2 (position - v2 radius radius) (v2 radius radius * 2f)) <> ContainmentType.Disjoint then
                                renderContext.DrawCircle (position, radius, color)
                        | :? Collision.Shapes.EdgeShape as edgeShape ->
                            let start = (AetherPhysicsEngine.toPixelV2 edgeShape.Vertex1).Transform transform
                            let stop = (AetherPhysicsEngine.toPixelV2 edgeShape.Vertex2).Transform transform
                            let bounds = Box2.Enclose (start, stop)
                            if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.ChainShape as chainShape ->
                            let vertices = chainShape.Vertices
                            if vertices.Count >= 2 then // when looped, the link from last point to first point is already included
                                for i in 0 .. vertices.Count - 2 do
                                    let start = (AetherPhysicsEngine.toPixelV2 vertices[i]).Transform transform
                                    let stop = (AetherPhysicsEngine.toPixelV2 vertices[inc i]).Transform transform
                                    let bounds = Box2.Enclose (start, stop)
                                    if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                        renderContext.DrawLine (start, stop, color)
                        | _ -> ()

            | _ -> ()

        member physicsEngine.ClearInternal () =
            physicsEngine.Joints.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.CreateBodyJointMessages.Clear ()
            physicsEngine.IntegrationMessages.Clear ()
            physicsEngine.PhysicsContext.Clear ()

        member physicsEngine.CleanUp () =
            ()