// Nu Game Engine.
// Copyright (C) Bryan Edds.

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

/// The 2d interface of PhysicsEngineRenderContext in terms of Aether Physics.
type PhysicsEngine2dRenderContext =
    inherit PhysicsEngineRenderContext
    abstract EyeBounds : Box2
    abstract DrawLine : start : Vector2 * stop : Vector2 * color : Color -> unit
    abstract DrawCircle : position : Vector2 * radius : single * color : Color -> unit

/// The 2d implementation of PhysicsEngine in terms of Aether Physics.
and [<ReferenceEquality>] PhysicsEngine2d =
    private
        { PhysicsContext : Dynamics.World
          Bodies : Dictionary<BodyId, Vector3 option * Dynamics.Body>
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
        Vector2 (PhysicsEngine2d.toPixel v2.X, PhysicsEngine2d.toPixel v2.Y)

    static member private toPixelV3 (v2 : Common.Vector2) =
        (PhysicsEngine2d.toPixelV2 v2).V3

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
        | Vehicle -> Log.infoOnce "Vehicle not supported by PhysicsEngine2d. Using Dynamic configuration instead."; Dynamics.BodyType.Dynamic

    static member private handlePenetration
        (bodyShape : Dynamics.Fixture)
        (bodyShape2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact)
        (integrationMessages : IntegrationMessage List) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyPenetrationMessage =
            { BodyShapeSource = bodyShape.Tag :?> BodyShapeIndex
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex
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
              BodyShapeSource2 = bodyShape2.Tag :?> BodyShapeIndex }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        integrationMessages.Add integrationMessage

    static member private handleBreak
        (joint : Joint)
        (jointError : single)
        (integrationMessages : IntegrationMessage List) =
        let jointBreakPointPixel = PhysicsEngine2d.toPixel joint.Breakpoint
        let jointErrorPixel = PhysicsEngine2d.toPixel jointError
        let bodyJointBreakMessage =
            { BodyJointId = joint.Tag :?> BodyJointId
              BreakingPoint = jointBreakPointPixel
              BreakingOverflow = jointErrorPixel - jointBreakPointPixel }
        let integrationMessage = BodyJointBreakMessage bodyJointBreakMessage
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
        body.BodyType <- PhysicsEngine2d.toPhysicsBodyType bodyProperties.BodyType // NOTE: BodyType must be set first or other configurations may be ignored!
        body.Enabled <- bodyProperties.Enabled
        body.SleepingAllowed <- bodyProperties.SleepingAllowed
        body.Position <- PhysicsEngine2d.toPhysicsV2 bodyProperties.Center
        body.Rotation <- bodyProperties.Rotation.Angle2d
        body.LinearVelocity <- PhysicsEngine2d.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularVelocity <- bodyProperties.AngularVelocity.Z
        body.AngularDamping <- bodyProperties.AngularDamping
        body.FixedRotation <- bodyProperties.AngularFactor.Z = 0.0f
        body.IgnoreGravity <- true // NOTE: body-specific gravity isn't supported by Aether, so we handle gravity ourselves.
        body.IgnoreCCD <- match bodyProperties.CollisionDetection with Discrete -> true | Continuous -> false
        body.Awake <- bodyProperties.Awake

    static member private attachBoxBody bodySource (bodyProperties : BodyProperties) (boxShape : BoxShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity boxShape.TransformOpt
        let width = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.X * transform.Scale.X)
        let height = PhysicsEngine2d.toPhysicsPolygonDiameter (boxShape.Size.Y * transform.Scale.Y)
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
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
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
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
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let offset = PhysicsEngine2d.toPhysicsV2 transform.Translation
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
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties capsuleShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : BoxRoundedShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (a : Affine) -> let mutable t = a in t.Matrix) m4Identity boxRoundedShape.TransformOpt
        if quatNeq transform.Rotation quatIdentity then Log.warnOnce "BoxRoundedShape rotation not yet supported by PhysicsEngine2d." // TODO: implement!
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
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties boxRoundedShape.PropertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachEdgeShape bodySource bodyProperties (edgeShape : EdgeShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity edgeShape.TransformOpt
        let bodyShape =
            body.CreateEdge
                (PhysicsEngine2d.toPhysicsV2 (edgeShape.Start.Transform transform),
                 PhysicsEngine2d.toPhysicsV2 (edgeShape.Stop.Transform transform))
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match edgeShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties edgeShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachContourShape bodySource bodyProperties (contourShape : ContourShape) (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity contourShape.TransformOpt
        let vertices' = Array.zeroCreate contourShape.Links.Length
        for i in 0 .. dec contourShape.Links.Length do
            vertices'.[i] <- PhysicsEngine2d.toPhysicsV2 (contourShape.Links.[i].Transform transform)
        let bodyShape =
            if contourShape.Closed
            then body.CreateLoopShape (Common.Vertices vertices')
            else body.CreateChainShape (Common.Vertices vertices')
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match contourShape.PropertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties contourShape.PropertiesOpt bodyShape
        Array.singleton bodyShape

    static member private attachBodyConvexHull bodySource bodyProperties (points : Vector3 array) transformOpt propertiesOpt (body : Body) =
        assert Settings.UseConvexHullPolygons // NOTE: this approach seems to assume this.
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let points' = Array.zeroCreate points.Length
        for i in 0 .. dec points.Length do
            points'.[i] <- PhysicsEngine2d.toPhysicsV2 (points.[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = points' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (Common.Vertices points', density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachBodyTriangles bodySource bodyProperties (vertices : Vector3 array) transformOpt propertiesOpt (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let vertices' = Array.zeroCreate vertices.Length
        for i in 0 .. dec vertices.Length do
            vertices'.[i] <- PhysicsEngine2d.toPhysicsV2 (vertices.[i].Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass ->
                let box = vertices' |> Array.map (fun v -> v2 v.X v.Y) |> Box2.Enclose // TODO: perhaps use a Sphere or Circle instead?
                mass / (box.Width * box.Height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let triangles = vertices' |> Array.chunkBySize 3 |> Array.map Common.Vertices |> List
        let bodyShapes = body.CreateCompoundPolygon (triangles, density)
        for bodyShape in bodyShapes do
            bodyShape.Tag <-
                { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
            PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        Array.ofSeq bodyShapes

    static member private attachBodyBounds bodySource bodyProperties (points : Vector3 array) transformOpt propertiesOpt (body : Body) =
        let transform = Option.mapOrDefaultValue (fun (t : Affine) -> let mutable t = t in t.Matrix) m4Identity transformOpt
        let bounds = points |> Array.map _.V2 |> Box2.Enclose
        let corners = bounds.Corners
        let corners' = Array.zeroCreate points.Length
        for i in 0 .. dec corners.Length do
            corners'.[i] <- PhysicsEngine2d.toPhysicsV2 (corners.[i].V3.Transform transform)
        let density =
            match bodyProperties.Substance with
            | Density density -> density
            | Mass mass -> mass / (bounds.Width * bounds.Height)
        let density = max 0.001f density // NOTE: Aether has collision reponse issue when density is 0 even if it's for a static shape!
        let bodyShape = body.CreatePolygon (bounds.Corners |> Array.map (fun v -> Common.Vector2 (v.X, v.Y)) |> Common.Vertices, density)
        bodyShape.Tag <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              BodyShapeIndex = match propertiesOpt with Some p -> p.BodyShapeIndex | None -> 0 }
        PhysicsEngine2d.configureBodyShapeProperties bodyProperties propertiesOpt bodyShape
        bodyShape

    static member private attachPointsShape bodySource bodyProperties (pointsShape : PointsShape) (body : Body) =
        match pointsShape.Profile with
        | Convex -> PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton
        | Concave ->
            Log.warnOnce "Creating a compound polygon with PointsShape; PointsShape generally specifies individual points rather than triangulated vertices, so unintended behavior may arise."
            PhysicsEngine2d.attachBodyTriangles bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body
        | Bounds -> PhysicsEngine2d.attachBodyBounds bodySource bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt body |> Array.singleton

    static member private attachGeometryShape bodySource bodyProperties (geometryShape : GeometryShape) body =
        match geometryShape.Profile with
        | Convex -> PhysicsEngine2d.attachBodyConvexHull bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton
        | Concave -> PhysicsEngine2d.attachBodyTriangles bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body
        | Bounds -> PhysicsEngine2d.attachBodyBounds bodySource bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt body |> Array.singleton

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
        | EdgeShape edgeShape -> PhysicsEngine2d.attachEdgeShape bodySource bodyProperties edgeShape body
        | ContourShape contourShape -> PhysicsEngine2d.attachContourShape bodySource bodyProperties contourShape body
        | PointsShape pointsShape -> PhysicsEngine2d.attachPointsShape bodySource bodyProperties pointsShape body |> Array.ofSeq
        | GeometryShape geometryShape -> PhysicsEngine2d.attachGeometryShape bodySource bodyProperties geometryShape body
        | StaticModelShape _ -> [||]
        | StaticModelSurfaceShape _ -> [||]
        | TerrainShape _ -> [||]
        | BodyShapes bodyShapes -> PhysicsEngine2d.attachBodyShapes bodySource bodyProperties bodyShapes body

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        let bodyRotation = bodyProperties.Rotation.Angle2d

        // make the body
        let body = physicsEngine.PhysicsContext.CreateBody (PhysicsEngine2d.toPhysicsV2 bodyProperties.Center, bodyRotation)
        body.Tag <- bodyId

        // configure body
        PhysicsEngine2d.configureBodyProperties bodyProperties body

        // attempt to attach body shape
        try PhysicsEngine2d.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape body |> ignore
        with :? ArgumentOutOfRangeException -> ()

        // listen for collisions
        body.add_OnCollision physicsEngine.PenetrationHandler
        body.add_OnSeparation physicsEngine.SeparationHandler

        // attempt to add the body
        let bodyId = { BodySource = createBodyMessage.BodyId.BodySource; BodyIndex = bodyProperties.BodyIndex }
        if not (physicsEngine.Bodies.TryAdd (bodyId, (bodyProperties.GravityOverride, body))) then
            Log.error ("Could not add body for '" + scstring bodyId + "'.")

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine2d.destroyBodyJointInternal bodyJointId physicsEngine
                PhysicsEngine2d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                PhysicsEngine2d.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine2d.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy body
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, (_, body)) ->
            physicsEngine.Bodies.Remove bodyId |> ignore
            physicsEngine.PhysicsContext.Remove body
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            PhysicsEngine2d.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =
        let resultOpt =
            match bodyJointProperties.BodyJoint with
            | EmptyJoint ->
                None
            | OneBodyJoint2d oneBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                match physicsEngine.Bodies.TryGetValue bodyId with
                | (true, (_, body)) ->
                    let joint = oneBodyJoint.CreateOneBodyJoint PhysicsEngine2d.toPhysics PhysicsEngine2d.toPhysicsV2 body
                    Some (joint, body, None)
                | (false, _) -> None
            | TwoBodyJoint2d twoBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                let body2IdOpt = bodyJointProperties.BodyJointTarget2Opt
                match body2IdOpt with
                | Some body2Id ->
                    match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
                    | ((true, (_, body)), (true, (_, body2))) ->
                        let joint = twoBodyJoint.CreateTwoBodyJoint PhysicsEngine2d.toPhysics PhysicsEngine2d.toPhysicsV2 body body2
                        Some (joint, body, Some body2)
                    | _ -> None
                | None -> None
            | OneBodyJoint3d _ | TwoBodyJoint3d _ ->
                Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for PhysicsEngine2d.")
                None
        match resultOpt with
        | Some (joint, body, body2Opt) ->
            joint.Tag <- bodyJointId
            joint.Breakpoint <- PhysicsEngine2d.toPhysics bodyJointProperties.BreakingPoint
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

        // log creation message
        for bodyTargetOpt in [Some createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) -> messages.Add createBodyJointMessage
                | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])
            | None -> ()

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        PhysicsEngine2d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine

    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Joints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Joints.Remove bodyJointId |> ignore
            physicsEngine.PhysicsContext.Remove joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message
        for bodyTargetOpt in [Some destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) ->
                    messages.RemoveAll (fun message ->
                        message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                        message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex)
                    |> ignore<int>
                | (false, _) -> ()
            | None -> ()

        // attempt to destroy body joint
        PhysicsEngine2d.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

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
            let rotation = setBodyRotationMessage.Rotation.Angle2d
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
                        (PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse,
                         PhysicsEngine2d.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyLinearImpulse
                        (PhysicsEngine2d.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
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
                        (PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Force,
                         PhysicsEngine2d.toPhysicsV2 originWorld)
                | None ->
                    body.ApplyForce
                        (PhysicsEngine2d.toPhysicsV2 applyBodyForceMessage.Force)
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
        [|for contact in PhysicsEngine2d.getBodyContacts bodyId physicsEngine do
            let normal = fst (contact.GetWorldManifold ())
            if normal <> Common.Vector2.Zero then // may be zero if from broad phase but not in narrow phase
                let bodyShapeIndex = contact.FixtureA.Tag :?> BodyShapeIndex
                let normal = if bodyShapeIndex.BodyId = bodyId then -normal else normal // negate normal when appropriate
                Vector3 (normal.X, normal.Y, 0.0f)|]

    static member private getBodyToGroundContactNormals bodyId physicsEngine =
        PhysicsEngine2d.getBodyContactNormals bodyId physicsEngine
        |> Array.filter (fun contactNormal ->
            let theta = contactNormal.Dot Vector3.UnitY |> max -1.0f |> min 1.0f |> acos
            theta <= Constants.Physics.GroundAngleMax && contactNormal.Y > 0.0f)
 
    static member private getBodyToGroundContactNormalOpt bodyId physicsEngine =
        match PhysicsEngine2d.getBodyToGroundContactNormals bodyId physicsEngine with
        | [||] -> None
        | groundNormals ->
            groundNormals
            |> Seq.map (fun normal -> struct (normal.Dot v3Down, normal))
            |> Seq.maxBy fst'
            |> snd'
            |> Some

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
        | (true, (_, body)) ->
            if  jumpBodyMessage.CanJumpInAir ||
                Array.notEmpty (PhysicsEngine2d.getBodyToGroundContactNormals jumpBodyMessage.BodyId physicsEngine) then
                body.LinearVelocity <- body.LinearVelocity + Common.Vector2 (0.0f, jumpBodyMessage.JumpSpeed)
                body.Awake <- true
        | (false, _) -> ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine2d.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine2d.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine2d.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine2d.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> PhysicsEngine2d.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> PhysicsEngine2d.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngine2d.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngine2d.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine2d.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine2d.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine2d.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | SetBodyVehicleForwardInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleRightInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyVehicleHandBrakeInputMessage _ -> () // no vehicle controller support
        | SetBodyJointMotorEnabledMessage setBodyJointMotorEnabledMessage -> PhysicsEngine2d.setBodyJointMotorEnabled setBodyJointMotorEnabledMessage physicsEngine
        | SetBodyJointMotorSpeedMessage setBodyJointMotorSpeedMessage -> PhysicsEngine2d.setBodyJointMotorSpeed setBodyJointMotorSpeedMessage physicsEngine
        | SetBodyJointTargetAngleMessage setBodyJointTargetAngleMessage -> PhysicsEngine2d.setBodyJointTargetAngle setBodyJointTargetAngleMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine2d.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine2d.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine2d.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngine2d.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> PhysicsEngine2d.jumpBody jumpBodyMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- PhysicsEngine2d.toPhysicsV2 gravity

    static member private createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine =
        for bodyEntry in physicsEngine.Bodies do
            let (_, body) = bodyEntry.Value
            if body.Awake then

                // append transform message
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.Tag :?> BodyId
                          Center = PhysicsEngine2d.toPixelV3 body.Position
                          Rotation = Quaternion.CreateFromAngle2d body.Rotation
                          LinearVelocity = PhysicsEngine2d.toPixelV3 body.LinearVelocity
                          AngularVelocity = v3 0.0f 0.0f body.AngularVelocity }
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
        Settings.UseConvexHullPolygons <- true
        let integrationMessages = List ()
        let penetrationHandler = fun fixture fixture2 collision -> PhysicsEngine2d.handlePenetration fixture fixture2 collision integrationMessages
        let separationHandler = fun fixture fixture2 _ -> PhysicsEngine2d.handleSeparation fixture fixture2 integrationMessages
        let breakHandler = fun joint jointError -> PhysicsEngine2d.handleBreak joint jointError integrationMessages
        let physicsEngine =
            { PhysicsContext = World (PhysicsEngine2d.toPhysicsV2 gravity)
              Bodies = Dictionary<BodyId, Vector3 option * Dynamics.Body> HashIdentity.Structural
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
            PhysicsEngine2d.toPixelV3 gravityDefault

        member physicsEngine.Gravity =
            PhysicsEngine2d.toPixelV3 physicsEngine.PhysicsContext.Gravity

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            PhysicsEngine2d.getBodyContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyLinearVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            PhysicsEngine2d.toPixelV3 body.LinearVelocity

        member physicsEngine.GetBodyAngularVelocity bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            v3 0.0f 0.0f body.AngularVelocity

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            PhysicsEngine2d.getBodyToGroundContactNormals bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            PhysicsEngine2d.getBodyToGroundContactNormalOpt bodyId physicsEngine

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3 (normal.Y, -normal.X, 0.0f))
            | None -> None

        member physicsEngine.GetBodyGrounded bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            Array.notEmpty groundNormals

        member physicsEngine.GetBodySensor bodyId =
            let (_, body) = physicsEngine.Bodies.[bodyId]
            let mutable found = false
            let mutable sensor = false
            let mutable i = 0
            while i < body.FixtureList.Count && not found do
                let fixture = body.FixtureList.[i]
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

        member physicsEngine.RayCast (ray, collisionMask, closestOnly) =
            let results = List ()
            let mutable fractionMin = Single.MaxValue
            let mutable closestOpt = None
            let callback =
                RayCastReportFixtureDelegate (fun fixture point normal fraction ->
                    match fixture.Tag with
                    | :? BodyShapeIndex as bodyShapeIndex ->
                        if (int fixture.CollidesWith &&& collisionMask) <> 0 then
                            let report = BodyIntersection.make bodyShapeIndex fraction (PhysicsEngine2d.toPixelV3 point) (v3 normal.X normal.Y 0.0f)
                            if fraction < fractionMin then
                                fractionMin <- fraction
                                closestOpt <- Some report
                            results.Add report
                    | _ -> ()
                    if closestOnly then fraction else 1.0f)
            let point = PhysicsEngine2d.toPhysicsV2 ray.Origin
            let offset = PhysicsEngine2d.toPhysicsV2 ray.Direction
            physicsEngine.PhysicsContext.RayCast (callback, point, point + offset)
            if closestOnly then
                match closestOpt with
                | Some closest -> [|closest|]
                | None -> [||]
            else Array.ofSeq results

        member physicsEngine.ShapeCast (_, _, _, _, _) =
            Log.warn "ShapeCast not implemented for PhysicsEngine2d."
            [||] // TODO: P1: implement.

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngine2d.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate stepTime =

            // constrain step time
            let stepTime = stepTime.Seconds
            let stepTime =
                if stepTime > 0.0f && stepTime < 0.001f then 0.001f
                elif stepTime > 0.1f then 0.1f
                else stepTime

            // integrate only when time has passed
            if stepTime > 0.0f then
                PhysicsEngine2d.applyGravity stepTime physicsEngine
                physicsEngine.PhysicsContext.Step stepTime
                PhysicsEngine2d.createIntegrationMessagesAndSleepAwakeStaticBodies physicsEngine
                let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                physicsEngine.IntegrationMessages.Clear ()
                Some integrationMessages
            else None

        member physicsEngine.TryRender renderContext =
            match renderContext with
            | :? PhysicsEngine2dRenderContext as renderContext ->
                for bodyEntry in physicsEngine.Bodies do
                    
                    // render fixtures in body
                    let (_, body) = bodyEntry.Value
                    let transform =
                        Matrix3x2.CreateRotation body.Rotation *
                        Matrix3x2.CreateTranslation (PhysicsEngine2d.toPixelV2 body.Position)
                    let eyeBounds = renderContext.EyeBounds
                    for fixture in body.FixtureList do

                        // compute color consistent with JoltSharp which defaults to MotionTypeColor: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/DrawSettings.cs#L33
                        // which is defined here: https://github.com/amerkoleci/JoltPhysicsSharp/blob/fbc0511c987043a16b6f985ae00633285ee56cb9/src/JoltPhysicsSharp/ShapeColor.cs#L20
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
                                let start = (PhysicsEngine2d.toPixelV2 vertices[i]).Transform transform
                                let stop = (PhysicsEngine2d.toPixelV2 vertices[if i < dec vertices.Count then inc i else 0]).Transform transform
                                let bounds = Box2.Enclose (start, stop)
                                if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                    renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.CircleShape as circleShape ->
                            let position = (PhysicsEngine2d.toPixelV2 circleShape.Position).Transform transform
                            let radius = PhysicsEngine2d.toPixel circleShape.Radius
                            if eyeBounds.Contains (box2 (position - v2 radius radius) (v2 radius radius * 2f)) <> ContainmentType.Disjoint then
                                renderContext.DrawCircle (position, radius, color)
                        | :? Collision.Shapes.EdgeShape as edgeShape ->
                            let start = (PhysicsEngine2d.toPixelV2 edgeShape.Vertex1).Transform transform
                            let stop = (PhysicsEngine2d.toPixelV2 edgeShape.Vertex2).Transform transform
                            let bounds = Box2.Enclose (start, stop)
                            if eyeBounds.Contains bounds <> ContainmentType.Disjoint then
                                renderContext.DrawLine (start, stop, color)
                        | :? Collision.Shapes.ChainShape as chainShape ->
                            let vertices = chainShape.Vertices
                            if vertices.Count >= 2 then // when looped, the link from last point to first point is already included
                                for i in 0 .. vertices.Count - 2 do
                                    let start = (PhysicsEngine2d.toPixelV2 vertices.[i]).Transform transform
                                    let stop = (PhysicsEngine2d.toPixelV2 vertices.[inc i]).Transform transform
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