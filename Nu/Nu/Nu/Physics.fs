namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open FarseerPhysics
open FarseerPhysics.Common
open FarseerPhysics.Dynamics
open FarseerPhysics.Dynamics.Contacts
open OpenTK
open Microsoft.Xna
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module PhysicsModule =

    /// Identifies a target whose body can be found in the Integrator.
    /// TODO: remove PhysicsId and have no more than one body per entity!
    type [<StructuralEquality; StructuralComparison>] PhysicsId =
        struct
            val Major : Guid
            val Minor : Guid
            new (major, minor) = { Major = major; PhysicsId.Minor = minor }
            override this.ToString () = "{Major = " + string this.Major + "; Minor = " + string this.Minor + "}"
            end

    /// Physics-specific vertices type.
    type Vertices = Vector2 list

    /// The shape of a physics box.
    type [<StructuralEquality; NoComparison>] BoxShape =
        { Extent : Vector2
          Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

    /// The shape of a physics circle.
    type [<StructuralEquality; NoComparison>] CircleShape =
        { Radius : single
          Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

    /// The shape of a physics capsule.
    type [<StructuralEquality; NoComparison>] CapsuleShape =
        { Height : single
          Radius : single
          Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

    /// The shape of a physics polygon.
    type [<StructuralEquality; NoComparison>] PolygonShape =
        { Vertices : Vertices
          Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

    /// The shape of a physics body.
    type [<StructuralEquality; NoComparison>] BodyShape =
        | BoxShape of BoxShape
        | CircleShape of CircleShape
        | CapsuleShape of CapsuleShape
        | PolygonShape of PolygonShape

    /// The type of a physics body; Static, Kinematic, or Dynamic.
    type [<StructuralEquality; NoComparison; TypeConverter (typeof<BodyTypeTypeConverter>)>] BodyType =
        | Static
        | Kinematic
        | Dynamic

    /// Converts BodyType types.
    /// TODO: factor out a simple DU type converter.
    and BodyTypeTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, _, source, _) =
            let bodyType = source :?> BodyType
            match bodyType with
            | Static -> "Static" :> obj
            | Kinematic -> "Kinematic" :> obj
            | Dynamic -> "Dynamic" :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector2> || sourceType = typeof<string>
        override this.ConvertFrom (_, _, source) =
            let sourceType = source.GetType ()
            if sourceType = typeof<BodyType> then source
            else
                match source :?> string with
                | "Static" -> Static :> obj
                | "Kinematic" -> Kinematic :> obj
                | "Dynamic" -> Dynamic :> obj
                | other -> failwith <| "Unknown BodyType '" + other + "'."

    /// The properties needed to describe a physical body.
    type [<StructuralEquality; NoComparison>] BodyProperties =
        { Shape : BodyShape
          BodyType : BodyType
          Density : single
          Friction : single
          Restitution : single
          FixedRotation : bool
          LinearDamping : single
          AngularDamping : single
          GravityScale : single
          CollisionCategories : int
          CollisionMask : int
          IsBullet : bool
          IsSensor : bool }

    /// A message to the physics system to create a body.
    type [<StructuralEquality; NoComparison>] CreateBodyMessage =
        { EntityAddress : Address
          PhysicsId : PhysicsId
          Position : Vector2
          Rotation : single
          BodyProperties : BodyProperties }

    /// A message to the physics system to destroy a body.
    type [<StructuralEquality; NoComparison>] DestroyBodyMessage =
        { PhysicsId : PhysicsId }

    /// A message to the physics system to destroy a body.
    type [<StructuralEquality; NoComparison>] SetPositionMessage =
        { PhysicsId : PhysicsId
          Position : Vector2 }

    /// A message to the physics system to set the rotation of a body.
    type [<StructuralEquality; NoComparison>] SetRotationMessage =
        { PhysicsId : PhysicsId
          Rotation : single }

    /// A message to the physics system to set the linear velocity of a body.
    type [<StructuralEquality; NoComparison>] SetLinearVelocityMessage =
        { PhysicsId : PhysicsId
          LinearVelocity : Vector2 }

    /// A message to the physics system to apply a linear impulse to a body.
    type [<StructuralEquality; NoComparison>] ApplyLinearImpulseMessage =
        { PhysicsId : PhysicsId
          LinearImpulse : Vector2 }

    /// A message to the physics system to apply a force to a body.
    type [<StructuralEquality; NoComparison>] ApplyForceMessage =
        { PhysicsId : PhysicsId
          Force : Vector2 }

    /// A message from the physics system describing a body collision that took place.
    type [<StructuralEquality; NoComparison>] BodyCollisionMessage =
        { EntityAddress : Address
          EntityAddress2 : Address
          Normal : Vector2
          Speed : single }

    /// A message from the physics system describing the updated transform of a body.
    type [<StructuralEquality; NoComparison>] BodyTransformMessage =
        { EntityAddress : Address
          Position : Vector2
          Rotation : single }

    /// Tracks physics bodies by their PhysicsIds.
    type BodyDictionary = Dictionary<PhysicsId, Dynamics.Body>

    /// A message to the physics system.
    type [<StructuralEquality; NoComparison>] PhysicsMessage =
        | CreateBodyMessage of CreateBodyMessage
        | DestroyBodyMessage of DestroyBodyMessage
        | SetPositionMessage of SetPositionMessage
        | SetRotationMessage of SetRotationMessage
        | SetLinearVelocityMessage of SetLinearVelocityMessage
        | ApplyLinearImpulseMessage of ApplyLinearImpulseMessage
        | ApplyForceMessage of ApplyForceMessage
        | SetGravityMessage of Vector2
        | RebuildPhysicsHackMessage

    /// A message from the physics system.
    type [<StructuralEquality; NoComparison>] IntegrationMessage =
        | BodyCollisionMessage of BodyCollisionMessage
        | BodyTransformMessage of BodyTransformMessage

    /// The physics integrator. Represent the physics system in Nu.
    /// NOTE: you should never access the PhysicsContext from outside the physics system.
    /// TODO: consider making the Integrator an abstract data type to hide the PhysicsContext, or
    /// at least give said field a name that communicates its desired privacy.
    type [<ReferenceEquality>] Integrator =
        { PhysicsContext : Dynamics.World
          Bodies : BodyDictionary
          IntegrationMessages : IntegrationMessage List
          FarseerCautionMode : bool // HACK: ensures two bodies aren't created in the same position, thus evading a Farseer bug
          mutable RebuildingHack : bool }
          
[<RequireQualifiedAccess>]
module Physics =

    /// The invalid physics id.
    let InvalidId =
        PhysicsId (Core.InvalidId, Core.InvalidId)

    /// Make a PhysicsId for an external entity.
    let makeId (entityId : Guid) =
        PhysicsId (entityId, Core.makeId ())

    let private toPixel value =
        value * PhysicsToPixelRatio

    let private toPhysics value =
        value * PixelToPhysicsRatio

    let private toPixelV2 (v2 : Framework.Vector2) =
        Vector2 (toPixel v2.X, toPixel v2.Y)

    let private toPhysicsV2 (v2 : Vector2) =
        Framework.Vector2 (toPhysics v2.X, toPhysics v2.Y)

    let private toPhysicsPolygonDiameter value =
        let value = toPhysics value
        value - Settings.PolygonRadius * 2.0f

    let private toPhysicsPolygonRadius value =
        let value = toPhysics value
        value - Settings.PolygonRadius

    let private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    let private getNormalAndManifold (contact : Contact) =
        let (normal, manifold) = (ref <| Framework.Vector2 (), ref <| FixedArray2<Framework.Vector2> ())
        contact.GetWorldManifold (normal, manifold)
        (!normal, !manifold)

    let private handleCollision
        integrator
        (fixture : Dynamics.Fixture)
        (fixture2 : Dynamics.Fixture)
        (contact : Dynamics.Contacts.Contact) =
        let (normal, _) = getNormalAndManifold contact
        let bodyCollisionMessage =
            { EntityAddress = fixture.Body.UserData :?> Address
              EntityAddress2 = fixture2.Body.UserData :?> Address
              Normal = Vector2 (normal.X, normal.Y)
              Speed = contact.TangentSpeed * PhysicsToPixelRatio }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        integrator.IntegrationMessages.Add integrationMessage
        true

    let private getBodyContacts physicsId integrator =
        let body = integrator.Bodies.[physicsId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while current <> null do
            contacts.Add current.Contact
            current <- current.Next
        List.ofSeq contacts

    /// Does the integrator contain the body with the given physics id?
    let bodyExists physicsId integrator =
        integrator.Bodies.ContainsKey physicsId

    /// Get the contact normals of the body with the given physics id.
    let getBodyContactNormals physicsId integrator =
        let contacts = getBodyContacts physicsId integrator
        List.map
            (fun (contact : Contact) ->
                let (normal, _) = getNormalAndManifold contact
                Vector2 (normal.X, normal.Y))
            contacts

    /// Get the linear velocity of the body with the given physics id.
    let getBodyLinearVelocity physicsId integrator =
        let body = integrator.Bodies.[physicsId]
        toPixelV2 body.LinearVelocity

    /// Get the contact normals where the body with the given physics id is touching the ground.
    let getBodyGroundContactNormals physicsId integrator =
        let normals = getBodyContactNormals physicsId integrator
        List.filter
            (fun normal ->
                let theta = Vector2.Dot (normal, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
                theta < Math.PI * 0.25)
            normals

    /// Try to get a contact normal where the body with the given physics id is touching the ground.
    let getOptBodyGroundContactNormal physicsId integrator =
        let groundNormals = getBodyGroundContactNormals physicsId integrator
        match groundNormals with
        | [] -> None
        | _ :: _ ->
            let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
            Some averageNormal

    /// Try to get a contact tangent where the body with the given physics id is touching the ground.
    let getOptBodyGroundContactTangent physicsId integrator =
        match getOptBodyGroundContactNormal physicsId integrator with
        | Some normal -> Some <| Vector2 (normal.Y, -normal.X)
        | None -> None

    /// Query that the body with the give physics id is on the ground.
    let isBodyOnGround physicsId integrator =
        let groundNormals = getBodyGroundContactNormals physicsId integrator
        not <| List.isEmpty groundNormals

    /// Convert a category expression to a value that represents collision categories.
    /// Examples -
    ///     * = -1
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either * or a binary number!
    let toCollisionCategories categoryExpr =
        match categoryExpr with
        | "*" -> -1
        | _ -> Convert.ToInt32 (categoryExpr, 2)

    /// Evaluate a collision expression.
    /// TODO: explain syntax.
    /// TODO: propagate errors rather than tracing in place
    let evalCollisionExpression (extent : Vector2) (expr : string) =
        let terms = List.ofArray <| expr.Split '?'
        let terms = List.map (fun (term : string) -> term.Trim ()) terms
        let defaultShape = BoxShape { Extent = extent * 0.5f; Center = Vector2.Zero }
        match terms with
        | [""] -> defaultShape
        | ["Box"] -> defaultShape
        | ["Circle"] -> CircleShape { Radius = extent.X * 0.5f; Center = Vector2.Zero }
        | ["Capsule"] -> CapsuleShape { Height = extent.Y * 0.5f; Radius = extent.Y * 0.25f; Center = Vector2.Zero }
        | ["Polygon"; verticesStr] ->
            let vertexStrs = List.ofArray <| verticesStr.Split '|'
            try let vertices = List.map (fun str -> (TypeDescriptor.GetConverter (typeof<Vector2>)).ConvertFromString str :?> Vector2) vertexStrs
                let vertices = List.map (fun vertex -> vertex - Vector2 0.5f) vertices
                let vertices = List.map (fun vertex -> Vector2.Multiply (vertex, extent)) vertices
                PolygonShape { Vertices = vertices; Center = Vector2.Zero }
            with :? NotSupportedException ->
                trace <| "Could not parse collision polygon vertices '" + verticesStr + "'. Format is 'Polygon ? 0.0;0.0 | 0.0;1.0 | 1.0;1.0 | 1.0;0.0'"
                defaultShape
        | _ ->
            trace <| "Invalid tile collision expression '" + expr + "'."
            defaultShape

    let private configureBodyProperties bodyPosition bodyRotation bodyProperties (body : Body) =
        body.Position <- toPhysicsV2 bodyPosition
        body.Rotation <- bodyRotation
        body.Friction <- bodyProperties.Friction
        body.Restitution <- bodyProperties.Restitution
        body.FixedRotation <- bodyProperties.FixedRotation
        body.LinearDamping <- bodyProperties.LinearDamping
        body.AngularDamping <- bodyProperties.AngularDamping
        body.GravityScale <- bodyProperties.GravityScale
        body.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
        body.CollidesWith <- enum<Category> bodyProperties.CollisionMask
        body.IsBullet <- bodyProperties.IsBullet
        body.IsSensor <- bodyProperties.IsSensor
        body.SleepingAllowed <- true

    let private createBoxBody (createBodyMessage : CreateBodyMessage) boxShape integrator =
        let body =
            Factories.BodyFactory.CreateRectangle (
                integrator.PhysicsContext,
                toPhysicsPolygonDiameter <| boxShape.Extent.X * 2.0f,
                toPhysicsPolygonDiameter <| boxShape.Extent.Y * 2.0f,
                createBodyMessage.BodyProperties.Density,
                toPhysicsV2 boxShape.Center,
                0.0f,
                toPhysicsBodyType createBodyMessage.BodyProperties.BodyType,
                createBodyMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- createBodyMessage.EntityAddress // BUG: ...so I set it again here :/
        body

    let private createCircleBody (createBodyMessage : CreateBodyMessage) (circleShape : CircleShape) integrator =
        let body =
            Factories.BodyFactory.CreateCircle (
                integrator.PhysicsContext,
                toPhysicsPolygonRadius circleShape.Radius,
                createBodyMessage.BodyProperties.Density,
                toPhysicsV2 circleShape.Center,
                toPhysicsBodyType createBodyMessage.BodyProperties.BodyType,
                createBodyMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- createBodyMessage.EntityAddress // BUG: ...so I set it again here :/
        body

    let private createCapsuleBody (createBodyMessage : CreateBodyMessage) capsuleShape integrator =
        let body =
            Factories.BodyFactory.CreateCapsule (
                integrator.PhysicsContext,
                toPhysicsPolygonDiameter capsuleShape.Height,
                toPhysicsPolygonRadius capsuleShape.Radius,
                createBodyMessage.BodyProperties.Density,
                toPhysicsV2 capsuleShape.Center,
                0.0f,
                toPhysicsBodyType createBodyMessage.BodyProperties.BodyType,
                createBodyMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- createBodyMessage.EntityAddress // BUG: ...so I set it again here :/
        // scale in the capsule's box to stop sticking
        let capsuleBox = body.FixtureList.[0].Shape :?> FarseerPhysics.Collision.Shapes.PolygonShape
        ignore <| capsuleBox.Vertices.Scale (Framework.Vector2 (0.75f, 1.0f))
        body

    let private createPolygonBody (createBodyMessage : CreateBodyMessage) polygonShape integrator =
        let body =
            Factories.BodyFactory.CreatePolygon (
                integrator.PhysicsContext,
                FarseerPhysics.Common.Vertices (List.map toPhysicsV2 polygonShape.Vertices),
                createBodyMessage.BodyProperties.Density,
                toPhysicsV2 polygonShape.Center,
                0.0f,
                toPhysicsBodyType createBodyMessage.BodyProperties.BodyType,
                createBodyMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- createBodyMessage.EntityAddress // BUG: ...so I set it again here :/
        body

    let private createBody createBodyMessage integrator =
        
        // make and configure the body
        let body =
            match createBodyMessage.BodyProperties.Shape with
            | BoxShape boxShape -> createBoxBody createBodyMessage boxShape integrator
            | CircleShape circleShape -> createCircleBody createBodyMessage circleShape integrator
            | CapsuleShape capsuleShape -> createCapsuleBody createBodyMessage capsuleShape integrator
            | PolygonShape polygonShape -> createPolygonBody createBodyMessage polygonShape integrator
        configureBodyProperties createBodyMessage.Position createBodyMessage.Rotation createBodyMessage.BodyProperties body
        body.add_OnCollision (fun fn fn2 collision -> handleCollision integrator fn fn2 collision) // NOTE: F# requires us to use an lambda inline here (not sure why)
        
        // make a very hack-assed attempt to keep to bodies from being created in the same position
        if  integrator.FarseerCautionMode &&
            integrator.Bodies |> Seq.exists (fun kvp -> kvp.Value.Position = body.Position)  then
            let random = System.Random ()
            let randomOffset = Framework.Vector2 (single <| random.NextDouble (), single <| random.NextDouble ())
            body.Position <- body.Position + randomOffset
        
        // attempt to add the body
        if not <| integrator.Bodies.TryAdd (createBodyMessage.PhysicsId, body) then
            debug <| "Could not add body via '" + string createBodyMessage + "'."

    let private destroyBody (destroyBodyMessage : DestroyBodyMessage) integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (destroyBodyMessage.PhysicsId, body) then
            ignore <| integrator.Bodies.Remove destroyBodyMessage.PhysicsId
            integrator.PhysicsContext.RemoveBody !body
        elif not integrator.RebuildingHack then
             debug <| "Could not destroy non-existent body with PhysicsId = " + string destroyBodyMessage.PhysicsId + "'."

    let private setPosition (setPositionMessage : SetPositionMessage) integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (setPositionMessage.PhysicsId, body) then
            (!body).Position <- toPhysicsV2 setPositionMessage.Position
        else debug <| "Could not set position of non-existent body with PhysicsId = " + string setPositionMessage.PhysicsId + "'."

    let private setRotation (setRotationMessage : SetRotationMessage) integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (setRotationMessage.PhysicsId, body) then
            (!body).Rotation <- setRotationMessage.Rotation
        else debug <| "Could not set rotation of non-existent body with PhysicsId = " + string setRotationMessage.PhysicsId + "'."

    let private setLinearVelocity (setLinearVelocityMessage : SetLinearVelocityMessage) integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (setLinearVelocityMessage.PhysicsId, body) then
            (!body).LinearVelocity <- toPhysicsV2 setLinearVelocityMessage.LinearVelocity
        else debug <| "Could not set linear velocity of non-existent body with PhysicsId = " + string setLinearVelocityMessage.PhysicsId + "'."

    let private applyLinearImpulse (applyLinearImpulseMessage : ApplyLinearImpulseMessage) integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (applyLinearImpulseMessage.PhysicsId, body) then
            (!body).ApplyLinearImpulse (toPhysicsV2 applyLinearImpulseMessage.LinearImpulse)
        else debug <| "Could not apply linear impulse to non-existent body with PhysicsId = " + string applyLinearImpulseMessage.PhysicsId + "'."

    let private applyForce applyForceMessage integrator =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (applyForceMessage.PhysicsId, body) then
            (!body).ApplyForce (toPhysicsV2 applyForceMessage.Force)
        else debug <| "Could not apply force to non-existent body with PhysicsId = " + string applyForceMessage.PhysicsId + "'."

    let private handlePhysicsMessage integrator physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> createBody createBodyMessage integrator
        | DestroyBodyMessage destroyBodyMessage -> destroyBody destroyBodyMessage integrator
        | SetPositionMessage setPositionMessage -> setPosition setPositionMessage integrator
        | SetRotationMessage setRotationMessage -> setRotation setRotationMessage integrator
        | SetLinearVelocityMessage setLinearVelocityMessage -> setLinearVelocity setLinearVelocityMessage integrator
        | ApplyLinearImpulseMessage applyLinearImpulseMessage -> applyLinearImpulse applyLinearImpulseMessage integrator
        | ApplyForceMessage applyForceMessage -> applyForce applyForceMessage integrator
        | SetGravityMessage gravity -> integrator.PhysicsContext.Gravity <- toPhysicsV2 gravity
        | RebuildPhysicsHackMessage ->
            integrator.RebuildingHack <- true
            integrator.PhysicsContext.Clear ()
            integrator.Bodies.Clear ()
            integrator.IntegrationMessages.Clear ()

    let private handlePhysicsMessages (physicsMessages : PhysicsMessage rQueue) integrator =
        let physicsMessagesRev = List.rev physicsMessages
        for physicsMessage in physicsMessagesRev do
            handlePhysicsMessage integrator physicsMessage
        integrator.RebuildingHack <- false

    let private createTransformMessages integrator =
        for body in integrator.Bodies.Values do
            if body.Awake && not body.IsStatic then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { EntityAddress = body.UserData :?> Address
                          Position = toPixelV2 body.Position
                          Rotation = body.Rotation }
                integrator.IntegrationMessages.Add bodyTransformMessage

    /// Integrate (or 'tick') the physics system one frame.
    let integrate (physicsMessages : PhysicsMessage rQueue) integrator =
        handlePhysicsMessages physicsMessages integrator
        integrator.PhysicsContext.Step PhysicsStepRate
        createTransformMessages integrator
        let messages = List.ofSeq integrator.IntegrationMessages
        integrator.IntegrationMessages.Clear ()
        messages

    /// Make a physics Integrator.
    let makeIntegrator farseerCautionMode gravity =
         { PhysicsContext = FarseerPhysics.Dynamics.World (toPhysicsV2 gravity)
           Bodies = BodyDictionary HashIdentity.Structural
           IntegrationMessages = List<IntegrationMessage> ()
           FarseerCautionMode = farseerCautionMode
           RebuildingHack = false }