// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.ComponentModel
open System.Collections.Generic
open FSharpx.Collections
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
    type [<StructuralEquality; StructuralComparison>] PhysicsId =
        { SourceId : Guid
          BodyId : Guid }

        /// The invalid physics id.
        static member InvalidId =
            { SourceId = InvalidId; BodyId = InvalidId }

        /// Make a PhysicsId for an external source.
        static member make (sourceId : Guid) =
            { SourceId = sourceId; BodyId = Core.makeId () }

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
    type BodyType =
        | Static
        | Kinematic
        | Dynamic

    /// The properties needed to describe the physical part of a body.
    type [<StructuralEquality; NoComparison>] BodyProperties =
        { BodyId : Guid
          Position : Vector2
          Rotation : single
          Shape : BodyShape
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
        { SourceAddress : obj Address
          SourceId : Guid
          BodyProperties : BodyProperties }

    /// A message to the physics system to create multiple bodies.
    type [<StructuralEquality; NoComparison>] CreateBodiesMessage =
        { SourceAddress : obj Address
          SourceId : Guid
          BodyPropertyList : BodyProperties list }

    /// A message to the physics system to destroy a body.
    type [<StructuralEquality; NoComparison>] DestroyBodyMessage =
        { PhysicsId : PhysicsId }

    /// A message to the physics system to destroy multiple bodies.
    type [<StructuralEquality; NoComparison>] DestroyBodiesMessage =
        { PhysicsIds : PhysicsId list }

    /// A message to the physics system to destroy a body.
    type [<StructuralEquality; NoComparison>] SetBodyPositionMessage =
        { PhysicsId : PhysicsId
          Position : Vector2 }

    /// A message to the physics system to set the rotation of a body.
    type [<StructuralEquality; NoComparison>] SetBodyRotationMessage =
        { PhysicsId : PhysicsId
          Rotation : single }

    /// A message to the physics system to set the linear velocity of a body.
    type [<StructuralEquality; NoComparison>] SetBodyLinearVelocityMessage =
        { PhysicsId : PhysicsId
          LinearVelocity : Vector2 }

    /// A message to the physics system to apply a linear impulse to a body.
    type [<StructuralEquality; NoComparison>] ApplyBodyLinearImpulseMessage =
        { PhysicsId : PhysicsId
          LinearImpulse : Vector2 }

    /// A message to the physics system to apply a force to a body.
    type [<StructuralEquality; NoComparison>] ApplyBodyForceMessage =
        { PhysicsId : PhysicsId
          Force : Vector2 }

    /// A message from the physics system describing a body collision that took place.
    type [<StructuralEquality; NoComparison>] BodyCollisionMessage =
        { SourceAddress : obj Address
          CollideeAddress : obj Address
          Normal : Vector2
          Speed : single }

    /// A message from the physics system describing the updated transform of a body.
    type [<StructuralEquality; NoComparison>] BodyTransformMessage =
        { SourceAddress : obj Address
          Position : Vector2
          Rotation : single }

    /// Tracks physics bodies by their PhysicsIds.
    type BodyDictionary = Dictionary<PhysicsId, Dynamics.Body>

    /// A message to the physics system.
    type [<StructuralEquality; NoComparison>] PhysicsMessage =
        | CreateBodyMessage of CreateBodyMessage
        | CreateBodiesMessage of CreateBodiesMessage
        | DestroyBodyMessage of DestroyBodyMessage
        | DestroyBodiesMessage of DestroyBodiesMessage
        | SetBodyPositionMessage of SetBodyPositionMessage
        | SetBodyRotationMessage of SetBodyRotationMessage
        | SetBodyLinearVelocityMessage of SetBodyLinearVelocityMessage
        | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
        | ApplyBodyForceMessage of ApplyBodyForceMessage
        | SetGravityMessage of Vector2
        | RebuildPhysicsHackMessage

    /// A message from the physics system.
    type [<StructuralEquality; NoComparison>] IntegrationMessage =
        | BodyCollisionMessage of BodyCollisionMessage
        | BodyTransformMessage of BodyTransformMessage

    /// The physics integrator. Represents the physics system in Nu.
    type IIntegrator =
        /// Query that the integrator contain the body with the given physics id.
        abstract BodyExists : PhysicsId -> bool
        /// Get the contact normals of the body with the given physics id.
        abstract GetBodyContactNormals : PhysicsId -> Vector2 list
        /// Get the linear velocity of the body with the given physics id.
        abstract GetBodyLinearVelocity : PhysicsId -> Vector2
        /// Get the contact normals where the body with the given physics id is touching the ground.
        abstract GetBodyGroundContactNormals : PhysicsId -> Vector2 list
        /// Try to get a contact normal where the body with the given physics id is touching the ground.
        abstract GetBodyOptGroundContactNormal : PhysicsId -> Vector2 option
        /// Try to get a contact tangent where the body with the given physics id is touching the ground.
        abstract GetBodyOptGroundContactTangent : PhysicsId -> Vector2 option
        /// Query that the body with the given physics id is on the ground.
        abstract BodyOnGround : PhysicsId -> bool
        /// Clear all of the physics messages that have been enqueued.
        abstract ClearMessages : unit -> IIntegrator
        /// Enqueue a message from an external source.
        abstract EnqueueMessage : PhysicsMessage -> IIntegrator
        /// Integrate the physics system one frame.
        abstract Integrate : int64 -> IntegrationMessage list * IIntegrator

    /// The primary implementation of IIntegrator.
    type [<ReferenceEquality>] Integrator =
        private
            { PhysicsContext : Dynamics.World
              Bodies : BodyDictionary
              PhysicsMessages : PhysicsMessage Queue
              IntegrationMessages : IntegrationMessage List
              FarseerCautionMode : bool // HACK: ensures two bodies aren't created in the same position, thus evading a Farseer bug
              mutable RebuildingHack : bool }

        static member private toPixel value =
            value * PhysicsToPixelRatio

        static member private toPhysics value =
            value * PixelToPhysicsRatio

        static member private toPixelV2 (v2 : Framework.Vector2) =
            Vector2 (Integrator.toPixel v2.X, Integrator.toPixel v2.Y)

        static member private toPhysicsV2 (v2 : Vector2) =
            Framework.Vector2 (Integrator.toPhysics v2.X, Integrator.toPhysics v2.Y)

        static member private toPhysicsPolygonDiameter value =
            let value = Integrator.toPhysics value
            value - Settings.PolygonRadius * 2.0f

        static member private toPhysicsPolygonRadius value =
            let value = Integrator.toPhysics value
            value - Settings.PolygonRadius

        static member private toPhysicsBodyType bodyType =
            match bodyType with
            | Static -> Dynamics.BodyType.Static
            | Kinematic -> Dynamics.BodyType.Kinematic
            | Dynamic -> Dynamics.BodyType.Dynamic

        static member private handleCollision
            integrator (fixture : Dynamics.Fixture) (fixture2 : Dynamics.Fixture) (contact : Dynamics.Contacts.Contact) =
            let normal = fst <| contact.GetWorldManifold ()
            let bodyCollisionMessage =
                { SourceAddress = fixture.Body.UserData :?> obj Address
                  CollideeAddress = fixture2.Body.UserData :?> obj Address
                  Normal = Vector2 (normal.X, normal.Y)
                  Speed = contact.TangentSpeed * PhysicsToPixelRatio }
            let integrationMessage = BodyCollisionMessage bodyCollisionMessage
            integrator.IntegrationMessages.Add integrationMessage
            true

        static member private getBodyContacts physicsId integrator =
            let body = integrator.Bodies.[physicsId]
            let contacts = List<Contact> ()
            let mutable current = body.ContactList
            while current <> null do
                contacts.Add current.Contact
                current <- current.Next
            List.ofSeq contacts

        static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
            body.Position <- Integrator.toPhysicsV2 bodyProperties.Position
            body.Rotation <- bodyProperties.Rotation
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

        static member private createBoxBody sourceAddress bodyProperties boxShape integrator =
            let body =
                Factories.BodyFactory.CreateRectangle
                    (integrator.PhysicsContext,
                     Integrator.toPhysicsPolygonDiameter <| boxShape.Extent.X * 2.0f,
                     Integrator.toPhysicsPolygonDiameter <| boxShape.Extent.Y * 2.0f,
                     bodyProperties.Density,
                     Integrator.toPhysicsV2 boxShape.Center,
                     0.0f,
                     Integrator.toPhysicsBodyType bodyProperties.BodyType,
                     sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
            body.UserData <- sourceAddress // BUG: ...so I set it again here :/
            body

        static member private createCircleBody sourceAddress bodyProperties (circleShape : CircleShape) integrator =
            let body =
                Factories.BodyFactory.CreateCircle
                    (integrator.PhysicsContext,
                     Integrator.toPhysicsPolygonRadius circleShape.Radius,
                     bodyProperties.Density,
                     Integrator.toPhysicsV2 circleShape.Center,
                     Integrator.toPhysicsBodyType bodyProperties.BodyType,
                     sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
            body.UserData <- sourceAddress // BUG: ...so I set it again here :/
            body

        static member private createCapsuleBody sourceAddress bodyProperties capsuleShape integrator =
            let body =
                Factories.BodyFactory.CreateCapsule
                    (integrator.PhysicsContext,
                     Integrator.toPhysicsPolygonDiameter capsuleShape.Height,
                     Integrator.toPhysicsPolygonRadius capsuleShape.Radius,
                     bodyProperties.Density,
                     Integrator.toPhysicsV2 capsuleShape.Center,
                     0.0f,
                     Integrator.toPhysicsBodyType bodyProperties.BodyType,
                     sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
            body.UserData <- sourceAddress // BUG: ...so I set it again here :/
            // scale in the capsule's box to stop sticking
            let capsuleBox = body.FixtureList.[0].Shape :?> FarseerPhysics.Collision.Shapes.PolygonShape
            ignore <| capsuleBox.Vertices.Scale (Framework.Vector2 (0.75f, 1.0f))
            body

        static member private createPolygonBody sourceAddress bodyProperties polygonShape integrator =
            let body =
                Factories.BodyFactory.CreatePolygon
                    (integrator.PhysicsContext,
                     FarseerPhysics.Common.Vertices (List.map Integrator.toPhysicsV2 polygonShape.Vertices),
                     bodyProperties.Density,
                     Integrator.toPhysicsV2 polygonShape.Center,
                     0.0f,
                     Integrator.toPhysicsBodyType bodyProperties.BodyType,
                     sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
            body.UserData <- sourceAddress // BUG: ...so I set it again here :/
            body

        static member private createBody4 sourceId sourceAddress bodyProperties integrator =
        
            // make and configure the body
            let body =
                match bodyProperties.Shape with
                | BoxShape boxShape -> Integrator.createBoxBody sourceAddress bodyProperties boxShape integrator
                | CircleShape circleShape -> Integrator.createCircleBody sourceAddress bodyProperties circleShape integrator
                | CapsuleShape capsuleShape -> Integrator.createCapsuleBody sourceAddress bodyProperties capsuleShape integrator
                | PolygonShape polygonShape -> Integrator.createPolygonBody sourceAddress bodyProperties polygonShape integrator
            Integrator.configureBodyProperties bodyProperties body
            body.add_OnCollision (fun fn fn2 collision -> Integrator.handleCollision integrator fn fn2 collision) // NOTE: F# requires us to use an lambda inline here (not sure why)

            // make a very hack-assed attempt to keep mobile bodies from being created in the same position
            let isBodyMobile = match bodyProperties.BodyType with Static -> false | Dynamic | Kinematic -> true
            if  isBodyMobile &&
                integrator.FarseerCautionMode &&
                integrator.Bodies |> Seq.exists (fun kvp -> kvp.Value.Position = body.Position)  then
                let random = System.Random ()
                let randomOffset = Framework.Vector2 (single <| random.NextDouble (), single <| random.NextDouble ())
                body.Position <- body.Position + randomOffset

            // attempt to add the body
            if not <| integrator.Bodies.TryAdd ({ SourceId = sourceId; BodyId = bodyProperties.BodyId }, body) then
                debug <| "Could not add body via '" + acstring bodyProperties + "'."

        static member private createBodies (createBodiesMessage : CreateBodiesMessage) integrator =
            List.iter
                (fun bodyProperties -> Integrator.createBody4 createBodiesMessage.SourceId createBodiesMessage.SourceAddress bodyProperties integrator)
                createBodiesMessage.BodyPropertyList

        static member private createBody (createBodyMessage : CreateBodyMessage) integrator =
            Integrator.createBody4 createBodyMessage.SourceId createBodyMessage.SourceAddress createBodyMessage.BodyProperties integrator

        static member private destroyBody2 physicsId integrator =
            match integrator.Bodies.TryGetValue physicsId with
            | (true, body) ->
                ignore <| integrator.Bodies.Remove physicsId
                integrator.PhysicsContext.RemoveBody body
            | (false, _) ->
                if not integrator.RebuildingHack then
                    debug <| "Could not destroy non-existent body with PhysicsId = " + acstring physicsId + "'."

        static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) integrator =
            Integrator.destroyBody2 destroyBodyMessage.PhysicsId integrator

        static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) integrator =
            List.iter (fun physicsId -> Integrator.destroyBody2 physicsId integrator) destroyBodiesMessage.PhysicsIds

        static member private setBodyPosition (setBodyPositionMessage : SetBodyPositionMessage) integrator =
            match integrator.Bodies.TryGetValue setBodyPositionMessage.PhysicsId with
            | (true, body) -> body.Position <- Integrator.toPhysicsV2 setBodyPositionMessage.Position
            | (false, _) -> debug <| "Could not set position of non-existent body with PhysicsId = " + acstring setBodyPositionMessage.PhysicsId + "'."

        static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) integrator =
            match integrator.Bodies.TryGetValue setBodyRotationMessage.PhysicsId with
            | (true, body) -> body.Rotation <- setBodyRotationMessage.Rotation
            | (false, _) -> debug <| "Could not set rotation of non-existent body with PhysicsId = " + acstring setBodyRotationMessage.PhysicsId + "'."

        static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) integrator =
            match integrator.Bodies.TryGetValue setBodyLinearVelocityMessage.PhysicsId with
            | (true, body) -> body.LinearVelocity <- Integrator.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
            | (false, _) -> debug <| "Could not set linear velocity of non-existent body with PhysicsId = " + acstring setBodyLinearVelocityMessage.PhysicsId + "'."

        static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) integrator =
            match integrator.Bodies.TryGetValue applyBodyLinearImpulseMessage.PhysicsId with
            | (true, body) -> body.ApplyLinearImpulse (Integrator.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
            | (false, _) -> debug <| "Could not apply linear impulse to non-existent body with PhysicsId = " + acstring applyBodyLinearImpulseMessage.PhysicsId + "'."

        static member private applyBodyForce applyBodyForceMessage integrator =
            match integrator.Bodies.TryGetValue applyBodyForceMessage.PhysicsId with
            | (true, body) -> body.ApplyForce (Integrator.toPhysicsV2 applyBodyForceMessage.Force)
            | (false, _) -> debug <| "Could not apply force to non-existent body with PhysicsId = " + acstring applyBodyForceMessage.PhysicsId + "'."

        static member private handlePhysicsMessage integrator physicsMessage =
            match physicsMessage with
            | CreateBodyMessage createBodyMessage -> Integrator.createBody createBodyMessage integrator
            | CreateBodiesMessage createBodiesMessage -> Integrator.createBodies createBodiesMessage integrator
            | DestroyBodyMessage destroyBodyMessage -> Integrator.destroyBody destroyBodyMessage integrator
            | DestroyBodiesMessage destroyBodiesMessage -> Integrator.destroyBodies destroyBodiesMessage integrator
            | SetBodyPositionMessage setBodyPositionMessage -> Integrator.setBodyPosition setBodyPositionMessage integrator
            | SetBodyRotationMessage setBodyRotationMessage -> Integrator.setBodyRotation setBodyRotationMessage integrator
            | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> Integrator.setBodyLinearVelocity setBodyLinearVelocityMessage integrator
            | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> Integrator.applyBodyLinearImpulse applyBodyLinearImpulseMessage integrator
            | ApplyBodyForceMessage applyBodyForceMessage -> Integrator.applyBodyForce applyBodyForceMessage integrator
            | SetGravityMessage gravity -> integrator.PhysicsContext.Gravity <- Integrator.toPhysicsV2 gravity
            | RebuildPhysicsHackMessage ->
                integrator.RebuildingHack <- true
                integrator.PhysicsContext.Clear ()
                integrator.Bodies.Clear ()
                integrator.IntegrationMessages.Clear ()

        static member private handlePhysicsMessages physicsMessages integrator =
            for physicsMessage in physicsMessages do
                Integrator.handlePhysicsMessage integrator physicsMessage
            integrator.RebuildingHack <- false

        static member private createTransformMessages integrator =
            for body in integrator.Bodies.Values do
                if body.Awake && not body.IsStatic then
                    let bodyTransformMessage =
                        BodyTransformMessage
                            { SourceAddress = body.UserData :?> obj Address
                              Position = Integrator.toPixelV2 body.Position
                              Rotation = body.Rotation }
                    integrator.IntegrationMessages.Add bodyTransformMessage

        /// Make an integrator.
        static member make farseerCautionMode gravity =
            let integrator =
                { PhysicsContext = FarseerPhysics.Dynamics.World (Integrator.toPhysicsV2 gravity)
                  Bodies = BodyDictionary HashIdentity.Structural
                  PhysicsMessages = Queue.empty
                  IntegrationMessages = List<IntegrationMessage> ()
                  FarseerCautionMode = farseerCautionMode
                  RebuildingHack = false }
            integrator :> IIntegrator

        interface IIntegrator with

            member integrator.BodyExists physicsId =
                integrator.Bodies.ContainsKey physicsId

            member integrator.GetBodyContactNormals physicsId =
                let contacts = Integrator.getBodyContacts physicsId integrator
                List.map
                    (fun (contact : Contact) ->
                        let normal = fst <| contact.GetWorldManifold ()
                        Vector2 (normal.X, normal.Y))
                    contacts

            member integrator.GetBodyLinearVelocity physicsId =
                let body = integrator.Bodies.[physicsId]
                Integrator.toPixelV2 body.LinearVelocity

            member integrator.GetBodyGroundContactNormals physicsId =
                let normals = (integrator :> IIntegrator).GetBodyContactNormals physicsId
                List.filter
                    (fun normal ->
                        let theta = Vector2.Dot (normal, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
                        theta < Math.PI * 0.25)
                    normals

            member integrator.GetBodyOptGroundContactNormal physicsId =
                let groundNormals = (integrator :> IIntegrator).GetBodyGroundContactNormals physicsId
                match groundNormals with
                | [] -> None
                | _ :: _ ->
                    let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                    Some averageNormal

            member integrator.GetBodyOptGroundContactTangent physicsId =
                match (integrator :> IIntegrator).GetBodyOptGroundContactNormal physicsId with
                | Some normal -> Some <| Vector2 (normal.Y, -normal.X)
                | None -> None

            member integrator.BodyOnGround physicsId =
                let groundNormals = (integrator :> IIntegrator).GetBodyGroundContactNormals physicsId
                not <| List.isEmpty groundNormals

            member integrator.ClearMessages () =
                let integrator = { integrator with PhysicsMessages = Queue.empty }
                integrator :> IIntegrator

            member integrator.EnqueueMessage physicsMessage =
                let physicsMessages = Queue.conj physicsMessage integrator.PhysicsMessages
                let integrator = { integrator with PhysicsMessages = physicsMessages }
                integrator :> IIntegrator

            member integrator.Integrate tickRate =
                let physicsMessages = integrator.PhysicsMessages
                let integrator = { integrator with PhysicsMessages = Queue.empty }
                Integrator.handlePhysicsMessages physicsMessages integrator
                let physicsStepAmount = PhysicsStepRate * single tickRate
                integrator.PhysicsContext.Step physicsStepAmount
                Integrator.createTransformMessages integrator
                let messages = List.ofSeq integrator.IntegrationMessages
                integrator.IntegrationMessages.Clear ()
                (messages, integrator :> IIntegrator)

    /// The mock implementation of IIntegrator.
    type MockIntegrator =
        { MockIntegrator : unit }
        interface IIntegrator with
            member integrator.BodyExists _ = false
            member integrator.GetBodyContactNormals _ = failwith "No bodies in MockIntegrator"
            member integrator.GetBodyLinearVelocity _ = failwith "No bodies in MockIntegrator"
            member integrator.GetBodyGroundContactNormals _ = failwith "No bodies in MockIntegrator"
            member integrator.GetBodyOptGroundContactNormal _ = failwith "No bodies in MockIntegrator"
            member integrator.GetBodyOptGroundContactTangent _ = failwith "No bodies in MockIntegrator"
            member integrator.BodyOnGround _ = failwith "No bodies in MockIntegrator"
            member integrator.ClearMessages () = integrator :> IIntegrator
            member integrator.EnqueueMessage _ = integrator :> IIntegrator
            member integrator.Integrate _ = ([], integrator :> IIntegrator)

[<RequireQualifiedAccess>]
module Physics =

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
    /// TODO: see if AlgebraicConverter can be used here instead of this shitty custom syntax.
    /// TODO: propagate errors rather than tracing in place
    let evalCollisionExpr (extent : Vector2) (expr : string) =
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
            try let vertices = List.map (fun str -> AlgebraicDescriptor.convertFromString str typeof<Vector2> :?> Vector2) vertexStrs
                let vertices = List.map (fun vertex -> vertex - Vector2 0.5f) vertices
                let vertices = List.map (fun vertex -> Vector2.Multiply (vertex, extent)) vertices
                PolygonShape { Vertices = vertices; Center = Vector2.Zero }
            with :? NotSupportedException ->
                trace <| "Could not parse collision polygon vertices '" + verticesStr + "'. Format is 'Polygon? 0.0, 0.0 | 0.0, 1.0 | 1.0, 1.0 | 1.0, 0.0'"
                defaultShape
        | _ -> trace <| "Invalid tile collision expression '" + expr + "'."; defaultShape