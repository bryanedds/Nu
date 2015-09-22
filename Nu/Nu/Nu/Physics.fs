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

/// Identifies a target whose body can be found in the physics engine.
type [<CustomEquality; NoComparison>] PhysicsId =
    { SourceId : Guid
      BodyId : Guid }

    /// The invalid physics id.
    static member InvalidId =
        { SourceId = Constants.Engine.InvalidId; BodyId = Constants.Engine.InvalidId }

    /// Hash a PhysicsId.
    static member hash pid =
        pid.SourceId.GetHashCode () ^^^ pid.BodyId.GetHashCode ()

    /// Equate PhysicsIds.
    static member equals pid pid2 =
        pid.SourceId = pid2.SourceId &&
        pid.BodyId = pid2.BodyId

    /// Make a PhysicsId for an external source.
    static member make (sourceId : Guid) =
        { SourceId = sourceId; BodyId = Core.makeId () }

    interface PhysicsId IEquatable with
        member this.Equals that =
            PhysicsId.equals this that

    override this.Equals that =
        match that with
        | :? PhysicsId as that -> PhysicsId.equals this that
        | _ -> false

    override this.GetHashCode () =
        PhysicsId.hash this

/// Physics-specific vertices type.
type Vertices = Vector2 list

/// The shape of a physics body box.
type [<StructuralEquality; NoComparison>] BodyBox =
    { Extent : Vector2
      Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

/// The shape of a physics body circle.
type [<StructuralEquality; NoComparison>] BodyCircle =
    { Radius : single
      Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

/// The shape of a physics body capsule.
type [<StructuralEquality; NoComparison>] BodyCapsule =
    { Height : single
      Radius : single
      Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

/// The shape of a physics body polygon.
type [<StructuralEquality; NoComparison>] BodyPolygon =
    { Vertices : Vertices
      Center : Vector2 } // NOTE: I guess this is like a center offset for the shape?

/// The shape of a physics body.
type [<StructuralEquality; NoComparison>] BodyShape =
    | BodyBox of BodyBox
    | BodyCircle of BodyCircle
    | BodyCapsule of BodyCapsule
    | BodyPolygon of BodyPolygon

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
      Awake : bool
      Density : single
      Friction : single
      Restitution : single
      FixedRotation : bool
      AngularVelocity : single
      AngularDamping : single
      LinearVelocity : Vector2
      LinearDamping : single
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

/// A message to the physics system to set the angular velocity of a body.
type [<StructuralEquality; NoComparison>] SetBodyAngularVelocityMessage =
    { PhysicsId : PhysicsId
      AngularVelocity : single }

/// A message to the physics system to apply a angular impulse to a body.
type [<StructuralEquality; NoComparison>] ApplyBodyAngularImpulseMessage =
    { PhysicsId : PhysicsId
      AngularImpulse : single }

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
    | SetBodyAngularVelocityMessage of SetBodyAngularVelocityMessage
    | ApplyBodyAngularImpulseMessage of ApplyBodyAngularImpulseMessage
    | SetBodyLinearVelocityMessage of SetBodyLinearVelocityMessage
    | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
    | ApplyBodyForceMessage of ApplyBodyForceMessage
    | SetGravityMessage of Vector2
    | RebuildPhysicsHackMessage

/// A message from the physics system.
type [<StructuralEquality; NoComparison>] IntegrationMessage =
    | BodyCollisionMessage of BodyCollisionMessage
    | BodyTransformMessage of BodyTransformMessage

/// Represents a physics engine in Nu.
type IPhysicsEngine =
    /// Query that the physics engine contain the body with the given physics id.
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
    abstract ClearMessages : unit -> IPhysicsEngine
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : PhysicsMessage -> IPhysicsEngine
    /// Integrate the physics system one frame.
    abstract Integrate : int64 -> IntegrationMessage list * IPhysicsEngine

/// The primary implementation of IPhysicsEngine.
type [<ReferenceEquality>] PhysicsEngine =
    private
        { PhysicsContext : Dynamics.World
          Bodies : BodyDictionary
          PhysicsMessages : PhysicsMessage Queue
          IntegrationMessages : IntegrationMessage List
          mutable RebuildingHack : bool }

    static member private toPixel value =
        value * Constants.Physics.PhysicsToPixelRatio

    static member private toPhysics value =
        value * Constants.Physics.PixelToPhysicsRatio

    static member private toPixelV2 (v2 : Framework.Vector2) =
        Vector2 (PhysicsEngine.toPixel v2.X, PhysicsEngine.toPixel v2.Y)

    static member private toPhysicsV2 (v2 : Vector2) =
        Framework.Vector2 (PhysicsEngine.toPhysics v2.X, PhysicsEngine.toPhysics v2.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = PhysicsEngine.toPhysics value
        value - Settings.PolygonRadius * 2.0f

    static member private toPhysicsPolygonRadius value =
        let value = PhysicsEngine.toPhysics value
        value - Settings.PolygonRadius

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    static member private handleCollision
        physicsEngine (fixture : Dynamics.Fixture) (fixture2 : Dynamics.Fixture) (contact : Dynamics.Contacts.Contact) =
        let normal = fst ^ contact.GetWorldManifold ()
        let bodyCollisionMessage =
            { SourceAddress = fixture.Body.UserData :?> obj Address
              CollideeAddress = fixture2.Body.UserData :?> obj Address
              Normal = Vector2 (normal.X, normal.Y)
              Speed = contact.TangentSpeed * Constants.Physics.PhysicsToPixelRatio }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        physicsEngine.IntegrationMessages.Add integrationMessage
        true

    static member private getBodyContacts physicsId physicsEngine =
        let body = physicsEngine.Bodies.[physicsId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while current <> null do
            contacts.Add current.Contact
            current <- current.Next
        List.ofSeq contacts

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.Awake <- bodyProperties.Awake
        body.Position <- PhysicsEngine.toPhysicsV2 bodyProperties.Position
        body.Rotation <- bodyProperties.Rotation
        body.Friction <- bodyProperties.Friction
        body.Restitution <- bodyProperties.Restitution
        body.FixedRotation <- bodyProperties.FixedRotation
        body.AngularVelocity <- bodyProperties.AngularVelocity
        body.AngularDamping <- bodyProperties.AngularDamping
        body.LinearVelocity <- PhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.GravityScale <- bodyProperties.GravityScale
        body.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
        body.CollidesWith <- enum<Category> bodyProperties.CollisionMask
        body.IsBullet <- bodyProperties.IsBullet
        body.IsSensor <- bodyProperties.IsSensor
        body.SleepingAllowed <- true

    static member private createBoxBody sourceAddress bodyProperties bodyBox physicsEngine =
        let body =
            Factories.BodyFactory.CreateRectangle
                (physicsEngine.PhysicsContext,
                 PhysicsEngine.toPhysicsPolygonDiameter ^ bodyBox.Extent.X * 2.0f,
                 PhysicsEngine.toPhysicsPolygonDiameter ^ bodyBox.Extent.Y * 2.0f,
                 bodyProperties.Density,
                 PhysicsEngine.toPhysicsV2 bodyBox.Center,
                 0.0f,
                 PhysicsEngine.toPhysicsBodyType bodyProperties.BodyType,
                 sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- sourceAddress // BUG: ...so I set it again here :/
        body

    static member private createCircleBody sourceAddress bodyProperties (bodyCircle : BodyCircle) physicsEngine =
        let body =
            Factories.BodyFactory.CreateCircle
                (physicsEngine.PhysicsContext,
                 PhysicsEngine.toPhysicsPolygonRadius bodyCircle.Radius,
                 bodyProperties.Density,
                 PhysicsEngine.toPhysicsV2 bodyCircle.Center,
                 PhysicsEngine.toPhysicsBodyType bodyProperties.BodyType,
                 sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- sourceAddress // BUG: ...so I set it again here :/
        body

    static member private createCapsuleBody sourceAddress bodyProperties bodyCapsule physicsEngine =
        let body =
            Factories.BodyFactory.CreateCapsule
                (physicsEngine.PhysicsContext,
                 PhysicsEngine.toPhysicsPolygonDiameter bodyCapsule.Height,
                 PhysicsEngine.toPhysicsPolygonRadius bodyCapsule.Radius,
                 bodyProperties.Density,
                 PhysicsEngine.toPhysicsV2 bodyCapsule.Center,
                 0.0f,
                 PhysicsEngine.toPhysicsBodyType bodyProperties.BodyType,
                 sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- sourceAddress // BUG: ...so I set it again here :/
        // scale in the capsule's box to stop sticking
        let capsuleBox = body.FixtureList.[0].Shape :?> FarseerPhysics.Collision.Shapes.PolygonShape
        capsuleBox.Vertices.Scale (Framework.Vector2 (0.75f, 1.0f)) |> ignore
        body

    static member private createPolygonBody sourceAddress bodyProperties bodyPolygon physicsEngine =
        let body =
            Factories.BodyFactory.CreatePolygon
                (physicsEngine.PhysicsContext,
                 FarseerPhysics.Common.Vertices (List.map PhysicsEngine.toPhysicsV2 bodyPolygon.Vertices),
                 bodyProperties.Density,
                 PhysicsEngine.toPhysicsV2 bodyPolygon.Center,
                 0.0f,
                 PhysicsEngine.toPhysicsBodyType bodyProperties.BodyType,
                 sourceAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        body.UserData <- sourceAddress // BUG: ...so I set it again here :/
        body

    static member private createBody4 sourceId sourceAddress bodyProperties physicsEngine =
    
        // make and configure the body
        let body =
            match bodyProperties.Shape with
            | BodyBox bodyBox -> PhysicsEngine.createBoxBody sourceAddress bodyProperties bodyBox physicsEngine
            | BodyCircle bodyCircle -> PhysicsEngine.createCircleBody sourceAddress bodyProperties bodyCircle physicsEngine
            | BodyCapsule bodyCapsule -> PhysicsEngine.createCapsuleBody sourceAddress bodyProperties bodyCapsule physicsEngine
            | BodyPolygon bodyPolygon -> PhysicsEngine.createPolygonBody sourceAddress bodyProperties bodyPolygon physicsEngine
        PhysicsEngine.configureBodyProperties bodyProperties body
        body.add_OnCollision (fun fn fn2 collision -> PhysicsEngine.handleCollision physicsEngine fn fn2 collision) // NOTE: F# requires us to use an lambda inline here (not sure why)

        // attempt to add the body
        if not ^ physicsEngine.Bodies.TryAdd ({ SourceId = sourceId; BodyId = bodyProperties.BodyId }, body) then
            debug ^ "Could not add body via '" + acstring bodyProperties + "'."

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun bodyProperties -> PhysicsEngine.createBody4 createBodiesMessage.SourceId createBodiesMessage.SourceAddress bodyProperties physicsEngine)
            createBodiesMessage.BodyPropertyList

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =
        PhysicsEngine.createBody4 createBodyMessage.SourceId createBodyMessage.SourceAddress createBodyMessage.BodyProperties physicsEngine

    static member private destroyBody2 physicsId physicsEngine =
        match physicsEngine.Bodies.TryGetValue physicsId with
        | (true, body) ->
            physicsEngine.Bodies.Remove physicsId |> ignore
            physicsEngine.PhysicsContext.RemoveBody body
        | (false, _) ->
            if not physicsEngine.RebuildingHack then
                debug ^ "Could not destroy non-existent body with PhysicsId = " + acstring physicsId + "'."

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        PhysicsEngine.destroyBody2 destroyBodyMessage.PhysicsId physicsEngine

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun physicsId -> PhysicsEngine.destroyBody2 physicsId physicsEngine) destroyBodiesMessage.PhysicsIds

    static member private setBodyPosition (setBodyPositionMessage : SetBodyPositionMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyPositionMessage.PhysicsId with
        | (true, body) -> body.Position <- PhysicsEngine.toPhysicsV2 setBodyPositionMessage.Position
        | (false, _) -> debug ^ "Could not set position of non-existent body with PhysicsId = " + acstring setBodyPositionMessage.PhysicsId + "'."

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.PhysicsId with
        | (true, body) -> body.Rotation <- setBodyRotationMessage.Rotation
        | (false, _) -> debug ^ "Could not set rotation of non-existent body with PhysicsId = " + acstring setBodyRotationMessage.PhysicsId + "'."

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.PhysicsId with
        | (true, body) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity
        | (false, _) -> debug ^ "Could not set angular velocity of non-existent body with PhysicsId = " + acstring setBodyAngularVelocityMessage.PhysicsId + "'."

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.PhysicsId with
        | (true, body) -> body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse)
        | (false, _) -> debug ^ "Could not apply angular impulse to non-existent body with PhysicsId = " + acstring applyBodyAngularImpulseMessage.PhysicsId + "'."

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.PhysicsId with
        | (true, body) -> body.LinearVelocity <- PhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> debug ^ "Could not set linear velocity of non-existent body with PhysicsId = " + acstring setBodyLinearVelocityMessage.PhysicsId + "'."

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.PhysicsId with
        | (true, body) -> body.ApplyLinearImpulse (PhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
        | (false, _) -> debug ^ "Could not apply linear impulse to non-existent body with PhysicsId = " + acstring applyBodyLinearImpulseMessage.PhysicsId + "'."

    static member private applyBodyForce applyBodyForceMessage physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.PhysicsId with
        | (true, body) -> body.ApplyForce (PhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force)
        | (false, _) -> debug ^ "Could not apply force to non-existent body with PhysicsId = " + acstring applyBodyForceMessage.PhysicsId + "'."

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | SetBodyPositionMessage setBodyPositionMessage -> PhysicsEngine.setBodyPosition setBodyPositionMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- PhysicsEngine.toPhysicsV2 gravity
        | RebuildPhysicsHackMessage ->
            physicsEngine.RebuildingHack <- true
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private handlePhysicsMessages physicsMessages physicsEngine =
        for physicsMessage in physicsMessages do
            PhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage
        physicsEngine.RebuildingHack <- false

    static member private createTransformMessages physicsEngine =
        for body in physicsEngine.Bodies.Values do
            if body.Awake && not body.IsStatic then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { SourceAddress = body.UserData :?> obj Address
                          Position = PhysicsEngine.toPixelV2 body.Position
                          Rotation = body.Rotation }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    /// Make a physics engine.
    static member make gravity =
        let physicsEngine =
            { PhysicsContext = FarseerPhysics.Dynamics.World (PhysicsEngine.toPhysicsV2 gravity)
              Bodies = BodyDictionary (HashIdentity.FromFunctions PhysicsId.hash PhysicsId.equals)
              PhysicsMessages = Queue.empty
              IntegrationMessages = List<IntegrationMessage> ()
              RebuildingHack = false }
        physicsEngine :> IPhysicsEngine

    interface IPhysicsEngine with

        member physicsEngine.BodyExists physicsId =
            physicsEngine.Bodies.ContainsKey physicsId

        member physicsEngine.GetBodyContactNormals physicsId =
            let contacts = PhysicsEngine.getBodyContacts physicsId physicsEngine
            List.map
                (fun (contact : Contact) ->
                    let normal = fst ^ contact.GetWorldManifold ()
                    Vector2 (normal.X, normal.Y))
                contacts

        member physicsEngine.GetBodyLinearVelocity physicsId =
            let body = physicsEngine.Bodies.[physicsId]
            PhysicsEngine.toPixelV2 body.LinearVelocity

        member physicsEngine.GetBodyGroundContactNormals physicsId =
            let normals = (physicsEngine :> IPhysicsEngine).GetBodyContactNormals physicsId
            List.filter
                (fun normal ->
                    let theta = Vector2.Dot (normal, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
                    theta < Math.PI * 0.25)
                normals

        member physicsEngine.GetBodyOptGroundContactNormal physicsId =
            let groundNormals = (physicsEngine :> IPhysicsEngine).GetBodyGroundContactNormals physicsId
            match groundNormals with
            | [] -> None
            | _ :: _ ->
                let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyOptGroundContactTangent physicsId =
            match (physicsEngine :> IPhysicsEngine).GetBodyOptGroundContactNormal physicsId with
            | Some normal -> Some ^ Vector2 (normal.Y, -normal.X)
            | None -> None

        member physicsEngine.BodyOnGround physicsId =
            let groundNormals = (physicsEngine :> IPhysicsEngine).GetBodyGroundContactNormals physicsId
            not ^ List.isEmpty groundNormals

        member physicsEngine.ClearMessages () =
            let physicsEngine = { physicsEngine with PhysicsMessages = Queue.empty }
            physicsEngine :> IPhysicsEngine

        member physicsEngine.EnqueueMessage physicsMessage =
            let physicsMessages = Queue.conj physicsMessage physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = physicsMessages }
            physicsEngine :> IPhysicsEngine

        member physicsEngine.Integrate tickRate =
            let physicsMessages = physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = Queue.empty }
            PhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            let physicsStepAmount = Constants.Physics.PhysicsStepRate * single tickRate
            physicsEngine.PhysicsContext.Step physicsStepAmount
            PhysicsEngine.createTransformMessages physicsEngine
            let messages = List.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            (messages, physicsEngine :> IPhysicsEngine)

/// The mock implementation of IPhysicsEngine.
type MockPhysicsEngine =
    { MockPhysicsEngine : unit }
    interface IPhysicsEngine with
        member physicsEngine.BodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyGroundContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyOptGroundContactNormal _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyOptGroundContactTangent _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.BodyOnGround _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.ClearMessages () = physicsEngine :> IPhysicsEngine
        member physicsEngine.EnqueueMessage _ = physicsEngine :> IPhysicsEngine
        member physicsEngine.Integrate _ = ([], physicsEngine :> IPhysicsEngine)

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
    let evalCollisionExpr (extent : Vector2) (expr : string) =
        let defaultShape = BodyBox { Extent = extent * 0.5f; Center = Vector2.Zero }
        match expr.Trim () with
        | "" -> defaultShape
        | _ ->
            let converter = AlgebraicConverter typeof<BodyShape>
            let bodyShape = converter.ConvertFromString expr :?> BodyShape
            match bodyShape with
            | BodyBox bodyBox -> BodyBox { Extent = Vector2.Multiply (extent, bodyBox.Extent); Center = Vector2.Multiply (extent, bodyBox.Center) }
            | BodyCircle bodyCircle -> BodyCircle { Radius = extent.X * bodyCircle.Radius; Center = extent.X * bodyCircle.Center }
            | BodyCapsule bodyCapsule -> BodyCapsule { Height = extent.Y * bodyCapsule.Height; Radius = extent.Y * bodyCapsule.Radius; Center = extent.Y * bodyCapsule.Center }
            | BodyPolygon bodyPolygon ->
                let vertices = List.map (fun vertex -> Vector2.Multiply (vertex, extent)) bodyPolygon.Vertices
                BodyPolygon { Vertices = vertices; Center = Vector2.Multiply (extent, bodyPolygon.Center) }