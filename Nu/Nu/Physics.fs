// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open FarseerPhysics
open FarseerPhysics.Dynamics
open FarseerPhysics.Dynamics.Contacts
open Microsoft.Xna
open Prime
open Nu

/// Identifies a target whose body can be found in the physics engine.
type [<CustomEquality; NoComparison>] PhysicsId =
    { SourceId : Guid
      CorrelationId : Guid }

    /// The invalid physics id.
    static member InvalidId =
        { SourceId = Constants.Engine.InvalidId; CorrelationId = Constants.Engine.InvalidId }

    /// Hash a PhysicsId.
    static member hash pid =
        pid.SourceId.GetHashCode () ^^^ pid.CorrelationId.GetHashCode ()

    /// Equate PhysicsIds.
    static member equals pid pid2 =
        pid.SourceId.Equals pid2.SourceId &&
        pid.CorrelationId.Equals pid2.CorrelationId

    /// Make a PhysicsId for an external source.
    static member make (sourceId : Guid) =
        { SourceId = sourceId; CorrelationId = Gen.id }

    interface PhysicsId IEquatable with
        member this.Equals that =
            PhysicsId.equals this that

    override this.Equals that =
        match that with
        | :? PhysicsId as that -> PhysicsId.equals this that
        | _ -> false

    override this.GetHashCode () =
        PhysicsId.hash this

/// Store origination information about a simulant physics body.
type [<StructuralEquality; NoComparison>] BodySourceInternal =
    { Simulant : Simulant
      BodyId : Guid }

/// Store origination information about a simulant physics shape body.
type [<StructuralEquality; NoComparison>] BodyShapeSourceInternal =
    { Simulant : Simulant
      BodyId : Guid
      ShapeId : Guid }

/// Describes body shape-specific properties.
type [<StructuralEquality; NoComparison>] BodyShapeProperties =
    { BodyShapeId : Guid
      FrictionOpt : single option
      RestitutionOpt : single option
      CollisionCategoriesOpt : int option
      CollisionMaskOpt : int option
      IsSensorOpt : bool option }

[<RequireQualifiedAccess>]
module BodyShapeProperties =

    let empty =
        { BodyShapeId = Gen.idEmpty
          FrictionOpt = None
          RestitutionOpt = None
          CollisionCategoriesOpt = None
          CollisionMaskOpt = None
          IsSensorOpt = None }

/// The shape of a physics body box.
type [<StructuralEquality; NoComparison>] BodyBox =
    { Extent : Vector2 // TODO: P1: consider if this should instead be size?
      Center : Vector2 // TODO: P1: consider if these should be called Offset instead?
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body circle.
type [<StructuralEquality; NoComparison>] BodyCircle =
    { Radius : single
      Center : Vector2
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type [<StructuralEquality; NoComparison>] BodyCapsule =
    { Height : single
      Radius : single
      Center : Vector2
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body polygon.
type [<StructuralEquality; NoComparison>] BodyPolygon =
    { Vertices : Vector2 array
      Center : Vector2
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body.
[<Syntax
    ("BodyEmpty BodyBox BodyCircle BodyCapsule BodyPolygon BodyShapes", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type [<StructuralEquality; NoComparison>] BodyShape =
    | BodyEmpty
    | BodyBox of BodyBox
    | BodyCircle of BodyCircle
    | BodyCapsule of BodyCapsule
    | BodyPolygon of BodyPolygon
    | BodyShapes of BodyShape list

/// The type of a physics body; Static, Kinematic, or Dynamic.
[<Syntax
    ("Static Kinematic Dynamic", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax);
     StructuralEquality; StructuralComparison>]
type BodyType =
    | Static
    | Kinematic
    | Dynamic

/// The properties needed to describe the physical part of a body.
type [<StructuralEquality; NoComparison>] BodyProperties =
    { BodyId : Guid
      Position : Vector2
      Rotation : single
      BodyShape : BodyShape
      BodyType : BodyType
      Awake : bool
      Enabled : bool
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

[<RequireQualifiedAccess>]
module BodyProperties =

    let empty =
        { BodyId = Gen.idEmpty
          Position = Vector2.Zero
          Rotation = 0.0f
          BodyShape = BodyEmpty
          BodyType = Dynamic
          Awake = true
          Enabled = true
          Density = Constants.Physics.NormalDensity
          Friction = 0.2f
          Restitution = 0.0f
          FixedRotation = false
          AngularVelocity = 0.0f
          AngularDamping = 0.0f
          LinearVelocity = Vector2.Zero
          LinearDamping = 0.0f
          GravityScale = 1.0f
          CollisionCategories = 1
          CollisionMask = -1
          IsBullet = false
          IsSensor = false }

type [<StructuralEquality; NoComparison>] JointAngle =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2
      TargetAngle : single
      Softness : single }

type [<StructuralEquality; NoComparison>] JointDistance =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2
      Length : single
      Frequency : single }

type [<StructuralEquality; NoComparison>] JointFriction =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointGear =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointMotor =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointPrismatic =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointPulley =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointRevolute =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointRope =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

type [<StructuralEquality; NoComparison>] JointWheel =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector2
      Anchor2 : Vector2 }

/// A joint on physics bodies.
[<Syntax
    ("JointAngle JointDistance JointFriction JointGear JointMotor JointPrismatic JointPulley JointRevolute JointRope JointWheel",
     "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax);
     StructuralEquality; NoComparison>]
type JointDevice =
    | JointEmpty
    | JointAngle of JointAngle
    | JointDistance of JointDistance
    | JointFriction of JointFriction
    | JointGear of JointGear
    | JointMotor of JointMotor
    | JointPrismatic of JointPrismatic
    | JointPulley of JointPulley
    | JointRevolute of JointRevolute
    | JointRope of JointRope
    | JointWheel of JointWheel

type [<StructuralEquality; NoComparison>] JointProperties =
    { JointId : Guid
      JointDevice : JointDevice }

[<RequireQualifiedAccess>]
module JointProperties =

    let empty =
        { JointId = Gen.idEmpty
          JointDevice = JointEmpty }

/// A message to the physics system to create a body.
type [<StructuralEquality; NoComparison>] CreateBodyMessage =
    { SourceSimulant : Simulant
      SourceId : Guid
      BodyProperties : BodyProperties }

/// A message to the physics system to create multiple bodies.
type [<StructuralEquality; NoComparison>] CreateBodiesMessage =
    { SourceSimulant : Simulant
      SourceId : Guid
      BodiesProperties : BodyProperties list }

/// A message to the physics system to destroy a body.
type [<StructuralEquality; NoComparison>] DestroyBodyMessage =
    { PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple bodies.
type [<StructuralEquality; NoComparison>] DestroyBodiesMessage =
    { PhysicsIds : PhysicsId list }

/// A message to the physics system to create a joint.
type [<StructuralEquality; NoComparison>] CreateJointMessage =
    { SourceSimulant : Simulant
      SourceId : Guid
      JointProperties : JointProperties }

/// A message to the physics system to create multiple joints.
type [<StructuralEquality; NoComparison>] CreateJointsMessage =
    { SourceSimulant : Simulant
      SourceId : Guid
      JointsProperties : JointProperties list }

/// A message to the physics system to destroy a joint.
type [<StructuralEquality; NoComparison>] DestroyJointMessage =
    { PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple joints.
type [<StructuralEquality; NoComparison>] DestroyJointsMessage =
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
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal
      Normal : Vector2
      Speed : single }

/// A message from the physics system describing a body separation that took place.
type [<StructuralEquality; NoComparison>] BodySeparationMessage =
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal }

/// A message from the physics system describing the updated transform of a body.
type [<StructuralEquality; NoComparison>] BodyTransformMessage =
    { BodySource : BodySourceInternal
      Position : Vector2
      Rotation : single }

/// Tracks physics bodies by their PhysicsIds.
type BodyDictionary = Dictionary<PhysicsId, Dynamics.Body>

/// Tracks physics joints by their PhysicsIds.
type JointDictionary = Dictionary<PhysicsId, Dynamics.Joints.Joint>

/// A message to the physics system.
type [<StructuralEquality; NoComparison>] PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateJointMessage of CreateJointMessage
    | CreateJointsMessage of CreateJointsMessage
    | DestroyJointMessage of DestroyJointMessage
    | DestroyJointsMessage of DestroyJointsMessage
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
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage

/// Represents a physics engine in Nu.
type PhysicsEngine =
    /// Check that the physics engine contain the body with the given physics id.
    abstract BodyExists : PhysicsId -> bool
    /// Get the contact normals of the body with the given physics id.
    abstract GetBodyContactNormals : PhysicsId -> Vector2 list
    /// Get the linear velocity of the body with the given physics id.
    abstract GetBodyLinearVelocity : PhysicsId -> Vector2
    /// Get the contact normals where the body with the given physics id is touching the ground.
    abstract GetBodyToGroundContactNormals : PhysicsId -> Vector2 list
    /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactNormalOpt : PhysicsId -> Vector2 option
    /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactTangentOpt : PhysicsId -> Vector2 option
    /// Check that the body with the given physics id is on the ground.
    abstract IsBodyOnGround : PhysicsId -> bool
    /// Pop all of the physics messages that have been enqueued.
    abstract PopMessages : unit -> PhysicsMessage UList * PhysicsEngine
    /// Clear all of the physics messages that have been enqueued.
    abstract ClearMessages : unit -> PhysicsEngine
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : PhysicsMessage -> PhysicsEngine
    /// Integrate the physics system one frame.
    abstract Integrate : int64 -> PhysicsMessage UList -> IntegrationMessage List

/// The mock implementation of PhysicsEngine.
type [<ReferenceEquality; NoComparison>] MockPhysicsEngine =
    private { MockPhysicsEngine : unit }
    static member make () = { MockPhysicsEngine = () }
    interface PhysicsEngine with
        member physicsEngine.BodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormalOpt _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactTangentOpt _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.IsBodyOnGround _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.PopMessages () = (UList.makeEmpty Functional, physicsEngine :> PhysicsEngine)
        member physicsEngine.ClearMessages () = physicsEngine :> PhysicsEngine
        member physicsEngine.EnqueueMessage _ = physicsEngine :> PhysicsEngine
        member physicsEngine.Integrate _ _ = List<IntegrationMessage> ()

/// The Farseer implementation of PhysicsEngine.
type [<ReferenceEquality; NoComparison>] FarseerPhysicsEngine =
    private
        { PhysicsContext : Dynamics.World
          Bodies : BodyDictionary
          Joints : JointDictionary
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          mutable RebuildingHack : bool }

    static member private toPixel value =
        value * Constants.Physics.PhysicsToPixelRatio

    static member private toPhysics value =
        value * Constants.Physics.PixelToPhysicsRatio

    static member private toPixelV2 (v2 : Framework.Vector2) =
        Vector2 (FarseerPhysicsEngine.toPixel v2.X, FarseerPhysicsEngine.toPixel v2.Y)

    static member private toPhysicsV2 (v2 : Vector2) =
        Framework.Vector2 (FarseerPhysicsEngine.toPhysics v2.X, FarseerPhysicsEngine.toPhysics v2.Y)

    static member private toPhysicsPolygonDiameter value =
        let value = FarseerPhysicsEngine.toPhysics value
        value - Settings.PolygonRadius * 2.0f

    static member private toPhysicsPolygonRadius value =
        let value = FarseerPhysicsEngine.toPhysics value
        value - Settings.PolygonRadius

    static member private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    static member private handleCollision
        physicsEngine (bodyShape : Dynamics.Fixture) (bodyShape2 : Dynamics.Fixture) (contact : Dynamics.Contacts.Contact) =
        let normal = fst (contact.GetWorldManifold ())
        let bodyCollisionMessage =
            { BodyShapeSource = bodyShape.UserData :?> BodyShapeSourceInternal
              BodyShapeSource2 = bodyShape2.UserData :?> BodyShapeSourceInternal
              Normal = Vector2 (normal.X, normal.Y)
              Speed = contact.TangentSpeed * Constants.Physics.PhysicsToPixelRatio }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        physicsEngine.IntegrationMessages.Add integrationMessage
        true

    static member private handleSeparation
        physicsEngine (bodyShape : Dynamics.Fixture) (bodyShape2 : Dynamics.Fixture) =
        let bodySeparationMessage =
            { BodyShapeSource = bodyShape.UserData :?> BodyShapeSourceInternal
              BodyShapeSource2 = bodyShape2.UserData :?> BodyShapeSourceInternal }
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private getBodyContacts physicsId physicsEngine =
        let body = physicsEngine.Bodies.[physicsId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while current <> null do
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
            bodyShape.IsSensor <- match bodyShapeProperties.IsSensorOpt with Some isSensor -> isSensor | None -> bodyProperties.IsSensor
            bodyShape
        | None ->
            bodyShape.Friction <- bodyProperties.Friction
            bodyShape.Restitution <- bodyProperties.Restitution
            bodyShape.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
            bodyShape.CollidesWith <- enum<Category> bodyProperties.CollisionMask
            bodyShape.IsSensor <- bodyProperties.IsSensor
            bodyShape

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : Body) =
        body.Awake <- bodyProperties.Awake
        body.Enabled <- bodyProperties.Enabled
        body.Rotation <- bodyProperties.Rotation
        body.Friction <- bodyProperties.Friction
        body.Restitution <- bodyProperties.Restitution
        body.FixedRotation <- bodyProperties.FixedRotation
        body.AngularVelocity <- bodyProperties.AngularVelocity
        body.AngularDamping <- bodyProperties.AngularDamping
        body.LinearVelocity <- FarseerPhysicsEngine.toPhysicsV2 bodyProperties.LinearVelocity
        body.LinearDamping <- bodyProperties.LinearDamping
        body.GravityScale <- bodyProperties.GravityScale
        body.CollisionCategories <- enum<Category> bodyProperties.CollisionCategories
        body.CollidesWith <- enum<Category> bodyProperties.CollisionMask
        body.IsBullet <- bodyProperties.IsBullet
        body.IsSensor <- bodyProperties.IsSensor
        body.SleepingAllowed <- true

    static member private attachBoxBody sourceSimulant (bodyProperties : BodyProperties) (bodyBox : BodyBox) body =
        let bodyShapeSource =
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyBox.PropertiesOpt with Some p -> p.BodyShapeId | None -> Gen.idEmpty }
        let bodyShape =
            Factories.FixtureFactory.AttachRectangle
                (FarseerPhysicsEngine.toPhysicsPolygonDiameter (bodyBox.Extent.X * 2.0f),
                 FarseerPhysicsEngine.toPhysicsPolygonDiameter (bodyBox.Extent.Y * 2.0f),
                 bodyProperties.Density,
                 FarseerPhysicsEngine.toPhysicsV2 bodyBox.Center,
                 body,
                 bodyShapeSource)
        FarseerPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBox.PropertiesOpt bodyShape

    static member private attachBodyCircle sourceSimulant (bodyProperties : BodyProperties) (bodyCircle : BodyCircle) body =
        let bodyShapeSource =
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyCircle.PropertiesOpt with Some p -> p.BodyShapeId | None -> Gen.idEmpty }
        let bodyShape =
            Factories.FixtureFactory.AttachCircle
                (FarseerPhysicsEngine.toPhysicsPolygonRadius bodyCircle.Radius,
                 bodyProperties.Density,
                 body,
                 FarseerPhysicsEngine.toPhysicsV2 bodyCircle.Center,
                 bodyShapeSource)
        FarseerPhysicsEngine.configureBodyShapeProperties bodyProperties bodyCircle.PropertiesOpt bodyShape

    static member private attachBodyCapsule sourceSimulant (bodyProperties : BodyProperties) (bodyCapsule : BodyCapsule) body =
        let bodyShapeSource =
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyCapsule.PropertiesOpt with Some p -> p.BodyShapeId | None -> Gen.idEmpty }
        let height = FarseerPhysicsEngine.toPhysicsPolygonDiameter bodyCapsule.Height
        let endRadius = FarseerPhysicsEngine.toPhysicsPolygonRadius bodyCapsule.Radius
        let density = bodyProperties.Density
        let center = FarseerPhysicsEngine.toPhysicsV2 bodyCapsule.Center
        let rectangle = Common.PolygonTools.CreateRectangle (endRadius * 0.75f, height * 0.5f, center, 0.0f) // scaled in the capsule's box by 0.75f to stop corner sticking
        let list = List<Common.Vertices> ()
        list.Add rectangle
        let bodyShapes = Factories.FixtureFactory.AttachCompoundPolygon (list, density, body, bodyShapeSource)
        let bodyShapeTop = Factories.FixtureFactory.AttachCircle (endRadius, density, body, Framework.Vector2 (0.0f, height * 0.5f), bodyShapeSource)
        let bodyShapeBottom = Factories.FixtureFactory.AttachCircle (endRadius, density, body, Framework.Vector2 (0.0f, 0.0f - height * 0.5f), bodyShapeSource)
        bodyShapes.Add bodyShapeTop
        bodyShapes.Add bodyShapeBottom
        for bodyShape in bodyShapes do
            FarseerPhysicsEngine.configureBodyShapeProperties bodyProperties bodyCapsule.PropertiesOpt bodyShape |> ignore
        Array.ofSeq bodyShapes

    static member private attachBodyPolygon sourceSimulant bodyProperties bodyPolygon body =
        let bodyShapeSource =
            { Simulant = sourceSimulant
              BodyId = bodyProperties.BodyId
              ShapeId = match bodyPolygon.PropertiesOpt with Some p -> p.BodyShapeId | None -> Gen.idEmpty }
        let vertices =
            bodyPolygon.Vertices |>
            Array.map (fun vertex -> vertex + bodyPolygon.Center) |>
            Array.map FarseerPhysicsEngine.toPhysicsV2
        let bodyShape =
            Factories.FixtureFactory.AttachPolygon
                (FarseerPhysics.Common.Vertices vertices,
                 bodyProperties.Density,
                 body,
                 bodyShapeSource)
        FarseerPhysicsEngine.configureBodyShapeProperties bodyProperties bodyPolygon.PropertiesOpt bodyShape

    static member private attachBodyShapes sourceSimulant bodyProperties bodyShapes (body : Body) =
        let list = List () // NOTE: was too lazy to write a fold, so used mutation and left this comment...
        for bodyShape in bodyShapes do
            let bodyShapes = FarseerPhysicsEngine.attachBodyShape sourceSimulant bodyProperties bodyShape body
            list.AddRange bodyShapes
        Array.ofSeq list

    static member private attachBodyShape sourceSimulant bodyProperties bodyShape (body : Body) =
        match bodyShape with
        | BodyEmpty -> [||]
        | BodyBox bodyBox -> FarseerPhysicsEngine.attachBoxBody sourceSimulant bodyProperties bodyBox body |> Array.singleton
        | BodyCircle bodyCircle -> FarseerPhysicsEngine.attachBodyCircle sourceSimulant bodyProperties bodyCircle body |> Array.singleton
        | BodyCapsule bodyCapsule -> FarseerPhysicsEngine.attachBodyCapsule sourceSimulant bodyProperties bodyCapsule body |> Array.ofSeq
        | BodyPolygon bodyPolygon -> FarseerPhysicsEngine.attachBodyPolygon sourceSimulant bodyProperties bodyPolygon body |> Array.singleton
        | BodyShapes bodyShapes -> FarseerPhysicsEngine.attachBodyShapes sourceSimulant bodyProperties bodyShapes body
        
    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // get fields
        let sourceSimulant = createBodyMessage.SourceSimulant
        let bodyProperties = createBodyMessage.BodyProperties
        let bodySource = { Simulant = sourceSimulant; BodyId = bodyProperties.BodyId }

        // make the body
        let body =
            Factories.BodyFactory.CreateBody
                (physicsEngine.PhysicsContext,
                 FarseerPhysicsEngine.toPhysicsV2 bodyProperties.Position,
                 bodyProperties.Rotation,
                 FarseerPhysicsEngine.toPhysicsBodyType bodyProperties.BodyType,
                 bodySource)

        // configure body
        FarseerPhysicsEngine.configureBodyProperties bodyProperties body

        // attach body shape
        FarseerPhysicsEngine.attachBodyShape sourceSimulant bodyProperties bodyProperties.BodyShape body |> ignore

        // listen for collisions
        body.add_OnCollision (fun fn fn2 collision -> FarseerPhysicsEngine.handleCollision physicsEngine fn fn2 collision)

        // listen for separations
        body.add_OnSeparation (fun fn fn2 -> FarseerPhysicsEngine.handleSeparation physicsEngine fn fn2)

        // attempt to add the body
        if not (physicsEngine.Bodies.TryAdd ({ SourceId = createBodyMessage.SourceId; CorrelationId = bodyProperties.BodyId }, body)) then
            Log.debug ("Could not add body via '" + scstring bodyProperties + "'.")

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun bodyProperties ->
                let createBodyMessage =
                    { SourceSimulant = createBodiesMessage.SourceSimulant
                      SourceId = createBodiesMessage.SourceId
                      BodyProperties = bodyProperties }
                FarseerPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        let physicsId = destroyBodyMessage.PhysicsId
        match physicsEngine.Bodies.TryGetValue physicsId with
        | (true, body) ->
            physicsEngine.Bodies.Remove physicsId |> ignore
            physicsEngine.PhysicsContext.RemoveBody body
        | (false, _) ->
            if not physicsEngine.RebuildingHack then
                Log.debug ("Could not destroy non-existent body with PhysicsId = " + scstring physicsId + "'.")

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter
            (fun physicsId -> FarseerPhysicsEngine.destroyBody { PhysicsId = physicsId } physicsEngine)
            destroyBodiesMessage.PhysicsIds

    static member private createJoint (createJointMessage : CreateJointMessage) physicsEngine =
        match createJointMessage.JointProperties.JointDevice with
        | JointEmpty ->
            ()
        | JointAngle jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, body), (true, body2)) ->
                let joint = Factories.JointFactory.CreateAngleJoint (physicsEngine.PhysicsContext, body, body2)
                joint.TargetAngle <- jointAngle.TargetAngle
                joint.Softness <- jointAngle.Softness
            | (_, _) -> Log.debug "Could not set create a joint for one or more non-existent bodies."
        | JointDistance jointDistance ->
            match (physicsEngine.Bodies.TryGetValue jointDistance.TargetId, physicsEngine.Bodies.TryGetValue jointDistance.TargetId2) with
            | ((true, body), (true, body2)) ->
                let joint = Factories.JointFactory.CreateDistanceJoint (physicsEngine.PhysicsContext, body, body2)
                joint.LocalAnchorA <- FarseerPhysicsEngine.toPhysicsV2 jointDistance.Anchor
                joint.LocalAnchorB <- FarseerPhysicsEngine.toPhysicsV2 jointDistance.Anchor2
                joint.Length <- FarseerPhysicsEngine.toPhysics jointDistance.Length
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
                FarseerPhysicsEngine.createJoint createJointMessage physicsEngine)
            createJointsMessage.JointsProperties

    static member private destroyJoint (destroyJointMessage : DestroyJointMessage) physicsEngine =
        match physicsEngine.Joints.TryGetValue destroyJointMessage.PhysicsId with
        | (true, joint) ->
            physicsEngine.Joints.Remove destroyJointMessage.PhysicsId |> ignore
            physicsEngine.PhysicsContext.RemoveJoint joint
        | (false, _) ->
            if not physicsEngine.RebuildingHack then
                Log.debug ("Could not destroy non-existent joint with PhysicsId = " + scstring destroyJointMessage.PhysicsId + "'.")

    static member private destroyJoints (destroyJointsMessage : DestroyJointsMessage) physicsEngine =
        List.iter
            (fun physicsId ->
                let destroyJointMessage = { PhysicsId = physicsId }
                FarseerPhysicsEngine.destroyJoint destroyJointMessage physicsEngine)
            destroyJointsMessage.PhysicsIds

    static member private setBodyPosition (setBodyPositionMessage : SetBodyPositionMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyPositionMessage.PhysicsId with
        | (true, body) -> body.Position <- FarseerPhysicsEngine.toPhysicsV2 setBodyPositionMessage.Position
        | (false, _) -> Log.debug ("Could not set position of non-existent body with PhysicsId = " + scstring setBodyPositionMessage.PhysicsId + "'.")

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.PhysicsId with
        | (true, body) -> body.Rotation <- setBodyRotationMessage.Rotation
        | (false, _) -> Log.debug ("Could not set rotation of non-existent body with PhysicsId = " + scstring setBodyRotationMessage.PhysicsId + "'.")

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.PhysicsId with
        | (true, body) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity
        | (false, _) -> Log.debug ("Could not set angular velocity of non-existent body with PhysicsId = " + scstring setBodyAngularVelocityMessage.PhysicsId + "'.")

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.PhysicsId with
        | (true, body) -> body.ApplyAngularImpulse (applyBodyAngularImpulseMessage.AngularImpulse)
        | (false, _) -> Log.debug ("Could not apply angular impulse to non-existent body with PhysicsId = " + scstring applyBodyAngularImpulseMessage.PhysicsId + "'.")

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.PhysicsId with
        | (true, body) -> body.LinearVelocity <- FarseerPhysicsEngine.toPhysicsV2 setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) -> Log.debug ("Could not set linear velocity of non-existent body with PhysicsId = " + scstring setBodyLinearVelocityMessage.PhysicsId + "'.")

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.PhysicsId with
        | (true, body) -> body.ApplyLinearImpulse (FarseerPhysicsEngine.toPhysicsV2 applyBodyLinearImpulseMessage.LinearImpulse)
        | (false, _) -> Log.debug ("Could not apply linear impulse to non-existent body with PhysicsId = " + scstring applyBodyLinearImpulseMessage.PhysicsId + "'.")

    static member private applyBodyForce applyBodyForceMessage physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.PhysicsId with
        | (true, body) -> body.ApplyForce (FarseerPhysicsEngine.toPhysicsV2 applyBodyForceMessage.Force)
        | (false, _) -> Log.debug ("Could not apply force to non-existent body with PhysicsId = " + scstring applyBodyForceMessage.PhysicsId + "'.")

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> FarseerPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> FarseerPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> FarseerPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> FarseerPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateJointMessage createJointMessage -> FarseerPhysicsEngine.createJoint createJointMessage physicsEngine
        | CreateJointsMessage createJointsMessage -> FarseerPhysicsEngine.createJoints createJointsMessage physicsEngine
        | DestroyJointMessage destroyJointMessage -> FarseerPhysicsEngine.destroyJoint destroyJointMessage physicsEngine
        | DestroyJointsMessage destroyJointsMessage -> FarseerPhysicsEngine.destroyJoints destroyJointsMessage physicsEngine
        | SetBodyPositionMessage setBodyPositionMessage -> FarseerPhysicsEngine.setBodyPosition setBodyPositionMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> FarseerPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> FarseerPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> FarseerPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> FarseerPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> FarseerPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> FarseerPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- FarseerPhysicsEngine.toPhysicsV2 gravity
        | RebuildPhysicsHackMessage ->
            physicsEngine.RebuildingHack <- true
            physicsEngine.PhysicsContext.Clear ()
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private handlePhysicsMessages physicsMessages physicsEngine =
        for physicsMessage in physicsMessages do
            FarseerPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage
        physicsEngine.RebuildingHack <- false

    static member private createTransformMessages physicsEngine =
        // NOTE: We should really be querying these bodies from the physics engine's internally-maintained awake-body
        // list. Note also that I tried building Farseer with #define USE_AWAKE_BODY_SET so we can query from that
        // AwakeBodyList, but there are compilation errors that, when I tried to fix, broke the whole system :)
        //
        // In truth, we just need a better physics engine implementation :)
        for body in physicsEngine.PhysicsContext.BodyList do
            if body.Awake && not body.IsStatic then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodySource = body.UserData :?> BodySourceInternal
                          Position = FarseerPhysicsEngine.toPixelV2 body.Position
                          Rotation = body.Rotation }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    /// Make a physics engine.
    static member make gravity =
        let physicsEngine =
            { PhysicsContext = FarseerPhysics.Dynamics.World (FarseerPhysicsEngine.toPhysicsV2 gravity)
              Bodies = BodyDictionary (HashIdentity.FromFunctions PhysicsId.hash PhysicsId.equals)
              Joints = JointDictionary (HashIdentity.FromFunctions PhysicsId.hash PhysicsId.equals)
              PhysicsMessages = UList.makeEmpty Constants.Physics.MessageListConfig
              IntegrationMessages = List<IntegrationMessage> ()
              RebuildingHack = false }
        physicsEngine :> PhysicsEngine

    interface PhysicsEngine with

        member physicsEngine.BodyExists physicsId =
            physicsEngine.Bodies.ContainsKey physicsId

        member physicsEngine.GetBodyContactNormals physicsId =
            FarseerPhysicsEngine.getBodyContacts physicsId physicsEngine |>
            Array.map (fun (contact : Contact) -> let normal = fst (contact.GetWorldManifold ()) in Vector2 (normal.X, normal.Y)) |>
            Array.toList

        member physicsEngine.GetBodyLinearVelocity physicsId =
            let body = physicsEngine.Bodies.[physicsId]
            FarseerPhysicsEngine.toPixelV2 body.LinearVelocity

        member physicsEngine.GetBodyToGroundContactNormals physicsId =
            List.filter
                (fun normal ->
                    let theta = Vector2.Dot (normal, Vector2.UnitY) |> double |> Math.Acos |> Math.Abs
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
            | Some normal -> Some (Vector2 (normal.Y, -normal.X))
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
            let physicsMessages = UList.add physicsMessage physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = physicsMessages }
            physicsEngine :> PhysicsEngine

        member physicsEngine.Integrate tickRate physicsMessages =
            FarseerPhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            let physicsStepAmount = Constants.Physics.PhysicsStepRate * single tickRate
            physicsEngine.PhysicsContext.Step physicsStepAmount
            FarseerPhysicsEngine.createTransformMessages physicsEngine
            let integrationMessages = List<IntegrationMessage> physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages

[<RequireQualifiedAccess>]
module PhysicsEngine =

    /// Convert a category mask to a value that represents collision categories.
    /// Examples -
    ///     @ = -1
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either @ or a binary number!
    let categorizeCollisionMask categoryMask =
        match categoryMask with
        | "@" -> -1
        | _ -> Convert.ToInt32 (categoryMask, 2)

    /// Localize a body shape to a specific physics object.
    let rec localizeBodyShape (extent : Vector2) (bodyShape : BodyShape) =
        match bodyShape with
        | BodyEmpty -> BodyEmpty
        | BodyBox bodyBox -> BodyBox { Extent = Vector2.Multiply (extent, bodyBox.Extent); Center = Vector2.Multiply (extent, bodyBox.Center); PropertiesOpt = bodyBox.PropertiesOpt }
        | BodyCircle bodyCircle -> BodyCircle { Radius = extent.X * bodyCircle.Radius; Center = extent.X * bodyCircle.Center; PropertiesOpt = bodyCircle.PropertiesOpt }
        | BodyCapsule bodyCapsule -> BodyCapsule { Height = extent.Y * bodyCapsule.Height; Radius = extent.Y * bodyCapsule.Radius; Center = extent.Y * bodyCapsule.Center; PropertiesOpt = bodyCapsule.PropertiesOpt }
        | BodyPolygon bodyPolygon ->
            let vertices = Array.map (fun vertex -> Vector2.Multiply (vertex, extent)) bodyPolygon.Vertices
            BodyPolygon { Vertices = vertices; Center = Vector2.Multiply (extent, bodyPolygon.Center); PropertiesOpt = bodyPolygon.PropertiesOpt }
        | BodyShapes bodyShapes ->
            let bodyShapes = List.map (localizeBodyShape extent) bodyShapes
            BodyShapes bodyShapes

    /// Check that the physics engine contain the body with the given physics id.
    let bodyExists physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.BodyExists physicsId

    /// Get the contact normals of the body with the given physics id.
    let getBodyContactNormals physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.GetBodyContactNormals physicsId

    /// Get the linear velocity of the body with the given physics id.
    let getBodyLinearVelocity physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.GetBodyLinearVelocity physicsId

    /// Get the contact normals where the body with the given physics id is touching the ground.
    let getBodyToGroundContactNormals physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.GetBodyToGroundContactNormals physicsId

    /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
    let getBodyToGroundContactNormalOpt physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.GetBodyToGroundContactNormalOpt physicsId

    /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
    let getBodyToGroundContactTangentOpt physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.GetBodyToGroundContactTangentOpt physicsId

    /// Check that the body with the given physics id is on the ground.
    let isBodyOnGround physicsId (physicsEngine : PhysicsEngine) =
        physicsEngine.IsBodyOnGround physicsId

    /// Clear all of the physics messages that have been enqueued.
    let clearMessages (physicsEngine : PhysicsEngine) =
        physicsEngine.ClearMessages ()

    /// Enqueue a message from an external source.
    let enqueueMessage physicsMessage (physicsEngine : PhysicsEngine) =
        physicsEngine.EnqueueMessage physicsMessage

    /// Integrate the physics system one frame.
    let integrate tickRate physicsMessages (physicsEngine : PhysicsEngine) =
        physicsEngine.Integrate tickRate physicsMessages
