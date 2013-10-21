module Nu.Physics
open System.Collections.Generic
open FarseerPhysics
open OpenTK
open Microsoft.Xna
open Nu.Core
open Nu.Constants

let getPhysicsId = createGetNextId ()

type [<StructuralEquality; NoComparison>] CommonShapeProperties =
    { Center : Vector2 // NOTE: I guess this is like a center offset for the shape?
      Restitution : single
      FixedRotation : bool
      LinearDamping : single
      AngularDamping : single }

type [<StructuralEquality; NoComparison>] BoxShape =
    { Extent : Vector2
      Properties : CommonShapeProperties }

type [<StructuralEquality; NoComparison>] CircleShape =
    { Radius : single
      Properties : CommonShapeProperties }

type [<StructuralEquality; NoComparison>] BodyShape =
    | BoxShape of BoxShape
    | CircleShape of CircleShape

type [<StructuralEquality; NoComparison>] BodyType =
    | Static
    | Kinematic
    | Dynamic

type [<StructuralEquality; NoComparison>] BodyCreateMessage =
    { EntityAddress : Address
      PhysicsId : Id
      Shape : BodyShape
      Position : Vector2
      Rotation : single
      Density : single
      BodyType : BodyType }

type [<StructuralEquality; NoComparison>] BodyDestroyMessage =
    { PhysicsId : Id }

type [<StructuralEquality; NoComparison>] ApplyImpulseMessage =
    { PhysicsId : Id
      Impulse : Vector2 }

type [<StructuralEquality; NoComparison>] BodyCollisionMessage =
    { EntityAddress : Address
      EntityAddress2 : Address
      Normal : Vector2
      Speed : single }

type [<StructuralEquality; NoComparison>] BodyTransformMessage =
    { EntityAddress : Address
      Position : Vector2
      Rotation : single }

type BodyDictionary = Dictionary<Id, Dynamics.Body>

type [<StructuralEquality; NoComparison>] PhysicsMessage =
    | BodyCreateMessage of BodyCreateMessage
    | BodyDestroyMessage of BodyDestroyMessage
    | ApplyImpulseMessage of ApplyImpulseMessage
    | SetGravityMessage of Vector2

type  [<StructuralEquality; NoComparison>] IntegrationMessage =
    | BodyCollisionMessage of BodyCollisionMessage
    | BodyTransformMessage of BodyTransformMessage

type [<ReferenceEquality>] Integrator =
    private
        { PhysicsContext : Dynamics.World
          Bodies : BodyDictionary
          IntegrationMessages : IntegrationMessage List }

let toPixel value =
    value * Constants.PhysicsToPixelRatio

let toPhysics value =
    value * Constants.PixelToPhysicsRatio

let toPixelV2 (v2 : Framework.Vector2) =
    Vector2 (toPixel v2.X, toPixel v2.Y)

let toPhysicsV2 (v2 : Vector2) =
    Framework.Vector2 (toPhysics v2.X, toPhysics v2.Y)

let toPhysicsBodyType bodyType =
    match bodyType with
    | Static -> Dynamics.BodyType.Static
    | Kinematic -> Dynamics.BodyType.Kinematic
    | Dynamic -> Dynamics.BodyType.Dynamic

let handlePhysicsCollision
    integrator
    (fixture : Dynamics.Fixture)
    (fixture2 : Dynamics.Fixture)
    (contact : Dynamics.Contacts.Contact) =
    let bodyCollisionMessage =
        { EntityAddress = fixture.Body.UserData :?> Address
          EntityAddress2 = fixture2.Body.UserData :?> Address
          Normal = let localNormal = contact.Manifold.LocalNormal in Vector2 (localNormal.X, localNormal.Y)
          Speed = contact.TangentSpeed * PhysicsToPixelRatio }
    let integrationMessage = BodyCollisionMessage bodyCollisionMessage
    integrator.IntegrationMessages.Add integrationMessage
    true

// TODO: remove code duplication here
let createBody integrator bodyCreateMessage =
    match bodyCreateMessage.Shape with
    | BoxShape boxShape ->
        let physicsShapeCenter = toPhysicsV2 boxShape.Properties.Center
        let physicsShapeSize = toPhysicsV2 (boxShape.Extent * 2.0f)
        let body =
            Factories.BodyFactory.CreateRectangle (
                integrator.PhysicsContext,
                physicsShapeSize.X,
                physicsShapeSize.Y,
                bodyCreateMessage.Density,
                physicsShapeCenter,
                bodyCreateMessage.EntityAddress)
        body.Position <- toPhysicsV2 bodyCreateMessage.Position
        body.Rotation <- bodyCreateMessage.Rotation
        body.BodyType <- toPhysicsBodyType bodyCreateMessage.BodyType
        body.Restitution <- boxShape.Properties.Restitution
        body.FixedRotation <- boxShape.Properties.FixedRotation
        body.LinearDamping <- boxShape.Properties.LinearDamping
        body.AngularDamping <- boxShape.Properties.AngularDamping
        body.SleepingAllowed <- true
        body.add_OnCollision (fun f f2 c -> handlePhysicsCollision integrator f f2 c) // NOTE: F# requires us to use an lambda inline here (no idea why)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsId, body)
    | CircleShape circleShape ->
        let physicsShapeCenter = toPhysicsV2 circleShape.Properties.Center
        let physicsShapeRadius = toPhysics circleShape.Radius
        let body =
            Factories.BodyFactory.CreateCircle (
                integrator.PhysicsContext,
                physicsShapeRadius,
                bodyCreateMessage.Density,
                physicsShapeCenter,
                bodyCreateMessage.EntityAddress) // BUG: Farseer doesn't set the UserData with the parameter I give it here...
        body.UserData <- bodyCreateMessage.EntityAddress // BUG: ...so I set it again here :/
        body.Position <- toPhysicsV2 bodyCreateMessage.Position
        body.Rotation <- bodyCreateMessage.Rotation
        body.BodyType <- toPhysicsBodyType bodyCreateMessage.BodyType
        body.Restitution <- circleShape.Properties.Restitution
        body.FixedRotation <- circleShape.Properties.FixedRotation
        body.LinearDamping <- circleShape.Properties.LinearDamping
        body.AngularDamping <- circleShape.Properties.AngularDamping
        body.SleepingAllowed <- true
        body.add_OnCollision (fun f f2 c -> handlePhysicsCollision integrator f f2 c) // NOTE: F# requires us to use an lambda inline here (no idea why)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsId, body)

let destroyBody integrator (bodyDestroyMessage : BodyDestroyMessage) =
    let body = ref Unchecked.defaultof<Dynamics.Body>
    if  integrator.Bodies.TryGetValue (bodyDestroyMessage.PhysicsId, body) then
        ignore (integrator.Bodies.Remove bodyDestroyMessage.PhysicsId)
        integrator.PhysicsContext.RemoveBody body.Value
    else debug ("Could not remove non-existent body with PhysicsId = " + str bodyDestroyMessage.PhysicsId + "'.")

let applyImpulse integrator applyImpulseMessage =
    let body = ref Unchecked.defaultof<Dynamics.Body>
    if  integrator.Bodies.TryGetValue (applyImpulseMessage.PhysicsId, body) then
        body.Value.ApplyLinearImpulse (toPhysicsV2 applyImpulseMessage.Impulse)
    else debug ("Could not apply impulse to non-existent body with PhysicsId = " + str applyImpulseMessage.PhysicsId + "'.")

let handlePhysicsMessage integrator physicsMessage =
    match physicsMessage with
    | BodyCreateMessage bodyCreateMessage -> createBody integrator bodyCreateMessage
    | BodyDestroyMessage bodyDestroyMessage -> destroyBody integrator bodyDestroyMessage
    | ApplyImpulseMessage applyImpulseMessage -> applyImpulse integrator applyImpulseMessage
    | SetGravityMessage gravity -> integrator.PhysicsContext.Gravity <- Framework.Vector2 (gravity.X, gravity.Y)
    
let handlePhysicsMessages integrator (physicsMessages : PhysicsMessage rQueue) =
    for physicsMessage in List.rev physicsMessages do
        handlePhysicsMessage integrator physicsMessage

let createTransformMessages integrator =
    for body in integrator.Bodies.Values do
        if body.Awake then
            let bodyTransformMessage =
                BodyTransformMessage
                    { EntityAddress = body.UserData :?> Address
                      Position = toPixelV2 body.Position
                      Rotation = body.Rotation }
            integrator.IntegrationMessages.Add bodyTransformMessage

let integrate (physicsMessages : PhysicsMessage rQueue) integrator : IntegrationMessage list =
    handlePhysicsMessages integrator physicsMessages
    integrator.PhysicsContext.Step PhysicsStepRate
    createTransformMessages integrator
    let messages = List.ofSeq integrator.IntegrationMessages
    integrator.IntegrationMessages.Clear ()
    messages

let makeIntegrator gravity =
     { PhysicsContext = FarseerPhysics.Dynamics.World Gravity
       Bodies = BodyDictionary ()
       IntegrationMessages = List<IntegrationMessage> () }