module Nu.Physics
open System.Collections.Generic
open FarseerPhysics
open OpenTK
open Microsoft.Xna
open Nu.Core
open Nu.Constants

let getPhysicsId = createGetNextId ()

type [<StructuralEquality; NoComparison>] BoxShape =
    { Center : Vector2 // NOTE: I guess this is like a center offset for the shape?
      Extent : Vector2 }

type [<StructuralEquality; NoComparison>] BodyShape =
    | BoxShape of BoxShape

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

type [<StructuralEquality; NoComparison>] ForceApplyMessage =
    { PhysicsId : Id
      Force : Vector2
      Point : Vector2 }

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
    | ForceApplyMessage of ForceApplyMessage

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

let createBody integrator bodyCreateMessage =
    match bodyCreateMessage.Shape with
    | BoxShape boxShape ->
        let physicsShapeCenter = toPhysicsV2 boxShape.Center
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
        body.add_OnCollision
            (fun fixture fixture2 contact ->
                let bodyCollisionMessage =
                    { EntityAddress = fixture.Body.UserData :?> Address
                      EntityAddress2 = fixture2.Body.UserData :?> Address
                      Normal = let localNormal = contact.Manifold.LocalNormal in Vector2 (localNormal.X, localNormal.Y)
                      Speed = contact.TangentSpeed * PhysicsToPixelRatio }
                let integrationMessage = BodyCollisionMessage bodyCollisionMessage
                integrator.IntegrationMessages.Add integrationMessage
                true)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsId, body)

let destroyBody integrator (bodyDestroyMessage : BodyDestroyMessage) =
    let body = ref Unchecked.defaultof<Dynamics.Body>
    if integrator.Bodies.TryGetValue (bodyDestroyMessage.PhysicsId, body) then
        ignore (integrator.Bodies.Remove bodyDestroyMessage.PhysicsId)
        integrator.PhysicsContext.RemoveBody body.Value
    else debug ("Could not remove non-existent body with PhysicsId = " + str bodyDestroyMessage.PhysicsId + "'.")

let forceApply integrator forceApplyMessage =
    let body = ref Unchecked.defaultof<Dynamics.Body>
    if integrator.Bodies.TryGetValue (forceApplyMessage.PhysicsId, body) then
        body.Value.ApplyForce (toPhysicsV2 forceApplyMessage.Force, toPhysicsV2 forceApplyMessage.Point)
    else debug ("Could not apply force to non-existent body with PhysicsId = " + str forceApplyMessage.PhysicsId + "'.")

let handlePhysicsMessage integrator physicsMessage =
    match physicsMessage with
    | BodyCreateMessage bodyCreateMessage -> createBody integrator bodyCreateMessage
    | BodyDestroyMessage bodyDestroyMessage -> destroyBody integrator bodyDestroyMessage
    | ForceApplyMessage forceApplyMessage -> forceApply integrator forceApplyMessage
    
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