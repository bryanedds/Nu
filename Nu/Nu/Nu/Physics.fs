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

let toVector2 (v2 : Framework.Vector2) =
    Vector2 (v2.X, v2.Y)

let toPhysicsVector2 (v2 : Vector2) =
    Framework.Vector2 (v2.X, v2.Y)

let toPhysicsBodyType bodyType =
    match bodyType with
    | Static -> Dynamics.BodyType.Static
    | Kinematic -> Dynamics.BodyType.Kinematic
    | Dynamic -> Dynamics.BodyType.Dynamic

let createBody integrator bodyCreateMessage =
    match bodyCreateMessage.Shape with
    | BoxShape boxShape ->
        let shapeCenter = toPhysicsVector2 boxShape.Center
        let body = Factories.BodyFactory.CreateRectangle (integrator.PhysicsContext, boxShape.Extent.X, boxShape.Extent.Y, bodyCreateMessage.Density, shapeCenter, bodyCreateMessage.EntityAddress)
        ignore (body.Position <- toPhysicsVector2 bodyCreateMessage.Position)
        ignore (body.Rotation <- bodyCreateMessage.Rotation)
        ignore (body.BodyType <- toPhysicsBodyType bodyCreateMessage.BodyType)
        body.add_OnCollision
            (fun fixture fixture2 contact ->
                let bodyCollisionMessage =
                    { EntityAddress = fixture.Body.UserData :?> Address
                      EntityAddress2 = fixture2.Body.UserData :?> Address
                      Normal = toVector2 contact.Manifold.LocalNormal
                      Speed = contact.TangentSpeed }
                let integrationMessage = BodyCollisionMessage bodyCollisionMessage
                ignore (integrator.IntegrationMessages.Add integrationMessage)
                true)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsId, body)

let destroyBody integrator (bodyDestroyMessage : BodyDestroyMessage) =
    let body = Unchecked.defaultof<Dynamics.Body>
    if integrator.Bodies.TryGetValue (bodyDestroyMessage.PhysicsId, ref body) then
        ignore (integrator.Bodies.Remove bodyDestroyMessage.PhysicsId)
        integrator.PhysicsContext.RemoveBody body
    else debug ("Could not remove non-existent body with PhysicsId = " + str bodyDestroyMessage.PhysicsId + "'.")

let forceApply integrator forceApplyMessage =
    let body = Unchecked.defaultof<Dynamics.Body>
    if integrator.Bodies.TryGetValue (forceApplyMessage.PhysicsId, ref body) then
        body.ApplyForce (toPhysicsVector2 forceApplyMessage.Force, toPhysicsVector2 forceApplyMessage.Point)
    else debug ("Could not apply force to non-existent body with PhysicsId = " + str forceApplyMessage.PhysicsId + "'.")

let handlePhysicsMessage integrator physicsMessage =
    match physicsMessage with
    | BodyCreateMessage bodyCreateMessage -> createBody integrator bodyCreateMessage
    | BodyDestroyMessage bodyDestroyMessage -> destroyBody integrator bodyDestroyMessage
    | ForceApplyMessage forceApplyMessage -> forceApply integrator forceApplyMessage
    
let handlePhysicsMessages integrator physicsMessages =
    for physicsMessage in physicsMessages do
        handlePhysicsMessage integrator physicsMessage

let createTransformMessages integrator =
    for body in integrator.Bodies.Values do
        if body.Awake then
            let bodyTransformMessage =
                BodyTransformMessage
                    { EntityAddress = body.UserData :?> Address
                      Position = toVector2 body.Position
                      Rotation = body.Rotation }
            integrator.IntegrationMessages.Add bodyTransformMessage

let integrate physicsMessages integrator : IntegrationMessage list =
    handlePhysicsMessages integrator physicsMessages
    integrator.PhysicsContext.Step PhysicsStepRate
    createTransformMessages integrator
    List.ofSeq integrator.IntegrationMessages

let makeIntegrator gravity =
     { PhysicsContext = FarseerPhysics.Dynamics.World Gravity
       Bodies = BodyDictionary ()
       IntegrationMessages = System.Collections.Generic.List<IntegrationMessage> () }