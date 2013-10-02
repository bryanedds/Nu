module Nu.Physics
open System.Collections.Generic
open FarseerPhysics
open OpenTK
open Microsoft.Xna
open Nu.Core
open Nu.Constants

type [<StructuralEquality; NoComparison>] BoxShape =
    { Position : Vector2 // NOTE: I guess this is like a center offset for the shape?
      Extent : Vector2 }

type [<StructuralEquality; NoComparison>] BodyShape =
    | BoxShape of BoxShape

type [<StructuralEquality; NoComparison>] BodyType =
    | Static
    | Kinematic
    | Dynamic

type [<StructuralEquality; NoComparison>] BodyCreateMessage =
    { PhysicsID : ID
      EntityAddress : Address
      Shape : BodyShape
      Position : Vector2
      Rotation : single
      Density : single
      BodyType : BodyType }

type [<StructuralEquality; NoComparison>] BodyDestroyMessage =
    { PhysicsID : ID }

type [<StructuralEquality; NoComparison>] ForceApplyMessage =
    { PhysicsID : ID
      Force : Vector2 }

type [<StructuralEquality; NoComparison>] CollisionMessage =
    { EntityAddress : Address
      EntityAddress2 : Address
      Normal : Vector2
      Speed : single }

type BodyDictionary = Dictionary<ID, Dynamics.Body>

type [<StructuralEquality; NoComparison>] PhysicsMessage =
    | BodyCreateMessage of BodyCreateMessage
    | BodyDestroyMessage of BodyDestroyMessage
    | ForceApplyMessage  of ForceApplyMessage

type  [<StructuralEquality; NoComparison>] IntegrationMessage =
    | CollisionMessage of CollisionMessage
    | BodyTranformMessage // of ...

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
        let shapePosition = toPhysicsVector2 boxShape.Position
        let body = Factories.BodyFactory.CreateRectangle (integrator.PhysicsContext, boxShape.Extent.X, boxShape.Extent.Y, bodyCreateMessage.Density, shapePosition, bodyCreateMessage.EntityAddress)
        ignore (body.Position <- toPhysicsVector2 bodyCreateMessage.Position)
        ignore (body.Rotation <- bodyCreateMessage.Rotation)
        ignore (body.BodyType <- toPhysicsBodyType bodyCreateMessage.BodyType)
        body.add_OnCollision
            (fun fixture fixture2 contact ->
                let collisionMessage =
                    { EntityAddress = fixture.Body.UserData :?> Address
                      EntityAddress2 = fixture2.Body.UserData :?> Address
                      Normal = toVector2 contact.Manifold.LocalNormal
                      Speed = contact.TangentSpeed }
                let integrationMessage = CollisionMessage collisionMessage
                ignore (integrator.IntegrationMessages.Add integrationMessage)
                true)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsID, body)

let destroyBody integrator bodyDestroyMessage =
    let body = Unchecked.defaultof<Dynamics.Body>
    if integrator.Bodies.TryGetValue (bodyDestroyMessage.PhysicsID, ref body) then integrator.PhysicsContext.RemoveBody body
    else debug ("Could not remove non-existent body with PhysicsID = " + str bodyDestroyMessage.PhysicsID + "'.")

let integrate physicsMessages integrator : IntegrationMessage list =
    // TODO: handle physics messages
    ignore (integrator.PhysicsContext.Step PhysicsStepRate)
    List.ofSeq integrator.IntegrationMessages

let makeIntegrator gravity =
     { PhysicsContext = FarseerPhysics.Dynamics.World Gravity
       Bodies = BodyDictionary ()
       IntegrationMessages = System.Collections.Generic.List<IntegrationMessage> () }