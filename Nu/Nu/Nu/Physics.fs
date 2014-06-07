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
open Nu.NuCore
open Nu.NuConstants

[<AutoOpen>]
module PhysicsModule =

    type PhysicsId = Guid * Guid

    type Vertices = Vector2 list

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

    type [<StructuralEquality; NoComparison>] PolygonShape =
        { Vertices : Vertices
          Properties : CommonShapeProperties }

    type [<StructuralEquality; NoComparison>] BodyShape =
        | BoxShape of BoxShape
        | CircleShape of CircleShape
        | PolygonShape of PolygonShape

    type [<StructuralEquality; NoComparison; TypeConverter (typeof<BodyTypeTypeConverter>)>] BodyType =
        | Static
        | Kinematic
        | Dynamic

    and BodyTypeTypeConverter () =
        inherit TypeConverter ()
        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        override this.ConvertTo (_, culture, obj : obj, _) =
            let bodyType = obj :?> BodyType
            match bodyType with
            | Static -> "Static" :> obj
            | Kinematic -> "Kinematic" :> obj
            | Dynamic -> "Dynamic" :> obj
        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Vector2> || sourceType = typeof<string>
        override this.ConvertFrom (_, culture, obj : obj) =
            let sourceType = obj.GetType ()
            if sourceType = typeof<BodyType> then obj
            else
                match obj :?> string with
                | "Static" -> Static :> obj
                | "Kinematic" -> Kinematic :> obj
                | "Dynamic" -> Dynamic :> obj
                | other -> failwith <| "Unknown BodyType '" + other + "'."

    type [<StructuralEquality; NoComparison>] BodyCreateMessage =
        { EntityAddress : Address
          PhysicsId : PhysicsId
          Shape : BodyShape
          Position : Vector2
          Rotation : single
          Density : single
          BodyType : BodyType }

    type [<StructuralEquality; NoComparison>] BodyDestroyMessage =
        { PhysicsId : PhysicsId }

    type [<StructuralEquality; NoComparison>] ApplyImpulseMessage =
        { PhysicsId : PhysicsId
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

    type BodyDictionary =
        Dictionary<PhysicsId, Dynamics.Body>

    type [<StructuralEquality; NoComparison>] PhysicsMessage =
        | BodyCreateMessage of BodyCreateMessage
        | BodyDestroyMessage of BodyDestroyMessage
        | ApplyImpulseMessage of ApplyImpulseMessage
        | SetGravityMessage of Vector2
        | ResetHackMessage

    type [<StructuralEquality; NoComparison>] IntegrationMessage =
        | BodyCollisionMessage of BodyCollisionMessage
        | BodyTransformMessage of BodyTransformMessage

    type [<ReferenceEquality>] Integrator =
        { PhysicsContext : Dynamics.World
          Bodies : BodyDictionary
          IntegrationMessages : IntegrationMessage List }

module Physics =

    let InvalidPhysicsId =
        (InvalidId, InvalidId)

    let getPhysicsId (entityId : Guid) =
        (entityId, Guid.NewGuid ())

    let private toPixel value =
        value * Nu.NuConstants.PhysicsToPixelRatio

    let private toPhysics value =
        value * Nu.NuConstants.PixelToPhysicsRatio

    let private toPixelV2 (v2 : Framework.Vector2) =
        Vector2 (toPixel v2.X, toPixel v2.Y)

    let private toPhysicsV2 (v2 : Vector2) =
        Framework.Vector2 (toPhysics v2.X, toPhysics v2.Y)

    let private toPhysicsBodyType bodyType =
        match bodyType with
        | Static -> Dynamics.BodyType.Static
        | Kinematic -> Dynamics.BodyType.Kinematic
        | Dynamic -> Dynamics.BodyType.Dynamic

    let private handlePhysicsCollision
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

    let getBodyContacts physicsId integrator =
        let body = integrator.Bodies.[physicsId]
        let contacts = List<Contact> ()
        let mutable current = body.ContactList
        while current <> null do
            contacts.Add current.Contact
            current <- current.Next
        List.ofSeq contacts

    let isBodyOnGround physicsId integrator =
        let normal = ref <| Framework.Vector2 ()
        let manifold = ref <| FixedArray2<Framework.Vector2> ()
        let contacts = getBodyContacts physicsId integrator
        List.exists
            (fun (contact : Contact) ->
                contact.GetWorldManifold (normal, manifold)
                let upVector = Framework.Vector2 (0.0f, 1.0f)
                let theta = Framework.Vector2.Dot (!normal, upVector) |> double |> Math.Acos |> Math.Abs
                theta < Math.PI * 0.5)
            contacts

    let private configureBodyProperties integrator bodyPosition bodyRotation commonShapeProperties (body : Body) =
        body.Position <- toPhysicsV2 bodyPosition
        body.Rotation <- bodyRotation
        body.Restitution <- commonShapeProperties.Restitution
        body.FixedRotation <- commonShapeProperties.FixedRotation
        body.LinearDamping <- commonShapeProperties.LinearDamping
        body.AngularDamping <- commonShapeProperties.AngularDamping
        body.SleepingAllowed <- true

    let private createBoxBody integrator bodyCreateMessage boxShape =
        let body =
            Factories.BodyFactory.CreateRectangle (
                integrator.PhysicsContext,
                toPhysics <| boxShape.Extent.X * 2.0f,
                toPhysics <| boxShape.Extent.Y * 2.0f,
                bodyCreateMessage.Density,
                toPhysicsV2 boxShape.Properties.Center,
                0.0f,
                toPhysicsBodyType bodyCreateMessage.BodyType,
                bodyCreateMessage.EntityAddress)
        configureBodyProperties integrator bodyCreateMessage.Position bodyCreateMessage.Rotation boxShape.Properties body
        body

    let private createCircleBody integrator bodyCreateMessage circleShape =
        let body =
            Factories.BodyFactory.CreateCircle (
                integrator.PhysicsContext,
                toPhysics circleShape.Radius,
                bodyCreateMessage.Density,
                toPhysicsV2 circleShape.Properties.Center,
                toPhysicsBodyType bodyCreateMessage.BodyType,
                bodyCreateMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        configureBodyProperties integrator bodyCreateMessage.Position bodyCreateMessage.Rotation circleShape.Properties body
        body.UserData <- bodyCreateMessage.EntityAddress // BUG: ...so I set it again here :/
        body

    let private createPolygonBody integrator bodyCreateMessage polygonShape =
        let body =
            Factories.BodyFactory.CreatePolygon (
                integrator.PhysicsContext,
                FarseerPhysics.Common.Vertices (List.map toPhysicsV2 polygonShape.Vertices),
                bodyCreateMessage.Density,
                toPhysicsV2 polygonShape.Properties.Center,
                0.0f,
                toPhysicsBodyType bodyCreateMessage.BodyType,
                bodyCreateMessage.EntityAddress) // BUG: Farseer doesn't seem to set the UserData with the parameter I give it here...
        configureBodyProperties integrator bodyCreateMessage.Position bodyCreateMessage.Rotation polygonShape.Properties body
        body.UserData <- bodyCreateMessage.EntityAddress // BUG: ...so I set it again here :/
        body

    // TODO: remove code duplication here
    let private createBody integrator bodyCreateMessage =
        let body =
            match bodyCreateMessage.Shape with
            | BoxShape boxShape -> createBoxBody integrator bodyCreateMessage boxShape
            | CircleShape circleShape -> createCircleBody integrator bodyCreateMessage circleShape
            | PolygonShape polygonShape -> createPolygonBody integrator bodyCreateMessage polygonShape
        body.add_OnCollision (fun fn fn2 collision -> handlePhysicsCollision integrator fn fn2 collision) // NOTE: F# requires us to use an lambda inline here (not sure why)
        integrator.Bodies.Add (bodyCreateMessage.PhysicsId, body)

    let private destroyBody integrator (bodyDestroyMessage : BodyDestroyMessage) =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (bodyDestroyMessage.PhysicsId, body) then
            ignore <| integrator.Bodies.Remove bodyDestroyMessage.PhysicsId
            integrator.PhysicsContext.RemoveBody !body
        else note <| "Could not remove non-existent body with PhysicsId = " + string bodyDestroyMessage.PhysicsId + "'."

    let private applyImpulse integrator applyImpulseMessage =
        let body = ref Unchecked.defaultof<Dynamics.Body>
        if  integrator.Bodies.TryGetValue (applyImpulseMessage.PhysicsId, body) then
            (!body).ApplyLinearImpulse (toPhysicsV2 applyImpulseMessage.Impulse)
        else debug <| "Could not apply impulse to non-existent body with PhysicsId = " + string applyImpulseMessage.PhysicsId + "'."

    let private handlePhysicsMessage integrator physicsMessage =
        match physicsMessage with
        | BodyCreateMessage bodyCreateMessage -> createBody integrator bodyCreateMessage
        | BodyDestroyMessage bodyDestroyMessage -> destroyBody integrator bodyDestroyMessage
        | ApplyImpulseMessage applyImpulseMessage -> applyImpulse integrator applyImpulseMessage
        | SetGravityMessage gravity -> integrator.PhysicsContext.Gravity <- toPhysicsV2 gravity
        | ResetHackMessage ->
            integrator.PhysicsContext.Clear ()
            integrator.Bodies.Clear ()
            integrator.IntegrationMessages.Clear ()
    
    let private handlePhysicsMessages integrator (physicsMessages : PhysicsMessage rQueue) =
        for physicsMessage in List.rev physicsMessages do
            handlePhysicsMessage integrator physicsMessage

    let private createTransformMessages integrator =
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
         { PhysicsContext = FarseerPhysics.Dynamics.World (toPhysicsV2 Gravity)
           Bodies = BodyDictionary ()
           IntegrationMessages = List<IntegrationMessage> () }