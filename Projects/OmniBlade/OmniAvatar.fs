namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module AvatarDispatcherModule =

    type [<NoComparison>] AvatarMessage =
        | Update
        | Collision of CollisionData
        | Separation of SeparationData

    type Entity with

        member this.GetAvatarModel = this.GetModel<AvatarModel>
        member this.SetAvatarModel = this.SetModel<AvatarModel>
        member this.AvatarModel = this.Model<AvatarModel> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher<AvatarModel, AvatarMessage, unit>
            (AvatarModel.make (v4Bounds (v2 128.0f 128.0f) Constants.Gameplay.CharacterSize) Assets.FinnAnimationSheet Downward)

        static let sensorShapeId =
            Gen.id

        static let getSpriteInset (avatar : Entity) world =
            let model = avatar.GetAvatarModel world
            let index = AvatarModel.getAnimationIndex (World.getTickTime world) model
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = v4Bounds offset Constants.Gameplay.CharacterSize
            inset

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Channel (_, entity) =
            [entity.UpdateEvent => msg Update
             entity.CollisionEvent =|> fun evt -> msg (Collision evt.Data)
             entity.SeparationEvent =|> fun evt -> msg (Separation evt.Data)]

        override this.Initializers (model, entity) =
            let bodyShapes =
                BodyShapes
                    [BodyCircle { Radius = 0.22f; Center = v2 0.0f -0.3f; PropertiesOpt = None }
                     BodyCircle { Radius = 0.33f; Center = v2 0.0f -0.3f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = sensorShapeId; IsSensorOpt = Some true }}]
            [entity.Bounds <== model.Map (fun model -> model.Bounds)
             entity.FixedRotation == true
             entity.GravityScale == 0.0f
             entity.BodyShape == bodyShapes]

        override this.Message (model, message, entity, world) =
            let time = World.getTickTime world
            match message with
            | Update ->

                // update animation
                let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
                let direction = Direction.fromVector2 velocity
                let speed = velocity.Length
                let model =
                    if speed > 10.0f then
                        if direction <> model.Direction || model.AnimationCycle = IdleCycle then
                            let model = AvatarModel.updateDirection (constant direction) model
                            AvatarModel.animate time WalkCycle model
                        else model
                    else AvatarModel.animate time IdleCycle model

                // update bounds
                let model = AvatarModel.updateBounds (constant (entity.GetBounds world)) model
                just model

            | Separation separation ->

                // remove separated body shape
                let model =
                    if separation.Separator.BodyShapeId = sensorShapeId
                    then AvatarModel.updateIntersectedBodyShapes (List.remove ((=) separation.Separatee)) model
                    else model
                just model

            | Collision collision ->

                // add collidee shape
                let model =
                    if collision.Collider.BodyShapeId = sensorShapeId
                    then AvatarModel.updateIntersectedBodyShapes (fun shapes -> collision.Collidee :: shapes) model
                    else model
                just model

        override this.View (model, entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                [Render
                    (LayerableDescriptor
                        { Depth = entity.GetDepth world
                          PositionY = (entity.GetPosition world).Y
                          AssetTag = model.AnimationSheet
                          LayeredDescriptor =
                          SpriteDescriptor
                            { Position = entity.GetPosition world
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              Offset = Vector2.Zero
                              ViewType = entity.GetViewType world
                              InsetOpt = Some (getSpriteInset entity world)
                              Image = model.AnimationSheet
                              Color = v4One
                              Glow = v4Zero
                              Flip = FlipNone }})]
            else []