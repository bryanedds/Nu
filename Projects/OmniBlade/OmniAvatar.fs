namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module AvatarDispatcherModule =

    type [<NoComparison>] AvatarMessage =
        | Update
        | PostUpdate
        | Collision of CollisionData
        | Separation of SeparationData
        | Face of Direction
        | Nil

    type [<NoComparison>] AvatarCommand =
        | Move of Vector2

    type Entity with

        member this.GetAvatarModel = this.GetModel<AvatarModel>
        member this.SetAvatarModel = this.SetModel<AvatarModel>
        member this.AvatarModel = this.Model<AvatarModel> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher<AvatarModel, AvatarMessage, AvatarCommand>
            (AvatarModel.make (v4Bounds (v2 128.0f 128.0f) Constants.Gameplay.CharacterSize) Assets.FinnAnimationSheet Downward)

        static let coreShapeId = Gen.id
        static let sensorShapeId = Gen.id

        static let getSpriteInset (avatar : Entity) world =
            let model = avatar.GetAvatarModel world
            let index = AvatarModel.getAnimationIndex (World.getTickTime world) model
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = v4Bounds offset Constants.Gameplay.CharacterSize
            inset

        static let isIntersectedBodyShape collider collidee world =
            if (collider.BodyShapeId = coreShapeId &&
                collidee.Entity.Exists world &&
                collidee.Entity.Is<PropDispatcher> world &&
                match (collidee.Entity.GetPropModel world).PropData with
                | Portal _ -> true
                | Sensor _ -> true
                | _ -> false) then
                true
            elif (collider.BodyShapeId = sensorShapeId &&
                  collidee.Entity.Exists world &&
                  collidee.Entity.Is<PropDispatcher> world &&
                  match (collidee.Entity.GetPropModel world).PropData with
                  | Portal _ -> false
                  | Sensor _ -> false
                  | _ -> true) then
                true
            else false

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initializers (model, entity) =
            let bodyShapes =
                BodyShapes
                    [BodyCircle { Radius = 0.22f; Center = v2 0.0f -0.3f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = coreShapeId }}
                     BodyCircle { Radius = 0.33f; Center = v2 0.0f -0.3f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = sensorShapeId; IsSensorOpt = Some true }}]
            [entity.Bounds <== model.Map (fun model -> model.Bounds)
             entity.FixedRotation == true
             entity.GravityScale == 0.0f
             entity.BodyShape == bodyShapes]

        override this.Channel (_, entity) =
            [entity.UpdateEvent => msg Update
             entity.Parent.PostUpdateEvent => msg PostUpdate
             entity.UpdateEvent =|> fun _ ->
                let force = v2Zero
                let force = if KeyboardState.isKeyDown KeyboardKey.Right then v2 Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Left then v2 -Constants.Field.WalkForce 0.0f + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Up then v2 0.0f Constants.Field.WalkForce + force else force
                let force = if KeyboardState.isKeyDown KeyboardKey.Down then v2 0.0f -Constants.Field.WalkForce + force else force
                cmd (Move force)
             entity.UpdateEvent =|> fun _ ->
                if KeyboardState.isKeyDown KeyboardKey.Right then msg (Face Rightward)
                elif KeyboardState.isKeyDown KeyboardKey.Left then msg (Face Leftward)
                elif KeyboardState.isKeyDown KeyboardKey.Up then msg (Face Upward)
                elif KeyboardState.isKeyDown KeyboardKey.Down then msg (Face Downward)
                else msg Nil
             entity.CollisionEvent =|> fun evt -> msg (Collision evt.Data)
             entity.SeparationEvent =|> fun evt -> msg (Separation evt.Data)]

        override this.Message (model, message, entity, world) =
            let time = World.getTickTime world
            match message with
            | Update ->

                // update animation generally
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

            | PostUpdate ->

                // clear all temporary body shapes
                let model = AvatarModel.updateCollidedBodyShapes (constant []) model
                let model = AvatarModel.updateSeparatedBodyShapes (constant []) model
                just model

            | Face direction ->

                // update facing if enabled, velocity is zero, and direction pressed
                let model =
                    if entity.GetEnabled world then
                        let velocity = entity.GetLinearVelocity world
                        if velocity = v2Zero
                        then AvatarModel.updateDirection (constant direction) model
                        else model
                    else model
                just model

            | Separation separation ->

                // add separated body shape
                let model =
                    if isIntersectedBodyShape separation.Separator separation.Separatee world then
                        let model = AvatarModel.updateSeparatedBodyShapes (fun shapes -> separation.Separatee :: shapes) model
                        let model = AvatarModel.updateIntersectedBodyShapes (List.remove ((=) separation.Separatee)) model
                        model
                    else model
                just model

            | Collision collision ->

                // add collided body shape
                let model =
                    if isIntersectedBodyShape collision.Collider collision.Collidee world then
                        let model = AvatarModel.updateCollidedBodyShapes (fun shapes -> collision.Collidee :: shapes) model
                        let model = AvatarModel.updateIntersectedBodyShapes (fun shapes -> collision.Collidee :: shapes) model
                        model
                    else model
                just model

            | Nil ->

                // nothing to do
                just model

        override this.Command (_, command, entity, world) =
            match command with
            | Move force ->
                if entity.GetEnabled world then
                    let physicsId = Simulants.FieldAvatar.GetPhysicsId world
                    let world = World.applyBodyForce force physicsId world
                    just world
                else just world

        override this.View (model, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize model.AnimationSheet,
                     SpriteDescriptor
                        { Transform = transform
                          Offset = Vector2.Zero
                          InsetOpt = Some (getSpriteInset entity world)
                          Image = model.AnimationSheet
                          Color = Color.White
                          Glow = Color.Zero
                          Flip = FlipNone })]
            else []