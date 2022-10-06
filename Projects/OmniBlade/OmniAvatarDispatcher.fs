// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module AvatarDispatcher =

    type [<StructuralEquality; NoComparison>] AvatarMessage =
        | Update
        | PostUpdate
        | BodyCollision of BodyCollisionData
        | BodySeparation of BodySeparationData
        | BodyRemoving of PhysicsId
        | TryFace of Direction
        | Nil

    type [<StructuralEquality; NoComparison>] AvatarCommand =
        | TryTravel of Vector3

    type Entity with
        member this.GetAvatar world = this.GetModelGeneric<Avatar> world
        member this.SetAvatar value world = this.SetModelGeneric<Avatar> value world
        member this.Avatar = this.ModelGeneric<Avatar> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher2d<Avatar, AvatarMessage, AvatarCommand>
            (false, true, Avatar.make (box3 v3Zero Constants.Gameplay.CharacterSize) Assets.Field.JinnAnimationSheet Downward)

        static let coreShapeId = Gen.id64
        static let sensorShapeId = Gen.id64

        static let getSpriteInset (entity : Entity) world =
            let avatar = entity.GetAvatar world
            let inset = Avatar.getAnimationInset (World.getUpdateTime world) avatar
            inset

        static let isIntersectedBodyShape collider collidee world =
            if (collider.BodyShapeId = coreShapeId &&
                collidee.Entity.Exists world &&
                collidee.Entity.Is<PropDispatcher> world &&
                match (collidee.Entity.GetProp world).PropData with
                | Portal _ -> true
                | Sensor _ -> true
                | _ -> false) then
                true
            elif (collider.BodyShapeId = sensorShapeId &&
                  collidee.Entity.Exists world &&
                  collidee.Entity.Is<PropDispatcher> world &&
                  match (collidee.Entity.GetProp world).PropData with
                  | Portal _ -> false
                  | Sensor _ -> false
                  | _ -> true) then
                true
            else false

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initializers (avatar, entity) =
            let bodyShapes =
                BodyShapes
                    [BodySphere { Radius = 0.160f; Center = v3 -0.016f -0.3667f 0.0f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = coreShapeId }}
                     BodySphere { Radius = 0.320f; Center = v3 -0.016f -0.3667f 0.0f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = sensorShapeId; SensorOpt = Some true }}]
            [entity.Perimeter <== avatar --> fun avatar -> avatar.Perimeter
             Entity.Presence == Omnipresent
             entity.FixedRotation == true
             entity.GravityScale == 0.0f
             entity.BodyShape == bodyShapes]

        override this.Channel (_, entity) =
            [entity.UpdateEvent => msg Update
             entity.Group.PostUpdateEvent => msg PostUpdate
             entity.BodyCollisionEvent =|> fun evt -> msg (BodyCollision evt.Data)
             entity.BodySeparationEvent =|> fun evt -> msg (BodySeparation evt.Data)
             Simulants.Game.BodyRemovingEvent =|> fun evt -> msg (BodyRemoving evt.Data)]

        override this.Message (avatar, message, entity, world) =
            let time = World.getUpdateTime world
            match message with
            | Update ->

                // update animation generally
                let velocity = entity.GetLinearVelocity world
                let speed = velocity.Magnitude
                let direction = Direction.ofVector3Biased velocity
                let avatar =
                    if speed > Constants.Field.AvatarIdleSpeedMax then
                        if direction <> avatar.Direction || avatar.CharacterAnimationType = IdleAnimation then
                            let avatar = Avatar.updateDirection (constant direction) avatar
                            Avatar.animate time WalkAnimation avatar
                        else avatar
                    else Avatar.animate time IdleAnimation avatar
                just avatar

            | PostUpdate ->

                // clear all temporary body shapes
                let avatar = Avatar.updateCollidedBodyShapes (constant []) avatar
                let avatar = Avatar.updateSeparatedBodyShapes (constant []) avatar
                just avatar

            | TryFace direction ->

                // update facing if enabled, speed is low, and direction pressed
                let avatar =
                    if  not (World.isSelectedScreenTransitioning world) &&
                        entity.GetEnabled world then
                        let velocity = entity.GetLinearVelocity world
                        let speed = velocity.Magnitude
                        if speed <= Constants.Field.AvatarIdleSpeedMax
                        then Avatar.updateDirection (constant direction) avatar
                        else avatar
                    else avatar
                just avatar

            | BodySeparation separation ->

                // add separated body shape
                let avatar =
                    if isIntersectedBodyShape separation.BodySeparator separation.BodySeparatee world then
                        let avatar = Avatar.updateSeparatedBodyShapes (fun shapes -> separation.BodySeparatee :: shapes) avatar
                        let avatar = Avatar.updateIntersectedBodyShapes (List.remove ((=) separation.BodySeparatee)) avatar
                        avatar
                    else avatar
                just avatar

            | BodyCollision collision ->

                // add collided body shape
                let avatar =
                    if isIntersectedBodyShape collision.BodyCollider collision.BodyCollidee world then
                        let avatar = Avatar.updateCollidedBodyShapes (fun shapes -> collision.BodyCollidee :: shapes) avatar
                        let avatar = Avatar.updateIntersectedBodyShapes (fun shapes -> collision.BodyCollidee :: shapes) avatar
                        avatar
                    else avatar
                just avatar

            | BodyRemoving physicsId ->
                
                // unfortunately, due to the fact that physics events like separation don't fire until the next frame,
                // we need to handle this Nu-generated event in order to remove the associated shape manually.
                let (separatedBodyShapes, intersectedBodyShapes) = List.split (fun shape -> shape.Entity.GetPhysicsId world = physicsId) avatar.IntersectedBodyShapes
                let avatar = Avatar.updateIntersectedBodyShapes (constant intersectedBodyShapes) avatar
                let avatar = Avatar.updateSeparatedBodyShapes ((@) separatedBodyShapes) avatar
                just avatar

            | Nil ->

                // nothing to do
                just avatar

        override this.Command (_, command, entity, world) =
            match command with
            | TryTravel force ->
                if  not (World.isSelectedScreenTransitioning world) &&
                    force <> v3Zero &&
                    entity.GetEnabled world then
                    let physicsId = Simulants.Field.Scene.Avatar.GetPhysicsId world
                    let world = World.applyBodyForce force physicsId world
                    just world
                else just world

        override this.Physics (position, _, _, _, avatar, _, _) =
            let avatar = Avatar.updatePosition (constant position) avatar
            just avatar

        override this.View (avatar, entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                Render2d (transform.Elevation, transform.Position.Y, AssetTag.generalize avatar.AnimationSheet,
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = ValueSome (getSpriteInset entity world)
                          Image = avatar.AnimationSheet
                          Color = Color.One
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone })
            else View.empty