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

    type [<NoComparison>] AvatarMessage =
        | Update
        | TryFace of Direction
        | Nil
        interface Message

    type [<NoComparison>] AvatarCommand =
        | TryTravel of Vector3
        interface Command

    type Entity with
        member this.GetAvatar world = this.GetModelGeneric<Avatar> world
        member this.SetAvatar value world = this.SetModelGeneric<Avatar> value world
        member this.Avatar = this.ModelGeneric<Avatar> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher2d<Avatar, AvatarMessage, AvatarCommand>
            (true, Avatar.make (box3 v3Zero Constants.Gameplay.CharacterSize) Assets.Field.JinnAnimationSheet Downward)

        static let getSpriteInset (entity : Entity) world =
            let avatar = entity.GetAvatar world
            let inset = Avatar.getAnimationInset (World.getUpdateTime world) avatar
            inset

        static let isIntersectedProp collider collidee (avatar : Avatar) world =
            if (collider.BodyShapeId = avatar.CoreShapeId &&
                collidee.Entity.Exists world &&
                collidee.Entity.Is<PropDispatcher> world &&
                match (collidee.Entity.GetPropPlus world).Prop.PropData with
                | Portal _ -> true
                | Sensor _ -> true
                | _ -> false) then
                true
            elif (collider.BodyShapeId = avatar.SensorShapeId &&
                  collidee.Entity.Exists world &&
                  collidee.Entity.Is<PropDispatcher> world &&
                  match (collidee.Entity.GetPropPlus world).Prop.PropData with
                  | Portal _ -> false
                  | Sensor _ -> false
                  | _ -> true) then
                true
            else false

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initialize (avatar, _) =
            let bodyShape =
                BodyShapes
                    [BodySphere { Center = v3 -0.016f -0.366f 0.0f; Radius = 0.160f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = avatar.CoreShapeId }}
                     BodySphere { Center = v3 -0.016f -0.366f 0.0f; Radius = 0.320f; PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeId = avatar.SensorShapeId; SensorOpt = Some true }}]
            [Entity.Perimeter := avatar.Perimeter
             Entity.Presence == Omnipresent
             Entity.FixedRotation == true
             Entity.GravityScale == 0.0f
             Entity.BodyShape := bodyShape
             Entity.UpdateEvent => Update]

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

            | Nil ->

                // nothing to do
                just avatar

        override this.Command (_, command, entity, world) =
            match command with
            | TryTravel force ->
                if  not (World.isSelectedScreenTransitioning world) &&
                    force <> v3Zero &&
                    entity.GetEnabled world then
                    let physicsId = Simulants.FieldSceneAvatar.GetPhysicsId world
                    let world = World.applyBodyForce force physicsId world
                    just world
                else just world

        override this.Physics (center, _, _, _, avatar, _, _) =
            let avatar = Avatar.updateCenter (constant center) avatar
            just avatar

        override this.View (avatar, entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                Render2d (transform.Elevation, transform.Horizon, AssetTag.generalize avatar.AnimationSheet,
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = ValueSome (getSpriteInset entity world)
                          Image = avatar.AnimationSheet
                          Color = Color.One
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone })
            else View.empty