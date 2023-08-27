// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module AvatarDispatcher =

    type Entity with
        member this.GetAvatar world = this.GetModelGeneric<Avatar> world
        member this.SetAvatar value world = this.SetModelGeneric<Avatar> value world
        member this.Avatar = this.ModelGeneric<Avatar> ()

    type AvatarDispatcher () =
        inherit EntityDispatcher2d<Avatar, Message, Command>
            (true, Avatar.make (box3 v3Zero Constants.Gameplay.CharacterSize) Assets.Field.JinnAnimationSheet Downward)

        static let getSpriteInset (entity : Entity) world =
            let avatar = entity.GetAvatar world
            let inset = Avatar.getAnimationInset world.UpdateTime avatar
            inset

        static member Facets =
            [typeof<RigidBodyFacet>]

        override this.Initialize (avatar, _) =
            let bodyShape =
                BodyShapes
                    [BodySphere { Radius = 0.172f; TransformOpt = Some (Matrix4x4.CreateTranslation (v3 -0.016f -0.355f 0.0f)); PropertiesOpt = Some { BodyShapeProperties.empty with ShapeIndex = Constants.Field.AvatarCollisionShapeIndex }}
                     BodySphere { Radius = 0.320f; TransformOpt = Some (Matrix4x4.CreateTranslation (v3 -0.016f -0.355f 0.0f)); PropertiesOpt = Some { BodyShapeProperties.empty with ShapeIndex = Constants.Field.AvatarSensorShapeIndex; SensorOpt = Some true }}]
            [Entity.Perimeter := avatar.Perimeter
             Entity.Presence == Omnipresent
             Entity.LinearDamping == 19.0f
             Entity.AngularFactor == v3Zero
             Entity.GravityOverride == Some v3Zero
             Entity.BodyShape := bodyShape
             Entity.ModelDriven == true]

        override this.View (avatar, entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                Render2d (transform.Elevation, transform.Horizon, AssetTag.generalize avatar.AnimationSheet,
                    RenderSprite
                        { Transform = transform
                          InsetOpt = ValueSome (getSpriteInset entity world)
                          Image = avatar.AnimationSheet
                          Color = Color.One
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone })
            else View.empty