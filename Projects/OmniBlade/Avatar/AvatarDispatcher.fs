// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<AutoOpen>]
module AvatarExtensions =
    type Entity with
        member this.GetAvatar world = this.GetModelGeneric<Avatar> world
        member this.SetAvatar value world = this.SetModelGeneric<Avatar> value world
        member this.Avatar = this.ModelGeneric<Avatar> ()

type AvatarDispatcher () =
    inherit Entity2dDispatcher<Avatar, Message, Command>
        (true, Avatar.make (box3 v3Zero Constants.Gameplay.CharacterSize) Assets.Field.JinnAnimationSheet Downward)

    static let getSpriteInset (entity : Entity) world =
        let avatar = entity.GetAvatar world
        let inset = Avatar.getAnimationInset world.UpdateTime avatar
        inset

    static member Facets =
        [typeof<RigidBodyFacet>]

    override this.Definitions (avatar, _) =
        let bodyShape =
            BodyShapes
                [SphereShape { Radius = 0.172f; TransformOpt = Some (Affine.makeTranslation (v3 -0.013f -0.354f 0.0f)); PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeIndex = Constants.Field.AvatarCollisionShapeIndex }}
                 SphereShape { Radius = 0.320f; TransformOpt = Some (Affine.makeTranslation (v3 -0.013f -0.354f 0.0f)); PropertiesOpt = Some { BodyShapeProperties.empty with BodyShapeIndex = Constants.Field.AvatarSensorShapeIndex; SensorOpt = Some true }}]
        [Entity.Perimeter := avatar.Perimeter
         Entity.Presence == Omnipresent
         Entity.LinearDamping == 19.0f
         Entity.AngularFactor == v3Zero
         Entity.GravityOverride == Some v3Zero
         Entity.BodyType == Dynamic
         Entity.BodyShape := bodyShape
         Entity.PhysicsMotion == ManualMotion]

    override this.Render (avatar, _, entity, world) =
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = avatar.AnimationSheet
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = ValueSome (getSpriteInset entity world)
                          Image = avatar.AnimationSheet
                          Color = Color.One
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world