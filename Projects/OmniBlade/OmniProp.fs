namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module PropDispatcherModule =

    type PropMessage =
        | Update

    type Entity with

        member this.GetPropModel = this.GetModel<PropModel>
        member this.SetPropModel = this.SetModel<PropModel>
        member this.PropModel = this.Model<PropModel> ()

    type PropDispatcher () =
        inherit EntityDispatcher<PropModel, PropMessage, unit>
            (PropModel.make (Chest (WoodenChest, Unlocked, Consumable GreenHerb)) (v4Bounds v2Zero Constants.Gameplay.TileSize))

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.FixedRotation true
             define Entity.GravityScale 0.0f
             define Entity.CollisionBody (BodyBox { Extent = Constants.Gameplay.TileSize * 0.5f; Center = v2Zero })]

        override this.Channel (_, entity, _) =
            [entity.UpdateEvent => [msg Update]]

        override this.Initializers (model, entity, _) =
            [entity.Bounds <== model.Map (fun model -> model.Bounds)
             entity.IsSensor <== model.Map (fun model -> match model.PropData with Sensor -> true | _ -> false)
             entity.BodyType == Static
             entity.LinearDamping == 0.0f
             entity.GravityScale == 0.0f]

        override this.Message (model, message, entity, world) =
            match message with
            | Update ->
                let model = PropModel.updateBounds (constant (entity.GetBounds world)) model
                just model

        override this.View (model, entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                let image =
                    match model.PropData with
                    | Chest (chestType, _, _) ->
                        match chestType with
                        | WoodenChest -> Assets.WoodenChestImage
                        | BrassChest -> Assets.BrassChestImage
                    | _ ->
                        Assets.CancelImage
                [Render
                    (LayerableDescriptor
                        { Depth = entity.GetDepth world
                          PositionY = (entity.GetPosition world).Y
                          AssetTag = image
                          LayeredDescriptor =
                          SpriteDescriptor
                            { Position = entity.GetPosition world
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              Offset = Vector2.Zero
                              ViewType = entity.GetViewType world
                              InsetOpt = None
                              Image = image
                              Color = v4One
                              Glow = v4Zero
                              Flip = FlipNone }})]
            else []