namespace OmniBlade
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
            (PropModel.make (v4Bounds v2Zero Constants.Gameplay.TileSize) 0.0f Set.empty PropData.empty NilState)

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.FixedRotation true
             define Entity.GravityScale 0.0f]

        override this.Channel (_, entity) =
            [entity.UpdateEvent => msg Update]

        override this.Initializers (model, entity) =
            [entity.Bounds <== model --> fun model -> model.Bounds
             entity.IsSensor <== model --> fun model -> match model.PropData with Sensor -> true | _ -> false
             entity.BodyType == Static
             entity.LinearDamping == 0.0f
             entity.GravityScale == 0.0f
             entity.BodyShape <== model --> fun model ->
                match model.PropData with
                | Npc _ -> BodyCircle { Radius = 0.22f; Center = v2 0.0f -0.3f; PropertiesOpt = None }
                | _ -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }]

        override this.Register (entity, world) =
            base.Register (entity, world)

        override this.Message (model, message, entity, world) =
            match message with
            | Update ->
                let model = PropModel.updateBounds (constant (entity.GetBounds world)) model
                just model

        override this.View (model, entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                let (image, insetOpt) =
                    match model.PropData with
                    | Chest (_, chestType, chestId, _, _) ->
                        let image =
                            match chestType with
                            | WoodenChest ->
                                if Set.contains (Opened chestId) model.Advents
                                then Assets.WoodenChestOpenedImage
                                else Assets.WoodenChestClosedImage
                            | BrassChest ->
                                if Set.contains (Opened chestId) model.Advents
                                then Assets.BrassChestOpenedImage
                                else Assets.BrassChestClosedImage
                        (image, None)
                    | Door (_, doorType) ->
                        let image =
                            match doorType with
                            | WoodenDoor ->
                                match model.PropState with
                                | DoorState opened -> if opened then Assets.WoodenDoorOpenedImage else Assets.WoodenDoorClosedImage
                                | _ -> failwithumf ()
                        (image, None)
                    | Portal -> (Assets.CancelImage, None)
                    | Switch -> (Assets.CancelImage, None)
                    | Sensor -> (Assets.CancelImage, None)
                    | Npc (npcType, direction, _) ->
                        let image = Assets.NpcAnimationSheet
                        let row =
                            match npcType with
                            | VillageMan -> 0
                            | VillageWoman -> 1
                            | VillageBoy -> 2
                            | VillageGirl -> 3
                        let column =
                            match direction with
                            | Downward -> 0
                            | Leftward -> 1
                            | Upward -> 2
                            | Rightward -> 3
                        let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterSize
                        let inset = v4Bounds insetPosition Constants.Gameplay.CharacterSize
                        (image, Some inset)
                    | Shopkeep shopkeepType ->
                        (Assets.CancelImage, None)
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
                              InsetOpt = insetOpt
                              Image = image
                              Color = v4One
                              Glow = v4Zero
                              Flip = FlipNone }})]
            else []