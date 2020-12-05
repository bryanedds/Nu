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
module PropDispatcher =

    type PropMessage =
        | Update

    type Entity with
        member this.GetProp = this.GetModel<Prop>
        member this.SetProp = this.SetModel<Prop>
        member this.Prop = this.Model<Prop> ()

    type PropDispatcher () =
        inherit EntityDispatcher<Prop, PropMessage, unit> (Prop.empty)

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.FixedRotation true
             define Entity.GravityScale 0.0f]

        override this.Channel (_, entity) =
            [entity.UpdateEvent => msg Update]

        override this.Initializers (prop, entity) =
            [entity.BodyType == Static
             entity.LinearDamping == 0.0f
             entity.GravityScale == 0.0f
             entity.Bounds <== prop --> fun prop -> prop.Bounds
             entity.IsSensor <== prop --> fun prop ->
                match prop.PropData with
                | Sensor _ | Portal _ | SavePoint -> true
                | _ -> false
             entity.BodyShape <== prop --> fun prop ->
                match prop.PropData with
                | Chest _ ->
                    BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Door _ ->
                    match prop.PropState with
                    | DoorState true -> BodyEmpty
                    | _ -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Sensor (_, shapeOpt, _, _) ->
                    match shapeOpt with
                    | Some shape -> shape
                    | None -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Npc _ ->
                    match prop.PropState with
                    | NpcState true -> BodyBox { Extent = v2 0.22f 0.22f; Center = v2 0.0f -0.3f; PropertiesOpt = None }
                    | _ -> BodyEmpty
                | Shopkeep _ ->
                    match prop.PropState with
                    | ShopkeepState true -> BodyBox { Extent = v2 0.22f 0.22f; Center = v2 0.0f -0.3f; PropertiesOpt = None }
                    | _ -> BodyEmpty
                | Portal _ | Switch _ | SavePoint _ ->
                    BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | ChestSpawn | EmptyProp ->
                    BodyEmpty]

        override this.Message (prop, message, entity, world) =
            match message with
            | Update ->
                let prop = Prop.updateBounds (constant (entity.GetBounds world)) prop
                just prop

        override this.View (prop, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                let (background, insetOpt, image) =
                    match prop.PropData with
                    | Chest (chestType, _, chestId, _, _, _) ->
                        let image =
                            match chestType with
                            | WoodenChest ->
                                if Set.contains (Opened chestId) prop.Advents
                                then Assets.Field.WoodenChestOpenedImage
                                else Assets.Field.WoodenChestClosedImage
                            | BrassChest ->
                                if Set.contains (Opened chestId) prop.Advents
                                then Assets.Field.BrassChestOpenedImage
                                else Assets.Field.BrassChestClosedImage
                        (false, None, image)
                    | Door (doorType, _, _) ->
                        let image =
                            match doorType with
                            | WoodenDoor ->
                                match prop.PropState with
                                | DoorState opened -> if opened then Assets.Field.WoodenDoorOpenedImage else Assets.Field.WoodenDoorClosedImage
                                | _ -> failwithumf ()
                        (false, None, image)
                    | Portal (_, _, _, _, _) ->
                        (false, None, Assets.Default.EmptyImage)
                    | Switch (switchType, _, _) ->
                        let image =
                            match switchType with
                            | ThrowSwitch ->
                                match prop.PropState with
                                | SwitchState on -> if on then Assets.Field.ThrowSwitchOnImage else Assets.Field.ThrowSwitchOffImage
                                | _ -> failwithumf ()
                        (false, None, image)
                    | Sensor (sensorType, _, _, _) ->
                        match sensorType with
                        | AirSensor -> (true, None, Assets.Default.EmptyImage)
                        | HiddenSensor -> (true, None, Assets.Default.EmptyImage)
                        | StepPlateSensor -> (true, None, Assets.Field.StepPlateImage)
                    | Npc (npcType, _, direction, _, _) ->
                        match prop.PropState with
                        | NpcState true ->
                            let image = Assets.Field.NpcAnimationSheet
                            let (row, column) =
                                match npcType with
                                | VillageMan -> (0, 0)
                                | VillageWoman -> (1, 0)
                                | VillageBoy -> (2, 0)
                                | VillageGirl -> (3, 0)
                                | FireGoblinNpc -> (0, 4)
                            let column = column + CharacterAnimationState.directionToInt direction
                            let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterSize
                            let inset = v4Bounds insetPosition Constants.Gameplay.CharacterSize
                            (false, Some inset, image)
                        | _ -> (false, None, Assets.Default.EmptyImage)
                    | Shopkeep (shopkeepType, direction, _, _) ->
                        match prop.PropState with
                        | ShopkeepState true ->
                            let image = Assets.Field.ShopkeepAnimationSheet
                            let row = match shopkeepType with ShopkeepMan -> 0
                            let column = CharacterAnimationState.directionToInt direction
                            let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterSize
                            let inset = v4Bounds insetPosition Constants.Gameplay.CharacterSize
                            (false, Some inset, image)
                        | _ -> (false, None, Assets.Default.EmptyImage)
                    | SavePoint ->
                        let time = World.getTickTime world
                        let image = Assets.Field.SavePointImage
                        let column = (int time / 15) % 4
                        let insetPosition = v2 (single column) 0.0f * Constants.Gameplay.TileSize
                        let inset = v4Bounds insetPosition Constants.Gameplay.TileSize
                        (false, Some inset, image)
                    | ChestSpawn | EmptyProp ->
                        (false, None, Assets.Default.EmptyImage)
                let depth = if background then Constants.Field.BackgroundDepth else Constants.Field.ForegroundDepth
                let positionY = transform.Position.Y
                let assetTag = AssetTag.generalize image
                [Render (depth, positionY, assetTag,
                    SpriteDescriptor
                        { Transform = transform
                          Offset = Vector2.Zero
                          InsetOpt = insetOpt
                          Image = image
                          Color = Color.White
                          Glow = Color.Zero
                          Flip = FlipNone })]
            else []