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

    type Entity with
        member this.GetProp = this.GetModel<Prop>
        member this.SetProp = this.SetModel<Prop>
        member this.Prop = this.Model<Prop> ()

    type PropDispatcher () =
        inherit EntityDispatcher<Prop, unit, unit> (Prop.empty)

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.FixedRotation true
             define Entity.GravityScale 0.0f]

        override this.Initializers (prop, entity) =
            [entity.BodyType == Static
             entity.LinearDamping == 0.0f
             entity.GravityScale == 0.0f
             entity.Bounds <== prop --> fun prop -> prop.Bounds
             entity.IsSensor <== prop --> fun prop ->
                match prop.PropData with
                | Portal _ | Sensor _ | SavePoint -> true
                | _ -> false
             entity.BodyShape <== prop --> fun prop ->
                match prop.PropData with
                | Portal _ | Switch _ | SavePoint _ ->
                    BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Door _ ->
                    match prop.PropState with
                    | DoorState true -> BodyEmpty
                    | _ -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Chest _ ->
                    BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Sensor (_, shapeOpt, _, _, _) ->
                    match shapeOpt with
                    | Some shape -> shape
                    | None -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Seal _ ->
                    match prop.PropState with
                    | SealState false -> BodyEmpty
                    | _ -> BodyBox { Extent = v2 0.5f 0.5f; Center = v2Zero; PropertiesOpt = None }
                | Npc _ | NpcBranching _ ->
                    match prop.PropState with
                    | NpcState (_, _, _, _, true) -> BodyBox { Extent = v2 0.16f 0.16f; Center = v2 -0.01f -0.36f; PropertiesOpt = None }
                    | _ -> BodyEmpty
                | Shopkeep _ ->
                    match prop.PropState with
                    | ShopkeepState true -> BodyBox { Extent = v2 0.16f 0.16f; Center = v2 -0.01f -0.36f; PropertiesOpt = None }
                    | _ -> BodyEmpty
                | Flame _ | ChestSpawn | EmptyProp ->
                    BodyEmpty]

        override this.Physics (position, _, _, _, prop, _, _) =
            let prop = Prop.updatePosition (constant position) prop
            just prop

        override this.View (prop, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                let (background, color, glow, insetOpt, image) =
                    match prop.PropData with
                    | Portal (portalType, _, direction, _, _, _, _) ->
                        match prop.PropState with
                        | PortalState active ->
                            if active then
                                match portalType with
                                | AirPortal -> (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                                | StairsPortal descending ->
                                    let offsetY = if descending then Constants.Gameplay.TileSize.Y else 0.0f
                                    let offsetX =
                                        match direction with
                                        | Upward -> 0.0f
                                        | Rightward -> Constants.Gameplay.TileSize.X
                                        | Downward -> Constants.Gameplay.TileSize.X * 2.0f
                                        | Leftward -> Constants.Gameplay.TileSize.X * 3.0f
                                    let offset = v2 offsetX offsetY
                                    (true, colWhite, colZero, Some (v4Bounds offset Constants.Gameplay.TileSize), Assets.Field.StairsImage)
                            else (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                        | _ -> (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                    | Door (doorType, _, _, _) ->
                        let image =
                            match doorType with
                            | WoodenDoor ->
                                match prop.PropState with
                                | DoorState opened -> if opened then Assets.Field.WoodenDoorOpenedImage else Assets.Field.WoodenDoorClosedImage
                                | _ -> failwithumf ()
                        (false, colWhite, colZero, None, image)
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
                        (false, colWhite, colZero, None, image)
                    | Switch (switchType, _, _, _) ->
                        let image =
                            match switchType with
                            | ThrowSwitch ->
                                match prop.PropState with
                                | SwitchState on -> if on then Assets.Field.ThrowSwitchOnImage else Assets.Field.ThrowSwitchOffImage
                                | _ -> failwithumf ()
                        (false, colWhite, colZero, None, image)
                    | Sensor (sensorType, _, _, _, _) ->
                        match sensorType with
                        | AirSensor -> (true, colWhite, colZero, None, Assets.Default.ImageEmpty)
                        | HiddenSensor -> (true, colWhite, colZero, None, Assets.Default.ImageEmpty)
                        | StepPlateSensor -> (true, colWhite, colZero, None, Assets.Field.StepPlateImage)
                    | Seal (color, _, _) ->
                        match prop.PropState with
                        | SealState true ->
                            let time = World.getTickTime world
                            let localTime = time / 20L
                            let celSize = v2 96.0f 96.0f // TODO: P1: put this in Constants.
                            let celColumn = single (localTime % 4L)
                            let inset = v4Bounds (v2 (celSize.X * celColumn) 0.0f) celSize // TODO: P1: turn this into a general animation function if one doesn't already exist...
                            let image = Assets.Field.SealAnimationSheet
                            (false, color, colZero, Some inset, image)
                        | _ -> (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                    | Npc (_, _, _, _) | NpcBranching (_, _, _, _) ->
                        match prop.PropState with
                        | NpcState (npcType, direction, color, glow, true) ->
                            let image =
                                match npcType with
                                | GarrouNpc -> Assets.Field.GarrouAnimationSheet
                                | MaelNpc -> Assets.Field.MaelAnimationSheet
                                | RiainNpc -> Assets.Field.RiainAnimationSheet
                                | PericNpc -> Assets.Field.PericAnimationSheet
                                | _ -> Assets.Field.NpcAnimationSheet
                            let (row, column) =
                                match npcType with
                                | GarrouNpc
                                | MaelNpc
                                | RiainNpc
                                | PericNpc -> (10, 0)
                                | RavelNpc -> (0, 0)
                                | AdvenNpc -> (1, 0)
                                | EildaenNpc -> (2, 0)
                                | ShamanaNpc -> (3, 0)
                                | FireGoblinNpc -> (4, 0)
                            let column = column + CharacterAnimationState.directionToInt direction
                            let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterSize
                            let inset = v4Bounds insetPosition Constants.Gameplay.CharacterSize
                            (false, color, glow, Some inset, image)
                        | _ -> (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                    | Shopkeep (shopkeepType, direction, _, _) ->
                        match prop.PropState with
                        | ShopkeepState true ->
                            let image = Assets.Field.ShopkeepAnimationSheet
                            let row = match shopkeepType with RobehnShopkeep -> 0 | SchaalShopkeep -> 1
                            let column = CharacterAnimationState.directionToInt direction
                            let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterSize
                            let inset = v4Bounds insetPosition Constants.Gameplay.CharacterSize
                            (false, colWhite, colZero, Some inset, image)
                        | _ -> (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                    | Flame (flameType, mirror) ->
                        let image = Assets.Field.FlameImage
                        let column = match flameType with FatFlame -> 0 | SkinnyFlame -> 3 | SmallFlame -> 1 | LargeFlame -> 2
                        let row = if mirror then 4 else 0
                        let cel = int (World.getTickTime world / 10L % 4L) // TODO: P1: put this in Constants.
                        let inset =
                            v4 // TODO: P1: put the following hard-coded dimensions in Constants.
                                (single column * 48.0f) (single (row + cel) * 48.0f)
                                48.0f 48.0f
                        (false, colWhite, colZero, Some inset, image)
                    | SavePoint ->
                        let time = World.getTickTime world
                        let image = Assets.Field.SavePointImage
                        let column = (int time / 15) % 4
                        let insetPosition = v2 (single column) 0.0f * Constants.Gameplay.TileSize
                        let inset = v4Bounds insetPosition Constants.Gameplay.TileSize
                        (false, colWhite, colZero, Some inset, image)
                    | ChestSpawn | EmptyProp ->
                        (false, colWhite, colZero, None, Assets.Default.ImageEmpty)
                let elevation = if background then Constants.Field.BackgroundElevation else Constants.Field.ForegroundElevation
                let positionY = transform.Position.Y
                let assetTag = AssetTag.generalize image
                Render (elevation, positionY, assetTag,
                    SpriteDescriptor
                        { Transform = transform
                          Absolute = entity.GetAbsolute world
                          Offset = Vector2.Zero
                          InsetOpt = insetOpt
                          Image = image
                          Color = color
                          Blend = Transparent
                          Glow = glow
                          Flip = FlipNone })
            else View.empty