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
        member this.GetProp world = this.GetModelGeneric<Prop> world
        member this.SetProp value world = this.SetModelGeneric<Prop> value world
        member this.Prop = this.ModelGeneric<Prop> ()

    type PropDispatcher () =
        inherit EntityDispatcher2d<Prop, unit, unit> (true, false, Prop.empty)

        static member Facets =
            [typeof<RigidBody2dFacet>]

        override this.Initializers (prop, entity) =
            [entity.BodyType == Static
             entity.LinearDamping == 0.0f
             entity.FixedRotation == true
             entity.GravityScale == 0.0f
             entity.BodyType == Static
             entity.Perimeter <== prop --> fun prop -> prop.Perimeter
             entity.IsSensor <== prop --> fun prop ->
                match prop.PropData with
                | Portal _ | Sensor _ | SavePoint -> true
                | _ -> false
             entity.BodyShape <== prop --> fun prop ->
                match prop.PropData with
                | Sprite _ ->
                    BodyEmpty
                | Portal _ | Switch _ ->
                    BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None }
                | SavePoint _ ->
                    BodySphere { Radius = 0.1f; Center = v3Zero; PropertiesOpt = None }
                | Door _ ->
                    match prop.PropState with
                    | DoorState true -> BodyEmpty
                    | _ -> BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None }
                | Chest _ ->
                    BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None }
                | Sensor (_, shapeOpt, _, _, _) ->
                    match shapeOpt with
                    | Some shape -> shape
                    | None -> BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None }
                | Seal (_, _, requirements) ->
                    if prop.Advents.IsSupersetOf requirements
                    then BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None }
                    else BodyEmpty
                | Character (_, _, _, _, _, requirements) ->
                    if prop.Advents.IsSupersetOf requirements
                    then BodyBox { Extent = v3 0.16f 0.16f 0.0f; Center = v3 -0.01f -0.36f 0.0f; PropertiesOpt = None }
                    else BodyEmpty
                | Npc (npcType, _, _, requirements) | NpcBranching (npcType, _, _, requirements) ->
                    if prop.Advents.IsSupersetOf requirements && NpcType.exists prop.Advents npcType then
                        match npcType with
                        | ShadeNpc | MaelNpc | RiainNpc | PericNpc
                        | RavelNpc | AdvenNpc | EildaenNpc | NostrusNpc
                        | MadTrixterNpc | HeavyArmorosNpc -> BodyBox { Extent = v3 0.16f 0.16f 0.0f; Center = v3 -0.01f -0.36f 0.0f; PropertiesOpt = None }
                        | AraneaImplicitumNpc -> BodyBox { Extent = v3 0.16f 0.16f 0.0f; Center = v3 -0.01f -0.36f 0.0f; PropertiesOpt = None }
                    else BodyEmpty
                | Shopkeep (_, _, _, requirements) ->
                    if prop.Advents.IsSupersetOf requirements
                    then BodyBox { Extent = v3 0.16f 0.16f 0.0f; Center = v3 -0.01f -0.36f 0.0f; PropertiesOpt = None }
                    else BodyEmpty
                | Flame _ | ChestSpawn | EmptyProp ->
                    BodyEmpty]

        override this.View (prop, entity, world) =
            if entity.GetVisible world && entity.GetInView2d world then
                let mutable transform = entity.GetTransform world
                let (background, image, color, blend, glow, insetOpt, flip) =
                    match prop.PropData with
                    | Sprite _ ->
                        match prop.PropState with
                        | SpriteState (image, color, blend, glow, flip, visible) ->
                            let image = if visible then image else Assets.Default.ImageEmpty
                            (false, image, color, blend, glow, ValueNone, flip)
                        | _ ->
                            (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Portal (portalType, _, direction, _, _, _, requirements) ->
                        if prop.Advents.IsSupersetOf requirements then
                            match portalType with
                            | AirPortal ->
                                (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                            | WarpPortal ->
                                let time = World.getUpdateTime world
                                let localTime = time / 10L
                                let celSize = Constants.Gameplay.TileCelSize
                                let celColumn = single (localTime % 4L)
                                let inset = box2 (v2 (celSize.X * celColumn) 0.0f) celSize // TODO: P1: turn this into a general animation function if one doesn't already exist...
                                let image = Assets.Field.WarpAnimationSheet
                                (true, image, Color.One, Transparent, Color.Zero, ValueSome inset, FlipNone)
                            | StairsPortal descending ->
                                let offsetY = if descending then Constants.Gameplay.TileCelSize.Y else 0.0f
                                let offsetX =
                                    match direction with
                                    | Upward -> 0.0f
                                    | Rightward -> Constants.Gameplay.TileCelSize.X
                                    | Downward -> Constants.Gameplay.TileCelSize.X * 2.0f
                                    | Leftward -> Constants.Gameplay.TileCelSize.X * 3.0f
                                let offset = v2 offsetX offsetY
                                (true, Assets.Field.StairsImage, Color.One, Transparent, Color.Zero, ValueSome (box2 offset Constants.Gameplay.TileCelSize), FlipNone)
                        else (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Door (doorType, _, _, _, _) ->
                        let image =
                            match doorType with
                            | WoodenDoor ->
                                match prop.PropState with
                                | DoorState opened -> if opened then Assets.Field.WoodenDoorOpenedImage else Assets.Field.WoodenDoorClosedImage
                                | _ -> failwithumf ()
                        (false, image, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Chest (chestType, _, id, _, _, _) ->
                        let opened = prop.Advents.Contains (Opened id)
                        let image =
                            match chestType with
                            | WoodenChest ->
                                if opened
                                then Assets.Field.WoodenChestOpenedImage
                                else Assets.Field.WoodenChestClosedImage
                            | BrassChest ->
                                if opened
                                then Assets.Field.BrassChestOpenedImage
                                else Assets.Field.BrassChestClosedImage
                        (false, image, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Switch (switchType, _, _, _) ->
                        let image =
                            match switchType with
                            | ThrowSwitch ->
                                match prop.PropState with
                                | SwitchState on -> if on then Assets.Field.ThrowSwitchOnImage else Assets.Field.ThrowSwitchOffImage
                                | _ -> failwithumf ()
                        (false, image, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Sensor (sensorType, _, _, _, _) ->
                        match sensorType with
                        | AirSensor -> (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                        | HiddenSensor -> (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                        | StepPlateSensor -> (true, Assets.Field.StepPlateImage, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Seal (color, _, requirements) ->
                        if prop.Advents.IsSupersetOf requirements then
                            let time = World.getUpdateTime world
                            let localTime = time / 20L
                            let celSize = v2 32.0f 32.0f // TODO: P1: put this in Constants.
                            let celColumn = single (localTime % 4L)
                            let inset = box2 (v2 (celSize.X * celColumn) 0.0f) celSize // TODO: P1: turn this into a general animation function if one doesn't already exist...
                            let image = Assets.Field.SealAnimationSheet
                            (false, image, color, Transparent, Color.Zero, ValueSome inset, FlipNone)
                        else (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Character (_, _, isEcho, _, _, requirements) ->
                        match prop.PropState with
                        | CharacterState (color, animationState) when prop.Advents.IsSupersetOf requirements->
                            let time = World.getUpdateTime world
                            let inset = CharacterAnimationState.inset time Constants.Gameplay.CharacterCelSize animationState
                            let (color, glow) =
                                if isEcho then
                                    let color = color.MapA ((/) 2.5f)
                                    let glowAmount = single (time % 120L) / 120.0f
                                    let glowAmount = if glowAmount > 0.5f then 1.0f - glowAmount else glowAmount
                                    let glow = color.MapA ((*) glowAmount)
                                    (color, glow)
                                else (color, Color.Zero)
                            (false, animationState.AnimationSheet, color, Transparent, glow, ValueSome inset, FlipNone)
                        | _ -> (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Npc (npcType, directionOpt, _, requirements) | NpcBranching (npcType, directionOpt, _, requirements) ->
                        if prop.Advents.IsSupersetOf requirements && NpcType.exists prop.Advents npcType then
                            let (image, size) =
                                match npcType with
                                | ShadeNpc -> (Assets.Field.ShadeAnimationSheet, Constants.Gameplay.CharacterSize)
                                | MaelNpc -> (Assets.Field.MaelAnimationSheet, Constants.Gameplay.CharacterSize)
                                | RiainNpc -> (Assets.Field.RiainAnimationSheet, Constants.Gameplay.CharacterSize)
                                | PericNpc -> (Assets.Field.PericAnimationSheet, Constants.Gameplay.CharacterSize)
                                | RavelNpc | AdvenNpc | EildaenNpc | NostrusNpc | MadTrixterNpc | HeavyArmorosNpc -> (Assets.Field.NpcAnimationSheet, Constants.Gameplay.CharacterSize)
                                | AraneaImplicitumNpc -> (Assets.Field.BossAnimationSheet, Constants.Gameplay.BossSize)
                            let (row, column) =
                                match npcType with
                                | ShadeNpc
                                | MaelNpc
                                | RiainNpc
                                | PericNpc -> (10, 0)
                                | RavelNpc -> (0, 0)
                                | AdvenNpc -> (1, 0)
                                | EildaenNpc -> (2, 0)
                                | NostrusNpc -> (3, 0)
                                | MadTrixterNpc -> (4, 0)
                                | HeavyArmorosNpc -> (5, 0)
                                | AraneaImplicitumNpc -> (0, 0)
                            let direction =
                                match directionOpt with
                                | Some direction -> direction
                                | None ->
                                    let direction = Direction.ofVector3 (prop.PointOfInterest - prop.Bottom)
                                    if direction = Upward then Downward else direction
                            let column = column + CharacterAnimationState.directionToInt direction
                            let celSize = (size / 3.0f).V2
                            let insetPosition = v2 (single column) (single row) * celSize
                            let inset = box2 insetPosition celSize
                            (false, image, Color.One, Transparent, Color.Zero, ValueSome inset, FlipNone)
                        else (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Shopkeep (shopkeepType, directionOpt, _, requirements) ->
                        if prop.Advents.IsSupersetOf requirements then
                            let image = Assets.Field.ShopkeepAnimationSheet
                            let row = match shopkeepType with RobehnShopkeep -> 0 | SchaalShopkeep -> 1
                            let direction =
                                match directionOpt with
                                | Some direction -> direction
                                | None ->
                                    let direction = Direction.ofVector3 (prop.PointOfInterest - prop.Bottom)
                                    if direction = Upward then Downward else direction
                            let column = CharacterAnimationState.directionToInt direction
                            let insetPosition = v2 (single column) (single row) * Constants.Gameplay.CharacterCelSize
                            let inset = box2 insetPosition Constants.Gameplay.CharacterCelSize
                            (false, image, Color.One, Transparent, Color.Zero, ValueSome inset, FlipNone)
                        else (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                    | Flame (flameType, mirror) ->
                        let image = Assets.Field.FlameImage
                        let column = match flameType with FatFlame -> 0 | SkinnyFlame -> 3 | SmallFlame -> 1 | LargeFlame -> 2
                        let row = if mirror then 4 else 0
                        let cel = int (World.getUpdateTime world / 10L % 4L) // TODO: P1: put this in Constants.
                        let inset =
                            box2 // TODO: P1: put the following hard-coded values in Constants.
                                (v2 (single column * 16.0f) (single (row + cel) * 16.0f))
                                (v2 16.0f 16.0f)
                        (false, image, Color.One, Transparent, Color.Zero, ValueSome inset, FlipNone)
                    | SavePoint ->
                        let time = World.getUpdateTime world
                        let image = Assets.Field.SavePointImage
                        let column = (int time / 15) % 4
                        let insetPosition = v2 (single column) 0.0f * Constants.Gameplay.TileCelSize
                        let inset = box2 insetPosition Constants.Gameplay.TileCelSize
                        (false, image, Color.One, Additive, Color.Zero, ValueSome inset, FlipNone)
                    | ChestSpawn | EmptyProp ->
                        (false, Assets.Default.ImageEmpty, Color.One, Transparent, Color.Zero, ValueNone, FlipNone)
                let elevation = if background then Constants.Field.FlooringElevation else Constants.Field.ForegroundElevation
                let positionY = transform.Perimeter.Position.Y
                let assetTag = AssetTag.generalize image
                Render2d (elevation, positionY, assetTag,
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = insetOpt
                          Image = image
                          Color = color
                          Blend = blend
                          Glow = glow
                          Flip = flip })
            else View.empty