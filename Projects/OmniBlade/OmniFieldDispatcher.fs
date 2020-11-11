// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module FieldDispatcher =

    type [<NoComparison; NoEquality>] FieldMessage =
        | UpdateAvatar of Avatar
        | UpdateDialog
        | UpdatePortal
        | UpdateSensor
        | UpdateFieldTransition
        | SubmenuLegionOpen
        | SubmenuLegionAlly of int
        | SubmenuItemOpen
        | SubmenuItemSelect of Lens<int * ItemType, World>
        | SubmenuItemUse of int
        | SubmenuItemCancel
        | SubmenuClose
        | ShopBuy
        | ShopSell
        | ShopPageUp
        | ShopPageDown
        | ShopSelect of Lens<int * ItemType, World>
        | ShopConfirmAccept
        | ShopConfirmDecline
        | ShopLeave
        | Traverse of Vector2
        | Interact

    type [<NoComparison>] FieldCommand =
        | UpdateEye
        | MoveAvatar of Vector2
        | PlayFieldSong
        | PlaySound of int64 * single * Sound AssetTag
        | PlaySong of int * single * Song AssetTag
        | FadeOutSong of int
        | Nop

    type Screen with
        member this.GetField = this.GetModel<Field>
        member this.SetField = this.SetModel<Field>
        member this.Field = this.Model<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.empty)

        static let pageItems pageIndex pageSize items =
            items |>
            Seq.chunkBySize pageSize |>
            Seq.trySkip pageIndex |>
            Seq.map List.ofArray |>
            Seq.tryHead |>
            Option.defaultValue []

        static let isFacingBodyShape bodyShape (avatar : Avatar) world =
            if bodyShape.Entity.Is<PropDispatcher> world then
                let v = bodyShape.Entity.GetBottom world - avatar.Bottom
                let direction = Direction.fromVector2 v
                direction = avatar.Direction
            else false

        static let getFacingBodyShapes (avatar : Avatar) world =
            List.filter
                (fun bodyShape -> isFacingBodyShape bodyShape avatar world)
                avatar.IntersectedBodyShapes

        static let tryGetFacingProp (avatar : Avatar) world =
            match getFacingBodyShapes avatar world with
            | head :: _ -> Some head.Entity
            | [] -> None

        static let isTouchingSavePoint (avatar : Avatar) world =
            List.exists (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | SavePoint -> true
                | _ -> false)
                avatar.IntersectedBodyShapes

        static let tryGetFacingInteraction dialogOpt advents prop =
            match dialogOpt with
            | None ->
                match prop with
                | Chest (_, _, chestId, _, _, _) ->
                    if Set.contains (Opened chestId) advents then None
                    else Some "Open"
                | Door _ -> Some "Open"
                | Portal (_, _, _, _, _) -> None
                | Switch (_, _, _) -> Some "Use"
                | Sensor (_, _, _, _) -> None
                | Npc _ -> Some "Talk"
                | Shopkeep _ -> Some "Shop"
                | SavePoint -> None
                | ChestSpawn -> None
                | EmptyProp -> None
            | Some dialog ->
                if dialog.DialogProgress > dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage].Length
                then Some "Next"
                else None

        static let tryGetInteraction dialogOpt advents (avatar : Avatar) world =
            if isTouchingSavePoint avatar world then
                Some "Save"
            else
                match tryGetFacingProp avatar world with
                | Some prop -> tryGetFacingInteraction dialogOpt advents (prop.GetProp world).PropData
                | None -> None

        static let tryGetTouchingPortal omniSeedState (avatar : Avatar) world =
            avatar.IntersectedBodyShapes |>
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Portal (_, _, fieldType, portalType, _) ->
                    match Map.tryFind fieldType Data.Value.Fields with
                    | Some fieldData ->
                        match FieldData.tryGetPortal omniSeedState portalType fieldData world with
                        | Some portal ->
                            match portal.PropData with
                            | Portal (_, direction, _, _, _) ->
                                let destinationCenter =
                                    match direction with
                                    | Upward -> portal.PropBounds.Top
                                    | Rightward -> portal.PropBounds.Right
                                    | Downward -> portal.PropBounds.Bottom
                                    | Leftward -> portal.PropBounds.Left
                                let destinationOffset = Direction.toVector2 direction * v2Dup Constants.Field.PortalOffset
                                let destination = destinationCenter + destinationOffset
                                Some (fieldType, destination, direction)
                            | _ -> None
                        | None -> None
                    | None -> None
                | _ -> None) |>
            List.tryHead

        static let getTouchedSensors (avatar : Avatar) world =
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Sensor (sensorType, _, requirements, consequents) -> Some (sensorType, requirements, consequents)
                | _ -> None)
                avatar.CollidedBodyShapes

        static let getUntouchedSensors (avatar : Avatar) world =
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Sensor (sensorType, _, requirements, consequents) -> Some (sensorType, requirements, consequents)
                | _ -> None)
                avatar.SeparatedBodyShapes

        static let interactDialog dialog field =
            if dialog.DialogPage < dialog.DialogText.Split(Constants.Gameplay.DialogSplit).Length - 1 then
                let dialog = { dialog with DialogProgress = 0; DialogPage = inc dialog.DialogPage }
                let field = Field.updateDialogOpt (constant (Some dialog)) field
                just field
            else just (Field.updateDialogOpt (constant None) field)

        static let interactChest time itemType chestId battleTypeOpt requirements consequents (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                match battleTypeOpt with
                | Some battleType ->
                    match Map.tryFind battleType Data.Value.Battles with
                    | Some battleData ->
                        let prizePool = { Items = []; Gold = 0; Exp = 0 }
                        let battle = Battle.makeFromLegion (Field.getParty field) field.Inventory prizePool battleData time
                        let field = Field.updateBattleOpt (constant (Some battle)) field
                        withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.BeastScreamSound)) field
                    | None -> just field
                | None ->
                    let field = Field.updateInventory (Inventory.addItem itemType) field
                    let field = Field.updateAdvents (Set.add (Opened chestId)) field
                    let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0 })) field
                    let field = Field.updateAdvents (Set.addMany consequents) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenChestSound)) field
            else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0 })) field)

        static let interactDoor requirements consequents (prop : Prop) (field : Field) =
            match prop.PropState with
            | DoorState false ->
                if field.Advents.IsSupersetOf requirements then
                    let field = Field.updateAdvents (Set.addMany consequents) field
                    let field = Field.updatePropStates (Map.add prop.PropId (DoorState true)) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenDoorSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0 })) field)
            | _ -> failwithumf ()

        static let interactSwitch requirements consequents (prop : Prop) (field : Field) =
            match prop.PropState with
            | SwitchState on ->
                if field.Advents.IsSupersetOf requirements then
                    let field = Field.updateAdvents (if on then Set.removeMany consequents else Set.addMany consequents) field
                    let field = Field.updatePropStates (Map.add prop.PropId (SwitchState (not on))) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.UseSwitchSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Won't budge!"; DialogProgress = 0; DialogPage = 0 })) field)
            | _ -> failwithumf ()

        static let interactNpc dialogs (field : Field) =
            let dialogs = dialogs |> List.choose (fun (dialog, requirements, consequents) -> if field.Advents.IsSupersetOf requirements then Some (dialog, consequents) else None) |> List.rev
            let (dialog, consequents) = match List.tryHead dialogs with Some dialog -> dialog | None -> ("...", Set.empty)
            let dialogForm = { DialogForm = DialogLarge; DialogText = dialog; DialogProgress = 0; DialogPage = 0 }
            let field = Field.updateDialogOpt (constant (Some dialogForm)) field
            let field = Field.updateAdvents (Set.addMany consequents) field
            just field

        static let interactShopkeep shopType (field : Field) =
            let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
            let field = Field.updateShopOpt (constant (Some shop)) field
            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)) field

        static let interactSavePoint (field : Field) =
            let field = Field.restoreLegion field
            Field.save field
            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.SaveSound)) field

        static let sidebar position depth (field : Lens<Field, World>) =
            Content.group Gen.name []
                [Content.button Gen.name
                   [Entity.PositionLocal == position; Entity.DepthLocal == depth; Entity.Size == v2 64.0f 64.0f
                    Entity.Text == "L"
                    Entity.EnabledLocal <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuLegion _ -> false | _ -> true
                    Entity.ClickEvent ==> msg SubmenuLegionOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 72.0f; Entity.DepthLocal == depth; Entity.Size == v2 64.0f 64.0f
                    Entity.Text == "I"
                    Entity.EnabledLocal <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuItem _ -> false | _ -> true
                    Entity.ClickEvent ==> msg SubmenuItemOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 144.0f; Entity.DepthLocal == depth; Entity.Size == v2 64.0f 64.0f
                    Entity.Text == "X"
                    Entity.ClickEvent ==> msg SubmenuClose]]

        static let legion (position : Vector2) depth rows (field : Lens<Field, World>) filter fieldMsg =
            Content.entities field
                (fun field -> (field.Legion, field.Submenu))
                (fun (legion, submenu) _ -> legion |> Map.toValueList |> List.filter (flip filter submenu))
                (fun i legionnaireLens world ->
                    let legionnaire = Lens.get legionnaireLens world
                    let x = position.X
                    let y = position.Y - single (i % rows) * 72.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.DepthLocal == depth
                         Entity.Text == CharacterType.getName legionnaire.CharacterType
                         Entity.ClickEvent ==> msg (fieldMsg i)])

        static let items (position : Vector2) depth field fieldMsg =
            Content.entities field
                (fun (field : Field) -> (field.Submenu, field.ShopOpt, field.Inventory))
                (fun (submenu, shopOpt, inventory : Inventory) _ ->
                    let items =
                        match submenu.SubmenuState with
                        | SubmenuItem submenu -> pageItems submenu.ItemPage 10 (Inventory.indexItems inventory)
                        | _ ->
                            match shopOpt with
                            | Some shop ->
                                match shop.ShopState with
                                | ShopBuying ->
                                    match Map.tryFind shop.ShopType Data.Value.Shops with
                                    | Some shopData -> shopData.ShopItems |> Set.toSeq |> Seq.indexed |> pageItems shop.ShopPage 10
                                    | None -> []
                                | ShopSelling ->
                                    inventory |>
                                    Inventory.indexItems |>
                                    Seq.choose (function (_, Equipment _ as item) | (_, Consumable _ as item) -> Some item | (_, KeyItem _) | (_, Stash _) -> None) |>
                                    pageItems shop.ShopPage 10
                            | None -> []
                    let itemsSorted = List.sortBy snd items
                    itemsSorted)
                (fun i selectionLens _ ->
                    let x = if i < 5 then position.X else position.X + 368.0f
                    let y = position.Y - single (i % 5) * 72.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.DepthLocal == depth; Entity.Size == v2 336.0f 64.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v2 16.0f 0.0f
                         Entity.Text <== selectionLens --> fun (_, itemType) -> ItemType.getName itemType
                         Entity.EnabledLocal <== selectionLens --> fun (_, itemType) -> match itemType with Consumable _ | Equipment _ -> true | KeyItem _ | Stash _ -> false
                         Entity.ClickEvent ==> msg (fieldMsg selectionLens)])

        override this.Channel (_, field) =
            [field.SelectEvent => cmd PlayFieldSong
             Simulants.FieldAvatar.Avatar.ChangeEvent =|> fun evt -> msg (UpdateAvatar (evt.Data.Value :?> Avatar))
             field.UpdateEvent => msg UpdateDialog
             field.UpdateEvent => msg UpdatePortal
             field.UpdateEvent => msg UpdateSensor
             Simulants.FieldInteract.ClickEvent => msg Interact
             field.PostUpdateEvent => msg UpdateFieldTransition
             field.PostUpdateEvent => cmd UpdateEye]

        override this.Message (field, message, _, world) =

            match message with
            | UpdateAvatar avatar ->
                let field = Field.updateAvatar (constant avatar) field
                just field

            | UpdateDialog ->
                match field.DialogOpt with
                | Some dialog ->
                    let increment = if World.getTickTime world % 2L = 0L then 1 else 0
                    let dialog = { dialog with DialogProgress = dialog.DialogProgress + increment }
                    let field = Field.updateDialogOpt (constant (Some dialog)) field
                    just field
                | None -> just field

            | UpdateFieldTransition ->

                // check if transitioning
                match field.FieldTransitionOpt with
                | Some fieldTransition ->

                    // handle field transition
                    let tickTime = World.getTickTime world
                    let currentSongOpt = world |> World.getCurrentSongOpt |> Option.map (fun song -> song.Song)
                    let (signals, field) =

                        // start transition
                        if tickTime = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime then
                            match Data.Value.Fields.TryGetValue fieldTransition.FieldType with
                            | (true, fieldData) ->
                                if currentSongOpt <> fieldData.FieldSongOpt
                                then withCmd (FadeOutSong Constants.Audio.DefaultFadeOutMs) field
                                else just field
                            | (false, _) -> just field
                        
                        // half-way point of transition
                        elif tickTime = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L then
                            let field = Field.updateFieldType (constant fieldTransition.FieldType) field
                            let field =
                                Field.updateAvatar (fun avatar ->
                                    let avatar = Avatar.updateDirection (constant fieldTransition.FieldDirection) avatar
                                    let avatar = Avatar.updateIntersectedBodyShapes (constant []) avatar
                                    avatar)
                                    field
                            let moveCmd = MoveAvatar fieldTransition.FieldDestination
                            let songCmd =
                                match Field.getFieldSongOpt field with
                                | Some fieldSong ->
                                    if currentSongOpt <> Some fieldSong
                                    then PlaySong (Constants.Audio.DefaultFadeOutMs, Constants.Audio.DefaultSongVolume, fieldSong)
                                    else Nop
                                | None -> Nop
                            withCmds [moveCmd; songCmd] field

                        // finish transition
                        elif tickTime = fieldTransition.FieldTransitionTime then
                            let field = Field.updateFieldTransitionOpt (constant None) field
                            just field

                        // intermediate state
                        else just field

                    // update field reference to make sure transition binding actuates
                    let field = Field.updateReference field
                    (signals, field)

                // no transition
                | None -> just field

            | UpdatePortal ->
                match field.FieldTransitionOpt with
                | None ->
                    match tryGetTouchingPortal field.OmniSeedState field.Avatar world with
                    | Some (fieldType, destination, direction) ->
                        let transition =
                            { FieldType = fieldType
                              FieldDestination = destination
                              FieldDirection = direction
                              FieldTransitionTime = World.getTickTime world + Constants.Field.TransitionTime }
                        let field = Field.updateFieldTransitionOpt (constant (Some transition)) field
                        withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.StairStepsSound)) field
                    | None -> just field
                | Some _ -> just field

            | UpdateSensor ->
                match field.FieldTransitionOpt with
                | None ->
                    let sensors = getTouchedSensors field.Avatar world
                    let results =
                        List.fold (fun (cmds : Signal<FieldMessage, FieldCommand> list, field : Field) (sensorType, requirements, consequents) ->
                            if field.Advents.IsSupersetOf requirements then
                                let field = Field.updateAdvents (constant (Set.union consequents field.Advents)) field
                                match sensorType with
                                | AirSensor -> (cmds, field)
                                | HiddenSensor | StepPlateSensor -> (Command (PlaySound (0L,  Constants.Audio.DefaultSoundVolume, Assets.TriggerSound)) :: cmds, field)
                            else (cmds, field))
                            ([], field) sensors
                    results
                | Some _ -> just field

            | SubmenuLegionOpen ->
                let state = SubmenuLegion { LegionIndex = 0; LegionIndices = Map.toKeyList field.Legion }
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuState = state }) field
                just field

            | SubmenuLegionAlly index ->
                let field =
                    Field.updateSubmenu (fun submenu ->
                        let state =
                            match submenu.SubmenuState with
                            | SubmenuLegion submenuLegion -> SubmenuLegion { submenuLegion with LegionIndex = index }
                            | state -> state
                        { submenu with SubmenuState = state })
                        field
                just field

            | SubmenuItemOpen ->
                let itemState = SubmenuItem { ItemPage = 0 }
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuState = itemState }) field
                just field

            | SubmenuItemSelect selectionLens ->
                let selection = Lens.get selectionLens world
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuUseOpt = SubmenuUse.tryMakeFromSelection selection }) field
                just field

            | SubmenuItemUse index ->
                match Map.tryFind index field.Legion with
                | Some legionnaire ->
                    match field.Submenu.SubmenuUseOpt with
                    | Some submenuUse ->
                        let itemType = snd submenuUse.SubmenuUseSelection
                        let (result, displacedOpt, legionnaire) = Legionnaire.tryUseItem itemType legionnaire
                        let field = if result then Field.updateInventory (Inventory.removeItem itemType) field else field
                        let field = match displacedOpt with Some displaced -> Field.updateInventory (Inventory.addItem displaced) field | None -> field
                        let field = Field.updateLegion (Map.add index legionnaire) field
                        let field = Field.updateSubmenu (constant { field.Submenu with SubmenuUseOpt = None }) field
                        if result then withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.PurchaseSound)) field
                        else just field
                    | None -> just field
                | None -> just field

            | SubmenuItemCancel ->
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuUseOpt = None }) field
                just field

            | SubmenuClose ->
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuState = SubmenuClosed }) field
                just field

            | ShopBuy ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopBuying; ShopPage = 0 })) field
                just field

            | ShopSell ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopSelling; ShopPage = 0 })) field
                just field

            | ShopPageUp ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = max 0 (dec shop.ShopPage) })) field
                just field

            | ShopPageDown ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = inc shop.ShopPage })) field
                just field

            | ShopSelect selectionLens ->
                let selection = Lens.get selectionLens world
                let field =
                    Field.updateShopOpt (Option.map (fun shop ->
                        let buying = match shop.ShopState with ShopBuying -> true | ShopSelling -> false
                        let shopConfirmOpt = ShopConfirm.tryMakeFromSelection buying field.Inventory selection
                        { shop with ShopConfirmOpt = shopConfirmOpt }))
                        field
                just field

            | ShopConfirmAccept ->
                match field.ShopOpt with
                | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        let valid = match shop.ShopState with ShopBuying -> field.Inventory.Gold >= shopConfirm.ShopConfirmPrice | ShopSelling -> true
                        if valid then
                            let itemType = snd shopConfirm.ShopConfirmSelection
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.addItem itemType | ShopSelling -> Inventory.removeItem itemType) field
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.updateGold (fun gold -> gold - shopConfirm.ShopConfirmPrice) | ShopSelling -> Inventory.updateGold (fun gold -> gold + shopConfirm.ShopConfirmPrice)) field
                            let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.PurchaseSound)) field
                        else withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.MistakeSound)) field
                    | None -> just field
                | None -> just field

            | ShopConfirmDecline ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                just field

            | ShopLeave ->
                let field = Field.updateShopOpt (constant None) field
                just field

            | Traverse velocity ->
                match Field.tryAdvanceEncounterCreep velocity field world with
                | (Some battleData, field) ->
                    let prizePool = { Items = []; Gold = 0; Exp = 0 }
                    let battle = Battle.makeFromLegion field.Legion field.Inventory prizePool battleData (World.getTickTime world)
                    let field = Field.updateBattleOpt (constant (Some battle)) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.BeastScreamSound)) field
                | (None, field) -> just field

            | Interact ->
                match field.DialogOpt with
                | None ->
                    if isTouchingSavePoint field.Avatar world then
                        interactSavePoint field
                    else
                        match tryGetFacingProp field.Avatar world with
                        | Some prop ->
                            let prop = prop.GetProp world
                            match prop.PropData with
                            | Chest (_, itemType, chestId, battleTypeOpt, requirements, consequents) -> interactChest (World.getTickTime world) itemType chestId battleTypeOpt requirements consequents field
                            | Door (_, requirements, consequents) -> interactDoor requirements consequents prop field
                            | Portal (_, _, _, _, _) -> just field
                            | Switch (_, requirements, consequents) -> interactSwitch requirements consequents prop field
                            | Sensor (_, _, _, _) -> just field
                            | Npc (_, _, dialogs, _) -> interactNpc dialogs field
                            | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType field
                            | SavePoint -> just field
                            | ChestSpawn -> just field
                            | EmptyProp -> just field
                        | None -> just field
                | Some dialog ->
                    interactDialog dialog field

        override this.Command (field, command, _, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyeCenter field.Avatar.Center world
                just world

            | MoveAvatar position ->
                let positionOffset = position - Constants.Field.AvatarBottomInset
                let world = Simulants.FieldAvatar.SetBottom positionOffset world
                just world

            | PlayFieldSong ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match fieldData.FieldSongOpt with
                    | Some fieldSong -> withCmd (PlaySong (Constants.Audio.DefaultFadeOutMs, Constants.Audio.DefaultSongVolume, fieldSong)) world
                    | None -> just world
                | (false, _) -> just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | PlaySong (fade, volume, assetTag) ->
                let world = World.playSong fade volume assetTag world
                just world

            | FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

            | Nop -> just world

        override this.Content (field, _) =

            [// main layer
             Content.layer Simulants.FieldScene.Name []

                [// backdrop sprite
                 Content.staticSprite Simulants.FieldBackdrop.Name
                    [Entity.Bounds <== field --|> (fun _ world -> World.getViewBoundsAbsolute world); Entity.Depth == Single.MinValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.DefaultImage9
                     Entity.Color <== field --> fun field ->
                        match Data.Value.Fields.TryGetValue field.FieldType with
                        | (true, fieldData) -> fieldData.FieldBackgroundColor
                        | (false, _) -> Color.Black]

                 // transition fade sprite
                 Content.staticSprite Simulants.FieldTransitionFade.Name
                   [Entity.Bounds <== field --|> (fun _ world -> World.getViewBoundsAbsolute world); Entity.Depth == Single.MaxValue; Entity.Absolute == true
                    Entity.StaticImage == Assets.DefaultImage9
                    Entity.Color <== field --|> fun field world ->
                        match field.FieldTransitionOpt with
                        | Some transition ->
                            let tickTime = World.getTickTime world
                            let deltaTime = single transition.FieldTransitionTime - single tickTime
                            let halfTransitionTime = single Constants.Field.TransitionTime * 0.5f
                            let progress =
                                if deltaTime < halfTransitionTime
                                then deltaTime / halfTransitionTime
                                else 1.0f - (deltaTime - halfTransitionTime) / halfTransitionTime
                            Color.Black.WithA (byte (progress * 255.0f))
                        | None -> Color.Zero]

                 // tmx map
                 Content.tmxMap Simulants.FieldTileMap.Name
                    [Entity.Depth == Constants.Field.BackgroundDepth
                     Entity.TmxMap <== field --|> fun field world ->
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetTileMap field.OmniSeedState fieldData world with
                            | Some tileMap -> tileMap
                            | None -> failwithumf ()
                        | None -> failwithumf ()
                     Entity.TileLayerClearance == 10.0f]

                 // avatar
                 Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                    [Entity.Position == v2 256.0f 256.0f; Entity.Depth == Constants.Field.ForgroundDepth; Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Enabled <== field --> fun field ->
                        field.Submenu.SubmenuState = SubmenuClosed &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.Avatar <== field --> fun field -> field.Avatar
                     Entity.TraverseEvent ==|> fun evt -> msg (Traverse evt.Data)]

                 // submenu button
                 Content.button Simulants.FieldSubmenu.Name
                    [Entity.Position == v2 -444.0f -240.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 192.0f 64.0f
                     Entity.UpImage == Assets.ButtonShortUpImage; Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Text == "Submenu"
                     Entity.Visible <== field --> fun field ->
                        field.Submenu.SubmenuState = SubmenuClosed &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.ClickEvent ==> msg SubmenuLegionOpen]

                 // interact button
                 Content.button Simulants.FieldInteract.Name
                    [Entity.Position == v2 248.0f -240.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 192.0f 64.0f
                     Entity.UpImage == Assets.ButtonShortUpImage; Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Visible <== field --|> fun field world ->
                        field.Submenu.SubmenuState = SubmenuClosed &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Option.isSome (tryGetInteraction field.DialogOpt field.Advents field.Avatar world)
                     Entity.Text <== field --|> fun field world ->
                        match tryGetInteraction field.DialogOpt field.Advents field.Avatar world with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None]

                 // dialog
                 Content.text Simulants.FieldDialog.Name
                    [Entity.Bounds <== field --> fun field ->
                        match field.DialogOpt with
                        | Some dialog ->
                            match dialog.DialogForm with
                            | DialogThin -> v4Bounds (v2 -448.0f 128.0f) (v2 896.0f 112.0f)
                            | DialogMedium -> v4Bounds (v2 -448.0f 0.0f) (v2 640.0f 256.0f)
                            | DialogLarge -> v4Bounds (v2 -448.0f 0.0f) (v2 896.0f 256.0f)
                        | None -> v4Zero
                     Entity.BackgroundImageOpt <== field --> fun field ->
                        let image =
                            match field.DialogOpt with
                            | Some dialog ->
                                match dialog.DialogForm with
                                | DialogThin -> Assets.DialogThinImage
                                | DialogMedium -> Assets.DialogMediumImage
                                | DialogLarge -> Assets.DialogLargeImage
                            | None -> Assets.DialogLargeImage
                        Some image
                     Entity.Text <== field --> fun field ->
                        match field.DialogOpt with
                        | Some dialog ->
                            let textPage = dialog.DialogPage
                            let text = dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[textPage]
                            let textToShow = String.tryTake dialog.DialogProgress text
                            textToShow
                        | None -> ""
                     Entity.Visible <== field --> fun field -> Option.isSome field.DialogOpt
                     Entity.Justification == Unjustified true
                     Entity.Margins == v2 40.0f 40.0f]

                 // props
                 Content.entities field
                    (fun field -> (field.FieldType, field.OmniSeedState, field.Advents, field.PropStates))
                    (fun (fieldType, rotatedSeedState, advents, propStates) world ->
                        match Map.tryFind fieldType Data.Value.Fields with
                        | Some fieldData ->
                            let props = FieldData.getProps rotatedSeedState fieldData world
                            List.map (fun prop -> (prop, advents, propStates)) props
                        | None -> [])
                    (fun _ propLens _ ->
                        let prop = flip Lens.map propLens (fun (prop, advents, propStates) ->
                            let propState =
                                match Map.tryFind prop.PropId propStates with
                                | None ->
                                    match prop.PropData with
                                    | Door (_, _, _) -> DoorState false
                                    | Switch (_, _, _) -> SwitchState false
                                    | Npc (_, _, _, requirements) -> NpcState (advents.IsSupersetOf requirements)
                                    | Shopkeep (_, _, _, requirements) -> ShopkeepState (advents.IsSupersetOf requirements)
                                    | _ -> NilState
                                | Some propState -> propState
                            Prop.make prop.PropBounds prop.PropDepth advents prop.PropData propState prop.PropId)
                        Content.entity<PropDispatcher> Gen.name [Entity.Prop <== prop])

                 // legion
                 Content.panel Simulants.SubmenuLegion.Name
                    [Entity.Position == v2 -448.0f -256.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 896.0f 512.0f
                     Entity.LabelImage == Assets.DialogXXLImage
                     Entity.Visible <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuLegion _ -> true | _ -> false]
                    [sidebar (v2 40.0f 424.0f) 1.0f field
                     legion (v2 136.0f 424.0f) 1.0f Int32.MaxValue field tautology2 SubmenuLegionAlly
                     Content.label Gen.name
                        [Entity.PositionLocal == v2 424.0f 288.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 192.0f 192.0f
                         Entity.LabelImage <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionData field.Legion submenu with
                                | Some characterData ->
                                    match characterData.MugOpt with
                                    | Some mug -> mug
                                    | None -> Assets.EmptyImage
                                | None -> Assets.EmptyImage
                            | _ -> Assets.EmptyImage]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 672.0f 384.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionData field.Legion submenu with
                                | Some characterData -> CharacterType.getName characterData.CharacterType
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 672.0f 328.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionnaire field.Legion submenu with
                                | Some legionnaire -> "Level " + string (Algorithms.expPointsToLevel legionnaire.ExpPoints)
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 448.0f 224.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionnaire field.Legion submenu with
                                | Some legionnaire -> "W: " + Option.getOrDefault "None" legionnaire.WeaponOpt
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 448.0f 184.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionnaire field.Legion submenu with
                                | Some legionnaire -> "A: " + Option.getOrDefault "None" legionnaire.ArmorOpt
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 448.0f 144.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionnaire field.Legion submenu with
                                | Some legionnaire -> "1: " + Option.getOrDefault "None" (List.tryHead legionnaire.Accessories)
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 448.0f -104.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 512.0f 256.0f
                         Entity.Justification == Unjustified true
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuLegion submenu ->
                                match SubmenuLegion.tryGetLegionnaireAndLegionData field.Legion submenu with
                                | Some (legionnaire, characterData) ->
                                    let level = Algorithms.expPointsToLevel legionnaire.ExpPoints
                                    let hpm = Algorithms.hitPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                                    let tpm = Algorithms.techPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                                    let pow = Algorithms.power legionnaire.WeaponOpt 1.0f characterData.ArchetypeType level
                                    let mag = Algorithms.magic legionnaire.WeaponOpt 1.0f characterData.ArchetypeType level
                                    "HP  "   + (string legionnaire.HitPoints).PadLeft 3 + "/" + (string hpm).PadLeft 3 +
                                    "\nTP  " + (string legionnaire.TechPoints).PadLeft 3 + "/" + (string tpm).PadLeft 3 +
                                    "\nPow " + (string pow).PadLeft 3 + "   Mag " + (string mag).PadLeft 3 +
                                    "\nExp " + (string legionnaire.ExpPoints).PadLeft 3 + "/" + (string (Legionnaire.getExpPointsForNextLevel legionnaire)).PadLeft 3
                                | None -> ""
                            | _ -> ""]]

                 // item
                 Content.panel Simulants.SubmenuItem.Name
                    [Entity.Position == v2 -448.0f -256.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 896.0f 512.0f
                     Entity.LabelImage == Assets.DialogXXLImage
                     Entity.Visible <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuItem _ -> true | _ -> false
                     Entity.Enabled <== field --> fun field -> Option.isNone field.Submenu.SubmenuUseOpt]
                    [sidebar (v2 40.0f 424.0f) 1.0f field
                     items (v2 136.0f 424.0f) 1.0f field SubmenuItemSelect
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 316.0f 12.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // use
                 Content.panel Simulants.SubmenuUse.Name
                    [Entity.Position == v2 -448.0f -192.0f; Entity.Depth == Constants.Field.GuiDepth + 10.0f; Entity.Size == v2 896.0f 384.0f
                     Entity.LabelImage == Assets.DialogXLImage
                     Entity.Visible <== field --> fun field -> Option.isSome field.Submenu.SubmenuUseOpt]
                    [legion (v2 160.0f 192.0f) 1.0f 3 field
                        (fun legionnaire submenu ->
                            match submenu.SubmenuUseOpt with
                            | Some submenuUse -> Legionnaire.canUseItem (snd submenuUse.SubmenuUseSelection) legionnaire
                            | None -> false)
                        SubmenuItemUse
                     Content.button Gen.name
                        [Entity.PositionLocal == v2 820.0f 312.0f; Entity.DepthLocal == 2.0f; Entity.Size == v2 64.0f 64.0f
                         Entity.Text == "X"
                         Entity.ClickEvent ==> msg SubmenuItemCancel]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 32.0f 304.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuUseOpt with
                            | Some submenu -> submenu.SubmenuUseLine1
                            | None -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 64.0f 256.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                             match field.Submenu.SubmenuUseOpt with
                             | Some submenu -> submenu.SubmenuUseLine2
                             | None -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 64.0f 208.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmLine2
                                | None -> ""
                            | None -> ""]]

                 // shop
                 Content.panel Simulants.FieldShop.Name
                    [Entity.Position == v2 -448.0f -256.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 896.0f 512.0f
                     Entity.LabelImage == Assets.DialogXXLImage
                     Entity.Visible <== field --> fun field -> Option.isSome field.ShopOpt
                     Entity.Enabled <== field --> fun field -> match field.ShopOpt with Some shop -> Option.isNone shop.ShopConfirmOpt | None -> true]
                    [items (v2 96.0f 368.0f) 1.0f field ShopSelect
                     Content.button Simulants.FieldShopBuy.Name
                        [Entity.PositionLocal == v2 12.0f 440.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Buy"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false
                         Entity.ClickEvent ==> msg ShopBuy]
                     Content.button Simulants.FieldShopSell.Name
                        [Entity.PositionLocal == v2 320.0f 440.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Sell"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false
                         Entity.ClickEvent ==> msg ShopSell]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 12.0f 440.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Buy what?"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 320.0f 440.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Sell what?"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false]
                     Content.button Simulants.FieldShopLeave.Name
                        [Entity.PositionLocal == v2 628.0f 440.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Leave"
                         Entity.ClickEvent ==> msg ShopLeave]
                     Content.button Simulants.FieldShopPageUp.Name
                        [Entity.PositionLocal == v2 16.0f 12.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                         Entity.Text == "<"
                         Entity.ClickEvent ==> msg ShopPageUp]
                     Content.button Simulants.FieldShopPageDown.Name
                        [Entity.PositionLocal == v2 832.0f 12.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                         Entity.Text == ">"
                         Entity.ClickEvent ==> msg ShopPageDown]
                     Content.text Simulants.FieldShopGold.Name
                        [Entity.PositionLocal == v2 316.0f 12.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // shop confirm
                 Content.panel Simulants.FieldShopConfirm.Name
                    [Entity.Position == v2 -448.0f -128.0f; Entity.Depth == Constants.Field.GuiDepth + 10.0f; Entity.Size == v2 896.0f 256.0f
                     Entity.LabelImage == Assets.DialogLargeImage
                     Entity.Visible <== field --> fun field ->
                        match field.ShopOpt with
                        | Some shop -> Option.isSome shop.ShopConfirmOpt
                        | None -> false]
                    [Content.button Simulants.FieldShopConfirmAccept.Name
                        [Entity.PositionLocal == v2 160.0f 16.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Accept"
                         Entity.ClickEvent ==> msg ShopConfirmAccept]
                     Content.button Simulants.FieldShopConfirmDecline.Name
                        [Entity.PositionLocal == v2 456.0f 16.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Decline"
                         Entity.ClickEvent ==> msg ShopConfirmDecline]
                     Content.text Simulants.FieldShopConfirmOffer.Name
                        [Entity.PositionLocal == v2 32.0f 176.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmOffer
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine1.Name
                        [Entity.PositionLocal == v2 64.0f 128.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmLine1
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine2.Name
                        [Entity.PositionLocal == v2 64.0f 80.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmLine2
                                | None -> ""
                            | None -> ""]]]]