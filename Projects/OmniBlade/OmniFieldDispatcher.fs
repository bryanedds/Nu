// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
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
        | SubmenuTeamOpen
        | SubmenuTeamAlly of int
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
        | TryBattle of Advent Set * BattleType
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
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.debug)

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
                if Dialog.canAdvance dialog then Some "Next"
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
            match Dialog.tryAdvance dialog with
            | (true, dialog) ->
                let field = Field.updateDialogOpt (constant (Some dialog)) field
                just field
            | (false, dialog) ->
                let field = Field.updateDialogOpt (constant None) field
                match dialog.DialogBattleOpt with
                | Some (consequents, battleType) -> withMsg (TryBattle (consequents, battleType)) field
                | None -> just field

        static let interactChest itemType chestId battleTypeOpt requirements consequents (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateInventory (Inventory.addItem itemType) field
                let field = Field.updateAdvents (Set.add (Opened chestId)) field
                let field =
                    match battleTypeOpt with
                    | Some battleType -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Found " + ItemType.getName itemType + "!^But something approaches!"; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = Some (Set.empty, battleType) })) field
                    | None -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = None })) field
                let field = Field.updateAdvents (Set.addMany consequents) field
                withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenChestSound)) field
            else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = None })) field)

        static let interactDoor requirements consequents (prop : Prop) (field : Field) =
            match prop.PropState with
            | DoorState false ->
                if field.Advents.IsSupersetOf requirements then
                    let field = Field.updateAdvents (Set.addMany consequents) field
                    let field = Field.updatePropStates (Map.add prop.PropId (DoorState true)) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenDoorSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = None })) field)
            | _ -> failwithumf ()

        static let interactSwitch requirements consequents (prop : Prop) (field : Field) =
            match prop.PropState with
            | SwitchState on ->
                if field.Advents.IsSupersetOf requirements then
                    let field = Field.updateAdvents (if on then Set.removeMany consequents else Set.addMany consequents) field
                    let field = Field.updatePropStates (Map.add prop.PropId (SwitchState (not on))) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.UseSwitchSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Won't budge!"; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = None })) field)
            | _ -> failwithumf ()

        static let interactNpc specialty requirements dialogs (field : Field) =
            let battleTypeOpt = NpcSpecialty.getBattleTypeOpt requirements specialty
            let dialogs = dialogs |> List.choose (fun (dialog, requirements, consequents) -> if field.Advents.IsSupersetOf requirements then Some (dialog, consequents) else None) |> List.rev
            let (dialog, consequents) = match List.tryHead dialogs with Some dialog -> dialog | None -> ("...", Set.empty)
            let dialogForm = { DialogForm = DialogThick; DialogText = dialog; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = battleTypeOpt }
            let field = Field.updateDialogOpt (constant (Some dialogForm)) field
            let field = Field.updateAdvents (Set.addMany consequents) field
            just field

        static let interactShopkeep shopType (field : Field) =
            let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
            let field = Field.updateShopOpt (constant (Some shop)) field
            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)) field

        static let interactSavePoint (field : Field) =
            let field = Field.restoreTeam field
            Field.save field
            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.SaveSound)) field

        static let sidebar position depth (field : Lens<Field, World>) =
            Content.group Gen.name []
                [Content.button Gen.name
                   [Entity.PositionLocal == position; Entity.DepthLocal == depth; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "T"
                    Entity.EnabledLocal <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuTeam _ -> false | _ -> true
                    Entity.ClickEvent ==> msg SubmenuTeamOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 80.0f; Entity.DepthLocal == depth; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "I"
                    Entity.EnabledLocal <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuItem _ -> false | _ -> true
                    Entity.ClickEvent ==> msg SubmenuItemOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 400.0f; Entity.DepthLocal == depth; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "X"
                    Entity.ClickEvent ==> msg SubmenuClose]]

        static let team (position : Vector2) depth rows (field : Lens<Field, World>) filter fieldMsg =
            Content.entities field
                (fun field -> (field.Team, field.Submenu))
                (fun (team, submenu) _ -> team |> Map.toValueList |> List.filter (flip filter submenu))
                (fun i teammateLens world ->
                    let teammate = Lens.get teammateLens world
                    let x = position.X
                    let y = position.Y - single (i % rows) * 72.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.DepthLocal == depth; Entity.Size == v2 256.0f 72.0f
                         Entity.Text == CharacterType.getName teammate.CharacterType
                         Entity.ClickEvent ==> msg (fieldMsg i)])

        static let items (position : Vector2) depth field fieldMsg =
            Content.entities field
                (fun (field : Field) -> (field.Submenu, field.ShopOpt, field.Inventory))
                (fun (submenu, shopOpt, inventory : Inventory) _ ->
                    match submenu.SubmenuState with
                    | SubmenuItem submenu -> pageItems submenu.ItemPage 10 (Inventory.indexItems inventory)
                    | _ ->
                        match shopOpt with
                        | Some shop ->
                            match shop.ShopState with
                            | ShopBuying ->
                                match Map.tryFind shop.ShopType Data.Value.Shops with
                                | Some shopData -> shopData.ShopItems |> List.indexed |> pageItems shop.ShopPage 10
                                | None -> []
                            | ShopSelling ->
                                inventory |>
                                Inventory.indexItems |>
                                Seq.choose (function (_, Equipment _ as item) | (_, Consumable _ as item) -> Some item | (_, KeyItem _) | (_, Stash _) -> None) |>
                                pageItems shop.ShopPage 10
                        | None -> [])
                (fun i selectionLens _ ->
                    let x = if i < 5 then position.X else position.X + 368.0f
                    let y = position.Y - single (i % 5) * 80.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.DepthLocal == depth; Entity.Size == v2 336.0f 72.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v2 16.0f 0.0f
                         Entity.Text <== selectionLens --> fun (_, itemType) -> ItemType.getName itemType
                         Entity.EnabledLocal <== selectionLens --> fun (_, itemType) -> match itemType with Consumable _ | Equipment _ -> true | KeyItem _ | Stash _ -> false
                         Entity.ClickEvent ==> msg (fieldMsg selectionLens)])

        override this.Channel (_, field) =
            [Simulants.FieldAvatar.Avatar.ChangeEvent =|> fun evt -> msg (UpdateAvatar (evt.Data.Value :?> Avatar))
             field.SelectEvent => cmd PlayFieldSong
             field.UpdateEvent => msg UpdateDialog
             field.UpdateEvent => msg UpdatePortal
             field.UpdateEvent => msg UpdateSensor
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
                    let dialog = Dialog.update dialog world
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
                        if Option.isNone field.BattleOpt then // make sure we don't teleport if a battle is started earlier in the frame
                            let transition =
                                { FieldType = fieldType
                                  FieldDestination = destination
                                  FieldDirection = direction
                                  FieldTransitionTime = World.getTickTime world + Constants.Field.TransitionTime }
                            let field = Field.updateFieldTransitionOpt (constant (Some transition)) field
                            withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.StairStepsSound)) field
                        else just field
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

            | SubmenuTeamOpen ->
                let state = SubmenuTeam { TeamIndex = 0; TeamIndices = Map.toKeyList field.Team }
                let field = Field.updateSubmenu (fun submenu -> { submenu with SubmenuState = state }) field
                just field

            | SubmenuTeamAlly index ->
                let field =
                    Field.updateSubmenu (fun submenu ->
                        let state =
                            match submenu.SubmenuState with
                            | SubmenuTeam submenuTeam -> SubmenuTeam { submenuTeam with TeamIndex = index }
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
                match Map.tryFind index field.Team with
                | Some teammate ->
                    match field.Submenu.SubmenuUseOpt with
                    | Some submenuUse ->
                        let itemType = snd submenuUse.SubmenuUseSelection
                        let (result, displacedOpt, teammate) = Teammate.tryUseItem itemType teammate
                        let field = if result then Field.updateInventory (Inventory.removeItem itemType) field else field
                        let field = match displacedOpt with Some displaced -> Field.updateInventory (Inventory.addItem displaced) field | None -> field
                        let field = Field.updateTeam (Map.add index teammate) field
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

            | TryBattle (consequents, battleType) ->
                match Map.tryFind battleType Data.Value.Battles with
                | Some battleData ->
                    let time = World.getTickTime world
                    let prizePool = { Consequents = consequents; Items = []; Gold = 0; Exp = 0 }
                    let battle = Battle.makeFromTeam (Field.getParty field) field.Inventory prizePool battleData time
                    let field = Field.updateBattleOpt (constant (Some battle)) field
                    withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.BeastScreamSound)) field
                | None -> just field

            | Traverse velocity ->
                match Field.tryAdvanceEncounterCreep velocity field world with
                | (Some battleData, field) ->
                    let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
                    let battle = Battle.makeFromTeam field.Team field.Inventory prizePool battleData (World.getTickTime world)
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
                            | Chest (_, itemType, chestId, battleTypeOpt, requirements, consequents) -> interactChest itemType chestId battleTypeOpt requirements consequents field
                            | Door (_, requirements, consequents) -> interactDoor requirements consequents prop field
                            | Portal (_, _, _, _, _) -> just field
                            | Switch (_, requirements, consequents) -> interactSwitch requirements consequents prop field
                            | Sensor (_, _, _, _) -> just field
                            | Npc (_, specialty, _, dialogs, requirements) -> interactNpc specialty requirements dialogs field
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
                let tileMapBounds = Simulants.FieldTileMap.GetBounds world
                let eyeBounds = tileMapBounds.WithPosition (tileMapBounds.Position + v2Dup 48.0f)
                let eyeBounds = eyeBounds.WithSize (tileMapBounds.Size - v2Dup 96.0f)
                let world = World.constrainEyeBounds eyeBounds world
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

            [// scene layer
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
                    Entity.Visible <== field --> fun field -> Option.isSome field.FieldTransitionOpt
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
                    [Entity.Position == v2Dup 120.0f; Entity.Depth == Constants.Field.ForgroundDepth; Entity.Size == Constants.Gameplay.CharacterSize
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
                    [Entity.Position == v2 -456.0f -246.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.ButtonShortUpImage; Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Text == "Submenu"
                     Entity.Visible <== field --> fun field ->
                        field.Submenu.SubmenuState = SubmenuClosed &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.ClickEvent ==> msg SubmenuTeamOpen]

                 // interact button
                 Content.button Simulants.FieldInteract.Name
                    [Entity.Position == v2 306.0f -246.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.ButtonShortUpImage; Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Visible <== field --|> fun field world ->
                        field.Submenu.SubmenuState = SubmenuClosed &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Option.isSome (tryGetInteraction field.DialogOpt field.Advents field.Avatar world)
                     Entity.Text <== field --|> fun field world ->
                        match tryGetInteraction field.DialogOpt field.Advents field.Avatar world with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> msg Interact]

                 // dialog
                 Dialog.content Simulants.FieldDialog.Name
                    (field --> fun field -> field.DialogOpt)

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
                                    | Npc (_, specialty, _, _, requirements) -> NpcState (advents.IsSupersetOf requirements && NpcSpecialty.exists advents specialty)
                                    | Shopkeep (_, _, _, requirements) -> ShopkeepState (advents.IsSupersetOf requirements)
                                    | Chest _ | Portal _ | Sensor _ | SavePoint | ChestSpawn | EmptyProp -> NilState
                                | Some propState -> propState
                            Prop.make prop.PropBounds prop.PropDepth advents prop.PropData propState prop.PropId)
                        Content.entity<PropDispatcher> Gen.name [Entity.Prop <== prop])

                 // team
                 Content.panel Simulants.SubmenuTeam.Name
                    [Entity.Position == v2 -448.0f -256.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 896.0f 512.0f
                     Entity.LabelImage == Assets.DialogXXLImage
                     Entity.Visible <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuTeam _ -> true | _ -> false]
                    [sidebar (v2 24.0f 420.0f) 1.0f field
                     team (v2 138.0f 420.0f) 1.0f Int32.MaxValue field tautology2 SubmenuTeamAlly
                     Content.label Gen.name
                        [Entity.PositionLocal == v2 438.0f 288.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 192.0f 192.0f
                         Entity.LabelImage <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeamData field.Team submenu with
                                | Some characterData ->
                                    match characterData.MugOpt with
                                    | Some mug -> mug
                                    | None -> Assets.EmptyImage
                                | None -> Assets.EmptyImage
                            | _ -> Assets.EmptyImage]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 666.0f 372.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeamData field.Team submenu with
                                | Some characterData -> CharacterType.getName characterData.CharacterType
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 666.0f 336.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeammate field.Team submenu with
                                | Some teammate -> "Level " + string (Algorithms.expPointsToLevel teammate.ExpPoints)
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 444.0f 234.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeammate field.Team submenu with
                                | Some teammate -> "W: " + Option.getOrDefault "None" teammate.WeaponOpt
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 444.0f 204.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeammate field.Team submenu with
                                | Some teammate -> "A: " + Option.getOrDefault "None" teammate.ArmorOpt
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 444.0f 174.0f; Entity.DepthLocal == 1.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeammate field.Team submenu with
                                | Some teammate -> "1: " + Option.getOrDefault "None" (List.tryHead teammate.Accessories)
                                | None -> ""
                            | _ -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 444.0f -84.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 512.0f 256.0f
                         Entity.Justification == Unjustified true
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuState with
                            | SubmenuTeam submenu ->
                                match SubmenuTeam.tryGetTeammateAndTeamData field.Team submenu with
                                | Some (teammate, characterData) ->
                                    let level = Algorithms.expPointsToLevel teammate.ExpPoints
                                    let hpm = Algorithms.hitPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                    let tpm = Algorithms.techPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                    let pow = Algorithms.power teammate.WeaponOpt 1.0f characterData.ArchetypeType level
                                    let mag = Algorithms.magic teammate.WeaponOpt 1.0f characterData.ArchetypeType level
                                    "HP  "   + (string teammate.HitPoints).PadLeft 3 + "/" + (string hpm).PadLeft 3 +
                                    "\nTP  " + (string teammate.TechPoints).PadLeft 3 + "/" + (string tpm).PadLeft 3 +
                                    "\nPow " + (string pow).PadLeft 3 + "   Mag " + (string mag).PadLeft 3 +
                                    "\nExp " + (string teammate.ExpPoints).PadLeft 3 + "/" + (string (Algorithms.expPointsForNextLevel teammate.ExpPoints)).PadLeft 3
                                | None -> ""
                            | _ -> ""]]

                 // item
                 Content.panel Simulants.SubmenuItem.Name
                    [Entity.Position == v2 -448.0f -256.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 896.0f 512.0f
                     Entity.LabelImage == Assets.DialogXXLImage
                     Entity.Visible <== field --> fun field -> match field.Submenu.SubmenuState with SubmenuItem _ -> true | _ -> false
                     Entity.Enabled <== field --> fun field -> Option.isNone field.Submenu.SubmenuUseOpt]
                    [sidebar (v2 24.0f 420.0f) 1.0f field
                     items (v2 136.0f 420.0f) 1.0f field SubmenuItemSelect
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 368.0f 3.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // use
                 Content.panel Simulants.SubmenuUse.Name
                    [Entity.Position == v2 -448.0f -192.0f; Entity.Depth == Constants.Field.GuiDepth + 10.0f; Entity.Size == v2 896.0f 384.0f
                     Entity.LabelImage == Assets.DialogXLImage
                     Entity.Visible <== field --> fun field -> Option.isSome field.Submenu.SubmenuUseOpt]
                    [team (v2 160.0f 150.0f) 1.0f 3 field
                        (fun teammate submenu ->
                            match submenu.SubmenuUseOpt with
                            | Some submenuUse -> Teammate.canUseItem (snd submenuUse.SubmenuUseSelection) teammate
                            | None -> false)
                        SubmenuItemUse
                     Content.button Gen.name
                        [Entity.PositionLocal == v2 810.0f 306.0f; Entity.DepthLocal == 2.0f; Entity.Size == v2 64.0f 64.0f
                         Entity.Text == "X"
                         Entity.ClickEvent ==> msg SubmenuItemCancel]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 42.0f 306.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.Submenu.SubmenuUseOpt with
                            | Some submenu -> submenu.SubmenuUseLine1
                            | None -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 60.0f 264.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                             match field.Submenu.SubmenuUseOpt with
                             | Some submenu -> submenu.SubmenuUseLine2
                             | None -> ""]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 60.0f 222.0f; Entity.DepthLocal == 2.0f
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
                        [Entity.PositionLocal == v2 24.0f 444.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Buy"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false
                         Entity.ClickEvent ==> msg ShopBuy]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 24.0f 444.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Buy what?"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false]
                     Content.button Simulants.FieldShopSell.Name
                        [Entity.PositionLocal == v2 352.0f 444.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Sell"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false
                         Entity.ClickEvent ==> msg ShopSell]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 352.0f 444.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Sell what?"
                         Entity.Visible <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false]
                     Content.button Simulants.FieldShopLeave.Name
                        [Entity.PositionLocal == v2 678.0f 444.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Leave"
                         Entity.ClickEvent ==> msg ShopLeave]
                     Content.button Simulants.FieldShopPageUp.Name
                        [Entity.PositionLocal == v2 24.0f 18.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                         Entity.Text == "<"
                         Entity.ClickEvent ==> msg ShopPageUp]
                     Content.button Simulants.FieldShopPageDown.Name
                        [Entity.PositionLocal == v2 822.0f 18.0f; Entity.DepthLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                         Entity.Text == ">"
                         Entity.ClickEvent ==> msg ShopPageDown]
                     Content.text Simulants.FieldShopGold.Name
                        [Entity.PositionLocal == v2 352.0f 3.0f; Entity.DepthLocal == 1.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // shop confirm
                 Content.panel Simulants.FieldShopConfirm.Name
                    [Entity.Position == v2 -448.0f -128.0f; Entity.Depth == Constants.Field.GuiDepth + 10.0f; Entity.Size == v2 864.0f 252.0f
                     Entity.LabelImage == Assets.DialogThickImage
                     Entity.Visible <== field --> fun field ->
                        match field.ShopOpt with
                        | Some shop -> Option.isSome shop.ShopConfirmOpt
                        | None -> false]
                    [Content.button Simulants.FieldShopConfirmAccept.Name
                        [Entity.PositionLocal == v2 198.0f 42.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Accept"
                         Entity.ClickEvent ==> msg ShopConfirmAccept]
                     Content.button Simulants.FieldShopConfirmDecline.Name
                        [Entity.PositionLocal == v2 498.0f 42.0f; Entity.DepthLocal == 2.0f
                         Entity.Text == "Decline"
                         Entity.ClickEvent ==> msg ShopConfirmDecline]
                     Content.text Simulants.FieldShopConfirmOffer.Name
                        [Entity.PositionLocal == v2 42.0f 180.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmOffer
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine1.Name
                        [Entity.PositionLocal == v2 60.0f 138.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmLine1
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine2.Name
                        [Entity.PositionLocal == v2 60.0f 96.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== field --> fun field ->
                            match field.ShopOpt with
                            | Some shop ->
                                match shop.ShopConfirmOpt with
                                | Some shopConfirm -> shopConfirm.ShopConfirmLine2
                                | None -> ""
                            | None -> ""]]]]