namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open TiledSharp
open OmniBlade

[<AutoOpen>]
module OmniField =

    type [<NoComparison; NoEquality>] FieldMessage =
        | UpdateAvatar of AvatarModel
        | UpdateFieldTransition
        | UpdateDialog
        | UpdatePortal
        | UpdateSensor
        | ShopPageUp
        | ShopPageDown
        | ShopConfirmPrompt of bool * Lens<int * ItemType, World>
        | ShopConfirmAccept
        | ShopConfirmDecline
        | ShopLeave
        | Interact

    type [<NoComparison>] FieldCommand =
        | PlaySound of int64 * single * Sound AssetTag
        | MoveAvatar of Vector2
        | UpdateEye

    type Screen with

        member this.GetFieldModel = this.GetModel<FieldModel>
        member this.SetFieldModel = this.SetModel<FieldModel>
        member this.FieldModel = this.Model<FieldModel> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<FieldModel, FieldMessage, FieldCommand> (FieldModel.initial)

        static let isFacingBodyShape bodyShape (avatar : AvatarModel) world =
            if bodyShape.Entity.Is<PropDispatcher> world then
                let v = bodyShape.Entity.GetBottom world - avatar.Bottom
                let direction = Direction.fromVector2 v
                direction = avatar.Direction
            else false

        static let getFacingBodyShapes (avatar : AvatarModel) world =
            List.filter
                (fun bodyShape -> isFacingBodyShape bodyShape avatar world)
                avatar.IntersectedBodyShapes

        static let tryGetFacingProp (avatar : AvatarModel) world =
            match getFacingBodyShapes avatar world with
            | head :: _ -> Some head.Entity
            | [] -> None

        static let tryGetInteraction3 dialogOpt advents prop =
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
            | Some dialog ->
                if dialog.DialogProgress > dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[dialog.DialogPage].Length
                then Some "Next"
                else None

        static let tryGetInteraction dialogOpt advents (avatar : AvatarModel) world =
            match tryGetFacingProp avatar world with
            | Some prop -> tryGetInteraction3 dialogOpt advents (prop.GetPropModel world).PropData
            | None -> None

        static let tryGetTouchingPortal (avatar : AvatarModel) world =
            avatar.IntersectedBodyShapes |>
            List.choose (fun shape ->
                match (shape.Entity.GetPropModel world).PropData with
                | Portal (_, fieldType, index, direction, _) -> Some (fieldType, index, direction)
                | _ -> None) |>
            List.tryHead

        static let getTouchedSensors (avatar : AvatarModel) world =
            List.choose (fun shape ->
                match (shape.Entity.GetPropModel world).PropData with
                | Sensor (sensorType, _, requirements, consequents) -> Some (sensorType, requirements, consequents)
                | _ -> None)
                avatar.CollidedBodyShapes

        static let getUntouchedSensors (avatar : AvatarModel) world =
            List.choose (fun shape ->
                match (shape.Entity.GetPropModel world).PropData with
                | Sensor (sensorType, _, requirements, consequents) -> Some (sensorType, requirements, consequents)
                | _ -> None)
                avatar.SeparatedBodyShapes

        static let interactDialog dialog model =
            if dialog.DialogPage < dialog.DialogText.Split(Constants.Gameplay.DialogSplit).Length - 1 then
                let dialog = { dialog with DialogProgress = 0; DialogPage = inc dialog.DialogPage }
                let model = FieldModel.updateDialogOpt (constant (Some dialog)) model
                just model
            else just (FieldModel.updateDialogOpt (constant None) model)

        static let interactChest time itemType chestId battleTypeOpt requirements consequents (model : FieldModel) =
            if model.Advents.IsSupersetOf requirements then
                match battleTypeOpt with
                | Some battleType ->
                    match Map.tryFind battleType data.Value.Battles with
                    | Some battleData ->
                        let battleModel = BattleModel.makeFromLegion (FieldModel.getParty model) model.Inventory (Some itemType) battleData time
                        let model = FieldModel.updateBattleOpt (constant (Some battleModel)) model
                        just model
                    | None -> just model
                | None ->
                    let model = FieldModel.updateInventory (Inventory.addItem itemType) model
                    let model = FieldModel.updateAdvents (Set.add (Opened chestId)) model
                    let model = FieldModel.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0 })) model
                    let model = FieldModel.updateAdvents (Set.addMany consequents) model
                    withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenChestSound))
            else just (FieldModel.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0 })) model)

        static let interactDoor requirements consequents (propModel : PropModel) (model : FieldModel) =
            match propModel.PropState with
            | DoorState false ->
                if model.Advents.IsSupersetOf requirements then
                    let model = FieldModel.updateAdvents (Set.addMany consequents) model
                    let model = FieldModel.updatePropStates (Map.add propModel.PropId (DoorState true)) model
                    withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.OpenDoorSound))
                else just (FieldModel.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Locked!"; DialogProgress = 0; DialogPage = 0 })) model)
            | _ -> failwithumf ()

        static let interactSwitch requirements consequents (propModel : PropModel) (model : FieldModel) =
            match propModel.PropState with
            | SwitchState on ->
                if model.Advents.IsSupersetOf requirements then
                    let model = FieldModel.updateAdvents (if on then Set.removeMany consequents else Set.addMany consequents) model
                    let model = FieldModel.updatePropStates (Map.add propModel.PropId (SwitchState (not on))) model
                    withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.UseSwitchSound))
                else just (FieldModel.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogText = "Won't budge!"; DialogProgress = 0; DialogPage = 0 })) model)
            | _ -> failwithumf ()

        static let interactNpc dialogs (model : FieldModel) =
            let dialogs = dialogs |> List.choose (fun (dialog, requirements, consequents) -> if model.Advents.IsSupersetOf requirements then Some (dialog, consequents) else None) |> List.rev
            let (dialog, consequents) = match List.tryHead dialogs with Some dialog -> dialog | None -> ("...", Set.empty)
            let dialogForm = { DialogForm = DialogLarge; DialogText = dialog; DialogProgress = 0; DialogPage = 0 }
            let model = FieldModel.updateDialogOpt (constant (Some dialogForm)) model
            let model = FieldModel.updateAdvents (Set.addMany consequents) model
            just model

        static let interactShopkeep shopType (model : FieldModel) =
            let shopModel = { ShopType = shopType; ShopState = ShopSelling; ShopPage = 0; ShopConfirmModelOpt = None }
            let model = FieldModel.updateShopModelOpt (constant (Some shopModel)) model
            just model

        override this.Channel (_, field) =
            [Simulants.FieldAvatar.AvatarModel.ChangeEvent =|> fun evt -> msg (UpdateAvatar (evt.Data.Value :?> AvatarModel))
             field.UpdateEvent => msg UpdateDialog
             field.UpdateEvent => msg UpdateFieldTransition
             field.UpdateEvent => msg UpdatePortal
             field.UpdateEvent => msg UpdateSensor
             Simulants.FieldInteract.ClickEvent => msg Interact
             field.PostUpdateEvent => cmd UpdateEye]

        override this.Message (model, message, _, world) =

            match message with
            | UpdateAvatar avatarModel ->
                let model = FieldModel.updateAvatar (constant avatarModel) model
                just model

            | UpdateDialog ->
                let model =
                    FieldModel.updateDialogOpt
                        (function
                         | Some dialog ->
                            let increment = if World.getTickTime world % 2L = 0L then 1 else 0
                            let dialog = { dialog with DialogProgress = dialog.DialogProgress + increment }
                            Some dialog
                         | None -> None)
                        model
                just model

            | UpdateFieldTransition ->
                match model.FieldTransitionOpt with
                | Some fieldTransition ->
                    let tickTime = World.getTickTime world
                    if tickTime = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L then
                        let model = FieldModel.updateFieldType (constant fieldTransition.FieldType) model
                        let model =
                            FieldModel.updateAvatar (fun avatar ->
                                let avatar = AvatarModel.updateDirection (constant fieldTransition.FieldDirection) avatar
                                let avatar = AvatarModel.updateDirection (constant fieldTransition.FieldDirection) avatar
                                let avatar = AvatarModel.updateIntersectedBodyShapes (constant []) avatar
                                avatar)
                                model
                        withCmd model (MoveAvatar fieldTransition.FieldIndex)
                    elif tickTime = fieldTransition.FieldTransitionTime then
                        let model = FieldModel.updateFieldTransitionOpt (constant None) model
                        just model
                    else just model
                | None -> just model

            | UpdatePortal ->
                match model.FieldTransitionOpt with
                | None ->
                    match tryGetTouchingPortal model.Avatar world with
                    | Some (fieldType, index, direction) ->
                        let transition =
                            { FieldType = fieldType
                              FieldIndex = index
                              FieldDirection = direction
                              FieldTransitionTime = World.getTickTime world + Constants.Field.TransitionTime }
                        let model = FieldModel.updateFieldTransitionOpt (constant (Some transition)) model
                        withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.StairStepsSound))
                    | None -> just model
                | Some _ -> just model

            | UpdateSensor ->
                match model.FieldTransitionOpt with
                | None ->
                    let sensors = getTouchedSensors model.Avatar world
                    let results =
                        List.fold (fun (model : FieldModel, cmds : Signal<FieldMessage, FieldCommand> list) (sensorType, requirements, consequents) ->
                            if model.Advents.IsSupersetOf requirements then
                                let model = FieldModel.updateAdvents (constant (Set.union consequents model.Advents)) model
                                match sensorType with
                                | AirSensor -> (model, cmds)
                                | HiddenSensor | StepPlateSensor -> (model, (Command (PlaySound (0L,  Constants.Audio.DefaultSoundVolume, Assets.TriggerSound)) :: cmds))
                            else (model, cmds))
                            (model, []) sensors
                    results
                | Some _ -> just model

            | ShopPageUp ->
                let model = FieldModel.updateShopModelOpt (Option.map (fun shopModel -> { shopModel with ShopPage = max 0 (dec shopModel.ShopPage) })) model
                just model

            | ShopPageDown ->
                let model = FieldModel.updateShopModelOpt (Option.map (fun shopModel -> { shopModel with ShopPage = inc shopModel.ShopPage })) model
                just model

            | ShopConfirmPrompt (buying, selectionLens) ->
                let selection = Lens.get selectionLens world
                let model =
                    FieldModel.updateShopModelOpt (Option.map (fun shopModel ->
                        let shopConfirmModelOpt = ShopConfirmModel.tryMakeFromSelection buying selection
                        { shopModel with ShopConfirmModelOpt = shopConfirmModelOpt }))
                        model
                just model

            | ShopConfirmAccept ->
                match model.ShopModelOpt with
                | Some shopModel ->
                    match shopModel.ShopConfirmModelOpt with
                    | Some shopConfirmModel ->
                        let itemType = snd shopConfirmModel.ShopConfirmSelection
                        let model = FieldModel.updateInventory (Inventory.removeItem itemType) model
                        let model = FieldModel.updateInventory (Inventory.updateGold ((if shopModel.ShopState = ShopBuying then (-) else (+)) shopConfirmModel.ShopConfirmPrice)) model
                        let model = FieldModel.updateShopModelOpt (Option.map (fun shopModel -> { shopModel with ShopConfirmModelOpt = None })) model
                        withCmd model (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.PurchaseSound))
                    | None -> just model
                | None -> just model

            | ShopConfirmDecline ->
                let model = FieldModel.updateShopModelOpt (Option.map (fun shopModel -> { shopModel with ShopConfirmModelOpt = None })) model
                just model

            | ShopLeave ->
                let model = FieldModel.updateShopModelOpt (constant None) model
                just model

            | Interact ->
                match model.DialogOpt with
                | None ->
                    match tryGetFacingProp model.Avatar world with
                    | Some prop ->
                        let propModel = prop.GetPropModel world
                        match propModel.PropData with
                        | Chest (_, itemType, chestId, battleTypeOpt, requirements, consequents) -> interactChest (World.getTickTime world) itemType chestId battleTypeOpt requirements consequents model
                        | Door (_, requirements, consequents) -> interactDoor requirements consequents propModel model
                        | Portal (_, _, _, _, _) -> just model
                        | Switch (_, requirements, consequents) -> interactSwitch requirements consequents propModel model
                        | Sensor (_, _, _, _) -> just model
                        | Npc (_, _, dialogs, _) -> interactNpc dialogs model
                        | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType model
                    | None -> just model
                | Some dialog -> interactDialog dialog model

        override this.Command (_, command, _, world) =

            match command with
            | UpdateEye ->
                let avatarModel = Simulants.FieldAvatar.GetAvatarModel world
                let world = World.setEyeCenter avatarModel.Center world
                just world

            | MoveAvatar position ->
                let world = Simulants.FieldAvatar.SetCenter position world
                let world = World.setBodyPosition position (Simulants.FieldAvatar.GetPhysicsId world) world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

        override this.Content (model, _) =

            [// main layer
             Content.layer Simulants.FieldScene.Name []

                [// backdrop sprite
                 Content.staticSprite Simulants.FieldBackdrop.Name
                    [Entity.Absolute == true
                     Entity.Depth == Single.MinValue
                     Entity.StaticImage == asset Assets.DefaultPackageName "Image9"
                     Entity.Bounds <== model ->> fun _ world -> World.getViewBoundsAbsolute world
                     Entity.Color <== model --> fun model ->
                        match data.Value.Fields.TryGetValue model.FieldType with
                        | (true, fieldData) -> fieldData.FieldBackgroundColor
                        | (false, _) -> Color.Black]

                 // portal fade sprite
                 Content.staticSprite Simulants.FieldPortalFade.Name
                   [Entity.Absolute == true
                    Entity.Depth == Single.MaxValue
                    Entity.StaticImage == asset Assets.DefaultPackageName "Image9"
                    Entity.Bounds <== model ->> fun _ world -> World.getViewBoundsAbsolute world
                    Entity.Color <== model ->> fun model world ->
                        match model.FieldTransitionOpt with
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

                 // tile map
                 Content.tileMap Simulants.FieldTileMap.Name
                    [Entity.Depth == Constants.Field.BackgroundDepth
                     Entity.TileMapAsset <== model --> fun model ->
                        match Map.tryFind model.FieldType data.Value.Fields with
                        | Some fieldData -> fieldData.FieldTileMap
                        | None -> Assets.DebugRoomTileMap
                     Entity.TileLayerClearance == 10.0f]

                 // avatar
                 Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                    [Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Position == v2 256.0f 256.0f
                     Entity.Depth == Constants.Field.ForgroundDepth
                     Entity.Enabled <== model --> fun model ->
                        Option.isNone model.DialogOpt &&
                        Option.isNone model.ShopModelOpt &&
                        Option.isNone model.FieldTransitionOpt
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.AvatarModel <== model --> fun model -> model.Avatar]

                 // interact button
                 Content.button Simulants.FieldInteract.Name
                    [Entity.Size == v2 192.0f 64.0f
                     Entity.Position == v2 248.0f -240.0f
                     Entity.Depth == Constants.Field.GuiDepth
                     Entity.UpImage == Assets.ButtonShortUpImage
                     Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Visible <== model ->> fun model world ->
                        let interactionOpt = tryGetInteraction model.DialogOpt model.Advents model.Avatar world
                        Option.isSome interactionOpt &&
                        Option.isNone model.ShopModelOpt
                     Entity.Text <== model ->> fun model world ->
                        match tryGetInteraction model.DialogOpt model.Advents model.Avatar world with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None]

                 // dialog
                 Content.text Simulants.FieldDialog.Name
                    [Entity.Bounds <== model --> fun model ->
                        match model.DialogOpt with
                        | Some dialog ->
                            match dialog.DialogForm with
                            | DialogThin -> v4Bounds (v2 -448.0f 128.0f) (v2 896.0f 112.0f)
                            | DialogMedium -> v4Bounds (v2 -448.0f 0.0f) (v2 640.0f 256.0f)
                            | DialogLarge -> v4Bounds (v2 -448.0f 0.0f) (v2 896.0f 256.0f)
                        | None -> v4Zero
                     Entity.BackgroundImageOpt <== model --> fun model ->
                        let image =
                            match model.DialogOpt with
                            | Some dialog ->
                                match dialog.DialogForm with
                                | DialogThin -> Assets.DialogThinImage
                                | DialogMedium -> Assets.DialogMediumImage
                                | DialogLarge -> Assets.DialogLargeImage
                            | None -> Assets.DialogLargeImage
                        Some image
                     Entity.Text <== model --> fun model ->
                        match model.DialogOpt with
                        | Some dialog ->
                            let textPage = dialog.DialogPage
                            let text = dialog.DialogText.Split(Constants.Gameplay.DialogSplit).[textPage]
                            let textToShow = String.tryTake dialog.DialogProgress text
                            textToShow
                        | None -> ""
                     Entity.Visible <== model --> fun model -> Option.isSome model.DialogOpt
                     Entity.Justification == Unjustified true
                     Entity.Margins == v2 40.0f 40.0f]

                 // props
                 Content.entities model
                    (fun model -> (model.FieldType, model.Advents, model.PropStates))
                    (fun (fieldType, advents, propStates) world ->
                        match Map.tryFind fieldType data.Value.Fields with
                        | Some fieldData ->
                            match World.tryGetTileMapMetadata fieldData.FieldTileMap world with
                            | Some (_, _, tileMap) ->
                                if tileMap.ObjectGroups.Contains Constants.Field.PropsLayerName then
                                    let group = tileMap.ObjectGroups.Item Constants.Field.PropsLayerName
                                    let objects = enumerable<TmxObject> group.Objects
                                    let results = Seq.map (fun object -> (object, group, tileMap, advents, propStates)) objects
                                    Seq.toList results
                                else []
                            | None -> []
                        | None -> [])
                    (fun _ model _ ->
                        let propModel = model.Map (fun (object, group, tileMap, advents, propStates) ->
                            let propPosition = v2 (single object.X) (single tileMap.Height * single tileMap.TileHeight - single object.Y) // invert y
                            let propSize = v2 (single object.Width) (single object.Height)
                            let propBounds = v4Bounds propPosition propSize
                            let propDepth =
                                match group.Properties.TryGetValue Constants.TileMap.DepthPropertyName with
                                | (true, depthStr) -> Constants.Field.ForgroundDepth + scvalue depthStr
                                | (false, _) -> Constants.Field.ForgroundDepth
                            let propData =
                                match object.Properties.TryGetValue Constants.TileMap.InfoPropertyName with
                                | (true, propDataStr) -> scvalue propDataStr
                                | (false, _) -> PropData.empty
                            let propState =
                                match Map.tryFind object.Id propStates with
                                | None ->
                                    match propData with
                                    | Door (_, _, _) -> DoorState false
                                    | Switch (_, _, _) -> SwitchState false
                                    | Npc (_, _, _, requirements) -> NpcState (advents.IsSupersetOf requirements)
                                    | Shopkeep (_, _, _, requirements) -> ShopkeepState (advents.IsSupersetOf requirements)
                                    | _ -> NilState
                                | Some propState -> propState
                            PropModel.make propBounds propDepth advents propData propState object.Id)
                        Content.entity<PropDispatcher> Gen.name [Entity.PropModel <== propModel])

                 // shop
                 Content.panel Simulants.FieldShop.Name
                    [Entity.Position == v2 -448.0f -256.0f
                     Entity.Size == v2 896.0f 512.0f
                     Entity.Depth == Constants.Field.GuiDepth
                     Entity.LabelImage == Assets.DialogHugeImage
                     Entity.Visible <== model --> fun model -> Option.isSome model.ShopModelOpt
                     Entity.Enabled <== model --> fun model -> match model.ShopModelOpt with Some shopModel -> Option.isNone shopModel.ShopConfirmModelOpt | None -> true]
                    [Content.button Simulants.FieldShopBuy.Name
                        [Entity.PositionLocal == v2 12.0f 440.0f; Entity.DepthLocal == 2.0f; Entity.Text == "Buy"]
                     Content.button Simulants.FieldShopSell.Name
                        [Entity.PositionLocal == v2 320.0f 440.0f; Entity.DepthLocal == 2.0f; Entity.Text == "Sell"]
                     Content.button Simulants.FieldShopLeave.Name
                        [Entity.PositionLocal == v2 628.0f 440.0f; Entity.DepthLocal == 2.0f; Entity.Text == "Leave"
                         Entity.ClickEvent ==|> fun _ -> msg ShopLeave]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 12.0f 440.0f; Entity.DepthLocal == 1.0f; Entity.Text == "Buy..."; Entity.Justification == Justified (JustifyCenter, JustifyMiddle)]
                     Content.text Gen.name
                        [Entity.PositionLocal == v2 320.0f 440.0f; Entity.DepthLocal == 1.0f; Entity.Text == "Sell..."; Entity.Justification == Justified (JustifyCenter, JustifyMiddle)]
                     Content.text Simulants.FieldShopGold.Name
                        [Entity.PositionLocal == v2 316.0f 12.0f; Entity.DepthLocal == 1.0f; Entity.Text <== model --> (fun model -> string model.Inventory.Gold + "G"); Entity.Justification == Justified (JustifyCenter, JustifyMiddle)]
                     Content.button Simulants.FieldShopPageUp.Name
                        [Entity.PositionLocal == v2 16.0f 12.0f; Entity.DepthLocal == 1.0f; Entity.Text == "<"; Entity.Size == v2 48.0f 64.0f
                         Entity.ClickEvent ==> msg ShopPageUp]
                     Content.button Simulants.FieldShopPageDown.Name
                        [Entity.PositionLocal == v2 832.0f 12.0f; Entity.DepthLocal == 1.0f; Entity.Text == ">"; Entity.Size == v2 48.0f 64.0f
                         Entity.ClickEvent ==> msg ShopPageDown]
                     Content.entities model
                        (fun (model : FieldModel) -> (model.ShopModelOpt, model.Inventory))
                        (fun (shopModelOpt, inventory : Inventory) _ ->
                            match shopModelOpt with
                            | Some shopModel ->
                                inventory |>
                                Inventory.indexItems |>
                                Seq.choose
                                    (function
                                     | (_, Equipment _ as item) -> Some item
                                     | (_, Consumable _ as item) -> Some item
                                     | (_, KeyItem _) -> None
                                     | (_, Stash _) -> None) |>
                                Seq.chunkBySize 10 |>
                                Seq.trySkip shopModel.ShopPage |>
                                Seq.map List.ofArray |>
                                Seq.tryHead |>
                                Option.defaultValue []
                            | None -> [])
                        (fun i selection _ ->
                            let x = if i < 5 then 12.0f else 448.0f
                            let y = 368.0f - single (i % 5) * 72.0f
                            Content.button Gen.name
                                [Entity.PositionLocal == v2 x y
                                 Entity.Size == v2 320.0f 64.0f
                                 Entity.DepthLocal == 1.0f
                                 Entity.Text <== selection --> fun (_, itemType) -> ItemType.getName itemType
                                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                 Entity.Margins == v2 16.0f 0.0f
                                 Entity.ClickEvent ==> msg (ShopConfirmPrompt (false, selection))])]

                 // shop confirm
                 Content.panel Simulants.FieldShopConfirm.Name
                    [Entity.Position == v2 -448.0f -128.0f
                     Entity.Size == v2 896.0f 256.0f
                     Entity.Depth == Constants.Field.GuiDepth + 10.0f
                     Entity.LabelImage == Assets.DialogLargeImage
                     Entity.Visible <== model --> fun model ->
                        match model.ShopModelOpt with
                        | Some shopModel -> Option.isSome shopModel.ShopConfirmModelOpt
                        | None -> false]
                    [Content.button Simulants.FieldShopConfirmAccept.Name
                        [Entity.PositionLocal == v2 160.0f 16.0f; Entity.DepthLocal == 2.0f; Entity.Text == "Accept"
                         Entity.ClickEvent ==> msg ShopConfirmAccept]
                     Content.button Simulants.FieldShopConfirmDecline.Name
                        [Entity.PositionLocal == v2 456.0f 16.0f; Entity.DepthLocal == 2.0f; Entity.Text == "Decline"
                         Entity.ClickEvent ==> msg ShopConfirmDecline]
                     Content.text Simulants.FieldShopConfirmOffer.Name
                        [Entity.PositionLocal == v2 32.0f 176.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== model --> fun model ->
                            match model.ShopModelOpt with
                            | Some shopModel ->
                                match shopModel.ShopConfirmModelOpt with
                                | Some shopConfirmModel -> shopConfirmModel.ShopConfirmOffer
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine1.Name
                        [Entity.PositionLocal == v2 64.0f 128.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== model --> fun model ->
                            match model.ShopModelOpt with
                            | Some shopModel ->
                                match shopModel.ShopConfirmModelOpt with
                                | Some shopConfirmModel -> shopConfirmModel.ShopConfirmLine1
                                | None -> ""
                            | None -> ""]
                     Content.text Simulants.FieldShopConfirmLine2.Name
                        [Entity.PositionLocal == v2 64.0f 80.0f; Entity.DepthLocal == 2.0f
                         Entity.Text <== model --> fun model ->
                            match model.ShopModelOpt with
                            | Some shopModel ->
                                match shopModel.ShopConfirmModelOpt with
                                | Some shopConfirmModel -> shopConfirmModel.ShopConfirmLine2
                                | None -> ""
                            | None -> ""]]]]