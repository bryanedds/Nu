// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module FieldDispatcher =

    type Screen with
        member this.GetField world = this.GetModelGeneric<Field> world
        member this.SetField value world = this.SetModelGeneric<Field> value world
        member this.Field = this.ModelGeneric<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.empty)

        static let detokenize (text : string) (field : Field) =
            text
                .Replace("$FEE", scstring (Field.getRecruitmentFee field))
                .Replace("$GOLD", scstring field.Inventory.Gold)

        static let isFacingProp propId (avatar : Avatar) (props : Map<int, Prop>) =
            match props.TryGetValue propId with
            | (true, prop) ->
                let v = prop.Bottom - avatar.Bottom
                let direction = Direction.ofVector3 v
                direction <> avatar.Direction.Opposite
            | (false, _) -> false

        static let getFacingProps (avatar : Avatar) props =
            List.filter
                (fun propId -> isFacingProp propId avatar props)
                avatar.IntersectedPropIds

        static let tryGetFacingProp (avatar : Avatar) props =
            match getFacingProps avatar props with
            | head :: _ -> Some (props.[head])
            | [] -> None

        static let isTouchingSavePoint (avatar : Avatar) (props : Map<int, Prop>) =
            List.exists (fun propId ->
                match props.TryGetValue propId with
                | (true, prop) -> prop.PropData = SavePoint
                | (false, _) -> false)
                avatar.IntersectedPropIds

        static let tryGetFacingInteraction avatarPositionY advents (prop : Prop) =
            match prop.PropData with
            | Sprite _ -> None
            | Portal (_, _, _, _, _, _, _) -> None
            | Door _ -> Some "Open"
            | Chest (_, _, chestId, _, _, _) -> if Set.contains (Opened chestId) advents then None else Some "Open"
            | Switch (_, _, _, _) -> Some "Use"
            | Sensor (_, _, _, _, _) -> None
            | Character (_, _, _, isRising, _, _) ->
                if isRising then
                    if prop.Bottom.Y - avatarPositionY > 40.0f // NOTE: just a bit of hard-coding to ensure player is interacting with the character from the south.
                    then Some "Talk"
                    else None
                else Some "Talk"
            | Npc _ | NpcBranching _ -> Some "Talk"
            | Shopkeep _ -> Some "Shop"
            | Seal _ -> Some "Touch"
            | Flame _ -> None
            | SavePoint -> None
            | ChestSpawn -> None
            | EmptyProp -> None

        static let tryGetInteraction dialogOpt advents (avatar : Avatar) (props : Map<int, Prop>) detokenize =
            match dialogOpt with
            | Some dialog ->
                if  Dialog.canAdvance detokenize dialog &&
                    not
                        (Dialog.isExhausted detokenize dialog &&
                         Option.isSome dialog.DialogPromptOpt)
                then Some "Next"
                else None
            | None ->
                if isTouchingSavePoint avatar props then
                    Some "Save"
                else
                    match tryGetFacingProp avatar props with
                    | Some prop -> tryGetFacingInteraction avatar.Position.Y advents prop
                    | None -> None

        static let tryGetTouchingPortal omniSeedState (advents : Advent Set) (avatar : Avatar) (props : Map<int, Prop>) =
            avatar.IntersectedPropIds |>
            List.choose (fun propId ->
                match props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Portal (portalType, _, _, fieldType, portalIndex, _, requirements) ->
                        if advents.IsSupersetOf requirements then
                            match Map.tryFind fieldType Data.Value.Fields with
                            | Some fieldData ->
                                match FieldData.tryGetPortal omniSeedState portalIndex fieldData with
                                | Some portal ->
                                    match portal.PropData with
                                    | Portal (_, _, direction, _, _, extended, _) ->
                                        let destination =
                                            match direction with
                                            | Upward -> portal.PropPerimeter.Top + v3 0.0f 8.0f 0.0f + if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                            | Rightward -> portal.PropPerimeter.Right + v3 32.0f 0.0f 0.0f + if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                            | Downward -> portal.PropPerimeter.Bottom + v3 0.0f -54.0f 0.0f - if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                            | Leftward -> portal.PropPerimeter.Left + v3 -32.0f 0.0f 0.0f - if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                        let isWarp = match portalType with AirPortal | StairsPortal _ -> false | WarpPortal -> true
                                        Some (fieldType, destination, direction, isWarp)
                                    | _ -> None
                                | None -> None
                            | None -> None
                        else None
                    | _ -> None
                | _ -> None) |>
            List.tryHead

        static let getTouchedSensors (avatar : Avatar) (props : Map<int, Prop>) =
            List.choose (fun propId ->
                match props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                    | _ -> None
                | (false, _) -> None)
                avatar.CollidedPropIds

        static let getUntouchedSensors (avatar : Avatar) (field : Field) =
            List.choose (fun propId ->
                match field.Props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                    | _ -> None
                | (false, _) -> None)
                avatar.SeparatedPropIds

        do ignore getUntouchedSensors // NOTE: suppressing warning.

        static let interactDialog dialog field =
            match Dialog.tryAdvance (flip detokenize field) dialog with
            | (true, dialog) ->
                let field = Field.updateDialogOpt (constant (Some dialog)) field
                just field
            | (false, dialog) ->
                let field = Field.updateDialogOpt (constant None) field
                match dialog.DialogBattleOpt with
                | Some (battleType, consequence) -> withMsg (TryBattle (battleType, consequence)) field
                | None -> just field

        static let interactChest itemType chestId battleTypeOpt cue requirements (prop : Prop) (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                let field = Field.updateAdvents (Set.add (Opened chestId)) field
                let field = Field.updateInventory (Inventory.tryAddItem itemType >> snd) field
                let field =
                    match battleTypeOpt with
                    | Some battleType -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!^But something approaches!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = Some (battleType, Set.empty) })) field
                    | None -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                let field = Field.updateCue (constant cue) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestOpenSound)) field
            else
                let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestLockedSound)) field

        static let interactDoor keyItemTypeOpt cue requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | DoorState false ->
                if  field.Advents.IsSupersetOf requirements &&
                    Option.mapOrDefaultValue (fun keyItemType -> Map.containsKey (KeyItem keyItemType) field.Inventory.Items) true keyItemTypeOpt then
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateCue (constant cue) field
                    let field = Field.updatePropState (constant (DoorState true)) prop.PropId field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorOpenSound)) field
                else
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorLockedSound)) field
            | _ -> failwithumf ()

        static let interactSwitch cue cue2 requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | SwitchState on ->
                if field.Advents.IsSupersetOf requirements then
                    let on = not on
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updatePropState (constant (SwitchState on)) prop.PropId field
                    let field = Field.updateCue (constant (if on then cue else cue2)) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchUseSound)) field
                else
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Won't budge!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchStuckSound)) field
            | _ -> failwithumf ()

        static let interactCharacter cue (prop : Prop) (field : Field) =
            let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
            let field = Field.updateCue (constant cue) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field
        
        static let interactNpc branches requirements (prop : Prop) (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
                let branchesFiltered = branches |> List.choose (fun branch -> if field.Advents.IsSupersetOf branch.Requirements then Some branch.Cue else None) |> List.rev
                let branchCue = match List.tryHead branchesFiltered with Some cue -> cue | None -> Dialog ("...", false)
                let field = Field.updateCue (constant branchCue) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field
            else just field

        static let interactShopkeep shopType (prop : Prop) (field : Field) =
            let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
            let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
            let field = Field.updateShopOpt (constant (Some shop)) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field

        static let interactSeal cue (field : Field) =
            let field = Field.updateCue (constant cue) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SealedSound)) field

        static let interactSavePoint (field : Field) =
            let field = Field.restoreTeam field
            Field.save field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.SlotSound)) field

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => cmd ProcessKeyInput
             Screen.UpdateEvent => msg Update
             Screen.PostUpdateEvent => msg UpdateFieldTransition
             Screen.PostUpdateEvent => cmd UpdateEye
             Screen.SelectEvent => cmd PlayFieldSong]

        override this.Message (field, message, _, world) =

            match message with
            | Update ->

                // pull field state from avatar
                let avatar = Simulants.Field.Scene.Avatar.GetAvatar world
                let field = if avatar <> field.Avatar then Field.updateAvatar (constant avatar) field else field

                // update field time
                let field = Field.advanceUpdateTime field

                // update cue, resetting definitions if finished
                let (cue, definitions, (signals, field)) = FieldCue.run field.Cue field.Definitions field
                let field =
                    match cue with
                    | Cue.Nil -> Field.updateDefinitions (constant field.DefinitionsOriginal) field
                    | _ -> Field.updateDefinitions (constant definitions) field
                let field = Field.updateCue (constant cue) field

                // update dialog
                let field =
                    match field.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.update (flip detokenize field) dialog world
                        Field.updateDialogOpt (constant (Some dialog)) field
                    | None -> field

                // update portal
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        match tryGetTouchingPortal field.OmniSeedState field.Advents field.Avatar field.Props with
                        | Some (fieldType, destination, direction, isWarp) ->
                            if Option.isNone field.BattleOpt then // make sure we don't teleport if a battle is started earlier in the frame
                                let transition =
                                    { FieldType = fieldType
                                      FieldDestination = destination
                                      FieldDirection = direction
                                      FieldTransitionTime = field.UpdateTime + Constants.Field.TransitionTime }
                                let field = Field.updateFieldTransitionOpt (constant (Some transition)) field
                                let playSound =
                                    if isWarp
                                    then FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepWarpSound)
                                    else FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepStairSound)
                                (cmd playSound :: signals, field)
                            else (signals, field)
                        | None -> (signals, field)
                    | Some _ -> (signals, field)

                // update sensor
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        let sensors = getTouchedSensors field.Avatar field.Props
                        let results =
                            List.fold (fun (signals : Signal<FieldMessage, FieldCommand> list, field : Field) (sensorType, cue, requirements) ->
                                if field.Advents.IsSupersetOf requirements then
                                    let field = Field.updateCue (constant cue) field
                                    match sensorType with
                                    | AirSensor -> (signals, field)
                                    | HiddenSensor | StepPlateSensor -> (Command (FieldCommand.PlaySound (0L,  Constants.Audio.SoundVolumeDefault, Assets.Field.StepPlateSound)) :: signals, field)
                                else (signals, field))
                                (signals, field) sensors
                        results
                    | Some _ -> (signals, field)

                // update spirits
                let (signals, field) =
                    if  field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt then
                        match Field.updateSpirits field world with
                        | Left (battleData, field) ->
                            let clockTime = let t = World.getClockTime world in t.ToUnixTimeMilliseconds ()
                            let playTime = Option.defaultValue clockTime field.FieldSongTimeOpt
                            let startTime = clockTime - playTime
                            let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
                            let battle = Battle.makeFromTeam field.Inventory prizePool field.Team field.Options.BattleSpeed battleData world
                            let field = Field.clearSpirits field
                            let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                            let field = Field.updateBattleOpt (constant (Some battle)) field
                            let fade = cmd (FieldCommand.FadeOutSong 1000)
                            let beastGrowl = cmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound))
                            (fade :: beastGrowl :: signals, field)
                        | Right field -> (signals, field)
                    else (signals, field)

                // fin
                (signals, field)

            | UpdateFieldTransition ->

                // check if transitioning
                match field.FieldTransitionOpt with
                | Some fieldTransition ->

                    // handle field transition
                    let time = field.UpdateTime
                    let currentSongOpt = world |> World.getCurrentSongOpt |> Option.map (fun song -> song.Song)
                    let (signals, field) =

                        // start transition
                        if time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime then
                            match Data.Value.Fields.TryGetValue fieldTransition.FieldType with
                            | (true, fieldData) ->
                                match (currentSongOpt, fieldData.FieldSongOpt) with
                                | (Some song, Some song2) when assetEq song song2 -> just field
                                | (_, _) -> withCmd (FieldCommand.FadeOutSong Constants.Audio.FadeOutMsDefault) field
                            | (false, _) -> just field

                        // just past half-way point of transition
                        elif time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L + 1L then
                            let field = Field.updateFieldType (constant fieldTransition.FieldType) field world
                            let field =
                                Field.updateAvatar (fun avatar ->
                                    let avatar = Avatar.updateDirection (constant fieldTransition.FieldDirection) avatar
                                    let avatar = Avatar.updateIntersectedPropIds (constant []) avatar
                                    let avatar = Avatar.updateBottom (constant fieldTransition.FieldDestination) avatar
                                    avatar)
                                    field
                            let songCmd =
                                match Field.getFieldSongOpt field with
                                | Some fieldSong ->
                                    match currentSongOpt with
                                    | Some song when assetEq song fieldSong -> Nop
                                    | _ -> FieldCommand.PlaySong (0, Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, 0.0, fieldSong)
                                | None -> Nop
                            withCmd songCmd field

                        // finish transition
                        elif time = fieldTransition.FieldTransitionTime then
                            let startTime = let t = World.getClockTime world in t.ToUnixTimeMilliseconds ()
                            let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                            let field = Field.updateFieldTransitionOpt (constant None) field
                            just field

                        // intermediate state
                        else just field

                    // update field reference to make sure transition binding actuates
                    let field = Field.updateReference field
                    (signals, field)

                // no transition
                | None -> just field

            | MenuTeamOpen ->
                let state = MenuTeam { TeamIndex = 0; TeamIndices = Map.toKeyList field.Team }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field

            | MenuTeamAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTeam menuTeam -> MenuTeam { menuTeam with TeamIndex = index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field

            | MenuItemsOpen ->
                let itemState = MenuItem { ItemPage = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = itemState }) field
                just field

            | MenuItemsPageUp ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuItem menuItem -> { menu with MenuState = MenuItem { ItemPage = max 0 (dec menuItem.ItemPage) }}
                        | _ -> menu)
                        field
                just field

            | MenuInventoryPageDown ->
                let field =
                    Field.updateMenu (fun menu ->
                        match menu.MenuState with
                        | MenuItem menuItem -> { menu with MenuState = MenuItem { ItemPage = inc menuItem.ItemPage }}
                        | _ -> menu)
                        field
                just field

            | MenuItemSelect (index, (itemType, _)) ->
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = MenuUse.tryMakeFromSelection (index, itemType) }) field
                just field

            | MenuItemUse index ->
                match Map.tryFind index field.Team with
                | Some teammate ->
                    match field.Menu.MenuUseOpt with
                    | Some menuUse ->
                        let itemType = snd menuUse.MenuUseSelection
                        let (result, displacedOpt, teammate) = Teammate.tryUseItem itemType teammate
                        let field = if result then Field.updateInventory (Inventory.tryRemoveItem itemType >> snd) field else field
                        let field = match displacedOpt with Some displaced -> Field.updateInventory (Inventory.tryAddItem displaced >> snd) field | None -> field
                        let field = Field.updateTeam (Map.add index teammate) field
                        let field = Field.updateMenu (constant { field.Menu with MenuUseOpt = None }) field
                        if result then withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)) field
                        else just field
                    | None -> just field
                | None -> just field

            | MenuItemCancel ->
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = None }) field
                just field

            | MenuTechOpen ->
                let state = MenuTech { TeammateIndex = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field
            
            | MenuTechAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTech menuTech -> MenuTech { menuTech with TeammateIndex = index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field
            
            | MenuTechSelect _ ->
                just field

            | MenuOptionsOpen ->
                let state = MenuOptions
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field

            | MenuOptionsSelectBattleSpeed battleSpeed ->
                let field = Field.updateOptions (constant { BattleSpeed = battleSpeed }) field
                just field
            
            | MenuClose ->
                let field = Field.updateMenu (fun menu -> { menu with MenuState = MenuClosed }) field
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

            | ShopSelect (index, (itemType, _)) ->
                let field =
                    Field.updateShopOpt (Option.map (fun shop ->
                        let buying = match shop.ShopState with ShopBuying -> true | ShopSelling -> false
                        let shopConfirmOpt = ShopConfirm.tryMakeFromSelection buying field.Inventory (index, itemType)
                        { shop with ShopConfirmOpt = shopConfirmOpt }))
                        field
                just field

            | ShopConfirmAccept ->
                match field.ShopOpt with
                | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        let itemType = snd shopConfirm.ShopConfirmSelection
                        let valid =
                            match shop.ShopState with
                            | ShopBuying ->
                                Inventory.canAddItem itemType field.Inventory &&
                                field.Inventory.Gold >= shopConfirm.ShopConfirmPrice
                            | ShopSelling -> true
                        if valid then
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.tryAddItem itemType >> snd | ShopSelling -> Inventory.tryRemoveItem itemType >> snd) field
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.updateGold (fun gold -> gold - shopConfirm.ShopConfirmPrice) | ShopSelling -> Inventory.updateGold (fun gold -> gold + shopConfirm.ShopConfirmPrice)) field
                            let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field
                        else withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)) field
                    | None -> just field
                | None -> just field

            | ShopConfirmDecline ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                just field

            | ShopLeave ->
                let field = Field.updateShopOpt (constant None) field
                just field

            | PromptLeft ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some ((_, promptCue), _) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | PromptRight ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some (_, (_, promptCue)) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | TryBattle (battleType, consequents) ->
                match Map.tryFind battleType Data.Value.Battles with
                | Some battleData ->
                    let clockTime = let t = World.getClockTime world in t.ToUnixTimeMilliseconds ()
                    let playTime = Option.defaultValue clockTime field.FieldSongTimeOpt
                    let startTime = clockTime - playTime
                    let prizePool = { Consequents = consequents; Items = []; Gold = 0; Exp = 0 }
                    let battle = Battle.makeFromTeam field.Inventory prizePool (Field.getParty field) field.Options.BattleSpeed battleData world
                    let field = Field.clearSpirits field
                    let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                    let field = Field.updateBattleOpt (constant (Some battle)) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound)) field
                | None -> just field

            | Interact ->
                match field.DialogOpt with
                | None ->
                    if isTouchingSavePoint field.Avatar field.Props then
                        interactSavePoint field
                    else
                        match tryGetFacingProp field.Avatar field.Props with
                        | Some prop ->
                            match prop.PropData with
                            | Sprite _ -> just field
                            | Portal _ -> just field
                            | Door (_, keyItemTypeOpt, cue, _, requirements) -> interactDoor keyItemTypeOpt cue requirements prop field
                            | Chest (_, itemType, chestId, battleTypeOpt, cue, requirements) -> interactChest itemType chestId battleTypeOpt cue requirements prop field
                            | Switch (_, cue, cue2, requirements) -> interactSwitch cue cue2 requirements prop field
                            | Sensor _ -> just field
                            | Character (_, _, _, _, cue, _) -> interactCharacter cue prop field
                            | Npc (_, _, cue, requirements) -> interactNpc [{ Cue = cue; Requirements = Set.empty }] requirements prop field
                            | NpcBranching (_, _, branches, requirements) -> interactNpc branches requirements prop field
                            | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType prop field
                            | Seal (_, cue, _) -> interactSeal cue field
                            | Flame _ -> just field
                            | SavePoint -> just field
                            | ChestSpawn -> just field
                            | EmptyProp -> just field
                        | None -> just field
                | Some dialog ->
                    interactDialog dialog field

        override this.Command (field, command, screen, world) =

            match command with
            | ProcessKeyInput ->
                if  Option.isNone field.FieldTransitionOpt &&
                    Simulants.Field.Scene.Feeler.GetTouched world |> not then
                    let avatar = Simulants.Field.Scene.Avatar
                    let force = v3Zero
                    let force = if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then v3 Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then v3 -Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then v3 0.0f Constants.Field.AvatarWalkForce 0.0f + force else force
                    let force = if World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then v3 0.0f -Constants.Field.AvatarWalkForce 0.0f + force else force
                    let world = avatar.Signal<Avatar, AvatarMessage, AvatarCommand> (cmd (TryTravel force)) world
                    let signal =
                        if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then msg (TryFace Rightward)
                        elif World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then msg (TryFace Leftward)
                        elif World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then msg (TryFace Upward)
                        elif World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then msg (TryFace Downward)
                        else msg Nil
                    let world = avatar.Signal<Avatar, AvatarMessage, AvatarCommand> signal world
                    just world
                else just world

            | ProcessTouchInput position ->
                if  Option.isNone field.FieldTransitionOpt && 
                    World.isKeyboardKeyUp KeyboardKey.Right world && World.isKeyboardKeyUp KeyboardKey.D world &&
                    World.isKeyboardKeyUp KeyboardKey.Left world && World.isKeyboardKeyUp KeyboardKey.A world &&
                    World.isKeyboardKeyUp KeyboardKey.Up world && World.isKeyboardKeyUp KeyboardKey.W world &&
                    World.isKeyboardKeyUp KeyboardKey.Down world && World.isKeyboardKeyUp KeyboardKey.S world then
                    let avatar = Simulants.Field.Scene.Avatar
                    let lowerCenter = field.Avatar.LowerCenter
                    let viewport = World.getViewport world
                    let eyePosition = World.getEyePosition2d world
                    let eyeSize = World.getEyeSize2d world
                    let position = viewport.MouseToWorld2d (false, position, eyePosition, eyeSize)
                    let heading = position.V3 - lowerCenter
                    if heading.Magnitude >= 6.0f then // TODO: make constant DeadZoneRadius.
                        let goalNormalized = Vector3.Normalize heading
                        let force = goalNormalized * Constants.Field.AvatarWalkForceMouse
                        let world = avatar.Signal<Avatar, AvatarMessage, AvatarCommand> (cmd (TryTravel force)) world
                        let world = avatar.Signal<Avatar, AvatarMessage, AvatarCommand> (msg (TryFace (Direction.ofVector3 heading))) world
                        just world
                    else just world
                else just world

            | UpdateEye ->
                if World.getUpdateRate world <> 0L then
                    let world = World.setEyePosition2d field.Avatar.Center.V2 world
                    let tileMapPerimeter2d = (Simulants.Field.Scene.TileMap.GetPerimeter world).Box2
                    let eyeBounds = tileMapPerimeter2d.WithPosition (tileMapPerimeter2d.Position + v2 48.0f 48.0f)
                    let eyeBounds = eyeBounds.WithSize (tileMapPerimeter2d.Size - v2 96.0f 96.0f)
                    let world = World.constrainEyeBounds2d eyeBounds world
                    just world
                else just world

            | PlayFieldSong ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match (fieldData.FieldSongOpt, World.getCurrentSongOpt world) with
                    | (Some fieldSong, Some currentSong) ->
                        if not (AssetTag.equals fieldSong currentSong.Song) then
                            let (playTime, startTime) =
                                let clockTime = let t = World.getClockTime world in t.ToUnixTimeMilliseconds ()
                                match field.FieldSongTimeOpt with
                                | Some playTime ->
                                    let deltaTime = clockTime - playTime
                                    if playTime < Constants.Audio.SongResumptionMaximum
                                    then (playTime, deltaTime)
                                    else (0L, clockTime)
                                | None -> (0L, clockTime)
                            let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInMs else 0
                            let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                            let world = screen.SetField field world
                            withCmd (FieldCommand.PlaySong (fadeIn, Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, double playTime / 1000.0, fieldSong)) world
                        else just world
                    | (Some fieldSong, None) ->
                        let (playTime, startTime) =
                            let clockTime = let t = World.getClockTime world in t.ToUnixTimeMilliseconds ()
                            match field.FieldSongTimeOpt with
                            | Some playTime ->
                                let deltaTime = clockTime - playTime
                                if playTime < Constants.Audio.SongResumptionMaximum
                                then (playTime, deltaTime)
                                else (0L, clockTime)
                            | None -> (0L, clockTime)
                        let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInMs else 0
                        let field = Field.updateFieldSongTimeOpt (constant (Some startTime)) field
                        let world = screen.SetField field world
                        withCmd (FieldCommand.PlaySong (fadeIn, Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, double playTime / 1000.0, fieldSong)) world
                    | (None, _) -> just world
                | (false, _) -> just world

            | FieldCommand.PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) delay screen world
                just world

            | FieldCommand.PlaySong (fadeIn, fadeOut, volume, start, assetTag) ->
                let world = World.playSong fadeIn fadeOut volume start assetTag world
                just world

            | FieldCommand.FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

            | Nop -> just world

        override this.Content (field, _) =

            [// scene group
             Content.group Simulants.Field.Scene.Group.Name []

                [// avatar
                 Content.entity<AvatarDispatcher> Simulants.Field.Scene.Avatar.Name
                    [Entity.Position == v3Zero; Entity.Elevation == Constants.Field.ForegroundElevation; Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Enabled :=
                        field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.Avatar := field.Avatar]

                 // props
                 for (index, prop) in field.Props.Pairs do // TODO: DIFF: memoize?

                    // prop
                    Content.entity<PropDispatcher> ("Prop+" + string index) [Entity.Prop := prop]

                 // spirit orb
                 if Field.hasEncounters field && Cue.isNil field.Cue then
                    Content.entity<SpiritOrbDispatcher> "SpiritOrb"
                        [Entity.Position == v3 -448.0f 48.0f 0.0f; Entity.Elevation == Constants.Field.SpiritOrbElevation; Entity.Size == v3 192.0f 192.0f 0.0f
                         Entity.SpiritOrb := { AvatarLowerCenter = field.Avatar.LowerCenter; Spirits = field.Spirits; Chests = Field.getChests field; Portals = Field.getNonWarpPortals field }]

                 // backdrop sprite
                 Content.staticSprite "Backdrop"
                    [Entity.Perimeter := field.ViewBoundsAbsolute.Box3; Entity.Elevation == Single.MinValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.Image9
                     Entity.Color :=
                        match Data.Value.Fields.TryGetValue field.FieldType with
                        | (true, fieldData) -> fieldData.FieldBackgroundColor
                        | (false, _) -> Color.Black]

                 // transition fade sprite
                 Content.staticSprite "Fade"
                    [Entity.Perimeter := field.ViewBoundsAbsolute.Box3; Entity.Elevation == Single.MaxValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.Image8
                     Entity.Visible := Option.isSome field.FieldTransitionOpt
                     Entity.Color :=
                        match field.FieldTransitionOpt with
                        | Some transition ->
                            let time = field.UpdateTime
                            let deltaTime = single transition.FieldTransitionTime - single time
                            let halfTransitionTime = single Constants.Field.TransitionTime * 0.5f
                            let progress =
                                if deltaTime < halfTransitionTime
                                then deltaTime / halfTransitionTime
                                else 1.0f - (deltaTime - halfTransitionTime) / halfTransitionTime
                            Color.Black.WithA progress
                        | None -> Color.Zero]

                 // tmx map
                 Content.tmxMap Simulants.Field.Scene.TileMap.Name
                    [Entity.Elevation == Constants.Field.BackgroundElevation
                     Entity.TmxMap :=
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                            | Some tileMapChc ->
                                match tileMapChc with
                                | Choice1Of3 tileMap
                                | Choice2Of3 (tileMap, _)
                                | Choice3Of3 (tileMap, _) -> tileMap
                            | None -> failwithumf ()
                        | None -> failwithumf ()
                     Entity.TileIndexOffset :=
                         match Map.tryFind field.FieldType Data.Value.Fields with
                         | Some fieldData -> fieldData.FieldTileIndexOffset
                         | None -> failwithumf ()
                     Entity.TileIndexOffsetRange :=
                         match Map.tryFind field.FieldType Data.Value.Fields with
                         | Some fieldData -> fieldData.FieldTileIndexOffsetRange
                         | None -> failwithumf ()
                     Entity.TileLayerClearance == 10.0f]

                 // tmx map fade
                 Content.tmxMap "TileMapFade"
                    [Entity.Elevation == Constants.Field.BackgroundElevation + 0.5f
                     Entity.Color :=
                        (let progress = 1.0f - (Constants.Field.ConnectorFadeYMax - field.Avatar.Bottom.Y) / Constants.Field.ConnectorFadeYMax
                         let fade = min 1.0f progress
                         Color.One.ScaleA fade)
                     Entity.TmxMap :=
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                           match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                           | Some tileMapChc ->
                               match tileMapChc with
                               | Choice1Of3 _ -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                               | Choice2Of3 (_, tileMapFade) -> tileMapFade
                               | Choice3Of3 (_, _) ->  Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                           | None -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                        | None -> Metadata.getTileMapMetadata Assets.Default.TileMapEmpty |> __c
                     Entity.TileLayerClearance == 10.0f]

                 // feeler
                 Content.feeler Simulants.Field.Scene.Feeler.Name
                    [Entity.Position == -Constants.Render.ResolutionF.V3 * 0.5f; Entity.Elevation == Constants.Field.FeelerElevation; Entity.Size == Constants.Render.ResolutionF.V3
                     Entity.TouchingEvent =|> fun evt -> cmd (ProcessTouchInput evt.Data)]

                 // menu button
                 Content.button "Menu"
                    [Entity.Position == v3 -456.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Text == "Menu"
                     Entity.Visible :=
                        field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.ClickEvent => msg MenuTeamOpen]

                 // interact button
                 Content.button "Interact"
                    [Entity.Position == v3 306.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible :=
                        field.Menu.MenuState = MenuClosed &&
                        (Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Option.isSome (tryGetInteraction field.DialogOpt field.Advents field.Avatar field.Props (flip detokenize field))
                     Entity.Text :=
                        match tryGetInteraction field.DialogOpt field.Advents field.Avatar field.Props (flip detokenize field) with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent => msg Interact]

                 // dialog
                 yield! Content.dialog "Dialog"
                    Constants.Field.GuiElevation PromptLeft PromptRight (flip detokenize field)
                    (match field.DialogOpt with Some dialog -> Some dialog | None -> None)

                 // menu
                 match field.Menu.MenuState with // TODO: DIFF: memoize?
                 | MenuTeam menuTeam ->

                    // team
                    Content.panel "Team"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuItemsOpen) (fun () -> MenuTechOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field tautology2 MenuTeamAlly
                         Content.label "Portrait"
                            [Entity.PositionLocal == v3 438.0f 288.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 192.0f 0.0f
                             Entity.LabelImage :=
                                match MenuTeam.tryGetTeamData field.Team menuTeam with
                                | Some characterData ->
                                    match characterData.PortraitOpt with
                                    | Some portrait -> portrait
                                    | None -> Assets.Default.ImageEmpty
                                | None -> Assets.Default.ImageEmpty]
                         Content.text "CharacterType"
                            [Entity.PositionLocal == v3 650.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeamData field.Team menuTeam with
                                | Some characterData -> CharacterType.getName characterData.CharacterType
                                | None -> ""]
                         Content.text "ArchetypeType"
                            [Entity.PositionLocal == v3 650.0f 336.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> string teammate.ArchetypeType + " Lv." + string (Algorithms.expPointsToLevel teammate.ExpPoints)
                                | None -> ""]
                         Content.text "Weapon"
                            [Entity.PositionLocal == v3 444.0f 234.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Wpn: " + Option.mapOrDefaultValue string "None" teammate.WeaponOpt
                                | None -> ""]
                         Content.text "Armor"
                            [Entity.PositionLocal == v3 444.0f 204.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Amr: " + Option.mapOrDefaultValue string "None" teammate.ArmorOpt
                                | None -> ""]
                         Content.text "Accessory"
                            [Entity.PositionLocal == v3 444.0f 174.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate -> "Acc: " + Option.mapOrDefaultValue string "None" (List.tryHead teammate.Accessories)
                                | None -> ""]
                         Content.text "Stats"
                            [Entity.PositionLocal == v3 444.0f -84.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 512.0f 256.0f 0.0f
                             Entity.Justification == Unjustified true
                             Entity.Text :=
                                match MenuTeam.tryGetTeammateAndTeamData field.Team menuTeam with
                                | Some (teammate, characterData) ->
                                    let level = Algorithms.expPointsToLevel teammate.ExpPoints
                                    let hpm = Algorithms.hitPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                    let tpm = Algorithms.techPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                    let pow = Algorithms.power teammate.WeaponOpt Map.empty characterData.ArchetypeType level // no statuses outside battle
                                    let mag = Algorithms.magic teammate.WeaponOpt Map.empty characterData.ArchetypeType level // no statuses outside battle
                                    let def = Algorithms.defense teammate.Accessories Map.empty characterData.ArchetypeType level // no statuses outside battle
                                    let abs = Algorithms.absorb characterData.AbsorbCreep teammate.Accessories Map.empty characterData.ArchetypeType level // no statuses outside battle
                                    "HP  "   + (string teammate.HitPoints).PadLeft 3 + " /" + (string hpm).PadLeft 3 +
                                    "\nTP  " + (string teammate.TechPoints).PadLeft 3 + " /" + (string tpm).PadLeft 3 +
                                    "\nPow " + (string pow).PadLeft 3 + "    Mag " + (string mag).PadLeft 3 +
                                    "\nDef " + (string def).PadLeft 3 + "    Abs " + (string abs).PadLeft 3 +
                                    "\nExp " + (string teammate.ExpPoints).PadLeft 3 + " /" + (string (Algorithms.expPointsForNextLevel teammate.ExpPoints)).PadLeft 3
                                | None -> ""]]

                 // inventory
                 | MenuItem _ ->
                    Content.panel "Inventory"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage
                         Entity.Enabled := Option.isNone field.Menu.MenuUseOpt]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuItemsOpen) (fun () -> MenuTechOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.items (v3 138.0f 417.0f 0.0f) 10 5 field MenuItemSelect
                         Content.text "Gold"
                            [Entity.PositionLocal == v3 399.0f 24.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text := string field.Inventory.Gold + "G"]
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := Content.pageItems 10 field |> a__
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => msg MenuItemsPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 777.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := Content.pageItems 10 field |> _b_
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => msg MenuInventoryPageDown]]

                 // tech team
                 | MenuTech _ ->
                    Content.panel "TechTeam"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuItemsOpen) (fun () -> MenuTechOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field tautology2 MenuTechAlly
                         yield! Content.techs (v3 466.0f 429.0f 0.0f) field MenuTechSelect]

                 // options
                 | MenuOptions ->
                    Content.panel "Options"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage]
                        [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuItemsOpen) (fun () -> MenuTechOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                         Content.text "BattleSpeed"
                            [Entity.PositionLocal == v3 384.0f 432.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text == "Battle Speed"]
                         Content.radioButton "Wait"
                            [Entity.PositionLocal == v3 180.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage; Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Wait"
                             Entity.Dialed := match field.Options.BattleSpeed with WaitSpeed -> true | _ -> false
                             Entity.DialedEvent => msg (MenuOptionsSelectBattleSpeed WaitSpeed)]
                         Content.radioButton "Paced"
                            [Entity.PositionLocal == v3 408.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage; Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Paced"
                             Entity.Dialed := match field.Options.BattleSpeed with PacedSpeed -> true | _ -> false
                             Entity.DialedEvent => msg (MenuOptionsSelectBattleSpeed PacedSpeed)]
                         Content.radioButton "Swift"
                            [Entity.PositionLocal == v3 636.0f 372.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage; Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Swift"
                             Entity.Dialed := match field.Options.BattleSpeed with SwiftSpeed -> true | _ -> false
                             Entity.DialedEvent => msg (MenuOptionsSelectBattleSpeed SwiftSpeed)]]

                 // closed
                 | MenuClosed -> ()

                 // use
                 match field.Menu.MenuUseOpt with
                 | Some menuUse ->
                    Content.panel "Use"
                        [Entity.Position == v3 -450.0f -216.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 432.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXLImage]
                        [yield! Content.team (v3 160.0f 183.0f 0.0f) 3 field
                            (fun teammate menu ->
                                match menu.MenuUseOpt with
                                | Some menuUse -> Teammate.canUseItem (snd menuUse.MenuUseSelection) teammate
                                | None -> false)
                            MenuItemUse
                         Content.button "Close"
                            [Entity.PositionLocal == v3 810.0f 342.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickEvent => msg MenuItemCancel]
                         Content.text "Line1"
                            [Entity.PositionLocal == v3 36.0f 354.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine1]
                         Content.text "Line2"
                            [Entity.PositionLocal == v3 66.0f 312.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine2]
                         Content.text "Line3"
                            [Entity.PositionLocal == v3 66.0f 270.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Text := menuUse.MenuUseLine3]]
                 | None -> ()

                 // shop
                 match field.ShopOpt with // TODO: DIFF: memoize?
                 | Some shop ->
                    let items = Content.pageItems 8 field
                    Content.panel "Shop"
                        [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                         Entity.LabelImage == Assets.Gui.DialogXXLImage
                         Entity.Enabled == Option.isNone shop.ShopConfirmOpt]
                        [yield! Content.items (v3 96.0f 347.0f 0.0f) 8 4 field ShopSelect
                         Content.button "Buy"
                            [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Buy"
                             Entity.VisibleLocal := shop.ShopState = ShopSelling
                             Entity.ClickEvent => msg ShopBuy]
                         Content.text "BuyWhat"
                            [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Buy what?"
                             Entity.VisibleLocal := shop.ShopState = ShopBuying]
                         Content.button "Sell"
                            [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Sell"
                             Entity.VisibleLocal := shop.ShopState = ShopBuying
                             Entity.ClickEvent => msg ShopSell]
                         Content.text "SellWhat"
                            [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Sell what?"
                             Entity.VisibleLocal := shop.ShopState = ShopSelling]
                         Content.button "Leave"
                            [Entity.PositionLocal == v3 678.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                             Entity.Text == "Leave"
                             Entity.ClickEvent => msg ShopLeave]
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 24.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := a__ items
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => msg ShopPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 804.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := _b_ items
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => msg ShopPageDown]
                         Content.text "Gold"
                            [Entity.PositionLocal == v3 352.0f 3.0f 0.0f; Entity.ElevationLocal == 1.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text := string field.Inventory.Gold + "G"]]
                 | None -> ()

                 // confirm
                 match field.ShopOpt with
                 | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        Content.panel "Dialog"
                           [Entity.Position == v3 -450.0f -128.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 252.0f 0.0f
                            Entity.LabelImage == Assets.Gui.DialogFatImage]
                           [Content.button "Accept"
                               [Entity.PositionLocal == v3 198.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                                Entity.Text == "Accept"
                                Entity.ClickEvent => msg ShopConfirmAccept]
                            Content.button "Decline"
                               [Entity.PositionLocal == v3 498.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                                Entity.Text == "Decline"
                                Entity.ClickEvent => msg ShopConfirmDecline]
                            Content.text "Offer"
                               [Entity.PositionLocal == v3 30.0f 180.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmOffer]
                            Content.text "Line1"
                               [Entity.PositionLocal == v3 60.0f 138.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmLine1]
                            Content.text "Line2"
                               [Entity.PositionLocal == v3 60.0f 96.0f 0.0f; Entity.ElevationLocal == 1.0f
                                Entity.Text := shopConfirm.ShopConfirmLine2]]
                    | None -> ()
                 | None -> ()]]