﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module FieldExtensions =
    type Screen with
        member this.GetField world = this.GetModelGeneric<Field> world
        member this.SetField value world = this.SetModelGeneric<Field> value world
        member this.Field = this.ModelGeneric<Field> ()
        member this.QuitFieldEvent = Events.QuitFieldEvent --> this
        member this.CommencingBattleEvent = Events.CommencingBattleEvent --> this
        member this.CommenceBattleEvent = Events.CommenceBattleEvent --> this

type FieldDispatcher () =
    inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.empty)

    // HACK: override songs under special conditions. Technically this should be data-driven, but it may not be worth
    // doing so for this game.
    static let overrideSong (_ : FieldType) (_ : Advent Set) song =
        // NOTE: no special conditions in demo.
        song

    static let loadMetadata fieldType =
        match fieldType with
        | TombInner ->
            Metadata.loadMetadataPackage (nameof Castle)
        | _ -> ()

    static let preloadFields fieldType (field : Field) =
        match fieldType with
        | CastleConnector -> for i in 0 .. 2 do FieldData.tryGetTileMap field.OmniSeedState (Data.Value.Fields.[Castle i]) |> ignore
        | _ -> ()

    static let isIntersectedProp (collider : BodyShapeIndex) (collidee : BodyShapeIndex) world =
        let collideeEntity = collidee.BodyId.BodySource :?> Entity
        if (collider.BodyShapeIndex = Constants.Field.AvatarCollisionShapeIndex &&
            collideeEntity.GetExists world &&
            collideeEntity.Is<PropDispatcher> world &&
            match (collideeEntity.GetPropPlus world).Prop.PropData with
            | Portal _ -> true
            | Sensor _ -> true
            | _ -> false) then
            true
        elif (collider.BodyShapeIndex = Constants.Field.AvatarSensorShapeIndex &&
              collideeEntity.GetExists world &&
              collideeEntity.Is<PropDispatcher> world &&
              match (collideeEntity.GetPropPlus world).Prop.PropData with
              | Portal _ -> false
              | Sensor _ -> false
              | _ -> true) then
            true
        else false

    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world || Simulants.Battle.GetSelected world
        then Field.initial world.UpdateTime Slot1
        else Field.empty

    override this.Definitions (field, _) =
        [Screen.UpdateEvent => Update
         Screen.UpdateEvent => ProcessKeyInput
         Screen.PostUpdateEvent => UpdateFieldTransition
         Screen.PostUpdateEvent => UpdateEye
         Screen.PostUpdateEvent => UpdateAvatarBodyTracking
         Screen.TimeUpdateEvent => TimeUpdate
         Screen.SelectEvent => StartPlaying
         Screen.IncomingStartEvent => ScreenTransitioning true
         Screen.IncomingFinishEvent => ScreenTransitioning false
         Screen.OutgoingStartEvent => ScreenTransitioning true
         Screen.OutgoingFinishEvent => ScreenTransitioning false
         match field.FieldState with
         | Playing -> Screen.DeselectingEvent => FinishQuitting
         | Battling (battleData, prizePool) -> Screen.DeselectingEvent => CommenceBattle (battleData, prizePool) | _ -> ()
         Simulants.FieldAvatar.BodyTransformEvent =|> fun evt -> AvatarBodyTransform evt.Data |> signal
         Simulants.FieldAvatar.BodyPenetrationEvent =|> fun evt -> AvatarBodyPenetration evt.Data |> signal
         Simulants.FieldAvatar.BodySeparationExplicitEvent =|> fun evt -> AvatarBodySeparationExplicit evt.Data |> signal
         Simulants.FieldAvatar.BodySeparationImplicitEvent =|> fun evt -> AvatarBodySeparationImplicit evt.Data |> signal
#if DEV
         Game.AssetsReloadEvent => ReloadProps
#endif
         ]

    override this.Message (field, message, _, world) =

        match message with
        | Update ->

            // update field
            Field.update field

        | TimeUpdate ->

            // update field time
            Field.updateFieldTime field

        | UpdateFieldTransition ->

            // check if field transitioning
            match field.FieldTransitionOpt with
            | Some fieldTransition ->

                // handle field
                match Data.Value.Fields.TryGetValue fieldTransition.FieldType with
                | (true, destinationData) ->

                    // start field transition
                    let time = field.FieldTime
                    let currentSongOpt = world |> World.getSongOpt |> Option.map (fun song -> song.Song)
                    if time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime then

                        // attempt to look up destination song
                        let desinationSongOpt =
                            match destinationData.FieldSongOpt with
                            | Some destinationSong -> Some (overrideSong fieldTransition.FieldType field.Advents destinationSong)
                            | None -> None
                        match (currentSongOpt, desinationSongOpt) with
                        | (Some song, Some song2) when assetEq song song2 -> just field
                        | (_, _) -> withSignal (FieldCommand.FadeOutSong 30L) field

                    // half-way transition (fully blacked out)
                    elif time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L + 1L then

                        // load metadata and pre-generate fields
                        match destinationData.FieldType with
                        | TombInner ->
                            Metadata.loadMetadataPackage (nameof Castle)
                        | CastleConnector ->
                            for i in 0 .. 2 do FieldData.tryGetTileMap field.OmniSeedState (Data.Value.Fields.[Castle i]) |> ignore
                        | _ -> ()

                        // transition field
                        loadMetadata destinationData.FieldType
                        preloadFields destinationData.FieldType field 
                        let field = Field.setFieldType world.UpdateTime fieldTransition.FieldType field
                        let field = Field.mapAvatar (Avatar.setDirection fieldTransition.FieldDirection) field
                        let warpAvatar = WarpAvatar fieldTransition.FieldDestination
                        let songCmd =
                            match Field.getFieldSongOpt field with
                            | Some fieldSong ->
                                let fieldSong = overrideSong field.FieldType field.Advents fieldSong
                                match currentSongOpt with
                                | Some song when assetEq song fieldSong -> Nop
                                | _ -> FieldCommand.PlaySong (0L, 30L, 0L, None, 0.5f, fieldSong)
                            | None -> Nop
                        withSignals [warpAvatar; songCmd] field

                    // finish field transition
                    elif time = fieldTransition.FieldTransitionTime then
                        let startTime = field.FieldTime
                        let field = Field.setFieldSongTimeOpt (Some startTime) field
                        let field = Field.setFieldTransitionOpt None field
                        just field

                    // intermediate field transition state
                    else just field

                // destination field not found
                | (false, _) -> just field

            // no field transition
            | None -> just field
        
        | UpdateAvatarBodyTracking ->

            // clear all temporary avatar body shapes
            let field = Field.mapAvatarCollidedPropIds (constant []) field
            let field = Field.mapAvatarSeparatedPropIds (constant []) field
            just field

        | AvatarBodyTransform transform ->

            // update avatar from transform if warped
            let time = world.UpdateTime
            let avatar = field.Avatar
            let avatar = { avatar with Perimeter = avatar.Perimeter.WithCenter transform.BodyCenter }
            let avatar =
                let direction = Direction.ofVector3Biased transform.BodyLinearVelocity
                let speed = transform.BodyLinearVelocity.Magnitude
                if speed > Constants.Field.AvatarIdleSpeedMax then
                    if direction <> avatar.Direction || avatar.CharacterAnimationType = IdleAnimation then
                        let avatar = Avatar.setDirection direction avatar
                        Avatar.animate time WalkAnimation avatar
                    else avatar
                else Avatar.animate time IdleAnimation avatar
            just (Field.mapAvatar (constant avatar) field)

        | AvatarBodyPenetration penetration ->

            // add collided body shape
            let field =
                if isIntersectedProp penetration.BodyShapePenetrator penetration.BodyShapePenetratee world then
                    let field = Field.mapAvatarCollidedPropIds (List.cons ((penetration.BodyShapePenetratee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                    let field = Field.mapAvatarIntersectedPropIds (List.cons ((penetration.BodyShapePenetratee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                    field
                else field
            just field

        | AvatarBodySeparationExplicit separation ->

            // add separated body shape
            let field =
                if isIntersectedProp separation.BodyShapeSeparator separation.BodyShapeSeparatee world then
                    let field = Field.mapAvatarSeparatedPropIds (List.cons ((separation.BodyShapeSeparatee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId) field
                    let field = Field.mapAvatarIntersectedPropIds (List.remove ((=) ((separation.BodyShapeSeparatee.BodyId.BodySource :?> Entity).GetPropPlus world).Prop.PropId)) field
                    field
                else field
            just field

        | AvatarBodySeparationImplicit separation ->

            // add separated body shape
            match separation.BodyId.BodySource with
            | :? Entity as entity when entity.Is<PropDispatcher> world ->
                let propId = (entity.GetPropPlus world).Prop.PropId
                let (separatedPropIds, intersectedPropIds) = List.split ((=) propId) field.AvatarIntersectedPropIds
                let field = Field.mapAvatarIntersectedPropIds (constant intersectedPropIds) field
                let field = Field.mapAvatarSeparatedPropIds ((@) separatedPropIds) field
                just field
            | _ -> just field

        | ScreenTransitioning transitioning ->
            let field = Field.setScreenTransitioning transitioning field
            just field

        | FinishQuitting ->
            just Field.empty

        | TryCommencingBattle (battleType, consequents) ->
            match Map.tryFind battleType Data.Value.Battles with
            | Some battleData ->
                let prizePool = { Consequents = consequents; Items = []; Gold = 0; Exp = 0 }
                let field = Field.commencingBattle battleData prizePool field
                withSignal (CommencingBattle battleData) field
            | None -> just field

        | MenuTeamOpen ->
            let state = MenuTeam { TeamIndex = 0; TeamIndices = Map.toKeyList field.Team; TeamEquipOpt = None }
            let field = Field.mapMenu (fun menu -> { menu with MenuState = state }) field
            just field

        | MenuAutoMapOpen ->
            let state = MenuAutoMap
            let field = Field.mapMenu (fun menu -> { menu with MenuState = state }) field
            just field

        | MenuTeamAlly index ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTeam menuTeam -> MenuTeam { menuTeam with TeamIndex = index }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuTeamEquip equipType ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTeam menuTeam ->
                            let equip = { EquipType = equipType; EquipPage = 0; EquipMenuUseOpt = None }
                            MenuTeam { menuTeam with TeamEquipOpt = Some equip }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuTeamEquipMenuUseOpen ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuTeam menuTeam ->
                        match menuTeam.TeamEquipOpt with
                        | Some equip ->
                            let teammate = field.Team.[menuTeam.TeamIndex]
                            let menuUseOpt =
                                match equip.EquipType with
                                | EquipWeapon _ ->
                                    match teammate.WeaponOpt with
                                    | Some weaponType ->
                                        match Data.Value.Weapons.TryFind weaponType with
                                        | Some weaponData -> Some (MenuUse.makeFromWeaponData (0, Equipment (WeaponType weaponType)) weaponData)
                                        | None -> None
                                    | None -> None
                                | EquipArmor _ ->
                                    match teammate.ArmorOpt with
                                    | Some armorType ->
                                        match Data.Value.Armors.TryFind armorType with
                                        | Some armorData -> Some (MenuUse.makeFromArmorData (0, Equipment (ArmorType armorType)) armorData)
                                        | None -> None
                                    | None -> None
                                | EquipAccessory _ ->
                                    match teammate.Accessories with
                                    | accessoryType :: _ ->
                                        match Data.Value.Accessories.TryFind accessoryType with
                                        | Some accessoryData -> Some (MenuUse.makeFromAccessoryData (0, Equipment (AccessoryType accessoryType)) accessoryData)
                                        | None -> None
                                    | [] -> None
                            let equip = { equip with EquipMenuUseOpt = menuUseOpt }
                            let menuTeam = { menuTeam with TeamEquipOpt = Some equip }
                            { menu with MenuState = MenuTeam menuTeam }
                        | None -> menu
                    | _ -> menu)
                    field
            just field

        | MenuTeamEquipMenuUseClose ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuTeam menuTeam ->
                        match menuTeam.TeamEquipOpt with
                        | Some equip ->
                            let equip = { equip with EquipMenuUseOpt = None }
                            let menuTeam = { menuTeam with TeamEquipOpt = Some equip }
                            { menu with MenuState = MenuTeam menuTeam }
                        | None -> menu
                    | _ -> menu)
                    field
            just field

        | MenuTeamEquipPageUp ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTeam menuTeam ->
                            match menuTeam.TeamEquipOpt with
                            | Some equip ->
                                let equip = { equip with EquipPage = max (dec equip.EquipPage) 0 }
                                MenuTeam { menuTeam with TeamEquipOpt = Some equip }
                            | None -> MenuTeam menuTeam
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuTeamEquipPageDown ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTeam menuTeam ->
                            match menuTeam.TeamEquipOpt with
                            | Some equip ->
                                let equip = { equip with EquipPage = inc equip.EquipPage }
                                MenuTeam { menuTeam with TeamEquipOpt = Some equip }
                            | None -> MenuTeam menuTeam
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuTeamEquipSelect (_, (itemType, _)) ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTeam menuTeam ->
                            match menuTeam.TeamEquipOpt with
                            | Some equip ->
                                let equipType =
                                    match equip.EquipType with
                                    | EquipWeapon _ -> EquipWeapon (Some (match itemType with Equipment (WeaponType weaponType) -> weaponType | _ -> failwithumf ()))
                                    | EquipArmor _ -> EquipArmor (Some (match itemType with Equipment (ArmorType armorType) -> armorType | _ -> failwithumf ()))
                                    | EquipAccessory _ -> EquipAccessory (Some (match itemType with Equipment (AccessoryType accessoryType) -> accessoryType | _ -> failwithumf ()))
                                let equip = { equip with EquipType = equipType }
                                MenuTeam { menuTeam with TeamEquipOpt = Some equip }
                            | None -> MenuTeam menuTeam
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuTeamEquipConfirm ->
            match field.Menu.MenuState with
            | MenuTeam menuTeam ->
                match menuTeam.TeamEquipOpt with
                | Some equip ->
                    match field.Team.TryGetValue menuTeam.TeamIndex with
                    | (true, teammate) ->
                        let inventory = field.Inventory
                        let (teammate, inventory) =
                            match equip.EquipType with
                            | EquipWeapon weaponTypeOpt ->
                                let inventory =
                                    match weaponTypeOpt with
                                    | Some weaponType -> Inventory.tryRemoveItem (Equipment (WeaponType weaponType)) inventory |> snd
                                    | None -> inventory
                                let inventory =
                                    match teammate.WeaponOpt with
                                    | Some weaponType -> Inventory.tryAddItem (Equipment (WeaponType weaponType)) inventory |> snd
                                    | None -> inventory
                                let teammate = Teammate.equipWeaponOpt weaponTypeOpt teammate
                                (teammate, inventory)
                            | EquipArmor armorTypeOpt ->
                                let inventory =
                                    match armorTypeOpt with
                                    | Some armorType -> Inventory.tryRemoveItem (Equipment (ArmorType armorType)) inventory |> snd
                                    | None -> inventory
                                let inventory =
                                    match teammate.ArmorOpt with
                                    | Some armorType -> Inventory.tryAddItem (Equipment (ArmorType armorType)) inventory |> snd
                                    | None -> inventory
                                let teammate = Teammate.equipArmorOpt armorTypeOpt teammate
                                (teammate, inventory)
                            | EquipAccessory accessoryTypeOpt ->
                                let inventory =
                                    match accessoryTypeOpt with
                                    | Some accessoryType -> Inventory.tryRemoveItem (Equipment (AccessoryType accessoryType)) inventory |> snd
                                    | None -> inventory
                                let inventory =
                                    match teammate.Accessories with
                                    | accessoryType :: _ -> Inventory.tryAddItem (Equipment (AccessoryType accessoryType)) inventory |> snd
                                    | [] -> inventory
                                let teammate = Teammate.equipAccessoryOpt accessoryTypeOpt teammate
                                (teammate, inventory)
                        let field = Field.mapTeammate (constant teammate) teammate.TeamIndex field
                        let field = Field.mapInventory (constant inventory) field
                        let field = Field.mapMenu (fun menu -> { menu with MenuState = MenuTeam { menuTeam with TeamEquipOpt = Some { equip with EquipMenuUseOpt = None }}}) field
                        just field
                    | (false, _) -> just field
                | None -> just field
            | _ -> just field

        | MenuTeamEquipCancel ->
            let field =
                Field.mapMenu (fun menu ->
                    let state = 
                        match menu.MenuState with
                        | MenuTeam menuTeam -> MenuTeam { menuTeam with TeamEquipOpt = None }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuInventoryOpen ->
            let inventoryState = MenuInventory { InventoryPage = 0 }
            let field = Field.mapMenu (fun menu -> { menu with MenuState = inventoryState }) field
            just field

        | MenuInventoryPageUp ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuInventory inventory -> { menu with MenuState = MenuInventory { InventoryPage = max 0 (dec inventory.InventoryPage) }}
                    | _ -> menu)
                    field
            just field

        | MenuInventoryPageDown ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuInventory menuInventory -> { menu with MenuState = MenuInventory { InventoryPage = inc menuInventory.InventoryPage }}
                    | _ -> menu)
                    field
            just field

        | MenuInventorySelect (index, (itemType, _)) ->
            let field = Field.mapMenu (fun menu -> { menu with MenuUseOpt = MenuUse.tryMakeFromSelection (index, itemType) }) field
            just field

        | MenuInventoryUse index ->
            match Map.tryFind index field.Team with
            | Some teammate ->
                match field.Menu.MenuUseOpt with
                | Some menuUse ->
                    let itemType = snd menuUse.MenuUseSelection
                    let (result, displacedOpt, teammate) = Teammate.tryUseItem itemType teammate
                    let field = if result then Field.mapInventory (Inventory.tryRemoveItem itemType >> snd) field else field
                    let field = match displacedOpt with Some displaced -> Field.mapInventory (Inventory.tryAddItem displaced >> snd) field | None -> field
                    let field = Field.mapTeam (Map.add index teammate) field
                    let field = Field.mapMenu (constant { field.Menu with MenuUseOpt = None }) field
                    if result then withSignal (ScheduleSound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)) field
                    else just field
                | None -> just field
            | None -> just field

        | MenuInventoryCancel ->
            let field = Field.mapMenu (fun menu -> { menu with MenuUseOpt = None }) field
            just field

        | MenuTechsOpen ->
            let state = MenuTechs { TeamIndex = 0; TechIndexOpt = None }
            let field = Field.mapMenu (fun menu -> { menu with MenuState = state }) field
            just field
        
        | MenuTechsAlly index ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTechs menuTech -> MenuTechs { menuTech with TeamIndex = index }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field
        
        | MenuTechsSelect techIndex ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTechs menuTech -> MenuTechs { menuTech with TechIndexOpt = Some techIndex }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field
        
        | MenuTechClose ->
            let field =
                Field.mapMenu (fun menu ->
                    let state =
                        match menu.MenuState with
                        | MenuTechs menuTech -> MenuTechs { menuTech with TechIndexOpt = None }
                        | state -> state
                    { menu with MenuState = state })
                    field
            just field

        | MenuKeyItemsOpen ->
            let inventoryState = MenuKeyItems { KeyItemsPage = 0 }
            let field = Field.mapMenu (fun menu -> { menu with MenuState = inventoryState }) field
            just field

        | MenuKeyItemsPageUp ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuKeyItems menuKeyItems -> { menu with MenuState = MenuKeyItems { menuKeyItems with KeyItemsPage = max 0 (dec menuKeyItems.KeyItemsPage) }}
                    | _ -> menu)
                    field
            just field

        | MenuKeyItemsPageDown ->
            let field =
                Field.mapMenu (fun menu ->
                    match menu.MenuState with
                    | MenuKeyItems menuKeyItems -> { menu with MenuState = MenuKeyItems { menuKeyItems with KeyItemsPage = inc menuKeyItems.KeyItemsPage }}
                    | _ -> menu)
                    field
            just field

        | MenuKeyItemsSelect (index, (itemType, _)) ->
            let field =
                Field.mapMenu (fun menu ->
                    { menu with MenuUseOpt = MenuUse.tryMakeFromSelection (index, itemType) })
                    field
            just field

        | MenuOptionsOpen ->
            let state = MenuOptions false
            let field = Field.mapMenu (fun menu -> { menu with MenuState = state }) field
            just field

        | MenuOptionsSelectBattleSpeed battleSpeed ->
            let field = Field.mapOptions (fun options -> { options with BattleSpeed = battleSpeed }) field
            just field

        | MenuOptionsSongVolumeDown ->
            let field = Field.mapOptions (fun options -> { options with SongVolume = max 0.0f (options.SongVolume - 0.05f) }) field
            World.setMasterSongVolume field.Options.SongVolume world
            just field

        | MenuOptionsSongVolumeUp ->
            let field = Field.mapOptions (fun options -> { options with SongVolume = min 1.0f (options.SongVolume + 0.05f) }) field
            World.setMasterSongVolume field.Options.SongVolume world
            just field

        | MenuOptionsQuitPrompt ->
            let field = Field.quitPrompt field
            just field

        | MenuOptionsQuitConfirm ->
            withSignals [FadeOutSong 60L; StartQuitting] field

        | MenuOptionsQuitCancel ->
            let field = Field.quitCancel field
            just field

        | MenuClose ->
            let field = Field.mapMenu (fun menu -> { menu with MenuState = MenuClosed }) field
            just field

        | PartyMenuOpen ->
            let field = Field.mapPartyMenu (fun partyMenu -> { partyMenu with PartyMenuState = PartyMenuOpened; PartyMenuSelections = [] }) field
            just field

        | PartyMenuSelect teamIndex ->
            let field =
                Field.mapPartyMenu (fun partyMenu ->
                    if not (List.contains teamIndex partyMenu.PartyMenuSelections)
                    then { partyMenu with PartyMenuSelections = partyMenu.PartyMenuSelections @ [teamIndex] }
                    else partyMenu)
                    field
            just field

        | PartyMenuDeselect teamIndex ->
            let field =
                Field.mapPartyMenu (fun partyMenu ->
                    { partyMenu with PartyMenuSelections = List.remove ((=) teamIndex) partyMenu.PartyMenuSelections })
                    field
            just field

        | PartyMenuClose ->
            let field = Field.arrangeTeam field.PartyMenu.PartyMenuSelections field
            let field = Field.mapPartyMenu (constant { PartyMenuState = PartyMenuClosed; PartyMenuSelections = [] }) field
            just field

        | ShopBuy ->
            let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopState = ShopBuying; ShopPage = 0 })) field
            just field

        | ShopSell ->
            let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopState = ShopSelling; ShopPage = 0 })) field
            just field

        | ShopPageUp ->
            let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopPage = max 0 (dec shop.ShopPage) })) field
            just field

        | ShopPageDown ->
            let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopPage = inc shop.ShopPage })) field
            just field

        | ShopSelect (index, (itemType, _)) ->
            let field =
                Field.mapShopOpt (Option.map (fun shop ->
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
                        let field = Field.mapInventory (match shop.ShopState with ShopBuying -> Inventory.tryAddItem itemType >> snd | ShopSelling -> Inventory.tryRemoveItem itemType >> snd) field
                        let field = Field.mapInventory (match shop.ShopState with ShopBuying -> Inventory.removeGold shopConfirm.ShopConfirmPrice | ShopSelling -> Inventory.addGold shopConfirm.ShopConfirmPrice) field
                        let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                        withSignal (ScheduleSound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field
                    else withSignal (ScheduleSound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)) field
                | None -> just field
            | None -> just field

        | ShopConfirmDecline ->
            let field = Field.mapShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
            just field

        | ShopLeave ->
            let field = Field.mapShopOpt (constant None) field
            just field

        | PromptLeft ->
            match field.DialogOpt with
            | Some dialog ->
                match dialog.DialogPromptOpt with
                | Some ((_, promptCue), _) ->
                    let field = Field.setDialogOpt None field
                    let field = Field.setCue promptCue field
                    just field
                | None -> just field
            | None -> just field

        | PromptRight ->
            match field.DialogOpt with
            | Some dialog ->
                match dialog.DialogPromptOpt with
                | Some (_, (_, promptCue)) ->
                    let field = Field.setDialogOpt None field
                    let field = Field.setCue promptCue field
                    just field
                | None -> just field
            | None -> just field

        | Interact ->
            let (signals, field) = Field.interact field
            (signals, field)

#if DEV
        | ReloadProps ->
            let field = Field.reloadProps field
            just field
#endif

    override this.Command (field, command, screen, world) =

        match command with
        | ProcessKeyInput ->
            if  field.FieldState = Playing &&
                field.Menu.MenuState = MenuClosed &&
                field.PartyMenu.PartyMenuState = PartyMenuClosed &&
                not field.ScreenTransitioning &&
                CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                Option.isNone field.DialogOpt &&
                Option.isNone field.ShopOpt &&
                Option.isNone field.FieldTransitionOpt &&
                not (Simulants.FieldFeeler.GetTouched world) then
                let force = v3Zero
                let force = if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then v3 Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                let force = if World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then v3 -Constants.Field.AvatarWalkForce 0.0f 0.0f + force else force
                let force = if World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then v3 0.0f Constants.Field.AvatarWalkForce 0.0f + force else force
                let force = if World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then v3 0.0f -Constants.Field.AvatarWalkForce 0.0f + force else force
                if force <> v3Zero then
                    World.applyBodyForce force None (Simulants.FieldAvatar.GetBodyId world) world
                let directionOpt =
                    if World.isKeyboardKeyDown KeyboardKey.Right world || World.isKeyboardKeyDown KeyboardKey.D world then Some Rightward
                    elif World.isKeyboardKeyDown KeyboardKey.Left world || World.isKeyboardKeyDown KeyboardKey.A world then Some Leftward
                    elif World.isKeyboardKeyDown KeyboardKey.Up world || World.isKeyboardKeyDown KeyboardKey.W world then Some Upward
                    elif World.isKeyboardKeyDown KeyboardKey.Down world || World.isKeyboardKeyDown KeyboardKey.S world then Some Downward
                    else None
                match directionOpt with
                | Some direction ->
                    let linearVelocity = World.getBodyLinearVelocity (Simulants.FieldAvatar.GetBodyId world) world
                    let speed = linearVelocity.Magnitude
                    let field =
                        if speed <= Constants.Field.AvatarIdleSpeedMax
                        then Field.mapAvatar (Avatar.setDirection direction) field
                        else field
                    screen.SetField field world
                | None -> ()

        | ProcessTouchInput position ->
            if  field.FieldState = Playing &&
                field.Menu.MenuState = MenuClosed &&
                field.PartyMenu.PartyMenuState = PartyMenuClosed &&
                not field.ScreenTransitioning &&
                CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                Option.isNone field.DialogOpt &&
                Option.isNone field.ShopOpt &&
                Option.isNone field.FieldTransitionOpt &&
                World.isKeyboardKeyUp KeyboardKey.Right world && World.isKeyboardKeyUp KeyboardKey.D world &&
                World.isKeyboardKeyUp KeyboardKey.Left world && World.isKeyboardKeyUp KeyboardKey.A world &&
                World.isKeyboardKeyUp KeyboardKey.Up world && World.isKeyboardKeyUp KeyboardKey.W world &&
                World.isKeyboardKeyUp KeyboardKey.Down world && World.isKeyboardKeyUp KeyboardKey.S world then
                let lowerCenter = field.Avatar.Perimeter.LowerCenter
                let viewport = World.getWindowViewport world
                let eyeCenter = World.getEye2dCenter world
                let eyeSize = World.getEye2dSize world
                let position = Viewport.mouseToWorld2d false eyeCenter eyeSize position viewport
                let heading = position.V3 - lowerCenter
                if heading.Magnitude >= 6.0f then // TODO: make constant DeadZoneRadius.
                    let goalNormalized = heading.Normalized
                    let force = goalNormalized * Constants.Field.AvatarWalkForceMouse
                    if force <> v3Zero then
                        World.applyBodyForce force None (Simulants.FieldAvatar.GetBodyId world) world
                    let linearVelocity = World.getBodyLinearVelocity (Simulants.FieldAvatar.GetBodyId world) world
                    let speed = linearVelocity.Magnitude
                    let field =
                        if speed <= Constants.Field.AvatarIdleSpeedMax
                        then Field.mapAvatar (Avatar.setDirection (Direction.ofVector3 heading)) field
                        else field
                    screen.SetField field world

        | UpdateEye ->
            if world.Advancing then
                World.setEye2dCenter field.Avatar.Perimeter.LowerCenter.V2 world
                let tileMapPerimeter2d = (Simulants.FieldTileMap.GetPerimeter world).Box2
                let eyeBounds = tileMapPerimeter2d.WithMin (tileMapPerimeter2d.Min + v2 48.0f 48.0f)
                let eyeBounds = eyeBounds.WithSize (tileMapPerimeter2d.Size - v2 96.0f 96.0f)
                World.constrainEye2dBounds eyeBounds world

        | WarpAvatar bottom ->
            let bodyBottomOffset = v3Up * Constants.Gameplay.CharacterSize.Y * 0.5f
            World.setBodyCenter (bottom + bodyBottomOffset) (Simulants.FieldAvatar.GetBodyId world) world

        | StartPlaying ->
            loadMetadata field.FieldType
            match Data.Value.Fields.TryGetValue field.FieldType with
            | (true, fieldData) ->
                match (fieldData.FieldSongOpt, World.getSongOpt world) with
                | (Some fieldSong, Some currentSong) ->
                    let fieldSong = overrideSong field.FieldType field.Advents fieldSong
                    if fieldSong <> currentSong.Song then
                        let (playTime, startTime) =
                            let time = field.FieldTime
                            match field.FieldSongTimeOpt with
                            | Some playTime ->
                                let startTime = time - playTime
                                if playTime < int64 Constants.Audio.SongResumptionMax
                                then (playTime, startTime)
                                else (0L, time)
                            | None -> (0L, time)
                        let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInTime else 0L
                        let field = Field.setFieldSongTimeOpt (Some startTime) field
                        screen.SetField field world
                        World.playSong fadeIn 30L playTime None 0.5f fieldSong world
                | (Some fieldSong, None) ->
                    let fieldSong = overrideSong field.FieldType field.Advents fieldSong
                    let (playTime, startTime) =
                        let time = field.FieldTime
                        match field.FieldSongTimeOpt with
                        | Some playTime ->
                            let startTime = time - playTime
                            if playTime < int64 Constants.Audio.SongResumptionMax
                            then (playTime, startTime)
                            else (0L, time)
                        | None -> (0L, time)
                    let fadeIn = if playTime <> 0L then Constants.Field.FieldSongFadeInTime else 0L
                    let field = Field.setFieldSongTimeOpt (Some startTime) field
                    screen.SetField field world
                    World.playSong fadeIn 30L playTime None 0.5f fieldSong world
                | (None, _) -> ()
            | (false, _) -> ()

        | StartQuitting ->
            World.publish () screen.QuitFieldEvent screen world

        | CommencingBattle _ ->
            World.publish () screen.CommencingBattleEvent screen world
            World.fadeOutSong 60L world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Field.BeastGrowlSound world

        | CommenceBattle (battleData, prizePool) ->
            World.publish (battleData, prizePool) screen.CommenceBattleEvent screen world

        | MenuOptionsToggleFullScreen ->
            if world.Unaccompanied then
                match World.tryGetWindowFullScreen world with
                | Some fullScreen -> World.trySetWindowFullScreen (not fullScreen) world
                | None -> ()

        | ScheduleSound (delay, volume, sound) ->
            World.schedule delay (fun world -> World.playSound volume sound world) screen world

        | PlaySong (fadeIn, fadeOut, start, repeatLimitOpt, volume, assetTag) ->
            World.playSong fadeIn fadeOut start repeatLimitOpt volume assetTag world

        | FadeOutSong fade ->
            World.fadeOutSong fade world

        | Nop -> ()

    override this.Content (field, _) =

        [// scene group
         Content.group Simulants.FieldScene.Name []

            [// avatar
             Content.entity<AvatarDispatcher> Simulants.FieldAvatar.Name
                [Entity.Elevation == Constants.Field.ForegroundElevation
                 Entity.Size == Constants.Gameplay.CharacterSize
                 Entity.Substance == Density 1.0f
                 Entity.Avatar := field.Avatar]

             // props
             for (index, prop) in field.Props.Pairs do
                Content.entity<PropDispatcher> ("Prop+" + string index)
                    [Entity.PropPlus := PropPlus.make field.FieldTime field.Avatar.Perimeter.Bottom field.Advents prop]

             // spirit orb
             if Field.hasEncounters field &&
                CueSystem.Cue.isFin field.Cue &&
                field.Menu.MenuState = MenuClosed then
                Content.entity<SpiritOrbDispatcher> "SpiritOrb"
                    [Entity.Position == v3 -448.0f 48.0f 0.0f; Entity.Elevation == Constants.Field.SpiritOrbElevation; Entity.Size == v3 192.0f 192.0f 0.0f
                     Entity.SpiritOrb :=
                        { AvatarLowerCenter = field.Avatar.Perimeter.LowerCenter
                          ShowUnopenedChests = Field.getShowUnopenedChests field
                          Chests = Field.getChests field
                          Portals = Field.getNonWarpPortals field
                          Narratives = Field.getNarratives field
                          Spirits = field.Spirits }]

             // backdrop sprite
             Content.staticSprite "Backdrop"
                [Entity.Position == v3 -480.0f -270.0f 0.0f; Entity.Size == v3 960.0f 540.0f 0.0f; Entity.Elevation == Single.MinValue
                 Entity.Absolute == true
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color :=
                    match Data.Value.Fields.TryGetValue field.FieldType with
                    | (true, fieldData) -> fieldData.FieldBackgroundColor
                    | (false, _) -> Color.Black]

             // tint sprite
             if field.Tint.A > 0.0f then
                Content.staticSprite "Tint"
                   [Entity.Position == v3 -480.0f -270.0f 0.0f; Entity.Size == v3 960.0f 540.0f 0.0f; Entity.Elevation == Constants.Field.TintElevation
                    Entity.Absolute == true
                    Entity.StaticImage == Assets.Default.White
                    Entity.Color := field.Tint]

             // transition fade sprite
             Content.staticSprite "Fade"
                [Entity.Position == v3 -480.0f -270.0f 0.0f; Entity.Size == v3 960.0f 540.0f 0.0f; Entity.Elevation == Single.MaxValue
                 Entity.Absolute == true
                 Entity.StaticImage == Assets.Default.Black
                 Entity.Visible := Option.isSome field.FieldTransitionOpt
                 Entity.Color :=
                    match field.FieldTransitionOpt with
                    | Some transition ->
                        let time = field.FieldTime
                        let localTime = single transition.FieldTransitionTime - single time
                        let halfTransitionTime = single Constants.Field.TransitionTime * 0.5f
                        let progress =
                            if localTime < halfTransitionTime then localTime / halfTransitionTime
                            elif localTime = dec halfTransitionTime || localTime = inc halfTransitionTime then 1.0f
                            else 1.0f - (localTime - halfTransitionTime) / halfTransitionTime
                        Color.Black.WithA progress
                    | None -> Color.Zero]

             // tmx map
             Content.tmxMap Simulants.FieldTileMap.Name
                [Entity.Elevation == Constants.Field.BackgroundElevation
                 Entity.TmxMap :=
                    match Map.tryFind field.FieldType Data.Value.Fields with
                    | Some fieldData ->
                        match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                        | Some tileMapChc ->
                            match tileMapChc with
                            | Choice1Of4 tileMap
                            | Choice2Of4 (tileMap, _)
                            | Choice3Of4 (_, tileMap, _)
                            | Choice4Of4 tileMap -> tileMap
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
                    (let progress = 1.0f - (Constants.Field.ConnectorFadeYMax - field.Avatar.Perimeter.Bottom.Y + Constants.Field.ConnectorFadeYMin) / Constants.Field.ConnectorFadeYMax
                     let fade = min 1.0f progress
                     Color.One.ScaleA fade)
                 Entity.TmxMap :=
                    match Map.tryFind field.FieldType Data.Value.Fields with
                    | Some fieldData ->
                       match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                       | Some tileMapChc ->
                           match tileMapChc with
                           | Choice1Of4 _ -> (Metadata.getTileMapMetadata Assets.Default.EmptyTileMap).TileMap
                           | Choice2Of4 (_, tileMapFade) -> tileMapFade
                           | Choice3Of4 (_, _, _) -> (Metadata.getTileMapMetadata Assets.Default.EmptyTileMap).TileMap
                           | Choice4Of4 _ -> (Metadata.getTileMapMetadata Assets.Default.EmptyTileMap).TileMap
                       | None -> (Metadata.getTileMapMetadata Assets.Default.EmptyTileMap).TileMap
                    | None -> (Metadata.getTileMapMetadata Assets.Default.EmptyTileMap).TileMap
                 Entity.TileLayerClearance == 10.0f]

             // feeler
             Content.feeler Simulants.FieldFeeler.Name
                [Entity.Position == -Constants.Render.DisplayVirtualResolution.V2.V3 * 0.5f; Entity.Elevation == Constants.Field.FeelerElevation; Entity.Size == Constants.Render.DisplayVirtualResolution.V2.V3
                 Entity.TouchingEvent =|> fun evt -> ProcessTouchInput evt.Data |> signal]

             // menu button
             Content.button "Menu"
                [Entity.Position == v3 -450.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                 Entity.UpImage == Assets.Gui.ButtonShortUpImage
                 Entity.DownImage == Assets.Gui.ButtonShortDownImage
                 Entity.Text == "Menu"
                 Entity.Visible :=
                    field.FieldState = Playing &&
                    field.Menu.MenuState = MenuClosed &&
                    field.PartyMenu.PartyMenuState = PartyMenuClosed &&
                    not field.ScreenTransitioning &&
                    CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
                    Option.isNone field.DialogOpt &&
                    Option.isNone field.ShopOpt &&
                    Option.isNone field.FieldTransitionOpt
                 Entity.ClickEvent => MenuTeamOpen]

             // party button
             Content.button "Party"
                [Entity.Position == v3 -72.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                 Entity.UpImage == Assets.Gui.ButtonShortUpImage
                 Entity.DownImage == Assets.Gui.ButtonShortDownImage
                 Entity.Visible :=
                    field.FieldState = Playing &&
                    field.Menu.MenuState = MenuClosed &&
                    field.PartyMenu.PartyMenuState = PartyMenuClosed &&
                    not field.ScreenTransitioning &&
                    (CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                    Option.isNone field.DialogOpt &&
                    Option.isNone field.ShopOpt &&
                    Option.isNone field.FieldTransitionOpt &&
                    Field.touchingSavePoint field &&
                    field.Team.Count >= 3
                 Entity.Text == "Party"
                 Entity.ClickEvent => PartyMenuOpen]

             // interact button
             Content.button "Interact"
                [Entity.Position == v3 306.0f -246.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                 Entity.UpImage == Assets.Gui.ButtonShortUpImage
                 Entity.DownImage == Assets.Gui.ButtonShortDownImage
                 Entity.Visible :=
                    field.FieldState = Playing &&
                    field.Menu.MenuState = MenuClosed &&
                    field.PartyMenu.PartyMenuState = PartyMenuClosed &&
                    not field.ScreenTransitioning &&
                    (CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                    Option.isNone field.ShopOpt &&
                    Option.isNone field.FieldTransitionOpt &&
                    Option.isSome (Field.tryGetInteraction field)
                 Entity.Text :=
                    match Field.tryGetInteraction field with
                    | Some interaction -> interaction
                    | None -> ""
                 Entity.ClickSoundOpt == None
                 Entity.ClickEvent => Interact]

             // dialog
             yield! Content.dialog "Dialog"
                Constants.Field.GuiElevation PromptLeft PromptRight (Field.detokenize field)
                (match field.DialogOpt with Some dialog -> Some dialog | None -> None)

             // menu
             match field.Menu.MenuState with
             | MenuTeam menuTeam ->

                // team
                Content.panel "Team"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage
                     Entity.Enabled := menuTeam.TeamEquipOpt.IsNone]
                    [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                     yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field (fun teammate menu ->
                        match menu.MenuState with
                        | MenuTeam team -> team.TeamIndex <> teammate.TeamIndex
                        | _ -> true)
                        MenuTeamAlly
                     Content.button "AutoMap"
                       [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 252.0f 72.0f 0.0f
                        Entity.EnabledLocal := Field.hasAutoMap field
                        Entity.UpImage == Assets.Field.ButtonAutoMapUpImage
                        Entity.DownImage == Assets.Field.ButtonAutoMapDownImage
                        Entity.TextColor == Color 0x93544CFFu
                        Entity.TextColorDisabled == Color 0x93544CC0u
                        Entity.Text == "Map"
                        Entity.ClickSoundOpt == Some Assets.Field.AutoMapSound
                        Entity.ClickEvent => MenuAutoMapOpen]
                     Content.label "Portrait"
                        [Entity.PositionLocal == v3 438.0f 345.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 144.0f 0.0f
                         Entity.BackdropImageOpt :=
                            match MenuTeam.tryGetCharacterData field.Team menuTeam with
                            | Some characterData ->
                                match characterData.PortraitOpt with
                                | Some portrait -> Some portrait
                                | None -> Some Assets.Default.EmptyImage
                            | None -> Some Assets.Default.EmptyImage]
                     Content.text "CharacterType"
                        [Entity.PositionLocal == v3 606.0f 414.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 32.0f 0.0f
                         Entity.Justification == Unjustified false
                         Entity.Text :=
                            match MenuTeam.tryGetCharacterData field.Team menuTeam with
                            | Some characterData -> characterData.CharacterType.Name
                            | None -> ""]
                     Content.text "ArchetypeType"
                        [Entity.PositionLocal == v3 606.0f 378.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 32.0f 0.0f
                         Entity.Justification == Unjustified false
                         Entity.Text :=
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> teammate.ArchetypeType.Name + " Lv." + string (Algorithms.expPointsToLevel teammate.ExpPoints)
                            | None -> ""]
                     Content.text "WeaponLabel"
                        [Entity.PositionLocal == v3 438.0f 291.0f 0.0f; Entity.ElevationLocal == 1.0f
                         Entity.Justification == Unjustified false
                         Entity.Text == "Wpn."]
                     Content.button "Weapon"
                        [Entity.PositionLocal == v3 513.0f 285.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 54.0f 0.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                         Entity.UpImage == Assets.Gui.ButtonSquishedUpImage
                         Entity.DownImage == Assets.Gui.ButtonSquishedDownImage
                         Entity.TextMargin == v2 15.0f 0.0f
                         Entity.Text :=
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> Option.mapOrDefaultValue (fun (w : WeaponType) -> w.Name) "None" teammate.WeaponOpt
                            | None -> ""
                         Entity.ClickEvent =>
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> MenuTeamEquip (EquipWeapon teammate.WeaponOpt) :> Signal
                            | None -> Nop]
                     Content.text "ArmorLabel"
                        [Entity.PositionLocal == v3 438.0f 234.0f 0.0f; Entity.ElevationLocal == 1.0f
                         Entity.Justification == Unjustified false
                         Entity.Text == "Amr."]
                     Content.button "Armor"
                        [Entity.PositionLocal == v3 513.0f 228.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 54.0f 0.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                         Entity.UpImage == Assets.Gui.ButtonSquishedUpImage
                         Entity.DownImage == Assets.Gui.ButtonSquishedDownImage
                         Entity.TextMargin == v2 15.0f 0.0f
                         Entity.Text :=
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> Option.mapOrDefaultValue (fun (a : ArmorType) -> a.Name) "None" teammate.ArmorOpt
                            | None -> ""
                         Entity.ClickEvent =>
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> MenuTeamEquip (EquipArmor teammate.ArmorOpt) :> Signal
                            | None -> Nop]
                     Content.text "AccessoryLabel"
                        [Entity.PositionLocal == v3 438.0f 177.0f 0.0f; Entity.ElevationLocal == 1.0f
                         Entity.Justification == Unjustified false
                         Entity.Text == "Acc."]
                     Content.button "Accessory"
                        [Entity.PositionLocal == v3 513.0f 171.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 54.0f 0.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                         Entity.UpImage == Assets.Gui.ButtonSquishedUpImage
                         Entity.DownImage == Assets.Gui.ButtonSquishedDownImage
                         Entity.TextMargin == v2 15.0f 0.0f
                         Entity.Text :=
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> Option.mapOrDefaultValue (fun (a : AccessoryType) -> a.Name) "None" (List.tryHead teammate.Accessories)
                            | None -> ""
                         Entity.ClickEvent =>
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate -> MenuTeamEquip (EquipAccessory (List.tryHead teammate.Accessories)) :> Signal
                            | None -> Nop]
                     Content.text "Stats"
                        [Entity.PositionLocal == v3 438.0f 32.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 432.0f 132.0f 0.0f
                         Entity.Justification == Unjustified true
                         Entity.Text :=
                            match MenuTeam.tryGetTeammate field.Team menuTeam with
                            | Some teammate ->
                                "HP   "   + (string teammate.HitPoints).PadRight 4 +  "/ " +    string teammate.HitPointsMax +
                                "\nTP   " + (string teammate.TechPoints).PadRight 4 + "/ " +    string teammate.TechPointsMax +
                                "\nPow. " + (string teammate.Power).PadRight 4 +      "Mag. " + string (teammate.Magic false) +
                                "\nDef. " + (string teammate.Defense).PadRight 4 +    "Abs. " + string teammate.Absorb +
                                "\nExp. " + (string teammate.ExpPoints).PadRight 3 +  " / " +   match Algorithms.expPointsForNextLevel teammate.ExpPoints with Int32.MaxValue -> "MAX" | next -> string next
                            | None -> ""]
                     Content.text "Gold"
                        [Entity.PositionLocal == v3 438.0f 0.0f 0.0f; Entity.ElevationLocal == 1.0f
                         Entity.Justification == Unjustified false
                         Entity.Text := string field.Inventory.Gold + "G"]]

                // equip
                match menuTeam.TeamEquipOpt with
                | Some equip when field.Team.ContainsKey menuTeam.TeamIndex ->
                    let teammate = field.Team.[menuTeam.TeamIndex]
                    let (changing, currentEquipmentName, teammate') =
                        match equip.EquipType with
                        | EquipWeapon weaponTypeOpt -> (teammate.WeaponOpt <> weaponTypeOpt, teammate.WeaponOpt |> Option.map _.Name |> Option.defaultValue "None", Teammate.equipWeaponOpt weaponTypeOpt teammate)
                        | EquipArmor armorTypeOpt -> (teammate.ArmorOpt <> armorTypeOpt, teammate.ArmorOpt |> Option.map _.Name |> Option.defaultValue "None", Teammate.equipArmorOpt armorTypeOpt teammate)
                        | EquipAccessory accessoryTypeOpt -> (teammate.Accessories <> Option.toList accessoryTypeOpt, teammate.Accessories |> List.map _.Name |> Seq.headOrDefault "None", Teammate.equipAccessoryOpt accessoryTypeOpt teammate)
                    Content.panel "Equip"
                        [Entity.Position == v3 -450.0f -177.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 351.0f 0.0f
                         Entity.BackdropImageOpt == Some Assets.Gui.DialogLargeImage
                         Entity.Enabled := Option.isNone equip.EquipMenuUseOpt]
                        [Content.text "Current"
                            [Entity.PositionLocal == v3 36.0f 282.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 384.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.Text := currentEquipmentName]
                         Content.button "Info"
                            [Entity.PositionLocal == v3 276.0f 282.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 96.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.VisibleLocal := currentEquipmentName <> "None" && not changing
                             Entity.TextMargin == v2 15.0f 0.0f
                             Entity.Text == "Info"
                             Entity.UpImage == Assets.Gui.ButtonTinyUpImage
                             Entity.DownImage == Assets.Gui.ButtonTinyDownImage
                             Entity.ClickEvent => MenuTeamEquipMenuUseOpen]
                         Content.label "Portrait"
                            [Entity.PositionLocal == v3 36.0f 129.0f 0.0f; Entity.ElevationLocal == 0.5f; Entity.Size == v3 144.0f 144.0f 0.0f
                             Entity.BackdropImageOpt :=
                                match MenuTeam.tryGetCharacterData field.Team menuTeam with
                                | Some characterData ->
                                    match characterData.PortraitOpt with
                                    | Some portrait -> Some portrait
                                    | None -> Some Assets.Default.EmptyImage
                                | None -> Some Assets.Default.EmptyImage]
                         Content.text "Stats"
                            [Entity.PositionLocal == v3 192.0f 129.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 144.0f 0.0f
                             Entity.Justification == Unjustified true
                             Entity.Text :=
                                match MenuTeam.tryGetTeammate field.Team menuTeam with
                                | Some teammate ->
                                    "HP   "   + (string teammate.HitPointsMax).PadRight 4 +     (if teammate.HitPointsMax <> teammate'.HitPointsMax then "> " +     string teammate'.HitPointsMax else "") +
                                    "\nTP   " + (string teammate.TechPointsMax).PadRight 4 +    (if teammate.TechPointsMax <> teammate'.TechPointsMax then "> " +   string teammate'.TechPointsMax else "") +
                                    "\nPow. " + (string teammate.Power).PadRight 4 +            (if teammate.Power <> teammate'.Power then "> " +                   string teammate'.Power else "") +
                                    "\nMag. " + (string $ teammate.Magic false).PadRight 4 +    (if teammate.Magic false <> teammate'.Magic false then "> " +       string (teammate'.Magic false) else "") +
                                    "\nDef. " + (string teammate.Defense).PadRight 4 +          (if teammate.Defense <> teammate'.Defense then "> " +               string teammate'.Defense else "") +
                                    "\nAbs. " + (string teammate.Absorb).PadRight 4 +           (if teammate.Absorb <> teammate'.Absorb then "> " +                 string teammate'.Absorb else "")
                                | None -> ""]
                         if changing then
                            Content.text "EquipLabel"
                                [Entity.PositionLocal == v3 36.0f 93.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 384.0f 32.0f 0.0f
                                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                 Entity.Text := "Equip?"]
                            Content.button "EquipButton"
                                [Entity.PositionLocal == v3 36.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 336.0f 72.0f 0.0f
                                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                 Entity.TextMargin == v2 15.0f 0.0f
                                 Entity.UpImage == Assets.Gui.ButtonLongUpImage
                                 Entity.DownImage == Assets.Gui.ButtonLongDownImage
                                 Entity.ClickSoundOpt == Some Assets.Field.EquipSound
                                 Entity.Text := equip.EquipType.ItemName
                                 Entity.ClickEvent => MenuTeamEquipConfirm]
                         Content.button "EquipCancel"
                            [Entity.PositionLocal == v3 810.0f 258.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickEvent => MenuTeamEquipCancel]
                         yield! Content.items (v3 462.0f 258.0f 0.0f) 3 3 field MenuTeamEquipSelect
                         Content.button "PageUp"
                            [Entity.PositionLocal == v3 462.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == "<"
                             Entity.VisibleLocal := Content.pageItems 3 field |> a__
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuTeamEquipPageUp]
                         Content.button "PageDown"
                            [Entity.PositionLocal == v3 726.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Text == ">"
                             Entity.VisibleLocal := Content.pageItems 3 field |> _b_
                             Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                             Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                             Entity.ClickEvent => MenuTeamEquipPageDown]]
                    match equip.EquipMenuUseOpt with
                    | Some menuUse ->
                        Content.panel "Info"
                            [Entity.Position == v3 -450.0f -128.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 20.0f; Entity.Size == v3 900.0f 252.0f 0.0f
                             Entity.BackdropImageOpt == Some Assets.Gui.DialogFatImage]
                            [Content.button "Close"
                                 [Entity.PositionLocal == v3 810.0f 162.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                                  Entity.UpImage == asset "Field" "CloseButtonUp"
                                  Entity.DownImage == asset "Field" "CloseButtonDown"
                                  Entity.ClickEvent => MenuTeamEquipMenuUseClose]
                             Content.text "Line1"
                                 [Entity.PositionLocal == v3 36.0f 174.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                                  Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                  Entity.Text := menuUse.MenuUseLine1]
                             Content.text "Line2"
                                 [Entity.PositionLocal == v3 66.0f 132.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                                  Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                  Entity.Text := menuUse.MenuUseLine2]
                             Content.text "Line3"
                                 [Entity.PositionLocal == v3 66.0f 90.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 810.0f 32.0f 0.0f
                                  Entity.Justification == Unjustified true
                                  Entity.Text := menuUse.MenuUseLine3]]
                    | None -> ()
                | Some _ | None -> ()

             // auto map
             | MenuAutoMap ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match FieldData.tryGetTileMap field.OmniSeedState fieldData with
                    | Some (Choice3Of4 (randMap, _, _)) ->
                        let mapSize = Constants.Field.RandMapSize.V3 * Constants.Field.AutoTileSize
                        let mapOffset = mapSize * -0.5f
                        let treasuresLeftPosition = v3 0.0f 126.0f 0.0f
                        let treasuresLeftSize = v3 0.0f 48.0f 0.0f
                        let treasuresLeftText = field |> Field.getChests |> Array.filter (not << _.Opened) |> Array.length |> fun count -> "Treasures Left: " + string count
                        Content.text "TreasuresLeft"
                            [Entity.Position == treasuresLeftPosition
                             Entity.Size == treasuresLeftSize
                             Entity.Elevation == Constants.Field.GuiElevation
                             Entity.Text := treasuresLeftText]
                        for x in 2.0f .. 2.0f .. 4.0f do
                            for y in 2.0f .. 2.0f .. 4.0f do
                                let offset = v3 -x -y 0.0f
                                Content.text ("TreasuresLeftDropShadow+" + scstring offset)
                                    [Entity.Position == treasuresLeftPosition + offset
                                     Entity.Size == treasuresLeftSize
                                     Entity.Elevation == Constants.Field.GuiElevation - 0.1f
                                     Entity.BackdropImageOpt == None
                                     Entity.Text := treasuresLeftText
                                     Entity.TextColor == Color.Black]
                        Content.staticSprite "AutoMap"
                            [Entity.Position == v3 -144.0f -144.0f 0.0f
                             Entity.Size == v3 288.0f 288.0f 0.0f
                             Entity.Elevation == Constants.Field.GuiElevation
                             Entity.Absolute == true
                             Entity.StaticImage == Assets.Field.AutoMapImage]
                        let autoMap = match field.AutoMaps.TryGetValue field.FieldType with (true, autoMap) -> autoMap | (false, _) -> Set.empty
                        for j in 0 .. dec Constants.Field.RandMapSize.Y do
                            for i in 0 .. dec Constants.Field.RandMapSize.X do
                                let index = v2i i j
                                if autoMap.Contains index then
                                    Content.staticSprite ("AutoTile+" + scstring index.X + "+" + scstring index.Y)
                                        [Entity.Position := index.V3 * Constants.Field.AutoTileSize + mapOffset
                                         Entity.Size == Constants.Field.AutoTileSize
                                         Entity.Elevation == Constants.Field.GuiElevation + 1.0f
                                         Entity.Absolute == true
                                         Entity.StaticImage := asset Assets.Field.PackageName (scstringMemo randMap.Segments.[dec Constants.Field.RandMapSize.Y - j].[i])]
                                if Some index = randMap.OriginOpt then
                                    Content.staticSprite "AutoOrigin"
                                        [Entity.Position :=
                                            index.MapY(fun y -> dec Constants.Field.RandMapSize.Y - y).V3 *
                                            Constants.Field.AutoTileSize +
                                            v3 3.0f 3.0f 0.0f +
                                            mapOffset
                                         Entity.Size == v3 18.0f 18.0f 0.0f
                                         Entity.Elevation == Constants.Field.GuiElevation + 2.0f
                                         Entity.Absolute == true
                                         Entity.StaticImage == Assets.Field.AutoOriginImage]
                        Content.staticSprite "AutoAvatar"
                            [Entity.Position :=
                                field.Avatar.Perimeter.BottomOffset5 /
                                Constants.Field.RoomSize.V3 /
                                Constants.Gameplay.TileSize *
                                Constants.Field.AutoTileSize +
                                v3 -7.5f -7.5f 0.0f +
                                mapOffset
                             Entity.Size == v3 15.0f 15.0f 0.0f
                             Entity.Elevation == Constants.Field.GuiElevation + 3.0f
                             Entity.Absolute == true
                             Entity.StaticImage == Assets.Field.AutoAvatarImage
                             Entity.Visible := field.FieldTime / 20L % 3L <> 0L]
                        Content.button "AutoBack"
                            [Entity.Position == v3 -78.0f -213.0f 0.0f
                             Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Elevation == Constants.Field.GuiElevation
                             Entity.UpImage == asset "Field" "BackButtonUp"
                             Entity.DownImage == asset "Field" "BackButtonDown"
                             Entity.ClickSoundOpt == Some Assets.Field.AutoMapSound
                             Entity.ClickEvent => MenuTeamOpen]
                        Content.button "AutoClose"
                            [Entity.Position == v3 6.0f -213.0f 0.0f
                             Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.Elevation == Constants.Field.GuiElevation
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickSoundOpt == Some Assets.Field.AutoMapSound
                             Entity.ClickEvent => MenuClose]
                    | Some _ | None -> ()
                | (false, _) -> ()

             // inventory
             | MenuInventory _ ->
                Content.panel "Inventory"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage
                     Entity.Enabled := Option.isNone field.Menu.MenuUseOpt]
                    [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                     yield! Content.items (v3 138.0f 417.0f 0.0f) 10 5 field MenuInventorySelect
                     Content.button "PageUp"
                        [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == "<"
                         Entity.VisibleLocal := Content.pageItems 10 field |> a__
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => MenuInventoryPageUp]
                     Content.button "PageDown"
                        [Entity.PositionLocal == v3 777.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == ">"
                         Entity.VisibleLocal := Content.pageItems 10 field |> _b_
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => MenuInventoryPageDown]]

             // techs
             | MenuTechs _ ->
                Content.panel "Techs"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage
                     Entity.Enabled := match field.Menu.MenuState with MenuTechs techs -> techs.TechIndexOpt.IsNone | _ -> true]
                    [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                     yield! Content.team (v3 138.0f 417.0f 0.0f) Int32.MaxValue field (fun teammate menu ->
                        match menu.MenuState with
                        | MenuTechs techs -> techs.TeamIndex <> teammate.TeamIndex
                        | _ -> true)
                        MenuTechsAlly
                     yield! Content.techs (v3 513.0f 417.0f 0.0f) field MenuTechsSelect]

             // key items
             | MenuKeyItems _ ->
                Content.panel "KeyItems"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage
                     Entity.Enabled := Option.isNone field.Menu.MenuUseOpt]
                    [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                     yield! Content.items (v3 138.0f 417.0f 0.0f) 10 5 field MenuKeyItemsSelect
                     Content.button "PageUp"
                        [Entity.PositionLocal == v3 138.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == "<"
                         Entity.VisibleLocal := Content.pageItems 10 field |> a__
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => MenuKeyItemsPageUp]
                     Content.button "PageDown"
                        [Entity.PositionLocal == v3 777.0f 12.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == ">"
                         Entity.VisibleLocal := Content.pageItems 10 field |> _b_
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => MenuKeyItemsPageDown]]

             // options
             | MenuOptions quitPrompt ->
                Content.panel "Options"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage]
                    [Content.sidebar "Sidebar" (v3 24.0f 417.0f 0.0f) field (fun () -> MenuTeamOpen) (fun () -> MenuInventoryOpen) (fun () -> MenuTechsOpen) (fun () -> MenuKeyItemsOpen) (fun () -> MenuOptionsOpen) (fun () -> MenuClose)
                     if not quitPrompt then
                        Content.text "BattleSpeed"
                            [Entity.PositionLocal == v3 336.0f 444.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Battle Speed"]
                        Content.radioButton "Wait"
                            [Entity.PositionLocal == v3 180.0f 390.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Wait"
                             Entity.Dialed := match field.Options.BattleSpeed with WaitSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed WaitSpeed]
                        Content.radioButton "Paced"
                            [Entity.PositionLocal == v3 408.0f 390.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Paced"
                             Entity.Dialed := match field.Options.BattleSpeed with PacedSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed PacedSpeed]
                        Content.radioButton "Swift"
                            [Entity.PositionLocal == v3 636.0f 390.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UndialedImage == Assets.Gui.ButtonShortUpImage
                             Entity.DialedImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Swift"
                             Entity.Dialed := match field.Options.BattleSpeed with SwiftSpeed -> true | _ -> false
                             Entity.DialedEvent => MenuOptionsSelectBattleSpeed SwiftSpeed]
                        Content.text "SongVolume"
                            [Entity.PositionLocal == v3 336.0f 336.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Song Volume"]
                        Content.button "SongVolumeDown"
                            [Entity.PositionLocal == v3 300.0f 282.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "-"
                             Entity.ClickEvent => MenuOptionsSongVolumeDown]
                        Content.text "SongVolumeInt"
                            [Entity.PositionLocal == v3 408.0f 282.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text := field.Options.SongVolume * 20.0f |> int |> string]
                        Content.button "SongVolumeUp"
                            [Entity.PositionLocal == v3 516.0f 282.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "+"
                             Entity.ClickEvent => MenuOptionsSongVolumeUp]
                        Content.text "FullScreen"
                            [Entity.PositionLocal == v3 336.0f 228.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Full Screen"]
                        Content.button "ToggleFullScreen"
                            [Entity.PositionLocal == v3 408.0f 174.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Toggle"
                             Entity.ClickEvent => MenuOptionsToggleFullScreen]
                        Content.text "QuitGame"
                            [Entity.PositionLocal == v3 336.0f 120.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Quit Game"]
                        Content.button "Quit"
                            [Entity.PositionLocal == v3 408.0f 66.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Quit"
                             Entity.ClickEvent => MenuOptionsQuitPrompt]
                        Content.text "About"
                            [Entity.PositionLocal == v3 262.0f 6.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 432.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Omni Blade Demo v1.0.9"]
                     else
                        Content.text "QuitConfirmation"
                            [Entity.PositionLocal == v3 336.0f 312.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 48.0f 0.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.Text == "Confirm Quit?"]
                        Content.button "QuitConfirm"
                            [Entity.PositionLocal == v3 252.0f 258.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Quit!"
                             Entity.ClickEvent => MenuOptionsQuitConfirm]
                        Content.button "QuitCancel"
                            [Entity.PositionLocal == v3 564.0f 258.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 144.0f 48.0f 0.0f
                             Entity.UpImage == Assets.Gui.ButtonShortUpImage
                             Entity.DownImage == Assets.Gui.ButtonShortDownImage
                             Entity.Text == "Cancel"
                             Entity.ClickEvent => MenuOptionsQuitCancel]]

             // closed
             | MenuClosed -> ()

             // party menu
             match field.PartyMenu.PartyMenuState with
             | PartyMenuOpened ->
                Content.panel "PartyMenu"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage]
                    [Content.button "Confirm"
                        [Entity.PositionLocal == v3 810.0f 420.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.EnabledLocal := field.PartyMenu.PartyMenuSelections.Length >= 3
                         Entity.UpImage == asset "Field" "ConfirmButtonUp"
                         Entity.DownImage == asset "Field" "ConfirmButtonDown"
                         Entity.ClickEvent => PartyMenuClose]
                     Content.text "Select"
                        [Entity.PositionLocal == v3 126.0f 429.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 32.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Select Party:"]
                     Content.text "Current"
                        [Entity.PositionLocal == v3 480.0f 429.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 288.0f 32.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Current Party:"]
                     for (teamIndex, teammate) in field.Team.Pairs do
                        let w =
                            match field.Menu.MenuState with
                            | MenuTechs _ -> 336.0f
                            | MenuTeam _ | _ -> 252.0f
                        let h = 72.0f
                        let x = 144.0f
                        let y = 339.0f - single teamIndex * 81.0f
                        let enabled = field.PartyMenu.PartyMenuSelections.Length < 3 && not (List.contains teamIndex field.PartyMenu.PartyMenuSelections)
                        Content.button ("Teammate+" + string teamIndex)
                            [Entity.PositionLocal == v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 w h 0.0f
                             Entity.EnabledLocal := enabled
                             Entity.Text := teammate.CharacterType.Name
                             Entity.UpImage == Assets.Gui.ButtonBigUpImage
                             Entity.DownImage == Assets.Gui.ButtonBigDownImage
                             Entity.ClickEvent => PartyMenuSelect teamIndex]
                     for i in 0 .. dec field.PartyMenu.PartyMenuSelections.Length do
                        let teamIndex = field.PartyMenu.PartyMenuSelections.[i]
                        let teammate = field.Team.[teamIndex]
                        let w =
                            match field.Menu.MenuState with
                            | MenuTechs _ -> 336.0f
                            | MenuTeam _ | _ -> 252.0f
                        let h = 72.0f
                        let x = 144.0f + 354.0f
                        let y = 339.0f - single i * 81.0f
                        Content.button ("Selected+" + string teamIndex)
                            [Entity.PositionLocal := v3 x y 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 w h 0.0f
                             Entity.Text := teammate.CharacterType.Name
                             Entity.UpImage == Assets.Gui.ButtonBigUpImage
                             Entity.DownImage == Assets.Gui.ButtonBigDownImage
                             Entity.ClickEvent => PartyMenuDeselect teamIndex]]
             | PartyMenuClosed -> ()

             // use
             match field.Menu.MenuUseOpt with
             | Some menuUse ->
                match menuUse.MenuUseSelection with
                | (_, Consumable _) ->
                    Content.panel "Use"
                        [Entity.Position == v3 -450.0f -216.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 432.0f 0.0f
                         Entity.BackdropImageOpt == Some Assets.Gui.DialogXLImage]
                        [Content.button "Close"
                            [Entity.PositionLocal == v3 810.0f 342.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickEvent => MenuInventoryCancel]
                         Content.text "Line1"
                            [Entity.PositionLocal == v3 36.0f 354.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.Text := menuUse.MenuUseLine1]
                         Content.text "Line2"
                            [Entity.PositionLocal == v3 66.0f 312.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.Text := menuUse.MenuUseLine2]
                         Content.text "Line3"
                            [Entity.PositionLocal == v3 66.0f 90.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 810.0f 32.0f 0.0f
                             Entity.Justification == Unjustified true
                             Entity.Text := menuUse.MenuUseLine3]
                         yield! Content.team (v3 160.0f 183.0f 0.0f) 3 field (fun teammate menu ->
                            match menu.MenuUseOpt with
                            | Some menuUse -> Teammate.canUseItem (snd menuUse.MenuUseSelection) teammate
                            | None -> false)
                            MenuInventoryUse]
                | (_, _) ->
                    Content.panel "Use"
                       [Entity.Position == v3 -450.0f -128.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 252.0f 0.0f
                        Entity.BackdropImageOpt == Some Assets.Gui.DialogFatImage]
                       [Content.button "Close"
                            [Entity.PositionLocal == v3 810.0f 162.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                             Entity.UpImage == asset "Field" "CloseButtonUp"
                             Entity.DownImage == asset "Field" "CloseButtonDown"
                             Entity.ClickEvent => MenuInventoryCancel]
                        Content.text "Line1"
                            [Entity.PositionLocal == v3 36.0f 174.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.Text := menuUse.MenuUseLine1]
                        Content.text "Line2"
                            [Entity.PositionLocal == v3 66.0f 132.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.Text := menuUse.MenuUseLine2]
                        Content.text "Line3"
                            [Entity.PositionLocal == v3 66.0f 90.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 810.0f 32.0f 0.0f
                             Entity.Justification == Unjustified true
                             Entity.Text := menuUse.MenuUseLine3]]
             | None -> ()

             // tech
             match field.Menu.MenuState with
             | MenuTechs techs ->
                match techs.TechIndexOpt with
                | Some techIndex ->
                    let techs =
                        match field.Menu.MenuState with
                        | MenuTechs menuTech ->
                            match Map.tryFind menuTech.TeamIndex field.Team with
                            | Some teammate -> teammate.Techs |> Seq.indexed |> Map.ofSeq
                            | None -> Map.empty
                        | _ -> Map.empty
                    match techs.TryGetValue techIndex with
                    | (true, techType) ->
                        match Data.Value.Techs.TryGetValue techType with
                        | (true, tech) ->
                            Content.panel "Tech"
                                [Entity.Position == v3 -450.0f -128.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v3 900.0f 252.0f 0.0f; Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                 Entity.BackdropImageOpt == Some Assets.Gui.DialogFatImage]
                                [Content.button "Close"
                                    [Entity.PositionLocal == v3 810.0f 162.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                                     Entity.UpImage == asset "Field" "CloseButtonUp"
                                     Entity.DownImage == asset "Field" "CloseButtonDown"
                                     Entity.ClickEvent => MenuTechClose]
                                 Content.text "Line1"
                                    [Entity.PositionLocal == v3 36.0f 174.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                                     Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                     Entity.Text := tech.TechType.Name]
                                 Content.text "Line2"
                                    [Entity.PositionLocal == v3 66.0f 132.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                                     Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                                     Entity.Text := "TP Cost: " + string tech.TechCost]
                                 Content.text "Line3"
                                    [Entity.PositionLocal == v3 66.0f -66.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 810.0f 192.0f 0.0f
                                     Entity.Justification == Unjustified true
                                     Entity.Text := tech.Description]]
                        | (false, _) -> ()
                    | (false, _) -> ()
                | None -> ()
             | _ -> ()

             // shop
             match field.ShopOpt with
             | Some shop ->
                let (pageSize, rows) =
                    match (shop.ShopType, shop.ShopState) with
                    | (Chemist, ShopBuying) -> (6, 3)
                    | _ -> (8, 4)
                let items = Content.pageItems 8 field
                Content.panel "Shop"
                    [Entity.Position == v3 -450.0f -255.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 900.0f 510.0f 0.0f
                     Entity.BackdropImageOpt == Some Assets.Gui.DialogXXLImage
                     Entity.Enabled := Option.isNone shop.ShopConfirmOpt]
                    [yield! Content.items (v3 96.0f 347.0f 0.0f) pageSize rows field ShopSelect
                     Content.button "Buy"
                        [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Text == "Buy"
                         Entity.VisibleLocal := shop.ShopState = ShopSelling
                         Entity.ClickEvent => ShopBuy]
                     Content.text "BuyWhat"
                        [Entity.PositionLocal == v3 24.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Buy what?"
                         Entity.VisibleLocal := shop.ShopState = ShopBuying]
                     Content.button "Sell"
                        [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Text == "Sell"
                         Entity.VisibleLocal := shop.ShopState = ShopBuying
                         Entity.ClickEvent => ShopSell]
                     Content.text "SellWhat"
                        [Entity.PositionLocal == v3 352.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.Text == "Sell what?"
                         Entity.VisibleLocal := shop.ShopState = ShopSelling]
                     Content.button "Leave"
                        [Entity.PositionLocal == v3 678.0f 438.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                         Entity.Text == "Leave"
                         Entity.ClickEvent => ShopLeave]
                     Content.button "PageUp"
                        [Entity.PositionLocal == v3 24.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == "<"
                         Entity.VisibleLocal := a__ items
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => ShopPageUp]
                     Content.button "PageDown"
                        [Entity.PositionLocal == v3 804.0f 15.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 72.0f 72.0f 0.0f
                         Entity.Text == ">"
                         Entity.VisibleLocal := _b_ items
                         Entity.UpImage == Assets.Gui.ButtonSmallUpImage
                         Entity.DownImage == Assets.Gui.ButtonSmallDownImage
                         Entity.ClickEvent => ShopPageDown]
                     Content.text "Gold"
                        [Entity.PositionLocal == v3 198.0f 3.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 512.0f 32.0f 0.0f
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
                        Entity.BackdropImageOpt == Some Assets.Gui.DialogFatImage]
                       [Content.button "Accept"
                           [Entity.PositionLocal == v3 198.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                            Entity.Text == "Accept"
                            Entity.ClickEvent => ShopConfirmAccept]
                        Content.button "Decline"
                           [Entity.PositionLocal == v3 498.0f 36.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 192.0f 48.0f 0.0f
                            Entity.Text == "Decline"
                            Entity.ClickEvent => ShopConfirmDecline]
                        Content.text "Offer"
                           [Entity.PositionLocal == v3 30.0f 180.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.Text := shopConfirm.ShopConfirmOffer]
                        Content.text "Line1"
                           [Entity.PositionLocal == v3 60.0f 138.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.Text := shopConfirm.ShopConfirmLine1]
                        Content.text "Line2"
                           [Entity.PositionLocal == v3 60.0f 96.0f 0.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v3 768.0f 32.0f 0.0f
                            Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                            Entity.Text := shopConfirm.ShopConfirmLine2]]
                | None -> ()
             | None -> ()]]

    override this.TruncateModel field =
        Field.truncate field

    override this.UntruncateModel (field, field') =
        Field.untruncate field field'