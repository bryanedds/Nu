﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

/// TODO: rename this to something more general since this is also used for current equip info display.
type MenuUse =
    { MenuUseSelection : int * ItemType
      MenuUseLine1 : string
      MenuUseLine2 : string
      MenuUseLine3 : string }

    static member make selection line1 line2 line3 =
        { MenuUseSelection = selection
          MenuUseLine1 = line1
          MenuUseLine2 = line2
          MenuUseLine3 = line3 }

    static member makeFromConsumableData selection (cd : ConsumableData) =
        let prompt = "Use " + cd.ConsumableType.Name + " on whom?"
        let effect = "Effect: " + cd.Description
        MenuUse.make selection prompt effect ""

    static member makeFromWeaponData selection (wd : WeaponData) =
        let prompt = wd.WeaponType.Name
        let stats = "(Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + ")"
        let description = "Description: " + wd.Description
        MenuUse.make selection prompt stats description

    static member makeFromArmorData selection (ad : ArmorData) =
        let prompt = ad.ArmorType.Name
        let stats = "(Edr: " + string ad.EnduranceBaseDisplay + " | Mnd: " + string ad.MindBaseDisplay + ")"
        let description = "Description: " + ad.Description
        MenuUse.make selection prompt stats description

    static member makeFromAccessoryData selection (ad : AccessoryData) =
        let prompt = ad.AccessoryType.Name
        let effect = "Effect: " + ad.Description
        MenuUse.make selection prompt effect ""

    static member tryMakeFromSelection selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty Data.Value.Consumables with
            | Some cd -> MenuUse.makeFromConsumableData selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name Data.Value.Weapons with
                | Some wd -> MenuUse.makeFromWeaponData selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name Data.Value.Armors with
                | Some ad -> MenuUse.makeFromArmorData selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name Data.Value.Accessories with
                | Some ad -> MenuUse.makeFromAccessoryData selection ad |> Some
                | None -> None
        | KeyItem keyItem ->
            let description = // TODO: pull this from key item data.
                match keyItem with
                | NonExistentKey -> "No man may hold this."
                | BrassKey -> "A polished brass key adorned with a lamb's head."
                | IronKey -> "A slightly rusted iron key adorned with goat horns."
                | CopperKey -> "A brass key adorned with a green eye."
                | AluminumKey -> "A cheap-looking key."
                | PewterKey -> "A weathered key. It looks bent but usable."
                | SteelKey -> "A key made of steel adorned with a crown."
            Some (MenuUse.make selection keyItem.Name "(Key Item)" description)
        | Stash _ -> None

type EquipType =
    | EquipWeapon of WeaponType option
    | EquipArmor of ArmorType option
    | EquipAccessory of AccessoryType option

    member this.ItemName =
        match this with
        | EquipWeapon weaponTypeOpt -> weaponTypeOpt |> Option.map _.Name |> Option.defaultValue "None"
        | EquipArmor armorTypeOpt -> armorTypeOpt |> Option.map _.Name |> Option.defaultValue "None"
        | EquipAccessory accessoryTypeOpt -> accessoryTypeOpt |> Option.map _.Name |> Option.defaultValue "None"

type Equip =
    { EquipType : EquipType
      EquipPage : int
      EquipMenuUseOpt : MenuUse option }

type MenuTeam =
    { TeamIndex : int
      TeamIndices : int list
      TeamEquipOpt : Equip option }

    static member tryGetTeammate (team : Map<int, Teammate>) menuTeam =
        Map.tryFind menuTeam.TeamIndex team

    static member tryGetTeammateAndCharacterData team menuTeam =
        match MenuTeam.tryGetTeammate team menuTeam with
        | Some teammate ->
            match Map.tryFind teammate.CharacterType Data.Value.Characters with
            | Some characterData -> Some (teammate, characterData)
            | None -> None
        | None -> None

    static member tryGetCharacterData team menuTeam =
        let tacdOpt = MenuTeam.tryGetTeammateAndCharacterData team menuTeam
        Option.map snd tacdOpt

type MenuInventory =
    { InventoryPage : int }

type MenuTechs =
    { TeamIndex : int
      TechIndexOpt : int option }

type MenuKeyItems =
    { KeyItemsPage : int }

type MenuState =
    | MenuTeam of MenuTeam
    | MenuAutoMap
    | MenuInventory of MenuInventory
    | MenuTechs of MenuTechs
    | MenuKeyItems of MenuKeyItems
    | MenuOptions of bool
    | MenuClosed

type [<SymbolicExpansion>] Menu =
    { MenuState : MenuState
      MenuUseOpt : MenuUse option }