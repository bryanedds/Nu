// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

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
        let prompt = "Use " + string cd.ConsumableType + " on whom?"
        let effect = "Effect: " + cd.Description
        MenuUse.make selection prompt effect ""

    static member makeFromWeaponData selection (wd : WeaponData) =
        let prompt = "Equip " + string wd.WeaponType + " to whom?"
        let stats = "(Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + ")"
        let effect = "(Effect: " + wd.Description + ")"
        MenuUse.make selection prompt stats effect

    static member makeFromArmorData selection (ad : ArmorData) =
        let prompt = "Equip " + string ad.ArmorType + " to whom?"
        let stats = "(Edr: " + string ad.EnduranceBaseDisplay + " | Mnd: " + string ad.MindBaseDisplay + ")"
        let effect = "(Effect: " + ad.Description + ")"
        MenuUse.make selection prompt stats effect

    static member makeFromAccessoryData selection (ad : AccessoryData) =
        let prompt = "Equip " + string ad.AccessoryType + " to whom?"
        let effect = "(Effect: " + ad.Description + ")"
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
        | KeyItem _ | Stash _ -> None

type MenuTeam =
    { TeamIndex : int
      TeamIndices : int list }
      
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
    | MenuInventory of MenuInventory
    | MenuTechs of MenuTechs
    | MenuKeyItems of MenuKeyItems
    | MenuOptions of bool
    | MenuClosed

type [<SymbolicExpansion>] Menu =
    { MenuState : MenuState
      MenuUseOpt : MenuUse option }