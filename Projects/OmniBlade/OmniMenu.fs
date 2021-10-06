// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] MenuUse =
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
        let prompt = "Qui doit ingerer " + ConsumableType.frenchWithUndefinedArticle cd.ConsumableType + "?"
        let effect = "(Effet: " + cd.Description + ")"
        MenuUse.make selection prompt effect ""

    static member makeFromWeaponData selection (wd : WeaponData) =
        let prompt = "Qui doit manier " + WeaponType.frenchWithUndefinedArticle wd.WeaponType + "?"
        let stats = "(Puissance: " + string wd.PowerBase + " | Magie: " + string wd.MagicBase + ")"
        let effect = "(Effet: " + string wd.Description + ")"
        MenuUse.make selection prompt stats effect

    static member makeFromArmorData selection (ad : ArmorData) =
        let prompt = "Qui doit revetir " + ArmorType.frenchWithUndefinedArticle ad.ArmorType + "?"
        let stats = "(Endurance: " + string ad.EnduranceBaseDisplay + " |  Mental: " + string ad.MindBaseDisplay + ")"
        let effect = "(Effet: " + string ad.Description + ")"
        MenuUse.make selection prompt stats effect

    static member makeFromAccessoryData selection (ad : AccessoryData) =
        let prompt = "Qui doter d'" + AccessoryType.frenchWithUndefinedArticle ad.AccessoryType + "?"
        let effect = "(Effet: " + string ad.Description + ")"
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

type [<ReferenceEquality; NoComparison>] MenuTeam =
    { TeamIndex : int
      TeamIndices : int list }
      
    static member tryGetTeammate (team : Map<int, Teammate>) menuTeam =
        Map.tryFind menuTeam.TeamIndex team

    static member tryGetTeammateAndTeamData team menuTeam =
        match MenuTeam.tryGetTeammate team menuTeam with
        | Some teammate ->
            match Map.tryFind teammate.CharacterType Data.Value.Characters with
            | Some characterData -> Some (teammate, characterData)
            | None -> None
        | None -> None

    static member tryGetTeamData team menuTeam =
        let lacdOpt = MenuTeam.tryGetTeammateAndTeamData team menuTeam
        Option.map snd lacdOpt

type [<ReferenceEquality; NoComparison>] MenuItem =
    { ItemPage : int }

type [<ReferenceEquality; NoComparison>] MenuTech =
    { TeammateIndex : int }

type [<StructuralEquality; NoComparison>] MenuState =
    | MenuTeam of MenuTeam
    | MenuItem of MenuItem
    | MenuTech of MenuTech
    | MenuOptions
    | MenuClosed

type [<ReferenceEquality; NoComparison>] Menu =
    { MenuState : MenuState
      MenuUseOpt : MenuUse option }