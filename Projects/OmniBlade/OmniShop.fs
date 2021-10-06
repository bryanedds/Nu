// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; NoComparison>] ShopState =
    | ShopBuying
    | ShopSelling

type [<ReferenceEquality; NoComparison>] ShopConfirm =
    { ShopConfirmSelection : int * ItemType
      ShopConfirmPrice : int
      ShopConfirmOffer : string
      ShopConfirmLine1 : string
      ShopConfirmLine2 : string }

    static member make selection price offer line1 line2 =
        { ShopConfirmSelection = selection
          ShopConfirmPrice = price
          ShopConfirmOffer = offer
          ShopConfirmLine1 = line1
          ShopConfirmLine2 = line2 }

    static member makeFromConsumableData buying inventory selection cd =
        let itemType = snd selection
        //let header = if buying then "Buy " else "Sell "
        let header = if buying then "Acheter " else "Vendre "
        let price = if buying then cd.Cost else cd.Cost / 2
        
        let offer = header + ItemType.frenchWithQuantity itemType + " pour " + string price + " Ors?"
        let effect = "Effet: " + cd.Description
        //let stats = "Own: "+ string (Inventory.getItemCount itemType inventory)
        let stats = "Acquis: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromWeaponData buying inventory selection (wd : WeaponData) =
        let itemType = snd selection
        let header = if buying then "Acheter " else "Vendre "
        let price = if buying then wd.Cost else wd.Cost / 2
        let offer = header + ItemType.frenchWithQuantity itemType + " pour " + string price + " Ors?"
        let effect = "Effet: " + wd.Description
        //let stats = "Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        let stats = "Puiss.: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Acquis: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromArmorData buying inventory selection (ad : ArmorData) =
        let itemType = snd selection
        let header = if buying then "Acheter " else "Vendre "
        let price = if buying then ad.Cost else ad.Cost / 2
        let offer = header + ItemType.frenchWithQuantity itemType + " pour " + string price + " Ors?"
        let effect = "Effet: " + ad.Description
        let stats = "End: " + string ad.EnduranceBaseDisplay + " | Esp: " + string ad.MindBaseDisplay + " | Acquis: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromAccessoryData buying inventory selection (ad : AccessoryData) =
        let itemType = snd selection
        let header = if buying then "Acheter " else "Vendre "
        let price = if buying then ad.Cost else ad.Cost / 2
        let offer = header + ItemType.frenchWithQuantity itemType + " pour " + string price + " Ors?"
        let effect = "Effet: " + ad.Description
        let stats = "Bouclier: " + string ad.ShieldBase + " | Contre: " + string ad.CounterBase + " | Acquis: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member tryMakeFromSelection buying inventory selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty Data.Value.Consumables with
            | Some cd -> ShopConfirm.makeFromConsumableData buying inventory selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name Data.Value.Weapons with
                | Some wd -> ShopConfirm.makeFromWeaponData buying inventory selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name Data.Value.Armors with
                | Some ad -> ShopConfirm.makeFromArmorData buying inventory selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name Data.Value.Accessories with
                | Some ad -> ShopConfirm.makeFromAccessoryData buying inventory selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] Shop =
    { ShopType : ShopType
      ShopState : ShopState
      ShopPage : int
      ShopConfirmOpt : ShopConfirm option }