// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] PrizePool =
    { Consequents : Advent Set
      Items : ItemType list
      Gold : int
      Exp : int }

// TODO: make this abstract due to item limit constraints.
type [<ReferenceEquality; NoComparison>] Inventory =
    { Items : Map<ItemType, int>
      Gold : int }

    static member getKeyItems inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (KeyItem keyItemType, count) -> Some (keyItemType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray
        
    static member getConsumables inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (Consumable consumableType, count) -> Some (consumableType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray

    static member containsItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 0 -> true
        | _ -> false

    static member canAddItem item inventory =
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount -> itemCount < Constants.Gameplay.ItemLimit
            | None -> true
        | Stash _ -> true

    static member tryAddItem item inventory =
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount ->
                if itemCount < Constants.Gameplay.ItemLimit then
                    let inventory = { inventory with Items = Map.add item (inc itemCount) inventory.Items }
                    (true, inventory)
                else (false, inventory)
            | None -> (true, { inventory with Items = Map.add item 1 inventory.Items })
        | Stash gold -> (true, { inventory with Gold = inventory.Gold + gold })

    static member tryAddItems items inventory =
        List.foldBack (fun item (failures, inventory) ->
            match Inventory.tryAddItem item inventory with
            | (true, inventory) -> (failures, inventory)
            | (false, inventory) -> (Some item :: failures, inventory))
            items ([], inventory)

    static member tryRemoveItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            (true, { inventory with Items = Map.add item (dec itemCount) inventory.Items })
        | Some itemCount when itemCount = 1 ->
            (true, { inventory with Items = Map.remove item inventory.Items })
        | _ -> (false, inventory)

    static member indexItems (inventory : Inventory) =
        inventory.Items |>
        Map.toSeq |>
        Seq.map (fun (ty, ct) -> List.init ct (fun _ -> ty)) |>
        Seq.concat |>
        Seq.index

    static member tryIndexItem index inventory =
        let items = Inventory.indexItems inventory
        let tail = Seq.trySkip index items
        Seq.tryHead tail

    static member getItemCount itemType inventory =
        match Map.tryFind itemType inventory.Items with
        | Some count -> count
        | None -> 0

    static member updateGold updater (inventory : Inventory) =
        { inventory with Gold = updater inventory.Gold }

    static member initial =
        { Items = Map.singleton (Consumable GreenHerb) 1; Gold = 0 }