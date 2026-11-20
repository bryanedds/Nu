// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// An inventory slot's item and how many of it.
type Slot =
    { SlotItem : ItemType
      SlotCount : int }

    /// How a slot reads in a menu - its item's name, how many if it stacks, and an E if it's the weapon in hand, or a
    /// dash if it's empty.
    static member describe (slotOpt : Slot option) (equippedOpt : ItemType option) =
        match slotOpt with
        | Some slot ->
            let name = Data.Items[slot.SlotItem].ItemName
            let count = if Data.Items[slot.SlotItem].Stack > 1 then " " + string slot.SlotCount else ""
            (if equippedOpt = Some slot.SlotItem then "E " else "") + name + count
        | None -> "-"

/// What the player carries - a few slots, the weapon in hand, and what's put away in the item box, which every item box
/// shares, as in the old games.
type Inventory =
    { Slots : Slot option array
      EquippedOpt : ItemType option
      Box : Map<ItemType, int> } // what's in the item box, any amount of each

    /// What two items combine into, if they combine, either way round.
    static let combine first second =
        Data.Combinations |>
        List.tryPick (fun combination ->
            if combination.First = first && combination.Second = second || combination.First = second && combination.Second = first
            then Some combination.Result
            else None)

    /// An empty inventory.
    static member val empty =
        { Slots = Array.create Constants.Gameplay.SlotCount None; EquippedOpt = None; Box = Map.empty }

    /// How many of an item share a slot.
    static member getStack item =
        Data.Items[item].Stack

    /// How many of an item the player carries.
    static member count item inventory =
        inventory.Slots |> Array.sumBy (function Some slot when slot.SlotItem = item -> slot.SlotCount | Some _ | None -> 0)

    /// Whether the player carries an item.
    static member has item inventory =
        Inventory.count item inventory > 0

    /// Whether there's room for one more of an item - a slot of it not yet full, or an empty slot.
    static member hasRoomFor item inventory =
        let stack = Inventory.getStack item
        inventory.Slots |> Array.exists (function Some slot -> slot.SlotItem = item && slot.SlotCount < stack | None -> true)

    /// Take in as many of an item as fit - topping up slots of it first, then filling empty ones - giving back how many
    /// didn't fit.
    static member add item count inventory =
        let stack = Inventory.getStack item
        let slots = Array.copy inventory.Slots
        let mutable left = count
        for i in 0 .. dec slots.Length do
            match slots[i] with
            | Some slot when left > 0 && slot.SlotItem = item && slot.SlotCount < stack ->
                let taken = min left (stack - slot.SlotCount)
                slots[i] <- Some { slot with SlotCount = slot.SlotCount + taken }
                left <- left - taken
            | Some _ | None -> ()
        for i in 0 .. dec slots.Length do
            if left > 0 && slots[i].IsNone then
                let taken = min left stack
                slots[i] <- Some { SlotItem = item; SlotCount = taken }
                left <- left - taken
        ({ inventory with Slots = slots }, left)

    /// Give up as many of an item as are carried, up to a count, from the last slots first - unequipping a weapon given
    /// up entirely.
    static member remove item count inventory =
        let slots = Array.copy inventory.Slots
        let mutable left = count
        for i in dec slots.Length .. -1 .. 0 do
            match slots[i] with
            | Some slot when left > 0 && slot.SlotItem = item ->
                let given = min left slot.SlotCount
                slots[i] <- if slot.SlotCount > given then Some { slot with SlotCount = slot.SlotCount - given } else None
                left <- left - given
            | Some _ | None -> ()
        let inventory = { inventory with Slots = slots }
        if inventory.EquippedOpt = Some item && not (Inventory.has item inventory) then { inventory with EquippedOpt = None } else inventory

    /// An inventory carrying items, each so many, in its slots in order - as many as fit.
    static member carrying items =
        items |> List.fold (fun inventory (item, count) -> fst (Inventory.add item count inventory)) Inventory.empty

    /// The inventory the player sets out with, its starting weapon in hand.
    static member val Initial =
        let inventory = Inventory.carrying [for start in Data.StartItems -> (start.StartItem, start.StartCount)]
        let weaponOpt =
            inventory.Slots |>
            Array.tryPick (function Some slot when Data.Items[slot.SlotItem].ItemKind.IsWeapon -> Some slot.SlotItem | Some _ | None -> None)
        { inventory with EquippedOpt = weaponOpt }

    /// Equip a weapon carried, or put away the one in hand.
    static member equip item inventory =
        if inventory.EquippedOpt = Some item then { inventory with EquippedOpt = None }
        elif Inventory.has item inventory && Data.Items[item].ItemKind.IsWeapon then { inventory with EquippedOpt = Some item }
        else inventory

    /// Combine the items of two slots, if they combine - one of each used up into what they make, which takes the first's
    /// slot if it's freed, or else wherever there's room for it.
    static member tryCombine first second inventory =
        match (inventory.Slots[first], inventory.Slots[second]) with
        | (Some a, Some b) when first <> second ->
            match combine a.SlotItem b.SlotItem with
            | Some result ->
                let slots = Array.copy inventory.Slots
                slots[first] <- if a.SlotCount > 1 then Some { a with SlotCount = dec a.SlotCount } else None
                slots[second] <- if b.SlotCount > 1 then Some { b with SlotCount = dec b.SlotCount } else None
                let inventory = { inventory with Slots = slots }
                if slots[first].IsNone then
                    let slots = Array.copy inventory.Slots
                    slots[first] <- Some { SlotItem = result; SlotCount = 1 }
                    Some { inventory with Slots = slots }
                else
                    match Inventory.add result 1 inventory with
                    | (inventory, 0) -> Some inventory
                    | (_, _) -> None // no room for what they'd make
            | None -> None
        | (_, _) -> None

    /// Put a slot's items away in the item box - but not the weapon in hand.
    static member store slotIndex inventory =
        match inventory.Slots[slotIndex] with
        | Some slot when inventory.EquippedOpt <> Some slot.SlotItem ->
            let slots = Array.copy inventory.Slots
            slots[slotIndex] <- None
            let boxed = Map.tryFind slot.SlotItem inventory.Box |> Option.defaultValue 0
            { inventory with Slots = slots; Box = Map.add slot.SlotItem (boxed + slot.SlotCount) inventory.Box }
        | Some _ | None -> inventory

    /// Take an item out of the item box - as many as fit, a slot's worth at most.
    static member retrieve item inventory =
        match Map.tryFind item inventory.Box with
        | Some boxed when boxed > 0 ->
            let wanted = min boxed (Inventory.getStack item)
            let (inventory, left) = Inventory.add item wanted inventory
            let taken = wanted - left
            let box = if boxed - taken > 0 then Map.add item (boxed - taken) inventory.Box else Map.remove item inventory.Box
            { inventory with Box = box }
        | Some _ | None -> inventory

    /// The items in the item box, in the order items are declared.
    static member getBoxed inventory =
        inventory.Box |> Map.toList |> List.filter (fun (_, count) -> count > 0)