// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What an inventory shows - the menu's cursor, the slot chosen and what's being chosen, beside the inventory, health
/// and intent it's given - and what's chosen in it, raised for the gameplay screen to do.
[<AutoOpen>]
module InventoryDispatcherExtensions =
    type Entity with
        member this.GetInventoryMenu world : InventoryMenu = this.Get (nameof this.InventoryMenu) world
        member this.SetInventoryMenu (value : InventoryMenu) world = this.Set (nameof this.InventoryMenu) value world
        member this.InventoryMenu = lens (nameof this.InventoryMenu) this this.GetInventoryMenu this.SetInventoryMenu
        member this.CloseEvent = Events.CloseEvent --> this
        member this.UseItemEvent = Events.UseItemEvent --> this
        member this.EquipItemEvent = Events.EquipItemEvent --> this
        member this.CombineItemsEvent = Events.CombineItemsEvent --> this

/// The inventory - the player's condition, its slots, and what can be done with the chosen one.
type InventoryDispatcher () =
    inherit GuiDispatcherImSim ()

    /// The color the player's condition shows in.
    static let getHealthColor condition =
        match condition with
        | Fine -> Color (0.35f, 0.9f, 0.4f, 1.0f)
        | Caution -> Color (1.0f, 0.75f, 0.2f, 1.0f)
        | Danger -> Color (1.0f, 0.25f, 0.2f, 1.0f)

    /// Answer the player's intent - closing, moving the cursor, and choosing what to do with the chosen slot's item,
    /// raising what's chosen.
    static let answer (intent : Intent) (entity : Entity) world =
        let menu = entity.GetInventoryMenu world
        let inventory = entity.GetInventory world
        if intent.ToggleInventory || intent.Back && menu.Choice = ChoosingSlot then
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuBackSound world
            World.publish () entity.CloseEvent entity world
        elif intent.Back then
            entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuBackSound world
        elif intent.Move <> v2iZero then
            match menu.Choice with
            | ChoosingSlot | ChoosingPartner ->
                let columns = Constants.Gui.SlotColumns
                let rows = Constants.Gameplay.SlotCount / columns
                let column = (menu.Cursor % columns + intent.Move.X + columns) % columns
                let row = (menu.Cursor / columns + intent.Move.Y + rows) % rows
                entity.SetInventoryMenu { menu with Cursor = row * columns + column } world
                World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
            | ChoosingAction index ->
                match inventory.Slots[menu.Chosen] with
                | Some slot ->
                    let count = (InventoryAction.forKind Data.Items[slot.SlotItem].ItemKind).Length
                    entity.SetInventoryMenu { menu with Choice = ChoosingAction ((index + intent.Move.Y + count) % count) } world
                    World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
                | None -> ()
            | Describing -> ()
        elif intent.Act then
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuConfirmSound world
            match menu.Choice with
            | ChoosingSlot ->
                if inventory.Slots[menu.Cursor].IsSome then entity.SetInventoryMenu { menu with Chosen = menu.Cursor; Choice = ChoosingAction 0 } world
            | ChoosingAction index ->
                match inventory.Slots[menu.Chosen] with
                | Some slot ->
                    match List.item index (InventoryAction.forKind Data.Items[slot.SlotItem].ItemKind) with
                    | UseItem ->
                        World.publish slot.SlotItem entity.UseItemEvent entity world
                        entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world
                    | EquipItem ->
                        World.publish slot.SlotItem entity.EquipItemEvent entity world
                        entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world
                    | CombineItem -> entity.SetInventoryMenu { menu with Choice = ChoosingPartner } world
                    | ExamineItem -> entity.SetInventoryMenu { menu with Choice = Describing } world
                | None -> entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world
            | ChoosingPartner ->
                World.publish (menu.Chosen, menu.Cursor) entity.CombineItemsEvent entity world
                entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world
            | Describing -> entity.SetInventoryMenu { menu with Choice = ChoosingSlot } world

    static member Properties =
        [define Entity.InventoryMenu { Cursor = 0; Chosen = 0; Choice = ChoosingSlot }
         define Entity.Inventory Inventory.empty
         define Entity.Health Constants.Gameplay.PlayerHitPoints
         define Entity.Intent Intent.empty]

    override this.Process (entity, world) =

        // answer the player's intent
        if world.TimeAdvancing then answer (entity.GetIntent world) entity world

        // the condition, the slots, and what's being chosen
        let menu = entity.GetInventoryMenu world
        let inventory = entity.GetInventory world
        let gameTime = if entity.GetTimeScaled world then world.ScaledTime else world.UnscaledTime
        let time = gameTime.SecondsF
        World.doText "InventoryPanel"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 520.0f 300.0f 0.0f
             Entity.Text .= ""
             Entity.TextColor .= Color.White
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color .= Color.White.WithA 0.86f] world
        World.doText "InventoryTitle"
            [Entity.Position .= v3 0.0f 128.0f 0.0f
             Entity.Size .= v3 480.0f 20.0f 0.0f
             Entity.Text .= "STATUS"
             Entity.TextColor .= Constants.Gui.DimColor] world
        let beat = 0.6f + 0.4f * abs (sin (time * 4.0f))
        World.doText "Condition"
            [Entity.Position .= v3 -150.0f 92.0f 0.0f
             Entity.Size .= v3 180.0f 20.0f 0.0f
             Entity.Text @= getCaseName (entity.GetCondition world)
             Entity.TextColor @= (getHealthColor (entity.GetCondition world)).WithA beat
             Entity.FontSizing .= Some 12.0f] world
        for i in 0 .. dec Constants.Gameplay.SlotCount do
            let column = i % Constants.Gui.SlotColumns
            let row = i / Constants.Gui.SlotColumns
            let chosen = (match menu.Choice with ChoosingAction _ | Describing | ChoosingPartner -> i = menu.Chosen | ChoosingSlot -> false)
            let highlighted = i = menu.Cursor && (match menu.Choice with ChoosingSlot | ChoosingPartner -> true | _ -> false)
            let color = if highlighted then Constants.Gui.BrightColor elif chosen then Color (0.9f, 0.9f, 0.9f, 1.0f) else Constants.Gui.DimColor
            World.doText ("Slot+" + string i)
                [Entity.Position .= v3 (40.0f + single column * 110.0f) (60.0f - single row * 34.0f) 0.0f
                 Entity.Size .= v3 104.0f 28.0f 0.0f
                 Entity.Text @= Slot.describe inventory.Slots[i] inventory.EquippedOpt
                 Entity.TextColor @= color
                 Entity.FontSizing .= Constants.Gui.SmallFontSizing
                 Entity.BackdropImageOpt .= Some Assets.Default.Black
                 Entity.Color @= Color.White.WithA (if highlighted then 0.4f else 0.2f)] world
        match menu.Choice with
        | ChoosingAction index ->
            match inventory.Slots[menu.Chosen] with
            | Some slot ->
                for (j, action) in List.indexed (InventoryAction.forKind Data.Items[slot.SlotItem].ItemKind) do
                    let label = match action with UseItem -> "Use" | EquipItem -> (if inventory.EquippedOpt = Some slot.SlotItem then "Unequip" else "Equip") | CombineItem -> "Combine" | ExamineItem -> "Examine"
                    World.doText ("Action+" + string j)
                        [Entity.Position .= v3 -150.0f (40.0f - single j * 22.0f) 0.0f
                         Entity.Size .= v3 140.0f 20.0f 0.0f
                         Entity.Text @= label
                         Entity.TextColor @= (if j = index then Constants.Gui.BrightColor else Constants.Gui.DimColor)] world
            | None -> ()
        | Describing ->
            match inventory.Slots[menu.Chosen] with
            | Some slot ->
                World.doText "Description"
                    [Entity.Position .= v3 0.0f -100.0f 0.0f
                     Entity.Size .= v3 480.0f 40.0f 0.0f
                     Entity.Text @= Data.Items[slot.SlotItem].Description
                     Entity.TextColor .= Color.White
                     Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
            | None -> ()
        | ChoosingPartner ->
            World.doText "PartnerHint"
                [Entity.Position .= v3 0.0f -100.0f 0.0f
                 Entity.Size .= v3 480.0f 20.0f 0.0f
                 Entity.Text .= "Combine with which?"
                 Entity.TextColor .= Constants.Gui.DimColor
                 Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
        | ChoosingSlot ->
            World.doText "SlotHint"
                [Entity.Position .= v3 0.0f -128.0f 0.0f
                 Entity.Size .= v3 480.0f 20.0f 0.0f
                 Entity.Text .= "WASD - Choose    Enter - Select    Tab - Close"
                 Entity.TextColor .= Constants.Gui.DimColor
                 Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
