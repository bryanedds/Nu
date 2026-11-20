// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What an item box shows - the side and the line highlighted, beside the inventory and intent it's given - and what's
/// put away and taken out at it, raised for the gameplay screen to do.
[<AutoOpen>]
module ItemBoxDispatcherExtensions =
    type Entity with
        member this.GetBoxMenu world : BoxMenu = this.Get (nameof this.BoxMenu) world
        member this.SetBoxMenu (value : BoxMenu) world = this.Set (nameof this.BoxMenu) value world
        member this.BoxMenu = lens (nameof this.BoxMenu) this this.GetBoxMenu this.SetBoxMenu
        member this.StoreEvent = Events.StoreEvent --> this
        member this.RetrieveEvent = Events.RetrieveEvent --> this

/// The item box - what's carried on the left, what's boxed on the right.
type ItemBoxDispatcher () =
    inherit GuiDispatcherImSim ()

    /// Answer the player's intent - closing, moving between the carried side and the boxed, and putting away and
    /// taking out, raising what's moved.
    static let answer (intent : Intent) (entity : Entity) world =
        let menu = entity.GetBoxMenu world
        let boxed = Inventory.getBoxed (entity.GetInventory world)
        if intent.Back || intent.ToggleInventory then
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuBackSound world
            World.publish () entity.CloseEvent entity world
        elif intent.Move <> v2iZero then
            let side = if intent.Move.X <> 0 then (match menu.BoxSide with Carried -> Boxed | Boxed -> Carried) else menu.BoxSide
            let count = match side with Carried -> Constants.Gameplay.SlotCount | Boxed -> max 1 boxed.Length
            let cursor = if intent.Move.X <> 0 then min menu.BoxCursor (dec count) else (menu.BoxCursor + intent.Move.Y + count) % count
            entity.SetBoxMenu { BoxSide = side; BoxCursor = cursor } world
            World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
        elif intent.Act then
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuConfirmSound world
            match menu.BoxSide with
            | Carried -> World.publish menu.BoxCursor entity.StoreEvent entity world
            | Boxed ->
                match List.tryItem menu.BoxCursor boxed with
                | Some (item, _) -> World.publish item entity.RetrieveEvent entity world
                | None -> ()

    static member Properties =
        [define Entity.BoxMenu { BoxSide = Carried; BoxCursor = 0 }
         define Entity.Inventory Inventory.empty
         define Entity.Intent Intent.empty]

    override this.Process (entity, world) =

        // the line highlighted kept among what's boxed as it's taken out, then the player's intent answered
        let menu = entity.GetBoxMenu world
        let boxedCount = (Inventory.getBoxed (entity.GetInventory world)).Length
        if menu.BoxSide = Boxed && menu.BoxCursor >= boxedCount then entity.SetBoxMenu { menu with BoxCursor = max 0 (dec boxedCount) } world
        if world.TimeAdvancing then answer (entity.GetIntent world) entity world

        // the carried and the boxed, the line highlighted
        let menu = entity.GetBoxMenu world
        let inventory = entity.GetInventory world
        let boxed = Inventory.getBoxed inventory
        World.doText "BoxPanel"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 520.0f 300.0f 0.0f
             Entity.Text .= ""
             Entity.TextColor .= Color.White
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color .= Color.White.WithA 0.86f] world
        World.doText "BoxCarriedTitle"
            [Entity.Position .= v3 -120.0f 128.0f 0.0f
             Entity.Size .= v3 200.0f 20.0f 0.0f
             Entity.Text .= "CARRIED"
             Entity.TextColor .= Constants.Gui.DimColor] world
        World.doText "BoxBoxedTitle"
            [Entity.Position .= v3 120.0f 128.0f 0.0f
             Entity.Size .= v3 200.0f 20.0f 0.0f
             Entity.Text .= "ITEM BOX"
             Entity.TextColor .= Constants.Gui.DimColor] world
        for i in 0 .. dec Constants.Gameplay.SlotCount do
            let highlighted = menu.BoxSide = Carried && menu.BoxCursor = i
            World.doText ("Carried+" + string i)
                [Entity.Position .= v3 -120.0f (90.0f - single i * 26.0f) 0.0f
                 Entity.Size .= v3 200.0f 22.0f 0.0f
                 Entity.Text @= Slot.describe inventory.Slots[i] inventory.EquippedOpt
                 Entity.TextColor @= (if highlighted then Constants.Gui.BrightColor else Constants.Gui.DimColor)
                 Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
        for (i, (item, count)) in List.indexed (List.truncate 12 boxed) do
            let highlighted = menu.BoxSide = Boxed && menu.BoxCursor = i
            let label = Data.Items[item].ItemName + (if count > 1 then " " + string count else "")
            World.doText ("Boxed+" + string i)
                [Entity.Position .= v3 120.0f (90.0f - single i * 18.0f) 0.0f
                 Entity.Size .= v3 200.0f 16.0f 0.0f
                 Entity.Text @= label
                 Entity.TextColor @= (if highlighted then Constants.Gui.BrightColor else Constants.Gui.DimColor)
                 Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
        World.doText "BoxHint"
            [Entity.Position .= v3 0.0f -128.0f 0.0f
             Entity.Size .= v3 480.0f 20.0f 0.0f
             Entity.Text .= "WASD - Choose    Enter - Move    Esc - Close"
             Entity.TextColor .= Constants.Gui.DimColor
             Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
