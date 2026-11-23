// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

/// Where a field menu's cursor rests.
type FieldCursorPosition =
    { InSidebar : bool
      SidebarRow : int
      Column : int
      Row : int }

    static member sidebar =
        { InSidebar = true; SidebarRow = 0; Column = 0; Row = 0 }

    static member content =
        { InSidebar = false; SidebarRow = 0; Column = 0; Row = 0 }

/// Where the field's menu cursor is, kept beside the field rather than in it, so it neither saves nor shows in Gaia.
/// The cursor starts over whenever the field shows another menu, except when coming back out of a menu opened over
/// the one it was in, where it returns to where it was.
type FieldCursor =
    { CursorMenu : string // the menu the cursor is in, such as "Team" or "Shop"
      CursorPosition : FieldCursorPosition
      CursorParents : (string * FieldCursorPosition) list } // the menus under this one and where the cursor was in each

    static member initial =
        { CursorMenu = ""
          CursorPosition = FieldCursorPosition.sidebar
          CursorParents = [] }

/// The field's gamepad and keyboard input - walking about, the dialog, and a routine for each menu.
[<RequireQualifiedAccess>]
module FieldInput =

    let private scene name = Simulants.FieldScene / name
    let private sound asset world = World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault asset world
    let private affirm signals world = Selection.playAffirm world; signals
    let private signal (s : Signal) = [s]

    (* Conditions, as the hud's buttons show *)

    let private exploring (field : Field) =
        field.FieldState = Playing &&
        field.Menu.MenuState = MenuClosed &&
        field.PartyMenu.PartyMenuState = PartyMenuClosed &&
        not field.ScreenTransitioning &&
        Option.isNone field.ShopOpt &&
        Option.isNone field.FieldTransitionOpt

    let private canInteract (field : Field) =
        exploring field &&
        (CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue || Option.isSome field.DialogOpt) &&
        Option.isSome (Field.tryGetInteraction field)

    let private canOpenMenu (field : Field) =
        exploring field &&
        CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
        Option.isNone field.DialogOpt

    let private canOpenParty (field : Field) =
        exploring field &&
        Option.isNone field.DialogOpt &&
        Field.touchingSavePoint field &&
        field.Team.Count >= 3

    let private canSkip (field : Field) =
        match field.DialogOpt with
        | Some dialog ->
            not (Dialog.canAdvance (Field.detokenize field) dialog) &&
            dialog.DialogSkippableWhenNotTombSealed &&
            not (field.Advents.Contains TombSealed)
        | None -> false

    let private promptShowing (field : Field) =
        match field.DialogOpt with
        | Some dialog -> Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted (Field.detokenize field) dialog
        | None -> false

    (* The menu the cursor is in *)

    /// Name the menu the field shows now, so the cursor knows when it's moved to another.
    let getMenu (field : Field) =
        match field.ShopOpt with
        | Some shop -> if shop.ShopConfirmOpt.IsSome then "ShopConfirm" else "Shop"
        | None ->
            if field.PartyMenu.PartyMenuState = PartyMenuOpened then "Party"
            else
                match field.Menu.MenuState.Shown with
                | MenuTeam menuTeam ->
                    match menuTeam.TeamEquipOpt with
                    | Some equip -> if equip.EquipMenuUseOpt.IsSome then "EquipInfo" else "Equip"
                    | None -> "Team"
                | MenuAutoMap -> "AutoMap"
                | MenuInventory _ -> if field.Menu.MenuUseOpt.IsSome then "Use" else "Inventory"
                | MenuTechs techs -> if techs.TechIndexOpt.IsSome then "Tech" else "Techs"
                | MenuKeyItems _ -> if field.Menu.MenuUseOpt.IsSome then "Use" else "KeyItems"
                | MenuOptions quitPrompt -> if quitPrompt then "Quit" else "Options"
                | MenuCloseSelected _ | MenuClosed -> if promptShowing field then "Prompt" elif field.DialogOpt.IsSome then "Dialog" else ""

    /// The sidebar row of a main menu.
    let private getSidebarRow menu =
        match menu with
        | "Inventory" -> 1
        | "Techs" -> 2
        | "KeyItems" -> 3
        | "Options" -> 4
        | _ -> 0

    /// Where the cursor starts in a menu - on a main menu's sidebar or in its content, and elsewhere on the first item.
    let private getStart menu onSidebar =
        match menu with
        | "Team" | "Inventory" | "Techs" | "KeyItems" | "Options" ->
            let position = if onSidebar then FieldCursorPosition.sidebar else FieldCursorPosition.content
            { position with SidebarRow = getSidebarRow menu }
        | "Equip" -> { FieldCursorPosition.content with Column = 1 }
        | "Quit" -> { FieldCursorPosition.content with Column = 1 }
        | _ -> FieldCursorPosition.content

    /// Whether a menu opens over another, so that closing it goes back to where the cursor was in the other.
    let private isSubmenu parent menu =
        match (parent, menu) with
        | ("Team", ("Equip" | "AutoMap"))
        | ("Equip", "EquipInfo")
        | (("Inventory" | "KeyItems"), "Use")
        | ("Techs", "Tech")
        | ("Options", "Quit")
        | ("Shop", "ShopConfirm") -> true
        | (_, _) -> false

    /// Move the cursor to the menu the field shows - back to where it was when a menu opened over that one closes,
    /// otherwise starting over there.
    let private follow (field : Field) (cursor : FieldCursor) =
        let menu = getMenu field
        if menu = cursor.CursorMenu then cursor
        elif menu = "" || menu = "Dialog" || menu = "Prompt" then { FieldCursor.initial with CursorMenu = menu }
        else
            match cursor.CursorParents with
            | (parent, position) :: parents when parent = menu -> { CursorMenu = menu; CursorPosition = position; CursorParents = parents }
            | _ ->
                if isSubmenu cursor.CursorMenu menu then
                    let parents = (cursor.CursorMenu, cursor.CursorPosition) :: cursor.CursorParents
                    { CursorMenu = menu; CursorPosition = getStart menu false; CursorParents = parents }
                else
                    // a main menu opened from the field or from another's sidebar, as by the mouse, starts on the
                    // sidebar
                    let isMain menu = getSidebarRow menu > 0 || menu = "Team"
                    let fromSidebar = cursor.CursorMenu = "" || cursor.CursorMenu = "Dialog" || cursor.CursorMenu = "Prompt" || isMain cursor.CursorMenu
                    { CursorMenu = menu; CursorPosition = getStart menu fromSidebar; CursorParents = [] }

    /// Enter a menu chosen from the sidebar, with the cursor on its first item rather than the sidebar.
    let private enter menu =
        { CursorMenu = menu; CursorPosition = getStart menu false; CursorParents = [] }

    (* Moving the cursor *)

    let private moveTo column row (cursor : FieldCursor) =
        { cursor with CursorPosition = { cursor.CursorPosition with InSidebar = false; Column = column; Row = row } }

    /// Back to the sidebar, the menu's own cursor starting over for when it comes back.
    let private toSidebar (cursor : FieldCursor) =
        { cursor with CursorPosition = { cursor.CursorPosition with InSidebar = true; Column = 0; Row = 0 } }

    let private teammateEntity panel row (field : Field) =
        scene panel / ("Teammate+" + string (Seq.item row field.Team.Values).TeamIndex)

    (* The sidebar *)

    let private sidebarButtons =
        [|("TeamButton", "Team", MenuTeamOpen)
          ("InventoryButton", "Inventory", MenuInventoryOpen)
          ("TechButton", "Techs", MenuTechsOpen)
          ("KeyItemsButton", "KeyItems", MenuKeyItemsOpen)
          ("OptionsButton", "Options", MenuOptionsOpen)
          ("CloseButton", "", MenuClose)|]

    let private sidebarEntity panel (cursor : FieldCursor) =
        let (name, _, _) = sidebarButtons[cursor.CursorPosition.SidebarRow]
        scene panel / "Sidebar" / name

    /// The sidebar down the left of the main menus - the button pointed at is the one selected, so stepping onto a
    /// menu's opens it and stepping onto close selects that, with the cursor staying in the sidebar. Confirm moves into
    /// the open menu, confirm on close closes it, and cancel or the menu button closes it too.
    let private updateSidebar (intent : Intent) panel hasContent (field : Field) (cursor : FieldCursor) world =
        let invoke = intent.Confirm || intent.Step = Some Rightward // right invokes as confirm does
        let cursor = toSidebar cursor
        let position = cursor.CursorPosition
        let closeRow = dec sidebarButtons.Length
        // follow the selection, however it was made
        let row = match field.Menu.MenuState with MenuCloseSelected _ -> closeRow | _ -> getSidebarRow panel
        let cursor = { cursor with CursorPosition = { position with SidebarRow = row } }
        let (cursor, opening) =
            match intent.Step with
            | Some (Upward | Downward as direction) ->
                let row' = Selection.wrap (Selection.rowDelta direction) sidebarButtons.Length row
                let (_, menu, message) = sidebarButtons[row']
                let cursor = { cursor with CursorPosition = { cursor.CursorPosition with SidebarRow = row' } }
                if row' = closeRow then (cursor, signal MenuCloseSelect)
                elif menu <> panel then ({ cursor with CursorMenu = menu }, signal message)
                elif row = closeRow then (cursor, signal MenuCloseDeselect)
                else (cursor, [])
            | _ -> (cursor, [])
        let (cursor, signals) =
            if invoke then
                let (_, menu, message) = sidebarButtons[cursor.CursorPosition.SidebarRow]
                if menu = "" then (cursor, affirm (opening @ signal MenuClose) world)
                // a menu opened this frame, so its content's unknown yet
                elif menu = cursor.CursorMenu && not opening.IsEmpty then (cursor, opening)
                elif menu = panel then ((if hasContent then moveTo 0 0 cursor else cursor), affirm opening world)
                else (enter menu, affirm (signal message) world)
            elif intent.Cancel || intent.Menu then (cursor, affirm (opening @ signal MenuClose) world)
            else (cursor, opening)
        let pointedOpt = if cursor.CursorPosition.InSidebar then Some (sidebarEntity panel cursor) else None
        (cursor, signals, pointedOpt)

    (* The main menus *)

    /// The team menu, in three levels - the sidebar, then the teammates with the auto map beneath them, then the
    /// selected teammate's weapon, armor and accessory. The teammate pointed at is the one selected, confirming goes
    /// down a level, and cancel comes back up one.
    let private updateTeam (intent : Intent) (menuTeam : MenuTeam) (field : Field) (cursor : FieldCursor) world =
        let position = cursor.CursorPosition
        let teamCount = field.Team.Count
        if position.InSidebar then updateSidebar intent "Team" true field cursor world
        elif position.Column = 0 then
            // the teammates and the auto map, the pointer following the selection, however it was made, right invoking
            // as confirm does, and left backing out as cancel does
            let invoke = intent.Confirm || intent.Step = Some Rightward
            let selected = if menuTeam.TeamAutoMapSelected then teamCount else menuTeam.TeamIndex
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) (inc teamCount) selected | _ -> selected
            let selecting =
                if row = selected then []
                elif row = teamCount then signal (MenuTeamAutoMapSelect true)
                elif row <> menuTeam.TeamIndex then signal (MenuTeamAlly row)
                else signal (MenuTeamAutoMapSelect false)
            // leaving the teammates leaves the map button unselected
            let deselecting = if menuTeam.TeamAutoMapSelected || row = teamCount then signal (MenuTeamAutoMapSelect false) else []
            let (cursor, signals) =
                if invoke && row < teamCount then
                    match MenuTeam.tryGetTeammate field.Team menuTeam with
                    | Some _ -> (moveTo 1 0 cursor, affirm selecting world)
                    | None -> (moveTo 0 row cursor, selecting)
                elif invoke then
                    if Field.hasAutoMap field
                    then (moveTo 0 row cursor, (sound Assets.Field.AutoMapSound world; signal MenuAutoMapOpen))
                    else (moveTo 0 row cursor, (Selection.playMistake world; []))
                elif intent.Cancel || intent.Step = Some Leftward then (toSidebar cursor, affirm deselecting world)
                elif intent.Menu then (moveTo 0 row cursor, selecting @ affirm (signal MenuClose) world)
                else (moveTo 0 row cursor, selecting)
            let pointedOpt =
                if cursor.CursorPosition.InSidebar then Some (sidebarEntity "Team" cursor)
                elif cursor.CursorPosition.Column = 1 then Some (scene "Team" / "Weapon")
                elif row < teamCount then Some (teammateEntity "Team" row field)
                else Some (scene "Team" / "AutoMap")
            (cursor, signals, pointedOpt)
        else
            // the selected teammate's equipment, top to bottom, left backing out as cancel does
            match MenuTeam.tryGetTeammate field.Team menuTeam with
            | Some teammate ->
                let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) 3 position.Row | _ -> position.Row
                let (cursor, signals) =
                    if intent.Confirm then
                        let message =
                            match row with
                            | 0 -> MenuTeamEquip (EquipWeapon teammate.WeaponOpt)
                            | 1 -> MenuTeamEquip (EquipArmor teammate.ArmorOpt)
                            | _ -> MenuTeamEquip (EquipAccessory (List.tryHead teammate.Accessories))
                        (moveTo 1 row cursor, affirm (signal message) world)
                    elif intent.Cancel || intent.Step = Some Leftward then (moveTo 0 menuTeam.TeamIndex cursor, affirm [] world)
                    elif intent.Menu then (moveTo 1 row cursor, affirm (signal MenuClose) world)
                    else (moveTo 1 row cursor, [])
                let pointedOpt =
                    if cursor.CursorPosition.Column = 0 then Some (teammateEntity "Team" menuTeam.TeamIndex field)
                    else Some (scene "Team" / (match row with 0 -> "Weapon" | 1 -> "Armor" | _ -> "Accessory"))
                (cursor, signals, pointedOpt)
            | None -> (moveTo 0 0 cursor, [], None)

    /// The techs menu, in three levels - the sidebar, then the teammates, then the selected teammate's techs. The
    /// teammate pointed at is the one selected, confirming goes down a level, and cancel comes back up one.
    let private updateTechs (intent : Intent) (menuTechs : MenuTechs) (field : Field) (cursor : FieldCursor) world =
        let position = cursor.CursorPosition
        let teamCount = field.Team.Count
        let techs = match Map.tryFind menuTechs.TeamIndex field.Team with Some teammate -> Array.ofSeq teammate.Techs | None -> [||]
        if position.InSidebar then updateSidebar intent "Techs" true field cursor world
        elif position.Column = 0 || techs.Length = 0 then
            // the teammates, the pointer following a teammate selected otherwise, as by the mouse, right invoking as
            // confirm does, and left backing out as cancel does
            let invoke = intent.Confirm || intent.Step = Some Rightward
            let row = menuTechs.TeamIndex
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) teamCount row | _ -> row
            let selecting = if row <> menuTechs.TeamIndex then signal (MenuTechsAlly row) else []
            let (cursor, signals) =
                if invoke && techs.Length > 0 then (moveTo 1 0 cursor, affirm selecting world)
                elif invoke then (moveTo 0 row cursor, (Selection.playMistake world; selecting))
                elif intent.Cancel || intent.Step = Some Leftward then (toSidebar cursor, affirm selecting world)
                elif intent.Menu then (moveTo 0 row cursor, selecting @ affirm (signal MenuClose) world)
                else (moveTo 0 row cursor, selecting)
            let pointedOpt =
                if cursor.CursorPosition.InSidebar then Some (sidebarEntity "Techs" cursor)
                elif cursor.CursorPosition.Column = 1 then Some (scene "Techs" / scstringMemo techs[0])
                else Some (teammateEntity "Techs" row field)
            (cursor, signals, pointedOpt)
        else
            // the selected teammate's techs, top to bottom, left backing out as cancel does
            let row = min position.Row (dec techs.Length)
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) techs.Length row | _ -> row
            let (cursor, signals) =
                if intent.Confirm then (moveTo 1 row cursor, affirm (signal (MenuTechsSelect row)) world)
                elif intent.Cancel || intent.Step = Some Leftward then (moveTo 0 menuTechs.TeamIndex cursor, affirm [] world)
                elif intent.Menu then (moveTo 1 row cursor, affirm (signal MenuClose) world)
                else (moveTo 1 row cursor, [])
            let pointedOpt =
                if cursor.CursorPosition.Column = 0 then Some (teammateEntity "Techs" menuTechs.TeamIndex field)
                else Some (scene "Techs" / scstringMemo techs[row])
            (cursor, signals, pointedOpt)

    /// The inventory and key items menus, which lay out a page of ten items alike - two columns of five, left of
    /// whose first is the sidebar. Right past the second column turns the page, as do the page buttons.
    let private updateItemPage (intent : Intent) panel enabled (select : _ -> Signal) pageUpMessage pageDownMessage (field : Field) (cursor : FieldCursor) world =
        let (pageUp, pageDown, items) = Content.pageItems 10 field
        let count = items.Count
        let position = cursor.CursorPosition
        if position.InSidebar || count = 0 then updateSidebar intent panel (count > 0) field cursor world
        else
            let paging = (if intent.PageUp && pageUp then [pageUpMessage] else []) @ (if intent.PageDown && pageDown then [pageDownMessage] else [])
            let index = min position.Row (dec count)
            let column = index / 5
            let row = index % 5
            let columnCount column = max 0 (min 5 (count - column * 5))
            let (indexOpt, stepping) =
                match intent.Step with
                | Some (Upward | Downward as direction) -> (Some (column * 5 + Selection.wrap (Selection.rowDelta direction) (columnCount column) row), [])
                | Some Leftward -> ((if column = 1 then Some (index - 5) else None), [])
                | Some Rightward ->
                    if column = 0 && columnCount 1 > 0 then (Some (5 + min row (dec (columnCount 1))), [])
                    elif pageDown then (Some index, [pageDownMessage])
                    else (Some index, [])
                | None -> (Some index, [])
            match indexOpt with
            | Some index ->
                let (_, (itemType : ItemType, _)) as page = items[index]
                let signals =
                    paging @ stepping @
                    (if intent.Confirm then
                        if enabled itemType then affirm (signal (select page)) world else (Selection.playMistake world; [])
                     elif intent.Menu then affirm (signal MenuClose) world
                     else [])
                let cursor = if intent.Cancel then (Selection.playAffirm world; toSidebar cursor) else moveTo 0 index cursor
                let pointedOpt = if cursor.CursorPosition.InSidebar then Some (sidebarEntity panel cursor) else Some (scene panel / itemType.Name)
                (cursor, signals, pointedOpt)
            | None ->
                let cursor = toSidebar cursor
                (cursor, paging, Some (sidebarEntity panel cursor))

    let private OptionsNames =
        [|"Wait"; "Paced"; "Swift"; "SongVolumeDown"; "SongVolumeUp"; "ToggleFullScreen"; "PlayModeSelection"; "Quit"|]

    /// Where each options button goes for each direction, as they're laid out - the battle speeds across the top, the
    /// song volume's down and up beneath, then full screen, play mode and quit down the middle. -1 is the sidebar.
    /// Moving up or down into a row lands on its leftmost button.
    let private stepOptions index direction =
        match (index, direction) with
        | (0, Upward) -> 7 | (0, Downward) -> 3 | (0, Leftward) -> -1 | (0, Rightward) -> 1 // Wait
        | (1, Upward) -> 7 | (1, Downward) -> 3 | (1, Leftward) -> 0 | (1, Rightward) -> 2 // Paced
        | (2, Upward) -> 7 | (2, Downward) -> 3 | (2, Leftward) -> 1 | (2, Rightward) -> 2 // Swift
        | (3, Upward) -> 0 | (3, Downward) -> 5 | (3, Leftward) -> -1 | (3, Rightward) -> 4 // SongVolumeDown
        | (4, Upward) -> 0 | (4, Downward) -> 5 | (4, Leftward) -> 3 | (4, Rightward) -> 4 // SongVolumeUp
        | (5, Upward) -> 3 | (5, Downward) -> 6 | (5, Leftward) -> -1 | (5, Rightward) -> 5 // ToggleFullScreen
        | (6, Upward) -> 5 | (6, Downward) -> 7 | (6, Leftward) -> -1 | (6, Rightward) -> 6 // PlayModeSelection
        | (_, Upward) -> 6 | (_, Downward) -> 0 | (_, Leftward) -> -1 | (_, Rightward) -> 7 // Quit

    let private pressOptions index =
        match index with
        | 0 -> signal (MenuOptionsSelectBattleSpeed WaitSpeed)
        | 1 -> signal (MenuOptionsSelectBattleSpeed PacedSpeed)
        | 2 -> signal (MenuOptionsSelectBattleSpeed SwiftSpeed)
        | 3 -> signal MenuOptionsSongVolumeDown
        | 4 -> signal MenuOptionsSongVolumeUp
        | 5 -> signal MenuOptionsToggleFullScreen
        | 6 -> signal MenuOptionsHardcoreModeToggle
        | _ -> signal MenuOptionsQuitPrompt

    /// The options menu - each button pressed by confirming it. Coming into the menu lands on its top left button,
    /// the cursor being placed in column 1 once it's in.
    let private updateOptions (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        if cursor.CursorPosition.InSidebar then updateSidebar intent "Options" true field cursor world
        else
            let index = if cursor.CursorPosition.Column = 0 then 0 else cursor.CursorPosition.Row
            let index = match intent.Step with Some direction -> stepOptions index direction | None -> index
            let (cursor, signals) =
                if index < 0 then (toSidebar cursor, [])
                elif intent.Cancel then (toSidebar cursor, affirm [] world)
                elif intent.Confirm then (moveTo 1 index cursor, affirm (pressOptions index) world)
                elif intent.Menu then (moveTo 1 index cursor, affirm (signal MenuClose) world)
                else (moveTo 1 index cursor, [])
            let pointedOpt = if cursor.CursorPosition.InSidebar then Some (sidebarEntity "Options" cursor) else Some (scene "Options" / OptionsNames[index])
            (cursor, signals, pointedOpt)

    (* The panels over the main menus *)

    /// A panel with one thing to do - confirm or cancel both close it.
    let private updateClose (intent : Intent) entity message (cursor : FieldCursor) world =
        let signals = if intent.Confirm || intent.Cancel || intent.Menu then affirm (signal message) world else []
        (cursor, signals, Some entity)

    /// Which of two buttons side by side a step lands on - left the first, right the second.
    let private stepPair (intent : Intent) column =
        match intent.Step with
        | Some Leftward -> 0
        | Some Rightward -> 1
        | _ -> column

    /// Using a consumable - the teammates in columns of three, confirm using it on the one pointed at.
    let private updateUse (intent : Intent) itemType (field : Field) (cursor : FieldCursor) world =
        let count = field.Team.Count
        let index = min cursor.CursorPosition.Row (dec count)
        let column = index / 3
        let row = index % 3
        let columnCount column = max 0 (min 3 (count - column * 3))
        let index =
            match intent.Step with
            | Some (Upward | Downward as direction) -> column * 3 + Selection.wrap (Selection.rowDelta direction) (columnCount column) row
            | Some (Leftward | Rightward as direction) ->
                let column' = column + Selection.columnDelta direction
                if column' >= 0 && columnCount column' > 0 then column' * 3 + min row (dec (columnCount column')) else index
            | None -> index
        let teammate = Seq.item index field.Team.Values
        let signals =
            if intent.Confirm then
                if Teammate.canUseItem itemType teammate
                then affirm (signal (MenuInventoryUse index)) world
                else Selection.playMistake world; []
            elif intent.Cancel then affirm (signal MenuInventoryCancel) world
            else []
        (moveTo 0 index cursor, signals, Some (teammateEntity "Use" index field))

    /// The equip panel - a page of items down the right, and on the left the current equipment's info or, once an
    /// item's chosen, the equip button. Choosing an item moves to the equip button; choosing that equips it.
    let private updateEquip (intent : Intent) (menuTeam : MenuTeam) (equip : Equip) (field : Field) (cursor : FieldCursor) world =
        let teammate = field.Team[menuTeam.TeamIndex]
        let (changing, currentName) =
            match equip.EquipType with
            | EquipWeapon weaponTypeOpt -> (teammate.WeaponOpt <> weaponTypeOpt, teammate.WeaponOpt |> Option.map _.Name |> Option.defaultValue "None")
            | EquipArmor armorTypeOpt -> (teammate.ArmorOpt <> armorTypeOpt, teammate.ArmorOpt |> Option.map _.Name |> Option.defaultValue "None")
            | EquipAccessory accessoryTypeOpt -> (teammate.Accessories <> Option.toList accessoryTypeOpt, teammate.Accessories |> List.map _.Name |> Seq.headOrDefault "None")
        let leftOpt = if changing then Some "EquipButton" elif currentName <> "None" then Some "Info" else None
        let (pageUp, pageDown, items) = Content.pageItems 3 field
        let count = items.Count
        let paging = (if intent.PageUp && pageUp then [MenuTeamEquipPageUp] else []) @ (if intent.PageDown && pageDown then [MenuTeamEquipPageDown] else [])
        let paging = List.map (fun message -> message :> Signal) paging
        let onLeft = (cursor.CursorPosition.Column = 0 || count = 0) && leftOpt.IsSome
        if onLeft then
            let left = leftOpt.Value
            let (cursor, signals) =
                match intent.Step with
                | Some Rightward when count > 0 -> (moveTo 1 0 cursor, [])
                | _ ->
                    let cursor = moveTo 0 0 cursor
                    if intent.Confirm then
                        if left = "EquipButton"
                        then (cursor, (sound Assets.Field.EquipSound world; signal MenuTeamEquipConfirm))
                        else (cursor, affirm (signal MenuTeamEquipMenuUseOpen) world)
                    elif intent.Cancel then (cursor, affirm (signal MenuTeamEquipCancel) world)
                    else (cursor, [])
            let pointedOpt =
                if cursor.CursorPosition.Column = 1 then Some (scene "Equip" / (fst (snd items[0])).Name)
                else Some (scene "Equip" / left)
            (cursor, paging @ signals, pointedOpt)
        elif count > 0 then
            let index = min cursor.CursorPosition.Row (dec count)
            let (index, column, stepping) =
                match intent.Step with
                | Some (Upward | Downward as direction) -> (Selection.wrap (Selection.rowDelta direction) count index, 1, [])
                | Some Leftward when leftOpt.IsSome -> (index, 0, [])
                | Some Leftward when pageUp -> (index, 1, [MenuTeamEquipPageUp :> Signal])
                | Some Rightward when pageDown -> (index, 1, [MenuTeamEquipPageDown :> Signal])
                | _ -> (index, 1, [])
            let page = items[index]
            let (column, signals) =
                // choosing an item moves to the equip button
                if column = 1 && intent.Confirm then (0, affirm (signal (MenuTeamEquipSelect page)) world)
                elif intent.Cancel then (column, affirm (signal MenuTeamEquipCancel) world)
                else (column, [])
            let cursor = moveTo column index cursor
            let pointedOpt =
                match (column, leftOpt) with
                | (0, Some left) when not intent.Confirm -> Some (scene "Equip" / left)
                | (_, _) -> Some (scene "Equip" / (fst (snd page)).Name)
            (cursor, paging @ stepping @ signals, pointedOpt)
        else
            let signals = if intent.Cancel then affirm (signal MenuTeamEquipCancel) world else []
            (cursor, paging @ signals, None)

    /// The party menu - the team to choose from on the left, those chosen in the middle, and the confirm button on the
    /// right, which the cursor moves to once three are chosen. Cancel takes back the last one chosen.
    let private updateParty (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        let selections = field.PartyMenu.PartyMenuSelections
        let teamIndices = Map.toKeyArray field.Team
        let position = cursor.CursorPosition
        let (column, row) =
            match position.Column with
            | 1 when selections.IsEmpty -> (0, 0)
            | 1 -> (1, min position.Row (dec selections.Length))
            | 2 -> (2, 0)
            | _ -> (0, min position.Row (dec teamIndices.Length))
        let (column, row) =
            match (intent.Step, column) with
            | (Some (Upward | Downward as direction), 0) -> (0, Selection.wrap (Selection.rowDelta direction) teamIndices.Length row)
            | (Some (Upward | Downward as direction), 1) -> (1, Selection.wrap (Selection.rowDelta direction) selections.Length row)
            | (Some Rightward, 0) -> if selections.IsEmpty then (2, 0) else (1, min row (dec selections.Length))
            | (Some Rightward, 1) -> (2, 0)
            | (Some Leftward, 1) -> (0, min row (dec teamIndices.Length))
            | (Some Leftward, 2) -> if selections.IsEmpty then (0, 0) else (1, 0)
            | (_, _) -> (column, row)
        let (column, row, signals) =
            if intent.Confirm then
                match column with
                | 0 ->
                    let teamIndex = teamIndices[row]
                    if selections.Length < 3 && not (List.contains teamIndex selections) then
                        let (column, row) = if selections.Length = 2 then (2, 0) else (column, row)
                        (column, row, affirm (signal (PartyMenuSelect teamIndex)) world)
                    else (column, row, (Selection.playMistake world; []))
                | 1 -> (column, row, affirm (signal (PartyMenuDeselect selections[row])) world)
                | _ ->
                    if selections.Length >= 3
                    then (column, row, affirm (signal PartyMenuClose) world)
                    else (column, row, (Selection.playMistake world; []))
            elif intent.Cancel then
                match List.tryLast selections with
                | Some teamIndex -> (column, row, affirm (signal (PartyMenuDeselect teamIndex)) world)
                | None -> (column, row, [])
            else (column, row, [])
        let pointedOpt =
            match column with
            | 0 -> Some (scene "PartyMenu" / ("Teammate+" + string teamIndices[row]))
            | 1 when row < selections.Length -> Some (scene "PartyMenu" / ("Selected+" + string selections[row]))
            | 1 -> None
            | _ -> Some (scene "PartyMenu" / "Confirm")
        (moveTo column row cursor, signals, pointedOpt)

    /// The shop - Buy or Sell and Leave along the top, the wares beneath in two columns, turning the page left and
    /// right past them as well as by the page buttons. Cancel leaves.
    let private updateShop (intent : Intent) (shop : Shop) (field : Field) (cursor : FieldCursor) world =
        let (pageSize, rows) = match (shop.ShopType, shop.ShopState) with (Chemist, ShopBuying) -> (6, 3) | _ -> (8, 4)
        let (pageUp, pageDown, items) = Content.pageItems pageSize field
        let count = items.Count
        let columnCount column = max 0 (min rows (count - column * rows))
        let paging = (if intent.PageUp && pageUp then [ShopPageUp] else []) @ (if intent.PageDown && pageDown then [ShopPageDown] else [])
        let paging = List.map (fun message -> message :> Signal) paging
        let position = cursor.CursorPosition
        // the header is row -1, its trade button column 0 and leave column 1; the wares are by index from row 0
        let (column, row) = if position.Row < 0 || count = 0 then (min 1 position.Column, -1) else (0, min position.Row (dec count))
        let (column, row, stepping) =
            if row < 0 then
                match intent.Step with
                | Some (Leftward | Rightward) -> (stepPair intent column, -1, [])
                | Some Downward when columnCount column > 0 -> (0, column * rows, [])
                | Some Downward when count > 0 -> (0, 0, [])
                | Some Upward when columnCount column > 0 -> (0, column * rows + dec (columnCount column), [])
                | Some Upward when count > 0 -> (0, dec count, [])
                | _ -> (column, -1, [])
            else
                let itemColumn = row / rows
                let itemRow = row % rows
                match intent.Step with
                | Some Upward when itemRow = 0 -> (itemColumn, -1, [])
                | Some Upward -> (0, dec row, [])
                | Some Downward when itemRow = dec (columnCount itemColumn) -> (itemColumn, -1, [])
                | Some Downward -> (0, inc row, [])
                | Some Leftward when itemColumn = 1 -> (0, row - rows, [])
                | Some Leftward when pageUp -> (0, row, [ShopPageUp :> Signal])
                | Some Rightward when itemColumn = 0 && columnCount 1 > 0 -> (0, rows + min itemRow (dec (columnCount 1)), [])
                | Some Rightward when pageDown -> (0, row, [ShopPageDown :> Signal])
                | _ -> (0, row, [])
        let trade = match shop.ShopState with ShopBuying -> "Sell" | ShopSelling -> "Buy"
        let signals =
            if intent.Confirm then
                if row >= 0 then affirm (signal (ShopSelect items[row])) world
                elif column = 1 then affirm (signal ShopLeave) world
                else affirm (signal (match shop.ShopState with ShopBuying -> ShopSell | ShopSelling -> ShopBuy)) world
            elif intent.Cancel then affirm (signal ShopLeave) world
            else []
        let pointedOpt =
            if row >= 0 then Some (scene "Shop" / (fst (snd items[row])).Name)
            else Some (scene "Shop" / (if column = 1 then "Leave" else trade))
        (moveTo column row cursor, paging @ stepping @ signals, pointedOpt)

    (* Walking about and the dialog *)

    /// Exploring - confirm interacts, the menu button opens the menu, and the party button the party menu at a save
    /// point. Walking reads the intent itself (see ProcessKeyInput).
    let private updateExploring (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        let signals =
            if intent.Confirm && canInteract field then signal Interact
            elif intent.Menu && canOpenMenu field then affirm (signal MenuTeamOpen) world
            elif intent.Party && canOpenParty field then affirm (signal PartyMenuOpen) world
            else []
        (cursor, signals, None)

    /// The dialog - confirm turns the page, and cancel skips it where it can be skipped.
    let private updateDialog (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        let signals =
            if intent.Confirm && canInteract field then signal Interact
            elif intent.Cancel && canSkip field then affirm (signal SkipDialog) world
            else []
        (cursor, signals, None)

    (* All together *)

    /// Advance the cursor and act on what the player means this frame, returning the cursor, the signals for the
    /// field, and what the pointer should point at.
    let update (intent : Intent) (field : Field) (cursor : FieldCursor) world : FieldCursor * Signal list * Entity option =
        let cursor = follow field cursor
        match cursor.CursorMenu with
        | "" -> updateExploring intent field cursor world
        | "Dialog" -> updateDialog intent field cursor world
        | "Prompt" ->
            // the dialog's two answers, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals = if intent.Confirm then affirm (signal (if column = 0 then PromptLeft else PromptRight)) world else []
            (moveTo column 0 cursor, signals, Some (scene (if column = 0 then "Left" else "Right")))
        | "Team" ->
            match field.Menu.MenuState.Shown with
            | MenuTeam menuTeam -> updateTeam intent menuTeam field cursor world
            | _ -> (cursor, [], None)
        | "Equip" ->
            match field.Menu.MenuState with
            | MenuTeam ({ TeamEquipOpt = Some equip } as menuTeam) when field.Team.ContainsKey menuTeam.TeamIndex ->
                updateEquip intent menuTeam equip field cursor world
            | _ -> (cursor, [], None)
        | "EquipInfo" -> updateClose intent (scene "Info" / "Close") MenuTeamEquipMenuUseClose cursor world
        | "AutoMap" ->
            // back to the team menu or closing the menu, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then sound Assets.Field.AutoMapSound world; signal (if column = 0 then MenuTeamOpen else MenuClose)
                elif intent.Cancel then sound Assets.Field.AutoMapSound world; signal MenuTeamOpen
                elif intent.Menu then affirm (signal MenuClose) world
                else []
            (moveTo column 0 cursor, signals, Some (scene (if column = 0 then "AutoBack" else "AutoClose")))
        | "Inventory" ->
            updateItemPage intent "Inventory" (function Stash _ -> false | _ -> true) (fun page -> MenuInventorySelect page)
                MenuInventoryPageUp MenuInventoryPageDown field cursor world
        | "KeyItems" ->
            updateItemPage intent "KeyItems" (constant true) (fun page -> MenuKeyItemsSelect page)
                MenuKeyItemsPageUp MenuKeyItemsPageDown field cursor world
        | "Use" ->
            match field.Menu.MenuUseOpt with
            | Some ({ MenuUseSelection = (_, (Consumable _ as itemType)) }) -> updateUse intent itemType field cursor world
            | Some _ -> updateClose intent (scene "Use" / "Close") MenuInventoryCancel cursor world
            | None -> (cursor, [], None)
        | "Techs" ->
            match field.Menu.MenuState.Shown with
            | MenuTechs menuTechs -> updateTechs intent menuTechs field cursor world
            | _ -> (cursor, [], None)
        | "Tech" -> updateClose intent (scene "Tech" / "Close") MenuTechClose cursor world
        | "Options" -> updateOptions intent field cursor world
        | "Quit" ->
            // quitting or not, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then affirm (signal (if column = 0 then MenuOptionsQuitConfirm else MenuOptionsQuitCancel)) world
                elif intent.Cancel then affirm (signal MenuOptionsQuitCancel) world
                else []
            (moveTo column 0 cursor, signals, Some (scene "Options" / (if column = 0 then "QuitConfirm" else "QuitCancel")))
        | "Party" -> updateParty intent field cursor world
        | "Shop" ->
            match field.ShopOpt with
            | Some shop -> updateShop intent shop field cursor world
            | None -> (cursor, [], None)
        | "ShopConfirm" ->
            // buying or selling or not, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then affirm (signal (if column = 0 then ShopConfirmAccept else ShopConfirmDecline)) world
                elif intent.Cancel then affirm (signal ShopConfirmDecline) world
                else []
            (moveTo column 0 cursor, signals, Some (scene "Dialog" / (if column = 0 then "Accept" else "Decline")))
        | _ -> (cursor, [], None)

    /// What the pointer points at for a cursor, without acting on anything.
    let tryGetPointed (field : Field) (cursor : FieldCursor) world =
        let (_, _, pointedOpt) = update Intent.empty field cursor world
        pointedOpt