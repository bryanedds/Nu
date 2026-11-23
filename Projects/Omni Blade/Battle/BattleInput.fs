// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

/// The battle's gamepad and keyboard input - a routine for the ring menus, one for choosing targets, and one for the
/// battle's end.
[<RequireQualifiedAccess>]
module BattleInput =

    let private signal (s : Signal) = [s]

    let private input allyIndex = Simulants.BattleScene / (CharacterIndex.toEntityName allyIndex + "+Input")

    /// A ring menu's items as the ring lays them out, clockwise from the top, and whether each can be chosen.
    let private getRingItems menu (ally : Character) (battle : Battle) =
        let items =
            match menu with
            | "Regular" -> [("Attack", (0, true)); ("Tech", (1, true)); ("Consumable", (2, true)); ("Defend", (3, true))]
            | "Consumable" ->
                battle.Inventory |> Inventory.getConsumables |> Seq.map (fun kvp -> (getCaseName kvp.Key, (getCaseTag kvp.Key, true))) |> List.ofSeq
            | "Tech" ->
                [for tech in ally.Techs do
                    let affordable =
                        match Map.tryFind tech Data.Value.Techs with
                        | Some techData -> techData.TechCost <= ally.TechPoints && not (Map.containsKey Silence ally.Statuses)
                        | None -> false
                    let castable =
                        if tech.ConjureTech then
                            match ally.ConjureChargeOpt with
                            | Some conjureCharge -> conjureCharge >= Constants.Battle.ChargeMax
                            | None -> true
                        else true
                    (getCaseName tech, (getCaseTag tech, affordable && castable))]
            | _ -> []
        items |> List.sortBy (fun (_, (tag, _)) -> tag) |> List.map (fun (name, (_, enabled)) -> (name, enabled)) |> Array.ofList

    let private getRingEntityName menu =
        match menu with
        | "Regular" -> "RegularMenu"
        | "Tech" -> "TechMenu"
        | _ -> "ConsumableMenu"

    /// A step through a turning ring - right to the item clockwise of the top, so the ring turns counterclockwise to
    /// bring it up, left the other way, and down or up to the item at the bottom - of two nearest the bottom of an odd
    /// ring, down the one clockwise of it and up the one counterclockwise.
    let private stepRing direction count index =
        match direction with
        | Rightward -> Selection.wrap 1 count index
        | Leftward -> Selection.wrap -1 count index
        | Downward -> Selection.wrap (count / 2) count index
        | Upward -> Selection.wrap -(count / 2) count index

    /// The item a ring was left on, where it's been turned to one, else its first.
    let private getRingFocusIndex allyIndex menu (items : (string * bool) array) world =
        let ring = input allyIndex / getRingEntityName menu
        match (if ring.GetExists world then ring.GetRingMenuFocusOpt world else None) with
        | Some focus -> items |> Array.tryFindIndex (fun (name, _) -> name = focus) |> Option.defaultValue 0
        | None -> 0

    let private TargetTie = 1.0f // how near two targets' corners must be to count as in the same column or row

    /// The targets an aim can choose from in the order left steps through them - right to left across the
    /// battlefield, top to bottom down each column. Columns are by the corner a target's placed by, which lines up
    /// targets placed in the same column whatever their sizes.
    let private getTargets aimType (battle : Battle) =
        Battle.getTargets aimType battle |>
        Map.toArray |>
        Array.sortWith (fun (_, left : Character) (_, right : Character) ->
            let (leftCorner, rightCorner) = (left.Perimeter.Min, right.Perimeter.Min)
            if abs (leftCorner.X - rightCorner.X) >= TargetTie then compare rightCorner.X leftCorner.X
            else compare right.Perimeter.Center.Y left.Perimeter.Center.Y) |>
        Array.map fst

    /// The ally whose input the cursor is in - one partway through choosing, else the one it was in while they can
    /// still choose, else the first that can.
    let private tryGetAlly (cursor : BattleCursor) (battle : Battle) =
        let allies = Battle.getCharactersHealthy battle |> Map.filter (fun index _ -> index.Ally)
        match allies |> Map.tryFindKey (fun _ ally -> match ally.CharacterInputState with TechMenu | ItemMenu | AimReticles _ -> true | _ -> false) with
        | Some allyIndex -> Some allyIndex
        | None ->
            let regular = allies |> Map.filter (fun _ ally -> ally.CharacterInputState = RegularMenu)
            match cursor.CursorAllyOpt with
            | Some allyIndex when regular.ContainsKey allyIndex -> Some allyIndex
            | _ -> regular |> Map.tryFindKey (fun _ _ -> true)

    /// The choice at the battle's end - retry or quit on a loss, otherwise just turning the page.
    let private updateResult (intent : Intent) (cursor : BattleCursor) (battle : Battle) world =
        match battle.DialogOpt with
        | Some dialog when Dialog.canAdvance id dialog ->
            let retryable = not battle.HardcoreMode && match battle.BattleState with BattleResult (_, LoseBattle) -> true | _ -> false
            let cursor = if cursor.CursorMenu <> "Result" then { cursor with CursorMenu = "Result"; CursorIndex = 0; CursorAllyOpt = None; CursorParents = [] } else cursor
            let index =
                match intent.Step with
                | Some (Leftward | Rightward) when retryable -> 1 - cursor.CursorIndex
                | _ -> if retryable then cursor.CursorIndex else 1
            let cursor = { cursor with CursorIndex = index }
            let signals =
                if intent.Confirm then
                    Selection.playAffirm world
                    signal (if index = 0 then Retry else InteractDialog)
                else []
            let pointed = Simulants.BattleScene / (if index = 0 then "Retry" else "DialogInteract")
            (cursor, signals, Some pointed)
        | _ -> ({ cursor with CursorMenu = "" }, [], None)

    /// Advance the cursor and act on what the player means this frame, returning the cursor, the signals for the
    /// battle, and what the pointer should point at.
    let update (intent : Intent) (battle : Battle) (cursor : BattleCursor) world : BattleCursor * Signal list * Entity option =
        match battle.BattleState with
        | BattleRunning when battle.DialogOpt.IsNone ->
            match tryGetAlly cursor battle with
            | Some allyIndex ->
                let ally = (Battle.getAllies battle)[allyIndex]
                let menu =
                    match ally.CharacterInputState with
                    | RegularMenu -> "Regular"
                    | TechMenu -> "Tech"
                    | ItemMenu -> "Consumable"
                    | AimReticles _ -> "Reticles"
                    | NoInput -> ""

                // follow the ally's input - back to what it was on when a menu opened over that one closes, otherwise
                // each ring on the item it was left on, else its first, and the reticles where aiming starts
                let cursor =
                    if cursor.CursorAllyOpt <> Some allyIndex || cursor.CursorMenu <> menu then
                        let sameAlly = cursor.CursorAllyOpt = Some allyIndex
                        match cursor.CursorParents with
                        | (parent, index) :: parents when sameAlly && parent = menu ->
                            { cursor with CursorMenu = menu; CursorIndex = index; CursorParents = parents }
                        | _ ->
                            let index =
                                match menu with
                                // the first in left's order, the top of the rightmost, allies and enemies alike
                                | "Reticles" -> 0
                                | "Regular" | "Tech" | "Consumable" -> getRingFocusIndex allyIndex menu (getRingItems menu ally battle) world
                                | _ -> 0
                            let submenu =
                                match (cursor.CursorMenu, menu) with
                                | ("Regular", ("Tech" | "Consumable" | "Reticles")) | (("Tech" | "Consumable"), "Reticles") -> true
                                | (_, _) -> false
                            let parents = if sameAlly && submenu then (cursor.CursorMenu, cursor.CursorIndex) :: cursor.CursorParents else []
                            { cursor with CursorAllyOpt = Some allyIndex; CursorMenu = menu; CursorIndex = index; CursorParents = parents }
                    else cursor

                match ally.CharacterInputState with
                | RegularMenu | TechMenu | ItemMenu ->

                    // a turning ring menu
                    let items = getRingItems menu ally battle
                    let index = if items.Length = 0 then 0 else min cursor.CursorIndex (dec items.Length)
                    let index = match intent.Step with Some direction -> stepRing direction items.Length index | None -> index
                    let cursor = { cursor with CursorIndex = index }

                    // switching between allies waiting on the regular menu, tab or start going to the next
                    let previous = intent.PageUp
                    let next = intent.PageDown || intent.Menu
                    let cursor =
                        if menu = "Regular" && (previous || next) then
                            let waiting = Battle.getCharactersHealthy battle |> Map.filter (fun i a -> i.Ally && a.CharacterInputState = RegularMenu) |> Map.toKeyArray
                            match Array.tryFindIndex ((=) allyIndex) waiting with
                            | Some i ->
                                // the next ally's menu picks up at the item its own ring was left on, else its first
                                let allyIndex' = waiting[Selection.wrap (if previous then -1 else 1) waiting.Length i]
                                if allyIndex' <> allyIndex then
                                    let index = getRingFocusIndex allyIndex' menu items world
                                    { cursor with CursorAllyOpt = Some allyIndex'; CursorIndex = index; CursorParents = [] }
                                else cursor
                            | None -> cursor
                        else cursor
                    let signals =
                        if intent.Confirm && items.Length > 0 then
                            let (name, enabled) = items[index]
                            if enabled then
                                Selection.playAffirm world
                                match menu with
                                | "Regular" -> signal (RegularItemSelect (allyIndex, name))
                                | "Tech" -> signal (TechItemSelect (allyIndex, name))
                                | _ -> signal (ConsumableItemSelect (allyIndex, name))
                            else Selection.playMistake world; []
                        elif intent.Cancel then
                            match menu with
                            | "Tech" -> Selection.playAffirm world; signal (TechItemCancel allyIndex)
                            | "Consumable" -> Selection.playAffirm world; signal (ConsumableItemCancel allyIndex)
                            | _ -> []
                        else []
                    let pointedOpt =
                        if items.Length > 0 && cursor.CursorAllyOpt = Some allyIndex
                        then Some (input allyIndex / getRingEntityName menu / scstring (fst items[index]))
                        else None
                    (cursor, signals, pointedOpt)

                | AimReticles (_, aimType) ->

                    // choosing a target
                    let targets = getTargets aimType battle
                    let index = if targets.Length = 0 then 0 else min cursor.CursorIndex (dec targets.Length)
                    let cursor =
                        match intent.Step with
                        | Some direction when targets.Length > 0 ->
                            // allies or enemies left through their order and right back, and down through rows top to
                            // bottom, each left to right, and up back, each wrapping round at the ends - rows by the
                            // corner a target's placed by, as columns are
                            let index =
                                match direction with
                                | Leftward -> Selection.wrap 1 targets.Length index
                                | Rightward -> Selection.wrap -1 targets.Length index
                                | Upward | Downward ->
                                    let corners =
                                        let characters = Battle.getCharacters battle
                                        targets |> Array.map (fun target -> characters[target].Perimeter.Min)
                                    let rows =
                                        [|0 .. dec corners.Length|] |>
                                        Array.sortWith (fun i j ->
                                            if abs (corners[i].Y - corners[j].Y) >= TargetTie then compare corners[j].Y corners[i].Y
                                            else compare corners[i].X corners[j].X)
                                    let place = Array.findIndex ((=) index) rows
                                    let place = Selection.wrap (if direction = Downward then 1 else -1) rows.Length place
                                    rows[place]
                            { cursor with CursorIndex = index }
                        | _ -> { cursor with CursorIndex = index }
                    let index = cursor.CursorIndex
                    let signals =
                        if intent.Confirm && targets.Length > 0 then Selection.playAffirm world; signal (ReticlesSelect (allyIndex, targets[index]))
                        elif intent.Cancel then Selection.playAffirm world; signal (ReticlesCancel allyIndex)
                        else []
                    // the target itself, as its reticle hides
                    let pointedOpt =
                        if targets.Length > 0
                        then Some (Simulants.BattleScene / CharacterIndex.toEntityName targets[index])
                        else None
                    (cursor, signals, pointedOpt)

                | NoInput -> (cursor, [], None)
            | None -> (cursor, [], None)
        | _ -> updateResult intent cursor battle world

    /// What the pointer points at for a cursor, without acting on anything.
    let tryGetPointed (battle : Battle) (cursor : BattleCursor) world =
        let (_, _, pointedOpt) = update Intent.empty battle cursor world
        pointedOpt

    /// The ring that turns to the cursor's item and that item.
    let tryGetTurningRingFocus (battle : Battle) (cursor : BattleCursor) world =
        match (cursor.CursorMenu, cursor.CursorAllyOpt, tryGetPointed battle cursor world) with
        | (("Regular" | "Tech" | "Consumable") as menu, Some allyIndex, Some _) ->
            match Map.tryFind allyIndex (Battle.getAllies battle) with
            | Some ally ->
                let items = getRingItems menu ally battle
                if items.Length > 0
                then Some (input allyIndex / getRingEntityName menu, fst items[min cursor.CursorIndex (dec items.Length)])
                else None
            | None -> None
        | (_, _, _) -> None