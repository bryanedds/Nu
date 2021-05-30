// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module FieldDispatcher =

    type [<NoEquality; NoComparison>] FieldMessage =
        | Update
        | UpdateAvatar of Avatar
        | UpdateFieldTransition
        | MenuTeamOpen
        | MenuTeamAlly of int
        | MenuItemOpen
        | MenuItemSelect of Lens<int * (ItemType * int Option), World>
        | MenuItemUse of int
        | MenuItemCancel
        | MenuTechOpen
        | MenuTechAlly of int
        | MenuTechSelect
        | MenuClose
        | ShopBuy
        | ShopSell
        | ShopPageUp
        | ShopPageDown
        | ShopSelect of Lens<int * (ItemType * int Option), World>
        | ShopConfirmAccept
        | ShopConfirmDecline
        | ShopLeave
        | PromptLeft
        | PromptRight
        | TryBattle of BattleType * Advent Set
        | Interact

    type [<NoEquality; NoComparison>] FieldCommand =
        | UpdateEye
        | PlayFieldSong
        | PlaySound of int64 * single * Sound AssetTag
        | PlaySong of int * single * Song AssetTag
        | FadeOutSong of int
        | Nop

    type Screen with
        member this.GetField = this.GetModel<Field>
        member this.SetField = this.SetModel<Field>
        member this.Field = this.Model<Field> ()

    type FieldDispatcher () =
        inherit ScreenDispatcher<Field, FieldMessage, FieldCommand> (Field.debug)

        static let detokenize (text : string) (field : Field) =
            text
                .Replace("$FEE", scstring (Field.getRecruitmentFee field))
                .Replace("$GOLD", scstring field.Inventory.Gold)

        static let pageItems pageIndex pageSize items =
            items |>
            Seq.chunkBySize pageSize |>
            Seq.trySkip pageIndex |>
            Seq.map List.ofArray |>
            Seq.tryHead |>
            Option.defaultValue [] |>
            Seq.indexed |>
            Map.ofSeq

        static let isFacingBodyShape bodyShape (avatar : Avatar) world =
            if bodyShape.Entity.Is<PropDispatcher> world then
                let v = bodyShape.Entity.GetBottom world - avatar.Bottom
                let direction = Direction.ofVector2 v
                direction = avatar.Direction
            else false

        static let getFacingBodyShapes (avatar : Avatar) world =
            List.filter
                (fun bodyShape -> isFacingBodyShape bodyShape avatar world)
                avatar.IntersectedBodyShapes

        static let tryGetFacingProp (avatar : Avatar) world =
            match getFacingBodyShapes avatar world with
            | head :: _ -> Some head.Entity
            | [] -> None

        static let isTouchingSavePoint (avatar : Avatar) world =
            List.exists (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | SavePoint -> true
                | _ -> false)
                avatar.IntersectedBodyShapes

        static let tryGetFacingInteraction dialogOpt advents prop field =
            match dialogOpt with
            | None ->
                match prop with
                | Portal (_, _, _, _, _, _, _) -> None
                | Door _ -> Some "Open"
                | Chest (_, _, chestId, _, _, _) -> if Set.contains (Opened chestId) advents then None else Some "Open"
                | Switch (_, _, _, _) -> Some "Use"
                | Sensor (_, _, _, _, _) -> None
                | Npc _ | NpcBranching _ -> Some "Talk"
                | Shopkeep _ -> Some "Shop"
                | Seal _ -> Some "Touch"
                | Flame _ -> None
                | SavePoint -> None
                | ChestSpawn -> None
                | EmptyProp -> None
            | Some dialog ->
                if  Dialog.canAdvance (flip detokenize field) dialog &&
                    not
                        (Dialog.isExhausted (flip detokenize field) dialog &&
                         Option.isSome dialog.DialogPromptOpt)
                then Some "Next"
                else None

        static let tryGetInteraction dialogOpt advents (avatar : Avatar) field world =
            if isTouchingSavePoint avatar world then
                Some "Save"
            else
                match tryGetFacingProp avatar world with
                | Some prop -> tryGetFacingInteraction dialogOpt advents (prop.GetProp world).PropData field
                | None -> None

        static let tryGetTouchingPortal omniSeedState (advents : Advent Set) (avatar : Avatar) world =
            avatar.IntersectedBodyShapes |>
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Portal (_, _, _, fieldType, portalIndex, _, requirements) ->
                    if advents.IsSupersetOf requirements then
                        match Map.tryFind fieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetPortal omniSeedState portalIndex fieldData world with
                            | Some portal ->
                                match portal.PropData with
                                | Portal (_, _, direction, _, _, extended, _) ->
                                    let destination =
                                        match direction with
                                        | Upward -> portal.PropBounds.Top + v2 0.0f 8.0f + if extended then v2 0.0f 48.0f else v2Zero
                                        | Rightward -> portal.PropBounds.Right + v2 32.0f 0.0f + if extended then v2 48.0f 0.0f else v2Zero
                                        | Downward -> portal.PropBounds.Bottom + v2 0.0f -54.0f - if extended then v2 0.0f 48.0f else v2Zero
                                        | Leftward -> portal.PropBounds.Left + v2 -32.0f 0.0f - if extended then v2 48.0f 0.0f else v2Zero
                                    Some (fieldType, destination, direction)
                                | _ -> None
                            | None -> None
                        | None -> None
                    else None
                | _ -> None) |>
            List.tryHead

        static let getTouchedSensors (avatar : Avatar) world =
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None)
                avatar.CollidedBodyShapes

        static let getUntouchedSensors (avatar : Avatar) world =
            List.choose (fun shape ->
                match (shape.Entity.GetProp world).PropData with
                | Sensor (sensorType, _, _, cue, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None)
                avatar.SeparatedBodyShapes

        static let interactDialog dialog field =
            match Dialog.tryAdvance (flip detokenize field) dialog with
            | (true, dialog) ->
                let field = Field.updateDialogOpt (constant (Some dialog)) field
                just field
            | (false, dialog) ->
                let field = Field.updateDialogOpt (constant None) field
                match dialog.DialogBattleOpt with
                | Some (battleType, consequence) -> withMsg (TryBattle (battleType, consequence)) field
                | None -> just field

        static let interactChest itemType chestId battleTypeOpt cue requirements (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateAdvents (Set.add (Opened chestId)) field
                // TODO: P1: rewrite this to use two new cues, Find and Guarded.
                let field = Field.updateInventory (Inventory.tryAddItem itemType >> snd) field
                let field =
                    match battleTypeOpt with
                    | Some battleType -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!^But something approaches!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = Some (battleType, Set.empty) })) field
                    | None -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                let field = Field.updateCue (constant cue) field
                withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestOpenSound)) field
            else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field)

        static let interactDoor cue requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | DoorState false ->
                if field.Advents.IsSupersetOf requirements then
                    let field = Field.updateCue (constant cue) field
                    let field = Field.updatePropStates (Map.add prop.PropId (DoorState true)) field
                    withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorOpenSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field)
            | _ -> failwithumf ()

        static let interactSwitch cue cue2 requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | SwitchState on ->
                if field.Advents.IsSupersetOf requirements then
                    let on = not on
                    let field = Field.updatePropStates (Map.add prop.PropId (SwitchState on)) field
                    let field = Field.updateCue (constant (if on then cue else cue2)) field
                    withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UseSwitchSound)) field
                else just (Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Won't budge!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field)
            | _ -> failwithumf ()
        
        static let interactNpc branches requirements (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let branchesFiltered = branches |> List.choose (fun branch -> if field.Advents.IsSupersetOf branch.Requirements then Some branch.Cue else None) |> List.rev
                let branchCue = match List.tryHead branchesFiltered with Some cue -> cue | None -> Dialog "..."
                let field = Field.updateCue (constant branchCue) field
                just field
            else just field

        static let interactShopkeep shopType (field : Field) =
            let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
            let field = Field.updateShopOpt (constant (Some shop)) field
            withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field

        static let interactSavePoint (field : Field) =
            let field = Field.restoreTeam field
            Field.save field
            withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SaveSound)) field

        static let rec runCue cue (field : Field) world : Cue * (Signal<FieldMessage, FieldCommand> list * Field) =

            match cue with
            | Cue.Nil ->
                (cue, just field)

            | Cue.PlaySound (volume, sound) ->
                (Cue.Nil, withCmd (PlaySound (0L, volume, sound)) field)

            | Cue.PlaySong (fade, volume, song) ->
                (Cue.Nil, withCmd (PlaySong (fade, volume, song)) field)

            | Cue.FadeOutSong fade ->
                (Cue.Nil, withCmd (FadeOutSong fade) field)

            | Cue.Face (direction, target) ->
                match target with
                | AvatarTarget ->
                    let field = Field.updateAvatar (Avatar.updateDirection (constant direction)) field
                    (Cue.Nil, just field)
                | NpcTarget npcType ->
                    match Map.tryFindKey (constant (function NpcState (npcType2, _, _, _, _) -> npcType2 = npcType | _ -> false)) field.PropStates with
                    | Some propKey ->
                        match field.PropStates.[propKey] with
                        | NpcState (_, _, color, glow, exists) ->
                            let field = Field.updatePropStates (Map.add propKey (NpcState (npcType, direction, color, glow, exists))) field
                            (Cue.Nil, just field)
                        | _ -> (Cue.Nil, just field)
                    | None -> (Cue.Nil, just field)
                | ShopkeepTarget _ | AllyTarget _ | EnemyTarget _ ->
                    (Cue.Nil, just field)

            | Glow (glow, target) ->
                match target with
                | NpcTarget npcType ->
                    match Map.tryFindKey (constant (function NpcState (npcType2, _, _, _, _) -> npcType2 = npcType | _ -> false)) field.PropStates with
                    | Some propKey ->
                        match field.PropStates.[propKey] with
                        | NpcState (_, direction, color, _, exists) ->
                            let field = Field.updatePropStates (Map.add propKey (NpcState (npcType, direction, color, glow, exists))) field
                            (Cue.Nil, just field)
                        | _ -> (Cue.Nil, just field)
                    | None -> (Cue.Nil, just field)
                | AvatarTarget _ | ShopkeepTarget _ | AllyTarget _ | EnemyTarget _ ->
                    (Cue.Nil, just field)

            | Animate (characterAnimationType, target) ->
                match target with
                | AvatarTarget ->
                    let field = Field.updateAvatar (Avatar.animate (World.getTickTime world) characterAnimationType) field
                    (Cue.Nil, just field)
                | NpcTarget _ | ShopkeepTarget _ | AllyTarget _ | EnemyTarget _ ->
                    (Cue.Nil, just field)

            | Recruit allyType ->
                let fee = Field.getRecruitmentFee field
                if field.Inventory.Gold >= fee then
                    let advent =
                        match allyType with
                        | Jinn -> failwithumf ()
                        | Shade -> ShadeRecruited
                        | Mael -> MaelRecruited
                        | Riain -> RiainRecruited
                        | Peric -> PericRecruited
                    let field = Field.recruit allyType field
                    let field = Field.updateAdvents (Set.add advent) field
                    let field = Field.updateInventory (Inventory.updateGold (fun gold -> gold - fee)) field
                    (Cue.Nil, withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field)
                else runCue (Parallel [Dialog "You don't have enough..."; Cue.PlaySound (Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)]) field world

            | Unseal (fee, consequent) ->
                if field.Inventory.Gold >= fee then
                    let field = Field.updateInventory (Inventory.updateGold (fun gold -> gold - fee)) field
                    let field = Field.updateAdvents (Set.add consequent) field
                    (Cue.Nil, withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SealedSound)) field) // TODO: P1: rename sound to Unsealed.
                else runCue (Parallel [Dialog "You don't have enough..."; Cue.PlaySound (Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)]) field world

            | AddItem itemType ->
                (Cue.Nil, just (Field.updateInventory (Inventory.tryAddItem itemType >> snd) field))

            | RemoveItem itemType ->
                (Cue.Nil, just (Field.updateInventory (Inventory.tryRemoveItem itemType >> snd) field))

            | AddAdvent advent ->
                (Cue.Nil, just (Field.updateAdvents (Set.add advent) field))

            | RemoveAdvent advent ->
                (Cue.Nil, just (Field.updateAdvents (Set.remove advent) field))

            | Wait time ->
                (WaitState (World.getTickTime world + time), just field)

            | WaitState time ->
                if World.getTickTime world < time
                then (cue, just field)
                else (Cue.Nil, just field)

            | Fade (time, fadeIn, target) ->
                (FadeState (World.getTickTime world, time, fadeIn, target), just field)

            | FadeState (startTime, totalTime, fadeIn, target) ->
                match target with
                | NpcTarget npcType ->
                    match Map.tryFindKey (constant (function NpcState (npcType2, _, _, _, _) -> npcType2 = npcType | _ -> false)) field.PropStates with
                    | Some propKey ->
                        match field.PropStates.[propKey] with
                        | NpcState (_, direction, _, glow, exists) ->
                            let localTime = World.getTickTime world - startTime
                            let progress = single localTime / single totalTime
                            let color = colWhite * if fadeIn then progress else 1.0f - progress
                            let field = Field.updatePropStates (Map.add propKey (NpcState (npcType, direction, color, glow, exists))) field
                            (Cue.Nil, just field)
                        | _ -> (Cue.Nil, just field)
                    | None -> (Cue.Nil, just field)
                | AvatarTarget _ | ShopkeepTarget _ | AllyTarget _ | EnemyTarget _ ->
                    (Cue.Nil, just field)

            | Warp (fieldType, fieldDestination, fieldDirection) ->
                match field.FieldTransitionOpt with
                | Some _ ->
                    (cue, just field)
                | None ->
                    let fieldTransition =
                        { FieldType = fieldType
                          FieldDestination = fieldDestination
                          FieldDirection = fieldDirection
                          FieldTransitionTime = World.getTickTime world + Constants.Field.TransitionTime }
                    let field = Field.updateFieldTransitionOpt (constant (Some fieldTransition)) field
                    (WarpState, just field)

            | WarpState ->
                match field.FieldTransitionOpt with
                | Some _ -> (cue, just field)
                | None -> (Cue.Nil, just field)

            | Battle (battleType, consequents) ->
                match field.BattleOpt with
                | Some _ -> (cue, just field)
                | None -> (BattleState, withMsg (TryBattle (battleType, consequents)) field)

            | BattleState ->
                match field.BattleOpt with
                | Some _ -> (cue, just field)
                | None -> (Cue.Nil, just field)

            | Dialog text ->
                match field.DialogOpt with
                | Some _ ->
                    (cue, just field)
                | None ->
                    let dialog = { DialogForm = DialogThick; DialogTokenized = text; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None }
                    let field = Field.updateDialogOpt (constant (Some dialog)) field
                    (DialogState, just field)

            | DialogState ->
                match field.DialogOpt with
                | None -> (Cue.Nil, just field)
                | Some _ -> (cue, just field)

            | Prompt (text, leftPrompt, rightPrompt) ->
                match field.DialogOpt with
                | Some _ ->
                    (cue, just field)
                | None ->
                    let dialog = { DialogForm = DialogThick; DialogTokenized = text; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = Some (leftPrompt, rightPrompt); DialogBattleOpt = None }
                    let field = Field.updateDialogOpt (constant (Some dialog)) field
                    (PromptState, just field)

            | PromptState ->
                match field.DialogOpt with
                | None -> (Cue.Nil, just field)
                | Some _ -> (cue, just field)

            | If (requirements, consequent, alternate) ->
                if field.Advents.IsSupersetOf requirements
                then (consequent, just field)
                else (alternate, just field)

            | Not (requirements, consequent, alternate) ->
                if not (field.Advents.IsSupersetOf requirements)
                then (consequent, just field)
                else (alternate, just field)

            | Parallel cues ->
                let (cues, (signals, field)) =
                    List.fold (fun (cues, (signals, field)) cue ->
                        let (cue, (signals2, field)) = runCue cue field world
                        if Cue.isNil cue
                        then (cues, (signals @ signals2, field))
                        else (cues @ [cue], (signals @ signals2, field)))
                        ([], ([], field))
                        cues
                match cues with
                | _ :: _ -> (Parallel cues, (signals, field))
                | [] -> (Cue.Nil, (signals, field))

            | Sequence cues ->
                let (_, haltedCues, (signals, field)) =
                    List.fold (fun (halted, haltedCues, (signals, field)) cue ->
                        if halted
                        then (halted, haltedCues @ [cue], (signals, field))
                        else
                            let (cue, (signals2, field)) = runCue cue field world
                            if Cue.isNil cue
                            then (false, [], (signals @ signals2, field))
                            else (true, [cue], (signals @ signals2, field)))
                        (false, [], ([], field))
                        cues
                match haltedCues with
                | _ :: _ -> (Sequence haltedCues, (signals, field))
                | [] -> (Cue.Nil, (signals, field))

        static let sidebar position elevation (field : Lens<Field, World>) =
            Content.association Gen.name []
                [Content.button Gen.name
                   [Entity.PositionLocal == position; Entity.ElevationLocal == elevation; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "T"
                    Entity.EnabledLocal <== field --> fun field -> match field.Menu.MenuState with MenuTeam _ -> false | _ -> true
                    Entity.ClickEvent ==> msg MenuTeamOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 80.0f; Entity.ElevationLocal == elevation; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "I"
                    Entity.EnabledLocal <== field --> fun field -> match field.Menu.MenuState with MenuItem _ -> false | _ -> true
                    Entity.ClickEvent ==> msg MenuItemOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 160.0f; Entity.ElevationLocal == elevation; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "T"
                    Entity.EnabledLocal <== field --> fun field -> match field.Menu.MenuState with MenuTech _ -> false | _ -> true
                    Entity.ClickEvent ==> msg MenuTechOpen]
                 Content.button Gen.name
                   [Entity.PositionLocal == position - v2 0.0f 400.0f; Entity.ElevationLocal == elevation; Entity.Size == v2 72.0f 72.0f
                    Entity.Text == "X"
                    Entity.ClickEvent ==> msg MenuClose]]

        static let team (position : Vector2) elevation rows (field : Lens<Field, World>) filter fieldMsg =
            Content.entities field
                (fun field _ -> (field.Team, field.Menu))
                (fun (team, menu) _ -> Map.map (fun _ teammate -> (teammate, menu)) team)
                (fun index teammateAndMenu _ ->
                    let x = position.X + if index >= rows then 256.0f + 48.0f else 0.0f
                    let y = position.Y - single (index % rows) * 72.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.ElevationLocal == elevation; Entity.Size == v2 256.0f 72.0f
                         Entity.EnabledLocal <== teammateAndMenu --> fun (teammate, menu) -> filter teammate menu
                         Entity.Text <== teammateAndMenu --> fun (teammate, _) -> CharacterType.getName teammate.CharacterType
                         Entity.ClickEvent ==> msg (fieldMsg index)])

        static let items (position : Vector2) elevation columns field fieldMsg =
            Content.entities field
                (fun (field : Field) _ -> (field.Menu, field.ShopOpt, field.Inventory))
                (fun (menu, shopOpt, inventory : Inventory) _ ->
                    let sorter (item, _) = match item with Consumable _ -> 0 | Equipment _ -> 1 | KeyItem _ -> 2 | Stash _ -> 3
                    match menu.MenuState with
                    | MenuItem menu -> inventory.Items |> Map.toSeq |> Seq.sortBy sorter |> Seq.index |> pageItems menu.ItemPage 10 |> Map.map (fun _ (i, (item, count)) -> (i, (item, Some count)))
                    | _ ->
                        match shopOpt with
                        | Some shop ->
                            match shop.ShopState with
                            | ShopBuying ->
                                match Map.tryFind shop.ShopType Data.Value.Shops with
                                | Some shopData -> shopData.ShopItems |> List.indexed |> pageItems shop.ShopPage 10 |> Map.map (fun _ (i, item) -> (i, (item, None)))
                                | None -> Map.empty
                            | ShopSelling ->
                                inventory.Items |> Map.toSeq |> Seq.sortBy sorter |> Seq.index |>
                                Seq.choose (function (_, (Equipment _, _) as item) | (_, (Consumable _, _) as item) -> Some item | (_, (KeyItem _, _)) | (_, (Stash _, _)) -> None) |>
                                pageItems shop.ShopPage 10 |> Map.map (fun _ (i, (item, count)) -> (i, (item, Some count)))
                        | None -> Map.empty)
                (fun i selectionLens _ ->
                    let x = if i < columns then position.X else position.X + 368.0f
                    let y = position.Y - single (i % columns) * 80.0f
                    Content.button Gen.name
                        [Entity.PositionLocal == v2 x y; Entity.ElevationLocal == elevation; Entity.Size == v2 336.0f 72.0f
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v2 16.0f 0.0f
                         Entity.Text <== selectionLens --> fun (_, (itemType, countOpt)) -> match countOpt with Some count when count > 1 -> ItemType.getName itemType + " x" + string count | _ -> ItemType.getName itemType
                         Entity.EnabledLocal <== selectionLens --> fun (_, (itemType, _)) -> match itemType with Consumable _ | Equipment _ -> true | KeyItem _ | Stash _ -> false
                         Entity.ClickEvent ==> msg (fieldMsg selectionLens)])

        override this.Channel (_, field) =
            [Simulants.Field.Scene.Avatar.Avatar.ChangeEvent =|> fun evt -> msg (UpdateAvatar (evt.Data.Value :?> Avatar))
             field.UpdateEvent => msg Update
             field.PostUpdateEvent => msg UpdateFieldTransition
             field.PostUpdateEvent => cmd UpdateEye
             field.SelectEvent => cmd PlayFieldSong]

        override this.Message (field, message, _, world) =

            match message with
            | Update ->

                // update cue
                let (cue, (signals, field)) = runCue field.Cue field world
                let field = Field.updateCue (constant cue) field

                // update dialog
                let field =
                    match field.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.update dialog world
                        Field.updateDialogOpt (constant (Some dialog)) field
                    | None -> field

                // update portal
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        match tryGetTouchingPortal field.OmniSeedState field.Advents field.Avatar world with
                        | Some (fieldType, destination, direction) ->
                            if Option.isNone field.BattleOpt then // make sure we don't teleport if a battle is started earlier in the frame
                                let transition =
                                    { FieldType = fieldType
                                      FieldDestination = destination
                                      FieldDirection = direction
                                      FieldTransitionTime = World.getTickTime world + Constants.Field.TransitionTime }
                                let field = Field.updateFieldTransitionOpt (constant (Some transition)) field
                                (cmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepStairSound)) :: signals, field)
                            else (signals, field)
                        | None -> (signals, field)
                    | Some _ -> (signals, field)

                // update sensor
                let (signals, field) =
                    match field.FieldTransitionOpt with
                    | None ->
                        let sensors = getTouchedSensors field.Avatar world
                        let results =
                            List.fold (fun (signals : Signal<FieldMessage, FieldCommand> list, field : Field) (sensorType, cue, requirements) ->
                                if field.Advents.IsSupersetOf requirements then
                                    let field = Field.updateCue (constant cue) field
                                    match sensorType with
                                    | AirSensor -> (signals, field)
                                    | HiddenSensor | StepPlateSensor -> (Command (PlaySound (0L,  Constants.Audio.SoundVolumeDefault, Assets.Field.StepPlateSound)) :: signals, field)
                                else (signals, field))
                                (signals, field) sensors
                        results
                    | Some _ -> (signals, field)

                // update spirits
                let (signals, field) =
#if !DEV_FIELD
                    if  field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt then
                        match Field.advanceSpirits field world with
                        | Left (battleData, field) ->
                            let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
                            let battle = Battle.makeFromTeam field.Inventory prizePool field.Team battleData (World.getTickTime world)
                            let field = Field.updateBattleOpt (constant (Some battle)) field
                            (cmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound)) :: signals, field)
                        | Right field -> (signals, field)
                    else (signals, field)
#else
                    (signals, field)
#endif

                // fin
                (signals, field)

            | UpdateAvatar avatar ->
                let field = Field.updateAvatar (constant avatar) field
                just field

            | UpdateFieldTransition ->

                // check if transitioning
                match field.FieldTransitionOpt with
                | Some fieldTransition ->

                    // handle field transition
                    let time = World.getTickTime world
                    let currentSongOpt = world |> World.getCurrentSongOpt |> Option.map (fun song -> song.Song)
                    let (signals, field) =

                        // start transition
                        if time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime then
                            match Data.Value.Fields.TryGetValue fieldTransition.FieldType with
                            | (true, fieldData) ->
                                match (currentSongOpt, fieldData.FieldSongOpt) with
                                | (Some song, Some song2) when assetEq song song2 -> just field
                                | (_, _) -> withCmd (FadeOutSong Constants.Audio.FadeOutMsDefault) field
                            | (false, _) -> just field
                            
                        // half-way point of transition
                        elif time = fieldTransition.FieldTransitionTime - Constants.Field.TransitionTime / 2L then
                            let field = Field.updateFieldType (constant fieldTransition.FieldType) field
                            let field =
                                Field.updateAvatar (fun avatar ->
                                    let avatar = Avatar.updateDirection (constant fieldTransition.FieldDirection) avatar
                                    let avatar = Avatar.updateIntersectedBodyShapes (constant []) avatar
                                    let avatar = Avatar.updateBottom (constant fieldTransition.FieldDestination) avatar
                                    avatar)
                                    field
                            let songCmd =
                                match Field.getFieldSongOpt field with
                                | Some fieldSong ->
                                    match currentSongOpt with
                                    | Some song when assetEq song fieldSong -> Nop
                                    | _ -> PlaySong (Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, fieldSong)
                                | None -> Nop
                            withCmd songCmd field

                        // finish transition
                        elif time = fieldTransition.FieldTransitionTime then
                            let field = Field.updateFieldTransitionOpt (constant None) field
                            just field

                        // intermediate state
                        else just field

                    // update field reference to make sure transition binding actuates
                    let field = Field.updateReference field
                    (signals, field)

                // no transition
                | None -> just field

            | MenuTeamOpen ->
                let state = MenuTeam { TeamIndex = 0; TeamIndices = Map.toKeyList field.Team }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field

            | MenuTeamAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTeam menuTeam -> MenuTeam { menuTeam with TeamIndex = index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field

            | MenuItemOpen ->
                let itemState = MenuItem { ItemPage = 0 }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = itemState }) field
                just field

            | MenuItemSelect selectionLens ->
                let selection = Lens.get selectionLens world
                let selection = (fst selection, fst (snd selection))
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = MenuUse.tryMakeFromSelection selection }) field
                just field

            | MenuItemUse index ->
                match Map.tryFind index field.Team with
                | Some teammate ->
                    match field.Menu.MenuUseOpt with
                    | Some menuUse ->
                        let itemType = snd menuUse.MenuUseSelection
                        let (result, displacedOpt, teammate) = Teammate.tryUseItem itemType teammate
                        let field = if result then Field.updateInventory (Inventory.tryRemoveItem itemType >> snd) field else field
                        let field = match displacedOpt with Some displaced -> Field.updateInventory (Inventory.tryAddItem displaced >> snd) field | None -> field
                        let field = Field.updateTeam (Map.add index teammate) field
                        let field = Field.updateMenu (constant { field.Menu with MenuUseOpt = None }) field
                        if result then withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)) field
                        else just field
                    | None -> just field
                | None -> just field

            | MenuItemCancel ->
                let field = Field.updateMenu (fun menu -> { menu with MenuUseOpt = None }) field
                just field

            | MenuTechOpen ->
                let state = MenuTech { TechIndexOpt = None }
                let field = Field.updateMenu (fun menu -> { menu with MenuState = state }) field
                just field
            
            | MenuTechAlly index ->
                let field =
                    Field.updateMenu (fun menu ->
                        let state =
                            match menu.MenuState with
                            | MenuTech menuTech -> MenuTech { menuTech with TechIndexOpt = Some index }
                            | state -> state
                        { menu with MenuState = state })
                        field
                just field
            
            | MenuTechSelect ->
                just field
            
            | MenuClose ->
                let field = Field.updateMenu (fun menu -> { menu with MenuState = MenuClosed }) field
                just field

            | ShopBuy ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopBuying; ShopPage = 0 })) field
                just field

            | ShopSell ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopState = ShopSelling; ShopPage = 0 })) field
                just field

            | ShopPageUp ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = max 0 (dec shop.ShopPage) })) field
                just field

            | ShopPageDown ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopPage = inc shop.ShopPage })) field
                just field

            | ShopSelect selectionLens ->
                let selection = Lens.get selectionLens world
                let selection = (fst selection, fst (snd selection))
                let field =
                    Field.updateShopOpt (Option.map (fun shop ->
                        let buying = match shop.ShopState with ShopBuying -> true | ShopSelling -> false
                        let shopConfirmOpt = ShopConfirm.tryMakeFromSelection buying field.Inventory selection
                        { shop with ShopConfirmOpt = shopConfirmOpt }))
                        field
                just field

            | ShopConfirmAccept ->
                match field.ShopOpt with
                | Some shop ->
                    match shop.ShopConfirmOpt with
                    | Some shopConfirm ->
                        let itemType = snd shopConfirm.ShopConfirmSelection
                        let valid =
                            match shop.ShopState with
                            | ShopBuying ->
                                Inventory.canAddItem itemType field.Inventory &&
                                field.Inventory.Gold >= shopConfirm.ShopConfirmPrice
                            | ShopSelling -> true
                        if valid then
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.tryAddItem itemType >> snd | ShopSelling -> Inventory.tryRemoveItem itemType >> snd) field
                            let field = Field.updateInventory (match shop.ShopState with ShopBuying -> Inventory.updateGold (fun gold -> gold - shopConfirm.ShopConfirmPrice) | ShopSelling -> Inventory.updateGold (fun gold -> gold + shopConfirm.ShopConfirmPrice)) field
                            let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                            withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field
                        else withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)) field
                    | None -> just field
                | None -> just field

            | ShopConfirmDecline ->
                let field = Field.updateShopOpt (Option.map (fun shop -> { shop with ShopConfirmOpt = None })) field
                just field

            | ShopLeave ->
                let field = Field.updateShopOpt (constant None) field
                just field

            | PromptLeft ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some ((_, promptCue), _) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | PromptRight ->
                match field.DialogOpt with
                | Some dialog ->
                    match dialog.DialogPromptOpt with
                    | Some (_, (_, promptCue)) ->
                        let field = Field.updateDialogOpt (constant None) field
                        let field = Field.updateCue (constant promptCue) field
                        just field
                    | None -> just field
                | None -> just field

            | TryBattle (battleType, consequents) ->
                match Map.tryFind battleType Data.Value.Battles with
                | Some battleData ->
                    let time = World.getTickTime world
                    let prizePool = { Consequents = consequents; Items = []; Gold = 0; Exp = 0 }
                    let battle = Battle.makeFromTeam field.Inventory prizePool (Field.getParty field) battleData time
                    let field = Field.updateBattleOpt (constant (Some battle)) field
                    withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastGrowlSound)) field
                | None -> just field

            | Interact ->
                match field.DialogOpt with
                | None ->
                    if isTouchingSavePoint field.Avatar world then
                        interactSavePoint field
                    else
                        match tryGetFacingProp field.Avatar world with
                        | Some prop ->
                            let prop = prop.GetProp world
                            match prop.PropData with
                            | Portal (_, _, _, _, _, _, _) -> just field
                            | Door (_, cue, _, requirements) -> interactDoor cue requirements prop field
                            | Chest (_, itemType, chestId, battleTypeOpt, cue, requirements) -> interactChest itemType chestId battleTypeOpt cue requirements field
                            | Switch (_, cue, cue2, requirements) -> interactSwitch cue cue2 requirements prop field
                            | Sensor (_, _, _, _, _) -> just field
                            | Npc (_, _, cue, requirements) -> interactNpc [{ Cue = cue; Requirements = Set.empty }] requirements field
                            | NpcBranching (_, _, branches, requirements) -> interactNpc branches requirements field
                            | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType field
                            | Seal (_, cue, _) -> just (Field.updateCue (constant cue) field)
                            | Flame _ -> just field
                            | SavePoint -> just field
                            | ChestSpawn -> just field
                            | EmptyProp -> just field
                        | None -> just field
                | Some dialog ->
                    interactDialog dialog field

        override this.Command (field, command, _, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyeCenter field.Avatar.Center world
                let tileMapBounds = Simulants.Field.Scene.TileMap.GetBounds world
                let eyeBounds = tileMapBounds.WithPosition (tileMapBounds.Position + v2Dup 48.0f)
                let eyeBounds = eyeBounds.WithSize (tileMapBounds.Size - v2Dup 96.0f)
                let world = World.constrainEyeBounds eyeBounds world
                just world

            | PlayFieldSong ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match (fieldData.FieldSongOpt, World.getCurrentSongOpt world) with
                    | (Some fieldSong, Some currentSong) ->
                        if not (AssetTag.equals fieldSong currentSong.Song)
                        then withCmd (PlaySong (Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, fieldSong)) world
                        else just world
                    | (Some fieldSong, None) -> withCmd (PlaySong (Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, fieldSong)) world
                    | (None, _) -> just world
                | (false, _) -> just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | PlaySong (fade, volume, assetTag) ->
                let world = World.playSong fade volume assetTag world
                just world

            | FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

            | Nop -> just world

        override this.Content (field, _) =

            [// scene group
             Content.group Simulants.Field.Scene.Group.Name []

                [// backdrop sprite
                 Content.staticSprite Gen.name
                    [Entity.Bounds <== field --|> (fun _ world -> World.getViewBoundsAbsolute world); Entity.Elevation == Single.MinValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.Image9
                     Entity.Color <== field --> fun field ->
                        match Data.Value.Fields.TryGetValue field.FieldType with
                        | (true, fieldData) -> fieldData.FieldBackgroundColor
                        | (false, _) -> Color.Black]

                 // transition fade sprite
                 Content.staticSprite Gen.name
                    [Entity.Bounds <== field --|> (fun _ world -> World.getViewBoundsAbsolute world); Entity.Elevation == Single.MaxValue; Entity.Absolute == true
                     Entity.StaticImage == Assets.Default.Image9
                     Entity.Visible <== field --> fun field -> Option.isSome field.FieldTransitionOpt
                     Entity.Color <== field --|> fun field world ->
                        match field.FieldTransitionOpt with
                        | Some transition ->
                            let time = World.getTickTime world
                            let deltaTime = single transition.FieldTransitionTime - single time
                            let halfTransitionTime = single Constants.Field.TransitionTime * 0.5f
                            let progress =
                                if deltaTime < halfTransitionTime
                                then deltaTime / halfTransitionTime
                                else 1.0f - (deltaTime - halfTransitionTime) / halfTransitionTime
                            Color.Black.WithA (byte (progress * 255.0f))
                        | None -> Color.Zero]

                 // tmx map
                 Content.tmxMap Simulants.Field.Scene.TileMap.Name
                    [Entity.Elevation == Constants.Field.BackgroundElevation
                     Entity.TmxMap <== field --|> fun field world ->
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetTileMap field.OmniSeedState fieldData world with
                            | Some tileMap -> tileMap
                            | None -> failwithumf ()
                        | None -> failwithumf ()
                     Entity.TileLayerClearance == 10.0f]

                 // tmx map fade
                 Content.tmxMap Gen.name
                    [Entity.Elevation == Constants.Field.BackgroundElevation + 0.5f
                     Entity.Color <== field --> fun field ->
                        let progress = 1.0f - (Constants.Field.ConnectorFadeYMax - field.Avatar.Bottom.Y) / Constants.Field.ConnectorFadeYMax
                        let fade = min 1.0f progress
                        colWhite.ScaleA fade
                     Entity.TmxMap <== field --|> fun field world ->
                        match Map.tryFind field.FieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetTileMapFade field.OmniSeedState fieldData world with
                            | Some tileMapFade -> tileMapFade
                            | None -> World.getTileMapMetadata Assets.Default.TileMapEmpty world |> __c
                        | None -> World.getTileMapMetadata Assets.Default.TileMapEmpty world |> __c
                     Entity.TileLayerClearance == 10.0f]

                 // avatar
                 Content.entity<AvatarDispatcher> Simulants.Field.Scene.Avatar.Name
                    [Entity.Position == v2Dup 144.0f; Entity.Elevation == Constants.Field.ForegroundElevation; Entity.Size == Constants.Gameplay.CharacterSize
                     Entity.Enabled <== field --> fun field ->
                        field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.LinearDamping == Constants.Field.LinearDamping
                     Entity.Avatar <== field --> fun field -> field.Avatar]

                 // menu button
                 Content.button Gen.name
                    [Entity.Position == v2 -456.0f -246.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Text == "Menu"
                     Entity.Visible <== field --> fun field ->
                        field.Menu.MenuState = MenuClosed &&
                        Cue.notInterrupting field.Advents field.Cue &&
                        Option.isNone field.DialogOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt
                     Entity.ClickEvent ==> msg MenuTeamOpen]

                 // interact button
                 Content.button Gen.name
                    [Entity.Position == v2 306.0f -246.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible <== field --|> fun field world ->
                        field.Menu.MenuState = MenuClosed &&
                        (Cue.notInterrupting field.Advents field.Cue || Option.isSome field.DialogOpt) &&
                        Option.isNone field.BattleOpt &&
                        Option.isNone field.ShopOpt &&
                        Option.isNone field.FieldTransitionOpt &&
                        Option.isSome (tryGetInteraction field.DialogOpt field.Advents field.Avatar field world)
                     Entity.Text <== field --|> fun field world ->
                        match tryGetInteraction field.DialogOpt field.Advents field.Avatar field world with
                        | Some interaction -> interaction
                        | None -> ""
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> msg Interact]

                 // props
                 Content.entities field
                    (fun field _ -> (field.FieldType, field.OmniSeedState, field.Advents, field.PropStates))
                    (fun (fieldType, rotatedSeedState, advents, propStates) world ->
                        match Map.tryFind fieldType Data.Value.Fields with
                        | Some fieldData ->
                            FieldData.getPropDescriptors rotatedSeedState fieldData world |>
                            Map.ofListBy (fun propDescriptor -> (propDescriptor.PropId, (propDescriptor, advents, propStates)))
                        | None -> Map.empty)
                    (fun _ propLens _ ->
                        let prop = flip Lens.map propLens (fun (propDescriptor, advents, propStates) ->
                            let propState = Field.getPropState propDescriptor advents propStates
                            Prop.make propDescriptor.PropBounds propDescriptor.PropElevation advents propDescriptor.PropData propState propDescriptor.PropId)
                        Content.entity<PropDispatcher> Gen.name [Entity.Prop <== prop])

                 // spirit orb
                 Content.entityIf field (fun field _ -> Field.hasEncounters field) $ fun field world ->
                    Content.entity<SpiritOrbDispatcher> Gen.name
                        [Entity.Position == v2 -448.0f 48.0f; Entity.Elevation == Constants.Field.SpiritOrbElevation; Entity.Size == v2 192.0f 192.0f
                         Entity.SpiritOrb <== field --> fun field -> { AvatarLowerCenter = field.Avatar.LowerCenter; Spirits = field.Spirits; Chests = Field.getChests field world }]

                 // dialog
                 Content.entityIf field (fun field _ -> Option.isSome field.DialogOpt) $ fun field _ ->
                    Dialog.content Gen.name
                       Constants.Field.GuiElevation PromptLeft PromptRight
                       (field --> fun field -> (flip detokenize field, field.DialogOpt))

                 // team
                 Content.entityIf field (fun field _ -> match field.Menu.MenuState with MenuTeam _ -> true | _ -> false) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -256.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 896.0f 512.0f
                        Entity.LabelImage == Assets.Gui.DialogXXLImage]
                       [sidebar (v2 24.0f 420.0f) 1.0f field
                        team (v2 138.0f 420.0f) 1.0f Int32.MaxValue field tautology2 MenuTeamAlly
                        Content.label Gen.name
                           [Entity.PositionLocal == v2 438.0f 288.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v2 192.0f 192.0f
                            Entity.LabelImage <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeamData field.Team menu with
                                   | Some characterData ->
                                       match characterData.PortraitOpt with
                                       | Some portrait -> portrait
                                       | None -> Assets.Default.ImageEmpty
                                   | None -> Assets.Default.ImageEmpty
                               | _ -> Assets.Default.ImageEmpty]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 666.0f 372.0f; Entity.ElevationLocal == 1.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeamData field.Team menu with
                                   | Some characterData -> CharacterType.getName characterData.CharacterType
                                   | None -> ""
                               | _ -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 666.0f 336.0f; Entity.ElevationLocal == 1.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeammate field.Team menu with
                                   | Some teammate -> "Level " + string (Algorithms.expPointsToLevel teammate.ExpPoints)
                                   | None -> ""
                               | _ -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 444.0f 234.0f; Entity.ElevationLocal == 1.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeammate field.Team menu with
                                   | Some teammate -> "W: " + Option.mapOrDefault string "None" teammate.WeaponOpt
                                   | None -> ""
                               | _ -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 444.0f 204.0f; Entity.ElevationLocal == 1.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeammate field.Team menu with
                                   | Some teammate -> "A: " + Option.mapOrDefault string "None" teammate.ArmorOpt
                                   | None -> ""
                               | _ -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 444.0f 174.0f; Entity.ElevationLocal == 1.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeammate field.Team menu with
                                   | Some teammate -> "1: " + Option.mapOrDefault string "None" (List.tryHead teammate.Accessories)
                                   | None -> ""
                               | _ -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 444.0f -84.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v2 512.0f 256.0f
                            Entity.Justification == Unjustified true
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuState with
                               | MenuTeam menu ->
                                   match MenuTeam.tryGetTeammateAndTeamData field.Team menu with
                                   | Some (teammate, characterData) ->
                                       let level = Algorithms.expPointsToLevel teammate.ExpPoints
                                       let hpm = Algorithms.hitPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                       let tpm = Algorithms.techPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                                       let pow = Algorithms.power teammate.WeaponOpt Map.empty characterData.ArchetypeType level // no statuses outside battle
                                       let mag = Algorithms.magic teammate.WeaponOpt Map.empty characterData.ArchetypeType level // no statuses outside battle
                                       let def = Algorithms.shield Physical teammate.Accessories Map.empty characterData.ArchetypeType level // no statuses outside battle
                                       let abs = Algorithms.shield Magical teammate.Accessories Map.empty characterData.ArchetypeType level // no statuses outside battle
                                       "HP  "   + (string teammate.HitPoints).PadLeft 3 + " /" + (string hpm).PadLeft 3 +
                                       "\nTP  " + (string teammate.TechPoints).PadLeft 3 + " /" + (string tpm).PadLeft 3 +
                                       "\nPow " + (string pow).PadLeft 3 + "    Mag " + (string mag).PadLeft 3 +
                                       "\nDef " + (string def).PadLeft 3 + "    Abs " + (string abs).PadLeft 3 +
                                       "\nExp " + (string teammate.ExpPoints).PadLeft 3 + " /" + (string (Algorithms.expPointsForNextLevel teammate.ExpPoints)).PadLeft 3
                                   | None -> ""
                               | _ -> ""]]

                 // item
                 Content.entityIf field (fun field _ -> match field.Menu.MenuState with MenuItem _ -> true | _ -> false) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -256.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 896.0f 512.0f
                        Entity.LabelImage == Assets.Gui.DialogXXLImage
                        Entity.Enabled <== field --> fun field -> Option.isNone field.Menu.MenuUseOpt]
                       [sidebar (v2 24.0f 420.0f) 1.0f field
                        items (v2 136.0f 420.0f) 1.0f 5 field MenuItemSelect
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 368.0f 3.0f; Entity.ElevationLocal == 1.0f
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // tech team
                 Content.entityIf field (fun field _ -> Menu.isTechTeam field.Menu) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -256.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 896.0f 512.0f
                        Entity.LabelImage == Assets.Gui.DialogXXLImage]
                       [sidebar (v2 24.0f 420.0f) 1.0f field
                        team (v2 138.0f 420.0f) 1.0f Int32.MaxValue field tautology2 MenuTechAlly]
                 
                 // tech teammate
                 Content.entityIf field (fun field _ -> Menu.isTechTeammate field.Menu) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -256.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 896.0f 512.0f
                        Entity.LabelImage == Assets.Gui.DialogXXLImage]
                       [sidebar (v2 24.0f 420.0f) 1.0f field
                        Content.entities field
                           (fun (field : Field) _ -> (field.Menu, field.Team))
                           (fun (menu, team) _ ->
                               match menu.MenuState with
                               | MenuTech menuTech ->
                                   match MenuTech.tryGetTeammate team menuTech with
                                   | Some teammate -> teammate.Techs |> Seq.index |> Map.ofSeq
                                   | None -> Map.empty
                               | _ -> Map.empty)
                           (fun i selectionLens _ ->
                               let position = (v2 136.0f 420.0f)
                               let x = if i < 5 then position.X else position.X + 368.0f
                               let y = position.Y - single (i % 5) * 80.0f
                               Content.button Gen.name
                                   [Entity.PositionLocal == v2 x y; Entity.ElevationLocal == 1.0f; Entity.Size == v2 336.0f 72.0f
                                    Entity.Justification == Justified (JustifyLeft, JustifyMiddle); Entity.Margins == v2 16.0f 0.0f
                                    Entity.Text <== selectionLens --> fun techType -> string techType
                                    Entity.EnabledLocal <== selectionLens --> fun _ -> true
                                    Entity.ClickEvent ==> msg MenuTechSelect])
                        Content.button Gen.name
                            [Entity.PositionLocal == v2 800.0f 16.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v2 84.0f 54.0f
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle); Entity.Margins == v2 16.0f 0.0f
                             Entity.Text == "Back"
                             Entity.ClickEvent ==> msg MenuTechOpen]]

                 // use
                 Content.entityIf field (fun field _ -> Option.isSome field.Menu.MenuUseOpt) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -192.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v2 896.0f 384.0f
                        Entity.LabelImage == Assets.Gui.DialogXLImage]
                       [team (v2 160.0f 150.0f) 1.0f 3 field
                           (fun teammate menu ->
                               match menu.MenuUseOpt with
                               | Some menuUse -> Teammate.canUseItem (snd menuUse.MenuUseSelection) teammate
                               | None -> false)
                           MenuItemUse
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 810.0f 306.0f; Entity.ElevationLocal == 2.0f; Entity.Size == v2 64.0f 64.0f
                            Entity.Text == "X"
                            Entity.ClickEvent ==> msg MenuItemCancel]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 42.0f 306.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== field --> fun field ->
                               match field.Menu.MenuUseOpt with
                               | Some menu -> menu.MenuUseLine1
                               | None -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 60.0f 264.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== field --> fun field ->
                                match field.Menu.MenuUseOpt with
                                | Some menu -> menu.MenuUseLine2
                                | None -> ""]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 60.0f 222.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== field --> fun field ->
                               match field.ShopOpt with
                               | Some shop ->
                                   match shop.ShopConfirmOpt with
                                   | Some shopConfirm -> shopConfirm.ShopConfirmLine2
                                   | None -> ""
                               | None -> ""]]

                 // shop
                 Content.entityIf field (fun field _ -> Option.isSome field.ShopOpt) $ fun field _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -256.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 896.0f 512.0f
                        Entity.LabelImage == Assets.Gui.DialogXXLImage
                        Entity.Enabled <== field --> fun field -> match field.ShopOpt with Some shop -> Option.isNone shop.ShopConfirmOpt | None -> true]
                       [items (v2 96.0f 350.0f) 1.0f 4 field ShopSelect
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 24.0f 444.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text == "Buy"
                            Entity.VisibleLocal <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false
                            Entity.ClickEvent ==> msg ShopBuy]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 24.0f 444.0f; Entity.ElevationLocal == 1.0f
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.Text == "Buy what?"
                            Entity.VisibleLocal <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false]
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 352.0f 444.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text == "Sell"
                            Entity.VisibleLocal <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopBuying | None -> false
                            Entity.ClickEvent ==> msg ShopSell]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 352.0f 444.0f; Entity.ElevationLocal == 1.0f
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.Text == "Sell what?"
                            Entity.VisibleLocal <== field --> fun field -> match field.ShopOpt with Some shop -> shop.ShopState = ShopSelling | None -> false]
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 678.0f 444.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text == "Leave"
                            Entity.ClickEvent ==> msg ShopLeave]
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 24.0f 18.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                            Entity.Text == "<"
                            Entity.ClickEvent ==> msg ShopPageUp]
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 822.0f 18.0f; Entity.ElevationLocal == 1.0f; Entity.Size == v2 48.0f 64.0f
                            Entity.Text == ">"
                            Entity.ClickEvent ==> msg ShopPageDown]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 352.0f 3.0f; Entity.ElevationLocal == 1.0f
                            Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                            Entity.Text <== field --> (fun field -> string field.Inventory.Gold + "G")]]

                 // shop confirm
                 Content.entityOpt field (fun field _ -> match field.ShopOpt with Some shop -> shop.ShopConfirmOpt | None -> None) $ fun shopConfirm _ ->
                    Content.panel Gen.name
                       [Entity.Position == v2 -448.0f -128.0f; Entity.Elevation == Constants.Field.GuiElevation + 10.0f; Entity.Size == v2 864.0f 252.0f
                        Entity.LabelImage == Assets.Gui.DialogThickImage]
                       [Content.button Gen.name
                           [Entity.PositionLocal == v2 198.0f 42.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text == "Accept"
                            Entity.ClickEvent ==> msg ShopConfirmAccept]
                        Content.button Gen.name
                           [Entity.PositionLocal == v2 498.0f 42.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text == "Decline"
                            Entity.ClickEvent ==> msg ShopConfirmDecline]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 42.0f 180.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== shopConfirm --> fun shopConfirm -> shopConfirm.ShopConfirmOffer]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 60.0f 138.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== shopConfirm --> fun shopConfirm -> shopConfirm.ShopConfirmLine1]
                        Content.text Gen.name
                           [Entity.PositionLocal == v2 60.0f 96.0f; Entity.ElevationLocal == 2.0f
                            Entity.Text <== shopConfirm --> fun shopConfirm -> shopConfirm.ShopConfirmLine2]]]]