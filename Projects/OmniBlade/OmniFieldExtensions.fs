// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module FieldExtensions =

    [<RequireQualifiedAccess>]
    module Field =

        let private tryGetFacingInteraction avatarPositionY (prop : Prop) (field : Field) =
            match prop.PropData with
            | Sprite _ -> None
            | Portal (_, _, _, _, _, _, _) -> None
            | Door _ -> Some "Open"
            | Chest (_, _, chestId, _, _, _) -> if Set.contains (Opened chestId) field.Advents then None else Some "Open"
            | Switch (_, _, _, _) -> Some "Use"
            | Sensor (_, _, _, _, _) -> None
            | Character (_, _, _, isRising, _, _) ->
                if isRising then
                    if prop.Bottom.Y - avatarPositionY > 40.0f // NOTE: just a bit of hard-coding to ensure player is interacting with the character from the south.
                    then Some "Talk"
                    else None
                else Some "Talk"
            | Npc _ | NpcBranching _ -> Some "Talk"
            | Shopkeep _ -> Some "Shop"
            | Seal _ -> Some "Touch"
            | Flame _ -> None
            | SavePoint -> None
            | ChestSpawn -> None
            | EmptyProp -> None

        let detokenize (field : Field) (text : string) =
            text
                .Replace("$FEE", scstring (Field.getRecruitmentFee field))
                .Replace("$GOLD", scstring field.Inventory.Gold)

        let isFacingProp propId (avatar : Avatar) (field : Field) =
            match field.Props.TryGetValue propId with
            | (true, prop) ->
                let v = prop.Bottom - avatar.Bottom
                let direction = Direction.ofVector3 v
                direction <> field.Avatar.Direction.Opposite
            | (false, _) -> false

        let getFacingProps (avatar : Avatar) (field : Field) =
            List.filter
                (fun propId -> isFacingProp propId avatar field)
                field.Avatar.IntersectedPropIds

        let tryGetFacingProp (avatar : Avatar) (field : Field) =
            match getFacingProps avatar field with
            | head :: _ -> Some (field.Props.[head])
            | [] -> None

        let isTouchingSavePoint (avatar : Avatar) (field : Field) =
            List.exists (fun propId ->
                match field.Props.TryGetValue propId with
                | (true, prop) -> prop.PropData = SavePoint
                | (false, _) -> false)
                avatar.IntersectedPropIds

        let tryGetInteraction (avatar : Avatar) (field : Field) =
            match field.DialogOpt with
            | Some dialog ->
                if  Dialog.canAdvance (detokenize field) dialog &&
                    not
                        (Dialog.isExhausted (detokenize field) dialog &&
                         Option.isSome dialog.DialogPromptOpt)
                then Some "Next"
                else None
            | None ->
                if isTouchingSavePoint avatar field then
                    Some "Save"
                else
                    match tryGetFacingProp avatar field with
                    | Some prop -> tryGetFacingInteraction field.Avatar.Position.Y prop field
                    | None -> None

        let tryGetTouchingPortal (avatar : Avatar) (field : Field) =
            avatar.IntersectedPropIds |>
            List.choose (fun propId ->
                match field.Props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Portal (portalType, _, _, fieldType, portalIndex, _, requirements) ->
                        if field.Advents.IsSupersetOf requirements then
                            match Map.tryFind fieldType Data.Value.Fields with
                            | Some fieldData ->
                                match FieldData.tryGetPortal field.OmniSeedState portalIndex fieldData with
                                | Some portal ->
                                    match portal.PropData with
                                    | Portal (_, _, direction, _, _, extended, _) ->
                                        let destination =
                                            match direction with
                                            | Upward -> portal.PropPerimeter.Top + v3 0.0f 8.0f 0.0f + if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                            | Rightward -> portal.PropPerimeter.Right + v3 32.0f 0.0f 0.0f + if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                            | Downward -> portal.PropPerimeter.Bottom + v3 0.0f -54.0f 0.0f - if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                            | Leftward -> portal.PropPerimeter.Left + v3 -32.0f 0.0f 0.0f - if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                        let isWarp = match portalType with AirPortal | StairsPortal _ -> false | WarpPortal -> true
                                        Some (fieldType, destination, direction, isWarp)
                                    | _ -> None
                                | None -> None
                            | None -> None
                        else None
                    | _ -> None
                | _ -> None) |>
            List.tryHead

        let getTouchedSensors (avatar : Avatar) (field : Field) =
            List.choose (fun propId ->
                match field.Props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                    | _ -> None
                | (false, _) -> None)
                avatar.CollidedPropIds

        let getUntouchedSensors (avatar : Avatar) (field : Field) =
            List.choose (fun propId ->
                match field.Props.TryGetValue propId with
                | (true, prop) ->
                    match prop.PropData with
                    | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                    | _ -> None
                | (false, _) -> None)
                avatar.SeparatedPropIds

        let interactDialog dialog field =
            match Dialog.tryAdvance (detokenize field) dialog with
            | (true, dialog) ->
                let field = Field.updateDialogOpt (constant (Some dialog)) field
                just field
            | (false, dialog) ->
                let field = Field.updateDialogOpt (constant None) field
                match dialog.DialogBattleOpt with
                | Some (battleType, consequence) -> withMsg (TryBattle (battleType, consequence)) field
                | None -> just field

        let interactChest itemType chestId battleTypeOpt cue requirements (prop : Prop) (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                let field = Field.updateAdvents (Set.add (Opened chestId)) field
                let field = Field.updateInventory (Inventory.tryAddItem itemType >> snd) field
                let field =
                    match battleTypeOpt with
                    | Some battleType -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!^But something approaches!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = Some (battleType, Set.empty) })) field
                    | None -> Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Found " + ItemType.getName itemType + "!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                let field = Field.updateCue (constant cue) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestOpenSound)) field
            else
                let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestLockedSound)) field

        let interactDoor keyItemTypeOpt cue requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | DoorState false ->
                if  field.Advents.IsSupersetOf requirements &&
                    Option.mapOrDefaultValue (fun keyItemType -> Map.containsKey (KeyItem keyItemType) field.Inventory.Items) true keyItemTypeOpt then
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateCue (constant cue) field
                    let field = Field.updatePropState (constant (DoorState true)) prop.PropId field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorOpenSound)) field
                else
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Locked!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorLockedSound)) field
            | _ -> failwithumf ()

        let interactSwitch cue cue2 requirements (prop : Prop) (field : Field) =
            match prop.PropState with
            | SwitchState on ->
                if field.Advents.IsSupersetOf requirements then
                    let on = not on
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updatePropState (constant (SwitchState on)) prop.PropId field
                    let field = Field.updateCue (constant (if on then cue else cue2)) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchUseSound)) field
                else
                    let field = Field.updateAvatar (Avatar.lookAt prop.Center) field
                    let field = Field.updateDialogOpt (constant (Some { DialogForm = DialogThin; DialogTokenized = "Won't budge!"; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None })) field
                    withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchStuckSound)) field
            | _ -> failwithumf ()

        let interactCharacter cue (prop : Prop) (field : Field) =
            let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
            let field = Field.updateCue (constant cue) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field
        
        let interactNpc branches requirements (prop : Prop) (field : Field) =
            if field.Advents.IsSupersetOf requirements then
                let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
                let branchesFiltered = branches |> List.choose (fun branch -> if field.Advents.IsSupersetOf branch.Requirements then Some branch.Cue else None) |> List.rev
                let branchCue = match List.tryHead branchesFiltered with Some cue -> cue | None -> Dialog ("...", false)
                let field = Field.updateCue (constant branchCue) field
                withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field
            else just field

        let interactShopkeep shopType (prop : Prop) (field : Field) =
            let field = Field.updateAvatar (Avatar.lookAt prop.BottomInset) field
            let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
            let field = Field.updateShopOpt (constant (Some shop)) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field

        let interactSeal cue (field : Field) =
            let field = Field.updateCue (constant cue) field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SealedSound)) field

        let interactSavePoint (field : Field) =
            let field = Field.restoreTeam field
            Field.save field
            withCmd (FieldCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.SlotSound)) field