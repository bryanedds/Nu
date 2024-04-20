// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open Prime
open Nu
open CueSystem

type FieldMessage =
    | Update
    | UpdateFieldTransition
    | TimeUpdate
    | UpdateAvatarBodyTracking
    | AvatarBodyTransform of BodyTransformData
    | AvatarBodyCollision of BodyCollisionData
    | AvatarBodySeparationExplicit of BodySeparationExplicitData
    | AvatarBodySeparationImplicit of BodySeparationImplicitData
    | ScreenTransitioning of bool
    | TryCommencingBattle of BattleType * Advent Set
    | MenuTeamOpen
    | MenuTeamAlly of int
    | MenuInventoryOpen
    | MenuInventoryPageUp
    | MenuInventoryPageDown
    | MenuInventorySelect of int * (ItemType * int Option)
    | MenuInventoryUse of int
    | MenuInventoryCancel
    | MenuTechsOpen
    | MenuTechsAlly of int
    | MenuTechsSelect of int
    | MenuTechClose
    | MenuKeyItemsOpen
    | MenuKeyItemsPageUp
    | MenuKeyItemsPageDown
    | MenuKeyItemsSelect of int * (ItemType * int Option)
    | MenuOptionsOpen
    | MenuOptionsSelectBattleSpeed of BattleSpeed
    | MenuOptionsQuitPrompt
    | MenuOptionsQuitConfirm
    | MenuOptionsQuitCancel
    | MenuClose
    | PartyMenuOpen
    | PartyMenuSelect of int
    | PartyMenuDeselect of int
    | PartyMenuClose
    | ShopBuy
    | ShopSell
    | ShopPageUp
    | ShopPageDown
    | ShopSelect of int * (ItemType * int Option)
    | ShopConfirmAccept
    | ShopConfirmDecline
    | ShopLeave
    | PromptLeft
    | PromptRight
    | Interact
    interface Message

type FieldCommand =
    | ProcessKeyInput
    | ProcessTouchInput of Vector2
    | UpdateEye
    | WarpAvatar of Vector3
    | MoveAvatar of Vector3
    | FaceAvatar of Direction
    | CommencingBattle
    | CommenceBattle of BattleData * PrizePool
    | PlayFieldSong
    | PlaySound of int64 * single * Sound AssetTag
    | PlaySong of int64 * int64 * int64 * single * Song AssetTag
    | FadeOutSong of int64
    | Nop
    interface Command

type SaveSlot =
    | Slot1
    | Slot2
    | Slot3

type [<SymbolicExpansion>] Options =
    { BattleSpeed : BattleSpeed }

type FieldState =
    | Playing
    | Battling of BattleData * PrizePool
    | Quitting
    | Quit

type FieldTransition =
    { FieldType : FieldType
      FieldDestination : Vector3
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; SymbolicExpansion>] Field =
        private
            { FieldTime_ : int64
              ViewBoundsAbsolute_ : Box2
              FieldType_ : FieldType
              SaveSlot_ : SaveSlot
              OmniSeedState_ : OmniSeedState
              Avatar_ : Avatar
              AvatarCollidedPropIds_ : int list
              AvatarSeparatedPropIds_ : int list
              AvatarIntersectedPropIds_ : int list
              Team_ : Map<int, Teammate>
              SpiritRate_ : single
              SpiritActivity_ : single
              Spirits_ : Spirit array
              Advents_ : Advent Set
              Props_ : Map<int, Prop>
              Inventory_ : Inventory
              Options_ : Options
              Menu_ : Menu
              PartyMenu_ : PartyMenu
              Definitions_ : CueSystem.CueDefinitions
              DefinitionsOriginal_ : CueSystem.CueDefinitions
              Cue_ : CueSystem.Cue
              ScreenTransitioning_ : bool
              FieldTransitionOpt_ : FieldTransition option
              Tint_ : Color
              ShopOpt_ : Shop option
              DialogOpt_ : Dialog option
              FieldSongTimeOpt_ : int64 option
              FieldState_ : FieldState }

        (* Local Properties *)
        member this.FieldTime = this.FieldTime_
        member this.ViewBoundsAbsolute = this.ViewBoundsAbsolute_
        member this.FieldType = this.FieldType_
        member this.OmniSeedState = this.OmniSeedState_
        member this.Avatar = this.Avatar_
        member this.AvatarCollidedPropIds = this.AvatarCollidedPropIds_
        member this.AvatarSeparatedPropIds = this.AvatarSeparatedPropIds_
        member this.AvatarIntersectedPropIds = this.AvatarIntersectedPropIds_
        member this.Team = this.Team_
        member this.SpiritActivity = this.SpiritActivity_
        member this.Spirits = this.Spirits_
        member this.Advents = this.Advents_
        member this.Props = this.Props_
        member this.Inventory = this.Inventory_
        member this.Options = this.Options_
        member this.Menu = this.Menu_
        member this.PartyMenu = this.PartyMenu_
        member this.Definitions = this.Definitions_
        member this.DefinitionsOriginal = this.DefinitionsOriginal_
        member this.Cue = this.Cue_
        member this.ScreenTransitioning = this.ScreenTransitioning_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.Tint = this.Tint_
        member this.ShopOpt = this.ShopOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.FieldSongTimeOpt = this.FieldSongTimeOpt_
        member this.FieldState = this.FieldState_

    (* Low-Level Operations *)

    let private makePropState time propDescriptor =
        match propDescriptor.PropData with
        | Sprite (_, image, color, blend, emission, flip, visible) -> SpriteState (image, color, blend, emission, flip, visible)
        | Door _ -> DoorState false
        | Switch (_, _, _, _, _) -> NilState
        | Character (characterType, direction, _, _, _, _) ->
            let animationSheet =
                match Data.Value.Characters.TryGetValue characterType with
                | (true, characterData) -> characterData.AnimationSheet
                | (false, _) -> Assets.Field.JinnAnimationSheet
            let characterAnimationState =
                { StartTime = time
                  AnimationSheet = animationSheet
                  CharacterAnimationType = IdleAnimation
                  MaterializationOpt = None
                  Direction = direction }
            CharacterState (Color.One, characterAnimationState)
        | Portal _ | Chest _ | Sensor _ | Npc _ | NpcBranching _ | Shopkeep _ | Seal _ | Flame _ | SavePoint | ChestSpawn | PortalSpawn | EmptyProp -> NilState

    let private makeProps time fieldType omniSeedState =
        match Map.tryFind fieldType Data.Value.Fields with
        | Some fieldData ->
            FieldData.getPropDescriptors omniSeedState fieldData |>
            Map.ofListBy (fun propDescriptor ->
                let propState = makePropState time propDescriptor
                let prop = Prop.make propDescriptor.PropPerimeter propDescriptor.PropElevation propDescriptor.PropData propState propDescriptor.PropId
                (propDescriptor.PropId, prop))
        | None -> Map.empty

    let private makeBattleFromTeam battleSpeed inventory (team : Map<int, Teammate>) prizePool battleData =
        let party = team |> Map.toList |> List.tryTake 3
        let allyPositions =
            if List.length party < 3
            then List.take 2 battleData.BattleAllyPositions
            else List.take 1 battleData.BattleAllyPositions @ List.skip 2 battleData.BattleAllyPositions
        let party =
            List.mapi
                (fun index (teamIndex, teammate) ->
                    match Map.tryFind teammate.CharacterType Data.Value.Characters with
                    | Some characterData ->
                        // TODO: bounds checking
                        let size = Constants.Gameplay.CharacterSize
                        let celSize = Constants.Gameplay.CharacterCelSize
                        let position = if party.Length = 1 then allyPositions.[teamIndex] + Constants.Battle.CharacterOffset else allyPositions.[teamIndex]
                        let bounds = box3 position size
                        let characterIndex = AllyIndex teamIndex
                        let characterType = characterData.CharacterType
                        let boss = characterData.Boss
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector3 -bounds.Bottom
                        let actionTime = 1000.0f - Constants.Battle.AllyActionTimeSpacing * single index
                        let characterState = CharacterState.make characterData teammate.HitPoints teammate.TechPoints teammate.ExpPoints teammate.WeaponOpt teammate.ArmorOpt teammate.Accessories
                        let character = Character.make bounds characterIndex characterType boss animationSheet celSize direction characterState None actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring teammate.CharacterType + "'."))
                party
        let battle = Battle.makeFromParty battleSpeed inventory party prizePool battleData
        battle
        
    let rec detokenize (field : Field) (text : string) =
        text
            .Replace("$FEE", scstring (getRecruitmentFee field))
            .Replace("$GOLD", scstring field.Inventory.Gold)

    and getRecruitmentFee (field : Field) =
        let advents = Set.ofArray [|ShadeRecruited; MaelRecruited; RiainRecruited; PericRecruited|]
        let recruiteds = Set.intersect advents field.Advents
        let recruited = Set.count recruiteds
        match Array.tryItem recruited Constants.Field.RecruitmentFees with
        | Some recruitmentFee -> recruitmentFee
        | None -> 0

    let getParty field =
        field.Team_ |>
        Map.filter (fun _ teammate -> Option.isSome teammate.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let getFieldSongOpt field =
        match Data.Value.Fields.TryGetValue field.FieldType_ with
        | (true, fieldData) -> fieldData.FieldSongOpt
        | (false, _) -> None

    let getProp propId field =
        field.Props_.[propId]

    let getChests field =
        field.Props_ |>
        Map.toValueArray |>
        Array.choose (fun prop ->
            match prop.PropData with
            | Chest (_, _, id, _, _, _) -> Some (Chest.make prop.Perimeter (field.Advents.Contains (Opened id)))
            | _ -> None)

    let getNonWarpPortals field =
        field.Props_ |>
        Map.toValueArray |>
        Array.choose (fun prop ->
            match prop.PropData with
            | Portal (portalType, _, _, _, _, _, requirements) when portalType <> WarpPortal -> Some (Portal.make prop.Perimeter (field.Advents.IsSupersetOf requirements))
            | _ -> None)

    let getNarratives field =
        field.Props_ |>
        Map.toValueArray |>
        Array.choose (fun prop ->
            let activeOpt =
                match prop.PropData with
                | Character (_, _, _, _, _, requirements) -> Some (field.Advents.IsSupersetOf requirements)
                | Npc (npcType, _, _, requirements) -> Some (field.Advents.IsSupersetOf requirements && NpcType.exists field.Advents npcType)
                | NpcBranching (npcType, _, _, requirements) -> Some (field.Advents.IsSupersetOf requirements && NpcType.exists field.Advents npcType)
                | _ -> None
            match activeOpt with
            | Some active -> Some (Narrative.make prop.Perimeter active)
            | None -> None)

    let getShowUnopenedChests (field : Field) =
        match Map.tryFind field.FieldType Data.Value.Fields with
        | Some fieldData -> fieldData.ShowUnopenedChests
        | None -> true

    let tryGetFacingInteraction (prop : Prop) (field : Field) =
        match prop.PropData with
        | Sprite _ -> None
        | Portal (_, _, _, _, _, _, _) -> None
        | Door _ -> Some "Open"
        | Chest (_, _, chestId, _, _, _) -> if Set.contains (Opened chestId) field.Advents then None else Some "Open"
        | Switch (_, _, _, _, _) -> Some "Use"
        | Sensor (_, _, _, _, _) -> None
        | Character (_, _, _, isRising, _, _) ->
            if isRising then
                if prop.Bottom.Y - field.Avatar_.Bottom.Y > 40.0f // NOTE: just a bit of hard-coding to ensure player is interacting with the character from the south.
                then Some "Talk"
                else None
            else Some "Talk"
        | Npc _ | NpcBranching _ -> Some "Talk"
        | Shopkeep _ -> Some "Shop"
        | Seal _ -> Some "Touch"
        | Flame _ | SavePoint | ChestSpawn | PortalSpawn | EmptyProp -> None

    let facingProp propId (field : Field) =
        match field.Props_.TryGetValue propId with
        | (true, prop) ->
            let v = prop.Bottom - field.Avatar_.Bottom
            let direction = Direction.ofVector3 v
            direction <> field.Avatar_.Direction.Opposite
        | (false, _) -> false

    let getFacingProps (field : Field) =
        List.filter
            (fun propId -> facingProp propId field)
            field.AvatarIntersectedPropIds

    let tryGetFacingProp (field : Field) =
        match getFacingProps field with
        | head :: _ -> Some (field.Props_.[head])
        | [] -> None

    // NOTE: I really don't like the need to do these inefficient reverse map look-ups as a matter of course. Perhaps
    // there's an elegant alternative that is more performant.
    let tryGetPropIdByData predicate field =
        Map.tryFindKey (fun _ (prop : Prop) ->
            predicate prop.PropData)
            field.Props_

    let touchingSavePoint (field : Field) =
        List.exists (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) -> prop.PropData = SavePoint
            | (false, _) -> false)
            field.AvatarIntersectedPropIds

    let tryGetInteraction (field : Field) =
        match field.DialogOpt with
        | Some dialog ->
            if  Dialog.canAdvance (detokenize field) dialog &&
                not
                    (Dialog.isExhausted (detokenize field) dialog &&
                     Option.isSome dialog.DialogPromptOpt)
            then Some "Next"
            else None
        | None ->
            if touchingSavePoint field then
                Some "Save"
            else
                match tryGetFacingProp field with
                | Some prop -> tryGetFacingInteraction prop field
                | None -> None

    let tryGetTouchingPortal (field : Field) =
        field.AvatarIntersectedPropIds |>
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
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
                                    let isWarp =
                                        match portalType with
                                        | AirPortal | StairsPortal (_, false) -> false
                                        | WarpPortal | StairsPortal (_, true) -> true
                                    Some (fieldType, destination, direction, isWarp)
                                | _ -> None
                            | None -> None
                        | None -> None
                    else None
                | _ -> None
            | _ -> None) |>
        List.tryHead

    let getTouchedSensors (field : Field) =
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) ->
                match prop.PropData with
                | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None
            | (false, _) -> None)
            field.AvatarCollidedPropIds

    let getUntouchedSensors (field : Field) =
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) ->
                match prop.PropData with
                | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None
            | (false, _) -> None)
            field.AvatarSeparatedPropIds

    let hasEncounters (field : Field) =
        match Data.Value.Fields.TryGetValue field.FieldType with
        | (true, fieldData) -> Option.isSome fieldData.EncounterTypeOpt
        | (false, _) -> false

    let mapFieldType time updater field =
        let fieldType = updater field.FieldType_
        match Map.tryFind fieldType Data.Value.Fields with
        | Some fieldData ->
            { field with
                FieldType_ = fieldType
                SpiritRate_ = fieldData.EncounterRate
                SpiritActivity_ = 0.0f
                Spirits_ = [||]
                Props_ = makeProps time fieldType field.OmniSeedState_
                FieldSongTimeOpt_ = None }
        | None -> field

    let mapFieldState updater field =
        { field with FieldState_ = updater field.FieldState_ }

    let mapAvatar updater field =
        let avatar = field.Avatar_
        let avatar = updater avatar
        if avatar =/= field.Avatar_ then { field with Avatar_ = avatar }
        else field

    let mapAvatarCollidedPropIds updater field =
        let propIds = updater field.AvatarCollidedPropIds_
        if propIds =/= field.AvatarCollidedPropIds_
        then { field with AvatarCollidedPropIds_ = propIds }
        else field

    let mapAvatarSeparatedPropIds updater field =
        let propIds = updater field.AvatarSeparatedPropIds_
        if propIds =/= field.AvatarSeparatedPropIds_
        then { field with AvatarSeparatedPropIds_ = propIds }
        else field

    let mapAvatarIntersectedPropIds updater field =
        let propIds = updater field.AvatarIntersectedPropIds_
        if propIds =/= field.AvatarIntersectedPropIds_
        then { field with AvatarIntersectedPropIds_ = propIds }
        else field

    let mapTeam updater field =
        { field with Team_ = updater field.Team_ }

    let mapAdvents updater field =
        let advents = updater field.Advents_
        if advents =/= field.Advents_ then { field with Advents_ = advents }
        else field

    let private mapProps updater field =
        { field with Props_ = updater field.Props_ }

    let mapProp updater propId field =
        match Map.tryFind propId field.Props_ with
        | Some prop -> mapProps (Map.add propId (updater prop)) field
        | None -> field
    
    let mapPropState updater propId field =
        mapProp (Prop.mapPropState updater) propId field

    let mapInventory updater field =
        { field with Inventory_ = updater field.Inventory_ }

    let mapOptions updater field =
        { field with Options_ = updater field.Options_ }

    let mapMenu updater field =
        { field with Menu_ = updater field.Menu_ }

    let mapPartyMenu updater field =
        { field with PartyMenu_ = updater field.PartyMenu_ }

    let mapDefinitions updater field =
        { field with Definitions_ = updater field.Definitions_ }

    let mapCue updater field =
        { field with Cue_ = updater field.Cue_ }

    let mapScreenTransitioning updater field =
        { field with ScreenTransitioning_ = updater field.ScreenTransitioning_ }

    let mapFieldTransitionOpt updater field =
        { field with FieldTransitionOpt_ = updater field.FieldTransitionOpt_ }

    let mapTint updater field =
        { field with Tint_ = updater field.Tint_ }

    let mapShopOpt updater field =
        { field with ShopOpt_ = updater field.ShopOpt_ }

    let mapDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let mapFieldSongTimeOpt updater field =
        { field with FieldSongTimeOpt_ = updater field.FieldSongTimeOpt_ }

    (* Mid-Level Operations *)

    let clearSpirits field =
        { field with SpiritActivity_ = 0.0f; Spirits_ = [||] }

    let recruit allyType (field : Field) =
        let lowestLevelTeammate = field.Team |> Map.toValueList |> Seq.sortBy (fun teammate -> teammate.Level) |> Seq.head
        let level = max 1 (dec lowestLevelTeammate.Level)
        let index = Map.count field.Team
        let teammate = Teammate.make level index allyType
        mapTeam (Map.add index teammate) field

    let arrangeTeam (order : int list) field =
        let (_, team) =
            Map.fold (fun (lastIndex, team) teamIndex (teammate : Teammate) ->
                match List.tryFindIndex ((=) teamIndex) order with
                | Some orderIndex -> (lastIndex, Map.add orderIndex { teammate with TeamIndex = orderIndex } team)
                | None -> (inc lastIndex, Map.add lastIndex { teammate with TeamIndex = lastIndex } team))
                (order.Length, Map.empty) field.Team_
        { field with Team_ = team }

    let restoreTeam field =
        { field with Team_ = Map.map (fun _ -> Teammate.restore) field.Team_ }

    let synchronizeTeamFromAllies allies field =
        Map.foldi (fun i field _ (ally : Character) ->
            mapTeam (fun team ->
                match Map.tryFind i team with
                | Some teammate ->
                    let teammate =
                        { teammate with
                            HitPoints = ally.HitPoints
                            TechPoints = ally.TechPoints
                            ExpPoints = ally.ExpPoints }
                    Map.add i teammate team
                | None -> team)
                field)
            field
            allies

    let synchronizeFromBattle consequents battle field =
        let allies = Battle.getAllies battle
        let field = synchronizeTeamFromAllies allies field
        let field = mapInventory (constant battle.Inventory) field
        let field = mapAdvents (Set.union consequents) field
        let field = mapAvatarIntersectedPropIds (constant []) field
        field

    let commencingBattle battleData prizePool field =
        let field = { field with FieldState_ = Battling (battleData, prizePool) }
        field

    let commenceBattle songTime battleData prizePool (field : Field) =
        let battle = makeBattleFromTeam field.Options.BattleSpeed field.Inventory field.Team prizePool battleData
        let field = mapFieldSongTimeOpt (constant (Some songTime)) field
        (battle, field)

    let concludeBattle consequents battle field =
        let field = synchronizeFromBattle consequents battle field
        let field = clearSpirits field
        let field = { field with FieldState_ = Playing }
        field

    let private toSavable field =
        { field with
            FieldTime_ = 0L
            Avatar_ = field.Avatar_
            AvatarCollidedPropIds_ = []
            AvatarSeparatedPropIds_ = []
            AvatarIntersectedPropIds_ = []
            Props_ = Map.empty
            FieldSongTimeOpt_ = None }

    let save field =
        let saveFilePath =
            match field.SaveSlot_ with
            | Slot1 -> Assets.Global.SaveFilePath1
            | Slot2 -> Assets.Global.SaveFilePath2
            | Slot3 -> Assets.Global.SaveFilePath3
        let fieldSavable = toSavable field
        let fieldSymbol = valueToSymbol fieldSavable
        let fileStr = PrettyPrinter.prettyPrintSymbol fieldSymbol PrettyPrinter.defaultPrinter
        try File.WriteAllText (saveFilePath, fileStr) with _ -> ()

    let truncate field =
        { field with Spirits_ = [||] }

    let untruncate current incoming =
        { incoming with Spirits_ = current.Spirits_ }

    (* High-Level Operations (signal-producing) *)

    let quitPrompt field =
        match field.Menu_.MenuState with
        | MenuOptions false -> { field with Menu_ = { field.Menu_ with MenuState = MenuOptions true }}
        | _ -> field

    let quitConfirm field =
        { field with FieldState_ = Quitting }

    let quitCancel field =
        match field.Menu_.MenuState with
        | MenuOptions true -> { field with Menu_ = { field.Menu_ with MenuState = MenuOptions false }}
        | _ -> field

    let private interactDialog dialog field =
        match Dialog.tryAdvance (detokenize field) dialog with
        | (true, dialog) ->
            let field = mapDialogOpt (constant (Some dialog)) field
            just field
        | (false, dialog) ->
            let field = mapDialogOpt (constant None) field
            match dialog.DialogBattleOpt with
            | Some (battleType, consequence) -> withSignal (TryCommencingBattle (battleType, consequence)) field
            | None -> just field

    let private interactChest itemType chestId battleTypeOpt cue requirements (prop : Prop) (field : Field) =
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.Center) field
            let field = mapAdvents (Set.add (Opened chestId)) field
            let field = mapInventory (Inventory.tryAddItem itemType >> snd) field
            let field =
                match battleTypeOpt with
                | Some battleType -> mapDialogOpt (constant (Some (Dialog.makePlus DialogThin ("Found " + ItemType.getName itemType + "!^But something approaches!") None (Some (battleType, Set.empty))))) field
                | None -> mapDialogOpt (constant (Some (Dialog.make DialogThin ("Found " + ItemType.getName itemType + "!")))) field
            let field = mapCue (constant cue) field
            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestOpenSound)) field
        else
            let field = mapAvatar (Avatar.lookAt prop.Center) field
            let field = mapDialogOpt (constant (Some (Dialog.make DialogThin "Locked!"))) field
            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ChestLockedSound)) field

    let private interactDoor keyItemTypeOpt cue requirements (prop : Prop) (field : Field) =
        match prop.PropState with
        | DoorState false ->
            if  field.Advents_.IsSupersetOf requirements &&
                Option.mapOrDefaultValue (fun keyItemType -> Map.containsKey (KeyItem keyItemType) field.Inventory_.Items) true keyItemTypeOpt then
                let field = mapAvatar (Avatar.lookAt prop.Center) field
                let field = mapCue (constant cue) field
                let field = mapPropState (constant (DoorState true)) prop.PropId field
                withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorOpenSound)) field
            else
                let field = mapAvatar (Avatar.lookAt prop.Center) field
                let field = mapDialogOpt (constant (Some (Dialog.make DialogThin "Locked!"))) field
                withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DoorLockedSound)) field
        | _ -> failwithumf ()

    let private interactSwitch cue cue2 onRequirements requirements (prop : Prop) (field : Field) =
        let on = field.Advents_.IsSupersetOf onRequirements
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.Center) field
            let field = mapCue (constant (if on then cue2 else cue)) field
            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchUseSound)) field
        else
            let field = mapAvatar (Avatar.lookAt prop.Center) field
            let field = mapDialogOpt (constant (Some (Dialog.make DialogThin "Won't budge!"))) field
            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SwitchStuckSound)) field

    let private interactCharacter cue (prop : Prop) (field : Field) =
        let field = mapAvatar (Avatar.lookAt prop.BottomInset) field
        let field = mapCue (constant cue) field
        withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field

    let private interactNpc branches requirements (prop : Prop) (field : Field) =
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.BottomInset) field
            let branchesFiltered = branches |> List.choose (fun (branch : CueSystem.CueBranch) -> if field.Advents_.IsSupersetOf branch.Requirements then Some branch.Cue else None) |> List.rev
            let branchCue = match List.tryHead branchesFiltered with Some cue -> cue | None -> CueSystem.Dialog ("...", false)
            let field = mapCue (constant branchCue) field
            withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field
        else just field

    let private interactShopkeep shopType (prop : Prop) (field : Field) =
        let field = mapAvatar (Avatar.lookAt prop.BottomInset) field
        let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
        let field = mapShopOpt (constant (Some shop)) field
        withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)) field

    let private interactSeal cue (field : Field) =
        let field = mapCue (constant cue) field
        withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.SealedSound)) field

    let private interactSavePoint (field : Field) =
        let field = restoreTeam field
        save field
        let field = mapDialogOpt (constant (Some (Dialog.make DialogThin "Recovered strength and saved game."))) field
        withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.SlotSound)) field

    let interact (field : Field) =
        match field.DialogOpt with
        | None ->
            if touchingSavePoint field then
                interactSavePoint field
            else
                match tryGetFacingProp field with
                | Some prop ->
                    match prop.PropData with
                    | Sprite _ -> just field
                    | Portal _ -> just field
                    | Door (_, keyItemTypeOpt, cue, _, requirements) -> interactDoor keyItemTypeOpt cue requirements prop field
                    | Chest (_, itemType, chestId, battleTypeOpt, cue, requirements) -> interactChest itemType chestId battleTypeOpt cue requirements prop field
                    | Switch (_, cue, cue2, onRequirements, requirements) -> interactSwitch cue cue2 onRequirements requirements prop field
                    | Sensor _ -> just field
                    | Character (_, _, _, _, cue, _) -> interactCharacter cue prop field
                    | Npc (_, _, cue, requirements) -> interactNpc [{ CueSystem.Cue = cue; CueSystem.Requirements = Set.empty }] requirements prop field
                    | NpcBranching (_, _, branches, requirements) -> interactNpc branches requirements prop field
                    | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType prop field
                    | Seal (_, cue, _) -> interactSeal cue field
                    | Flame _ -> just field
                    | SavePoint -> just field
                    | ChestSpawn -> just field
                    | PortalSpawn -> just field
                    | EmptyProp -> just field
                | None -> just field
        | Some dialog ->
            interactDialog dialog field

    let rec private updateCue (cue : Cue) (definitions : CueDefinitions) (field : Field) :
        Cue * CueDefinitions * (Signal list * Field) =

        match cue with
        | Fin ->
            (cue, definitions, just field)

        | Cue.PlaySound (volume, sound) ->
            (Fin, definitions, withSignal (PlaySound (0L, volume, sound)) field)

        | Cue.PlaySong (fadeIn, fadeOut, start, volume, song) ->
            (Fin, definitions, withSignal (PlaySong (fadeIn, fadeOut, start, volume, song)) field)

        | Cue.FadeOutSong fade ->
            (Fin, definitions, withSignal (FadeOutSong fade) field)

        | Face (target, direction) ->
            match target with
            | AvatarTarget ->
                let field = mapAvatar (Avatar.mapDirection (constant direction)) field
                (Fin, definitions, just field)
            | CharacterTarget characterType ->
                let propIdOpt =
                    tryGetPropIdByData
                        (function
                         | Character (characterType2, _, _, _, _, requirements) -> characterType = characterType2 && field.Advents_.IsSupersetOf requirements
                         | _ -> false)
                        field
                match propIdOpt with
                | Some propId ->
                    let field =
                        mapPropState
                            (function
                             | CharacterState (color, animationState) ->
                                let animationState = CharacterAnimationState.face direction animationState
                                CharacterState (color, animationState)
                             | propState -> propState)
                            propId
                            field
                    (Fin, definitions, just field)
                | None ->
                    (Fin, definitions, just field)
            | NpcTarget _ | ShopkeepTarget _ | CharacterIndexTarget _ | SpriteTarget _ ->
                (Fin, definitions, just field)

        | ClearSpirits ->
            let field = clearSpirits field
            (Fin, definitions, just field)

        | Recruit allyType ->
            let fee = getRecruitmentFee field
            if field.Inventory_.Gold >= fee then
                let advent =
                    match allyType with
                    | Jinn -> failwithumf ()
                    | Shade -> ShadeRecruited
                    | Mael -> MaelRecruited
                    | Riain -> RiainRecruited
                    | Peric -> PericRecruited
                let field = recruit allyType field
                let field = mapAdvents (Set.add advent) field
                let field = mapInventory (Inventory.removeGold fee) field
                (Fin, definitions, withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.PurchaseSound)) field)
            else
                updateCue
                    (Parallel
                        [Cue.Dialog ("You don't have enough...", false)
                         Cue.PlaySound (Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)]) definitions field

        | AddItem itemType ->
            (Fin, definitions, just (mapInventory (Inventory.tryAddItem itemType >> snd) field))

        | RemoveItem itemType ->
            (Fin, definitions, just (mapInventory (Inventory.tryRemoveItem itemType >> snd) field))

        | AddAdvent advent ->
            (Fin, definitions, just (mapAdvents (Set.add advent) field))

        | RemoveAdvent advent ->
            (Fin, definitions, just (mapAdvents (Set.remove advent) field))

        | ReplaceAdvent (remove, add) ->
            (Fin, definitions, just (mapAdvents (Set.remove remove >> Set.add add) field))

        | Wait time ->
            (WaitState (field.FieldTime_ + time), definitions, just field)

        | WaitState time ->
            if field.FieldTime_ < time
            then (cue, definitions, just field)
            else (Fin, definitions, just field)

        | Tint (length, colorStart, colorStop) ->
            (TintState (field.FieldTime_, length, colorStart, colorStop), definitions, just field)

        | TintState (startTime, length, colorStart, colorStop) ->
            let time = field.FieldTime_
            let localTime = time - startTime
            let progress = single localTime / single length
            let tint = colorStart * (1.0f - progress) + colorStop * progress
            let field = mapTint (constant tint) field
            if progress >= 1.0f
            then (Fin, definitions, just field)
            else (cue, definitions, just field)

        | Fade (target, length, fadeIn) ->
            (FadeState (field.FieldTime_, target, length, fadeIn), definitions, just field)

        | FadeState (startTime, target, length, fadeIn) ->
            let time = field.FieldTime_
            let localTime = time - startTime
            let progress = single localTime / single length
            let progress = if fadeIn then progress else 1.0f - progress
            let field =
                match target with
                | CharacterTarget characterType ->
                    let propIdOpt =
                        tryGetPropIdByData
                            (function
                             | Character (characterType2, _, _, _, _, requirements) -> characterType = characterType2 && field.Advents_.IsSupersetOf requirements
                             | _ -> false)
                            field
                    match propIdOpt with
                    | Some propId ->
                        mapPropState
                            (function
                             | CharacterState (_, animationState) -> CharacterState (Color.One.MapA ((*) progress), animationState)
                             | propState -> propState)
                            propId
                            field
                    | None -> field
                | SpriteTarget spriteName ->
                    match tryGetPropIdByData (function Sprite (spriteName2, _, _, _, _, _, _) -> spriteName = spriteName2 | _ -> false) field with
                    | Some propId ->
                        mapProp
                            (fun prop ->
                                match prop.PropData with
                                | Sprite (_, _, color, _, _, _, _) ->
                                    Prop.mapPropState
                                        (function
                                         | SpriteState (image, _, blend, emission, flip, _) -> SpriteState (image, color.MapA ((*) progress), blend, emission, flip, true)
                                         | propState -> propState)
                                        prop
                                | _ -> prop)
                            propId
                            field
                    | None -> field
                | _ -> field
            if  fadeIn && progress >= 1.0f ||
                not fadeIn && progress <= 0.0f then
                (Fin, definitions, just field)
            else (cue, definitions, just field)

        | Animate (target, characterAnimationType, wait) ->
            match target with
            | AvatarTarget ->
                let field = mapAvatar (Avatar.animate field.FieldTime_ characterAnimationType) field
                match wait with
                | Timed 0L | NoWait -> (Fin, definitions, just field)
                | CueWait.Wait | Timed _ -> (AnimateState (field.FieldTime_, wait), definitions, just field)
            | CharacterTarget characterType ->
                let propIdOpt =
                    tryGetPropIdByData
                        (function
                         | Character (characterType2, _, _, _, _, requirements) -> characterType = characterType2 && field.Advents_.IsSupersetOf requirements
                         | _ -> false)
                        field
                match propIdOpt with
                | Some propId ->
                    let field =
                        mapPropState
                            (function
                             | CharacterState (color, animationState) ->
                                let animationState = CharacterAnimationState.setCharacterAnimationType field.FieldTime_ characterAnimationType animationState
                                CharacterState (color, animationState)
                             | propState -> propState)
                            propId
                            field
                    (Fin, definitions, just field)
                | None ->
                    (Fin, definitions, just field)
            | NpcTarget _ | ShopkeepTarget _ | CharacterIndexTarget _ | SpriteTarget _ ->
                (Fin, definitions, just field)

        | AnimateState (startTime, wait) ->
            let time = field.FieldTime_
            match wait with
            | CueWait.Wait ->
                if Avatar.getAnimationFinished time field.Avatar_
                then (Fin, definitions, just field)
                else (cue, definitions, just field)
            | Timed waitTime ->
                let localTime = time - startTime
                if localTime < waitTime
                then (cue, definitions, just field)
                else (Fin, definitions, just field)
            | NoWait ->
                (Fin, definitions, just field)

        | Move (target, destination, moveType) ->
            match target with
            | AvatarTarget ->
                let cue = MoveState (field.FieldTime_, target, field.Avatar_.Bottom, destination, moveType)
                (cue, definitions, just field)
            | CharacterTarget characterType ->
                let propIdOpt =
                    tryGetPropIdByData
                        (function
                         | Character (characterType2, _, _, _, _, requirements) -> characterType = characterType2 && field.Advents_.IsSupersetOf requirements
                         | _ -> false)
                        field
                match propIdOpt with
                | Some propId ->
                    let prop = getProp propId field
                    let cue = MoveState (field.FieldTime_, target, prop.Perimeter.Bottom, destination, moveType)
                    (cue, definitions, just field)
                | None -> (Fin, definitions, just field)
            | NpcTarget _ | ShopkeepTarget _ | CharacterIndexTarget _ | SpriteTarget _ ->
                (Fin, definitions, just field)

        | MoveState (startTime, target, origin, translation, moveType) ->
            match target with
            | AvatarTarget ->
                let time = field.FieldTime_
                let localTime = time - startTime
                let (step, stepCount) = CueMovement.computeStepAndStepCount translation moveType
                let totalTime = int64 (dec stepCount)
                if localTime < totalTime then
                    let field = mapAvatar (Avatar.mapBottom ((+) step)) field
                    (cue, definitions, just field)
                else
                    let field = mapAvatar (Avatar.mapBottom (constant (origin + translation))) field
                    (Fin, definitions, just field)
            | CharacterTarget characterType ->
                let propIdOpt =
                    tryGetPropIdByData
                        (function
                         | Character (characterType2, _, _, _, _, requirements) -> characterType = characterType2 && field.Advents_.IsSupersetOf requirements
                         | _ -> false)
                        field
                match propIdOpt with
                | Some propId ->
                    let time = field.FieldTime_
                    let prop = getProp propId field
                    let localTime = time - startTime
                    let (step, stepCount) = CueMovement.computeStepAndStepCount translation moveType
                    let finishTime = int64 (dec stepCount)
                    if localTime < finishTime then
                        let bounds = prop.Perimeter.Translate step
                        let field = mapProp (Prop.mapPerimeter (constant bounds)) propId field
                        (cue, definitions, just field)
                    else
                        let bounds = prop.Perimeter.WithBottom (origin + translation)
                        let field = mapProp (Prop.mapPerimeter (constant bounds)) propId field
                        (Fin, definitions, just field)
                | None -> (Fin, definitions, just field)
            | NpcTarget _ | ShopkeepTarget _ | CharacterIndexTarget _ | SpriteTarget _ ->
                (Fin, definitions, just field)

        | Warp (fieldType, fieldDestination, fieldDirection) ->
            match field.FieldTransitionOpt_ with
            | Some _ ->
                (cue, definitions, just field)
            | None ->
                let fieldTransition =
                    { FieldType = fieldType
                      FieldDestination = fieldDestination
                      FieldDirection = fieldDirection
                      FieldTransitionTime = field.FieldTime_ + Constants.Field.TransitionTime }
                let field = mapFieldTransitionOpt (constant (Some fieldTransition)) field
                (WarpState, definitions, just field)

        | WarpState ->
            match field.FieldTransitionOpt_ with
            | Some _ -> (cue, definitions, just field)
            | None -> (Fin, definitions, just field)

        | Dialog (text, isNarration) ->
            match field.DialogOpt_ with
            | Some _ ->
                (cue, definitions, just field)
            | None ->
                let dialogForm = if isNarration then DialogNarration else DialogThick
                let dialog = Dialog.make dialogForm text
                let field = mapDialogOpt (constant (Some dialog)) field
                (DialogState, definitions, just field)

        | DialogState ->
            match field.DialogOpt_ with
            | None -> (Fin, definitions, just field)
            | Some _ -> (cue, definitions, just field)

        | Prompt (text, leftPrompt, rightPrompt) ->
            match field.DialogOpt_ with
            | Some _ ->
                (cue, definitions, just field)
            | None ->
                let dialog = Dialog.makePrompt DialogThick text (leftPrompt, rightPrompt)
                let field = mapDialogOpt (constant (Some dialog)) field
                (PromptState, definitions, just field)

        | PromptState ->
            match field.DialogOpt_ with
            | None -> (Fin, definitions, just field)
            | Some _ -> (cue, definitions, just field)

        | Battle (battleType, consequents) ->
            (Fin, definitions, withSignal (TryCommencingBattle (battleType, consequents)) field)

        | If (p, c, a) ->
            match p with
            | Gold gold -> if field.Inventory_.Gold >= gold then (c, definitions, just field) else (a, definitions, just field)
            | Item itemType -> if Inventory.containsItem itemType field.Inventory_ then (c, definitions, just field) else (a, definitions, just field)
            | Items itemTypes -> if Inventory.containsItems itemTypes field.Inventory_ then (c, definitions, just field) else (a, definitions, just field)
            | Advent advent -> if field.Advents_.Contains advent then (c, definitions, just field) else (a, definitions, just field)
            | Advents advents -> if field.Advents_.IsSupersetOf advents then (c, definitions, just field) else (a, definitions, just field)

        | Not (p, c, a) ->
            match p with
            | Gold gold -> if field.Inventory_.Gold < gold then (c, definitions, just field) else (a, definitions, just field)
            | Item itemType -> if not (Inventory.containsItem itemType field.Inventory_) then (c, definitions, just field) else (a, definitions, just field)
            | Items itemTypes -> if not (Inventory.containsItems itemTypes field.Inventory_) then (c, definitions, just field) else (a, definitions, just field)
            | Advent advent -> if not (field.Advents_.Contains advent) then (c, definitions, just field) else (a, definitions, just field)
            | Advents advents -> if not (field.Advents_.IsSupersetOf advents) then (c, definitions, just field) else (a, definitions, just field)

        | Define (name, body) ->
            if not (Map.containsKey name definitions) then
                (Fin, Map.add name body definitions, just field)
            else
                Log.debug ("Cue definition '" + name + "' already found.")
                (Fin, definitions, just field)

        | Assign (name, body) ->
            if Map.containsKey name definitions then
                (Fin, Map.add name body definitions, just field)
            else
                Log.debug ("Cue definition '" + name + "' not found.")
                (Fin, definitions, just field)

        | Expand name ->
            match Map.tryFind name definitions with
            | Some body ->
                updateCue body definitions field
            | None ->
                Log.debug ("Cue definition '" + name + "' not found.")
                (Fin, definitions, ([], field))

        | Parallel cues ->
            let (cues, definitions, (signals, field)) =
                List.fold (fun (cues, definitions, (signals, field)) cue ->
                    let (cue, definitions, (signals2, field)) = updateCue cue definitions field
                    if Cue.isFin cue
                    then (cues, definitions, (signals @ signals2, field))
                    else (cues @ [cue], definitions, (signals @ signals2, field)))
                    ([], definitions, ([], field))
                    cues
            match cues with
            | _ :: _ -> (Parallel cues, definitions, (signals, field))
            | [] -> (Fin, definitions, (signals, field))

        | Sequence cues ->
            let (_, haltedCues, definitions, (signals, field)) =
                List.fold (fun (halted, haltedCues, definitions, (signals, field)) cue ->
                    if halted
                    then (halted, haltedCues @ [cue], definitions, (signals, field))
                    else
                        let (cue, definitions, (signals2, field)) = updateCue cue definitions field
                        if Cue.isFin cue
                        then (false, [], definitions, (signals @ signals2, field))
                        else (true, [cue], definitions, (signals @ signals2, field)))
                    (false, [], definitions, ([], field))
                    cues
            match haltedCues with
            | _ :: _ -> (Sequence haltedCues, definitions, (signals, field))
            | [] -> (Fin, definitions, (signals, field))

    let private updateSpirits (field : Field) =
        match field.FieldTransitionOpt with
        | None ->
            let field =
                { field with
                    SpiritActivity_ = field.SpiritActivity_ + field.SpiritRate_ }
            let field =
                { field with
                    Spirits_ =
                        Array.map (Spirit.update field.FieldTime_ field.Avatar_.Center) field.Spirits_ }
            let field =
                { field with
                    Spirits_ =
                        Array.filter (fun (spirit : Spirit) ->
                            let delta = field.Avatar_.Bottom - spirit.Center
                            let distance = delta.Magnitude
                            distance < Constants.Field.SpiritRadius * 1.25f)
                            field.Spirits }
            let field =
                let spiritsNeeded = int (field.SpiritActivity_ / single Constants.Field.SpiritActivityThreshold)
                let spiritsDeficient = spiritsNeeded - Array.length field.Spirits_
                let spiritsSpawned =
                    match Data.Value.Fields.TryGetValue field.FieldType_ with
                    | (true, fieldData) ->
                        [|0 .. spiritsDeficient - 1|] |>
                        Array.map (fun _ ->
                            let spiritPattern =
                                if spiritsNeeded >= Constants.Field.SpiritActivityAggressionThreshold
                                then SpiritPattern.generate ()
                                else SpiritPattern.Confused
                            match FieldData.tryGetSpiritType field.OmniSeedState_ field.Avatar_.Bottom fieldData with
                            | Some spiritType ->
                                let spiritMovement = SpiritPattern.toSpiritMovement spiritPattern
                                let spirit = Spirit.spawn field.FieldTime_ field.Avatar_.Bottom spiritType spiritMovement
                                Some spirit
                            | None -> None) |>
                        Array.definitize
                    | (false, _) -> [||]
                { field with Spirits_ = Array.append field.Spirits_ spiritsSpawned }
            match Array.tryFind (fun (spirit : Spirit) -> Vector3.Distance (field.Avatar_.LowerCenter, spirit.Bottom) < Constants.Field.SpiritCollisionRadius) field.Spirits_ with
            | Some spirit ->
                match Data.Value.Fields.TryGetValue field.FieldType_ with
                | (true, fieldData) ->
                    match fieldData.EncounterTypeOpt with
                    | Some encounterType ->
                        match Data.Value.Encounters.TryGetValue encounterType with
                        | (true, encounterData) ->
                            let battleType =
                                // TODO: toughen up this code.
                                match spirit.SpiritType with
                                | WeakSpirit -> encounterData.BattleTypes.[Gen.random2 0 3]
                                | NormalSpirit -> encounterData.BattleTypes.[Gen.random2 3 6]
                                | StrongSpirit -> encounterData.BattleTypes.[Gen.random2 6 9]
                            match Data.Value.Battles.TryGetValue battleType with
                            | (true, battleData) -> Left (battleData, field)
                            | (false, _) -> Right field
                        | (false, _) -> Right field
                    | None -> Right field
                | (false, _) -> Right field
            | None -> Right field
        | Some _ -> Right field

    let update field =

        // ensure we're playing
        match field.FieldState_ with
        | Playing ->

            // update dialog
            let field =
                match field.DialogOpt_ with
                | Some dialog ->
                    let dialog = Dialog.update (detokenize field) field.FieldTime_ dialog
                    mapDialogOpt (constant (Some dialog)) field
                | None -> field

            // update cue
            let (cue, definitions, (signals, field)) = updateCue field.Cue_ field.Definitions_ field

            // reset cue definitions if finished
            let field =
                match cue with
                | CueSystem.Fin -> mapDefinitions (constant field.DefinitionsOriginal_) field
                | _ -> mapDefinitions (constant definitions) field
            let field = mapCue (constant cue) field

            // update portal
            let (signals, field) =
                match field.FieldTransitionOpt_ with
                | None ->
                    match tryGetTouchingPortal field with
                    | Some (fieldType, destination, direction, isWarp) ->
                        let transition =
                            { FieldType = fieldType
                              FieldDestination = destination
                              FieldDirection = direction
                              FieldTransitionTime = field.FieldTime_ + Constants.Field.TransitionTime }
                        let field = mapFieldTransitionOpt (constant (Some transition)) field
                        let playSound =
                            if isWarp
                            then PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepWarpSound)
                            else PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.StepStairSound)
                        (signal playSound :: signals, field)
                    | None -> (signals, field)
                | Some _ -> (signals, field)

            // update sensor
            let (signals, field) =
                match field.FieldTransitionOpt_ with
                | None ->
                    let sensors = getTouchedSensors field
                    let results =
                        List.fold (fun (signals : Signal list, field : Field) (sensorType, cue, requirements) ->
                            if field.Advents_.IsSupersetOf requirements then
                                let field = mapCue (constant cue) field
                                match sensorType with
                                | AirSensor -> (signals, field)
                                | HiddenSensor | StepPlateSensor -> (signal (PlaySound (0L,  Constants.Audio.SoundVolumeDefault, Assets.Field.StepPlateSound)) :: signals, field)
                            else (signals, field))
                            (signals, field) sensors
                    results
                | Some _ -> (signals, field)

            // update spirits
            let (signals : Signal list, field) =
                if  field.Menu_.MenuState = MenuClosed &&
                    CueSystem.Cue.notInterrupting field.Inventory_ field.Advents_ field.Cue_ &&
                    Option.isNone field.DialogOpt_ &&
                    Option.isNone field.ShopOpt_ &&
                    Option.isNone field.FieldTransitionOpt_ then
                    match updateSpirits field with
                    | Left (battleData, field) ->
                        let prizePool = { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }
                        let field = commencingBattle battleData prizePool field
                        (signal CommencingBattle :: signals, field)
                    | Right field -> (signals, field)
                else (signals, field)

            // fin
            (signals, field)

        // fin
        | _ -> just field

    let updateFieldTime field =
        let field = { field with FieldTime_ = inc field.FieldTime_ }
        just field

    let make time viewBounds2dAbsolute fieldType saveSlot randSeedState (avatar : Avatar) team advents inventory =
        let (spiritRate, debugAdvents, debugKeyItems, definitions) =
            match Data.Value.Fields.TryGetValue fieldType with
            | (true, fieldData) -> (fieldData.EncounterRate, fieldData.FieldDebugAdvents, fieldData.FieldDebugKeyItems, fieldData.Definitions)
            | (false, _) -> (1.0f, Set.empty, List.empty, Map.empty)
        let (advents, inventory) =
            match fieldType with
            | DebugField -> (debugAdvents, snd (Inventory.tryAddItems (List.map KeyItem debugKeyItems) inventory))
            | _ -> (advents, inventory)
        let omniSeedState = OmniSeedState.makeFromSeedState randSeedState
        let props = makeProps time fieldType omniSeedState
        { FieldTime_ = 0L
          ViewBoundsAbsolute_ = viewBounds2dAbsolute
          FieldType_ = fieldType
          SaveSlot_ = saveSlot
          OmniSeedState_ = omniSeedState
          Avatar_ = avatar
          AvatarCollidedPropIds_ = []
          AvatarSeparatedPropIds_ = []
          AvatarIntersectedPropIds_ = []
          SpiritRate_ = spiritRate
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = team
          Advents_ = advents
          Props_ = props
          Inventory_ = inventory
          Options_ = { BattleSpeed = PacedSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          PartyMenu_ = { PartyMenuState = PartyMenuClosed; PartyMenuSelections = [] }
          Definitions_ = definitions
          DefinitionsOriginal_ = definitions
          Cue_ = CueSystem.Fin
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          Tint_ = Color.Zero
          ShopOpt_ = None
          DialogOpt_ = None
          FieldSongTimeOpt_ = None
          FieldState_ = Playing }

    let empty viewBounds2dAbsolute =
        { FieldTime_ = 0L
          ViewBoundsAbsolute_ = viewBounds2dAbsolute
          FieldType_ = EmptyField
          SaveSlot_ = Slot1
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.empty ()
          AvatarCollidedPropIds_ = []
          AvatarSeparatedPropIds_ = []
          AvatarIntersectedPropIds_ = []
          SpiritRate_ = 1.0f
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = Map.empty
          Advents_ = Advents.empty
          Props_ = Map.empty
          Inventory_ = Inventory.initial
          Options_ = { BattleSpeed = PacedSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          PartyMenu_ = { PartyMenuState = PartyMenuClosed; PartyMenuSelections = [] }
          Definitions_ = Map.empty
          DefinitionsOriginal_ = Map.empty
          Cue_ = CueSystem.Fin
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          Tint_ = Color.Zero
          ShopOpt_ = None
          DialogOpt_ = None
          FieldSongTimeOpt_ = None
          FieldState_ = Quit }

    let initial time viewBounds2dAbsolute saveSlot =
        make time viewBounds2dAbsolute TombOuter saveSlot (max 1UL Gen.randomul) (Avatar.initial ()) (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial

    let debug time viewBounds2dAbsolute =
        make time viewBounds2dAbsolute DebugField Slot1 Rand.DefaultSeedState (Avatar.empty ()) (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial

    let tryLoad time saveSlot =
        try let saveFilePath =
                match saveSlot with
                | Slot1 -> Assets.Global.SaveFilePath1
                | Slot2 -> Assets.Global.SaveFilePath2
                | Slot3 -> Assets.Global.SaveFilePath3
            let fieldStr = File.ReadAllText saveFilePath
            let field = scvalue<Field> fieldStr
            let props = makeProps time field.FieldType_ field.OmniSeedState_
            Some { field with Props_ = props }
        with _ -> None

    let loadOrInitial time viewBounds2dAbsolute saveSlot =
        match tryLoad time saveSlot with
        | Some field -> field
        | None -> initial time viewBounds2dAbsolute saveSlot

type Field = Field.Field