// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open Prime
open Nu
open CueSystem

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

/// What the field shows that the cursor can be in - walking about, the dialog or its prompt, or a menu or panel.
type FieldCursorMenu =
    | NoFieldMenu
    | DialogPanel
    | PromptPanel
    | TeamPanel
    | EquipPanel
    | EquipInfoPanel
    | AutoMapPanel
    | InventoryPanel
    | KeyItemsPanel
    | UsePanel
    | TechsPanel
    | TechPanel
    | OptionsPanel
    | QuitPanel
    | PartyPanel
    | ShopPanel
    | ShopConfirmPanel

/// Where the field's menu cursor is, for the gamepad and keyboard. It starts over whenever the field shows another
/// menu, except when coming back out of a menu opened over the one it was in, where it returns to where it was.
type FieldCursor =
    { CursorMenu : FieldCursorMenu
      CursorPosition : FieldCursorPosition
      CursorParents : (FieldCursorMenu * FieldCursorPosition) list } // the menus under this one and the cursor in each

    static member initial =
        { CursorMenu = NoFieldMenu
          CursorPosition = FieldCursorPosition.sidebar
          CursorParents = [] }

/// What the field's pointer points at.
type FieldPointed =
    | TeamSidebarPointed of int // the sidebar's row
    | InventorySidebarPointed of int
    | TechsSidebarPointed of int
    | KeyItemsSidebarPointed of int
    | OptionsSidebarPointed of int
    | TeamTeammatePointed of int // the teammate's team index
    | TechsTeammatePointed of int
    | UseTeammatePointed of int
    | PartyTeammatePointed of int
    | EquipmentPointed of int // weapon, armor or accessory
    | AutoMapPointed
    | TechPointed of TechType
    | InventoryItemPointed of ItemType
    | KeyItemPointed of ItemType
    | EquipItemPointed of ItemType
    | ShopItemPointed of ItemType
    | OptionPointed of int // the options menu's button
    | EquipButtonPointed
    | EquipInfoPointed
    | EquipCancelPointed
    | InfoClosePointed
    | UseClosePointed
    | TechClosePointed
    | PromptChoicePointed of int // of two side by side, left or right
    | AutoMapChoicePointed of int
    | QuitChoicePointed of int
    | ShopConfirmChoicePointed of int
    | PartySelectedPointed of int
    | PartyConfirmPointed
    | ShopTradePointed
    | ShopLeavePointed

type FieldMessage =
    | ProcessMenuInput
    | Update
    | UpdateFieldTransition
    | AdvanceTime
    | UpdateAvatarBodyTracking
    | AvatarBodyTransform of BodyTransformData
    | AvatarBodyPenetration of BodyPenetrationData
    | AvatarBodySeparationExplicit of BodySeparationExplicitData
    | AvatarBodySeparationImplicit of BodySeparationImplicitData
    | ScreenTransitioning of bool
    | FinishQuitting
    | TryCommencingBattle of BattleType * Advent Set
    | MenuTeamOpen
    | MenuTeamAlly of int
    | MenuTeamAutoMapSelect of bool
    | MenuTeamEquip of EquipType
    | MenuTeamEquipMenuUseOpen
    | MenuTeamEquipMenuUseClose
    | MenuTeamEquipPageUp
    | MenuTeamEquipPageDown
    | MenuTeamEquipSelect of int * (ItemType * int Option)
    | MenuTeamEquipDeselect
    | MenuTeamEquipConfirm
    | MenuTeamEquipCancel
    | MenuAutoMapOpen
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
    | MenuOptionsHardcoreModeToggle
    | MenuOptionsQuitPrompt
    | MenuOptionsQuitConfirm
    | MenuOptionsQuitCancel
    | MenuOptionsSongVolumeDown
    | MenuOptionsSongVolumeUp
    | MenuCloseSelect
    | MenuCloseDeselect
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
    | SkipDialog
#if DEV
    | ReloadProps
#endif
    interface Message

type FieldCommand =
    | ProcessKeyInput
    | ProcessTouchInput of Vector2
    | UpdateEye
    | WarpAvatar of Vector3
    | StartPlaying
    | StartQuitting
    | CommencingBattle of BattleData
    | CommenceBattle of BattleData * PrizePool
    | MenuOptionsToggleFullScreen
    | Nop
    interface Command

type [<SymbolicExpansion>] Options =
    { BattleSpeed : BattleSpeed
      HardcoreMode : bool
      SongVolume : single }

type FieldState =
    | Playing
    | Battling of BattleData * PrizePool
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
              FieldState_ : FieldState
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
              Menu_ : Menu
              PartyMenu_ : PartyMenu
              ShopOpt_ : Shop option
              Options_ : Options
              CueDefinitions_ : CueSystem.CueDefinitions
              CueDefinitionsOriginal_ : CueSystem.CueDefinitions
              Cue_ : CueSystem.Cue
              Tint_ : Color
              ScreenTransitioning_ : bool
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : Dialog option
              FieldSongTimeOpt_ : int64 option
              AutoMaps_ : Map<FieldType, Vector2i Set>
              FieldType_ : FieldType
              FieldCursor_ : FieldCursor // never saved, starting over when loaded
              FieldPointerShown_ : bool // never saved, as with the intent
              FieldIntent_ : Intent }

        (* Local Properties *)
        member this.FieldTime = this.FieldTime_
        member this.FieldState = this.FieldState_
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
        member this.Menu = this.Menu_
        member this.PartyMenu = this.PartyMenu_
        member this.ShopOpt = this.ShopOpt_
        member this.Options = this.Options_
        member this.CueDefinitions = this.CueDefinitions_
        member this.CueDefinitionsOriginal = this.CueDefinitionsOriginal_
        member this.Cue = this.Cue_
        member this.Tint = this.Tint_
        member this.ScreenTransitioning = this.ScreenTransitioning_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.FieldSongTimeOpt = this.FieldSongTimeOpt_
        member this.AutoMaps = this.AutoMaps_
        member this.FieldType = this.FieldType_
        member this.FieldCursor = this.FieldCursor_
        member this.FieldPointerShown = this.FieldPointerShown_
        member this.FieldIntent = this.FieldIntent_

    (* Low-Level Operations *)

    let private ChecksumToken =
        "\n[H3CK$UM:"

    let private SidebarButtons =
        [|(Some TeamPanel, MenuTeamOpen)
          (Some InventoryPanel, MenuInventoryOpen)
          (Some TechsPanel, MenuTechsOpen)
          (Some KeyItemsPanel, MenuKeyItemsOpen)
          (Some OptionsPanel, MenuOptionsOpen)
          (None, MenuClose)|]

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

    let private makeBattleFromTeam hardcoreMode battleSpeed inventory (team : Map<int, Teammate>) prizePool battleData =
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
                        let position = if party.Length = 1 then allyPositions[teamIndex] + Constants.Battle.CharacterOffset else allyPositions[teamIndex]
                        let bounds = box3 position size
                        let characterIndex = AllyIndex teamIndex
                        let characterType = characterData.CharacterType
                        let boss = characterData.Boss
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector3 -bounds.Bottom
                        let actionTime = 1000.0f - Constants.Battle.AllyActionTimeSpacing * single index
                        let characterState = CharacterState.make characterData teammate.HitPoints teammate.TechPoints teammate.ExpPoints teammate.WeaponOpt teammate.ArmorOpt teammate.Accessories
                        let conjureChargeRemainder = teammate.ConjureChargeRemainder
                        let character = Character.make bounds characterIndex 0 characterType boss animationSheet celSize direction characterState conjureChargeRemainder None actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring teammate.CharacterType + "'."))
                party
        Battle.makeFromParty hardcoreMode inventory party prizePool battleSpeed battleData

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

    let hasAutoMap field =
        match Data.Value.Fields.TryGetValue field.FieldType_ with
        | (true, fieldData) ->
            match FieldData.tryGetTileMap field.OmniSeedState_ fieldData with
            | Some (Choice3Of4 (_, _, _)) -> true
            | Some _ | None -> false
        | (false, _) -> false

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
        field.Props_[propId]

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
                if prop.Perimeter.Bottom.Y - field.Avatar_.Perimeter.Bottom.Y > 40.0f // NOTE: just a bit of hard-coding to ensure player is interacting with the character from the south.
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
            let v = prop.Perimeter.Bottom - field.Avatar_.Perimeter.Bottom
            let direction = Direction.ofVector3 v
            direction <> field.Avatar_.Direction.Opposite
        | (false, _) -> false

    let getFacingProps (field : Field) =
        List.filter
            (fun propId -> facingProp propId field)
            field.AvatarIntersectedPropIds

    let tryGetFacingProp (field : Field) =
        match getFacingProps field with
        | head :: _ -> Some (field.Props_[head])
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

    let setFieldType time fieldType field =
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

    let mapAvatar mapper field =
        let avatar = mapper field.Avatar_
        if avatar =/= field.Avatar_
        then { field with Avatar_ = avatar }
        else field

    let mapAvatarCollidedPropIds mapper field =
        let propIds = mapper field.AvatarCollidedPropIds_
        if propIds =/= field.AvatarCollidedPropIds_
        then { field with AvatarCollidedPropIds_ = propIds }
        else field

    let mapAvatarSeparatedPropIds mapper field =
        let propIds = mapper field.AvatarSeparatedPropIds_
        if propIds =/= field.AvatarSeparatedPropIds_
        then { field with AvatarSeparatedPropIds_ = propIds }
        else field

    let mapAvatarIntersectedPropIds mapper field =
        let propIds = mapper field.AvatarIntersectedPropIds_
        if propIds =/= field.AvatarIntersectedPropIds_
        then { field with AvatarIntersectedPropIds_ = propIds }
        else field

    let mapTeam mapper field =
        { field with Team_ = mapper field.Team_ }

    let mapParty mapper field =
        let party = getParty field
        mapTeam (Map.map (fun teamIndex teammate ->
            if Map.containsKey teamIndex party
            then mapper teammate
            else teammate))
            field

    let mapBench mapper field =
        let party = getParty field
        mapTeam (Map.map (fun teamIndex teammate ->
            if not (Map.containsKey teamIndex party)
            then mapper teammate
            else teammate))
            field

    let mapTeammate mapper teamIndex field =
        mapTeam (fun team ->
            match Map.tryFind teamIndex team with
            | Some teammate -> Map.add teamIndex (mapper teammate) team
            | None -> team)
            field

    let mapAdvents mapper field =
        let advents = mapper field.Advents_
        if advents =/= field.Advents_ then { field with Advents_ = advents }
        else field

    let mapProps mapper field =
        { field with Props_ = mapper field.Props_ }

    let mapProp mapper propId field =
        match Map.tryFind propId field.Props_ with
        | Some prop -> mapProps (Map.add propId (mapper prop)) field
        | None -> field
    
    let mapPropState mapper propId field =
        mapProp (Prop.mapPropState mapper) propId field

    let mapInventory mapper field =
        { field with Inventory_ = mapper field.Inventory_ }

    let setFieldCursor cursor field =
        { field with FieldCursor_ = cursor }

    let setFieldPointerShown pointerShown field =
        { field with FieldPointerShown_ = pointerShown }

    let setFieldIntent intent field =
        { field with FieldIntent_ = intent }

    let mapMenu mapper field =
        { field with Menu_ = mapper field.Menu_ }

    let mapPartyMenu mapper field =
        { field with PartyMenu_ = mapper field.PartyMenu_ }

    let mapShopOpt mapper field =
        { field with ShopOpt_ = mapper field.ShopOpt_ }

    let mapOptions mapper field =
        { field with Options_ = mapper field.Options_ }

    let setCueDefinitions definitions field =
        { field with CueDefinitions_ = definitions }

    let setCue cue field =
        { field with Cue_ = cue }

    let setTint tint field =
        { field with Tint_ = tint }

    let setScreenTransitioning screenTransitioning field =
        { field with ScreenTransitioning_ = screenTransitioning }

    let setFieldTransitionOpt transitionOpt field =
        { field with FieldTransitionOpt_ = transitionOpt }

    let setDialogOpt dialogOpt field =
        { field with DialogOpt_ = dialogOpt }

    let setFieldSongTimeOpt songTimeOpt field =
        { field with FieldSongTimeOpt_ = songTimeOpt }

    (* Mid-Level Operations *)

    let clearSpirits field =
        { field with SpiritActivity_ = 0.0f; Spirits_ = [||] }

    let recruit allyType (field : Field) =
        let highestLevel = field.Team |> Map.toValueList |> List.map _.Level |> Seq.sortDescending |> Seq.headOrDefault 1
        let level = max 1 highestLevel
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
            mapTeammate (fun teammate ->
                { teammate with
                    HitPoints = ally.HitPoints
                    TechPoints = ally.TechPoints
                    ExpPoints = ally.ExpPoints
                    ConjureChargeRemainder = Option.defaultValue 0 ally.ConjureChargeOpt })
                i field)
            field allies

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
        let battle = makeBattleFromTeam field.Options.HardcoreMode field.Options.BattleSpeed field.Inventory field.Team prizePool battleData
        let field = setFieldSongTimeOpt (Some songTime) field
        (battle, field)

    let concludeBattle (prizePool : PrizePool) battle field =
        let field = synchronizeFromBattle prizePool.Consequents battle field
        let field = // award half exp to bench, restoring any who level up
            mapBench (fun teammate ->
                let level = teammate.Level
                let teammate = { teammate with ExpPoints = teammate.ExpPoints + prizePool.Exp / Constants.Gameplay.BenchExpDivisor }
                if teammate.Level > level then Teammate.restore teammate else teammate)
                field
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
            FieldSongTimeOpt_ = None
            FieldCursor_ = FieldCursor.initial
            FieldPointerShown_ = true
            FieldIntent_ = Intent.empty }

    let private computeChecksum (str : string) =
        let mutable checksum = 13L
        for c in str do checksum <- checksum + (int64 c <<< (int checksum % 64))
        checksum

    let save field =
        let saveFilePath =
            match field.SaveSlot_ with
            | Slot1 -> Assets.User.SaveFilePath1
            | Slot2 -> Assets.User.SaveFilePath2
            | Slot3 -> Assets.User.SaveFilePath3
        let fieldSavable = toSavable field
        let fieldSymbol = valueToSymbol fieldSavable
        let fieldStr = PrettyPrinter.prettyPrintSymbol fieldSymbol PrettyPrinter.defaultPrinter
        let checksumStr = string (computeChecksum fieldStr)
        try File.WriteAllText (saveFilePath, fieldStr + ChecksumToken + checksumStr) with _ -> ()

    let truncate field =
        { field with
            Spirits_ = [||]
            Advents_ = field.Advents_ |> Set.filter (fun value -> not value.IsOpened) }

    let untruncate current incoming =
        { incoming with
            Spirits_ = current.Spirits_
            Advents_ = current.Advents_ }

    (* Input *)

    let private equipmentItems equipmentType archetypeType (inventory : Inventory) =
        let filter (equipment, _) =
            match (equipmentType, equipment) with
            | (EquipWeapon _, Equipment (WeaponType weaponType)) ->
                match Map.tryFind weaponType Data.Value.Weapons with
                | Some weaponData ->
                    match Map.tryFind archetypeType Data.Value.Archetypes with
                    | Some archetypeData -> weaponData.WeaponSubtype = archetypeData.WeaponSubtype
                    | None -> false
                | None -> false
            | (EquipArmor _, Equipment (ArmorType armorType)) ->
                match Map.tryFind armorType Data.Value.Armors with
                | Some armorData ->
                    match Map.tryFind archetypeType Data.Value.Archetypes with
                    | Some archetypeData -> armorData.ArmorSubtype = archetypeData.ArmorSubtype
                    | None -> false
                | None -> false
            | (EquipAccessory _, Equipment (AccessoryType _)) -> true
            | _ -> false
        inventory.Items |>
        Map.toArray |>
        Array.filter filter |>
        Array.map (function (Equipment _ as equipment, count) -> Some (equipment, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray

    let private pageItems5 pageSize pageIndex filter sort (items : Map<ItemType, int>) =
        let items =
            items |>
            Map.toSeq |>
            (fun items -> if filter then ItemType.filterSellableItems items else items) |>
            (fun items -> if sort then ItemType.sortItems items else items)
        let itemsPaged =
            items |>
            Seq.indexed |>
            Seq.chunkBySize pageSize |>
            Seq.trySkip pageIndex |>
            Seq.map List.ofArray |>
            Seq.tryHead |>
            Option.defaultValue [] |>
            Seq.indexed |>
            Map.ofSeq |>
            Map.map (fun _ (i, (item, count)) -> (i, (item, Some count)))
        let pageUp =
            if itemsPaged.Count <> 0 then
                let firstItemPaged = Seq.head itemsPaged
                fst (snd firstItemPaged.Value) <> fst (Seq.head items)
            else false
        let pageDown =
            if itemsPaged.Count <> 0 then
                let lastItemPaged = Seq.last itemsPaged
                fst (snd lastItemPaged.Value) <> fst (Seq.last items)
            else false
        (pageUp, pageDown, itemsPaged)

    let pageItems pageSize (field : Field) =
        match field.Menu.MenuState with
        | MenuTeam menuTeam ->
            match menuTeam.TeamEquipOpt with
            | Some equip ->
                match Map.tryFind menuTeam.TeamIndex field.Team with
                | Some teammate ->
                    let items = equipmentItems equip.EquipType teammate.ArchetypeType field.Inventory
                    pageItems5 pageSize equip.EquipPage false true items
                | None -> (false, false, Map.empty)
            | None -> (false, false, Map.empty)
        | MenuInventory inventory ->
            let items = Inventory.getNonKeyItems field.Inventory
            pageItems5 pageSize inventory.InventoryPage false true items
        | MenuKeyItems keyItems ->
            let items = Inventory.getKeyItems field.Inventory
            pageItems5 pageSize keyItems.KeyItemsPage false true items
        | _ ->
            match field.ShopOpt with
            | Some shop ->
                match shop.ShopState with
                | ShopBuying ->
                    let shopItems = ShopType.getShopBuyItems field.Advents shop.ShopType
                    pageItems5 pageSize shop.ShopPage false false (Map.ofListBy (flip Pair.make 1) shopItems)
                | ShopSelling ->
                    let items = Inventory.getNonKeyItems field.Inventory
                    pageItems5 pageSize shop.ShopPage true true items
            | None -> (false, false, Map.empty)


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
        Option.isSome (tryGetInteraction field)

    let private canOpenMenu (field : Field) =
        exploring field &&
        CueSystem.Cue.notInterrupting field.Inventory field.Advents field.Cue &&
        Option.isNone field.DialogOpt

    let private canOpenParty (field : Field) =
        exploring field &&
        Option.isNone field.DialogOpt &&
        touchingSavePoint field &&
        field.Team.Count >= 3

    let private canSkip (field : Field) =
        match field.DialogOpt with
        | Some dialog ->
            not (Dialog.canAdvance (detokenize field) dialog) &&
            dialog.DialogSkippableWhenNotTombSealed &&
            not (field.Advents.Contains TombSealed)
        | None -> false

    let private promptShowing (field : Field) =
        match field.DialogOpt with
        | Some dialog -> Option.isSome dialog.DialogPromptOpt && Dialog.isExhausted (detokenize field) dialog
        | None -> false

    /// The menu the field shows now, so the cursor knows when it's moved to another.
    let getCursorMenu (field : Field) =
        match field.ShopOpt with
        | Some shop -> if shop.ShopConfirmOpt.IsSome then ShopConfirmPanel else ShopPanel
        | None ->
            if field.PartyMenu.PartyMenuState = PartyMenuOpened then PartyPanel
            else
                match field.Menu.MenuState.Shown with
                | MenuTeam menuTeam ->
                    match menuTeam.TeamEquipOpt with
                    | Some equip -> if equip.EquipMenuUseOpt.IsSome then EquipInfoPanel else EquipPanel
                    | None -> TeamPanel
                | MenuAutoMap -> AutoMapPanel
                | MenuInventory _ -> if field.Menu.MenuUseOpt.IsSome then UsePanel else InventoryPanel
                | MenuTechs techs -> if techs.TechIndexOpt.IsSome then TechPanel else TechsPanel
                | MenuKeyItems _ -> if field.Menu.MenuUseOpt.IsSome then UsePanel else KeyItemsPanel
                | MenuOptions quitPrompt -> if quitPrompt then QuitPanel else OptionsPanel
                | MenuCloseSelected _ | MenuClosed -> if promptShowing field then PromptPanel elif field.DialogOpt.IsSome then DialogPanel else NoFieldMenu

    /// The sidebar row of a main menu.
    let private getSidebarRow menu =
        match menu with
        | InventoryPanel -> 1
        | TechsPanel -> 2
        | KeyItemsPanel -> 3
        | OptionsPanel -> 4
        | _ -> 0

    /// Whether a menu's one of the main menus, which the sidebar chooses between.
    let private isMain menu =
        match menu with
        | TeamPanel | InventoryPanel | TechsPanel | KeyItemsPanel | OptionsPanel -> true
        | _ -> false

    /// Where the cursor starts in a menu - on a main menu's sidebar or in its content, and elsewhere on the first item.
    let private getStart menu onSidebar =
        match menu with
        | TeamPanel | InventoryPanel | TechsPanel | KeyItemsPanel | OptionsPanel ->
            let position = if onSidebar then FieldCursorPosition.sidebar else FieldCursorPosition.content
            { position with SidebarRow = getSidebarRow menu }
        | EquipPanel -> { FieldCursorPosition.content with Column = 1 }
        | QuitPanel -> { FieldCursorPosition.content with Column = 1 }
        | _ -> FieldCursorPosition.content

    /// Whether a menu opens over another, so that closing it goes back to where the cursor was in the other.
    let private isSubmenu parent menu =
        match (parent, menu) with
        | (TeamPanel, (EquipPanel | AutoMapPanel))
        | (EquipPanel, EquipInfoPanel)
        | ((InventoryPanel | KeyItemsPanel), UsePanel)
        | (TechsPanel, TechPanel)
        | (OptionsPanel, QuitPanel)
        | (ShopPanel, ShopConfirmPanel) -> true
        | (_, _) -> false

    /// Move the cursor to the menu the field shows - back to where it was when a menu opened over that one closes,
    /// otherwise starting over there.
    let private followInput (field : Field) (cursor : FieldCursor) =
        let menu = getCursorMenu field
        if menu = cursor.CursorMenu then cursor
        else
            match menu with
            | NoFieldMenu | DialogPanel | PromptPanel -> { FieldCursor.initial with CursorMenu = menu }
            | _ ->
                match cursor.CursorParents with
                | (parent, position) :: parents when parent = menu -> { CursorMenu = menu; CursorPosition = position; CursorParents = parents }
                | _ ->
                    if isSubmenu cursor.CursorMenu menu then
                        let parents = (cursor.CursorMenu, cursor.CursorPosition) :: cursor.CursorParents
                        { CursorMenu = menu; CursorPosition = getStart menu false; CursorParents = parents }
                    else
                        // a main menu opened from the field or from another's sidebar, as by the mouse, starts on the
                        // sidebar
                        let fromSidebar =
                            match cursor.CursorMenu with
                            | NoFieldMenu | DialogPanel | PromptPanel -> true
                            | parent -> isMain parent
                        { CursorMenu = menu; CursorPosition = getStart menu fromSidebar; CursorParents = [] }

    /// Enter a menu chosen from the sidebar, with the cursor on its first item rather than the sidebar.
    let private enter menu =
        { CursorMenu = menu; CursorPosition = getStart menu false; CursorParents = [] }

    let private moveTo column row (cursor : FieldCursor) =
        { cursor with CursorPosition = { cursor.CursorPosition with InSidebar = false; Column = column; Row = row } }

    /// Back to the sidebar, the menu's own cursor starting over for when it comes back.
    let private toSidebar (cursor : FieldCursor) =
        { cursor with CursorPosition = { cursor.CursorPosition with InSidebar = true; Column = 0; Row = 0 } }



    /// The sidebar down the left of the main menus - the button pointed at is the one selected, so stepping onto a
    /// menu's opens it and stepping onto close selects that, with the cursor staying in the sidebar. Confirm moves into
    /// the open menu, confirm on close closes it, and cancel or the menu button closes it too.
    let private updateSidebar (intent : Intent) panel sidebarPointed hasContent (field : Field) (cursor : FieldCursor) world =
        let invoke = intent.Confirm || intent.Step = Some Rightward // right invokes as confirm does, though silently
        let cursor = toSidebar cursor
        let position = cursor.CursorPosition
        let closeRow = dec SidebarButtons.Length
        // follow the selection, however it was made
        let row = match field.Menu.MenuState with MenuCloseSelected _ -> closeRow | _ -> getSidebarRow panel
        let cursor = { cursor with CursorPosition = { position with SidebarRow = row } }
        let (cursor, opening) =
            match intent.Step with
            | Some (Upward | Downward as direction) ->
                let row' = Selection.wrap (Selection.rowDelta direction) SidebarButtons.Length row
                let (menu, message) = SidebarButtons[row']
                let cursor = { cursor with CursorPosition = { cursor.CursorPosition with SidebarRow = row' } }
                match menu with
                | None -> (cursor, [signal MenuCloseSelect])
                | Some menu when menu <> panel -> ({ cursor with CursorMenu = menu }, [signal message])
                | Some _ when row = closeRow -> (cursor, [signal MenuCloseDeselect])
                | Some _ -> (cursor, [])
            | _ -> (cursor, [])
        let (cursor, signals) =
            if invoke then
                let (menuOpt, message) = SidebarButtons[cursor.CursorPosition.SidebarRow]
                match menuOpt with
                | None -> (cursor, ((if intent.Confirm then Selection.playAffirm world); opening @ [signal MenuClose]))
                // a menu opened this frame, so its content's unknown yet
                | Some menu when menu = cursor.CursorMenu && not opening.IsEmpty -> (cursor, opening)
                | Some menu when menu = panel -> ((if hasContent then moveTo 0 0 cursor else cursor), ((if intent.Confirm then Selection.playAffirm world); opening))
                | Some menu -> (enter menu, ((if intent.Confirm then Selection.playAffirm world); [signal message]))
            elif intent.Cancel || intent.Menu then (cursor, (Selection.playAffirm world; opening @ [signal MenuClose]))
            else (cursor, opening)
        let pointedOpt = if cursor.CursorPosition.InSidebar then Some (sidebarPointed cursor.CursorPosition.SidebarRow) else None
        (cursor, signals, pointedOpt)

    /// The team menu, in three levels - the sidebar, then the teammates with the auto map beneath them, then the
    /// selected teammate's weapon, armor and accessory. The teammate pointed at is the one selected, confirming goes
    /// down a level, and cancel comes back up one.
    let private updateTeam (intent : Intent) (menuTeam : MenuTeam) (field : Field) (cursor : FieldCursor) world =
        let position = cursor.CursorPosition
        let teamCount = field.Team.Count
        if position.InSidebar then updateSidebar intent TeamPanel TeamSidebarPointed true field cursor world
        elif position.Column = 0 then
            // the teammates and the auto map where the field has one, the pointer following the selection, however it
            // was made, right invoking as confirm does and left backing out as cancel does, though silently
            let invoke = intent.Confirm || intent.Step = Some Rightward
            let rowCount = if hasAutoMap field then inc teamCount else teamCount
            let selected = if menuTeam.TeamAutoMapSelected then teamCount else menuTeam.TeamIndex
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) rowCount selected | _ -> selected
            let selecting =
                if row = selected then []
                elif row = teamCount then [signal (MenuTeamAutoMapSelect true)]
                elif row <> menuTeam.TeamIndex then [signal (MenuTeamAlly row)]
                else [signal (MenuTeamAutoMapSelect false)]
            // leaving the teammates leaves the map button unselected
            let deselecting = if menuTeam.TeamAutoMapSelected || row = teamCount then [signal (MenuTeamAutoMapSelect false)] else []
            let (cursor, signals) =
                if invoke && row < teamCount then
                    match MenuTeam.tryGetTeammate field.Team menuTeam with
                    | Some _ -> (moveTo 1 0 cursor, ((if intent.Confirm then Selection.playAffirm world); selecting))
                    | None -> (moveTo 0 row cursor, selecting)
                elif invoke then
                    if intent.Confirm then World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.AutoMapSound world
                    (moveTo 0 row cursor, [signal MenuAutoMapOpen])
                elif intent.Cancel || intent.Step = Some Leftward then (toSidebar cursor, ((if intent.Cancel then Selection.playAffirm world); deselecting))
                elif intent.Menu then (moveTo 0 row cursor, selecting @ (Selection.playAffirm world; [signal MenuClose]))
                else (moveTo 0 row cursor, selecting)
            let pointedOpt =
                if cursor.CursorPosition.InSidebar then Some (TeamSidebarPointed cursor.CursorPosition.SidebarRow)
                elif cursor.CursorPosition.Column = 1 then Some (EquipmentPointed 0)
                elif row < teamCount then Some (TeamTeammatePointed (Seq.item row field.Team.Values).TeamIndex)
                else Some AutoMapPointed
            (cursor, signals, pointedOpt)
        else
            // the selected teammate's equipment, top to bottom, left backing out as cancel does, though silently
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
                        (moveTo 1 row cursor, (Selection.playAffirm world; [signal message]))
                    elif intent.Cancel || intent.Step = Some Leftward then (moveTo 0 menuTeam.TeamIndex cursor, ((if intent.Cancel then Selection.playAffirm world); []))
                    elif intent.Menu then (moveTo 1 row cursor, (Selection.playAffirm world; [signal MenuClose]))
                    else (moveTo 1 row cursor, [])
                let pointedOpt =
                    if cursor.CursorPosition.Column = 0 then Some (TeamTeammatePointed (Seq.item menuTeam.TeamIndex field.Team.Values).TeamIndex)
                    else Some (EquipmentPointed row)
                (cursor, signals, pointedOpt)
            | None -> (moveTo 0 0 cursor, [], None)

    /// The techs menu, in three levels - the sidebar, then the teammates, then the selected teammate's techs. The
    /// teammate pointed at is the one selected, confirming goes down a level, and cancel comes back up one.
    let private updateTechs (intent : Intent) (menuTechs : MenuTechs) (field : Field) (cursor : FieldCursor) world =
        let position = cursor.CursorPosition
        let teamCount = field.Team.Count
        let techs = match Map.tryFind menuTechs.TeamIndex field.Team with Some teammate -> Array.ofSeq teammate.Techs | None -> [||]
        if position.InSidebar then updateSidebar intent TechsPanel TechsSidebarPointed true field cursor world
        elif position.Column = 0 || techs.Length = 0 then
            // the teammates, the pointer following a teammate selected otherwise, as by the mouse, right invoking as
            // confirm does and left backing out as cancel does, though silently
            let invoke = intent.Confirm || intent.Step = Some Rightward
            let row = menuTechs.TeamIndex
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) teamCount row | _ -> row
            let selecting = if row <> menuTechs.TeamIndex then [signal (MenuTechsAlly row)] else []
            let (cursor, signals) =
                if invoke && techs.Length > 0 then (moveTo 1 0 cursor, ((if intent.Confirm then Selection.playAffirm world); selecting))
                elif invoke then (moveTo 0 row cursor, ((if intent.Confirm then Selection.playMistake world); selecting))
                elif intent.Cancel || intent.Step = Some Leftward then (toSidebar cursor, ((if intent.Cancel then Selection.playAffirm world); selecting))
                elif intent.Menu then (moveTo 0 row cursor, selecting @ (Selection.playAffirm world; [signal MenuClose]))
                else (moveTo 0 row cursor, selecting)
            let pointedOpt =
                if cursor.CursorPosition.InSidebar then Some (TechsSidebarPointed cursor.CursorPosition.SidebarRow)
                elif cursor.CursorPosition.Column = 1 then Some (TechPointed techs[0])
                else Some (TechsTeammatePointed (Seq.item row field.Team.Values).TeamIndex)
            (cursor, signals, pointedOpt)
        else
            // the selected teammate's techs, top to bottom, left backing out as cancel does, though silently
            let row = min position.Row (dec techs.Length)
            let row = match intent.Step with Some (Upward | Downward as direction) -> Selection.wrap (Selection.rowDelta direction) techs.Length row | _ -> row
            let (cursor, signals) =
                if intent.Confirm then (moveTo 1 row cursor, (Selection.playAffirm world; [signal (MenuTechsSelect row)]))
                elif intent.Cancel || intent.Step = Some Leftward then (moveTo 0 menuTechs.TeamIndex cursor, ((if intent.Cancel then Selection.playAffirm world); []))
                elif intent.Menu then (moveTo 1 row cursor, (Selection.playAffirm world; [signal MenuClose]))
                else (moveTo 1 row cursor, [])
            let pointedOpt =
                if cursor.CursorPosition.Column = 0 then Some (TechsTeammatePointed (Seq.item menuTechs.TeamIndex field.Team.Values).TeamIndex)
                else Some (TechPointed techs[row])
            (cursor, signals, pointedOpt)

    /// The inventory and key items menus, which lay out a page of ten items alike - two columns of five, left of
    /// whose first is the sidebar. Right past the second column turns to the next page and left past the first to the
    /// previous, as do the page buttons, left past the first going to the sidebar from the first page.
    let private updateItemPage (intent : Intent) panel sidebarPointed itemPointed enabled (select : _ -> Signal) pageUpMessage pageDownMessage (field : Field) (cursor : FieldCursor) world =
        let (pageUp, pageDown, items) = pageItems 10 field
        let count = items.Count
        let position = cursor.CursorPosition
        if position.InSidebar || count = 0 then updateSidebar intent panel sidebarPointed (count > 0) field cursor world
        else
            let paging = (if intent.PageUp && pageUp then [pageUpMessage] else []) @ (if intent.PageDown && pageDown then [pageDownMessage] else [])
            let index = min position.Row (dec count)
            let column = index / 5
            let row = index % 5
            let columnCount column = max 0 (min 5 (count - column * 5))
            let (indexOpt, stepping) =
                match intent.Step with
                | Some (Upward | Downward as direction) -> (Some (column * 5 + Selection.wrap (Selection.rowDelta direction) (columnCount column) row), [])
                | Some Leftward when column = 1 -> (Some (index - 5), [])
                | Some Leftward when pageUp -> (Some index, [pageUpMessage])
                | Some Leftward -> (None, [])
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
                        if enabled itemType then (Selection.playAffirm world; [signal (select page)]) else (Selection.playMistake world; [])
                     elif intent.Menu then (Selection.playAffirm world; [signal MenuClose])
                     else [])
                let cursor = if intent.Cancel then (Selection.playAffirm world; toSidebar cursor) else moveTo 0 index cursor
                let pointedOpt = if cursor.CursorPosition.InSidebar then Some (sidebarPointed cursor.CursorPosition.SidebarRow) else Some (itemPointed itemType)
                (cursor, signals, pointedOpt)
            | None ->
                let cursor = toSidebar cursor
                (cursor, paging, Some (sidebarPointed cursor.CursorPosition.SidebarRow))

    /// The options menu - each button pressed by confirming it. Coming into the menu lands on its top left button,
    /// the cursor being placed in column 1 once it's in.
    let private updateOptions (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        if cursor.CursorPosition.InSidebar then updateSidebar intent OptionsPanel OptionsSidebarPointed true field cursor world
        else
            let index = if cursor.CursorPosition.Column = 0 then 0 else cursor.CursorPosition.Row
            // where each button goes for each direction, as they're laid out - the battle speeds wait, paced and swift
            // across the top, the song volume's down and up beneath, then full screen, play mode and quit down the
            // middle, -1 being the sidebar, and moving up or down into a row landing on its leftmost button
            let index =
                match intent.Step with
                | Some direction ->
                    match (index, direction) with
                    | (0, Upward) -> 7 | (0, Downward) -> 3 | (0, Leftward) -> -1 | (0, Rightward) -> 1
                    | (1, Upward) -> 7 | (1, Downward) -> 3 | (1, Leftward) -> 0 | (1, Rightward) -> 2
                    | (2, Upward) -> 7 | (2, Downward) -> 3 | (2, Leftward) -> 1 | (2, Rightward) -> 2
                    | (3, Upward) -> 0 | (3, Downward) -> 5 | (3, Leftward) -> -1 | (3, Rightward) -> 4
                    | (4, Upward) -> 0 | (4, Downward) -> 5 | (4, Leftward) -> 3 | (4, Rightward) -> 4
                    | (5, Upward) -> 3 | (5, Downward) -> 6 | (5, Leftward) -> -1 | (5, Rightward) -> 5
                    | (6, Upward) -> 5 | (6, Downward) -> 7 | (6, Leftward) -> -1 | (6, Rightward) -> 6
                    | (_, Upward) -> 6 | (_, Downward) -> 0 | (_, Leftward) -> -1 | (_, Rightward) -> 7
                | None -> index
            let (cursor, signals) =
                if index < 0 then (toSidebar cursor, [])
                elif intent.Cancel then (toSidebar cursor, (Selection.playAffirm world; []))
                elif intent.Confirm then
                    let message =
                        match index with
                        | 0 -> signal (MenuOptionsSelectBattleSpeed WaitSpeed)
                        | 1 -> signal (MenuOptionsSelectBattleSpeed PacedSpeed)
                        | 2 -> signal (MenuOptionsSelectBattleSpeed SwiftSpeed)
                        | 3 -> signal MenuOptionsSongVolumeDown
                        | 4 -> signal MenuOptionsSongVolumeUp
                        | 5 -> signal MenuOptionsToggleFullScreen
                        | 6 -> signal MenuOptionsHardcoreModeToggle
                        | _ -> signal MenuOptionsQuitPrompt
                    (moveTo 1 index cursor, (Selection.playAffirm world; [message]))
                elif intent.Menu then (moveTo 1 index cursor, (Selection.playAffirm world; [signal MenuClose]))
                else (moveTo 1 index cursor, [])
            let pointedOpt = if cursor.CursorPosition.InSidebar then Some (OptionsSidebarPointed cursor.CursorPosition.SidebarRow) else Some (OptionPointed index)
            (cursor, signals, pointedOpt)

    /// A panel with one thing to do - confirm or cancel both close it.
    let private updateClose (intent : Intent) closePointed message (cursor : FieldCursor) world =
        let signals = if intent.Confirm || intent.Cancel || intent.Menu then (Selection.playAffirm world; [signal message]) else []
        (cursor, signals, Some closePointed)

    /// Which of two buttons side by side a step lands on - left the first, right the second.
    let private stepPair (intent : Intent) column =
        match intent.Step with
        | Some Leftward -> 0
        | Some Rightward -> 1
        | _ -> column

    /// Using a consumable - the teammates in columns of three, up and down cycling through each, confirm using it on
    /// the one pointed at, and the close button above and right of them, reached right from the last column.
    let private updateUse (intent : Intent) itemType (field : Field) (cursor : FieldCursor) world =
        let count = field.Team.Count
        let lastColumn = dec count / 3
        let columnCount column = max 0 (min 3 (count - column * 3))
        if cursor.CursorPosition.Column = 1 then

            // the close button, left going back to the top of the last column
            let cursor =
                match intent.Step with
                | Some Leftward -> moveTo 0 (lastColumn * 3) cursor
                | _ -> cursor
            let signals =
                if cursor.CursorPosition.Column = 1 && (intent.Confirm || intent.Cancel)
                then (Selection.playAffirm world; [signal MenuInventoryCancel])
                else []
            let pointedOpt =
                if cursor.CursorPosition.Column = 1 then Some UseClosePointed
                else Some (UseTeammatePointed (Seq.item cursor.CursorPosition.Row field.Team.Values).TeamIndex)
            (cursor, signals, pointedOpt)
        else
            let index = min cursor.CursorPosition.Row (dec count)
            let column = index / 3
            let row = index % 3
            let indexOpt =
                match intent.Step with
                | Some (Upward | Downward as direction) -> Some (column * 3 + Selection.wrap (Selection.rowDelta direction) (columnCount column) row)
                | Some Rightward when column = lastColumn -> None
                | Some (Leftward | Rightward as direction) ->
                    let column' = column + Selection.columnDelta direction
                    Some (if column' >= 0 && columnCount column' > 0 then column' * 3 + min row (dec (columnCount column')) else index)
                | None -> Some index
            match indexOpt with
            | Some index ->
                let teammate = Seq.item index field.Team.Values
                let signals =
                    if intent.Confirm then
                        if Teammate.canUseItem itemType teammate
                        then (Selection.playAffirm world; [signal (MenuInventoryUse index)])
                        else Selection.playMistake world; []
                    elif intent.Cancel then (Selection.playAffirm world; [signal MenuInventoryCancel])
                    else []
                (moveTo 0 index cursor, signals, Some (UseTeammatePointed (Seq.item index field.Team.Values).TeamIndex))
            | None ->
                let signals = if intent.Cancel then (Selection.playAffirm world; [signal MenuInventoryCancel]) else []
                (moveTo 1 0 cursor, signals, Some UseClosePointed)

    /// The equip panel - a page of items down the middle, the close button right of the first, and on the left the
    /// current equipment's info or, once an item's chosen, the equip button. Choosing an item moves to the equip
    /// button; choosing that equips it. Once an item's chosen, the close button shows as a back button, and backing
    /// out unchooses the item, returning to it, rather than closing the panel.
    let private updateEquip (intent : Intent) (menuTeam : MenuTeam) (equip : Equip) (field : Field) (cursor : FieldCursor) world =
        let teammate = field.Team[menuTeam.TeamIndex]
        let (changing, currentName) =
            match equip.EquipType with
            | EquipWeapon weaponTypeOpt -> (teammate.WeaponOpt <> weaponTypeOpt, teammate.WeaponOpt |> Option.map _.Name |> Option.defaultValue "None")
            | EquipArmor armorTypeOpt -> (teammate.ArmorOpt <> armorTypeOpt, teammate.ArmorOpt |> Option.map _.Name |> Option.defaultValue "None")
            | EquipAccessory accessoryTypeOpt -> (teammate.Accessories <> Option.toList accessoryTypeOpt, teammate.Accessories |> List.map _.Name |> Seq.headOrDefault "None")
        let leftOpt = if changing then Some EquipButtonPointed elif currentName <> "None" then Some EquipInfoPointed else None
        let (pageUp, pageDown, items) = pageItems 3 field
        let count = items.Count
        let paging = (if intent.PageUp && pageUp then [MenuTeamEquipPageUp] else []) @ (if intent.PageDown && pageDown then [MenuTeamEquipPageDown] else [])
        let paging = List.map (fun message -> message :> Signal) paging
        let selectedRow =
            let selectedOpt =
                match equip.EquipType with
                | EquipWeapon weaponTypeOpt -> Option.map (WeaponType >> Equipment) weaponTypeOpt
                | EquipArmor armorTypeOpt -> Option.map (ArmorType >> Equipment) armorTypeOpt
                | EquipAccessory accessoryTypeOpt -> Option.map (AccessoryType >> Equipment) accessoryTypeOpt
            items |> Map.tryFindKey (fun _ (_, (itemType, _)) -> Some itemType = selectedOpt) |> Option.defaultValue 0
        let backOut cursor =
            Selection.playAffirm world
            if changing
            then ((if count > 0 then moveTo 1 selectedRow cursor else cursor), [signal MenuTeamEquipDeselect])
            else (cursor, [signal MenuTeamEquipCancel])
        let onClose = cursor.CursorPosition.Column = 2 || (count = 0 && leftOpt.IsNone)
        let onLeft = not onClose && (cursor.CursorPosition.Column = 0 || count = 0) && leftOpt.IsSome
        if onClose then
            // the close button, top right, right of the first item, left going back to it
            let (cursor, signals) =
                match intent.Step with
                | Some Leftward when count > 0 -> (moveTo 1 0 cursor, [])
                | Some Leftward when leftOpt.IsSome -> (moveTo 0 0 cursor, [])
                | _ ->
                    let cursor = moveTo 2 0 cursor
                    if intent.Confirm || intent.Cancel then backOut cursor else (cursor, [])
            let pointedOpt =
                match (cursor.CursorPosition.Column, leftOpt) with
                | (1, _) -> Some (EquipItemPointed (fst (snd items[min cursor.CursorPosition.Row (dec count)])))
                | (0, Some left) -> Some left
                | (_, _) -> Some EquipCancelPointed
            (cursor, paging @ signals, pointedOpt)
        elif onLeft then
            let left = leftOpt.Value
            let (cursor, signals) =
                match intent.Step with
                | Some Rightward when count > 0 -> (moveTo 1 0 cursor, [])
                | Some Rightward -> (moveTo 2 0 cursor, [])
                | _ ->
                    let cursor = moveTo 0 0 cursor
                    if intent.Confirm then
                        // equipping goes back to the items, where the panel starts
                        if left = EquipButtonPointed
                        then (moveTo 1 0 cursor, (World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.EquipSound world; [signal MenuTeamEquipConfirm]))
                        else (cursor, (Selection.playAffirm world; [signal MenuTeamEquipMenuUseOpen]))
                    elif intent.Cancel then backOut cursor
                    else (cursor, [])
            let pointedOpt =
                match cursor.CursorPosition.Column with
                | 1 when count > 0 -> Some (EquipItemPointed (fst (snd items[min cursor.CursorPosition.Row (dec count)])))
                | 2 -> Some EquipCancelPointed
                | _ -> Some left
            (cursor, paging @ signals, pointedOpt)
        elif count > 0 then
            let index = min cursor.CursorPosition.Row (dec count)
            let (index, column, stepping) =
                match intent.Step with
                | Some (Upward | Downward as direction) -> (Selection.wrap (Selection.rowDelta direction) count index, 1, [])
                | Some Leftward when leftOpt.IsSome -> (index, 0, [])
                | Some Leftward when pageUp -> (index, 1, [MenuTeamEquipPageUp :> Signal])
                // right from the first item goes to the close button, from the others to the next page if any
                | Some Rightward when index > 0 && pageDown -> (index, 1, [MenuTeamEquipPageDown :> Signal])
                | Some Rightward -> (0, 2, [])
                | _ -> (index, 1, [])
            let (cursor, signals) =
                // choosing an item moves to the equip button
                if column = 1 && intent.Confirm then (moveTo 0 index cursor, (Selection.playAffirm world; [signal (MenuTeamEquipSelect items[index])]))
                elif intent.Cancel then backOut (moveTo column index cursor)
                else (moveTo column index cursor, [])
            let (column, page) = (cursor.CursorPosition.Column, items[min cursor.CursorPosition.Row (dec count)])
            let pointedOpt =
                match (column, leftOpt) with
                | (0, Some left) when not intent.Confirm -> Some left
                | (2, _) -> Some EquipCancelPointed
                | (_, _) -> Some (EquipItemPointed (fst (snd page)))
            (cursor, paging @ stepping @ signals, pointedOpt)
        else (cursor, paging, None)

    /// The party menu - the team to choose from on the left, those chosen in the middle, and the confirm button on the
    /// right, which the cursor moves to once three are chosen, else to the next teammate yet to be chosen. Cancel takes
    /// back the last one chosen.
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
                        let (column, row) =
                            if selections.Length = 2 then (2, 0)
                            else
                                let chosen = teamIndex :: selections
                                [1 .. dec teamIndices.Length]
                                |> List.map (fun offset -> (row + offset) % teamIndices.Length)
                                |> List.tryFind (fun row' -> not (List.contains teamIndices[row'] chosen))
                                |> Option.map (fun row' -> (column, row'))
                                |> Option.defaultValue (column, row)
                        (column, row, (Selection.playAffirm world; [signal (PartyMenuSelect teamIndex)]))
                    else (column, row, (Selection.playMistake world; []))
                | 1 -> (column, row, (Selection.playAffirm world; [signal (PartyMenuDeselect selections[row])]))
                | _ ->
                    if selections.Length >= 3
                    then (column, row, (Selection.playAffirm world; [signal PartyMenuClose]))
                    else (column, row, (Selection.playMistake world; []))
            elif intent.Cancel then
                match List.tryLast selections with
                | Some teamIndex -> (column, row, (Selection.playAffirm world; [signal (PartyMenuDeselect teamIndex)]))
                | None -> (column, row, [])
            else (column, row, [])
        let pointedOpt =
            match column with
            | 0 -> Some (PartyTeammatePointed teamIndices[row])
            | 1 when row < selections.Length -> Some (PartySelectedPointed selections[row])
            | 1 -> None
            | _ -> Some PartyConfirmPointed
        (moveTo column row cursor, signals, pointedOpt)

    /// The shop - Buy or Sell and Leave along the top, the wares beneath in two columns, turning the page left and
    /// right past them as well as by the page buttons. Cancel leaves.
    let private updateShop (intent : Intent) (shop : Shop) (field : Field) (cursor : FieldCursor) world =
        let (pageSize, rows) = match (shop.ShopType, shop.ShopState) with (Chemist, ShopBuying) -> (6, 3) | _ -> (8, 4)
        let (pageUp, pageDown, items) = pageItems pageSize field
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
                | Some Upward when itemRow = 0 -> (0, -1, []) // up to the top row's leftmost button, buy or sell
                | Some Upward -> (0, dec row, [])
                | Some Downward when itemRow = dec (columnCount itemColumn) -> (itemColumn, -1, [])
                | Some Downward -> (0, inc row, [])
                | Some Leftward when itemColumn = 1 -> (0, row - rows, [])
                | Some Leftward when pageUp -> (0, row, [ShopPageUp :> Signal])
                | Some Rightward when itemColumn = 0 && columnCount 1 > 0 -> (0, rows + min itemRow (dec (columnCount 1)), [])
                | Some Rightward when pageDown -> (0, row, [ShopPageDown :> Signal])
                | _ -> (0, row, [])
        let signals =
            if intent.Confirm then
                if row >= 0 then (Selection.playAffirm world; [signal (ShopSelect items[row])])
                elif column = 1 then (Selection.playAffirm world; [signal ShopLeave])
                else (Selection.playAffirm world; [signal (match shop.ShopState with ShopBuying -> ShopSell | ShopSelling -> ShopBuy)])
            elif intent.Cancel then (Selection.playAffirm world; [signal ShopLeave])
            else []
        let pointedOpt =
            if row >= 0 then Some (ShopItemPointed (fst (snd items[row])))
            else Some (if column = 1 then ShopLeavePointed else ShopTradePointed)
        (moveTo column row cursor, paging @ stepping @ signals, pointedOpt)

    /// Exploring - confirm interacts, the menu button opens the menu, and the party button the party menu at a save
    /// point. Walking reads the intent itself (see ProcessKeyInput).
    let private updateExploring (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        let signals =
            if intent.Confirm && canInteract field then [signal Interact]
            elif intent.Menu && canOpenMenu field then (Selection.playAffirm world; [signal MenuTeamOpen])
            elif intent.Party && canOpenParty field then (Selection.playAffirm world; [signal PartyMenuOpen])
            else []
        (cursor, signals, None)

    /// The dialog - confirm or cancel presses the skip button where it shows, and otherwise confirm turns the page.
    let private updateDialog (intent : Intent) (field : Field) (cursor : FieldCursor) world =
        let signals =
            if (intent.Confirm || intent.Cancel) && canSkip field then (Selection.playAffirm world; [signal SkipDialog])
            elif intent.Confirm && canInteract field then [signal Interact]
            else []
        (cursor, signals, None)

    /// Advance the cursor and act on what the player means this frame, returning the cursor, the signals for the
    /// field, and what the pointer should point at.
    let updateInput (intent : Intent) (field : Field) (cursor : FieldCursor) world : FieldCursor * Signal list * FieldPointed option =
        let cursor = followInput field cursor
        match cursor.CursorMenu with
        | NoFieldMenu -> updateExploring intent field cursor world
        | DialogPanel -> updateDialog intent field cursor world
        | PromptPanel ->
            // the dialog's two answers, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals = if intent.Confirm then (Selection.playAffirm world; [signal (if column = 0 then PromptLeft else PromptRight)]) else []
            (moveTo column 0 cursor, signals, Some (PromptChoicePointed column))
        | TeamPanel ->
            match field.Menu.MenuState.Shown with
            | MenuTeam menuTeam -> updateTeam intent menuTeam field cursor world
            | _ -> (cursor, [], None)
        | EquipPanel ->
            match field.Menu.MenuState with
            | MenuTeam ({ TeamEquipOpt = Some equip } as menuTeam) when field.Team.ContainsKey menuTeam.TeamIndex ->
                updateEquip intent menuTeam equip field cursor world
            | _ -> (cursor, [], None)
        | EquipInfoPanel -> updateClose intent InfoClosePointed MenuTeamEquipMenuUseClose cursor world
        | AutoMapPanel ->
            // back to the team menu or closing the menu, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.AutoMapSound world; [signal (if column = 0 then MenuTeamOpen else MenuClose)]
                elif intent.Cancel then World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.AutoMapSound world; [signal MenuTeamOpen]
                elif intent.Menu then (Selection.playAffirm world; [signal MenuClose])
                else []
            (moveTo column 0 cursor, signals, Some (AutoMapChoicePointed column))
        | InventoryPanel ->
            updateItemPage intent InventoryPanel InventorySidebarPointed InventoryItemPointed (function Stash _ -> false | _ -> true) (fun page -> MenuInventorySelect page)
                MenuInventoryPageUp MenuInventoryPageDown field cursor world
        | KeyItemsPanel ->
            updateItemPage intent KeyItemsPanel KeyItemsSidebarPointed KeyItemPointed (constant true) (fun page -> MenuKeyItemsSelect page)
                MenuKeyItemsPageUp MenuKeyItemsPageDown field cursor world
        | UsePanel ->
            match field.Menu.MenuUseOpt with
            | Some ({ MenuUseSelection = (_, (Consumable _ as itemType)) }) -> updateUse intent itemType field cursor world
            | Some _ -> updateClose intent UseClosePointed MenuInventoryCancel cursor world
            | None -> (cursor, [], None)
        | TechsPanel ->
            match field.Menu.MenuState.Shown with
            | MenuTechs menuTechs -> updateTechs intent menuTechs field cursor world
            | _ -> (cursor, [], None)
        | TechPanel -> updateClose intent TechClosePointed MenuTechClose cursor world
        | OptionsPanel -> updateOptions intent field cursor world
        | QuitPanel ->
            // quitting or not, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then (Selection.playAffirm world; [signal (if column = 0 then MenuOptionsQuitConfirm else MenuOptionsQuitCancel)])
                elif intent.Cancel then (Selection.playAffirm world; [signal MenuOptionsQuitCancel])
                else []
            (moveTo column 0 cursor, signals, Some (QuitChoicePointed column))
        | PartyPanel -> updateParty intent field cursor world
        | ShopPanel ->
            match field.ShopOpt with
            | Some shop -> updateShop intent shop field cursor world
            | None -> (cursor, [], None)
        | ShopConfirmPanel ->
            // buying or selling or not, side by side
            let column = stepPair intent cursor.CursorPosition.Column
            let signals =
                if intent.Confirm then (Selection.playAffirm world; [signal (if column = 0 then ShopConfirmAccept else ShopConfirmDecline)])
                elif intent.Cancel then (Selection.playAffirm world; [signal ShopConfirmDecline])
                else []
            (moveTo column 0 cursor, signals, Some (ShopConfirmChoicePointed column))

#if DEV
    let reloadProps field =
        FieldData.clearMemoized ()
        let props = makeProps field.FieldTime_ field.FieldType_ field.OmniSeedState_
        { field with Props_ = props }
#endif

    (* High-Level Operations (signal-producing) *)

    let hardcoreModeToggle field =
        { field with Options_ = { field.Options with HardcoreMode = not field.Options_.HardcoreMode }}

    let quitPrompt field =
        match field.Menu_.MenuState with
        | MenuOptions false -> { field with Menu_ = { field.Menu_ with MenuState = MenuOptions true }}
        | _ -> field

    let quitCancel field =
        match field.Menu_.MenuState with
        | MenuOptions true -> { field with Menu_ = { field.Menu_ with MenuState = MenuOptions false }}
        | _ -> field

    let private interactDialog dialog field =
        match Dialog.tryAdvance (detokenize field) dialog with
        | (true, dialog) ->
            let field = setDialogOpt (Some dialog) field
            just field
        | (false, dialog) ->
            let field = setDialogOpt None field
            match dialog.DialogBattleOpt with
            | Some (battleType, consequence) -> withSignal (TryCommencingBattle (battleType, consequence)) field
            | None -> just field

    let private interactChest itemType chestId battleTypeOpt cue requirements (prop : Prop) (field : Field) world =
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
            let field = mapAdvents (Set.add (Opened chestId)) field
            let field = mapInventory (Inventory.tryAddItem field.Options.HardcoreMode itemType >> snd) field
            let field =
                match battleTypeOpt with
                | Some battleType -> setDialogOpt (Some (Dialog.makePlus DialogThin ("Found " + itemType.Name + "!^But something approaches!") None (Some (battleType, Set.empty)) false)) field
                | None -> setDialogOpt (Some (Dialog.make DialogThin ("Found " + itemType.Name + "!") false)) field
            let field = setCue cue field
            World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.ChestOpenSound world |> ignore
            just field
        else
            let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
            let field = setDialogOpt (Some (Dialog.make DialogThin "Locked!" false)) field
            World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.ChestLockedSound world |> ignore
            just field

    let private interactDoor keyItemTypeOpt cue requirements (prop : Prop) (field : Field) world =
        match prop.PropState with
        | DoorState false ->
            if  field.Advents_.IsSupersetOf requirements &&
                Option.mapOrDefaultValue (fun keyItemType -> Map.containsKey (KeyItem keyItemType) field.Inventory_.Items) true keyItemTypeOpt then
                let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
                let field = setCue cue field
                let field = mapPropState (constant (DoorState true)) prop.PropId field
                World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.DoorOpenSound world |> ignore
                just field
            else
                let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
                let field = setDialogOpt (Some (Dialog.make DialogThin "Locked!" false)) field
                World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.DoorLockedSound world |> ignore
                just field
        | _ -> failwithumf ()

    let private interactSwitch cue cue2 onRequirements requirements (prop : Prop) (field : Field) world =
        let on = field.Advents_.IsSupersetOf onRequirements
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
            let field = setCue (if on then cue2 else cue) field
            World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.SwitchUseSound world |> ignore
            just field
        else
            let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
            let field = setDialogOpt (Some (Dialog.make DialogThin "Won't budge!" false)) field
            World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.SwitchStuckSound world |> ignore
            just field

    let private interactCharacter cue (prop : Prop) (field : Field) world =
        let field = mapAvatar (Avatar.lookAt prop.Perimeter.BottomOffset5) field
        let field = setCue cue field
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.AffirmSound world |> ignore
        just field

    let private interactNpc branches requirements (prop : Prop) (field : Field) world =
        if field.Advents_.IsSupersetOf requirements then
            let field = mapAvatar (Avatar.lookAt prop.Perimeter.BottomOffset5) field
            let branchesFiltered = branches |> List.choose (fun (branch : CueSystem.CueBranch) -> if field.Advents_.IsSupersetOf branch.Requirements then Some branch.Cue else None) |> List.rev
            let branchCue = match List.tryHead branchesFiltered with Some cue -> cue | None -> CueSystem.Dialog ("...", false, false)
            let field = setCue branchCue field
            World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.AffirmSound world |> ignore
            just field
        else just field

    let private interactShopkeep shopType (prop : Prop) (field : Field) world =
        let field = mapAvatar (Avatar.lookAt prop.Perimeter.BottomOffset5) field
        let shop = { ShopType = shopType; ShopState = ShopBuying; ShopPage = 0; ShopConfirmOpt = None }
        let field = mapShopOpt (constant (Some shop)) field
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.AffirmSound world |> ignore
        just field

    let private interactSeal cue (prop : Prop) (field : Field) world =
        let field = mapAvatar (Avatar.lookAt prop.Perimeter.Center) field
        let field = setCue cue field
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.SealedSound world |> ignore
        just field

    let private interactSavePoint (field : Field) world =
        let field = restoreTeam field
        save field
        let field = setDialogOpt (Some (Dialog.make DialogThin "Recovered strength and saved game." false)) field
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.SlotSound world |> ignore
        just field

    let interact (field : Field) world =
        match field.DialogOpt with
        | None ->
            if touchingSavePoint field then
                interactSavePoint field world
            else
                match tryGetFacingProp field with
                | Some prop ->
                    match prop.PropData with
                    | Sprite _ -> just field
                    | Portal _ -> just field
                    | Door (_, keyItemTypeOpt, cue, _, requirements) -> interactDoor keyItemTypeOpt cue requirements prop field world
                    | Chest (_, itemType, chestId, battleTypeOpt, cue, requirements) -> interactChest itemType chestId battleTypeOpt cue requirements prop field world
                    | Switch (_, cue, cue2, onRequirements, requirements) -> interactSwitch cue cue2 onRequirements requirements prop field world
                    | Sensor _ -> just field
                    | Character (_, _, _, _, cue, _) -> interactCharacter cue prop field world
                    | Npc (_, _, cue, requirements) -> interactNpc [{ CueSystem.Cue = cue; CueSystem.Requirements = Set.empty }] requirements prop field world
                    | NpcBranching (_, _, branches, requirements) -> interactNpc branches requirements prop field world
                    | Shopkeep (_, _, shopType, _) -> interactShopkeep shopType prop field world
                    | Seal (_, cue, _) -> interactSeal cue prop field world
                    | Flame _ -> just field
                    | SavePoint -> just field
                    | ChestSpawn -> just field
                    | PortalSpawn -> just field
                    | EmptyProp -> just field
                | None -> just field
        | Some dialog ->
            interactDialog dialog field

    let rec private updateCue (cue : Cue) (definitions : CueDefinitions) (field : Field) world :
        Cue * CueDefinitions * (Signal list * Field) =

        match cue with
        | Fin ->
            (cue, definitions, just field)

        | PlaySound (volume, sound) ->
            World.playSound 0.0f 0.0f volume sound world
            (Fin, definitions, just field)

        | PlaySong (fadeIn, fadeOut, start, volume, song) ->
            // TODO: update this cue and all data to include repeat limit opt.
            World.playSong fadeIn fadeOut start None volume song world
            (Fin, definitions, just field)

        | FadeOutSong fade ->
            World.fadeOutSong fade world
            (Fin, definitions, just field)

        | Face (target, direction) ->
            match target with
            | AvatarTarget ->
                let field = mapAvatar (Avatar.setDirection direction) field
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
                World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.PurchaseSound world |> ignore
                (Fin, definitions, just field)
            else
                updateCue
                    (Parallel
                        [Cue.Dialog ("You don't have enough...", false, false)
                         PlaySound (Constants.Audio.SoundVolumeDefault, Assets.Gui.MistakeSound)]) definitions field
                    world

        | AddItem itemType ->
            (Fin, definitions, just (mapInventory (Inventory.tryAddItem field.Options.HardcoreMode itemType >> snd) field))

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
            let field = setTint tint field
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
                let cue = MoveState (field.FieldTime_, target, field.Avatar_.Perimeter.Bottom, destination, moveType)
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
                    let bottom = field.Avatar_.Perimeter.Bottom + step
                    let field = mapAvatar (fun avatar -> { avatar with Perimeter = avatar.Perimeter.WithBottom bottom }) field
                    (cue, definitions, withSignal (WarpAvatar bottom) field)
                else
                    let bottom = origin + translation
                    let field = mapAvatar (fun avatar -> { avatar with Perimeter = avatar.Perimeter.WithBottom bottom }) field
                    (Fin, definitions, withSignal (WarpAvatar bottom) field)
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
                    let localTime = time - startTime
                    let (step, stepCount) = CueMovement.computeStepAndStepCount translation moveType
                    let finishTime = int64 (dec stepCount)
                    if localTime < finishTime then
                        let field = mapProp (fun prop -> { prop with Perimeter = prop.Perimeter.Translate step }) propId field
                        (cue, definitions, just field)
                    else
                        let field = mapProp (fun prop -> { prop with Perimeter = prop.Perimeter.WithBottom (origin + translation) }) propId field
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
                let field = setFieldTransitionOpt (Some fieldTransition) field
                (WarpState, definitions, just field)

        | WarpState ->
            match field.FieldTransitionOpt_ with
            | Some _ -> (cue, definitions, just field)
            | None -> (Fin, definitions, just field)

        | Dialog (text, isNarration, skippableWhenNotTombSealed) ->
            match field.DialogOpt_ with
            | Some _ ->
                (cue, definitions, just field)
            | None ->
                let dialogForm = if isNarration then DialogNarration else DialogThick
                let dialog = Dialog.make dialogForm text skippableWhenNotTombSealed
                let field = setDialogOpt (Some dialog) field
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
                let field = setDialogOpt (Some dialog) field
                (PromptState, definitions, just field)

        | PromptState ->
            match field.DialogOpt_ with
            | None -> (Fin, definitions, just field)
            | Some _ -> (cue, definitions, just field)

        | Battle (battleType, consequents) ->
            (BattleState, definitions, withSignal (TryCommencingBattle (battleType, consequents)) field)

        | BattleState ->
            match field.FieldState_ with
            | Playing -> (Fin, definitions, just field)
            | _ -> (cue, definitions, just field)

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
                Log.error ("Cue definition '" + name + "' already found.")
                (Fin, definitions, just field)

        | Assign (name, body) ->
            if Map.containsKey name definitions then
                (Fin, Map.add name body definitions, just field)
            else
                Log.error ("Cue definition '" + name + "' not found.")
                (Fin, definitions, just field)

        | Expand name ->
            match Map.tryFind name definitions with
            | Some body ->
                updateCue body definitions field world
            | None ->
                Log.error ("Cue definition '" + name + "' not found.")
                (Fin, definitions, ([], field))

        | Parallel cues ->
            let (cues, definitions, (signals, field)) =
                List.fold (fun (cues, definitions, (signals, field)) cue ->
                    let (cue, definitions, (signals2, field)) = updateCue cue definitions field world
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
                        let (cue, definitions, (signals2, field)) = updateCue cue definitions field world
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
                        Array.map (Spirit.update field.FieldTime_ field.Avatar_.Perimeter.Center) field.Spirits_ }
            let field =
                { field with
                    Spirits_ =
                        Array.filter (fun (spirit : Spirit) ->
                            let delta = field.Avatar_.Perimeter.Bottom - spirit.Perimeter.Center
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
                            match FieldData.tryGetSpiritType field.OmniSeedState_ field.Avatar_.Perimeter.Bottom fieldData with
                            | Some spiritType ->
                                let spiritMovement = SpiritPattern.toSpiritMovement spiritPattern
                                let spirit = Spirit.spawn field.FieldTime_ field.Avatar_.Perimeter.Bottom spiritType spiritMovement
                                Some spirit
                            | None -> None) |>
                        Array.definitize
                    | (false, _) -> [||]
                { field with Spirits_ = Array.append field.Spirits_ spiritsSpawned }
            match Array.tryFind (fun (spirit : Spirit) -> Vector3.Distance (field.Avatar_.Perimeter.LowerCenter, spirit.Perimeter.Bottom) < Constants.Field.SpiritCollisionRadius) field.Spirits_ with
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
                                | WeakSpirit -> encounterData.BattleTypes[Gen.random2 0 3]
                                | NormalSpirit -> encounterData.BattleTypes[Gen.random2 3 6]
                                | StrongSpirit -> encounterData.BattleTypes[Gen.random2 6 9]
                            match Data.Value.Battles.TryGetValue battleType with
                            | (true, battleData) -> Left (battleData, field)
                            | (false, _) -> Right field
                        | (false, _) -> Right field
                    | None -> Right field
                | (false, _) -> Right field
            | None -> Right field
        | Some _ -> Right field

    let update avatarIntersectedPropIds field world =

        // ensure we're playing
        match field.FieldState_ with
        | Playing ->

            // update avater intersected prop IDs
            let field =
                { field with AvatarIntersectedPropIds_ = avatarIntersectedPropIds }

            // update dialog
            let field =
                match field.DialogOpt_ with
                | Some dialog ->
                    let dialog = Dialog.update (detokenize field) field.FieldTime_ dialog
                    setDialogOpt (Some dialog) field
                | None -> field

            // update cue
            let (cue, definitions, (signals, field)) = updateCue field.Cue_ field.CueDefinitions_ field world

            // reset cue definitions if finished
            let field =
                match cue with
                | CueSystem.Fin -> setCueDefinitions field.CueDefinitionsOriginal_ field
                | _ -> setCueDefinitions definitions field
            let field = setCue cue field

            // update portal
            let field =
                match field.FieldTransitionOpt_ with
                | None ->
                    match tryGetTouchingPortal field with
                    | Some (fieldType, destination, direction, isWarp) ->
                        let transition =
                            { FieldType = fieldType
                              FieldDestination = destination
                              FieldDirection = direction
                              FieldTransitionTime = field.FieldTime_ + Constants.Field.TransitionTime }
                        let field = setFieldTransitionOpt (Some transition) field
                        if isWarp
                        then World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.StepWarpSound world
                        else World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.StepStairSound world
                        field
                    | None -> field
                | Some _ -> field

            // update sensor
            let field =
                match field.FieldTransitionOpt_ with
                | None ->
                    let sensors = getTouchedSensors field
                    List.fold (fun (field : Field) (sensorType, cue, requirements) ->
                        if field.Advents_.IsSupersetOf requirements then
                            let field = setCue cue field
                            match sensorType with
                            | AirSensor -> field
                            | HiddenSensor | StepPlateSensor ->
                                World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Field.StepPlateSound world |> ignore
                                field
                        else field)
                        field sensors
                | Some _ -> field

            // update auto maps
            let field =
                match Data.Value.Fields.TryGetValue field.FieldType_ with
                | (true, fieldData) ->
                    match FieldData.tryGetTileMap field.OmniSeedState_ fieldData with
                    | Some (Choice3Of4 (_, _, _)) ->
                        let index = (field.Avatar_.Perimeter.BottomOffset5.V2 / Constants.Field.RoomSize.V2 / Constants.Gameplay.TileSize.V2).V2i
                        let autoMap = match field.AutoMaps_.TryGetValue fieldData.FieldType with (true, autoMap) -> autoMap | (false, _) -> Set.empty
                        if  index.X >= 0 && index.X < Constants.Field.RandomMapSize.X &&
                            index.Y >= 0 && index.Y < Constants.Field.RandomMapSize.Y then
                            let autoMap = Set.add index autoMap
                            { field with AutoMaps_ = Map.add fieldData.FieldType autoMap field.AutoMaps_ }
                        else field
                    | Some _ | None -> field
                | (false, _) -> field

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
                        (signal (CommencingBattle battleData) :: signals, field)
                    | Right field -> (signals, field)
                else (signals, field)

            // fin
            (signals, field)

        // fin
        | _ -> just field

    let advanceTime field =
        let field = { field with FieldTime_ = inc field.FieldTime_ }
        just field

    let make time fieldType saveSlot randSeedState (avatar : Avatar) team advents inventory =
        let (spiritRate, debugAdvents, debugKeyItems, cueDefinitions) =
            match Data.Value.Fields.TryGetValue fieldType with
            | (true, fieldData) -> (fieldData.EncounterRate, fieldData.FieldDebugAdvents, fieldData.FieldDebugKeyItems, fieldData.Definitions)
            | (false, _) -> (1.0f, Set.empty, List.empty, Map.empty)
        let options =
            { BattleSpeed = WaitSpeed
              HardcoreMode = false
              SongVolume = Constants.Gameplay.SongVolumeDefault }
        let (advents, inventory) =
            match fieldType with
            | DebugField -> (debugAdvents, snd (Inventory.tryAddItems options.HardcoreMode (List.map KeyItem debugKeyItems) inventory))
            | _ -> (advents, inventory)
        let omniSeedState = OmniSeedState.makeFromSeedState randSeedState
        let props = makeProps time fieldType omniSeedState
        { FieldTime_ = 0L
          FieldState_ = Playing
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
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          PartyMenu_ = { PartyMenuState = PartyMenuClosed; PartyMenuSelections = [] }
          ShopOpt_ = None
          Options_ = options
          CueDefinitions_ = cueDefinitions
          CueDefinitionsOriginal_ = cueDefinitions
          Cue_ = CueSystem.Fin
          Tint_ = Color.Zero
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          FieldSongTimeOpt_ = None
          AutoMaps_ = Map.empty
          FieldType_ = fieldType
          FieldCursor_ = FieldCursor.initial
          FieldPointerShown_ = true
          FieldIntent_ = Intent.empty }

    let empty =
        { FieldTime_ = 0L
          FieldState_ = Quit
          SaveSlot_ = Slot1
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.empty
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
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          PartyMenu_ = { PartyMenuState = PartyMenuClosed; PartyMenuSelections = [] }
          ShopOpt_ = None
          Options_ = { BattleSpeed = WaitSpeed; HardcoreMode = false; SongVolume = Constants.Gameplay.SongVolumeDefault }
          CueDefinitions_ = Map.empty
          CueDefinitionsOriginal_ = Map.empty
          Cue_ = CueSystem.Fin
          Tint_ = Color.Zero
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          FieldSongTimeOpt_ = None
          AutoMaps_ = Map.empty
          FieldType_ = EmptyField
          FieldCursor_ = FieldCursor.initial
          FieldPointerShown_ = true
          FieldIntent_ = Intent.empty }

    let initial time saveSlot =
        make time TombOuter saveSlot (max 1UL Gen.randomul) Avatar.initial (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial

    let debug time =
        make time DebugField Slot1 Constants.Gameplay.SeedStateDefault Avatar.empty (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial

    let tryLoad time saveSlot =
        try let saveFilePath =
                match saveSlot with
                | Slot1 -> Assets.User.SaveFilePath1
                | Slot2 -> Assets.User.SaveFilePath2
                | Slot3 -> Assets.User.SaveFilePath3
            let fileStr = File.ReadAllText saveFilePath
            match fileStr.Split ChecksumToken with
            | [|fieldStr; checksumStr|] ->
                let checksumExpected = computeChecksum fieldStr
                let mutable checksumFound = 0L
                if Int64.TryParse (checksumStr, &checksumFound) then
                    let saveValidated =
#if !DEBUG
                        checksumFound = checksumExpected
#else
                        ignore checksumExpected; true
#endif
                    if saveValidated then
                        let field = scvalue<Field> fieldStr
                        let props = makeProps time field.FieldType_ field.OmniSeedState_
                        Some { field with Props_ = props; FieldCursor_ = FieldCursor.initial; FieldPointerShown_ = true; FieldIntent_ = Intent.empty }
                    else Log.error "Failed to load save file due to invalid checksum."; None
                else Log.error "Failed to load save file due to unparsable checksum."; None
            | _ -> Log.error "Failed to load save file due to missing checksum or invalid format."; None
        with exn -> Log.error ("Failed to load save file due to: " + scstring exn); None

    let loadOrInitial time saveSlot =
        match tryLoad time saveSlot with
        | Some field -> field
        | None -> initial time saveSlot

type Field = Field.Field