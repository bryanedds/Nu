// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type [<NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<ReferenceEquality; NoComparison>] Dialog =
    { DialogForm : DialogForm
      DialogText : string
      DialogProgress : int
      DialogPage : int
      DialogBattleOpt : BattleType option }

type [<ReferenceEquality; NoComparison>] SubmenuUse =
    { SubmenuUseSelection : int * ItemType
      SubmenuUseLine1 : string
      SubmenuUseLine2 : string }

    static member make selection line1 line2 =
        { SubmenuUseSelection = selection
          SubmenuUseLine1 = line1
          SubmenuUseLine2 = line2 }

    static member makeFromConsumableData selection (cd : ConsumableData) =
        let prompt = "Use " + string cd.ConsumableType + " on whom?"
        let effect = "(Effect: " + cd.Description + ")"
        SubmenuUse.make selection prompt effect

    static member makeFromWeaponData selection (wd : WeaponData) =
        let prompt = "Equip " + wd.WeaponType + " to whom?"
        let stats = "(Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + ")"
        SubmenuUse.make selection prompt stats

    static member makeFromArmorData selection (ad : ArmorData) =
        let prompt = "Equip " + ad.ArmorType + " to whom?"
        let stats = "(HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + ")"
        SubmenuUse.make selection prompt stats

    static member makeFromAccessoryData selection (ad : AccessoryData) =
        let prompt = "Equip " + ad.AccessoryType + " to whom?"
        let stats = "(Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + ")"
        SubmenuUse.make selection prompt stats

    static member tryMakeFromSelection selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty Data.Value.Consumables with
            | Some cd -> SubmenuUse.makeFromConsumableData selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name Data.Value.Weapons with
                | Some wd -> SubmenuUse.makeFromWeaponData selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name Data.Value.Armors with
                | Some ad -> SubmenuUse.makeFromArmorData selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name Data.Value.Accessories with
                | Some ad -> SubmenuUse.makeFromAccessoryData selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] SubmenuTeam =
    { TeamIndex : int
      TeamIndices : int list }
      
    static member tryGetTeammate (team : Team) submenuTeam =
        Map.tryFind submenuTeam.TeamIndex team

    static member tryGetTeammateAndTeamData team submenuTeam =
        match SubmenuTeam.tryGetTeammate team submenuTeam with
        | Some teammate ->
            match Map.tryFind teammate.CharacterType Data.Value.Characters with
            | Some characterData -> Some (teammate, characterData)
            | None -> None
        | None -> None

    static member tryGetTeamData team submenuTeam =
        let lacdOpt = SubmenuTeam.tryGetTeammateAndTeamData team submenuTeam
        Option.map snd lacdOpt

type [<ReferenceEquality; NoComparison>] SubmenuItem =
    { ItemPage : int }

type [<NoComparison>] SubmenuState =
    | SubmenuTeam of SubmenuTeam
    | SubmenuItem of SubmenuItem
    | SubmenuClosed

type [<ReferenceEquality; NoComparison>] Submenu =
    { SubmenuState : SubmenuState
      SubmenuUseOpt : SubmenuUse option }

type ShopState =
    | ShopBuying
    | ShopSelling

type [<ReferenceEquality; NoComparison>] ShopConfirm =
    { ShopConfirmSelection : int * ItemType
      ShopConfirmPrice : int
      ShopConfirmOffer : string
      ShopConfirmLine1 : string
      ShopConfirmLine2 : string }

    static member make selection price offer line1 line2 =
        { ShopConfirmSelection = selection
          ShopConfirmPrice = price
          ShopConfirmOffer = offer
          ShopConfirmLine1 = line1
          ShopConfirmLine2 = line2 }

    static member makeFromConsumableData buying inventory selection cd =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then cd.Cost else cd.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + cd.Description
        let stats = "Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromWeaponData buying inventory selection (wd : WeaponData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then wd.Cost else wd.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + wd.Description
        let stats = "Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromArmorData buying inventory selection (ad : ArmorData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + ad.Description
        let stats = "HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member makeFromAccessoryData buying inventory selection (ad : AccessoryData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + ad.Description
        let stats = "Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirm.make selection price offer effect stats

    static member tryMakeFromSelection buying inventory selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty Data.Value.Consumables with
            | Some cd -> ShopConfirm.makeFromConsumableData buying inventory selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name Data.Value.Weapons with
                | Some wd -> ShopConfirm.makeFromWeaponData buying inventory selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name Data.Value.Armors with
                | Some ad -> ShopConfirm.makeFromArmorData buying inventory selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name Data.Value.Accessories with
                | Some ad -> ShopConfirm.makeFromAccessoryData buying inventory selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] Shop =
    { ShopType : ShopType
      ShopState : ShopState
      ShopPage : int
      ShopConfirmOpt : ShopConfirm option }

type [<ReferenceEquality; NoComparison>] FieldTransition =
    { FieldType : FieldType
      FieldDestination : Vector2
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; NoComparison>] Field =
        private
            { FieldType_ : FieldType
              OmniSeedState_ : OmniSeedState
              Avatar_ : Avatar
              Team_ : Team
              EncounterCreep_ : single
              EncounterThresholdScalar_ : single
              Advents_ : Advent Set
              PropStates_ : Map<int, PropState>
              Inventory_ : Inventory
              Submenu_ : Submenu
              ShopOpt_ : Shop option
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : Dialog option
              BattleOpt_ : Battle option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.OmniSeedState = this.OmniSeedState_
        member this.Avatar = this.Avatar_
        member this.Team = this.Team_
        member this.EncounterCreep = this.EncounterCreep_
        member this.Advents = this.Advents_
        member this.PropStates = this.PropStates_
        member this.Inventory = this.Inventory_
        member this.Submenu = this.Submenu_
        member this.ShopOpt = this.ShopOpt_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_

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

    let updateFieldType updater field =
        { field with
            FieldType_ = updater field.FieldType_
            EncounterCreep_ = 0.0f
            EncounterThresholdScalar_ = Gen.randomf + 0.5f }

    let updateAvatar updater field =
        { field with Avatar_ = updater field.Avatar_ }

    let updateTeam updater field =
        { field with Team_ = updater field.Team_ }

    let updateAdvents updater field =
        { field with Advents_ = updater field.Advents_ }

    let updatePropStates updater field =
        { field with PropStates_ = updater field.PropStates_ }

    let updateInventory updater field =
        { field with Inventory_ = updater field.Inventory_ }

    let updateSubmenu updater field =
        { field with Submenu_ = updater field.Submenu_ }

    let updateShopOpt updater field =
        { field with ShopOpt_ = updater field.ShopOpt_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let updateFieldTransitionOpt updater field =
        { field with FieldTransitionOpt_ = updater field.FieldTransitionOpt_ }

    let updateBattleOpt updater field =
        let battleOpt = updater field.BattleOpt_
        { field with
            BattleOpt_ = battleOpt
            EncounterCreep_ = if Option.isSome battleOpt then 0.0f else field.EncounterCreep_ }

    let updateReference field =
        { field with FieldType_ = field.FieldType_ }

    let restoreTeam field =
        { field with Team_ = Map.map (fun _ -> Teammate.restore) field.Team_ }

    let tryAdvanceEncounterCreep (velocity : Vector2) (field : Field) world =
        match field.FieldTransitionOpt with
        | None ->
            match Data.Value.Fields.TryGetValue field.FieldType with
            | (true, fieldData) ->
                match fieldData.EncounterTypeOpt with
                | Some encounterType ->
                    match Data.Value.Encounters.TryGetValue encounterType with
                    | (true, encounterData) ->
                        let encounterThreshold = encounterData.Threshold * field.EncounterThresholdScalar_
                        let speed = velocity.Length () / 60.0f
                        let creep = speed
                        let field =
                            if creep <> 0.0f
                            then { field with EncounterCreep_ = field.EncounterCreep_ + creep }
                            else field
                        if field.EncounterCreep_ >= encounterThreshold then
                            match FieldData.tryGetBattleType field.OmniSeedState field.Avatar.Position encounterData.BattleTypes fieldData world with
                            | Some battleType ->
                                match Data.Value.Battles.TryGetValue battleType with
                                | (true, battleData) -> (Some battleData, field)
                                | (false, _) -> (None, field)
                            | None -> (None, field)
                        else (None, field)
                    | (false, _) -> (None, field)
                | None -> (None, field)
            | (false, _) -> (None, field)
        | Some _ -> (None, field)

    let synchronizeTeamFromAllies allies field =
        List.foldi (fun i field (ally : Character) ->
            updateTeam (fun team ->
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
            field allies

    let synchronizeFromBattle battle field =
        let allies = Battle.getAllies battle
        let field = synchronizeTeamFromAllies allies field
        let field = updateInventory (constant battle.Inventory) field
        let field = updateBattleOpt (constant None) field
        field

    let toSymbolizable field =
        { field with Avatar_ = Avatar.toSymbolizable field.Avatar }

    let make fieldType randSeedState avatar team advents inventory =
        { FieldType_ = fieldType
          OmniSeedState_ = OmniSeedState.makeFromSeedState randSeedState
          Avatar_ = avatar
          EncounterCreep_ = 0.0f
          EncounterThresholdScalar_ = 1.0f
          Team_ = team
          Advents_ = advents
          PropStates_ = Map.empty
          Inventory_ = inventory
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let empty =
        { FieldType_ = DebugRoom
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.empty
          Team_ = Map.empty
          EncounterCreep_ = 0.0f
          EncounterThresholdScalar_ = 1.0f
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let initial randSeedState =
        { FieldType_ = TombOuter
          OmniSeedState_ = OmniSeedState.makeFromSeedState randSeedState
          Avatar_ = Avatar.initial
          Team_ = Map.ofList [(0, Teammate.finn)]
          EncounterCreep_ = 0.0f
          EncounterThresholdScalar_ = 1.0f
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = Inventory.initial
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let save field =
        let fieldSymbolizable = toSymbolizable field
        let fileStr = scstring fieldSymbolizable
        try File.WriteAllText (Assets.SaveFilePath, fileStr) with _ -> ()

    let loadOrInitial randSeedState =
        try let fieldStr = File.ReadAllText Assets.SaveFilePath
            scvalue<Field> fieldStr
        with _ -> initial randSeedState

type Field = Field.Field