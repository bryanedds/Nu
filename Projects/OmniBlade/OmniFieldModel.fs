namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<ReferenceEquality; NoComparison>] DialogModel =
    { DialogForm : DialogForm
      DialogText : string
      DialogProgress : int
      DialogPage : int }

type [<ReferenceEquality; NoComparison>] SubmenuUse =
    { SubmenuUseSelection : int * ItemType
      SubmenuUseEffect : string
      SubmenuUsePrompt : string
      SubmenuUseTargets : int list }

    static member make selection effect prompt targets =
        { SubmenuUseSelection = selection
          SubmenuUseEffect = effect
          SubmenuUsePrompt = prompt
          SubmenuUseTargets = targets }

    static member makeFromConsumableData selection targets (cd : ConsumableData) =
        let itemType = snd selection
        let effect = "Effect: " + cd.Description
        let prompt = "Use " + ItemType.getName itemType + " on:"
        SubmenuUse.make selection prompt effect targets

    static member makeFromWeaponData selection targets (wd : WeaponData) =
        let itemType = snd selection
        let effect = "Effect: " + wd.Description
        //let stats = "Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        let prompt = "Equip " + ItemType.getName itemType + " on:"
        SubmenuUse.make selection prompt effect targets

    static member makeFromArmorData selection targets (ad : ArmorData) =
        let itemType = snd selection
        let effect = "Effect: " + ad.Description
        //let stats = "HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        let prompt = "Equip " + ItemType.getName itemType + " on:"
        SubmenuUse.make selection prompt effect targets

    static member makeFromAccessoryData selection targets (ad : AccessoryData) =
        let itemType = snd selection
        let effect = "Effect: " + ad.Description
        //let stats = "Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        let prompt = "Equip " + ItemType.getName itemType + " on:"
        SubmenuUse.make selection effect prompt targets

    static member tryMakeFromSelection selection targets =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty data.Value.Consumables with
            | Some cd -> SubmenuUse.makeFromConsumableData selection targets cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name data.Value.Weapons with
                | Some wd -> SubmenuUse.makeFromWeaponData selection targets wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name data.Value.Armors with
                | Some ad -> SubmenuUse.makeFromArmorData selection targets ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name data.Value.Accessories with
                | Some ad -> SubmenuUse.makeFromAccessoryData selection targets ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] SubmenuLegion =
    { LegionIndex : int
      LegionIndices : int list }
      
    static member tryGetLegionnaire (legion : Legion) submenuLegion =
        Map.tryFind submenuLegion.LegionIndex legion

    static member tryGetLegionnaireAndLegionData legion submenuLegion =
        match SubmenuLegion.tryGetLegionnaire legion submenuLegion with
        | Some legionnaire ->
            match Map.tryFind legionnaire.CharacterType data.Value.Characters with
            | Some characterData -> Some (legionnaire, characterData)
            | None -> None
        | None -> None

    static member tryGetLegionData legion submenuLegion =
        let lacdOpt = SubmenuLegion.tryGetLegionnaireAndLegionData legion submenuLegion
        Option.map snd lacdOpt

type [<ReferenceEquality; NoComparison>] SubmenuItem =
    { ItemPage : int }

type [<StructuralEquality; NoComparison>] SubmenuState =
    | SubmenuLegion of SubmenuLegion
    | SubmenuItem of SubmenuItem
    | SubmenuClosed

type [<ReferenceEquality; NoComparison>] Submenu =
    { SubmenuState : SubmenuState
      SubmenuUseOpt : SubmenuUse option }

type [<StructuralEquality; StructuralComparison>] ShopState =
    | ShopBuying
    | ShopSelling

type [<ReferenceEquality; NoComparison>] ShopConfirmModel =
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
        ShopConfirmModel.make selection price offer stats effect

    static member makeFromWeaponData buying inventory selection (wd : WeaponData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then wd.Cost else wd.Cost / 2
        let effect = "Effect: " + wd.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "Pow: " + string wd.PowerBase + " | Mag: " + string wd.MagicBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirmModel.make selection price offer stats effect

    static member makeFromArmorData buying inventory selection (ad : ArmorData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "HP: " + string ad.HitPointsBase + " | TP: " + string ad.TechPointsBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirmModel.make selection price offer stats effect

    static member makeFromAccessoryData buying inventory selection (ad : AccessoryData) =
        let itemType = snd selection
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let stats = "Blk: " + string ad.ShieldBase + " | Ctr: " + string ad.CounterBase + " | Own: " + string (Inventory.getItemCount itemType inventory)
        ShopConfirmModel.make selection price offer stats effect

    static member tryMakeFromSelection buying inventory selection =
        match snd selection with
        | Consumable ty ->
            match Map.tryFind ty data.Value.Consumables with
            | Some cd -> ShopConfirmModel.makeFromConsumableData buying inventory selection cd |> Some
            | None -> None
        | Equipment ty ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name data.Value.Weapons with
                | Some wd -> ShopConfirmModel.makeFromWeaponData buying inventory selection wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name data.Value.Armors with
                | Some ad -> ShopConfirmModel.makeFromArmorData buying inventory selection ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name data.Value.Accessories with
                | Some ad -> ShopConfirmModel.makeFromAccessoryData buying inventory selection ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<ReferenceEquality; NoComparison>] Shop =
    { ShopType : ShopType
      ShopState : ShopState
      ShopPage : int
      ShopConfirmModelOpt : ShopConfirmModel option }

type [<ReferenceEquality; NoComparison>] FieldTransition =
    { FieldType : FieldType
      FieldIndex : Vector2
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module FieldModel =

    type [<ReferenceEquality; NoComparison>] FieldModel =
        private
            { FieldType_ : FieldType
              Avatar_ : AvatarModel
              Legion_ : Legion
              Advents_ : Advent Set
              PropStates_ : Map<int, PropState>
              Inventory_ : Inventory
              Submenu_ : Submenu
              ShopOpt_ : Shop option
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : DialogModel option
              BattleOpt_ : BattleModel option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Legion = this.Legion_
        member this.Advents = this.Advents_
        member this.PropStates = this.PropStates_
        member this.Inventory = this.Inventory_
        member this.Submenu = this.Submenu_
        member this.ShopOpt = this.ShopOpt_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_

    let getParty fieldModel =
        fieldModel.Legion_ |>
        Map.filter (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let updateFieldType updater fieldModel =
        { fieldModel with FieldType_ = updater fieldModel.FieldType_ }

    let updateAvatar updater fieldModel =
        { fieldModel with Avatar_ = updater fieldModel.Avatar_ }

    let updateAdvents updater fieldModel =
        { fieldModel with Advents_ = updater fieldModel.Advents_ }

    let updatePropStates updater fieldModel =
        { fieldModel with PropStates_ = updater fieldModel.PropStates_ }

    let updateInventory updater fieldModel =
        { fieldModel with Inventory_ = updater fieldModel.Inventory_ }

    let updateSubmenu updater fieldModel =
        { fieldModel with Submenu_ = updater fieldModel.Submenu_ }

    let updateShopOpt updater fieldModel =
        { fieldModel with ShopOpt_ = updater fieldModel.ShopOpt_ }

    let updateDialogOpt updater fieldModel =
        { fieldModel with DialogOpt_ = updater fieldModel.DialogOpt_ }

    let updateFieldTransitionOpt updater fieldModel =
        { fieldModel with FieldTransitionOpt_ = updater fieldModel.FieldTransitionOpt_ }

    let updateBattleOpt updater fieldModel =
        { fieldModel with BattleOpt_ = updater fieldModel.BattleOpt_ }

    let make fieldType avatarModel legion advents inventory =
        { FieldType_ = fieldType
          Avatar_ = avatarModel
          Legion_ = legion
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
          Avatar_ = AvatarModel.empty
          Legion_ = Map.empty
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let initial =
        { FieldType_ = DebugRoom
          Avatar_ = AvatarModel.empty
          Legion_ = Map.ofList [(0, Legionnaire.finn); (1, Legionnaire.glenn)]
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          Submenu_ = { SubmenuState = SubmenuClosed; SubmenuUseOpt = None }
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel