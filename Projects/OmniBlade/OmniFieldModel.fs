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

type [<ReferenceEquality; NoComparison>] SubmenuUseModel =
    { SubmenuUseSelection : int * ItemType
      SubmenuUsePrompt : string
      SubmenuUseEffect : string
      SubmenuUseTargets : int list }

    static member make selection prompt effect targets =
        { SubmenuUseSelection = selection
          SubmenuUsePrompt = prompt
          SubmenuUseEffect = effect
          SubmenuUseTargets = targets }

type [<ReferenceEquality; NoComparison>] SubmenuEquip =
    { EquipmentCurrentAlly : int
      EquipmentAllies : int list }

type [<ReferenceEquality; NoComparison>] SubmenuItem =
    { ItemUnused : unit }

type [<StructuralEquality; NoComparison>] SubmenuState =
    | SubmenuEquip of SubmenuEquip
    | SubmenuItem of SubmenuItem
    | SubmenuClosed

type [<ReferenceEquality; NoComparison>] SubmenuModel =
    { SubmenuState : SubmenuState
      SubmenuUseModelOpt : SubmenuUseModel option }

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

type [<ReferenceEquality; NoComparison>] ShopModel =
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
              SubmenuModel_ : SubmenuModel
              ShopModelOpt_ : ShopModel option
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
        member this.SubmenuModel = this.SubmenuModel_
        member this.ShopModelOpt = this.ShopModelOpt_
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

    let updateSubmenuModel updater fieldModel =
        { fieldModel with SubmenuModel_ = updater fieldModel.SubmenuModel_ }

    let updateShopModelOpt updater fieldModel =
        { fieldModel with ShopModelOpt_ = updater fieldModel.ShopModelOpt_ }

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
          SubmenuModel_ = { SubmenuState = SubmenuClosed; SubmenuUseModelOpt = None }
          ShopModelOpt_ = None
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
          SubmenuModel_ = { SubmenuState = SubmenuClosed; SubmenuUseModelOpt = None }
          ShopModelOpt_ = None
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
          SubmenuModel_ = { SubmenuState = SubmenuClosed; SubmenuUseModelOpt = None }
          ShopModelOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel