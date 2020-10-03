namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<StructuralEquality; NoComparison>] DialogModel =
    { DialogForm : DialogForm
      DialogText : string
      DialogProgress : int
      DialogPage : int }

type [<StructuralEquality; NoComparison>] ShopConfirmModel =
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

    static member makeFromConsumableData buying selection itemType cd =
        let header = if buying then "Buy " else "Sell "
        let price = if buying then cd.Cost else cd.Cost / 2
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        let effect = "Effect: " + cd.Description
        ShopConfirmModel.make selection price offer effect ""

    static member makeFromWeaponData buying selection itemType (wd : WeaponData) =
        let header = if buying then "Buy " else "Sell "
        let price = if buying then wd.Cost else wd.Cost / 2
        let effect = "Effect: " + wd.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        ShopConfirmModel.make selection price offer effect ""

    static member makeFromArmorData buying selection itemType (ad : ArmorData) =
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        ShopConfirmModel.make selection price offer effect ""

    static member makeFromAccessoryData buying selection itemType (ad : AccessoryData) =
        let header = if buying then "Buy " else "Sell "
        let price = if buying then ad.Cost else ad.Cost / 2
        let effect = "Effect: " + ad.Description
        let offer = header + ItemType.getName itemType + " for " + string price + "G?"
        ShopConfirmModel.make selection price offer effect ""

    static member tryMakeFromSelection buying selection =
        match snd selection with
        | Consumable ty as itemType ->
            match Map.tryFind ty data.Value.Consumables with
            | Some cd -> ShopConfirmModel.makeFromConsumableData buying selection itemType cd |> Some
            | None -> None
        | Equipment ty as itemType ->
            match ty with
            | WeaponType name ->
                match Map.tryFind name data.Value.Weapons with
                | Some wd -> ShopConfirmModel.makeFromWeaponData buying selection itemType wd |> Some
                | None -> None
            | ArmorType name ->
                match Map.tryFind name data.Value.Armors with
                | Some ad -> ShopConfirmModel.makeFromArmorData buying selection itemType ad |> Some
                | None -> None
            | AccessoryType name ->
                match Map.tryFind name data.Value.Accessories with
                | Some ad -> ShopConfirmModel.makeFromAccessoryData buying selection itemType ad |> Some
                | None -> None
        | KeyItem _ | Stash _ -> None

type [<StructuralEquality; NoComparison>] ShopModel =
    { ShopType : ShopType
      ShopState : ShopState
      ShopPage : int
      ShopConfirmModelOpt : ShopConfirmModel option }

type FieldTransition =
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
          ShopModelOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel