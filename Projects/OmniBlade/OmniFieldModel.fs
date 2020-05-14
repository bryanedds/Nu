namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType }

type DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type DialogModel =
    { DialogForm : DialogForm
      DialogText : Dialog
      DialogProgress : int }

[<RequireQualifiedAccess>]
module FieldModel =

    type [<ReferenceEquality; NoComparison>] FieldModel =
        private
            { FieldType_ : FieldType
              Avatar_ : AvatarModel
              Legion_ : Map<int, Legionnaire>
              Advents_ : Advent Set
              Inventory_ : Inventory
              Gold_ : int
              DialogOpt_ : DialogModel option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Advents = this.Advents_
        member this.Inventory = this.Inventory_
        member this.Gold = this.Gold_
        member this.DialogOpt = this.DialogOpt_

    let getPartyMembers fieldModel =
        Map.filter
            (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt)
            fieldModel.Legion_

    let updateAvatar updater fieldModel =
        { fieldModel with Avatar_ = updater fieldModel.Avatar_ }

    let updateAdvents updater model =
        { model with Advents_ = updater model.Advents_ }

    let updateInventory updater model =
        { model with Inventory_ = updater model.Inventory_ }

    let updateGold updater model =
        { model with Gold_ = updater model.Gold_ }

    let updateDialogOpt updater model =
        { model with DialogOpt_ = updater model.DialogOpt_ }

    let make fieldType avatarModel legion advents inventory gold =
        { FieldType_ = fieldType
          Avatar_ = avatarModel
          Legion_ = legion
          Advents_ = advents
          Inventory_ = inventory
          Gold_ = gold
          DialogOpt_ = None }

    let empty =
        { FieldType_ = FieldType.DebugRoom
          Avatar_ = AvatarModel.empty
          Legion_ = Map.empty
          Advents_ = Set.empty
          Inventory_ = { Items = Map.empty }
          Gold_ = 0
          DialogOpt_ = None }

type FieldModel = FieldModel.FieldModel