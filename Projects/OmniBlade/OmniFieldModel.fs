namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType
      ExpPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list }

    static member empty =
        { LegionIndex = 0
          PartyIndexOpt = Some 0
          CharacterType = Ally Finn
          ExpPoints = 0
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

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
              DialogOpt_ : DialogModel option
              BattleOpt_ : BattleModel option}

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Legion = this.Legion_
        member this.Advents = this.Advents_
        member this.Inventory = this.Inventory_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_

    let getParty fieldModel =
        fieldModel.Legion_ |>
        Map.filter (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let updateAvatar updater fieldModel =
        { fieldModel with Avatar_ = updater fieldModel.Avatar_ }

    let updateAdvents updater model =
        { model with Advents_ = updater model.Advents_ }

    let updateInventory updater model =
        { model with Inventory_ = updater model.Inventory_ }

    let updateDialogOpt updater model =
        { model with DialogOpt_ = updater model.DialogOpt_ }

    let updateBattleOpt updater model =
        { model with BattleOpt_ = updater model.BattleOpt_ }

    let make fieldType avatarModel legion advents inventory =
        { FieldType_ = fieldType
          Avatar_ = avatarModel
          Legion_ = legion
          Advents_ = advents
          Inventory_ = inventory
          DialogOpt_ = None
          BattleOpt_ = None }

    let empty =
        { FieldType_ = DebugField
          Avatar_ = AvatarModel.empty
          Legion_ = Map.singleton 0 Legionnaire.empty
          Advents_ = Set.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel