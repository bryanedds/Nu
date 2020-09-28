namespace OmniBlade
open System
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

    static member finn =
        { LegionIndex = 0
          PartyIndexOpt = Some 0
          CharacterType = Ally Finn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

    static member glenn =
        { LegionIndex = 1
          PartyIndexOpt = Some 1
          CharacterType = Ally Glenn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

type [<StructuralEquality; NoComparison>] DialogForm =
    | DialogThin
    | DialogMedium
    | DialogLarge

type [<StructuralEquality; NoComparison>] DialogModel =
    { DialogForm : DialogForm
      DialogText : Dialog
      DialogProgress : int }

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
              Legion_ : Map<int, Legionnaire>
              Advents_ : Advent Set
              Inventory_ : Inventory
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : DialogModel option
              BattleOpt_ : BattleModel option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.Avatar_
        member this.Legion = this.Legion_
        member this.Advents = this.Advents_
        member this.Inventory = this.Inventory_
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

    let updateInventory updater fieldModel =
        { fieldModel with Inventory_ = updater fieldModel.Inventory_ }

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
          Inventory_ = inventory
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let empty =
        { FieldType_ = DebugRoom
          Avatar_ = AvatarModel.empty
          Legion_ = Map.empty
          Advents_ = Set.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

    let initial =
        { FieldType_ = DebugRoom
          Avatar_ = AvatarModel.empty
          Legion_ = Map.ofList [(0, Legionnaire.finn); (1, Legionnaire.glenn)]
          Advents_ = Set.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None }

type FieldModel = FieldModel.FieldModel