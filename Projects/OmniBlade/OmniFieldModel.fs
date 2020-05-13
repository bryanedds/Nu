namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType }

[<RequireQualifiedAccess>]
module FieldModel =

    type [<ReferenceEquality; NoComparison>] FieldModel =
        private
            { FieldType_ : FieldType
              AvatarModel_ : AvatarModel
              Legion_ : Map<int, Legionnaire>
              Advents_ : Advent Set
              Inventory_ : Inventory
              Gold_ : int }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.Avatar = this.AvatarModel_
        member this.Advents = this.Advents_
        member this.Inventory = this.Inventory_
        member this.Gold = this.Gold_

        static member getPartyMembers fieldModel =
            Map.filter
                (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt)
                fieldModel.Legion_

        static member updateAvatar updater fieldModel =
            { fieldModel with AvatarModel_ = updater fieldModel.AvatarModel_ }

        static member updateAdvents updater model =
            { model with Advents_ = updater model.Advents_ }

        static member updateInventory updater model =
            { model with Inventory_ = updater model.Inventory_ }

        static member updateGold updater model =
            { model with Gold_ = updater model.Gold_ }

        static member make fieldType avatarModel legion advents inventory gold =
            { FieldType_ = fieldType
              AvatarModel_ = avatarModel
              Legion_ = legion
              Advents_ = advents
              Inventory_ = inventory
              Gold_ = gold }

type FieldModel = FieldModel.FieldModel