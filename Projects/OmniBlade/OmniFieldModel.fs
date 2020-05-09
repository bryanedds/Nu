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
            { FieldType : FieldType
              Legion : Map<int, Legionnaire>
              Advents_ : Set<Advent>
              Inventory_ : Inventory
              Gold_ : int }

        static member getPartyMembers fieldModel =
            Map.filter
                (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt)
                fieldModel.Legion

        static member make fieldType legion advents inventory gold =
            { FieldType = fieldType
              Legion = legion
              Advents_ = advents
              Inventory_ = inventory
              Gold_ = gold }

type FieldModel = FieldModel.FieldModel