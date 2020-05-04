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
            { Legion : Map<int, Legionnaire>
              GameEvents_ : Set<GameEvent>
              Inventory_ : Inventory
              Gold_ : int }

        static member getPartyMembers fieldModel =
            Map.filter
                (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt)
                fieldModel.Legion

        static member make legion gameEvents inventory gold =
            { Legion = legion
              GameEvents_ = gameEvents
              Inventory_ = inventory
              Gold_ = gold }

type FieldModel = FieldModel.FieldModel