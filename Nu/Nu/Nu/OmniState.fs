namespace Nu
open OpenTK
open Nu.OmniTypes
open Nu.OmniData
module OmniState =

    type [<StructuralEquality; NoComparison>] Character =
        { Id : Id
          Data : CharacterData
          Hp : int // hp max is queried
          Sp : int // sp max is queried
          Tt : int // turn ticks
          Defending : bool
          Statuses : StatusType Set
          EquippedWeapon : WeaponType option
          EquippedArmor : ArmorType option
          EquippedRelics : RelicType list
          Experience : int } // level is queried

    // NOTE: movement states probably ought to be keyed off the animation engine, but since it's
    // not implemented, movement states are defined manually
    type [<StructuralEquality; NoComparison>] CharacterUseAttackState =
        | AttackLag1 of int
        | AttackLaunch1 of int
        | AttackLag2 of int
        | AttackLaunch2 of int
        | AttackDeltaSpawn
        | AttackLag3 of int

    type [<StructuralEquality; NoComparison>] CharacterUseItemState =
        | ItemLag1 of int
        | ItemLaunch1 of int
        | ItemLag2 of int
        | ItemEffect2 of int
        | ItemDeltaSpawn
        | ItemLag3 of int

    type [<StructuralEquality; NoComparison>] CharacterUseSpecialState =
        | SpecialLag1 of int
        | SpecialLaunch1 of int
        | SpecialLag2 of int
        | SpecialEffect2 of int
        | SpecialDeltaSpawn
        | SpecialLag3 of int

    type [<StructuralEquality; NoComparison>] CharacterActionState =
        | CharacterUseAttack of CharacterUseAttackState
        | CharacterUseItem of CharacterUseItemState
        | CharacterUseSpecial of CharacterUseSpecialState

    type [<StructuralEquality; NoComparison>] BattleActionState =
        | IdleAction
        | CharacterAction of CharacterActionState

    type [<StructuralEquality; NoComparison>] BattleInputState =
        | GoInput of Id Set
        | CrossInput of Id * Id Set
        | TargetableInput of TargetingInputType
        | TargetInput of TargetingInputType

    type [<StructuralEquality; NoComparison>] BattleState =
        | BattleBegin
        | BattleAdvance of BattleInputState * CharacterActionState
        | BattleEnding of bool

    type [<StructuralEquality; NoComparison>] Move =
        { Data : MoveData
          CharacterIds : Id list } // characters involved in the move

    type [<StructuralEquality; NoComparison>] Battle =
        { Data : BattleData
          State : BattleState
          MoveQueue : Move rQueue
          Enemies : Character list
          MinorEnemySpawns : int }

    type [<StructuralEquality; NoComparison>] Player =
        { FieldId : Id
          FieldPosition : Vector2
          OptBattleDataId : Id option
          Allies : Character list
          Gold : int }