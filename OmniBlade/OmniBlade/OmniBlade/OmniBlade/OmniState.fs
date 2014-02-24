namespace OmniBlade
open OpenTK
open Nu

type [<StructuralEquality; NoComparison>] OmniCharacter =
    { Id : Id
      Data : OmniCharacterData
      Hp : int // hp max is calculated
      Sp : int // sp max is calculated
      Tt : int // turn ticks
      Defending : bool
      Statuses : OmniStatusType Set
      EquippedWeapon : OmniWeaponType option
      EquippedArmor : OmniArmorType option
      EquippedRelics : OmniRelicType list
      Experience : int } // level is calculated

// NOTE: movement states probably ought to be keyed off the animation engine, but since it's
// not implemented, movement states are defined manually
type [<StructuralEquality; NoComparison>] OmniCharacterUseAttackState =
    | OmniAttackLag1 of int
    | OmniAttackLaunch1 of int
    | OmniAttackLag2 of int
    | OmniAttackLaunch2 of int
    | OmniAttackDeltaSpawn
    | OmniAttackLag3 of int

type [<StructuralEquality; NoComparison>] OmniCharacterUseItemState =
    | OmniItemLag1 of int
    | OmniItemLaunch1 of int
    | OmniItemLag2 of int
    | OmniItemEffect2 of int
    | OmniItemDeltaSpawn
    | OmniItemLag3 of int

type [<StructuralEquality; NoComparison>] OmniCharacterUseSpecialState =
    | OmniSpecialLag1 of int
    | OmniSpecialLaunch1 of int
    | OmniSpecialLag2 of int
    | OmniSpecialEffect2 of int
    | OmniSpecialDeltaSpawn
    | OmniSpecialLag3 of int

type [<StructuralEquality; NoComparison>] OmniCharacterActionState =
    | OmniCharacterUseAttack of OmniCharacterUseAttackState
    | OmniCharacterUseItem of OmniCharacterUseItemState
    | OmniCharacterUseSpecial of OmniCharacterUseSpecialState

type [<StructuralEquality; NoComparison>] OmniBattleActionState =
    | OmniIdleAction
    | OmniCharacterAction of OmniCharacterActionState

type [<StructuralEquality; NoComparison>] OmniBattleInputState =
    | OmniGoInput of Id Set
    | OmniCrossInput of Id * Id Set
    | OmniTargetableInput of OmniTargetingInputType
    | OmniTargetInput of OmniTargetingInputType

type [<StructuralEquality; NoComparison>] OmniBattleState =
    | OmniBattleBegin
    | OmniBattleAdvance of OmniBattleInputState * OmniCharacterActionState
    | OmniBattleEnding of bool

type [<StructuralEquality; NoComparison>] OmniMove =
    { Data : OmniMoveData
      ParticipantIds : Id list // characters participating in the move
      TargetIds : Id list } // characters targetted by the move

type [<StructuralEquality; NoComparison>] OmniBattle =
    { Data : OmniBattleData
      State : OmniBattleState
      MoveQueue : OmniMove rQueue
      Enemies : OmniCharacter list
      MinorEnemySpawns : int }

type [<StructuralEquality; NoComparison>] OmniPlayer =
    { FieldId : Id
      FieldPosition : Vector2
      OptBattleDataId : Id option
      Allies : OmniCharacter list
      Gold : int }