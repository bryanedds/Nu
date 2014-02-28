namespace OmniBlade
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module OmniDataModule =

    type [<StructuralEquality; NoComparison>] OmniWeaponData =
        { Type : OmniWeaponType
          PhysicalPower : int
          MagicalPower : int
          ElementTypes : OmniElementType Set }

    type [<StructuralEquality; NoComparison>] OmniArmorData =
        { Type : OmniArmorType
          PhysicalDefense : int
          MagicalDefense : int
          ElementTypes : OmniElementType Set }

    type [<StructuralEquality; NoComparison>] OmniMoveData =
        { Type : OmniMoveType
          Name : string
          SpecialAbilityType : OmniSpecialAbilityType
          OmniEffectType : OmniEffectType
          SpecialPointCost : int
          PowerMultiplier : single // multiplier to the relevant powers queried from the move's participants
          SuccessRate : single
          Curative : bool
          Percentive : bool
          Defending : bool
          PreCountering : bool
          Countering : bool // can also be active with pre-countering!
          OmniElementType : OmniElementType
          AddStatusType : OmniStatusType Set
          RemoveStatusType : OmniStatusType Set
          TargetType : OmniTargetType
          MortalityType : OmniMortalityType }

    type [<StructuralEquality; NoComparison>] OmniItemData =
        { Type : OmniItemType
          Name : string
          OptMoveData : OmniMoveData option }

    type [<StructuralEquality; NoComparison>] OmniCharacterData =
        { Type : OmniCharacterType
          Name : string
          MoveData : OmniMoveData list // usable moves are queried
          EquipWeaponType : OmniWeaponType
          EquipArmorType : OmniArmorType
          PhysicalRating : int // physical power is calculated
          MagicRating : int // magic power is calculated
          StaminaRating : int // hp max is calculated
          AgilityRating : int // tt gain is calculated
          WillRating : int // sp max is calculated
          StartingExperience : int // this is the only experience for enemies
          ExperienceEarned : int // only used with enemies
          GoldEarned : int } // only used with enemies

    type [<StructuralEquality; NoComparison>] OmniFieldData =
        { Id : Id
          GroupFileName : string }

    type [<StructuralEquality; NoComparison>] OmniBattleData =
        { Id : Id
          Name : string
          BossEnemy : OmniCharacterData
          BossEnemyDefeatEndsBattle : bool
          MinorEnemies : OmniCharacterData list
          MinorEnemySpawnsMax : int
          AwardType : OmniItemType }