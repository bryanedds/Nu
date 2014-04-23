namespace OmniBlade
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module OmniDataModule =

    type [<StructuralEquality; NoComparison>] OmniWeaponData =
        { Name : Lun
          Type : OmniWeaponType
          PhysicalPower : int
          MagicalPower : int
          ElementTypes : OmniElementType Set }

    type [<StructuralEquality; NoComparison>] OmniArmorData =
        { Name : Lun
          Type : OmniArmorType
          PhysicalDefense : int
          MagicalDefense : int
          ElementTypes : OmniElementType Set }

    type [<StructuralEquality; NoComparison>] OmniMoveData =
        { Name : Lun
          Type : OmniMoveType
          SpecialAbilityType : OmniSpecialAbilityType
          OmniEffectType : OmniEffectType
          SpecialPointCost : int
          PowerMultiplier : single // multiplier to the relevant powers queried from the move's participants
          SuccessRate : single
          Curative : bool
          Percentive : bool
          Defending : bool
          PreCountering : bool
          Countering : bool // can be active simultaneously with pre-countering
          OmniElementType : OmniElementType
          AddStatusType : OmniStatusType Set
          RemoveStatusType : OmniStatusType Set
          TargetType : OmniTargetType
          MortalityType : OmniMortalityType }

    type [<StructuralEquality; NoComparison>] OmniItemData =
        { Name : Lun
          Type : OmniItemType
          OptMoveData : OmniMoveData option }

    type [<StructuralEquality; NoComparison>] OmniCharacterData =
        { Name : Lun // because character names are used as keys, characters cannot be renamed in OmniBlade
          MoveData : OmniMoveData list // usable moves are queried
          EquipWeaponType : OmniWeaponType
          EquipArmorType : OmniArmorType
          PhysicalRating : int // physical power is calculated
          MagicRating : int // magic power is calculated
          StaminaRating : int // hp max is calculated
          AgilityRating : int // tt gain is calculated
          WillRating : int // sp max is calculated
          StartingExperience : int
          ExperienceRewarded : int
          GoldRewarded : int }

    type [<StructuralEquality; NoComparison>] OmniFieldData =
        { Name : Lun
          GroupFileName : string }

    type [<StructuralEquality; NoComparison>] OmniBattleData =
        { Name : Lun
          BossEnemy : OmniCharacterData
          BossEnemyDefeatEndsBattle : bool
          MinorEnemies : OmniCharacterData list
          MinorEnemySpawnsMax : int
          AwardType : OmniItemType }