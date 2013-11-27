namespace Nu
open OpenTK
open Nu.OmniTypes
module OmniData =

    type [<StructuralEquality; NoComparison>] WeaponData =
        { Type : WeaponType
          PhysicalPower : int
          MagicalPower : int
          ElementTypes : ElementType Set }

    type [<StructuralEquality; NoComparison>] ArmorData =
        { Type : ArmorType
          PhysicalDefense : int
          MagicalDefense : int
          ElementTypes : ElementType Set }

    type [<StructuralEquality; NoComparison>] MoveData =
        { Name : string
          Type : MoveType
          SpecialAbilityType : SpecialAbilityType
          EffectType : EffectType
          SpecialPoints : int
          PowerMultiplier : single // multiplier to the relevant powers queried from the move's participants
          SuccessRate : single
          Curative : bool
          Percentive : bool
          Defending : bool
          Countering : bool
          ElementType : ElementType
          AddStatusType : StatusType Set
          RemoveStatusType : StatusType Set
          TargetType : TargetType
          Revive : bool }

    type [<StructuralEquality; NoComparison>] ItemData =
        { Name : string
          Type : ItemType
          OptMoveData : MoveData option }

    type [<StructuralEquality; NoComparison>] CharacterData =
        { Name : string
          Type : CharacterType
          MoveData : MoveData list // usable moves are queried
          EquipWeaponType : WeaponType
          EquipArmorType : ArmorType
          PhysicalRating : int // physical power is queried
          MagicRating : int // magic power is queried
          StaminaRating : int // hp max is queried
          AgilityRating : int // tt gain is queried
          WillRating : int // sp max is queried
          StartingExperience : int // this is the only experience for enemies
          ExperienceEarned : int // only used with enemies
          GoldEarned : int } // only used with enemies

    type [<StructuralEquality; NoComparison>] FieldData =
        { Id : Id
          GroupModelFileName : string }

    type [<StructuralEquality; NoComparison>] BattleData =
        { Id : Id
          Name : string
          BossEnemy : CharacterData
          BossEnemyDefeatEndsBattle : bool
          MinorEnemies : CharacterData list
          MinorEnemySpawnsMax : int
          AwardType : ItemType }