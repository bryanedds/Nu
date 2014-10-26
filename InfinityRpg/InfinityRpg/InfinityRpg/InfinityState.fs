namespace InfinityRpg
open System
open Nu

[<AutoOpen>]
module InfinityStateModule =

    type [<StructuralEquality; StructuralComparisonAttribute>] ElementType =
        | Fire
        | Ice
        | Lightning

    type [<StructuralEquality; StructuralComparisonAttribute>] StatusType =
        | Defending
        | Poison
        | Mute
        | Sleep

    type [<StructuralEquality; NoComparison>] EquipmentType =
        | Sword
        | Armor
        | Relic

    type [<StructuralEquality; NoComparison>] ConsumableType =
        | Herb

    type [<StructuralEquality; NoComparison>] KeyItemType =
        | Feather // jump
        | Gills // swim in shallow water
        | Hammer // smash obstacles
        | SteelSoles // walk over spikes
        | Wings // fly in overworld
        | Raft // navigate over deep water

    type [<StructuralEquality; NoComparison>] ItemType =
        | Equipment of EquipmentType
        | Consumable of ConsumableType
        | KeyItem of KeyItemType

    type [<StructuralEquality; NoComparison>] TargetType =
        | SingleTarget of Range : int * SelfOnly : bool
        | RadiusTarget of Range : int * SelfOnly : bool * Diameter : int
        | LineTarget of Range : int
        | QuarterScreenTarget
        | FullScreenTarget

    type [<StructuralEquality; NoComparison>] EffectType =
        | Physical
        | Magical

    type [<StructuralEquality; NoComparison>] SpecialType =
        | Spark
        | Bomb
        | DragonBreath
        | Snowball
        | Volt
        | PowerUp
        | PowerDown
        | ShieldUp
        | ShieldDown
        | MindUp
        | MindDown
        | CounterUp
        | CounterDown
        | Sleep
        | Poison
        | Immunity // immunizes against status changes

    type [<StructuralEquality; NoComparison>] InteractionType =
        | Attack
        | Defend // auto counters at rate of counter-stat
        | Consume of ConsumableType
        | Special of SpecialType
        | Investigate // might also talk to NPCs

    type [<StructuralEquality; NoComparison>] WeaponType =
        | BentSword
        | WindCutter
        | BigMeany

    type [<StructuralEquality; NoComparison>] ArmorType =
        | HobosClobos
        | LeatherHide
        | ChainMail

    type [<StructuralEquality; NoComparison>] RelicType =
        | WussInBoots
        | BlingRing
        | MagicPorkRinds

    type [<StructuralEquality; NoComparison>] CharacterType =
        | Player
        | Slimebo
        | Skelebone
        | Dargon

    type [<StructuralEquality; NoComparison>] EquipmentRatingData =
        { Level : int
          PhysicalRating : single // physical power = Level * PhysicalRating
          MagicalRating : single } // magical power = Level * MagicalRating

    type [<StructuralEquality; NoComparison>] WeaponData =
        { EquipmentRating : EquipmentRatingData }

    type [<StructuralEquality; NoComparison>] ArmorData =
        { EquipmentRating : EquipmentRatingData }

    type [<StructuralEquality; NoComparison>] RelicData =
        { EquipmentRating : EquipmentRatingData }

    type [<StructuralEquality; NoComparison>] EquipmentData =
        | WeaponData of WeaponData
        | ArmorData of ArmorData
        | RelicData of RelicData

    type [<StructuralEquality; NoComparison>] ConsumableData =
        { ConsumableData : unit }

    type [<StructuralEquality; NoComparison>] KeyItemData =
        { KeyItemData : unit }

    type [<StructuralEquality; NoComparison>] BasicItemData =
        | EquipmentData of EquipmentData
        | ConsumableData of ConsumableData
        | KeyItemData of KeyItemData

    type [<StructuralEquality; NoComparison>] ItemData =
        { Name : string
          Description : string
          BasicData : BasicItemData }

    type [<StructuralEquality; NoComparison>] InteractionData =
        { Name : string
          InteractionType : InteractionType
          SpecialType : SpecialType
          EffectType : EffectType
          SpecialPointCost : int
          SuccessRate : single
          Curative : bool
          Defending : bool // also implies countering
          ElementType : ElementType
          AddStatusType : StatusType Set
          RemoveStatusType : StatusType Set
          TargetType : TargetType }

    type [<StructuralEquality; NoComparison>] CharacterRatingData =
        { BaseExperience : int // used to calculate base level for all instances of character
          PhysicalRating : int // physical power is calculated based on level
          MagicRating : int // magic power is calculated based on level
          StaminaRating : int // hp max is calculated based on level
          AgilityRating : int // tt gain is calculated based on level
          WillRating : int } // sp max is calculated based on level

    type [<StructuralEquality; NoComparison>] RewardData =
        { Experience : int
          Gold : int }

    type [<StructuralEquality; NoComparison>] CharacterData =
        { Name : string
          CharacterType : CharacterType
          CharacterRating : CharacterRatingData
          BaseInteractions : InteractionData list // base level for all instances of character
          Reward : RewardData }

    type [<StructuralEquality; NoComparison>] Character =
        { Id : Guid
          CharacterType : CharacterType
          HitPoints : int // hp max is calculated
          SpecialPoints : int // sp max is calculated
          PowerBuff : single // rate at which power is buffed / debuffed
          SheildBuff : single // rate at which shield is buffed / debuffed
          MindBuff : single // rate at which mind is buffed / debuffed
          CounterBuff : single // rate at which counter is buffed / debuffed
          Statuses : StatusType Set
          EquippedWeapon : WeaponType option
          EquippedArmor : ArmorType option
          EquippedRelics : RelicType list
          GainedExperience : int } // level is calculated from base experience + gained experience

    type [<StructuralEquality; NoComparison>] CharacterActionState =
        | CharacterInteraction of InteractionType
        | CharacterNavigate
        | CharacterIdle
