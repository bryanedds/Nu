namespace InfinityRpg
open System
open Nu

[<AutoOpen>]
module CharacterStateModule =

    type ElementType =
        | Fire
        | Ice
        | Lightning

    type StatusType =
        | Defending // also implies countering
        | Poison
        | Mute
        | Sleep

    type EquipmentType =
        | Sword
        | Armor
        | Relic

    type ConsumableType =
        | Herb

    type KeyItemType =
        | Feather // jump
        | Gills // swim in shallow water
        | Hammer // smash obstacles
        | SteelSoles // walk over spikes
        | Wings // fly in overworld
        | Raft // navigate over deep water

    type ItemType =
        | Equipment of EquipmentType
        | Consumable of ConsumableType
        | KeyItem of KeyItemType

    type TargetType =
        | SingleTarget of Range : int * SelfOnly : bool
        | RadiusTarget of Range : int * SelfOnly : bool * Diameter : int
        | LineTarget of Range : int
        | QuarterScreenTarget
        | FullScreenTarget

    type EffectType =
        | Physical
        | Magical

    type SpecialType =
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

    type InteractionType =
        | Attack
        | Defend // auto counters at rate of counter-stat
        | Consume of ConsumableType
        | Special of SpecialType
        | Interact // general interaction such as talking to NPCs

    type WeaponType =
        | BentSword
        | WindCutter
        | BigMeany

    type ArmorType =
        | HobosClobos
        | LeatherHide
        | ChainMail

    type RelicType =
        | WussInBoots
        | BlingRing
        | MagicPorkRinds

    type CharacterType =
        | Player
        | Slimebo
        | Skelebone
        | Dargon

    type EquipmentRatingData =
        { Level : int
          PhysicalRating : single // physical power = Level * PhysicalRating
          MagicalRating : single } // magical power = Level * MagicalRating

    type WeaponData =
        { EquipmentRating : EquipmentRatingData }

    type ArmorData =
        { EquipmentRating : EquipmentRatingData }

    type RelicData =
        { EquipmentRating : EquipmentRatingData }

    type EquipmentData =
        | WeaponData of WeaponData
        | ArmorData of ArmorData
        | RelicData of RelicData

    type ConsumableData =
        { ConsumableData : unit }

    type KeyItemData =
        { KeyItemData : unit }

    type BasicItemData =
        | EquipmentData of EquipmentData
        | ConsumableData of ConsumableData
        | KeyItemData of KeyItemData

    type ItemData =
        { Name : string
          Description : string
          BasicData : BasicItemData }

    type InteractionData =
        { Name : string
          InteractionType : InteractionType
          SpecialType : SpecialType
          EffectType : EffectType
          SpecialPointCost : int
          SuccessRate : single
          Curative : bool
          ElementType : ElementType
          AddStatusType : StatusType Set
          RemoveStatusType : StatusType Set
          TargetType : TargetType }

    type CharacterRatingData =
        { BaseExperience : int // used to calculate base level for all instances of character
          PhysicalRating : int // physical power is calculated based on level
          MagicRating : int // magic power is calculated based on level
          StaminaRating : int // hp max is calculated based on level
          WillRating : int } // sp max is calculated based on level

    type RewardData =
        { Experience : int
          Gold : int }

    type CharacterData =
        { Name : string
          CharacterType : CharacterType
          CharacterRating : CharacterRatingData
          BaseInteractions : InteractionData list // base level for all instances of character
          Reward : RewardData }

    type Character =
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
          AddedExperience : int } // level is calculated from base experience + added experience

    type CharacterAction =
        | Interacting of InteractionType
        | Navigating
        | Standing