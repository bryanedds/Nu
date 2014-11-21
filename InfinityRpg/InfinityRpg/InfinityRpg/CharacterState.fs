namespace InfinityRpg
open System
open OpenTK
open Nu

[<AutoOpen>]
module CharacterStateModule =

    type ElementType =
        | Fire // beats nothing; strongest
        | Water // beats fire, lightning; weakest
        | Lightning // beats water

    type StatusType =
        | Defending // also implies countering
        | Poisoned
        | Muted
        | Sleeping

    type EquipmentType =
        | Weapon
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
        | SingleTarget of Distance : int * SelfOnly : bool
        | RangeTarget of Distance : int * SelfOnly : bool * Range : int
        | LineTarget of Distance : int
        | QuarterScreenTarget // in one of four cardinal directions like <, >, ^, or v
        | FullScreenTarget

    type EffectType =
        | Physical
        | Magical

    type SpecialType =
        | Spark
        | Bomb
        | DragonBreath
        | Plasma
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

    // NOTE: this seems to be a wierd type; I can't specify the Consume type without also specifying
    // a ConsumableType, for example. Whether this is a real problem remains to be seen.
    type ActionType =
        | Attack
        | Defend // auto counters at rate of counter stat
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
        | Goopy
        | Skelebone
        | Dargon

    type SpecialData =
        { SpecialType : SpecialType
          EffectType : EffectType
          SpecialPointCost : int
          SuccessRate : single
          Curative : bool
          ElementType : ElementType
          AddStatusType : StatusType Set
          RemoveStatusType : StatusType Set
          TargetType : TargetType }

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

    type ActionData =
        { Name : string
          ActionType : ActionType }

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
          BaseActions : ActionData list // base actions for all instances of character
          Reward : RewardData }

    type WalkDescriptor =
        { WalkDirection : Direction
          WalkOriginM : Vector2i }

    type NavigationDescriptor =
        { WalkDescriptor : WalkDescriptor
          NavigationGoalM : Vector2i }

    type [<StructuralEquality; NoComparison>] ActivityState =
        | Standing
        | Navigating of NavigationDescriptor
        | Acting of ActionData

    type Arrival =
        | Arriving
        | Arrived