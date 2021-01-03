namespace InfinityRpg
open Prime

type CharacterIndex =
    | EnemyIndex of int
    | PlayerIndex
    member this.IsEnemy = match this with EnemyIndex _ -> true | PlayerIndex -> false
    member this.IsAlly = not this.IsEnemy

type AllyType =
    | Player

type EnemyType =
    | Goopy
    | Goblin
    | Snake
    | Zommie

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

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
    | Accessory

type ConsumableType =
    | Herb

type SpecialType =
    | MagicMissile
    | Spark
    | Bomb
    | DragonBreath
    | Plasma
    | Bolt
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
    | Special of SpecialType
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

type WeaponType =
    | Body
    | OakSword

type WeaponSubtype =
    | Melee
    | Sword

type WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int }

type ArmorType =
    | HoboRags
    | LeatherHide
    | ChainMail

type ArmorSubtype =
    | Robe
    | Vest
    | Mail

type ArmorData =
    { ArmorType : ArmorType // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      SpecialPointsBase : int }

type AccessoryType =
    | LeatherBoots
    | BlingRing
    | MagicPorkRinds

type AccessoryData =
    { AccessoryType : AccessoryType // key
      ShieldBase : int
      CounterBase : int }