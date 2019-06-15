namespace InfinityRpg
open Prime

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

type ActionType =
    | Attack
    | Defend // auto counters at rate of counter stat
    | Consume of ConsumableType
    | Special of SpecialType
    | Interact // general interaction such as talking to NPCs

type WeaponType =
    | BentSword
    | WindCutter
    | BFS

type WeaponSubtype =
    | Melee
    | Sword
    | Axe
    | Bow
    | Staff
    | Rod

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

type RelicType =
    | LeatherBoots
    | BlingRing
    | MagicPorkRinds

type RelicData =
    { RelicType : RelicType // key
      ShieldBase : int
      CounterBase : int }

type AllyType =
    | Player

type EnemyType =
    | Goblin
    | Snake
    | Zommie

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

type DrainData =
    { EffectType : EffectType
      Percentage : single }

type ActionData =
    { ActionType : ActionType
      EffectType : EffectType
      SpecialPointCost : int
      SuccessRate : single
      Curative : bool
      ElementType : ElementType
      AddStatusType : StatusType Set
      RemoveStatusType : StatusType Set
      TargetType : TargetType }

type ConsumableData =
    { ConsumableData : unit }

type KeyItemData =
    { KeyItemData : unit }

type ItemData =
    { ItemType : ItemType // key
      Description : string }

type RewardData =
    { Gold : int }

type CharacterData =
    { CharacterType : CharacterType // key
      CharacterName : string
      BaseActions : ActionData list // base actions for all instances of character
      Reward : RewardData }

type ControlType =
    | PlayerControlled
    | Chaos
    | Uncontrolled

type CharacterState =
    { CharacterType : CharacterType
      ControlType : ControlType
      ExpPoints : int
      HitPoints : int
      SpecialPoints : int
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      Statuses : StatusType Set
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Relics : RelicType list }

    static member empty =
        { CharacterType = Ally Player
          ControlType = PlayerControlled
          ExpPoints = 0
          HitPoints = 10 // note this is an arbitrary number as hp max is calculated
          SpecialPoints = 1 // sp max is calculated
          PowerBuff = 1.0f // rate at which power is buffed / debuffed
          MagicBuff = 1.0f // rate at which magic is buffed / debuffed
          ShieldBuff = 1.0f // rate at which shield is buffed / debuffed
          CounterBuff = 1.0f // rate at which counter is buffed / debuffed
          Statuses = Set.empty<StatusType>
          WeaponOpt = Option<WeaponType>.None
          ArmorOpt = Option<ArmorType>.None
          Relics = [] } // level is calculated from base experience + added experience

    member this.IsAlly =
        match this.CharacterType with Ally _ -> true | Enemy _ -> false

    member this.IsEnemy =
        not this.IsAlly

    member this.Level =
        if this.ExpPoints < 8 then 1
        elif this.ExpPoints < 16 then 2
        elif this.ExpPoints < 24 then 3
        elif this.ExpPoints < 36 then 4
        elif this.ExpPoints < 50 then 5
        elif this.ExpPoints < 75 then 6
        elif this.ExpPoints < 100 then 6
        elif this.ExpPoints < 150 then 8
        elif this.ExpPoints < 225 then 9
        elif this.ExpPoints < 350 then 10
        elif this.ExpPoints < 500 then 11
        elif this.ExpPoints < 750 then 12
        elif this.ExpPoints < 1000 then 13
        elif this.ExpPoints < 1500 then 14
        elif this.ExpPoints < 2250 then 15
        elif this.ExpPoints < 3500 then 16
        elif this.ExpPoints < 5000 then 17
        elif this.ExpPoints < 7500 then 18
        elif this.ExpPoints < 10000 then 19
        else 20