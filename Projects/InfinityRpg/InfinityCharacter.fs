namespace InfinityRpg
open Prime

type CharacterIndex =
    | EnemyIndex of int
    | PlayerIndex

    member this.IsEnemy =
        match this with EnemyIndex _ -> true | PlayerIndex -> false

    member this.IsAlly =
        not this.IsEnemy

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

type ControlType =
    | PlayerControlled
    | Chaos
    | Uncontrolled

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

type AccessoryType =
    | LeatherBoots
    | BlingRing
    | MagicPorkRinds

type AccessoryData =
    { AccessoryType : AccessoryType // key
      ShieldBase : int
      CounterBase : int }

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

type [<ReferenceEquality; NoComparison>] Character =
    { CharacterIndex : CharacterIndex
      CharacterType : CharacterType
      ControlType : ControlType
      FacingDirection : Direction
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
      Accessories : AccessoryType list }

    member this.IsAlly =
        match this.CharacterType with Ally _ -> true | Enemy _ -> false

    member this.IsEnemy =
        not this.IsAlly

    member this.IsAlive =
        this.HitPoints > 0
    
    static member updateControlType updater character =
        { character with ControlType = updater character.ControlType }
    
    static member updateFacingDirection updater character =
        { character with FacingDirection = updater character.FacingDirection }
    
    static member updateHitPoints updater character =
        { character with HitPoints = updater character.HitPoints }

    static member empty =
        { CharacterIndex = PlayerIndex
          CharacterType = Ally Player
          ControlType = Uncontrolled
          FacingDirection = Upward
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
          Accessories = [] } // level is calculated from base experience + added experience

    static member makePlayer = { Character.empty with HitPoints = 30; ControlType = PlayerControlled }

    static member makeEnemy index = { Character.empty with CharacterIndex = index; CharacterType = Enemy Goopy; HitPoints = 10; ControlType = Chaos }