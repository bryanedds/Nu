namespace OmniBlade
open System
open System.IO
open FSharpx.Collections
open Prime
open Nu

type Direction =
    | Downward
    | Leftward
    | Upward
    | Rightward

    static member fromVector2 (v2 : Vector2) =
        let angle = double (atan2 v2.Y v2.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.75 || angle <= Math.PI * 0.25 then  Rightward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then  Upward
            elif    angle > Math.PI * 0.75 && angle <= Math.PI * 1.25 then  Leftward
            else                                                            Downward
        direction

type EffectType =
    | Physical
    | Magical

type ElementType =
    | Fire // beats ice, average scalar
    | Ice // beats fire, lightning; average scaler
    | Lightning // beats water, average scalar
    | Water // beats lightning, average scalar
    | Dark // beats light, stronger scalar
    | Light // beats dark, weaker scalar
    | Earth // beats nothing, strongest scalar

type StatusType =
    | DefendStatus // also applies a perhaps stackable buff for attributes such as countering or magic power depending on class
    | PoisonStatus
    | MuteStatus
    | SleepStatus

type EquipmentType =
    | Weapon
    | Armor
    | Accessory

type ConsumableType =
    | GreenHerb
    | RedHerb

type KeyItemType =
    | BrassKey

type ItemType =
    | Equipment of EquipmentType
    | Consumable of ConsumableType
    | KeyItem of KeyItemType

type AimType =
    | EnemyAim
    | AllyAim of bool // healthy
    | AnyAim of bool // healthy
    | NoAim

type TargetType =
    | SingleTarget of AimType
    | RadialTarget of AimType
    | LineTarget of AimType
    | AllTarget of AimType

type SpecialType =
    | HeadSlash
    | Cyclone
    | Tremor
    | Bolt

type ActionType =
    | Attack
    | Consume of ConsumableType
    | Special of SpecialType
    | Wound

type WeaponType =
    string

type WeaponSubtype =
    | Melee
    | Sword
    | Axe
    | Bow
    | Staff
    | Rod

type ArmorType =
    string

type ArmorSubtype =
    | Robe
    | Vest
    | Mail

type AccessoryType =
    string

type PoiseType =
    | Poising
    | Defending
    | Charging

type AnimationType =
    | LoopedWithDirection
    | LoopedWithoutDirection
    | SaturatedWithDirection
    | SaturatedWithoutDirection

type CharacterAnimationCycle =
    | WalkCycle
    | CelebrateCycle
    | ReadyCycle
    | PoiseCycle of PoiseType
    | AttackCycle
    | CastCycle
    | SpinCycle
    | DamageCycle
    | IdleCycle
    | Cast2Cycle
    | WhirlCycle
    | BuryCycle
    | FlyCycle
    | HopForwardCycle
    | HopBackCycle
    | WoundCycle

type AllyType =
    | Jinn
    | Glenn

type EnemyType =
    | Goblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

type AccessoryData =
    { AccessoryType : AccessoryType // key
      ShieldBase : int
      CounterBase : int
      Description : string }

type WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int
      Description : string }

type ArmorData =
    { ArmorType : ArmorType // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      SpecialPointsBase : int
      Description : string }

type SpecialData =
    { SpecialType : SpecialType // key
      SpecialCost : int
      EffectType : EffectType
      Scalar : single
      SuccessRate : single
      Curative : bool
      Cancels : bool
      Absorb : single // percentage of outcome that is absorbed by the caster
      ElementTypeOpt : ElementType option
      StatusesAdded : StatusType Set
      StatusesRemoved : StatusType Set
      TargetType : TargetType
      Description : string }

type ConsumableData =
    { ConsumableType : ConsumableType // key
      Scalar : single
      Curative : bool
      AimType : AimType
      Description : string }

type KeyItemData =
    { KeyItemData : unit }

type RewardData =
    { Gold : int }

type CharacterData =
    { CharacterType : CharacterType // key
      BaseSpecials : SpecialData list // base actions for all instances of character
      Reward : RewardData
      Description : string }

type CharacterAnimationData =
    { CharacterAnimationCycle : CharacterAnimationCycle // key
      AnimationType : AnimationType
      Run : int
      Stutter : int64
      LengthOpt : int64
      Offset : Vector2i }

type Rom =
    { Weapons : Map<WeaponType, WeaponData>
      Armors : Map<ArmorType, ArmorData>
      Accessories : Map<AccessoryType, AccessoryData>
      Specials : Map<SpecialType, SpecialData>
      Consumables : Map<ConsumableType, ConsumableData>
      Characters : Map<CharacterType, CharacterData>
      CharacterAnimationData : Map<CharacterAnimationCycle, CharacterAnimationData> }

    static member readSheet<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        File.ReadAllText filePath |>
        flip (Symbol.fromStringCsv true) (Some filePath) |>
        symbolToValue<'d list> |>
        Map.ofListBy (fun data -> getKey data, data)

    static member readFromFiles () =
        { Weapons = Rom.readSheet Assets.WeaponDataFilePath (fun data -> data.WeaponType)
          Armors = Rom.readSheet Assets.ArmorDataFilePath (fun data -> data.ArmorType)
          Accessories = Rom.readSheet Assets.AccessoryDataFilePath (fun data -> data.AccessoryType)
          Specials = Rom.readSheet Assets.SpecialDataFilePath (fun data -> data.SpecialType)
          Consumables = Rom.readSheet Assets.ConsumableDataFilePath (fun data -> data.ConsumableType)
          Characters = Map.empty
          CharacterAnimationData = Rom.readSheet Assets.CharacterAnimationDataFilePath (fun data -> data.CharacterAnimationCycle) }