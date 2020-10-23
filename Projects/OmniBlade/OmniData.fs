// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open System.IO
open FSharpx.Collections
open Prime
open Nu

type Advent =
    | Opened of Guid
    | DebugSwitch
    | DebugSwitch2
    | KilledFinalBoss
    | SavedPrincess

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
    | WeaponType of string
    | ArmorType of string
    | AccessoryType of string

type ConsumableType =
    | GreenHerb
    | RedHerb

type KeyItemType =
    | BrassKey

type ItemType =
    | Consumable of ConsumableType
    | Equipment of EquipmentType
    | KeyItem of KeyItemType
    | Stash of int
    static member getName item =
        match item with
        | Consumable ty -> string ty
        | Equipment ty -> match ty with WeaponType name | ArmorType name | AccessoryType name -> name
        | KeyItem ty -> string ty
        | Stash gold -> string gold + "G"

type AimType =
    | EnemyAim of bool // healthy (N/A)
    | AllyAim of bool // healthy
    | AnyAim of bool // healthy
    | NoAim

type TargetType =
    | SingleTarget of AimType
    | ProximityTarget of AimType * single
    | RadialTarget of AimType * single
    | LineTarget of AimType * single
    | AllTarget of AimType

type TechType =
    | Critical
    | Cyclone
    | Bolt
    | Tremor

type ActionType =
    | Attack
    | Consume of ConsumableType
    | Tech of TechType
    | Wound

type ArchetypeType =
    | Squire
    | Mage
    | Fighter
    | Brawler
    | Wizard
    | Cleric
    | Goblin

type WeaponSubtype =
    | Melee
    | Sword
    | Heavesword
    | Bow
    | Staff
    | Rod

type ArmorSubtype =
    | Robe
    | Vest
    | Mail
    | Pelt

type ShopType =
    | PodunkChemist
    | PodunkArmory

type ShopkeepAppearanceType =
    | Male
    | Female
    | Fancy

type LockType =
    | BrassKey

type ChestType =
    | WoodenChest
    | BrassChest

type DoorType =
    | WoodenDoor

type NpcType =
    | VillageMan
    | VillageWoman
    | VillageBoy
    | VillageGirl

type ShopkeepType =
    | ShopkeepMan

type FieldType =
    | DebugRoom
    | DebugRoom2

type SwitchType =
    | ThrowSwitch
    
type SensorType =
    | AirSensor
    | HiddenSensor
    | StepPlateSensor

type BattleType =
    | DebugBattle

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
    | Finn
    | Glenn

type EnemyType =
    | Goblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

    static member getName characterType =
        match characterType with
        | Ally ty -> string ty
        | Enemy ty -> string ty

type WeaponData =
    { WeaponType : string // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int
      Cost : int
      Description : string }

type ArmorData =
    { ArmorType : string // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      TechPointsBase : int
      Cost : int
      Description : string }

type AccessoryData =
    { AccessoryType : string // key
      ShieldBase : int
      CounterBase : int
      Cost : int
      Description : string }

type ConsumableData =
    { ConsumableType : ConsumableType // key
      Scalar : single
      Curative : bool
      AimType : AimType
      Cost : int
      Description : string }

type TechData =
    { TechType : TechType // key
      TechCost : int
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

type ArchetypeData =
    { ArchetypeType : ArchetypeType // key
      Stamina : single // hit points scalar
      Strength : single // power scalar
      Focus : single // tech points scalar
      Intelligence : single // magic scalar
      Toughness : single // shield scalar
      Wealth : single // gold scalar
      Mythos : single // exp scala
      WeaponSubtype : WeaponSubtype
      ArmorSubtype : ArmorSubtype
      Techs : Map<int, TechType> } // tech availability according to level

type TechAnimationData =
    { TechType : TechType // key
      TechStart : int64
      TechingStart : int64
      AffectingStart : int64
      AffectingStop : int64
      TechingStop : int64
      TechStop : int64 }

type KeyItemData =
    { KeyItemData : unit }

type [<NoComparison>] DoorData =
    { DoorType : DoorType // key
      DoorKeyOpt : string option
      OpenImage : Image AssetTag
      ClosedImage : Image AssetTag }

type ShopData =
    { ShopType : ShopType // key
      ShopItems : ItemType Set }

type [<NoComparison>] PropData =
    | Chest of ChestType * ItemType * Guid * BattleType option * Advent Set * Advent Set
    | Door of DoorType * Advent Set * Advent Set // for simplicity, we'll just have north / south doors
    | Portal of int * FieldType * Vector2 * Direction * Advent Set // leads to a different portal
    | Switch of SwitchType * Advent Set * Advent Set // anything that can affect another thing on the field through interaction
    | Sensor of SensorType * BodyShape option * Advent Set * Advent Set // anything that can affect another thing on the field through traversal
    | Npc of NpcType * Direction * (string * Advent Set * Advent Set) list * Advent Set
    | Shopkeep of ShopkeepType * Direction * ShopType * Advent Set
    static member empty = Chest (WoodenChest, Consumable GreenHerb, Gen.idEmpty, None, Set.empty, Set.empty)

type [<NoComparison>] FieldData =
    { FieldType : FieldType // key
      FieldTileMap : TileMap AssetTag
      FieldProps : PropData list
      FieldSongOpt : Song AssetTag option
      FieldAmbienceOpt : Song AssetTag option
      FieldBackgroundColor : Color }

type [<NoComparison>] EnemyData =
    { EnemyType : EnemyType // key
      EnemyPosition : Vector2 }

type [<NoComparison>] BattleData =
    { BattleType : BattleType // key
      BattleAllyPositions : Vector2 list
      BattleEnemies : EnemyData list
      BattleSongOpt : Song AssetTag option }

type [<NoComparison>] CharacterData =
    { CharacterType : CharacterType // key
      ArchetypeType : ArchetypeType
      LevelBase : int
      AnimationSheet : Image AssetTag
      MugOpt : Image AssetTag option
      GoldScalar : single
      ExpScalar : single
      Description : string }

type CharacterAnimationData =
    { CharacterAnimationCycle : CharacterAnimationCycle // key
      AnimationType : AnimationType
      LengthOpt : int64 option
      Run : int
      Stutter : int64
      Offset : Vector2i }

[<AutoOpen>]
module Data =

    type [<NoComparison>] Data =
        { Weapons : Map<string, WeaponData>
          Armors : Map<string, ArmorData>
          Accessories : Map<string, AccessoryData>
          Consumables : Map<ConsumableType, ConsumableData>
          Techs : Map<TechType, TechData>
          Archetypes : Map<ArchetypeType, ArchetypeData>
          Characters : Map<CharacterType, CharacterData>
          Shops : Map<ShopType, ShopData>
          Fields : Map<FieldType, FieldData>
          Battles : Map<BattleType, BattleData>
          TechAnimations : Map<TechType, TechAnimationData>
          CharacterAnimations : Map<CharacterAnimationCycle, CharacterAnimationData> }

    let private readSheet<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        let text = File.ReadAllText filePath
        let symbol = flip (Symbol.fromStringCsv true) (Some filePath) text
        let value = symbolToValue<'d list> symbol
        Map.ofListBy (fun data -> getKey data, data) value

    let private readFromFiles () =
        { Weapons = readSheet Assets.WeaponDataFilePath (fun data -> data.WeaponType)
          Armors = readSheet Assets.ArmorDataFilePath (fun data -> data.ArmorType)
          Accessories = readSheet Assets.AccessoryDataFilePath (fun data -> data.AccessoryType)
          Consumables = readSheet Assets.ConsumableDataFilePath (fun data -> data.ConsumableType)
          Techs = readSheet Assets.TechDataFilePath (fun data -> data.TechType)
          Archetypes = readSheet Assets.ArchetypeDataFilePath (fun data -> data.ArchetypeType)
          Characters = readSheet Assets.CharacterDataFilePath (fun data -> data.CharacterType)
          Shops = readSheet Assets.ShopDataFilePath (fun data -> data.ShopType)
          Fields = readSheet Assets.FieldDataFilePath (fun data -> data.FieldType)
          Battles = readSheet Assets.BattleDataFilePath (fun data -> data.BattleType)
          TechAnimations = readSheet Assets.TechAnimationDataFilePath (fun data -> data.TechType)
          CharacterAnimations = readSheet Assets.CharacterAnimationDataFilePath (fun data -> data.CharacterAnimationCycle) }

    let data =
        lazy (readFromFiles ())

type Data = Data.Data