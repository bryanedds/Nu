namespace OmniBlade
open System
open System.IO
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; StructuralComparison>] Advent =
    | Opened of Guid
    | DebugSwitch
    | DebugSwitch2
    | KilledFinalBoss
    | SavedPrincess

type [<StructuralEquality; StructuralComparison>] Direction =
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

type [<StructuralEquality; StructuralComparison>] EffectType =
    | Physical
    | Magical

type [<StructuralEquality; StructuralComparison>] ElementType =
    | Fire // beats ice, average scalar
    | Ice // beats fire, lightning; average scaler
    | Lightning // beats water, average scalar
    | Water // beats lightning, average scalar
    | Dark // beats light, stronger scalar
    | Light // beats dark, weaker scalar
    | Earth // beats nothing, strongest scalar

type [<StructuralEquality; StructuralComparison>] StatusType =
    | DefendStatus // also applies a perhaps stackable buff for attributes such as countering or magic power depending on class
    | PoisonStatus
    | MuteStatus
    | SleepStatus

type [<StructuralEquality; StructuralComparison>] EquipmentType =
    | Weapon
    | Armor
    | Accessory

type [<StructuralEquality; StructuralComparison>] ConsumableType =
    | GreenHerb
    | RedHerb

type [<StructuralEquality; StructuralComparison>] KeyItemType =
    | BrassKey

type [<StructuralEquality; StructuralComparison>] ItemType =
    | Equipment of EquipmentType
    | Consumable of ConsumableType
    | KeyItem of KeyItemType
    static member getName item =
        match item with
        | Equipment ty -> string ty
        | Consumable ty -> string ty
        | KeyItem ty -> string ty

type [<StructuralEquality; StructuralComparison>] AimType =
    | EnemyAim of bool // healthy (N/A)
    | AllyAim of bool // healthy
    | AnyAim of bool // healthy
    | NoAim

type [<StructuralEquality; StructuralComparison>] TargetType =
    | SingleTarget of AimType
    | ProximityTarget of AimType * single
    | RadialTarget of AimType * single
    | LineTarget of AimType * single
    | AllTarget of AimType

type [<StructuralEquality; StructuralComparison>] TechType =
    | Critical
    | Cyclone
    | Bolt
    | Tremor

type [<StructuralEquality; StructuralComparison>] ActionType =
    | Attack
    | Consume of ConsumableType
    | Tech of TechType
    | Wound

type [<StructuralEquality; StructuralComparison>] ArchetypeType =
    | Squire
    | Mage
    | Fighter
    | Brawler
    | Wizard
    | Cleric
    | Goblin

type WeaponType =
    string

type [<StructuralEquality; StructuralComparison>] WeaponSubtype =
    | Melee
    | Sword
    | Heavesword
    | Bow
    | Staff
    | Rod

type ArmorType =
    string

type [<StructuralEquality; StructuralComparison>] ArmorSubtype =
    | Robe
    | Vest
    | Mail
    | Pelt

type AccessoryType =
    string

type [<StructuralEquality; StructuralComparison>] ShopType =
    | WeaponShopkeep of int // level
    | ArmorShopKeep of int // level
    | AccessoryShopKeep of int // level

type [<StructuralEquality; StructuralComparison>] ShopkeepAppearanceType =
    | Male
    | Female
    | Fancy

type [<StructuralEquality; StructuralComparison>] LockType =
    | BrassKey

type [<StructuralEquality; StructuralComparison>] ChestType =
    | WoodenChest
    | BrassChest

type [<StructuralEquality; StructuralComparison>] DoorType =
    | WoodenDoor

type [<StructuralEquality; StructuralComparison>] NpcType =
    | VillageMan
    | VillageWoman
    | VillageBoy
    | VillageGirl

type [<StructuralEquality; StructuralComparison>] ShopkeepType =
    | ShopkeepMan

type [<StructuralEquality; StructuralComparison>] FieldType =
    | DebugRoom
    | DebugRoom2

type [<StructuralEquality; StructuralComparison>] SwitchType =
    | ThrowSwitch
    
type [<StructuralEquality; StructuralComparison>] SensorType =
    | AirSensor
    | HiddenSensor
    | StepPlateSensor

type [<StructuralEquality; StructuralComparison>] BattleType =
    | DebugBattle

type [<StructuralEquality; StructuralComparison>] PoiseType =
    | Poising
    | Defending
    | Charging

type [<StructuralEquality; StructuralComparison>] AnimationType =
    | LoopedWithDirection
    | LoopedWithoutDirection
    | SaturatedWithDirection
    | SaturatedWithoutDirection

type [<StructuralEquality; StructuralComparison>] CharacterAnimationCycle =
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

type [<StructuralEquality; StructuralComparison>] AllyType =
    | Finn
    | Glenn

type [<StructuralEquality; StructuralComparison>] EnemyType =
    | Goblin

type [<StructuralEquality; StructuralComparison>] CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

type [<StructuralEquality; NoComparison>] WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int
      Description : string }

type [<StructuralEquality; NoComparison>] ArmorData =
    { ArmorType : ArmorType // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      TechPointsBase : int
      Description : string }

type [<StructuralEquality; NoComparison>] AccessoryData =
    { AccessoryType : AccessoryType // key
      ShieldBase : int
      CounterBase : int
      Description : string }

type [<StructuralEquality; NoComparison>] ConsumableData =
    { ConsumableType : ConsumableType // key
      Scalar : single
      Curative : bool
      AimType : AimType
      Description : string }

type [<StructuralEquality; NoComparison>] TechData =
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

type [<StructuralEquality; NoComparison>] ArchetypeData =
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

type [<StructuralEquality; NoComparison>] TechAnimationData =
    { TechType : TechType // key
      TechStart : int64
      TechingStart : int64
      AffectingStart : int64
      AffectingStop : int64
      TechingStop : int64
      TechStop : int64 }

type [<StructuralEquality; NoComparison>] KeyItemData =
    { KeyItemData : unit }

type [<StructuralEquality; NoComparison>] DoorData =
    { DoorType : DoorType // key
      DoorKeyOpt : string option
      OpenImage : Image AssetTag
      ClosedImage : Image AssetTag }

type [<StructuralEquality; NoComparison>] ShopkeepData =
    { ShopkeepType : ShopkeepType // key
      ShopkeepAppearanceType : ShopkeepAppearanceType
      ShopkeepItems : ItemType Set
      ShopkeepGreet : string list
      ShopkeepFarewell : string list }

type [<StructuralEquality; NoComparison>] PropData =
    | Chest of ChestType * ItemType * Guid * BattleType option * Advent Set * Advent Set
    | Door of DoorType * Advent Set * Advent Set // for simplicity, we'll just have north / south doors
    | Portal of int * FieldType * Vector2 * Direction * Advent Set // leads to a different portal
    | Switch of SwitchType * Advent Set * Advent Set // anything that can affect another thing on the field through interaction
    | Sensor of SensorType * BodyShape option * Advent Set * Advent Set // anything that can affect another thing on the field through traversal
    | Npc of NpcType * Direction * (string * Advent Set * Advent Set) list * Advent Set
    | Shopkeep of ShopkeepType * Direction * Advent Set
    static member empty = Chest (WoodenChest, Consumable GreenHerb, Gen.idEmpty, None, Set.empty, Set.empty)

type [<StructuralEquality; NoComparison>] FieldData =
    { FieldType : FieldType // key
      FieldTileMap : TileMap AssetTag
      FieldProps : PropData list
      FieldSongOpt : Song AssetTag option
      FieldAmbienceOpt : Song AssetTag option
      FieldBackgroundColor : Color }

type [<StructuralEquality; NoComparison>] EnemyData =
    { EnemyType : EnemyType // key
      EnemyPosition : Vector2 }

type [<StructuralEquality; NoComparison>] BattleData =
    { BattleType : BattleType // key
      BattleAllyPositions : Vector2 list
      BattleEnemies : EnemyData list
      BattleSongOpt : Song AssetTag option }

type [<StructuralEquality; NoComparison>] CharacterData =
    { CharacterType : CharacterType // key
      ArchetypeType : ArchetypeType
      LevelBase : int
      AnimationSheet : Image AssetTag
      Description : string }

type [<StructuralEquality; NoComparison>] CharacterAnimationData =
    { CharacterAnimationCycle : CharacterAnimationCycle // key
      AnimationType : AnimationType
      LengthOpt : int64 option
      Run : int
      Stutter : int64
      Offset : Vector2i }

[<AutoOpen>]
module Data =

    type [<StructuralEquality; NoComparison>] Data =
        { Weapons : Map<WeaponType, WeaponData>
          Armors : Map<ArmorType, ArmorData>
          Accessories : Map<AccessoryType, AccessoryData>
          Consumables : Map<ConsumableType, ConsumableData>
          Techs : Map<TechType, TechData>
          Archetypes : Map<ArchetypeType, ArchetypeData>
          Characters : Map<CharacterType, CharacterData>
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
          Fields = readSheet Assets.FieldDataFilePath (fun data -> data.FieldType)
          Battles = readSheet Assets.BattleDataFilePath (fun data -> data.BattleType)
          TechAnimations = readSheet Assets.TechAnimationDataFilePath (fun data -> data.TechType)
          CharacterAnimations = readSheet Assets.CharacterAnimationDataFilePath (fun data -> data.CharacterAnimationCycle) }

    let data =
        lazy (readFromFiles ())

type Data = Data.Data