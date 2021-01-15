// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open System.IO
open FSharpx.Collections
open TiledSharp
open Prime
open Nu

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

    static member isFriendly index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

type Advent =
    | DebugSwitch
    | DebugSwitch2
    | Opened of Guid
    | FireGoblinDefeated

type Direction =
    | Downward
    | Leftward
    | Upward
    | Rightward

    static member ofVector2 (v2 : Vector2) =
        let angle = double (atan2 v2.Y v2.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.75 || angle <= Math.PI * 0.25 then  Rightward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then  Upward
            elif    angle > Math.PI * 0.75 && angle <= Math.PI * 1.25 then  Leftward
            else                                                            Downward
        direction

    static member toVector2 direction =
        match direction with
        | Rightward -> v2Right
        | Upward -> v2Up
        | Leftward -> v2Left
        | Downward -> v2Down

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

type [<CustomEquality; CustomComparison>] StatusType =
    | Poison
    | Silence
    | Sleep
    | Time of bool // true = Haste, false = Slow
    | Counter of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Power of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Magic of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Shield of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Provoke
    | ProvokeOne of CharacterIndex

    static member enumerate this =
        match this with
        | Poison -> 0
        | Silence -> 1
        | Sleep -> 2
        | Time _ -> 3
        | Counter (_, _) -> 4
        | Power (_, _) -> 5
        | Magic (_, _) -> 6
        | Shield (_, _) -> 7
        | Provoke -> 8
        | ProvokeOne i -> 9 + (match i with AllyIndex i -> i | EnemyIndex i -> i) <<< 6

    static member compare this that =
        compare
            (StatusType.enumerate this)
            (StatusType.enumerate that)

    interface StatusType IComparable with
        member this.CompareTo that =
            StatusType.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? StatusType as that -> (this :> StatusType IComparable).CompareTo that
            | _ -> failwithumf ()

    override this.Equals that =
        match that with
        | :? StatusType as that -> StatusType.enumerate this = StatusType.enumerate that
        | _ -> false

    override this.GetHashCode () =
        StatusType.enumerate this

type EquipmentType =
    | WeaponType of string
    | ArmorType of string
    | AccessoryType of string // TODO: might want to make this a static type since many accessories will involve custom coding.

type ConsumableType =
    | GreenHerb
    | RedHerb
    | GoldHerb

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
    | Slash
    | Bolt
    | Tremor

type ActionType =
    | Attack
    | Defend
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
    | Chemist
    | Armory

type ShopkeepAppearanceType =
    | Male
    | Female
    | Fancy

type BattleType =
    | DebugBattle
    | CaveBattle
    | CaveBattle2
    | CaveBattle3
    | CaveBattle4
    | FireGoblinBattle

type EncounterType =
    | DebugEncounter
    | CaveEncounter

type LockType =
    | BrassKey

type ChestType =
    | WoodenChest
    | BrassChest

type DoorType =
    | WoodenDoor

type PortalType =
    | Center
    | North
    | East
    | South
    | West
    | NE
    | SE
    | NW
    | SW
    | IX of int

type NpcType =
    | VillageMan
    | VillageWoman
    | VillageBoy
    | VillageGirl
    | FireGoblinNpc

type NpcSpecialty =
    | NoSpecialty
    | FireGoblinSpecialty
    static member exists advents specialty =
        match specialty with
        | NoSpecialty -> true
        | FireGoblinSpecialty -> not (Set.contains FireGoblinDefeated advents)
    static member getBattleTypeOpt advents specialty =
        match specialty with
        | NoSpecialty -> None
        | FireGoblinSpecialty -> if not (Set.contains FireGoblinDefeated advents) then Some (Set.singleton FireGoblinDefeated, FireGoblinBattle) else None

type ShopkeepType =
    | ShopkeepMan

type FieldType =
    | DebugRoom
    | DebugRoom2
    | TombInner
    | TombOuter
    | Cave

type SwitchType =
    | ThrowSwitch
    
type SensorType =
    | AirSensor
    | HiddenSensor
    | StepPlateSensor

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
    | SlashCycle
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
    | BlueGoblin
    | PoisonGoblin
    | FireGoblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

    static member getName characterType =
        match characterType with
        | Ally ty -> string ty
        | Enemy ty -> string ty

[<RequireQualifiedAccess>]
module OmniSeedState =

    type OmniSeedState =
        private
            { RandSeedState : uint64 }
    
    let rotate fieldType state =
        match fieldType with
        | DebugRoom | DebugRoom2 | TombInner | TombOuter -> state.RandSeedState
        | Cave -> rotl64 1 state.RandSeedState

    let makeFromSeedState randSeedState =
        { RandSeedState = randSeedState }

    let make () =
        { RandSeedState = Rand.DefaultSeedState }

type OmniSeedState = OmniSeedState.OmniSeedState

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

type [<NoEquality; NoComparison>] DoorData =
    { DoorType : DoorType // key
      DoorKeyOpt : string option
      OpenImage : Image AssetTag
      ClosedImage : Image AssetTag }

type [<NoEquality; NoComparison>] ShopData =
    { ShopType : ShopType // key
      ShopItems : ItemType list }

type [<NoEquality; NoComparison>] PropData =
    | Chest of ChestType * ItemType * Guid * BattleType option * Advent Set * Advent Set
    | Door of DoorType * Advent Set * Advent Set // for simplicity, we'll just have north / south doors
    | Portal of PortalType * Direction * FieldType * PortalType * Advent Set // leads to a different portal
    | Switch of SwitchType * Advent Set * Advent Set // anything that can affect another thing on the field through interaction
    | Sensor of SensorType * BodyShape option * Advent Set * Advent Set // anything that can affect another thing on the field through traversal
    | Npc of NpcType * NpcSpecialty * Direction * (string * Advent Set * Advent Set) list * Advent Set
    | Shopkeep of ShopkeepType * Direction * ShopType * Advent Set
    | SavePoint
    | ChestSpawn
    | EmptyProp

type [<NoEquality; NoComparison>] PropDescriptor =
    { PropBounds : Vector4
      PropElevation : single
      PropData : PropData
      PropId : int }

type [<NoEquality; NoComparison>] FieldTileMap =
    | FieldStatic of TileMap AssetTag
    | FieldRandom of int * single * OriginRand * string

type [<NoEquality; NoComparison>] FieldData =
    { FieldType : FieldType // key
      FieldTileMap : FieldTileMap
      FieldBackgroundColor : Color
      FieldSongOpt : Song AssetTag option
      EncounterTypeOpt : EncounterType option
      Treasures : ItemType list }

[<RequireQualifiedAccess>]
module FieldData =

    let mutable tileMapsMemoized = Map.empty<uint64 * FieldType, TmxMap option>
    let mutable propObjectsMemoized = Map.empty<uint64 * FieldType, (TmxMap * TmxObjectGroup * TmxObject) list>
    let mutable propsMemoized = Map.empty<uint64 * FieldType, PropDescriptor list>

    let objectToPropOpt (object : TmxObject) (group : TmxObjectGroup) (tileMap : TmxMap) =
        let propPosition = v2 (single object.X) (single tileMap.Height * single tileMap.TileHeight - single object.Y) // invert y
        let propSize = v2 (single object.Width) (single object.Height)
        let propBounds = v4Bounds propPosition propSize
        let propElevation =
            match group.Properties.TryGetValue Constants.TileMap.ElevationPropertyName with
            | (true, elevationStr) -> Constants.Field.ForegroundElevation + scvalue elevationStr
            | (false, _) -> Constants.Field.ForegroundElevation
        match object.Properties.TryGetValue Constants.TileMap.InfoPropertyName with
        | (true, propDataStr) ->
            let propData = scvalue propDataStr
            Some { PropBounds = propBounds; PropElevation = propElevation; PropData = propData; PropId = object.Id }
        | (false, _) -> None

    let inflateProp prop (treasures : ItemType FStack) rand =
        match prop.PropData with
        | ChestSpawn ->
            let (probability, rand) = Rand.nextSingleUnder 1.0f rand
            if probability < Constants.Field.TreasureProbability then
                let (treasure, treasures, rand) =
                    if FStack.notEmpty treasures then
                        let (index, rand) = Rand.nextIntUnder (FStack.length treasures) rand
                        (FStack.index index treasures, FStack.removeAt index treasures, rand)
                    else (Consumable GreenHerb, treasures, rand)
                let (id, rand) = let (i, rand) = Rand.nextInt rand in let (j, rand) = Rand.nextInt rand in (Gen.idFromInts i j, rand)
                let prop = { prop with PropData = Chest (WoodenChest, treasure, id, None, Set.empty, Set.empty) }
                (prop, treasures, rand)
            else ({ prop with PropData = EmptyProp }, treasures, rand)
        | _ -> (prop, treasures, rand)

    let tryGetTileMap omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey tileMapsMemoized with
        | None ->
            let tileMapOpt =
                match fieldData.FieldTileMap with
                | FieldStatic fieldAsset ->
                    match World.tryGetTileMapMetadata fieldAsset world with
                    | Some (_, _, tileMap) -> Some tileMap
                    | None -> None
                | FieldRandom (walkLength, bias, origin, fieldPath) ->
                    let rand = Rand.makeFromSeedState rotatedSeedState
                    let (cursor, mapRand, _) = MapRand.makeFromRand walkLength bias Constants.Field.MapRandSize origin rand
                    let mapTmx = MapRand.toTmx fieldPath origin cursor mapRand
                    Some mapTmx
            tileMapsMemoized <- Map.add memoKey tileMapOpt tileMapsMemoized
            tileMapOpt
        | Some tileMapOpt -> tileMapOpt

    let getPropObjects omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey propObjectsMemoized with
        | None ->
            let propObjects =
                match tryGetTileMap omniSeedState fieldData world with
                | Some tileMap ->
                    if tileMap.ObjectGroups.Contains Constants.Field.PropsLayerName then
                        let group = tileMap.ObjectGroups.Item Constants.Field.PropsLayerName
                        enumerable<TmxObject> group.Objects |> Seq.map (fun propObject -> (tileMap, group, propObject)) |> Seq.toList
                    else []
                | None -> []
            propObjectsMemoized <- Map.add memoKey propObjects propObjectsMemoized
            propObjects
        | Some propObjects -> propObjects

    let getProps omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey propsMemoized with
        | None ->
            let propObjects = getPropObjects omniSeedState fieldData world
            let propsUninflated =
                propObjects |>
                List.map (fun (tileMap, group, object) -> objectToPropOpt object group tileMap) |>
                List.definitize
            let (props, _, _) =
                List.foldBack (fun prop (props, treasures, rand) ->
                    let (prop, treasures, rand) = inflateProp prop treasures rand
                    let treasures = if FStack.isEmpty treasures then FStack.ofSeq fieldData.Treasures else treasures
                    (prop :: props, treasures, rand))
                    propsUninflated
                    ([], FStack.ofSeq fieldData.Treasures, Rand.makeFromSeedState rotatedSeedState)
            propsMemoized <- Map.add memoKey props propsMemoized
            props
        | Some props -> props

    let getPortals omniSeedState fieldData world =
        let props = getProps omniSeedState fieldData world
        List.filter (fun prop -> match prop.PropData with Portal _ -> true | _ -> false) props

    let tryGetPortal omniSeedState portalType fieldData world =
        let portals = getPortals omniSeedState fieldData world
        List.tryFind (fun prop -> match prop.PropData with Portal (portalType2, _, _, _, _) -> portalType2 = portalType | _ -> failwithumf ()) portals

    let tryGetBattleType omniSeedState avatarPosition (battleTypes : BattleType list) fieldData world =
        match tryGetTileMap omniSeedState fieldData world with
        | Some tmxTileMap ->
            match fieldData.FieldTileMap with
            | FieldRandom (_, _, origin, _) ->
                let tileMapBounds = v4Bounds v2Zero (v2 (single tmxTileMap.Width * single tmxTileMap.TileWidth) (single tmxTileMap.Height * single tmxTileMap.TileHeight))
                let distanceFromOriginMax = let delta = tileMapBounds.Bottom - tileMapBounds.Top in delta.Length ()
                let distanceFromOrigin =
                    match origin with
                    | OriginC -> let delta = avatarPosition - tileMapBounds.Center in delta.Length ()
                    | OriginN -> let delta = avatarPosition - tileMapBounds.Top in delta.Length ()
                    | OriginE -> let delta = avatarPosition - tileMapBounds.Right in delta.Length ()
                    | OriginS -> let delta = avatarPosition - tileMapBounds.Bottom in delta.Length ()
                    | OriginW -> let delta = avatarPosition - tileMapBounds.Left in delta.Length ()
                    | OriginNE -> let delta = avatarPosition - tileMapBounds.TopRight in delta.Length ()
                    | OriginNW -> let delta = avatarPosition - tileMapBounds.TopLeft in delta.Length ()
                    | OriginSE -> let delta = avatarPosition - tileMapBounds.BottomRight in delta.Length ()
                    | OriginSW -> let delta = avatarPosition - tileMapBounds.BottomLeft in delta.Length ()
                let battleTypes = battleTypes
                let battleTypesLength = List.length battleTypes
                let battleIndex = int (single battleTypesLength / distanceFromOriginMax * distanceFromOrigin)
                let battleIndex =
                    if Gen.randomf < Constants.Field.EasierEncounterProbability && battleIndex > 0
                    then inc battleIndex
                    else battleIndex
                if battleIndex >= 0 && battleIndex < battleTypesLength
                then Some battleTypes.[battleIndex]
                else List.tryItem (dec battleTypesLength) battleTypes
            | FieldStatic _ -> Gen.randomItem battleTypes
        | None -> None

type [<NoEquality; NoComparison>] EnemyData =
    { EnemyType : EnemyType // key
      EnemyPosition : Vector2 }

type [<NoEquality; NoComparison>] BattleData =
    { BattleType : BattleType // key
      BattleAllyPositions : Vector2 list
      BattleEnemies : EnemyData list
      BattleSongOpt : Song AssetTag option }

type [<NoEquality; NoComparison>] EncounterData =
    { EncounterType : EncounterType // key
      BattleTypes : BattleType list
      Threshold : single }

type [<NoEquality; NoComparison>] CharacterData =
    { CharacterType : CharacterType // key
      ArchetypeType : ArchetypeType
      LevelBase : int
      AnimationSheet : Image AssetTag
      PortraitOpt : Image AssetTag option
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list
      GoldScalar : single
      ExpScalar : single
      Description : string }

type [<NoEquality; NoComparison>] CharacterAnimationData =
    { CharacterAnimationCycle : CharacterAnimationCycle // key
      AnimationType : AnimationType
      LengthOpt : int64 option
      Run : int
      Delay : int64
      Offset : Vector2i }

[<RequireQualifiedAccess>]
module Data =

    type [<NoEquality; NoComparison>] OmniData =
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
          Encounters : Map<EncounterType, EncounterData>
          TechAnimations : Map<TechType, TechAnimationData>
          CharacterAnimations : Map<CharacterAnimationCycle, CharacterAnimationData> }

    let private readSheet<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        Math.init () // HACK: initializing Math type converters for required type converters in fsx script.
        let text = File.ReadAllText filePath
        let symbol = flip (Symbol.ofStringCsv true) (Some filePath) text
        let value = symbolToValue<'d list> symbol
        Map.ofListBy (fun data -> getKey data, data) value

    let private readFromFiles () =
        { Weapons = readSheet Assets.Field.WeaponDataFilePath (fun data -> data.WeaponType)
          Armors = readSheet Assets.Field.ArmorDataFilePath (fun data -> data.ArmorType)
          Accessories = readSheet Assets.Field.AccessoryDataFilePath (fun data -> data.AccessoryType)
          Consumables = readSheet Assets.Field.ConsumableDataFilePath (fun data -> data.ConsumableType)
          Techs = readSheet Assets.Field.TechDataFilePath (fun data -> data.TechType)
          Archetypes = readSheet Assets.Field.ArchetypeDataFilePath (fun data -> data.ArchetypeType)
          Characters = readSheet Assets.Field.CharacterDataFilePath (fun data -> data.CharacterType)
          Shops = readSheet Assets.Field.ShopDataFilePath (fun data -> data.ShopType)
          Fields = readSheet Assets.Field.FieldDataFilePath (fun data -> data.FieldType)
          Battles = readSheet Assets.Field.BattleDataFilePath (fun data -> data.BattleType)
          Encounters = readSheet Assets.Field.EncounterDataFilePath (fun data -> data.EncounterType)
          TechAnimations = readSheet Assets.Field.TechAnimationDataFilePath (fun data -> data.TechType)
          CharacterAnimations = readSheet Assets.Field.CharacterAnimationDataFilePath (fun data -> data.CharacterAnimationCycle) }

    let Value =
        readFromFiles ()