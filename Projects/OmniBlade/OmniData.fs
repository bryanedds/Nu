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

    static member toEntityName index =
        match index with
        | AllyIndex i -> "Ally+" + scstring i
        | EnemyIndex i -> "Enemy+" + scstring i

type Advent =
    | DebugSwitch
    | DebugSwitch2
    | Opened of Guid
    | ShadeRecruited
    | MaelRecruited
    | RiainRecruited
    | PericRecruited
    | FireGoblinDefeated
    | HeavyArmorosDefeated
    | CastleUnsealed
    | ForestUnsealed
    | FactoryUnsealed
    | MountainUnsealed
    | DeadSeaUnsealed
    | RuinsUnsealed
    | DesertUnsealed
    | Castle2Unsealed
    | SeasonsUnsealed
    | VolcanoUnsealed

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

    static member ofVector2 (v2 : Vector2) =
        let angle = double (atan2 v2.Y v2.X)
        let angle = if angle < 0.0 then angle + Math.PI * 2.0 else angle
        let direction =
            if      angle > Math.PI * 1.74997 || angle <= Math.PI * 0.25003 then    Rightward
            elif    angle > Math.PI * 0.74997 && angle <= Math.PI * 1.25003 then    Leftward
            elif    angle > Math.PI * 0.25 && angle <= Math.PI * 0.75 then          Upward
            else                                                                    Downward
        direction

    static member toVector2 direction =
        match direction with
        | Upward -> v2Up
        | Rightward -> v2Right
        | Downward -> v2Down
        | Leftward -> v2Left

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
    | Blind
    | Silence
    | Sleep
    | Confuse
    | Previve
    | Time of bool // true = Haste, false = Slow
    | Counter of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Power of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Magic of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Shield of bool * bool // true = Up, false = Down; true = 2, false = 1
    | Provoke of CharacterIndex

    static member enumerate this =
        match this with
        | Poison -> 0
        | Blind -> 1
        | Silence -> 2
        | Sleep -> 3
        | Confuse -> 4
        | Previve -> 5
        | Time _ -> 6
        | Counter (_, _) -> 7
        | Power (_, _) -> 8
        | Magic (_, _) -> 9
        | Shield (_, _) -> 10
        | Provoke i -> 11 + (match i with AllyIndex i -> i | EnemyIndex i -> i) <<< 6

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

type WeaponType =
    | ShortSword
    | Dagger
    | OakRod
    | OakBow
    | Paws
    | BronzeSword
    | BronzeKatana
    | BronzeRod
    | LightBow
    | Claws
    | IronSword
    | IronKatana
    | SightedBow
    | IvoryRod
    | Fangs

type ArmorType =
    | TinMail
    | CottonVest
    | CottonRobe
    | ThinFur
    | BronzeMail
    | LeatherVest
    | LeatherRobe
    | ThickFur
    | IronMail
    | RubberVest
    | SilkRobe
    | ToughHide

type AccessoryType =
    | LeatherBrace

type WeaponSubtype =
    | Melee
    | Sword
    | Knife
    | Rod
    | Bow

type ArmorSubtype =
    | Robe
    | Vest
    | Mail
    | Pelt

type EquipmentType =
    | WeaponType of WeaponType
    | ArmorType of ArmorType
    | AccessoryType of AccessoryType

type ConsumableType =
    | GreenHerb
    | RedHerb
    | GoldHerb
    | Remedy
    | Ether
    | HighEther
    | TurboEther
    | Revive

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
        | Equipment ty -> match ty with WeaponType ty -> string ty | ArmorType ty -> string ty | AccessoryType ty -> string ty
        | KeyItem ty -> string ty
        | Stash gold -> string gold + "G"

type AimType =
    | EnemyAim of bool // healthy (N/A)
    | AllyAim of bool // healthy
    | AnyAim of bool // healthy
    | NoAim

type TargetType =
    | SingleTarget of AimType
    | ProximityTarget of single * AimType
    | RadialTarget of single * AimType
    | LineTarget of single * AimType
    | SegmentTarget of single * AimType
    | VerticalTarget of single * AimType
    | HorizontalTarget of single * AimType
    | AllTarget of AimType

    static member getAimType targetType =
        match targetType with
        | SingleTarget aimType -> aimType
        | ProximityTarget (_, aimType) -> aimType
        | RadialTarget (_, aimType) -> aimType
        | LineTarget (_, aimType) -> aimType
        | SegmentTarget (_, aimType) -> aimType
        | VerticalTarget (_, aimType) -> aimType
        | HorizontalTarget (_, aimType) -> aimType
        | AllTarget aimType -> aimType

type TechType =
    | Critical
    | Slash
    | DarkCritical
    | Cyclone
    | PowerCut
    | SneakCut
    | DoubleCut
    | ProvokeCut
    | Fire
    | Flame
    | Ice
    | Snowball
    | Bolt
    | BoltBeam
    | Stone
    | Quake
    | Aura
    | Empower
    | Enlighten
    | Protect
    | Weaken
    | Muddle
    | ConjureIfrit
    | Slow
    | Purify

type ActionType =
    | Attack
    | Defend
    | Consume of ConsumableType
    | Tech of TechType
    | Wound

type StatureType =
    | SmallStature
    | NormalStature
    | LargeStature
    | HugeStature

type ArchetypeType =
    | Apprentice
    | Fighter
    | Ninja
    | Wizard
    | Conjuror
    | Cleric
    | Bat
    | Spider
    | Snake
    | Willowisp
    | Chillowisp
    | Fairy
    | Gel
    | Beetle
    | Rat
    | Scorpion
    | Plant
    | Goblin
    | Ghost
    | Imp
    | Zombie
    | Skeleton
    | Shaman
    | Glurble
    | Wolfman
    | Dryad
    | Mummy
    | Witch
    | Squidly
    | Merman
    | Feral
    | Thief
    | Lizardman
    | Trixter
    | Monk
    | Gorgon
    | Tortoise
    | Robot
    | Harpy
    | Minotaur
    | Dragon
    | Troll
    | Mare
    | Ogre
    | Djinn
    | Naga
    | Avian
    | Armoros
    | Golem
    | Jack
    | Trap
    | Vampire
    | Cerebus
    | Hydra
    | FireElemental
    | IceElemental
    | LightningElemental
    | EarthElemental
    | ShamanBig
    | RobotBig
    | Dinoman

type ShopType =
    | Chemist
    | Armory

type ShopkeepAppearanceType =
    | Male
    | Female
    | Fancy

type FieldType =
    | DebugRoom
    | DebugRoom2
    | TombOuter
    | TombGround
    | TombBasement
    | Castle of int
    | CastleConnector
    | Forest of int
    | ForestConnector
    | Factory of int
    | FactoryConnector
    | Mountain of int
    | MountainConnector
    | DeadSea of int
    | DeadSeaConnector
    | Ruins of int
    | RuinsConnector
    | Castle2 of int
    | Castle2Connector
    | Desert of int
    | DesertConnector
    | Seasons of int
    | SeasonsConnector
    | Volcano of int
    | VolcanoConnector

    static member toFieldName (fieldType : FieldType) =
        match valueToSymbol fieldType with
        | Symbol.Atom (name, _) -> name
        | Symbols ([Symbol.Atom (name , _); _], _) -> name
        | _ -> failwithumf ()

type BattleType =
    | DebugBattle
    | CastleBattle
    | CastleBattle2
    | CastleBattle3
    | CastleBattle4
    | CastleBattle5
    | CastleBattle6
    | CastleBattle7
    | CastleBattle8
    | CastleBattle9
    | FireGoblinBattle
    | Castle2Battle
    | Castle2Battle2
    | Castle2Battle3
    | Castle2Battle4
    | Castle2Battle5
    | Castle2Battle6
    | Castle2Battle7
    | Castle2Battle8
    | Castle2Battle9
    | HeavyArmorosBattle

type EncounterType =
    | DebugEncounter
    | CastleEncounter
    | Castle2Encounter

type LockType =
    | BrassKey

type ChestType =
    | WoodenChest
    | BrassChest

type DoorType =
    | WoodenDoor

type PortalIndex =
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

type PortalType =
    | AirPortal
    | StairsPortal of bool

type NpcType =
    | ShadeNpc
    | MaelNpc
    | RiainNpc
    | PericNpc
    | RavelNpc
    | AdvenNpc
    | EildaenNpc
    | ShamanaNpc
    | FireGoblinNpc
    | HeavyArmorosNpc
    
    static member exists advents specialty =
        match specialty with
        | ShadeNpc -> not (Set.contains ShadeRecruited advents)
        | MaelNpc -> not (Set.contains MaelRecruited advents)
        | RiainNpc -> not (Set.contains RiainRecruited advents)
        | PericNpc -> not (Set.contains PericRecruited advents)
        | FireGoblinNpc -> not (Set.contains FireGoblinDefeated advents)
        | HeavyArmorosNpc -> not (Set.contains HeavyArmorosDefeated advents)
        | RavelNpc | AdvenNpc | EildaenNpc | ShamanaNpc -> true

type ShopkeepType =
    | RobehnShopkeep
    | SchaalShopkeep

type FlameType =
    | FatFlame
    | SkinnyFlame
    | SmallFlame
    | LargeFlame

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

type CharacterAnimationType =
    | WalkAnimation
    | CelebrateAnimation
    | ReadyAnimation
    | PoiseAnimation of PoiseType
    | AttackAnimation
    | WoundAnimation
    | SpinAnimation
    | DamageAnimation
    | IdleAnimation
    | CastAnimation
    | Cast2Animation
    | SlashAnimation
    | WhirlAnimation
    | BuryAnimation // TODO: get rid of this

type AllyType =
    | Jinn
    | Shade
    | Mael
    | Riain
    | Peric

type EnemyType =
    | DebugGoblin
    | DarkBat
    | BlueGoblin
    | MadMinotaur
    | FireGoblin
    | PoisonGorgon
    | FacelessSoldier
    | Hawk
    | HeavyArmoros

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

    static member getName characterType =
        match characterType with
        | Ally ty -> string ty
        | Enemy ty -> string ty

type [<NoEquality; NoComparison>] SpiritType =
    | WeakSpirit
    | NormalSpirit
    | StrongSpirit

    static member getColor spiritType =
        match spiritType with
        | WeakSpirit -> Color (byte 255, byte 255, byte 255, byte 127)
        | NormalSpirit -> Color (byte 255, byte 199, byte 199, byte 127)
        | StrongSpirit -> Color (byte 255, byte 149, byte 149, byte 127)

type [<NoEquality; NoComparison>] CueTarget =
    | AvatarTarget // (field only)
    | NpcTarget of NpcType // (field only)
    | ShopkeepTarget of ShopkeepType // (field only)
    | AllyTarget of int // (battle only)
    | EnemyTarget of int // (battle only)

type [<NoEquality; NoComparison>] Cue =
    | Nil
    | PlaySound of single * Sound AssetTag
    | PlaySong of int * single * Song AssetTag
    | FadeOutSong of int
    | Face of Direction * CueTarget
    | Glow of Color * CueTarget
    | Animate of CharacterAnimationType * CueTarget
    | Recruit of AllyType
    | Unseal of int * Advent
    | AddItem of ItemType
    | RemoveItem of ItemType
    | AddAdvent of Advent
    | RemoveAdvent of Advent
    | Wait of int64
    | WaitState of int64
    | Fade of int64 * bool * CueTarget
    | FadeState of int64 * int64 * bool * CueTarget
    | Warp of FieldType * Vector2 * Direction
    | WarpState
    | Battle of BattleType * Advent Set // TODO: P1: consider using three Cues (start, end, post) in battle rather than advents directly...
    | BattleState
    | Dialog of string
    | DialogState
    | Prompt of string * (string * Cue) * (string * Cue)
    | PromptState
    | If of Advent Set * Cue * Cue
    | Not of Advent Set * Cue * Cue
    | Parallel of Cue list
    | Sequence of Cue list
    static member isNil cue = match cue with Nil -> true | _ -> false
    static member notNil cue = match cue with Nil -> false | _ -> true
    static member isInterrupting (advents : Advent Set) cue =
        match cue with
        | Nil | PlaySound _ | PlaySong _ | FadeOutSong _ | Face _ | Glow _ | Animate _ | Recruit _ | Unseal _ | AddItem _ | RemoveItem _ | AddAdvent _ | RemoveAdvent _ -> false
        | Wait _ | WaitState _ | Fade _ | FadeState _ | Warp _ | WarpState _ | Battle _ | BattleState _ | Dialog _ | DialogState _ | Prompt _ | PromptState _ -> true
        | If (r, c, a) -> if advents.IsSupersetOf r then Cue.isInterrupting advents c else Cue.isInterrupting advents a
        | Not (r, c, a) -> if not (advents.IsSupersetOf r) then Cue.isInterrupting advents c else Cue.isInterrupting advents a
        | Parallel cues -> List.exists (Cue.isInterrupting advents) cues
        | Sequence cues -> List.exists (Cue.isInterrupting advents) cues
    static member notInterrupting advents cue = not (Cue.isInterrupting advents cue)

type [<NoEquality; NoComparison>] Branch =
    { Cue : Cue
      Requirements : Advent Set }

[<RequireQualifiedAccess>]
module OmniSeedState =

    type OmniSeedState =
        private
            { RandSeedState : uint64 }

    let rotate isFade fieldType state =
        if not isFade then
            match fieldType with
            | DebugRoom | DebugRoom2 | TombOuter | TombGround | TombBasement
            | CastleConnector | ForestConnector | FactoryConnector | MountainConnector | DeadSeaConnector
            | RuinsConnector | Castle2Connector | DesertConnector | SeasonsConnector | VolcanoConnector -> state.RandSeedState
            | Castle n -> state.RandSeedState <<< n
            | Forest n -> state.RandSeedState <<< n + 6
            | Factory n -> state.RandSeedState <<< n + 12
            | Mountain n -> state.RandSeedState <<< n + 18
            | DeadSea n -> state.RandSeedState <<< n + 24
            | Ruins n -> state.RandSeedState <<< n + 30
            | Castle2 n -> state.RandSeedState <<< n + 36
            | Desert n -> state.RandSeedState <<< n + 42
            | Seasons n -> state.RandSeedState <<< n + 48
            | Volcano n -> state.RandSeedState <<< n + 54
        else state.RandSeedState <<< 60

    let makeFromSeedState randSeedState =
        { RandSeedState = randSeedState }

    let make () =
        { RandSeedState = Rand.DefaultSeedState }

type OmniSeedState = OmniSeedState.OmniSeedState

type WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int
      Cost : int
      Description : string }

type ArmorData =
    { ArmorType : ArmorType // key
      ArmorSubtype : ArmorSubtype
      HitPointsBase : int
      TechPointsBase : int
      Cost : int
      Description : string }

type AccessoryData =
    { AccessoryType : AccessoryType // key
      ShieldBase : int
      CounterBase : int
      Cost : int
      Description : string }

type ConsumableData =
    { ConsumableType : ConsumableType // key
      Scalar : single
      Curative : bool
      Techative : bool
      Revive : bool
      StatusesAdded : StatusType Set
      StatusesRemoved : StatusType Set
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
      Sneakening : bool
      Provocative : bool
      Cancels : bool
      Absorb : single // percentage of outcome that is absorbed by the caster
      ElementTypeOpt : ElementType option
      StatusesAdded : StatusType Set
      StatusesRemoved : StatusType Set
      TargetType : TargetType
      Description : string }

    member this.AimType =
        TargetType.getAimType this.TargetType

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
      Techs : Map<int, TechType> // tech availability according to level
      Stature : StatureType
      Description : string }

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

type [<NoEquality; NoComparison>] EnemyDescriptor =
    { EnemyType : EnemyType
      EnemyPosition : Vector2 }

type [<NoEquality; NoComparison>] BattleData =
    { BattleType : BattleType // key
      BattleAllyPositions : Vector2 list
      BattleEnemies : EnemyType list
      BattleTileMap : TileMap AssetTag
      BattleSongOpt : Song AssetTag option }

type [<NoEquality; NoComparison>] EncounterData =
    { EncounterType : EncounterType // key
      BattleTypes : BattleType list }

type [<NoEquality; NoComparison>] CharacterData =
    { CharacterType : CharacterType // key
      ArchetypeType : ArchetypeType
      LevelBase : int
      AnimationSheet : Image AssetTag
      PortraitOpt : Image AssetTag option
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      TechProbabilityOpt : single option
      GoldScalar : single
      ExpScalar : single
      Description : string }

type [<NoEquality; NoComparison>] CharacterAnimationData =
    { CharacterAnimationType : CharacterAnimationType // key
      AnimationType : AnimationType
      LengthOpt : int64 option
      Run : int
      Delay : int64
      Offset : Vector2i }

type [<NoEquality; NoComparison>] PropData =
    | Portal of PortalType * PortalIndex * Direction * FieldType * PortalIndex * bool * Advent Set // leads to a different portal
    | Door of DoorType * Cue * Cue * Advent Set // for simplicity, we'll just have north / south doors
    | Chest of ChestType * ItemType * Guid * BattleType option * Cue * Advent Set
    | Switch of SwitchType * Cue * Cue * Advent Set // anything that can affect another thing on the field through interaction
    | Sensor of SensorType * BodyShape option * Cue * Cue * Advent Set // anything that can affect another thing on the field through traversal
    | Npc of NpcType * Direction * Cue * Advent Set
    | NpcBranching of NpcType * Direction * Branch list * Advent Set
    | Shopkeep of ShopkeepType * Direction * ShopType * Advent Set
    | Seal of Color * Cue * Advent Set
    | Flame of FlameType * bool
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
    | FieldConnector of TileMap AssetTag * TileMap AssetTag
    | FieldRandom of int * single * OriginRand * int * string

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
    let mutable propDescriptorsMemoized = Map.empty<uint64 * FieldType, PropDescriptor list>

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
                let prop = { prop with PropData = Chest (WoodenChest, treasure, id, None, Cue.Nil, Set.empty) }
                (prop, treasures, rand)
            else ({ prop with PropData = EmptyProp }, treasures, rand)
        | _ -> (prop, treasures, rand)

    let tryGetTileMap omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate false fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey tileMapsMemoized with
        | None ->
            let tileMapOpt =
                match fieldData.FieldTileMap with
                | FieldStatic fieldAsset
                | FieldConnector (fieldAsset, _) ->
                    match World.tryGetTileMapMetadata fieldAsset world with
                    | Some (_, _, tileMap) -> Some tileMap
                    | None -> None
                | FieldRandom (walkLength, bias, origin, floor, fieldPath) ->
                    let rand = Rand.makeFromSeedState rotatedSeedState
                    let (cursor, mapRand, _) = MapRand.makeFromRand walkLength bias Constants.Field.MapRandSize origin floor rand
                    let fieldName = FieldType.toFieldName fieldData.FieldType
                    let mapTmx = MapRand.toTmx fieldName fieldPath origin cursor floor mapRand
                    Some mapTmx
            tileMapsMemoized <- Map.add memoKey tileMapOpt tileMapsMemoized
            tileMapOpt
        | Some tileMapOpt -> tileMapOpt

    let tryGetTileMapFade omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate true fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey tileMapsMemoized with
        | None ->
            let tileMapOpt =
                match fieldData.FieldTileMap with
                | FieldStatic _
                | FieldRandom _ -> None
                | FieldConnector (_, fieldFadeAsset) ->
                    match World.tryGetTileMapMetadata fieldFadeAsset world with
                    | Some (_, _, tileMap) -> Some tileMap
                    | None -> None
            tileMapsMemoized <- Map.add memoKey tileMapOpt tileMapsMemoized
            tileMapOpt
        | Some tileMapOpt -> tileMapOpt

    let getPropObjects omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate false fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey propObjectsMemoized with
        | None ->
            let propObjects =
                match tryGetTileMap omniSeedState fieldData world with
                | Some tileMap ->
                    if tileMap.ObjectGroups.Contains Constants.Field.PropsGroupName then
                        let group = tileMap.ObjectGroups.Item Constants.Field.PropsGroupName
                        enumerable<TmxObject> group.Objects |> Seq.map (fun propObject -> (tileMap, group, propObject)) |> Seq.toList
                    else []
                | None -> []
            propObjectsMemoized <- Map.add memoKey propObjects propObjectsMemoized
            propObjects
        | Some propObjects -> propObjects

    let getPropDescriptors omniSeedState fieldData world =
        let rotatedSeedState = OmniSeedState.rotate false fieldData.FieldType omniSeedState
        let memoKey = (rotatedSeedState, fieldData.FieldType)
        match Map.tryFind memoKey propDescriptorsMemoized with
        | None ->
            let propObjects = getPropObjects omniSeedState fieldData world
            let propsUninflated = List.choose (fun (tileMap, group, object) -> objectToPropOpt object group tileMap) propObjects
            let (propDescriptors, _, _) =
                List.foldBack (fun prop (propDescriptors, treasures, rand) ->
                    let (propDescriptor, treasures, rand) = inflateProp prop treasures rand
                    let treasures = if FStack.isEmpty treasures then FStack.ofSeq fieldData.Treasures else treasures
                    (propDescriptor :: propDescriptors, treasures, rand))
                    propsUninflated
                    ([], FStack.ofSeq fieldData.Treasures, Rand.makeFromSeedState rotatedSeedState)
            propDescriptorsMemoized <- Map.add memoKey propDescriptors propDescriptorsMemoized
            propDescriptors
        | Some propDescriptors -> propDescriptors

    let getPortals omniSeedState fieldData world =
        let propDescriptors = getPropDescriptors omniSeedState fieldData world
        List.filter (fun propDescriptor -> match propDescriptor.PropData with Portal _ -> true | _ -> false) propDescriptors

    let tryGetPortal omniSeedState portalIndex fieldData world =
        let portals = getPortals omniSeedState fieldData world
        List.tryFind (fun prop -> match prop.PropData with Portal (_, portalIndex2, _, _, _, _, _) -> portalIndex2 = portalIndex | _ -> failwithumf ()) portals

    let tryGetSpiritType omniSeedState avatarBottom fieldData world =
        match tryGetTileMap omniSeedState fieldData world with
        | Some tmxTileMap ->
            match fieldData.FieldTileMap with
            | FieldRandom (walkLength, _, origin, _, _) ->
                let tileMapBounds = v4Bounds v2Zero (v2 (single tmxTileMap.Width * single tmxTileMap.TileWidth) (single tmxTileMap.Height * single tmxTileMap.TileHeight))
                let distanceFromOriginMax =
                    let walkRatio = single walkLength * Constants.Field.WalkLengthScalar
                    let tileMapBoundsScaled = tileMapBounds.Scale (v2Dup walkRatio)
                    let delta = tileMapBoundsScaled.Bottom - tileMapBoundsScaled.Top
                    delta.Length ()
                let distanceFromOrigin =
                    match origin with
                    | OriginC -> let delta = avatarBottom - tileMapBounds.Center in delta.Length ()
                    | OriginN -> let delta = avatarBottom - tileMapBounds.Top in delta.Length ()
                    | OriginE -> let delta = avatarBottom - tileMapBounds.Right in delta.Length ()
                    | OriginS -> let delta = avatarBottom - tileMapBounds.Bottom in delta.Length ()
                    | OriginW -> let delta = avatarBottom - tileMapBounds.Left in delta.Length ()
                    | OriginNE -> let delta = avatarBottom - tileMapBounds.TopRight in delta.Length ()
                    | OriginNW -> let delta = avatarBottom - tileMapBounds.TopLeft in delta.Length ()
                    | OriginSE -> let delta = avatarBottom - tileMapBounds.BottomRight in delta.Length ()
                    | OriginSW -> let delta = avatarBottom - tileMapBounds.BottomLeft in delta.Length ()
                let battleIndex = int (3.0f / distanceFromOriginMax * distanceFromOrigin)
                match battleIndex with
                | 0 -> Some WeakSpirit
                | 1 -> Some NormalSpirit
                | _ -> Some StrongSpirit
            | FieldConnector _ -> None
            | FieldStatic _ -> None
        | None -> None

[<RequireQualifiedAccess>]
module Data =

    type [<NoEquality; NoComparison>] OmniData =
        { Weapons : Map<WeaponType, WeaponData>
          Armors : Map<ArmorType, ArmorData>
          Accessories : Map<AccessoryType, AccessoryData>
          Consumables : Map<ConsumableType, ConsumableData>
          Techs : Map<TechType, TechData>
          Archetypes : Map<ArchetypeType, ArchetypeData>
          Characters : Map<CharacterType, CharacterData>
          Shops : Map<ShopType, ShopData>
          Battles : Map<BattleType, BattleData>
          Encounters : Map<EncounterType, EncounterData>
          TechAnimations : Map<TechType, TechAnimationData>
          CharacterAnimations : Map<CharacterAnimationType, CharacterAnimationData>
          Fields : Map<FieldType, FieldData> }

    let private readSheet<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        Math.init () // HACK: initializing Math type converters for required type converters in fsx script.
        let text = File.ReadAllText filePath
        let symbol = flip (Symbol.ofStringCsv true) (Some filePath) text
        let value = symbolToValue<'d list> symbol
        Map.ofListBy (fun data -> getKey data, data) value

    let private readFromFiles () =
        { Weapons = readSheet Assets.Data.WeaponDataFilePath (fun data -> data.WeaponType)
          Armors = readSheet Assets.Data.ArmorDataFilePath (fun data -> data.ArmorType)
          Accessories = readSheet Assets.Data.AccessoryDataFilePath (fun data -> data.AccessoryType)
          Consumables = readSheet Assets.Data.ConsumableDataFilePath (fun data -> data.ConsumableType)
          Techs = readSheet Assets.Data.TechDataFilePath (fun data -> data.TechType)
          Archetypes = readSheet Assets.Data.ArchetypeDataFilePath (fun data -> data.ArchetypeType)
          Characters = readSheet Assets.Data.CharacterDataFilePath (fun data -> data.CharacterType)
          Shops = readSheet Assets.Data.ShopDataFilePath (fun data -> data.ShopType)
          Battles = readSheet Assets.Data.BattleDataFilePath (fun data -> data.BattleType)
          Encounters = readSheet Assets.Data.EncounterDataFilePath (fun data -> data.EncounterType)
          TechAnimations = readSheet Assets.Data.TechAnimationDataFilePath (fun data -> data.TechType)
          CharacterAnimations = readSheet Assets.Data.CharacterAnimationDataFilePath (fun data -> data.CharacterAnimationType)
          Fields = readSheet Assets.Data.FieldDataFilePath (fun data -> data.FieldType) }

    let Value =
        readFromFiles ()