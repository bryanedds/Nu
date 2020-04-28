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

type ElementType =
    | Fire // beats nothing; strongest
    | Lightning // beats water
    | Water // beats fire, lightning; weakest

type StatusType =
    | DefendStatus // also applies a perhaps stackable buff for attributes such as countering or magic power depending on class
    | PoisonStatus
    | MuteStatus
    | SleepStatus

type EquipmentType =
    | Weapon
    | Armor
    | Relic

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
    | AllyAim of bool
    | AnyAim
    | NoAim

type TargetType =
    | SingleTarget of AimType
    | RadialTarget of AimType
    | LineTarget of AimType
    | AllTarget of AimType

type EffectType =
    | Physical
    | Magical

type SpecialType =
    | JumpSlash

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

type WeaponData =
    { WeaponType : WeaponType // key
      WeaponSubtype : WeaponSubtype
      PowerBase : int
      MagicBase : int }

type ArmorType =
    string

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
    string

type RelicData =
    { RelicType : RelicType // key
      ShieldBase : int
      CounterBase : int }

type AllyType =
    | Jinn

type EnemyType =
    | Goblin

type CharacterType =
    | Ally of AllyType
    | Enemy of EnemyType

type DrainData =
    { EffectType : EffectType
      Percentage : single }

type ActionData =
    { ActionType : ActionType // key
      ActionName : string
      EffectType : EffectType
      MagicPointCost : int
      SuccessRate : single
      Curative : bool
      DrainData : DrainData
      ElementType : ElementType
      StatusesAdded : StatusType Set
      StatusesRemoved : StatusType Set
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

type [<NoComparison>] Rom =
    { Weapons : Map<WeaponType, WeaponData>
      Armors : Map<ArmorType, ArmorData>
      Relics : Map<RelicType, RelicData>
      Actions : Map<ActionType, ActionData>
      Items : Map<ItemType, ItemData>
      Characters : Map<CharacterType, CharacterData> }

    static member readSheet<'d, 'k when 'k : comparison> filePath (getKey : 'd -> 'k) =
        File.ReadAllText filePath |>
        flip (Symbol.fromStringCsv true) (Some filePath) |>
        symbolToValue<'d list> |>
        Map.ofListBy (fun data -> getKey data, data)

    static member readFromFiles () =
        { Weapons = Rom.readSheet Assets.WeaponDataFilePath (fun data -> data.WeaponType)
          Armors = Rom.readSheet Assets.ArmorDataFilePath (fun data -> data.ArmorType)
          Relics = Rom.readSheet Assets.RelicDataFilePath (fun data -> data.RelicType)
          Actions = Map.empty
          Items = Map.empty
          Characters = Map.empty }

type Inventory =
    { Items : Map<ItemType, int> }

    static member getKeyItems inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (KeyItem keyItemType, count) -> Some (keyItemType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray
        
    static member getConsumables inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (Consumable consumableType, count) -> Some (consumableType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray

type [<CustomEquality; CustomComparison>] GameEvent =
    | SavedPrincess
    | FoughtBadfdie of bool

    member private this.ToInt () =
        match this with
        | SavedPrincess -> 0
        | FoughtBadfdie _ -> 1
        
    override this.GetHashCode () =
        let rand = Rand.makeFromInt (this.ToInt ())
        let (result, _) = Rand.nextInt rand
        result

    override this.Equals that =
        match that with
        | :? GameEvent as thatEvent -> (this :> IComparable<GameEvent>).CompareTo thatEvent = 0
        | _ -> false

    interface IComparable<GameEvent> with
        member this.CompareTo that =
            let thisInt = this.ToInt ()
            let thatInt = that.ToInt ()
            thisInt.CompareTo thatInt
            
    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? GameEvent as thatEvent -> (this :> IComparable<GameEvent>).CompareTo thatEvent
            | _ -> -1
                
type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType }

// Rom -> FieldModel -> BattleModel -> CharacterModel -> etc.
type [<NoComparison>] FieldModel =
    { Legion : Map<int, Legionnaire>
      GameEvents : Set<GameEvent>
      Inventory : Inventory
      Gold : int }

    static member getPartyMembers fieldModel =
        Map.filter
            (fun _ legionnaire -> Option.isSome legionnaire.PartyIndexOpt)
            fieldModel.Legion

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

type CharacterState =
    { CharacterType : CharacterType
      PartyIndex : int
      ActionTime : int
      ExpPoints : int
      HitPoints : int
      SpecialPoints : int
      Defending : bool
      Charging : bool
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      Specials : SpecialType Set
      Statuses : StatusType Set
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Relics : RelicType list }

    static member empty =
        { CharacterType = Ally Jinn
          PartyIndex = 0
          ActionTime = 0
          ExpPoints = 0
          HitPoints = 10 // note this is an arbitrary number as hp max is calculated
          SpecialPoints = 1 // sp max is calculated
          Defending = false
          Charging = false
          PowerBuff = 1.0f // rate at which power is buffed / debuffed
          MagicBuff = 1.0f // rate at which magic is buffed / debuffed
          ShieldBuff = 1.0f // rate at which shield is buffed / debuffed
          CounterBuff = 1.0f // rate at which counter is buffed / debuffed
          Specials = Set.empty
          Statuses = Set.empty
          WeaponOpt = None
          ArmorOpt = None
          Relics = [] } // level is calculated from base experience + added experience

    member this.Name = match this.CharacterType with Ally ally -> scstring ally | Enemy enemy -> scstring enemy
    member this.CharacterIndex = match this.CharacterType with Ally _ -> AllyIndex this.PartyIndex | Enemy _ -> EnemyIndex this.PartyIndex
    member this.IsAlly = match this.CharacterType with Ally _ -> true | Enemy _ -> false
    member this.IsEnemy = not this.IsAlly
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0

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

    member this.ComputeHitPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.HitPointsBase |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level |> int |> max 1

    member this.ComputeSpecialPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.SpecialPointsBase |> max 2.0f
                | None -> 2.0f
            | None -> 2.0f
        intermediate * single this.Level |> int |> max 1

    member this.ComputePower rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.PowerBase * this.PowerBuff |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level / 5.0f |> int |> max 1

    member this.ComputeMagic rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.MagicBase * this.MagicBuff |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level / 5.0f |> int |> max 1

    member this.ComputeShield (rom : Rom) =
        let intermediate =
            match this.Relics with
            | relic :: _ -> // just the first relic for now
                match Map.tryFind relic rom.Relics with
                | Some weaponData -> single weaponData.ShieldBase * this.ShieldBuff |> max 0.0f
                | None -> 0.0f
            | _ -> 0.0f
        intermediate * single this.Level / 5.0f |> int |> max 0

type PoiseType =
    | Poising
    | Defending
    | Charging

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
    | WoundCycle

type CharacterAnimationState =
    { TimeStart : int64
      AnimationSheet : Image AssetTag
      AnimationCycle : CharacterAnimationCycle
      Direction : Direction
      Stutter : int }

    static member setCycle timeOpt cycle state =
        match timeOpt with
        | Some time -> { state with TimeStart = time; AnimationCycle = cycle }
        | None -> { state with AnimationCycle = cycle }

    static member directionToInt direction =
        match direction with
        | Downward -> 0
        | Leftward -> 1
        | Upward -> 2
        | Rightward -> 3

    static member timeLocal time state =
        time - state.TimeStart

    static member indexCel time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int timeLocal / state.Stutter

    static member indexLooped run time state =
        CharacterAnimationState.indexCel time state % run

    static member indexSaturated run time state =
        let cel = CharacterAnimationState.indexCel time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection row run time state =
        let offset = CharacterAnimationState.directionToInt state.Direction * run
        Vector2i (CharacterAnimationState.indexLooped run time state + offset, row)

    static member indexLoopedWithoutDirection row run time state =
        Vector2i (CharacterAnimationState.indexLooped run time state, row)

    static member indexSaturatedWithDirection row run time state =
        let offset = CharacterAnimationState.directionToInt state.Direction * run
        Vector2i (CharacterAnimationState.indexSaturated run time state + offset, row)

    static member indexSaturatedWithoutDirection row run time state =
        Vector2i (CharacterAnimationState.indexSaturated run time state, row)

    static member index time state =
        match state.AnimationCycle with
        | WalkCycle -> CharacterAnimationState.indexLoopedWithDirection 0 6 time state
        | CelebrateCycle -> CharacterAnimationState.indexLoopedWithDirection 1 2 time state
        | ReadyCycle -> CharacterAnimationState.indexSaturatedWithDirection 2 3 time state
        | PoiseCycle Poising -> CharacterAnimationState.indexLoopedWithDirection 3 3 time state
        | PoiseCycle Defending -> CharacterAnimationState.indexLoopedWithDirection 9 1 time state
        | PoiseCycle Charging -> CharacterAnimationState.indexLoopedWithDirection 5 2 time state
        | AttackCycle -> CharacterAnimationState.indexSaturatedWithDirection 4 3 time state
        | CastCycle -> CharacterAnimationState.indexLoopedWithDirection 5 2 time state
        | SpinCycle -> CharacterAnimationState.indexLoopedWithoutDirection 7 4 time state
        | DamageCycle -> CharacterAnimationState.indexSaturatedWithDirection 6 1 time state
        | IdleCycle -> CharacterAnimationState.indexSaturatedWithDirection 8 1 time state
        | WoundCycle -> Vector2i (0, 8)

    static member cycleLengthOpt state =
        match state.AnimationCycle with
        | WalkCycle -> None
        | CelebrateCycle -> None
        | ReadyCycle -> Some (int64 (5 * state.Stutter))
        | PoiseCycle _ -> None
        | AttackCycle -> Some (int64 (4 * state.Stutter))
        | CastCycle -> None
        | SpinCycle -> Some (int64 (4 * state.Stutter))
        | DamageCycle -> Some (int64 (3 * state.Stutter))
        | IdleCycle -> None
        | WoundCycle -> Some (int64 (5 * state.Stutter))

    static member progressOpt time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        match CharacterAnimationState.cycleLengthOpt state with
        | Some length -> Some (min 1.0f (single timeLocal / single length))
        | None -> None

    static member finished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> false

type CharacterInputState =
    | NoInput
    | RegularMenu
    | SpecialMenu
    | ItemMenu
    | AimReticles of string * AimType

    member this.AimType =
        match this with
        | NoInput | RegularMenu | SpecialMenu | ItemMenu -> NoAim
        | AimReticles (_, aimType) -> aimType

type [<NoComparison>] CharacterModel =
    { CharacterState : CharacterState
      AnimationState : CharacterAnimationState
      InputState : CharacterInputState
      Position : Vector2
      Size : Vector2 }

    member this.Center =
        this.Position + this.Size * 0.5f

type CharacterModels =
    Map<CharacterIndex, CharacterModel>

module CharacterModels =

    let getAllies (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getEnemies (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getAlliesHealthy (characters : CharacterModels) =
        getAllies characters |>
        List.filter (fun character -> character.CharacterState.IsHealthy)

    let getAlliesWounded (characters : CharacterModels) =
        getAllies characters |>
        List.filter (fun character -> character.CharacterState.IsWounded)

    let getTargets aimType (characters : CharacterModels) =
        match aimType with
        | EnemyAim -> getEnemies characters
        | AllyAim healthy -> if healthy then getAlliesHealthy characters else getAlliesWounded characters
        | AnyAim -> Map.toValueList characters
        | NoAim -> []

type [<NoComparison>] ReticlesModel =
    { Characters : Map<CharacterIndex, CharacterModel>
      AimType : AimType }

type RingMenuModel =
    { Items : string list
      ItemCancelOpt : string option }

type [<NoEquality; NoComparison>] ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type [<NoEquality; NoComparison>] CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

type [<NoEquality; NoComparison>] BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleCease of bool * int64

type [<NoEquality; NoComparison>] BattleModel =
    { BattleState : BattleState
      Characters : Map<CharacterIndex, CharacterModel>
      CurrentCommandOpt : CurrentCommand option
      ActionQueue : ActionCommand Queue
      Inventory : Inventory
      Gold : int }