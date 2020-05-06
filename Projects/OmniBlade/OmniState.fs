namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type [<CustomEquality; CustomComparison>] GameEvent =
    | SavedPrincess
    | FoughtBaddie of bool

    member private this.ToInt () =
        match this with
        | SavedPrincess -> 0
        | FoughtBaddie _ -> 1

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

    static member containsItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 0 -> true
        | _ -> false

    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int

type PoiseType =
    | Poising
    | Defending
    | Charging

type AutoBattle =
    { AutoTarget : CharacterIndex
      AutoSpecialOpt : SpecialType option }

/// The state of a character.
/// Used both inside and outside of battle.
/// Level is calculated from base experience + added experience.
type CharacterState =
    { PartyIndex : int // key
      CharacterType : CharacterType
      ExpPoints : int
      HitPoints : int
      SpecialPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      Specials : SpecialType Set
      Statuses : StatusType Set
      Defending : bool
      Charging : bool
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      ActionTime : int
      AutoBattleOpt : AutoBattle option }

    static member empty =
        { PartyIndex = 0
          CharacterType = Ally Jinn
          ExpPoints = 0
          HitPoints = 1 // note this is an arbitrary number as hp max is calculated
          SpecialPoints = 0 // sp max is calculated
          WeaponOpt = None
          ArmorOpt = None
          Accessories = []
          Specials = Set.empty
          Statuses = Set.empty
          Defending = false
          Charging = false
          PowerBuff = 1.0f // rate at which power is buffed / debuffed
          MagicBuff = 1.0f // rate at which magic is buffed / debuffed
          ShieldBuff = 1.0f // rate at which shield is buffed / debuffed
          CounterBuff = 1.0f // rate at which counter is buffed / debuffed
          ActionTime = 0
          AutoBattleOpt = None }

    member this.CharacterIndex = match this.CharacterType with Ally _ -> AllyIndex this.PartyIndex | Enemy _ -> EnemyIndex this.PartyIndex
    member this.Name = match this.CharacterType with Ally ally -> scstring ally | Enemy enemy -> scstring enemy
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

    member this.HitPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.HitPointsBase
                | None -> 8.0f
            | None -> 8.0f
        intermediate * single this.Level |> int

    member this.SpecialPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.SpecialPointsBase
                | None -> 4.0f
            | None -> 4.0f
        intermediate * single this.Level |> int

    member this.Power rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.PowerBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single this.Level * this.PowerBuff |> int |> max 1

    member this.Magic rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.MagicBase
                | None -> 1.0f
            | None -> 1.0f
        intermediate * single this.Level * this.MagicBuff |> int |> max 1

    member this.Shield (rom : Rom) =
        let intermediate =
            match this.Accessories with
            | accessory :: _ -> // just the first relic for now
                match Map.tryFind accessory rom.Accessories with
                | Some weaponData -> single weaponData.ShieldBase
                | None -> 0.0f
            | _ -> 0.0f
        intermediate * single this.Level * this.ShieldBuff |> int |> max 0
        
    static member runningSpecialAutoBattle state =
        match state.AutoBattleOpt with
        | Some autoBattle -> Option.isSome autoBattle.AutoSpecialOpt
        | None -> false

    static member getAttackResult rom (source : CharacterState) (target : CharacterState) =
        let power = source.Power rom
        let shield = target.Shield rom
        let damageUnscaled = power - shield
        let damage = single damageUnscaled |> int |> max 1
        damage

    static member updateActionTime updater state =
        { state with ActionTime = updater state.ActionTime }

    static member updateAutoBattleOpt updater state =
        { state with AutoBattleOpt = updater state.AutoBattleOpt }

    static member updateHitPoints rom updater (state : CharacterState) =
        let (hitPoints, cancel) = updater state.HitPoints
        let hitPoints = max 0 hitPoints
        let hitPoints = min (state.HitPointsMax rom) hitPoints
        let autoBattleOpt = 
            match state.AutoBattleOpt with
            | Some autoBattle when cancel -> Some { autoBattle with AutoSpecialOpt = None }
            | _ -> None
        { state with HitPoints = hitPoints; AutoBattleOpt = autoBattleOpt }

    static member updateSpecialPoints rom updater state =
        let specialPoints = updater state.SpecialPoints
        let specialPoints = max 0 specialPoints
        let specialPoints = min (state.SpecialPointsMax rom) specialPoints
        { state with SpecialPoints = specialPoints }

    static member tryGetSpecialRandom state =
        let specials = state.Specials
        if Set.notEmpty specials then
            let specialIndex = Gen.random1 specials.Count
            let special = Seq.item specialIndex specials
            Some special
        else None

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

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

type CharacterAnimationState =
    { TimeStart : int64
      AnimationSheet : Image AssetTag
      AnimationCycle : CharacterAnimationCycle
      Direction : Direction }

    static member setCycle timeOpt cycle state =
        if state.AnimationCycle <> cycle then
            match timeOpt with
            | Some time -> { state with TimeStart = time; AnimationCycle = cycle }
            | None -> { state with AnimationCycle = cycle }
        else state

    static member directionToInt direction =
        match direction with
        | Downward -> 0
        | Leftward -> 1
        | Upward -> 2
        | Rightward -> 3

    static member timeLocal time state =
        time - state.TimeStart

    static member cycleStutter state =
        match state.AnimationCycle with
        | WalkCycle -> 10L
        | CelebrateCycle -> 15L
        | ReadyCycle -> 10L
        | PoiseCycle _ -> 10L
        | AttackCycle -> 15L
        | CastCycle -> 10L
        | SpinCycle -> 10L
        | DamageCycle -> 10L
        | IdleCycle -> 10L
        | Cast2Cycle -> 10L
        | WhirlCycle -> 5L
        | BuryCycle -> 10L
        | FlyCycle -> 10L
        | HopForwardCycle -> 10L
        | HopBackCycle -> 10L
        | WoundCycle -> 10L

    static member cycleLengthOpt state =
        let stutter = CharacterAnimationState.cycleStutter state
        match state.AnimationCycle with
        | WalkCycle -> None
        | CelebrateCycle -> None
        | ReadyCycle -> Some (6L * stutter)
        | PoiseCycle _ -> None
        | AttackCycle -> Some (4L * stutter)
        | CastCycle -> None
        | SpinCycle -> Some (4L * stutter)
        | DamageCycle -> Some (4L * stutter)
        | IdleCycle -> None
        | Cast2Cycle -> Some (12L * stutter)
        | WhirlCycle -> Some (12L * stutter)
        | BuryCycle -> Some (8L * stutter)
        | FlyCycle -> None
        | HopForwardCycle -> None
        | HopBackCycle -> None
        | WoundCycle -> Some (6L * stutter)

    static member indexCel stutter time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int (timeLocal / stutter)

    static member indexLooped run stutter time state =
        CharacterAnimationState.indexCel stutter time state % run

    static member indexSaturated run stutter time state =
        let cel = CharacterAnimationState.indexCel stutter time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection row run stutter time state =
        let offset = CharacterAnimationState.directionToInt state.Direction * run
        Vector2i (CharacterAnimationState.indexLooped run stutter time state + offset, row)

    static member indexLoopedWithoutDirection row run stutter time state =
        Vector2i (CharacterAnimationState.indexLooped run stutter time state, row)

    static member indexSaturatedWithDirection row run stutter time state =
        let offset = CharacterAnimationState.directionToInt state.Direction * run
        Vector2i (CharacterAnimationState.indexSaturated run stutter time state + offset, row)

    static member indexSaturatedWithoutDirection row run stutter time state =
        Vector2i (CharacterAnimationState.indexSaturated run stutter time state, row)

    static member index time state =
        let stutter = CharacterAnimationState.cycleStutter state
        match state.AnimationCycle with
        | WalkCycle -> CharacterAnimationState.indexLoopedWithDirection 0 6 stutter time state
        | CelebrateCycle -> CharacterAnimationState.indexLoopedWithDirection 1 2 stutter time state
        | ReadyCycle -> CharacterAnimationState.indexSaturatedWithDirection 2 3 stutter time state
        | PoiseCycle Poising -> CharacterAnimationState.indexLoopedWithDirection 3 3 stutter time state
        | PoiseCycle Defending -> CharacterAnimationState.indexLoopedWithDirection 9 1 stutter time state
        | PoiseCycle Charging -> CharacterAnimationState.indexLoopedWithDirection 5 2 stutter time state
        | AttackCycle -> CharacterAnimationState.indexSaturatedWithDirection 4 3 stutter time state
        | CastCycle -> CharacterAnimationState.indexLoopedWithDirection 5 2 stutter time state
        | SpinCycle -> CharacterAnimationState.indexLoopedWithoutDirection 7 4 stutter time state
        | DamageCycle -> CharacterAnimationState.indexSaturatedWithDirection 6 1 stutter time state
        | IdleCycle -> CharacterAnimationState.indexSaturatedWithDirection 8 1 stutter time state
        | Cast2Cycle -> CharacterAnimationState.indexSaturatedWithDirection 10 2 stutter time state
        | WhirlCycle -> CharacterAnimationState.indexLoopedWithoutDirection 11 4 stutter time state
        | BuryCycle -> CharacterAnimationState.indexSaturatedWithoutDirection 11 4 stutter time state
        | FlyCycle -> CharacterAnimationState.indexLoopedWithDirection 12 1 stutter time state
        | HopForwardCycle -> CharacterAnimationState.indexLoopedWithDirection 13 1 stutter time state
        | HopBackCycle -> CharacterAnimationState.indexLoopedWithDirection 14 1 stutter time state
        | WoundCycle -> Vector2i (0, 8)

    static member progressOpt time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        match CharacterAnimationState.cycleLengthOpt state with
        | Some length -> Some (min 1.0f (single timeLocal / single length))
        | None -> None

    static member getFinished time state =
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