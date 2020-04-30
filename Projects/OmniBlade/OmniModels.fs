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
    | Volt

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

type Rom =
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
type FieldModel =
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

type AutoBattle =
    { AutoTarget : CharacterIndex
      AutoSpecialOpt : SpecialType option }

type CharacterState =
    { CharacterType : CharacterType
      PartyIndex : int
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

    member this.HitPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.HitPointsBase |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level |> int |> max 1

    member this.SpecialPointsMax rom =
        let intermediate =
            match this.ArmorOpt with
            | Some armor ->
                match Map.tryFind armor rom.Armors with
                | Some armorData -> single armorData.SpecialPointsBase |> max 2.0f
                | None -> 2.0f
            | None -> 2.0f
        intermediate * single this.Level |> int |> max 1

    member this.Power rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.PowerBase * this.PowerBuff |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level / 5.0f |> int |> max 1

    member this.Magic rom =
        let intermediate =
            match this.WeaponOpt with
            | Some weapon ->
                match Map.tryFind weapon rom.Weapons with
                | Some weaponData -> single weaponData.MagicBase * this.MagicBuff |> max 5.0f
                | None -> 5.0f
            | None -> 5.0f
        intermediate * single this.Level / 5.0f |> int |> max 1

    member this.Shield (rom : Rom) =
        let intermediate =
            match this.Relics with
            | relic :: _ -> // just the first relic for now
                match Map.tryFind relic rom.Relics with
                | Some weaponData -> single weaponData.ShieldBase * this.ShieldBuff |> max 0.0f
                | None -> 0.0f
            | _ -> 0.0f
        intermediate * single this.Level / 5.0f |> int |> max 0

    static member getDamage scalar (source : CharacterState) (target : CharacterState) rom =
        let power = source.Power rom
        let shield = target.Shield rom
        let damage = max 1 (int (Math.Ceiling (double (power - shield))))
        damage * scalar

    static member tryGetSpecialRandom state =
        let specials = state.Specials
        if Set.notEmpty specials then
            let specialIndex = Random().Next specials.Count
            let special = Seq.item specialIndex specials
            Some special
        else None

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
      ActionTime : int
      AutoBattleOpt : AutoBattle option
      InputState : CharacterInputState
      Position : Vector2
      Size : Vector2 }

    member this.Center = this.Position + this.Size * 0.5f
    member this.Bottom = this.Position + v2 (this.Size.X * 0.5f) 0.0f
    member this.IsEnemy = this.CharacterState.IsEnemy
    member this.IsAlly = this.CharacterState.IsAlly
    member this.IsHealthy = this.CharacterState.IsHealthy
    member this.IsWonded = this.CharacterState.IsWounded

    static member setInputState inputState character =
        { character with InputState = inputState }

    static member setAnimationCycle time cycle character =
        { character with AnimationState = CharacterAnimationState.setCycle (Some time) cycle character.AnimationState }

    static member setActionTime actionTime character =
        { character with ActionTime = actionTime }

    static member setAutoBattleOpt autoBattleOpt character =
        { character with AutoBattleOpt = autoBattleOpt }

    static member changeActionTime delta character =
        CharacterModel.setActionTime (character.ActionTime + delta) character

    static member changeHitPoints delta character =
        let hitPoints = character.CharacterState.HitPoints
        let hitPoints =  max 0 (hitPoints + delta)
        { character with CharacterState = { character.CharacterState with HitPoints = hitPoints }}

    static member getPoiseType character =
        if character.CharacterState.Defending then Defending
        elif character.CharacterState.Charging then Charging
        else Poising

    static member tryGetSpecialRandom character =
        CharacterState.tryGetSpecialRandom character.CharacterState

    static member readyForAutoBattle (character : CharacterModel) =
        character.IsEnemy &&
        character.AutoBattleOpt.IsNone &&
        character.ActionTime > 222

    static member runningSpecialAutoBattle character =
        match character.AutoBattleOpt with
        | Some autoBattle -> Option.isSome autoBattle.AutoSpecialOpt
        | None -> false

type CharacterModels =
    Map<CharacterIndex, CharacterModel>

module CharacterModels =

    let getAllies (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getEnemies (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map snd |> Seq.toList

    let getAllyIndices (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (AllyIndex _, _) -> true | _ -> false) |> Seq.map fst |> Seq.toList

    let getEnemyIndices (characters : CharacterModels) =
        characters |> Map.toSeq |> Seq.filter (function (EnemyIndex _, _) -> true | _ -> false) |> Seq.map fst |> Seq.toList

    let getAllyIndexRandom characters =
        let allyIndices = getAllyIndices characters
        let allyIndex = List.item (Random().Next allyIndices.Length) allyIndices
        allyIndex

    let getEnemyIndexRagdom characters =
        let enemyIndices = getEnemyIndices characters
        let enemyIndex = List.item (Random().Next enemyIndices.Length) enemyIndices
        enemyIndex

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

type ActionCommand =
    { Action : ActionType
      Source : CharacterIndex
      TargetOpt : CharacterIndex option }

    static member make action source targetOpt =
        { Action = action
          Source = source
          TargetOpt = targetOpt }

type CurrentCommand =
    { TimeStart : int64
      ActionCommand : ActionCommand }

    static member make timeStart actionCommand =
        { TimeStart = timeStart; ActionCommand = actionCommand }

type BattleState =
    | BattleReady of int64
    | BattleRunning
    | BattleCease of bool * int64

type [<NoComparison>] BattleModel =
    { BattleState : BattleState
      Characters : Map<CharacterIndex, CharacterModel>
      CurrentCommandOpt : CurrentCommand option
      ActionCommands : ActionCommand Queue
      Inventory : Inventory
      Gold : int }

    static member getAllies model =
        CharacterModels.getAllies model.Characters

    static member getEnemies model =
        CharacterModels.getEnemies model.Characters

    static member getAllyIndices model =
        CharacterModels.getAllyIndices model.Characters

    static member getEnemyIndices model =
        CharacterModels.getEnemyIndices model.Characters

    static member getAllyIndexRandom model =
        CharacterModels.getAllyIndexRandom model.Characters

    static member updateCharactersIf predicate updater model =
        { model with BattleModel.Characters = Map.map (fun index character -> if predicate index then updater character else character) model.Characters }

    static member updateCharacters updater model =
        BattleModel.updateCharactersIf tautology updater model

    static member updateAllies updater model =
        BattleModel.updateCharactersIf (function AllyIndex _ -> true | _ -> false) updater model

    static member updateEnemies updater model =
        BattleModel.updateCharactersIf (function EnemyIndex _ -> true | _ -> false) updater model

    static member tryGetCharacter characterIndex model =
        Map.tryFind characterIndex model.Characters

    static member getCharacter characterIndex model =
        BattleModel.tryGetCharacter characterIndex model |> Option.get

    static member tryUpdateCharacter updater characterIndex model =
        match BattleModel.tryGetCharacter characterIndex model with
        | Some character ->
            let character = updater character
            { model with Characters = Map.add characterIndex character model.Characters }
        | None -> model

    static member updateCharacter updater characterIndex model =
        let character = BattleModel.getCharacter characterIndex model
        let character = updater character
        { model with Characters = Map.add characterIndex character model.Characters }

    static member runAutoBattle character model =
        let allyIndex = BattleModel.getAllyIndexRandom model
        let ally = BattleModel.getCharacter allyIndex model
        let characterToAlly = ally.Position - character.Position
        let direction = Direction.fromVector2 characterToAlly
        let specialOpt =
            match Random().Next 4 with
            | 0 -> CharacterModel.tryGetSpecialRandom character
            | _ -> None
        let autoBattle = { AutoTarget = allyIndex; AutoSpecialOpt = specialOpt }
        let animationState = { character.AnimationState with Direction = direction }
        { character with AutoBattleOpt = Some autoBattle; AnimationState = animationState }

    static member conjActionCommand command model =
        { model with ActionCommands = Queue.conj command model.ActionCommands }