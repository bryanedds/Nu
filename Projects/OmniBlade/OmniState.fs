namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type PropState =
    | DoorState of bool
    | SwitchState of bool
    | NpcState of bool
    | ShopkeepState of bool
    | NilState

type [<StructuralEquality; NoComparison>] Inventory =
    { Items : Map<ItemType, int>
      Gold : int }

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

    static member addItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount ->
            { inventory with Items = Map.add item (inc itemCount) inventory.Items }
        | None ->
            { inventory with Items = Map.add item 1 inventory.Items }

    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType
      ExpPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list }

    static member finn =
        { LegionIndex = 0
          PartyIndexOpt = Some 0
          CharacterType = Ally Finn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

    static member glenn =
        { LegionIndex = 1
          PartyIndexOpt = Some 1
          CharacterType = Ally Glenn
          ExpPoints = 15
          WeaponOpt = None
          ArmorOpt = None
          Accessories = [] }

type Legion =
    Map<int, Legionnaire>

type [<StructuralEquality; StructuralComparison>] CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int
    static member isTeammate index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

type [<StructuralEquality; NoComparison>] CharacterState =
    { ArchetypeType : ArchetypeType
      ExpPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      HitPoints : int
      TechPoints : int
      Statuses : StatusType Set
      Defending : bool
      Charging : bool
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single }

    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt this.PowerBuff this.ArchetypeType this.Level
    member this.Magic = Algorithms.magic this.WeaponOpt this.MagicBuff this.ArchetypeType this.Level
    member this.Shield effectType = Algorithms.shield effectType this.Accessories this.ShieldBuff this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member getAttackResult effectType (source : CharacterState) (target : CharacterState) =
        let power = source.Power
        let shield = target.Shield effectType
        let damageUnscaled = power - shield
        let damage = single damageUnscaled |> int |> max 1
        damage

    static member updateHitPoints updater (state : CharacterState) =
        let hitPoints = updater state.HitPoints
        let hitPoints = max 0 hitPoints
        let hitPoints = min state.HitPointsMax hitPoints
        { state with HitPoints = hitPoints }

    static member updateTechPoints updater state =
        let specialPoints = updater state.TechPoints
        let specialPoints = max 0 specialPoints
        let specialPoints = min state.TechPointsMax specialPoints
        { state with TechPoints = specialPoints }

    static member tryGetTechRandom (state : CharacterState) =
        let specials = state.Techs
        if Set.notEmpty specials then
            let specialIndex = Gen.random1 specials.Count
            let special = Seq.item specialIndex specials
            Some special
        else None

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

    static member make characterData expPoints weaponOpt armorOpt accessories =
        let levelBase = characterData.LevelBase
        let archetypeType = characterData.ArchetypeType
        let expPointsTotal = Algorithms.levelToExpPoints levelBase + expPoints
        let level = Algorithms.expPointsToLevel expPointsTotal
        let hitPointsMax = Algorithms.hitPointsMax armorOpt archetypeType level
        let techPointsMax = Algorithms.hitPointsMax armorOpt archetypeType level
        let characterState =
            { ArchetypeType = archetypeType
              ExpPoints = expPointsTotal
              WeaponOpt = weaponOpt
              ArmorOpt = armorOpt
              Accessories = accessories
              HitPoints = hitPointsMax
              TechPoints = techPointsMax
              Statuses = Set.empty
              Defending = false
              Charging = false
              PowerBuff = 1.0f
              MagicBuff = 1.0f
              ShieldBuff = 1.0f
              CounterBuff = 1.0f }
        characterState

    static member empty =
        let characterState =
            { ArchetypeType = Squire
              ExpPoints = 0
              WeaponOpt = None
              ArmorOpt = None
              Accessories = []
              HitPoints = 1
              TechPoints = 0
              Statuses = Set.empty
              Defending = false
              Charging = false
              PowerBuff = 1.0f
              MagicBuff = 1.0f
              ShieldBuff = 1.0f
              CounterBuff = 1.0f }
        characterState

type [<StructuralEquality; NoComparison>] CharacterAnimationState =
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

    static member indexCel stutter time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int (timeLocal / stutter)

    static member indexLooped run stutter time state =
        CharacterAnimationState.indexCel stutter time state % run

    static member indexSaturated run stutter time state =
        let cel = CharacterAnimationState.indexCel stutter time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexLooped run stutter time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
        | None -> v2iZero

    static member progressOpt time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            let timeLocal = CharacterAnimationState.timeLocal time state
            match animationData.LengthOpt with
            | Some length -> Some (min 1.0f (single timeLocal / single length))
            | None -> None
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> false

    static member empty =
        { TimeStart = 0L
          AnimationSheet = Assets.FinnAnimationSheet
          AnimationCycle = IdleCycle
          Direction = Downward }

type [<StructuralEquality; StructuralComparison>] CharacterInputState =
    | NoInput
    | RegularMenu
    | TechMenu
    | ItemMenu
    | AimReticles of string * AimType

    member this.AimType =
        match this with
        | NoInput | RegularMenu | TechMenu | ItemMenu -> NoAim
        | AimReticles (_, aimType) -> aimType