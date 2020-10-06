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
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount -> { inventory with Items = Map.add item (inc itemCount) inventory.Items }
            | None -> { inventory with Items = Map.add item 1 inventory.Items }
        | Stash gold -> { inventory with Gold = inventory.Gold + gold }

    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory

    static member indexItems (inventory : Inventory) =
        inventory.Items |>
        Map.toSeq |>
        Seq.map (fun (ty, ct) -> List.init ct (fun _ -> ty)) |>
        Seq.concat |>
        Seq.index

    static member tryIndexItem index inventory =
        let items = Inventory.indexItems inventory
        let tail = Seq.trySkip index items
        Seq.tryHead tail

    static member getItemCount itemType inventory =
        match Map.tryFind itemType inventory.Items with
        | Some count -> count
        | None -> 0

    static member updateGold updater inventory =
        { inventory with Gold = updater inventory.Gold }

type Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType
      HitPoints : int
      TechPoints : int
      ExpPoints : int
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list }

    static member canUseItem itemType legionnaire =
        match Map.tryFind legionnaire.CharacterType data.Value.Characters with
        | Some characterData ->
            match Map.tryFind characterData.ArchetypeType data.Value.Archetypes with
            | Some archetypeData ->
                match itemType with
                | Consumable _ -> true
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType ->
                        match Map.tryFind weaponType data.Value.Weapons with
                        | Some weaponData -> weaponData.WeaponSubtype = archetypeData.WeaponSubtype
                        | None -> false
                    | ArmorType armorType ->
                        match Map.tryFind armorType data.Value.Armors with
                        | Some armorData -> armorData.ArmorSubtype = archetypeData.ArmorSubtype
                        | None -> false
                    | AccessoryType _ -> true
                | KeyItem _ -> false
                | Stash _ -> false
            | None -> false
        | None -> false

    static member tryUseItem itemType legionnaire =
        if Legionnaire.canUseItem itemType legionnaire then
            match Map.tryFind legionnaire.CharacterType data.Value.Characters with
            | Some characterData ->
                match itemType with
                | Consumable consumableType ->
                    match consumableType with
                    | GreenHerb ->
                        let level = Algorithms.expPointsToLevel legionnaire.ExpPoints
                        let hpm = Algorithms.hitPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                        let legionnaire = { legionnaire with HitPoints = min hpm (legionnaire.HitPoints + 50) } // TODO: pull from data!
                        (true, None, legionnaire)
                    | RedHerb ->
                        let level = Algorithms.expPointsToLevel legionnaire.ExpPoints
                        let hpm = Algorithms.hitPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                        let legionnaire = { legionnaire with HitPoints = min hpm (legionnaire.HitPoints + 250) } // TODO: pull from data!
                        (true, None, legionnaire)
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType -> (true, Option.map (Equipment << WeaponType) legionnaire.WeaponOpt, { legionnaire with WeaponOpt = Some weaponType })
                    | ArmorType armorType -> (true, Option.map (Equipment << ArmorType) legionnaire.ArmorOpt, { legionnaire with ArmorOpt = Some armorType })
                    | AccessoryType accessoryType -> (true, Option.map (Equipment << AccessoryType) (List.tryHead legionnaire.Accessories), { legionnaire with Accessories = [accessoryType] })
                | KeyItem _ -> (false, None, legionnaire)
                | Stash _ -> (false, None, legionnaire)
            | None -> (false, None, legionnaire)
        else (false, None, legionnaire)

    static member finn =
        let index = 0
        let characterType = Ally Finn
        let character = Map.find characterType data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = None
        let armorOpt = None
        { LegionIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
          Accessories = [] }

    static member glenn =
        let index = 1
        let characterType = Ally Glenn
        let character = Map.find characterType data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = None
        let armorOpt = None
        { LegionIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
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
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list
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

    static member make characterData hitPoints techPoints expPoints weaponOpt armorOpt accessories =
        let levelBase = characterData.LevelBase
        let archetypeType = characterData.ArchetypeType
        let expPointsTotal = Algorithms.levelToExpPoints levelBase + expPoints
        let characterState =
            { ArchetypeType = archetypeType
              ExpPoints = expPointsTotal
              WeaponOpt = weaponOpt
              ArmorOpt = armorOpt
              Accessories = accessories
              HitPoints = hitPoints
              TechPoints = techPoints
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