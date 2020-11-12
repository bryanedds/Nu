// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

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

type [<ReferenceEquality; NoComparison>] PrizePool =
    { Consequents : Advent Set
      Items : ItemType list
      Gold : int
      Exp : int }

type [<ReferenceEquality; NoComparison>] Inventory =
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

    static member addItems items inventory =
        List.fold (flip Inventory.addItem) inventory items

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

    static member updateGold updater (inventory : Inventory) =
        { inventory with Gold = updater inventory.Gold }

    static member initial =
        { Items = Map.singleton (Consumable GreenHerb) 1; Gold = 0 }

type [<ReferenceEquality; NoComparison>] Teammate =
    { TeamIndex : int // key
      PartyIndexOpt : int option
      ArchetypeType : ArchetypeType
      CharacterType : CharacterType
      HitPoints : int
      TechPoints : int
      ExpPoints : int
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list }

    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt 1.0f this.ArchetypeType this.Level
    member this.Magic = Algorithms.magic this.WeaponOpt 1.0f this.ArchetypeType this.Level
    member this.Shield effectType = Algorithms.shield effectType this.Accessories 1.0f this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member equipWeaponOpt weaponTypeOpt teammate =
        { teammate with WeaponOpt = weaponTypeOpt }

    static member equipArmorOpt weaponTypeOpt teammate =
        let teammate = { teammate with WeaponOpt = weaponTypeOpt }
        let teammate = { teammate with HitPoints = min teammate.HitPoints teammate.HitPointsMax; TechPoints = min teammate.TechPoints teammate.HitPointsMax }
        teammate

    static member equipAccessory1Opt accessoryTypeOpt teammate =
        { teammate with Accessories = Option.toList accessoryTypeOpt }

    static member canUseItem itemType teammate =
        match Map.tryFind teammate.CharacterType Data.Value.Characters with
        | Some characterData ->
            match Map.tryFind characterData.ArchetypeType Data.Value.Archetypes with
            | Some archetypeData ->
                match itemType with
                | Consumable _ -> true
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType ->
                        match Map.tryFind weaponType Data.Value.Weapons with
                        | Some weaponData -> weaponData.WeaponSubtype = archetypeData.WeaponSubtype
                        | None -> false
                    | ArmorType armorType ->
                        match Map.tryFind armorType Data.Value.Armors with
                        | Some armorData -> armorData.ArmorSubtype = archetypeData.ArmorSubtype
                        | None -> false
                    | AccessoryType _ -> true
                | KeyItem _ -> false
                | Stash _ -> false
            | None -> false
        | None -> false

    static member tryUseItem itemType teammate =
        if Teammate.canUseItem itemType teammate then
            match Map.tryFind teammate.CharacterType Data.Value.Characters with
            | Some characterData ->
                match itemType with
                | Consumable consumableType ->
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumableData) ->
                        match consumableType with
                        | GreenHerb | RedHerb | GoldHerb ->
                            let level = Algorithms.expPointsToLevel teammate.ExpPoints
                            let hpm = Algorithms.hitPointsMax teammate.ArmorOpt characterData.ArchetypeType level
                            let teammate = { teammate with HitPoints = min hpm (teammate.HitPoints + int consumableData.Scalar) }
                            (true, None, teammate)
                    | (false, _) -> (false, None, teammate)
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType -> (true, Option.map (Equipment << WeaponType) teammate.WeaponOpt, Teammate.equipWeaponOpt (Some weaponType) teammate)
                    | ArmorType armorType -> (true, Option.map (Equipment << ArmorType) teammate.ArmorOpt, Teammate.equipArmorOpt (Some armorType) teammate)
                    | AccessoryType accessoryType -> (true, Option.map (Equipment << AccessoryType) (List.tryHead teammate.Accessories), Teammate.equipAccessory1Opt (Some accessoryType) teammate)
                | KeyItem _ -> (false, None, teammate)
                | Stash _ -> (false, None, teammate)
            | None -> (false, None, teammate)
        else (false, None, teammate)

    static member getExpPointsForNextLevel teammate =
        let level = Algorithms.expPointsToLevel teammate.ExpPoints
        let (_, nextExp) = Algorithms.levelToExpPointsRange level
        nextExp

    static member getExpPointsRemainingForNextLevel teammate =
        match Teammate.getExpPointsForNextLevel teammate with
        | Int32.MaxValue -> 0
        | nextExp -> nextExp - teammate.ExpPoints

    static member restore teammate =
        { teammate with
            HitPoints = teammate.HitPointsMax
            TechPoints = teammate.TechPointsMax }

    static member finn =
        let index = 0
        let characterType = Ally Finn
        let character = Map.find characterType Data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = Some "OakSword"
        let armorOpt = Some "LeatherMail"
        { ArchetypeType = archetypeType
          TeamIndex = index
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
        let character = Map.find characterType Data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = Some "StoneSword"
        let armorOpt = None
        { ArchetypeType = archetypeType
          TeamIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
          Accessories = [] }

type Team =
    Map<int, Teammate>

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int
    static member isTeammate index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

type [<ReferenceEquality; NoComparison>] CharacterState =
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
      CounterBuff : single
      GoldPrize : int
      ExpPrize : int }

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
        let techPoints = updater state.TechPoints
        let techPoints = max 0 techPoints
        let techPoints = min state.TechPointsMax techPoints
        { state with TechPoints = techPoints }

    static member updateExpPoints updater state =
        let expPoints = updater state.ExpPoints
        let expPoints = max 0 expPoints
        { state with ExpPoints = expPoints }

    static member tryGetTechRandom (state : CharacterState) =
        let techs = state.Techs
        if Set.notEmpty techs then
            let techIndex = Gen.random1 techs.Count
            let tech = Seq.item techIndex techs
            Some tech
        else None

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

    static member make (characterData : CharacterData) hitPoints techPoints expPoints weaponOpt armorOpt accessories =
        let archetypeType = characterData.ArchetypeType
        let level = Algorithms.expPointsToLevel expPoints
        let characterState =
            { ArchetypeType = archetypeType
              ExpPoints = expPoints
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
              CounterBuff = 1.0f
              GoldPrize = Algorithms.goldPrize characterData.GoldScalar level
              ExpPrize = Algorithms.expPrize characterData.ExpScalar level }
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
              CounterBuff = 1.0f
              GoldPrize = 0
              ExpPrize = 0 }
        characterState

type [<ReferenceEquality; NoComparison>] CharacterAnimationState =
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
        match Map.tryFind state.AnimationCycle Data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
        | None -> v2iZero

    static member progressOpt time state =
        match Map.tryFind state.AnimationCycle Data.Value.CharacterAnimations with
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

    static member initial =
        { CharacterAnimationState.empty with Direction = Upward }

type CharacterInputState =
    | NoInput
    | RegularMenu
    | TechMenu
    | ItemMenu
    | AimReticles of string * AimType

    member this.AimType =
        match this with
        | NoInput | RegularMenu | TechMenu | ItemMenu -> NoAim
        | AimReticles (_, aimType) -> aimType