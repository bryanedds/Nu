// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Collections.Generic
open System.Numerics
open FSharp.Reflection
open Prime
open Nu

type [<NoComparison>] AutoBattle =
    { AutoTarget : CharacterIndex
      AutoTechOpt : TechType option
      IsChargeTech : bool }

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

type [<NoComparison>] CharacterAnimationState =
    { StartTime : int64
      AnimationSheet : Image AssetTag
      CharacterAnimationType : CharacterAnimationType
      Direction : Direction }

    static member face direction state =
        { state with Direction = direction }

    static member setCharacterAnimationType timeOpt characterAnimationType state =
        if state.CharacterAnimationType <> characterAnimationType then
            match timeOpt with
            | Some time -> { state with StartTime = time; CharacterAnimationType = characterAnimationType }
            | None -> { state with CharacterAnimationType = characterAnimationType }
        else state

    static member directionToInt direction =
        match direction with
        | Upward -> 0
        | Rightward -> 1
        | Downward -> 2
        | Leftward -> 3

    static member localTime time state =
        time - state.StartTime

    static member indexCel delay time state =
        let localTime = CharacterAnimationState.localTime time state
        int (localTime / delay)

    static member indexLooped run delay time state =
        CharacterAnimationState.indexCel delay time state % run

    static member indexSaturated run delay time state =
        let cel = CharacterAnimationState.indexCel delay time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run delay time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run delay offset time state =
        let position = CharacterAnimationState.indexLooped run delay time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run delay offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run delay time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match Map.tryFind state.CharacterAnimationType Data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Delay animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Delay animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Delay animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Delay animationData.Offset time state
        | None -> v2iZero

    static member inset time (celSize : Vector2) state =
        let index = CharacterAnimationState.index time state
        let offset = v2 (single index.X) (single index.Y) * celSize
        let inset = box2 offset celSize
        inset

    static member progressOpt time state =
        match Map.tryFind state.CharacterAnimationType Data.Value.CharacterAnimations with
        | Some animationData ->
            let localTime = CharacterAnimationState.localTime time state
            match animationData.LengthOpt with
            | Some length -> Some (min 1.0f (single localTime / single length))
            | None -> None
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> true

    static member empty =
        { StartTime = 0L
          AnimationSheet = Assets.Field.JinnAnimationSheet
          CharacterAnimationType = IdleAnimation
          Direction = Downward }

    static member initial =
        { CharacterAnimationState.empty with Direction = Upward }

type [<NoComparison>] CharacterState =
    { ArchetypeType : ArchetypeType
      ExpPoints : int
      AbsorbCreep : single
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      HitPoints : int
      TechPoints : int
      Statuses : Map<StatusType, single>
      Defending : bool // also applies a perhaps stackable buff for attributes such as countering or magic power depending on class
      Charging : bool
      TechProbabilityOpt : single option
      GoldPrize : int
      ExpPrize : int
      ItemPrizeOpt : ItemType option }

    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt this.Statuses this.ArchetypeType this.Level
    member this.Magic = Algorithms.magic this.WeaponOpt this.Statuses this.ArchetypeType this.Level
    member this.Shield effectType = Algorithms.shield effectType this.AbsorbCreep this.Accessories this.Statuses this.ArchetypeType this.Level
    member this.Defense = Algorithms.defense this.Accessories this.Statuses this.ArchetypeType this.Level
    member this.Absorb = Algorithms.absorb this.AbsorbCreep this.Accessories this.Statuses this.ArchetypeType this.Level
    member this.AffinityOpt = Algorithms.affinityOpt this.Accessories this.ArchetypeType this.Level
    member this.Immunities = Algorithms.immunities this.Accessories this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level
    member this.ChargeTechs = Algorithms.chargeTechs this.ArchetypeType this.Level
    member this.Stature = match Map.tryFind this.ArchetypeType Data.Value.Archetypes with Some archetypeData -> archetypeData.Stature | None -> NormalStature

    static member getAttackResult effectType (source : CharacterState) (target : CharacterState) =
        let power = source.Power
        let shield = target.Shield effectType
        let defendingScalar = if target.Defending then Constants.Battle.DefendingScalar else 1.0f
        let damage = single (power - shield) * defendingScalar |> int |> max 1
        damage

    static member burndownStatuses burndown state =
        let statuses =
            Map.fold (fun statuses status burndown2 ->
                let burndown3 = burndown2 - burndown
                if burndown3 <= 0.0f
                then Map.remove status statuses
                else Map.add status burndown3 statuses)
                Map.empty
                state.Statuses
        { state with Statuses = statuses }

    static member updateHitPoints updater (state : CharacterState) =
        let hitPoints = updater state.HitPoints
        let hitPoints = max 0 hitPoints
        let hitPoints = min state.HitPointsMax hitPoints
        { state with
            HitPoints = hitPoints
            Statuses = if hitPoints = 0 then Map.empty else state.Statuses }

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
        Gen.randomItemOpt state.Techs

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

    static member make (characterData : CharacterData) hitPoints techPoints expPoints weaponOpt armorOpt accessories =
        let archetypeType = characterData.ArchetypeType
        let absorbCreep = characterData.AbsorbCreep
        let level = Algorithms.expPointsToLevel expPoints
        let characterState =
            { ArchetypeType = archetypeType
              ExpPoints = expPoints
              AbsorbCreep = absorbCreep
              WeaponOpt = weaponOpt
              ArmorOpt = armorOpt
              Accessories = accessories
              HitPoints = hitPoints
              TechPoints = techPoints
              Statuses = Map.empty
              Defending = false
              Charging = false
              TechProbabilityOpt = characterData.TechProbabilityOpt
              GoldPrize = Algorithms.goldPrize archetypeType characterData.GoldScalar level
              ExpPrize = Algorithms.expPrize archetypeType characterData.ExpScalar level
              ItemPrizeOpt = Algorithms.itemPrizeOpt archetypeType level }
        characterState

    static member empty =
        let characterState =
            { ArchetypeType = Apprentice
              ExpPoints = 0
              AbsorbCreep = 1.0f
              WeaponOpt = None
              ArmorOpt = None
              Accessories = []
              HitPoints = 1
              TechPoints = 0
              Statuses = Map.empty
              Defending = false
              Charging = false
              TechProbabilityOpt = None
              GoldPrize = 0
              ExpPrize = 0
              ItemPrizeOpt = None }
        characterState