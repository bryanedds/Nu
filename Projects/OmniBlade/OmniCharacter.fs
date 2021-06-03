// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharp.Reflection
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] CharacterState =
    { ArchetypeType : ArchetypeType
      ExpPoints : int
      WeaponOpt : WeaponType option
      ArmorOpt : ArmorType option
      Accessories : AccessoryType list
      HitPoints : int
      TechPoints : int
      Statuses : Map<StatusType, int>
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
    member this.Shield effectType = Algorithms.shield effectType this.Accessories this.Statuses this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member getAttackResult effectType (source : CharacterState) (target : CharacterState) =
        let power = source.Power
        let shield = target.Shield effectType
        let defendingScalar = if target.Defending then Constants.Battle.DefendingDamageScalar else 1.0f
        let damage = single (power - shield) * defendingScalar |> int |> max 1
        damage

    static member burndownStatuses burndown state =
        let statuses =
            Map.fold (fun statuses status burndown2 ->
                let burndown3 = burndown2 - burndown
                if burndown3 <= 0
                then Map.remove status statuses
                else Map.add status burndown3 statuses)
                Map.empty
                state.Statuses
        { state with Statuses = statuses }

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

type [<ReferenceEquality; NoComparison>] CharacterAnimationState =
    { TimeStart : int64
      AnimationSheet : Image AssetTag
      CharacterAnimationType : CharacterAnimationType
      Direction : Direction }

    static member face direction state =
        { state with Direction = direction }

    static member setCharacterAnimationType timeOpt characterAnimationType state =
        if state.CharacterAnimationType <> characterAnimationType then
            match timeOpt with
            | Some time -> { state with TimeStart = time; CharacterAnimationType = characterAnimationType }
            | None -> { state with CharacterAnimationType = characterAnimationType }
        else state

    static member directionToInt direction =
        match direction with
        | Upward -> 0
        | Rightward -> 1
        | Downward -> 2
        | Leftward -> 3

    static member timeLocal time state =
        time - state.TimeStart

    static member indexCel delay time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int (timeLocal / delay)

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

    static member progressOpt time state =
        match Map.tryFind state.CharacterAnimationType Data.Value.CharacterAnimations with
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
          AnimationSheet = Assets.Field.JinnAnimationSheet
          CharacterAnimationType = IdleAnimation
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

type [<ReferenceEquality; NoComparison>] AutoBattle =
    { AutoTarget : CharacterIndex
      AutoTechOpt : TechType option }

[<RequireQualifiedAccess>]
module Character =

    type [<ReferenceEquality; NoComparison>] Character =
        private
            { BoundsOriginal_ : Vector4
              Bounds_ : Vector4
              CharacterIndex_ : CharacterIndex
              CharacterType_ : CharacterType
              CharacterState_ : CharacterState
              CharacterAnimationState_ : CharacterAnimationState
              AutoBattleOpt_ : AutoBattle option
              ActionTime_ : int
              InputState_ : CharacterInputState }

        (* Bounds Original Properties *)
        member this.BoundsOriginal = this.BoundsOriginal_
        member this.PositionOriginal = this.BoundsOriginal_.Position
        member this.CenterOriginal = this.BoundsOriginal_.Center
        member this.BottomOriginal = this.BoundsOriginal_.Bottom
        member this.SizeOriginal = this.BoundsOriginal_.Size

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Helper Properties *)
        member this.CenterOffset = this.Center + Constants.Battle.CharacterCenterOffset
        member this.CenterOffset2 = this.Center + Constants.Battle.CharacterCenterOffset2
        member this.CenterOffset3 = this.Center + Constants.Battle.CharacterCenterOffset3
        member this.BottomOffset = this.Bottom + Constants.Battle.CharacterBottomOffset
        member this.BottomOffset2 = this.Bottom + Constants.Battle.CharacterBottomOffset2
        member this.BottomOffset3 = this.Bottom + Constants.Battle.CharacterBottomOffset3

        (* CharacterState Properties *)
        member this.Name = CharacterType.getName this.CharacterType_
        member this.CharacterIndex = this.CharacterIndex_
        member this.CharacterType = this.CharacterType_
        member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
        member this.IsAlly = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
        member this.IsEnemy = not this.IsAlly
        member this.ActionTime = this.ActionTime_
        member this.ArchetypeType = this.CharacterState_.ArchetypeType
        member this.ExpPoints = this.CharacterState_.ExpPoints
        member this.HitPoints = this.CharacterState_.HitPoints
        member this.TechPoints = this.CharacterState_.TechPoints
        member this.WeaponOpt = this.CharacterState_.WeaponOpt
        member this.ArmorOpt = this.CharacterState_.ArmorOpt
        member this.Accessories = this.CharacterState_.Accessories
        member this.Techs = this.CharacterState_.Techs
        member this.Statuses = this.CharacterState_.Statuses
        member this.Defending = this.CharacterState_.Defending
        member this.Charging = this.CharacterState_.Charging
        member this.IsHealthy = this.CharacterState_.IsHealthy
        member this.IsWounded = this.CharacterState_.IsWounded
        member this.Level = this.CharacterState_.Level
        member this.HitPointsMax = this.CharacterState_.HitPointsMax
        member this.Power = this.CharacterState_.Power
        member this.Magic = this.CharacterState_.Magic
        member this.Shield = this.CharacterState_.Shield
        member this.GoldPrize = this.CharacterState_.GoldPrize
        member this.ExpPrize = this.CharacterState_.ExpPrize

        (* Animation Properties *)
        member this.TimeStart = this.CharacterAnimationState_.TimeStart
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction

        (* Local Properties *)
        member this.AutoBattleOpt = this.AutoBattleOpt_
        member this.InputState = this.InputState_

    let isFriendly (character : Character) (character2 : Character) =
        CharacterIndex.isFriendly character.CharacterIndex character2.CharacterIndex

    let isReadyForAutoBattle character =
        Option.isNone character.AutoBattleOpt_ &&
        character.ActionTime >= Constants.Battle.AutoBattleReadyTime &&
        character.IsEnemy

    let isAutoBattling character =
        match character.AutoBattleOpt_ with
        | Some autoBattle -> Option.isSome autoBattle.AutoTechOpt
        | None -> false

    let shouldCounter (character : Character) =
        character.IsAlly
    
    let evaluateAutoBattle (source : Character) (target : Character) =
        let techOpt =
            if Gen.randomf < Option.getOrDefault 0.0f source.CharacterState_.TechProbabilityOpt
            then CharacterState.tryGetTechRandom source.CharacterState_
            else None
        { AutoTarget = target.CharacterIndex; AutoTechOpt = techOpt }

    let evaluateAimType aimType (target : Character) (characters : Map<CharacterIndex, Character>) =
        match aimType with
        | AnyAim healthy ->
            characters |>
            Map.filter (fun _ c -> c.IsHealthy = healthy)
        | EnemyAim healthy | AllyAim healthy ->
            characters |>
            Map.filter (fun _ c -> isFriendly target c) |>
            Map.filter (fun _ c -> c.IsHealthy = healthy)
        | NoAim ->
            Map.empty

    let evaluateTargetType targetType (source : Character) (target : Character) characters =
        match targetType with
        | SingleTarget _ ->
            Map.singleton target.CharacterIndex target
        | ProximityTarget (radius, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Bottom - source.Bottom
                v.Length () <= radius)
        | RadialTarget (radius, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Bottom - target.Bottom
                v.Length () <= radius)
        | LineTarget (offset, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Bottom - source.Bottom
                let b = target.Bottom - source.Bottom
                if Vector2.Dot (a, b) > 0.0f then
                    let r = a - (Vector2.Dot (a, b) / Vector2.Dot (b, b)) * b // vector rejection
                    let d = r.Length ()
                    d <= offset 
                else false)
        | SegmentTarget (offset, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Bottom - source.Bottom
                let b = target.Bottom - source.Bottom
                if a.Length () <= b.Length () then
                    if Vector2.Dot (a, b) > 0.0f then
                        let r = a - (Vector2.Dot (a, b) / Vector2.Dot (b, b)) * b // vector rejection
                        let d = r.Length ()
                        d <= offset
                    else false
                else false)
        | VerticalTarget (width, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let x = target.Bottom.X
                character.Bottom.X >= x - width &&
                character.Bottom.X <= x + width)
        | HorizontalTarget (width, aimType) ->
            characters |>
            evaluateAimType aimType target |>
            Map.filter (fun _ character ->
                let y = target.Bottom.Y
                character.Bottom.Y >= y - width &&
                character.Bottom.Y <= y + width)
        | AllTarget aimType ->
            characters |>
            evaluateAimType aimType target

    let evaluateTech techData source (target : Character) =
        let efficacy =
            match techData.EffectType with
            | Physical -> source.CharacterState_.Power
            | Magical -> source.CharacterState_.Magic
        if techData.Curative then
            let healing = single efficacy * techData.Scalar |> int |> max 1
            (target.CharacterIndex, false, healing)
        else
            let cancelled = techData.Cancels && isAutoBattling target
            let shield = target.Shield techData.EffectType
            let defendingScalar = if target.Defending then Constants.Battle.DefendingDamageScalar else 1.0f
            let damage = single (efficacy - shield) * techData.Scalar * defendingScalar |> int |> max 1
            (target.CharacterIndex, cancelled, -damage)

    let evaluateTechMove techData source target characters =
        let targets = evaluateTargetType techData.TargetType source target characters
        Map.fold (fun results _ target ->
            let (index, cancelled, delta) = evaluateTech techData source target
            Map.add index (cancelled, delta) results)
            Map.empty
            targets

    let getPoiseType character =
        CharacterState.getPoiseType character.CharacterState_

    let getAttackResult effectType source target =
        CharacterState.getAttackResult effectType source.CharacterState_ target.CharacterState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.CharacterAnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.CharacterAnimationState_

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.CharacterAnimationState_

    let getInputState character =
        character.InputState_

    let getActionTypeOpt character =
        match character.InputState_ with
        | AimReticles (item, _) ->
            let actionType =
                if typeof<ConsumableType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = item) then Consume (scvalue item)
                elif typeof<TechType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = item) then Tech (scvalue item)
                else Attack
            Some actionType
        | _ -> None

    let updateActionTime updater character =
        let actionTime = updater character.ActionTime_
        let actionTimeDelta = actionTime - character.ActionTime
        { character with
            ActionTime_ = updater character.ActionTime_
            CharacterState_ = CharacterState.burndownStatuses actionTimeDelta character.CharacterState_ }

    let updateStatuses updater character =
        let characterState = { character.CharacterState_ with Statuses = updater character.CharacterState_.Statuses }
        { character with CharacterState_ = characterState }

    let updateHitPoints updater affectWounded character =
        let (characterState, cancel) =
            if character.CharacterState_.IsHealthy || affectWounded then
                let (hitPoints, cancel) = updater character.CharacterState_.HitPoints
                let characterState = CharacterState.updateHitPoints (constant hitPoints) character.CharacterState_
                (characterState, cancel)
            else (character.CharacterState_, false)
        let autoBattleOpt =
            match character.AutoBattleOpt_ with
            | Some autoBattle when cancel -> Some { autoBattle with AutoTechOpt = None }
            | autoBattleOpt -> autoBattleOpt // use existing state if not cancelled
        { character with CharacterState_ = characterState; AutoBattleOpt_ = autoBattleOpt }

    let updateTechPoints updater character =
        { character with CharacterState_ = CharacterState.updateTechPoints updater character.CharacterState_ }

    let updateExpPoints updater character =
        { character with CharacterState_ = CharacterState.updateExpPoints updater character.CharacterState_ }

    let updateInputState updater character =
        { character with InputState_ = updater character.InputState_ }

    let updateAutoBattleOpt updater character =
        { character with AutoBattleOpt_ = updater character.AutoBattleOpt_ }

    let updateBounds updater (character : Character) =
        { character with Bounds_ = updater character.Bounds_ }

    let updatePosition updater (character : Character) =
        { character with Bounds_ = character.Position |> updater |> character.Bounds.WithPosition }

    let updateCenter updater (character : Character) =
        { character with Bounds_ = character.Center |> updater |> character.Bounds.WithCenter }

    let updateBottom updater (character : Character) =
        { character with Bounds_ = character.Bottom |> updater |> character.Bounds.WithBottom }

    let autoBattle (source : Character) (target : Character) =
        let sourceToTarget = target.Position - source.Position
        let direction = Direction.ofVector2 sourceToTarget
        let animationState = { source.CharacterAnimationState_ with Direction = direction }
        let autoBattle = evaluateAutoBattle source target
        { source with CharacterAnimationState_ = animationState; AutoBattleOpt_ = Some autoBattle }

    let defend character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Defending = true }
        { character with CharacterState_ = characterState }

    let undefend character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Defending = false }
        { character with CharacterState_ = characterState }

    let face direction character =
        { character with CharacterAnimationState_ = CharacterAnimationState.face direction character.CharacterAnimationState_ }

    let animate time characterAnimationType character =
        { character with CharacterAnimationState_ = CharacterAnimationState.setCharacterAnimationType (Some time) characterAnimationType character.CharacterAnimationState_ }

    let make bounds characterIndex characterType (characterState : CharacterState) animationSheet direction actionTime =
        let animationType = if characterState.IsHealthy then IdleAnimation else WoundAnimation
        let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; CharacterAnimationType = animationType; Direction = direction }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterIndex_ = characterIndex
          CharacterType_ = characterType
          CharacterState_ = characterState
          CharacterAnimationState_ = animationState
          AutoBattleOpt_ = None
          ActionTime_ = actionTime
          InputState_ = NoInput }

    let tryMakeEnemy index offsetCharacters enemyData =
        match Map.tryFind (Enemy enemyData.EnemyType) Data.Value.Characters with
        | Some characterData ->
            let archetypeType = characterData.ArchetypeType
            let size = Constants.Gameplay.CharacterSize
            let position = if offsetCharacters then enemyData.EnemyPosition + Constants.Battle.CharacterOffset else enemyData.EnemyPosition
            let bounds = v4Bounds position size
            let hitPoints = Algorithms.hitPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
            let techPoints = Algorithms.techPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
            let expPoints = Algorithms.levelToExpPoints characterData.LevelBase
            let characterType = characterData.CharacterType
            let characterState = CharacterState.make characterData hitPoints techPoints expPoints characterData.WeaponOpt characterData.ArmorOpt characterData.Accessories
            let actionTime = 250 - 50 * index // NOTE: started enemies in the negative, but I think that works with the algorithm okay. TODO: P1: put this in Constants.
            let enemy = make bounds (EnemyIndex index) characterType characterState characterData.AnimationSheet Rightward actionTime
            Some enemy
        | None -> None

    let empty =
        let bounds = v4Bounds v2Zero Constants.Gameplay.CharacterSize
        let characterAnimationState = { TimeStart = 0L; AnimationSheet = Assets.Field.JinnAnimationSheet; CharacterAnimationType = IdleAnimation; Direction = Downward }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterIndex_ = AllyIndex 0
          CharacterType_ = Ally Jinn
          CharacterState_ = CharacterState.empty
          CharacterAnimationState_ = characterAnimationState
          AutoBattleOpt_ = None
          ActionTime_ = 0
          InputState_ = NoInput }

type Character = Character.Character

type Party = Character list