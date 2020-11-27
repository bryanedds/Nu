// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharp.Reflection
open Prime
open Nu

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
              AnimationState_ : CharacterAnimationState
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

        (* AnimationState Properties *)
        member this.TimeStart = this.AnimationState_.TimeStart
        member this.AnimationSheet = this.AnimationState_.AnimationSheet
        member this.AnimationCycle = this.AnimationState_.AnimationCycle
        member this.Direction = this.AnimationState_.Direction

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

    let evaluateAutoBattle source (target : Character) =
        let techOpt =
            match Gen.random1 Constants.Battle.AutoBattleTechFrequency with
            | 0 -> CharacterState.tryGetTechRandom source.CharacterState_
            | _ -> None
        { AutoTarget = target.CharacterIndex; AutoTechOpt = techOpt }

    let evaluateAimType aimType (target : Character) (characters : Character list) =
        match aimType with
        | AnyAim healthy ->
            characters |>
            List.filter (fun target -> target.IsHealthy = healthy)
        | EnemyAim healthy | AllyAim healthy ->
            characters |>
            List.filter (isFriendly target) |>
            List.filter (fun target -> target.IsHealthy = healthy)
        | NoAim ->
            []

    let evaluateTargetType targetType (source : Character) target characters =
        match targetType with
        | SingleTarget _ ->
            [target]
        | ProximityTarget (aimType, radius) ->
            characters |>
            evaluateAimType aimType target |>
            List.filter (fun character ->
                let v = character.Bottom - source.Bottom
                v.Length () <= radius)
        | RadialTarget (aimType, radius) ->
            characters |>
            evaluateAimType aimType target |>
            List.filter (fun character ->
                let v = character.Bottom - target.Bottom
                v.Length () <= radius)
        | LineTarget (aimType, width) ->
            characters |>
            evaluateAimType aimType target |>
            List.filter (fun character ->
                let a = source.Bottom
                let b = target.Bottom
                let c = character.Bottom
                let (ab, ac, bc) = (b - a, c - a, c - b)
                let e = Vector2.Dot (ac, ab)
                let d =
                    if e > 0.0f then
                        let f = Vector2.Dot(ab, ab);
                        if e < f
                        then Vector2.Dot (ac, ac) - e * e / f
                        else Vector2.Dot (bc, bc)
                    else Vector2.Dot (ac, ac)
                d <= width)
        | AllTarget aimType ->
            characters |>
            evaluateAimType aimType target

    let evaluateTech techData source (target : Character) =
        let power = source.CharacterState_.Power
        if techData.Curative then
            let healing = single power * techData.Scalar |> int |> max 1
            (false, healing, target.CharacterIndex)
        else
            let cancelled = techData.Cancels && isAutoBattling target
            let shield = target.Shield techData.EffectType
            let defendingScalar = if target.Defending then Constants.Battle.DefendingDamageScalar else 1.0f
            let damage = single (power - shield) * techData.Scalar * defendingScalar |> int |> max 1
            (cancelled, -damage, target.CharacterIndex)

    let evaluateTechMove techData source target characters =
        let targets =
            evaluateTargetType techData.TargetType source target characters
        let resultsRev =
            List.fold (fun results target ->
                let result = evaluateTech techData source target
                result :: results)
                [] targets
        List.rev resultsRev

    let getPoiseType character =
        CharacterState.getPoiseType character.CharacterState_

    let getAttackResult effectType source target =
        CharacterState.getAttackResult effectType source.CharacterState_ target.CharacterState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.AnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.AnimationState_

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.AnimationState_

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

    let updateHitPoints updater character =
        let (hitPoints, cancel) = updater character.CharacterState_.HitPoints
        let characterState = CharacterState.updateHitPoints (constant hitPoints) character.CharacterState_
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
        let animationState = { source.AnimationState_ with Direction = direction }
        let autoBattle = evaluateAutoBattle source target
        { source with AnimationState_ = animationState; AutoBattleOpt_ = Some autoBattle }

    let defend character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Defending = true }
        { character with CharacterState_ = characterState }

    let undefend character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Defending = false }
        { character with CharacterState_ = characterState }

    let animate time cycle character =
        { character with AnimationState_ = CharacterAnimationState.setCycle (Some time) cycle character.AnimationState_ }

    let make bounds characterIndex characterType (characterState : CharacterState) animationSheet direction actionTime =
        let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = IdleCycle; Direction = direction }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterIndex_ = characterIndex
          CharacterType_ = characterType
          CharacterState_ = characterState
          AnimationState_ = animationState
          AutoBattleOpt_ = None
          ActionTime_ = actionTime
          InputState_ = NoInput }

    let tryMakeEnemy index enemyData =
        match Map.tryFind (Enemy enemyData.EnemyType) Data.Value.Characters with
        | Some characterData ->
            let archetypeType = characterData.ArchetypeType
            let bounds = v4Bounds enemyData.EnemyPosition Constants.Gameplay.CharacterSize
            let hitPoints = Algorithms.hitPointsMax None archetypeType characterData.LevelBase
            let techPoints = Algorithms.techPointsMax None archetypeType characterData.LevelBase
            let expPoints = Algorithms.levelToExpPoints characterData.LevelBase
            let characterType = characterData.CharacterType
            let characterState = CharacterState.make characterData hitPoints techPoints expPoints characterData.WeaponOpt characterData.ArmorOpt characterData.Accessories
            let actionTime = Constants.Battle.EnemyActionTimeInitial
            let enemy = make bounds (EnemyIndex index) characterType characterState characterData.AnimationSheet Leftward actionTime
            Some enemy
        | None -> None

    let empty =
        let bounds = v4Bounds v2Zero Constants.Gameplay.CharacterSize
        let animationState = { TimeStart = 0L; AnimationSheet = Assets.FinnAnimationSheet; AnimationCycle = IdleCycle; Direction = Downward }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterIndex_ = AllyIndex 0
          CharacterType_ = Ally Finn
          CharacterState_ = CharacterState.empty
          AnimationState_ = animationState
          AutoBattleOpt_ = None
          ActionTime_ = 0
          InputState_ = NoInput }

type Character = Character.Character