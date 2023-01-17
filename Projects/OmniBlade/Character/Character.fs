// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharp.Reflection
open Prime
open Nu
open OmniBlade

[<RequireQualifiedAccess>]
module Character =

    type [<NoComparison>] Character =
        private
            { PerimeterOriginal_ : Box3
              Perimeter_ : Box3
              CharacterIndex_ : CharacterIndex
              CharacterType_ : CharacterType
              Boss_ : bool
              CharacterAnimationState_ : CharacterAnimationState
              CharacterInputState_ : CharacterInputState
              CharacterState_ : CharacterState
              ConjureChargeOpt_ : int option
              TechChargeOpt_ : (int * int * TechType) option
              AutoBattleOpt_ : AutoBattle option
              ActionTime_ : single
              CelSize_ : Vector2 }

        (* Perimeter Original Properties *)
        member this.PerimeterOriginal = this.PerimeterOriginal_
        member this.CenterOriginal = this.PerimeterOriginal_.Center
        member this.BottomOriginal = this.PerimeterOriginal_.Bottom
        member this.SizeOriginal = this.PerimeterOriginal_.Size

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.Size = this.Perimeter_.Size

        (* Helper Properties *)
        member this.CenterOffset = this.Center + Constants.Battle.CharacterCenterOffset
        member this.CenterOffset2 = this.Center + Constants.Battle.CharacterCenterOffset2
        member this.CenterOffset3 = this.Center + Constants.Battle.CharacterCenterOffset3
        member this.CenterOffset4 = this.Center + Constants.Battle.CharacterCenterOffset4
        member this.BottomOffset = this.Bottom + Constants.Battle.CharacterBottomOffset
        member this.BottomOffset2 = this.Bottom + Constants.Battle.CharacterBottomOffset2
        member this.BottomOffset3 = this.Bottom + Constants.Battle.CharacterBottomOffset3
        member this.BottomOffset4 = this.Bottom + Constants.Battle.CharacterBottomOffset4
        member this.BottomOffset5 = this.Bottom + Constants.Battle.CharacterBottomOffset5

        (* CharacterState Properties *)
        member this.Name = CharacterType.getName this.CharacterType_
        member this.CharacterIndex = this.CharacterIndex_
        member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
        member this.CharacterType = this.CharacterType_
        member this.IsAlly = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
        member this.IsEnemy = not this.IsAlly
        member this.Boss = this.Boss_
        member this.ActionTime = this.ActionTime_
        member this.CelSize = this.CelSize_
        member this.ArchetypeType = this.CharacterState_.ArchetypeType
        member this.ExpPoints = this.CharacterState_.ExpPoints
        member this.HitPoints = this.CharacterState_.HitPoints
        member this.TechPoints = this.CharacterState_.TechPoints
        member this.WeaponOpt = this.CharacterState_.WeaponOpt
        member this.ArmorOpt = this.CharacterState_.ArmorOpt
        member this.Accessories = this.CharacterState_.Accessories
        member this.Techs = this.CharacterState_.Techs
        member this.Stature = this.CharacterState_.Stature
        member this.Statuses = this.CharacterState_.Statuses
        member this.Defending = this.CharacterState_.Defending
        member this.Charging = this.CharacterState_.Charging
        member this.IsHealthy = this.CharacterState_.IsHealthy
        member this.IsWounded = this.CharacterState_.IsWounded
        member this.IsWounding = match this.CharacterAnimationState_.CharacterAnimationType with WoundAnimation -> true | _ -> false
        member this.Level = this.CharacterState_.Level
        member this.HitPointsMax = this.CharacterState_.HitPointsMax
        member this.TechPointsMax = this.CharacterState_.TechPointsMax
        member this.Power = this.CharacterState_.Power
        member this.Magic = this.CharacterState_.Magic
        member this.Shield = this.CharacterState_.Shield
        member this.Defense = this.CharacterState_.Defense
        member this.Absorb = this.CharacterState_.Absorb
        member this.AffinityOpt = this.CharacterState_.AffinityOpt
        member this.Immunities = this.CharacterState_.Immunities
        member this.GoldPrize = this.CharacterState_.GoldPrize
        member this.ExpPrize = this.CharacterState_.ExpPrize
        member this.ItemPrizeOpt = this.CharacterState_.ItemPrizeOpt

        (* Animation Properties *)
        member this.TimeStart = this.CharacterAnimationState_.StartTime
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction

        (* Local Properties *)
        member this.CharacterInputState = this.CharacterInputState_
        member this.ConjureChargeOpt = this.ConjureChargeOpt_
        member this.TechChargeOpt = this.TechChargeOpt_
        member this.AutoBattleOpt = this.AutoBattleOpt_

    let isFriendly (character : Character) (character2 : Character) =
        CharacterIndex.isFriendly character.CharacterIndex character2.CharacterIndex

    let isReadyForAutoBattle character =
        Option.isNone character.AutoBattleOpt_ &&
        character.IsEnemy &&
        character.ActionTime >= 60.0f

    let isAutoTeching character =
        match character.AutoBattleOpt_ with
        | Some autoBattle -> Option.isSome autoBattle.AutoTechOpt && not autoBattle.IsChargeTech
        | None -> false

    let shouldCounter (character : Character) =
        // TODO: pull this from stats
        character.ArchetypeType = Fighter &&
        Gen.random1 10 = 0

    let evalAimType aimType (target : Character) (characters : Map<CharacterIndex, Character>) =
        match aimType with
        | AnyAim healthy ->
            Map.filter (fun _ (c : Character) -> c.IsHealthy = healthy) characters
        | EnemyAim healthy | AllyAim healthy ->
            Map.filter (fun _ (c : Character) -> c.IsHealthy = healthy && isFriendly target c) characters
        | NoAim ->
            Map.empty

    let evaluateTargetType targetType (source : Character) (target : Character) characters =
        match targetType with
        | SingleTarget _ ->
            Map.singleton target.CharacterIndex target
        | ProximityTarget (radius, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Bottom - source.Bottom
                v.Magnitude <= radius)
        | RadialTarget (radius, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Bottom - target.Bottom
                v.Magnitude <= radius)
        | LineTarget (offset, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Bottom - source.Bottom
                let b = target.Bottom - source.Bottom
                if Vector3.Dot (a, b) > 0.0f then
                    let r = a - (Vector3.Dot (a, b) / Vector3.Dot (b, b)) * b // vector rejection
                    let d = r.Magnitude
                    d <= offset 
                else false)
        | SegmentTarget (offset, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Bottom - source.Bottom
                let b = target.Bottom - source.Bottom
                if a.Magnitude <= b.Magnitude then
                    if Vector3.Dot (a, b) > 0.0f then
                        let r = a - (Vector3.Dot (a, b) / Vector3.Dot (b, b)) * b // vector rejection
                        let d = r.Magnitude
                        d <= offset
                    else false
                else false)
        | VerticalTarget (width, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let x = target.Bottom.X
                character.Bottom.X >= x - width &&
                character.Bottom.X <= x + width)
        | HorizontalTarget (width, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let y = target.Bottom.Y
                character.Bottom.Y >= y - width &&
                character.Bottom.Y <= y + width)
        | AllTarget aimType ->
            characters |>
            evalAimType aimType target

    let evalTech (targetCount : int) techData source (target : Character) =
        let efficacy =
            match techData.EffectType with
            | Physical -> source.CharacterState_.Power
            | Magical -> source.CharacterState_.Magic (techData.AffinityOpt = Some Metal)
        let affinityScalar =
            match (source.AffinityOpt, techData.AffinityOpt) with
            | (Some affinitySource, Some affinityTarget) -> AffinityType.getScalar affinitySource affinityTarget
            | (_, _) -> 1.0f
        let techScalar =
            // NOTE: certain techs can't be used effectively by enemies, so they are given a special scalar.
            // TODO: pull this from TechData.EnemyScalarOpt.
            if source.IsEnemy then
                match techData.TechType with
                | Slash -> 1.333f
                | TechType.Flame -> 1.45f
                | Snowball -> 1.15f
                | Cure -> 1.5f
                | Aura -> 1f
                | Empower -> 0.75f
                | Enlighten -> 0.75f
                | Protect -> 0.75f
                | _ -> techData.Scalar
            else techData.Scalar
        let splitScalar =
            if source.IsAlly then
                if techData.Split && targetCount > 1
                then 1.5f / min 3.0f (single targetCount)
                else 1.0f
            else
                if techData.Split
                then Constants.Battle.EnemySplitScalar
                else 1.0f
        let specialAddend =
            // NOTE: certain techs need to be stronger at the start of the game but adjusting their scalars isn't adequate.
            // TODO: consider pulling this from TechData.AddendOpt.
            match techData.TechType with
            | Critical -> 1.0f
            | _ -> 0.0f
        if techData.Curative then
            let healing = single efficacy * techScalar * splitScalar |> int |> max 1
            (target.CharacterIndex, false, false, healing, techData.StatusesAdded, techData.StatusesRemoved)
        else
            let cancelled = techData.Cancels && isAutoTeching target
            let shield = target.Shield techData.EffectType
            let defendingScalar = if target.Defending then Constants.Battle.DefendingScalar else 1.0f
            let damage = (single efficacy * affinityScalar * techScalar * splitScalar + specialAddend - single shield) * defendingScalar |> int |> max 1
            (target.CharacterIndex, cancelled, false, -damage, Set.difference techData.StatusesAdded target.Immunities, techData.StatusesRemoved)

    let evalTechMove techData source target characters =
        let targets = evaluateTargetType techData.TargetType source target characters
        let targetsCount = Map.count targets
        Map.fold (fun results _ target ->
            let (index, cancelled, affectsWounded, delta, added, removed) = evalTech targetsCount techData source target
            Map.add index (cancelled, affectsWounded, delta, added, removed) results)
            Map.empty
            targets

    let getPoiseType character =
        CharacterState.getPoiseType character.CharacterState_

    let getAttackResult effectType source target =
        CharacterState.getAttackResult effectType source.CharacterState_ target.CharacterState_

    let getAnimationInset time (character : Character) =
        CharacterAnimationState.inset time character.CelSize_ character.CharacterAnimationState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.CharacterAnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.CharacterAnimationState_

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.CharacterAnimationState_

    let getCharacterInputState character =
        character.CharacterInputState_

    let getActionTypeOpt character =
        match character.CharacterInputState_ with
        | AimReticles (item, _) ->
            let actionType =
                if typeof<ConsumableType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = item) then Consume (scvalue item)
                elif typeof<TechType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = item) then Tech (scvalue item)
                else Attack
            Some actionType
        | _ -> None

    let getConjureTechs (character : Character) =
        if character.IsAlly
        then character.Techs |> Set.filter (fun techType -> techType.ConjureTech)
        else Set.empty

    let hasConjureTechs character =
        getConjureTechs character |> Set.notEmpty

    let burndownStatuses burndownTime character =
        { character with CharacterState_ = CharacterState.burndownStatuses burndownTime character.CharacterState_ }

    let updateCharacterInputState updater character =
        { character with CharacterInputState_ = updater character.CharacterInputState_ }

    let updateStatuses updater character =
        let characterState = { character.CharacterState_ with Statuses = updater character.CharacterState_.Statuses }
        { character with CharacterState_ = characterState }

    let updateHitPoints updater affectWounded alliesHealthy character =
        let (cancel, characterState) =
            if character.CharacterState_.IsHealthy || affectWounded then
                let (cancel, hitPoints) = updater character.CharacterState_.HitPoints
                let characterState = CharacterState.updateHitPoints (constant hitPoints) character.CharacterState_
                (cancel, characterState)
            else (false, character.CharacterState_)
        let autoBattleOpt =
            match character.AutoBattleOpt_ with
            | Some autoBattle when cancel && not autoBattle.IsChargeTech -> // cannot cancel charge tech
                match autoBattle.AutoTarget with
                | AllyIndex _ as ally -> Some { AutoTarget = ally; AutoTechOpt = None; IsChargeTech = false }
                | EnemyIndex _ ->
                    match Gen.randomKeyOpt alliesHealthy with
                    | Some ally -> Some { AutoTarget = ally; AutoTechOpt = None; IsChargeTech = false }
                    | None -> None
            | autoBattleOpt -> autoBattleOpt // use existing state if not cancelled
        { character with CharacterState_ = characterState; AutoBattleOpt_ = autoBattleOpt }

    let updateTechPoints updater character =
        { character with CharacterState_ = CharacterState.updateTechPoints updater character.CharacterState_ }

    let updateExpPoints updater character =
        { character with CharacterState_ = CharacterState.updateExpPoints updater character.CharacterState_ }

    let updateConjureChargeOpt updater character =
        { character with ConjureChargeOpt_ = updater character.ConjureChargeOpt_ }

    let updateTechChargeOpt updater character =
        { character with TechChargeOpt_ = updater character.TechChargeOpt_ }

    let updateAutoBattleOpt updater character =
        { character with AutoBattleOpt_ = updater character.AutoBattleOpt_ }

    let updateActionTime updater character =
        { character with ActionTime_ = updater character.ActionTime_ }

    let updatePerimeter updater (character : Character) =
        { character with Perimeter_ = updater character.Perimeter_ }

    let updateBottom updater (character : Character) =
        { character with Perimeter_ = character.Bottom |> updater |> character.Perimeter.WithBottom }

    let applyStatusChanges statusesAdded statusesRemoved (character : Character) =
        if character.IsHealthy then
            let character =
                updateStatuses (fun statuses ->
                    let statuses = Set.fold (fun statuses status -> Map.add status Constants.Battle.BurndownTime statuses) statuses statusesAdded
                    let statuses = Set.fold (fun statuses status -> Map.remove status statuses) statuses statusesRemoved
                    statuses)
                    character
            updateActionTime (fun actionTime ->
                if  statusesAdded.Contains (Time false) &&
                    actionTime < Constants.Battle.ActionTime then
                    actionTime * Constants.Battle.ActionTimeSlowScalar character.Boss_
                else actionTime)
                character
        else character

    let resetConjureCharge character =
        updateConjureChargeOpt (Option.map (constant -Constants.Battle.ConjureChargeRate)) character

    let advanceConjureCharge (character : Character) =
        if hasConjureTechs character then
            match character.ConjureChargeOpt with
            | Some conjureCharge ->
                { character with ConjureChargeOpt_ = Some (conjureCharge + Constants.Battle.ConjureChargeRate) }
            | None ->
                { character with ConjureChargeOpt_ = Some 0 }
        else character

    let resetTechCharge (character : Character) =
        updateTechChargeOpt
            (function
             | Some (_, chargeTime, _) as chargeTechOpt ->
                if chargeTime >= Constants.Battle.ChargeMax then
                    let chargeTechs = Algorithms.chargeTechs character.ArchetypeType character.Level
                    chargeTechs |> Gen.randomItemOpt |> Option.map (fun (chargeRate, chargeTech) -> (chargeRate, -chargeRate, chargeTech))
                else chargeTechOpt
             | None -> None)
            character

    let advanceTechCharge (character : Character) =
        updateTechChargeOpt
            (function
             | Some (chargeRate, chargeTime, techType) -> Some (chargeRate, chargeRate + chargeTime, techType)
             | None -> None)
            character

    let autoBattle (alliesHealthy : Map<_, _>) alliesWounded enemiesHealthy enemiesWounded (source : Character) =

        // TODO: once techs have the ability to revive, check for that in the curative case.
        ignore (enemiesWounded, alliesWounded)

        // advance tech charge
        let source = advanceTechCharge source

        // choose a tech
        let (techOpt, isChargeTech) =

            // see if we're charged
            match source.TechChargeOpt with
            | Some (_, chargeAmount, chargeTech) when chargeAmount >= Constants.Battle.ChargeMax -> (Some chargeTech, true)
            | Some _ | None ->
                if  Gen.randomf < Option.defaultValue 0.0f source.CharacterState_.TechProbabilityOpt &&
                    not (Map.containsKey Silence source.Statuses) then // silence only blocks non-charge techs
                    let techOpt = CharacterState.tryGetTechRandom source.CharacterState_
                    (techOpt, false)
                else (None, false)

        // attempt to randomly choose a target
        let targetOpt =
            match techOpt with
            | Some tech ->
                match Data.Value.Techs.TryGetValue tech with
                | (true, techData) ->
                    if not techData.Curative then
                        let leadAlly = AllyIndex 0 // have 50% chance of selecting lead ally
                        if Seq.length alliesHealthy > 2 && alliesHealthy.ContainsKey leadAlly then
                            if Gen.randomb
                            then Some alliesHealthy.[leadAlly]
                            else alliesHealthy |> Map.remove leadAlly |> Gen.randomValueOpt
                        else Gen.randomValueOpt alliesHealthy
                    else Gen.randomValueOpt enemiesHealthy
                | (false, _) -> None
            | None -> Gen.randomValueOpt alliesHealthy

        // attempt to update character with auto-battle and appropriate facing direction
        match targetOpt with
        | Some (target : Character) ->
            let sourceToTarget = target.Bottom - source.Bottom
            let direction = if sourceToTarget.X >= 0.0f then Rightward else Leftward // only two directions in this game
            let animationState = { source.CharacterAnimationState_ with Direction = direction }
            let autoBattle = { AutoTarget = target.CharacterIndex; AutoTechOpt = techOpt; IsChargeTech = isChargeTech }
            { source with CharacterAnimationState_ = animationState; AutoBattleOpt_ = Some autoBattle }
        | None -> source

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

    let make bounds characterIndex characterType boss animationSheet celSize direction (characterState : CharacterState) chargeTechOpt actionTime =
        let animationType = if characterState.IsHealthy then IdleAnimation else WoundAnimation
        let animationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = animationType; Direction = direction }
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterIndex_ = characterIndex
          CharacterType_ = characterType
          Boss_ = boss
          CharacterAnimationState_ = animationState
          CharacterInputState_ = NoInput
          CharacterState_ = characterState
          ConjureChargeOpt_ = None
          TechChargeOpt_ = chargeTechOpt
          AutoBattleOpt_ = None
          ActionTime_ = actionTime
          CelSize_ = celSize }

    let tryMakeEnemy allyCount index offsetCharacters waitSpeed enemyData =
        match Map.tryFind (Enemy enemyData.EnemyType) Data.Value.Characters with
        | Some characterData ->
            let archetypeType = characterData.ArchetypeType
            match Data.Value.Archetypes.TryFind characterData.ArchetypeType with
            | Some archetypeData ->
                let (size, celSize) =
                    match archetypeData.Stature with
                    | SmallStature | NormalStature | LargeStature -> (Constants.Gameplay.CharacterSize, Constants.Gameplay.CharacterCelSize)
                    | BossStature -> (Constants.Gameplay.BossSize, Constants.Gameplay.BossCelSize)
                let position = if offsetCharacters then enemyData.EnemyPosition + Constants.Battle.CharacterOffset else enemyData.EnemyPosition
                let bounds = box3 position size
                let hitPoints = Algorithms.hitPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
                let techPoints = Algorithms.techPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
                let expPoints = Algorithms.levelToExpPoints characterData.LevelBase
                let chargeTechs = Algorithms.chargeTechs archetypeType characterData.LevelBase
                let chargeTechOpt = chargeTechs |> Gen.randomItemOpt |> Option.map (fun (chargeRate, chargeTech) -> (chargeRate, -chargeRate, chargeTech))
                let characterType = characterData.CharacterType
                let characterState = CharacterState.make characterData hitPoints techPoints expPoints characterData.WeaponOpt characterData.ArmorOpt characterData.Accessories
                let actionTime =
                    if waitSpeed then       1000.0f - 125.0f - Gen.randomf1 8.0f * 75.0f
                    elif allyCount = 1 then 1000.0f - 400.0f - Gen.randomf1 6.0f * 75.0f
                    else                    1000.0f - 450.0f - Gen.randomf1 8.0f * 75.0f
                let enemy = make bounds (EnemyIndex index) characterType characterData.Boss characterData.AnimationSheet celSize Rightward characterState chargeTechOpt actionTime
                Some enemy
            | None -> None
        | None -> None

    let empty =
        let bounds = box3 v3Zero Constants.Gameplay.CharacterSize
        let characterAnimationState = { StartTime = 0L; AnimationSheet = Assets.Field.JinnAnimationSheet; CharacterAnimationType = IdleAnimation; Direction = Downward }
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterIndex_ = AllyIndex 0
          CharacterType_ = Ally Jinn
          Boss_ = false
          CharacterAnimationState_ = characterAnimationState
          CharacterInputState_ = NoInput
          CharacterState_ = CharacterState.empty
          ConjureChargeOpt_ = None
          TechChargeOpt_ = None
          AutoBattleOpt_ = None
          ActionTime_ = 0.0f
          CelSize_ = Constants.Gameplay.CharacterSize.V2 }

type Character = Character.Character

type Party = Character list