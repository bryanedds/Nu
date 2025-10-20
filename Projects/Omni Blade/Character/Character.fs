﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open FSharp.Reflection
open Prime
open Nu

[<RequireQualifiedAccess>]
module Character =

    type [<ReferenceEquality; SymbolicExpansion>] Character =
        private
            { PerimeterOriginal_ : Box3
              Perimeter_ : Box3
              CharacterIndex_ : CharacterIndex
              CharacterOrderRev_ : int
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

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_

        (* CharacterState Properties *)
        member this.Name = this.CharacterType_.Name
        member this.CharacterIndex = this.CharacterIndex_
        member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
        member this.CharacterOrderRev = this.CharacterOrderRev_
        member this.CharacterType = this.CharacterType_
        member this.Ally = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
        member this.Enemy = not this.Ally
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
        member this.Vulnerabilities = this.CharacterState_.Vulnerabilities
        member this.Interactions = this.CharacterState_.Interactions
        member this.Healthy = this.CharacterState_.Healthy
        member this.Wounded = this.CharacterState_.Wounded
        member this.Standing = match this.CharacterAnimationState_.CharacterAnimationType with WoundAnimation -> false | _ -> true
        member this.Swooning = not this.Standing
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
        member this.AnimationStartTime = this.CharacterAnimationState_.StartTime
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.MaterializationOpt = this.CharacterAnimationState_.MaterializationOpt
        member this.Direction = this.CharacterAnimationState_.Direction

        (* Local Properties *)
        member this.CharacterInputState = this.CharacterInputState_
        member this.ConjureChargeOpt = this.ConjureChargeOpt_
        member this.TechChargeOpt = this.TechChargeOpt_
        member this.AutoBattleOpt = this.AutoBattleOpt_

    let friendly (character : Character) (character2 : Character) =
        CharacterIndex.friendly character.CharacterIndex character2.CharacterIndex

    let readyForAutoBattle character =
        Option.isNone character.AutoBattleOpt_ &&
        character.Enemy &&
        character.ActionTime >= 60.0f

    let autoTeching character =
        match character.AutoBattleOpt_ with
        | Some autoBattle -> Option.isSome autoBattle.AutoTechOpt && not autoBattle.ChargeTech
        | None -> false

    let shouldCounter (character : Character) =
        let counterRoll = character.ArchetypeType = Fighter && Gen.random1 10 = 0
        counterRoll &&
        character.Healthy &&
        not (Map.containsKey StatusType.Sleep character.Statuses)

    let evalAttack effectType (source : Character) (target : Character) =
        let power = source.Power
        let shield = target.Shield effectType
        let defendingScalar = if target.Defending then Constants.Battle.DefendingScalar else 1.0f
        let damage0 = single (power - shield) * defendingScalar |> int |> max 1
        let damage1 =
            match target.Vulnerabilities.TryGetValue VulnerabilityType.Physical with
            | (true, rank) ->
                match rank with
                | Vulnerable -> damage0 + damage0 / 2
                | Resistant -> damage0 / 2
                | Invulnerable -> 0
            | (false, _) -> damage0
        let damage2 =
            if source.CharacterType = Enemy BlueGoblin // HACK: blue goblins can't attack for more than 2 damage.
            then min 2 damage1
            else damage1
        let damage = damage2 |> max 1
        damage

    let evalAimType aimType (target : Character) (characters : Map<CharacterIndex, Character>) =
        match aimType with
        | AnyAim healthy ->
            Map.filter (fun _ (c : Character) -> c.Healthy = healthy) characters
        | EnemyAim healthy | AllyAim healthy ->
            Map.filter (fun _ (c : Character) -> c.Healthy = healthy && friendly target c) characters
        | NoAim ->
            Map.empty

    let evalTargetType targetType (source : Character) (target : Character) characters =
        match targetType with
        | SingleTarget _ ->
            (Map.singleton target.CharacterIndex target, Map.empty)
        | AllTarget (aimType, splashDamage) ->
            let all =
                characters |>
                evalAimType aimType target
            if splashDamage
            then (Map.singleton target.CharacterIndex target, Map.remove target.CharacterIndex all)
            else (all, Map.empty)
        | ProximityTarget (radius, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Perimeter.Bottom - source.Perimeter.Bottom
                v.Magnitude <= radius) |>
            fun result -> (result, Map.empty)
        | RadialTarget (radius, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let v = character.Perimeter.Bottom - target.Perimeter.Bottom
                v.Magnitude <= radius) |>
            fun result -> (result, Map.empty)
        | LineTarget (offset, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Perimeter.Bottom - source.Perimeter.Bottom
                let b = target.Perimeter.Bottom - source.Perimeter.Bottom
                if a.Dot b > 0.0f then
                    let r = a - a.Dot b / b.Dot b * b // vector rejection
                    let d = r.Magnitude
                    d <= offset 
                else false) |>
            fun result -> (result, Map.empty)
        | SegmentTarget (offset, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let a = character.Perimeter.Bottom - source.Perimeter.Bottom
                let b = target.Perimeter.Bottom - source.Perimeter.Bottom
                if a.Magnitude <= b.Magnitude then
                    if a.Dot b > 0.0f then
                        let r = a - a.Dot b / b.Dot b * b // vector rejection
                        let d = r.Magnitude
                        d <= offset
                    else false
                else false) |>
            fun result -> (result, Map.empty)
        | VerticalTarget (width, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let x = target.Perimeter.Bottom.X
                character.Perimeter.Bottom.X >= x - width &&
                character.Perimeter.Bottom.X <= x + width) |>
            fun result -> (result, Map.empty)
        | HorizontalTarget (width, aimType) ->
            characters |>
            evalAimType aimType target |>
            Map.filter (fun _ character ->
                let y = target.Perimeter.Bottom.Y
                character.Perimeter.Bottom.Y >= y - width &&
                character.Perimeter.Bottom.Y <= y + width) |>
            fun result -> (result, Map.empty)

    let evalTechUnary splash (targetCount : int) techData source (target : Character) =
        let efficacy =
            match techData.EffectType with
            | Physical -> source.CharacterState_.Power
            | Magical -> source.CharacterState_.Magic (techData.AffinityOpt = Some Wind || techData.AffinityOpt = Some Shadow)
        let affinityScalar =
            match (techData.AffinityOpt, target.AffinityOpt) with
            | (Some affinitySource, Some affinityTarget) -> AffinityType.getScalar affinitySource affinityTarget
            | (_, _) -> 1.0f
        let techScalar =
            // NOTE: certain techs can't be used effectively by enemies, so they are given a special scalar.
            // TODO: pull this from TechData.EnemyScalarOpt.
            if source.Enemy then
                match techData.TechType with
                | Slash -> 1.333f
                | TechType.Flame -> 1.45f
                | Snowball -> 1.15f
                | Cure -> 1.5f
                | Aura -> 1f
                | Empower -> 0.75f
                | Enlighten -> 0.75f
                | Protect -> 1.0f
                | _ -> techData.Scalar
            else techData.Scalar
        let splitScalar =
            if source.Ally then
                if techData.Split && targetCount > 1
                then 1.75f / min 3.0f (single targetCount)
                else 1.0f
            else
                if techData.Split
                then Constants.Battle.EnemySplitScalar
                else 1.0f
        let splashScalar =
            if splash
            then 0.5f
            else 1.0f
        let specialAddend =
            // HACK: special case for Critical tech to get desired behavior of 1 more damage than normal attack in the
            // beginning of the game.
            match techData.TechType with
            | Critical -> 1.0f
            | _ -> 0.0f
        if techData.Curative then
            let affectsWounded = techData.TechType = Vita // TODO: pull from tech data.
            let healing0 = single efficacy * techScalar * splitScalar * splashScalar |> int |> max 1
            let healing1 = if target.Statuses.ContainsKey StatusType.Curse then 0 else healing0
            (target.CharacterIndex, false, affectsWounded, healing1, techData.StatusesAdded, techData.StatusesRemoved)
        else
            let cancelled = techData.Cancels && autoTeching target
            let shield = target.Shield techData.EffectType
            let defendingScalar = if target.Defending then Constants.Battle.DefendingScalar else 1.0f
            let damage0 = (single efficacy * affinityScalar * techScalar * splitScalar * splashScalar + specialAddend - single shield) * defendingScalar |> int |> max 1
            let damage1 =
                match techData.EffectType with
                | Physical | Magical when techData.AffinityOpt = Some Wind || techData.AffinityOpt = Some Shadow ->
                    match target.Vulnerabilities.TryGetValue VulnerabilityType.Physical with
                    | (true, rank) ->
                        match rank with
                        | Vulnerable -> damage0 + damage0 / 2
                        | Resistant -> damage0 / 2
                        | Invulnerable -> 0
                    | (false, _) -> damage0
                | _ ->
                    match target.Vulnerabilities.TryGetValue VulnerabilityType.Magical with
                    | (true, rank) ->
                        match rank with
                        | Vulnerable -> damage0 + damage0 / 2
                        | Resistant -> damage0 / 2
                        | Invulnerable -> 0
                    | (false, _) -> damage0
            let damage2 =
                match techData.AffinityOpt with
                | Some affinity ->
                    match target.Vulnerabilities.TryGetValue (Affinity affinity) with
                    | (true, rank) ->
                        match rank with
                        | Vulnerable -> damage1 + damage1 / 2
                        | Resistant -> damage1 / 2
                        | Invulnerable -> 0
                    | (false, _) -> damage1
                | None -> damage1
            let damage = damage2 |> max 1
            (target.CharacterIndex, cancelled, false, -damage, Set.difference techData.StatusesAdded target.Immunities, techData.StatusesRemoved)

    let evalTech techData source target characters =
        let (direct, splashing) = evalTargetType techData.TargetType source target characters
        let targetsCount = Map.count direct + Map.count splashing
        let directResults =
            Map.fold (fun results _ target ->
                let (index, cancelled, affectsWounded, delta, added, removed) = evalTechUnary false targetsCount techData source target
                Map.add index (cancelled, affectsWounded, delta, added, removed) results)
                Map.empty
                direct
        let splashResults =
            Map.fold (fun results _ target ->
                let (index, cancelled, affectsWounded, delta, added, removed) = evalTechUnary true targetsCount techData source target
                Map.add index (cancelled, affectsWounded, delta, added, removed) results)
                Map.empty
                splashing
        let results = directResults @@ splashResults
        (techData.SpawnOpt, results)

    let getPoiseType character =
        CharacterState.getPoiseType character.CharacterState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.CharacterAnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.CharacterAnimationState_

    let getAnimationInset time (character : Character) =
        CharacterAnimationState.inset time character.CelSize_ character.CharacterAnimationState_

    let getAnimationColor time (character : Character) =
        let color =
            if character.CharacterAnimationType = WoundAnimation && character.Enemy then
                match getAnimationProgressOpt time character with
                | Some progress -> Color (byte 255, byte 128, byte 255, byte 255 - (byte (progress * 255.0f))) // purple
                | None -> failwithumf ()
            else Color.One
        let color =
            match character.MaterializationOpt with
            | Some materialization ->
                match materialization with
                | Materializing ->
                    let localTime = time - character.AnimationStartTime
                    let progress = single localTime / single Constants.Battle.CharacterMaterializeDuration
                    color.ScaleA progress
                | Dematerializing ->
                    let localTime = time - character.AnimationStartTime
                    let progress = 1.0f - (single localTime / single Constants.Battle.CharacterDematerializeDuration)
                    color.ScaleA progress
            | None -> color
        color

    let getAnimationEmission time (character : Character) =
        if character.MaterializationOpt.IsNone then
            let localTime = time - character.AnimationStartTime
            match character.CharacterAnimationType with
            | AttackAnimation when localTime >= 5L && localTime < 10L ->
                Color.White
            | DamageAnimation when localTime < 10L ->
                Color.White
            | _ ->
                let pulseTime = time % Constants.Battle.CharacterSpritePulseDuration
                let pulseProgress = single pulseTime / single Constants.Battle.CharacterSpritePulseDuration
                let pulseIntensity = byte (sin (pulseProgress * MathF.PI) * 255.0f)
                let statuses = character.Statuses
                if character.Wounded then Color.Zero
                elif autoTeching character then Color (byte 255, byte 64, byte 64, pulseIntensity) // bright red
                elif Map.exists (fun key _ -> match key with Time true -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 255, pulseIntensity) // bright white
                elif Map.exists (fun key _ -> match key with Time false -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 127, pulseIntensity) // dark white
                elif Map.exists (fun key _ -> match key with Power (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 127, pulseIntensity) // bright orange
                elif Map.exists (fun key _ -> match key with Power (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 0, pulseIntensity) // dark orange
                elif Map.exists (fun key _ -> match key with Magic (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 127, byte 255, pulseIntensity) // bright purple
                elif Map.exists (fun key _ -> match key with Magic (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 0, byte 127, pulseIntensity) // dark purple
                elif Map.exists (fun key _ -> match key with Shield (true, _) -> true | _ -> false) statuses then Color (byte 127, byte 255, byte 127, pulseIntensity) // bright yellow
                elif Map.exists (fun key _ -> match key with Shield (false, _) -> true | _ -> false) statuses then Color (byte 0, byte 127, byte 0, pulseIntensity) // dark yellow
                elif Map.containsKey Confuse statuses then Color (byte 191, byte 191, byte 255, pulseIntensity) // blue-green
                elif Map.containsKey StatusType.Sleep statuses then Color (byte 0, byte 0, byte 255, pulseIntensity) // blue
                elif Map.containsKey Silence statuses then Color (byte 255,byte 255, byte 0, pulseIntensity) // orange
                elif Map.containsKey Poison statuses then Color (byte 0, byte 191, byte 0, pulseIntensity) // green
                else Color.Zero
        else Color.Zero

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.CharacterAnimationState_

    let getCharacterInputState character =
        character.CharacterInputState_

    let getActionTypeOpt character =
        match character.CharacterInputState_ with
        | AimReticles (actionStr, _) ->
            let actionType =
                if typeof<ConsumableType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = actionStr) then Consume (scvalue actionStr)
                elif typeof<TechType> |> FSharpType.GetUnionCases |> Array.exists (fun case -> case.Name = actionStr) then Tech (scvalue actionStr)
                else Attack
            Some actionType
        | _ -> None

    let getConjureTechs (character : Character) =
        if character.Ally
        then character.Techs |> Set.filter (fun techType -> techType.ConjureTech)
        else Set.empty

    let hasConjureTechs character =
        getConjureTechs character |> Set.notEmpty

    let burndownStatuses burndownTime character =
        { character with CharacterState_ = CharacterState.burndownStatuses burndownTime character.CharacterState_ }

    let setCharacterInputState inputState character =
        { character with CharacterInputState_ = inputState }

    let setStatuses statuses character =
        let characterState = { character.CharacterState_ with Statuses = statuses }
        { character with CharacterState_ = characterState }

    let setVulnerabilities vulnerabilities character =
        let characterState = { character.CharacterState_ with Vulnerabilities = vulnerabilities }
        { character with CharacterState_ = characterState }

    let setTechPoints techPoints character =
        { character with CharacterState_ = CharacterState.setTechPoints techPoints character.CharacterState_ }

    let setExpPoints expPoints character =
        { character with CharacterState_ = CharacterState.setExpPoints expPoints character.CharacterState_ }

    let setConjureChargeOpt conjureChargeOpt character =
        { character with ConjureChargeOpt_ = conjureChargeOpt }

    let setTechChargeOpt techChargeOpt character =
        { character with TechChargeOpt_ = techChargeOpt }

    let setAutoBattleOpt autoBattleOpt character =
        { character with AutoBattleOpt_ = autoBattleOpt }

    let setActionTime actionTime character =
        { character with ActionTime_ = actionTime }

    let setBottom bottom (character : Character) =
        { character with Perimeter_ = character.Perimeter.WithBottom bottom }

    let restore (character : Character) =
        { character with CharacterState_ = CharacterState.restore character.CharacterState_ }

    let applyStatusChanges statusesAdded statusesRemoved (character : Character) =
        if character.Healthy then
            let statuses = character.Statuses
            let statuses = Set.fold (fun statuses status -> Map.add status Constants.Battle.StatusBurndownTime statuses) statuses statusesAdded
            let statuses =
                Set.fold (fun statuses status ->
                    match Map.tryFindKey (StatusType.same status >> constant) statuses with // NOTE: being extra specific here since StatusType equality ignore fields.
                    | Some _ -> Map.remove status statuses
                    | None -> statuses)
                    statuses
                    statusesRemoved
            let character = setStatuses statuses character
            let actionTime =
                if  Set.exists (function Time false -> true | _ -> false) statusesAdded &&
                    character.ActionTime_ < Constants.Battle.ActionTime then
                    let slowScalar =
                        if character.Ally then Constants.Battle.ActionTimeSlowScalar
                        elif character.Boss then Constants.Battle.ActionTimeSlowerScalar
                        else Constants.Battle.ActionTimeSlowestScalar
                    character.ActionTime_ * slowScalar
                else character.ActionTime_
            setActionTime actionTime character
        else character

    let applyVulnerabilityChanges vulnerabilitiesAdded vulnerabilitiesRemoved (character : Character) =
        let vulnerabilities = character.Vulnerabilities
        let vulnerabilities = Map.fold (fun vulnerabilities vulnerabilityType vulnerabilityRank -> Map.add vulnerabilityType vulnerabilityRank vulnerabilities) vulnerabilities vulnerabilitiesAdded
        let vulnerabilities = Set.fold (fun vulnerabilities vulnerabilityType -> Map.remove vulnerabilityType vulnerabilities) vulnerabilities vulnerabilitiesRemoved
        setVulnerabilities vulnerabilities character

    let resetConjureCharge character =
        let conjureChargeOpt = (Option.map (constant -Constants.Battle.ConjureChargeRate)) character.ConjureChargeOpt_
        setConjureChargeOpt conjureChargeOpt character

    let updateConjureCharge character =
        if hasConjureTechs character then
            match character.ConjureChargeOpt_ with
            | Some conjureCharge ->
                { character with ConjureChargeOpt_ = Some (conjureCharge + Constants.Battle.ConjureChargeRate) }
            | None ->
                { character with ConjureChargeOpt_ = Some 0 }
        else character

    let resetTechCharge character =
        let techChargeOpt =
            match character.TechChargeOpt_ with
            | Some (_, chargeAmount, _) as chargeTechOpt ->
                if chargeAmount >= Constants.Battle.ChargeMax then
                    let chargeTechs = Algorithms.chargeTechs character.ArchetypeType character.Level
                    chargeTechs |> Gen.randomChoiceOpt |> Option.map (fun (chargeRate, chargeTech) -> (chargeRate, -chargeRate, chargeTech))
                else chargeTechOpt
            | None -> None
        setTechChargeOpt techChargeOpt character

    let updateTechCharge character =
        let techChargeOpt =
            match character.TechChargeOpt_ with
            | Some (chargeRate, chargeAmount, techType) -> Some (chargeRate, chargeRate + chargeAmount, techType)
            | None -> None
        setTechChargeOpt techChargeOpt character

    let autoBattle jinnInParty (alliesHealthy : Map<_, _>) alliesWounded enemiesStanding enemiesSwooning (source : Character) =

        // TODO: once techs have the ability to revive, check for that in the curative case.
        ignore (alliesWounded, enemiesSwooning)

        // update tech charge
        let source = updateTechCharge source

        // choose a tech
        let (techOpt, isChargeTech) =

            // see if we're charged. NOTE: silence only blocks non-enemy, non-charge techs.
            match source.TechChargeOpt with
            | Some (_, chargeAmount, chargeTech) when chargeAmount >= Constants.Battle.ChargeMax -> (Some chargeTech, true)
            | Some _ | None ->
                let techProbability =
                    source.CharacterState_.TechProbabilityOpt |>
                    Option.map (fun techProbability -> techProbability * if jinnInParty then 1.0f else Constants.Battle.TechProbabilityReductionScalar) |>
                    Option.defaultValue 0.0f 
                if  Gen.randomf < techProbability &&
                    not (Map.containsKey Silence source.Statuses) then
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
                    else Gen.randomValueOpt enemiesStanding
                | (false, _) -> None
            | None -> Gen.randomValueOpt alliesHealthy

        // attempt to update character with auto-battle and appropriate facing direction
        match targetOpt with
        | Some (target : Character) ->
            let sourceToTarget = target.Perimeter.Bottom - source.Perimeter.Bottom
            let direction = if sourceToTarget.X >= 0.0f then Rightward else Leftward // only two directions in this game
            let animationState = { source.CharacterAnimationState_ with Direction = direction }
            let autoBattle = { AutoTarget = target.CharacterIndex; AutoTechOpt = techOpt; ChargeTech = isChargeTech }
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

    let materialize time character =
        { character with CharacterAnimationState_ = CharacterAnimationState.materialize time character.CharacterAnimationState_ }

    let dematerialize time character =
        { character with CharacterAnimationState_ = CharacterAnimationState.dematerialize time character.CharacterAnimationState_ }

    let materialized time character =
        { character with CharacterAnimationState_ = CharacterAnimationState.materialized time character.CharacterAnimationState_ }

    let face direction character =
        { character with CharacterAnimationState_ = CharacterAnimationState.face direction character.CharacterAnimationState_ }

    let animate time characterAnimationType character =
        { character with CharacterAnimationState_ = CharacterAnimationState.setCharacterAnimationType time characterAnimationType character.CharacterAnimationState_ }

    let addInteraction interaction character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Interactions = characterState.Interactions @ [interaction] }
        { character with CharacterState_ = characterState }

    let clearInteractions character =
        let characterState = character.CharacterState_
        let characterState = { characterState with Interactions = [] }
        { character with CharacterState_ = characterState }

    let modifyHitPoints affectWounded cancelOpt hitPoints character =
        let (cancelOpt, character) =
            if character.CharacterState_.Healthy || affectWounded then
                let characterState = CharacterState.transformHitPoints (constant hitPoints) character.CharacterState_
                let character = { character with CharacterState_ = characterState }
                (cancelOpt, character)
            else (None, character)
        let (cancelled, character) =
            match (cancelOpt, character.AutoBattleOpt_) with
            | (Some (sourceBottom : Vector3, sourceIndex), Some autoBattle) when not autoBattle.ChargeTech -> // cannot cancel charge tech
                match autoBattle.AutoTarget with
                | AllyIndex _ as ally ->
                    let character = { character with AutoBattleOpt_ = Some { AutoTarget = ally; AutoTechOpt = None; ChargeTech = false }}
                    (true, character)
                | EnemyIndex _ ->
                    let character =
                        if autoBattle.AutoTarget.Enemy then
                            let character = { character with AutoBattleOpt_ = Some { AutoTarget = sourceIndex; AutoTechOpt = None; ChargeTech = false }}
                            if character.PerimeterOriginal.Bottom.X < sourceBottom.X then face Rightward character
                            elif character.PerimeterOriginal.Bottom.X > sourceBottom.X then face Leftward character
                            else character
                        else character
                    (true, character)
            | (_, _) -> (false, character)
        let character =
            let actionTime =
                if cancelled
                then max Constants.Battle.ActionTimeCancelMinimum (character.ActionTime_ - Constants.Battle.ActionTimeCancelReduction)
                else character.ActionTime_
            { character with ActionTime_ = actionTime }
        character

    let make bounds characterIndex characterOrderRev characterType boss animationSheet celSize direction (characterState : CharacterState) chargeTechOpt actionTime =
        let animationType = if characterState.Healthy then IdleAnimation else WoundAnimation
        let animationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = animationType; MaterializationOpt = None; Direction = direction }
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterIndex_ = characterIndex
          CharacterOrderRev_ = characterOrderRev
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

    let tryMakeEnemy allyCount subindex waitSpeed actionTimeAdvanced position enemyOrderRev enemyType =
        match Map.tryFind (Enemy enemyType) Data.Value.Characters with
        | Some characterData ->
            let archetypeType = characterData.ArchetypeType
            match Data.Value.Archetypes.TryFind characterData.ArchetypeType with
            | Some archetypeData ->
                let (size, celSize) =
                    match archetypeData.Stature with
                    | SmallStature | NormalStature | LargeStature -> (Constants.Gameplay.CharacterSize, Constants.Gameplay.CharacterCelSize)
                    | BossStature -> (Constants.Gameplay.BossSize, Constants.Gameplay.BossCelSize)
                let position = if allyCount = 1 then position + Constants.Battle.CharacterOffset else position
                let bounds = box3 position size
                let hitPoints = Algorithms.hitPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
                let techPoints = Algorithms.techPointsMax characterData.ArmorOpt archetypeType characterData.LevelBase
                let expPoints = Algorithms.levelToExpPoints characterData.LevelBase
                let chargeTechs = Algorithms.chargeTechs archetypeType characterData.LevelBase
                let chargeTechOpt = chargeTechs |> Gen.randomChoiceOpt |> Option.map (fun (chargeRate, chargeTech) -> (chargeRate, -chargeRate, chargeTech))
                let characterType = characterData.CharacterType
                let characterState = CharacterState.make characterData hitPoints techPoints expPoints characterData.WeaponOpt characterData.ArmorOpt characterData.Accessories
                let actionTime =
                    if actionTimeAdvanced then
                        if waitSpeed then       1000.0f - 125.0f - single enemyOrderRev * 75.0f
                        elif allyCount = 1 then 1000.0f - 400.0f - single enemyOrderRev * 75.0f
                        else                    1000.0f - 375.0f - single enemyOrderRev * 75.0f
                    else
                        if waitSpeed
                        then -25.0f
                        else -275.0f
                let enemy = make bounds (EnemyIndex subindex) enemyOrderRev characterType characterData.Boss characterData.AnimationSheet celSize Rightward characterState chargeTechOpt actionTime
                Some enemy
            | None -> None
        | None -> None

    let empty =
        let bounds = box3 v3Zero Constants.Gameplay.CharacterSize
        let characterAnimationState = { StartTime = 0L; AnimationSheet = Assets.Field.JinnAnimationSheet; CharacterAnimationType = IdleAnimation; MaterializationOpt = None; Direction = Downward }
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterIndex_ = AllyIndex 0
          CharacterOrderRev_ = 0
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