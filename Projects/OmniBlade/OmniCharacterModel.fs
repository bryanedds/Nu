namespace OmniBlade
open Prime
open Nu

type AutoBattle =
    { AutoTarget : CharacterIndex
      AutoTechOpt : TechType option }

[<RequireQualifiedAccess>]
module CharacterModel =

    type [<ReferenceEquality; NoComparison>] CharacterModel =
        private
            { BoundsOriginal_ : Vector4
              Bounds_ : Vector4
              CharacterState_ : CharacterState
              AnimationState_ : CharacterAnimationState
              AutoBattleOpt_ : AutoBattle option
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

        (* CharacterState Properties *)
        member this.PartyIndex = this.CharacterState_.PartyIndex
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
        member this.PowerBuff = this.CharacterState_.PowerBuff
        member this.ShieldBuff = this.CharacterState_.ShieldBuff
        member this.MagicBuff = this.CharacterState_.MagicBuff
        member this.CounterBuff = this.CharacterState_.CounterBuff
        member this.ActionTime = this.CharacterState_.ActionTime
        member this.CharacterIndex = this.CharacterState_.CharacterIndex
        member this.IsEnemy = this.CharacterState_.IsEnemy
        member this.IsAlly = this.CharacterState_.IsAlly
        member this.IsHealthy = this.CharacterState_.IsHealthy
        member this.IsWounded = this.CharacterState_.IsWounded
        member this.Level = this.CharacterState_.Level
        member this.HitPointsMax = this.CharacterState_.HitPointsMax
        member this.Power = this.CharacterState_.Power
        member this.Magic = this.CharacterState_.Magic
        member this.Shield = this.CharacterState_.Shield

        (* AnimationState Properties *)
        member this.TimeStart = this.AnimationState_.TimeStart
        member this.AnimationSheet = this.AnimationState_.AnimationSheet
        member this.AnimationCycle = this.AnimationState_.AnimationCycle
        member this.Direction = this.AnimationState_.Direction

        (* Local Properties *)
        member this.AutoBattleOpt = this.AutoBattleOpt_
        member this.InputState = this.InputState_

        static member evaluateAutoBattle source (target : CharacterModel) =
            let techOpt =
                match Gen.random1 Constants.Battle.AutoBattleTechFrequency with
                | 0 -> CharacterState.tryGetTechRandom source.CharacterState_
                | _ -> None
            { AutoTarget = target.CharacterIndex; AutoTechOpt = techOpt }

        static member evaluateAimType aimType (target : CharacterModel) (characters : CharacterModel list) =
            match aimType with
            | AnyAim healthy ->
                characters |>
                List.filter (fun target -> target.IsHealthy = healthy)
            | EnemyAim healthy | AllyAim healthy ->
                characters |>
                List.filter (CharacterModel.isTeammate target) |>
                List.filter (fun target -> target.IsHealthy = healthy)
            | NoAim ->
                []

        static member evaluateTargetType targetType (source : CharacterModel) target characters =
            match targetType with
            | SingleTarget _ ->
                [target]
            | ProximityTarget (aimType, radius) ->
                characters |>
                CharacterModel.evaluateAimType aimType target |>
                List.filter (fun character ->
                    let v = character.Bottom - source.Bottom
                    v.Length <= radius)
            | RadialTarget (aimType, radius) ->
                characters |>
                CharacterModel.evaluateAimType aimType target |>
                List.filter (fun character ->
                    let v = character.Bottom - target.Bottom
                    v.Length <= radius)
            | LineTarget (aimType, width) ->
                characters |>
                CharacterModel.evaluateAimType aimType target |>
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
                CharacterModel.evaluateAimType aimType target

        static member evaluateTech techData source (target : CharacterModel) =
            let power = source.CharacterState_.Power
            if techData.Curative then
                let healing = single power * techData.Scalar |> int |> max 1
                (false, healing, target.CharacterIndex)
            else
                let cancelled = techData.Cancels && CharacterModel.runningTechAutoBattle target
                let shield = target.CharacterState_.Shield techData.EffectType
                let damageUnscaled = power - shield
                let damage = single damageUnscaled * techData.Scalar |> int |> max 1
                (cancelled, -damage, target.CharacterIndex)

        static member evaluateTechMove techData source target characters =
            let targets =
                CharacterModel.evaluateTargetType techData.TargetType source target characters
            let resultsRev =
                List.fold (fun results target ->
                    let result = CharacterModel.evaluateTech techData source target
                    result :: results)
                    [] targets
            List.rev resultsRev

        static member getPoiseType character =
            CharacterState.getPoiseType character.CharacterState_

        static member getAttackResult effectType source target =
            CharacterState.getAttackResult effectType source.CharacterState_ target.CharacterState_

        static member getAnimationIndex time character =
            CharacterAnimationState.index time character.AnimationState_

        static member getAnimationProgressOpt time character =
            CharacterAnimationState.progressOpt time character.AnimationState_

        static member getAnimationFinished time character =
            CharacterAnimationState.getFinished time character.AnimationState_
        
        static member runningTechAutoBattle character =
            match character.AutoBattleOpt_ with
            | Some autoBattle -> Option.isSome autoBattle.AutoTechOpt
            | None -> false

        static member isTeammate character character2 =
            CharacterState.isTeammate character.CharacterState_ character2.CharacterState_

        static member isReadyForAutoBattle character =
            Option.isNone character.AutoBattleOpt_ &&
            character.CharacterState_.ActionTime > Constants.Battle.AutoBattleReadyTime &&
            character.CharacterState_.IsEnemy

        static member updateHitPoints updater character =
            let (hitPoints, cancel) = updater character.CharacterState_.HitPoints
            let characterState = CharacterState.updateHitPoints (constant hitPoints) character.CharacterState_
            let autoBattleOpt = 
                match character.AutoBattleOpt_ with
                | Some autoBattle when cancel -> Some { autoBattle with AutoTechOpt = None }
                | _ -> None
            { character with CharacterState_ = characterState; AutoBattleOpt_ = autoBattleOpt }

        static member updateTechPoints updater character =
            { character with CharacterState_ = CharacterState.updateTechPoints updater character.CharacterState_ }

        static member updateInputState updater character =
            { character with InputState_ = updater character.InputState_ }
    
        static member updateActionTime updater character =
            { character with CharacterState_ = CharacterState.updateActionTime updater character.CharacterState_ }

        static member updateAutoBattleOpt updater character =
            { character with AutoBattleOpt_ = updater character.AutoBattleOpt_ }

        static member updateBounds updater (character : CharacterModel) =
            { character with Bounds_ = updater character.Bounds_ }

        static member updatePosition updater (character : CharacterModel) =
            { character with Bounds_ = character.Position |> updater |> character.Bounds.WithPosition }

        static member updateCenter updater (character : CharacterModel) =
            { character with Bounds_ = character.Center |> updater |> character.Bounds.WithCenter }

        static member updateBottom updater (character : CharacterModel) =
            { character with Bounds_ = character.Bottom |> updater |> character.Bounds.WithBottom }

        static member autoBattle (source : CharacterModel) (target : CharacterModel) =
            let sourceToTarget = target.Position - source.Position
            let direction = Direction.fromVector2 sourceToTarget
            let animationState = { source.AnimationState_ with Direction = direction }
            let autoBattle = CharacterModel.evaluateAutoBattle source target
            { source with AnimationState_ = animationState; AutoBattleOpt_ = Some autoBattle }

        static member defend character =
            let characterState = character.CharacterState_
            let characterState =
                // TODO: shield buff
                if not characterState.Defending
                then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff + Constants.Battle.DefendingCounterBuff) }
                else characterState
            let characterState = { characterState with Defending = true }
            { character with CharacterState_ = characterState }

        static member undefend character =
            let characterState = character.CharacterState_
            let characterState =
                // TODO: shield buff
                if characterState.Defending
                then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff - Constants.Battle.DefendingCounterBuff) }
                else characterState
            let characterState = { characterState with Defending = false }
            { character with CharacterState_ = characterState }

        static member animate time cycle character =
            { character with AnimationState_ = CharacterAnimationState.setCycle (Some time) cycle character.AnimationState_ }

        static member makeEnemy index enemyData =
            let animationSheet = 
                let characterType = Enemy enemyData.EnemyType
                match Map.tryFind characterType data.Value.Characters with
                | Some characterData -> characterData.AnimationSheet
                | None -> Assets.BlueGoblinAnimationSheet
            let enemy =
                CharacterModel.make
                    (v4Bounds enemyData.EnemyPosition Constants.Gameplay.CharacterSize)
                    (EnemyIndex index)
                    (Enemy enemyData.EnemyType)
                    0
                    None None [] // TODO: figure out if / how we should populate these 
                    animationSheet
                    Leftward
            enemy

        static member make bounds characterIndex characterType expPoints weaponOpt armorOpt accessories animationSheet direction =
            let characterState = CharacterState.make characterIndex characterType expPoints weaponOpt armorOpt accessories animationSheet
            let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = ReadyCycle; Direction = direction }
            { BoundsOriginal_ = bounds
              Bounds_ = bounds
              CharacterState_ = characterState
              AnimationState_ = animationState
              AutoBattleOpt_ = None
              InputState_ = NoInput }

type CharacterModel = CharacterModel.CharacterModel