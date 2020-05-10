namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module CharacterModel =

    type [<ReferenceEquality; NoComparison>] CharacterModel =
        private
            { CharacterState : CharacterState
              AnimationState : CharacterAnimationState
              InputState_ : CharacterInputState
              BoundsOriginal_ : Vector4
              Bounds_ : Vector4 }

        (* CharacterState Properties *)
        member this.PartyIndex = this.CharacterState.PartyIndex
        member this.ExpPoints = this.CharacterState.ExpPoints
        member this.HitPoints = this.CharacterState.HitPoints
        member this.TechPoints = this.CharacterState.TechPoints
        member this.WeaponOpt = this.CharacterState.WeaponOpt
        member this.ArmorOpt = this.CharacterState.ArmorOpt
        member this.Accessories = this.CharacterState.Accessories
        member this.Techs = this.CharacterState.Techs
        member this.Statuses = this.CharacterState.Statuses
        member this.Defending = this.CharacterState.Defending
        member this.Charging = this.CharacterState.Charging
        member this.PowerBuff = this.CharacterState.PowerBuff
        member this.ShieldBuff = this.CharacterState.ShieldBuff
        member this.MagicBuff = this.CharacterState.MagicBuff
        member this.CounterBuff = this.CharacterState.CounterBuff
        member this.ActionTime = this.CharacterState.ActionTime
        member this.AutoBattleOpt = this.CharacterState.AutoBattleOpt
        member this.CharacterIndex = this.CharacterState.CharacterIndex
        member this.IsEnemy = this.CharacterState.IsEnemy
        member this.IsAlly = this.CharacterState.IsAlly
        member this.IsHealthy = this.CharacterState.IsHealthy
        member this.IsWounded = this.CharacterState.IsWounded
        member this.Level = this.CharacterState.Level
        member this.HitPointsMax = this.CharacterState.HitPointsMax
        member this.Power = this.CharacterState.Power
        member this.Magic = this.CharacterState.Magic
        member this.Shield = this.CharacterState.Shield

        (* AnimationState Properties *)
        member this.TimeStart = this.AnimationState.TimeStart
        member this.AnimationSheet = this.AnimationState.AnimationSheet
        member this.AnimationCycle = this.AnimationState.AnimationCycle
        member this.Direction = this.AnimationState.Direction

        (* InputState Properties *)
        member this.InputState = this.InputState_

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

        static member evaluateAutoBattle source (target : CharacterModel) =
            let techOpt =
                match Gen.random1 Constants.Battle.AutoBattleTechFrequency with
                | 0 -> CharacterState.tryGetTechRandom source.CharacterState
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

        static member evaluateTechPhysical techData source (target : CharacterModel) =
            let power = source.CharacterState.Power
            if techData.Curative then
                let healing = single power * techData.Scalar |> int |> max 1
                (false, healing, target.CharacterIndex)
            else
                let cancelled = techData.Cancels && CharacterState.runningTechAutoBattle target.CharacterState
                let shield = target.CharacterState.Shield
                let damageUnscaled = power - shield
                let damage = single damageUnscaled * techData.Scalar |> int |> max 1
                (cancelled, -damage, target.CharacterIndex)

        static member evaluateTechMagical techData source (target : CharacterModel) =
            let magic = source.CharacterState.Magic
            if techData.Curative then
                let healing = single magic * techData.Scalar |> int |> max 1
                (false, healing, target.CharacterIndex)
            else
                let cancelled = techData.Cancels && CharacterState.runningTechAutoBattle target.CharacterState
                let shield = target.CharacterState.Shield
                let damageUnscaled = magic - shield
                let damage = single damageUnscaled * techData.Scalar |> int |> max 1
                (cancelled, -damage, target.CharacterIndex)

        static member evaluateTech techData source target =
            match techData.EffectType with
            | Physical -> CharacterModel.evaluateTechPhysical techData source target
            | Magical -> CharacterModel.evaluateTechMagical techData source target

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
            CharacterState.getPoiseType character.CharacterState

        static member getAttackResult source target =
            CharacterState.getAttackResult source.CharacterState target.CharacterState

        static member getAnimationIndex time character =
            CharacterAnimationState.index time character.AnimationState

        static member getAnimationProgressOpt time character =
            CharacterAnimationState.progressOpt time character.AnimationState

        static member getAnimationFinished time character =
            CharacterAnimationState.getFinished time character.AnimationState

        static member runningTechAutoBattle character =
            CharacterState.runningTechAutoBattle character.CharacterState

        static member isTeammate character character2 =
            CharacterState.isTeammate character.CharacterState character2.CharacterState

        static member isReadyForAutoBattle character =
            Option.isNone character.CharacterState.AutoBattleOpt &&
            character.CharacterState.ActionTime > Constants.Battle.AutoBattleReadyTime &&
            character.CharacterState.IsEnemy

        static member updateHitPoints updater character =
            { character with CharacterState = CharacterState.updateHitPoints updater character.CharacterState }

        static member updateTechPoints updater character =
            { character with CharacterState = CharacterState.updateTechPoints updater character.CharacterState }

        static member updateInputState updater character =
            { character with InputState_ = updater character.InputState_ }
    
        static member updateActionTime updater character =
            { character with CharacterState = CharacterState.updateActionTime updater character.CharacterState }

        static member updateAutoBattleOpt updater character =
            { character with CharacterState = CharacterState.updateAutoBattleOpt updater character.CharacterState }

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
            let autoBattle = CharacterModel.evaluateAutoBattle source target
            let characterState = { source.CharacterState with AutoBattleOpt = Some autoBattle }
            let direction = Direction.fromVector2 sourceToTarget
            let animationState = { source.AnimationState with Direction = direction }
            { source with CharacterState = characterState; AnimationState = animationState }

        static member defend character =
            let characterState = character.CharacterState
            let characterState =
                // TODO: shield buff
                if not characterState.Defending
                then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff + Constants.Battle.DefendingCounterBuff) }
                else characterState
            let characterState = { characterState with Defending = true }
            { character with CharacterState = characterState }

        static member undefend character =
            let characterState = character.CharacterState
            let characterState =
                // TODO: shield buff
                if characterState.Defending
                then { characterState with CounterBuff = max 0.0f (characterState.CounterBuff - Constants.Battle.DefendingCounterBuff) }
                else characterState
            let characterState = { characterState with Defending = false }
            { character with CharacterState = characterState }

        static member animate time cycle character =
            { character with AnimationState = CharacterAnimationState.setCycle (Some time) cycle character.AnimationState }

        static member make characterIndex characterType expPoints weaponOpt armorOpt accessories animationSheet direction bounds =
            let characterState = CharacterState.make characterIndex characterType expPoints weaponOpt armorOpt accessories
            let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = ReadyCycle; Direction = direction }
            { CharacterState = characterState
              AnimationState = animationState
              InputState_ = NoInput
              BoundsOriginal_ = bounds
              Bounds_ = bounds }

type CharacterModel = CharacterModel.CharacterModel