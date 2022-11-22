// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module BattleDispatcher =

    type [<NoComparison>] Positioning =
        | Position of Vector3
        | Center of Vector3
        | Bottom of Vector3

    type Screen with
        member this.GetBattle world = this.GetModelGeneric<Battle> world
        member this.SetBattle value world = this.SetModelGeneric<Battle> value world
        member this.Battle = this.ModelGeneric<Battle> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<Battle, BattleMessage, BattleCommand> (Battle.empty)

        static let displayEffect delay size positioning effect screen world =
            World.schedule (fun world ->
                let (entity, world) = World.createEntity<EffectDispatcher2d> None DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetEffect effect world
                let world = entity.SetSize size world
                let world =
                    match positioning with
                    | Position position -> entity.SetPosition position world
                    | Center center -> entity.SetCenter center world
                    | Bottom bottom -> entity.SetBottom bottom world
                let world = entity.SetElevation Constants.Battle.EffectElevation world
                entity.SetSelfDestruct true world)
                delay screen world

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => msg Update
             Screen.PostUpdateEvent => cmd UpdateEye]

        override this.Message (battle, message, _, world) =

            match message with
            | Update ->

                // update
                let (signals, battle) = 
                    if World.getAdvancing world
                    then Battle.update (World.getUpdateTime world) battle
                    else just battle

                // update dialog
                let battle =
                    match battle.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.update id dialog world
                        Battle.updateDialogOpt (constant (Some dialog)) battle
                    | None -> battle

                // fin
                (signals, battle)

            | InteractDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    match Dialog.tryAdvance id dialog with
                    | (true, dialog) ->
                        let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                        just battle
                    | (false, _) ->
                        let battle = Battle.updateDialogOpt (constant None) battle
                        just battle
                | None -> just battle

            | RegularItemSelect (characterIndex, item) ->
                let battle =
                    match item with
                    | "Attack" ->
                        battle |>
                        Battle.updateCharacterInputState (constant (AimReticles (item, EnemyAim true))) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Defend" ->
                        let battle = Battle.updateCharacterInputState (constant NoInput) characterIndex battle
                        let command = ActionCommand.make Defend characterIndex None
                        let battle = Battle.appendActionCommand command battle
                        battle
                    | "Tech" ->
                        battle |>
                        Battle.updateCharacterInputState (constant TechMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Consumable" ->
                        battle |>
                        Battle.updateCharacterInputState (constant ItemMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | _ -> failwithumf ()
                just battle
            
            | RegularItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle
            
            | ConsumableItemSelect (characterIndex, item) ->
                let consumableType =
                    scvalue<ConsumableType> item
                let aimType =
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumableData) -> consumableData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle

            | ConsumableItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle
            
            | TechItemSelect (characterIndex, item) ->
                let techType =
                    scvalue<TechType> item
                let aimType =
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, techData) -> techData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle
            
            | TechItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle

            | ReticlesSelect (sourceIndex, targetIndex) ->
                match battle.BattleState with
                | BattleRunning ->
                    let battle = Battle.confirmCharacterInput sourceIndex targetIndex battle
                    withMsg (ResetCharacter sourceIndex) battle
                | _ -> just battle

            | ReticlesCancel characterIndex ->
                let battle = Battle.cancelCharacterInput characterIndex battle
                just battle

            | ReadyCharacters localTime ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharactersReady time battle
                if localTime = 30L
                then withCmd (BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UnsheatheSound)) battle
                else just battle

            | PoiseCharacters ->
                let time = World.getUpdateTime world
                let battle = Battle.animatedCharactersPoised time battle
                just battle

            | CelebrateCharacters outcome ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharactersCelebrate time outcome battle
                just battle

            | AttackCharacter1 sourceIndex ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                let playHit = BattleCommand.PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                withCmd playHit battle

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getUpdateTime world
                let damage = Battle.evalAttack Physical sourceIndex targetIndex battle
                let battle = Battle.updateCharacterHitPoints false false -damage targetIndex battle
                let battle = Battle.animateCharacter time DamageAnimation targetIndex battle
                let sigs = if Battle.isCharacterWounded targetIndex battle then [msg (ResetCharacter targetIndex)] else []
                withSigs (cmd (DisplayHitPointsChange (targetIndex, -damage)) :: sigs) battle

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacter time CastAnimation sourceIndex battle
                let battle = Battle.updateInventory (Inventory.tryRemoveItem (Consumable consumable) >> snd) battle
                just battle

            | ConsumeCharacter2 (consumableType, targetIndex) ->
                let time = World.getUpdateTime world
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumableData) ->
                    if consumableData.Curative then
                        let healing = int consumableData.Scalar
                        let battle =
                            if consumableData.Techative
                            then Battle.updateCharacterTechPoints healing targetIndex battle
                            else Battle.updateCharacterHitPoints false consumableData.Revive healing targetIndex battle
                        let battle = Battle.applyCharacterStatuses consumableData.StatusesAdded consumableData.StatusesRemoved targetIndex battle
                        let battle = Battle.animateCharacter time SpinAnimation targetIndex battle
                        let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                        let playHealSound = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)
                        withCmds [displayHitPointsChange; playHealSound] battle
                    else
                        // TODO: non-curative case
                        just battle
                | (false, _) -> just battle

            | TechCharacter1 (sourceIndex, targetIndex, techType) ->
                let sourcePerimeter = Battle.getCharacterPerimeter sourceIndex battle
                let targetPerimeter = Battle.getCharacterPerimeter targetIndex battle
                let effectOpt =
                    match techType with
                    | Critical | DarkCritical | PoisonCut | PowerCut | DispelCut | DoubleCut ->
                        let hopDirection = Direction.ofVector3 (v3 (targetPerimeter.Bottom.X - sourcePerimeter.Bottom.X) 0.0f 0.0f)
                        let hopStop = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                        Left (DisplayHop { HopStart = sourcePerimeter.Bottom; HopStop = hopStop })
                    | Cyclone ->
                        Left (DisplayHop { HopStart = sourcePerimeter.Bottom; HopStop = targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3 })
                    | _ ->
                        match Battle.getCharacterArchetypeType sourceIndex battle with
                        | Cleric ->
                            let playCharge = BattleCommand.PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeHolySound)
                            let displayCast = DisplayHolyCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                        | Wizard ->
                            let playCharge = BattleCommand.PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayArcaneCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                        | _ ->
                            let playCharge = BattleCommand.PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayDimensionalCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                match effectOpt with
                | Left hopEffect ->
                    withCmd hopEffect battle
                | Right chargeEffects ->
                    if Battle.isCharacterWounded targetIndex battle then
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    else withSigs (msg (ChargeCharacter sourceIndex) :: chargeEffects) battle

            | TechCharacter2 (sourceIndex, targetIndex, techType) ->
                match techType with
                | Critical ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; impactSplash] battle
                | Cyclone ->
                    let time = World.getUpdateTime world
                    let radius = 64.0f
                    let perimeter = Battle.getCharacterPerimeter sourceIndex battle
                    let position = perimeter.Bottom
                    let playHits =
                        [BattleCommand.PlaySound (20L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         BattleCommand.PlaySound (40L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         BattleCommand.PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         BattleCommand.PlaySound (80L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)]
                    let battle = Battle.animateCharacter time WhirlAnimation sourceIndex battle
                    withCmds (DisplayCircle (position, radius) :: DisplayCycloneBlur (0L, sourceIndex, radius) :: playHits) battle
                | DarkCritical ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex) // TODO: darker impact splash to represent element.
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; impactSplash] battle
                | Slash ->
                    let time = World.getUpdateTime world
                    let playSlash = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.SlashSound)
                    let playHit = BattleCommand.PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let perimeter = Battle.getCharacterPerimeter sourceIndex battle
                    let slashSpike = DisplaySlashSpike (10L, perimeter.Bottom, targetIndex)
                    let impactSplashes = Battle.evalTechMove sourceIndex targetIndex techType battle |> snd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayImpactSplash (70L, targetIndex))
                    let battle = Battle.animateCharacter time SlashAnimation sourceIndex battle
                    withCmds (playSlash :: playHit :: slashSpike :: impactSplashes) battle
                | PowerCut ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; cut] battle
                | PoisonCut ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; cut] battle
                | DoubleCut ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; cut] battle
                | DispelCut ->
                    let time = World.getUpdateTime world
                    let playHit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let displayCut = DisplayCut (30L, true, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withCmds [playHit; displayCut] battle
                | Fire ->
                    let time = World.getUpdateTime world
                    let playFire = BattleCommand.PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.FireSound)
                    let displayFire = DisplayFire (0L, sourceIndex, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmds [playFire; displayFire] battle
                | TechType.Flame ->
                    let time = World.getUpdateTime world
                    let playFlame = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.FlameSound)
                    let displayFlame = DisplayFlame (0L, sourceIndex, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmds [playFlame; displayFlame] battle
                | Ice ->
                    let time = World.getUpdateTime world
                    let playIce = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.IceSound)
                    let displayIce = DisplayIce (0L, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmds [playIce; displayIce] battle
                | Snowball ->
                    let time = World.getUpdateTime world
                    let playSnowball = BattleCommand.PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.SnowballSound)
                    let displaySnowball = DisplaySnowball (0L, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmds [playSnowball; displaySnowball] battle
                | Bolt ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use sound.
                | BoltBeam ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Stone ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmd (DisplayIce (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Quake ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Cure ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playCure = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                    let displayCures = Battle.evalTechMove sourceIndex targetIndex techType battle |> snd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex))
                    withCmds (playCure :: displayCures) battle
                | Empower ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Power (true, true), targetIndex)
                    withCmds [playBuff; displayBuff] battle
                | Aura ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playCure = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                    let displayCures = Battle.evalTechMove sourceIndex targetIndex techType battle |> snd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex))
                    withCmds (playCure :: displayCures) battle
                | Enlighten ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Magic (true, true), targetIndex)
                    withCmds [playBuff; displayBuff] battle
                | Protect ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Shield (true, true), targetIndex)
                    withCmds [playBuff; displayBuff] battle
                | Weaken ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Power (false, false), targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | Muddle ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Magic (false, false), targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | ConjureIfrit ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playIfrit = BattleCommand.PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.IfritSound)
                    let displayConjureIfrit = DisplayConjureIfrit 0L
                    withCmds [playIfrit; displayConjureIfrit] battle
                | Slow ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Time false, targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | Purify ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withCmd (DisplayPurify (0L, targetIndex)) battle // TODO: use new sound and effect.

            | TechCharacter3 (sourceIndex, targetIndex, techType) ->
                let time = World.getUpdateTime world
                let results = Battle.evalTechMove sourceIndex targetIndex techType battle |> snd
                let (battle, cmds) =
                    Map.fold (fun (battle, cmds) characterIndex (cancelled, _, hitPointsChange, _, _) ->
                        if hitPointsChange < 0 && Battle.isCharacterHealthy characterIndex battle then
                            let battle = Battle.animateCharacter time DamageAnimation characterIndex battle
                            let cmds = if cancelled then DisplayCancel characterIndex :: cmds else cmds
                            (battle, cmds)
                        else (battle, cmds))
                        (battle, [])
                        results
                withCmds cmds battle

            | TechCharacter4 (sourceIndex, targetIndex, techType) ->
                let results = Battle.evalTechMove sourceIndex targetIndex techType battle |> snd 
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) _ (_, _, _, _, _) ->
                        // TODO: glow effect
                        (battle, sigs))
                        (battle, [])
                        results
                withSigs sigs battle

            | TechCharacter5 (sourceIndex, targetIndex, techType) ->
                let sourcePerimeterOriginal = Battle.getCharacterPerimeterOriginal sourceIndex battle
                let targetPerimeter = Battle.getCharacterPerimeter targetIndex battle
                let hopOpt =
                    match techType with
                    | Critical | DarkCritical | PoisonCut | PowerCut | DispelCut | DoubleCut ->
                        let hopDirection = Direction.ofVector3 (targetPerimeter.Bottom - sourcePerimeterOriginal.Bottom)
                        let hopStart = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                        Some
                            { HopStart = hopStart
                              HopStop = sourcePerimeterOriginal.Bottom }
                    | Cyclone ->
                        Some
                            { HopStart = targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3
                              HopStop = sourcePerimeterOriginal.Bottom }
                    | _ -> None
                match hopOpt with
                | Some hop -> withCmd (DisplayHop hop) battle
                | None -> just battle

            | TechCharacter6 (sourceIndex, targetIndex, techType) ->
                let (techCost, results) = Battle.evalTechMove sourceIndex targetIndex techType battle
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) characterIndex (cancelled, affectsWounded, hitPointsChange, added, removed) ->
                        let battle = Battle.updateCharacterHitPoints cancelled affectsWounded hitPointsChange characterIndex battle
                        let randomizer = if sourceIndex.IsAlly then StatusType.randomizeStrong else StatusType.randomizeWeak
                        let added = added |> Set.toSeq |> Seq.filter randomizer |> Set.ofSeq
                        let battle = Battle.applyCharacterStatuses added removed characterIndex battle
                        let wounded = Battle.isCharacterWounded characterIndex battle
                        let sigs = if wounded then Message (ResetCharacter characterIndex) :: sigs else sigs
                        let sigs = if hitPointsChange <> 0 then Command (DisplayHitPointsChange (characterIndex, hitPointsChange)) :: sigs else sigs
                        let (battle, sigs) =
                            if wounded then
                                let woundCommand = ActionCommand.make Wound sourceIndex (Some characterIndex)
                                let battle = Battle.prependActionCommand woundCommand battle
                                (battle, sigs)
                            else
                                let sigs = Message (PoiseCharacter characterIndex) :: sigs
                                (battle, sigs)
                        (battle, sigs))
                        (battle, [])
                        results
                let battle = Battle.updateCharacterTechPoints -techCost sourceIndex battle
                let battle = Battle.advanceChargeTech sourceIndex battle
                let battle =
                    if Battle.shouldCounter sourceIndex targetIndex battle
                    then Battle.counterAttack sourceIndex targetIndex battle
                    else battle
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                let sigs = Message (PoiseCharacter sourceIndex) :: sigs
                withSigs sigs battle

            | TechCharacterAmbient (sourceIndex, _, _) ->
                if Simulants.Battle.Scene.Ride.Exists world then
                    let battle =
                        let tags = Simulants.Battle.Scene.Ride.GetEffectTags world
                        match Map.tryFind "Tag" tags with
                        | Some tag -> Battle.updateCharacterBottom (constant tag.Position) sourceIndex battle
                        | None -> battle
                    just battle
                else just battle

            | AutoBattleEnemies ->
                let battle = Battle.autoBattleEnemies battle
                just battle

            | ChargeCharacter sourceIndex ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacter time (PoiseAnimation Charging) sourceIndex battle
                just battle

            | PoiseCharacter characterIndex ->
                let time = World.getUpdateTime world
                let battle = Battle.animationCharacterPoise time characterIndex battle
                just battle

            | WoundCharacter characterIndex ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacterWound time characterIndex battle
                just battle

            | ResetCharacter characterIndex ->
                let battle = Battle.updateCharacterActionTime (constant 0.0f) characterIndex battle
                let battle =
                    if characterIndex.IsAlly
                    then Battle.updateCharacterInputState (constant NoInput) characterIndex battle
                    else Battle.updateCharacterAutoBattleOpt (constant None) characterIndex battle
                just battle

            | DestroyCharacter characterIndex ->
                let battle = if characterIndex.IsEnemy then Battle.removeCharacter characterIndex battle else battle
                just battle

            | Nop -> just battle

        override this.Command (battle, command, screen, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyePosition2d v2Zero world
                just world
            
            | DisplayHop hop ->
                let effect = Effects.makeHopEffect hop.HopStart hop.HopStop
                let (entity, world) = World.createEntity<EffectDispatcher2d> (Some Simulants.Battle.Scene.Ride.Surnames) DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v3Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCircle (position, radius) ->
                let effect = Effects.makeCircleEffect radius
                let (entity, world) = World.createEntity<EffectDispatcher2d> (Some Simulants.Battle.Scene.Ride.Surnames) DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetPosition position world
                let world = entity.SetEffect effect world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCancel targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeCancelEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher2d> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetPosition target.CenterOffset4 world
                    let world = entity.SetEffect effect world
                    let world = entity.SetElevation (Constants.Battle.GuiEffectElevation + 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher2d> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetPosition target.CenterOffset3 world
                    let world = entity.SetEffect effect world
                    let world = entity.SetElevation Constants.Battle.GuiEffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 758.0f 0.0f) (Bottom target.Bottom) (Effects.makeBoltEffect ()) screen world |> just
                | None -> just world

            | DisplayCycloneBlur (delay, targetIndex, radius) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 234.0f 234.0f 0.0f) (Center target.Center) (Effects.makeCycloneBlurEffect radius) screen world |> just
                | None -> just world

            | DisplayImpactSplash (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 96.0f 0.0f) (Bottom target.Bottom) (Effects.makeImpactSplashEffect ()) screen world |> just
                | None -> just world

            | DisplayCut (delay, light, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) (Effects.makeCutEffect light) screen world |> just
                | None -> just world
            
            | DisplaySlashSpike (delay, bottom, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let projection = Vector3.Normalize (target.Bottom - bottom) * single Constants.Render.VirtualResolutionX + target.Bottom
                    let effect = (Effects.makeSlashSpikeEffect bottom projection)
                    let world = displayEffect delay (v3 96.0f 96.0f 0.0f) (Bottom bottom) effect screen world
                    just world
                | None -> just world

            | DisplayArcaneCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 120.0f 0.0f)) (Effects.makeArcaneCastEffect ()) screen world |> just
                | None -> just world
            
            | DisplayFire (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFireEffect (source.Bottom + (v3 80.0f 80.0f 0.0f)) (target.Bottom + (v3 0.0f 20.0f 0.0f))
                        let world = displayEffect delay (v3 100.0f 100.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 50.0f 0.0f)) effect screen world
                        just world
                    | None -> just world
                | None -> just world

            | DisplayFlame (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFlameEffect source.CenterOffset target.CenterOffset
                        let world = displayEffect delay (v3 144.0f 144.0f 0.0f) (Bottom source.Bottom) effect screen world
                        just world
                    | None -> just world
                | None -> just world
            
            | DisplayIce (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeIceEffect ()) screen world |> just
                | None -> just world
            
            | DisplaySnowball (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 432.0f 432.0f 0.0f) (Bottom target.Bottom) (Effects.makeSnowballEffect ()) screen world |> just
                | None -> just world

            | DisplayHolyCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 100.0f 0.0f)) (Effects.makeHolyCastEffect ()) screen world |> just
                | None -> just world
            
            | DisplayPurify (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 192.0f 0.0f) (Bottom (target.Bottom - v3 0.0f 100.0f 0.0f)) (Effects.makePurifyEffect ()) screen world |> just
                | None -> just world

            | DisplayCure (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeCureEffect ()) screen world |> just
                | None -> just world
            
            | DisplayProtect (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeProtectEffect ()) screen world |> just
                | None -> just world

            | DisplayDimensionalCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom source.Bottom) (Effects.makeDimensionalCastEffect ()) screen world |> just
                | None -> just world

            | DisplayBuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeBuffEffect statusType) screen world |> just
                | None -> just world

            | DisplayDebuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeDebuffEffect statusType) screen world |> just
                | None -> just world

            | DisplayConjureIfrit delay ->
                displayEffect delay (v3 48.0f 48.0f 0.0f) (Position (v3 0.0f 0.0f 0.0f)) (Effects.makeConjureIfritEffect ()) screen world |> just

            | BattleCommand.PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) delay screen world
                just world

            | BattleCommand.PlaySong (fadeIn, fadeOut, volume, start, assetTag) ->
                let world = World.playSong fadeIn fadeOut volume start assetTag world
                just world

            | BattleCommand.FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

        override this.Content (battle, _) =

            [// scene group
             Content.group Simulants.Battle.Scene.Group.Name []

                [// tile map
                 Content.tileMap "TileMap"
                    [Entity.Position == v3 -480.0f -270.0f 0.0f
                     Entity.Elevation == Constants.Battle.BackgroundElevation
                     Entity.TileMap := battle.TileMap
                     Entity.TileIndexOffset := battle.TileIndexOffset
                     Entity.TileIndexOffsetRange := battle.TileIndexOffsetRange]

                 // dialog
                 yield! Content.dialog "Dialog"
                    (Constants.Battle.GuiElevation + 2.0f) Nop Nop id
                    (match battle.DialogOpt with Some dialog -> Some dialog | None -> None)

                 // dialog interact button
                 Content.button "DialogInteract"
                    [Entity.Position == v3 248.0f -240.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible := match battle.DialogOpt with Some dialog -> Dialog.canAdvance id dialog | None -> false
                     Entity.Text == "Next"
                     Entity.ClickEvent => msg InteractDialog]

                 // characters
                 for (index, character) in (Battle.getCharacters battle).Pairs do

                    // character
                    Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character := character]

                 // hud
                 for (index, ally) in (Battle.getAlliesHealthy battle).Pairs do

                    // bars
                    Content.composite (CharacterIndex.toEntityName index + "+Hud") []
                        
                        [// health bar
                         Content.fillBar "HealthBar" 
                            [Entity.MountOpt == None
                             Entity.Size == v3 48.0f 6.0f 0.0f
                             Entity.Center := ally.BottomOffset
                             Entity.Elevation == Constants.Battle.GuiElevation
                             Entity.Fill := single ally.HitPoints / single ally.HitPointsMax]
                            
                         // tech bar
                         Content.fillBar "TechBar" 
                            [Entity.MountOpt == None
                             Entity.Size == v3 48.0f 6.0f 0.0f
                             Entity.Center := ally.BottomOffset2
                             Entity.Elevation == Constants.Battle.GuiElevation
                             Entity.FillColor == Color (byte 74, byte 91, byte 169, byte 255)
                             Entity.Fill := single ally.TechPoints / single ally.TechPointsMax]]]

             // inputs condition
             if battle.Running then

                // inputs group
                Content.group Simulants.Battle.Inputs.Group.Name []

                    [// inputs
                     for (index, ally) in (Battle.getAlliesHealthy battle).Pairs do

                        // input
                        Content.composite (CharacterIndex.toEntityName index + "+Input") []
                            [match ally.InputState with
                             | RegularMenu ->
                                Content.entity<RingMenuDispatcher> "RegularMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.Enabled :=
                                        (let allies = battle |> Battle.getAllies |> Map.toValueList
                                         let alliesPastRegularMenu =
                                            Seq.notExists (fun (ally : Character) ->
                                                match ally.InputState with NoInput | RegularMenu -> false | _ -> true)
                                                allies
                                         alliesPastRegularMenu)
                                     Entity.RingMenu == { Items = Map.ofList [("Attack", (0, true)); ("Tech", (1, true)); ("Consumable", (2, true)); ("Defend", (3, true))]; ItemCancelOpt = None }
                                     Entity.ItemSelectEvent =|> fun evt -> msg (RegularItemSelect (index, evt.Data))
                                     Entity.CancelEvent => msg (RegularItemCancel index)]
                             | ItemMenu ->
                                Content.entity<RingMenuDispatcher> "ConsumableMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.RingMenu :=
                                        (let consumables =
                                            battle.Inventory |>
                                            Inventory.getConsumables |>
                                            Map.ofSeqBy (fun kvp -> (scstringm kvp.Key, (getTag kvp.Key, true)))
                                         { Items = consumables; ItemCancelOpt = Some "Cancel" })
                                     Entity.ItemSelectEvent =|> fun evt -> msg (ConsumableItemSelect (index, evt.Data))
                                     Entity.CancelEvent => msg (ConsumableItemCancel index)]
                             | TechMenu ->
                                Content.entity<RingMenuDispatcher> "TechMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.RingMenu :=
                                        (let techs =
                                            ally.Techs |>
                                            Map.ofSeqBy (fun tech ->
                                                let techUsable =
                                                    match Map.tryFind tech Data.Value.Techs with
                                                    | Some techData -> techData.TechCost <= ally.TechPoints && not (Map.containsKey Silence ally.Statuses)
                                                    | None -> false
                                                (scstringm tech, (getTag tech, techUsable)))
                                         { Items = techs; ItemCancelOpt = Some "Cancel" })
                                     Entity.ItemSelectEvent =|> fun evt -> msg (TechItemSelect (index, evt.Data))
                                     Entity.CancelEvent => msg (TechItemCancel index)]
                             | AimReticles _ ->
                                Content.entity<ReticlesDispatcher> "Reticles"
                                    [Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.Reticles :=
                                        (let aimType =
                                            match Battle.tryGetCharacter index battle with
                                            | Some character -> character.InputState.AimType
                                            | None -> NoAim
                                         let characters = Battle.getTargets aimType battle
                                         let reticles =
                                            Map.map (fun _ (c : Character) ->
                                                match c.Stature with
                                                | BossStature -> c.CenterOffset2
                                                | _ -> c.CenterOffset)
                                                characters
                                         reticles)
                                     Entity.TargetSelectEvent =|> fun evt -> msg (ReticlesSelect (index, evt.Data))
                                     Entity.CancelEvent => msg (ReticlesCancel index)]
                             | NoInput -> ()]]]