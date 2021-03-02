// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module BattleDispatcher =

    type [<ReferenceEquality; NoComparison>] Hop =
        { HopStart : Vector2
          HopStop : Vector2 }

    type BattleMessage =
        | Tick
        | UpdateDialog
        | InteractDialog
        | RegularItemSelect of CharacterIndex * string
        | RegularItemCancel of CharacterIndex
        | ConsumableItemSelect of CharacterIndex * string
        | ConsumableItemCancel of CharacterIndex
        | TechItemSelect of CharacterIndex * string
        | TechItemCancel of CharacterIndex
        | ReticlesSelect of CharacterIndex * CharacterIndex
        | ReticlesCancel of CharacterIndex
        | ReadyCharacters of int64
        | PoiseCharacters
        | CelebrateCharacters of bool
        | AttackCharacter1 of CharacterIndex
        | AttackCharacter2 of CharacterIndex * CharacterIndex
        | ConsumeCharacter1 of ConsumableType * CharacterIndex
        | ConsumeCharacter2 of ConsumableType * CharacterIndex
        | TechCharacter1 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter2 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter3 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter4 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter5 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter6 of CharacterIndex * CharacterIndex * TechType
        | TechCharacterAmbient of CharacterIndex * CharacterIndex * TechType
        | ChargeCharacter of CharacterIndex
        | PoiseCharacter of CharacterIndex
        | WoundCharacter of CharacterIndex
        | ResetCharacter of CharacterIndex
        | DestroyCharacter of CharacterIndex

    type [<NoEquality; NoComparison>] BattleCommand =
        | UpdateEye
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of CharacterIndex
        | DisplayCycloneBlur of CharacterIndex * single
        | DisplayImpactSplash of CharacterIndex * int64
        | DisplaySlashSpike of Vector2 * CharacterIndex * int64
        | DisplayHop of Hop
        | DisplayCircle of Vector2 * single
        | PlaySound of int64 * single * AssetTag<Sound>
        | PlaySong of int * single * Song AssetTag
        | FadeOutSong of int

    type Screen with
        member this.GetBattle = this.GetModel<Battle>
        member this.SetBattle = this.SetModel<Battle>
        member this.Battle = this.Model<Battle> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<Battle, BattleMessage, BattleCommand> (Battle.empty)

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    match timeLocal with
                    | 0L ->
                        if target.IsHealthy
                        then withMsg (AttackCharacter1 sourceIndex) battle
                        else
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    | 15L ->
                        withMsg (AttackCharacter2 (sourceIndex, targetIndex)) battle
                    | _ when Character.getAnimationFinished time target ->
                        let target = Battle.getCharacter targetIndex battle
                        if target.IsHealthy then
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [PoiseCharacter sourceIndex; PoiseCharacter targetIndex] battle
                        else
                            let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                            let battle = Battle.updateCurrentCommandOpt (constant (Some woundCommand)) battle
                            withMsg (PoiseCharacter sourceIndex) battle
                    | _ -> just battle
                | None ->
                    // TODO: change target automatically.
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle

        static let tickDefend sourceIndex time timeLocal battle =
            match timeLocal with
            | 0L ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateActionTime (constant 0) >>
                         Character.updateInputState (constant NoInput) >>
                         Character.animate time (PoiseCycle Defending) >>
                         Character.defend)
                        sourceIndex
                        battle
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle
            | _ -> just battle

        static let tickConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    match timeLocal with
                    | 0L ->
                        if target.IsHealthy
                        then withMsg (ConsumeCharacter1 (consumable, sourceIndex)) battle
                        else
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    | 30L ->
                        withMsg (ConsumeCharacter2 (consumable, targetIndex)) battle
                    | _ when Character.getAnimationFinished time target ->
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [PoiseCharacter sourceIndex; PoiseCharacter targetIndex] battle
                    | _ -> just battle
                | None ->
                    // TODO: change target automatically
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle

        static let tickTech techType sourceIndex (targetIndexOpt : CharacterIndex option) (_ : int64) timeLocal battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some _ ->
                    match Map.tryFind techType Data.Value.TechAnimations with
                    | Some techData ->
                        let (msgs, battle) =
                            if timeLocal = techData.TechStart then ([TechCharacter1 (sourceIndex, targetIndex, techType)], battle)
                            elif timeLocal = techData.TechingStart then ([TechCharacter2 (sourceIndex, targetIndex, techType)], battle)
                            elif timeLocal = techData.AffectingStart then ([TechCharacter3 (sourceIndex, targetIndex, techType)], battle)
                            elif timeLocal = techData.AffectingStop then ([TechCharacter4 (sourceIndex, targetIndex, techType)], battle)
                            elif timeLocal = techData.TechingStop then ([TechCharacter5 (sourceIndex, targetIndex, techType)], battle)
                            elif timeLocal = techData.TechStop then ([TechCharacter6 (sourceIndex, targetIndex, techType)], battle)
                            else ([], battle)
                        let (msgs, battle) = (msgs @ [TechCharacterAmbient (sourceIndex, targetIndex, techType)], battle)
                        withMsgs msgs battle
                    | None ->
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | None ->
                    // TODO: change target automatically
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle

        static let rec tickWound targetIndexOpt time battle =
            match targetIndexOpt with
            | Some targetIndex ->
                let character = Battle.getCharacter targetIndex battle
                let (sigs, battle) =
                    if character.IsAlly then
                        match character.AnimationCycle with
                        | DamageCycle ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                ([Message (WoundCharacter targetIndex)], battle)
                            else ([], battle)
                        | _ -> failwithumf ()
                    else
                        match character.AnimationCycle with
                        | DamageCycle ->
                            if Character.getAnimationFinished time character then
                                let woundCharacter = WoundCharacter targetIndex
                                let playDeathSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastDeathSound)
                                ([Message woundCharacter; Command playDeathSound], battle)
                            else ([], battle)
                        | WoundCycle ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                ([Message (DestroyCharacter targetIndex)], battle)
                            else ([], battle)
                        | _ -> failwithumf ()
                let (sigs, battle) =
                    match battle.CurrentCommandOpt with
                    | None ->
                        let allies = battle |> Battle.getAllies |> Map.toValueList
                        let enemies = battle |> Battle.getEnemies |> Map.toValueList
                        if List.forall (fun (character : Character) -> character.IsWounded) allies then
                            // lost battle
                            let battle = Battle.updateBattleState (constant (BattleCease (false, Set.empty, time))) battle
                            let (sigs2, battle) = tick time battle
                            (sigs @ sigs2, battle)
                        elif
                            List.forall (fun (character : Character) -> character.IsWounded) enemies &&
                            List.hasAtMost 1 enemies then
                            // won battle
                            let battle = Battle.updateBattleState (constant (BattleResults (true, time))) battle
                            let (sigs2, battle) = tick time battle
                            (sigs @ sigs2, battle)
                        else (sigs, battle)
                    | Some _ -> (sigs, battle)
                withSigs sigs battle
            | None -> just battle

        and tickReady time timeStart (battle : Battle) =
            let timeLocal = time - timeStart
            if timeLocal = inc 61L then // first frame after transitioning in (including last transition frame)
                match battle.BattleSongOpt with
                | Some battleSong -> withCmd (PlaySong (Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, battleSong)) battle
                | None -> just battle
            elif timeLocal >= 90L && timeLocal < 160L then
                let timeLocalReady = timeLocal - 90L
                withMsg (ReadyCharacters timeLocalReady) battle
            elif timeLocal = 160L then
                let battle = Battle.updateBattleState (constant BattleRunning) battle
                withMsg PoiseCharacters battle
            else just battle

        and tickCurrentCommand time currentCommand battle =
            let timeLocal = time - currentCommand.TimeStart
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickAttack source targetOpt time timeLocal battle
            | Defend ->
                let source = currentCommand.ActionCommand.Source
                tickDefend source time timeLocal battle
            | Tech techType ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickTech techType source targetOpt time timeLocal battle
            | Consume consumable ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickConsume consumable source targetOpt time timeLocal battle
            | Wound ->
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickWound targetOpt time battle

        and tickNextCommand time nextCommand futureCommands (battle : Battle) =
            let command = CurrentCommand.make time nextCommand
            let battle = Battle.updateCurrentCommandOpt (constant (Some command)) battle
            let battle = Battle.updateActionCommands (constant futureCommands) battle
            tick time battle

        and tickNoNextCommand (_ : int64) (battle : Battle) =
            let (allySignalsRev, battle) =
                Map.fold (fun (signals, battle) allyIndex (ally : Character) ->
                    if  ally.ActionTime >= Constants.Battle.ActionTime &&
                        ally.InputState = NoInput then
                        let battle = Battle.updateCharacter (Character.updateInputState (constant RegularMenu)) allyIndex battle
                        let playActionTimeSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gui.AffirmSound)
                        (Command playActionTimeSound :: signals, battle)
                    else (signals, battle))
                    ([], battle)
                    (Battle.getAllies battle)
            let (enemySignalsRev, battle) =
                Map.fold (fun (signals, battle) enemyIndex (enemy : Character) ->
                    if  enemy.ActionTime >= Constants.Battle.ActionTime &&
                        not (Battle.characterAppendedActionCommand enemyIndex battle) then
                        match enemy.AutoBattleOpt with
                        | Some autoBattle ->
                            let actionCommand =
                                match autoBattle.AutoTechOpt with
                                | Some tech -> { Action = Tech tech; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                | None -> { Action = Attack; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                            let battle = Battle.appendActionCommand actionCommand battle
                            (Message (ResetCharacter enemyIndex) :: signals, battle)
                        | None -> (Message (ResetCharacter enemyIndex) :: signals, battle)
                    else (signals, battle))
                    ([], battle)
                    (Battle.getEnemies battle)
            let battle =
                Battle.updateCharacters (fun character ->
                    let actionTimeDelta =
                        if character.IsAlly
                        then Constants.Battle.AllyActionTimeDelta
                        else Constants.Battle.EnemyActionTimeDelta
                    let character =
                        if character.IsHealthy
                        then Character.updateActionTime ((+) actionTimeDelta) character
                        else character
                    let character =
                        if Character.isReadyForAutoBattle character then
                            let targetIndex = Battle.getAllyIndexRandom battle
                            let target = Battle.getCharacter targetIndex battle
                            Character.autoBattle character target
                        else character
                    character)
                    battle
            withSigs (List.rev (allySignalsRev @ enemySignalsRev)) battle

        and tickNoCurrentCommand time (battle : Battle) =
            match battle.ActionCommands with
            | Queue.Cons (nextCommand, futureCommands) -> tickNextCommand time nextCommand futureCommands battle
            | Queue.Nil -> tickNoNextCommand time battle

        and tickRunning time (battle : Battle) =
            match battle.CurrentCommandOpt with
            | Some currentCommand -> tickCurrentCommand time currentCommand battle
            | None -> tickNoCurrentCommand time battle

        and tickBattleResults time timeStart outcome (battle : Battle) =
            let localTime = time - timeStart
            if localTime = 0L then
                let alliesLevelingUp =
                    battle |> Battle.getAllies |> Map.toValueList |>
                    List.filter (fun ally -> Algorithms.expPointsRemainingForNextLevel ally.ExpPoints <= battle.PrizePool.Exp)
                let textA =
                    match alliesLevelingUp with
                    | _ :: _ -> "Level up for " + (alliesLevelingUp |> List.map (fun c -> c.Name) |> String.join ", ") + "!^"
                    | [] -> "Enemies defeated!^"
                let textB =
                    alliesLevelingUp |>
                    List.choose (fun ally ->
                        let techs = Algorithms.expPointsToTechs3 ally.ExpPoints battle.PrizePool.Exp ally.ArchetypeType
                        if Set.notEmpty techs then Some (ally, techs) else None) |>
                    List.map (fun (ally, techs) ->
                        let text = techs |> Set.toList |> List.map scstring |> String.join ", "
                        ally.Name + " learned " + text + "!") |>
                    function
                    | _ :: _ as texts -> String.join "\n" texts + "^"
                    | [] -> ""
                let textC = "Gained " + string battle.PrizePool.Exp + " Exp!\nGained " + string battle.PrizePool.Gold + " Gold!"
                let text = textA + textB + textC
                let dialog = { DialogForm = DialogThick; DialogText = text; DialogProgress = 0; DialogPage = 0; DialogBattleOpt = None }
                let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                let sigs = [msg (CelebrateCharacters outcome)]
                if outcome then
                    let battle = Battle.updateAllies (fun ally -> if ally.IsHealthy then Character.updateExpPoints ((+) battle.PrizePool.Exp) ally else ally) battle
                    let battle = Battle.updateInventory (fun inv -> { inv with Gold = inv.Gold + battle.PrizePool.Gold }) battle
                    let battle = Battle.updateInventory (Inventory.tryAddItems battle.PrizePool.Items >> snd) battle
                    let sigs = if List.notEmpty alliesLevelingUp then cmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.GrowthSound)) :: sigs else sigs
                    withSigs sigs battle
                else withSigs sigs battle
            else
                match battle.DialogOpt with
                | None -> just (Battle.updateBattleState (constant (BattleCease (outcome, battle.PrizePool.Consequents, time))) battle)
                | Some _ -> just battle

        and tickBattleCease time timeStart battle =
            let localTime = time - timeStart
            if localTime = 0L
            then withCmd (FadeOutSong Constants.Audio.FadeOutMsDefault) battle
            else just battle

        and tick time (battle : Battle) =
            match battle.BattleState with
            | BattleReady timeStart -> tickReady time timeStart battle
            | BattleRunning -> tickRunning time battle
            | BattleResults (outcome, timeStart) -> tickBattleResults time timeStart outcome battle
            | BattleCease (_, _, timeStart) -> tickBattleCease time timeStart battle

        override this.Channel (_, battle) =
            [battle.UpdateEvent => msg Tick
             battle.UpdateEvent => msg UpdateDialog
             battle.PostUpdateEvent => cmd UpdateEye]

        override this.Message (battle, message, _, world) =

            match message with
            | Tick ->
                if World.isTicking world
                then tick (World.getTickTime world) battle
                else just battle

            | UpdateDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    let dialog = Dialog.update dialog world
                    let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                    just battle
                | None -> just battle

            | InteractDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    match Dialog.tryAdvance dialog with
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
                        Battle.updateCharacter
                            (Character.updateInputState (constant (AimReticles (item, EnemyAim true))) >> Character.undefend)
                            characterIndex
                            battle
                    | "Defend" ->
                        let battle =
                            Battle.updateCharacter
                                (Character.updateInputState (constant NoInput))
                                characterIndex
                                battle
                        let command = ActionCommand.make Defend characterIndex None
                        let battle = Battle.appendActionCommand command battle
                        battle
                    | "Tech" ->
                        Battle.updateCharacter
                            (Character.updateInputState (constant TechMenu) >> Character.undefend)
                            characterIndex
                            battle
                    | "Consumable" ->
                        Battle.updateCharacter
                            (Character.updateInputState (constant ItemMenu) >> Character.undefend)
                            characterIndex
                            battle
                    | _ -> failwithumf ()
                just battle
            
            | RegularItemCancel characterIndex ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant RegularMenu))
                        characterIndex
                        battle
                just battle
            
            | ConsumableItemSelect (characterIndex, item) ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant (AimReticles (item, AllyAim true))))
                        characterIndex
                        battle
                just battle

            | ConsumableItemCancel characterIndex ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant RegularMenu))
                        characterIndex
                        battle
                just battle
            
            | TechItemSelect (characterIndex, item) ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant (AimReticles (item, EnemyAim true))))
                        characterIndex
                        battle
                just battle
            
            | TechItemCancel characterIndex ->
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant RegularMenu))
                        characterIndex
                        battle
                just battle

            | ReticlesSelect (targetIndex, allyIndex) ->
                match battle.BattleState with
                | BattleRunning ->
                    match Battle.tryGetCharacter allyIndex battle with
                    | Some ally ->
                        match Character.getActionTypeOpt ally with
                        | Some actionType ->
                            let command = ActionCommand.make actionType allyIndex (Some targetIndex)
                            let battle = Battle.appendActionCommand command battle
                            withMsg (ResetCharacter allyIndex) battle
                        | None -> just battle
                    | None -> just battle
                | _ -> just battle

            | ReticlesCancel characterIndex ->
                let battle =
                    Battle.tryUpdateCharacter (fun character ->
                        match Character.getActionTypeOpt character with
                        | Some actionType ->
                            let inputState =
                                match actionType with
                                | Attack -> RegularMenu
                                | Defend -> RegularMenu
                                | Tech _ -> TechMenu
                                | Consume _ -> ItemMenu
                                | Wound -> failwithumf ()
                            Character.updateInputState (constant inputState) character
                        | None -> character)
                        characterIndex
                        battle
                just battle

            | ReadyCharacters timeLocal ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacters (Character.animate time ReadyCycle) battle
                if timeLocal = 30L
                then withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UnsheatheSound)) battle
                else just battle

            | PoiseCharacters ->
                let time = World.getTickTime world
                let battle =
                    Battle.updateCharacters
                        (fun character ->
                            let poiseType = Character.getPoiseType character
                            let character = Character.animate time (PoiseCycle poiseType) character
                            character)
                        battle
                just battle

            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let battle =
                    if outcome
                    then Battle.updateAllies (Character.animate time CelebrateCycle) battle
                    else Battle.updateEnemies (Character.animate time CelebrateCycle) battle
                just battle

            | AttackCharacter1 sourceIndex ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time AttackCycle) sourceIndex battle
                let playHit = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                withCmd playHit battle

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let source = Battle.getCharacter sourceIndex battle
                let target = Battle.getCharacter targetIndex battle
                let damage = Character.getAttackResult Physical source target
                let battle = Battle.updateCharacter (Character.updateHitPoints (fun hitPoints -> (hitPoints - damage, false))) targetIndex battle
                let (battle, sigs) =
                    if target.HitPoints <= 0
                    then (battle, [Message (ResetCharacter targetIndex)]) // wounded
                    else (Battle.updateCharacter (Character.animate time DamageCycle) targetIndex battle, []) // just damaged
                let sigs = Command (DisplayHitPointsChange (targetIndex, -damage)) :: sigs
                withSigs sigs battle

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time CastCycle) sourceIndex battle
                let item = Consumable consumable
                let battle = Battle.updateInventory (Inventory.removeItem item) battle
                just battle

            | ConsumeCharacter2 (consumableType, targetIndex) ->
                let time = World.getTickTime world
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumable) ->
                    if consumable.Curative then
                        let healing = int consumable.Scalar
                        let battle = Battle.updateCharacter (Character.updateHitPoints (fun hitPoints -> (hitPoints + healing, false))) targetIndex battle
                        let battle = Battle.updateCharacter (Character.animate time SpinCycle) targetIndex battle
                        let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                        let playHealSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)
                        withCmds [displayHitPointsChange; playHealSound] battle
                    else
                        // TODO: non-curative case
                        just battle
                | (false, _) -> just battle

            | TechCharacter1 (sourceIndex, targetIndex, techType) ->
                let source = Battle.getCharacter sourceIndex battle
                let target = Battle.getCharacter targetIndex battle
                let hopOpt =
                    // TODO: pull behavior from data
                    match techType with
                    | Critical ->
                        let hopDirection = Direction.ofVector2 (target.Bottom - source.Bottom)
                        let hopStop = target.Bottom - Direction.toVector2 hopDirection * Constants.Battle.StrikingDistance
                        Some { HopStart = source.Bottom; HopStop = hopStop }
                    | Cyclone -> Some { HopStart = source.Bottom; HopStop = target.BottomOffset2 }
                    | Bolt | Slash | Tremor -> None
                match hopOpt with
                | None ->
                    if target.IsHealthy then
                        withMsg (ChargeCharacter sourceIndex) battle
                    else
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | Some hop ->
                    withCmd (DisplayHop hop) battle

            | TechCharacter2 (sourceIndex, targetIndex, techType) ->
                match techType with
                | Critical ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (targetIndex, 30L)
                    let battle = Battle.updateCharacter (Character.animate time AttackCycle) sourceIndex battle
                    withCmds [playHit; impactSplash] battle
                | Cyclone ->
                    let time = World.getTickTime world
                    let radius = 64.0f
                    let position = (Battle.getCharacter sourceIndex battle).Bottom
                    let playHits =
                        [PlaySound (20L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         PlaySound (40L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                         PlaySound (80L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)]
                    let battle = Battle.updateCharacter (Character.animate time WhirlCycle) sourceIndex battle
                    withCmds (DisplayCircle (position, radius) :: DisplayCycloneBlur (sourceIndex, radius) :: playHits) battle
                | Slash ->
                    let time = World.getTickTime world
                    let playSlash = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.SlashSound)
                    let playHit = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let slashSpike = DisplaySlashSpike ((Battle.getCharacter sourceIndex battle).Bottom, targetIndex, 10L)
                    let impactSplash = DisplayImpactSplash (targetIndex, 70L)
                    let battle = Battle.updateCharacter (Character.animate time SlashCycle) sourceIndex battle
                    withCmds [playSlash; playHit; slashSpike; impactSplash] battle
                | Bolt ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Cycle) sourceIndex battle
                    withCmd (DisplayBolt targetIndex) battle
                | Tremor ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time BuryCycle) sourceIndex battle
                    withCmd (DisplayBolt targetIndex) battle

            | TechCharacter3 (sourceIndex, targetIndex, techType) ->
                let time = World.getTickTime world
                match Map.tryFind techType Data.Value.Techs with
                | Some techData ->
                    let source = Battle.getCharacter sourceIndex battle
                    let target = Battle.getCharacter targetIndex battle
                    let characters = Battle.getCharacters battle
                    let results = Character.evaluateTechMove techData source target characters
                    let (battle, cmds) =
                        Map.fold (fun (battle, cmds) characterIndex (cancelled, hitPointsChange) ->
                            let character = Battle.getCharacter characterIndex battle
                            if hitPointsChange < 0 && character.IsHealthy then
                                let battle = Battle.updateCharacter (Character.animate time DamageCycle) characterIndex battle
                                let cmds = if cancelled then DisplayCancel characterIndex :: cmds else cmds
                                (battle, cmds)
                            else (battle, cmds))
                            (battle, [])
                            results
                    withCmds cmds battle
                | None -> just battle

            | TechCharacter4 (sourceIndex, targetIndex, techType) ->
                match Map.tryFind techType Data.Value.Techs with
                | Some techData ->
                    let source = Battle.getCharacter sourceIndex battle
                    let target = Battle.getCharacter targetIndex battle
                    let characters = Battle.getCharacters battle
                    let results = Character.evaluateTechMove techData source target characters
                    let (battle, sigs) =
                        Map.fold (fun (battle, sigs) _ (_, _) ->
                            // TODO: glow effect
                            (battle, sigs))
                            (battle, [])
                            results
                    withSigs sigs battle
                | None -> just battle

            | TechCharacter5 (sourceIndex, targetIndex, techType) ->
                let source = Battle.getCharacter sourceIndex battle
                let target = Battle.getCharacter targetIndex battle
                let hopOpt =
                    match techType with
                    | Critical ->
                        let hopDirection = Direction.ofVector2 (target.Bottom - source.BottomOriginal)
                        let hopStart = target.Bottom - Direction.toVector2 hopDirection * Constants.Battle.StrikingDistance
                        Some { HopStart = hopStart; HopStop = source.BottomOriginal }
                    | Cyclone -> Some { HopStart = target.BottomOffset2; HopStop = source.BottomOriginal }
                    | Bolt | Slash | Tremor -> None
                match hopOpt with
                | None -> just battle
                | Some hop -> withCmd (DisplayHop hop) battle

            | TechCharacter6 (sourceIndex, targetIndex, techType) ->
                match Map.tryFind techType Data.Value.Techs with
                | Some techData ->
                    let source = Battle.getCharacter sourceIndex battle
                    let target = Battle.getCharacter targetIndex battle
                    let characters = Battle.getCharacters battle
                    let results = Character.evaluateTechMove techData source target characters
                    let (battle, sigs) =
                        Map.fold (fun (battle, sigs) characterIndex (cancelled, hitPointsChange) ->
                            let battle = Battle.updateCharacter (Character.updateHitPoints (fun hitPoints -> (hitPoints + hitPointsChange, cancelled))) characterIndex battle
                            let wounded = (Battle.getCharacter characterIndex battle).IsWounded
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
                    let battle = Battle.updateCharacter (Character.updateTechPoints ((+) -techData.TechCost)) sourceIndex battle
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    let sigs = Message (PoiseCharacter sourceIndex) :: sigs
                    withSigs sigs battle
                | None -> just battle
                    
            | TechCharacterAmbient (sourceIndex, _, _) ->
                if Simulants.Battle.Scene.Ride.Exists world then
                    let battle =
                        let tags = Simulants.Battle.Scene.Ride.GetEffectTags world
                        match Map.tryFind "Tag" tags with
                        | Some tag -> Battle.updateCharacter (Character.updateBottom (constant tag.Position)) sourceIndex battle
                        | None -> battle
                    just battle
                else just battle

            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time (PoiseCycle Charging)) sourceIndex battle
                just battle

            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let battle =
                    Battle.updateCharacter
                        (fun character ->
                            let poiseType = Character.getPoiseType character
                            let character = Character.animate time (PoiseCycle poiseType) character
                            character)
                        characterIndex
                        battle
                just battle

            | WoundCharacter characterIndex ->
                let time = World.getTickTime world
                let battle =
                    Battle.updateCharacter
                        (fun character ->
                            let character =
                                if character.IsAlly
                                then Character.updateInputState (constant NoInput) character
                                else character
                            let character = Character.animate time WoundCycle character
                            character)
                        characterIndex
                        battle
                just battle

            | ResetCharacter characterIndex ->
                let character = Battle.getCharacter characterIndex battle
                let battle = Battle.updateCharacter (Character.updateActionTime (constant 0)) characterIndex battle
                let battle =
                    if character.IsAlly
                    then Battle.updateCharacter (Character.updateInputState (constant NoInput)) characterIndex battle
                    else Battle.updateCharacter (Character.updateAutoBattleOpt (constant None)) characterIndex battle
                just battle

            | DestroyCharacter characterIndex ->
                let character = Battle.getCharacter characterIndex battle
                let battle = if character.IsEnemy then Battle.removeCharacter characterIndex battle else battle
                just battle

        override this.Command (battle, command, _, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyeCenter v2Zero world
                just world

            | DisplayCancel targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeCancelEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset3 world
                    let world = entity.SetElevation (Constants.Battle.GuiEffectElevation - 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset2 world
                    let world = entity.SetElevation Constants.Battle.GuiEffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeBoltEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize (v2 192.0f 758.0f) world
                    let world = entity.SetBottom target.Bottom world
                    let world = entity.SetElevation Constants.Battle.EffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayCycloneBlur (targetIndex, radius) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeCycloneBlurEffect radius
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize (v2 234.0f 234.0f) world
                    let world = entity.SetCenter target.Center world
                    let world = entity.SetElevation Constants.Battle.EffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayImpactSplash (targetIndex, delay) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let world =
                        World.delay (fun world ->
                            let effect = Effects.makeImpactSplashEffect ()
                            let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                            let world = entity.SetEffect effect world
                            let world = entity.SetSize (v2 192.0f 96.0f) world
                            let world = entity.SetBottom target.Bottom world
                            let world = entity.SetElevation Constants.Battle.EffectElevation world
                            entity.SetSelfDestruct true world)
                            delay
                            world
                    just world
                | None -> just world

            | DisplaySlashSpike (position, targetIndex, delay) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let world =
                        World.delay (fun world ->
                            let effect = Effects.makeSlashSpikeEffect position target.Bottom
                            let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                            let world = entity.SetEffect effect world
                            let world = entity.SetSize (v2 96.0f 96.0f) world
                            let world = entity.SetBottom target.Bottom world
                            entity.SetSelfDestruct true world)
                            delay
                            world
                    just world
                | None -> just world

            | DisplayHop hop ->
                let effect = Effects.makeHopEffect hop.HopStart hop.HopStop
                let (entity, world) = World.createEntity<EffectDispatcher> (Some Simulants.Battle.Scene.Ride.Name) DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v2Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCircle (position, radius) ->
                let effect = Effects.makeCircleEffect radius
                let (entity, world) = World.createEntity<EffectDispatcher> (Some Simulants.Battle.Scene.Ride.Name) DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetPosition position world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v2Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | PlaySong (fade, volume, assetTag) ->
                let world = World.playSong fade volume assetTag world
                just world

            | FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world
                

        override this.Content (battle, _) =

            // scene group
            [Content.group Simulants.Battle.Scene.Group.Name []

                [// tile map
                 Content.tileMap Gen.name
                    [Entity.Position == v2 -480.0f -270.0f
                     Entity.Elevation == Constants.Battle.BackgroundElevation
                     Entity.TileMap <== battle --> fun battle -> battle.TileMap]

                 // dialog
                 Dialog.content Simulants.Battle.Gui.Dialog.Name
                    (battle --> fun battle -> battle.DialogOpt)

                 // dialog interact button
                 Content.button Simulants.Battle.Gui.Interact.Name
                    [Entity.Position == v2 248.0f -240.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible <== battle --> fun battle -> match battle.DialogOpt with Some dialog -> Dialog.canAdvance dialog | None -> false
                     Entity.Text == "Next"
                     Entity.ClickEvent ==> msg InteractDialog]

                 // allies
                 Content.entities battle
                    (fun battle -> Battle.getAllies battle) constant
                    (fun index battle _ -> Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character <== battle])

                 // enemies
                 Content.entities battle
                    (fun battle -> Battle.getEnemies battle) constant
                    (fun index battle _ -> Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character <== battle])]

             // input groups
             Content.groups battle (fun battle -> Battle.getAllies battle) constant $ fun index ally _ ->

                // input group
                let inputName = "Input" + "+" + CharacterIndex.toEntityName index
                Content.group inputName []

                    [// health bar
                     Content.fillBar "HealthBar" 
                        [Entity.Size == v2 48.0f 6.0f
                         Entity.Center <== ally --> fun ally -> ally.BottomOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> ally.HitPoints > 0
                         Entity.Fill <== ally --> fun ally -> single ally.HitPoints / single ally.HitPointsMax]

                     // regular menu
                     Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.Enabled <== battle --> fun battle ->
                            let allies = battle |> Battle.getAllies |> Map.toValueList
                            let alliesPastRegularMenu =
                                Seq.notExists (fun (ally : Character) ->
                                    match ally.InputState with NoInput | RegularMenu -> false | _ -> true)
                                    allies
                            alliesPastRegularMenu
                         Entity.RingMenu == { Items = Map.ofList [(0, ("Attack", true)); (1, ("Defend", true)); (2, ("Tech", true)); (3, ("Consumable", true))]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (index, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel index)]

                     // consumable menu
                     Content.entity<RingMenuDispatcher> "ConsumableMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenu <== battle --> fun battle ->
                            let consumables =
                                battle.Inventory |>
                                Inventory.getConsumables |>
                                Map.toKeyList |>
                                Map.ofListBy (fun consumable -> (getTag consumable, (scstring consumable, true))) |>
                                Map.toValueList |>
                                Map.indexed
                            { Items = consumables; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (ConsumableItemSelect (index, evt.Data))
                         Entity.CancelEvent ==> msg (ConsumableItemCancel index)]

                     // tech menu
                     Content.entity<RingMenuDispatcher> "TechMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> ally.InputState = TechMenu
                         Entity.RingMenu <== ally --> fun ally ->
                            let techs =
                                ally.Techs |>
                                Map.ofSeqBy (fun tech ->
                                    let techName = scstring tech
                                    let techUsable =
                                        match Map.tryFind tech Data.Value.Techs with
                                        | Some techData -> techData.TechCost <= ally.TechPoints
                                        | None -> false
                                    (getTag tech, (techName, techUsable))) |>
                                Map.toValueList |>
                                Map.indexed
                            { Items = techs; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (TechItemSelect (index, evt.Data))
                         Entity.CancelEvent ==> msg (TechItemCancel index)]

                     // reticles
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.Reticles <== battle --> fun battle ->
                            let aimType =
                                match Battle.tryGetCharacter index battle with
                                | Some character -> character.InputState.AimType
                                | None -> NoAim
                            { Battle = battle; AimType = aimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, index))
                         Entity.CancelEvent ==> msg (ReticlesCancel index)]]]