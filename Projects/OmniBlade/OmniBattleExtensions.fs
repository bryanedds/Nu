// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module BattleExtensions =

    [<RequireQualifiedAccess>]
    module Battle =

        let private updateAttack sourceIndex (targetIndexOpt : CharacterIndex option) time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.IsHealthy ->
                match targetIndexOpt with
                | Some targetIndex ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        match localTime with
                        | 0L ->
                            if target.IsHealthy then
                                withMsg (AttackCharacter1 sourceIndex) battle
                            else
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                        | 15L ->
                            withMsg (AttackCharacter2 (sourceIndex, targetIndex)) battle
                        | _ when localTime > 15L && Character.getAnimationFinished time target ->
                            let target = Battle.getCharacter targetIndex battle
                            if target.IsHealthy then
                                let battle =
                                    if  (match source.CharacterType with Enemy MadMinotaur -> false | _ -> true) && // HACK: disallow countering mad minotaurs since it nerfs challenge of first battle.
                                        Battle.shouldCounter sourceIndex targetIndex battle
                                    then Battle.counterAttack sourceIndex targetIndex battle
                                    else battle
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                withMsgs [PoiseCharacter sourceIndex; PoiseCharacter targetIndex] battle
                            else
                                let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                                let battle = Battle.updateCurrentCommandOpt (constant (Some woundCommand)) battle
                                withMsg (PoiseCharacter sourceIndex) battle
                        | _ -> just battle
                    | None ->
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | None ->
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        let private updateDefend sourceIndex time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.IsHealthy ->
                match localTime with
                | 0L ->
                    let battle =
                        battle |>
                        Battle.updateCharacterActionTime (constant 0.0f) sourceIndex |>
                        Battle.updateCharacterInputState (constant NoInput) sourceIndex |>
                        Battle.animateCharacter time (PoiseAnimation Defending) sourceIndex |>
                        Battle.defendCharacter sourceIndex
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    just battle
                | _ -> just battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        let private updateConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.IsHealthy ->
                match targetIndexOpt with
                | Some targetIndex ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        match localTime with
                        | 0L ->
                            if target.IsHealthy || consumable = Revive then // HACK: should really be checked ConsumableData.
                                withMsg (ConsumeCharacter1 (consumable, sourceIndex)) battle
                            else
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                        | 30L ->
                            withMsg (ConsumeCharacter2 (consumable, targetIndex)) battle
                        | _ when localTime > 30L && Character.getAnimationFinished time target ->
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [PoiseCharacter sourceIndex; PoiseCharacter targetIndex] battle
                        | _ -> just battle
                    | None ->
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | None ->
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        let private updateTech techType sourceIndex (targetIndexOpt : CharacterIndex option) (_ : int64) localTime battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    match (Map.tryFind techType Data.Value.Techs,  Map.tryFind techType Data.Value.TechAnimations) with
                    | (Some techData, Some techAnimationData) ->
                        ignore techData // TODO: check for target.IsWounded case if techData is affecting wounded...
                        if target.IsHealthy then
                            let (msgs, battle) =
                                if localTime = techAnimationData.TechStart then ([TechCharacter1 (sourceIndex, targetIndex, techType)], battle)
                                elif localTime = techAnimationData.TechingStart then ([TechCharacter2 (sourceIndex, targetIndex, techType)], battle)
                                elif localTime = techAnimationData.AffectingStart then ([TechCharacter3 (sourceIndex, targetIndex, techType)], battle)
                                elif localTime = techAnimationData.AffectingStop then ([TechCharacter4 (sourceIndex, targetIndex, techType)], battle)
                                elif localTime = techAnimationData.TechingStop then ([TechCharacter5 (sourceIndex, targetIndex, techType)], battle)
                                elif localTime = techAnimationData.TechStop then ([TechCharacter6 (sourceIndex, targetIndex, techType)], battle)
                                else ([], battle)
                            let (msgs, battle) = (msgs @ [TechCharacterAmbient (sourceIndex, targetIndex, techType)], battle)
                            withMsgs msgs battle
                        else
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    | (_, _) ->
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | None ->
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle

        let rec private updateWound targetIndexOpt time battle =
            match targetIndexOpt with
            | Some targetIndex ->
                let character = Battle.getCharacter targetIndex battle
                let (sigs, battle) =
                    if character.IsAlly then
                        match character.CharacterAnimationType with
                        | DamageAnimation ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                ([Message (WoundCharacter targetIndex)], battle)
                            else ([], battle)
                        | PoiseAnimation _ -> // allies don't have a wound animation state but rather return to poise state
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            ([Message (WoundCharacter targetIndex)], battle)
                        | _ -> failwithumf ()
                    else
                        match character.CharacterAnimationType with
                        | DamageAnimation ->
                            if Character.getAnimationFinished time character then
                                let woundCharacter = WoundCharacter targetIndex
                                let playDeathSound = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastDeathSound)
                                ([Message woundCharacter; Command playDeathSound], battle)
                            else ([], battle)
                        | WoundAnimation ->
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
                            let battle = Battle.updateBattleState (constant (BattleQuitting (time, false, Set.empty))) battle
                            let (sigs2, battle) = update time battle
                            (msg (CelebrateCharacters false) :: sigs @ sigs2, battle)
                        elif List.forall (fun (character : Character) -> character.IsWounded) enemies && List.hasAtMost 1 enemies then
                            // won battle
                            let battle = Battle.updateBattleState (constant (BattleResult (time, true))) battle
                            let (sigs2, battle) = update time battle
                            (msg (CelebrateCharacters true) :: sigs @ sigs2, battle)
                        else (sigs, battle)
                    | Some _ -> (sigs, battle)
                withSigs sigs battle
            | None -> just battle

        and private updateReady time startTime (battle : Battle) =
            let localTime = time - startTime
            if localTime = inc 63L then // first frame after transitioning in
                match battle.BattleSongOpt with
                | Some battleSong -> withCmd (BattleCommand.PlaySong (0, Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, 0.0, battleSong)) battle
                | None -> just battle
            elif localTime >= 90L && localTime < 160L then
                let localTimeReady = localTime - 90L
                withMsg (ReadyCharacters localTimeReady) battle
            elif localTime = 160L then
                let battle = Battle.updateBattleState (constant BattleRunning) battle
                withMsgs [PoiseCharacters; AutoBattleEnemies] battle
            else just battle

        and private updateCurrentCommand time currentCommand battle =
            let localTime = time - currentCommand.StartTime
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                updateAttack source targetOpt time localTime battle
            | Defend ->
                let source = currentCommand.ActionCommand.Source
                updateDefend source time localTime battle
            | Tech techType ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                updateTech techType source targetOpt time localTime battle
            | Consume consumable ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                updateConsume consumable source targetOpt time localTime battle
            | Wound ->
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                updateWound targetOpt time battle

        and private updateNextCommand time nextCommand futureCommands battle =
            let command = CurrentCommand.make time nextCommand
            let sourceIndex = command.ActionCommand.Source
            let targetIndexOpt = command.ActionCommand.TargetOpt
            let source = Battle.getCharacter sourceIndex battle
            let battle =
                match command.ActionCommand.Action with
                | Attack | Defend ->
                    if source.IsHealthy && not (Map.containsKey Sleep source.Statuses) then
                        let targetIndexOpt = Battle.tryRetargetIfNeeded false targetIndexOpt battle
                        let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                        Battle.updateCurrentCommandOpt (constant (Some command)) battle
                    else battle
                | Consume consumableType ->
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumable) ->
                        if source.IsHealthy && not (Map.containsKey Sleep source.Statuses) then
                            let targetIndexOpt = Battle.tryRetargetIfNeeded consumable.Revive targetIndexOpt battle
                            let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                            Battle.updateCurrentCommandOpt (constant (Some command)) battle
                        else battle
                    | (false, _) -> battle
                | Tech techType ->
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, _) ->
                        if source.IsHealthy && not (Map.containsKey Sleep source.Statuses) && not (Map.containsKey Silence source.Statuses) then
                            let targetIndexOpt = Battle.tryRetargetIfNeeded false targetIndexOpt battle // TODO: consider affecting wounded.
                            let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                            Battle.updateCurrentCommandOpt (constant (Some command)) battle
                        else battle
                    | (false, _) -> battle
                | Wound ->
                    Battle.updateCurrentCommandOpt (constant (Some command)) battle
            let battle = Battle.updateActionCommands (constant futureCommands) battle
            update time battle

        and private updateNoNextCommand (_ : int64) battle =
            let (allySignalsRev, battle) =
                Map.fold (fun (signals, battle) allyIndex (ally : Character) ->
                    if  ally.ActionTime >= Constants.Battle.ActionTime &&
                        ally.InputState = NoInput then
                        let battle = Battle.updateCharacterInputState (constant RegularMenu) allyIndex battle
                        let playReadySound = BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ReadySound)
                        (Command playReadySound :: signals, battle)
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
                        if character.IsAlly || battle.BattleSpeed = WaitSpeed
                        then Constants.Battle.AllyActionTimeDelta
                        else Constants.Battle.EnemyActionTimeDelta
                    let actionTimeDelta =
                        if Map.containsKey (Time false) character.Statuses then actionTimeDelta * Constants.Battle.ActionTimeSlowScalar
                        elif Map.containsKey (Time true) character.Statuses then actionTimeDelta * Constants.Battle.ActionTimeHasteScalar
                        else actionTimeDelta
                    let actionTimeDelta =
                        let anyAlliesInputting = Battle.getAlliesHealthy battle |> Map.toValueList |> List.exists (fun ally -> ally.InputState <> CharacterInputState.NoInput)
                        if anyAlliesInputting then
                            match battle.BattleSpeed with
                            | SwiftSpeed -> actionTimeDelta * Constants.Battle.SwiftSpeedScalar
                            | PacedSpeed -> actionTimeDelta * Constants.Battle.PacedSpeedScalar
                            | WaitSpeed -> 0.0f
                        else actionTimeDelta * 1.0f
                    let poisoned =
                        let actionTime = character.ActionTime + actionTimeDelta
                        Map.containsKey Poison character.Statuses &&
                        character.ActionTime % 500.0f < 250.0f &&
                        actionTime % 500.0f >= 250.0f
                    let character =
                        if character.IsHealthy && not (Map.containsKey Sleep character.Statuses)
                        then Character.updateActionTime ((+) actionTimeDelta) character
                        else character
                    let character =
                        if character.IsHealthy
                        then Character.burndownStatuses actionTimeDelta character
                        else character
                    let character =
                        if character.IsHealthy && poisoned then
                            let damage = single character.HitPointsMax * Constants.Battle.PoisonDrainRate |> max 1.0f |> int
                            let alliesHealthy = Battle.getAlliesHealthy battle
                            Character.updateHitPoints (fun hp -> (false, max 1 (hp - damage))) false alliesHealthy character
                        else character
                    let character =
                        if character.IsHealthy && Character.isReadyForAutoBattle character then
                            let alliesHealthy = Battle.getAlliesHealthy battle
                            let alliesWounded = Battle.getAlliesWounded battle
                            let enemiesHealthy = Battle.getEnemiesHealthy battle
                            let enemiesWounded = Battle.getEnemiesWounded battle
                            Character.autoBattle alliesHealthy alliesWounded enemiesHealthy enemiesWounded character
                        else character
                    character)
                    battle
            withSigs (List.rev (allySignalsRev @ enemySignalsRev)) battle

        and private updateNoCurrentCommand time (battle : Battle) =
            match battle.ActionCommands with
            | Queue.Cons (nextCommand, futureCommands) -> updateNextCommand time nextCommand futureCommands battle
            | Queue.Nil -> updateNoNextCommand time battle

        and private updateRunning time (battle : Battle) =
            match battle.CurrentCommandOpt with
            | Some currentCommand -> updateCurrentCommand time currentCommand battle
            | None -> updateNoCurrentCommand time battle

        and private updateResults time startTime outcome (battle : Battle) =
            let localTime = time - startTime
            if localTime = 0L then
                let alliesLevelingUp =
                    battle |> Battle.getAllies |> Map.toValueList |>
                    List.filter (fun ally -> ally.HitPoints > 0) |>
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
                let textD =
                    match battle.PrizePool.Items with
                    | _ :: _ as items -> "^Found " + (items |> List.map (fun i -> ItemType.getName i) |> String.join ", ") + "!"
                    | [] -> ""
                let text = textA + textB + textC + textD
                let dialog = { DialogForm = DialogThick; DialogTokenized = text; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None }
                let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                let (sigs, battle) =
                    if outcome then
                        let battle = Battle.updateAllies (fun ally -> if ally.IsHealthy then Character.updateExpPoints ((+) battle.PrizePool.Exp) ally else ally) battle
                        let battle = Battle.updateInventory (fun inv -> { inv with Gold = inv.Gold + battle.PrizePool.Gold }) battle
                        let battle = Battle.updateInventory (Inventory.tryAddItems battle.PrizePool.Items >> snd) battle
                        if List.notEmpty alliesLevelingUp
                        then ([cmd (BattleCommand.PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.GrowthSound))], battle)
                        else ([], battle)
                    else ([], battle)
                (cmd (BattleCommand.FadeOutSong 6000) :: sigs, battle)
            else
                match battle.DialogOpt with
                | None -> just (Battle.updateBattleState (constant (BattleQuitting (time, outcome, battle.PrizePool.Consequents))) battle)
                | Some _ -> just battle

        and private updateCease time startTime battle =
            let localTime = time - startTime
            if localTime = 0L
            then withCmd (BattleCommand.FadeOutSong Constants.Audio.FadeOutMsDefault) battle
            else just battle

        and update time (battle : Battle) =
            match battle.BattleState with
            | BattleReady startTime -> updateReady time startTime battle
            | BattleRunning -> updateRunning time battle
            | BattleResult (startTime, outcome) -> updateResults time startTime outcome battle
            | BattleQuitting (startTime, _, _) -> updateCease time startTime battle
            | BattleQuit -> just battle