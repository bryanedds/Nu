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

    type [<NoComparison>] BattleCommand =
        | UpdateEye
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of CharacterIndex
        | DisplayHop of Hop
        | DisplayCircle of Vector2 * single
        | PlaySound of int64 * single * AssetTag<Sound>

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
                                let playDeathSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.DeathSound)
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
                        let allies = Battle.getAllies battle
                        let enemies = Battle.getEnemies battle
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

        and tickReady time timeStart battle =
            let timeLocal = time - timeStart
            if timeLocal >= 90L && timeLocal < 160L then
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
                List.fold (fun (signals, battle) (ally : Character) ->
                    if  ally.ActionTime >= Constants.Battle.ActionTime &&
                        ally.InputState = NoInput then
                        let battle = Battle.updateCharacter (Character.updateInputState (constant RegularMenu)) ally.CharacterIndex battle
                        let playActionTimeSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)
                        (Command playActionTimeSound :: signals, battle)
                    else (signals, battle))
                    ([], battle)
                    (Battle.getAllies battle)
            let (enemySignalsRev, battle) =
                List.fold (fun (signals, battle) (enemy : Character) ->
                    if  enemy.ActionTime >= Constants.Battle.ActionTime &&
                        not (Battle.characterAppendedActionCommand enemy.CharacterIndex battle) then
                        let enemyIndex = enemy.CharacterIndex
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
                    battle |>
                    Battle.getAllies |>
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
                    let sigs = if List.notEmpty alliesLevelingUp then cmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.GrowthSound)) :: sigs else sigs
                    withSigs sigs battle
                else withSigs sigs battle
            else
                match battle.DialogOpt with
                | None -> just (Battle.updateBattleState (constant (BattleCease (outcome, battle.PrizePool.Consequents, time))) battle)
                | Some _ -> just battle

        and tick time battle =
            match battle.BattleState with
            | BattleReady timeStart -> tickReady time timeStart battle
            | BattleRunning -> tickRunning time battle
            | BattleResults (outcome, timeStart) -> tickBattleResults time timeStart outcome battle
            | BattleCease (_, _, _) -> just battle

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
                        let time = World.getTickTime world
                        Battle.updateCharacter
                            (Character.updateActionTime (constant 0) >>
                             Character.updateInputState (constant NoInput) >>
                             Character.animate time (PoiseCycle Defending) >>
                             Character.defend)
                            characterIndex
                            battle
                    | "Consumable" ->
                        Battle.updateCharacter
                            (Character.updateInputState (constant ItemMenu) >> Character.undefend)
                            characterIndex
                            battle
                    | "Tech" ->
                        Battle.updateCharacter
                            (Character.updateInputState (constant TechMenu) >> Character.undefend)
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
                then withCmd (PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.UnsheatheSound)) battle
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
                let playHitSoundDelay = 15L
                let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                withCmd playHitSound battle

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
                        let playHealSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.HealSound)
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
                    | Critical -> Some { HopStart = source.Bottom; HopStop = target.BottomOffset3 }
                    | Cyclone -> Some { HopStart = source.Bottom; HopStop = target.BottomOffset2 }
                    | Bolt | Tremor -> None
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
                    let battle = Battle.updateCharacter (Character.animate time AttackCycle) sourceIndex battle
                    let playHitSoundDelay = 10L
                    let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                    withCmd playHitSound battle
                | Cyclone ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time WhirlCycle) sourceIndex battle
                    let playHitSounds =
                        [PlaySound (20L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (40L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (60L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (80L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)]
                    withCmds (DisplayCircle ((Battle.getCharacter sourceIndex battle).Bottom, 64.0f) :: playHitSounds) battle
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
                        List.fold (fun (battle, cmds) (cancelled, hitPointsChange, characterIndex) ->
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
                        List.fold (fun (battle, sigs) (_, _, _) ->
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
                    | Critical | Cyclone -> Some { HopStart = target.BottomOffset2; HopStop = source.BottomOriginal }
                    | Bolt | Tremor -> None
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
                        List.fold (fun (battle, sigs) (cancelled, hitPointsChange, characterIndex) ->
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
                if Simulants.BattleRide.Exists world then
                    let battle =
                        let tags = Simulants.BattleRide.GetEffectTags world
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
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset3 world
                    let world = entity.SetDepth (Constants.Battle.GuiEffectDepth - 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset2 world
                    let world = entity.SetDepth Constants.Battle.GuiEffectDepth world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeBoltEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize (v2 192.0f 758.0f) world
                    let world = entity.SetBottom target.Bottom world
                    let world = entity.SetDepth Constants.Battle.EffectDepth world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHop hop ->
                let effect = Effects.makeHopEffect hop.HopStart hop.HopStop
                let (entity, world) = World.createEntity<EffectDispatcher> (Some Simulants.BattleRide.Name) DefaultOverlay Simulants.BattleScene world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v2Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCircle (position, radius) ->
                let effect = Effects.makeCircleEffect radius
                let (entity, world) = World.createEntity<EffectDispatcher> (Some Simulants.BattleRide.Name) DefaultOverlay Simulants.BattleScene world
                let world = entity.SetPosition position world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v2Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

        override this.Content (battle, screen) =

            // scene layer
            let background = Simulants.BattleScene / "Background"
            [Content.layer Simulants.BattleScene.Name []

                [// background
                 Content.label background.Name
                    [background.Position == v2 -480.0f -270.0f
                     background.Size == v2 960.0f 580.0f
                     background.Depth == Constants.Battle.BackgroundDepth
                     background.LabelImage == asset "Battle" "Background"]

                 // dialog
                 Dialog.content Simulants.BattleDialog.Name
                    (battle --> fun battle -> battle.DialogOpt)

                 // dialog interact button
                 Content.button Simulants.BattleInteract.Name
                    [Entity.Position == v2 248.0f -240.0f; Entity.Depth == Constants.Field.GuiDepth; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.ButtonShortUpImage; Entity.DownImage == Assets.ButtonShortDownImage
                     Entity.Visible <== battle --> fun battle -> match battle.DialogOpt with Some dialog -> Dialog.canAdvance dialog | None -> false
                     Entity.Text == "Next"
                     Entity.ClickEvent ==> msg InteractDialog]

                 // allies
                 Content.entitiesTrackedBy battle
                    (fun battle -> Battle.getAllies battle) constant
                    (fun battle -> battle.PartyIndex)
                    (fun index battle _ -> Content.entity<CharacterDispatcher> ("Ally+" + scstring index) [Entity.Character <== battle])

                 // enemies
                 Content.entitiesTrackedBy battle
                    (fun battle -> Battle.getEnemies battle) constant
                    (fun battle -> battle.PartyIndex)
                    (fun index battle _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.Character <== battle])]

             // input layers
             Content.layers battle (fun battle -> Battle.getAllies battle) constant $ fun index ally _ ->

                // input layer
                let allyIndex = AllyIndex index
                let input = screen / ("Input" + "+" + scstring index)
                Content.layer input.Name []

                    [// health bar
                     Content.fillBar "HealthBar" 
                        [Entity.Size == v2 48.0f 6.0f
                         Entity.Center <== ally --> fun ally -> ally.BottomOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.HitPoints > 0
                         Entity.Fill <== ally --> fun ally -> single ally.HitPoints / single ally.HitPointsMax]

                     // regular menu
                     Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.Enabled <== battle --> fun battle ->
                            let allies = Battle.getAllies battle
                            let alliesPastRegularMenu =
                                List.notExists (fun (ally : Character) ->
                                    match ally.InputState with NoInput | RegularMenu -> false | _ -> true)
                                    allies
                            alliesPastRegularMenu
                         Entity.RingMenu == { Items = [(0, (true, "Attack")); (1, (true, "Defend")); (2, (true, "Consumable")); (3, (true, "Tech"))]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel allyIndex)]

                     // consumable menu
                     Content.entity<RingMenuDispatcher> "ConsumableMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenu <== battle --> fun battle ->
                            let consumables = Inventory.getConsumables battle.Inventory
                            let consumables = Map.toKeyList consumables
                            let consumables = List.map (fun consumable -> (getTag consumable, (true, scstring consumable))) consumables
                            { Items = consumables; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (ConsumableItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (ConsumableItemCancel allyIndex)]

                     // tech menu
                     Content.entity<RingMenuDispatcher> "TechMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = TechMenu
                         Entity.RingMenu <== ally --> fun ally ->
                            let techs = List.ofSeq ally.Techs
                            let techs =
                                List.map (fun tech ->
                                    let techTag = getTag tech
                                    let techUsable =
                                        match Map.tryFind tech Data.Value.Techs with
                                        | Some techData -> techData.TechCost <= ally.TechPoints
                                        | None -> false
                                    let techName = scstring tech
                                    (techTag, (techUsable, techName)))
                                    techs
                            { Items = techs; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (TechItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (TechItemCancel allyIndex)]

                     // reticles
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.Reticles <== battle --> fun battle ->
                            let aimType =
                                match Battle.tryGetCharacter allyIndex battle with
                                | Some character -> character.InputState.AimType
                                | None -> NoAim
                            { Battle = battle; AimType = aimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]]