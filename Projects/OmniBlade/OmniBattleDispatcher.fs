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

    type [<StructuralEquality; NoComparison>] Positioning =
        | Position of Vector2
        | Center of Vector2
        | Bottom of Vector2

    type [<ReferenceEquality; NoComparison>] Hop =
        { HopStart : Vector2
          HopStop : Vector2 }

    type BattleMessage =
        | Update
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
        | Nop

    type [<NoEquality; NoComparison>] BattleCommand =
        | UpdateEye
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of int64 * CharacterIndex
        | DisplayCycloneBlur of int64 * CharacterIndex * single
        | DisplayImpactSplash of int64 * CharacterIndex
        | DisplayCut of int64 * bool * CharacterIndex
        | DisplaySlashSpike of int64 * Vector2 * CharacterIndex
        | DisplayArcaneCast of int64 * CharacterIndex
        | DisplayFire of int64 * CharacterIndex * CharacterIndex
        | DisplayFlame of int64 * CharacterIndex * CharacterIndex
        | DisplayIce of int64 * CharacterIndex
        | DisplaySnowball of int64 * CharacterIndex
        | DisplayHolyCast of int64 * CharacterIndex
        | DisplayPurify of int64 * CharacterIndex
        | DisplayAura of int64 * CharacterIndex
        | DisplayProtect of int64 * CharacterIndex
        | DisplayDimensionalCast of int64 * CharacterIndex
        | DisplayDebuff of int64 * StatusType * CharacterIndex
        | DisplayConjureIfrit of int64
        | DisplayHop of Hop
        | DisplayCircle of Vector2 * single
        | PlaySound of int64 * single * AssetTag<Sound>
        | PlaySong of int * int * single * double * Song AssetTag
        | FadeOutSong of int

    type Screen with
        member this.GetBattle = this.GetModel<Battle>
        member this.SetBattle = this.SetModel<Battle>
        member this.Battle = this.Model<Battle> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<Battle, BattleMessage, BattleCommand> (Battle.debug)

        static let displayEffect delay size positioning effect world =
            World.delay (fun world ->
                let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                let world = entity.SetEffect effect world
                let world = entity.SetSize size world
                let world =
                    match positioning with
                    | Position position -> entity.SetPosition position world
                    | Center center -> entity.SetCenter center world
                    | Bottom bottom -> entity.SetBottom bottom world
                let world = entity.SetElevation Constants.Battle.EffectElevation world
                entity.SetSelfDestruct true world)
                delay
                world

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.IsHealthy ->
                match targetIndexOpt with
                | Some targetIndex ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        match timeLocal with
                        | 0L ->
                            if target.IsHealthy then
                                withMsg (AttackCharacter1 sourceIndex) battle
                            else
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                        | 15L ->
                            withMsg (AttackCharacter2 (sourceIndex, targetIndex)) battle
                        | _ when timeLocal > 15L && Character.getAnimationFinished time target ->
                            let target = Battle.getCharacter targetIndex battle
                            if target.IsHealthy then
                                let battle =
                                    if Character.shouldCounter target
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
                        // TODO: change target automatically.
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                | None ->
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        static let tickDefend sourceIndex time timeLocal battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.IsHealthy ->
                match timeLocal with
                | 0L ->
                    let battle =
                        Battle.updateCharacter
                            (Character.updateActionTime (constant 0.0f) >>
                             Character.updateInputState (constant NoInput) >>
                             Character.animate time (PoiseAnimation Defending) >>
                             Character.defend)
                            sourceIndex
                            battle
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    just battle
                | _ -> just battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        static let tickConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    match timeLocal with
                    | 0L ->
                        if target.IsHealthy || consumable = Revive then // HACK: should really be checked ConsumableData.
                            withMsg (ConsumeCharacter1 (consumable, sourceIndex)) battle
                        else
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    | 30L ->
                        withMsg (ConsumeCharacter2 (consumable, targetIndex)) battle
                    | _ when timeLocal > 30L && Character.getAnimationFinished time target ->
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
                                let playDeathSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastDeathSound)
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
                            let battle = Battle.updateBattleState (constant (BattleCease (false, Set.empty, time))) battle
                            let (sigs2, battle) = tick time battle
                            (msg (CelebrateCharacters false) :: sigs @ sigs2, battle)
                        elif
                            List.forall (fun (character : Character) -> character.IsWounded) enemies &&
                            List.hasAtMost 1 enemies then
                            // won battle
                            let battle = Battle.updateBattleState (constant (BattleResults (true, time))) battle
                            let (sigs2, battle) = tick time battle
                            (msg (CelebrateCharacters true) :: sigs @ sigs2, battle)
                        else (sigs, battle)
                    | Some _ -> (sigs, battle)
                withSigs sigs battle
            | None -> just battle

        and tickReady time timeStart (battle : Battle) =
            let timeLocal = time - timeStart
            if timeLocal = inc 62L then // first frame after transitioning in
                match battle.BattleSongOpt with
                | Some battleSong -> withCmd (PlaySong (0, Constants.Audio.FadeOutMsDefault, Constants.Audio.SongVolumeDefault, 0.0, battleSong)) battle
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
                            let alliesHealthy = Battle.getAlliesHealthy battle
                            let alliesWounded = Battle.getAlliesWounded battle
                            let enemiesHealthy = Battle.getEnemiesHealthy battle
                            let enemiesWounded = Battle.getEnemiesWounded battle
                            Character.autoBattle character alliesHealthy alliesWounded enemiesHealthy enemiesWounded
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

        and tickBattleResults time startTime outcome (battle : Battle) =
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
                        then ([cmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.GrowthSound))], battle)
                        else ([], battle)
                    else ([], battle)
                (cmd (FadeOutSong 6000) :: sigs, battle)
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
            [battle.UpdateEvent => msg Update
             battle.PostUpdateEvent => cmd UpdateEye]

        override this.Message (battle, message, _, world) =

            match message with
            | Update ->

                // tick
                let (signals, battle) = 
                    if World.isTicking world
                    then tick (World.getTickTime world) battle
                    else just battle

                // update dialog
                let battle =
                    match battle.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.update dialog world
                        Battle.updateDialogOpt (constant (Some dialog)) battle
                    | None -> battle

                // fin
                (signals, battle)

            | InteractDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    match Dialog.tryAdvance id dialog with // TODO: P1: pass in a real detokenizer!
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
                let consumableType =
                    scvalue<ConsumableType> item
                let aimType =
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumableData) -> consumableData.AimType
                    | (false, _) -> NoAim
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant (AimReticles (item, aimType))))
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
                let techType =
                    scvalue<TechType> item
                let aimType =
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, techData) -> techData.AimType
                    | (false, _) -> NoAim
                let battle =
                    Battle.updateCharacter
                        (Character.updateInputState (constant (AimReticles (item, aimType))))
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
                let battle = Battle.updateCharactersHealthy (Character.animate time ReadyAnimation) battle
                if timeLocal = 30L
                then withCmd (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UnsheatheSound)) battle
                else just battle

            | PoiseCharacters ->
                let time = World.getTickTime world
                let battle =
                    Battle.updateCharactersHealthy
                        (fun character ->
                            let poiseType = Character.getPoiseType character
                            let character = Character.animate time (PoiseAnimation poiseType) character
                            character)
                        battle
                just battle

            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let battle =
                    if outcome
                    then Battle.updateAlliesIf (fun _ ally -> ally.IsHealthy) (Character.animate time CelebrateAnimation) battle
                    else Battle.updateEnemiesIf (fun _ enemy -> enemy.IsHealthy) (Character.animate time CelebrateAnimation) battle
                just battle

            | AttackCharacter1 sourceIndex ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                let playHit = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                withCmd playHit battle

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let source = Battle.getCharacter sourceIndex battle
                let target = Battle.getCharacter targetIndex battle
                let alliesHealthy = Battle.getAlliesHealthy battle
                let damage = Character.getAttackResult Physical source target
                let battle = Battle.updateHitPoints targetIndex false false -damage battle
                let (battle, sigs) =
                    if target.HitPoints <= 0
                    then (battle, [Message (ResetCharacter targetIndex)]) // wounded
                    else (Battle.updateCharacter (Character.animate time DamageAnimation) targetIndex battle, []) // just damaged
                let sigs = Command (DisplayHitPointsChange (targetIndex, -damage)) :: sigs
                withSigs sigs battle

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time CastAnimation) sourceIndex battle
                let battle = Battle.updateInventory (Inventory.tryRemoveItem (Consumable consumable) >> snd) battle
                just battle

            | ConsumeCharacter2 (consumableType, targetIndex) ->
                let time = World.getTickTime world
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumableData) ->
                    if consumableData.Curative then
                        let healing = int consumableData.Scalar
                        let battle =
                            if consumableData.Techative
                            then Battle.updateTechPoints targetIndex healing battle
                            else Battle.updateHitPoints targetIndex false consumableData.Revive healing battle
                        let battle = Battle.updateCharacter (Character.updateStatuses (fun statuses -> Map.removeMany consumableData.StatusesRemoved statuses)) targetIndex battle
                        let battle = Battle.updateCharacter (Character.updateStatuses (fun statuses -> Map.addMany (Seq.map (flip pair Constants.Battle.BurndownTime) consumableData.StatusesAdded) statuses)) targetIndex battle
                        let battle = Battle.updateCharacter (Character.animate time SpinAnimation) targetIndex battle
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
                let effectOpt =
                    match techType with
                    | Critical | DarkCritical | PowerCut | SneakCut | DoubleCut | ProvokeCut ->
                        let hopDirection = Direction.ofVector2 (target.Bottom - source.Bottom)
                        let hopStop = target.Bottom - Direction.toVector2 hopDirection * Constants.Battle.StrikingDistance
                        Left (DisplayHop { HopStart = source.Bottom; HopStop = hopStop })
                    | Cyclone ->
                        Left (DisplayHop { HopStart = source.Bottom; HopStop = target.BottomOffset2 })
                    | _ ->
                        match source.ArchetypeType with
                        | Cleric ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeHolySound)
                            let displayCast = DisplayHolyCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                        | Wizard ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayArcaneCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                        | _ ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayDimensionalCast (0L, sourceIndex)
                            Right [cmd playCharge; cmd displayCast]
                match effectOpt with
                | Left hopEffect ->
                    withCmd hopEffect battle
                | Right chargeEffects ->
                    if target.IsWounded then
                        let battle = Battle.updateCurrentCommandOpt (constant None) battle
                        withMsgs [ResetCharacter sourceIndex; PoiseCharacter sourceIndex] battle
                    else withSigs (msg (ChargeCharacter sourceIndex) :: chargeEffects) battle

            | TechCharacter2 (sourceIndex, targetIndex, techType) ->
                match techType with
                | Critical ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
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
                    let battle = Battle.updateCharacter (Character.animate time WhirlAnimation) sourceIndex battle
                    withCmds (DisplayCircle (position, radius) :: DisplayCycloneBlur (0L, sourceIndex, radius) :: playHits) battle
                | DarkCritical ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex) // TODO: darker impact splash to represent element.
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                    withCmds [playHit; impactSplash] battle
                | Slash ->
                    let time = World.getTickTime world
                    let playSlash = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.SlashSound)
                    let playHit = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let slashSpike = DisplaySlashSpike (10L, (Battle.getCharacter sourceIndex battle).Bottom, targetIndex)
                    let impactSplashes = Battle.evaluateTechMove sourceIndex targetIndex techType battle |> snd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayImpactSplash (70L, targetIndex))
                    let battle = Battle.updateCharacter (Character.animate time SlashAnimation) sourceIndex battle
                    withCmds (playSlash :: playHit :: slashSpike :: impactSplashes) battle
                | PowerCut ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                    withCmds [playHit; cut] battle
                | SneakCut ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                    withCmds [playHit; cut] battle
                | DoubleCut ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                    withCmds [playHit; cut] battle
                | ProvokeCut ->
                    let time = World.getTickTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let displayCut = DisplayCut (30L, true, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time AttackAnimation) sourceIndex battle
                    withCmds [playHit; displayCut] battle
                | Fire ->
                    let time = World.getTickTime world
                    let playFire = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.FireSound)
                    let displayFire = DisplayFire (0L, sourceIndex, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmds [playFire; displayFire] battle
                | TechType.Flame ->
                    let time = World.getTickTime world
                    let playFlame = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.FlameSound)
                    let displayFlame = DisplayFlame (0L, sourceIndex, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmds [playFlame; displayFlame] battle
                | Ice ->
                    let time = World.getTickTime world
                    let playIce = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.IceSound)
                    let displayIce = DisplayIce (0L, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmds [playIce; displayIce] battle
                | Snowball ->
                    let time = World.getTickTime world
                    let playSnowball = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.SnowballSound)
                    let displaySnowball = DisplaySnowball (0L, targetIndex)
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmds [playSnowball; displaySnowball] battle
                | Bolt ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use sound.
                | BoltBeam ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Stone ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmd (DisplayIce (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Quake ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmd (DisplayBolt (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Aura ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playAura = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.AuraSound)
                    let displayAuras = Battle.evaluateTechMove sourceIndex targetIndex techType battle |> snd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayAura (0L, targetIndex))
                    withCmds (playAura :: displayAuras) battle
                | Empower ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playAura = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.AuraSound) // TODO: use new sound and effect.
                    let displayAura = DisplayAura (0L, targetIndex)
                    withCmds [playAura; displayAura] battle
                | Enlighten ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playAura = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.AuraSound) // TODO: use new sound and effect.
                    let displayAura = DisplayAura (0L, targetIndex)
                    withCmds [playAura; displayAura] battle
                | Protect ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playAura = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.AuraSound) // TODO: use new sound.
                    let displayProtect = DisplayProtect (0L, targetIndex)
                    withCmds [playAura; displayProtect] battle
                | Weaken ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Power (false, false), targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | Muddle ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Magic (false, false), targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | ConjureIfrit ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playIfrit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.IfritSound)
                    let displayConjureIfrit = DisplayConjureIfrit 0L
                    withCmds [playIfrit; displayConjureIfrit] battle
                | Slow ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Time false, targetIndex)
                    withCmds [playDebuff; displayDebuff] battle
                | Purify ->
                    let time = World.getTickTime world
                    let battle = Battle.updateCharacter (Character.animate time Cast2Animation) sourceIndex battle
                    withCmd (DisplayPurify (0L, targetIndex)) battle // TODO: use new sound and effect.

            | TechCharacter3 (sourceIndex, targetIndex, techType) ->
                let time = World.getTickTime world
                let results = Battle.evaluateTechMove sourceIndex targetIndex techType battle |> snd
                let (battle, cmds) =
                    Map.fold (fun (battle, cmds) characterIndex (cancelled, _, hitPointsChange, _, _) ->
                        if hitPointsChange < 0 && Battle.getCharacterHealthy characterIndex battle then
                            let battle = Battle.updateCharacter (Character.animate time DamageAnimation) characterIndex battle
                            let cmds = if cancelled then DisplayCancel characterIndex :: cmds else cmds
                            (battle, cmds)
                        else (battle, cmds))
                        (battle, [])
                        results
                withCmds cmds battle

            | TechCharacter4 (sourceIndex, targetIndex, techType) ->
                let results = Battle.evaluateTechMove sourceIndex targetIndex techType battle |> snd 
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) _ (_, _, _, _, _) ->
                        // TODO: glow effect
                        (battle, sigs))
                        (battle, [])
                        results
                withSigs sigs battle

            | TechCharacter5 (sourceIndex, targetIndex, techType) ->
                let source = Battle.getCharacter sourceIndex battle
                let target = Battle.getCharacter targetIndex battle
                let hopOpt =
                    match techType with
                    | Critical | DarkCritical | PowerCut | SneakCut | DoubleCut | ProvokeCut ->
                        let hopDirection = Direction.ofVector2 (target.Bottom - source.BottomOriginal)
                        let hopStart = target.Bottom - Direction.toVector2 hopDirection * Constants.Battle.StrikingDistance
                        Some { HopStart = hopStart; HopStop = source.BottomOriginal }
                    | Cyclone -> Some { HopStart = target.BottomOffset2; HopStop = source.BottomOriginal }
                    | _ -> None
                match hopOpt with
                | Some hop -> withCmd (DisplayHop hop) battle
                | None -> just battle

            | TechCharacter6 (sourceIndex, targetIndex, techType) ->
                let (techCost, results) = Battle.evaluateTechMove sourceIndex targetIndex techType battle
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) characterIndex (cancelled, affectsWounded, hitPointsChange, added, removed) ->
                        let battle = Battle.updateHitPoints characterIndex cancelled affectsWounded hitPointsChange battle
                        let battle = Battle.applyStatusChanges characterIndex added removed battle
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
                let battle = Battle.updateTechPoints sourceIndex -techCost battle
                let battle =
                    if Battle.shouldCounter targetIndex battle
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
                        | Some tag -> Battle.updateCharacter (Character.updateBottom (constant tag.Position)) sourceIndex battle
                        | None -> battle
                    just battle
                else just battle

            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let battle = Battle.updateCharacter (Character.animate time (PoiseAnimation Charging)) sourceIndex battle
                just battle

            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let battle =
                    Battle.updateCharacter
                        (fun character ->
                            let poiseType = Character.getPoiseType character
                            let character = Character.animate time (PoiseAnimation poiseType) character
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
                            let character = Character.animate time WoundAnimation character
                            character)
                        characterIndex
                        battle
                just battle

            | ResetCharacter characterIndex ->
                let character = Battle.getCharacter characterIndex battle
                let battle = Battle.updateCharacter (Character.updateActionTime (constant 0.0f)) characterIndex battle
                let battle =
                    if character.IsAlly
                    then Battle.updateCharacter (Character.updateInputState (constant NoInput)) characterIndex battle
                    else Battle.updateCharacter (Character.updateAutoBattleOpt (constant None)) characterIndex battle
                just battle

            | DestroyCharacter characterIndex ->
                let character = Battle.getCharacter characterIndex battle
                let battle = if character.IsEnemy then Battle.removeCharacter characterIndex battle else battle
                just battle

            | Nop -> just battle

        override this.Command (battle, command, _, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyeCenter v2Zero world
                just world
            
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

            | DisplayCancel targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeCancelEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset4 world
                    let world = entity.SetElevation (Constants.Battle.GuiEffectElevation + 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Battle.Scene.Group world
                    let world = entity.SetEffect effect world
                    let world = entity.SetCenter target.CenterOffset3 world
                    let world = entity.SetElevation Constants.Battle.GuiEffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 192.0f 758.0f) (Bottom target.Bottom) (Effects.makeBoltEffect ()) world |> just
                | None -> just world

            | DisplayCycloneBlur (delay, targetIndex, radius) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 234.0f 234.0f) (Center target.Center) (Effects.makeCycloneBlurEffect radius) world |> just
                | None -> just world

            | DisplayImpactSplash (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 192.0f 96.0f) (Bottom target.Bottom) (Effects.makeImpactSplashEffect ()) world |> just
                | None -> just world

            | DisplayCut (delay, light, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 48.0f 144.0f) (Bottom target.Bottom) (Effects.makeCutEffect light) world |> just
                | None -> just world
            
            | DisplaySlashSpike (delay, bottom, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let projection = Vector2.Normalize (target.Bottom - bottom) * single Constants.Render.VirtualResolutionX + target.Bottom
                    let effect = (Effects.makeSlashSpikeEffect bottom projection)
                    let world = displayEffect delay (v2 96.0f 96.0f) (Bottom bottom) effect world
                    just world
                | None -> just world

            | DisplayArcaneCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v2 300.0f 300.0f) (Bottom (source.Bottom - v2 0.0f 120.0f)) (Effects.makeArcaneCastEffect ()) world |> just
                | None -> just world
            
            | DisplayFire (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFireEffect (source.Bottom + (v2 80.0f 80.0f)) (target.Bottom + (v2 0.0f 20.0f))
                        let world = displayEffect delay (v2 100.0f 100.0f) (Bottom (source.Bottom - v2 0.0f 50.0f)) effect world
                        just world
                    | None -> just world
                | None -> just world

            | DisplayFlame (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFlameEffect source.CenterOffset target.CenterOffset
                        let world = displayEffect delay (v2 144.0f 144.0f) (Bottom source.Bottom) effect world
                        just world
                    | None -> just world
                | None -> just world
            
            | DisplayIce (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 48.0f 48.0f) (Bottom target.Bottom) (Effects.makeIceEffect ()) world |> just
                | None -> just world
            
            | DisplaySnowball (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 432.0f 432.0f) (Bottom target.Bottom) (Effects.makeSnowballEffect ()) world |> just
                | None -> just world

            | DisplayHolyCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v2 300.0f 300.0f) (Bottom (source.Bottom - v2 0.0f 100.0f)) (Effects.makeHolyCastEffect ()) world |> just
                | None -> just world
            
            | DisplayPurify (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 192.0f 192.0f) (Bottom (target.Bottom - v2 0.0f 100.0f)) (Effects.makePurifyEffect ()) world |> just
                | None -> just world

            | DisplayAura (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 48.0f 48.0f) (Bottom target.Bottom) (Effects.makeAuraEffect ()) world |> just
                | None -> just world

            | DisplayProtect (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 48.0f 48.0f) (Bottom target.Bottom) (Effects.makeProtectEffect ()) world |> just
                | None -> just world

            | DisplayDimensionalCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v2 48.0f 48.0f) (Bottom source.Bottom) (Effects.makeDimensionalCastEffect ()) world |> just
                | None -> just world

            | DisplayDebuff (delay, powerDown, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v2 48.0f 48.0f) (Bottom target.Bottom) (Effects.makeDebuffEffect powerDown) world |> just
                | None -> just world

            | DisplayConjureIfrit delay ->
                displayEffect delay (v2 48.0f 48.0f) (Position (v2 0.0f 0.0f)) (Effects.makeConjureIfritEffect ()) world |> just

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | PlaySong (fadeIn, fadeOut, volume, start, assetTag) ->
                let world = World.playSong fadeIn fadeOut volume start assetTag world
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
                     Entity.TileMap <== battle --> fun battle -> battle.TileMap
                     Entity.TileIndexOffset <== battle --> fun battle -> battle.TileIndexOffset]

                 // dialog
                 Dialog.content Gen.name
                    (Constants.Battle.GuiElevation + 2.0f) Nop Nop
                    (battle --> fun battle -> (id, battle.DialogOpt)) // TODO: P1: pass in a real detokenizer!

                 // dialog interact button
                 Content.button Gen.name
                    [Entity.Position == v2 248.0f -240.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v2 144.0f 48.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible <== battle --> fun battle -> match battle.DialogOpt with Some dialog -> Dialog.canAdvance id dialog | None -> false // TODO: P1: pass in a real detokenizer!
                     Entity.Text == "Next"
                     Entity.ClickEvent ==> msg InteractDialog]

                 // allies
                 Content.entities battle
                    (fun battle _ -> Battle.getAllies battle) constant
                    (fun index battle _ -> Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character <== battle])

                 // enemies
                 Content.entities battle
                    (fun battle _ -> Battle.getEnemies battle) constant
                    (fun index battle _ -> Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character <== battle])]

             // input groups
             Content.groups battle (fun battle _ -> if battle.Running then Battle.getAllies battle else Map.empty) constant $ fun index ally _ ->

                // input group
                let inputName = "Input" + "+" + CharacterIndex.toEntityName index
                Content.group inputName [Group.Visible <== ally --> fun ally -> ally.IsHealthy]

                    [// health bar
                     Content.fillBar "HealthBar" 
                        [Entity.Size == v2 48.0f 6.0f
                         Entity.Center <== ally --> fun ally -> ally.BottomOffset
                         Entity.Elevation == Constants.Battle.GuiElevation
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
                         Entity.RingMenu <== ally --> fun ally ->
                            if ally.Direction = Leftward
                            then { Items = Map.ofList [("Attack", (0, true)); ("Tech", (1, true)); ("Consumable", (2, true)); ("Defend", (3, true))]; ItemCancelOpt = None }
                            else { Items = Map.ofList [("Attack", (0, true)); ("Defend", (1, true)); ("Consumable", (2, true)); ("Tech", (3, true))]; ItemCancelOpt = None }
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
                                Map.ofSeqBy (fun kvp -> (scstringm kvp.Key, (getTag kvp.Key, true)))
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
                                    let techUsable =
                                        match Map.tryFind tech Data.Value.Techs with
                                        | Some techData -> techData.TechCost <= ally.TechPoints
                                        | None -> false
                                    (scstringm tech, (getTag tech, techUsable)))
                            { Items = techs; ItemCancelOpt = Some "Cancel" }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (TechItemSelect (index, evt.Data))
                         Entity.CancelEvent ==> msg (TechItemCancel index)]

                     // reticles
                     Content.entity<ReticlesDispatcher> "Reticles"
                        [Entity.Elevation == Constants.Battle.GuiElevation
                         Entity.Visible <== ally --> fun ally -> match ally.InputState with AimReticles _ -> true | _ -> false
                         Entity.Reticles <== battle --> fun battle ->
                            let aimType =
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
                            reticles
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, index))
                         Entity.CancelEvent ==> msg (ReticlesCancel index)]]]