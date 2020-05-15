namespace OmniBlade
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniBattle =

    type Hop =
        { HopStart : Vector2
          HopStop : Vector2 }

    type BattleMessage =
        | RegularItemSelect of CharacterIndex * string
        | RegularItemCancel of CharacterIndex
        | ConsumableItemSelect of CharacterIndex * string
        | ConsumableItemCancel of CharacterIndex
        | TechItemSelect of CharacterIndex * string
        | TechItemCancel of CharacterIndex
        | ReticlesSelect of CharacterIndex * CharacterIndex
        | ReticlesCancel of CharacterIndex
        | ReadyCharacters
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
        | Tick

    type BattleCommand =
        | PlaySound of int64 * single * AssetTag<Sound>
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of CharacterIndex
        | DisplayHop of Hop
        | DisplayCircle of Vector2 * single
        | InitializeBattle
        | FinalizeBattle

    type Screen with

        member this.GetBattleModel = this.GetModel<BattleModel>
        member this.SetBattleModel = this.SetModel<BattleModel>
        member this.BattleModel = this.Model<BattleModel> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<BattleModel, BattleMessage, BattleCommand> (BattleModel.empty)

        static let tickAttack sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.IsHealthy
                    then withMsg model (AttackCharacter1 sourceIndex)
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | 15L ->
                    withMsg model (AttackCharacter2 (sourceIndex, targetIndex))
                | _ when CharacterModel.getAnimationFinished time target ->
                    let target = BattleModel.getCharacter targetIndex model
                    if target.IsHealthy then
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                    else
                        let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                        let model = BattleModel.updateCurrentCommandOpt (constant (Some woundCommand)) model
                        withMsg model (PoiseCharacter sourceIndex)
                | _ -> just model
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                let target = BattleModel.getCharacter targetIndex model
                match timeLocal with
                | 0L ->
                    if target.IsHealthy
                    then withMsg model (ConsumeCharacter1 (consumable, sourceIndex))
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | 30L ->
                    withMsg model (ConsumeCharacter2 (consumable, targetIndex))
                | _ when CharacterModel.getAnimationFinished time target ->
                    let model = BattleModel.updateCurrentCommandOpt (constant None) model
                    withMsgs model [PoiseCharacter sourceIndex; PoiseCharacter targetIndex]
                | _ -> just model
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let tickTech techType sourceIndex (targetIndexOpt : CharacterIndex option) (_ : int64) timeLocal model =
            match targetIndexOpt with
            | Some targetIndex ->
                match Map.tryFind techType data.Value.TechAnimations with
                | Some techData ->
                    let (model, msgs) =
                        if timeLocal = techData.TechStart then (model, [TechCharacter1 (sourceIndex, targetIndex, techType)])
                        elif timeLocal = techData.TechingStart then (model, [TechCharacter2 (sourceIndex, targetIndex, techType)])
                        elif timeLocal = techData.AffectingStart then (model, [TechCharacter3 (sourceIndex, targetIndex, techType)])
                        elif timeLocal = techData.AffectingStop then (model, [TechCharacter4 (sourceIndex, targetIndex, techType)])
                        elif timeLocal = techData.TechingStop then (model, [TechCharacter5 (sourceIndex, targetIndex, techType)])
                        elif timeLocal = techData.TechStop then (model, [TechCharacter6 (sourceIndex, targetIndex, techType)])
                        else (model, [])
                    let (model, msgs) = (model, msgs @ [TechCharacterAmbient (sourceIndex, targetIndex, techType)])
                    withMsgs model msgs
                | None ->
                    let model = BattleModel.updateCurrentCommandOpt (constant None) model
                    withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
            | None ->
                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]

        static let rec tickWound targetIndexOpt time model =
            match targetIndexOpt with
            | Some targetIndex ->
                let character = BattleModel.getCharacter targetIndex model
                let (model, sigs) =
                    if character.IsAlly then
                        match character.AnimationCycle with
                        | DamageCycle ->
                            if CharacterModel.getAnimationFinished time character then
                                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                                (model, [Message (WoundCharacter targetIndex)])
                            else (model, [])
                        | _ -> failwithumf ()
                    else
                        match character.AnimationCycle with
                        | DamageCycle ->
                            if CharacterModel.getAnimationFinished time character then
                                let woundCharacter = WoundCharacter targetIndex
                                let playDeathSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.DeathSound)
                                (model, [Message woundCharacter; Command playDeathSound])
                            else (model, [])
                        | WoundCycle ->
                            if CharacterModel.getAnimationFinished time character then
                                let model = BattleModel.updateCurrentCommandOpt (constant None) model
                                (model, [Message (DestroyCharacter targetIndex)])
                            else (model, [])
                        | _ -> failwithumf ()
                let (model, sigs) =
                    match model.CurrentCommandOpt with
                    | None ->
                        let allies = BattleModel.getAllies model
                        let enemies = BattleModel.getEnemies model
                        if List.forall (fun (character : CharacterModel) -> character.IsWounded) allies then
                            let model = BattleModel.updateBattleState (constant (BattleCease (false, time))) model
                            let (model, sigs2) = tick time model
                            (model, sigs @ sigs2)
                        elif
                            List.forall (fun (character : CharacterModel) -> character.IsWounded) enemies &&
                            List.hasAtMost 1 enemies then
                            let model = BattleModel.updateBattleState (constant (BattleCease (true, time))) model
                            let (model, sigs2) = tick time model
                            (model, sigs @ sigs2)
                        else (model, sigs)
                    | Some _ -> (model, sigs)
                withSigs model sigs
            | None -> just model

        and tickReady time timeStart model =
            let timeLocal = time - timeStart
            if timeLocal = 0L then withMsg model ReadyCharacters
            elif timeLocal >= 30L then
                let model = BattleModel.updateBattleState (constant BattleRunning) model
                withMsg model PoiseCharacters
            else just model

        and tickCurrentCommand time currentCommand model =
            let timeLocal = time - currentCommand.TimeStart
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickAttack source targetOpt time timeLocal model
            | Tech techType ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickTech techType source targetOpt time timeLocal model
            | Consume consumable ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickConsume consumable source targetOpt time timeLocal model
            | Wound ->
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                tickWound targetOpt time model

        and tickNextCommand time nextCommand futureCommands (model : BattleModel) =
            let command = CurrentCommand.make time nextCommand
            let model = BattleModel.updateCurrentCommandOpt (constant (Some command)) model
            let model = BattleModel.updateActionCommands (constant futureCommands) model
            tick time model

        and tickNoNextCommand time (model : BattleModel) =
            let (allySignalsRev, model) =
                List.fold (fun (signals, model) (ally : CharacterModel) ->
                    if ally.ActionTime = Constants.Battle.ActionTime then
                        let model =
                            BattleModel.updateCharacter
                                (fun character ->
                                    let character = CharacterModel.updateInputState (constant RegularMenu) character
                                    let character = CharacterModel.animate time (PoiseCycle Poising) character
                                    character)
                                ally.CharacterIndex
                                model
                        let playActionTimeSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.AffirmSound)
                        (Command playActionTimeSound :: signals, model)
                    else (signals, model))
                    ([], model)
                    (BattleModel.getAllies model)
            let (enemySignalsRev, model) =
                List.fold (fun (signals, model) (enemy : CharacterModel) ->
                    if enemy.ActionTime = Constants.Battle.ActionTime then
                        let enemyIndex = enemy.CharacterIndex
                        match enemy.AutoBattleOpt with
                        | Some autoBattle ->
                            let actionCommand =
                                match autoBattle.AutoTechOpt with
                                | Some tech -> { Action = Tech tech; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                | None -> { Action = Attack; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                            let model = BattleModel.appendActionCommand actionCommand model
                            (Message (ResetCharacter enemyIndex) :: signals, model)
                        | None -> (Message (ResetCharacter enemyIndex) :: signals, model)
                    else (signals, model))
                    ([], model)
                    (BattleModel.getEnemies model)
            let model =
                BattleModel.updateCharacters (fun character ->
                    let character =
                        CharacterModel.updateActionTime ((+) Constants.Battle.ActionTimeInc) character
                    let character =
                        if CharacterModel.isReadyForAutoBattle character then
                            let targetIndex = BattleModel.getAllyIndexRandom model
                            let target = BattleModel.getCharacter targetIndex model
                            CharacterModel.autoBattle character target
                        else character
                    character)
                    model
            withSigs model (List.rev (allySignalsRev @ enemySignalsRev))

        and tickNoCurrentCommand time (model : BattleModel) =
            match model.ActionCommands with
            | Queue.Cons (nextCommand, futureCommands) -> tickNextCommand time nextCommand futureCommands model
            | Queue.Nil -> tickNoNextCommand time model

        and tickRunning time (model : BattleModel) =
            match model.CurrentCommandOpt with
            | Some currentCommand -> tickCurrentCommand time currentCommand model
            | None -> tickNoCurrentCommand time model

        and tickCease time timeStart outcome model =
            let timeLocal = time - timeStart
            match timeLocal with
            | 0L -> withMsg model (CelebrateCharacters outcome)
            | _ -> just model

        and tick time model =
            let (model, sigs) =
                match model.BattleState with
                | BattleReady timeStart -> tickReady time timeStart model
                | BattleRunning -> tickRunning time model
                | BattleCease (outcome, timeStart) -> tickCease time timeStart outcome model
            (model, sigs)

        override this.Channel (_, battle, _) =
            [battle.SelectEvent => [cmd InitializeBattle]
             battle.DeselectEvent => [cmd FinalizeBattle]
             battle.UpdateEvent => [msg Tick]]

        override this.Message (model, message, _, world) =

            match message with
            | RegularItemSelect (characterIndex, item) ->
                let model =
                    match item with
                    | "Attack" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant (AimReticles (item, EnemyAim true))))
                            characterIndex
                            model
                    | "Defend" ->
                        BattleModel.updateCharacter
                            (fun character ->
                                let time = World.getTickTime world
                                let character = CharacterModel.updateActionTime (constant 0) character
                                let character = CharacterModel.updateInputState (constant NoInput) character
                                let character = CharacterModel.defend character
                                let character = CharacterModel.animate time (PoiseCycle Defending) character
                                character)
                            characterIndex
                            model
                    | "Consumable" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant ItemMenu))
                            characterIndex
                            model
                    | "Tech" ->
                        BattleModel.updateCharacter
                            (CharacterModel.updateInputState (constant TechMenu))
                            characterIndex
                            model
                    | _ -> failwithumf ()
                just model
            
            | RegularItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model
            
            | ConsumableItemSelect (characterIndex, item) ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant (AimReticles (item, AllyAim true))))
                        characterIndex
                        model
                just model

            | ConsumableItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model
            
            | TechItemSelect (characterIndex, item) ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant (AimReticles (item, EnemyAim true))))
                        characterIndex
                        model
                just model
            
            | TechItemCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model

            | ReticlesSelect (targetIndex, allyIndex) ->
                match model.BattleState with
                | BattleRunning ->
                    let ally = BattleModel.getCharacter allyIndex model
                    match ally.InputState with
                    | AimReticles (item, _) ->
                        let command =
                            match item with
                            | "GreenHerb" -> ActionCommand.make (Consume GreenHerb) allyIndex (Some targetIndex)
                            | "RedHerb" -> ActionCommand.make (Consume RedHerb) allyIndex (Some targetIndex)
                            | "Critical" -> ActionCommand.make (Tech Critical) allyIndex (Some targetIndex)
                            | "Cyclone" -> ActionCommand.make (Tech Cyclone) allyIndex (Some targetIndex)
                            | "Bolt" -> ActionCommand.make (Tech Bolt) allyIndex (Some targetIndex)
                            | "Tremor" -> ActionCommand.make (Tech Tremor) allyIndex (Some targetIndex)
                            | _ -> ActionCommand.make Attack allyIndex (Some targetIndex)
                        let model = BattleModel.appendActionCommand command model
                        withMsg model (ResetCharacter allyIndex)
                    | _ -> just model
                | _ -> just model

            | ReticlesCancel characterIndex ->
                let model =
                    BattleModel.updateCharacter
                        (CharacterModel.updateInputState (constant RegularMenu))
                        characterIndex
                        model
                just model

            | ReadyCharacters ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacters
                        (CharacterModel.animate time ReadyCycle)
                        model
                just model

            | PoiseCharacters ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacters
                        (fun character ->
                            let poiseType = CharacterModel.getPoiseType character
                            let character = CharacterModel.animate time (PoiseCycle poiseType) character
                            character)
                        model
                just model

            | CelebrateCharacters outcome ->
                let time = World.getTickTime world
                let model =
                    if outcome
                    then BattleModel.updateAllies (CharacterModel.animate time CelebrateCycle) model
                    else BattleModel.updateEnemies (CharacterModel.animate time CelebrateCycle) model
                just model

            | AttackCharacter1 sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time AttackCycle) sourceIndex model
                let playHitSoundDelay = 15L
                let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                withCmd model playHitSound

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getTickTime world
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let damage = CharacterModel.getAttackResult Physical source target
                let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints (fun hitPoints -> (hitPoints - damage, false))) targetIndex model
                let (model, sigs) =
                    if target.HitPoints <= 0
                    then (model, [Message (ResetCharacter targetIndex)]) // wounded
                    else (BattleModel.updateCharacter (CharacterModel.animate time DamageCycle) targetIndex model, []) // just damaged
                let sigs = Command (DisplayHitPointsChange (targetIndex, -damage)) :: sigs
                withSigs model sigs

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time CastCycle) sourceIndex model
                let item = Consumable consumable
                let model = BattleModel.updateInventory (Inventory.removeItem item) model
                just model

            | ConsumeCharacter2 (consumable, targetIndex) ->
                let time = World.getTickTime world
                let healing =
                    match consumable with
                    | GreenHerb -> 50 // TODO: pull from data
                    | RedHerb -> 500 // TODO: pull from data
                let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints (fun hitPoints -> (hitPoints + healing, false))) targetIndex model
                let model = BattleModel.updateCharacter (CharacterModel.animate time SpinCycle) targetIndex model
                let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                let playHealSound = PlaySound (0L, Constants.Audio.DefaultSoundVolume, Assets.HealSound)
                withCmds model [displayHitPointsChange; playHealSound]
            
            | TechCharacter1 (sourceIndex, targetIndex, techType) ->
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let hopOpt =
                    match techType with
                    | Critical  | Cyclone -> Some { HopStart = source.Bottom; HopStop = target.BottomOffset2 }
                    | Bolt | Tremor -> None
                match hopOpt with
                | None ->
                    if target.IsHealthy
                    then withMsg model (ChargeCharacter sourceIndex)
                    else
                        let model = BattleModel.updateCurrentCommandOpt (constant None) model
                        withMsgs model [ResetCharacter sourceIndex; PoiseCharacter sourceIndex]
                | Some hop ->
                    withCmd model (DisplayHop hop)

            | TechCharacter2 (sourceIndex, targetIndex, techType) ->
                match techType with
                | Critical ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time AttackCycle) sourceIndex model
                    let playHitSoundDelay = 10L
                    let playHitSound = PlaySound (playHitSoundDelay, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                    withCmd model playHitSound
                | Cyclone ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time WhirlCycle) sourceIndex model
                    let playHitSounds =
                        [PlaySound (20L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (40L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (60L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)
                         PlaySound (80L, Constants.Audio.DefaultSoundVolume, Assets.HitSound)]
                    withCmds model (DisplayCircle ((BattleModel.getCharacter sourceIndex model).Bottom, 64.0f) :: playHitSounds)
                | Bolt ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time Cast2Cycle) sourceIndex model
                    withCmd model (DisplayBolt targetIndex)
                | Tremor ->
                    let time = World.getTickTime world
                    let model = BattleModel.updateCharacter (CharacterModel.animate time BuryCycle) sourceIndex model
                    withCmd model (DisplayBolt targetIndex)

            | TechCharacter3 (sourceIndex, targetIndex, techType) ->
                let time = World.getTickTime world
                match Map.tryFind techType data.Value.Techs with
                | Some techData ->
                    let source = BattleModel.getCharacter sourceIndex model
                    let target = BattleModel.getCharacter targetIndex model
                    let characters = BattleModel.getCharacters model
                    let results = CharacterModel.evaluateTechMove techData source target characters
                    let (model, cmds) =
                        List.fold (fun (model, cmds) (cancelled, hitPointsChange, characterIndex) ->
                            let character = BattleModel.getCharacter characterIndex model
                            if hitPointsChange < 0 && character.IsHealthy then
                                let model = BattleModel.updateCharacter (CharacterModel.animate time DamageCycle) characterIndex model
                                let cmds = if cancelled then DisplayCancel characterIndex :: cmds else cmds
                                (model, cmds)
                            else (model, cmds))
                            (model, [])
                            results
                    withCmds model cmds
                | None -> just model

            | TechCharacter4 (sourceIndex, targetIndex, techType) ->
                match Map.tryFind techType data.Value.Techs with
                | Some techData ->
                    let source = BattleModel.getCharacter sourceIndex model
                    let target = BattleModel.getCharacter targetIndex model
                    let characters = BattleModel.getCharacters model
                    let results = CharacterModel.evaluateTechMove techData source target characters
                    let (model, sigs) =
                        List.fold (fun (model, sigs) (_, _, _) ->
                            // TODO: glow effect
                            (model, sigs))
                            (model, [])
                            results
                    withSigs model sigs
                | None -> just model

            | TechCharacter5 (sourceIndex, targetIndex, techType) ->
                let source = BattleModel.getCharacter sourceIndex model
                let target = BattleModel.getCharacter targetIndex model
                let hopOpt =
                    match techType with
                    | Critical | Cyclone -> Some { HopStart = target.BottomOffset2; HopStop = source.BottomOriginal }
                    | Bolt | Tremor -> None
                match hopOpt with
                | None -> just model
                | Some hop -> withCmd model (DisplayHop hop)

            | TechCharacter6 (sourceIndex, targetIndex, techType) ->
                match Map.tryFind techType data.Value.Techs with
                | Some techData ->
                    let source = BattleModel.getCharacter sourceIndex model
                    let target = BattleModel.getCharacter targetIndex model
                    let characters = BattleModel.getCharacters model
                    let results = CharacterModel.evaluateTechMove techData source target characters
                    let (model, sigs) =
                        List.fold (fun (model, sigs) (cancelled, hitPointsChange, characterIndex) ->
                            let model = BattleModel.updateCharacter (CharacterModel.updateTechPoints ((+) -techData.TechCost)) sourceIndex model
                            let model = BattleModel.updateCharacter (CharacterModel.updateHitPoints (fun hitPoints -> (hitPoints + hitPointsChange, cancelled))) characterIndex model
                            let wounded = (BattleModel.getCharacter characterIndex model).IsWounded
                            let sigs = if wounded then Message (ResetCharacter characterIndex) :: sigs else sigs
                            let sigs = if hitPointsChange <> 0 then Command (DisplayHitPointsChange (characterIndex, hitPointsChange)) :: sigs else sigs
                            let (model, sigs) =
                                if wounded then
                                    let woundCommand = ActionCommand.make Wound sourceIndex (Some characterIndex)
                                    let model = BattleModel.prependActionCommand woundCommand model
                                    (model, sigs)
                                else
                                    let sigs = Message (PoiseCharacter characterIndex) :: sigs
                                    (model, sigs)
                            (model, sigs))
                            (model, [])
                            results
                    let model = BattleModel.updateCurrentCommandOpt (constant None) model
                    let sigs = Message (PoiseCharacter sourceIndex) :: sigs
                    withSigs model sigs
                | None -> just model
                    
            | TechCharacterAmbient (sourceIndex, _, _) ->
                if Simulants.BattleRide.Exists world then
                    let model =
                        let tags = Simulants.BattleRide.GetEffectTags world
                        match Map.tryFind "Tag" tags with
                        | Some tag -> BattleModel.updateCharacter (CharacterModel.updateBottom (constant tag.Position)) sourceIndex model
                        | None -> model
                    just model
                else just model

            | ChargeCharacter sourceIndex ->
                let time = World.getTickTime world
                let model = BattleModel.updateCharacter (CharacterModel.animate time (PoiseCycle Charging)) sourceIndex model
                just model

            | PoiseCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let poiseType = CharacterModel.getPoiseType character
                            let character = CharacterModel.animate time (PoiseCycle poiseType) character
                            character)
                        characterIndex
                        model
                just model

            | WoundCharacter characterIndex ->
                let time = World.getTickTime world
                let model =
                    BattleModel.updateCharacter
                        (fun character ->
                            let character =
                                if character.IsAlly
                                then CharacterModel.updateInputState (constant NoInput) character
                                else character
                            let character = CharacterModel.animate time WoundCycle character
                            character)
                        characterIndex
                        model
                just model

            | ResetCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model = BattleModel.updateCharacter (CharacterModel.updateActionTime (constant 0)) characterIndex model
                let model =
                    if character.IsAlly
                    then BattleModel.updateCharacter (CharacterModel.updateInputState (constant NoInput)) characterIndex model
                    else BattleModel.updateCharacter (CharacterModel.updateAutoBattleOpt (constant None)) characterIndex model
                just model

            | DestroyCharacter characterIndex ->
                let character = BattleModel.getCharacter characterIndex model
                let model = if character.IsEnemy then BattleModel.removeCharacter characterIndex model else model
                just model

            | Tick ->
                if World.isTicking world
                then tick (World.getTickTime world) model
                else just model

        override this.Command (model, command, battle, world) =

            match command with
            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) (World.getTickTime world + delay) world
                just world

            | DisplayCancel targetIndex ->
                match BattleModel.tryGetCharacter targetIndex model with
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
                match BattleModel.tryGetCharacter targetIndex model with
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
                match BattleModel.tryGetCharacter targetIndex model with
                | Some target ->
                    let effect = Effects.makeBoltEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.BattleScene world
                    let world = entity.SetEffect effect world
                    let world = entity.SetSize (v2 256.0f 1024.0f) world
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

            | InitializeBattle ->
                let world = World.setEyeCenter v2Zero world
                let world = World.hintRenderPackageUse Assets.BattlePackageName world
                let world = World.hintAudioPackageUse Assets.BattlePackageName world
                let world = World.playSong 0 Constants.Audio.DefaultSongVolume Assets.BattleSong world
                let model = BattleModel.updateBattleState (constant (BattleReady (World.getTickTime world))) model
                let world = battle.SetBattleModel model world
                just world

            | FinalizeBattle ->
                let world = World.hintRenderPackageDisuse Assets.BattlePackageName world
                just (World.hintAudioPackageDisuse Assets.BattlePackageName world)

        member private this.SceneContent (model : Lens<BattleModel, World>, _ : Screen) =

            // scene layer
            let background = Simulants.BattleScene / "Background"
            Content.layer Simulants.BattleScene.Name []

                // background
                [Content.label background.Name
                    [background.Position == v2 -480.0f -512.0f
                     background.Size == v2 1024.0f 1024.0f
                     background.Depth == Constants.Battle.BackgroundDepth
                     background.LabelImage == asset "Battle" "Background"]

                 // allies
                 Content.entitiesIndexedBy
                    (model --> fun model -> BattleModel.getAllies model)
                    (fun model -> model.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Ally+" + scstring index) [Entity.CharacterModel <== model])

                 // enemies
                 Content.entitiesIndexedBy
                    (model --> fun model -> BattleModel.getEnemies model)
                    (fun model -> model.PartyIndex)
                    (fun index model _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.CharacterModel <== model])]

        member private this.InputContent (model : Lens<BattleModel, World>, screen : Screen) =

            // input layers
            Content.layers (model --> fun model -> BattleModel.getAllies model) $ fun index ally _ ->

                // input layer
                let allyIndex = AllyIndex index
                let input = screen / ("Input" + "+" + scstring index)
                Content.layer input.Name []

                    // health bar
                    [Content.fillBar "HealthBar" 
                        [Entity.Size == v2 64.0f 8.0f
                         Entity.Center <== ally --> fun ally -> ally.BottomOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.HitPoints > 0
                         Entity.Fill <== ally --> fun ally -> single ally.HitPoints / single ally.HitPointsMax]

                     // regular menu
                     Content.entity<RingMenuDispatcher> "RegularMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = RegularMenu
                         Entity.Enabled <== model --> fun model ->
                            let allies = BattleModel.getAllies model
                            let alliesPastRegularMenu =
                                List.notExists (fun (ally : CharacterModel) ->
                                    match ally.InputState with NoInput | RegularMenu -> false | _ -> true)
                                    allies
                            alliesPastRegularMenu
                         Entity.RingMenuModel == { Items = [(0, (true, "Attack")); (1, (true, "Defend")); (2, (true, "Consumable")); (3, (true, "Tech"))]; ItemCancelOpt = None }
                         Entity.ItemSelectEvent ==|> fun evt -> msg (RegularItemSelect (allyIndex, evt.Data))
                         Entity.CancelEvent ==> msg (RegularItemCancel allyIndex)]

                     // consumable menu
                     Content.entity<RingMenuDispatcher> "ConsumableMenu"
                        [Entity.Position <== ally --> fun ally -> ally.CenterOffset
                         Entity.Depth == Constants.Battle.GuiDepth
                         Entity.Visible <== ally --> fun ally -> ally.InputState = ItemMenu
                         Entity.RingMenuModel <== model --> fun model ->
                            let consumables = Inventory.getConsumables model.Inventory
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
                         Entity.RingMenuModel <== ally --> fun ally ->
                            let techs = List.ofSeq ally.Techs
                            let techs =
                                List.map (fun tech ->
                                    let techTag = getTag tech
                                    let techUsable =
                                        match Map.tryFind tech data.Value.Techs with
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
                         Entity.ReticlesModel <== model --> fun model ->
                            let aimType =
                                match BattleModel.tryGetCharacter allyIndex model with
                                | Some character -> character.InputState.AimType
                                | None -> NoAim
                            { BattleModel = model; AimType = aimType }
                         Entity.TargetSelectEvent ==|> fun evt -> msg (ReticlesSelect (evt.Data, allyIndex))
                         Entity.CancelEvent ==> msg (ReticlesCancel allyIndex)]]

        override this.Content (model, screen) =
            [this.SceneContent (model, screen)
             this.InputContent (model, screen)]